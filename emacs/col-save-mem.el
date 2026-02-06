;;; col-save-mem.el --- Memory Save with LLM Summarization -*- lexical-binding: t; -*-
;;;; Memory Save with LLM Summarization

(defcustom col-mem-summary-system-prompt
  "You are a memory extraction assistant. Extract key information worth remembering for future conversations.

Output JSON with exactly two fields:
- \"title\": A short descriptive title (max 10 words)
- \"summary\": A list of key facts, preferences, decisions, or learnings (2-5 bullet points as strings)

Focus on:
- User preferences and opinions
- Technical decisions and rationale
- Project-specific knowledge
- Patterns and conventions

Do NOT include:
- Transient information
- Generic knowledge
- Conversation filler

Respond ONLY with valid JSON, no markdown code blocks."
  "System prompt for LLM summarization."
  :type 'string
  :group 'col-mem)

(defun col-mem--get-content ()
  "Get content to summarize: region, org heading, or buffer.
Returns (CONTENT . SOURCE) cons cell."
  (cond
   ;; Active region
   ((use-region-p)
    (cons (buffer-substring-no-properties (region-beginning) (region-end))
          (format "region in %s" (buffer-name))))
   ;; Org heading (get subtree content)
   ((and (derived-mode-p 'org-mode)
         (save-excursion
           (org-back-to-heading-or-point-min t)
           (org-at-heading-p)))
    (save-excursion
      (org-back-to-heading-or-point-min t)
      (let ((heading (org-get-heading t t t t))
            (content (buffer-substring-no-properties
                      (point)
                      (save-excursion (org-end-of-subtree t t) (point)))))
        (cons content
              (format "heading '%s' in %s" heading (buffer-name))))))
   ;; Whole buffer
   (t
    (cons (buffer-substring-no-properties (point-min) (point-max))
          (format "buffer %s" (buffer-name))))))

(defun col-mem--detect-project ()
  "Detect project name from project.el or directory."
  (or col-mem-default-project
      (when-let ((proj (project-current)))
        (file-name-nondirectory (directory-file-name (project-root proj))))
      (file-name-nondirectory (directory-file-name default-directory))))

(defun col-mem--parse-llm-response (response)
  "Parse LLM RESPONSE as JSON, returning alist with title and summary."
  (condition-case err
      (let* ((cleaned (string-trim response))
             ;; Remove markdown code blocks if present
             (cleaned (if (string-prefix-p "```" cleaned)
                          (replace-regexp-in-string
                           "```json\\|```" "" cleaned)
                        cleaned))
             (parsed (json-read-from-string (string-trim cleaned)))
             (title (alist-get 'title parsed))
             (summary-raw (alist-get 'summary parsed))
             ;; Handle summary as list or string
             (summary (if (vectorp summary-raw)
                          (mapconcat #'identity (append summary-raw nil) "\n- ")
                        summary-raw)))
        `((title . ,title)
          (summary . ,(if (and summary (not (string-prefix-p "- " summary)))
                          (concat "- " summary)
                        summary))))
    (error
     (message "JSON parse error: %s" err)
     nil)))

(defun col-mem--confirm-save (title summary project source)
  "Show TITLE and SUMMARY for confirmation before saving.
PROJECT and SOURCE are metadata. Offers confirm/edit/cancel."
  (let ((buf (get-buffer-create "*col-mem-confirm*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Memory to Save\n" 'face '(:weight bold :height 1.2)))
        (insert (make-string 50 ?─) "\n\n")
        (insert (propertize "Title: " 'face 'font-lock-keyword-face))
        (insert title "\n\n")
        (insert (propertize "Summary:\n" 'face 'font-lock-keyword-face))
        (insert summary "\n\n")
        (insert (make-string 50 ?─) "\n")
        (insert (propertize "Project: " 'face 'font-lock-comment-face))
        (insert (or project "none") "\n")
        (insert (propertize "Source: " 'face 'font-lock-comment-face))
        (insert (or source "unknown") "\n\n")
        (insert (propertize "[y]es  [e]dit  [q]uit" 'face 'font-lock-doc-face)))
      (setq-local col-mem--pending-save
                  (list :title title :summary summary :project project :source source))
      (setq buffer-read-only t)
      (local-set-key (kbd "y") #'col-mem--do-save)
      (local-set-key (kbd "e") #'col-mem--edit-save)
      (local-set-key (kbd "q") #'col-mem--cancel-save)
      (local-set-key (kbd "C-g") #'col-mem--cancel-save))
    (pop-to-buffer buf '((display-buffer-below-selected)
                         (window-height . fit-window-to-buffer)))))

(defun col-mem--do-save ()
  "Save the pending memory."
  (interactive)
  (when-let ((pending col-mem--pending-save))
    (let ((title (plist-get pending :title))
          (summary (plist-get pending :summary))
          (project (plist-get pending :project))
          (source (plist-get pending :source)))
      (col-mem--store title summary project nil nil source)
      (message "✓ Saved memory: %s" title)))
  (kill-buffer-and-window))

(defun col-mem--edit-save ()
  "Edit title and summary before saving."
  (interactive)
  (when-let ((pending col-mem--pending-save))
    (let* ((title (read-string "Title: " (plist-get pending :title)))
           (summary (read-string "Summary: " (plist-get pending :summary)))
           (project (plist-get pending :project))
           (source (plist-get pending :source)))
      (col-mem--store title summary project nil nil source)
      (message "✓ Saved memory: %s" title)))
  (kill-buffer-and-window))

(defun col-mem--cancel-save ()
  "Cancel saving memory."
  (interactive)
  (message "Cancelled")
  (kill-buffer-and-window))

;;;###autoload
(defun col-mem-save ()
  "Summarize buffer/region/heading with LLM and store to memory."
  (interactive)
  (let* ((content-info (col-mem--get-content))
         (content (car content-info))
         (source (cdr content-info))
         (project (col-mem--detect-project)))
    (when (or (null content) (string-empty-p (string-trim content)))
      (user-error "No content to save"))
    (message "Summarizing with LLM... (source: %s)" source)
    ;; Capture variables for async callback
    (let ((proj project)
          (src source))
      (gptel-request
       (format "Summarize this content for long-term memory:\n\n%s" content)
       :system col-mem-summary-system-prompt
       :callback
       (lambda (response info)
         (if (not response)
             (message "LLM request failed: %s" (plist-get info :status))
           (let ((parsed (col-mem--parse-llm-response response)))
             (if (not parsed)
                 (message "Failed to parse LLM response: %s" response)
               (let ((title (alist-get 'title parsed))
                     (summary (alist-get 'summary parsed)))
                 (col-mem--confirm-save title summary proj src))))))))))

;;;###autoload
(defun col-mem-save-direct ()
  "Manually enter title and summary to store as memory."
  (interactive)
  (let* ((title (read-string "Memory title: "))
         (summary (read-string "Summary: "))
         (project (col-mem--detect-project))
         (source (format "manual entry from %s" (buffer-name))))
    (when (string-empty-p title)
      (user-error "Title cannot be empty"))
    (col-mem--store title summary project nil nil source)
    (message "✓ Saved memory: %s" title)))
