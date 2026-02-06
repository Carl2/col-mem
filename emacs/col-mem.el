;;; col-mem.el --- Long-term memory for gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Colin
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gptel "0.5") (consult "0.35"))
;; Keywords: ai, tools, convenience
;; URL: https://github.com/colin/col-mem

;;; Commentary:

;; Persistent, semantically-searchable memory for gptel conversations.
;;
;; This package provides:
;; - `col-mem-save': Summarize buffer/region and store to vector DB
;; - `col-mem-recall': Search memories and inject context into gptel
;;
;; Requires the col-mem Python backend to be installed.

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Customization

(defgroup col-mem nil
  "Long-term memory for gptel."
  :group 'gptel
  :prefix "col-mem-")

(defcustom col-mem-python-command "uv"
  "Python command to run col-mem CLI.
Can be 'uv', 'python', or a full path."
  :type 'string
  :group 'col-mem)

(defcustom col-mem-project-root "~/git/personal/col_mem/"
  "Path to col-mem Python project root."
  :type 'directory
  :group 'col-mem)

(defcustom col-mem-default-top-k 10
  "Default number of search results to return."
  :type 'integer
  :group 'col-mem)

(defcustom col-mem-default-project nil
  "Default project name for storing/filtering memories.
If nil, no project filter is applied."
  :type '(choice (const nil) string)
  :group 'col-mem)

;;;; Internal functions

(defun col-mem--run-cli (args)
  "Run col-mem CLI with ARGS and return parsed JSON."
  (let* ((default-directory (expand-file-name col-mem-project-root))
         (command (if (string= col-mem-python-command "uv")
                      (append '("uv" "run" "col-mem") args)
                    (append (list col-mem-python-command "-m" "col_mem.cli") args)))
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (apply #'call-process (car command) nil t nil (cdr command))))))
    (condition-case err
        (json-read-from-string output)
      (error
       (message "col-mem CLI error: %s\nOutput: %s" err output)
       nil))))

(defun col-mem--store (title summary &optional project type tags source)
  "Store a memory with TITLE, SUMMARY, and optional metadata.
PROJECT, TYPE, TAGS (list), and SOURCE are optional."
  (let* ((data `((title . ,title)
                 (summary . ,summary)))
         (json-encoding-pretty-print nil))
    (when project (push `(project . ,project) data))
    (when type (push `(type . ,type) data))
    (when tags (push `(tags . ,tags) data))
    (when source (push `(source . ,source) data))
    (col-mem--run-cli (list "store" (json-encode data)))))

(defun col-mem--search (query &optional top-k project type)
  "Search memories matching QUERY.
TOP-K limits results, PROJECT and TYPE filter by metadata."
  (let ((args (list "search" query)))
    (when top-k
      (setq args (append args (list "--top-k" (number-to-string top-k)))))
    (when project
      (setq args (append args (list "--project" project))))
    (when type
      (setq args (append args (list "--type" type))))
    (col-mem--run-cli args)))

(defun col-mem--list (&optional project type)
  "List all memories, optionally filtered by PROJECT or TYPE."
  (let ((args '("list")))
    (when project
      (setq args (append args (list "--project" project))))
    (when type
      (setq args (append args (list "--type" type))))
    (col-mem--run-cli args)))

(defun col-mem--delete (id)
  "Delete memory with ID."
  (col-mem--run-cli (list "delete" id)))

;;;; Public API

;; TODO: col-mem-save - summarize buffer/region with LLM, store to DB
;; TODO: col-mem-recall - search, consult select, inject context


;;;; Consult Integration

(require 'consult)

(defun col-mem--format-candidate (memory)
  "Format MEMORY alist as a propertized candidate string."
  (let* ((id (alist-get 'id memory))
         (title (alist-get 'title memory))
         (summary (alist-get 'summary memory))
         (score (alist-get 'score memory))
         (project (alist-get 'project memory))
         (display (if score
                      (format "[%.1f] %s" (* score 100) title)
                    title)))
    (propertize display
                'col-mem-id id
                'col-mem-title title
                'col-mem-summary summary
                'col-mem-project project
                'col-mem-memory memory)))

(defun col-mem--get-memory (candidate)
  "Extract memory alist from propertized CANDIDATE string."
  (when candidate
    (get-text-property 0 'col-mem-memory candidate)))

(defun col-mem--lookup (candidate candidates _input _narrow)
  "Lookup function for consult: find CANDIDATE in CANDIDATES.
Returns the propertized candidate from the list (not the input string)."
  (when candidate
    (car (member candidate candidates))))

(defun col-mem--preview (action candidate)
  "Preview function for consult.
ACTION is 'preview, 'return, or 'exit. CANDIDATE is the current selection."
  (pcase action
    ('preview
     (when candidate
       (let* ((memory (col-mem--get-memory candidate))
              (title (alist-get 'title memory))
              (summary (alist-get 'summary memory))
              (project (alist-get 'project memory))
              (source (alist-get 'source memory))
              (created (alist-get 'created_at memory))
              (buf (get-buffer-create "*col-mem-preview*")))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (propertize (or title "Untitled") 'face 'bold) "\n")
             (insert (make-string 40 ?─) "\n\n")
             (when project (insert (format "Project: %s\n" project)))
             (when source (insert (format "Source: %s\n" source)))
             (when created (insert (format "Created: %s\n" created)))
             (insert "\n")
             (insert (or summary "No summary"))))
         (display-buffer buf '(display-buffer-in-side-window
                               (side . right)
                               (window-width . 0.4))))))
    ('exit
     (when-let ((buf (get-buffer "*col-mem-preview*")))
       (kill-buffer buf)))))

(defun col-mem--format-context (memories)
  "Format MEMORIES list as context string for injection."
  (mapconcat
   (lambda (mem)
     (format "* %s\n%s"
             (alist-get 'title mem)
             (alist-get 'summary mem)))
   memories
   "\n\n"))

;;;###autoload
(defun col-mem-recall (query)
  "Search memories matching QUERY and insert selected at point."
  (interactive "sSearch memories: ")
  (let* ((response (col-mem--search query col-mem-default-top-k col-mem-default-project))
         (results (alist-get 'results response)))
    (if (or (null results) (= 0 (length results)))
        (message "No memories found for: %s" query)
      (let* ((candidates (mapcar #'col-mem--format-candidate results))
             (selected (consult--read candidates
                                      :prompt "Select memory: "
                                      :category 'col-mem
                                      :lookup #'col-mem--lookup
                                      :state #'col-mem--preview
                                      :sort nil)))
        (when selected
          (let* ((memory (col-mem--get-memory selected))
                 (context (col-mem--format-context (list memory))))
            (insert context)
            (message "✓ Inserted memory: %s" (alist-get 'title memory))))))))

;;;###autoload
(defun col-mem-recall-multi (query)
  "Search memories matching QUERY and insert multiple selected at point.
Keep selecting memories until you choose [Done] to finish."
  (interactive "sSearch memories: ")
  (let* ((response (col-mem--search query col-mem-default-top-k col-mem-default-project))
         (results (alist-get 'results response)))
    (if (or (null results) (= 0 (length results)))
        (message "No memories found for: %s" query)
      (let* ((candidates (mapcar #'col-mem--format-candidate results))
             (done-candidate (propertize "[Done - finish selecting]" 'face 'font-lock-comment-face))
             (all-candidates (cons done-candidate candidates))
             (count 0))
        (catch 'done
          (while t
            (let ((selected (consult--read all-candidates
                                           :prompt (format "Select memory (%d selected): " count)
                                           :category 'col-mem
                                           :lookup #'col-mem--lookup
                                           :state #'col-mem--preview
                                           :sort nil)))
              (cond
               ((or (null selected) (equal selected done-candidate))
                (throw 'done nil))
               (t
                (let* ((memory (col-mem--get-memory selected))
                       (context (col-mem--format-context (list memory))))
                  (insert context)
                  (insert "\n\n")
                  (cl-incf count)))))))
        (message "✓ Inserted %d memor%s" count (if (= 1 count) "y" "ies"))))))


;;;###autoload
(defun col-mem-list-all ()
  "List and browse all stored memories."
  (interactive)
  (let* ((response (col-mem--list col-mem-default-project))
         (results (alist-get 'memories response)))
    (if (or (null results) (= 0 (length results)))
        (message "No memories stored")
      (let* ((candidates (mapcar #'col-mem--format-candidate results))
             (selected (consult--read candidates
                                      :prompt "Browse memories: "
                                      :category 'col-mem
                                      :lookup #'col-mem--lookup
                                      :state #'col-mem--preview
                                      :sort nil)))
        (when selected
          (let ((memory (col-mem--get-memory selected)))
            (message "Selected: %s" (alist-get 'title memory))))))))

;;;###autoload
(defun col-mem-delete ()
  "Interactively select and delete a memory."
  (interactive)
  (let* ((response (col-mem--list col-mem-default-project))
         (results (alist-get 'memories response)))
    (if (or (null results) (= 0 (length results)))
        (message "No memories to delete")
      (let* ((candidates (mapcar #'col-mem--format-candidate results))
             (selected (consult--read candidates
                                      :prompt "Delete memory: "
                                      :category 'col-mem
                                      :lookup #'col-mem--lookup
                                      :state #'col-mem--preview
                                      :sort nil)))
        (when selected
          (let* ((memory (col-mem--get-memory selected))
                 (id (alist-get 'id memory))
                 (title (alist-get 'title memory)))
            (when (yes-or-no-p (format "Delete '%s'? " title))
              (col-mem--delete id)
              (message "✓ Deleted: %s" title))))))))

;;;###autoload
(defun col-mem-recall-project (project)
  "Recall memories from PROJECT, select with consult, insert at point.
With prefix arg, prompt for project name."
  (interactive
   (list (if current-prefix-arg
             (read-string "Project: " col-mem-default-project)
           (or col-mem-default-project
               (col-mem--detect-project)))))
  (let* ((response (col-mem--list project))
         (results (alist-get 'memories response)))
    (if (or (null results) (= 0 (length results)))
        (message "No memories found for project: %s" project)
      (let* ((candidates (mapcar #'col-mem--format-candidate results))
             (selected (consult--read candidates
                                      :prompt (format "Memory [%s]: " project)
                                      :category 'col-mem
                                      :lookup #'col-mem--lookup
                                      :state #'col-mem--preview
                                      :sort nil)))
        (when selected
          (let* ((memory (col-mem--get-memory selected))
                 (context (col-mem--format-context (list memory))))
            (insert context)))))))

(provide 'col-mem)
;;; col-mem.el ends here
