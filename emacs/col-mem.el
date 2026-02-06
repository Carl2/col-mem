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

(provide 'col-mem)
;;; col-mem.el ends here
