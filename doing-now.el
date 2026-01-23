;;; doing-now.el --- Capture commands for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands for capturing new activities in the doing log.
;; Provides the main entry point for starting and tracking activities.

;;; Code:

(require 'doing-lib)

;;;###autoload
(defun doing-now (title &optional tags)
  "Start new activity with TITLE and optional TAGS.
Creates a new entry in today.org with automatic timestamp and ID.
If called interactively, prompts for TITLE. TAGS should be a list
of strings, e.g., (\"emacs\" \"coding\")."
  (interactive "sWhat are you doing? ")
  (doing--ensure-directory)
  (let ((entry (list :id (doing--generate-id)
                     :title title
                     :tags tags
                     :started (doing--timestamp-now))))
    (doing--append-entry-to-file entry (doing--file-today))
    (message "Started: %s" title)))

(provide 'doing-now)

;;; doing-now.el ends here
