;;; doing-note.el --- Add notes to current activity for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands for adding notes to the current unfinished activity.
;; Appends text to the body of the current entry.

;;; Code:

(require 'doing-lib)
(require 'doing-current)

;;;###autoload
(defun doing-note (text)
  "Add TEXT as note to current activity.
Appends TEXT to the body of the current entry after the properties drawer.
Signals a user-error if no activity is in progress."
  (interactive "sNote: ")
  (if-let ((entry (doing--current-entry)))
      (let ((id (plist-get entry :id)))
        (with-current-buffer (find-file-noselect (doing--file-today))
          (save-excursion
            (when (doing--goto-entry id)
              (org-end-of-meta-data t)
              (insert text "\n")
              (save-buffer))))
        (message "Note added"))
    (user-error "No activity in progress")))

(provide 'doing-note)

;;; doing-note.el ends here
