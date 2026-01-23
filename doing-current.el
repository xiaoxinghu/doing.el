;;; doing-current.el --- Show current activity for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands for displaying the current unfinished activity.
;; Provides utility functions to show what you're currently doing.

;;; Code:

(require 'doing-lib)

(defun doing--current-entry ()
  "Return the current (unfinished) entry or nil.
Searches today.org for entries without an ENDED property,
returning the most recent one."
  (let ((entries (doing--parse-file (doing--file-today))))
    (seq-find (lambda (e) (null (plist-get e :ended)))
              (reverse entries))))  ; most recent first

;;;###autoload
(defun doing-current ()
  "Show current activity in minibuffer.
Displays the current unfinished activity with elapsed time.
If no activity is in progress, shows a message indicating so."
  (interactive)
  (if-let ((entry (doing--current-entry)))
      (let* ((started (plist-get entry :started))
             (elapsed (doing--duration-minutes started (doing--timestamp-now)))
             (title (plist-get entry :title)))
        (message "[%s] %s" (doing--duration-format elapsed) title))
    (message "No activity in progress")))

(provide 'doing-current)

;;; doing-current.el ends here
