;;; doing-again.el --- Resume commands for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands for resuming previous activities in the doing log.
;; Provides convenient ways to restart recently completed tasks.

;;; Code:

(require 'doing-lib)
(require 'doing-now)

(defun doing--last-finished-entry ()
  "Return the most recent finished entry or nil.
Searches today.org for entries with an ENDED property and returns
the most recent one (last in the file)."
  (let* ((entries (doing--parse-file (doing--file-today)))
         (finished (seq-filter (lambda (e) (plist-get e :ended)) entries)))
    (car (last finished))))

;;;###autoload
(defun doing-again ()
  "Resume the last finished activity.
Creates a new entry with the same title and tags as the most recent
finished activity.  The new entry gets a fresh ID and start timestamp.
If no previous finished activity exists, signals an error."
  (interactive)
  (if-let ((entry (doing--last-finished-entry)))
      (doing-now (plist-get entry :title) (plist-get entry :tags))
    (user-error "No previous activity to resume")))

(provide 'doing-again)

;;; doing-again.el ends here
