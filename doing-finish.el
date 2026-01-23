;;; doing-finish.el --- Finish current activity for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands for finishing the current unfinished activity.
;; Marks the current activity as complete by setting ENDED timestamp
;; and computing the DURATION.

;;; Code:

(require 'doing-lib)
(require 'doing-current)

;;;###autoload
(defun doing-finish ()
  "Finish the current activity.
Sets the ENDED property to current time and computes DURATION.
Signals a user-error if no activity is in progress."
  (interactive)
  (if-let ((entry (doing--current-entry)))
      (let* ((id (plist-get entry :id))
             (title (plist-get entry :title))
             (started (plist-get entry :started))
             (ended (doing--timestamp-now))
             (duration (doing--duration-minutes started ended)))
        (doing--update-entry-property id "ENDED" ended)
        (doing--update-entry-property id "DURATION"
                                      (doing--duration-format duration))
        (message "Finished: %s (%s)" title (doing--duration-format duration)))
    (user-error "No activity in progress")))

(provide 'doing-finish)

;;; doing-finish.el ends here
