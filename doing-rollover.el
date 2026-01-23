;;; doing-rollover.el --- Entry rollover utilities for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal functions for rolling over entries between files:
;; - Daily rollover: today.org → week.org
;; - Weekly rollover: week.org → archive/YYYY-WNN.org
;;
;; These functions are called automatically by user-facing commands
;; to ensure entries are properly organized over time.

;;; Code:

(require 'doing-lib)

;;; Daily Rollover

(defun doing--rollover-daily ()
  "Move entries from previous days to week.org.
Returns the number of entries moved, or nil if none were moved."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (today-file (doing--file-today))
         (entries (doing--parse-file today-file))
         (old (seq-filter
               (lambda (e)
                 (let ((started (plist-get e :started)))
                   (and started
                        (not (string-prefix-p today (doing--timestamp-date started))))))
               entries)))
    (when old
      ;; Append old entries to week.org
      (dolist (entry old)
        (doing--append-entry-to-file entry (doing--file-week)))
      ;; Remove old entries from today.org
      (dolist (entry old)
        (doing--delete-entry (plist-get entry :id) today-file))
      (length old))))

(provide 'doing-rollover)

;;; doing-rollover.el ends here
