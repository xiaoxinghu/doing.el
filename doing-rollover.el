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

;;; Weekly Rollover

(defun doing--rollover-weekly ()
  "Archive entries from previous weeks to archive/YYYY-WNN.org.
Returns the number of entries moved, or nil if none were moved."
  (let* ((current-week (doing--iso-week))
         (week-file (doing--file-week))
         (entries (doing--parse-file week-file))
         (moved 0))
    (dolist (entry entries)
      (let* ((started (plist-get entry :started))
             (entry-time (when started
                           (encode-time (org-parse-time-string started))))
             (entry-week (when entry-time
                           (doing--iso-week entry-time))))
        (when (and entry-week
                   (not (equal entry-week current-week)))
          ;; Archive this entry
          (let ((archive-file (doing--file-archive
                               (car entry-week) (cadr entry-week))))
            (doing--append-entry-to-file entry archive-file)
            (doing--delete-entry (plist-get entry :id) week-file)
            (setq moved (1+ moved))))))
    (when (> moved 0) moved)))

(provide 'doing-rollover)

;;; doing-rollover.el ends here
