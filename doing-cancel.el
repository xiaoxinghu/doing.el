;;; doing-cancel.el --- Cancel current activity -*- lexical-binding: t -*-

;; Copyright (C) 2026 Xiaoxing Hu

;; This file is part of doing.el.

;;; Commentary:

;; This module provides the `doing-cancel' command for canceling
;; the current unfinished activity.

;;; Code:

(require 'doing-lib)
(require 'doing-current)

;;;###autoload
(defun doing-cancel ()
  "Cancel the current activity."
  (interactive)
  (if-let ((entry (doing--current-entry)))
      (let ((id (plist-get entry :id))
            (title (plist-get entry :title)))
        (when (yes-or-no-p (format "Cancel \"%s\"? " title))
          (doing--delete-entry id)
          (message "Cancelled: %s" title)))
    (user-error "No activity in progress")))

(provide 'doing-cancel)
;;; doing-cancel.el ends here
