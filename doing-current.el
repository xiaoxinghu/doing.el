;;; doing-current.el --- Show current activity for doing.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Xiaoxing Hu

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Commands for displaying the current unfinished activity.
;; Provides utility functions to show what you're currently doing.

;;; Code:

(require 'doing-lib)
(require 'doing-rollover)

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
  (doing--ensure-rollover)
  (if-let ((entry (doing--current-entry)))
      (let* ((started (plist-get entry :started))
             (elapsed (doing--duration-minutes started (doing--timestamp-now)))
             (title (plist-get entry :title)))
        (message "[%s] %s" (doing--duration-format elapsed) title))
    (message "No activity in progress")))

(provide 'doing-current)

;;; doing-current.el ends here
