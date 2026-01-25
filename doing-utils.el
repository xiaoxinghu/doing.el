;;; doing-utils.el --- Utility commands for doing.el -*- lexical-binding: t; -*-

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

;; Utility commands for inspecting and editing doing entries.
;; Provides convenient access to view last entry, edit current entry,
;; and open log files.

;;; Code:

(require 'doing-lib)
(require 'doing-current)

;;;###autoload
(defun doing-last ()
  "Show the last entry (finished or not) in the minibuffer.
Displays the most recent entry from today.org with an asterisk (*)
if it is still active (unfinished)."
  (interactive)
  (let ((entries (doing--parse-file (doing--file-today))))
    (if-let ((entry (car (last entries))))
        (message "%s%s"
                 (plist-get entry :title)
                 (if (plist-get entry :ended) "" " *"))
      (message "No entries"))))

;;;###autoload
(defun doing-edit ()
  "Open today.org and jump to the current entry.
If there is an active (unfinished) entry, jump to it and show context.
Otherwise, open today.org at the last entry."
  (interactive)
  (find-file (doing--file-today))
  (when-let ((entry (doing--current-entry)))
    (goto-char (plist-get entry :begin))
    ;; Use org-fold-show-context if available (Org 9.6+), fallback to org-show-context
    (if (fboundp 'org-fold-show-context)
        (org-fold-show-context)
      (with-no-warnings (org-show-context)))))

;;;###autoload
(defun doing-open (&optional file)
  "Open a doing log FILE directly.
Without prefix argument, opens today.org.
With prefix argument, prompt to choose between today and week files."
  (interactive
   (list (when current-prefix-arg
           (completing-read "File: "
                            '("today" "week")))))
  (find-file (pcase file
               ("week" (doing--file-week))
               (_ (doing--file-today)))))

(provide 'doing-utils)

;;; doing-utils.el ends here
