;;; doing-note.el --- Add notes to current activity for doing.el -*- lexical-binding: t; -*-

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
