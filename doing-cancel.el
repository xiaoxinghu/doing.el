;;; doing-cancel.el --- Cancel current activity -*- lexical-binding: t -*-

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
