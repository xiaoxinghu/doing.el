;;; doing-totals.el --- Duration aggregation and totals reporting -*- lexical-binding: t -*-

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

;; This module provides duration aggregation utilities and the `doing-totals'
;; command for generating time reports grouped by tag or project.

;;; Code:

(require 'doing-lib)
(require 'doing-view)
(require 'doing-rollover)

;;; Duration Aggregation Utilities

(defun doing--group-by-tag (entries)
  "Return alist of (TAG . ENTRIES) for each tag in ENTRIES.
Each entry may have multiple tags, so entries can appear in multiple groups."
  (let ((result nil))
    (dolist (entry entries)
      (let ((tags (plist-get entry :tags)))
        (when tags
          (dolist (tag tags)
            (let ((existing (assoc tag result)))
              (if existing
                  (setcdr existing (cons entry (cdr existing)))
                (push (cons tag (list entry)) result)))))))
    result))

(defun doing--totals-by-tag (entries)
  "Return alist of (TAG . MINUTES) sorted by minutes descending.
Computes total duration for each tag across all ENTRIES."
  (let ((groups (doing--group-by-tag entries)))
    (seq-sort-by #'cdr #'>
                 (mapcar (lambda (g)
                           (cons (car g) (doing--sum-durations (cdr g))))
                         groups))))

;;; User-Facing Commands

;;;###autoload
(defun doing-totals ()
  "Show time totals for this week grouped by tag.
Displays total duration for each tag and overall total in a temporary buffer."
  (interactive)
  (doing--ensure-rollover)
  (let* ((entries (append (doing--parse-file (doing--file-today))
                          (doing--parse-file (doing--file-week))))
         (totals (doing--totals-by-tag entries))
         (buf (get-buffer-create "*doing: totals*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Totals for %s\n\n" (doing--iso-week-string)))
        (insert "By tag:\n")
        (dolist (pair totals)
          (insert (format "  %-16s %s\n" (car pair)
                          (doing--duration-format (cdr pair)))))
        (insert "  ----------------\n")
        (insert (format "  %-16s %s\n" "Total"
                        (doing--duration-format
                         (doing--sum-durations entries))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))

(provide 'doing-totals)

;;; doing-totals.el ends here
