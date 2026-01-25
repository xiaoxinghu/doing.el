;;; doing-view-commands.el --- View commands for doing.el -*- lexical-binding: t; -*-

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

;; Standard view commands for displaying entries:
;; - doing-view-today: Display today's activities
;; - doing-view-yesterday: Display yesterday's activities
;; - doing-view-week: Display this week's activities grouped by date

;;; Code:

(require 'doing-lib)
(require 'doing-view)
(require 'doing-rollover)
(require 'seq)
(require 'subr-x)

;;; Standard View Commands

;;;###autoload
(defun doing-view-today ()
  "Display today's activities."
  (interactive)
  (doing--ensure-rollover)
  (let ((entries (doing--parse-file (doing--file-today))))
    (doing--view-buffer "today" entries)))

;;;###autoload
(defun doing-view-yesterday ()
  "Display yesterday's activities."
  (interactive)
  (doing--ensure-rollover)
  (let* ((yesterday (format-time-string "%F"
                      (time-subtract (current-time) (days-to-time 1))))
         (all-entries (append (doing--parse-file (doing--file-today))
                              (doing--parse-file (doing--file-week))))
         (filtered (seq-filter
                    (lambda (e)
                      (string= yesterday (doing--timestamp-date (plist-get e :started))))
                    all-entries)))
    (doing--view-buffer "yesterday" filtered)))

;;;###autoload
(defun doing-view-week ()
  "Display this week's activities grouped by date."
  (interactive)
  (doing--ensure-rollover)
  (let ((entries (append (doing--parse-file (doing--file-today))
                         (doing--parse-file (doing--file-week)))))
    (doing--view-buffer "this week" entries
                        (lambda (e) (doing--timestamp-date (plist-get e :started))))))

;;;###autoload
(defun doing-view-recent (&optional n)
  "Display N most recent entries (default 10).
With prefix argument, prompt for N."
  (interactive "P")
  (doing--ensure-rollover)
  (let* ((n (if (numberp n) n (or n 10)))
         (entries (append (doing--parse-file (doing--file-today))
                          (doing--parse-file (doing--file-week))))
         (sorted (seq-sort-by (lambda (e) (plist-get e :started))
                              #'string> entries))
         (recent (seq-take sorted n)))
    (doing--view-buffer (format "recent (%d)" (length recent)) recent)))

;;;###autoload
(defun doing-view-since (date)
  "Display entries since DATE.
DATE is prompted using `org-read-date'."
  (interactive (list (org-read-date nil nil nil "Since: ")))
  (doing--ensure-rollover)
  (let* ((all-entries (append (doing--parse-file (doing--file-today))
                              (doing--parse-file (doing--file-week))))
         (filtered (seq-filter
                    (lambda (e)
                      (not (string-lessp (doing--timestamp-date (plist-get e :started))
                                         date)))
                    all-entries)))
    (doing--view-buffer (format "since %s" date) filtered)))

(provide 'doing-view-commands)

;;; doing-view-commands.el ends here
