;;; doing-search.el --- Search functionality for doing.el -*- lexical-binding: t -*-

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

;; Search and filter entries by text or tags without requiring org-ql.

;;; Code:

(require 'doing-lib)
(require 'doing-rollover)
(require 'doing-view)

(defun doing--entries-matching (predicate)
  "Return entries matching PREDICATE from today and week files."
  (let ((entries (append (doing--parse-file (doing--file-today))
                         (doing--parse-file (doing--file-week)))))
    (seq-filter predicate entries)))

;;;###autoload
(defun doing-search (query)
  "Search entries matching QUERY (text or @tag).
If QUERY starts with @, search by tag.
Otherwise, search by text in entry title."
  (interactive "sSearch: ")
  (doing--ensure-rollover)
  (let* ((predicate
          (cond
           ;; @tag syntax
           ((string-prefix-p "@" query)
            (let ((tag (substring query 1)))
              (lambda (e) (member tag (plist-get e :tags)))))
           ;; Plain text
           (t (lambda (e)
                (string-match-p (regexp-quote query)
                                (plist-get e :title))))))
         (results (doing--entries-matching predicate)))
    (if results
        (doing--view-buffer (format "search: %s" query) results)
      (message "No entries matching \"%s\"" query))))

(provide 'doing-search)

;;; doing-search.el ends here
