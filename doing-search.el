;;; doing-search.el --- Search functionality for doing.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 Xiaoxing Hu

;; This file is part of doing.el.

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
