;;; doing-view.el --- View infrastructure for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared infrastructure for displaying entries in temporary buffers.
;; Provides functions for creating view buffers, formatting entries,
;; and computing duration totals.

;;; Code:

(require 'doing-lib)
(require 'org)
(require 'seq)

;;; View Buffer Creation

(defun doing--view-buffer (name entries &optional group-fn)
  "Display ENTRIES in buffer *doing: NAME*.
Optional GROUP-FN groups entries (e.g., by date).
If GROUP-FN is nil, entries are displayed in a flat list.
If GROUP-FN is provided, it should be a function that takes an entry
and returns a grouping key (e.g., date string)."
  (let ((buf (get-buffer-create (format "*doing: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        ;; Insert entries
        (if group-fn
            ;; Grouped display
            (dolist (group (seq-group-by group-fn entries))
              (insert (format "* %s\n" (car group)))
              (dolist (entry (cdr group))
                (insert (doing--format-entry-line entry))))
          ;; Flat display
          (dolist (entry entries)
            (insert (doing--format-entry-line entry))))
        ;; Insert totals
        (insert "\n---\n")
        (insert (format "Total: %s\n"
                        (doing--duration-format
                         (doing--sum-durations entries))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))

;;; Entry Formatting

(defun doing--format-entry-line (entry)
  "Format ENTRY as a single line for views.
Shows duration, title, active marker (*), and tags.
Returns a formatted string with newline."
  (let ((title (plist-get entry :title))
        (started (plist-get entry :started))
        (ended (plist-get entry :ended))
        (tags (plist-get entry :tags)))
    (format "- [%s] %s%s%s\n"
            (if ended
                (doing--duration-format
                 (doing--duration-minutes started ended))
              (doing--duration-format
               (doing--duration-minutes started (doing--timestamp-now))))
            title
            (if ended "" " *")
            (if tags (concat " :" (mapconcat #'identity tags ":") ":") ""))))

;;; Duration Aggregation

(defun doing--sum-durations (entries)
  "Sum durations of ENTRIES in minutes.
For unfinished entries, uses current time as end time.
Returns total duration as a float in minutes."
  (apply #'+
         (mapcar (lambda (e)
                   (let ((started (plist-get e :started))
                         (ended (or (plist-get e :ended)
                                    (doing--timestamp-now))))
                     (doing--duration-minutes started ended)))
                 entries)))

(provide 'doing-view)

;;; doing-view.el ends here
