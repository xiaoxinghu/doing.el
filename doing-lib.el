;;; doing-lib.el --- Internal utilities for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal helper functions for file/directory management, parsing,
;; and other shared functionality used across the doing package.
;;
;; All functions in this file use the `doing--` prefix (double hyphen)
;; to indicate they are internal and not part of the public API.

;;; Code:

(require 'org)
(require 'org-element)

;; Forward declarations for variables defined in doing.el
(defvar doing-directory)
(defvar doing--file-today-name)
(defvar doing--file-week-name)
(defvar doing--archive-directory-name)
(defvar doing-auto-tags)

;;; File and Directory Utilities

(defun doing--directory ()
  "Return expanded `doing-directory', creating it if needed.
Creates both the main directory and the archive subdirectory."
  (let ((dir (expand-file-name doing-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    ;; Also ensure archive directory exists
    (let ((archive-dir (expand-file-name doing--archive-directory-name dir)))
      (unless (file-exists-p archive-dir)
        (make-directory archive-dir t)))
    dir))

(defun doing--file-today ()
  "Return full path to today.org file."
  (expand-file-name doing--file-today-name (doing--directory)))

(defun doing--file-week ()
  "Return full path to week.org file."
  (expand-file-name doing--file-week-name (doing--directory)))

(defun doing--file-archive (year week)
  "Return full path to archive file for YEAR and WEEK.
Format: archive/YYYY-WNN.org where YYYY is the year and NN is
the zero-padded ISO week number."
  (let ((archive-dir (expand-file-name doing--archive-directory-name
                                       (doing--directory)))
        (filename (format "%04d-W%02d.org" year week)))
    (expand-file-name filename archive-dir)))

(defun doing--ensure-directory ()
  "Ensure `doing-directory' and archive subdirectory exist.
Creates directories with parents if they don't exist."
  (doing--directory))  ; This function already creates both directories

(defun doing--ensure-file (path &optional title)
  "Ensure file at PATH exists, creating it with Org header if needed.
If TITLE is provided, use it as the #+TITLE header.
Otherwise, derive title from filename."
  (unless (file-exists-p path)
    (let ((file-title (or title
                          (file-name-sans-extension
                           (file-name-nondirectory path)))))
      (with-temp-buffer
        (insert (format "#+TITLE: %s\n\n" file-title))
        (write-region (point-min) (point-max) path)))))

;;; Timestamp and Duration Utilities

(defun doing--timestamp-now ()
  "Return current time as Org inactive timestamp.
Format: [YYYY-MM-DD DDD HH:MM]"
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun doing--timestamp-to-time (timestamp)
  "Parse Org TIMESTAMP string to Emacs time.
Returns time value suitable for `encode-time' and related functions."
  (org-parse-time-string timestamp))

(defun doing--timestamp-date (timestamp)
  "Extract YYYY-MM-DD from TIMESTAMP.
TIMESTAMP should be an Org timestamp like [2026-01-23 Thu 14:30]."
  (substring timestamp 1 11))

(defun doing--duration-minutes (start-ts end-ts)
  "Compute minutes between START-TS and END-TS timestamps.
Both arguments should be Org timestamp strings.
Returns duration as a float representing minutes."
  (let ((start (org-parse-time-string start-ts))
        (end (org-parse-time-string end-ts)))
    (/ (float-time (time-subtract (encode-time end) (encode-time start)))
       60.0)))

(defun doing--duration-format (minutes)
  "Format MINUTES as H:MM string.
Uses `org-duration-from-minutes' for consistent formatting."
  (org-duration-from-minutes minutes))

(defun doing--iso-week (&optional time)
  "Return (YEAR WEEK) for TIME or now.
Returns a list of two integers: (YEAR WEEK-NUMBER).
Uses ISO 8601 week date system."
  (let ((time (or time (current-time))))
    (list (string-to-number (format-time-string "%G" time))
          (string-to-number (format-time-string "%V" time)))))

(defun doing--iso-week-string (&optional time)
  "Return YYYY-WNN string for TIME or now.
Format matches archive file naming: 2026-W04"
  (format-time-string "%G-W%V" (or time (current-time))))

;;; Entry ID Generation

(defun doing--generate-id ()
  "Generate unique ID for entry.
Returns a timestamp-based ID in format: YYYYMMDDTHHMMSS"
  (format-time-string "%Y%m%dT%H%M%S"))

;;; Auto-Tagging

(defun doing--auto-tags-for-directory (&optional dir)
  "Return auto-tags plist for DIR, or nil.
DIR defaults to `default-directory'.
Matches DIR against `doing-auto-tags' and returns the plist
for the longest matching directory prefix.
Returns nil if no match found."
  (let ((dir (expand-file-name (or dir default-directory))))
    (cdr (seq-find (lambda (entry)
                     (string-prefix-p (expand-file-name (car entry)) dir))
                   (seq-sort-by (lambda (e) (length (car e))) #'>
                                doing-auto-tags)))))

;;; Entry Parsing Utilities

(defun doing--parse-entry-at-point ()
  "Parse headline at point into entry plist.
Returns a plist with keys: :id, :title, :tags, :started, :ended,
:project, :begin, :end.
Returns nil if point is not at a headline."
  (let ((el (org-element-at-point)))
    (when (eq 'headline (org-element-type el))
      (list :id (org-element-property :ID el)
            :title (org-element-property :raw-value el)
            :tags (org-element-property :tags el)
            :started (org-element-property :STARTED el)
            :ended (org-element-property :ENDED el)
            :project (org-element-property :PROJECT el)
            :begin (org-element-property :begin el)
            :end (org-element-property :end el)))))

(defun doing--parse-buffer ()
  "Return list of entries in current buffer.
Each entry is a plist with keys: :id, :title, :tags, :started,
:ended, :project, :begin."
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (hl)
      (list :id (org-element-property :ID hl)
            :title (org-element-property :raw-value hl)
            :tags (org-element-property :tags hl)
            :started (org-element-property :STARTED hl)
            :ended (org-element-property :ENDED hl)
            :project (org-element-property :PROJECT hl)
            :begin (org-element-property :begin hl)))))

(defun doing--parse-file (path)
  "Return list of entries from file at PATH.
Returns nil if file doesn't exist.
Each entry is a plist with entry data."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (org-mode)
      (doing--parse-buffer))))

;;; Entry Serialization Utilities

(defun doing--entry-to-org (entry)
  "Convert ENTRY plist to Org headline string.
ENTRY should be a plist with keys: :id, :title, :tags, :started,
:ended, :project, :body (optional).
Returns a formatted Org headline with properties drawer."
  (let ((title (plist-get entry :title))
        (tags (plist-get entry :tags))
        (id (plist-get entry :id))
        (started (plist-get entry :started))
        (ended (plist-get entry :ended))
        (project (plist-get entry :project))
        (body (plist-get entry :body)))
    (concat
     "* " title
     (when tags
       (concat " :" (mapconcat #'identity tags ":") ":"))
     "\n"
     ":PROPERTIES:\n"
     (format ":ID:       %s\n" id)
     (format ":STARTED:  %s\n" started)
     (when ended (format ":ENDED:    %s\n" ended))
     (when ended
       (format ":DURATION: %s\n"
               (doing--duration-format
                (doing--duration-minutes started ended))))
     (when project (format ":PROJECT:  %s\n" project))
     ":END:\n"
     (when body (concat body "\n")))))

(defun doing--append-entry-to-file (entry path)
  "Append ENTRY to file at PATH.
ENTRY should be a plist suitable for `doing--entry-to-org'.
Creates the file with proper Org header if it doesn't exist."
  (doing--ensure-file path)
  (with-temp-buffer
    (insert (doing--entry-to-org entry))
    (append-to-file (point-min) (point-max) path)))

;;; Entry Modification Utilities

(defun doing--goto-entry (id &optional file)
  "Go to entry with ID in FILE (default today.org).
Returns point if entry is found, nil otherwise.
Leaves point at the beginning of the entry headline.
Must be called within the buffer containing the entry."
  (let ((file (or file (doing--file-today))))
    (when (file-exists-p file)
      (goto-char (point-min))
      (when (re-search-forward
             (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id))
             nil t)
        (org-back-to-heading t)
        (point)))))

(defun doing--update-entry-property (id property value &optional file)
  "Set PROPERTY to VALUE for entry with ID in FILE.
FILE defaults to today.org if not specified.
Uses `org-entry-put' to update the property.
Returns t if successful, nil if entry not found."
  (let ((file (or file (doing--file-today))))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (when (doing--goto-entry id file)
            (org-entry-put nil property value)
            (save-buffer)
            t))))))

(defun doing--delete-entry (id &optional file)
  "Delete entry with ID from FILE.
FILE defaults to today.org if not specified.
Uses `org-cut-subtree' to remove the entire entry.
Returns t if successful, nil if entry not found."
  (let ((file (or file (doing--file-today))))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (when (doing--goto-entry id file)
            (org-cut-subtree)
            (save-buffer)
            t))))))

(provide 'doing-lib)

;;; doing-lib.el ends here
