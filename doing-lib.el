;;; doing-lib.el --- Internal utilities for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal helper functions for file/directory management, parsing,
;; and other shared functionality used across the doing package.
;;
;; All functions in this file use the `doing--` prefix (double hyphen)
;; to indicate they are internal and not part of the public API.

;;; Code:

(require 'org)

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

(provide 'doing-lib)

;;; doing-lib.el ends here
