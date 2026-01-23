;;; doing-test.el --- Tests for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the doing package using ERT.

;;; Code:

(require 'ert)
(require 'doing)
(require 'doing-lib)

;;; Phase 1: Package skeleton and configuration tests

(ert-deftest doing-test-customization-group-exists ()
  "Test that the `doing' customization group is defined."
  (should (get 'doing 'custom-group)))

(ert-deftest doing-test-directory-default ()
  "Test that `doing-directory' has a sensible default value."
  (should (boundp 'doing-directory))
  (should (stringp doing-directory))
  ;; Should expand to an absolute path containing "doing"
  (should (string-match-p "doing" doing-directory))
  ;; Should be an absolute path
  (should (file-name-absolute-p doing-directory)))

(ert-deftest doing-test-feature-provided ()
  "Test that the `doing' feature is provided."
  (should (featurep 'doing)))

(ert-deftest doing-test-internal-config-defined ()
  "Test that internal configuration constants are defined."
  (should (boundp 'doing--file-today-name))
  (should (boundp 'doing--file-week-name))
  (should (boundp 'doing--archive-directory-name))
  (should (string= doing--file-today-name "today.org"))
  (should (string= doing--file-week-name "week.org"))
  (should (string= doing--archive-directory-name "archive")))

;;; Phase 2: File and directory utilities tests

(ert-deftest doing-test-directory-creation ()
  "Test that `doing--directory' creates the directory structure."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Call doing--directory to ensure it exists
          (should (equal (doing--directory) (expand-file-name temp-dir)))
          ;; Verify main directory exists
          (should (file-exists-p temp-dir))
          ;; Verify archive subdirectory exists
          (should (file-exists-p
                   (expand-file-name doing--archive-directory-name temp-dir))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-file-paths ()
  "Test that file path functions return correct paths."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Test today file path
          (should (string= (doing--file-today)
                          (expand-file-name "today.org" temp-dir)))
          ;; Test week file path
          (should (string= (doing--file-week)
                          (expand-file-name "week.org" temp-dir)))
          ;; Test archive file path
          (should (string= (doing--file-archive 2026 4)
                          (expand-file-name "archive/2026-W04.org" temp-dir)))
          ;; Test archive file path with single-digit week
          (should (string= (doing--file-archive 2026 1)
                          (expand-file-name "archive/2026-W01.org" temp-dir))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-ensure-file-creates-with-header ()
  "Test that `doing--ensure-file' creates file with proper Org header."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir)
         (test-file (expand-file-name "test.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create file with custom title
          (doing--ensure-file test-file "Test Title")
          (should (file-exists-p test-file))
          ;; Verify content
          (with-temp-buffer
            (insert-file-contents test-file)
            (should (string-match-p "^#\\+TITLE: Test Title$" (buffer-string))))
          ;; Delete file and recreate without title
          (delete-file test-file)
          (doing--ensure-file test-file)
          (should (file-exists-p test-file))
          ;; Verify default title is derived from filename
          (with-temp-buffer
            (insert-file-contents test-file)
            (should (string-match-p "^#\\+TITLE: test$" (buffer-string))))
          ;; Test that calling again doesn't overwrite
          (with-temp-buffer
            (insert "existing content\n")
            (write-region (point-min) (point-max) test-file))
          (doing--ensure-file test-file "Should Not Change")
          (with-temp-buffer
            (insert-file-contents test-file)
            (should (string= (buffer-string) "existing content\n"))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-ensure-directory ()
  "Test that `doing--ensure-directory' creates required directories."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    ;; Delete the temp dir first to test creation from scratch
    (delete-directory temp-dir t)
    (unwind-protect
        (progn
          ;; Ensure directories are created
          (doing--ensure-directory)
          (should (file-exists-p temp-dir))
          (should (file-directory-p temp-dir))
          (should (file-exists-p
                   (expand-file-name doing--archive-directory-name temp-dir)))
          (should (file-directory-p
                   (expand-file-name doing--archive-directory-name temp-dir))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Phase 3: Timestamp and duration utilities tests

(ert-deftest doing-test-timestamp-now-format ()
  "Test that `doing--timestamp-now' returns properly formatted timestamp."
  (let ((ts (doing--timestamp-now)))
    ;; Should be a string
    (should (stringp ts))
    ;; Should match Org inactive timestamp format [YYYY-MM-DD DDD HH:MM]
    (should (string-match-p "^\\[20[0-9][0-9]-[0-1][0-9]-[0-3][0-9] [A-Z][a-z][a-z] [0-2][0-9]:[0-5][0-9]\\]$" ts))
    ;; Should start with [
    (should (string-prefix-p "[" ts))
    ;; Should end with ]
    (should (string-suffix-p "]" ts))))

(ert-deftest doing-test-timestamp-date-extraction ()
  "Test that `doing--timestamp-date' extracts date correctly."
  (should (string= (doing--timestamp-date "[2026-01-23 Thu 14:30]")
                   "2026-01-23"))
  (should (string= (doing--timestamp-date "[2025-12-31 Wed 23:59]")
                   "2025-12-31"))
  (should (string= (doing--timestamp-date "[2026-01-01 Thu 00:00]")
                   "2026-01-01")))

(ert-deftest doing-test-duration-calculation ()
  "Test that `doing--duration-minutes' calculates duration correctly."
  ;; 1 hour = 60 minutes
  (should (= (doing--duration-minutes
              "[2026-01-23 Thu 10:00]"
              "[2026-01-23 Thu 11:00]")
             60.0))
  ;; 45 minutes
  (should (= (doing--duration-minutes
              "[2026-01-23 Thu 10:00]"
              "[2026-01-23 Thu 10:45]")
             45.0))
  ;; 2.5 hours = 150 minutes
  (should (= (doing--duration-minutes
              "[2026-01-23 Thu 09:30]"
              "[2026-01-23 Thu 12:00]")
             150.0))
  ;; Cross-day duration
  (should (= (doing--duration-minutes
              "[2026-01-23 Thu 23:30]"
              "[2026-01-24 Fri 00:30]")
             60.0)))

(ert-deftest doing-test-duration-format ()
  "Test that `doing--duration-format' formats minutes correctly."
  ;; Note: org-duration-from-minutes format depends on org-duration-format
  ;; Default format is typically H:MM
  (let ((formatted-60 (doing--duration-format 60.0))
        (formatted-45 (doing--duration-format 45.0))
        (formatted-150 (doing--duration-format 150.0)))
    ;; All should be strings
    (should (stringp formatted-60))
    (should (stringp formatted-45))
    (should (stringp formatted-150))
    ;; Basic sanity checks - exact format depends on org-duration-format
    ;; but should contain time components
    (should (string-match-p "[0-9]" formatted-60))
    (should (string-match-p "[0-9]" formatted-45))
    (should (string-match-p "[0-9]" formatted-150))))

(ert-deftest doing-test-iso-week-string ()
  "Test that `doing--iso-week-string' returns properly formatted week string."
  ;; Create a known time: 2026-01-23 is in week 4 of 2026
  (let* ((time (encode-time 0 0 12 23 1 2026))  ; 2026-01-23 12:00:00
         (week-str (doing--iso-week-string time)))
    ;; Should match YYYY-WNN format
    (should (string-match-p "^20[0-9][0-9]-W[0-9][0-9]$" week-str))
    ;; For 2026-01-23, should be week 04
    (should (string= week-str "2026-W04")))

  ;; Test first week of year
  (let* ((time (encode-time 0 0 12 5 1 2026))  ; 2026-01-05 is in W02
         (week-str (doing--iso-week-string time)))
    (should (string= week-str "2026-W02")))

  ;; Test without argument (current time) - should return valid format
  (let ((week-str (doing--iso-week-string)))
    (should (stringp week-str))
    (should (string-match-p "^20[0-9][0-9]-W[0-9][0-9]$" week-str))))

(ert-deftest doing-test-iso-week ()
  "Test that `doing--iso-week' returns (YEAR WEEK) list correctly."
  ;; Create a known time: 2026-01-23 is in week 4 of 2026
  (let* ((time (encode-time 0 0 12 23 1 2026))
         (week-data (doing--iso-week time)))
    ;; Should return a list
    (should (listp week-data))
    ;; Should have 2 elements
    (should (= (length week-data) 2))
    ;; Both should be integers
    (should (integerp (car week-data)))
    (should (integerp (cadr week-data)))
    ;; Should be 2026 and 4
    (should (= (car week-data) 2026))
    (should (= (cadr week-data) 4)))

  ;; Test without argument (current time)
  (let ((week-data (doing--iso-week)))
    (should (listp week-data))
    (should (= (length week-data) 2))
    (should (integerp (car week-data)))
    (should (integerp (cadr week-data)))
    ;; Year should be reasonable (between 2020 and 2100)
    (should (>= (car week-data) 2020))
    (should (<= (car week-data) 2100))
    ;; Week should be between 1 and 53
    (should (>= (cadr week-data) 1))
    (should (<= (cadr week-data) 53))))

(ert-deftest doing-test-timestamp-to-time ()
  "Test that `doing--timestamp-to-time' parses timestamps correctly."
  (let ((parsed (doing--timestamp-to-time "[2026-01-23 Thu 14:30]")))
    ;; Should return a time structure (list)
    (should (listp parsed))
    ;; Should have 9 elements (org-parse-time-string returns decoded time)
    (should (= (length parsed) 9))
    ;; Minute should be 30
    (should (= (nth 1 parsed) 30))
    ;; Hour should be 14
    (should (= (nth 2 parsed) 14))
    ;; Day should be 23
    (should (= (nth 3 parsed) 23))
    ;; Month should be 1 (January)
    (should (= (nth 4 parsed) 1))
    ;; Year should be 2026
    (should (= (nth 5 parsed) 2026))))

;;; doing-test.el ends here
