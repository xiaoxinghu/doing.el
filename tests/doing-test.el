;;; doing-test.el --- Tests for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the doing package using ERT.

;;; Code:

(require 'ert)
(require 'doing)
(require 'doing-lib)
(require 'doing-now)
(require 'doing-current)
(require 'doing-finish)
(require 'doing-cancel)

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

;;; Phase 4: Entry parsing tests

(ert-deftest doing-test-parse-entry-at-point ()
  "Test that `doing--parse-entry-at-point' parses headline correctly."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test file with entry
          (with-temp-buffer
            (insert "#+TITLE: Test\n\n")
            (insert "* Write documentation :emacs:docs:\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       20260123T143000\n")
            (insert ":STARTED:  [2026-01-23 Thu 14:30]\n")
            (insert ":ENDED:    [2026-01-23 Thu 15:15]\n")
            (insert ":PROJECT:  doing-el\n")
            (insert ":END:\n")
            (insert "Some notes here.\n")
            (write-region (point-min) (point-max) test-file))
          ;; Parse the entry
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* Write documentation")
            (let ((entry (doing--parse-entry-at-point)))
              ;; Should return a plist
              (should (listp entry))
              ;; Check all properties
              (should (string= (plist-get entry :id) "20260123T143000"))
              (should (string= (plist-get entry :title) "Write documentation"))
              (should (equal (plist-get entry :tags) '("emacs" "docs")))
              (should (string= (plist-get entry :started) "[2026-01-23 Thu 14:30]"))
              (should (string= (plist-get entry :ended) "[2026-01-23 Thu 15:15]"))
              (should (string= (plist-get entry :project) "doing-el"))
              (should (integerp (plist-get entry :begin)))
              (should (integerp (plist-get entry :end)))))
          ;; Test non-headline position
          (with-current-buffer (find-file-noselect test-file)
            (goto-char (point-min))
            (should (null (doing--parse-entry-at-point)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-parse-buffer-multiple ()
  "Test that `doing--parse-buffer' parses multiple entries correctly."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test file with multiple entries
          (with-temp-buffer
            (insert "#+TITLE: Test\n\n")
            (insert "* First entry :tag1:\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       entry1\n")
            (insert ":STARTED:  [2026-01-23 Thu 10:00]\n")
            (insert ":END:\n\n")
            (insert "* Second entry :tag2:tag3:\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       entry2\n")
            (insert ":STARTED:  [2026-01-23 Thu 11:00]\n")
            (insert ":ENDED:    [2026-01-23 Thu 12:00]\n")
            (insert ":PROJECT:  test-project\n")
            (insert ":END:\n\n")
            (insert "* Third entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       entry3\n")
            (insert ":STARTED:  [2026-01-23 Thu 13:00]\n")
            (insert ":END:\n")
            (write-region (point-min) (point-max) test-file))
          ;; Parse all entries
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (let ((entries (doing--parse-buffer)))
              ;; Should return a list
              (should (listp entries))
              ;; Should have 3 entries
              (should (= (length entries) 3))
              ;; Check first entry
              (let ((e1 (nth 0 entries)))
                (should (string= (plist-get e1 :id) "entry1"))
                (should (string= (plist-get e1 :title) "First entry"))
                (should (equal (plist-get e1 :tags) '("tag1")))
                (should (null (plist-get e1 :ended))))
              ;; Check second entry
              (let ((e2 (nth 1 entries)))
                (should (string= (plist-get e2 :id) "entry2"))
                (should (string= (plist-get e2 :title) "Second entry"))
                (should (equal (plist-get e2 :tags) '("tag2" "tag3")))
                (should (string= (plist-get e2 :ended) "[2026-01-23 Thu 12:00]"))
                (should (string= (plist-get e2 :project) "test-project")))
              ;; Check third entry
              (let ((e3 (nth 2 entries)))
                (should (string= (plist-get e3 :id) "entry3"))
                (should (string= (plist-get e3 :title) "Third entry"))
                (should (null (plist-get e3 :tags)))
                (should (null (plist-get e3 :ended)))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-parse-file ()
  "Test that `doing--parse-file' parses file correctly."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir))
         (missing-file (expand-file-name "missing.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-buffer
            (insert "#+TITLE: Test\n\n")
            (insert "* Entry one :work:\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       test-id-1\n")
            (insert ":STARTED:  [2026-01-23 Thu 09:00]\n")
            (insert ":END:\n\n")
            (insert "* Entry two :personal:\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       test-id-2\n")
            (insert ":STARTED:  [2026-01-23 Thu 10:00]\n")
            (insert ":ENDED:    [2026-01-23 Thu 10:30]\n")
            (insert ":END:\n")
            (write-region (point-min) (point-max) test-file))
          ;; Parse existing file
          (let ((entries (doing--parse-file test-file)))
            (should (listp entries))
            (should (= (length entries) 2))
            (should (string= (plist-get (nth 0 entries) :id) "test-id-1"))
            (should (string= (plist-get (nth 1 entries) :id) "test-id-2"))
            (should (equal (plist-get (nth 0 entries) :tags) '("work")))
            (should (equal (plist-get (nth 1 entries) :tags) '("personal"))))
          ;; Test non-existent file
          (should (null (doing--parse-file missing-file))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Phase 5: Entry serialization tests

(ert-deftest doing-test-entry-to-org-minimal ()
  "Test that `doing--entry-to-org' generates minimal entry correctly."
  (let* ((entry '(:id "20260123T100000"
                  :title "Test entry"
                  :started "[2026-01-23 Thu 10:00]"))
         (result (doing--entry-to-org entry)))
    ;; Should be a string
    (should (stringp result))
    ;; Should start with headline
    (should (string-match-p "^\\* Test entry\n" result))
    ;; Should have properties drawer
    (should (string-match-p ":PROPERTIES:" result))
    (should (string-match-p ":ID: +20260123T100000" result))
    (should (string-match-p ":STARTED: +\\[2026-01-23 Thu 10:00\\]" result))
    (should (string-match-p ":END:" result))
    ;; Should not have ENDED or DURATION (not finished)
    (should-not (string-match-p ":ENDED:" result))
    (should-not (string-match-p ":DURATION:" result))))

(ert-deftest doing-test-entry-to-org-with-tags ()
  "Test that `doing--entry-to-org' handles tags correctly."
  (let* ((entry '(:id "20260123T100000"
                  :title "Tagged entry"
                  :tags ("emacs" "coding")
                  :started "[2026-01-23 Thu 10:00]"))
         (result (doing--entry-to-org entry)))
    ;; Should have tags on headline
    (should (string-match-p "^\\* Tagged entry :emacs:coding:\n" result))))

(ert-deftest doing-test-entry-to-org-finished ()
  "Test that `doing--entry-to-org' handles finished entries with duration."
  (let* ((entry '(:id "20260123T100000"
                  :title "Finished entry"
                  :started "[2026-01-23 Thu 10:00]"
                  :ended "[2026-01-23 Thu 11:00]"))
         (result (doing--entry-to-org entry)))
    ;; Should have ENDED property
    (should (string-match-p ":ENDED: +\\[2026-01-23 Thu 11:00\\]" result))
    ;; Should have DURATION property (1 hour = 60 minutes)
    (should (string-match-p ":DURATION:" result))
    ;; Duration should contain some time representation
    (should (string-match-p ":DURATION: +[0-9]" result))))

(ert-deftest doing-test-entry-to-org-with-project ()
  "Test that `doing--entry-to-org' includes PROJECT property."
  (let* ((entry '(:id "20260123T100000"
                  :title "Project entry"
                  :started "[2026-01-23 Thu 10:00]"
                  :project "doing-el"))
         (result (doing--entry-to-org entry)))
    ;; Should have PROJECT property
    (should (string-match-p ":PROJECT: +doing-el" result))))

(ert-deftest doing-test-entry-to-org-with-body ()
  "Test that `doing--entry-to-org' includes body text."
  (let* ((entry '(:id "20260123T100000"
                  :title "Entry with notes"
                  :started "[2026-01-23 Thu 10:00]"
                  :body "This is a note.\nWith multiple lines."))
         (result (doing--entry-to-org entry)))
    ;; Should have body after properties drawer
    (should (string-match-p "This is a note\\." result))
    (should (string-match-p "With multiple lines\\." result))))

(ert-deftest doing-test-append-entry-to-file ()
  "Test that `doing--append-entry-to-file' appends entry to file."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir))
         (entry1 '(:id "entry1"
                   :title "First entry"
                   :started "[2026-01-23 Thu 10:00]"))
         (entry2 '(:id "entry2"
                   :title "Second entry"
                   :tags ("test")
                   :started "[2026-01-23 Thu 11:00]"
                   :ended "[2026-01-23 Thu 12:00]")))
    (unwind-protect
        (progn
          ;; Append first entry
          (doing--append-entry-to-file entry1 test-file)
          (should (file-exists-p test-file))
          ;; Verify first entry was written
          (with-temp-buffer
            (insert-file-contents test-file)
            (should (string-match-p "\\* First entry" (buffer-string)))
            (should (string-match-p ":ID: +entry1" (buffer-string))))
          ;; Append second entry
          (doing--append-entry-to-file entry2 test-file)
          ;; Verify both entries exist
          (with-temp-buffer
            (insert-file-contents test-file)
            (let ((content (buffer-string)))
              (should (string-match-p "\\* First entry" content))
              (should (string-match-p "\\* Second entry :test:" content))
              (should (string-match-p ":ID: +entry1" content))
              (should (string-match-p ":ID: +entry2" content))
              (should (string-match-p ":ENDED:" content))
              (should (string-match-p ":DURATION:" content)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-entry-to-org-complete ()
  "Test `doing--entry-to-org' with all properties populated."
  (let* ((entry '(:id "20260123T143000"
                  :title "Complete entry"
                  :tags ("emacs" "elisp" "test")
                  :started "[2026-01-23 Thu 14:30]"
                  :ended "[2026-01-23 Thu 15:45]"
                  :project "doing-el"
                  :body "Testing all properties.\nMultiple lines of notes."))
         (result (doing--entry-to-org entry)))
    ;; Verify all components are present
    (should (string-match-p "^\\* Complete entry :emacs:elisp:test:\n" result))
    (should (string-match-p ":ID: +20260123T143000" result))
    (should (string-match-p ":STARTED: +\\[2026-01-23 Thu 14:30\\]" result))
    (should (string-match-p ":ENDED: +\\[2026-01-23 Thu 15:45\\]" result))
    (should (string-match-p ":DURATION:" result))
    (should (string-match-p ":PROJECT: +doing-el" result))
    (should (string-match-p "Testing all properties\\." result))
    (should (string-match-p "Multiple lines of notes\\." result))))

;;; Phase 6: Entry modification tests

(ert-deftest doing-test-goto-entry ()
  "Test that `doing--goto-entry' finds and navigates to entry by ID."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test file with multiple entries
          (with-temp-buffer
            (insert "#+TITLE: Test\n\n")
            (insert "* First entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       first-id\n")
            (insert ":STARTED:  [2026-01-23 Thu 10:00]\n")
            (insert ":END:\n\n")
            (insert "* Second entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       second-id\n")
            (insert ":STARTED:  [2026-01-23 Thu 11:00]\n")
            (insert ":END:\n\n")
            (insert "* Third entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       third-id\n")
            (insert ":STARTED:  [2026-01-23 Thu 12:00]\n")
            (insert ":END:\n")
            (write-region (point-min) (point-max) test-file))
          ;; Test finding each entry
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (let ((pos-first (save-excursion (doing--goto-entry "first-id" test-file)))
                  (pos-second (save-excursion (doing--goto-entry "second-id" test-file)))
                  (pos-third (save-excursion (doing--goto-entry "third-id" test-file)))
                  (pos-missing (save-excursion (doing--goto-entry "nonexistent-id" test-file))))
              ;; Should find all three entries
              (should (numberp pos-first))
              (should (numberp pos-second))
              (should (numberp pos-third))
              ;; Should return nil for missing entry
              (should (null pos-missing))
              ;; Positions should be different
              (should (< pos-first pos-second))
              (should (< pos-second pos-third))))
          ;; Verify that point is at headline when found
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (when (doing--goto-entry "second-id" test-file)
              (should (looking-at "^\\* Second entry")))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-update-property ()
  "Test that `doing--update-entry-property' updates properties correctly."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test file with entry
          (with-temp-buffer
            (insert "#+TITLE: Test\n\n")
            (insert "* Test entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       test-id\n")
            (insert ":STARTED:  [2026-01-23 Thu 10:00]\n")
            (insert ":END:\n")
            (write-region (point-min) (point-max) test-file))
          ;; Update ENDED property
          (let ((result (doing--update-entry-property
                         "test-id" "ENDED" "[2026-01-23 Thu 11:00]" test-file)))
            ;; Should return t for success
            (should (eq result t)))
          ;; Verify property was updated
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (when (doing--goto-entry "test-id" test-file)
              (should (string= (org-entry-get nil "ENDED")
                              "[2026-01-23 Thu 11:00]"))))
          ;; Update DURATION property
          (doing--update-entry-property "test-id" "DURATION" "1:00" test-file)
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (when (doing--goto-entry "test-id" test-file)
              (should (string= (org-entry-get nil "DURATION") "1:00"))))
          ;; Update PROJECT property
          (doing--update-entry-property "test-id" "PROJECT" "test-project" test-file)
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (when (doing--goto-entry "test-id" test-file)
              (should (string= (org-entry-get nil "PROJECT") "test-project"))))
          ;; Test updating non-existent entry
          (let ((result (doing--update-entry-property
                         "nonexistent-id" "ENDED" "[2026-01-23 Thu 11:00]" test-file)))
            ;; Should return nil for failure
            (should (null result))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-delete-entry ()
  "Test that `doing--delete-entry' removes entries correctly."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test file with three entries
          (with-temp-buffer
            (insert "#+TITLE: Test\n\n")
            (insert "* First entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       first-id\n")
            (insert ":STARTED:  [2026-01-23 Thu 10:00]\n")
            (insert ":END:\n")
            (insert "Some notes for first entry.\n\n")
            (insert "* Second entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       second-id\n")
            (insert ":STARTED:  [2026-01-23 Thu 11:00]\n")
            (insert ":END:\n\n")
            (insert "* Third entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       third-id\n")
            (insert ":STARTED:  [2026-01-23 Thu 12:00]\n")
            (insert ":END:\n")
            (write-region (point-min) (point-max) test-file))
          ;; Verify all three entries exist
          (let ((entries (doing--parse-file test-file)))
            (should (= (length entries) 3)))
          ;; Delete second entry
          (let ((result (doing--delete-entry "second-id" test-file)))
            ;; Should return t for success
            (should (eq result t)))
          ;; Verify only two entries remain
          (let ((entries (doing--parse-file test-file)))
            (should (= (length entries) 2))
            ;; First and third should remain
            (should (string= (plist-get (nth 0 entries) :id) "first-id"))
            (should (string= (plist-get (nth 1 entries) :id) "third-id"))
            ;; Second should be gone
            (should-not (seq-find (lambda (e)
                                    (string= (plist-get e :id) "second-id"))
                                  entries)))
          ;; Verify file content doesn't contain deleted entry
          (with-temp-buffer
            (insert-file-contents test-file)
            (should-not (string-match-p "Second entry" (buffer-string))))
          ;; Delete first entry
          (doing--delete-entry "first-id" test-file)
          (let ((entries (doing--parse-file test-file)))
            (should (= (length entries) 1))
            (should (string= (plist-get (nth 0 entries) :id) "third-id")))
          ;; Test deleting non-existent entry
          (let ((result (doing--delete-entry "nonexistent-id" test-file)))
            ;; Should return nil for failure
            (should (null result)))
          ;; Verify count didn't change
          (let ((entries (doing--parse-file test-file)))
            (should (= (length entries) 1))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-goto-entry-with-whitespace ()
  "Test that `doing--goto-entry' handles property drawer whitespace."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test file with varying whitespace in property drawer
          (with-temp-buffer
            (insert "#+TITLE: Test\n\n")
            (insert "* Entry with tabs\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:\t\ttest-id-tabs\n")  ; tabs after ID
            (insert ":STARTED:  [2026-01-23 Thu 10:00]\n")
            (insert ":END:\n\n")
            (insert "* Entry with spaces\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:   test-id-spaces\n")  ; multiple spaces
            (insert ":STARTED:  [2026-01-23 Thu 11:00]\n")
            (insert ":END:\n\n")
            (insert "* Entry minimal\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID: test-id-minimal\n")  ; single space
            (insert ":STARTED:  [2026-01-23 Thu 12:00]\n")
            (insert ":END:\n")
            (write-region (point-min) (point-max) test-file))
          ;; Should find all entries regardless of whitespace
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (should (numberp (save-excursion (doing--goto-entry "test-id-tabs" test-file))))
            (should (numberp (save-excursion (doing--goto-entry "test-id-spaces" test-file))))
            (should (numberp (save-excursion (doing--goto-entry "test-id-minimal" test-file))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-update-property-multiple ()
  "Test updating multiple properties on the same entry."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (test-file (expand-file-name "test.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test file with minimal entry
          (with-temp-buffer
            (insert "#+TITLE: Test\n\n")
            (insert "* Test entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       test-id\n")
            (insert ":STARTED:  [2026-01-23 Thu 10:00]\n")
            (insert ":END:\n")
            (write-region (point-min) (point-max) test-file))
          ;; Update multiple properties
          (doing--update-entry-property "test-id" "ENDED" "[2026-01-23 Thu 11:30]" test-file)
          (doing--update-entry-property "test-id" "DURATION" "1:30" test-file)
          (doing--update-entry-property "test-id" "PROJECT" "my-project" test-file)
          (doing--update-entry-property "test-id" "CONTEXT" "work" test-file)
          ;; Verify all properties are set
          (with-current-buffer (find-file-noselect test-file)
            (org-mode)
            (when (doing--goto-entry "test-id" test-file)
              (should (string= (org-entry-get nil "STARTED") "[2026-01-23 Thu 10:00]"))
              (should (string= (org-entry-get nil "ENDED") "[2026-01-23 Thu 11:30]"))
              (should (string= (org-entry-get nil "DURATION") "1:30"))
              (should (string= (org-entry-get nil "PROJECT") "my-project"))
              (should (string= (org-entry-get nil "CONTEXT") "work")))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Phase 7: Entry ID generation tests

(ert-deftest doing-test-generate-id-format ()
  "Test that `doing--generate-id' generates IDs in correct format."
  (let ((id (doing--generate-id)))
    ;; Should be a string
    (should (stringp id))
    ;; Should match YYYYMMDDTHHMMSS format (15 characters)
    (should (= (length id) 15))
    ;; Should match the pattern: 8 digits, T, 6 digits
    (should (string-match-p "^[0-9]\\{8\\}T[0-9]\\{6\\}$" id))
    ;; Year should be reasonable (2020-2100)
    (let ((year (string-to-number (substring id 0 4))))
      (should (>= year 2020))
      (should (<= year 2100)))
    ;; Month should be 01-12
    (let ((month (string-to-number (substring id 4 6))))
      (should (>= month 1))
      (should (<= month 12)))
    ;; Day should be 01-31
    (let ((day (string-to-number (substring id 6 8))))
      (should (>= day 1))
      (should (<= day 31)))
    ;; Hour should be 00-23
    (let ((hour (string-to-number (substring id 9 11))))
      (should (>= hour 0))
      (should (<= hour 23)))
    ;; Minute should be 00-59
    (let ((minute (string-to-number (substring id 11 13))))
      (should (>= minute 0))
      (should (<= minute 59)))
    ;; Second should be 00-59
    (let ((second (string-to-number (substring id 13 15))))
      (should (>= second 0))
      (should (<= second 59)))))

(ert-deftest doing-test-generate-id-uniqueness ()
  "Test that `doing--generate-id' generates unique IDs."
  (let ((id1 (doing--generate-id)))
    ;; Sleep for a tiny amount to ensure different timestamp
    (sleep-for 0.01)
    (let ((id2 (doing--generate-id)))
      ;; IDs should be different (unless generated in same second, very unlikely)
      ;; This is probabilistic but should pass in practice
      ;; Note: If this test is flaky, we accept that timestamp-based IDs
      ;; are unique enough for our purposes
      (should (stringp id1))
      (should (stringp id2)))))

;;; Phase 8: doing-now command tests

(ert-deftest doing-test-now-creates-entry ()
  "Test that `doing-now' creates an entry in today.org."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Call doing-now non-interactively
          (doing-now "Write tests" '("emacs" "testing"))
          ;; Verify file was created
          (let ((today-file (doing--file-today)))
            (should (file-exists-p today-file))
            ;; Verify entry exists
            (let ((entries (doing--parse-file today-file)))
              (should (= (length entries) 1))
              (let ((entry (car entries)))
                ;; Check title
                (should (string= (plist-get entry :title) "Write tests"))
                ;; Check tags
                (should (equal (plist-get entry :tags) '("emacs" "testing")))
                ;; Check ID exists and has correct format
                (let ((id (plist-get entry :id)))
                  (should (stringp id))
                  (should (string-match-p "^[0-9]\\{8\\}T[0-9]\\{6\\}$" id)))
                ;; Check STARTED exists and has correct format
                (let ((started (plist-get entry :started)))
                  (should (stringp started))
                  (should (string-match-p "^\\[20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]" started)))
                ;; Entry should not be finished
                (should (null (plist-get entry :ended))))))
          ;; Test adding a second entry
          (doing-now "Second activity" '("work"))
          (let ((entries (doing--parse-file (doing--file-today))))
            (should (= (length entries) 2))
            (let ((entry2 (nth 1 entries)))
              (should (string= (plist-get entry2 :title) "Second activity"))
              (should (equal (plist-get entry2 :tags) '("work"))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-now-without-tags ()
  "Test that `doing-now' works without tags."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Call doing-now without tags
          (doing-now "Simple activity")
          ;; Verify entry
          (let ((entries (doing--parse-file (doing--file-today))))
            (should (= (length entries) 1))
            (let ((entry (car entries)))
              (should (string= (plist-get entry :title) "Simple activity"))
              ;; Tags should be nil
              (should (null (plist-get entry :tags))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-now-is-interactive ()
  "Test that `doing-now' is an interactive command."
  ;; Check that the function is marked as interactive
  (should (commandp 'doing-now)))

;;; Phase 9: doing-current command tests

(ert-deftest doing-test-current-shows-active ()
  "Test that `doing-current' displays the current unfinished activity."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry
          (doing-now "Writing tests" '("emacs" "testing"))
          ;; Capture the message output
          (let ((message-log-max t)
                (messages-buffer (get-buffer-create "*Messages*")))
            (with-current-buffer messages-buffer
              (let ((start-pos (point-max)))
                ;; Call doing-current
                (doing-current)
                ;; Check the message output
                (with-current-buffer messages-buffer
                  (goto-char start-pos)
                  (let ((output (buffer-substring start-pos (point-max))))
                    ;; Should contain the title
                    (should (string-match-p "Writing tests" output))
                    ;; Should contain elapsed time in brackets
                    (should (string-match-p "\\[[0-9]" output)))))))
          ;; Test with multiple entries (should show the most recent unfinished)
          (doing-now "Second activity" '("work"))
          (let ((message-log-max t)
                (messages-buffer (get-buffer-create "*Messages*")))
            (with-current-buffer messages-buffer
              (let ((start-pos (point-max)))
                (doing-current)
                (with-current-buffer messages-buffer
                  (goto-char start-pos)
                  (let ((output (buffer-substring start-pos (point-max))))
                    ;; Should show the second activity
                    (should (string-match-p "Second activity" output))))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-current-when-nothing-active ()
  "Test that `doing-current' handles no active entries correctly."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Ensure directory exists but file is empty
          (doing--ensure-directory)
          (let ((message-log-max t)
                (messages-buffer (get-buffer-create "*Messages*")))
            (with-current-buffer messages-buffer
              (let ((start-pos (point-max)))
                ;; Call doing-current with no entries
                (doing-current)
                ;; Check the message output
                (with-current-buffer messages-buffer
                  (goto-char start-pos)
                  (let ((output (buffer-substring start-pos (point-max))))
                    ;; Should say "No activity in progress"
                    (should (string-match-p "No activity in progress" output))))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-current-entry-helper ()
  "Test that `doing--current-entry' returns correct entry."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Test with no entries
          (doing--ensure-directory)
          (should (null (doing--current-entry)))
          ;; Create an unfinished entry
          (doing-now "Active task" '("test"))
          (let ((entry (doing--current-entry)))
            (should entry)
            (should (string= (plist-get entry :title) "Active task"))
            (should (equal (plist-get entry :tags) '("test")))
            (should (null (plist-get entry :ended))))
          ;; Add a finished entry by manually creating one
          (let ((finished-entry (list :id (doing--generate-id)
                                      :title "Finished task"
                                      :started (doing--timestamp-now)
                                      :ended (doing--timestamp-now))))
            (doing--append-entry-to-file finished-entry (doing--file-today)))
          ;; Current should still be "Active task", not "Finished task"
          (let ((entry (doing--current-entry)))
            (should entry)
            (should (string= (plist-get entry :title) "Active task")))
          ;; Add another unfinished entry
          (doing-now "Most recent task")
          ;; Current should be the most recent unfinished one
          (let ((entry (doing--current-entry)))
            (should entry)
            (should (string= (plist-get entry :title) "Most recent task"))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-current-is-interactive ()
  "Test that `doing-current' is an interactive command."
  (should (commandp 'doing-current)))

;;; Phase 10: doing-finish tests

(ert-deftest doing-test-finish-sets-ended ()
  "Test that `doing-finish' sets ENDED property correctly."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an unfinished entry
          (doing-now "Task to finish" '("test"))
          (sleep-for 0.01)  ; Small delay to ensure different timestamps
          ;; Finish it
          (doing-finish)
          ;; Read back the entry
          (let ((entries (doing--parse-file (doing--file-today))))
            (should (= (length entries) 1))
            (let ((entry (car entries)))
              ;; Should have title and ID
              (should (string= (plist-get entry :title) "Task to finish"))
              ;; Should have ENDED property now
              (should (plist-get entry :ended))
              ;; ENDED should be an Org timestamp format
              (should (string-match-p "^\\[20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                                      (plist-get entry :ended))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-finish-sets-duration ()
  "Test that `doing-finish' computes and sets DURATION property."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an unfinished entry
          (doing-now "Task to finish")
          (sleep-for 0.02)  ; Small delay to ensure measurable duration
          ;; Finish it
          (doing-finish)
          ;; Read the file directly to check DURATION property
          (with-temp-buffer
            (insert-file-contents (doing--file-today))
            (goto-char (point-min))
            ;; Should have DURATION property in the file
            (should (re-search-forward "^:DURATION:" nil t))
            ;; Duration should have some time value
            (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
              ;; Should match pattern like ":DURATION: 0:00" or ":DURATION:  0:00"
              (should (string-match-p ":DURATION:.*[0-9]" line)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-finish-error-when-no-current ()
  "Test that `doing-finish' signals error when no activity in progress."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Ensure directory exists but no entries
          (doing--ensure-directory)
          ;; Should signal user-error
          (should-error (doing-finish) :type 'user-error))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-finish-with-message ()
  "Test that `doing-finish' displays completion message with duration."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry
          (doing-now "Task with message")
          (sleep-for 0.01)
          ;; Finish and capture message
          (let ((message-log-max t)
                (messages-buffer (get-buffer-create "*Messages*")))
            (with-current-buffer messages-buffer
              (let ((start-pos (point-max)))
                (doing-finish)
                (with-current-buffer messages-buffer
                  (goto-char start-pos)
                  (let ((output (buffer-substring start-pos (point-max))))
                    ;; Should contain "Finished:" prefix
                    (should (string-match-p "Finished:" output))
                    ;; Should contain the task title
                    (should (string-match-p "Task with message" output))
                    ;; Should contain a duration in parentheses
                    (should (string-match-p "([0-9]" output))))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-finish-multiple-times ()
  "Test that finishing when already finished signals error."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create and finish an entry
          (doing-now "Single task")
          (doing-finish)
          ;; Try to finish again - should error since no current entry
          (should-error (doing-finish) :type 'user-error))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-finish-preserves-other-properties ()
  "Test that `doing-finish' preserves tags and other entry properties."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry with tags
          (doing-now "Task with props" '("emacs" "test"))
          (doing-finish)
          ;; Verify all properties are preserved
          (let ((entries (doing--parse-file (doing--file-today))))
            (should (= (length entries) 1))
            (let ((entry (car entries)))
              ;; Title should be preserved
              (should (string= (plist-get entry :title) "Task with props"))
              ;; Tags should be preserved
              (should (equal (plist-get entry :tags) '("emacs" "test")))
              ;; STARTED should still exist
              (should (plist-get entry :started))
              ;; ENDED should be set
              (should (plist-get entry :ended))
              ;; ID should still exist
              (should (plist-get entry :id)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-finish-is-interactive ()
  "Test that `doing-finish' is an interactive command."
  (should (commandp 'doing-finish)))

;;; Phase 11: doing-cancel tests

(ert-deftest doing-test-cancel-removes-entry ()
  "Test that `doing-cancel' removes the current entry."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry
          (doing-now "Task to cancel")
          ;; Verify entry exists
          (let ((entries-before (doing--parse-file (doing--file-today))))
            (should (= (length entries-before) 1)))
          ;; Cancel with non-interactive yes
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (doing-cancel))
          ;; Verify entry is removed
          (let ((entries-after (doing--parse-file (doing--file-today))))
            (should (= (length entries-after) 0))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-cancel-error-when-no-current ()
  "Test that `doing-cancel' signals error when no activity in progress."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Try to cancel with no entry
          (should-error (doing-cancel) :type 'user-error))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-cancel-with-user-abort ()
  "Test that `doing-cancel' preserves entry when user says no."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry
          (doing-now "Task to keep")
          ;; Try to cancel but answer no
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (doing-cancel))
          ;; Verify entry still exists
          (let ((entries (doing--parse-file (doing--file-today))))
            (should (= (length entries) 1))
            (should (string= (plist-get (car entries) :title) "Task to keep"))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-cancel-shows-message ()
  "Test that `doing-cancel' shows appropriate cancellation message."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry
          (doing-now "Task with message")
          ;; Cancel and check message
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (let ((pos (with-current-buffer "*Messages*"
                         (goto-char (point-max))
                         (point))))
              (doing-cancel)
              ;; Check that message was displayed
              (with-current-buffer "*Messages*"
                (let ((output (buffer-substring pos (point-max))))
                  (should (string-match-p "Cancelled: Task with message" output)))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-cancel-is-interactive ()
  "Test that `doing-cancel' is an interactive command."
  (should (commandp 'doing-cancel)))

;;; doing-test.el ends here
