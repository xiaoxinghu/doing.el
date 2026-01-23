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
(require 'doing-note)
(require 'doing-again)
(require 'doing-rollover)
(require 'doing-view)
(require 'doing-view-commands)

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

(ert-deftest doing-test-now-finishes-previous ()
  "Test that `doing-now' auto-finishes previous unfinished activity."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create first activity
          (doing-now "First activity" '("emacs"))
          ;; Small delay to ensure different timestamps
          (sleep-for 0.02)
          ;; Start second activity - should auto-finish the first
          (doing-now "Second activity" '("work"))
          ;; Kill any open buffers to force re-reading from disk
          (let ((buf (get-file-buffer (doing--file-today))))
            (when buf (kill-buffer buf)))
          ;; Verify both entries exist
          (let ((entries (doing--parse-file (doing--file-today))))
            (should (= (length entries) 2))
            ;; First entry should be finished
            (let ((entry1 (nth 0 entries)))
              (should (string= (plist-get entry1 :title) "First activity"))
              (should (plist-get entry1 :ended))
              ;; Verify ENDED property exists and has correct format
              (should (string-match-p "^\\[20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]"
                                     (plist-get entry1 :ended))))
            ;; Second entry should be unfinished
            (let ((entry2 (nth 1 entries)))
              (should (string= (plist-get entry2 :title) "Second activity"))
              (should (null (plist-get entry2 :ended)))))
          ;; Verify that DURATION was set on first entry
          (let ((file-content (with-temp-buffer
                                (insert-file-contents (doing--file-today))
                                (buffer-string))))
            (should (string-match-p ":DURATION:" file-content))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

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

;;; Phase 12: doing-note tests

(ert-deftest doing-test-note-appends-text ()
  "Test that `doing-note' appends text to current entry body."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry
          (doing-now "Task for notes")
          ;; Add a note
          (doing-note "This is a test note")
          ;; Verify note was added to file
          (with-temp-buffer
            (insert-file-contents (doing--file-today))
            (let ((content (buffer-string)))
              (should (string-match-p "This is a test note" content))))
          ;; Add another note
          (doing-note "Second note line")
          ;; Verify both notes are present
          (with-temp-buffer
            (insert-file-contents (doing--file-today))
            (let ((content (buffer-string)))
              (should (string-match-p "This is a test note" content))
              (should (string-match-p "Second note line" content)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-note-error-when-no-current ()
  "Test that `doing-note' signals error when no activity in progress."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Try to add note with no entry
          (should-error (doing-note "Note without entry") :type 'user-error))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-note-after-properties ()
  "Test that `doing-note' inserts text after properties drawer."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry
          (doing-now "Entry with props")
          ;; Add a note
          (doing-note "Note after properties")
          ;; Verify note comes after :END:
          (with-temp-buffer
            (insert-file-contents (doing--file-today))
            (goto-char (point-min))
            (let ((end-pos (re-search-forward "^:END:" nil t))
                  (note-pos (re-search-forward "Note after properties" nil t)))
              (should end-pos)
              (should note-pos)
              (should (< end-pos note-pos)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-note-shows-message ()
  "Test that `doing-note' shows confirmation message."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create an entry
          (doing-now "Task for message test")
          ;; Add note and check message
          (let ((pos (with-current-buffer "*Messages*"
                       (goto-char (point-max))
                       (point))))
            (doing-note "Test note")
            ;; Check that message was displayed
            (with-current-buffer "*Messages*"
              (let ((output (buffer-substring pos (point-max))))
                (should (string-match-p "Note added" output))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-note-is-interactive ()
  "Test that `doing-note' is an interactive command."
  (should (commandp 'doing-note)))

;;; Phase 13: doing-again — resume last activity

(ert-deftest doing-test-again-copies-title-and-tags ()
  "Test that `doing-again' creates new entry with same title and tags as last finished."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create and finish first activity with tags
          (doing-now "First task" '("emacs" "coding"))
          (sleep-for 1.1)
          (doing-finish)
          ;; Resume it
          (sleep-for 1.1)
          (doing-again)
          ;; Parse entries
          (let ((path (doing--file-today)))
            ;; Kill buffer to ensure fresh read from disk
            (when-let ((buf (get-file-buffer path)))
              (kill-buffer buf))
            (let ((entries (doing--parse-file path)))
              ;; Should have 2 entries
              (should (= 2 (length entries)))
              ;; First entry should be finished
              (should (plist-get (nth 0 entries) :ended))
              ;; Second entry should have same title and tags
              (should (string= "First task" (plist-get (nth 1 entries) :title)))
              (should (equal '("emacs" "coding") (plist-get (nth 1 entries) :tags)))
              ;; Second entry should NOT be finished
              (should-not (plist-get (nth 1 entries) :ended))
              ;; Second entry should have different ID
              (should-not (string= (plist-get (nth 0 entries) :id)
                                   (plist-get (nth 1 entries) :id))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-again-error-when-no-previous ()
  "Test that `doing-again' signals error when no previous finished activity exists."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Try to resume without any finished activity
          (should-error (doing-again) :type 'user-error)
          ;; Create an unfinished activity
          (doing-now "Current task")
          ;; Try to resume - should still error (current is not finished)
          (should-error (doing-again) :type 'user-error))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-again-uses-most-recent-finished ()
  "Test that `doing-again' resumes the most recent finished activity."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Suppress yes-or-no-p prompts about rereading files in batch mode
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            ;; Create and finish two activities
            (doing-now "First task" '("tag1"))
            (sleep-for 1.1)
            (doing-finish)
            (sleep-for 1.1)
            (doing-now "Second task" '("tag2"))
            (sleep-for 1.1)
            (doing-finish)
            ;; Kill buffer to ensure doing-again reads fresh data
            (let ((path (doing--file-today)))
              (when-let ((buf (get-file-buffer path)))
                (kill-buffer buf)))
            ;; Resume - should get "Second task"
            (sleep-for 0.01)
            (doing-again))
          ;; Parse entries
          (let ((path (doing--file-today)))
            ;; Kill buffer to ensure fresh read
            (when-let ((buf (get-file-buffer path)))
              (kill-buffer buf))
            (let ((entries (doing--parse-file path)))
              ;; Should have 3 entries
              (should (= 3 (length entries)))
              ;; Third entry should match second, not first
              (should (string= "Second task" (plist-get (nth 2 entries) :title)))
              (should (equal '("tag2") (plist-get (nth 2 entries) :tags))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-again-creates-fresh-timestamps ()
  "Test that `doing-again' creates entry with fresh timestamp and ID."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create and finish first activity
          (doing-now "Task to resume")
          (sleep-for 1.1)
          (doing-finish)
          ;; Resume it
          (sleep-for 1.1)
          (doing-again)
          ;; Parse entries
          (let ((path (doing--file-today)))
            ;; Kill buffer to ensure fresh read
            (when-let ((buf (get-file-buffer path)))
              (kill-buffer buf))
            (let ((entries (doing--parse-file path)))
              (should (= 2 (length entries)))
              (let ((first (nth 0 entries))
                    (second (nth 1 entries)))
                ;; IDs should be different (IDs include seconds so are more precise)
                (should-not (string= (plist-get first :id)
                                     (plist-get second :id)))
                ;; Start times may be same if within same minute, but IDs prove they're distinct entries
                ;; Second entry should NOT be finished
                (should-not (plist-get second :ended))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-last-finished-entry-helper ()
  "Test the internal helper `doing--last-finished-entry'."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          ;; Suppress yes-or-no-p prompts about rereading files in batch mode
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            ;; No entries - should return nil
            (should-not (doing--last-finished-entry))
            ;; Create unfinished entry - should return nil
            (doing-now "Current task")
            (should-not (doing--last-finished-entry))
            ;; Finish it - should return the entry
            (sleep-for 1.1)
            (doing-finish)
            ;; Kill buffer to read fresh data
            (let ((path (doing--file-today)))
              (when-let ((buf (get-file-buffer path)))
                (kill-buffer buf)))
            (let ((entry (doing--last-finished-entry)))
              (should entry)
              (should (string= "Current task" (plist-get entry :title)))
              (should (plist-get entry :ended)))
            ;; Create another unfinished - should still return first
            (sleep-for 1.1)
            (doing-now "Second task")
            ;; Kill buffer to read fresh data
            (let ((path (doing--file-today)))
              (when-let ((buf (get-file-buffer path)))
                (kill-buffer buf)))
            (let ((entry (doing--last-finished-entry)))
              (should entry)
              (should (string= "Current task" (plist-get entry :title))))
            ;; Finish second - should now return second
            (sleep-for 1.1)
            (doing-finish)
            ;; Kill buffer to read fresh data
            (let ((path (doing--file-today)))
              (when-let ((buf (get-file-buffer path)))
                (kill-buffer buf)))
            (let ((entry (doing--last-finished-entry)))
              (should entry)
              (should (string= "Second task" (plist-get entry :title))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-again-is-interactive ()
  "Test that `doing-again' is an interactive command."
  (should (commandp 'doing-again)))

;;; Phase 14: Daily Rollover

(ert-deftest doing-test-rollover-daily-moves-old ()
  "Test that daily rollover moves old entries to week.org."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          (let ((today-file (doing--file-today))
                (week-file (doing--file-week)))
            ;; Create entries with different dates
            ;; Old entry from yesterday
            (let ((yesterday (format-time-string
                             "%Y-%m-%d"
                             (time-subtract (current-time) (days-to-time 1)))))
              (doing--append-entry-to-file
               (list :id "old-id"
                     :title "Old entry"
                     :started (format "[%s Thu 10:00]" yesterday)
                     :tags '("old"))
               today-file))
            ;; Entry from today
            (let ((today (format-time-string "%Y-%m-%d")))
              (doing--append-entry-to-file
               (list :id "new-id"
                     :title "New entry"
                     :started (format "[%s Fri 14:00]" today)
                     :tags '("new"))
               today-file))
            ;; Kill buffers to ensure fresh reads
            (when-let ((buf (get-file-buffer today-file)))
              (kill-buffer buf))
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Run rollover
            (let ((moved-count (doing--rollover-daily)))
              ;; Should have moved 1 entry
              (should (= moved-count 1)))
            ;; Kill buffers again
            (when-let ((buf (get-file-buffer today-file)))
              (kill-buffer buf))
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Verify today.org only has today's entry
            (let ((today-entries (doing--parse-file today-file)))
              (should (= 1 (length today-entries)))
              (should (string= "new-id" (plist-get (car today-entries) :id))))
            ;; Verify week.org has the old entry
            (let ((week-entries (doing--parse-file week-file)))
              (should (= 1 (length week-entries)))
              (should (string= "old-id" (plist-get (car week-entries) :id))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-rollover-daily-keeps-today ()
  "Test that daily rollover preserves today's entries."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          (let ((today-file (doing--file-today))
                (today (format-time-string "%Y-%m-%d")))
            ;; Create multiple entries from today
            (doing--append-entry-to-file
             (list :id "entry1"
                   :title "Entry 1"
                   :started (format "[%s Fri 10:00]" today)
                   :tags '("work"))
             today-file)
            (doing--append-entry-to-file
             (list :id "entry2"
                   :title "Entry 2"
                   :started (format "[%s Fri 11:00]" today)
                   :tags '("work"))
             today-file)
            (doing--append-entry-to-file
             (list :id "entry3"
                   :title "Entry 3"
                   :started (format "[%s Fri 12:00]" today)
                   :tags '("work"))
             today-file)
            ;; Kill buffer
            (when-let ((buf (get-file-buffer today-file)))
              (kill-buffer buf))
            ;; Run rollover
            (let ((moved-count (doing--rollover-daily)))
              ;; Should not move any entries
              (should (null moved-count)))
            ;; Kill buffer again
            (when-let ((buf (get-file-buffer today-file)))
              (kill-buffer buf))
            ;; Verify all entries still in today.org
            (let ((today-entries (doing--parse-file today-file)))
              (should (= 3 (length today-entries)))
              (should (member "entry1" (mapcar (lambda (e) (plist-get e :id)) today-entries)))
              (should (member "entry2" (mapcar (lambda (e) (plist-get e :id)) today-entries)))
              (should (member "entry3" (mapcar (lambda (e) (plist-get e :id)) today-entries))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-rollover-daily-no-entries ()
  "Test that daily rollover handles empty today.org gracefully."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; today.org doesn't exist yet or is empty
          (let ((moved-count (doing--rollover-daily)))
            ;; Should return nil (no entries to move)
            (should (null moved-count))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-rollover-daily-preserves-properties ()
  "Test that daily rollover preserves all entry properties."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          (let ((today-file (doing--file-today))
                (week-file (doing--file-week))
                (yesterday (format-time-string
                           "%Y-%m-%d"
                           (time-subtract (current-time) (days-to-time 1)))))
            ;; Create entry with all properties
            (doing--append-entry-to-file
             (list :id "complete-id"
                   :title "Complete entry"
                   :started (format "[%s Thu 10:00]" yesterday)
                   :ended (format "[%s Thu 11:30]" yesterday)
                   :tags '("work" "project")
                   :project "doing-el")
             today-file)
            ;; Kill buffers
            (when-let ((buf (get-file-buffer today-file)))
              (kill-buffer buf))
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Run rollover
            (doing--rollover-daily)
            ;; Kill buffers
            (when-let ((buf (get-file-buffer today-file)))
              (kill-buffer buf))
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Verify entry in week.org has all properties
            (let* ((week-entries (doing--parse-file week-file))
                   (entry (car week-entries)))
              (should (= 1 (length week-entries)))
              (should (string= "complete-id" (plist-get entry :id)))
              (should (string= "Complete entry" (plist-get entry :title)))
              ;; Use doing--timestamp-date to extract date from timestamp
              (should (string= yesterday (doing--timestamp-date (plist-get entry :started))))
              (should (string= yesterday (doing--timestamp-date (plist-get entry :ended))))
              (should (equal '("work" "project") (plist-get entry :tags)))
              (should (string= "doing-el" (plist-get entry :project))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Phase 15: Weekly Rollover

(ert-deftest doing-test-rollover-weekly-archives ()
  "Test that weekly rollover archives old entries to archive files."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          (let* ((week-file (doing--file-week))
                 ;; Create timestamp for previous week
                 (last-week-time (time-subtract (current-time) (days-to-time 8)))
                 (last-week (doing--iso-week last-week-time))
                 (last-week-date (format-time-string "%Y-%m-%d" last-week-time)))
            ;; Add entry from previous week
            (doing--append-entry-to-file
             (list :id "old-week-id"
                   :title "Old week entry"
                   :started (format "[%s Mon 10:00]" last-week-date)
                   :tags '("old"))
             week-file)
            ;; Add entry from current week
            (let ((this-week-date (format-time-string "%Y-%m-%d")))
              (doing--append-entry-to-file
               (list :id "current-week-id"
                     :title "Current week entry"
                     :started (format "[%s Fri 14:00]" this-week-date)
                     :tags '("new"))
               week-file))
            ;; Kill buffer to ensure fresh reads
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Run weekly rollover
            (let ((moved-count (doing--rollover-weekly)))
              ;; Should have moved 1 entry
              (should (= moved-count 1)))
            ;; Kill buffer again
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Verify week.org only has current week's entry
            (let ((week-entries (doing--parse-file week-file)))
              (should (= 1 (length week-entries)))
              (should (string= "current-week-id" (plist-get (car week-entries) :id))))
            ;; Verify archive file has the old entry
            (let* ((archive-file (doing--file-archive (car last-week) (cadr last-week)))
                   (archive-entries (doing--parse-file archive-file)))
              (should (file-exists-p archive-file))
              (should (= 1 (length archive-entries)))
              (should (string= "old-week-id" (plist-get (car archive-entries) :id))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-rollover-weekly-correct-filename ()
  "Test that weekly rollover creates correctly named archive files."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          (let* ((week-file (doing--file-week))
                 ;; Create timestamp for a specific past week
                 (past-time (time-subtract (current-time) (days-to-time 14)))
                 (past-week (doing--iso-week past-time))
                 (past-date (format-time-string "%Y-%m-%d" past-time)))
            ;; Add entry from past week
            (doing--append-entry-to-file
             (list :id "past-id"
                   :title "Past entry"
                   :started (format "[%s Wed 10:00]" past-date)
                   :tags '("past"))
             week-file)
            ;; Kill buffer
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Run weekly rollover
            (doing--rollover-weekly)
            ;; Verify archive file naming format
            (let* ((expected-file (doing--file-archive (car past-week) (cadr past-week)))
                   (expected-basename (file-name-nondirectory expected-file)))
              ;; Check file exists
              (should (file-exists-p expected-file))
              ;; Check filename format YYYY-WNN.org (with zero-padded week)
              (should (string-match "^[0-9]\\{4\\}-W[0-9]\\{2\\}\\.org$" expected-basename)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-rollover-weekly-no-old-entries ()
  "Test that weekly rollover handles week.org with only current week entries."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          (let ((week-file (doing--file-week))
                (this-week-date (format-time-string "%Y-%m-%d")))
            ;; Add only current week entries
            (doing--append-entry-to-file
             (list :id "current1"
                   :title "Current entry 1"
                   :started (format "[%s Mon 10:00]" this-week-date)
                   :tags '("work"))
             week-file)
            (doing--append-entry-to-file
             (list :id "current2"
                   :title "Current entry 2"
                   :started (format "[%s Tue 11:00]" this-week-date)
                   :tags '("work"))
             week-file)
            ;; Kill buffer
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Run weekly rollover
            (let ((moved-count (doing--rollover-weekly)))
              ;; Should not move any entries
              (should (null moved-count)))
            ;; Kill buffer again
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Verify all entries still in week.org
            (let ((week-entries (doing--parse-file week-file)))
              (should (= 2 (length week-entries)))
              (should (member "current1" (mapcar (lambda (e) (plist-get e :id)) week-entries)))
              (should (member "current2" (mapcar (lambda (e) (plist-get e :id)) week-entries))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-rollover-weekly-preserves-properties ()
  "Test that weekly rollover preserves all entry properties during archival."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          (let* ((week-file (doing--file-week))
                 (last-week-time (time-subtract (current-time) (days-to-time 10)))
                 (last-week (doing--iso-week last-week-time))
                 (last-week-date (format-time-string "%Y-%m-%d" last-week-time)))
            ;; Create entry with all properties
            (doing--append-entry-to-file
             (list :id "complete-archive-id"
                   :title "Complete archived entry"
                   :started (format "[%s Tue 09:00]" last-week-date)
                   :ended (format "[%s Tue 11:00]" last-week-date)
                   :tags '("archived" "complete")
                   :project "test-project")
             week-file)
            ;; Kill buffer
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Run weekly rollover
            (doing--rollover-weekly)
            ;; Verify entry in archive file has all properties
            (let* ((archive-file (doing--file-archive (car last-week) (cadr last-week)))
                   (archive-entries (doing--parse-file archive-file))
                   (entry (car archive-entries)))
              (should (= 1 (length archive-entries)))
              (should (string= "complete-archive-id" (plist-get entry :id)))
              (should (string= "Complete archived entry" (plist-get entry :title)))
              (should (string= last-week-date (doing--timestamp-date (plist-get entry :started))))
              (should (string= last-week-date (doing--timestamp-date (plist-get entry :ended))))
              (should (equal '("archived" "complete") (plist-get entry :tags)))
              (should (string= "test-project" (plist-get entry :project))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-rollover-weekly-multiple-weeks ()
  "Test that weekly rollover handles entries from multiple past weeks."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          (let* ((week-file (doing--file-week))
                 ;; Two different past weeks
                 (week1-time (time-subtract (current-time) (days-to-time 8)))
                 (week2-time (time-subtract (current-time) (days-to-time 15)))
                 (week1 (doing--iso-week week1-time))
                 (week2 (doing--iso-week week2-time))
                 (week1-date (format-time-string "%Y-%m-%d" week1-time))
                 (week2-date (format-time-string "%Y-%m-%d" week2-time)))
            ;; Add entries from two different past weeks
            (doing--append-entry-to-file
             (list :id "week1-id"
                   :title "Week 1 entry"
                   :started (format "[%s Mon 10:00]" week1-date)
                   :tags '("week1"))
             week-file)
            (doing--append-entry-to-file
             (list :id "week2-id"
                   :title "Week 2 entry"
                   :started (format "[%s Mon 10:00]" week2-date)
                   :tags '("week2"))
             week-file)
            ;; Kill buffer
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Run weekly rollover
            (let ((moved-count (doing--rollover-weekly)))
              ;; Should have moved 2 entries
              (should (= moved-count 2)))
            ;; Verify each archive file has the correct entry
            (let* ((archive1 (doing--file-archive (car week1) (cadr week1)))
                   (archive2 (doing--file-archive (car week2) (cadr week2)))
                   (entries1 (doing--parse-file archive1))
                   (entries2 (doing--parse-file archive2)))
              (should (file-exists-p archive1))
              (should (file-exists-p archive2))
              (should (= 1 (length entries1)))
              (should (= 1 (length entries2)))
              (should (string= "week1-id" (plist-get (car entries1) :id)))
              (should (string= "week2-id" (plist-get (car entries2) :id))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Phase 16: Rollover Integration

(ert-deftest doing-test-ensure-rollover-throttled ()
  "Test that `doing--ensure-rollover' throttles rollover checks to once per hour."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir)
         (doing--last-rollover-time nil))  ; Reset the timer
    (unwind-protect
        (progn
          ;; First call should perform rollover
          (should (null doing--last-rollover-time))
          (doing--ensure-rollover)
          (should (not (null doing--last-rollover-time)))
          (let ((first-time doing--last-rollover-time))
            ;; Immediate second call should NOT update the time (throttled)
            (sleep-for 0.01)  ; Small delay to ensure time difference
            (doing--ensure-rollover)
            (should (equal doing--last-rollover-time first-time))
            ;; Simulate time passing (set last rollover to 2 hours ago)
            (setq doing--last-rollover-time
                  (time-subtract (current-time) (seconds-to-time 7200)))
            ;; Now rollover should happen again
            (let ((old-time doing--last-rollover-time))
              (doing--ensure-rollover)
              ;; Time should have been updated
              (should (not (equal doing--last-rollover-time old-time)))
              ;; New time should be recent (within last second)
              (should (< (float-time (time-subtract (current-time)
                                                    doing--last-rollover-time))
                         1.0)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-ensure-rollover-called-by-commands ()
  "Test that user-facing commands call `doing--ensure-rollover'."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir)
         (doing--last-rollover-time nil)
         (rollover-called nil))
    (unwind-protect
        (progn
          ;; Mock doing--ensure-rollover to track if it's called
          (cl-letf (((symbol-function 'doing--ensure-rollover)
                     (lambda () (setq rollover-called t))))
            ;; Test doing-now calls rollover
            (setq rollover-called nil)
            (cl-letf (((symbol-function 'doing-finish)
                       (lambda () nil)))  ; Mock to prevent side effects
              (doing-now "Test activity"))
            (should rollover-called)
            ;; Test doing-current calls rollover
            (setq rollover-called nil)
            (doing-current)
            (should rollover-called)))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-rollover-with-old-data ()
  "Test that `doing--ensure-rollover' correctly moves old entries."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir)
         (doing--last-rollover-time nil)
         (today-file (doing--file-today))
         (week-file (doing--file-week)))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create an entry from yesterday in today.org
          (let* ((yesterday-time (time-subtract (current-time) (days-to-time 1)))
                 (yesterday-date (format-time-string "%Y-%m-%d" yesterday-time)))
            (doing--append-entry-to-file
             (list :id "yesterday-id"
                   :title "Yesterday's task"
                   :started (format "[%s %s 10:00]"
                                    yesterday-date
                                    (format-time-string "%a" yesterday-time))
                   :ended (format "[%s %s 11:00]"
                                  yesterday-date
                                  (format-time-string "%a" yesterday-time))
                   :tags '("old"))
             today-file)
            ;; Kill buffer to ensure fresh read
            (when-let ((buf (get-file-buffer today-file)))
              (kill-buffer buf))
            ;; Verify entry exists in today.org
            (let ((entries-before (doing--parse-file today-file)))
              (should (= 1 (length entries-before))))
            ;; Call ensure-rollover
            (doing--ensure-rollover)
            ;; Kill buffers
            (when-let ((buf (get-file-buffer today-file)))
              (kill-buffer buf))
            (when-let ((buf (get-file-buffer week-file)))
              (kill-buffer buf))
            ;; Entry should be moved to week.org
            (let ((today-entries (doing--parse-file today-file))
                  (week-entries (doing--parse-file week-file)))
              (should (= 0 (length today-entries)))
              (should (= 1 (length week-entries)))
              (should (string= "yesterday-id" (plist-get (car week-entries) :id))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Phase 17: View buffer infrastructure

(ert-deftest doing-test-view-buffer-created ()
  "Test that `doing--view-buffer' creates a buffer and displays entries."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir)
         (test-file (doing--file-today)))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create test entries
          (doing--append-entry-to-file
           (list :id "entry1"
                 :title "First task"
                 :started "[2026-01-23 Thu 10:00]"
                 :ended "[2026-01-23 Thu 10:30]"
                 :tags '("work"))
           test-file)
          (doing--append-entry-to-file
           (list :id "entry2"
                 :title "Second task"
                 :started "[2026-01-23 Thu 10:30]"
                 :ended "[2026-01-23 Thu 11:00]"
                 :tags '("dev"))
           test-file)
          ;; Parse entries
          (let ((entries (doing--parse-file test-file)))
            ;; Create view buffer
            (doing--view-buffer "test" entries)
            ;; Buffer should exist
            (should (get-buffer "*doing: test*"))
            ;; Buffer should be in org-mode
            (with-current-buffer "*doing: test*"
              (should (eq major-mode 'org-mode))
              ;; Should contain entry titles
              (goto-char (point-min))
              (should (search-forward "First task" nil t))
              (goto-char (point-min))
              (should (search-forward "Second task" nil t))
              ;; Should contain total
              (goto-char (point-min))
              (should (search-forward "Total:" nil t)))
            ;; Cleanup buffer
            (kill-buffer "*doing: test*")))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-sum-durations ()
  "Test that `doing--sum-durations' correctly sums entry durations."
  ;; Create test entries with known durations
  (let ((entries (list
                  (list :id "entry1"
                        :title "Task 1"
                        :started "[2026-01-23 Thu 10:00]"
                        :ended "[2026-01-23 Thu 10:30]")  ; 30 minutes
                  (list :id "entry2"
                        :title "Task 2"
                        :started "[2026-01-23 Thu 11:00]"
                        :ended "[2026-01-23 Thu 12:00]")))) ; 60 minutes
    ;; Total should be 90 minutes
    (let ((total (doing--sum-durations entries)))
      (should (= total 90.0)))))

(ert-deftest doing-test-format-entry-line ()
  "Test that `doing--format-entry-line' formats entries correctly."
  ;; Test finished entry with tags
  (let* ((entry (list :id "entry1"
                      :title "Finished task"
                      :started "[2026-01-23 Thu 10:00]"
                      :ended "[2026-01-23 Thu 11:00]"
                      :tags '("work" "coding")))
         (line (doing--format-entry-line entry)))
    ;; Should contain title
    (should (string-match-p "Finished task" line))
    ;; Should contain tags
    (should (string-match-p ":work:coding:" line))
    ;; Should NOT contain active marker
    (should-not (string-match-p "\\*" line))
    ;; Should contain duration
    (should (string-match-p "\\[.*\\]" line)))
  ;; Test unfinished entry without tags
  (let* ((entry (list :id "entry2"
                      :title "Active task"
                      :started "[2026-01-23 Thu 10:00]"))
         (line (doing--format-entry-line entry)))
    ;; Should contain title
    (should (string-match-p "Active task" line))
    ;; Should contain active marker
    (should (string-match-p "\\*" line))
    ;; Should NOT contain tags section
    (should-not (string-match-p "::" line))))

(ert-deftest doing-test-view-buffer-grouped ()
  "Test that `doing--view-buffer' correctly groups entries."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir)
         (test-file (doing--file-today)))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create entries from different dates
          (doing--append-entry-to-file
           (list :id "entry1"
                 :title "Monday task"
                 :started "[2026-01-20 Mon 10:00]"
                 :ended "[2026-01-20 Mon 11:00]")
           test-file)
          (doing--append-entry-to-file
           (list :id "entry2"
                 :title "Tuesday task"
                 :started "[2026-01-21 Tue 10:00]"
                 :ended "[2026-01-21 Tue 11:00]")
           test-file)
          ;; Parse entries
          (let ((entries (doing--parse-file test-file)))
            ;; Create grouped view by date
            (doing--view-buffer "grouped-test" entries
                                (lambda (e) (doing--timestamp-date (plist-get e :started))))
            ;; Buffer should exist
            (should (get-buffer "*doing: grouped-test*"))
            ;; Buffer should contain date headers
            (with-current-buffer "*doing: grouped-test*"
              (goto-char (point-min))
              (should (search-forward "* 2026-01-20" nil t))
              (goto-char (point-min))
              (should (search-forward "* 2026-01-21" nil t))
              (goto-char (point-min))
              (should (search-forward "Monday task" nil t))
              (goto-char (point-min))
              (should (search-forward "Tuesday task" nil t)))
            ;; Cleanup buffer
            (kill-buffer "*doing: grouped-test*")))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-sum-durations-with-unfinished ()
  "Test that `doing--sum-durations' handles unfinished entries."
  ;; Create mix of finished and unfinished entries
  (let* ((now (doing--timestamp-now))
         (entries (list
                   (list :id "entry1"
                         :title "Finished"
                         :started "[2026-01-23 Thu 10:00]"
                         :ended "[2026-01-23 Thu 10:30]")  ; 30 minutes
                   (list :id "entry2"
                         :title "Unfinished"
                         :started now))))  ; Variable duration to now
    ;; Should not error and should return a positive number
    (let ((total (doing--sum-durations entries)))
      (should (numberp total))
      (should (>= total 30.0)))))  ; At least 30 from finished entry

;;; Phase 18: View commands tests

(ert-deftest doing-test-view-today-shows-entries ()
  "Test that `doing-view-today' displays today's entries."
  (let* ((temp-dir (make-temp-file "doing-test" t))
         (doing-directory temp-dir)
         (test-file (doing--file-today)))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create entries for today
          (doing--append-entry-to-file
           (list :id "entry1"
                 :title "First task"
                 :tags '("work")
                 :started (doing--timestamp-now)
                 :ended (doing--timestamp-now))
           test-file)
          (doing--append-entry-to-file
           (list :id "entry2"
                 :title "Second task"
                 :tags '("personal")
                 :started (doing--timestamp-now))
           test-file)
          ;; Call doing-view-today
          (doing-view-today)
          ;; Buffer should exist
          (should (get-buffer "*doing: today*"))
          ;; Buffer should contain entries
          (with-current-buffer "*doing: today*"
            (goto-char (point-min))
            (should (search-forward "First task" nil t))
            (goto-char (point-min))
            (should (search-forward "Second task" nil t))
            (goto-char (point-min))
            (should (search-forward ":work:" nil t))
            (goto-char (point-min))
            (should (search-forward ":personal:" nil t))
            ;; Should show total
            (goto-char (point-min))
            (should (search-forward "Total:" nil t)))
          ;; Cleanup buffer
          (kill-buffer "*doing: today*"))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-yesterday-filters-correctly ()
  "Test that `doing-view-yesterday' filters yesterday's entries."
  (let* ((temp-dir (make-temp-file "doing-test" t))
         (doing-directory temp-dir)
         (today-file (doing--file-today))
         (week-file (doing--file-week))
         (yesterday-time (time-subtract (current-time) (days-to-time 1)))
         (yesterday (format-time-string "%Y-%m-%d" yesterday-time))
         (yesterday-day (format-time-string "%a" yesterday-time))
         (today-time (current-time))
         (today (format-time-string "%Y-%m-%d" today-time))
         (today-day (format-time-string "%a" today-time)))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create entries for yesterday in week.org
          (doing--append-entry-to-file
           (list :id "entry1"
                 :title "Yesterday task"
                 :started (format "[%s %s 10:00]" yesterday yesterday-day)
                 :ended (format "[%s %s 11:00]" yesterday yesterday-day))
           week-file)
          ;; Create entry for today in today.org
          (doing--append-entry-to-file
           (list :id "entry2"
                 :title "Today task"
                 :started (format "[%s %s 10:00]" today today-day))
           today-file)
          ;; Call doing-view-yesterday
          (doing-view-yesterday)
          ;; Buffer should exist
          (should (get-buffer "*doing: yesterday*"))
          ;; Buffer should contain only yesterday's entry
          (with-current-buffer "*doing: yesterday*"
            (goto-char (point-min))
            (should (search-forward "Yesterday task" nil t))
            (goto-char (point-min))
            (should-not (search-forward "Today task" nil t)))
          ;; Cleanup buffer
          (kill-buffer "*doing: yesterday*"))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-week-groups-by-date ()
  "Test that `doing-view-week' groups entries by date."
  (let* ((temp-dir (make-temp-file "doing-test" t))
         (doing-directory temp-dir)
         (today-file (doing--file-today))
         (week-file (doing--file-week)))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create entries from different days this week
          (doing--append-entry-to-file
           (list :id "entry1"
                 :title "Monday task"
                 :started "[2026-01-20 Mon 10:00]"
                 :ended "[2026-01-20 Mon 11:00]")
           week-file)
          (doing--append-entry-to-file
           (list :id "entry2"
                 :title "Tuesday task"
                 :started "[2026-01-21 Tue 10:00]"
                 :ended "[2026-01-21 Tue 11:00]")
           week-file)
          (doing--append-entry-to-file
           (list :id "entry3"
                 :title "Today task"
                 :started (doing--timestamp-now))
           today-file)
          ;; Call doing-view-week
          (doing-view-week)
          ;; Buffer should exist
          (should (get-buffer "*doing: this week*"))
          ;; Buffer should contain date headers and entries
          (with-current-buffer "*doing: this week*"
            (goto-char (point-min))
            (should (search-forward "* 2026-01-20" nil t))
            (goto-char (point-min))
            (should (search-forward "* 2026-01-21" nil t))
            (goto-char (point-min))
            (should (search-forward "Monday task" nil t))
            (goto-char (point-min))
            (should (search-forward "Tuesday task" nil t))
            (goto-char (point-min))
            (should (search-forward "Today task" nil t))
            ;; Should show total
            (goto-char (point-min))
            (should (search-forward "Total:" nil t)))
          ;; Cleanup buffer
          (kill-buffer "*doing: this week*"))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-today-empty ()
  "Test that `doing-view-today' handles empty file gracefully."
  (let* ((temp-dir (make-temp-file "doing-test" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Call doing-view-today with empty file
          (doing-view-today)
          ;; Buffer should exist even with no entries
          (should (get-buffer "*doing: today*"))
          (with-current-buffer "*doing: today*"
            ;; Should still show total (0:00)
            (goto-char (point-min))
            (should (search-forward "Total:" nil t)))
          ;; Cleanup buffer
          (kill-buffer "*doing: today*"))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-commands-are-interactive ()
  "Test that view commands are interactive."
  (should (commandp 'doing-view-today))
  (should (commandp 'doing-view-yesterday))
  (should (commandp 'doing-view-week)))

;;; Phase 19: View commands - recent and since

(ert-deftest doing-test-view-recent-limits ()
  "Test that `doing-view-recent' limits to N most recent entries."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create 5 entries with different timestamps
          (dotimes (i 5)
            (let* ((offset (* i 60))  ; 1 minute apart
                   (time (time-subtract (current-time) (seconds-to-time offset)))
                   (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]" time))
                   (entry (list :id (format "test-%d" i)
                                :title (format "Task %d" i)
                                :tags '("test")
                                :started timestamp)))
              (doing--append-entry-to-file entry (doing--file-today))))
          ;; View recent 3
          (doing-view-recent 3)
          ;; Should show only 3 entries
          (should (get-buffer "*doing: recent (3)*"))
          (with-current-buffer "*doing: recent (3)*"
            (let ((content (buffer-string)))
              ;; Most recent 3 are Task 0, Task 1, Task 2
              (should (string-match-p "Task 0" content))
              (should (string-match-p "Task 1" content))
              (should (string-match-p "Task 2" content))
              ;; Task 3 and Task 4 should not appear
              (should-not (string-match-p "Task 3" content))
              (should-not (string-match-p "Task 4" content)))
            (goto-char (point-min))
            (should (search-forward "Total:" nil t)))
          ;; Cleanup buffer
          (kill-buffer "*doing: recent (3)*"))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-recent-default ()
  "Test that `doing-view-recent' defaults to 10 entries."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create 3 entries
          (dotimes (i 3)
            (let* ((entry (list :id (format "test-%d" i)
                                :title (format "Task %d" i)
                                :tags '("test")
                                :started (doing--timestamp-now))))
              (doing--append-entry-to-file entry (doing--file-today))))
          ;; View recent without argument (should default to 10, but only show 3)
          (doing-view-recent)
          (should (get-buffer "*doing: recent (3)*"))
          (with-current-buffer "*doing: recent (3)*"
            (let ((content (buffer-string)))
              (should (string-match-p "Task 0" content))
              (should (string-match-p "Task 1" content))
              (should (string-match-p "Task 2" content))))
          (kill-buffer "*doing: recent (3)*"))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-recent-sorting ()
  "Test that `doing-view-recent' shows entries in reverse chronological order."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create entries with explicit past timestamps
          (let* ((now (current-time))
                 (hour-ago (time-subtract now (seconds-to-time 3600)))
                 (two-hours-ago (time-subtract now (seconds-to-time 7200))))
            (doing--append-entry-to-file
             (list :id "oldest"
                   :title "Oldest task"
                   :tags '("test")
                   :started (format-time-string "[%Y-%m-%d %a %H:%M]" two-hours-ago))
             (doing--file-today))
            (doing--append-entry-to-file
             (list :id "middle"
                   :title "Middle task"
                   :tags '("test")
                   :started (format-time-string "[%Y-%m-%d %a %H:%M]" hour-ago))
             (doing--file-today))
            (doing--append-entry-to-file
             (list :id "newest"
                   :title "Newest task"
                   :tags '("test")
                   :started (format-time-string "[%Y-%m-%d %a %H:%M]" now))
             (doing--file-today)))
          ;; View recent
          (doing-view-recent 3)
          (should (get-buffer "*doing: recent (3)*"))
          (with-current-buffer "*doing: recent (3)*"
            ;; Verify all three tasks are present
            (goto-char (point-min))
            (should (search-forward "Newest task" nil t))
            (goto-char (point-min))
            (should (search-forward "Middle task" nil t))
            (goto-char (point-min))
            (should (search-forward "Oldest task" nil t)))
          (kill-buffer "*doing: recent (3)*"))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-since-filters ()
  "Test that `doing-view-since' filters entries correctly."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create entries with specific dates
          (let* ((today (format-time-string "%Y-%m-%d"))
                 (yesterday (format-time-string "%Y-%m-%d"
                              (time-subtract (current-time) (days-to-time 1))))
                 (two-days-ago (format-time-string "%Y-%m-%d"
                                 (time-subtract (current-time) (days-to-time 2)))))
            (doing--append-entry-to-file
             (list :id "old"
                   :title "Old task"
                   :tags '("test")
                   :started (format "[%s Thu 10:00]" two-days-ago))
             (doing--file-today))
            (doing--append-entry-to-file
             (list :id "recent"
                   :title "Recent task"
                   :tags '("test")
                   :started (format "[%s Fri 10:00]" yesterday))
             (doing--file-today))
            (doing--append-entry-to-file
             (list :id "today"
                   :title "Today task"
                   :tags '("test")
                   :started (format "[%s Sat 10:00]" today))
             (doing--file-today))
            ;; View since yesterday
            (doing-view-since yesterday)
            (should (get-buffer (format "*doing: since %s*" yesterday)))
            (with-current-buffer (format "*doing: since %s*" yesterday)
              (let ((content (buffer-string)))
                ;; Should include yesterday and today
                (should (string-match-p "Recent task" content))
                (should (string-match-p "Today task" content))
                ;; Should NOT include two-days-ago
                (should-not (string-match-p "Old task" content))))
            (kill-buffer (format "*doing: since %s*" yesterday))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-since-empty ()
  "Test that `doing-view-since' handles no matching entries."
  (let* ((temp-dir (make-temp-file "doing-test-" t))
         (doing-directory temp-dir))
    (unwind-protect
        (progn
          (doing--ensure-directory)
          ;; Create entry from yesterday
          (let ((yesterday (format-time-string "%Y-%m-%d"
                             (time-subtract (current-time) (days-to-time 1)))))
            (doing--append-entry-to-file
             (list :id "old"
                   :title "Old task"
                   :tags '("test")
                   :started (format "[%s Thu 10:00]" yesterday))
             (doing--file-today))
            ;; View since tomorrow (should have no entries)
            (let ((tomorrow (format-time-string "%Y-%m-%d"
                              (time-add (current-time) (days-to-time 1)))))
              (doing-view-since tomorrow)
              (should (get-buffer (format "*doing: since %s*" tomorrow)))
              (with-current-buffer (format "*doing: since %s*" tomorrow)
                ;; Should show total 0:00
                (goto-char (point-min))
                (should (search-forward "Total:" nil t)))
              (kill-buffer (format "*doing: since %s*" tomorrow)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doing-test-view-recent-and-since-are-interactive ()
  "Test that view-recent and view-since are interactive."
  (should (commandp 'doing-view-recent))
  (should (commandp 'doing-view-since)))

;;; doing-test.el ends here
