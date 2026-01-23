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

;;; doing-test.el ends here
