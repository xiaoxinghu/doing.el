;;; doing-test.el --- Tests for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the doing package using ERT.

;;; Code:

(require 'ert)
(require 'doing)

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

;;; doing-test.el ends here
