;;; doing-test.el --- Tests for doing.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the doing package using ERT.

;;; Code:

(require 'ert)
(require 'doing)

;;; Basic tests

(ert-deftest doing-test-function-exists ()
  "Test that the `doing' function is defined."
  (should (fboundp 'doing)))

(ert-deftest doing-test-is-interactive ()
  "Test that `doing' is an interactive command."
  (should (commandp 'doing)))

(ert-deftest doing-test-message-output ()
  "Test that `doing' outputs the expected message."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (doing)
      (should (equal (car messages) "go back to work")))))

(ert-deftest doing-test-feature-provided ()
  "Test that the `doing' feature is provided."
  (should (featurep 'doing)))

;;; doing-test.el ends here
