;;; hello-world.el --- A tiny hello world package -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; URL: https://example.com/hello-world

;;; Commentary:

;; A minimal example Emacs Lisp package.
;; Provides a single command that prints "hello world".

;;; Code:

(defun doing ()
  "remember what I was doing."
  (interactive)
  (message "go back to work"))

(provide 'doing)

;;; doing.el ends here
