;;; doing.el --- A frictionless activity log for Emacs -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <x@huxx.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: convenience, org, time-tracking
;; URL: https://github.com/xiaoxinghu/doing.el

;;; Commentary:

;; doing.el is an Org-mode based activity log inspired by Brett Terpstra's
;; "doing" CLI tool.  It provides a frictionless way to capture what you're
;; working on, with automatic time tracking and reporting.
;;
;; Key features:
;; - Fast, low-friction capture of activities
;; - Automatic time tracking via timestamps
;; - Time totals and reports
;; - Context-aware auto-tagging
;;
;; Quick start:
;; - M-x doing-now     Start a new activity
;; - M-x doing-finish  Finish current activity
;; - M-x doing-current Show what you're doing
;;
;; Storage model:
;; - today.org   — entries from today
;; - week.org    — entries from current week
;; - archive/    — past weeks (YYYY-WNN.org)

;;; Code:

(require 'org)
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
(require 'doing-totals)
(require 'doing-search)

;;; Customization

(defgroup doing nil
  "A frictionless activity log for Emacs.
Track what you're doing with minimal friction, automatic time
tracking, and easy reporting."
  :group 'org
  :prefix "doing-"
  :link '(url-link :tag "GitHub" "https://github.com/xiaoxinghu/doing.el"))

(defcustom doing-directory (expand-file-name "~/org/doing/")
  "Directory where doing.el stores activity log files.
This directory will contain:
- today.org   — current day's activities
- week.org    — current week's activities
- archive/    — archived weekly files"
  :type 'directory
  :group 'doing)

;;; Internal Configuration
;; These are not exposed as customization options to keep the model simple.

(defconst doing--file-today-name "today.org"
  "Name of the file for today's entries.")

(defconst doing--file-week-name "week.org"
  "Name of the file for current week's entries.")

(defconst doing--archive-directory-name "archive"
  "Name of the directory for archived weekly files.")

(provide 'doing)

;;; doing.el ends here
