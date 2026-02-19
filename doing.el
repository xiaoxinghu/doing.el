;;; doing.el --- Frictionless activity log and time tracking -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: convenience, org, time-tracking
;; URL: https://github.com/xiaoxinghu/doing.el

;; Copyright (C) 2026 Xiaoxing Hu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
(require 'doing-utils)

;;; Keymap

(defvar doing-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'doing-now)
    (define-key map "f" #'doing-finish)
    (define-key map "c" #'doing-current)
    (define-key map "a" #'doing-again)
    (define-key map "t" #'doing-view-today)
    (define-key map "w" #'doing-view-week)
    (define-key map "T" #'doing-totals)
    (define-key map "s" #'doing-search)
    (define-key map "e" #'doing-edit)
    (define-key map "o" #'doing-open)
    map)
  "Keymap for doing commands.
Suggested binding: (global-set-key (kbd \"C-c d\") doing-command-map)")

(provide 'doing)

;;; doing.el ends here
