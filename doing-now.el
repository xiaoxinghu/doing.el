;;; doing-now.el --- Capture commands for doing.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Xiaoxing Hu

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
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

;; Commands for capturing new activities in the doing log.
;; Provides the main entry point for starting and tracking activities.

;;; Code:

(require 'doing-lib)
(require 'doing-current)
(require 'doing-finish)
(require 'doing-rollover)

;;;###autoload
(defun doing-now (title &optional tags)
  "Start new activity with TITLE and optional TAGS.

TITLE can include inline @tag syntax (e.g., \"Writing code @emacs\").
Tags are extracted and merged with auto-tags. The @tags are removed
from the title when saved to the Org file.

Examples:
  Interactive: \"Fix bug @testing @emacs\"
    → Creates entry with title \"Fix bug\" and tags :testing:emacs:
  Programmatic: (doing-now \"Code review @work\" \\='(\"urgent\"))
    → Merges inline tag \"work\" with programmatic tag \"urgent\"

Creates a new entry in today.org with automatic timestamp and ID.
If a previous activity is still in progress, it will be automatically
finished before starting the new one.

Auto-tags from `doing-auto-tags' are applied based on the current
`default-directory'. Inline @tags and programmatically-provided TAGS
are merged with auto-tags (user-provided tags take precedence).

When called interactively, prompts for TITLE. Inline @tags are
automatically extracted. TAGS should be a list of strings when
calling programmatically."
  (interactive
   (let* ((input (read-string "What are you doing? "))
          (parsed (doing--parse-tags-from-title input))
          (title (car parsed))
          (tags (cdr parsed)))
     ;; Reject empty title
     (while (string-empty-p title)
       (setq input (read-string "Title cannot be empty. What are you doing? "))
       (setq parsed (doing--parse-tags-from-title input))
       (setq title (car parsed))
       (setq tags (cdr parsed)))
     (list title tags)))
  (doing--ensure-rollover)
  (doing--ensure-directory)
  ;; Parse @tags from title (even for programmatic calls)
  (let* ((parsed (doing--parse-tags-from-title title))
         (clean-title (car parsed))
         (inline-tags (cdr parsed)))
    ;; Auto-finish previous activity if it exists
    (when (doing--current-entry)
      (doing-finish))
    ;; Get auto-tags based on current directory
    (let* ((auto-config (doing--auto-tags-for-directory))
           (auto-tags (plist-get auto-config :tags))
           (auto-project (plist-get auto-config :project))
           ;; Merge inline tags, programmatic tags, and auto-tags
           ;; User tags (both inline and programmatic) take precedence
           (all-tags (delete-dups (append inline-tags tags auto-tags)))
           (entry (list :id (doing--generate-id)
                        :title clean-title
                        :tags all-tags
                        :started (doing--timestamp-now))))
      ;; Add project property if auto-configured
      (when auto-project
        (plist-put entry :project auto-project))
      (doing--append-entry-to-file entry (doing--file-today))
      (message "Started: %s" clean-title))))

(provide 'doing-now)

;;; doing-now.el ends here
