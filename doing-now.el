;;; doing-now.el --- Capture commands for doing.el -*- lexical-binding: t; -*-

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
Creates a new entry in today.org with automatic timestamp and ID.
If a previous activity is still in progress, it will be automatically
finished before starting the new one.

Auto-tags from `doing-auto-tags' are applied based on the current
`default-directory'. If user-provided TAGS are given, they are merged
with auto-tags.

If called interactively, prompts for TITLE. TAGS should be a list
of strings, e.g., (\"emacs\" \"coding\")."
  (interactive "sWhat are you doing? ")
  (doing--ensure-rollover)
  (doing--ensure-directory)
  ;; Auto-finish previous activity if it exists
  (when (doing--current-entry)
    (doing-finish))
  ;; Get auto-tags based on current directory
  (let* ((auto-config (doing--auto-tags-for-directory))
         (auto-tags (plist-get auto-config :tags))
         (auto-project (plist-get auto-config :project))
         ;; Merge user tags with auto-tags (user tags take precedence)
         (all-tags (delete-dups (append tags auto-tags)))
         (entry (list :id (doing--generate-id)
                      :title title
                      :tags all-tags
                      :started (doing--timestamp-now))))
    ;; Add project property if auto-configured
    (when auto-project
      (plist-put entry :project auto-project))
    (doing--append-entry-to-file entry (doing--file-today))
    (message "Started: %s" title)))

(provide 'doing-now)

;;; doing-now.el ends here
