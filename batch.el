(require 'org)
(require 'org-id)

(defun my-org-next-task ()
  "Return the ID of the first TODO entry in the current org file.
If the entry has no ID, generate one, save the buffer, and return it."
  (let (found-id)
    (org-map-entries
     (lambda ()
       (unless found-id
         (setq found-id
               (or (org-entry-get (point) "ID")
                   (progn
                     (org-id-get-create)
                     (org-entry-get (point) "ID"))))))
     "TODO=\"TODO\""
     'file)

    (unless found-id
      (error "No TODO entry found"))

    (save-buffer)
    (princ found-id)))

(defun my-org-task-content (task-id)
  "Print the subtree text for TASK-ID in org-mode format."
  (org-id-goto task-id)
  (princ
   (buffer-substring
    (point)
    (org-end-of-subtree t t))))

(defun my-org-mark-done (task-id props)
  "Mark org task TASK-ID as DONE and attach PROPS as org properties.

PROPS must be an alist of (KEY . VALUE)."
  (org-id-goto task-id)

  ;; Mark DONE
  (org-todo 'done)

  ;; Apply properties
  (dolist (entry props)
    (let ((key (format "CLAUDE_%s"
                       (upcase (symbol-name (car entry)))))
          (value (format "%s" (cdr entry))))
      (org-set-property key value)))

  (save-buffer))
