;; DONT
(use-package anki-editor
  :disabled
  :ensure t
  :after org-capture
  :init 
  (setq org-my-anki-file (concat (file-name-as-directory org-directory)
                               "anki-notes.org"))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  ;; (setq org-my-anki-file (concat (file-name-as-directory org-directory)
  ;;                              "anki-notes.org"))

  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags nil
        anki-editor-break-consecutive-braces-in-latex t)
  
  (cl-pushnew "@anki" anki-editor-ignored-org-tags)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number))

(use-package org-anki
  :ensure t
  :after org
  :commands (my/org-anki-sync-maybe)
  :init (add-hook 'org-ctrl-c-ctrl-c-final-hook
            #'my/org-anki-sync-maybe 80)
  :config
  (setq org-anki-default-deck "Default"
        org-anki-default-match "@anki&todo<>\"TODO\""
        org-anki-inherit-tags nil)
  (defun my/org-anki-sync-maybe ()
    (when (and (fboundp 'org-anki-sync-entry)
               (member "@anki" (org-get-tags)))
      (when-let ((buf (condition-case nil
                          (url-retrieve-synchronously
                           (or org-anki-ankiconnnect-listen-address
                               "http://127.0.0.1:8765")
                           'silent nil 1)
                        (file-error nil))))
        (kill-buffer buf)
        (org-anki-sync-entry)))))

(provide 'setup-anki)
