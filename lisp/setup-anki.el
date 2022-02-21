(use-package anki-editor
  :straight t
  :after org-capture
  :init 
  (setq org-my-anki-file (concat (file-name-as-directory org-directory)
                               "anki-notes.org"))
  :bind (:map org-mode-map
              ("C-c C-+"  . anki-editor-cloze-region-auto-incr)
              ("C-c C--"  . anki-editor-cloze-region-dont-incr)
              ("C-c C-="  . anki-editor-reset-cloze-number)
              ("C-c p"    . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  ;; (setq org-my-anki-file (concat (file-name-as-directory org-directory)
  ;;                              "anki-notes.org"))

  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t
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
  (anki-editor-reset-cloze-number)
  )


(with-eval-after-load 'org-capture
  ;; Org-capture templates
  (pcase-dolist
     (`(,key . ,template)
      '(("ah" "Here"
           entry
           (function (lambda ()
                        (cl-assert (eq major-mode 'org-mode) nil "Cannot insert here, not in org-mode.")
                        (outline-back-to-heading)))
           "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Default\n:END:\n** Front\n%?\n** Back\n\n")
        ("ab" "Anki basic"
           entry
           (file+headline ,org-my-anki-file "Dispatch Shelf")
           "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Default\n:END:\n** Front\n%?\n** Back\n\n"
           :kill-buffer t)
        ("ac" "Anki cloze"
           entry
           (file+headline ,org-my-anki-file "Dispatch Shelf")
           "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Default\n:END:\n** Text\n\n** Extra\n"
           :kill-buffer t)
          ("a" "Anki")))
   (setf (alist-get key org-capture-templates nil nil #'equal)
         template)))

;; ;; Allow Emacs to access content from clipboard.
;; (setq x-select-enable-clipboard t
;;       x-select-enable-primary t)

(provide 'setup-anki)
