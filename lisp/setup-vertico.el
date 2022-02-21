;; -*- lexical-binding: t -*-

;; Vertico
(use-package vertico
  ;; :commands vertico-mode
  :straight (vertico :files (:defaults "extensions/*.el"))
  :after minibuffer
  :commands vertico-mode
  :init (vertico-mode 1)
  :bind (:map vertico-map
              ("M-s"     . nil)
              ("M-i"     . vertico-insert)
              ("C-M-n"   . vertico-next-group)
              ("C-M-p"   . vertico-previous-group)
              ("C-j"     . (lambda () (interactive)
	        	     (if minibuffer--require-match
	        	         (minibuffer-complete-and-exit)
	        	       (exit-minibuffer))))
              ("C->"     . embark-become)
              (">"       . embark-become)
              ("C-<tab>"   . embark-act-with-completing-read)
              ("C-o"     . embark-minimal-act)
              ("C-M-o"   . embark-minimal-act-noexit)
              ("M-s o"   . embark-export)
              ("C-c C-o" . embark-export)
              ("C-l"     . embark-export))
  :config
  (setq vertico-count 15
        vertico-cycle t
        vertico-resize t)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

(use-package vertico-multiform
  :commands vertico-multiform-mode
  :after vertico-flat
  :bind (:map vertico-map
              ("M-q" . vertico-multiform-grid)
              ("C-l" . vertico-multiform-unobtrusive)
              ("C-M-l" . embark-export))
  :init (vertico-multiform-mode 1)
  :config
  (setq vertico-multiform-categories
         '((file my/vertico-grid-mode reverse)
           (project-file my/vertico-grid-mode reverse)
           (imenu buffer)
           (consult-location buffer)
           (consult-grep buffer)
           (notmuch-result reverse)
           (minor-mode reverse)
           (reftex-label reverse)
           (citar-reference reverse)
           (xref-location reverse)
           (t unobtrusive)))
   (setq vertico-multiform-commands
         '((load-theme my/vertico-grid-mode reverse)
           (my/toggle-theme my/vertico-grid-mode reverse)
           (consult-dir-maybe reverse)
           (consult-dir reverse)
           (consult-flymake reverse)
           (consult-history reverse)
           (consult-completion-in-region reverse)
           (completion-at-point reverse)
           (org-roam-node-find reverse)
           (embark-completing-read-prompter reverse)
           (embark-act-with-completing-read reverse)
           (embark-prefix-help-command reverse)
           (embark-bindings reverse)
           (consult-org-heading reverse)
           (consult-dff unobtrusive)
           (my/eshell-previous-matching-input reverse)
           (tmm-menubar reverse)))
  
   (defun vertico-multiform-unobtrusive ()
     "Toggle the quiet display."
     (interactive)
     (vertico-multiform--display-toggle 'vertico-unobtrusive-mode)
     (if vertico-unobtrusive-mode
         (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
       (vertico-multiform--temporary-mode 'vertico-reverse-mode 1))))

(use-package vertico-unobtrusive
  :after vertico-flat)

(use-package vertico-grid
  :after vertico
  ;; :bind (:map vertico-map ("M-q" . vertico-grid-mode))
  :config
  (defvar my/vertico-count-orig vertico-count)
  (define-minor-mode my/vertico-grid-mode
    "Vertico-grid display with modified row count."
    :global t :group 'vertico
    (cond
     (my/vertico-grid-mode
      (setq my/vertico-count-orig vertico-count)
      (setq vertico-count 4)
      (vertico-grid-mode 1))
     (t (vertico-grid-mode 0)
        (setq vertico-count my/vertico-count-orig))))
  (setq vertico-grid-separator "    ")
  (setq vertico-grid-lookahead 50))

(use-package vertico-quick
  :after vertico
  :bind (:map vertico-map
         ("M-i" . vertico-quick-insert)
         ("C-'" . vertico-quick-exit)
         ("C-o" . vertico-quick-embark))
  :config
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg))))

(use-package vertico-directory
  :hook (rfn-eshadow-update-overlay vertico-directory-tidy)
  :after vertico
  :bind (:map vertico-map
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("C-w"   . vertico-directory-delete-word)
         ("RET"   . vertico-directory-enter)))

(use-package vertico-repeat
  :after vertico
  :bind (("C-x ." . vertico-repeat)
         ("H-."   . vertico-repeat)))

(use-package vertico-reverse
  ;; :disabled
  :after vertico)

(use-package vertico-flat
  ;; :bind (:map vertico-map
  ;;             ("M-q" . vertico-flat-mode))
  :after vertico)

(use-package vertico-buffer
  :after vertico
  ;; :hook (vertico-buffer-mode . vertico-buffer-setup)
  :config
  (setq vertico-buffer-display-action 'display-buffer-reuse-window))

(provide 'setup-vertico)
;; setup-vertico.el ends here
