;; -*- lexical-binding: t -*-

;; Vertico
(use-package vertico
  ;; :commands vertico-mode
  :ensure (:files (:defaults "extensions/*.el"))
  :after minibuffer
  :commands vertico-mode
  :init (vertico-mode 1)
  :bind (:map vertico-map
         ("M-RET"   . nil)
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
         ("M-*"      . embark-act-all)
         ("M-s o"   . embark-collect)
         ("C-c C-o" . embark-collect)
         ("C-M-l"     . embark-export))
  :config
  (setq vertico-count 10
        vertico-cycle t
        vertico-resize t)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (advice-add #'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args)))))

(use-package vertico-multiform
  :commands vertico-multiform-mode
  :after vertico-flat
  :bind (:map vertico-map
              ("C-H-SPC" . embark-select)
              ("M-q" . vertico-multiform-flat)
              ("C-l" . my/vertico-multiform-unobtrusive)
              ("C-M-l" . embark-export))
  :init (vertico-multiform-mode 1)
  :config
  (setq vertico-multiform-categories
         '((file my/vertico-grid-mode reverse)
           (jinx grid (vertico-grid-annotate . 20))
           (project-file my/vertico-grid-mode reverse)
           (imenu (vertico-count . 14))
           (face reverse (vertico-count . 8))
           (consult-location buffer)
           (consult-grep buffer)
           (notmuch-result reverse)
           (minor-mode reverse)
           (reftex-label (:not unobtrusive))
           (embark-keybinding grid)
           (citar-reference reverse)
           ;; (xref-location reverse)
           (history reverse)
           (url reverse)
           (consult-info buffer)
           (kill-ring reverse)
           (consult-compile-error reverse)
           (buffer flat (vertico-cycle . t))
           (t flat)))
   (setq vertico-multiform-commands
         '((org-ql-find reverse)
           (org-ql-refile reverse)
           (my/consult-mark)
           (jinx-correct reverse)
           (tab-bookmark-open reverse)
           (dired-goto-file unobtrusive)
           (load-theme my/vertico-grid-mode reverse)
           (my/toggle-theme my/vertico-grid-mode reverse)
           (org-refile reverse)
           (org-agenda-refile reverse)
           (org-capture-refile reverse)
           (affe-find reverse)
           (execute-extended-command unobtrusive)
           (dired-goto-file flat)
           (consult-project-buffer flat)
           (consult-dir-maybe reverse)
           (consult-dir reverse)
           (consult-flymake reverse)
           (consult-history reverse)
           (consult-completion-in-region reverse)
           (consult-recoll buffer)
           (cw-search reverse)
           (citar-insert-citation reverse)
           (completion-at-point reverse)
           (org-roam-node-find reverse)
           ;; (embark-completing-read-prompter reverse)
           ;; (embark-act-with-completing-read reverse)
           ;; (embark-bindings reverse)
           (consult-org-heading reverse)
           (consult-dff unobtrusive)
           (embark-find-definition reverse)
           (xref-find-definitions reverse)
           (xref-find-references reverse)
           (my/eshell-previous-matching-input reverse)
           (tmm-menubar reverse)))
  
   (defun my/vertico-multiform-unobtrusive ()
     "Toggle between vertico-unobtrusive and vertico-reverse."
     (interactive)
     (vertico-multiform-vertical 'vertico-reverse-mode)))

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
         ("'" . vertico-quick-exit)
         ("C-'" . vertico-quick-embark))
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
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-x ." . vertico-repeat)
         ("H-."   . vertico-repeat))
  :config
  (use-package savehist
    :defer
    :config
    (add-to-list 'savehist-additional-variables
                 'vertico-repeat-history)))

(use-package vertico-suspend
  :after vertico
  :bind ("M-z" . vertico-suspend))

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
