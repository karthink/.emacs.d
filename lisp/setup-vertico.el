;; -*- lexical-binding: t -*-
;; Vertico
(use-package vertico
  :defer
  :load-path "~/.local/share/git/vertico/"
  :after minibuffer
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
        vertico-cycle t)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

(use-package vertico-directory
  :load-path "~/.local/share/git/vertico/extensions/"
  :hook (rfn-eshadow-update-overlay vertico-directory-tidy)
  :after vertico
  :bind (:map vertico-map
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("C-w"   . vertico-directory-delete-word)
         ("RET"   . vertico-directory-enter)))

(use-package vertico-repeat
  :load-path "~/.local/share/git/vertico/extensions/"
  :after vertico
  :bind (("C-x ." . vertico-repeat)
         ("H-."   . vertico-repeat)))

(use-package vertico-reverse
  :load-path "~/.local/share/git/vertico/extensions/"
  :after vertico
  :hook (vertico-reverse-mode . my/vertico-reverse-setup)
  :config
  (defun my/vertico-reverse-setup ()
    (setq vertico-resize vertico-reverse-mode)))

(use-package vertico-flat
  :disabled
  :load-path "~/.local/share/git/vertico/extensions/"
  :after vertico
  :defer 2
  :bind (:map vertico-map
         ("M-q" . vertico-flat-mode))
  :hook ((minibuffer-setup . my/vertico-list-mode-setup)
         (minibuffer-exit . my/vertico-list-mode-exit))
  :config
  (defvar vertico-list-mode-commands nil
    "List of commands that should not use vertico-flat mode")
  (defvar my/vertico-flat-mode-restore nil
    "Flag to restore vertico mode")
  
  (setq vertico-list-mode-commands
        '(consult-line
          consult-line-symbol-at-point
          consult-outline
          consult-register-load
          consult-imenu consult-project-imenu
          consult-completion-in-region
          consult-yank-pop
          embark-keymap-help
          consult-grep consult-ripgrep consult-git-grep
          bibtex-actions-insert-key bibtex-actions-insert-citation
          bibtex-actions-insert-reference bibtex-actions-insert-bibtex
          consult-reftex-insert-reference
          consult-find affe-find affe-grep
          my/search-occur-browse-url my/eshell-previous-matching-input
          eshell/cd))
  
  (defun my/vertico-list-mode-setup ()
    (when (and vertico-flat-mode
               (member this-command vertico-list-mode-commands))
      (vertico-flat-mode -1)
      (setq my/vertico-flat-mode-restore t)))
  
  (defun my/vertico-list-mode-exit ()
    (when my/vertico-flat-mode-restore
      (vertico-flat-mode 1)
      (setq my/vertico-flat-mode-restore nil))))

(use-package vertico-buffer
  :disabled
  :load-path "~/.local/share/git/vertico/extensions/"
  :after vertico
  :hook (vertico-buffer-mode . vertico-buffer-setup)
  :config
  (defun vertico-buffer-setup ()
    (setq vertico-count (if vertico-buffer-mode 50 12)))
  (setq vertico-buffer-display-action 'display-buffer-reuse-window))

(provide 'setup-vertico)
