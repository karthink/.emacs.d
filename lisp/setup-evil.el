(use-package evil-leader
  :ensure
  :commands global-evil-leader-mode
  :init
  (setq evil-want-keybinding nil)
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "B" (lambda () "Byte-compile file"
                                                       (if buffer-file-truename
                                                           (byte-compile-file buffer-true-filename)
                                                         (message "Not visiting a file!"))))
  (evil-leader/set-key-for-mode 'latex-mode "cc" 'TeX-command-master)
  (evil-leader/set-key-for-mode 'latex-mode "ca" 'TeX-command-run-all)
  (evil-leader/set-key-for-mode 'latex-mode "=" 'reftex-toc)
  (evil-leader/set-key-for-mode 'latex-mode "("  'reftex-label)
  (evil-leader/set-key-for-mode 'latex-mode ")" 'reftex-reference)
  (evil-leader/set-key-for-mode 'latex-mode "[" 'reftex-citation)
  (evil-leader/set-key
    ;; "e" 'find-file
    "u"  'universal-argument

    ;; File commands
    "fz" 'counsel-fzf
    "ff" 'counsel-find-file
    "f." 'counsel-find-file
    "fS" 'sudo-find-file
    "fd" 'dired
    ;; "f~" (lambda () "Find file from ~/" (find-fil))
    "fr" 'counsel-recentf
    "fj" 'counsel-file-jump
    "fg" 'counsel-git
    "f'" 'counsel-bookmark
    "fE" 'find-file-emacs-config
    "fD" 'find-file-Documents
    "fR" 'find-file-Research
    "fC" 'find-file-system-config
    
    ;; Searching
    "//" 'counsel-grep-or-swiper
    "/q" 'query-replace-regexp
    "/s" 'replace-regexp
    "/r" 'query-replace
    "/S" 'batch-replace-strings
    "/a" 'counsel-ag
    "/g" 'counsel-grep
    
    ;; Buffer commands
    "b" 'switch-to-buffer
    "w" 'save-buffer
    "q" 'evil-quit
    "`" 'evil-switch-to-windows-last-buffer
    ;; Navigation commands
    "j" 'ace-jump-mode

    "k" '(lambda () (interactive) (kill-buffer (current-buffer)))
    "K" 'kill-buffer-and-window
    "n" (lambda (&optional arg)
          "Next Buffer"
                  (interactive "P")
                  (if arg (previous-user-buffer) (next-user-buffer)))
    "p" (lambda (&optional arg)
          "Previous Buffer"
                  (interactive "P")
                  (if arg (next-user-buffer) (previous-user-buffer)))
    "N" 'next-buffer
    "P" 'previous-buffer
    "B" (lambda ()
          "Open Bibtex file"
          (interactive)
          (find-file-other-window (getenv "BIB")))
    "," 'er/expand-region
    "cn" 'next-error
    "cp" 'previous-error
    "vf" 'ido-find-file-other-window
    "vb" 'ido-switch-buffer-other-window)
  :config
  ;; Helper functions
  (defun find-file-system-config ()
    "Find file in system config"
    (interactive)
    (let ((configdir (getenv "CONFIGDIR")))
      (if configdir
          (counsel-file-jump "" (getenv "CONFIGDIR"))
        (message "ENV variable $CONFIGDIR not set"))))

  (defun find-file-emacs-config ()
    "Find file in emacs config"
    (interactive)
    (counsel-file-jump "" (concat
                           (file-name-as-directory (getenv "HOME"))
                           ".emacs.d")))

  (defun find-file-Documents ()
    "Find file in user documents"
    (interactive)
    (counsel-file-jump "" (concat
                           (file-name-as-directory (getenv "HOME"))
                           "Documents")))

  (defun find-file-Research ()
    "Find file in user research documents"
    (interactive)
    (counsel-file-jump "" (concat
                           (file-name-as-directory (getenv "HOME"))
                           (file-name-as-directory"Documents")
                           "research")))
  )

(use-package evil
  :ensure t
  :after evil-leader
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq-default evil-symbol-word-search t)
  (setq evil-emacs-state-cursor '(hbar . 4))
  (setq evil-vsplit-window-right t)
  (setq evil-spilt-window-below t)
  (add-hook 'evil-jumps-post-jump-hook #'recenter)
  :bind (:map evil-motion-state-map
              ("C-w C-h" . evil-window-left)
              ("C-w C-l" . evil-window-right)
              ("C-w C-k" . evil-window-up)
              ("C-w C-j" . evil-window-down)
              ("C-w C-f" . winner-redo)
              ("C-w C-b" . winner-undo)
              ("C-w C-w" . winner-undo)
              ("C-w |" . toggle-window-split)
              :map evil-normal-state-map
              ("[o" . open-previous-line)
              ("]o" . open-next-line))
  :config
  (evil-mode 1)
  ;; (define-key evil-motion-state-map ";" 'evil-repeat-find-char)
  ;; (define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
  (when evil-want-C-u-scroll
    (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
  (evil-define-key '(normal visual insert) helpful-mode-map "q" 'quit-window)
  (evil-define-key '(normal visual insert) special-mode-map "q" 'quit-window)
  
  (progn
    (evil-define-key '(visual normal) matlab-mode-map (kbd "zb") 'matlab-shell-run-block)
    (evil-define-key '(visual normal) matlab-mode-map (kbd "zr") 'matlab-shell-run-region-or-line)
    (evil-define-key '(normal visual) matlab-mode-map (kbd "[[") #'matlab-backward-section)
    (evil-define-key '(normal visual) matlab-mode-map (kbd "]]") #'matlab-forward-section))
  
  (defvar dotemacs--original-mode-line-bg (face-background 'mode-line))
  (defadvice evil-set-cursor-color (after dotemacs activate)
    (cond ((evil-emacs-state-p)
           (set-face-background 'mode-line "#440000"))
          ;; ((evil-insert-state-p)
          ;;  (set-face-background 'mode-line "#002244"))
          ;; ((evil-visual-state-p)
          ;;  (set-face-background 'mode-line "#440044"))
          (t
           (set-face-background 'mode-line dotemacs--original-mode-line-bg))))
  ;; Setup text-objects for use in LaTeX. Putting this here because it doesn't make sense to put this in the AucTeX section without enabling evil-mode first.
  (require 'evil-latex-textobjects nil t)
  (add-hook 'LaTeX-mode-hook 'turn-on-evil-latex-textobjects-mode)

  ;; c/d/y s {motion}{delimiter} to change/delete/add delimiter around motion.
  (use-package evil-surround
    :ensure
    :commands turn-on-evil-surround-mode
    :init
    (global-evil-surround-mode 1))

  ;; (use-package evil-embrace
  ;;   :ensure t
  ;;   :after evil-surround
  ;;   :commands embrace-add-pair embrace-add-pair-regexp
  ;;   :init (evil-embrace-enable-evil-surround-integration)
  ;;   ;;(setq evil-embrace-show-help-p nil)
  ;;   )

  ;; gc{motion} to comment/uncomment
  (use-package evil-commentary
    :ensure
    :diminish ""
    :commands evil-commentary-mode
    :init
    (evil-commentary-mode 1)
    )

  ;; gx{motion} to select, gx{motion} on second object to exchange
  (use-package evil-exchange
    :ensure t
    :config
    (evil-exchange-install))

  ;; gl{motion}{char} to align on char
  (use-package evil-lion
    :ensure t
    :config
    (evil-lion-mode))

  ;; % to match delimiters, % as text-object to manipulate
  (use-package evil-matchit
    :ensure t
    :init (global-evil-matchit-mode 1))

  ;; + and - to increment/decrement number at point
  (use-package evil-numbers
    :ensure t
    :bind (:map evil-normal-state-map
                ("+" . evil-numbers/inc-at-pt)
                ("-" . evil-numbers/dec-at-pt)))

  ;; C-a, C-e, C-f, C-b, C-d and C-k have same definitions as in emacs mode.
  ;; C-n and C-p work like in emacs if auto-complete is loaded.
  (use-package evil-rsi
    :ensure t
    :config
    (evil-rsi-mode)
    :diminish "")

  ;; s to snipe for next occurrence of chars
  ;; in operator mode, z or x to operate including/excluding next ocurrence of chars
  (use-package evil-snipe
    :after evil-collection
    :ensure t
    :config
    ;; (evil-snipe-override-mode nil)
    (evil-snipe-mode 1)
    (setq evil-snipe-spillover-scope 'whole-visible)
    (setq evil-snipe-smart-case t)
    ;; (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
    ;; (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S)
    (evil-define-key 'normal snipe-local-mode-map "S" 'evil-snipe-S)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
    :diminish "")

  ;; Hit ; or , (originally <SPC>) to repeat last movement.
  ;; (use-package evil-space
  ;;   :ensure t
  ;;   :init
  ;;   (evil-space-mode)
  ;;   (setq evil-space-next-key ";")
  ;;   (setq evil-space-prev-key ","))

  ;; Select with visual-mode and hit * or # to find next occurrence
  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode)
    (setq evil-visualstar/persistent t)
    :diminish "")
  
  ;; (use-package evil-paredit
  ;;   :ensure t
  ;;   :config
  ;;   (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))
  (use-package org-evil
  :ensure t)
;;----------------------------------------------
;; EVIL-SMARTPARENS
;;----------------------------------------------
  (use-package evil-smartparens
    :ensure t
    :init
    (add-hook 'lisp-interaction-mode-hook #'evil-smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
    ;; (add-hook 'lisp-interaction-mode-hook #'evil-smartparens-mode)
    :config
    (evil-define-key 'normal evil-smartparens-mode-map (kbd "(") #'sp-backward-sexp)
    (evil-define-key 'normal evil-smartparens-mode-map (kbd ")") #'sp-forward-sexp)
    ;; (evil-define-key 'normal evil-smartparens-mode-map (kbd "J") #'sp-join-sexp)
    )
 
   (require 'evil-matlab-textobjects nil t)
  )

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (defvar evil-collection-enabled-mode-list
    '(ag
      (package-menu package)
      help
      helpful
      eshell
      ivy
      custom
      dired
      wdired
      magit
      man
      woman
      diff-mode
      ediff
      calc
      which-key
      reftex)
    "The list of `evil-collection' modules to load. evil-mode bindings will be enabled for these modes. See `evil-collection-mode-list' for the full set of supported modes.")
  :config
  (evil-collection-init evil-collection-enabled-mode-list)
  ;; (evil-collection-setup-minibuffer nil)
  ;; Additional bindings
  (evil-define-key* 'normal process-menu-mode-map
  "q" #'kill-current-buffer
  "d" #'process-menu-delete-process)
  )

(provide 'setup-evil)
