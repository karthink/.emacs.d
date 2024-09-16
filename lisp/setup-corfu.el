(use-package corfu
  :ensure (:host github :repo "minad/corfu")
  :hook (((prog-mode text-mode tex-mode ielm-mode) . corfu-mode)
         ((shell-mode eshell-mode) . my/corfu-shell-settings)
         (minibuffer-setup . my/corfu-enable-always-in-minibuffer))
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("RET" . nil)
         ("M-RET" . corfu-insert)
         ("M-." . corfu-show-location)
         ("M-h" . nil)
         ([remap next-line] . nil)
         ([remap previous-line] . nil)
         ("M-." . corfu-info-location)
         ("C-h" . corfu-info-documentation))
  :config
  (setq corfu-auto-prefix 3
        corfu-auto-delay 0.05
        corfu-count 8
        corfu-auto  t
        corfu-cycle t
        corfu-quit-no-match 'separator
        corfu-preselect 'prompt
        corfu-scroll-margin 5)
  
  ;; Extensions
  (use-package corfu-info
    :bind (:map corfu-map ("M-g" . nil)))
  (use-package corfu-history :defer 3 :config (corfu-history-mode 1))
  (use-package corfu-popupinfo
  :config (corfu-popupinfo-mode 1)
  :bind (:map corfu-map
         ([remap corfu-info-documentation] . corfu-popupinfo-toggle)))
  (use-package corfu-quick
    :bind (:map corfu-map ("'" . corfu-quick-complete))
    :config (setq corfu-quick1 "asdfghjkl;"))

  ;; Corfu in the minibuffer
  (defvar my-corfu-minibuffer-exclude-modes (list read-passwd-map)
    "Minibuffer-local keymaps for which Corfu should be disabled.")
  (defvar my-corfu-minibuffer-exclude-commands
    '(org-ql-find)
    "Minibuffer commands for which Corfu should be disabled.")
  (defun my/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (or (bound-and-true-p vertico--input)
                (memq this-command my-corfu-minibuffer-exclude-commands)
                (memq (current-local-map)
                      my-corfu-minibuffer-exclude-modes))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (use-package consult
    :bind (:map corfu-map
           ("M-m" . corfu-move-to-minibuffer)
           ("C-<tab>" . corfu-move-to-minibuffer))
    :config
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
         (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
           (consult-completion-in-region beg end table pred)))))
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))
  
  ;; Corfu in the shell
  (defun my/corfu-shell-settings ()
    (setq-local corfu-quit-no-match t
                corfu-auto nil)
    (setq-local corfu-map (copy-keymap corfu-map)
                completion-cycle-threshold nil)
    (define-key corfu-map "\r" #'corfu-insert-and-send)
    (corfu-mode))
  (defun corfu-insert-and-send ()
    (interactive)
    ;; 1. First insert the completed candidate
    (corfu-insert)
    ;; 2. Send the entire prompt input to the shell
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((derived-mode-p 'comint-mode)
      (comint-send-input))))

  ;; Faster corfu
  ;; Disabled -- interferes with dynamic completion tables
  (use-package orderless
    :disabled
    :after orderless
    :hook (corfu-mode . my/corfu-comp-style)
    :config
    (defun my/corfu-comp-style ()
      "Set/unset a fast completion style for corfu"
      (if corfu-mode
          (setq-local completion-styles '(orderless-fast))
        (kill-local-variable 'completion-styles)))))

(use-package kind-icon
  :disabled
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  ;; (setq nerd-icons-corfu-mapping
  ;;       '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
  ;;         (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
  ;;         ;; ...
  ;;         (t :style "cod" :icon "code" :face font-lock-warning-face)))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :bind (("C-$" . cape-dict)
         ("C-S-f" . cape-file)
         ("C-M-/" . cape-dabbrev)
         :map corfu-map
         ("M-/" . cape-dabbrev)
         ("C-x C-f" . cape-file))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  (setq cape-dict-file (getenv "WORDLIST"))
  (defun my/cape-text-mode-capfs ()
    (add-to-list 'completion-at-point-functions #'cape-dict))
  
  (when (< emacs-major-version 29)
    (use-package pcomplete
      :defer
      :config
      ;; Silence the pcomplete capf, no errors or messages!
      (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
        ;; Ensure that pcomplete does not write to the buffer
      ;; and behaves as a pure `completion-at-point-function'.
      (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))))

(provide 'setup-corfu)
;;; setup-corfu.el ends here
