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
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (setq this-command #'consult-completion-in-region)
        (apply #'consult-completion-in-region completion-in-region--data))))

  ;; Disabled -- interferes with dynamic completion tables
  (use-package orderless
    :disabled
    :hook (corfu-mode . my/corfu-comp-style)
    :config
    (defun my/corfu-comp-style ()
      "Set/unset a fast completion style for corfu"
      (if corfu-mode
          (setq-local completion-styles '(orderless-fast))
        (kill-local-variable 'completion-styles))))
    
  (setq corfu-auto-prefix 3
        corfu-auto-delay 0.05
        corfu-count 8
        corfu-auto  t
        corfu-cycle t
        corfu-quit-no-match 'separator
        corfu-preselect 'prompt
        corfu-scroll-margin 5)
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
      (comint-send-input)))))

(use-package corfu-info
  :after corfu
  :ensure (:host github :repo "minad/corfu"
             :files ("extensions/corfu-info.el")))

(use-package corfu-indexed
  :disabled
  :after corfu
  :ensure (:host github :repo "minad/corfu"
             :files ("extensions/corfu-indexed.el")))

(use-package corfu-quick
  :after corfu
  :ensure (:host github :repo "minad/corfu"
             :files ("extensions/corfu-quick.el"))
  :bind (:map corfu-map
         ("'" . corfu-quick-complete))
  :config
  (setq corfu-quick1 "asdfghjkl;"))

(use-package corfu-history
  :disabled
  :after corfu
  :ensure (:host github :repo "minad/corfu"
             :files ("extensions/corfu-history.el")))
                                                     
(use-package corfu-popupinfo
  :after corfu
  :config (corfu-popupinfo-mode 1)
  :ensure (:host github :repo "minad/corfu"
             :files ("extensions/corfu-popupinfo.el"))
  :bind (:map corfu-map
         ([remap corfu-info-documentation] . corfu-popupinfo-toggle)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Corfu-doc shows help in an adjacent popup window
;; (Package is deprecated, using corfu-popupinfo instead)
(use-package corfu-doc
    :disabled
    :ensure (corfu-doc :host github :repo "galeo/corfu-doc")
    :after corfu
    :bind (:map corfu-map
           ([remap corfu-show-documentation] . corfu-doc-toggle)
           ([remap scroll-other-window] . corfu-doc-scroll-up)
           ([remap scroll-other-window-down] . corfu-doc-scroll-down))
    :config
    (setq corfu-doc-max-width  77
          corfu-echo-documentation nil
          corfu-doc-max-height 20
          corfu-doc-delay 0.2)
    (defun corfu-doc--cleanup ()
      (advice-remove 'corfu--popup-hide #'corfu-doc--cleanup)
      (advice-remove 'corfu--popup-show #'corfu-doc--set-timer)
      (corfu-doc--hide))

    (defun corfu-doc-toggle ()
      (interactive)
      (advice-add 'corfu--popup-hide :after #'corfu-doc--cleanup)
      (if (and (frame-live-p corfu-doc--frame) (frame-visible-p corfu-doc--frame))
          (progn (corfu-doc--hide)
                 (advice-remove 'corfu--popup-show #'corfu-doc--set-timer))
        (corfu-doc--show)
        (advice-add 'corfu--popup-show :after #'corfu-doc--set-timer))))

;; Add extensions
(use-package cape
  :ensure t
  :bind (("C-$" . cape-dict)
         ;; ("C-; e" . cape-line)
         ("C-S-f" . cape-file)
         ;; ("C-M-/" . cape-dabbrev)
         :map corfu-map
         ("M-/" . cape-dabbrev)
         ("C-x C-f" . cape-file))
  ;; Bind dedicated completion commands
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-symbol)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p i" . cape-ispell)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  ;; :hook (text-mode . my/cape-text-mode-capfs)
  :init
  
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  (setq cape-dict-file (getenv "WORDLIST"))
  (defun my/cape-text-mode-capfs ()
    (add-to-list 'completion-at-point-functions #'cape-dict))
  
  (use-package pcomplete
    :defer
    :config
    ;; Silence the pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    ;; (advice-remove 'pcomplete-completions-at-point #'cape-wrap-silent)

    ;; Ensure that pcomplete does not write to the buffer
    ;; and behaves as a pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
    ;; (advice-remove 'pcomplete-completions-at-point #'cape-wrap-purify)
))

(provide 'setup-corfu)
;;; setup-corfu.el ends here

