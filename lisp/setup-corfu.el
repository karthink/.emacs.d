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
  (setq corfu-auto-prefix 4
        corfu-auto-delay 0.07
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
    :bind ( :map corfu-map
            ([remap corfu-info-documentation] . corfu-popupinfo-toggle))
    :config
    (setq corfu-popupinfo-hide nil
          corfu-popupinfo-delay '(2 . 0.2))
    (corfu-popupinfo-mode 1))
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
  :after (corfu orderless)
  :defines my/toggle-writing-capf
  :bind (("C-$" . cape-dict)
         ("C-S-f" . cape-file)
         ("C-M-/" . cape-dabbrev)
         :map corfu-map
         ("M-/" . cape-dabbrev)
         ("C-x C-f" . cape-file))
  :hook ((text-mode conf-mode) . my/text-mode-capfs)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-file 85)
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev 90)
  :config
  (defvar my/writing-capf (cape-capf-super #'cape-dabbrev #'cape-dict))
  (defun my/toggle-writing-capf ()
    "Turn on CAPFs suitable for writing."
    (interactive)
    (if (memq my/writing-capf completion-at-point-functions)
        (progn
          (kill-local-variable 'corfu-count)
          (kill-local-variable 'corfu-auto-prefix)
          (kill-local-variable 'completion-styles)
          (remove-hook 'completion-at-point-functions
                       my/writing-capf 'local)
          (message "Turned off writing CAPFs."))
      (setq-local corfu-count 5 corfu-auto-prefix 5)
      (add-hook 'completion-at-point-functions
                my/writing-capf nil 'local)
      (setq-local completion-styles '(basic emacs22))
      (message "Turned on writing CAPFs.")))

  ;; Cape-dict settings

  ;; Use Peter Norvig's 300,000 most used English words as the word list
  (let ((wordlist (expand-file-name
                   "wordlist.txt" user-cache-directory)))
    (setq cape-dict-file
          (cond
           ((or (file-readable-p wordlist)
                (and (url-copy-file "https://norvig.com/ngrams/count_1w.txt"
                                    wordlist)
                     (shell-command
                      (concat "sed -re 's_[ \t0-9]*__g' -e '/^.{1,4}$/d' -i "
                              wordlist))))
            (message "Downloaded wordlist")
            wordlist)
           ((getenv "WORDLIST"))
           (t "/usr/share/dict/words"))))
  (setq ispell-alternate-dictionary cape-dict-file)
  (when (bound-and-true-p text-mode-ispell-word-completion)
    (setq text-mode-ispell-word-completion nil))
  
  (setf cape-dict-limit 4
        (alist-get 'cape-dict completion-category-defaults)
        '((styles basic)))

  ;; Cape-dabbrev settings
  (defun my/text-mode-capfs ()
    (add-hook 'completion-at-point-functions 'cape-dabbrev 85 t))
  (setf cape-dabbrev-check-other-buffers nil
        (alist-get 'cape-dabbrev completion-category-defaults)
        '((styles orderless-fast)))

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
