;; -*- lexical-binding: t -*-
;; Consult
(use-package consult
  :ensure t
  :after minibuffer
  :hook ((shell-mode eshell-mode) . (lambda () (setq completion-in-region-function
                                                #'consult-completion-in-region)))
  :config
  (setq consult-narrow-key "<")
  (setq consult-line-numbers-widen t)
  (setq consult-preview-buffer nil)
  (setq consult-preview-mark nil)
  (setq consult-preview-line 'any)
  (setq consult-preview-outline nil)
  (setq consult-preview-key 'any)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep 
   consult-bookmark consult--source-buffer consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "C-M-m"))
  (setq consult-project-root-function (lambda () "Return current project root"
                                        (project-root (project-current))))
  ;; (setq consult-find-args
  ;;       "fd --color=never --hidden -t f -t d -t l --follow")
  (when (executable-find "plocate")
    (setq consult-locate-args "plocate --ignore-case --existing --regexp"))
  (defun consult-buffer-other-tab ()
  "Variant of `consult-buffer' which opens in other frame."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-tab))
    (consult-buffer)))

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  
(defvar consult--fd-command nil)
(defun consult--fd-builder (input)
  (unless consult--fd-command
    (setq consult--fd-command
          (if (eq 0 (call-process-shell-command "fdfind"))
              "fdfind"
            "fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended)))
    (when re
      (list :command (append
                      (list consult--fd-command
                            "--color=never" "--full-path"
                            (consult--join-regexps re 'extended))
                      opts)
            :highlight hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))
  
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  
  (defcustom my/consult-ripgrep-or-line-limit 300000
  "Buffer size threshold for `my/consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
  :type 'integer)

  ;; Combine `consult-imenu' and `consult-project-imenu'
  (defun consult-imenu-all (&optional arg)
    "Call `consult-imenu'. With prefix-command ARG, call
    `consult-project-imenu'."
    (interactive "P")
    (if arg (consult-imenu-multi) (consult-imenu)))
  
  ;; From https://github.com/minad/consult/wiki
  (defun my/consult-ripgrep-or-line (&optional initial start)
  "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
  (interactive (list nil (not (not current-prefix-arg))))
  (if (or (not buffer-file-name)
          (buffer-narrowed-p)
          (ignore-errors
            (file-remote-p buffer-file-name))
          (jka-compr-get-compression-info buffer-file-name)
          (<= (buffer-size)
              (/ my/consult-ripgrep-or-line-limit
                 (if (eq major-mode 'org-mode) 4 1))))
      (consult-line initial start)
    (when (file-writable-p buffer-file-name)
      (save-buffer))
    (let ((consult-ripgrep-args
           (concat "rg "
                   "--null "
                   "--line-buffered "
                   "--color=ansi "
                   "--max-columns=250 "
                   "--no-heading "
                   "--line-number "
                   ;; adding these to default
                   "--smart-case "
                   "--hidden "
                   "--max-columns-preview "
                   ;; add back filename to get parsing to work
                   "--with-filename "
                   ;; defaults
                   "-e "
                   (shell-quote-argument buffer-file-name))))
      (consult-ripgrep default-directory initial))))

  (defun consult-line-symbol-at-point ()
  (interactive)
  (my/consult-ripgrep-or-line (thing-at-point 'symbol)))

  ;; ;; This is obviated by consult-yank-pop.
  ;; (defun my/consult-yank-or-yank-pop (&optional arg)
  ;;   "Call `consult-yank'. If called after a yank, call `yank-pop' instead."
  ;;   (interactive "*p")
  ;;   (if (eq last-command 'yank)
  ;;       (yank-pop arg)
  ;;     (consult-yank)))

;;   (defun my/consult-find-multi-dir (dirlist &optional prompt initial)
;;     "Search for regexp with find in directories in DIRLIST with INITIAL input.

;; The find process is started asynchronously, similar to `consult-find'."
;;     (interactive "P")
;;     (let ((consult-find-command (concat "fd --hidden -t f -t d -t l --follow "
;;                                         (mapconcat (lambda (dir)
;;                                                      (concat "--search-path "
;;                                                              (file-name-as-directory dir)))
;;                                                    '("~/Documents" "~/Dropbox") " ")
;;                                         " ARG")))
;;       (consult--find (or prompt "Find: ") consult-find-command initial)))

  (use-package org
  :defer
  :bind (:map org-mode-map
         ("C-c C-j" . consult-org-heading)
         ("M-s M-j" . consult-org-heading)))

  :bind (("C-x b"   . consult-buffer)
         ("C-x H-r" . consult-recent-file)
         ("C-x M-:" . consult-complex-command)
         ("M-s M-o" . consult-multi-occur)
         ("M-X" . consult-mode-command)
         ("C-c C-j" . consult-outline)
         ("M-s M-j" . consult-outline)
         ("M-s l"   . consult-line-symbol-at-point)
         ("M-s f"   . consult-fd)
         ("M-s M-l" . consult-locate)
         ("M-s g"   . consult-ripgrep)
         ("M-s G"   . consult-git-grep)
         ("C-x C-r" . consult-recent-file)
         ("<help> a" . consult-apropos)
         ("M-s i" . consult-imenu-all)
         ("s-b" . consult-buffer)
         ("M-g f" . consult-flymake)
         ("M-g j" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ;; ("H-b" . consult-buffer)
         ("M-m" . consult-register-store)
         ("M-s k l" . consult-focus-lines)
         ("M-'" . consult-register-load)
         ("M-y" . consult-yank-pop)
         :map ctl-x-r-map
         ("b" . consult-bookmark)
         ("x" . consult-register)
         :map ctl-x-4-map
         ("b" . consult-buffer-other-window)
         :map ctl-x-5-map
         ("b" . consult-buffer-other-frame)
         :map tab-prefix-map
         ("b" . consult-buffer-other-tab)
         :map space-menu-file-map
         ("l" . consult-locate)
         :map minibuffer-local-map
         ("C-r" . consult-history)))

(use-package consult-dir
  :load-path "~/.local/share/git/consult-dir"
  :defer 2
  :after (consult bookmark marginalia)
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-M-d" . consult-dir-maybe)
         ("H-M-d" . consult-dir-maybe)
         ("C-M-j" . consult-dir-jump-file)
         ("H-M-j" . consult-dir-jump-file)
         ("M-s f" . consult-dir-jump-file)
         :map embark-become-file+buffer-map
         ("d" . consult-dir))
  :config
  (setq consult-dir-shadow-filenames nil)
  (defun consult-dir-maybe ()
    (interactive)
    (let* ((full-category (completion-metadata-get (embark--metadata) 'category))
           (category (pcase full-category
                       ('consult-multi (car (get-text-property
                                             0 'consult-multi
                                             (vertico--candidate))))
                       (_ full-category))))
      (if (member category '(file))
          (call-interactively #'consult-dir)
        (call-interactively (lookup-key global-map (kbd "C-M-d"))))))
  (use-package vertico
    :defer
    :bind (:map vertico-map
                ("C-M-d" . consult-dir-maybe)
                ("H-M-d" . consult-dir-maybe)
                ("M-s f" . consult-dir-jump-file)
                ("C-M-j" . consult-dir-jump-file)
                ("H-M-j" . consult-dir-jump-file))))

(use-package affe
  :ensure t
  :bind (("M-s M-f" . affe-find)
         ("M-s M-g" . affe-grep))
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)
    ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "C-M-m")))

(provide 'setup-consult)
