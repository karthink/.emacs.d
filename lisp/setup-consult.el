;; -*- lexical-binding: t -*-

;; Consult built-in options
(use-package consult
  :straight t
  ;; :hook (minibuffer-setup . consult-completion-enable-in-minibuffer)
  ;; :hook ((shell-mode eshell-mode) . (lambda () (setq completion-in-region-function
  ;;                                               #'consult-completion-in-region)))
  :init
  ;; (defun consult-completion-enable-in-minibuffer ()
  ;;     "Enable consult-completion-in-region in the minibuffer if
  ;; `completion-at-point' is bound."
  ;;     (when (where-is-internal #'completion-at-point (list (current-local-map)))
  ;;       ;; (setq-local corfu-auto nil) Enable/disable auto completion
  ;;       (setq completion-in-region-function #'consult-completion-in-region)))
  :after minibuffer
  :config
  (setq consult-narrow-key "<")
  (setq consult-line-numbers-widen t)
  (setq consult-preview-key 'any)
  (consult-customize
   ;; consult-ripgrep consult-git-grep consult-grep consult-xref
   consult-bookmark consult--source-buffer consult-recent-file
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark consult--source-project-buffer
   consult-info
   :preview-key "C-M-m"
   consult-theme :preview-key (list :debounce 1.0 "C-M-m"))

  (when (executable-find "plocate")
    (setq consult-locate-args "plocate --ignore-case --existing --regexp"))
  
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  (setq register-preview-delay 1.0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  
  (use-package consult-flymake
    :bind ("M-g f" . consult-flymake))

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
         ("C-h C-m" . consult-minor-mode-menu)
         ("C-c C-j" . consult-outline)
         ("M-s M-j" . consult-outline)
         ("M-s M-l" . consult-locate)
         ("M-s g"   . consult-ripgrep)
         ("M-s G"   . consult-git-grep)
         ("C-x C-r" . consult-recent-file)
         ("<help> a" . consult-apropos)
         ("s-b" . consult-buffer)
         ("M-g j" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ;; ("H-b" . consult-buffer)
         ("M-m" . consult-register-store)
         ("M-s k l" . consult-focus-lines)
         ("M-'" . consult-register-load)
         ("M-y" . consult-yank-pop)
         ("C-x `" . consult-compile-error)
         :map help-map
         ("TAB" . consult-info)
         :map ctl-x-r-map
         ("b" . consult-bookmark)
         ("x" . consult-register)
         :map ctl-x-4-map
         ("b" . consult-buffer-other-window)
         :map ctl-x-5-map
         ("b" . consult-buffer-other-frame)
         :map space-menu-file-map
         ("l" . consult-locate)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         :map project-prefix-map
         ("b" . consult-project-buffer)))

;; Consult extra commands and add-ons
(use-package consult
  :defer
  :bind
  (("M-s l"   . my/consult-ripgrep-or-line)
   ("M-s f"   . consult-fd)
   ("M-s i" . consult-imenu-all)
   ("C-x '" . my/consult-mark)
   :map tab-prefix-map
   ("b" . consult-buffer-other-tab)
   :map isearch-mode-map
   ("M-s l" . consult-line))
  :config
  
  (use-package orderless
    :config
    (defun my/orderless-dollar-dispatcher (pattern _index _total)
      (when (string-suffix-p "$" pattern)
        (cons 'orderless-regexp
              (format "%s[%c-%c]*$"
                      (substring pattern 0 -1)
                      consult--tofu-char
                      (+ consult--tofu-char consult--tofu-range -1)))))

    (add-to-list 'orderless-style-dispatchers 'my/orderless-dollar-dispatcher))
  
  (defun consult-buffer-other-tab ()
    "Variant of `consult-buffer' which opens in other frame."
    (interactive)
    (let ((consult--buffer-display #'switch-to-buffer-other-tab))
      (consult-buffer)))

  ;; Combine `consult-imenu' and `consult-project-imenu'
  (defun consult-imenu-all (&optional arg)
    "Call `consult-imenu'. With prefix-command ARG, call
    `consult-project-imenu'."
    (interactive "P")
    (if arg (consult-imenu-multi) (consult-imenu)))
  
  ;; From https://github.com/minad/consult/wiki
  (defcustom my/consult-ripgrep-or-line-limit 300000
    "Buffer size threshold for `my/consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
    :group 'consult
    :type 'integer)

  (defun my/consult-mark (&optional arg)
    (interactive "P")
    (if arg (consult-global-mark) (consult-mark)))
  
  (defun my/consult-ripgrep-or-line (&optional arg)
    "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
    ;; (interactive (list nil (not (not current-prefix-arg))))
    (interactive "p")
    (if (or (not buffer-file-name)
            (buffer-narrowed-p)
            (ignore-errors
              (file-remote-p buffer-file-name))
            (jka-compr-get-compression-info buffer-file-name)
            (<= (buffer-size)
                (/ my/consult-ripgrep-or-line-limit
                   (if (eq major-mode 'org-mode) 4 1))))
        (pcase arg
          (4 (consult-line-multi nil))
          (16 (consult-line-multi t))
          (_ (consult-line)))
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
        (let ((current-prefix-arg (or arg nil)))
          (call-interactively #'consult-ripgrep)))))

  (defvar consult--fd-command (or (executable-find "fdfind")
                                  (executable-find "fd")))
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command "fd"))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended nil)))
      (when re
        (cons (append
               (list consult--fd-command
                     "--color=never" "--full-path" "--hidden"
                     (consult--join-regexps re 'extended))
               opts)
              hl))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                 (default-directory dir))
      (find-file (consult--find prompt #'consult--fd-builder initial))))

  (dolist (func '(consult-fd consult-git-grep
                  consult-ripgrep consult-grep))
    (advice-add func :before (defun my/mark-jump-point (&rest _)
                               (xref-push-marker-stack)
                               (push-mark)))))

;; Attempt to use consult-ripgrep-all
(use-package consult
  :defer
  :when (executable-find "rga")
  :bind (("M-s M-g" . consult-ripgrep-all))
  :config
  (defun consult-ripgrep-all (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-args
           "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number ."))
      (consult--grep "Ripgrep All" #'consult--ripgrep-make-builder dir initial))))

;; Library support for consult-buffer
(use-package consult
  :defer
  :config
  (defvar consult--source-library nil
    "Emacs libary candidate source for `consult-buffer'.")

  (defvar consult--library-hash nil
    "Hash table of all library names.")
  
  (defun consult--library-make-hash ()
    "Return hash table of all library names."
    (require 'find-func)
    (setq consult--library-hash 
          (consult--string-hash
           (let ((suffix-regexp (mapconcat
                                 (lambda (suffix)
                                   (concat (regexp-quote suffix) "\\'"))
                                 (find-library-suffixes)
                                 "\\|")))

             (cl-loop for dir in (or find-library-source-path load-path)
                      when (file-readable-p dir)
                      append (mapcar
                              (lambda (file)
                                (replace-regexp-in-string suffix-regexp
                                                          "" file))
                              (directory-files dir nil
                                               suffix-regexp)))))))
  
  (defvar consult--library-history nil)
  
  (consult--define-state library)
  (defun consult--library-action (lib)
    "Find library via `consult--library-action'."
    (consult--buffer-action
     (find-file-noselect
      (find-library-name lib)))
    (run-hooks 'find-function-after-hook))
  
  (defun consult--library-preview ()
    "Create preview function for libraries."
    (let ((open (consult--temporary-files))
          (preview (consult--buffer-preview)))
      (lambda (action cand)
        (unless cand (funcall open))
        (funcall preview action
                 (and cand
                      (eq action 'preview)
                      (funcall open (find-library-name cand)))))))

  (setq consult--source-library
    `(:name  "Library"
      :narrow ?l
      :category library
      :face consult-buffer
      :history consult--library-history
      :hidden t
      :items ,(lambda ()
                (let ((ht consult--library-hash))
                  (unless ht (setq ht (consult--library-make-hash)))
                  (hash-table-keys ht)))
      :state ,#'consult--library-state))
  (add-to-list 'consult-buffer-sources 'consult--source-library)
  (consult-customize consult--source-library :preview-key "C-M-m"))

(use-package consult-dir
  :load-path "plugins/consult-dir/"
  ;; :commands (consult-dir consult-dir-maybe)
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-filename-completion-map
         ("C-M-d" . consult-dir)
         ("H-M-d" . consult-dir)
         ("C-M-j" . consult-dir-jump-file)
         ("H-M-j" . consult-dir-jump-file)
         ("M-s f" . consult-dir-jump-file)
         :map embark-become-file+buffer-map
         ("d" . consult-dir))
  :init
  (use-package vertico
    :bind (:map vertico-map
           ("C-M-d" . consult-dir)
           ("H-M-d" . consult-dir)
           ("M-s f" . consult-dir-jump-file)
           ("C-M-j" . consult-dir-jump-file)
           ("H-M-j" . consult-dir-jump-file)))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (setq consult-dir-shadow-filenames nil))

;; I never use this, disabling in favor of consult-grep et al.
(use-package affe
  :disabled
  :straight t
  :bind (("M-s M-f" . affe-find)
         ("M-s M-g" . affe-grep))
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)
    ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "C-M-m")))

;; Disable in favor of consult-recoll
(use-package consult
  :disabled
  :when (executable-find "ff-cache")
  :bind ("M-s /" . consult-dff)
  :config
  (defvar dff-cache-file "/tmp/dmenufindfile.cache")
  (defvar dff-file-name-history nil)
  (defvar dff-cache nil)
  (defun consult-dff (&optional arg)
    (interactive "P")
    (when-let
        ((default-directory (getenv "HOME"))
         (file-name
           (consult--read
            (if (or arg (not dff-cache))
                (setq dff-cache
                      (progn
                        (shell-command "ff-cache -r")
                        (with-temp-buffer
                          (insert-file-contents dff-cache-file)
                          (split-string (buffer-substring (point-min) (point-max)) "\n"))))
              dff-cache)
            :prompt "Find file: "
            :require-match t
            ;; :state (consult--file-state)
            :history 'dff-file-name-history
            :category 'file)))
      (find-file file-name)))
  (consult-customize consult-dff :preview-key "C-M-m"))

(use-package consult-recoll
  :straight t
  :bind ("M-s /" . consult-recoll)
  :config
  (setq consult-recoll-inline-snippets nil)
  (setf (alist-get "application/epub+zip"
                   consult-recoll-open-fns
                   nil nil #'equal)
        #'find-file)
  (consult-customize consult-recoll :preview-key "C-M-m"))


(provide 'setup-consult)
;; setup-consult.el ends here
