;; -*- lexical-binding: t -*-
;; Consult
(use-package consult
  :ensure t
  :hook (minibuffer-setup . consult-completion-enable-in-minibuffer)
  ;; :hook ((shell-mode eshell-mode) . (lambda () (setq completion-in-region-function
  ;;                                               #'consult-completion-in-region)))
  :init
  (defun consult-completion-enable-in-minibuffer ()
    "Enable consult-completion-in-region in the minibuffer if
`completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (setq completion-in-region-function #'consult-completion-in-region)))
  :after minibuffer
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
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark consult--source-project-buffer
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
  (dolist (func '(consult-fd consult-git-grep
                             consult-ripgrep consult-grep))
    (advice-add func :before (defun my/mark-jump-point (&rest _)
                               (xref-push-marker-stack)
                               (push-mark))))
  
  (use-package consult-flymake
    :bind ("M-g f" . consult-flymake)
    ;; :config
    ;; (advice-add 'consult-flymake :before
    ;;             #'my/consult-flymake-ensure)
    ;; (defun my/consult-flymake-ensure ()
    ;;   (interactive)
    ;;   (flymake-mode 1))
    )
  
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
         ("M-s l"   . consult-line-symbol-at-point)
         ("M-s f"   . consult-fd)
         ("M-s M-l" . consult-locate)
         ("M-s g"   . consult-ripgrep)
         ("M-s G"   . consult-git-grep)
         ("C-x C-r" . consult-recent-file)
         ("<help> a" . consult-apropos)
         ("M-s i" . consult-imenu-all)
         ("s-b" . consult-buffer)
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
         ("M-r" . consult-history)))

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

             (cl-loop for dir in (or find-function-source-path load-path)
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
      (lambda (cand restore)
        (if restore
            (progn
              (funcall preview nil t)
              (funcall open))
          (funcall
           preview
           (and cand (funcall open (find-library-name cand)))
           nil)))))

  (setq consult--source-library
    `(:name  "Library"
      :narrow ?l
      :category library
      :face consult-buffer
      :history 'consult--library-history
      :hidden t
      :items ,(lambda ()
                (let ((ht consult--library-hash))
                  (unless ht (setq ht (consult--library-make-hash)))
                  (hash-table-keys ht)))
      :state ,#'consult--library-state))
  (add-to-list 'consult-buffer-sources 'consult--source-library)
  (consult-customize consult--source-library :preview-key (kbd "C-M-m")))

(use-package consult-dir
  :load-path "plugins/consult-dir/"
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

(use-package consult
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
            :history 'dff-file-name-history
            :category 'file)))
      (find-file file-name))))

(provide 'setup-consult)
;; setup-consult.el ends here
