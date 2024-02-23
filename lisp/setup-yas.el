
(use-package yasnippet
  :ensure t
  ;; :defer 5
  ;; :after warnings
  :hook (((prog-mode LaTeX-mode org-mode
           eval-expression-minibuffer-setup)
          . yas-minor-mode)
         (yas-minor-mode . my/yas-auto-setup))
  :config
  ;; (use-package yasnippet-snippets
  ;;   :straight t)
  ;; (yas-reload-all)
  ;; Redefine yas expand key from TAB because company-mode uses TAB.

  ;; (push '(yasnippet backquote-change) warning-suppress-types)

  ;; Don't throw a warning if lisp code in a snippet modifies the
  ;; buffer. We need this for auto expanded snippets in latex/org.

  (let ((ydus yas--default-user-snippets-dir))
    (and (member ydus yas-snippet-dirs)
         (yas-load-directory ydus)))

  (setq yas-wrap-around-region t
        yas-triggers-in-field t)

  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  (defun my/yas-auto-setup ()
    (add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets nil t))

  (with-eval-after-load 'corfu
    (defun my/yas-corfu-cancel ()
      "company-abort or yas-abort-snippet."
      (interactive)
      (if corfu--candidates
          (corfu-quit)
        (yas-abort-snippet)))
    (define-key yas-keymap (kbd "C-g") #'my/yas-corfu-cancel))

(with-eval-after-load 'company
  ;; (defun my/yas-company-next-field ()
  ;;     "company-complete-common or yas-next-field-or-maybe-expand."
  ;;     (interactive)
  ;;     (if company-candidates (company-complete-common)
  ;;       (yas-next-field-or-maybe-expand)))

  ;;   (define-key yas-keymap [tab] #'my/yas-company-next-field)
  ;;   (define-key yas-keymap (kbd "TAB") #'my/yas-company-next-field)

;;;###autoload
    (defun my/yas-company-cancel ()
      "company-abort or yas-abort-snippet."
      (interactive)
      (if company-candidates
          (company-abort)
        (yas-abort-snippet)))

    (define-key yas-keymap (kbd "C-g") #'my/yas-company-cancel))


  ;; (when (fboundp 'smartparens)
  ;;   (with-eval-after-load 'smartparens
  ;;     (defvar yas--smartparen-flag nil)
  ;;     (add-hook 'yas-before-expand-snippet-hook (lambda () (when smartparens-mode
  ;;                                                       (smartparens-mode -1)
  ;;                                                       (setq-local yas--smartparen-flag t))))
  ;;     (add-hook 'yas-after-exit-snippet-hook (lambda () (when yas--smartparen-flag)
  ;;                                              (smartparens-mode +1)
  ;;                                              (setq-local yas--smartparen-flag nil)))))

  ;; (define-key yas-minor-mode-map (kbd "S-SPC") (lambda (&optional num) (interactive "P")
  ;;                                                (or (yas-expand)
  ;;                                                    (insert (kbd "SPC")))))
  ;; (define-key yas-keymap (kbd "S-SPC") (lambda (&optional num) (interactive "P")
  ;;                                        (or (yas-next-field-or-maybe-expand)
  ;;                                            (insert (kbd "SPC")))))
  ;; (dolist (keymap (list yas-minor-mode-map yas-keymap))
  ;;   (define-key keymap (kbd "TAB") nil)
  ;;   (define-key keymap [(tab)] nil))
  ;; (global-set-key (kbd "M-S-SPC") 'company-yasnippet)
  )

(use-package yasnippet-snippets
  :ensure (:post-build
           (let ((default-directory
                  (file-name-as-directory
                   (file-name-concat
                    elpaca-builds-directory
                    "yasnippet-snippets"
                    "snippets"
                    "latex-mode"))))
             (with-temp-buffer
               (write-file ".yas-skip"))))
  :after yasnippet)

(use-package yasnippet-capf
  :ensure (:host github :repo "elken/yasnippet-capf")
  :after yasnippet
  :config
  ;; (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

(use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change) warning-suppress-types
                :test 'equal))

(use-package yasnippet
  :defer
  :config
  (use-package cdlatex
    :hook ((cdlatex-tab . yas-expand)
           (cdlatex-tab . cdlatex-in-yas-field))
    :bind (:map yas-keymap
                ("TAB" . yas-next-field-or-cdlatex)
                ([tab] . yas-next-field-or-cdlatex))
    :config
    ;; Allow cdlatex tab to work inside Yas fields
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    ;; (add-hook 'cdlatex-tab-hook #'yas-expand)
    ;; (add-hook 'cdlatex-tab-hook #'cdlatex-in-yas-field)
    ;; (define-key yas-keymap (kbd "TAB")
    ;; (define-key yas-keymap [tab] 'yas-next-field-or-cdlatex)
    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

(provide 'setup-yas)
