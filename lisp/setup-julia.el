(use-package julia-mode
  :ensure t
  :mode ("\\.jl\\'" . julia-mode)
  :bind (:map julia-mode-map
              ("`" . my/julia-latexsub-or-indent))
  :config
  (add-hook 'julia-mode-hook
            (defun my/julia-mode-settings ()
              (setq-local outline-regexp "^##+")
              (remove-hook 'completion-at-point-functions
                           #'julia-mode-latexsub-completion-at-point-before
                           'local)
              (remove-hook 'completion-at-point-functions
                           #'julia-mode-latexsub-completion-at-point-around
                           'local)
              (outline-minor-mode 1)))
  ;; (add-to-list 'julia-arguments "-t12")
  (setq julia-automatic-latexsub nil)
  (defun my/julia-latexsub-or-indent ()
    (interactive)
    (require 'cdlatex nil t)
    (cl-letf (((symbol-function 'texmathp) #'always)
              (inhibit-redisplay t))
      (cdlatex-math-symbol)
      ;; (julia-latexsub-or-indent 0)
      (expand-abbrev)
      ;; (completion-at-point)
      )))

(use-package julia-snail
  :ensure (:remotes ("copy" :host github :protocol ssh
                     :repo "karthink/julia-snail"))
  :hook (julia-mode . julia-snail-mode)
  :bind (:map julia-snail-mode-map
         ("C-h ." . my/julia-snail-doc-at-point)
         :map julia-snail-repl-mode-map
         ("C-c C-d" . julia-snail-doc-lookup)
         ("C-h ." . my/julia-snail-doc-at-point))
  :config
  ;; (with-eval-after-load 'julia-snail/ob-julia
  ;;   (setq-default
  ;;    julia-snail/ob-julia-capture-io nil
  ;;    julia-snail/ob-julia-use-error-pane t))
  (setenv "JULIA_PKG_PRESERVE_TIERED_INSTALLED" "true")
  (defun my/julia-snail-doc-at-point ()
    (interactive)
    (let ((win (selected-window)))
      (julia-snail-doc-lookup
       (julia-snail--identifier-at-point))
      (and (window-live-p win)
          (select-window win))))
  (setq julia-snail-terminal-type :vterm)
  (setq-default
   julia-snail-extra-args '("--threads=auto,auto" "-q")
   julia-snail-multimedia-enable t
   julia-snail-multimedia-buffer-style :single-new
   julia-snail-show-error-window t))

(use-package julia-repl
  :disabled
  :ensure t
  :commands julia-repl-mode
  :bind (:map julia-repl-mode-map
         ("C-c C-p" . nil)
         ("C-c p" . julia-repl-cd))
  :config
  (make-variable-buffer-local 'julia-repl-inferior-buffer-name-suffix)
  (add-hook 'julia-repl-hook
            (defun my/julia-repl-settings ()
              (setq-local
               term-prompt-regexp
               "^\\(julia\\|help\\?\\|[)@(.pkgv0-9 ]+\\)> *")))
  (julia-repl-set-terminal-backend 'vterm)
  (setq julia-repl-switches "-t12 -q"))

(use-package eglot-jl
  :ensure t
  :commands eglot-jl-init
  :config
  (cl-defmethod project-root ((project (head julia)))
    (cdr project))
  ;; Workaround until LanguageServer.jl is fixed
  (setq eglot-jl-language-server-project
        (expand-file-name "eglot-jl-project" user-cache-directory)))

;;;----------------------------------------------------------------
;; ** ESS
;;;----------------------------------------------------------------
;; Need this for ob-julia -- disabled while I try julia-snail
(use-package ess-julia
  :disabled
  :ensure ess
  :after ob-julia
  ;; :bind (:map ess-julia-mode-map
  ;;        ("`" . my/ess-julia-cdlatex-symbol)
  ;;        :map inferior-ess-julia-mode-map
  ;;        ("`" . my/ess-julia-cdlatex-symbol))
  :config
  (setq auto-mode-alist
        (delete '("\\.jl\\'" . ess-julia-mode)
                auto-mode-alist))
  (setq inferior-julia-args "-t12 -q")
  (defun my/ess-julia-cdlatex-symbol ()
    (interactive)
    (require 'cdlatex)
    (cl-letf (((symbol-function 'texmathp)
               (lambda () t)))
      (cdlatex-math-symbol))
    (call-interactively 'completion-at-point)
    (forward-sexp))
  (define-key ess-julia-mode-map (kbd "`") 'my/ess-julia-cdlatex-symbol)
  (define-key inferior-ess-julia-mode-map (kbd "`") 'my/ess-julia-cdlatex-symbol))

;; ESS DONT
;; Disabled in favor of julia-snail
(use-package ess
  :disabled
  :ensure t
  :after ob-julia
  :config
  (use-package ess-julia))

;;;----------------------------------------------------------------
;; ** OB-JULIA
;;;----------------------------------------------------------------
(use-package ob-julia
  :ensure (:host github :repo "nico202/ob-julia"
                 :files ("*.el" "julia")
                 :remotes ("fork" :host github :repo "karthink/ob-julia"
                           :branch "main" :protocol ssh))
  :after (ob org)
  :autoload (org-babel-execute:julia org-babel-expand-body:julia
                                     org-babel-prep-session:julia)
  :hook (org-babel-julia-after-async-execute . my/org-redisplay-babel-result)
  :init
  ;; (setq ob-julia-insert-latex-environment-advice nil)
  (add-to-list 'org-structure-template-alist '("j" . "src julia"))
  :config
  (when (featurep 'julia-snail) (require 'ob-julia-snail))
  (setq org-babel-default-header-args:julia
        '((:session . nil)
          (:async   . "yes")))
  (setq org-babel-julia-backend 'julia-snail)
  (when (featurep 'ess)
    (setq ess-eval-visibly 'nowait)
    (defun org-babel-julia-initiate-session (&optional session params)
      "Create or switch to an ESS Julia session.

Return the initialized session, if any."
      (unless (string= session "none")
        (let ((session (or session "*julia*")))
          (if (org-babel-comint-buffer-livep session)
              session
            (save-window-excursion
              (org-babel-prep-session:julia session params))))))))

(provide 'setup-julia)
