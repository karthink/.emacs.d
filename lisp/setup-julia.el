(use-package julia-mode
  :straight t
  :mode ("\\.jl\\'" . julia-mode)
  :bind (:map julia-mode-map
              ("`" . my/julia-latexsub-or-indent))
  :config
  (add-hook 'julia-mode-hook
            (defun my/julia-mode-settings ()
              (setq-local outline-regexp "^##+")
              (outline-minor-mode 1)))
  (add-to-list 'julia-arguments "-t12")
  (defun my/julia-latexsub-or-indent ()
    (interactive)
    (require 'cdlatex nil t)
    (cl-letf (((symbol-function 'texmathp)
               (lambda () t)))
      (cdlatex-math-symbol)
      (julia-latexsub-or-indent 0))))

(use-package julia-repl
  :straight t
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
  :straight t
  :commands eglot-jl-init
  :config
  (cl-defmethod project-root ((project (head julia)))
    (cdr project))
  ;; Workaround until LanguageServer.jl is fixed
  (setq eglot-jl-language-server-project
        (dir-concat user-cache-directory "eglot-jl-project")))

;;;----------------------------------------------------------------
;; ** ESS
;;;----------------------------------------------------------------
;; Need this for ob-julia
(use-package ess-julia
  :straight ess
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

(provide 'setup-julia)
