(use-package julia-mode
  :straight t
  :bind (:map julia-mode-map
              ("`" . my/julia-latexsub-or-indent))
  :config
  (defun my/julia-latexsub-or-indent ()
    (interactive)
    (require 'cdlatex nil t)
    (cl-letf (((symbol-function 'texmathp)
               (lambda () t)))
      (cdlatex-math-symbol)
      (julia-latexsub-or-indent))))

(use-package julia-repl
  :straight t
  :commands julia-repl-mode
  :config
  (julia-repl-set-terminal-backend 'vterm))

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
  :mode ("\\.jl\\'" . ess-julia-mode)
  :config
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
