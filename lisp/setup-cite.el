(use-package citar
  :straight t
  :after latex
  :defer
  :bind (:map LaTeX-mode-map
         ("C-c ]" . citar-insert-citation)
         :map org-mode-map
         ("C-c C-x ]" . citar-insert-citation))
  :config
  (setq citar-bibliography ;; '("~/Documents/research/control_systems.bib")
        '("~/Documents/roam/biblio.bib")
        citar-at-point-function 'embark-act
        citar-file-open-function #'consult-file-externally)
  
  (use-package cdlatex
    :config
    (defun my/cdlatex-bibtex-action ()
      "Call `citar-insert-citation' interactively."
      (call-interactively 'citar-insert-citation))
    (setf (alist-get "cite" cdlatex-command-alist nil nil 'equal)
          '("Make a citation interactively"
            "" my/cdlatex-bibtex-action nil t nil))
    (setf (alist-get "cite{" cdlatex-command-alist nil nil 'equal)
          '("Make a citation interactively"
            "cite{" my/cdlatex-bibtex-action nil t nil))))

(use-package citar-embark
  :straight t
  :after (citar embark)
  :config
  (citar-embark--enable))

(provide 'setup-cite)
