;; -*- lexical-binding: t; -*-

(use-package cider
  :defer
  :init
  (dolist (mode '(cider-mode-hook cider-repl-mode-hook))
    (add-hook mode #'my/cider-comp-styles)
    (add-hook mode #'company-mode)
    (add-hook mode #'smartparens-mode))
  :config
  (defun my/cider-comp-styles ()
    (make-variable-buffer-local 'completion-styles)
    (add-to-list 'completion-styles 'basic)))

(provide 'setup-clojure)
