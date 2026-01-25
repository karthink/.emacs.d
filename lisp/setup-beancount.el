;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; ** BEANCOUNT
;;----------------------------------------------------------------
(use-package beancount
  :ensure (:host github :repo "beancount/beancount-mode")
  :defer
  :init
  (add-to-list 'popper-reference-buffers 'bean-query-mode)
  (setf (alist-get '(major-mode . bean-query-mode)
                   display-buffer-alist nil nil #'equal)
        '((display-buffer-below-selected
           display-buffer-pop-up-window)
          (window-height . 0.5)
          (post-command-select-window . t)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(beancount-mode . ("beancount-language-server" "--stdio")))))

(provide 'setup-beancount)
