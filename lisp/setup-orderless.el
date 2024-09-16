;; -*- lexical-binding: t -*-

;; Orderless

(use-package orderless
  :after setup-minibuffer
  :ensure t
  :demand
  :config
  (setq orderless-component-separator #'split-string-and-unquote)
  (setq completion-styles '(orderless partial-completion basic))
  (setf (alist-get ?` orderless-affix-dispatch-alist) #'orderless-flex)

  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
       ;; `(orderless-regexp . ,(concat "^" (regexp-quote word)))
       (cons 'orderless-literal-prefix word)))
  
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch
                                   orderless-affix-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))
  
  :bind (:map minibuffer-local-completion-map
              ("SPC" . self-insert-command)))

(provide 'setup-orderless)
;; setup-orderless.el ends here
