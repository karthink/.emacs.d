;; -*- lexical-binding: t -*-
;; Orderless
(use-package orderless
  :after setup-minibuffer
  :ensure t
  :demand
  :config
  (setq orderless-component-separator #'split-string-and-unquote)
  (setq completion-styles '(orderless partial-completion))
  (setq orderless-matching-styles
        '(orderless-regexp
          orderless-strict-leading-initialism)
        orderless-style-dispatchers
        '(my/orderless-flex-dispatcher
          my/orderless-literal-dispatcher
          my/orderless-initialism-dispatcher
          my/orderless-exclude-dispatcher
          my/orderless-dollar-dispatcher))

  (defun my/orderless-dollar-dispatcher (pattern _index _total)
    (when (string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                    "[\x100000-\x10FFFD]*$"))))
    
  (defun my/orderless-flex-dispatcher (pattern _index _total)
    (when (or (string-suffix-p "`" pattern)
              (string-suffix-p "~" pattern))
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun my/orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun my/orderless-initialism-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-strict-full-initialism . ,(substring pattern 0 -1))))

  (defun my/orderless-exclude-dispatcher (pattern _index _total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))

  :bind (:map minibuffer-local-completion-map
              ("SPC" . self-insert-command)))

(provide 'setup-orderless)
