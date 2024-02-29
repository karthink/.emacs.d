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
  ;; (setq orderless-style-dispatchers
  ;;       '(my/orderless-flex-dispatcher
  ;;         my/orderless-literal-dispatcher
  ;;         my/orderless-initialism-dispatcher
  ;;         my/orderless-exclude-dispatcher))

  (defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  ;; (defun my/orderless-flex-dispatcher (pattern _index _total)
  ;;   (when (or (string-suffix-p "`" pattern)
  ;;             (string-suffix-p "~" pattern))
  ;;     `(orderless-flex . ,(substring pattern 0 -1))))

  ;; (defun my/orderless-literal-dispatcher (pattern _index _total)
  ;;   (when (string-suffix-p "=" pattern)
  ;;     `(orderless-literal . ,(substring pattern 0 -1))))

  ;; (defun my/orderless-initialism-dispatcher (pattern _index _total)
  ;;   (when (string-suffix-p "," pattern)
  ;;     `(orderless-initialism . ,(substring pattern 0 -1))))

  ;; (defun my/orderless-exclude-dispatcher (pattern _index _total)
  ;;   (when (string-prefix-p "!" pattern)
  ;;     `(orderless-without-literal . ,(substring pattern 1))))
  
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch
                                   my/orderless-initialism-dispatcher
                                   my/orderless-flex-dispatcher))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))
  
;;   ;; These were removed from Orderless, I use them.
;;   (defun orderless--strict-*-initialism (component &optional anchored)
;;     "Match a COMPONENT as a strict initialism, optionally ANCHORED.
;; The characters in COMPONENT must occur in the candidate in that
;; order at the beginning of subsequent words comprised of letters.
;; Only non-letters can be in between the words that start with the
;; initials.
;; If ANCHORED is `start' require that the first initial appear in
;; the first word of the candidate.  If ANCHORED is `both' require
;; that the first and last initials appear in the first and last
;; words of the candidate, respectively."
;;     (orderless--separated-by
;;      '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
;;      (cl-loop for char across component collect `(seq word-start ,char))
;;      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
;;      (when (eq anchored 'both)
;;        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

;;   (defun orderless-strict-initialism (component)
;;     "Match a COMPONENT as a strict initialism.
;; This means the characters in COMPONENT must occur in the
;; candidate in that order at the beginning of subsequent words
;; comprised of letters.  Only non-letters can be in between the
;; words that start with the initials."
;;     (orderless--strict-*-initialism component))

;;   (defun orderless-strict-leading-initialism (component)
;;     "Match a COMPONENT as a strict initialism, anchored at start.
;; See `orderless-strict-initialism'.  Additionally require that the
;; first initial appear in the first word of the candidate."
;;     (orderless--strict-*-initialism component 'start))

;;   (defun orderless-strict-full-initialism (component)
;;     "Match a COMPONENT as a strict initialism, anchored at both ends.
;; See `orderless-strict-initialism'.  Additionally require that the
;; first and last initials appear in the first and last words of the
;; candidate, respectively."
;;     (orderless--strict-*-initialism component 'both))
  
  :bind (:map minibuffer-local-completion-map
              ("SPC" . self-insert-command)))

(provide 'setup-orderless)
;; setup-orderless.el ends here
