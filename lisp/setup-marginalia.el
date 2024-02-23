;; -*- lexical-binding: t -*-

;; Prefer richer, more heavy, annotations over the lighter default variant.
;; E.g. M-x will show the documentation string additional to the keybinding. By
;; default only the keybinding is shown as annotation. Note that there is the
;; command `marginalia-cycle-annotators` to switch between the annotators.

(use-package marginalia
  :ensure t
  :after setup-minibuffer
  :init (marginalia-mode 1)
  :bind (:map vertico-map
         ("M-]" . marginalia-cycle))
  :config
  (pcase-dolist (`(,regexp . ,category)
                 '(("\\burl\\b" . url)
                   ("\\bHistory\\b" . history)
                   ("\\bdefinitions?\\b" . xref-location)
                   ("\\bxref\\b" . xref-location)))
    (setf (alist-get regexp marginalia-prompt-categories
                     nil nil #'equal)
          category)))

(provide 'setup-marginalia)
;; setup-marginalia.el ends here
