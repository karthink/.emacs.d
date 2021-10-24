;; -*- lexical-binding: t -*-

(use-package marginalia
  :ensure t
  :after setup-minibuffer
  :init (marginalia-mode 1)
  ;; :bind (:map vertico-map
  ;;        ("M-]" . marginalia-cycle))
  :config
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  ;; (marginalia-mode 1)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to 
  ;; switch between the annotators.
  (add-to-list 'marginalia-prompt-categories '("\\burl\\b" . url))
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light)))

(provide 'setup-marginalia)
