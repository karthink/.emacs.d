;; -*- lexical-binding: t; -*-

(use-package lua-mode :ensure t :defer)

;; *** FENNEL
(use-package fennel-mode
  :ensure t
  :defer
  :config
  (setq
   fennel-program
   "~/dotfiles/mpv/.config/mpv/fnl/fennel-1.0.0"))

(provide 'setup-lua)
