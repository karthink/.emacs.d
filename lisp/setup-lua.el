;; -*- lexical-binding: t; -*-

(use-package lua-mode :straight t :defer)
(use-package eglot-lua
  :straight (:host github
             :repo "juergenhoetzel/eglot-lua")
  :defer
  :config
  (setq eglot-lua-server-install-dir
                     (concat
                      (file-name-as-directory user-cache-directory)
                      "EmmyLua-LanguageServer/")))

;; *** FENNEL
(use-package fennel-mode
  :straight t :defer
  :config
  (setq
   fennel-program
   "~/dotfiles/mpv/.config/mpv/fnl/fennel-1.0.0"))

(provide 'setup-lua)
