(use-package diff-mode
  :defer
  :general
  (:keymaps 'diff-mode-map
            :states 'motion
            "i" 'ignore
            "f" 'next-error-follow-minor-mode
            "q" 'quit-window)
  :config
  (use-package outline
    :hook (diff-mode . my/outline-mode-diff)
    :config
    (defun my/outline-mode-diff ()
      (setq-local outline-regexp "---\\|\\+\\+\\|@@ ")
      (outline-minor-mode 1))))

(use-package ediff
  :defer t
  :functions ediff-setup-windows-plain
  :hook ((ediff-prepare-buffer       . my/ediff-expand-outlines)
         (ediff-before-setup         . my/ediff-save-wconf-h)
         ((ediff-quit ediff-suspend) . my/ediff-restore-wconf-h))
  :init
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-keep-variants nil)
  :config
  (defun my/ediff-expand-outlines ()
    "If outline minor mode is active, expand the ediff buffers
fully before starting comparison."
    (when outline-minor-mode
      (outline-show-all)))
  (defvar my/ediff-saved-wconf nil)
  (defun my/ediff-save-wconf-h ()
    (setq my/ediff-saved-wconf (current-window-configuration)))
  (defun my/ediff-restore-wconf-h ()
    (when (window-configuration-p my/ediff-saved-wconf)
      (set-window-configuration my/ediff-saved-wconf))))

(provide 'setup-diff)
