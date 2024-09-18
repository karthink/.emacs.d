(use-package calc
  :bind (("C-x c" . calc)
         ("C-S-e" . latex-math-from-calc)
         ("C-c e" . calc-embedded))
  :config
  (use-package calc-embed
    :bind (:map calc-override-minor-modes-map
           ("'" . calc-algebraic-entry)))
  (use-package calc-yank
    :defer
    :config
    (define-advice calc-finish-stack-edit (:around (orig-fn &rest args) pop-to-buffer)
      (cl-letf (((symbol-function 'switch-to-buffer)
                 #'pop-to-buffer))
        (apply orig-fn args))))
  (setq calc-make-windows-dedicated t)
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (let ((lang (if (memq major-mode '(org-mode latex-mode LaTex-mode))
                  'latex 'normal)))
      (cond ((region-active-p)
             (let* ((beg (region-beginning))
                    (end (region-end))
                    (string (buffer-substring-no-properties beg end)))
               (kill-region beg end)
               (insert (calc-eval `(,string calc-language ,lang
                                            calc-prefer-frac t
                                            calc-angle-mode rad)))))
            (t (let ((l (thing-at-point 'line)))
                 (end-of-line 1) (kill-line 0)
                 (insert (calc-eval `(,l
                                      calc-language ,lang
                                      calc-prefer-frac t
                                      calc-angle-mode rad)))))))))

(use-package calc
  :after org-latex-preview
  :hook (calc-mode . my/olp-calc-settings)
  :config
  (defun my/olp-calc-settings ()
    (setq-local org-latex-preview-numbered nil
                org-latex-preview-auto-ignored-commands
                '(mwheel-scroll pixel-scroll-precision
                  scroll-up-command scroll-down-command
                  scroll-other-window scroll-other-window-down))))



(provide 'setup-calc)
