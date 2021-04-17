(use-package embark
  :ensure t
  :after minibuffer
  :bind (:map minibuffer-local-completion-map
              ("M-o" . embark-exit-and-act)
              ("C-M-o" . embark-act)
              ("C-c C-o" . embark-occur)
              :map embark-occur-mode-map
              ("M-o" . embark-exit-and-act)
              ("C-M-o" . embark-act)
              :map completion-list-mode-map
              ("M-o" . embark-exit-and-act)
              ("C-M-o" . embark-act)
              ("C-c C-o" . embark-occur)
              :map embark-general-map
              ("M-o" . embark-occur)
              :map embark-file-map
              ("x" . embark-open-extern)) 
  :config
  (define-key minibuffer-local-completion-map (kbd "C-M-o") 'embark-occur)
  (define-key embark-file-map (kbd "x") 'embark-open-extern)
  (defun embark-open-extern ()
    "Open externally"
    (interactive)
    (call-process-shell-command (format "%s %s"
                                        (cl-case system-type
                                          (darwin "open")
                                          (cygwin "cygstart")
                                          (t "xdg-open"))
                                        (shell-quote-argument (embark-target)))
                                nil 0))
  (eval-after-load 'which-key 
    (advice-add #'embark--start
                :after
                (defun embark-wk-help+ (&rest _)
                  (let ((which-key-show-transient-maps t))
                    (which-key--update)))))

  (defun embark-act-and-exit ()
    "Run embark action, then exit minibuffer"
    (interactive)
    (when (minibufferp)
      (embark-act)
      (top-level)))
  )

(use-package live-completions
  :disabled
  :after minibuffer
  :commands live-completions-mode
  :load-path "~/.local/share/git/melpa/live-completions/"
  :hook (live-completions-mode . temp-buffer-resize-mode)
  :bind (:map minibuffer-local-completion-map
              ( "M-q" . 'live-completions-set-columns))
  :config
  (setq live-completions-sort-order 'cycle)
  (defvar old-max-height-function temp-buffer-max-height)

  (defun max-completions-height (buffer)
    (if (string= (buffer-name buffer) "*Completions*")
        (/ (frame-height) 3)
      (funcall old-max-height-function temp-buffer-max-height)))

  (setq temp-buffer-max-height #'max-completions-height)
)

(provide 'setup-embark)

