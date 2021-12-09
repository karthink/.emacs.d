(use-package mct
:defer
:bind (:map mct-completion-list-mode-map
       ("C-M-n" . mct-next-completion-group)
       ("C-M-p" . mct-previous-completion-group)
       ("C-M-l" . embark-export)
       :map mct-minibuffer-local-completion-map
       ("C-M-l" . embark-export)
       :map mct-minibuffer-local-filename-completion-map
       ("C-M-l" . embark-export))
:config
(setq mct-apply-completion-stripes t
      mct-completions-format 'one-column)
(setq mct-completion-blocklist '()
      mct-completion-passlist
      '(consult-line consult-outline 
        ;; consult-line-symbol-at-point 
        consult-completion-in-region             
        consult-imenu consult-imenu-all
        consult-xref consult-org-heading
        embark-completing-read-prompter
        embark-act-with-completing-read
        embark-act
        embark-prefix-help-command
        consult-yank-pop))
(setq mct-display-buffer-action
      (quote ((display-buffer-reuse-window
               display-buffer-at-bottom)
              (window-height . (lambda (win) (fit-window-to-buffer win
                                         (floor (frame-height) 3))))))))
