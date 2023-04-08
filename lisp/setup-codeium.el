(use-package codeium
  :after cape
  :straight '(:type git :host github
              :repo "Exafunction/codeium.el")
  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'completion-at-point-functions
  ;;                       #'codeium-completion-at-point
  ;;                       -40 t)))
  
  :bind ("C-x ;" . my/codeium-complete)
  :config
  (defalias 'my/codeium-complete
    (cape-interactive-capf #'codeium-completion-at-point))
  (setq codeium/metadata/api_key
        (auth-source-pass-get 'secret "api/codeium"))
  (setq codeium-command-executable
        (expand-file-name
         (pcase system-type
           ('windows-nt "codeium_language_server.exe")
           (_ "codeium_language_server"))
         (expand-file-name "codeium" user-cache-directory)))
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest
                      GetAuthToken RegisterUser auth-redirect
                      AcceptCompletion))))
  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties
     (max (- (point) 3000) (point-min))
     (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties
      (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset
        'my-codeium/document/cursor_offset))

(provide 'setup-codeium)
