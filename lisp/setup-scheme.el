(cond ((string= system-type "gnu/linux")
       (setq scheme-program-name "mit-scheme"))
      
      ((or (string= system-type "windows-nt") (string= system-type "cygwin"))
       (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
       (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
       (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
       (add-hook 'scheme-mode-hook (function gambit-mode))
       (setq scheme-program-name "gsi -:t")))

(provide 'setup-scheme)
