(add-hook 'dired-mode-hook
          (function (lambda ()
                      ;; Set dired-x buffer-local variables here.
                      ;; dired-details and dired-details+ add the
                      ;; option to display only filenames in dired.
                      ;; ")" to toggle
                      (require 'dired-details+ nil t)
                      ;; dired-x lets you jump to the directory the
                      ;; current file is in with C-x C-j
                      (require 'dired-x nil t)
                      (setq dired-omit-mode 1)
                      )))

(provide 'setup-dired)
