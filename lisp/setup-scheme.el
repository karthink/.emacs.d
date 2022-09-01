(use-package geiser
  :defer
  :if (> emacs-major-version 27)
  :straight t
  :init
  ;; (add-hook 'geiser-repl-mode-hook (lambda ()
  ;;                                    (setq-local company-idle-delay nil)
  ;;                                    ;; (company-mode-on)
  ;;                                    ))
  :config
  (when (not (fboundp 'run-geiser))
    (defalias 'run-geiser 'geiser))
  (setq geiser-default-implementation 'guile)
  ;; (setq geiser-mit-binary "mechanics")
  (setq geiser-mit-binary "mit-scheme"))

(use-package geiser-guile :straight t :defer)
(use-package geiser-mit
  :straight t
  :commands mechanics
  :config
  (advice-add 'run-geiser :before
              (defun mit-sicm-include (&rest _)
                "Include SICM utils if running mit scheme."
                (setenv "MITSCHEME_BAND" "mechanics.com")
                (setenv "MITSCHEME_HEAP_SIZE" "100000")
                (setq geiser-repl-skip-version-check-p t)))
  (defun mechanics ()
    "Run mit-scheme with SCMUTILS loaded, to work with (Structure
and Interpretation of Classical Mechanics) - The book."
    (interactive)
    (setenv "MITSCHEME_BAND" "mechanics.com")
    (setenv "MITSCHEME_HEAP_SIZE" "100000")
    (let ((geiser-repl-skip-version-check-p t))
      (run-geiser 'mit))))

;; Make sure mit-scheme (from repos) and scmutils (from internet + sudo ./install.sh)are installed
;;;###autoload
;; (defun mechanics ()
;;   "Run mit-scheme with SCMUTILS loaded, to work with (Structure
;; and Interpretation of Classical Mechanics) - The book."
;;   (interactive)
;;   (setenv "MITSCHEME_BAND" "mechanics.com")
;;   (setenv "MITSCHEME_HEAP_SIZE" "100000")
;;   (run-scheme
;;    "/usr/bin/mit-scheme --library /opt/mit-scheme/lib/mit-scheme-x86-64/"))

(provide 'setup-scheme)
