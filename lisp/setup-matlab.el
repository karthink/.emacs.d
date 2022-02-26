(use-package matlab
  :straight (matlab-mode :repo "https://git.code.sf.net/p/matlab-emacs/src")
  :commands (matlab-shell matlab-mode)
  :functions my/matlab-shell-help-at-point
  ;; :straight matlab-mode
  ;; :commands (matlab-mode matlab-shell matlab-shell-run-block)
  :mode ("\\.m\\'" . matlab-mode)
  :hook ((matlab-mode . company-mode-on)
         (matlab-mode . (lambda ()
                          (setq-local buffer-file-coding-system 'us-ascii)
                          (outline-minor-mode)
                          (setq-local page-delimiter "%%+")
                          (setq-local outline-regexp "^\\s-*%%+")
                          ;; (outline-hide-sublevels 3)
                          ;; (when (require 'matlab-xref nil t)
                          ;;   (make-local-variable 'xref-backend-functions)
                          ;;   (add-hook 'xref-backend-functions #'matlab-shell-xref-activate))
                          ))
         (org-mode . (lambda ()
                       (when (require 'matlab-xref nil t)
                         (add-hook 'xref-backend-functions #'matlab-shell-xref-activate 10 t)))))
  :bind (:map matlab-mode-map
              ("M-j" . nil)
              ("C-c C-n" . 'outline-next-heading)
              ("C-c C-p" . 'outline-previous-heading)
              ("C-c C-b" . 'matlab-shell-run-block)
              ("C-h ." . 'my/matlab-shell-help-at-point)
              ("M-s" . nil)
              ("C-c C-z" . 'matlab-show-matlab-shell-buffer))
  :config
  ;; (load-library "matlab-load")
  ;; (matlab-cedet-setup)
  ;; (semantic-mode 1)
  ;; (global-semantic-stickyfunc-mode 1)
  ;; (global-semantic-decoration-mode 1)
  ;; (add-hook 'matlab-mode-hook #'company-mode-on)
  ;; (add-hook 'matlab-mode-hook #'hs-minor-mode)
  ;; (add-hook 'matlab-mode-hook (lambda ()  (interactive)
  ;;                               (setq-local buffer-file-coding-system 'us-ascii)
  ;;                              (outline-minor-mode)
  ;;                               (setq-local page-delimiter "%%+")
  ;;                               (setq-local outline-regexp "^\\s-*%%+")
  ;;                               (outline-hide-sublevels 3)
  ;;                               ))

  ;; (add-hook 'matlab-mode-hook #'turn-on-evil-matlab-textobjects-mode)
  ;; (add-hook 'matlab-shell-mode-hook (lambda ()
  ;;                                     (setq-local company-idle-delay nil)
  ;;                                     (company-mode-on) ))

  ;; :config
  ;; (with-demoted-errors "Error loading Matlab autoloads: %s"
  ;;   (load-library "matlab-mode-autoloads")
  ;;   (load-library "matlab-shell")
  ;;   (load-library "mlint"))
  (setq matlab-shell-debug-tooltips-p t)
  (setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))
  ;; (setq matlab-shell-echoes nil)
  (setq matlab-shell-run-region-function 'matlab-shell-region->script)
  ;; (setq matlab-shell-run-region-function 'matlab-shell-region->internal)
  
  ;; ;;;###autoload
  ;; (defun +matlab-shell-no-select-a (&rest _args)
  ;;   "Switch back to matlab file buffer after evaluating region"
  ;;   (other-window -1))
  ;; (advice-add 'matlab-shell-run-region :after #'+matlab-shell-no-select-a)

  (defun matlab-select-block ()
    (save-excursion
      (let ((block-beg (search-backward-regexp "^%%" nil t))
            (block-end (search-forward-regexp "^%%" nil t 2)))
        (cons (or block-beg (point-min)) (if block-end
                                             (- block-end 2)
                                           (point-max))))))

  ;; These are obviated by outline-next-heading and co:
  ;;
  ;; (defun matlab-forward-section ()
  ;;   "Move forward section in matlab mode"
  ;;   (interactive)
  ;;   (beginning-of-line 2)
  ;;   (re-search-forward "^\\s-*%%" nil t)
  ;;   (match-end 0))

  ;; (defun matlab-backward-section ()
  ;;   "Move forward section in matlab mode"
  ;;   (interactive)
  ;;   (re-search-backward "^\\s-*%%" nil t)
  ;;   (match-beginning 0))

  (defun matlab-shell-run-block (&optional prefix)
    "Run a block of code around point separated by %% and display
  result in MATLAB shell. If prefix argument is non-nil, replace
  newlines with commas to suppress output. This command requires an
  active MATLAB shell."
    (interactive "P")
    (let* ((block (matlab-select-block))
           (beg (car block))
           (end (cdr block)))
      (if prefix
          (matlab-shell-run-region beg end prefix)
        (matlab-shell-run-region beg end)))))

;; Company-specific setup for Matlab-mode
(use-package matlab
  :hook (matlab-mode . my/matlab-company-settings)
  :config
  ;; (add-to-list 'company-backends 'company-matlab 'company-semantic)
  ;; (add-to-list 'company-backends 'company-matlab-shell)
  
  (defun my/matlab-company-settings ()
    ;; (unless (featurep 'company-matlab)
    ;;   (require 'company-matlab))
    (when (boundp 'company-mode-on)
      (make-local-variable 'company-backends)
      (setq-local company-backends '((company-files company-capf)))
      (company-mode-on))))

;; Some helpers for =matlab-shell=.
;; - Matlab-shell's window focus behavior is annoying.
;; - A help-at-point function
;; - Company customizations

(use-package matlab-shell
  :defer
  :after matlab
  :hook ((matlab-shell-mode . my/matlab-shell-company-settings)
         (matlab-shell-mode . (lambda ()
                                (buffer-disable-undo)
                                (setq comint-process-echoes t)
                                (define-key matlab-shell-mode-map (kbd "C-<tab>") nil)
                                (define-key matlab-shell-mode-map (kbd "C-h .")
                                  'my/matlab-shell-help-at-point))))
  :config
  (defun my/matlab-shell-company-settings ()
    (make-local-variable 'company-backends)
    (setq-local company-idle-delay 0.3)
    (company-mode-on))
  
  (defun my/matlab-shell-help-at-point (&optional arg)
    (interactive "P")
    (let ((fcn (matlab-read-word-at-point)))
      (if (and fcn (not (equal fcn "")))
          (matlab-shell-describe-command fcn))))

  (advice-add 'matlab-shell-run-region :around #'my-matlab-shell-no-display)
  (defun my-matlab-shell-no-display (orig-fn beg end &optional noshow)
    "Do not display the matlab-shell buffer after sending commands."
    (interactive "r")
    (cl-letf ((display-buffer-alist nil)
              ((symbol-function 'display-buffer-reuse-window) #'display-buffer-no-window)
              ((symbol-function 'display-buffer-at-bottom) #'display-buffer-no-window))
      (save-window-excursion (funcall orig-fn beg end noshow)))))

(provide 'setup-matlab)
