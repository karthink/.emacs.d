(require 'use-package nil t)
;; (add-hook 'dired-mode-hook
;;           (function (lambda ()
;;                       ;; Set dired-x buffer-local variables here.
;;                       ;; dired-details and dired-details+ add the
;;                       ;; option to display only filenames in dired.
;;                       ;; ")" to toggle
;;                       (require 'dired-details+ nil t)
;;                       ;; dired-x lets you jump to the directory the
;;                       ;; current file is in with C-x C-j
;;                       (require 'dired-x nil t)
;;                       (setq dired-omit-mode 1)
;;                       (setq ls-lisp-use-insert-directory-program nil)
;;                       (require 'ls-lisp)
;;                       (setq directory-free-space-program nil)
;;                       (define-key dired-mode-map "e" 'ora-ediff-files)
;;                       )))
;; -*- lexical-binding: t -*-
;;;###autoload
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(use-package dired-sidebar
  :ensure t
  ;; :commands (dired-sidebar-toggle-sidebar)
  :bind (("C-x C-d" . dired-sidebar-toggle-sidebar)
         ("C-x D"   . list-directory))
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package ibuffer-sidebar
  ;; :load-path "~/.emacs.d/fork/ibuffer-sidebar"
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  ;; (setq ibuffer-sidebar-use-custom-font t)
  ;; (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
  (defun +sidebar-toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (dired-sidebar-toggle-sidebar)
    (ibuffer-sidebar-toggle-sidebar)))

(provide 'setup-dired)
