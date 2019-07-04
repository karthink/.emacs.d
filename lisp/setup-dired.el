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
                      (setq ls-lisp-use-insert-directory-program nil)
                      (require 'ls-lisp)
                      (setq directory-free-space-program nil)
                      (define-key dired-mode-map "e" 'ora-ediff-files)
                      )))
;; -*- lexical-binding: t -*-
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

(provide 'setup-dired)
