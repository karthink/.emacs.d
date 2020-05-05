;; -*- lexical-binding: t -*-
;;(require 'use-package nil t)
(use-package dired
  :commands dired
  :hook (;; (dired-mode-hook . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
   (setq dired-listing-switches
        "-AGFhlv --time-style=long-iso")
   (setq dired-recursive-copies 'always
         dired-recursive-deletes 'always)
    (setq dired-dwim-target t)

  :general
  (:keymaps 'space-menu-map
            "fd" '(dired :wk "Dired"))
  (:keymaps 'dired-mode-map
            "e" '(ora-ediff-files :wk "Diff marked files")))

(use-package dired-x
  :after dired
  :config
  (setq dired-omit-mode 1)
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (setq directory-free-space-program nil)
  (setq dired-x-hands-off-my-keys t)
  )

(use-package find-dired
  :defer
  :after dired
  :general
  ("M-s f" 'find-name-dired
   "M-s g" 'find-grep-dired)
  :config
  (setq find-ls-option
        '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
  (setq find-name-arg "-iname"))

(use-package async
  :ensure)

(use-package dired-async
  :after (dired async)
  :hook (dired-mode-hook . dired-async-mode))

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

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))

(use-package dired-sidebar
  :after dired
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :general
  ("C-x D"  'list-directory)
  (:keymaps 'dired-sidebar-mode-map
   :states  '(normal)
   "gO"     'dired-sidebar-find-file-alt
   "RET"    'dired-sidebar-find-file)

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
  :ensure t
  :after (dired dired-sidebar ibuffer)
  :commands (ibuffer-sidebar-toggle-sidebar +ibuffer-sidebar-toggle)
  :general
  ("C-x C-d" '+ibuffer-sidebar-toggle)
  :config
  ;; (setq ibuffer-sidebar-use-custom-font t)
  ;; (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
  (defun +ibuffer-sidebar-toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (ibuffer-sidebar-toggle-sidebar)
    (dired-sidebar-toggle-sidebar)))

(provide 'setup-dired)
