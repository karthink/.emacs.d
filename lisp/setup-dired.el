;; -*- lexical-binding: t -*-
;;(require 'use-package nil t)
(use-package dired
  :commands dired
  :hook (;; (dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :general
  ("C-x D" 'list-directory)
  (:keymaps 'space-menu-map "fd" '(dired :wk "Dired"))
  (:keymaps 'dired-mode-map "e" '(ora-ediff-files :wk "Diff marked files"))
  (:keymaps 'dired-mode-map :states '(normal visual) "SPC" 'space-menu)
  (:keymaps 'evil-window-map :states '(normal visual) "d" 'dired-sidebar-toggle-sidebar)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-AGFhlv"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-dwim-target t)
  (advice-add 'dired-view-file :around
              (defun dired-view-other-buffer-a (orig-fn &rest args)
                (cl-letf (((symbol-function 'view-file) #'view-file-other-window))
                  (funcall orig-fn))))
  )

(use-package dired-x
  :after dired
  :config
  (setq dired-omit-mode 1)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (require 'ls-lisp)
  (setq directory-free-space-program nil)
  (setq dired-x-hands-off-my-keys t)
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(use-package find-dired
  :defer
  :after dired
  :general
  ("M-s f" 'find-name-dired
   "M-s g" 'find-grep-dired)
  (:keymaps 'space-menu-search-map
   :wk-full-keys nil
   "F" '(find-name-dired :wk "file by name"))
  :config
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
        ;; '("-ls" . "-AGFhlv --group-directories-first"))
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

(use-package peep-dired
  :ensure t
  :general
  (:states '(normal visual)
           :keymaps 'dired-mode-map
           "z p" 'peep-dired)
  :config
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (general-def
    :states '(normal visual)
    :keymaps 'peep-dired-mode-map
    :prefix "SPC"
    "SPC" 'peep-dired-scroll-page-down
    "S-SPC" 'peep-dired-scroll-page-up)
  (general-def
    :states '(normal visual)
    :keymaps 'peep-dired-mode-map
    "<backspace>" 'peep-dired-scroll-page-up
    "j" 'peep-dired-next-file
    "k" 'peep-dired-prev-file)
  ;; (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
  ;;   (kbd "C-<SPC>") 'peep-dired-scroll-page-up
  ;;   (kbd "<backspace>") 'peep-dired-scroll-page-up
  ;;   (kbd "j") 'peep-dired-next-file
  ;;   (kbd "k") 'peep-dired-prev-file)
(setq peep-dired-cleanup-on-disable t)
(setq peep-dired-cleanup-eagerly nil)
(setq peep-dired-enable-on-directories nil)
(setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "djvu" "one" "mat"
                                      "fig" "nb" "slx" "slxc" "r2016b" "onetoc2"))
)

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
  :commands +ibuffer-sidebar-toggle
  :general
  ("C-x C-d" '+ibuffer-sidebar-toggle)
  (:states '(normal visual)
   "C-x C-d" '+ibuffer-sidebar-toggle)
  :config
  ;; (setq ibuffer-sidebar-use-custom-font t)
  ;; (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
  (defun +ibuffer-sidebar-toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (when (featurep 'ibuffer)
      (ibuffer-sidebar-toggle-sidebar))
    (dired-sidebar-toggle-sidebar)))

(provide 'setup-dired)
