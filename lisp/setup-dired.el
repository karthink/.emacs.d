;; -*- lexical-binding: t -*-
;;(require 'use-package nil t)
(use-package dired
  :commands dired
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . toggle-truncate-lines))
  :bind
  (:map dired-mode-map
        ("M-s f" . nil)
        ("M-s g" . nil))
  :general
  ("C-x D" 'list-directory)
  (:keymaps 'space-menu-map "fd" '(dired :wk "Dired"))
  (:keymaps 'dired-mode-map "e" '(ora-ediff-files :wk "Diff marked files")
                            "M-s f" nil)
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

  (defun dired-find-file-other-window ()
  "In Dired, visit this file or directory in another window. If `ace-window' is available, use it to select window for visiting this file.`"
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (window 
         (if (fboundp 'aw-select)
             (aw-select "Select Window")
           (next-window))))
    (select-window window)
    (find-file file)))
  )

(use-package dired-aux
  ;; This functionality is superceded by affe-find and affe-grep from the
  ;; affe.el library and is thus disabled.
  :disabled
  :config
  (defmacro my/dired-fd (name doc prompt &rest flags)
    "Make commands for selecting 'fd' results with completion.
NAME is how the function should be named.  DOC is the function's
documentation string.  PROMPT describes the scope of the query.
FLAGS are the command-line arguments passed to the 'fd'
executable, each of which is a string."
    `(defun ,name (&optional arg)
       ,doc
       (interactive "P")
       (let* ((vc (vc-root-dir))
              (dir (expand-file-name (or vc default-directory)))
              (regexp (read-regexp
                       (format "%s matching REGEXP in %s: " ,prompt
                               (propertize dir 'face 'bold))))
              (names (process-lines "fd" ,@flags regexp dir))
              (buf "*FD Dired*"))
         (if names
             (if arg
                 (dired (cons (generate-new-buffer-name buf) names))
               (find-file
                (completing-read (format "Items matching %s (%s): "
                                         (propertize regexp 'face 'success)
                                         (length names))
                                 names nil t))))
         (user-error (format "No matches for « %s » in %s" regexp dir)))))

  (my/dired-fd
   my/dired-fd-dirs
   "Search for directories in VC root or PWD.
With \\[universal-argument] put the results in a `dired' buffer.
This relies on the external 'fd' executable."
   "Subdirectories"
   "-i" "-H" "-a" "-t" "d" "-c" "never")

  (my/dired-fd
   my/dired-fd-files-and-dirs
   "Search for files and directories in VC root or PWD.
With \\[universal-argument] put the results in a `dired' buffer.
This relies on the external 'fd' executable."
   "Files and dirs"
   "-i" "-H" "-a" "-t" "d" "-t" "f" "-c" "never")

  :bind (("M-s M-f" . my/dired-fd-files-and-dirs)
         ("M-s M-d" . my/dired-fd-dirs)))

(use-package dired-x
  :after dired
  :bind ("H-d" . dired-jump)
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
  :disabled
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
  :hook (dired-mode . dired-async-mode))

(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

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
              ("<S-iso-lefttab>" . dired-subtree-remove)
              ))

(use-package peep-dired
  :load-path "plugins/peep-dired/"
  :general
  (:states '(normal visual)
           :keymaps 'dired-mode-map
           "z p" 'peep-dired)
  (:keymaps 'dired-mode-map
            "P" 'peep-dired)
  :hook (peep-dired-display-file . auto-revert-mode)
  :config
  ;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (ignore-errors
    (setq peep-dired-display-action-alist
          '(display-buffer-in-direction
            (direction . below)
            (window-height . (lambda (win) (fit-window-to-buffer
                                       win
                                       (floor (* 0.6 (frame-height))))))
            (window-parameters . ((dedicated . t))))))
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
(setq peep-dired-ignored-extensions
      '("mkv" "iso" "mp4" "pdf" "djvu" "one" "mat"
        "fig" "nb" "slx" "slxc" "r2016b" "onetoc2")))

(use-package image-dired
  :commands image-dired
  :config
  (setq image-dired-dir
        (dir-concat user-cache-directory "image-dired/")))

(use-package gnus-dired
  :defer 5
  :after dired)

(use-package dired-sidebar
  :after dired
  :disabled
  :commands (dired-sidebar-toggle-sidebar)
  :general
  ("C-x D"  'list-directory
   "C-x C-d" 'dired-sidebar-toggle-sidebar
   :states '(normal visual)
   "C-w C-d" 'dired-sidebar-toggle-sidebar)
  
  (:keymaps 'space-menu-map
   :wk-full-keys nil
   :prefix "f"
    "t" '(dired-sidebar-toggle-sidebar :wk "Dired tree"))

  (:keymaps 'dired-sidebar-mode-map
   :states  '(normal)
   "gO"     'dired-sidebar-find-file-alt
   "RET"    'dired-sidebar-find-file)

  (:keymaps 'dired-sidebar-mode-map
   "-" nil)
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
  :disabled
  :commands +ibuffer-sidebar-toggle
  :general
  ("C-x C-d" '+ibuffer-sidebar-toggle)
  (:states '(normal visual)
   "C-x C-d" '+ibuffer-sidebar-toggle)
  (:keymaps 'space-menu-buffer-map
            :wk-full-keys nil
            "t" '(ibuffer-sidebar-toggle-sidebar :wk "Buffer sidebar"))
  :config
  ;; (setq ibuffer-sidebar-use-custom-font t)
  ;; (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
  (defun +ibuffer-sidebar-toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (when (featurep 'ibuffer)
      (ibuffer-sidebar-toggle-sidebar))
    (dired-sidebar-toggle-sidebar)))

(use-package dired-rsync
  :ensure t
  :bind (:map dired-mode-map
         ("r" . dired-rsync))
  :hook (dired-rsync-failed . dired-rsync--pop-to-rsync-failed-buf)
  :config
  (setq dired-rsync-unmark-on-completion nil))

(use-package dired-filter
  :ensure t
  :after dired)

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(provide 'setup-dired)
