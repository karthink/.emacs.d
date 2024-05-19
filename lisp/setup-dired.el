;; -*- lexical-binding: t -*-

;; Dired's been 
;; Many of these settings are overridden by =dirvish= below, but I'm undecided
;; for now if I'm going to stick with it.

(declare-function gnus-dired-attach "gnus-dired")
(use-package dired
  :commands dired
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . toggle-truncate-lines))
  :bind
  (("C-x D" . list-directory)
   :map dired-mode-map
   ("M-s f" . nil)
   ("M-s g" . nil)
   ("C-c C-a" . gnus-dired-attach)
   ("," . dired-up-directory)
   ("." . dired-find-file)
   ("e" . ora-ediff-files)
   ("M-s f" . nil))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-AGFhlv --group-directories-first"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-kill-when-opening-new-dired-buffer t
        dired-dwim-target t)
  (and (version< "29" emacs-version)
       (setq dired-mouse-drag-files t))
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
  (defun my/dired-scroll-other-window (&optional arg)
    "Scroll other window or go to next file."
    (interactive "p")
    (let* ((scroll-error-top-bottom nil)
           (num (if (= arg 1) nil arg)))
      (condition-case-unless-debug nil
          (scroll-other-window num)
        (error (dired-next-line 1)))))
  (defun my/dired-scroll-other-window-down (&optional arg)
    "Scroll other window or go to next file."
    (interactive "p")
    (let* ((scroll-error-top-bottom nil)
           (num (if (= arg 1) nil arg)))
      (condition-case-unless-debug nil
          (scroll-other-window-down num)
        (error (dired-previous-line 1))))))

(use-package dired
  :if (>= emacs-major-version 28)
  :defer
  :config
  (setq  dired-switches-in-mode-line 'as-is
         dired-do-revert-buffer t
         dired-mark-region t))

(use-package dired-aux
  :defer
  :config
  (setq dired-vc-rename-file t))

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

;; =find-dired= produces dired listings of the find (or equivalent) command. I
;; used to use it all the time until around 2018. Now it's another utility that
;; is superceded by the consistent export interface of Embark combined with a
;; Consult command.

(use-package find-dired
  :disabled
  :defer
  :after dired
  :bind
  (("M-s f" . find-name-dired)
   ("M-s g" . find-grep-dired))
  :config
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
        ;; '("-ls" . "-AGFhlv --group-directories-first"))
  (setq find-name-arg "-iname"))

;; dirvish handles dired actions asynchronously by default so =dired-async= is
;; disabled for now.

(use-package dired-async
  :disabled
  :after (dired async)
  :hook (dired-mode . dired-async-mode))

;; wdired: The first time Emacs blew my mind, back in halcyon 2005.

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
  ;; :disabled
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  (defun my/dired-subtree-up (&optional arg)
    "Jump up one directory."
    (interactive "p")
    (or (dired-subtree-up arg) (dired-up-directory (eq arg 4))))
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)
              ("," . my/dired-subtree-up)))

(use-package peep-dired
  :disabled
  :load-path "plugins/peep-dired/"
  :bind
  (:map  dired-mode-map
   ("P" . peep-dired))
  :hook ((peep-dired-display-file . auto-revert-mode)
         (peep-dired-display-file . peep-dired-fit-image))
  :config
  ;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (defun peep-dired-fit-image ()
    (when (derived-mode-p 'image-mode)
      (image-transform-fit-both)))
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
      '("mkv" "iso" "mp4" "djvu" "one" "mat"
        "fig" "nb" "slx" "slxc" "r2016b" "onetoc2")))

(use-package image-dired
  :commands image-dired
  :config
  (setq image-dired-dir
        (dir-concat user-cache-directory "image-dired/")))

(use-package gnus-dired
  :commands gnus-dired-attach
  :after dired)

(use-package dired-sidebar
  :after dired
  :disabled
  :commands (dired-sidebar-toggle-sidebar)
  :bind
  (("C-x D" . list-directory)
   ("C-x C-d" . dired-sidebar-toggle-sidebar))
  
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
  :bind
  ("C-x C-d" . +ibuffer-sidebar-toggle)
  :config
  ;; (setq ibuffer-sidebar-use-custom-font t)
  ;; (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
  (defun +ibuffer-sidebar-toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (when (featurep 'ibuffer)
      (ibuffer-sidebar-toggle-sidebar))
    (dired-sidebar-toggle-sidebar)))

;; Disabled while testing dirvish
(use-package dired-rsync
  :ensure t
  :hook (dired-rsync-failed . dired-rsync--pop-to-rsync-failed-buf)
  :config
  (setq dired-rsync-unmark-on-completion nil))

(use-package dired-rsync-transient
  :ensure t
  :after dired-x
  :bind (:map dired-mode-map
         ("V" . my/dired-rsync-transient))
  :config
  (defun my/dired-rsync-transient (&optional arg)
    "Run dired-rsync or dired-rsync-transient."
    (interactive "P")
    (if arg (dired-rsync-transient)
      (call-interactively #'dired-rsync))))

(use-package dired-filter
  :ensure t
  :after dired)

(use-package diredfl
  :disabled
  :straight t
  :hook (dired-mode . diredfl-mode))

(use-package dired-hist
  ;; :disabled
  :ensure (:host github :protocol ssh
           :repo "karthink/dired-hist")
  ;; :load-path "plugins/dired-hist/"
  :after dired
  :bind (:map dired-mode-map
         ("l" . dired-hist-go-back)
         ("r" . dired-hist-go-forward))
  :config (dired-hist-mode 1))

(use-package dirvish
  :disabled
  :straight t
  :after dired
  :demand t
  :config
  (dirvish-override-dired-mode 1)
  (setq dired-filter-revert 'always)
  ;; (setq dired-listing-switches
  ;;       "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (advice-add
   'dirvish-dired-noselect-a
   :before-until
   (defun my/dirvish-dired-noselect-on-lists (&rest args)
     (and (listp (cadr args))
          (apply (car args) (cdr args)))))
  (setq dirvish-cache-dir
        (expand-file-name
         "dirvish" user-cache-directory))
  (setq dirvish-attributes '(vc-state subtree-state collapse)
        dirvish-hide-cursor nil)
  (add-to-list 'dirvish-preview-disabled-exts "mat")
  (add-to-list 'dirvish-preview-disabled-exts "bci")
  (defun my/dirvish-sort-toggle-or-edit (&optional arg)
    (interactive "P")
    (call-interactively
     (if arg
         #'dirvish-ls-switches-menu
       #'dirvish-quicksort)))
  (use-package dirvish-side
    :defer
    :config
    (setq dirvish-side-display-alist
          '((side . right) (slot . 20))))
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("M-s M-f" . dirvish-fd)
   :map dirvish-mode-map
   ("SPC" . my/dired-scroll-other-window)
   ("S-SPC" . my/dired-scroll-other-window-down)
   :map dired-mode-map ; Dirvish respects all the keybindings in this map
   ;; ("h" . dired-up-directory)
   ;; ("j" . dired-next-line)
   ;; ("k" . dired-previous-line)
   ;; ("l" . dired-find-file)
   ;; ("i" . wdired-change-to-wdired-mode)
   ;; ("." . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("s"   . my/dirvish-sort-toggle-or-edit) ; remapped `dired-sort-toggle-or-edit'
   ("?"   . dirvish-dispatch)  ; remapped `dired-summary'
   ("TAB" . dirvish-subtree-toggle)
   ;; ("SPC" . dirvish-history-jump)
   ("r" . dirvish-history-go-forward)
   ("l" . dirvish-history-go-backward)
   ;; ("M-l" . dirvish-ls-switches-menu)
   ("M-*" . dirvish-mark-menu)
   ("f" . dirvish-layout-toggle)
   ;; ("M-e" . dirvish-emerge-menu)
   ;; ("M-j" . dirvish-fd-jump)
   ;; ("M-s" . dirvish-setup-menu)
   ))

(use-package dired-delight
  :ensure (:host github :protocol ssh
           :repo "karthink/dired-delight")
  ;; :straight (:local-repo "~/.local/share/git/dired-delight/")
  :bind (:map dired-mode-map
         ("@" . dired-delight)
         ("*c" . dired-delight-mark-color)))

(use-package dired-preview
  :after dired
  :ensure (:host github :protocol ssh
           :repo "karthink/dired-preview"
           :branch "main")
  :bind (:map dired-mode-map
         ("P" . dired-preview-mode)
         :map dired-preview-mode-map
         ("SPC" . my/dired-scroll-other-window)
         ("DEL" . my/dired-scroll-other-window-down)
         ("S-SPC" . my/dired-scroll-other-window-down))
  :config
  (setq dired-preview-delay 0.2)
  ;; (advice-add 'dired-preview--display-buffer :around
  ;;             (defun my/dired-preview--display-buffer (origfn buffer)
  ;;               (let ((display-buffer-base-action))
  ;;                 (funcall origfn buffer))))
  )

(provide 'setup-dired)
