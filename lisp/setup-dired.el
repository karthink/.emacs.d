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
   ("," . dired-up-directory)
   ("." . dired-find-file)
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

(use-package sidle
  :after dired
  :bind ( :map dired-mode-map
          ("M-RET" . sidle-show)
          ("SPC" . sidle-scroll-up-command)
          ("S-SPC" . sidle-scroll-down-command)
          ("DEL" . sidle-scroll-down-command)
          ("q" . sidle-quit)
          ("M-n" . sidle-next)
          ("M-p" . sidle-prev)
          ("M-<" . sidle-top)
          ("M->" . sidle-bottom)
          ("M-s i" . sidle-imenu))
  :config
  (defvar-local dired--sidle nil)
  (sidle-register-backend 'dired
    :list-mode 'dired-mode
    :entry-condition
    (lambda (buf)
      (and-let* ((win (get-buffer-window buf))
                 ((window-live-p win)))
        (buffer-local-value 'dired--sidle buf)))
    :show (lambda () (interactive)
            (let ((buf (dired--find-file #'find-file-noselect
                                         (dired-get-file-for-visit))))
              (sidle-display-buffer buf))
            (setq-local dired--sidle t))
    :display-action '((display-buffer-reuse-mode-window
                       display-buffer-in-previous-window
                       display-buffer-use-some-window)
                      (inhibit-same-window . t)
                      (some-window . mru))
    :next #'dired-next-line
    :prev #'dired-previous-line
    :quit-list #'quit-window
    :quit-entry #'quit-window))

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
  :bind (("H-d" . dired-jump)
         :map ctl-x-map
         ("C-4" . dired-jump-other-window))
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

(use-package dired-subtree
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

(use-package image-dired
  :commands image-dired
  :config
  (setq image-dired-dir
        (expand-file-name "image-dired/" user-cache-directory)))

(use-package gnus-dired
  :bind (:map dired-mode-map
         ("C-c C-a" . gnus-dired-attach))
  :after dired)

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
  :disabled
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
         ("SPC" . dired-preview-page-down)
         ("DEL" . dired-preview-page-up)
         ("S-SPC" . dired-preview-page-up))
  :config
  (setq dired-preview--buffers-threshold 2)
  (setq dired-preview-delay 0.2)
  (setq dired-preview-ignored-extensions-regexp
        "\\.\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a\\|flac\\|wav\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip\\|iso\\|epub\\)$"
        dired-preview-max-size (* 1024 1024 10))
  (cl-defmethod dired-preview--get-buffer :around ((file (head text)))
    (let ((org-inhibit-startup t)) (cl-call-next-method)))
  ;; (define-advice dired-preview--clean-up-window
  ;;     (:after (&optional window) run-mode-hooks)
  ;;   (run-mode-hooks))
  (cl-defmethod dired-preview--get-buffer :around ((file (head image)))
    (let ((buf (cl-call-next-method)))
      (prog1 buf
        (with-current-buffer buf
          (setq image-transform-resize image-auto-resize))))))

(use-package dired-preview-popup
  :hook (dired-preview-mode . dired-preview-popup-mode))

(provide 'setup-dired)
