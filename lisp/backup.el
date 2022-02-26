;; Backed up lisp that I no longer use

;;----------------------------------------------------------------------
;; INIT.EL
;;----------------------------------------------------------------------

;; (when (featurep 'package)
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;;######################################################################
;; PERSONAL
;;######################################################################
;;; My account data for Weblogger, Jabber & other services
;; (require 'personal nil t)

;; (defun bury-compile-buffer-if-successful (buffer string)
;;  "Bury a compilation buffer if succeeded without warnings "
;;  (when (and
;;          (buffer-live-p buffer)
;;          (string-match "*Compile-Log*" (buffer-name buffer))
;;          (string-match "finished" string)
;;          ;; (not
;;          ;;  (with-current-buffer buffer
;;          ;;    (goto-char (point-min))
;;          ;;    (search-forward "warning" nil t)))
;;          )
;;     (run-with-timer 1 nil
;;                     (lambda (buf)
;;                       (bury-buffer buf)
;;                       (switch-to-prev-buffer (get-buffer-window buf) 'kill))
;;                     buffer)))
;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;;----------------------------------------------------------------------
;; JULIA-MODE
;;----------------------------------------------------------------------
;; (defun my-julia-mode-hooks ()
;;   (require 'julia-shell-mode))
;; (add-hook 'julia-mode-hook 'my-julia-mode-hooks)
;; (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;; (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)

;;----------------------------------------------------------------------
;; LUA-MODE
;;----------------------------------------------------------------------
;; (setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;----------------------------------------------------------------------
;; SCHEME MODE
;;----------------------------------------------------------------------
;; (require 'setup-scheme nil t)

;;----------------------------------------------------------------------
;; MATH-MODE
;;----------------------------------------------------------------------
;; Major mode for running Mathematica in Emacs
;; (eval-after-load 'math
;;  '(define-key math-mode-map (kbd "<tab>") 'math-complete-symbol))

;; (autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
;; (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

;; (require 'auto-complete-config nil t)
;; (setq ac-auto-show-menu t
;;       ac-auto-start t
;;       ac-show-menu-immediately-on-auto-complete t)

;;----------------------------------------------------------------------
;; MULTIPLE-CURSORS
;;----------------------------------------------------------------------
;;; ELPA package, run (package-initialize) first
;; (require 'multiple-cursors nil t)
;; (when (featurep 'multiple-cursors)
;;   (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
;;   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;   (global-set-key (kbd "M-n") 'mc/mark-next-word-like-this)
;;   (global-set-key (kbd "M-@") 'mc/mark-all-words-like-this)
;;   (global-set-key (kbd "C-@") 'mc/mark-all-like-this)
;;   (global-set-key (kbd "M-p") 'mc/mark-previous-word-like-this)
;;   (global-set-key (kbd "<C-return>") 'set-rectangular-region-anchor)
;;   ;; (global-set-key (kbd "C-x C-a") 'mc/edit-beginnings-of-lines)
;;   ;; (global-set-key (kbd "C-x SPC") 'mc/mark-all-dwim)
;;   (global-set-key (kbd "M-N") 'mc/insert-numbers)
;;   (global-set-key (kbd "M-S") 'mc/sort-regions))

;; (require 'expand-region nil t)
;; (when (featurep 'expand-region)
;;   (global-set-key (kbd "C-=") 'er/expand-region)
;;   (global-set-key (kbd "C-,") 'er/expand-region))

;; Ace jump mode major function
;; (autoload
;;     'ace-jump-mode
;;     "ace-jump-mode"
;;     "Emacs quick move minor mode" t)

;; (require 'ace-jump-mode nil t)
;; (when (featurep 'ace-jump-mode)
;;   (define-key global-map (kbd "M-m") 'ace-jump-mode)
;;   (define-key global-map (kbd "C-'") 'ace-jump-mode))

;; Enable a more powerful jump back function from ace jump mode
;;
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; (global-set-key
;;     "\M-x"
;;     (lambda ()
;;       (interactive)
;;       (call-interactively
;;        (intern
;;         (ido-completing-read
;;          "M-x "
;;          (all-completions "" obarray 'commandp))))))

;;  -x. Use with care!
;;; Smex
;;(require 'smex nil t)

;; (when (commandp 'ido-grid-mode)
  ;; (ido-grid-mode 1))
;;----------------------------------------------------------------------
;; AUTO-COMPLETE MODE
;;----------------------------------------------------------------------
;;; ELPA package, run (package-initialize) first

;; (use-package auto-complete
;;   :ensure t
;;   :commands global-auto-complete-mode
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode 1)))

;;---------------------------------------------------------------------
;; PAREDIT-MODE
;;---------------------------------------------------------------------
;; (use-package paredit
;;   :ensure t
;;   :commands enable-paredit-mode
;;   :hook ((emacs-lisp-mode
;;           eval-expression-minibuffer-setup
;;           ielm-mode
;;           lisp-mode
;;           lisp-interaction-mode
;;           scheme-mode) . enable-paredit-mode)

;;   ;; (autoload 'enable-paredit-mode "paredit"
;;   ;; "Turn on pseudo-structural editing of Lisp code." t)
;;   ;; :bind (:map paredit-mode-map
;;   ;;             ;; ("M-s" . nil)
;;   ;;             ;; ("M-r" . nil)
;;   ;;             ;; ("<M-up>" . nil)
;;   ;;             ;; ("<M-down>" . nil)
;;   ;;             ("C-c <up>" . 'paredit-splice-sexp-killing-backward)
;;   ;;             ("C-c <down>" . 'paredit-splice-sexp-killing-forward)
;;   ;;             ("C-c s" . 'paredit-splice-sexp)
;;   ;;             ("C-c r" . 'paredit-raise-sexp))
;;   )

;;----------------------------------------------------------------------
;; PRETTY LAMBDA MODE
;;----------------------------------------------------------------------

;; Superceded by prettify-symbols-mode

;; (require 'pretty-lambdada)
;; (add-hook 'emacs-lisp-mode-hook 'pretty-lambda)
;; (add-hook 'scheme-mode-hook 'pretty-lambda)
;; (add-hook 'lisp-mode-hook 'pretty-lambda)
;; (add-hook 'lisp-interaction-mode-hook 'pretty-lambda)
;; (add-hook 'inferior-scheme-mode-hook 'pretty-lambda)
;; ;;(pretty-lambda-for-modes)

;;----------------------------------------------------------------------
;; IDO-MODE.
;;----------------------------------------------------------------------
;; (require 'ido nil t)

;; (when (featurep 'ido)

;;   (ido-mode t)
;;   (setq ido-enable-flex-matching t) ;; enable fuzzy matching
;;   (ido-everywhere 1)
;;   ;; (if (require 'ido-completing-read+))
;;   (ido-ubiquitous-mode 1)
;;   (require 'icomplete)
;;   (icomplete-mode 1)

;;   (autoload 'idomenu "idomenu" nil t)
;;   (eval-after-load "idomenu"
;;     (global-set-key (kbd "M-.") 'imenu))

;;   ;; Custom keybindings
;;   (defun ido-my-keys ()
;;     (mapc (lambda (K)
;;             (let* ((key (car K)) (fun (cdr K)))
;;               (define-key ido-completion-map (edmacro-parse-keys key) fun)))
;;           '(("C-n" . ido-next-match)
;;             ("C-p"  . ido-prev-match))))


;;   ;; Enable ido-completion over TAGS
;;   (defun ido-find-file-in-tag-files ()
;;     (interactive)
;;     (save-excursion
;;       (let ((enable-recursive-minibuffers t))
;;         (visit-tags-table-buffer))
;;       (find-file
;;        (expand-file-name
;;         (ido-completing-read
;;          "Project file: " (tags-table-files) nil t)))))

;;   ;; (define-key ido-mode-map (kbd "C-x t") 'ido-find-file-in-tag-files)

;;   (add-hook 'ido-setup-hook (lambda nil
;;                               (ido-my-keys)))

;;   (require 'ido-other-window nil t))

;; (use-package ido-grid-mode
;;   :ensure
;;   :init
;;   (ido-grid-mode 1))

;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-S-x") 'smex-major-mode-commands)


;;----------------------------------------------------------------------
;; DOT-MODE
;;----------------------------------------------------------------------
;; (Vi like redo edits with C-.)
;; (require 'dot-mode nil t)
;; (when (featurep 'dot-mode)
;;   (add-hook 'find-file-hooks 'dot-mode-on)
;;   (global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
;;                                    (message "Dot mode activated."))))

;;----------------------------------------------------------------------
;; AUTOPAIR and WRAP-REGION
;;----------------------------------------------------------------------
;; Better paren handling:
;; (require 'autopair)
;; (autopair-global-mode)

;; (require 'wrap-region)
;; (wrap-region-global-mode t)

;;----------------------------------------------------------------------
;; BOOKMARKS
;;----------------------------------------------------------------------
;; Bookmark+ mode
;;(require 'bookmark+)
;; (defalias 'bs 'bookmark-set)
;; (defalias 'bl 'bookmark-bmenu-list)
;; Enable bookmarks in file completion
;; (defun bookmark-to-abbrevs ()
;;   "Create abbrevs based on `bookmark-alist'."
;;   (dolist (bookmark bookmark-alist)
;;     (let* ((name (car bookmark))
;;            (file (bookmark-get-filename name)))
;;       (define-abbrev global-abbrev-table name file))))

;; ;; Jump recently selected bookmarks to the top
;; (defadvice bookmark-jump (after bookmark-jump activate)
;;   (let ((latest (bookmark-get-bookmark bookmark)))
;;     (setq bookmark-alist (delq latest bookmark-alist))
;;     (add-to-list 'bookmark-alist latest)))

;; (bookmark-to-abbrevs)

;;----------------------------------------------------------------------
;; ARTIST-MODE
;;----------------------------------------------------------------------
;; Artist mode for drawing ASCII art!
;; (autoload 'artist-mode "artist" "Enter artist-mode" t)

;; Keybindings for org-mode and org-remember
;; (when (featurep 'org-init)
;;   (global-set-key "\C-cr" 'remember)
;;   (global-set-key "\C-\M-r" 'org-remember))

;;----------------------------------------------------------------------
;; LONGLINES-MODE
;;----------------------------------------------------------------------
;; Set keyboard shortcut for turning on longlines-mode in text-mode.
;; (define-key text-mode-map "\C-cL" 'longlines-mode)

;;----------------------------------------------------------------------
;; FLYSPELL-PROG-MODE
;;----------------------------------------------------------------------
;; enable flyspell-prog-mode for all cc-modes, cperl and python mode
;; (dolist (hook '(c-mode-common-hook
;;                 cperl-mode-hook
;;                 html-mode-hook
;;                 css-mode-hook
;;                 lisp-mode-hook))
;;   (add-hook hook (lambda ()
;;                    (flyspell-prog-mode))))

; let there be a marker on every empty line on the left fringe
;;(setq default-indicate-empty-lines nil)

;; (defun fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen
;;                        (if (frame-parameter nil 'fullscreen)
;;                            nil 'fullboth)))

;; Zenburn color theme
;; (if (equal system-type 'gnu/linux)
;;     (progn (require 'zenburn)
;;             (color-theme-zenburn)))

;; (use-package evil-tabs
;;   :commands global-evil-tabs-mode
;;   :init
;;   (global-evil-tabs-mode t))

;; (require 'evil)
;; (evil-mode 1)
;; (turn-on-evil-surround-mode)
;; (evil-commentary-mode 1)

;; (require 'diminish)
;; (require 'bind-key)
;; (package-initialize)

;; (require 'cl-lib)
;; (require 'package)
;;(setq package-enable-at-startup nil)

;;----------------------------------------------------------------------
;; REMEMBER-MODE
;;----------------------------------------------------------------------
;; Customizations to remember-mode so I can use it as my journal/notes.

;; (add-hook 'remember-mode-hook 
;;           '(lambda nil
;;              (setq remember-data-file (expand-file-name 
;;                                        "~/doodles/notes.txt"))
;;              (setq remember-leader-text "")))

;; (defadvice remember-finalize (after remember-mode-add-delimiter activate compile)
;;   (let ( (buf (find-file remember-data-file)) )
;;     (save-excursion
;;       (goto-char (point-max))
;;       (insert "\n%\n"))
;;     (save-buffer)
;;     (kill-buffer buf)
;;     ))

;; (defun remember-journal ()
;;                "Run (remember) with my journal as the data file."
;;                (interactive)
;;                (setq remember-data-file 
;;                      (expand-file-name 
;;                       "~/doodles/journal.txt"))
;;                (remember)
;;                (setq remember-data-file "~/doodles/notes.txt"))


;;######################################################################
;;;* MISCELLANEOUS PREFERENCES
;;######################################################################

;; (use-package! hl-line
;;   ;; Highlights the current line
;;   :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
;;   :config
;;   ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
;;   ;; performance boost. I also don't need to see it in other buffers.
;;   (setq hl-line-sticky-flag nil
;;         global-hl-line-sticky-flag nil)

;;   ;; Disable `hl-line' in evil-visual mode (temporarily). `hl-line' can make the
;;   ;; selection region harder to see while in evil visual mode.
;;   (after! evil
;;     (defvar doom-buffer-hl-line-mode nil)
;;     (add-hook! 'evil-visual-state-entry-hook
;;       (defun doom-disable-hl-line-h ()
;;         (when hl-line-mode
;;           (setq-local doom-buffer-hl-line-mode t)
;;           (hl-line-mode -1))))
;;     (add-hook! 'evil-visual-state-exit-hook
;;       (defun doom-enable-hl-line-maybe-h ()
;;         (when doom-buffer-hl-line-mode
;;           (hl-line-mode +1))))))


;; ;;;###package whitespace
;; (setq whitespace-line-column nil
;;       whitespace-style
;;       '(face indentation tabs tab-mark spaces space-mark newline newline-mark
;;         trailing lines-tail)
;;       whitespace-display-mappings
;;       '((tab-mark ?\t [?› ?\t])
;;         (newline-mark ?\n [?¬ ?\n])
;;         (space-mark ?\  [?·] [?.])))
;; (after! whitespace
;;   (defun doom-disable-whitespace-mode-in-childframes-a (orig-fn)
;;     "`whitespace-mode' inundates child frames with whitspace markers, so disable
;; it to fix all that visual noise."
;;     (unless (frame-parameter nil 'parent-frame)
;;       (funcall orig-fn)))
;;   (add-function :around whitespace-enable-predicate #'doom-disable-whitespace-mode-in-childframes-a))

;; (use-package! paren
;;   ;; highlight matching delimiters
;;   :after-call after-find-file doom-switch-buffer-hook
;;   :config
;;   (setq show-paren-delay 0.1
;;         show-paren-highlight-openparen t
;;         show-paren-when-point-inside-paren t)
;;   (show-paren-mode +1))

;; (setq visible-bell t)
;; (setq ring-bell-function
;;       (lambda ()
;;         (let ((orig-fg (face-foreground 'mode-line)))
;;           (set-face-foreground 'mode-line "#F2804F")
;;           (run-with-idle-timer 0.1 nil
;;                                (lambda (fg) (set-face-foreground 'mode-line fg))
;;                                orig-fg))))


;; (use-package! winner
;;   ;; undo/redo changes to Emacs' window layout
;;   :after-call after-find-file doom-switch-window-hook
;;   :preface (defvar winner-dont-bind-my-keys t)
;;   :config (winner-mode +1)) ; I'll bind keys myself

;;######################################################################
;;;* COLORS & COLOR THEMES
;;######################################################################

;; Load theme after the frame is created.
;; (add-hook 'after-make-frame-functions ;'after-init-hook
;;           (lambda (frame)
;;             (mapc #'disable-theme custom-enabled-themes)
;;             (and (display-graphic-p)
;;                  (progn
;;                    (if (or (< (nth 2 (decode-time (current-time))) 7)
;;                                 (> (nth 2 (decode-time (current-time))) 18))
;;                             (progn (load-theme 'dracula t)
;;                                    ;; (load-theme 'smart-mode-line-dark)
;;                                    )
;;                           (progn (load-theme 'dracula t)
;;                                  ;; (load-theme 'smart-mode-line-dark)
;;                                  ))))
;;             ))

;; before loading new theme
;; (defun load-theme--disable-old-theme-a(theme &rest args)
;;   "Disable current theme before loading new one."
;;   (unless (member theme '(smart-mode-line-dark smart-mode-line-light))
;;     (mapcar #'disable-theme
;;             (remove 'smart-mode-line-light
;;                     (remove 'smart-mode-line-dark custom-enabled-themes)))))
;; (advice-add 'load-theme :before #'load-theme--disable-old-theme-a)

;; Tao:
;; (defun tao-palette () (tao-theme-golden-grayscale-yin-palette))
;; (tao-with-color-variables tao-palette
;;   (progn
;;     (setq
;;       hl-paren-colors (list color-14 color-11 color-9 color-7 color-6)
;;       hl-paren-background-colors (list color-4 color-4 color-4 color-4 color-4))))

;; Other themes
;; (list 'tsdh-light
;;       'ample-flat-theme 'ample-light-theme 'ample-theme
;;       'tao 'tao-yin 'tao-yang
;;       'gruvbox-dark-hard 'gruvbox-light-hard
;;       'spacemacs-light-theme 'spacemacs-dark-theme)

;;######################################################################
;;;* MODELINE:
;;######################################################################

;; (use-package telephone-line
;;   :ensure t
;;   :init
;;   (telephone-line-mode 1))

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))


;;----------------------------------------------------------------------
;; ORG-MODE
;;----------------------------------------------------------------------
;;; Org-Remember: Org-mode with Remember-mode

;; (org-remember-insinuate)
;; (setq org-directory "~/doodles")
;; (setq org-default-notes-file
;;       (expand-file-name (concat org-directory "tasks.org")))
;; ;; (setq org-remember-default-headline "stuff")

;; ;;Templates for org-remember:
;; (setq org-remember-templates
;;       (quote (("Journal" ?j
;;                "* %^{Title}\n  %U\n  %?\n  %i\n"
;;                "journal.org" top
;;                )
;;               ("Notes" ?n
;;                "* %?\n  "
;;                "tasks.org" bottom
;;                ))))

;; (defun make-remember-frame ()
;;   "Turn the current frame into a small popup frame for remember mode;
;; this is meant to be called with
;;      emacsclient -c -e '(make-remember-frame)'"
;;   (modify-frame-parameters nil
;;     '( (name . "*Remember*") ;; must be same as in mode-hook below
;;        (width .  80)
;;        (height . 14)
;;        (vertical-scroll-bars . nil)
;;        (menu-bar-lines . nil)
;;        (tool-bar-lines . nil)))
;;   (org-remember)
;;   (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only....
;;   (delete-other-windows))

;; when we're in such a remember-frame, close it when done.
;; (add-hook 'org-remember-mode-hook
;;   (lambda()
;;     (define-key org-remember-mode-map (kbd "C-c C-c")
;;       '(lambda()(interactive)
;;          (let ((remember-frame-p
;;                  (string= (frame-parameter nil 'name) "*Remember*")))
;;            (when remember-frame-p (make-frame-invisible))  ;; hide quickly
;;            (org-remember-finalize)
;;            (when remember-frame-p (delete-frame)))))
;;     (define-key org-remember-mode-map (kbd "C-c C-k")
;;       '(lambda() (interactive)
;;          (let ((remember-frame-p
;;                  (string= (frame-parameter nil 'name) "*Remember*")))
;;            (when remember-frame-p (make-frame-invisible))  ;; hide quickly
;;            (org-kill-note-or-show-branches)
;;            (when remember-frame-p (delete-frame)))))
;;       ))

;; (define-key org-mode-map (kbd "C-c r") nil)

 '(counsel-projectile-mode t nil (counsel-projectile))
  ;; (add-to-list 'embark-keymap-alist
  ;;              '(virtual-buffer . embark-buffer-map))
  ;; (add-to-list 'embark-exporters-alist
  ;;              '(virtual-buffer . embark-export-virtual-ibuffer))

  ;; (defun embark-export-virtual-ibuffer (virtual-buffers)
  ;;   "docstring"
  ;;   (let ((buffers (mapcar (lambda (buf) (substring buf 1))
  ;;                          (cl-remove-if-not
  ;;                           (lambda (buf) (equal (- (elt buf 0)
  ;;                                              consult--special-char)
  ;;                                           ?b))
  ;;                           virtual-buffers))))
  ;;     (ibuffer t "*Embark Export Ibuffer*"
  ;;              `((predicate . (member (buffer-name) ',buffers))))))

;; (defun my/dired-ffmpeg-crop ()
;;     (interactive)
;;     (let ((file-suffix ""))
;;       (cl-loop
;;        for file in (dired-get-marked-files) do
;;        (start-process "ffmpeg" "ffmpeg" "/usr/bin/ffmpeg"
;;                       "-i" file
;;                       "-ss" (progn (start-process (concat "mpv " (file-name-base file))
;;                                                   (concat "mpv " (file-name-base file))
;;                                                   "/usr/bin/mpv"
;;                                                   file)
;;                                    (setq file-suffix (read-from-minibuffer "Video type: "))
;;                                    (read-from-minibuffer "From: "))
;;                       "-to" (read-from-minibuffer "To: ")
;;                       "-vf" "scale='trunc(iw/2):trunc(ih/2)'"
;;                       "-an"
;;                       (replace-regexp-in-string "\\.mp4" (concat "_small_"
;;                                                                  (string-join
;;                                                                   (split-string file-suffix)
;;                                                                   "_")
;;                                                                  ".mp4")
;;                                                 file)))))
;; (global-set-key (kbd "<C-delete>")
;;                 (lambda () (interactive)
;;                   (funcall (if (string= (buffer-name) "*scratch*")
;;                                'bury-buffer
;;                              (lambda ()
;;                                (and (buffer-file-name) (save-buffer))
;;                                (kill-buffer)
;;                                (delete-window))))))
;;
;;----------------------------------------------------------------------
;; DISMISS-WINDOW 
;;----------------------------------------------------------------------
;; Code to dismiss *Help* windows and other popups by saving and
;; restoring window configurations.
;; DEPRECATED: winner mode handles this better
;; DEPRECATED: popper mode now handles this


;;; Use C-` or ` to dismiss *Help* and *info* windows. If there are
;;; no *Help*/*info* windows open, C-` will cycle between this buffer
;;; and (other-buffer) instead, and ` will self-insert.
;; (global-set-key (kbd "C-`") 'bbuf-dismiss-or-switch)

;; (global-set-key (kbd "`") (lambda () (interactive)
;;                             (bbuf-dismiss-or-insert "`")))

;; (defvar bbuf-window-configuration nil
;;   "Variable to store a window configuration to restore later.
;;   Will be updated when a *Help* window springs up.")

;; (defvar bbuf-bury-buffer-list '("*help*"
;;                                 "*info*"
;;                                 "*compile-log*"
;;                                 "*apropos*"
;;                                 "*backtrace*"
;;                                 "*warning*"
;;                                 "*Warning*"
;;                                 "*Completions*"
;;                                 "*helpful")
;;   "List of buffer names that will be buried (with respective
;;   windows deleted) by bbuf-dismiss-windows")

;; ;;; Save window-configuration to bbuf-window-configuration
;; ;;; when a *Help* window pops up. 
;; ;;; (But only when there are no pre-existing *Help* buffers)
;; (add-hook 'help-mode-hook
;;           (lambda () 
;;             (bbuf-save-window-configuration "*Help*")))

;; (add-hook 'compilation-finish-functions
;;           (lambda ()
;;             (bbuf-save-window-configuration "*Compile-Log*")))

;; (add-hook 'apropos-mode-hook
;;           (lambda ()
;;             (bbuf-save-window-configuration "*Apropos*")))


;; (defun bbuf-save-window-configuration (buffer-name)
;;   "if buffer-name is not one of the currently displayed buffers,
;; save the current window configuration"
;;   (if (not (member buffer-name
;;                    (mapcar (lambda (w) (buffer-name 
;;                                         (window-buffer w))) 
;;                            (window-list))))
;;       (setq bbuf-window-configuration 
;;             (current-window-configuration))))

;; (defun bbuf-dismiss-windows (no-dismiss-window-function)
;;   "Restore the window configuration to the one just before
;; certain windows/buffers are created. The windows/buffers to
;; dismiss are given by buffer names in
;; bbuf-bury-buffer-list. If there are no windows to
;; dismiss, run no-dismiss-window-function instead.

;; Typically, running this function will bury any open *Help* buffer
;; and dismiss its window."
;;   (let ((buf (window-buffer (next-window))))
;;     (if (member (downcase (buffer-name buf))
;;                 bbuf-bury-buffer-list)
;;         (progn (bury-buffer buf)
;;                (set-window-configuration
;;                 bbuf-window-configuration))
;;       (funcall no-dismiss-window-function))))

;; (defun bbuf-dismiss-or-switch (arg)
;;   "Restore window configuration or cycle (current buffer)"
;;   (interactive "P")
;;   (bbuf-dismiss-windows 
;;    (lambda () (switch-to-buffer 
;;           (other-buffer (current-buffer) arg)))))

;; (defun bbuf-dismiss-or-insert (char)
;;   "Restore window configuration or insert a character"
;;   (bbuf-dismiss-windows
;;    (lambda () (insert char))))
;; ;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; SCHEME MODE
;;----------------------------------------------------------------------
;; ;; (defun mechanics ()
;; ;;   (interactive)
;; ;;   (run-scheme
;; ;;     "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib"
;; ;;   ))
;; (cond ((string= system-type "gnu/linux")
;;        (setq scheme-program-name "mit-scheme"))
      
;;       ((or (string= system-type "windows-nt") (string= system-type "cygwin"))
;;        (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
;;        (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
;;        (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;;        (add-hook 'scheme-mode-hook (function gambit-mode))
;;        (setq scheme-program-name "gsi -:t")))

;; ;; (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
;; ;; (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
;; ;; (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;; ;; (add-hook 'scheme-mode-hook (function gambit-mode))
;; ;; (setq scheme-program-name "gsi -:t")

;; (add-hook 'scheme-mode-hook 
;;           (lambda () 
;;             (define-key scheme-mode-map (kbd "C-c C-c") 
;;               'scheme-send-definition-and-go)
;;             (define-key scheme-mode-map (kbd "C-c M-e") 
;;               'scheme-compile-definition-and-go)))
