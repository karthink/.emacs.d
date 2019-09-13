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

