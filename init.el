                        ; my dot emacs grows ;
                     ; one day i look inside it ;
                           ; singularity ;

; Karthik's .emacs file

(setq gc-cons-threshold most-positive-fixnum)

(setq user-full-name "Karthik C")
; (setq user-mail-address "karthik[AT]gmail.com")

;;######################################################################
;; PATHS
;;######################################################################

;; Get custom-set-variables out of init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Set directory
(setq default-directory
      (cond ((equal (system-name) "surface")
             "/cygdrive/c/Users/karth/OneDrive/Documents/")
            ((equal (system-name) "cube")
             "/cygdrive/c/Users/karth/OneDrive/Documents/")
            ((equal (system-name) "thinkpad")
             "~/")
            (t "~/")))

;; Adds ~/.emacs.d to the load-path
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(eval-after-load "setup-org"
  (setq initial-buffer-choice (concat(getenv "HOME") "/do.org")))
;;########################################################################
;; CORE
;;########################################################################
(require 'setup-core)

;;########################################################################
;; UI FIXES
;;########################################################################

;; Get rid of the splash screen
;; Make *scratch* buffer suitable for writing
(setq inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Stop cursor from blinking
(blink-cursor-mode 0)
;; No fat cursors
(setq x-stretch-cursor nil)

;; Turn off the menu, tool bars, tooltips and the scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
;; Turn on image viewing
(auto-image-file-mode t)

;; Show me what I type, immediately
(setq echo-keystrokes 0.01)

;; Middle-click paste at point, not at cursor
(setq mouse-yank-at-point t)
;; Mouse available in terminal
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; Scrolling
(setq scroll-margin 0
      scroll-preserve-screen-position t)
      ;; mouse
;; (setq mouse-wheel-scroll-amount '(t ((shift) . 2))
;;       mouse-wheel-progressive-speed t)

;; (setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; remove continuation arrow on right fringe
;; (delq! 'continuation fringe-indicator-alist 'assq)

;; Don't resize emacs in steps.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

;; No popup dialogs
(setq use-dialog-box nil)
(if (bound-and-true-p tooltip-mode) (tooltip-mode -1))
;; native linux tooltips are ugly
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;; WINDOW SPLITTING
;; Set horizontal splits as the default
;; (setq split-width-threshold 120
;;       split-height-threshold 80)
 ;; Favor vertical splits over horizontal ones
(setq split-width-threshold 160
      split-height-threshold nil)

;; ;;;###package pos-tip
;; (setq pos-tip-internal-border-width 6
;;       pos-tip-border-width 1)
;; ;; Better fontification of number literals in code

;; (use-package! highlight-numbers
;;   :hook ((prog-mode conf-mode) . highlight-numbers-mode)
;;   :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; ;;;###package hide-mode-line-mode
;; (add-hook! '(completion-list-mode-hook Man-mode-hook)
;;            #'hide-mode-line-mode)

;;########################################################################
;; SAVE AND BACKUP
;;########################################################################
;; Put backups elsewhere:
(setq auto-save-interval 2400)
(setq auto-save-timeout 300)
(setq backup-directory-alist '(("." . "~/.emacs-backup"))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5 ; Old versions to keep
      )

;;######################################################################
;; AUTOLOADS
;;######################################################################
(require 'loaddefs nil t)

;;######################################################################
;; MISCELLANEOUS PREFERENCES
;;######################################################################

;; Prevent Emacs from bugging me about C-x n n not being
;; user-friendly.
(put 'narrow-to-region 'disabled nil)

;; For lazy typists
(fset 'yes-or-no-p 'y-or-n-p)
;; Move the mouse away if the cursor gets close
(mouse-avoidance-mode 'animate)

;; highlight the current line, as in Matlab
;; (global-hl-line-mode)

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

;; when you mark a region, you can delete it or replace it as in other
;; Windows programs. simply hit delete or type whatever you want or
;; yank
(delete-selection-mode)

; show the matching parentheses immediately
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(show-paren-mode 1)

;; (use-package! paren
;;   ;; highlight matching delimiters
;;   :after-call after-find-file doom-switch-buffer-hook
;;   :config
;;   (setq show-paren-delay 0.1
;;         show-paren-highlight-openparen t
;;         show-paren-when-point-inside-paren t)
;;   (show-paren-mode +1))

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; FULLSCREEN
(global-set-key [f11] 'toggle-frame-fullscreen)

;; Byte-compile elisp files immediately after saving them if .elc exists:
(defun auto-byte-recompile ()
"If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
file corresponding to the current buffer file, then recompile the file."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)

(global-prettify-symbols-mode 1)

;;######################################################################
;; PACKAGE MANAGEMENT
;;######################################################################
;;; Set load paths for ELPA packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(add-hook 'package-menu-mode-hook 'hl-line-mode)

;;######################################################################
;; INTERFACING WITH THE OS
;;######################################################################

(if (equal (system-name) 'windows-nt)
    (setq shell-file-name "C:/cygwin/cygwin.bat"))

;; Set default www browser
(if (equal system-type 'gnu/linux)
    (setq
     ;browse-url-browser-function 'browse-url-generic
     browse-url-generic-program "/usr/bin/qutebrowser"
     ))

;; Consult clipboard before primary selection
;; http://www.gnu.org/software/emacs/manual/
;; html_node/emacs/Clipboard.html
(setq select-enable-clipboard t)

;; Arrange for Emacs to notice password prompts and turn off echoing for them, as follows:
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)

;; Get rid of the annoying system beep
;; (setq visible-bell t)
(setq ring-bell-function (lambda ()
                           (call-process-shell-command
                            "xset led 3; xset -led 3" nil 0 nil)))

;; Key to run current line as bash command
(global-set-key (kbd "C-!") 'shell-command-at-line)

(defun shell-command-at-line (&optional prefix)
  "Run contents of line around point as a shell command and replace the line with output. With a prefix argument, append the output instead"
  (interactive "P")
  (let ( (command (thing-at-point 'line)) )
    (cond ((null prefix)
           (kill-whole-line)
           (indent-according-to-mode))
          (t (newline-and-indent)))
    (shell-command command t nil)
    (exchange-point-and-mark)))

(defun describe-word-at-point (&optional prefix)
  "Briefly describe word at point. With PREFIX argument, show
verbose descriptions with hyperlinks."
  (interactive "P")
  (let ( (word (thing-at-point 'word)) )
    (shell-command (concat "dict " word (cond ((null prefix) nil)
                                                  (t " -v"))))))

(defun describe-word (word &optional prefix)
  "Briefly describe WORD entered by user. With PREFIX argument,
show verbose descriptions with hyperlinks."
  (interactive "sDescribe word: \nP")
  (shell-command (concat "dict " word (cond ((null prefix) nil)
                                                (t " -v")))))

;;----------------------------------------------------------------------
;; ESHELL PREFERENCES
;;----------------------------------------------------------------------
(setq eshell-buffer-shorthand t)
(defun delete-window-if-not-single ()
  "Delete window if not the only one"
  (when (not (one-window-p))
    (delete-window)))
(advice-add 'eshell-life-is-too-much :after 'delete-window-if-not-single)

;;######################################################################
;; FONTS
;;######################################################################
(cond ((equal system-type 'gnu/linux)
       (custom-set-faces
         '(default ((t (:family "Fantasque Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 125 :width normal))))))
      ((equal system-type 'windows-nt)
       (custom-set-faces
        '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Consolas"))))))
      ((equal system-type 'cygwin)
       (custom-set-faces
        '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))))

;;######################################################################
;; LINE NUMBERS
;;######################################################################
(line-number-mode 1)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;;######################################################################
;; EDITING
;;######################################################################
(require 'better-editing nil t)

;;######################################################################
;; BUFFER MANAGEMENT
;;######################################################################
(require 'better-buffers nil t)
;; (require 'popup-buffers nil t)
(autoload 'popup-buffers-update-open-popups "popup-buffers")
(add-hook 'window-configuration-change-hook 'popup-buffers-update-open-popups)
(global-set-key (kbd "C-`") 'popup-buffers-toggle-latest) 
(global-set-key (kbd "M-`") 'popup-buffers-cycle)
;; (global-set-key (kbd "<f7>") 'popup-buffers-close-latest)
;; (global-set-key (kbd "<f8>") 'popup-buffers-open-latest)

(use-package winner
  :commands winner-undo
  ;; :bind ("C-c <left>" . winner-undo)
  :config 
  (winner-mode +1))
;; (use-package! winner
;;   ;; undo/redo changes to Emacs' window layout
;;   :after-call after-find-file doom-switch-window-hook
;;   :preface (defvar winner-dont-bind-my-keys t)
;;   :config (winner-mode +1)) ; I'll bind keys myself



;;######################################################################
;; UTILITY
;;######################################################################
;; Count words, print ASCII table, etc
(require 'utilities nil t)
;; Colorize color names in buffers
(use-package rainbow-mode
  :ensure t
  :config
  (setq rainbow-delimiters-max-face-count 3)
  (rainbow-mode))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

(defun doom/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (sudo-find-file (file-truename buffer-file-name)))

(global-set-key (kbd "C-x C-S-f") 'sudo-find-file)

;;######################################################################
;; COMPILATION
;;######################################################################

;; compile!
(use-package compile
  :defer t
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (global-set-key [(f9)] 'compile)
  (global-set-key [(f10)] 'recompile)
  
  (defun +apply-ansi-color-to-compilation-buffer-h ()
    "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
    (with-silent-modifications
      (ansi-color-apply-on-region compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook #'+apply-ansi-color-to-compilation-buffer-h)
  (add-hook 'compilation-finish-functions
            (lambda (buf str)

              (if (or
                   (string-match "exited abnormally" str)
                   (string-match "matches found" str))
                  ;;there were errors
                  (message "Press M-g n/p to visit")

                ;;no errors, make the compilation window go away in 0.5 seconds
                (save-excursion
                  (run-at-time 1.0 nil 'bury-buffer buf))
                (message "NO COMPILATION ERRORS!")))))

;;######################################################################
;; LANGUAGE MODES
;;######################################################################

;;----------------------------------------------------------------------
;; AUCTEX-MODE & ADDITIONS
;;----------------------------------------------------------------------
(use-package auctex
  :ensure t
  :defines (TeX-auto-save
            TeX-parse-self
            TeX-electric-escape
            TeX-PDF-mode
            TeX-source-correlate-method
            TeX-newline-function
            TeX-view-program-list
            TeX-view-program-selection
            TeX-mode-map)
  :bind (:map TeX-mode-map
              ("M-SPC" . TeX-matrix-spacer)
              ("C-M-9" . TeX-insert-smallmatrix)
              ("C-M-]" . TeX-insert-bmatrix)
              ("C-;" . TeX-complete-symbol))
  :config
  (progn  (defun TeX-matrix-spacer () (interactive) (insert " & "))
          (defun TeX-insert-smallmatrix () (interactive)
            (insert "[\\begin{smallmatrix}  \\end{smallmatrix}]")
            (backward-char 19))
          (defun TeX-insert-bmatrix () (interactive)
            (insert "\\begin{bmatrix}  \\end{bmatrix}")
            (backward-char 14))
          (setq
           TeX-auto-save t
           TeX-parse-self t
           TeX-electric-escape nil
           TeX-PDF-mode t)
         (setq-default TeX-source-correlate-mode t)
         (setq TeX-source-correlate-method 'synctex)
         (setq-default TeX-source-correlate-start-server t)
         (setq TeX-newline-function 'reindent-then-newline-and-indent)
         (cond ((equal system-type 'cygwin)
                (setq TeX-view-program-list
                      '(("Sumatra PDF" ("\"/cygdrive/c/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                                        (mode-io-correlate " -forward-search %b %n ") " %o"))))

                (setq TeX-view-program-selection
                      '(((output-dvi has-no-display-manager) "dvi2tty")
                        ((output-dvi style-pstricks) "dvips and gv")
                        (output-dvi "xdvi")
                        (output-pdf "pdf-tools")
                        (output-html "xdg-open"))))
               ((equal system-type 'gnu/linux)
                (setq TeX-view-program-selection
                      '(((output-dvi has-no-display-manager) "dvi2tty")
                        ((output-dvi style-pstricks) "dvips and gv")
                        (output-dvi "xdvi")
                        (output-pdf "Zathura")
                        (output-html "xdg-open"))))
               )
         (TeX-fold-mode 1)
         ))

(use-package reftex
  ;; :defer 3
  :commands turn-on-reftex
  :config
  (progn (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
         (add-hook 'latex-mode-hook 'turn-on-reftex)
         (setq reftex-plug-into-AUCTeX t)))
;; (autoload 'reftex "reftex")

;; (setq-default TeX-master nil)
(use-package cdlatex
  :ensure t
  ;; :defer 2
  :commands turn-on-cdlatex
  :config
  (progn
    (setq cdlatex-command-alist
          '(("vc" "Insert \\vect{}" "\\vect{?}"
             cdlatex-position-cursor nil nil t)
            ("smat" "Insert smallmatrix env"
             "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
             cdlatex-position-cursor nil nil t)
            ("bmat" "Insert bmatrix env"
             "\\begin{bmatrix}\n?\n\\end{bmatrix}"
             cdlatex-position-cursor nil nil t)
            ("pmat" "Insert pmatrix env"
             "\\begin{pmatrix}\n?\n\\end{pmatrix}"
             cdlatex-position-cursor nil nil t)
            ("equ*" "Insert equation* env"
             "\\begin{equation*}\n?\n\\end{equation*}"
             cdlatex-position-cursor nil t nil)
            ("ssn" "Insert subsection env"
             "\\subsection{?}"
             cdlatex-position-cursor nil t nil)
            ("ssn*" "Insert subsection* env"
             "\\subsection*{?}"
             cdlatex-position-cursor nil t nil)))
    (setq cdlatex-math-symbol-alist '((?F ("\\Phi"))
                                      (?o ("\\omega" "\\mho" "\\mathcal{O}"))))
    (setq cdlatex-paired-parens "$[{("))
  :hook (LaTeX-mode . turn-on-cdlatex))

;;----------------------------------------------------------------------
;; MATLAB
;;----------------------------------------------------------------------
(use-package matlab
  ;; :after 'evil
  ;; :commands (matlab-mode matlab-shell matlab-shell-run-block)
  :bind (:map matlab-mode-map
              ("C-c C-b" . 'matlab-shell-run-block))
  :init
  (add-hook 'matlab-mode-hook #'company-mode-on)
  (add-hook 'matlab-mode-hook #'hs-minor-mode)
  ;; (add-hook 'matlab-mode-hook #'turn-on-evil-matlab-textobjects-mode)
  (add-hook 'matlab-shell-mode-hook #'company-mode-on)

  ;; (define-key matlab-mode-map (kbd "C-c C-b") #'matlab-shell-run-block)  
  
  ;; :config
  (add-hook 'matlab-shell-mode-hook (lambda () (interactive)
                                      (define-key matlab-shell-mode-map (kbd "C-<tab>") nil)))
  
  (defun matlab-select-block ()
    (save-excursion
      (let ((block-beg (search-backward-regexp "^%%" nil t))
            (block-end (search-forward-regexp "^%%" nil t 2)))
        (cons block-beg block-end))))

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
        (matlab-shell-run-region beg end))))
  
(defun matlab-forward-section ()
  "Move forward section in matlab mode"
  (interactive)
  (beginning-of-line 2)
  (re-search-forward "^%%" nil t)
  (match-end 0))

(defun matlab-backward-section ()
  "Move forward section in matlab mode"
  (interactive)
  (re-search-backward "^%%" nil t)
  (match-beginning 0))

  )

;;######################################################################
;; PLUGINS
;;######################################################################

;;----------------------------------------------------------------------
;; NAV-FLASH
;;----------------------------------------------------------------------
;; (use-package nav-flash)

;;----------------------------------------------------------------------
;; YASNIPPET
;;----------------------------------------------------------------------

;; (use-package yasnippet

;;   :config
;;   ;; Redefine yas expand key from TAB because company-mode uses TAB.
;;   (define-key yas-minor-mode-map (kbd "C-j") 'yas-expand)
;;   (define-key yas-keymap "\C-j" 'yas-next-field-or-maybe-expand)
;;   (dolist (keymap (list yas-minor-mode-map yas-keymap))
;;     (define-key keymap (kbd "TAB") nil)
;;     (define-key keymap [(tab)] nil))
;;   )

;;----------------------------------------------------------------------
;; HIDESHOW is built in
;;----------------------------------------------------------------------
(use-package hideshow ; built-in
  :commands (hs-toggle-hiding
             hs-hide-block
             hs-hide-level
             hs-show-all
             hs-hide-all)
  :config
  (setq hs-hide-comments-when-hiding-all nil)
  ;; (setq hs-hide-comments-when-hiding-all nil
  ;;       ;; Nicer code-folding overlays (with fringe indicators)
  ;;       hs-set-up-overlay #'+fold-hideshow-set-up-overlay-fn)

  (dolist (hs-command (list #'hs-toggle-hiding 
                            #'hs-hide-block 
                            #'hs-hide-level 
                            #'hs-show-all 
                            #'hs-hide-all))
    (advice-add hs-command :before
                (lambda () "Advice to ensure `hs-minor-mode' is enabled"
                  (unless (bound-and-true-p hs-minor-mode)
                    (hs-minor-mode +1)))))
  
  ;; (defadvice! +fold--hideshow-ensure-mode-a (&rest _)
  ;;   "Ensure `hs-minor-mode' is enabled."
  ;;   :before '(hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
  ;;   (unless (bound-and-true-p hs-minor-mode)
  ;;     (hs-minor-mode +1)))

  ;; extra folding support for more languages
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append
           '((vimrc-mode "{{{" "}}}" "\"")
             ;; (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
             ;;            ""
             ;;            "#"
             ;;            +fold-hideshow-forward-block-by-indent-fn nil)
             ;; (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp-fn nil)
             ;; (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
             ;;            "end\\|[]}]"
             ;;            "#\\|=begin"
             ;;            ruby-forward-sexp)
             ;; (enh-ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
             ;;                "end\\|[]}]"
             ;;                "#\\|=begin"
             ;;                enh-ruby-forward-sexp nil)
             (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch\\|function\\|^%%"
                          "end\\|^%%"
                          "%[^%]" (lambda (_arg) (matlab-forward-sexp)))
             (nxml-mode "<!--\\|<[^/>]*[^/]>"
                        "-->\\|</[^/>]*[^/]>"
                        "<!--" sgml-skip-tag-forward nil))
           hs-special-modes-alist
           '((t))))))

;;----------------------------------------------------------------------
;; VIMISH-FOLD
;;----------------------------------------------------------------------
;; (use-package vimish-fold
;;   :ensure t
;;   )

;;----------------------------------------------------------------------
;; EDIFF (built-in)
;;----------------------------------------------------------------------
(use-package ediff
  :defer t
  :functions ediff-setup-windows-plain
  :init
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defvar +ediff-saved-wconf nil)
  (defun +ediff-save-wconf-h ()
      (setq +ediff-saved-wconf (current-window-configuration)))
  (defun +ediff-restore-wconf-h ()
    (when (window-configuration-p +ediff-saved-wconf)
      (set-window-configuration +ediff-saved-wconf)))
  ;; Restore window config after quitting ediff
  (add-hook 'ediff-before-setup-hook #'+ediff-save-wconf-h)
  (add-hook 'ediff-quit-hook #'+ediff-restore-wconf-h t)
  (add-hook 'ediff-suspend-hook #'+ediff-restore-wconf-h t)
)

;;----------------------------------------------------------------------
;; HELPFUl
;;----------------------------------------------------------------------
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable)
  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h C") #'helpful-command)
  (global-set-key (kbd "C-h .") #'helpful-at-point)
  (global-set-key (kbd "C-h C-.") #'helpful-at-point))

;;----------------------------------------------------------------------
;; SHACKLE
;;----------------------------------------------------------------------
(use-package shackle
  :ensure t
  :init (shackle-mode))

;;----------------------------------------------------------------------
;; VERSION CONTROL
;;----------------------------------------------------------------------
(use-package magit
  ;; :defer 4
  :commands magit-status
  :ensure t)

;;----------------------------------------------------------------------
;; WHICH-KEY
;;----------------------------------------------------------------------
(use-package which-key
  :ensure t
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'which-key-init-buffer-hook 
            (lambda () (setq-local line-spacing 3)))

  ;; (which-key-add-key-based-replacements doom-leader-key "<leader>")
  ;; (which-key-add-key-based-replacements doom-localleader-key "<localleader>")
  (which-key-mode +1)
  
  :diminish "")

;;----------------------------------------------------------------------
;; CALC
;;----------------------------------------------------------------------
(defun calc-on-line () (interactive)
  (cond ((region-active-p)
         (let* ((beg (region-beginning))
                (end (region-end))
                (string (buffer-substring-no-properties beg end)))
           (kill-region beg end)
           (insert (calc-eval string))))
        (t (end-of-line) (insert " = " (calc-eval (thing-at-point 'line))))))
(global-set-key (kbd "C-S-e") 'calc-on-line)

;;----------------------------------------------------------------------
;; ABBREV MODE
;;----------------------------------------------------------------------
(setq save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;;----------------------------------------------------------------------
; COMPANY-MODE
;;----------------------------------------------------------------------
(use-package company
  :ensure t
  :defer 1
  :diminish "c->"
  :config
  ;; (add-to-list 'company-backends 'company-files)
  ;; (add-to-list 'company-backends 'company-dabbrev)
  ;; (add-to-list 'company-backends 'company-jedi)
  ;; (add-to-list 'company-backends 'company-dict)

  (global-company-mode)
  (setq company-idle-delay 0.15
        company-dabbrev-downcase 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-code-other-buffers t
        company-dabbrev-ignore-case nil
        company-transformers '(company-sort-by-occurrence)
        company-global-modes
        '(not erc-mode message-mode
              help-mode gud-mode eshell-mode
              package-menu-mode)
        company-backends '(company-files company-capf company-dabbrev)
        ) 
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  ;; (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  
  (define-key company-active-map (kbd "C-w") nil)
  (define-key company-active-map (kbd "C-]") 'company-show-location)
   
  ;; AC-mode style settings
  (defun company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (defun my-company-visible-and-explicit-action-p ()
      (and (company-tooltip-visible-p)
           (company-explicit-action-p)))
    (setq company-require-match nil)
    (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  ;; (setq company-frontends
  ;;       '(company-pseudo-tooltip-unless-just-one-frontend
  ;;         company-preview-frontend
  ;;         company-echo-metadata-frontend))
    (setq company-frontends '(company-echo-metadata-frontend
                              company-pseudo-tooltip-unless-just-one-frontend-with-delay
                              company-preview-frontend))
    (define-key company-active-map [tab]
      'company-select-next-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "TAB")
      'company-select-next-if-tooltip-visible-or-complete-selection))
  
  ;; Tab'n'Go style settings
  (defun company-tng-setup ()
    (define-key company-active-map (kbd "TAB") 'company-select-next)
    (define-key company-active-map (kbd "<tab>") 'company-select-next)
    (define-key company-active-map (kbd "<ret>") nil)
    (setq company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-tng-frontend
            company-echo-metadata-frontend)))

  ;; (setq company-idle-delay 0)
  ;; (setq company-echo-delay 0)
  ;; (setq company-require-match nil)
  ;; (setq company-tooltip-flip-when-above t)
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  ;; (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  
  ;; Not needed. cdlatex mode handles completion just fine
  ;; (use-package company-auctex
  ;;   :defer t
  ;;   :config
  ;;   (add-to-list 'company-backends 'company-auctex)
  ;;   (company-auctex-init))
  
  (company-ac-setup)
  ;; (company-tng-setup)
  
  (use-package company-statistics
    :ensure t
    :init
    (add-hook 'after-init-hook #'company-statistics-mode)))

;;----------------------------------------------------------------------
;; SMARTPARENS-MODE
;;----------------------------------------------------------------------
(use-package smartparens
  ;; :defer 5
  :ensure t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (define-key smartparens-mode-map (kbd "M-<up>") 'sp-raise-sexp)
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-<right>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-barf-sexp)
  )

;;----------------------------------------------------------------------
;; EXPAND-REGION
;;----------------------------------------------------------------------
(use-package expand-region
  :ensure t
  :commands expand-region
  :bind ("C-," . 'er/expand-region))

;;----------------------------------------------------------------------
;; ACE-JUMP-MODE
;;----------------------------------------------------------------------
(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :bind ("C-'" . 'ace-jump-mode))

;;----------------------------------------------------------------------
;; IY-GO-TO-CHAR
;;----------------------------------------------------------------------
(require 'iy-go-to-char nil t)
(when (featurep 'iy-go-to-char)
  (define-key global-map (kbd "M-s") 'iy-go-to-char)
  (define-key global-map (kbd "M-r") 'iy-go-to-char-backward))

;;----------------------------------------------------------------------
;; WRAP-REGION MODE
;;----------------------------------------------------------------------
(use-package wrap-region
  :ensure t
  :init (wrap-region-mode 1))
;; (require 'wrap-region nil t)
;; (add-hook 'text-mode-hook 'wrap-region-mode)

;;----------------------------------------------------------------------
;; IVY/COUNSEL/SWIPER
;;----------------------------------------------------------------------
(require 'setup-ivy)
;;; Bibtex management from ivy. Call ivy-bibtex.
(use-package ivy-bibtex
  :ensure t
  :functions bibtex-completion-open-pdf
  :commands ivy-bibtex
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key
        ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
                                (t . ivy--regex-plus))
        bibtex-completion-bibliography (getenv "BIB")
        bibtex-completion-library-path '("~/Documents/research/lit")
        bibtex-completion-pdf-field "File"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-symbol "⌘"
        bibtex-completion-notes-symbol "✎")

  (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
    (let* ((pdf-reader (getenv "READER"))
           (bibtex-completion-pdf-open-function
               (lambda (fpath) (start-process pdf-reader "*ivy-bibtex-evince*" pdf-reader fpath))))
      (bibtex-completion-open-pdf keys fallback-action)))

  (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)

  (ivy-add-actions
   'ivy-bibtex
   '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)"))))

;;----------------------------------------------------------------------
;; TRAMP
;;----------------------------------------------------------------------
;; Tramp ssh'es into root@host to edit files. The emacs sudo, kindof.
(autoload 'tramp "tramp")

;;----------------------------------------------------------------------
;; DIRED
;;----------------------------------------------------------------------
;; Dired preferences
(require 'setup-dired nil t)

;;----------------------------------------------------------------------
;; ORG-MODE
;;----------------------------------------------------------------------
;;;; Org mode
;; org-init.el is loaded from the "lisp" directory.
(require 'setup-org nil t)
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;;----------------------------------------------------------------------
;; MACROS
;;----------------------------------------------------------------------
;; Bind call last macro to F4
(global-set-key (kbd "<f3>") 'kmacro-start-macro)
(global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro)

;;######################################################################
;; COLORS & COLOR THEMES
;;######################################################################

;;; Load theme after the frame is created.
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (load-theme 'gruvbox-dark-hard t)
;;             ;; (load-theme 'smart-mode-line-dark t)
;; ))

;; Create function for line number format
;; Basic idea is to align line number to the right and to make
;; height of linum dependent of total line numbers.

;; (defun linum-format-func (line)
;;   "Format LINE for 'linum-mode'."
;;   (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
;;     (propertize (format (format "%%%dd%c" w ?\x2007) line) 'face 'linum)))
;; ;; Set last function as linum formatter
;; (setq linum-format 'linum-format-func)

;; before loading new theme
(defun load-theme--disable-old-theme(theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)

;; After loading new theme

;; (defun load-theme--restore-line-numbering(theme &rest args)
;;   "Set linum-format again after loading any theme."
;;   (setq linum-format 'linum-format-func))
;; (advice-add 'load-theme :after #'load-theme--restore-line-numbering)

;;######################################################################
;; MODELINE:
;;######################################################################

(use-package smart-mode-line
  :ensure t
  :init (sml/setup))

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generally unhelpful and only add confusing visual clutter.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defvar mode-line-cleaner-alist
  `((company-mode . " Ĉ")
    (yas-minor-mode . " Υ")
    (smartparens-mode . " )(")
    (evil-smartparens-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (evil-snipe-local-mode . "")
    (evil-owl-mode . "")
    (evil-rsi-mode . "")
    (evil-commentary-mode . "")
    (ivy-mode . "")
    (wrap-region-mode . "")
    (rainbow-mode . "")
    (which-key-mode . "")
    (undo-tree-mode . " ⎌")
    (auto-revert-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "Eλ")
    (nxhtml-mode . "nx")
    (dot-mode . " .")
    (scheme-mode . " SCM")
    (matlab-mode . "M")
    (org-mode . "⦿")
    (latex-mode . "TeX"))
  "Alist for `clean-mode-line'.

; ;; When you add a new element to the alist, keep in mind that you
; ;; must pass the correct minor/major mode symbol and a string you
; ;; want to use in the modeline *in lieu of* the original.")


(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)

; (display-time-mode 0)

;;######################################################################
;; MINIBUFFER
;;######################################################################

;; Enable recursive minibuffer edits
(setq enable-recursive-minibuffers 1)

;;######################################################################
;; EVIL-MODE
;;######################################################################
(require 'setup-evil)

