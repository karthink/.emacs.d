                        ; my dot emacs grows ;
                     ; one day i look inside it ;
                           ; singularity ;

; Karthik's .emacs file 
; 02 June 2019 

(setq user-full-name "Karthik C")
; (setq user-mail-address "karthik[AT]gmail.com")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-interval 2400)
 '(auto-save-timeout 300)
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "80ae3a89f1eca6fb94a525004f66b544e347c6f756aaafb728c7cdaef85ea1f5" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (god-mode use-package fzf evil-surround gruvbox-theme ido-completing-read+ cdlatex evil-commentary evil-goggles evil-paredit evil-replace-with-register iy-go-to-char smex ido-grid-mode composable evil ace-jump-mode wolfram-mode auto-complete julia-repl julia-shell julia-mode matlab-mode auctex dash deferred request-deferred s dash-functional ein ein-mumamo color-theme-modern hc-zenburn-theme labburn-theme zenburn-theme yasnippet expand-region multiple-cursors)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tex-dvi-view-command
   (quote
    (cond
     ((eq window-system
          (quote x))
      "evince")
     ((eq window-system
          (quote w32))
      "yap")
     (t "dvi2tty * | cat -s"))))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

(cond ((equal system-type 'gnu/linux)
       ;; (custom-set-faces
       ;;  '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "CYRE" :family "Inconsolata")))))
       )
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
(setq display-line-numbers 'relative)

;;######################################################################
;; PATHS
;;######################################################################

;; Set directory
(setq default-directory 
      (cond ((equal (system-name) "surface")
             "/cygdrive/c/Users/karthik/OneDrive/Documents/")
            ((equal (system-name) "cube")
             "/cygdrive/c/Users/karth/OneDrive/Documents/")
            (t "~/")))
;; Adds ~/.emacs.d to the load-path
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;######################################################################
;; PACKAGE MANAGEMENT
;;######################################################################
;;; Set load paths for ELPA packages
;; (require 'cl-lib)
;; (require 'package)
;;(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
 
;; (require 'diminish)
;; (require 'bind-key)
;; (package-initialize)

;; (when (featurep 'package)
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;;######################################################################
;; PERSONAL
;;######################################################################
;;; My account data for Weblogger, Jabber & other services
(require 'personal nil t)

;;######################################################################
;; EDITING
;;######################################################################
(require 'better-editing nil t)

;;######################################################################
;; BUFFER MANAGEMENT
;;######################################################################
(require 'better-buffers nil t)

;;######################################################################
;; UTILITY FUNCTIONS
;;######################################################################
;; Count words, print ASCII table, etc
(require 'utilities nil t)

;;######################################################################
;; INTERFACING WITH THE OS
;;######################################################################

(if (equal (system-name) 'windows-nt)
    (setq shell-file-name "C:/cygwin/cygwin.bat"))

;; Set default www browser
(if (equal system-type 'gnu/linux)
    (setq 
     ;browse-url-browser-function 'browse-url-generic
     browse-url-generic-program "/usr/bin/palemoon"
     ))

;; Consult clipboard before primary selection
;; http://www.gnu.org/software/emacs/manual/
;; html_node/emacs/Clipboard.html
(setq select-enable-clipboard t)

;; Arrange for Emacs to notice password prompts and turn off echoing for them, as follows:
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

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

;;######################################################################
;; COMPILATION
;;######################################################################

;; compile!
(global-set-key [(f9)] 'compile)
(global-set-key [(f10)] 'recompile)
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
              (message "NO COMPILATION ERRORS!"))))

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

;;######################################################################
;; LANGUAGE MODES
;;######################################################################

;;----------------------------------------------------------------------
;; JULIA-MODE
;;----------------------------------------------------------------------
(defun my-julia-mode-hooks ()
  (require 'julia-shell-mode))
(add-hook 'julia-mode-hook 'my-julia-mode-hooks)
;; (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;; (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)

;;----------------------------------------------------------------------
;; LUA-MODE
;;----------------------------------------------------------------------
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;----------------------------------------------------------------------
;; SCHEME MODE
;;----------------------------------------------------------------------
(require 'setup-scheme nil t)

;;---------------------------------------------------------------------- 
;; MATH-MODE
;;---------------------------------------------------------------------- 
;; Major mode for running Mathematica in Emacs
;; (eval-after-load 'math 
;;  '(define-key math-mode-map (kbd "<tab>") 'math-complete-symbol))

;;----------------------------------------------------------------------
;; AUCTEX-MODE & ADDITIONS
;;---------------------------------------------------------------------- 
(use-package latex 
  :ensure t
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
                                        (mode-io-correlate " -forward-search %b %n ") " %o"))))))
	 (setq TeX-view-program-selection
          '(((output-dvi has-no-display-manager) "dvi2tty")
           ((output-dvi style-pstricks) "dvips and gv")
           (output-dvi "xdvi")
           (output-pdf "Sumatra PDF")
           (output-html "xdg-open")))
	 (TeX-fold-mode 1))
  )

;; (setq-default TeX-master nil)
(use-package cdlatex
  :ensure t
  :defer t
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

;; (autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
;; (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

;;######################################################################
;; PLUGINS
;;######################################################################

;;----------------------------------------------------------------------
;; PRETTY LAMBDA MODE
;;----------------------------------------------------------------------
(require 'pretty-lambdada)
(add-hook 'emacs-lisp-mode-hook 'pretty-lambda)
(add-hook 'scheme-mode-hook 'pretty-lambda)
(add-hook 'lisp-mode-hook 'pretty-lambda)
(add-hook 'lisp-interaction-mode-hook 'pretty-lambda)
(add-hook 'inferior-scheme-mode-hook 'pretty-lambda)
;;(pretty-lambda-for-modes)

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
;; AUTO-COMPLETE MODE
;;----------------------------------------------------------------------
;;; ELPA package, run (package-initialize) first

(use-package auto-complete
  :ensure t
  :commands global-auto-complete-mode
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode 1)))

;; (require 'auto-complete-config nil t)
;; (setq ac-auto-show-menu t
;;       ac-auto-start t
;;       ac-show-menu-immediately-on-auto-complete t) 

;;---------------------------------------------------------------------
;; PAREDIT-MODE
;;---------------------------------------------------------------------
(use-package paredit
  :commands enable-paredit-mode
  :hook ((emacs-lisp-mode 
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode) . enable-paredit-mode)
  ;; (autoload 'enable-paredit-mode "paredit"
  ;; "Turn on pseudo-structural editing of Lisp code." t)
  ;; :bind (:map paredit-mode-map
  ;;             ;; ("M-s" . nil)
  ;;             ;; ("M-r" . nil)
  ;;             ;; ("<M-up>" . nil)
  ;;             ;; ("<M-down>" . nil)
  ;;             ("C-c <up>" . 'paredit-splice-sexp-killing-backward)
  ;;             ("C-c <down>" . 'paredit-splice-sexp-killing-forward)
  ;;             ("C-c s" . 'paredit-splice-sexp)
  ;;             ("C-c r" . 'paredit-raise-sexp))
  )

;;----------------------------------------------------------------------
;; MULTIPLE-CURSORS 
;;----------------------------------------------------------------------
;;; ELPA package, run (package-initialize) first
(require 'multiple-cursors nil t)
(when (featurep 'multiple-cursors)
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)  
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-n") 'mc/mark-next-word-like-this) 
  (global-set-key (kbd "M-@") 'mc/mark-all-words-like-this)
  (global-set-key (kbd "C-@") 'mc/mark-all-like-this)
  (global-set-key (kbd "M-p") 'mc/mark-previous-word-like-this) 
  (global-set-key (kbd "<C-return>") 'set-rectangular-region-anchor) 
  ;; (global-set-key (kbd "C-x C-a") 'mc/edit-beginnings-of-lines)     
  ;; (global-set-key (kbd "C-x SPC") 'mc/mark-all-dwim)                
  (global-set-key (kbd "M-N") 'mc/insert-numbers)
  (global-set-key (kbd "M-S") 'mc/sort-regions))

;;----------------------------------------------------------------------
;; EXPAND-REGION
;;----------------------------------------------------------------------
;;; ELPA package, run (package-initialize) first
(use-package expand-region
  :commands expand-region
  :init
  (progn (bind-key "C-=" 'er/expand-region)
         (bind-key "C-," 'er/expand-region)))

;; (require 'expand-region nil t)
;; (when (featurep 'expand-region)
;;   (global-set-key (kbd "C-=") 'er/expand-region)
;;   (global-set-key (kbd "C-,") 'er/expand-region))

;;----------------------------------------------------------------------
;; ACE-JUMP-MODE
;;----------------------------------------------------------------------
;; Ace jump mode major function
;; (autoload
;;     'ace-jump-mode
;;     "ace-jump-mode"
;;     "Emacs quick move minor mode" t)
(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "M-j" 'ace-jump-mode))
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
(require 'wrap-region nil t)
(add-hook 'text-mode-hook 'wrap-region-mode)

;;----------------------------------------------------------------------
;; IDO-MODE.
;;----------------------------------------------------------------------
(require 'ido nil t)

(when (featurep 'ido) 
  
  (ido-mode t)
  (setq ido-enable-flex-matching t) ;; enable fuzzy matching
  (ido-everywhere 1)
  ;; (if (require 'ido-completing-read+))
  (ido-ubiquitous-mode 1)
  (require 'icomplete)
  (icomplete-mode 1)
  
  (autoload 'idomenu "idomenu" nil t)
  (eval-after-load "idomenu"
    (global-set-key (kbd "M-.") 'imenu))
  
  ;; Custom keybindings
  (defun ido-my-keys ()
    (mapc (lambda (K) 
            (let* ((key (car K)) (fun (cdr K)))
              (define-key ido-completion-map (edmacro-parse-keys key) fun)))
          '(("C-n" . ido-next-match)
            ("C-p"  . ido-prev-match))))
  

  ;; Enable ido-completion over TAGS
  (defun ido-find-file-in-tag-files ()
    (interactive)
    (save-excursion
      (let ((enable-recursive-minibuffers t))
        (visit-tags-table-buffer))
      (find-file
       (expand-file-name
        (ido-completing-read
         "Project file: " (tags-table-files) nil t)))))

  ;; (define-key ido-mode-map (kbd "C-x t") 'ido-find-file-in-tag-files)

  (add-hook 'ido-setup-hook (lambda nil
                              (ido-my-keys)))

  (require 'ido-other-window nil t))

(when (commandp 'ido-grid-mode)
  (ido-grid-mode 1))

;;  -x. Use with care!
;;; Smex
;;(require 'smex nil t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-S-x") 'smex-major-mode-commands)

;; (global-set-key
;;     "\M-x"
;;     (lambda ()
;;       (interactive)
;;       (call-interactively
;;        (intern
;;         (ido-completing-read
;;          "M-x "
;;          (all-completions "" obarray 'commandp))))))

;;----------------------------------------------------------------------
;; AUTOPAIR and WRAP-REGION
;;----------------------------------------------------------------------
;; Better paren handling:
;; (require 'autopair)
;; (autopair-global-mode)

;; (require 'wrap-region)
;; (wrap-region-global-mode t)

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
;; BOOKMARKS
;;----------------------------------------------------------------------
;; Bookmark+ mode
;;(require 'bookmark+)
(defalias 'bs 'bookmark-set)
(defalias 'bl 'bookmark-bmenu-list)
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

;;----------------------------------------------------------------------
;; ORG-MODE
;;----------------------------------------------------------------------
;;;; Org mode
;; org-init.el is loaded from the "lisp" directory.
(require 'setup-org nil t)
;; Keybindings for org-mode and org-remember
;; (when (featurep 'org-init)
;;   (global-set-key "\C-cr" 'remember)
;;   (global-set-key "\C-\M-r" 'org-remember))

;;----------------------------------------------------------------------
;; DOT-MODE
;;----------------------------------------------------------------------
;; (Vi like redo edits with C-.)
(require 'dot-mode nil t)
(when (featurep 'dot-mode)
  (add-hook 'find-file-hooks 'dot-mode-on)
  (global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
                                   (message "Dot mode activated."))))

;;----------------------------------------------------------------------
;; LONGLINES-MODE
;;----------------------------------------------------------------------
;; Set keyboard shortcut for turning on longlines-mode in text-mode.
(define-key text-mode-map "\C-cL" 'longlines-mode)

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

;;######################################################################
;; MISCELLANEOUS PREFERENCES
;;######################################################################

;; Get rid of that AWFUL splash screen
(setq inhibit-splash-screen t)
;; Make *scratch* buffer suitable for writing
(setq initial-scratch-message nil)

;; Stop cursor from blinking
(blink-cursor-mode 0)

;; Turn off the menu and tool bars and the tooltip mode
;; (menu-bar-mode 0)
;; (tool-bar-mode 0)
;; (tooltip-mode 0)

;; Turn on image viewing
(auto-image-file-mode t)

;; Show me what I type, immediately
(setq echo-keystrokes 0.01)

;; Put backups elsewhere:
(setq backup-directory-alist '(("." . "~/.emacs-backup"))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5 ; Old versions to keep
) 

;; Prevent Emacs from bugging me about C-x n n not being
;; user-friendly.
(put 'narrow-to-region 'disabled nil)

;; For lazy typists
(fset 'yes-or-no-p 'y-or-n-p)
;; Move the mouse away if the cursor gets close
(mouse-avoidance-mode 'animate)

;; highlight the current line, as in Matlab
;; (global-hl-line-mode)

;; when you mark a region, you can delete it or replace it as in other
;; Windows programs. simply hit delete or type whatever you want or
;; yank
(delete-selection-mode)

; let there be a marker on every empty line on the left fringe
;;(setq default-indicate-empty-lines nil)

; show the matching parentheses immediately
(setq show-paren-delay 0)

;; FULLSCREEN
;; (defun fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen
;;                        (if (frame-parameter nil 'fullscreen) 
;;                            nil 'fullboth)))
(global-set-key [f11] 'toggle-frame-fullscreen)

;; WINDOW SPLITTING
;; Set horizontal splits as the default
(setq split-width-threshold 120)
(setq split-height-threshold 60)

;; Byte-compile elisp files immediately after saving them if .elc exists:
(defun auto-byte-recompile ()
"If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
file corresponding to the current buffer file, then recompile the file."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)

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
(add-hook 'after-make-frame-functions
          (lambda (frame) (load-theme 'gruvbox-dark-soft t)))
;; Zenburn color theme
;; (if (equal system-type 'gnu/linux) 
;;     (progn (require 'zenburn)
;;             (color-theme-zenburn)))

;;######################################################################
;; MODELINE:
;;######################################################################

(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-minor-mode . " Υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "Eλ")
    (nxhtml-mode . "nx")
    (dot-mode . " .")
    (scheme-mode . " SCM"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


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

(display-time-mode 0)

;;######################################################################
;; MINIBUFFER
;;######################################################################

;; Enable recursive minibuffer edits
(setq enable-recursive-minibuffers 1)

;;######################################################################
;; EVIL-MODE
;;######################################################################
(use-package evil
  :init
  (evil-mode)
  )
(use-package evil-surround
  :commands turn-on-evil-surround-mode
  :init
  (turn-on-evil-surround-mode)
  )
(use-package evil-commentary
  :commands evil-commentary-mode
  :init
  (evil-commentary-mode 1)
  )

;; (require 'evil)
;; (evil-mode 1)
;; (turn-on-evil-surround-mode)
;; (evil-commentary-mode 1)



