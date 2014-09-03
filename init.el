                        ; my dot emacs grows ;
                     ; one day i look inside it ;
                           ; singularity ;

; Karthik's .emacs file 
; Karthik Chikmagalur
; 02 June 2009 

(setq user-full-name "Karthik C")
; (setq user-mail-address "karthik[AT]gmail.com")

; Each section in this file is introduced by a
; line beginning with four semicolons; and each
; entry is introduced by a line beginning with
; three semicolons.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(display-time-mode t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(inferior-lisp-program "clisp")
 '(jabber-default-status "Available
")
 '(markdown-command "markdown_py -x footnotes /dev/stdin")
 '(markdown-enable-math nil)
 '(menu-bar-mode nil)
 '(safe-local-variable-values (quote ((simplenote-key . "agtzaW1wbGUtbm90ZXINCxIETm90ZRjBxo4GDA") (simplenote-key . "agtzaW1wbGUtbm90ZXINCxIETm90ZRjw_qcFDA") (major-mode . makefile-mode) (folded-file . t))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tex-dvi-view-command (quote (cond ((eq window-system (quote x)) "evince") ((eq window-system (quote w32)) "yap") (t "dvi2tty * | cat -s"))))
 '(tool-bar-mode nil)
 '(transient-mark-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "Droid Sans Mono")))))

;;##################
;; MY CUSTOMIZATIONS
;;##################

;; Set directory
(setq default-directory "~/")
;; Adds ~/.emacs.d to the load-path
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;######################################################################
;; PACKAGE MANAGEMENT
;;######################################################################
(require 'package nil t)
(when (featurep 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

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

;; Set default www browser
(if (equal system-type "gnu/linux")
    (setq 
     browse-url-browser-function 'browse-url-generic
     browse-url-generic-program "xdg-open"))

(if (or (equal system-name "ansatz") (equal system-name "langevin-235w"))
    (setq 
     browse-url-browser-function 'browse-url-generic
     browse-url-generic-program "google-chrome"))


;; Extends path to include my ~/bin directory
;; Not required if starting Emacs from shell.
;;(setenv "PATH" (concat
;;                (getenv "PATH")
;;                path-separator
;;                (getenv "HOME")
;;                "/"
;;                "bin"))

;; Consult clipboard before primary selection
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Clipboard.html
(setq x-select-enable-clipboard t)

;; Arrange for Emacs to notice password prompts and turn off echoing for them, as follows:
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Get rid of the annoying system beep
;; (setq visible-bell t)
(setq ring-bell-function (lambda ()
                           (call-process-shell-command 
                            "xset led 3; xset -led 3" nil 0 nil)))

;; For Emacs 23 only. use the system's trash can when deleting files and foldrs
;; (setq delete-by-moving-to-trash t)

;; Keys to use my describe.rb ruby script to query google
(global-set-key (kbd "C-?") 'describe-word-at-point)
(global-set-key (kbd "C-x ?") 'describe-word)

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


;;######################################################################
;; PLUGINS AND MODES
;;######################################################################

;;---------------------------------------------------------------------
;; PAREDIT-MODE
;;---------------------------------------------------------------------
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(eval-after-load "paredit"
  ;; Move the keys for split-sexp and raise-sexp to C-c r/s.
  '(progn (define-key paredit-mode-map (kbd "M-s") nil)
         (define-key paredit-mode-map (kbd "M-r") nil)
         (define-key paredit-mode-map (kbd "C-c s") 'paredit-splice-sexp)
         (define-key paredit-mode-map (kbd "C-c r") 'paredit-raise-sexp)))

;;----------------------------------------------------------------------
;; MULTIPLE-CURSORS 
;;----------------------------------------------------------------------
(require 'multiple-cursors nil t)
(when (featurep 'multiple-cursors)
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)  
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-n") 'mc/mark-next-word-like-this) 
  (global-set-key (kbd "M-@") 'mc/mark-all-words-like-this)
  (global-set-key (kbd "C-@") 'mc/mark-all-like-this)
  (global-set-key (kbd "M-p") 'mc/mark-previous-word-like-this) 
  (global-set-key (kbd "<C-return>") 'set-rectangular-region-anchor) 
  (global-set-key (kbd "C-x C-a") 'mc/edit-beginnings-of-lines)     
  (global-set-key (kbd "C-x SPC") 'mc/mark-all-dwim)                
  (global-set-key (kbd "M-N") 'mc/insert-numbers)
  (global-set-key (kbd "M-S") 'mc/sort-regions))

;;----------------------------------------------------------------------
;; EXPAND-REGION
;;----------------------------------------------------------------------
(require 'expand-region nil t)
(when (featurep 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-,") 'er/expand-region))

;;----------------------------------------------------------------------
;; ACE-JUMP-MODE
;;----------------------------------------------------------------------
;; Ace jump mode major function
;; (autoload
;;     'ace-jump-mode
;;     "ace-jump-mode"
;;     "Emacs quick move minor mode" t)
(require 'ace-jump-mode nil t)
(when (featurep 'ace-jump-mode)
  (define-key global-map (kbd "M-m") 'ace-jump-mode)
  (define-key global-map (kbd "C-'") 'ace-jump-mode))

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
;; SIMPLENOTE-SETUP
;;----------------------------------------------------------------------
;; (require 'simplenote)
;; (setq simplenote-email "simplenoteemail@provider.com")
;; (setq simplenote-password "simplenotepassword")
;; (simplenote-setup)

;;----------------------------------------------------------------------
;; LUA-MODE
;;----------------------------------------------------------------------
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;----------------------------------------------------------------------
;; SCHEME-MODE
;;----------------------------------------------------------------------
(defun mechanics ()
  (interactive)
  (run-scheme 
    "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib"
  ))

;;----------------------------------------------------------------------
;; IDO-MODE.
;;----------------------------------------------------------------------
(require 'ido nil t)
(when (featurep 'ido) 
  (ido-mode t)
  (setq ido-enable-flex-matching t) ;; enable fuzzy matching
  (ido-everywhere 1)

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

;; Supercharge M-x. Use with care!
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
;; RUBY-MODE
;;----------------------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/plugins/ruby/")
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))
;; Ri in emacs
;; (autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
;; (require 'inf-ruby nil t)
;; (when (featurep 'inf-ruby) 
;;   (setq ri-ruby-script (expand-file-name "~/.emacs.d/plugins/ri-emacs.rb"))
;;   (autoload 'ri (expand-file-name "~/.emacs.d/plugins/ri-ruby.el") nil t)
;;   (add-hook 'ruby-mode-hook (lambda ()                                          
;;                               (local-set-key (kbd "<f1>") 'ri)                           
;;                               (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;;                               (local-set-key (kbd "<f5>") 'ri-ruby-show-args)            
;;                               )))


;;----------------------------------------------------------------------
;; TRAMP
;;----------------------------------------------------------------------
;; Tramp ssh'es into root@host to edit files. The emacs sudo, kindof.
(autoload 'tramp "tramp")


;;----------------------------------------------------------------------
;; C-MODE
;;----------------------------------------------------------------------
;; C mode preferences
;;(setq compilation-window-height 8)
;;(c-toggle-hungry-state 1)
;; (add-hook 'c-mode-hook
;;           (lambda nil
;;             (progn
;;               (c-toggle-hungry-state 1)
;;               (c-subword-mode))))


;;----------------------------------------------------------------------
;; DIRED
;;----------------------------------------------------------------------
;; Dired preferences
(add-hook 'dired-load-hook
          (function (lambda ()
                      (load "dired-x")
                      ;; Set dired-x global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      ;; (setq dired-x-hands-off-my-keys nil)
                      )))
(add-hook 'dired-mode-hook
          (function (lambda ()
                      ;; Set dired-x buffer-local variables here.  For example:
                      (require 'dired-x)
                      (setq dired-omit-mode 1)
                      )))


;;----------------------------------------------------------------------
;; YASNIPPET MODE
;;----------------------------------------------------------------------
;; Yasnippet bundle for Emacs
;; (require 'yasnippet-bundle)
(require 'yasnippet) ;; not yasnippet-bundle
(when (featurep 'yasnippet)
  (setq yas-snippet-dirs "~/.emacs.d/snippets")
  ;; (yas/load-directory "~/.emacs.d/snippets")
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas/dropdown-prompt yas/x-prompt))
  (setq yas-wrap-around-region t))


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
;; W3M
;;----------------------------------------------------------------------
;; w3m-el browser
;; (autoload 'w3m-el "w3m-el" "w3m in Emacs" t)
;; (setq w3m-use-cookies t)


;;----------------------------------------------------------------------
;; ARTIST-MODE
;;----------------------------------------------------------------------
;; Artist mode for drawing ASCII art!
;; (autoload 'artist-mode "artist" "Enter artist-mode" t)


;;----------------------------------------------------------------------
;; FOOTNOTE-MODE
;;----------------------------------------------------------------------
;;; Footnote mode
                                        ; Adds a function to read in existing footnotes upon starting a new
                                        ; session. 'tis a bit flaky.
;; (require 'footnote-init nil t)
;; (when (featurep 'footnote-init) (add-hook
;;                                  'footnote-mode-hook 'footnote-init))


;;----------------------------------------------------------------------
;; ORG-MODE
;;----------------------------------------------------------------------
;;;; Org mode
;; org-init.el is loaded from the "lisp" directory.
(require 'org-init nil t)
;; Keybindings for org-mode and org-remember
(when (featurep 'org-init)
  (global-set-key "\C-cr" 'remember)
  (global-set-key "\C-\M-r" 'org-remember))


;;----------------------------------------------------------------------
;; MARKDOWN-MODE
;;----------------------------------------------------------------------
;; Load from the "lisp" directory, also associate .text with markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.text" . markdown-mode) auto-mode-alist))
(eval-after-load "markdown-mode.el"
  '(progn 
    (defun markdown-unset-tab ()
      "markdown-mode-hook"
      (define-key markdown-mode-map (kbd "<tab>") nil))
    (add-hook 'markdown-mode-hook
              '(lambda() (markdown-unset-tab) (visual-line-mode)))))


;;----------------------------------------------------------------------
;; OCTAVE-MODE
;;----------------------------------------------------------------------
;; Load .m files as octave mode
(setq auto-mode-alist (cons '("\\.m" . octave-mode) auto-mode-alist))


;;----------------------------------------------------------------------
;; WEBLOGGER-MODE
;;----------------------------------------------------------------------
;; Publish to Wordpress from Emacs
;; (autoload 'weblogger "weblogger.el" "Publish to Wordpress from Emacs" t)


;;----------------------------------------------------------------------
;; MUSE-MODE
;;----------------------------------------------------------------------
                                        ; Set up muse mode for easy publishing.
                                        ;(require 'muse-mode)
;; (require 'muse-html)     ; load publishing styles I use
;; (require 'muse-latex)
;; (require 'muse-texinfo)
;; (require 'muse-docbook)
;; (require 'muse-xml)

;; ; Muse styles
;; (muse-derive-style "w3-xhtml" "xhtml"
;; :style-sheet "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"http://www.w3.org/StyleSheets/Core/Steely\" />") 

                                        ;(defvar muse-styles-w3 '("Oldstyle" 
                                        ;                           "Modernist" 
                                        ;                           "Midnight" 
                                        ;                           "Ultramarine" 
                                        ;                           "Swiss"
                                        ;                           "Chocolate"
                                        ;                           "Traditional"
                                        ;                           "Steely")
                                        ;                       "CSS styles for muse style w3-xhtml" )


;;----------------------------------------------------------------------
;; BABEL
;;----------------------------------------------------------------------
;;; Babel: Translate text between languages
(autoload 'babel "babel"
  "Use a web translation service to translate the message MSG." t)
(autoload 'babel-region "babel"
  "Use a web translation service to translate the current region." t)
(autoload 'babel-as-string "babel"
  "Use a web translation service to translate MSG, returning a string." t)
(autoload 'babel-buffer "babel"
  "Use a web translation service to translate the current buffer." t)


;;----------------------------------------------------------------------
;; PYTHON-MODE
;;----------------------------------------------------------------------
;;; Ipython mode
;;   (setq ipython-command "/usr/bin/ipython")
;;   (require 'ipython)


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
;; ERC
;;----------------------------------------------------------------------
;; ERC preferences
(erc-track-mode t)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))



;;----------------------------------------------------------------------
;; JABBER
;;----------------------------------------------------------------------
;; ;; Emacs Jabber, set up for Google Talk.
;; (setq jabber-account-list '(
;;                             ("myaddr@gmail.com"
;;                              ;; (:password . nil) or (:password . "your-pass")
;;                              (:network-server . "talk.google.com")
;;                              ;; (:port . 5223)
;;                              (:connection-type . ssl))
;;                             ))

;; ;; Misc jabber settings:
;; ;; Auto-highlight links in jabber buffers
;; (add-hook 'jabber-chat-mode-hook 'goto-address)
;; (add-hook 'jabber-chat-mode-hook 'flyspell-mode)
;; (setq jabber-show-offline-contacts nil)
;; (setq jabber-roster-show-bindings nil)
;; (setq jabber-vcard-avatars-retrieve nil)
;;                                         ;(setq jabber-default-status (shell-command-to-string "fortune cookie linuxcookie -n short"))

;;----------------------------------------------------------------------
;; HTMLIZE
;;----------------------------------------------------------------------
;; Htmlize Emacs buffers
;;(autoload 'htmlize "htmlize" "HTML-ize Emacs regions/buffers" nil t)


;;----------------------------------------------------------------------
;; SWEET-KILL
;;----------------------------------------------------------------------
;; Make *scratch* buffer suitable for writing
(setq initial-scratch-message nil)
;; (progn 
;;   (set-buffer "*scratch*")
;;   (erase-buffer))
;;   (save-excursion
;;   (let ((h (- (/ (window-height) 2) 1)) (l (length haiku-emacs)))
;;     (beginning-of-buffer)
;;     (open-line h)
;;     (goto-line h)
;;     (mapc (lambda (x)
;;             (insert x)
;;             (center-line)
;;             (newline))
;;           (nth (random l) haiku-emacs))))
;;   (message "C-x k"))

;;    (sleep-for 2)
;;    (kill-buffer)))
;; Sweet kill Emacs (Prints a haiku at exit)
;;(require 'sweet-kill)
;; Haiku upon Emacs invocation! (Depends on sweet-kill)


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

;;----------------------------------------------------------------------
;; AUCTEX-MODE & ADDITIONS
;;---------------------------------------------------------------------- 
;; (setq 
;;  TeX-auto-save t
;;  TeX-parse-self t
;;  TeX-electric-escape nil)
;; (setq-default TeX-master nil)

;; (autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
;; (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)
;; (add-hook 'LaTeX-mode-hook 
;;           (lambda nil
;;             (progn 
;;               (turn-on-auto-fill)
;;               (setq TeX-newline-function 'reindent-then-newline-and-indent)
;; 	      (define-key TeX-mode-map (kbd "C-;") 'TeX-complete-symbol)
;;               (TeX-fold-mode 1)
;;               (setq cdlatex-command-alist
;;                     '(("vc" "Insert \\vect{}" "\\vect{?}" cdlatex-position-cursor nil nil t)))
;;               (turn-on-cdlatex)
;;               (setq cdlatex-paired-parens "$[{("))))


;;---------------------------------------------------------------------- 
;; MATH-MODE
;;---------------------------------------------------------------------- 
;; Major mode for running Mathematica in Emacs
;; (eval-after-load 'math 
;;  '(define-key math-mode-map (kbd "<tab>") 'math-complete-symbol))

;;######################################################################
;; MISCELLANEOUS PREFERENCES
;;######################################################################

;; Get rid of that AWFUL splash screen
(setq inhibit-splash-screen t)

;; Stop cursor from blinking
(blink-cursor-mode 0)

;; Turn on menu bar (this bar has text)
;; (Use numeric argument to turn on)
(menu-bar-mode 0)
;; Turn off tool bar (this bar has icons)
;; (Use numeric argument to turn on)
(tool-bar-mode 0)
;; Turn off tooltip mode for tool bar
;; (This mode causes icon explanations to pop up)
;; (Use numeric argument to turn on)
(tooltip-mode 0)

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

;;; Prevent Emacs from bugging me about C-x n n not being user-friendly.
(put 'narrow-to-region 'disabled nil)

;; For lazy typists
(fset 'yes-or-no-p 'y-or-n-p)
;; Move the mouse away if the cursor gets close
(mouse-avoidance-mode 'animate)

;; highlight the current line, as in Matlab
;;(global-hl-line-mode)

; when you mark a region, you can delete it or replace it as in other Windows programs:
;; simply hit delete or type whatever you want or yank
;;(delete-selection-mode)

; let there be a marker on every empty line on the left fringe
;;(setq default-indicate-empty-lines nil)

; show the matching parentheses immediately
(setq show-paren-delay 0)

;; FULLSCREEN
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)


;; WINDOW SPLITTING
;; Set horizontal splits as the default
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 80)

;; Byte-compile init.el immediately after saving it:
(add-hook 'after-save-hook
          '(lambda nil (cond 
                        ((string= 
                          (buffer-file-name)
                          (expand-file-name 
                           (concat default-directory
                                   "init.el")))
                         (byte-compile-file (buffer-file-name)))
                        ((or (string=
                          (buffer-file-name)
                          (expand-file-name 
                           (concat default-directory
                                   "journal.org")))
                         (string=
                          (buffer-file-name)
                          (expand-file-name 
                           (concat default-directory
                                   "projects.org"))))
                         (when (featurep 'simplenote) 
                           (simplenote-push-buffer))))))

;; Sync with Simplenote immediately after opening certain files
(add-hook 'find-file-hook '(lambda nil 
                             (if (and (featurep 'simplenote) 
                                      (or (string=
                                           (buffer-file-name)
                                           (expand-file-name 
                                            (concat default-directory
                                                    "journal.org")))
                                          (string=
                                           (buffer-file-name)
                                           (expand-file-name 
                                            (concat default-directory
                                                    "tasks.org")))
                                          (string=
                                           (buffer-file-name)
                                           (expand-file-name 
                                            (concat default-directory
                                                    "projects.org")))))
                                 (simplenote-pull-buffer))))

                           
                                  
;;----------------------------------------------------------------------
;; MACROS
;;----------------------------------------------------------------------

;; Bind call last macro to F4
(global-set-key (kbd "<f3>") 'kmacro-start-macro)
(global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro)

;;----------------------------------------------------------------------
;; SLIME
;;----------------------------------------------------------------------
;;(setq inferior-lisp-program "clisp")
;;(require 'slime-autoloads)
;;(slime-setup)
          
;;######################################################################
;; COLORS & COLOR THEMES
;;######################################################################

;; Zenburn color theme
(require 'zenburn)
(color-theme-zenburn)


;;######################################################################
;; MODELINE:
;;######################################################################

;; Set a Mode Line that tells me which machine, which directory,
;; and which line I am on, plus the other customary information.
(setq mode-line-format
      (quote
       (#("-" 0 1
          (help-echo
           "mouse-1: select window, mouse-2: delete others ..."))
        mode-line-mule-info
        mode-line-modified
        mode-line-frame-identification
        "  "
        mode-line-buffer-identification
        "  "
        ;;(:eval (substring
        ;;        (system-name) 0 (string-match "\\..+" (system-name))))
        ;;":"
        default-directory
        #(" " 0 1
          (help-echo
           "mouse-1: select window, mouse-2: delete others ..."))
        (line-number-mode " Line %l ")
        global-mode-string
        #("   %[(" 0 6
          (help-echo
           "mouse-1: select window, mouse-2: delete others ..."))
        ;;(:eval (mode-line-mode-name))
        mode-line-process
        minor-mode-alist
        #("%n" 0 2 (help-echo "mouse-2: widen" local-map (keymap ...)))
        ")%] "
        (-3 . "%P")
        ;;   "-%-"
        )))

;;######################################################################
;; MINIBUFFER
;;######################################################################

;; Enable recursive minibuffer edits
(setq enable-recursive-minibuffers 1)

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)
