                        ; my dot emacs grows ;
                     ; one day i look inside it ;
                           ; singularity ;

; Karthik's .emacs file
(eval-when-compile (require 'cl))
(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

(setq user-full-name "Karthik C")
; (setq user-mail-address "karthik[AT]gmail.com")

;;########################################################################
;; UI FIXES
;;########################################################################

;; Get rid of the splash screen
(setq inhibit-splash-screen t)
;; Make *scratch* buffer suitable for writing
(setq initial-scratch-message nil)

;; Stop cursor from blinking
(blink-cursor-mode 0)

;; Turn off the menu, tool bars, tooltips and the scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
;; Turn on image viewing
(auto-image-file-mode t)

;; Show me what I type, immediately
(setq echo-keystrokes 0.01)

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

;; when you mark a region, you can delete it or replace it as in other
;; Windows programs. simply hit delete or type whatever you want or
;; yank
(delete-selection-mode)

; show the matching parentheses immediately
(setq show-paren-delay 0)

;; FULLSCREEN
(global-set-key [f11] 'toggle-frame-fullscreen)

;; WINDOW SPLITTING
;; Set horizontal splits as the default
(setq split-width-threshold 120)
(setq split-height-threshold 80)

;; Byte-compile elisp files immediately after saving them if .elc exists:
(defun auto-byte-recompile ()
"If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
file corresponding to the current buffer file, then recompile the file."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)

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

;; ;; Set default www browser
;; (if (equal system-type 'gnu/linux)
;;     (setq
;;      ;browse-url-browser-function 'browse-url-generic
;;      ;browse-url-generic-program "/usr/bin/palemoon"
;;      ))

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
(winner-mode)

;;######################################################################
;; UTILITY
;;######################################################################
;; Count words, print ASCII table, etc
(require 'utilities nil t)
;; Colorize color names in buffers
(use-package rainbow-mode
  :ensure t
  :config (rainbow-mode))

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
;; LANGUAGE MODES
;;######################################################################

;;----------------------------------------------------------------------
;; AUCTEX-MODE & ADDITIONS
;;----------------------------------------------------------------------
(use-package auctex
  :ensure t
  :bind (:map TeX-mode-map
              ("M-SPC" . TeX-matrix-spacer)
              ("C-M-9" . TeX-insert-smallmatrix)
              ("C-M-]" . TeX-insert-bmatrix)
              ("C-;" . TeX-complete-symbol))
  :init
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
         (TeX-fold-mode 1)))

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

;;######################################################################
;; PLUGINS
;;######################################################################
;;----------------------------------------------------------------------
;; VERSION CONTROL
;;----------------------------------------------------------------------
(use-package magit
  :ensure t)

;;----------------------------------------------------------------------
;; WHICH-KEY
;;----------------------------------------------------------------------
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

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
; COMPANY-MODE
;;----------------------------------------------------------------------
(use-package company
  :ensure t
  :diminish ""
  :config
  (global-company-mode)
  ;; (setq company-tooltip-limit 10)
  (setq company-dabbrev-downcase 0)
  ;; (setq company-idle-delay 0)
  ;; (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  ;; (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  ;; (setq company-tooltip-flip-when-above t)
  (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  )

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
(use-package paredit
  :ensure t
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

  (use-package evil-paredit
    :ensure t
    :init (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

;;----------------------------------------------------------------------
;; EXPAND-REGION
;;----------------------------------------------------------------------
(use-package expand-region
  :ensure
  :commands expand-region
  :bind ("C-," . 'er/expand-region))

;;----------------------------------------------------------------------
;; ACE-JUMP-MODE
;;----------------------------------------------------------------------
(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :bind ("M-j" . 'ace-jump-mode))

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
;; IVY/COUNSEL/SWIPER
;;----------------------------------------------------------------------
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)

  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  ;; (defun my-ivy-switch-file-search ()
  ;;   "Switch to counsel-file-jump, preserving current input."
  ;;   (interactive)
  ;;   (let ((input (ivy--input)))
  ;;     (ivy-quit-and-run (counsel-file-jump))))

  ;; (define-key counsel-find-file-map (kbd "C-s") 'my-ivy-switch-file-search)
  :diminish ivy-mode
)

;; ivy-rich shows descriptions along with selection candidates in ivy
(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;;; ivy-hydra allows additional actions and vim-like navigation on ivy candidates.
;;; Initialize in ivy with C-o
;; (use-package ivy-hydra)

;;; Bibtex management from ivy. Call ivy-bibtex.
(use-package ivy-bibtex
  :ensure t
  :commands ivy-bibtex
  :config
  (setq ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
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
(use-package org-evil
  :ensure t)

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
;; MACROS
;;----------------------------------------------------------------------

;; Bind call last macro to F4
(global-set-key (kbd "<f3>") 'kmacro-start-macro)
(global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro)

;;######################################################################
;; COLORS & COLOR THEMES
;;######################################################################

;; ;;; Load theme after the frame is created.
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (load-theme 'gruvbox-dark-medium t)
;;             (load-theme 'smart-mode-line-dark t)))

;;######################################################################
;; MODELINE:
;;######################################################################

(use-package smart-mode-line
  :ensure t
  :init (sml/setup))

; (defvar mode-line-cleaner-alist
;   `((auto-complete-mode . " α")
;     (yas-minor-mode . " Υ")
;     (paredit-mode . " π")
;     (eldoc-mode . "")
;     (abbrev-mode . "")
;     ;; Major modes
;     (lisp-interaction-mode . "λ")
;     (hi-lock-mode . "")
;     (python-mode . "Py")
;     (emacs-lisp-mode . "Eλ")
;     (nxhtml-mode . "nx")
;     (dot-mode . " .")
;     (scheme-mode . " SCM"))
;   "Alist for `clean-mode-line'.

; ;; When you add a new element to the alist, keep in mind that you
; ;; must pass the correct minor/major mode symbol and a string you
; ;; want to use in the modeline *in lieu of* the original.")


; (defun clean-mode-line ()
;   (interactive)
;   (loop for cleaner in mode-line-cleaner-alist
;         do (let* ((mode (car cleaner))
;                  (mode-str (cdr cleaner))
;                  (old-mode-str (cdr (assq mode minor-mode-alist))))
;              (when old-mode-str
;                  (setcar old-mode-str mode-str))
;                ;; major mode
;              (when (eq mode major-mode)
;                (setq mode-name mode-str)))))


; (add-hook 'after-change-major-mode-hook 'clean-mode-line)

; (display-time-mode 0)

;;######################################################################
;; MINIBUFFER
;;######################################################################

;; Enable recursive minibuffer edits
(setq enable-recursive-minibuffers 1)

;;######################################################################
;; EVIL-MODE
;;######################################################################
(use-package evil-leader
  :ensure
  :commands global-evil-leader-mode
  :init
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "B" 'byte-compile-file)
  (evil-leader/set-key-for-mode 'latex-mode "cc" 'TeX-command-master)
  (evil-leader/set-key
    ;; "e" 'find-file
    "fF" 'counsel-fzf
    "ff" 'find-file
    "f." 'find-file
    "fr" 'counsel-recentf
    "fj" 'counsel-file-jump
    "fg" 'counsel-git
    "fa" 'counsel-ag
    ;; "F" 'counsel-fzf-dir-function
    "b" 'switch-to-buffer
    "w" 'save-buffer
    "q" 'evil-quit
    "j" 'ace-jump-mode

    "k" (lambda () "Kill buffer" (interactive) (kill-buffer (current-buffer)))
    "n" (lambda (&optional arg)
          "Next Buffer"
                  (interactive "P")
                  (if arg (next-user-buffer) (previous-user-buffer)))
    "p" (lambda (&optional arg)
          "Previous Buffer"
                  (interactive "P")
                  (if arg (previous-user-buffer) (next-user-buffer)))
    "B" (lambda ()
          "Open Bibtex file"
          (interactive)
          (find-file-other-window (getenv "BIB")))
    "," 'er/expand-region
    "cn" 'next-error
    "cp" 'previous-error
    "vf" 'ido-find-file-other-window
    "vb" 'ido-switch-buffer-other-window))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-emacs-state-cursor '(hbar . 4))
  (setq evil-vsplit-window-right t)
  (add-hook 'evil-jumps-post-jump-hook #'recenter)
  (evil-mode 1)
  :bind (:map evil-motion-state-map
              ("C-w C-h" . evil-window-left)
              ("C-w C-l" . evil-window-right)
              ("C-w C-k" . evil-window-up)
              ("C-w C-j" . evil-window-down)
              ("C-w C-f" . winner-redo)
              ("C-w C-b" . winner-undo)
              ("C-w C-w" . winner-undo)
              ("C-w |" . toggle-window-split)
              :map evil-normal-state-map
              ("[o" . open-previous-line)
              ("]o" . open-next-line))
  :config
  ;; (define-key evil-motion-state-map ";" 'evil-repeat-find-char)
  ;; (define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
  (defvar dotemacs--original-mode-line-bg (face-background 'mode-line))
  (defadvice evil-set-cursor-color (after dotemacs activate)
    (cond ((evil-emacs-state-p)
           (set-face-background 'mode-line "#440000"))
          ;; ((evil-insert-state-p)
          ;;  (set-face-background 'mode-line "#002244"))
          ;; ((evil-visual-state-p)
          ;;  (set-face-background 'mode-line "#440044"))
          (t
           (set-face-background 'mode-line dotemacs--original-mode-line-bg))))
  ;; Setup text-objects for use in LaTeX. Putting this here because it doesn't make sense to put this in the AucTeX section without enabling evil-mode first.
  (require 'evil-latex-textobjects nil t)
  (add-hook 'LaTeX-mode-hook 'turn-on-evil-latex-textobjects-mode)

  ;; c/d/y s {motion}{delimiter} to change/delete/add delimiter around motion.
  (use-package evil-surround
    :ensure
    :commands turn-on-evil-surround-mode
    :init
    (global-evil-surround-mode 1))

  ;; gc{motion} to comment/uncomment
  (use-package evil-commentary
    :ensure
    :commands evil-commentary-mode
    :init
    (evil-commentary-mode 1)
    :diminish)

  ;; gx{motion} to select, gx{motion} on second object to exchange
  (use-package evil-exchange
    :ensure t
    :config
    (evil-exchange-install))

  ;; gl{motion}{char} to align on char
  (use-package evil-lion
    :ensure t
    :config
    (evil-lion-mode))

  ;; % to match delimiters, % as text-object to manipulate
  (use-package evil-matchit
    :ensure t
    :init (global-evil-matchit-mode 1))

  ;; + and - to increment/decrement number at point
  (use-package evil-numbers
    :ensure t
    :bind (:map evil-normal-state-map
                ("+" . evil-numbers/inc-at-pt)
                ("-" . evil-numbers/dec-at-pt)))

  ;; C-a, C-e, C-f, C-b, C-d and C-k have same definitions as in emacs mode.
  ;; C-n and C-p work like in emacs if auto-complete is loaded.
  (use-package evil-rsi
    :ensure t
    :config
    (evil-rsi-mode)
    :diminish evil-rsi-mode)

  ;; s to snipe for next occurrence of chars
  ;; in operator mode, z or x to operate including/excluding next ocurrence of chars
  (use-package evil-snipe
    :ensure t
    :config
    ;; (evil-snipe-override-mode nil)
    (evil-snipe-mode 1)
    (setq evil-snipe-spillover-scope 'whole-visible)
    (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
    (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
    :diminish evil-snipe-mode)

  ;; Hit ; or , (originally <SPC>) to repeat last movement.
  ;; (use-package evil-space
  ;;   :ensure t
  ;;   :init
  ;;   (evil-space-mode)
  ;;   (setq evil-space-next-key ";")
  ;;   (setq evil-space-prev-key ","))

  ;; Select with visual-mode and hit * or # to find next occurrence
  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode)
    (setq evil-visualstar/persistent t))
  )

