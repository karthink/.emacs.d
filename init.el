;; -*- lexical-binding: t -*-
                                        ; my dot emacs grows ;
                                        ; one day i look inside it ;
                                        ; singularity ;

(setq gc-cons-threshold most-positive-fixnum)

;;;* PATHS
;;######################################################################

;; (setq user-emacs-directory "~/.emacs.d/")

(defun dir-concat (dir file)
  "join path DIR with filename FILE correctly"
  (concat (file-name-as-directory dir) file))

;; Get custom-set-variables out of init.el
(setq custom-file (dir-concat user-emacs-directory "custom.el"))
(load custom-file)

;; Set directory
(setq default-directory
      (cond ((equal (system-name) "surface")
             "/cygdrive/c/Users/karth/OneDrive/Documents/")
            ((equal (system-name) "cube")
             "/cygdrive/c/Users/karth/OneDrive/Documents/")
            ((equal (system-name) "thinkpad") "~/")
            (t "~/")))

;; Adds ~/.emacs.d to the load-path
(push (dir-concat user-emacs-directory "plugins/") load-path)
(push (dir-concat user-emacs-directory "lisp/") load-path)

;; (eval-after-load "setup-org"
;;   (setq initial-buffer-choice (concat (file-name-as-directory (getenv "HOME"))
;;                                       "do.org")))

;;########################################################################
;;;* CORE
;;########################################################################
(require 'setup-core)

;;########################################################################
;;;* PERSONAL INFO
;;########################################################################
(and (require 'personal nil t)
     (setq user-full-name my-full-name)
     (setq user-mail-address my-email-address)
     (defun encrypt-personal-data ()
       "Automatically encrypt personal.el to personal.el.gpg when it is saved"
       (interactive)
       (if (equal buffer-file-name (concat (expand-file-name user-emacs-directory) "lisp/personal.el"))
           (epa-encrypt-file buffer-file-name my-email-address))))

;;########################################################################
;;;* UI FIXES
;;########################################################################
(require 'setup-ui)

;;########################################################################
;;;* AUTOLOADS
;;######################################################################
(require 'setup-autoloads nil t)
(require 'plugin-autoloads nil t)

;;######################################################################
;;;* PACKAGE MANAGEMENT
;;######################################################################
  ;;; Set load paths for ELPA packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  ;; (defvar use-package-verbose t)
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'cl-lib)
  (require 'use-package)
  ;; (setq use-package-verbose t
  ;;       use-package-compute-statistics t
  ;;       ;use-package-ignore-unknown-keywords t
  ;;       use-package-minimum-reported-time 0.01)
  )

(require 'bind-key)

(add-hook 'package-menu-mode-hook 'hl-line-mode)

;;######################################################################
;;;* KEYBIND SETUP
;;######################################################################
(use-package general
  ;; :preface (setq use-package-ignore-unknown-keywords t)
  :ensure t
  :demand t
  :commands (general-def general-define-key)
  :init
  (defvar general-leader "SPC"
    "Leader key for Evil")
  (defvar general-leader-alt "M-SPC"
    "Leader key for Emacs and Evil Insert states")
  (defvar general-localleader ","
    "Local leader key for major-mode specific commands")
  (defvar general-localleader-alt "M-SPC ,"
    "Local leader key for major-mode specific commands for Emacs and Evil Insert states.")

  ;; With evil-mode
    (general-define-key
     :states '(normal visual emacs)
     :prefix general-leader
     :non-normal-prefix general-leader-alt
     :prefix-command 'space-menu
     :prefix-map 'space-menu-map
     :wk "Leader key for emacs")

    (general-create-definer leader-define-key
      :states '(normal visual motion emacs)
      :prefix general-leader
      :non-normal-prefix general-leader-alt)
    (general-create-definer localleader-define-key
      :states '(normal visual motion emacs)
      :prefix general-localleader
      :non-normal-prefix general-localleader-alt)

    (general-def
      :states '(motion)
      :prefix general-leader-alt
      "" 'space-menu)
    ;; Pure emacs
     ;; (general-define-key
     ;;   ;; :states '(normal motion visual emacs)
     ;;   ;; :prefix general-leader
     ;;   ;; :non-normal-prefix general-leader-alt
     ;;   :prefix general-leader-alt
     ;;   :prefix-command 'space-menu
     ;;   :prefix-map 'space-menu-map
     ;;   :wk "Leader key for emacs")

     ;;  (general-create-definer leader-define-key
     ;;    ;; :states '(normal visual motion emacs)
     ;;    ;; :non-normal-prefix general-leader-alt
     ;;    :prefix general-leader-alt)

     ;;  (general-create-definer localleader-define-key
     ;;    ;; :states '(normal visual motion emacs)
     ;;    ;; :non-normal-prefix general-localleader-alt
     ;;    :prefix general-localleader-alt)

  (general-def
   :keymaps 'space-menu-map
   :wk-full-keys nil
   ;; unbind SPC and give it a title for which-key (see echo area)
   ;;"x" '(Control-X-prefix :wk "C-x")
   "SPC" 'scroll-other-window
   "M-SPC" '(scroll-other-window :wk nil)
   "M-S-SPC" '(scroll-other-window-down :wk nil)
   "S-SPC" 'scroll-other-window-down
   "z" '(repeat-complex-command :wk "M-x again")
   "x" '(execute-extended-command :wk "M-x")
   "f" '(:ignore t :wk "file")
   "q" '(:ignore t :wk "quit")
   "b" '(:ignore t :wk "buffer")
   "g" '(vc-prefix-map :wk "git/VC")
   "c" '(:ignore t :wk "code")
   "k" '(kill-this-buffer :wk "Kill buffer")
   "/" '(:prefix-command space-menu-search
         :prefix-map space-menu-search-map
         :wk "search")
   "b" '(:prefix-command space-menu-buffer
         :prefix-map space-menu-buffer-map
         :wk "buffers")
   "p" '(:prefix-command space-menu-project
         :prefix-map space-menu-project-map
         :wk "project")
   "h" '(help-command :wk "help")
   ;; "h" '(:prefix-command space-menu-help
   ;;       :prefix-map space-menu-help-map
   ;;       :wk "help")
   "w" '(:prefix-command space-menu-window
         :prefix-map space-menu-window-map
         :wk "window")
   )

  (general-def
    :keymaps 'space-menu-map
    :wk-full-keys nil
    "s" '(space-menu-search :wk "search"))

  (general-def
    :keymaps 'space-menu-buffer-map
    :wk-full-keys nil
    "r" '(revert-buffer         :wk "revert buffer")
    "b" '(switch-to-buffer      :wk "switch to buffer")
    "d" '(kill-buffer           :wk "delete buffers")
    "k" '(kill-this-buffer      :wk "kill buffer")
    "z" '(bury-buffer           :wk "bury buffer")
    "[" '(previous-buffer       :wk "prev buffer")
    "]" '(next-buffer           :wk "next buffer")
    "=" '(diff-buffer-with-file :wk "diff against file")
    "C-o" '(display-buffer      :wk "display buffer")
    )

  (general-def
    :keymaps 'space-menu-window-map
    :wk-full-keys nil
    "S" 'window-configuration-to-register
    "J" '(jump-to-register :wk "window config jump")
    "k" '(delete-window :wk "delete window")
    "K" '(kill-buffer-and-window :wk "kill buf and win"))

  (general-def
    :keymaps 'space-menu-search-map
    :wk-full-keys nil
    "o" '(occur :wk "occur")
    "." '(isearch-forward-symbol-at-point :wk "search thing-at-pt")
    "h" '(highlight-regexp :wk "highlight regexp")
    "_" '(isearch-forward-symbol :wk "search for symbol")
    "f" '(grep-find :wk "grep through find")
    "b" '(batch-replace-strings :wk "batch-replace")
    "i" '(imenu :wk "imenu"))

  (general-def
   :keymaps 'space-menu-project-map
   :wk-full-keys nil
   "f" '(project-find-file :wk "find file in proj")
   "q" '(project-query-replace-regexp :wk "query replace in proj")
   "g" '(project-search :wk "grep in proj")
   "o" '(project-find-regexp :wk "occur in proj"))

  (general-def
    :keymaps 'space-menu-map
    :wk-full-keys nil
    "," '(switch-to-buffer   :wk "switch buffer")
    ";" '(eval-expression    :wk "eval expr")
    "u" '(universal-argument :wk "universal arg")
    )

  (general-def
    :keymaps 'space-menu-map
    :wk-full-keys nil
    :prefix "q"
    "q" '(save-buffers-kill-terminal :wk "quit emacs (eject!)")
    "f" '(delete-frame               :wk "quit frame")
    "d" '(server-edit                :wk "done with buffer"))

  (general-def
    :keymaps 'space-menu-map
    :wk-full-keys nil
    :prefix "f"
    "s" '(save-buffer       :wk "Save file")
    "w" '(write-file        :wk "Save as?")
    "S" '(save-some-buffers :wk "Save bufferS")
    "U" '(sudo-find-file    :wk "Sudo find file")
    "^" '(sudo-this-file    :wk "Sudo THIS file")
    "f" '(find-file         :wk "Find file")
    "l" '(locate            :wk "Locate file on system")
    "'" '(bookmark-jump     :wk "Jump to bookmark")
    "m" '(bookmark-set      :wk "Set bookmark")
    "." '(find-file         :wk "Find file (FIXME)")
    )

  (localleader-define-key
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "g" '(nil            :wk "goto")
    "gl" '(find-library  :wk "Find Library")
    "gv" '(find-variable :wk "Find Variable")
    "gf" '(find-function :wk "Find Function")
    "x" '(eval-defun     :wk "Eval defun")
    "l" '(load-library   :wk "Load library")
    "b" '(eval-buffer    :wk "Eval Buffer")
    "r" '(eval-region    :wk "Eval Region")
    "B" `(,(defun byte-compile-this-file () "Byte-compile file"
                 (interactive)
                 (if buffer-file-name
                     (byte-compile-file
                      buffer-file-name)
                   (message "Not visiting a file!")))
                         :wk "Byte-compile file")
    "L" `(,(defun load-this-file () "Load current file"
                 (interactive)
                  (if buffer-file-name
                      (load-file
                       buffer-file-name)
                    (message "Not visiting a file!")))
                         :wk "Load this file"))
  (general-def :keymaps 'space-menu-help-map
    "m" '(describe-mode :wk "describe mode"))
  )
;;######################################################################
;;;* SAVE AND BACKUP
;;########################################################################
;; Put backups elsewhere:
(setq auto-save-interval 2400)
(setq auto-save-timeout 300)
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/.saves-")
(setq backup-directory-alist '(("." . "~/cache/emacs/backup"))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5 ; Old versions to keep
      )

;;######################################################################
;;;* MISCELLANEOUS PREFERENCES
;;######################################################################

;; For lazy typists
(fset 'yes-or-no-p 'y-or-n-p)
;; Move the mouse away if the cursor gets close
(mouse-avoidance-mode 'animate)

;; highlight the current line, as in Matlab
;; (global-hl-line-mode)

;; Confirm when killing Emacs
(setq confirm-kill-emacs (lambda (prompt)
                           (y-or-n-p-with-timeout prompt 2 nil)))

(use-package uniquify
  :init  (setq uniquify-buffer-name-style 'forward))

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t))

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; FULLSCREEN
(global-set-key [f11] 'toggle-frame-fullscreen)

;; Byte-compile elisp files immediately after saving them if .elc exists:
(defun auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already
  exists an `.elc' file corresponding to the current buffer file,
  then recompile the file."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(global-prettify-symbols-mode 1)

;; Save and resume session
(use-package desktop
  :config
  (setq desktop-auto-save-timeout 300)
  (setq desktop-path '("~/.cache/emacsdesktop"))
  (setq desktop-dirname "~/.cache/emacsdesktop")
  (setq desktop-base-file-name "desktop")
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning t)
  (setq desktop-restore-eager 4)
  (setq desktop-restore-frames t)
  (setq desktop-save 'ask-if-new)
  (desktop-save-mode 1))

;;--------------------------
;;;** MINIBUFFER PREFERENCES
;;--------------------------
;; Enable recursive minibuffer edits
(setq enable-recursive-minibuffers 1)

;;######################################################################
;;;* INTERFACING WITH THE OS
;;######################################################################

(if IS-WINDOWS
    (setq shell-file-name "C:/cygwin/cygwin.bat"))

;; Set default www browser
(if IS-LINUX
    (setq
    ;browse-url-browser-function 'browse-url-generic
     browse-url-generic-program "/usr/bin/qutebrowser"
     ))

(use-package auth-source-pass
  :init (auth-source-pass-enable))
;; Consult clipboard before primary selection
;; http://www.gnu.org/software/emacs/manual/
;; html_node/emacs/Clipboard.html

(use-package select
  :config
  (setq select-enable-clipboard t)
  )


(use-package comint
  :general
  ("C-!" 'shell-command-at-line)
  (:keymaps 'shell-mode-map
   :states  '(insert emacs)
   "SPC"    'comint-magic-space)
  :config
  ;; Arrange for Emacs to notice password prompts and turn off echoing for them, as follows:
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt)

; package ansi-color
(setq ansi-color-for-comint-mode t)

;; Auto-kill buffer and window of comint process when done
(advice-add 'comint-send-eof :after
            (defun comint-kill-after-finish-a (&rest _args)
              (let (confirm-kill-processes kill-buffer-query-functions)
                ;; (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
                (ignore-errors (kill-buffer-and-window)))))

;;;###autoload
(defun shell-command-at-line (&optional prefix)
  "Run contents of line around point as a shell command and
replace the line with output. With a prefix argument, append the
output instead."
  (interactive "P")
  (let ( (command (thing-at-point 'line)) )
    (cond ((null prefix)
           (kill-whole-line)
           (indent-according-to-mode))
          (t (newline-and-indent)))
    (shell-command command t nil)
    (exchange-point-and-mark))))

(use-package piper
  :load-path "~/.local/share/git/melpa/emacs-piper/"
  :bind ("C-x |" . piper)
  :config
  (defun +piper-start (&optional arg)
    "Start piper. With prefix ARG, start piper on current buffer"
    (interactive "P")
    (if arg (piper) (piper-user-interface))
    ))

(use-package explain-pause-mode
  :load-path "~/.local/share/git/melpa/explain-pause-mode/")

;;----------------------------------------------------------------------
;;;** ESHELL PREFERENCES
;;----------------------------------------------------------------------
(setq eshell-buffer-shorthand t)
(setq eshell-directory-name "~/.cache/emacs/eshell/")
(defun delete-window-if-not-single ()
  "Delete window if not the only one."
  (when (not (one-window-p))
    (delete-window)))
(advice-add 'eshell-life-is-too-much :after 'delete-window-if-not-single)

;;######################################################################
;;;* LINE NUMBERS
;;######################################################################
(line-number-mode 1)

(defvar +addons-enabled-modes (list 'prog-mode-hook
                                    'conf-unix-mode-hook
                                    'conf-windows-mode-hook
                                    'conf-javaprop-mode-hook
                                    'tex-mode-hook
                                    'text-mode-hook
                                    'message-mode-hook)
  "List of modes where special features (like line numbers) should be enabled.")

;; (dolist (mode-hook +addons-enabled-modes)
;;   (add-hook mode-hook (lambda () "Turn on line numbers for major-mode"
;;                         (interactive)
;;                         (display-line-numbers-mode))))

(setq display-line-numbers-width-start t
      display-line-numbers-type 'relative)

;;######################################################################
;;;* EDITING
;;######################################################################
(use-package dabbrev
  :commands (dabbrev-expand dabbrev-completion)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t))

(use-package visual-fill-column-mode
  :commands visual-fill-column-mode
  )

(use-package diff-mode
  :defer
  :general
  (:keymaps 'diff-mode-map
   :states 'motion
   "i" 'ignore
   "f" 'next-error-follow-minor-mode
   "q" 'quit-window))

(use-package iedit
  :commands iedit-dwim
  :ensure t
  :general ("C-M-;" 'iedit-dwim)
  :config
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
        (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (when (region-active-p)
              (narrow-to-region (region-beginning) (region-end)))
            (iedit-start (current-word) (point-min) (point-max))))))))

(use-package replace
  :defer
  :general
   (:keymaps 'occur-mode-map
    :states '(normal motion)
    "gc" 'next-error-follow-minor-mode
    :states 'motion
    "f" 'next-error-follow-minor-mode)
  )
(require 'better-editing nil t)

(use-package emacs
  :config
   (setq set-mark-command-repeat-pop t)
  :general
  ("M-z"  'zap-up-to-char
   "<C-M-backspace>" 'backward-kill-sexp))

(use-package view
  :general
  (:keymaps 'view-mode-map
   :states '(normal motion visual)
   "M-SPC" 'space-menu))

(use-package register
  :general
  ("M-r" (general-simulate-key "C-x r")))

;;######################################################################
;;;* BUFFER AND WINDOW MANAGEMENT
;;######################################################################
(use-package recentf
  :defer 2
  :config
  (setq recentf-save-file "~/.cache/emacs/recentf")
  )

(use-package setup-windows
  :demand t
  :hook ((help-mode . visual-line-mode)
         (Custom-mode . visual-line-mode)
         (helpful-mode . visual-line-mode))
  ;; :bind (;; ("C-x +" . balance-windows-area)
  ;;        ("<f8>" . +make-frame-floating-with-current-buffer)
  ;;        ("C-M-`" . window-toggle-side-windows))
  :general
  ("<f8>" '+make-frame-floating-with-current-buffer
   "C-M-`" 'window-toggle-side-windows)
  (:keymaps 'space-menu-window-map
   :wk-full-keys nil
   "w" '(window-toggle-side-windows :wk "toggle side windows"))
  )

(require 'better-buffers nil t)

(use-package popup-buffers
  :after setup-windows
  :init
  (setq popup-buffers-reference-modes-list
        (append +help-modes-list
                +repl-modes-list
                +occur-grep-modes-list
                +man-modes-list
                '(Custom-mode
                  compilation-mode)))

  (setq popup-buffers-reference-buffer-list
        '("^\\*Warnings\\*"
          "^\\*Compile-Log\\*"
          "^\\*Messages\\*"
          "^\\*Backtrace\\*"
          "^\\*evil-registers\\*"
          "^\\*Apropos"
          "^Calc:"
          "^\\*ielm\\*"
          "^\\*TeX Help\\*"
          "\\*Shell Command Output\\*"
          "\\*Async Shell Command\\*"
          "\\*Completions\\*"
          "[Oo]utput\\*"
          "\\*scratch\\*"))

  (popup-buffers-mode +1)

  (defun +popup-raise-popup ()
    "Choose a popup-window to raise as a regular window"
    (interactive)
    (popup-buffers-raise-popup
     (completing-read "Raise popup: "
                      (mapcar (lambda (win-and-buf) (buffer-name (cdr win-and-buf)))
                              (append popup-buffers-open-buffer-window-alist
                                      popup-buffers-buried-buffer-window-alist))
     nil t)))

  (defun +popup-lower-to-popup ()
    "Choose a regular window to make a lower"
    (interactive)
    (let ((window-list (cl-set-difference
                        (window-list)
                        (mapcar 'car popup-buffers-open-buffer-window-alist))))
      (if (< (length window-list) 2)
          (message "Only one main window!")
        (popup-buffers-lower-to-popup
         (get-buffer
          (completing-read "Lower to popup: "
                           (mapcar (lambda (win) (buffer-name (window-buffer win)))
                                   window-list)
                           nil t))))))
  :general
  (:states 'motion
   "C-w ^" '(popup-buffers-raise-popup :wk "raise popup")
   "C-w _" '(popup-buffers-lower-to-popup :wk "lower to popup"))
  (:keymaps 'space-menu-window-map
   "^" '(+popup-raise-popup :wk "raise popup")
   "_" '(+popup-lower-to-popup :wk "lower to popup"))
  )

(use-package winum
  :ensure
  :init

  (eval-when-compile 
    (defmacro +winum-select (num)
      `(lambda (&optional arg) (interactive "P")
         (if arg 
             (winum-select-window-by-number (- 0 ,num))
           (if (equal ,num (winum-get-number))
               (winum-select-window-by-number (winum-get-number (get-mru-window t)))
             (winum-select-window-by-number ,num))))))

  (setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
      (dolist (num '(1 2 3 4 5 6 7 8 9) nil)
        (define-key map (kbd (concat "M-" (int-to-string num)))
          (+winum-select num)))
      map))

;;;###autoload
(defun +evil-mode-line-faces ()
  (if (not (fboundp 'evil-mode))
      'winum-face
    (cond
     ((evil-emacs-state-p)    '((bold :background "SkyBlue2" :foreground "black")))
     ((evil-insert-state-p)   '((bold :background "chartreuse3" :foreground "black")))
     ((evil-replace-state-p)  '((bold :background "chocolate" :foreground "black")))
     ((evil-motion-state-p)   '((bold :background "plum3" :foreground "black")))
     ((evil-visual-state-p)   '((bold :background "gray" :foreground "black")))
     ((evil-operator-state-p) '((bold :background "sandy brown" :foreground "black")))
     (t ;;'((bold :background "DarkGoldenrod2" :foreground "black")) 
         '((bold :inherit mode-line))
        )
    )))

(setq winum--mode-line-segment
      '(:eval
        (propertize (format winum-format (int-to-string (winum-get-number)))
                    'face (+evil-mode-line-faces))))
  (winum-mode 1))

(use-package winner
  :commands winner-undo
  ;; :bind ("C-c <left>" . winner-undo)
  :general
  (:keymaps 'space-menu-window-map
   :wk-full-keys nil
   "u" 'winner-undo
   "r" 'winner-redo)
  :config
  (winner-mode +1))

(use-package ace-window
  :ensure t
  ;; :bind ("C-x o" . ace-window)
  :general
  ("C-x o" 'ace-window)
  (:keymaps 'space-menu-map
   "`" 'ace-window)
  :config
  (setq aw-dispatch-always t
        aw-scope 'global
        aw-background nil
        aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?\t aw-flip-window)
          (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?s aw-split-window-vert "Split Vert Window")
          (?v aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help))))

;;######################################################################
;;;* UTILITY
;;######################################################################
;; Count words, print ASCII table, etc
(require 'utilities nil t)

(use-package dashboard
  :disabled
  :ensure t
  :init (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-show-shortcuts nil
        dashboard-center-content t
        dashboard-items '((recents  . 15)
                          ;; (projects . 6)
                          ;; (bookmarks . 5)
                          ;; (agenda . 5)
                          ;; (registers . 5)
                          )))

;; Colorize color names and parens in buffers
(use-package rainbow-mode
  :commands rainbow-mode
  :ensure t
  :config
  ;; (setq rainbow-delimiters-max-face-count 3)
  )

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

;;;###autoload
(defun sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (sudo-find-file (file-truename buffer-file-name)))

(global-set-key (kbd "C-x C-S-f") 'sudo-find-file)

;;;###autoload
(defun describe-word (word &optional prefix)
  "Briefly describe WORD entered by user. With PREFIX argument,
  show verbose descriptions with hyperlinks."
  (interactive "sDescribe word: \nP")
  (shell-command (concat "dict " word (cond ((null prefix) nil)
                                            (t " -v")))))

;;;###autoload
(defun describe-word-at-point (&optional prefix)
  "Briefly describe word at point. With PREFIX argument, show
  verbose descriptions with hyperlinks."
  (interactive "P")
  (let ( (word
          (if (region-active-p)
              (buffer-substring (region-beginning)
                                (region-end))
            (thing-at-point 'word))) )
    (shell-command (concat "dict " (cond ((null prefix) nil)
                                         (t "-f "))
                           word))))

(use-package outline
  :bind (:map outline-minor-mode-map
              ("<tab>" . outline-cycle))
  :config
  (define-key outline-minor-mode-map (kbd "<backtab>") (lambda () (interactive)
                                                         (outline-back-to-heading)
                                                         (outline-cycle)))
;;;###autoload
  (defun outline-next-line ()
    "Forward line, but mover over invisible line ends.
Essentially a much simplified version of `next-line'."
    (interactive)
    (beginning-of-line 2)
    (while (and (not (eobp))
                (get-char-property (1- (point)) 'invisible))
      (beginning-of-line 2)))

  (defvar outline-cycle-emulate-tab t
    "Use tab to indent (when not on a heading) in outline-minor-mode")

  (defun outline-cycle () (interactive)
         (cond
          ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
           ;; At a heading: rotate between three different views
           (outline-back-to-heading)
           (let ((goal-column 0) beg eoh eol eos)
             ;; First, some boundaries
             (save-excursion
               (outline-back-to-heading)           (setq beg (point))
               (save-excursion (outline-next-line) (setq eol (point)))
               (outline-end-of-heading)            (setq eoh (point))
               (outline-end-of-subtree)            (setq eos (point)))
             ;; Find out what to do next and set `this-command'
             (cond
              ((= eos eoh)
               ;; Nothing is hidden behind this heading
               (message "EMPTY ENTRY"))
              ((>= eol eos)
               ;; Entire subtree is hidden in one line: open it
               (outline-show-entry)
               (outline-show-children)
               (message "CHILDREN")
               (setq this-command 'outline-cycle-children))
              ((eq last-command 'outline-cycle-children)
               ;; We just showed the children, now show everything.
               (outline-show-subtree)
               (message "SUBTREE"))
              (t
               ;; Default action: hide the subtree.
               (outline-hide-subtree)
               (message "FOLDED")))))

          ;; TAB emulation
          (outline-cycle-emulate-tab
           (call-interactively (key-binding (vector last-input-event)))
           ;; (indent-according-to-mode)
           )

          (t
           ;; Not at a headline: Do indent-relative
           (outline-back-to-heading))))
  )

;;######################################################################
;;;* COMPILATION
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

;; (add-hook 'compilation-mode-hook
;;           (lambda (&optional args)
;;             (run-at-time 3 nil
;;                          (lambda () (delete-windows-on (get-buffer "*Compile-Log*"))))))

;;######################################################################
;;;* LANGUAGE MODES
;;######################################################################

;;----------------------------------------------------------------------
;;;** LSP SUPPORT
;;----------------------------------------------------------------------
(use-package lsp-mode
  :disabled
  ;; :hook (python-mode . lsp-deferred)
  :bind (("C-c C-d" . lsp-describe-thing-at-point))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-auto-configure t
        lsp-enable-symbol-highlighting nil
        lsp-pyls-plugins-rope-completion-enabled t)
  (use-package company-lsp :ensure t :commands company-lsp)
  (add-to-list 'lsp-language-id-configuration '(matlab-mode . "matlab"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/.local/share/git/matlab-langserver/matlab-langserver.sh")
                    :major-modes '(matlab-mode)
                    :server-id 'matlab-langserver)
   ;; (make-lsp-client :new-connection (lsp-stdio-connection
   ;;                                   `("java",(let ((MATLABROOT "/opt/MATLAB/R2018b")
   ;;                                                 (SERVERROOT "~/.local/share/git/matlab-langserver"))
   ;;                                             (concat " -Djava.library.path="
   ;;                                                     MATLABROOT
   ;;                                                     "/bin/glnxa64 -cp "
   ;;                                                     MATLABROOT
   ;;                                                     "/extern/engines/java/jar/engine.jar:"
   ;;                                                     MATLABROOT
   ;;                                                     "/java/jar/jmi.jar:"
   ;;                                                     SERVERROOT
   ;;                                                     "/build/libs/lsp-matlab-0.1.jar org.tokor.lspmatlab.Application"))))
   ;;                  :major-modes '(matlab-mode)
   ;;                  :server-id 'matlab-langserver)
   )
  (use-package lsp-ui
    :disabled t
    :commands lsp-ui-mode
    :config
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    (setq lsp-ui-sideline-enable nil
          lsp-ui-doc-enable nil
          lsp-ui-flycheck-enable nil
          lsp-ui-imenu-enable t
          lsp-ui-sideline-ignore-duplicate t))
  )


;;----------------------------------------------------------------------
;;;** EGLOT - LSP
;;----------------------------------------------------------------------
(use-package eglot
  ;; :disabled t
  :ensure t
  :commands eglot
  :bind (:map eglot-mode-map
              ("C-h ." . eglot-help-at-point))
  :config
  (add-to-list 'eglot-server-programs '(matlab-mode . ("~/.local/share/git/matlab-langserver/matlab-langserver.sh" "")))
  )
;;----------------------------------------------------------------------
;;;** AUCTEX-MODE & ADDITIONS
;;----------------------------------------------------------------------
(use-package latex
  :defer 5
  :after tex
  :ensure auctex
  :hook (LaTeX-mode . electric-pair-mode)
  :mode
  ("\\.tex\\'" . latex-mode)
  :init (add-hook 'latex-mode-hook
                  (lambda ()  (interactive) (outline-minor-mode)
                    (setq-local page-delimiter "\\\\section\\**{")
                    (setq-local outline-regexp "\\\\\\(sub\\)*section\\**{")
                    ;(setq-local prettify-symbols-alist tex--prettify-symbols-alist)
                    (outline-hide-sublevels 3)
                    ))
  :defines (TeX-auto-save
            TeX-parse-self
            TeX-electric-escape
            TeX-PDF-mode
            TeX-source-correlate-method
            TeX-newline-function
            TeX-view-program-list
            TeX-view-program-selection
            TeX-mode-map
            )
  ;; :bind (:map TeX-mode-map
  ;;             ;; ("M-SPC" . TeX-matrix-spacer)
  ;;             ("C-M-9" . TeX-insert-smallmatrix)
  ;;             ("C-M-]" . TeX-insert-bmatrix)
  ;;             ;; ("C-;" . TeX-complete-symbol)
  ;;             )
  :general
  (leader-define-key :keymaps 'LaTeX-mode-map
   "cn" '(TeX-next-error :wk "Next Error")
   "cp" '(TeX-previous-error :wk "Prev Error"))

  (localleader-define-key :keymaps 'LaTeX-mode-map

    "c" '(:ignore t                   :wk "Compile")
    "cc" '(TeX-command-master         :wk "Compile doc")
    "C" '(TeX-command-run-all         :wk "Compile and view")
    "ca" '(TeX-command-run-all        :wk "Compile and view")
    "cr" '(TeX-command-run-all-region :wk "Compile region")
    "cn" '(TeX-next-error             :wk "Next Error")
    "cp" '(TeX-previous-error         :wk "Prev Error")

    "p"  '(:ignore t                  :wk "Preview")
    "pp" '(preview-at-point           :wk "Preview at point")
    "pb" '(preview-buffer             :wk "Preview buffer")
    "pr" '(preview-region             :wk "Preview region")
    "pe" '(preview-environment        :wk "Preview environment")
    "pd" '(preview-document           :wk "Preview document")
    "ps" '(preview-section            :wk "Preview section")
    "pw" '(preview-copy-region-as-mml :wk "Copy MathML")
    "pc" '(preview-clearout           :wk "Clearout")
    ;; "pC" '(preview-clearout-buffer    :wk "!Clearout in buffer")

    ;; "t" '(:ignore t                   :wk "Toggle")
    ;; "t8" '(prettify-symbols-mode      :wk "!Pretty Symbols mode")
    ;; "tp" '(preview-clearout-at-point  :wk "!Preview at point")
    ;; "ts" '(preview-clearout-section   :wk "!Preview in section")
    ;; "tb" '(preview-clearout-buffer    :wk "!Preview in buffer")

    "=" '(reftex-toc                  :wk "TOC")
    "(" '(reftex-label                :wk "Insert Label")
    ")" '(reftex-reference            :wk "Insert Ref")
    "[" '(reftex-citation             :wk "Insert Cite")
    )

    ;; (evil-leader/set-key-for-mode 'latex-mode
    ;; "cc" 'TeX-command-master
    ;; "ca" 'TeX-command-run-all
    ;; "=" 'reftex-toc
    ;; "("  'reftex-label
    ;; ")" 'reftex-reference
    ;; "[" 'reftex-citation
    ;; "{" 'cdlatex-environment)

  :config
  (progn
    (defvar my-preamble-file (concat (expand-file-name
                                      (file-name-as-directory "~/Documents/"))
                                     "hwstyle.tex")
      "File containing my stock preamble for LaTeX documents")
    ;; (defun TeX-matrix-spacer () (interactive) (insert " & "))
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
     ;; Setting this to t messes up previews
     ;; If previews still don't show disable the hyperref package
     TeX-PDF-mode nil)
    (setq-default TeX-source-correlate-mode t)
    (setq TeX-source-correlate-method 'synctex)
    (setq-default TeX-source-correlate-start-server t)
    (setq TeX-newline-function 'reindent-then-newline-and-indent)
    (setq TeX-PDF-from-DVI "Dvips")
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

(use-package latex-extra
  :after latex
  :defines (latex-extra-mode)
  :hook (LaTeX-mode . latex-extra-mode)
  :general
  (localleader-define-key
    :keymaps 'latex-extra-mode-map
    "C-q" '(latex/clean-fill-indent-environment :wk "clean up doc")))
;; :config
;; (defface latex/unimportant-latex-face
;;  '((t :height 0.7
;;       :inherit font-lock-comment-face))
;;  "Face used on less relevant math commands.")

;; (font-lock-add-keywords
;;  'latex-mode
;;  `((,(rx (or (and "\\" (or (any ",.!;")
;;                            (and (or "left" "right"
;;                                     "big" "Big")
;;                                 symbol-end)))
;;              (any "_^")))
;;     0 'latex/unimportant-latex-face prepend))
;;  'end)


(use-package preview
  :after latex
  :defer 5
  :init
  (setq preview-scale-function '+preview-scale-larger)
  :config
  (defun +preview-scale-larger ()
    "Increase the size of `preview-latex' images"
    (lambda nil (* 1.25 (funcall (preview-scale-from-face))))))

(use-package reftex
  :after latex
  :defer 20
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-use-multiple-selection-buffers t)
  )

;; (setq-default TeX-master nil)
(use-package cdlatex
  :after latex
  :ensure t
  :defer 2
  ;; :commands turn-on-cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :general (:keymaps 'cdlatex-mode-map "[" nil "(" nil "{" nil)
  :config
  (progn
    (setq cdlatex-command-alist
          '(("vc" "Insert \\vect{}" "\\vect{?}"
             cdlatex-position-cursor nil nil t)
            ("smat" "Insert smallmatrix env"
             "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
             cdlatex-position-cursor nil nil t)
            ("bmat" "Insert bmatrix env"
             "\\begin{bmatrix} ? \\end{bmatrix}"
             cdlatex-position-cursor nil nil t)
            ("pmat" "Insert pmatrix env"
             "\\begin{pmatrix} ? \\end{pmatrix}"
             cdlatex-position-cursor nil nil t)
            ("equ*" "Insert equation* env"
             "\\begin{equation*}\n?\n\\end{equation*}"
             cdlatex-position-cursor nil t nil)
            ("sn*" "Insert section* env"
             "\\section*{?}"
             cdlatex-position-cursor nil t nil)
            ("ss*" "Insert subsection* env"
             "\\subsection*{?}"
             cdlatex-position-cursor nil t nil)
            ("sss*" "Insert subsubsection* env"
             "\\subsubsection*{?}"
             cdlatex-position-cursor nil t nil)))

    (setq cdlatex-math-symbol-alist '((?F ("\\Phi"))
                                      (?o ("\\omega" "\\mho" "\\mathcal{O}"))
                                      (?6 ("\\partial"))
                                      (?v ("\\vee" "\\forall"))))
    (setq cdlatex-math-modify-alist '((?b "\\mathbb" "\\textbf" t nil nil)
                                      (?B "\\mathbf" "\\textbf" t nil nil)))
    (setq cdlatex-paired-parens "$[{("))
  )

(use-package inkscape-figures
  :defer
  :after latex
  :general (:keymaps 'LaTeX-mode-map
            "C-c i" '+inkscape-figures-create-at-point
            "C-c e" '+inkscape-figures-edit
            )
  )

;;----------------------------------------------------------------------
;;;** MATLAB
;;----------------------------------------------------------------------
(use-package matlab
  :defer
  :ensure matlab-mode
  ;; :after 'evil
  ;; :commands (matlab-mode matlab-shell matlab-shell-run-block)
  :hook ((matlab-mode . company-mode-on)
         (matlab-mode . (lambda ()  (interactive)
                          (setq-local buffer-file-coding-system 'us-ascii)
                          (outline-minor-mode)
                          (setq-local page-delimiter "%%+")
                          (setq-local outline-regexp "^\\s-*%%+")
                          (outline-hide-sublevels 3)
                          ))
         (matlab-shell-mode . (lambda ()
                                (setq comint-process-echoes t)
                                (setq-local company-idle-delay 0.1)
                                (company-mode-on))))
  :bind (:map matlab-mode-map
              ("C-c C-b" . 'matlab-shell-run-block))
  :config
  ;; (load-library "matlab-load")
  (matlab-cedet-setup)
  (semantic-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (global-semantic-decoration-mode 1)
  ;; (add-hook 'matlab-mode-hook #'company-mode-on)
  ;; (add-hook 'matlab-mode-hook #'hs-minor-mode)
  ;; (add-hook 'matlab-mode-hook (lambda ()  (interactive)
  ;;                               (setq-local buffer-file-coding-system 'us-ascii)
  ;;                              (outline-minor-mode)
  ;;                               (setq-local page-delimiter "%%+")
  ;;                               (setq-local outline-regexp "^\\s-*%%+")
  ;;                               (outline-hide-sublevels 3)
  ;;                               ))

  ;; (add-hook 'matlab-mode-hook #'turn-on-evil-matlab-textobjects-mode)
  ;; (add-hook 'matlab-shell-mode-hook (lambda ()
  ;;                                     (setq-local company-idle-delay nil)
  ;;                                     (company-mode-on) ))

  ;; (define-key matlab-mode-map (kbd "C-c C-b") #'matlab-shell-run-block)

  ;; :config
  (setq matlab-shell-echoes nil)
  (setq matlab-shell-run-region-function 'matlab-shell-region->script)
  (add-hook 'matlab-shell-mode-hook (lambda () (interactive)
                                      (define-key matlab-shell-mode-map (kbd "C-<tab>") nil)))
;;;###autoload
  (defun +matlab-shell-no-select-a (&rest _args)
   "Switch back to matlab file buffer after evaluating region"  
   (select-window (get-mru-window)))
  (advice-add 'matlab-shell-run-region :after #'+matlab-shell-no-select-a)

;;;###autoload
  (defun matlab-select-block ()
    (save-excursion
      (let ((block-beg (search-backward-regexp "^%%" nil t))
            (block-end (search-forward-regexp "^%%" nil t 2)))
        (cons block-beg block-end))))

;;;###autoload
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

;;;###autoload
  (defun matlab-forward-section ()
    "Move forward section in matlab mode"
    (interactive)
    (beginning-of-line 2)
    (re-search-forward "^\\s-*%%" nil t)
    (match-end 0))

;;;###autoload
  (defun matlab-backward-section ()
    "Move forward section in matlab mode"
    (interactive)
    (re-search-backward "^\\s-*%%" nil t)
    (match-beginning 0))

  )

;;----------------------------------------------------------------------
;;;** PYTHON-MODE
;;----------------------------------------------------------------------

(use-package pyvenv
  :disabled t
  :ensure t
  :config
  (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python))

(use-package elpy
  :disabled t
  ;; :ensure t
  ;; :init
  ;; (setq python-shell-interpreter "jupyter"
  ;;       python-shell-interpreter-args "console --simple-prompt"
  ;;       python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;;              "jupyter")
  ;; (advice-add 'python-mode :before 'elpy-enable)
  :config
  (add-hook
   'python-mode-hook
   (lambda ()
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(;; Syntax
             ("def" .      #x2131)
             ("not" .      #x2757)
             ("in" .       #x2208)
             ("not in" .   #x2209)
             ("return" .   #x27fc)
             ("yield" .    #x27fb)
             ("for" .      #x2200)
             ;; Base Types
             ("int" .      #x2124)
             ("float" .    #x211d)
             ("str" .      #x1d54a)
             ("True" .     #x1d54b)
             ("False" .    #x1d53d)
             ;; Mypy
             ("Dict" .     #x1d507)
             ("List" .     #x2112)
             ("Tuple" .    #x2a02)
             ("Set" .      #x2126)
             ("Iterable" . #x1d50a)
             ("Any" .      #x2754)
             ("Union" .    #x22c3))))))

;;----------------------------------------------------------------------
;;;** GEISER
;;----------------------------------------------------------------------
(use-package geiser
  :defer
  :if (not (version-list-<
            (version-to-list emacs-version)
            '(27 0 0 0)))
  :ensure t
  :init
  (add-hook 'geiser-repl-mode-hook (lambda ()
                                     (setq-local company-idle-delay nil)
                                     ;; (company-mode-on)
                                     ))
  :config
  (setq geiser-default-implementation 'mit)
  (setq geiser-mit-binary "mechanics"))

;;----------------------------------------------------------------------
;;;** EVAL-IN-REPL
;;----------------------------------------------------------------------
(use-package eval-in-repl
  :disabled 
  :ensure t
  :init
  ;; (require 'eval-in-repl-geiser)
  (add-hook 'geiser-mode-hook
            '(lambda ()
               (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser))))

;;----------------------------------------------------------------------
;;;** SCHEME - SICM
;;----------------------------------------------------------------------

;; Make sure mit-scheme (from repos) and scmutils (from internet + sudo ./install.sh)are installed
;;;###autoload
(defun mechanics ()
  "Run mit-scheme with SCMUTILS loaded, to work with (Structure
and Interpretation of Classical Mechanics) - The book."
  (interactive)
  (setenv "MITSCHEME_BAND" "mechanics.com")
  (setenv "MITSCHEME_HEAP_SIZE" "100000")
  (run-scheme
   "/usr/bin/mit-scheme --library /opt/mit-scheme/lib/mit-scheme-x86-64/")
  )

;;######################################################################
;;;* PLUGINS
;;######################################################################

;;----------------------------------------------------------------------
;; UNDO-TREE
;;----------------------------------------------------------------------
(use-package undo-tree
  :defer
  :config (setq undo-tree-enable-undo-in-region  t)
  )

;;----------------------------------------------------------------------
;; FLYMAKE
;;----------------------------------------------------------------------
(use-package flymake
  :defer
  :config
;;;###autoload
  (defun flymake--take-over-error-a (orig-fn &optional arg reset)
    "If there is no `next-error' locus use `next-error' to go to
    flymake errors instead"
   (interactive "P")
   (let ((sys (+error-delegate)))
     (cond
      ((eq 'flymake sys) (funcall 'flymake-goto-next-error arg
                                  (if current-prefix-arg
                                      '(:error :warning))
                                  t))
      ((eq 'emacs sys) (funcall orig-fn arg reset)))))

;;;###autoload
  (defun +error-delegate ()
    "Decide which error API to delegate to.

Delegates to flymake if it is enabled and the `next-error' buffer
is not visible. Otherwise delegates to regular Emacs next-error."
    (if (and (bound-and-true-p flymake-mode)
             (let ((buf (ignore-errors (next-error-find-buffer))))
               (not (and buf (get-buffer-window buf)))))
        'flymake
      'emacs))

  (advice-add 'next-error :around #'flymake--take-over-error-a))
    
;;----------------------------------------------------------------------
;; BROWSE-URL
;;----------------------------------------------------------------------
(use-package browse-url
  :commands (browse-url-at-point-mpv browse-url-mpv) 
  :config
  (when IS-LINUX
    (defun browse-url-mpv (url &optional single)
      (start-process "mpv" nil (if single "mpv" "umpv")
                     (shell-quote-wildcard-pattern url)))

    (defun browse-url-at-point-mpv (&optional single)
        "Open link in mpv"
        (interactive "P")
        (let ((browse-url-browser-function
               (if single
                   (lambda (url &optional _new-window) (browse-url-mpv url t))
                 #'browse-url-mpv)))
        (browse-url-at-point)))

    (setq browse-url-browser-function
          '(("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
            ("." . browse-url-generic)))))

;;----------------------------------------------------------------------
;; TRANSIENT
;;----------------------------------------------------------------------
(use-package transient
  :defer
  :config
  (setq transient-history-file "~/.cache/emacs/transient/history.el"
        transient-levels-file "~/.cache/emacs/transient/levels.el"
        transient-values-file "~/.cache/emacs/transient/values.el"))

;;----------------------------------------------------------------------
;;;** HYDRAS
;;----------------------------------------------------------------------
(use-package hydra
  :defer
  :config
  (with-eval-after-load 'ediff
    (defhydra hydra-ediff (:color blue :hint nil)
      "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
      ("b" ediff-buffers)
      ("B" ediff-buffers3)
      ("=" ediff-files)
      ("f" ediff-files)
      ("F" ediff-files3)
      ("c" ediff-current-file)
      ("r" ediff-revision)
      ("l" ediff-regions-linewise)
      ("w" ediff-regions-wordwise)))

  (with-eval-after-load 'smerge-mode
    (defhydra hydra-smerge
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue)))

(defhydra go-menu ()
  "Occur mode"
  ("n" next-error "next error" :color red)
  ("p" previous-error "prev error" :color red)
  ("g" goto-line "goto line" :color blue)
  ("TAB" move-to-column "goto col" :color blue)
  ("c" goto-char "goto char" :color blue )
  )

;; (eval
;;  `(defhydra hydra-evil-window-map (:columns 2)
;;     ,@(mapcar (lambda (x)
;;                 (list (car x) (intern (cdr x)) (cdr x)))
;;               (which-key--get-keymap-bindings evil-window-map t))
;;     ("q" nil "quit")))
;; (hydra-evil-window-map/body)

(defhydra hydra-toggle-menu (:color blue :hint nil)
  "
^Toggle!^ [_Q_]uit
^Appearance^          ^Editing^             ^Highlight^        ^Code^
^^^^^^^-------------------------------------------------------------------------------
_t_: color theme       _r_: read only      _h l_: line         _g_: vc gutter
_B_: BIG mode          _n_: line numbers   _h p_: paren        _f_: flymake
_M_: smart mode line   _q_: auto fill      _h w_: whitespace   _o_: outline/folding
                   _v l_: visual lines   _h d_: delimiters   _e_: electric pair 
_8_: pretty symbols  _v f_: visual fill    _h r_: rainbow    _s p_: smart parens
                     _V_: view mode
" 
 
  ("e" electric-pair-mode)
  ("s p" smartparens-mode)
  ("n" display-line-numbers-mode)
  ("v l" visual-line-mode)
  ("v f" (lambda () (interactive)
           (cond
            (visual-fill-column-mode
             (visual-line-mode -1)
             (visual-fill-column-mode -1))
            (t
             (visual-line-mode 1)
             (visual-fill-column-mode 1)))))
  ("8" prettify-symbols-mode)
  ("B" presentation-mode)
  ("t" toggle-theme)
  ("M" nil)
  ("r" read-only-mode)
  ("q" auto-fill-mode)
  ("V" view-mode)
  ("h l" hl-line-mode)
  ("h p" show-paren-mode)
  ("h w" whitespace-mode)
  ("h d" rainbow-delimiters-mode)
  ("h r" rainbow-mode)
  ("g" diff-hl-mode)
  ("f" flymake-mode)
  ("o" outline-minor-mode)
  ("Q" nil "quit" :color blue))

(defhydra hydra-winner (:body-pre (funcall 'winner-undo))
  "winner"
  ("u" winner-undo "undo")
  ("r" winner-redo "redo")
  ("q" nil "quit" :color blue)
  )
  ;; (:states 'emacs
  ;;  "C-n" (hydra-move hydra-move-down  'next-line)
  ;;  "C-p" (hydra-move hydra-move-up    'previous-line)
  ;;  )
  
(with-eval-after-load 'outline
  (defhydra hydra-outline (:color pink :hint nil)
    "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
    ;; Hide
    ("q" outline-hide-sublevels)    ; Hide everything but the top-level headings
    ("t" outline-hide-body)         ; Hide everything but headings (all body lines)
    ("o" outline-hide-other)        ; Hide other branches
    ("c" outline-hide-entry)        ; Hide this entry's body
    ("l" outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
    ("d" outline-hide-subtree)      ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" outline-show-all)          ; Show (expand) everything
    ("e" outline-show-entry)        ; Show this heading's body
    ("i" outline-show-children)     ; Show this heading's immediate child sub-headings
    ("k" outline-show-branches)     ; Show all sub-headings under this heading
    ("s" outline-show-subtree)      ; Show (expand) everything in this heading & below
    ;; Move
    ("u" outline-up-heading)                ; Up
    ("n" outline-next-visible-heading)      ; Next
    ("p" outline-previous-visible-heading)  ; Previous
    ("f" outline-forward-same-level)        ; Forward - same level
    ("b" outline-backward-same-level)       ; Backward - same level
    ("z" nil "leave")))

  :general
  ("M-g" 'go-menu/body)
  (:keymaps 'smerge-mode-map
   "C-c s" 'hydra-smerge/body)
  (:keymaps 'space-menu-window-map
    "u" '(hydra-winner/body 
          :wk "winner-mode"))
  (:states '(motion)
    "C-w u" 'hydra-winner/body)
  ("<f8>"  'hydra-toggle-menu/body
   "C-c <tab>" 'hydra-outline/body
   )
   (:keymaps 'space-menu-map
             "t" 'hydra-toggle-menu/body)
   (:keymaps 'space-menu-map
    :prefix "f"
    "=" 'hydra-ediff/body)
  )

;;----------------------------------------------------------------------
;;;** TABS!TABS!TABS!
;;;*** TAB-BAR
;;----------------------------------------------------------------------
(use-package tab-bar
  :if (not (version-list-<
            (version-to-list emacs-version)
            '(27 0 1 0)))
  :after cus-face
  :defer
  :general
  ("C-x t 2" 'tab-new
   "C-<tab>" 'tab-bar-switch-to-next-tab
   "C-S-<tab>" 'tab-bar-switch-to-prev-tab)

  :config
  (setq  tab-bar-close-last-tab-choice 'tab-bar-mode-disable
         tab-bar-show                  1
         tab-bar-tab-name-truncated-max 14
         tab-bar-new-tab-choice        'ibuffer
         ;; tab-bar-select-tab-modifiers  '(meta)
         tab-bar-tab-name-function     '(lambda nil (upcase (buffer-name))))

  (custom-set-faces
   '(tab-bar ((t (:inherit nil :height 1.1))))
   '(tab-bar-tab ((t (:inherit tab-bar :underline t :weight bold))))
   '(tab-bar-tab-inactive ((t (:inherit tab-bar :weight normal :height 0.8)))))

  (advice-add 'tab-bar-rename-tab
              :after
              (defun +tab-bar-name-upcase (_name &optional _arg)
                "Upcase current tab name"
                (let* ((tab (assq 'current-tab (frame-parameter nil 'tabs)))
                       (tab-name (alist-get 'name tab)))
                  (setf (alist-get 'name tab) (upcase tab-name)
                        (alist-get 'explicit-name tab) t))
                )))
;;----------------------------------------------------------------------
;;;*** EYEBROWSE
;;----------------------------------------------------------------------
;; This is superceded by native tabs (tab-bar-mode) in Emacs 27, only
;; load if running a lower Emacs version
(when (version-list-<
       (version-to-list emacs-version)
       '(27 0 1 0))
  (use-package eyebrowse
    :ensure t
    ;; :bind ("C-c C-w c" . eyebrowse-create-window-config)
    ;; :commands eyebrowse-create-window-config
    :hook (after-init . eyebrowse-mode)
    :init (setq eyebrowse-keymap-prefix (kbd "C-x t"))
    :config
    (setq eyebrowse-new-workspace (lambda nil "Buffer menu for user buffers" (buffer-menu 1))
          eyebrowse-wrap-around t
          eyebrowse-switch-back-and-forth t)
    (define-key eyebrowse-mode-map (kbd "C-M-TAB") 'eyebrowse-next-window-config)
    (define-key eyebrowse-mode-map (kbd "C-M-<tab>") 'eyebrowse-next-window-config)
    (define-key eyebrowse-mode-map (kbd "<C-M-iso-lefttab>") 'eyebrowse-last-window-config)
    ;; (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    ;; (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    ;; (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    ;; (defmacro eyebrowse-remap-tab-keys () )
    ;; (eyebrowse-setup-opinionated-keys)

    ;; Display tab configuration in Emacs title bar
    (defun my-title-bar-format()
      (let* ((current-slot (eyebrowse--get 'current-slot))
             (window-configs (eyebrowse--get 'window-configs))
             (window-config (assoc current-slot window-configs))
             (window-config-name (nth 2 window-config))
             (num-slots (length window-configs)))
        (concat window-config-name " [" (number-to-string current-slot)
                "/" (number-to-string num-slots) "] | " "%b")))

    (if (display-graphic-p)
        (progn
          (setq frame-title-format
                '(:eval (my-title-bar-format)))))

    ))
;; (define-key ivy-minibuffer-map (kbd "C-M-w") 'ivy-yank-word)
;;----------------------------------------------------------------------
;;;** MIXED-PITCH-MODE
;;----------------------------------------------------------------------
(use-package mixed-pitch
  :disabled
  :defer 5
  :ensure t
  :hook (text-mode . mixed-pitch-mode)
  :config (add-to-list 'mixed-pitch-fixed-pitch-faces 'line-number))

;;----------------------------------------------------------------------
;;;** HIGHLIGHTS
;;----------------------------------------------------------------------
;; Flash lines
(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my/recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my/recenter-and-pulse-line))
  :init
  (with-no-warnings
    (defun my/pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my/pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my/pulse-momentary-line)))

    (defun my/recenter-and-pulse(&rest _)
      "Recenter and pulse the region or the current line."
      (recenter)
      (my/pulse-momentary))

    (defun my/recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my/pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window windmove-do-window-select
                   ace-window aw--select-window
                   pager-page-down pager-page-up
                   winum-select-window-by-number
                   ;; treemacs-select-window
                   symbol-overlay-basic-jump))
      (advice-add cmd :after #'my/pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   goto-last-change
                   scroll-up-command
                   scroll-down-command))
      (advice-add cmd :after #'my/recenter-and-pulse))))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :ensure
  :defer
  :custom-face
  (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  (diff-hl-insert ((t (:background nil))))
  (diff-hl-delete ((t (:background nil))))
  :hook ((dired-mode . diff-hl-dired-mode))
  :init
  (setq diff-hl-draw-borders t)
  (dolist (mode-hook +addons-enabled-modes)
    (add-hook mode-hook #'diff-hl-mode)) 
  :general
  (:keymaps 'vc-prefix-map
            "SPC" 'diff-hl-mark-hunk
            "n"   'diff-hl-next-hunk
            "p"   'diff-hl-previous-hunk
            "["   'nil
            "]"   'nil
            "R"   'diff-hl-revert-hunk)
  (:states '(normal visual)
           "]d"   'diff-hl-next-hunk
           "[d"   'diff-hl-previous-hunk)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Recenter to location of diff
  (advice-add 'diff-hl-next-hunk :after (lambda (&optional _) (recenter)))
  
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector #b11111100) ;(if sys/macp #b11100000 #b11111100)
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)


    ;; (unless (display-graphic-p)
    ;;   (setq diff-hl-margin-symbols-alist
    ;;         '((insert . " ") (delete . " ") (change . " ")
    ;;           (unknown . " ") (ignored . " ")))
    ;;   ;; Fall back to the display margin since the fringe is unavailable in tty
    ;;   (diff-hl-margin-mode 1)
    ;;   ;; Avoid restoring `diff-hl-margin-mode'
    ;;   (with-eval-after-load 'desktop
    ;;     (add-to-list 'desktop-minor-mode-table
    ;;                  '(diff-hl-margin-mode nil))))
    )

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  )

;;----------------------------------------------------------------------
;;;** NOTMUCH
;;----------------------------------------------------------------------
;; Left unchecked, every program grows to the point where it can be
;; used to manage your email
(require 'setup-email)

;;----------------------------------------------------------------------
;;;** NAV-FLASH
;;----------------------------------------------------------------------
;; (use-package nav-flash)

;;----------------------------------------------------------------------
;;;** YASNIPPET
;;----------------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :defer 5
  :after warnings
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :config
  ;; (use-package yasnippet-snippets
  ;;   :ensure t)
  ;; (yas-reload-all)
  ;; Redefine yas expand key from TAB because company-mode uses TAB.

  ;; (push '(yasnippet backquote-change) warning-suppress-types)

  ;; Don't throw a warning if lisp code in a snippet modifies the
  ;; buffer. We need this for auto expanded snippets in latex/org.
  (cl-pushnew '(yasnippet backquote-change) warning-suppress-types
              :test 'equal)

  (setq yas-wrap-around-region t
        yas-triggers-in-field t)

  (defun my-yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  (add-hook 'post-self-insert-hook #'my-yas-try-expanding-auto-snippets)

  (with-eval-after-load 'cdlatex
    (add-hook 'cdlatex-tab-hook #'yas-expand))

  (with-eval-after-load 'company
;;;###autoload
    (defun my-yas-company-next-field ()
      "company-complete-common or yas-next-field-or-maybe-expand."
      (interactive)
      (if company-candidates (company-complete-common)
        (yas-next-field-or-maybe-expand)))

    (define-key yas-keymap [tab] #'my-yas-company-next-field)
    (define-key yas-keymap (kbd "TAB") #'my-yas-company-next-field)

;;;###autoload
    (defun my-yas-company-cancel ()
      "company-abort or yas-abort-snippet."
      (interactive)
      (if company-candidates
          (company-abort)
        (yas-abort-snippet)))

    (define-key yas-keymap (kbd "C-g") #'my-yas-company-cancel))


  ;; (when (fboundp 'smartparens)
  ;;   (with-eval-after-load 'smartparens
  ;;     (defvar yas--smartparen-flag nil)
  ;;     (add-hook 'yas-before-expand-snippet-hook (lambda () (when smartparens-mode
  ;;                                                       (smartparens-mode -1)
  ;;                                                       (setq-local yas--smartparen-flag t))))
  ;;     (add-hook 'yas-after-exit-snippet-hook (lambda () (when yas--smartparen-flag)
  ;;                                              (smartparens-mode +1)
  ;;                                              (setq-local yas--smartparen-flag nil)))))

  ;; (define-key yas-minor-mode-map (kbd "S-SPC") (lambda (&optional num) (interactive "P")
  ;;                                                (or (yas-expand)
  ;;                                                    (insert (kbd "SPC")))))
  ;; (define-key yas-keymap (kbd "S-SPC") (lambda (&optional num) (interactive "P")
  ;;                                        (or (yas-next-field-or-maybe-expand)
  ;;                                            (insert (kbd "SPC")))))
  ;; (dolist (keymap (list yas-minor-mode-map yas-keymap))
  ;;   (define-key keymap (kbd "TAB") nil)
  ;;   (define-key keymap [(tab)] nil))
  ;; (global-set-key (kbd "M-S-SPC") 'company-yasnippet)
  )

;;----------------------------------------------------------------------
;;;** HIDESHOW (built in (DISABLED))
;;----------------------------------------------------------------------
(use-package hideshow ; built-in
  :disabled t
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
                (lambda (&optional end) "Advice to ensure `hs-minor-mode' is enabled"
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
             (matlab-mode "^\s*if\\|switch\\|case\\|otherwise\\|while\\|^\s*for\\|try\\|catch\\|function"
                          "end"
                          "" (lambda (_arg) (matlab-forward-sexp)))
             (nxml-mode "<!--\\|<[^/>]*[^/]>"
                        "-->\\|</[^/>]*[^/]>"
                        "<!--" sgml-skip-tag-forward nil))
           hs-special-modes-alist
           '((t))))))

;;----------------------------------------------------------------------
;;;** VIMISH-FOLD
;;----------------------------------------------------------------------
;;(use-package vimish-fold
;;   :ensure t
;;   )

;;----------------------------------------------------------------------
;;;** EDIFF (built-in)
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
;;;** HELPFUl
;;----------------------------------------------------------------------
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable)
  ;; :hook (helpful-mode . (lambda () (line-number-mode 0)))
  :init
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h C") #'helpful-command)
  (global-set-key (kbd "C-h .") #'helpful-at-point)
  (global-set-key (kbd "C-h C-.") #'helpful-at-point)
  :general
  (:keymaps 'help-map
            :wk-full-keys nil
            "." '(helpful-at-point :wk "help at point"))
  ;; (:keymaps 'space-menu-help-map
  ;;           :wk-full-keys nil
  ;;           "f" '(helpful-callable :wk "Describe function")
  ;;           "v" '(helpful-variable :wk "Describe variable")
  ;;           "k" '(helpful-key :wk "Describe keybind")
  ;;           "." '(helpful-at-point :wk "Help at point")
  ;;           "C" '(helpful-command :wk "Describe command"))
  )


;;----------------------------------------------------------------------
;;;** SHACKLE
;;----------------------------------------------------------------------
(use-package shackle
  :disabled t
  :init (shackle-mode))

;;----------------------------------------------------------------------
;;;** VERSION CONTROL
;;----------------------------------------------------------------------
(use-package magit
  :defer t
  ;; :commands magit-status
  :ensure t
  :bind ("C-x g" . magit-status)
  :hook (magit-diff-visit-file . (lambda ()
                                   (when (and smerge-mode
                                              (fboundp 'hydra-smerge/body))
                                     (hydra-smerge/body))))
  :config
  (define-key magit-mode-map (kbd "C-TAB") nil)
  (define-key magit-mode-map (kbd "C-<tab>") nil)
  ;; (dolist (keymap (list magit-diff-mode-map magit-log-mode-map))
  ;;   (define-key keymap (kbd "C-TAB") nil)
  ;;   (define-key keymap (kbd "C-<tab>") nil))
  )

;;----------------------------------------------------------------------
;;;** WHICH-KEY
;;----------------------------------------------------------------------
(use-package which-key
  :ensure t
  :defer 1
  :general
  (:keymaps 'help-map
   "h" 'which-key-show-major-mode)
  :init
  (setq which-key-sort-order #'which-key-description-order
        ;; which-key-sort-order #'which-key-prefix-then-key-order
        which-key-idle-delay 0.8
        which-key-idle-secondary-delay 0.1
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 0
        which-key-max-display-columns nil
        which-key-min-display-lines 8
        which-key-side-window-slot -10
        which-key-show-transient-maps nil)
  :config
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
      which-key-replacement-alist)
  (with-eval-after-load 'general
    (which-key-add-key-based-replacements general-localleader "major-mode")
    (which-key-add-key-based-replacements general-localleader-alt "major-mode"))

  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  ;; (which-key-setup-side-window-bottom)
  (which-key-setup-side-window-right-bottom)
  (add-hook 'which-key-init-buffer-hook
            (lambda () (setq-local line-spacing 3)))

  (which-key-mode +1)

  :diminish "")

;;----------------------------------------------------------------------
;;;** CALC
;;----------------------------------------------------------------------
;;;###autoload
(defun calc-on-line ()
 "Evaluate `calc' on the contents of line at point." 
  (interactive)
       (cond ((region-active-p)
              (let* ((beg (region-beginning))
                     (end (region-end))
                     (string (buffer-substring-no-properties beg end)))
                (kill-region beg end)
                (insert (calc-eval string))))
             (t (end-of-line) (insert " = " (calc-eval (thing-at-point 'line))))))
(global-set-key (kbd "C-S-e") 'calc-on-line)

;;----------------------------------------------------------------------
;;;** ISEARCH
;;----------------------------------------------------------------------
(require 'setup-isearch)

;;----------------------------------------------------------------------
;;;** ABBREV MODE
;;----------------------------------------------------------------------
;; (setq save-abbrevs t)
;; (if (file-exists-p abbrev-file-name)
;;     (quietly-read-abbrev-file))

;;----------------------------------------------------------------------
;;;** COMPANY-MODE
;;----------------------------------------------------------------------
(use-package company
  :ensure t
  :defer 3
  :general
  ("C-;"      'company-complete)

  (:keymaps   'company-active-map
  "C-;"       'company-other-backend
  "C-w" nil
  "C-]"       'company-show-location
  "M-."       'company-show-location)

  (:keymaps   'company-search-map
   [return]   'company-complete-selection
   "RET"      'company-complete-selection
   "S-SPC"    'company-search-toggle-filtering)

  ;; (:keymaps   'company-active-map
  ;; "<tab>"     'company-complete-common-or-cycle
  ;; "TAB"       'company-complete-common-or-cycle
  ;; "<backtab>" 'company-select-previous
  ;; "S-TAB"     'company-select-previous
  ;; "M-n"        nil
  ;; "M-p"        nil
  ;; "C-n"       'company-select-next
  ;; "C-p"       'company-select-previous)

  :config
  ;; (add-to-list 'company-backends 'company-files)
  ;; (add-to-list 'company-backends 'company-dabbrev)
  ;; (add-to-list 'company-backends 'company-jedi)
  ;; (add-to-list 'company-backends 'company-dict)
  (setq company-idle-delay 0.2
        company-dabbrev-downcase 0
        company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-code-other-buffers t
        company-dabbrev-ignore-case nil
        ;;company-tooltip-flip-when-above t
        ;; company-transformers '(company-sort-by-occurrence)
        ;; company-transformers '(company-sort-by-backend-importance)
        ;; company-transformers '(company-sort-by-statistics)
        company-global-modes '(latex-mode matlab-mode emacs-lisp-mode lisp-interaction-mode
                               python-mode sh-mode fish-mode conf-mode text-mode org-mode)
        company-backends '((company-files company-capf company-keywords) company-dabbrev))

  (add-hook 'matlab-mode-hook (lambda ()
                                ;; (unless (featurep 'company-matlab)
                                ;;   (require 'company-matlab))
                                (make-local-variable 'company-backends)
                                (setq-local company-backends '((company-files company-capf company-dabbrev)))
                                ;; (add-to-list 'company-backends
                                ;;              ;; 'company-matlab
                                ;;              'company-semantic
                                ;;              )
                                ))
  (add-hook 'matlab-shell-mode-hook (lambda ()
                                      (make-local-variable 'company-backends)
                                      (setq-local company-idle-delay 0.3)
                                      ;; (add-to-list 'company-backends 'company-matlab-shell)
                                      ))

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (make-local-variable 'company-idle-delay)
                               (setq-local company-idle-delay 0.5)
                               ))

  ;; ;; AC-mode style settings
  ;; (defun company-ac-setup ()
  ;;   "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
  ;;   (defun my-company-visible-and-explicit-action-p ()
  ;;     (and (company-tooltip-visible-p)
  ;;          (company-explicit-action-p)))
  ;;   (setq company-require-match nil)
  ;;   (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  ;;   ;; (setq company-frontends
  ;;   ;;       '(company-pseudo-tooltip-unless-just-one-frontend
  ;;   ;;         company-preview-frontend
  ;;   ;;         company-echo-metadata-frontend))
  ;;   (setq company-frontends '(company-echo-metadata-frontend
  ;;                             company-pseudo-tooltip-unless-just-one-frontend-with-delay
  ;;                             company-preview-frontend))
  ;;   (define-key company-active-map [tab]
  ;;     'company-select-next-if-tooltip-visible-or-complete-selection)
  ;;   (define-key company-active-map (kbd "TAB")
  ;;     'company-select-next-if-tooltip-visible-or-complete-selection))

  ;; Not needed. cdlatex mode handles completion just fine
  (use-package company-auctex
    :disabled
    :defer t
    :config
    (add-to-list 'company-backends 'company-auctex)
    (company-auctex-init))

  ;; (company-ac-setup)
  (company-tng-configure-default)
  (global-company-mode)
  )

(use-package company-prescient
  :after company
  :defer 3
  :ensure t
  :init (company-prescient-mode))
(use-package company-statistics
  :disabled
  :after company
  :defer 5
  :ensure t
  ;; :hook (after-init . company-statistics-mode)
  :init  (company-statistics-mode) 
  :config
  (setq company-statistics-file (concat (expand-file-name
                                         (file-name-as-directory "~/.cache"))
                                        "company-statistics-cache.el")))

;;----------------------------------------------------------------------
;;;** SMARTPARENS-MODE
;;----------------------------------------------------------------------
(or (use-package elec-pair
      :disabled
      :defer 3
      :config (electric-pair-mode +1))
    (use-package smartparens
      :hook ((emacs-lisp-mode lisp-interaction-mode) . smartparens-mode)
      :general
      (:keymaps        'smartparens-mode-map
       "C-M-<up>"      'sp-raise-sexp
       "C-<right>"     'sp-forward-slurp-sexp
       "C-<left>"      'sp-backward-slurp-sexp
       "M-<right>"     'sp-forward-barf-sexp
       "M-<left>"      'sp-backward-barf-sexp
       "C-k"           'sp-kill-hybrid-sexp
       "C-x C-t"       'sp-transpose-hybrid-sexp
       "C-M-n"         'sp-next-sexp
       "C-M-p"         'sp-previous-sexp
       "C-<backspace>" 'sp-backward-kill-word) 
      :config
      (sp-with-modes sp-lisp-modes
        ;; disable ', it's the quote character!
        (sp-local-pair "'" nil :actions nil))
      ;; (require 'smartparens-config)
      ))

;;----------------------------------------------------------------------
;;;** EXPAND-REGION
;;----------------------------------------------------------------------
(use-package expand-region
  :ensure t
  :commands expand-region
  :bind ("C-," . 'er/expand-region))

;;----------------------------------------------------------------------
;;;** AVY-MODE
;;----------------------------------------------------------------------
(use-package avy
  :ensure t
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :general
  ("C-'" '(avy-goto-word-or-subword-1 :wk "Goto word")
   "M-j" '(avy-goto-char-2 :wk "Goto char"))
  (:states '(normal visual)
   :prefix "g"
   "s" 'avy-goto-char-timer)
  )

;;----------------------------------------------------------------------
;;;** IY-GO-TO-CHAR
;;----------------------------------------------------------------------
(use-package iy-go-to-char
  :disabled
  :bind (("M-j" . iy-go-to-char)
         ("M-r" . iy-go-to-char-key-backward)))

;;----------------------------------------------------------------------
;;;** WRAP-REGION MODE
;;----------------------------------------------------------------------
(use-package wrap-region
  :ensure t
  :init (wrap-region-mode 1))
;; (add-hook 'text-mode-hook 'wrap-region-mode)

;;----------------------------------------------------------------------
;;;** ORG-MODE
;;----------------------------------------------------------------------
(require 'setup-org nil t)
;;----------------------------------------------------------------------
;;;** ORG-ADDONS (ANKI)
;;----------------------------------------------------------------------
(use-package setup-anki
  :after (org-capture org))
;;######################################################################
;;;** COMPLETION FRAMEWORKS:
;;----------------------------------------------------------------------
;;;*** ICOMPLETE
;;----------------------------------------------------------------------
(use-package icomplete
  :disabled
  :demand
  :config
  (require 'setup-icomplete nil t)
  (icomplete-mode 1)
  )

;;----------------------------------------------------------------------
;;;*** IVY/COUNSEL/SWIPER
;;----------------------------------------------------------------------
(require 'setup-ivy)

;;; Bibtex management from ivy. Call ivy-bibtex.
(use-package ivy-bibtex
  :disabled
  :ensure t
  :after ivy
  :functions bibtex-completion-open-pdf
  :commands ivy-bibtex
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation
        bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-cite-default-as-initial-input t
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

(use-package ivy-youtube
  :ensure
  :after ivy
  :general 
  (:keymaps 'space-menu-map
    "Y" '(ivy-youtube :wk "Youtube search"))
  :config
  (setq ivy-youtube-history-file
        "~/.cache/emacs/ivy-youtube-history")
  (setq ivy-youtube-key my-ivy-youtube-key
        ivy-youtube-play-at (expand-file-name
                             "~/.local/bin/i3cmds/umpv")))

(use-package counsel-spotify
  :disabled
  :commands counsel-spotify-start-search
  :after counsel
  :general
  (:keymaps 'space-menu-map
    "U" '(counsel-spotify-start-search :wk "spotify"))
  :config
  (defun counsel-spotify-start-search ()
    (interactive)
    (counsel-M-x "counsel-spotify-search-"))
  (setq counsel-spotify-service-name "spotify")
  (setq counsel-spotify-client-id my-counsel-spotify-client-id
        counsel-spotify-client-secret my-counsel-spotify-client-secret
        counsel-spotify-use-notifications nil))

;;----------------------------------------------------------------------
;;;** TRAMP
;;----------------------------------------------------------------------
;; Tramp ssh'es into root@host to edit files. The emacs sudo, kindof.
(use-package tramp
  :defer
  :config
  (setq tramp-persistency-file-name (dir-concat
                                     (getenv "HOME")
                                     ".cache/emacs/tramp"))
  )
;;----------------------------------------------------------------------
;;;** DIRED
;;----------------------------------------------------------------------
(require 'setup-dired nil t)

(use-package projectile
  :disabled t
  :ensure t
  :init (projectile-mode +1))
;;----------------------------------------------------------------------
;;;* MODELINE:
;;######################################################################

;; (use-package telephone-line
;;   :ensure t
;;   :init
;;   (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
;;         telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
;;         telephone-line-primary-right-separator 'telephone-line-cubed-right
;;         telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
;;   (setq telephone-line-height 24
;;         telephone-line-evil-use-short-tag t)
;;   (telephone-line-mode 1))

;; (use-package spaceline
;;   :ensure t
;;   :init
;;   (require 'spaceline-config)
;;   (setq powerline-default-separator 'contour
;;         spaceline-buffer-encoding-abbrev-p nil
;;         spaceline-buffer-size-p nil
;;         spaceline-line-column-p t)
;;   (spaceline-emacs-theme))

(use-package doom-modeline
  :disabled
  :init (doom-modeline-mode 1))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/theme nil)
  (sml/setup)
  :defines sml/fix-mode-line-a
  :config
  (defun sml/fix-mode-line-a (_theme &rest _args)
    "Advice to `load-theme' to fix the mode-line height after activating/deactivating theme"
    (set-face-attribute 'mode-line nil
                        :box `(:line-width 3 :color ,(plist-get
                                                      (custom-face-attributes-get 'mode-line nil)
                                                      :background))))

  (advice-add 'disable-theme :after #'sml/fix-mode-line-a)
  (advice-add 'load-theme :after #'sml/fix-mode-line-a)

  ;; (custom-set-faces
  ;;  '(mode-line ((t (:box (:line-width 4 :color ))))))

  ;;         (lexical-let ((default-color (cons (face-background 'mode-line)
  ;;                                            (face-foreground 'mode-line))))
  ;;           (add-hook 'post-command-hook
  ;;                     (lambda ()
  ;;                       (let ((color (cond ((minibufferp) default-color)
  ;;                                          ((evil-insert-state-p) '("DarkGoldenrod2" . "black"))
  ;;                                          ((evil-emacs-state-p)  '("SkyBlue2" . "black"))
  ;;                                          ;; ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
  ;;                                          (t default-color))))
  ;;                         (set-face-background 'mode-line (car color))
  ;;                         (set-face-foreground 'mode-line (cdr color)))))))
  ;;   )
  )

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generally unhelpful and only add confusing visual clutter.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defvar mode-line-cleaner-alist
  `((company-mode . " ⇝")
    (yas-minor-mode .  " Y";; " Υ"
                    )
    (smartparens-mode . " ()";; " ﴾﴿"
                      )
    (evil-smartparens-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (evil-snipe-local-mode . "")
    (evil-owl-mode . "")
    (evil-rsi-mode . "")
    (evil-commentary-mode . "")
    (ivy-mode . "")
    (counsel-mode . "")
    (wrap-region-mode . "")
    (rainbow-mode . "")
    (which-key-mode . "")
    (undo-tree-mode . "")
    ;; (undo-tree-mode . " ⎌")
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
    (org-mode . " ORG";; "⦿"
              )
    (org-cdlatex-mode . "")
    (org-indent-mode . "")
    (org-roam-mode . "")
    (visual-line-mode . "")
    (latex-mode . "TeX")
    ;; (projectile-mode . " ϸ")
    (outline-minor-mode . " [o]";; " ֍"
                        )
    ;; Evil modes
    (evil-traces-mode . "")
    )
  "Alist for `clean-mode-line'.

  ; ;; When you add a new element to the alist, keep in mind that you
  ; ;; must pass the correct minor/major mode symbol and a string you
  ; ;; want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
           do (let* ((mode (car cleaner))
                     (mode-str (cdr cleaner))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; (display-time-mode 0)

(use-package custom
  ;; :general
  :commands toggle-theme
  :config
  (setq custom-theme-directory (expand-file-name "lisp" user-emacs-directory))

  (defun toggle-theme (theme)
    "Swap color themes. With prefix arg, don't disable the currently loaded theme first."
    (interactive
     (list
      (intern (completing-read "Load theme: "
                               (cons "user" (mapcar #'symbol-name
                                                    (custom-available-themes)))
                                     nil t))))
    (unless current-prefix-arg
      (mapc #'disable-theme custom-enabled-themes))
    (load-theme theme t))
  
  ;; :init
  ;; (load-theme 'smart-mode-line-atom-one-dark)
  ;; (load-theme 'atom-one-dark t)
  )

;;######################################################################
;;;* MINIBUFFER
;;######################################################################
(use-package minibuffer
  :config
 (require 'setup-minibuffer nil t))

;;;* FONTS AND COLORS
;;######################################################################
(use-package cus-face
  :config
  (use-package dracula-theme
    :defer
    :config
    (custom-theme-set-faces 'dracula
                            '(aw-background-face
                              ((t (:background "#282a36" :inverse-video nil :weight normal))))
                            '(aw-leading-char-face
                              ((t (:foreground "#bd93f9" :height 2.5 :weight normal))))))
  (use-package dichromacy-theme
    :defer
    :config
    (custom-theme-set-faces 'dichromacy
                            '(aw-background-face
                              ((t (:background "#ffffff" :inverse-video nil :weight normal))))
                            '(aw-leading-char-face
                              ((t (:foreground "#009e73" :height 2.5 :weight normal))))
                            '(org-level-1 ((t (:foreground "#0072b2" :inherit bold :height 1.3))))
                            '(org-level-2 ((t (:foreground "#d55e00" :inherit bold :height 1.1))))
                            '(org-document-title ((t (:inherit bold :height 1.5))))
                            ))
  (use-package gruvbox-dark-hard-theme
    :defer
    :config
    (custom-theme-set-faces 'gruvbox-dark-hard
                            '(aw-leading-char-face
                              ((t (:height 2.5 :weight normal))))
                            '(org-level-1 ((t (:height 1.3 :foreground "#83a598" :inherit (bold) ))))
                            '(org-level-2 ((t (:height 1.1 :foreground "#fabd2f" :inherit (bold) ))))
                            '(org-document-title ((t (:inherit (bold) :height 1.5))))
                          ))
  (cond (IS-LINUX
         (custom-set-faces
          '(default ((t (:family "Iosevka" :foundry "PfEd" :slant normal :weight normal :height 122 :width normal)
                        ;; (:family "FantasqueSansMono Nerd Font" :foundry "PfEd" :slant normal :weight normal :height 128 :width normal)
                        )))))
        (IS-WINDOWS
         (custom-set-faces
          '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))))

(custom-set-faces '(variable-pitch ((t (:family "Bitstream Vera Serif"))))))

;; '(org-document-title ((t (:weight bold :height 1.4))))
;; '(org-level-1 ((t (:inherit outline-1 :weight bold :height 1.3))))
;; '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.1))))

;; Unicode symbols
(when IS-LINUX (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; (add-to-list 'default-frame-alist '(alpha 100 100))

;; (custom-theme-set-faces 'dichromancy
;;                         ;; tab-bar & tab-line (since Emacs 27.1)
;;                         '(tab-bar ((t ( :foreground ,dracula-pink :background ,bg2
;;                                                     :inherit variable-pitch))))
;;                         '(tab-bar-tab ((t (:background ,dracula-current :inherit tab-bar))))
;;                         '(tab-bar-tab-inactive ((t (:foreground ,dracula-purple :background ,bg3
;;                                                                 :inherit tab-bar-tab))))
;;                         '(tab-line ((t (:height 0.9 :foreground ,dracula-pink
;;                                                 :background ,bg2 :inherit variable-pitch))))
;;                         '(tab-line-tab ((t (:background ,dracula-current :inherit tab-line))))
;;                         '(tab-line-tab-inactive ((t (:foreground ,dracula-purple :background ,bg3
;;                                                                  :inherit tab-line-tab)))))

;;######################################################################
;;;* EVIL-MODE
;;######################################################################
(require 'setup-evil)

;;######################################################################
;;;* MISC SETTINGS
;;######################################################################
;; Settings that I'm not sure where to put:
(setq srecode-map-save-file "~/.cache/emacs/srecode-map.el" )
(setq url-configuration-directory "~/.cache/emacs/url/")
(setq request-storage-directory "~/.cache/emacs/request/")
(setq semanticdb-default-save-directory "~/.cache/emacs/semanticdb/")

;;;* LOCAL-VARIABLES
;; Local Variables:
;; outline-regexp: ";;;\\*+"
;; page-delimiter: ";;;\\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:
