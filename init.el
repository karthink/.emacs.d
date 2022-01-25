;; -*- lexical-binding: t -*-

;; #+begin_quote
;;                                       my dot emacs grows
;;
;;                                       one day i look inside it
;;
;;                                       singularity
;; #+end_quote

;; * PACKAGE MANAGEMENT

;; "Activate" packages, /i.e./ autoload commands, set paths, info-nodes and so
;; on. Set load paths for ELPA packages. This is unnecessary in Emacs 27 with an
;; early-init.el, but I haven't checked.
(package-initialize)
(package-activate-all)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Package repositories that are no longer used or included by default:
;; #+begin_src emacs-lisp
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; #+end_src

;; This is a workaround for a bug that's probably been fixed by now!
;; #+begin_src emacs-lisp
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; #+end_src

;; ** USE PACKAGE

;; =use-package= is a neat wrapper over =with-eval-after-load= and =require=, a
;; judicious combination of which helps with lazy loading code. It does a lot
;; more besides, like simplify code to add hooks, bind keys and generate
;; autoloads.
;;
;; The one thing it's not is a package manager!
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

;;; (require 'bind-key)

(use-package package
  :hook (package-menu-mode . hl-line-mode))

;; Packages not managed by package.el: These are packages that for various
;; reasons I prefer to manage myself instead of installing from ELPA/MELPA. Thus
;; an ersatz one-function "package manager".

(defun my/package-sync-personal ()
  "Download my packages not managed by package.el."
  (let* ((my-packages-urls-extra)
         (download-dir "~/.local/share/git/")
         (default-directory (progn (or (file-directory-p download-dir)
                                       (make-directory download-dir))
                                   download-dir)))
    (setq my-packages-urls-extra
          '(("https://codeberg.org/jao/consult-notmuch.git")
            ("https://github.com/xFA25E/ytel-show.git")
            ("https://github.com/minad/vertico.git")
            ("https://github.com/johnbcoughlin/calctex.git")
            ("https://github.com/skeeto/elfeed.git")
            ("git@github.com:karthink/ffmpeg-dispatch.git" . "ffmpeg")
            ("https://git.code.sf.net/p/matlab-emacs/src" . "matlab-emacs-src")
            ("https://github.com/nico202/ob-julia.git")
            ("https://github.com/tecosaur/screenshot.git" . "emacs-screenshot")
            ("git@github.com:karthink/sicp.git")
            ("https://github.com/chenyanming/wallabag.el.git")
            ;; ("git@github.com:karthink/lazytab.git")
            ;; ("git@github.com:karthink/consult-reftex.git")
            ;; ("git@github.com:karthink/consult-dir.git")
            ;; ("git@github.com:karthink/ink.git")
            ;; ("git@github.com:karthink/popper.git")
            ;; ("git@github.com:karthink/project-x.git")
            ;; ("git@github.com:karthink/wallabag.el.git" . "wallabag")
            ;; ("git@github.com:karthink/peep-dired.git")
            ))
    (dolist (pack my-packages-urls-extra)
      (let* ((packurl (car pack))
             (packdir (or (cdr pack) (file-name-base (car pack))))
             (buf (concat packdir ".out")))
        (if (file-directory-p packdir)
            (let ((default-directory (concat default-directory packdir)))
              (start-process packdir buf "git" "pull" packurl)
              (message (format "Updating %s in %s" packurl packdir)))
          (start-process packdir buf "git" "clone" packurl packdir)
          (message (format "Cloning %s to %s" packurl packdir)))))))

;; Packages I've authored are not on this list any more as I've taken to
;; including them as git submodules. They are commented out above in
;; =my-packages-urls-extra=.

;; * PATHS

;; I avoid defining too many custom helpers, =dir-concat= is an exception.
(use-package emacs
  :config
  (defun dir-concat (dir file)
    "join path DIR with filename FILE correctly"
    (concat (file-name-as-directory dir) file))

  ;; Set directory
  (setq default-directory
        (cond ((equal (system-name) "surface")
               "/cygdrive/c/Users/karth/OneDrive/Documents/")
              ((equal system-type 'nt)
               "/cygdrive/c/Users/karth/OneDrive/Documents/")
              (t "~/")))

  ;; Adds ~/.emacs.d to the load-path
  (push (dir-concat user-emacs-directory "plugins/") load-path)
  (push (dir-concat user-emacs-directory "lisp/") load-path)
  
  ;; cache directory
  (defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by emacs are placed."))

;; "plugins/" contains downloaded packages or plugins I've written. "lisp/" is
;; configuration and glue code.

;; * CORE

;; Optimizations to make Emacs more responsive. These are mostly copied from
;; Doom Emacs.
(require 'setup-core)

;; Trying out [[https://gitlab.com/koral/gcmh][gcmh]] on an experimental basis.
(condition-case-unless-debug nil 
    (use-package gcmh
      :defer 2
      :ensure
      ;; :hook (after-init . gcmh-mode)
      :config
      (setq gcmh-idle-delay 'auto  ; default is 15s
            gcmh-high-cons-threshold (* 32 1024 1024)
            gcmh-verbose nil)
      (gcmh-mode 1))
  (error (setq gc-cons-threshold (* 16 1024 1024))))

;; setup-core is the first of many concerns to be shunted into its own file.
;;;----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-core.org"
;;;----------------------------------------------------------------

;; * DAEMON
;;;################################################################

;; Hack: When starting a server, silently load all the "heavy" libraries and
;; goodies I use. There are more elegant approaches to this issue, such as DOOM
;; Emacs' =:defer-incrementally= =use-package= keyword, but this is good enough.
;; A regular (non-daemon) Emacs session still launches in ~0.3 seconds.
(when (daemonp)
  (add-hook
   'after-init-hook
   (defun my/load-packages-eagerly ()
     (run-at-time 1 nil
                  (lambda () 
                    (let ((after-init-time (current-time)))
                      (dolist (lib '("org" "ob" "ox" "ol" "org-roam"
                                     "org-capture" "org-agenda" "org-fragtog"
                                     "org-gcal" "latex" "reftex" "cdlatex"
                                     "consult" "helpful" "elisp-mode"
                                     "notmuch" "elfeed" "simple"
                                     "expand-region" "embrace"
                                     "ace-window" "avy" "yasnippet"
                                     "magit" "modus-themes" "diff-hl"
                                     "dired" "ibuffer" "pdf-tools"))
                        (with-demoted-errors "Error: %S" (load-library lib)))
                      (load-theme 'doom-rouge t)
                      (when (featurep 'pdf-tools) (pdf-tools-install))
                      (let ((elapsed (float-time (time-subtract (current-time)
                                                                after-init-time))))
                        (message "[Pre-loaded packages in %.3fs]" elapsed))))))))

;;;################################################################
;; * PERSONAL INFO
;;;################################################################
;; Store personal information in a GPG encrypted file. This keeps the config
;; user agnostic. Starting Emacs requires both this file and my GPG keys to be
;; present. If you want to use this configuration wholesale (a terrible idea),
;; you'll need to set the my-* variables appropriately.
(let* ((personal-file (concat user-emacs-directory "lisp/personal.el.gpg"))
       (personal-file-bc (concat personal-file ".elc")))
  (unless (file-exists-p personal-file-bc)
    (epa-file-enable)
    (byte-compile-file personal-file)))
(with-demoted-errors "Error (personal info): %S" (load-library "personal.el.gpg"))
(setq user-full-name my-full-name)
(setq user-mail-address my-email-address)

;;;################################################################
;; * UI
;;;################################################################

;; Miscellaneous UI preferences.
(load-library "setup-ui")

;;;----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-ui.org"
;;;----------------------------------------------------------------

;; * AUTOLOADS

;; A relic of a past era when I would generate autoloads manually.
;; #+begin_src emacs-lisp
;; (require 'setup-autoloads nil t)
;; (require 'plugin-autoloads nil t)
;; #+end_src

;;;################################################################
;; * CUSTOM FILE
;;;################################################################

;; Don't populate the init file with custom-set-variables, create and use a
;; separate file instead.
(use-package cus-edit
  :config
  ;; Get custom-set-variables out of init.el
  (defvar my/custom-file (dir-concat user-emacs-directory "custom.el"))
  (setq custom-file my/custom-file)

  (defun my/cus-edit ()
    (let ((file my/custom-file))
      (unless (file-exists-p file)
        (make-empty-file file))
      (load-file file)))
  :hook (after-init . my/cus-edit))

;;;################################################################
;; * KEYBIND SETUP
;;;################################################################

;; The first of several packages that I no longer use but are too entangled with
;; everything else to remove safely. So it stays in.
;;
;; These are mostly leader based keybindings that make sense to use with
;; evil-mode... which I don't use.
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
    "f" '(:prefix-command space-menu-file
                          :prefix-map space-menu-file-map
                          :wk "files") ;; :wk "file")
    "q" '(:ignore t :wk "quit")
    "b" '(:ignore t)
    "g" '(vc-prefix-map :wk "git/VC")
    "c" '(:ignore t) ;; :wk "code")
    "k" '(kill-this-buffer :wk "Kill buffer")
    "/" '(:prefix-command space-menu-search
                          :prefix-map space-menu-search-map
                          :wk "search")
    "b" '(:prefix-command space-menu-buffer
                          :prefix-map space-menu-buffer-map
                          :wk "buffers")
    "p" '(project-prefix-map :wk "project")
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
    "g" '(revert-buffer         :wk "revert buffer")
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

  ;; (general-def
  ;;   :keymaps 'space-menu-project-map
  ;;   :wk-full-keys nil
  ;;   "f" '(project-find-file :wk "find file in proj")
  ;;   "q" '(project-query-replace-regexp :wk "query replace in proj")
  ;;   "g" '(project-search :wk "grep in proj")
  ;;   "o" '(project-find-regexp :wk "occur in proj"))

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
    :keymaps 'space-menu-file-map
    :wk-full-keys nil
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
    "m" '(describe-mode :wk "describe mode")))

(use-package god-mode
  :disabled
  :init
  (setq which-key--god-mode-support-enabled t)
  (setq god-mode-enable-function-key-translation nil)
  (defun my-god-mode-update-cursor ()
    (setq cursor-type (if (or god-local-mode)
                          'hollow
                        'box)))
  (global-set-key (kbd "<escape>") #'god-local-mode)
  ;; (global-set-key (kbd "S-SPC") #'god-local-mode)
  (add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor))

;;;################################################################
;; * SAVE AND BACKUP
;;;################################################################
;; Put backups elsewhere:
(setq auto-save-interval 2400)
(setq auto-save-timeout 300)
(setq auto-save-list-file-prefix
      (dir-concat user-cache-directory "auto-save-list/.saves-"))
(setq backup-directory-alist
      `(("." . ,(dir-concat user-cache-directory "backup")))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5 ; Old versions to keep
      )

;;;################################################################
;; * MISCELLANEOUS PREFERENCES
;;;################################################################
;; For lazy typists
(fset 'yes-or-no-p 'y-or-n-p)
;; Move the mouse away if the cursor gets close
;; (mouse-avoidance-mode 'animate)

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

(use-package repeat
  :if (version< "28.0" emacs-version)
  :bind ("H-z" . repeat)
  :hook (after-init . my/repeat-mode)
  :config
  (defun my/repeat-mode ()
    (let ((inhibit-message t)
          (message-log-max nil))
      (repeat-mode)))
  
  (use-package which-key
    :after which-key
    :config
    ;; (advice-add 'repeat-post-hook :after
    ;;             (defun my/which-key-repeat ()
    ;;               (when-let ((cmd (or this-command real-this-command))
    ;;                          (keymap (repeat--command-property 'repeat-map)))
    ;;                 (run-at-time
    ;;                  which-key-idle-delay nil
    ;;                  (lambda () 
    ;;                    (which-key--create-buffer-and-show
    ;;                     nil (symbol-value keymap)))))))

    (defun my/which-key-repeat-mode-dispatch ()
      (interactive)
      (setq this-command last-command)
      (when-let (keymap (repeat--command-property 'repeat-map))
        (which-key--create-buffer-and-show
         nil (symbol-value keymap))))
    
    ;; (defun my/which-key-repeat-mode-binding ()
    ;;   (when repeat-mode
    ;;     (when-let* ((rep-map-sym (or repeat-map (repeat--command-property 'repeat-map)))
    ;;                 (keymap (and (symbolp rep-map-sym) (symbol-value rep-map-sym))))
    ;;       (set-transient-map
    ;;        (make-composed-keymap
    ;;         (let ((map (make-sparse-keymap)))
    ;;           (define-key map (kbd "C-h") #'my/which-key-repeat-mode-dispatch)
    ;;           map)
    ;;         keymap)))))
    
    (defun my/which-key-repeat-mode-binding ()
      (when repeat-mode
        (when-let* ((rep-map-sym (or repeat-map (repeat--command-property 'repeat-map)))
                    (keymap (and (symbolp rep-map-sym) (symbol-value rep-map-sym))))
          (set-transient-map
           (let ((map (make-sparse-keymap)))
             (set-keymap-parent map keymap)
             (define-key map (kbd "C-h") #'my/which-key-repeat-mode-dispatch)
             map)))))
    (advice-add 'repeat-post-hook :after #'my/which-key-repeat-mode-binding)))

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; FULLSCREEN
(global-set-key [f11] 'toggle-frame-fullscreen)

;; Byte-compile elisp files immediately after saving them if .elc exists:
(defun auto-byte-recompile ()
  "If the current buffer is in `emacs-lisp-mode' and there
  already exists an `.elc' file corresponding to the current
  buffer file, then recompile the file."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (not ;; (string= user-init-file (buffer-file-name))
              (string-match-p "init\\.el$" (buffer-file-name)))
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-recompile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)
(add-hook 'kill-emacs-hook (lambda () (byte-recompile-file user-init-file)))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(global-prettify-symbols-mode 1)

;; Hyper bindings for emacs. Why use a pinky when you can use a thumb?
(use-package emacs
  :bind-keymap (("H-f" . space-menu-file-map)
                ("H-b" . space-menu-buffer-map)
                ("H-r" . ctl-x-r-map))
  :bind (("M-ESC ESC" . nil)
         ("H-x" . H-x)
         ("H-c" . H-c)
         ("H-z" . repeat)
         ("H-=" . text-scale-increase)
         ("H--" . text-scale-decrease)
         ("H-M--" . shrink-window-if-larger-than-buffer)
         ("H-h" . mark-whole-buffer)
         ("H-M-x" . eval-defun)
         ("H-s" . isearch-forward)
         ("H-r" . isearch-backward)
         ("H-q" . kill-buffer-and-window)
         :map isearch-mode-map
         ("H-s" . isearch-repeat-forward)
         ("H-r" . isearch-repeat-backward)
         ;; :map ctl-x-map
         ;; ("H-s" . save-buffer)
         ;; ("H-e" . eval-last-sexp)
         ;; ("H-c" . save-buffers-kill-terminal)
         ;; ("H-f" . find-file)
         ;; ("H-q" . read-only-mode)
         )
  :config
  (defun hyperify-prefix-key (key)
    (let* ((convert-function
	    (lambda (event)
	      (vector
	       (if (memq 'hyper (event-modifiers event))
		   (event-apply-modifier (event-basic-type event) 'control 26 "C-")
	         event))))
	   (first-key-sequence (vconcat key (funcall convert-function (read-event))))
	   (command (or (let ((minor-cmd (lookup-key (current-minor-mode-maps) first-key-sequence)))
                          (unless (equal minor-cmd 1) minor-cmd))
                        (let ((local-cmd (lookup-key (current-local-map) first-key-sequence)))
                          (unless (equal local-cmd 1) local-cmd))
                        (lookup-key (current-global-map) first-key-sequence))))
      (catch 'finished
        (while t
	  (cond ((commandp command)
	         (call-interactively command)
	         (throw 'finished t))
	        ((keymapp command)
	         (setq command (lookup-key command (funcall convert-function (read-event)))))
	        (t (error "ABORT")))))))

  (defun H-x ()
    (interactive)
    (hyperify-prefix-key [24]))

  (defun H-c ()
    (interactive)
    (hyperify-prefix-key [3])))

;;;######################################################################
;; * INTERFACING WITH THE OS
;;;######################################################################

(if IS-WINDOWS
    (setq shell-file-name "C:/cygwin/cygwin.bat"))

(use-package auth-source-pass
  :init (auth-source-pass-enable))
;; Consult clipboard before primary selection
;; http://www.gnu.org/software/emacs/manual/
;; html_node/emacs/Clipboard.html

(use-package select
  :config
  (setq select-enable-clipboard t))

(use-package comint
  :commands (comint-mode shell-command-at-line)
  :bind
  ("C-!" . shell-command-at-line)

  :general
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
  :disabled
  :load-path "~/.local/share/git/melpa/emacs-piper/"
  :bind ("C-x |" . piper)
  :config
  (defun +piper-start (&optional arg)
    "Start piper. With prefix ARG, start piper on current buffer"
    (interactive "P")
    (if arg (piper) (piper-user-interface))
    ))

(use-package 0x0
  :ensure
  :commands (0x0-upload 0x0-dwim)
  :bind ("C-x U" . 0x0-dwim))

(use-package explain-pause-mode
  :disabled
  :load-path "~/.local/share/git/melpa/explain-pause-mode/")

(use-package vterm
  :ensure t
  :defer)

;;;----------------------------------------------------------------
;; ** SHELL AND ESHELL PREFERENCES
;;;----------------------------------------------------------------
(use-package setup-shells)

;;;######################################################################
;; * LINE NUMBERS
;;;######################################################################
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

;;;################################################################
;; * EDITING
;;;######################################################################
(use-package visual-fill-column-mode
  :disabled
  :commands visual-fill-column-mode
  )

(use-package diff-mode
  :defer
  :general
  (:keymaps 'diff-mode-map
            :states 'motion
            "i" 'ignore
            "f" 'next-error-follow-minor-mode
            "q" 'quit-window)
  :config
  (use-package outline
    :hook (diff-mode . my/outline-mode-diff)
    :config
    (defun my/outline-mode-diff ()
      (setq-local outline-regexp "---\\|\\+\\+\\|@@ ")
      (outline-minor-mode 1))))

(use-package iedit
  :ensure t
  :bind ("C-M-;" . iedit-mode))

(use-package replace
  :defer
  :bind (:map occur-mode-map
              ("C-x C-q" . occur-edit-mode))
  :general
  (:keymaps 'occur-mode-map
            :states '(normal motion)
            "gc" 'next-error-follow-minor-mode
            :states 'motion
            "f" 'next-error-follow-minor-mode))

(require 'better-editing nil t)

(use-package emacs
  :config
  (setq set-mark-command-repeat-pop t)
  (global-set-key (kbd "M-r") ctl-x-r-map)
  (setq undo-limit (* 80 1024 1024))
  :bind
  (("M-z" . zap-to-char-save)
   ("<C-M-backspace>" . backward-kill-sexp)))

(use-package view
  :general
  (:keymaps 'view-mode-map
            :states '(normal motion visual)
            "M-SPC" 'space-menu))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . #'easy-kill)
         ([remap mark-sexp]      . #'easy-mark)
         :map easy-kill-base-map
         ("," . easy-kill-expand))
  :config
  (add-to-list 'easy-kill-alist '(62 page "\n"))
  (add-to-list 'easy-kill-alist '(104 paragraph "\n")))

(use-package goto-chg
  :ensure t
  :bind (("M-g ;" . goto-last-change)
         ("M-i" . goto-last-change)
         ("M-g M-;" . goto-last-change)))

;; * MANAGE STATE
;; ** RECENTF
;; Keep track of recently opened files. Also feeds into the list of recent
;; directories used by consult-dir.
(use-package recentf
  :defer 2
  :config
  (setq recentf-save-file (dir-concat user-cache-directory "recentf")
        recentf-max-saved-items 200)
  (recentf-mode 1))

;; ** SAVEHIST
;; Save history across various prompts
(use-package savehist
  :defer 2
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (dir-concat user-cache-directory "savehist"))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

;; ** DESKTOP
;; Save and resume Emacs sessions.
(use-package desktop
  :hook (kill-emacs . desktop-save-in-desktop-dir)
  :config
  ;; (when (daemonp)
  ;;   (defun my/restore-desktop (frame)
  ;;     "Restores desktop and cancels hook after first frame opens."
  ;;     (with-selected-frame frame
  ;;       (desktop-save-mode 1)
  ;;       (desktop-read)
  ;;       (remove-hook 'after-make-frame-functions 'my/restore-desktop)))
  ;;   (add-hook 'after-make-frame-functions 'my/restore-desktop))
  (setq desktop-auto-save-timeout 300
        desktop-path `(,(dir-concat user-cache-directory "desktop"))
        desktop-dirname (dir-concat user-cache-directory "desktop")
        desktop-base-file-name "desktop"
        desktop-restore-forces-onscreen nil
        desktop-globals-to-clear nil
        desktop-load-locked-desktop t
        desktop-missing-file-warning nil
        desktop-restore-eager 20
        desktop-restore-frames t
        desktop-save 'ask-if-new))

;;;################################################################
;; ** UNDO HISTORY

;; The =undo-fu-session= package saves and restores the undo states of buffers
;; across Emacs sessions.
(use-package undo-fu-session
  :ensure t
  :hook ((prog-mode conf-mode text-mode tex-mode) . undo-fu-session-mode)
  :config
  (setq undo-fu-session-directory
        (dir-concat user-cache-directory "undo-fu-session/")))

;; * BUFFER MANAGEMENT
(load-library "better-buffers")

;;;----------------------------------------------------------------
;; #+INCLUDE: "./lisp/better-buffers.org" :minlevel 2
;;;----------------------------------------------------------------

;; ** IBUFFER
(load-library "setup-ibuffer")

;;;----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-ibuffer.org" :minlevel 3
;;;----------------------------------------------------------------

;; * WINDOW MANAGEMENT
;;;################################################################

;;;----------------------------------------------------------------
;; ** +SHACKLE+
;;;----------------------------------------------------------------

;; Wasamasa's Shackle package simplifies Emacs' rather arcane display-buffer
;; rules so you don't have to tear your hair out understanding how to configure
;; it. Unfortunately I did, see [[*SETUP-WINDOWS][setup-windows]].
(use-package shackle
  :disabled t
  :init (shackle-mode))

;;;----------------------------------------------------------------
;; ** SETUP-WINDOWS
;;;----------------------------------------------------------------

;; Setup-windows defines window rules for displaying various kinds of buffers.
(use-package setup-windows
  :demand t
  :hook ((help-mode . visual-line-mode)
         (Custom-mode . visual-line-mode)
         (helpful-mode . visual-line-mode))
  ;; :bind (;; ("C-x +" . balance-windows-area)
  ;;        ("<f8>" . +make-frame-floating-with-current-buffer)
  ;;        ("C-M-`" . window-toggle-side-windows))
  :bind
  ("<f9>" . +make-frame-floating-with-current-buffer)
  ;; "C-M-`" 'window-toggle-side-windows
  :general
  (:keymaps 'space-menu-window-map
            :wk-full-keys nil
            "w" '(window-toggle-side-windows :wk "toggle side windows")))

(use-package window
  :bind (("H-+" . balance-windows-area)
         ("C-x +" . balance-windows-area)
         ("C-x q" . kill-buffer-and-window)))

;; setup-windows:
;; #+INCLUDE: "./lisp/setup-windows.org" :minlevel 2

;;;----------------------------------------------------------------
;; ** POPUPS
;;;----------------------------------------------------------------

;; Designate buffers to popup status and toggle or cycle through them
(use-package popper
  :load-path "plugins/popper/"
  :after (setup-windows setup-project)
  :commands popper-mode
  :bind (("C-`" . popper-toggle-latest)
         ("C-M-`" . popper-cycle)
         ("H-`" . popper-toggle-latest)
         ("H-M-`" . popper-cycle)
         ("H-6" . popper-toggle-type)
         ("H-M-k" . popper-kill-latest-popup))
  :init
  (setq popper-group-function
        (defun my/popper-group-by-heuristic ()
          "Group popups according to heuristic rules suitable for
          my usage."
          (let ((dd (abbreviate-file-name default-directory)))
            (cond
             ((string-match-p "\\(?:~/\\.config/\\|~/dotfiles/\\)" dd)
              'config)
             ((or (string-match-p "local/share/git" dd)
                  (string-match-p "plugins/" dd))
              'projects)
             ((string-match-p "\\(?:KarthikBa\\|research/\\)" dd)
              'research)
             ((string-match-p "karthinks" dd) 'website)
             ((locate-dominating-file dd "research") 'documents)
             ((locate-dominating-file dd "init.el") 'emacs)
             (t (popper-group-by-project))))))
  (setq popper-mode-line nil
        popper-reference-buffers
        (append my/help-modes-list
                my/repl-modes-list
                my/occur-grep-modes-list
                ;; my/man-modes-list
                '(Custom-mode
                  (compilation-mode . hide)
                  messages-buffer-mode)
                '(("^\\*Warnings\\*$" . hide)
                  ("^\\*Compile-Log\\*$" . hide)
                  "^\\*Matlab Help\\*"
                  ;; "^\\*Messages\\*$"
                  "^\\*Backtrace\\*"
                  "^\\*evil-registers\\*"
                  "^\\*Apropos"
                  "^Calc:"
                  "^\\*eldoc\\*"
                  "^\\*TeX errors\\*"
                  "^\\*ielm\\*"
                  "^\\*TeX Help\\*"
                  "\\*Shell Command Output\\*"
                  ("\\*Async Shell Command\\*" . hide)
                  "\\*Completions\\*"
                  ;; "\\*scratch\\*"
                  "[Oo]utput\\*")))

  (use-package popper-echo
    :config
    (defun popper-message-shorten (name)
      (cond
       ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
        (concat (match-string 1 name)
                "(H)"))
       ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
        (concat (match-string 1 name)
                "(H)"))
       ((string-match "^\\*eshell:? ?\\(.*\\)\\*$" name)
        (concat (match-string 1 name)
                (if (string-empty-p (match-string 1 name)) "shell(E)" "(E)")))
       ((string-match "^\\*\\(.*?\\)\\(?:Output\\|Command\\)\\*$" name)
        (concat (match-string 1 name)
                "(O)"))
       ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
        (concat (match-string 1 name)
                "(L)"))
       ((string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
        (concat (match-string 1 name)
                "(C)"))
       (t name)))
    (setq popper-echo-transform-function #'popper-message-shorten)
    (setq popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
          popper-echo-dispatch-actions t)
    (advice-add 'popper-echo :around
                (defun my/popper-echo-no-which-key (orig-fn)
                  (let ((which-key-show-transient-maps nil))
                    (funcall orig-fn))))
    (popper-echo-mode +1))

  :config
  (setq popper-display-control 'user)
  (defun my/popup-raise-popup ()
    "Choose a popup-window to raise as a regular window"
    (interactive)
    (popper-raise-popup
     (completing-read "Raise popup: "
                      (mapcar (lambda (win-and-buf) (buffer-name (cdr win-and-buf)))
                              (cl-mapcan (lambda (group) )
                                         (append popper-open-popup-alist
                                                popper-buried-popup-alist)))
                      nil t)))

  (defun my/popup-lower-to-popup ()
    "Choose a regular window to make a popup"
    (interactive)
    (let ((window-list (cl-set-difference
                        (window-list)
                        (mapcar 'car popper-open-popup-alist))))
      (if (< (length window-list) 2)
          (message "Only one main window!")
        (popper-lower-to-popup
         (get-buffer
          (completing-read "Lower to popup: "
                           (mapcar (lambda (win) (buffer-name (window-buffer win)))
                                   window-list)
                           nil t))))))
  :general
  (:states 'motion
           "C-w ^" '(popper-raise-popup :wk "raise popup")
           "C-w _" '(popper-lower-to-popup :wk "lower to popup"))
  (:keymaps 'space-menu-window-map
            "^" '(my/popup-raise-popup :wk "raise popup")
            "_" '(my/popup-lower-to-popup :wk "lower to popup")))

;;----------------------------------------------------------------
;; ** WINUM
;;----------------------------------------------------------------

;; Add window numbers and use them to switch windows
(use-package winum
  :ensure
  :init
  (defun my/winum-select (num)
    (lambda (&optional arg) (interactive "P")
      (if arg
          (winum-select-window-by-number (- 0 num))
        (if (equal num (winum-get-number))
            (winum-select-window-by-number (winum-get-number (get-mru-window t)))
          (winum-select-window-by-number num)))))

  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-M-0") 'winum-select-window-0-or-10)
          (dolist (num '(1 2 3 4 5 6 7 8 9) nil)
            (define-key map (kbd (concat "C-M-" (int-to-string num)))
              (my/winum-select num)))
          map))

  ;; If evil-mode is enabled further mode-line customization is needed before
  ;; enabling winum:
  (unless (bound-and-true-p evil-mode)
    (winum-mode 1)))

;;----------------------------------------------------------------
;; ** +WINNER+
;;----------------------------------------------------------------

;; Winner mode is disabled in favor of =tab-bar-history-mode=, which does the
;; same but with a separate window configuration history for each tab. This is
;; usually what I want.
(use-package winner
  :disabled
  :commands winner-undo
  :bind (("C-c <left>" . winner-undo)
         ("C-x C-/" . winner-undo)
         ("H-/" . winner-undo)
         ("s-u" . winner-undo))
  :general
  (:keymaps 'space-menu-window-map
            :wk-full-keys nil
            "u" 'winner-undo
            "r" 'winner-redo)
  :config
  (winner-mode +1))

;;----------------------------------------------------------------
;; ** Ace-window
;;----------------------------------------------------------------

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window)
   ("H-o"   . ace-window)
   ("M-o" . other-window)
   ("M-O" . my/other-window-prev))
  :general
  (:keymaps 'space-menu-map
            "`" 'ace-window)
  ;; :custom-face
  ;; (aw-leading-char-face ((t (:height 2.5 :weight normal))))
  :config
  (setq aw-dispatch-always t
        aw-scope 'global
        aw-background nil
        aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?p))
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
          (?? aw-show-dispatch-help)))
  (defun my/other-window-prev (&optional arg all-frames)
    (interactive "p")
    (other-window (if arg (- arg) -1) all-frames)))

(use-package emacs
  :config
  (defun my/enlarge-window-horizontally (&optional repeat)
    "Enlarge window horizontally by 8% of the frame width."
    (interactive "p")
    (enlarge-window-horizontally (* (or repeat 1)
                                    (floor (frame-width) 20))))
  (defun my/shrink-window-horizontally (&optional repeat)
    "Enlarge window horizontally by 8% of the frame width."
    (interactive "p")
    (shrink-window-horizontally (* (or repeat 1)
                                   (floor (frame-width) 20))))
  (defun my/shrink-window (&optional repeat)
    "Enlarge window horizontally by 8% of the frame height."
    (interactive "p")
    (shrink-window (* (or repeat 1)
                      (floor (frame-height) 20))))
  (defun my/enlarge-window (&optional repeat)
    "Enlarge window horizontally by 8% of the frame height."
    (interactive "p")
    (enlarge-window (* (or repeat 1)
                       (floor (frame-height) 20))))
  :bind
  (("<C-S-right>" . my/enlarge-window-horizontally)
   ("<C-S-left>"  . my/shrink-window-horizontally)
   ("<C-S-up>"    . my/enlarge-window)
   ("<C-S-down>"  . my/shrink-window)))

;;----------------------------------------------------------------
;; ** Windmove
;;----------------------------------------------------------------

(use-package windmove
  :bind
  (("H-<right>" . windmove-swap-states-right)
   ("H-<down>" . windmove-swap-states-down)
   ("H-<up>" . windmove-swap-states-up)
   ("H-<left>" . windmove-swap-states-left))
  :config
  (use-package emacs-i3))

;;----------------------------------------------------------------
;; ** Transpose-frame
;;----------------------------------------------------------------

(use-package transpose-frame
  :ensure t
  :bind (("H-\\" . rotate-frame-anticlockwise)
         :map ctl-x-4-map
         ("|" . flop-frame)
         ("_" . flip-frame)
         ("\\" . rotate-frame-anticlockwise)))

;;----------------------------------------------------------------
;; ** Auto-revert
;;----------------------------------------------------------------

(use-package autorevert
  :hook ((prog-mode
          text-mode
          tex-mode
          org-mode
          conf-mode) . auto-revert-mode))

;;----------------------------------------------------------------
;; ** Re-Builder
;;----------------------------------------------------------------
(use-package re-builder
  :bind (("C-M-5" . re-builder)
         ("C-M-%" . re-builder)
         :map reb-mode-map
         ("C-c C-k" . reb-quit)
         ("RET" . reb-replace-regexp)
         :map reb-lisp-mode-map
         ("RET" . reb-replace-regexp))
  :config
  ;; reb-fix modifies reb-update-overlays to restrict matches to region
  (use-package reb-fix)
  (defvar my/re-builder-positions nil
    "Store point and region bounds before calling re-builder")
  (advice-add 're-builder
              :before
              (defun my/re-builder-save-state (&rest _)
                "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
                (setq my/re-builder-positions
                      (cons (point)
                            (when (region-active-p)
                              (list (region-beginning)
                                    (region-end)))))))
  (defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
  (interactive "P")
  (reb-update-regexp)
  (let* ((re (reb-target-binding reb-regexp))
         (replacement (query-replace-read-to
                       re 
                       (concat "Query replace"
		               (if current-prefix-arg
		                   (if (eq current-prefix-arg '-) " backward" " word")
		                 "")
		               " regexp"
		               (if (with-selected-window reb-target-window
                                     (region-active-p)) " in region" ""))
                       t))
         (pnt (car my/re-builder-positions))
         (beg (cadr my/re-builder-positions))
         (end (caddr my/re-builder-positions)))
    (with-selected-window reb-target-window
      (goto-char pnt)
      (setq my/re-builder-positions nil)
      (reb-quit)
      (query-replace-regexp re replacement delimited beg end)))))

;;;----------------------------------------------------------------
;; #+INCLUDE: "./lisp/reb-fix.org" 
;;;----------------------------------------------------------------

;; * UTILITY
;;;################################################################
;; Count words, print ASCII table, etc
(require 'utilities nil t)

(use-package async
  :hook (package-menu-mode . my/async-bytecomp-ensure)
  :config
  (defun my/async-bytecomp-ensure ()
    (async-bytecomp-package-mode 1)))

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

(use-package screenshot
  :load-path "~/.local/share/git/emacs-screenshot"
  :commands screenshot
  :requires posframe)

;; Colorize color names and parens in buffers
(use-package rainbow-mode
  :commands rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :ensure t)

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
              ("TAB" . outline-cycle)
              ("<tab>" . outline-cycle)
              ("C-c C-n" . 'outline-next-visible-heading)
              ("C-c C-p" . 'outline-previous-visible-heading))
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

  (defvar outline-cycle-emulate-tab nil
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
           ;; Not at a headline: Do whatever this key would do otherwise.
           ;; (outline-back-to-heading)
           (let ((normal-binding (let ((outline-minor-mode nil))
                                    (key-binding (this-command-keys-vector)))))
             (if normal-binding
                 (progn
                   (setq this-command normal-binding)
                   (call-interactively normal-binding))
               (indent-according-to-mode)))))))

(use-package outline
  :when (version< "28.0" emacs-version)
  :defer
  :bind (:map outline-navigation-repeat-map
              ("TAB" . outline-cycle)
              ("<tab>" . outline-cycle)
              ("C-n" . nil)
              ("C-p" . nil)
              ("C-f" . nil)
              ("C-b" . nil))
  :config
  (put 'outline-cycle 'repeat-map 'outline-navigation-repeat-map))

(use-package imenu
  :hook (imenu-after-jump . my/imenu-show-entry)
  :bind ("M-s i" . imenu)
  :config
  (setq imenu-use-markers t
        imenu-auto-rescan t
        imenu-max-item-length 100
        imenu-use-popup-menu nil
        imenu-eager-completion-buffer t
        imenu-space-replacement " "
        imenu-level-separator "/")

  (declare-function org-at-heading-p "org")
  (declare-function org-show-entry "org")
  (declare-function org-reveal "org")
  (declare-function outline-show-entry "outline")

  (defun my/imenu-show-entry ()
    "Reveal index at point after successful `imenu' execution.
To be used with `imenu-after-jump-hook' or equivalent."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry)
      (org-reveal t))
     ((bound-and-true-p prot-outline-minor-mode)
      (outline-show-entry)))))

(use-package flimenu
  :disabled ; Handled by consult-imenu instead
  :after imenu
  :config
  (flimenu-global-mode 1))

(use-package imenu-list
  :disabled
  :after imenu
  :defer
  :bind ("M-s M-i" . imenu-list))

(use-package scratch
  :ensure
  :config
  (defun my/scratch-buffer-setup ()
  "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
  (let* ((mode major-mode)
         (string (format "Scratch buffer for: %s\n\n" mode))
         (region (with-current-buffer (current-buffer)
                     (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end)))
                     ""))
         (text (concat string region)))
    (when scratch-buffer
      (save-excursion
        (insert text)
        (goto-char (point-min))
        (comment-region (point-at-bol) (point-at-eol)))
      (forward-line 2))
    (rename-buffer (format "*Scratch for %s*" mode) t)))
  :hook (scratch-create-buffer . my/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

(defun delete-window-if-not-single ()
  "Delete window if not the only one."
  (when (not (one-window-p))
    (delete-window)))

;; (defun multi-replace-regexp-in-string (replacements-list string &optional rest)
;;   "Replace multiple regexps in a string, in the order of listing.
;; `REPLACEMENTS-LIST' is an alist, each cons cell of which is of
;; the form (regexp . replacement)."
;;   (if (null replacements-list)
;;       string
;;     (let ((regex (caar replacements-list))
;;           (replacement (cdar replacements-list)))
;;       (multi-replace-regexp-in-string (cdr replacements-list)
;;                                       (replace-regexp-in-string regex replacement
;;                                                                 string rest)))))

;;;################################################################
;; * COMPILATION
;;;################################################################

;; compile!
(use-package compile
  :defer t
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (global-set-key [(f9)] 'compile)
  (global-set-key [(f10)] 'recompile)

  (defun my/apply-ansi-color-to-compilation-buffer-h ()
    "Applies ansi codes to the compilation buffers. Meant for
  `compilation-filter-hook'."
    (with-silent-modifications
      (ansi-color-apply-on-region compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook #'my/apply-ansi-color-to-compilation-buffer-h)
  ;; (add-hook 'compilation-finish-functions
  ;;           (lambda (buf str)

  ;;             (if (or
  ;;                  (string-match "exited abnormally" str)
  ;;                  (string-match "matches found" str))
  ;;                 ;;there were errors
  ;;                 (message "Press M-g n/p to visit")

  ;;               ;;no errors, make the compilation window go away in 0.5 seconds
  ;;               (save-excursion
  ;;                 (run-at-time 1.0 nil 'bury-buffer buf))
  ;;               (message "NO COMPILATION ERRORS!"))))
  )

;; (add-hook 'compilation-mode-hook
;;           (lambda (&optional args)
;;             (run-at-time 3 nil
;;                          (lambda () (delete-windows-on (get-buffer "*Compile-Log*"))))))

;;;################################################################
;; * LANGUAGE MODES
;;;################################################################

;;;----------------------------------------------------------------
;; ** LSP SUPPORT
;;;----------------------------------------------------------------
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


;;;----------------------------------------------------------------
;; ** EGLOT - LSP
;;;----------------------------------------------------------------
(use-package eglot
  ;; :disabled t
  :ensure t
  :commands eglot
  :bind (:map eglot-mode-map
              ("C-h ." . eldoc))
  :config
  (setq eglot-put-doc-in-help-buffer nil)
  (add-to-list 'eglot-server-programs
               '(matlab-mode . ("~/.local/share/git/matlab-langserver/matlab-langserver.sh" ""))))

;;;----------------------------------------------------------------
;; ** EMACS-LISP
;;;----------------------------------------------------------------
(use-package pp
  :bind ([remap eval-last-sexp] . eval-sexp-maybe-pp)
  :config
  (defun eval-sexp-maybe-pp (&optional arg)
    (interactive "P")
    (if arg
        (let ((current-prefix-arg '4))
          (call-interactively #'pp-eval-last-sexp))
      (call-interactively #'eval-last-sexp))))

;;;----------------------------------------------------------------
;; ** AUCTEX-MODE & ADDITIONS
;;;----------------------------------------------------------------
(use-package latex
  :defer 5
  :after tex
  :ensure auctex
  :hook ((LaTeX-mode . electric-pair-mode)
         (LaTeX-mode . my/latex-with-outline))
  :mode ("\\.tex\\'" . latex-mode)
  ;; :init (add-hook 'LaTeX-mode-hook
  ;;                 (lambda ()  (interactive)
  ;;                   (outline-minor-mode)
  ;;                   (setq-local page-delimiter "\\\\section\\**{")
  ;;                   (setq-local outline-regexp "\\\\\\(sub\\)*section\\**{")
  ;;                   (setq-local prettify-symbols-alist tex--prettify-symbols-alist)
  ;;                   (outline-hide-sublevels 3)
  ;;                   ))
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
  :bind
  (:map LaTeX-mode-map
        ("M-RET" . LaTeX-insert-item))
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

    "p"  '(:ignore t :wk "Preview")
    "pp" '(preview-at-point           :wk "Preview at point")
    "pb" '(preview-buffer             :wk "Preview buffer")
    "pr" '(preview-region             :wk "Preview region")
    "pe" '(preview-environment        :wk "Preview environment")
    "pd" '(preview-document           :wk "Preview document")
    "ps" '(preview-section            :wk "Preview section")
    "pw" '(preview-copy-region-as-mml :wk "Copy MathML")
    "pc" '(preview-clearout           :wk "Clearout")
    "pS" '(preview-clearout-section   :wk "Clearout section")
    "pB" '(preview-clearout-buffer    :wk "Clearout buffer")
    "pP" '(preview-clearout-at-point  :wk "Clearout at point")
    "pD" '(preview-clearout-document  :wk "Clearout document")
    ;; "pC" '(preview-clearout-buffer :wk "!Clearout in buffer")

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
  ;; (add-to-list 'Info-directory-list "/usr/share/texmf-dist/tex/texinfo/")
  (defun my/latex-with-outline ()
    (add-to-list 'minor-mode-overriding-map-alist
                 `(outline-minor-mode . ,outline-minor-mode-map))
    (outline-minor-mode 1))
  
  (use-package embrace
      :bind (:map TeX-mode-map
             ("M-s a" . embrace-add)
             ("M-s c" . embrace-change)
             ("M-s d" . embrace-delete)))

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
     TeX-PDF-mode nil
     TeX-error-overview-open-after-TeX-run nil)
    (setq LaTeX-command "latex")
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
                   (output-html "xdg-open")))))
    (TeX-fold-mode 1))
  
  ;; Monkey patching: Stop this from marking to the end of the line at the end
  ;; of the env.
  (defun LaTeX-mark-environment (&optional count)
    "Set mark to end of current environment and point to the matching begin.
If prefix argument COUNT is given, mark the respective number of
enclosing environments.  The command will not work properly if
there are unbalanced begin-end pairs in comments and verbatim
environments."
    (interactive "p")
    (setq count (if count (abs count) 1))
    (let ((cur (point)) beg end)
      ;; Only change point and mark after beginning and end were found.
      ;; Point should not end up in the middle of nowhere if the search fails.
      (save-excursion
        (dotimes (_ count) (LaTeX-find-matching-end))
        (setq end (point))
        (goto-char cur)
        (dotimes (_ count) (LaTeX-find-matching-begin))
        (setq beg (point)))
      (push-mark end)
      (goto-char beg)
      (TeX-activate-region))))

(use-package latex
  :defer
  :if (version<= "28.0" emacs-version)
  :config
  (defvar my/TeX-error-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'TeX-next-error)
      (define-key map "p" 'TeX-previous-error)
      map))
  (put 'TeX-next-error 'repeat-map 'my/TeX-error-map)
  (put 'TeX-previous-error 'repeat-map 'my/TeX-error-map))

(use-package tex-fold
  :after latex
  :defer
  :config
  (setq ;; TeX-fold-folded-face '((t (:height 1.0 :foreground "SlateBlue1")))
        TeX-fold-auto t
        TeX-fold-type-list '(env macro comment)))

(use-package latex-extra
  :disabled
  :after latex
  ;; :ensure
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
  :hook (LaTeX-mode . my/preview-scale-larger)
  :config
  (define-key LaTeX-mode-map (kbd "C-c C-x") preview-map)
  (defun my/preview-scale-larger ()
    "Increase the size of `preview-latex' images"
    (setq preview-scale-function 
          (lambda nil (* 1.25 (funcall (preview-scale-from-face)))))))

(use-package reftex
  :after latex
  :defer 2
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :config
  (setq reftex-default-bibliography '("~/Documents/research/control_systems.bib"))
  (setq reftex-insert-label-flags '("sf" "sfte"))
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-use-multiple-selection-buffers t))

(use-package company-reftex
  :disabled
  :after (reftex company)
  :hook ((latex-mode LaTeX-mode) . my/company-reftex-completions)
  :config
  (defun my/company-reftex-completions ()
    "Add company-reftex based completions to company-backends in
    latex/org buffers."
    (make-variable-buffer-local 'company-backends)
    (add-to-list 'company-backends '(company-reftex-labels company-reftex-citations))))

(use-package consult-reftex
  :load-path "plugins/consult-reftex/"
  :after (reftex consult embark)
  :bind (:map reftex-mode-map
         ;; ("C-c )"   . consult-reftex-insert-reference)
         ("C-c M-." . consult-reftex-goto-label))
  :config (setq consult-reftex-preview-function
                #'consult-reftex-make-window-preview))

;; (setq-default TeX-master nil)
(use-package cdlatex
  :after latex
  :ensure t
  ;; :commands turn-on-cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map ("[" . nil) ("(" . nil) ("{" . nil)
              ("<tab>" . cdlatex-tab))
  :config
  (progn
    (setq cdlatex-command-alist
          '(("vc" "Insert \\vect{}" "\\vect{?}"
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
                                      (?v ("\\vee" "\\forall"))
                                      (?^ ("\\uparrow" "\\Updownarrow" "\\updownarrow"))))
    (setq cdlatex-math-modify-alist '((?b "\\mathbb" "\\textbf" t nil nil)
                                      (?B "\\mathbf" "\\textbf" t nil nil)
                                      (?t "\\text" nil t nil nil)))
    (setq cdlatex-paired-parens "$[{(")))

;; Make cdlatex play nice inside org tables
(use-package lazytab
  :load-path "plugins/lazytab/";; 
  :after cdlatex
  :hook (cdlatex-tab . lazytab-cdlatex-or-orgtbl-next-field)
  :bind (:map orgtbl-mode-map
              ("<tab>" . lazytab-org-table-next-field-maybe)
              ("TAB" . lazytab-org-table-next-field-maybe))
  :init
  (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
                                       "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                       "\\begin{bmatrix} ? \\end{bmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                       "\\begin{pmatrix} ? \\end{pmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
                                        "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                                       lazytab-position-cursor-and-edit
                                       nil t nil)))

(use-package inkscape-figures
  :disabled
  :after latex
  :bind (:map LaTeX-mode-map
            ("C-c i" . +inkscape-figures-create-at-point-latex)
            ("C-c e" . +inkscape-figures-edit)))

(use-package ink
  :load-path "plugins/ink/"
  :after latex
  :commands (ink-make-figure ink-edit-figure))

;; *** BIBTEX
(use-package citar
  :ensure
  :after (latex reftex)
  :bind (:map LaTeX-mode-map
         ("C-c [" . citar-insert-citation)
         :map reftex-mode-map
         ("C-c [" . citar-insert-citation))
  :config
  (setq citar-bibliography '("~/Documents/research/control_systems.bib"))
  
  (use-package cdlatex
    :config
    (defun my/cdlatex-bibtex-action ()
      "Call `citar-insert-citation' interactively."
      (call-interactively 'citar-insert-citation))
    (setf (alist-get "cite" cdlatex-command-alist nil nil 'equal)
          '("Make a citation interactively"
            "" my/cdlatex-bibtex-action nil t nil))
    (setf (alist-get "cite{" cdlatex-command-alist nil nil 'equal)
          '("Make a citation interactively"
            "cite{" my/cdlatex-bibtex-action nil t nil))))

;; *** PDFs
(use-package pdf-tools
  :commands pdf-tools-install
  :ensure)

;;;----------------------------------------------------------------
;; ** MATLAB
;;;----------------------------------------------------------------
(use-package matlab
  :load-path "~/.local/share/git/matlab-emacs-src/"
  :commands (matlab-shell matlab-mode)
  :functions my/matlab-shell-help-at-point
  ;; :ensure matlab-mode
  ;; :commands (matlab-mode matlab-shell matlab-shell-run-block)
  :mode ("\\.m\\'" . matlab-mode)
  :hook ((matlab-mode . company-mode-on)
         (matlab-mode . (lambda ()
                          (setq-local buffer-file-coding-system 'us-ascii)
                          (outline-minor-mode)
                          (setq-local page-delimiter "%%+")
                          (setq-local outline-regexp "^\\s-*%%+")
                          ;; (outline-hide-sublevels 3)
                          ;; (when (require 'matlab-xref nil t)
                          ;;   (make-local-variable 'xref-backend-functions)
                          ;;   (add-hook 'xref-backend-functions #'matlab-shell-xref-activate))
                          ))
         (org-mode . (lambda ()
                       (when (require 'matlab-xref nil t)
                         (add-hook 'xref-backend-functions #'matlab-shell-xref-activate 10 t)))))
  :bind (:map matlab-mode-map
              ("M-j" . nil)
              ("C-c C-n" . 'outline-next-heading)
              ("C-c C-p" . 'outline-previous-heading)
              ("C-c C-b" . 'matlab-shell-run-block)
              ("C-h ." . 'my/matlab-shell-help-at-point)
              ("M-s" . nil)
              ("C-c C-z" . 'matlab-show-matlab-shell-buffer))
  :config
  ;; (load-library "matlab-load")
  ;; (matlab-cedet-setup)
  ;; (semantic-mode 1)
  ;; (global-semantic-stickyfunc-mode 1)
  ;; (global-semantic-decoration-mode 1)
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

  ;; :config
  ;; (setq matlab-shell-command "matlab")
  ;; (add-to-list 'matlab-shell-command-switches "-nosplash")
  (with-demoted-errors "Error loading Matlab autoloads: %s"
    (load-library "matlab-autoloads")
    (load-library "matlab-shell")
    (load-library "mlint"))
  (setq matlab-shell-debug-tooltips-p t)
  (setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))
  ;; (setq matlab-shell-echoes nil)
  (setq matlab-shell-run-region-function 'matlab-shell-region->script)
  ;; (setq matlab-shell-run-region-function 'matlab-shell-region->internal)
  (add-hook 'matlab-shell-mode-hook (lambda () (interactive)
                                      (define-key matlab-shell-mode-map (kbd "C-<tab>") nil)))

  ;; ;;;###autoload
  ;; (defun +matlab-shell-no-select-a (&rest _args)
  ;;   "Switch back to matlab file buffer after evaluating region"
  ;;   (other-window -1))
  ;; (advice-add 'matlab-shell-run-region :after #'+matlab-shell-no-select-a)

  (defun matlab-select-block ()
    (save-excursion
      (let ((block-beg (search-backward-regexp "^%%" nil t))
            (block-end (search-forward-regexp "^%%" nil t 2)))
        (cons (or block-beg (point-min)) (if block-end
                                             (- block-end 2)
                                           (point-max))))))

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

  ;; These are obviated by outline-next-heading and co:
  ;;
  ;; (defun matlab-forward-section ()
  ;;   "Move forward section in matlab mode"
  ;;   (interactive)
  ;;   (beginning-of-line 2)
  ;;   (re-search-forward "^\\s-*%%" nil t)
  ;;   (match-end 0))

  ;; (defun matlab-backward-section ()
  ;;   "Move forward section in matlab mode"
  ;;   (interactive)
  ;;   (re-search-backward "^\\s-*%%" nil t)
  ;;   (match-beginning 0))

  )

;; Company-specific setup for Matlab-mode
(use-package matlab
  :load-path "~/.local/share/git/matlab-emacs-src/"
  :init
  (use-package company
    :if (featurep 'company)
    :hook (matlab-mode . my/matlab-company-settings)
    :config
    ;; (add-to-list 'company-backends 'company-matlab 'company-semantic)
    ;; (add-to-list 'company-backends 'company-matlab-shell)
    
    (defun my/matlab-company-settings ()
      ;; (unless (featurep 'company-matlab)
      ;;   (require 'company-matlab))
      (make-local-variable 'company-backends)
      (setq-local company-backends '((company-files company-capf)))
      (company-mode-on))))

;; Some helpers for =matlab-shell=.
;; - Matlab-shell's window focus behavior is annoying.
;; - A help-at-point function
;; - Company customizations

(use-package matlab-shell
  :load-path "~/.local/share/git/matlab-emacs-src/"
  :defer
  :after matlab
  :hook ((matlab-shell-mode . my/matlab-shell-company-settings)
         (matlab-shell-mode . (lambda ()
                                (buffer-disable-undo)
                                (setq comint-process-echoes t)
                                (define-key matlab-shell-mode-map (kbd "C-h .")
                                  'my/matlab-shell-help-at-point))))
  :config
  (defun my/matlab-shell-company-settings ()
    (make-local-variable 'company-backends)
    (setq-local company-idle-delay 0.3)
    (company-mode-on))
  
  (defun my/matlab-shell-help-at-point (&optional arg)
    (interactive "P")
    (let ((fcn (matlab-read-word-at-point)))
      (if (and fcn (not (equal fcn "")))
          (matlab-shell-describe-command fcn))))

  (advice-add 'matlab-shell-run-region :around #'my-matlab-shell-no-display)
  (defun my-matlab-shell-no-display (orig-fn beg end &optional noshow)
    "Do not display the matlab-shell buffer after sending commands."
    (interactive "r")
    (cl-letf ((display-buffer-alist nil)
              ((symbol-function 'display-buffer-reuse-window) #'display-buffer-no-window)
              ((symbol-function 'display-buffer-at-bottom) #'display-buffer-no-window))
      (save-window-excursion (funcall orig-fn beg end noshow)))))

;;;----------------------------------------------------------------
;; ** PYTHON-MODE
;;;----------------------------------------------------------------

(use-package pyvenv
  :disabled t
  :ensure t
  :config
  (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python))

(use-package elpy
  :disabled
  ;; :ensure t
  :commands elpy
  ;; :init
  ;; (setq python-shell-interpreter "jupyter"
  ;;       python-shell-interpreter-args "console --simple-prompt"
  ;;       python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;;              "jupyter")
  ;; (advice-add 'python-mode :before 'elpy-enable)
  :config
  ;; (add-hook
  ;;  'python-mode-hook
  ;;  (lambda ()
  ;;    (mapc (lambda (pair) (push pair prettify-symbols-alist))
  ;;          '(;; Syntax
  ;;            ("def" .      #x2131)
  ;;            ("not" .      #x2757)
  ;;            ("in" .       #x2208)
  ;;            ("not in" .   #x2209)
  ;;            ("return" .   #x27fc)
  ;;            ("yield" .    #x27fb)
  ;;            ("for" .      #x2200)
  ;;            ;; Base Types
  ;;            ("int" .      #x2124)
  ;;            ("float" .    #x211d)
  ;;            ("str" .      #x1d54a)
  ;;            ("True" .     #x1d54b)
  ;;            ("False" .    #x1d53d)
  ;;            ;; Mypy
  ;;            ("Dict" .     #x1d507)
  ;;            ("List" .     #x2112)
  ;;            ("Tuple" .    #x2a02)
  ;;            ("Set" .      #x2126)
  ;;            ("Iterable" . #x1d50a)
  ;;            ("Any" .      #x2754)
  ;;            ("Union" .    #x22c3)))))
  )

(use-package jupyter 
  :defer
  :ensure t)

(use-package conda
  :commands conda-env-activate
  :ensure t
  :config
  (setq conda-anaconda-home "/opt/miniconda3/")
  (setq conda-env-home-directory (expand-file-name "~/.conda/"))
  ;; (setq conda-env-subdirectory "envs")
  ;; (unless (getenv "CONDA_DEFAULT_ENV")
  ;;   (conda-env-activate "base"))
  ;; if you want interactive shell support, include:
  ;; (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  ;; (conda-env-autoactivate-mode t)
  ;; if you want to automatically activate a conda environment on the opening of a file:
  ;; (add-to-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
  ;;                                      (conda-env-activate-for-buffer))))
  )

;;;----------------------------------------------------------------
;; ** GEISER
;;;----------------------------------------------------------------
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
  ;; (setq geiser-mit-binary "mechanics")
  (setq geiser-mit-binary "mit-scheme"))

;;;----------------------------------------------------------------
;; ** EVAL-IN-REPL
;;;----------------------------------------------------------------
(use-package eval-in-repl
  :disabled
  :ensure t
  :init
  ;; (require 'eval-in-repl-geiser)
  (add-hook 'geiser-mode-hook
            '(lambda ()
               (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser))))

;;;----------------------------------------------------------------
;; ** SCHEME - SICM
;;;----------------------------------------------------------------

;; Make sure mit-scheme (from repos) and scmutils (from internet + sudo ./install.sh)are installed
;;;###autoload
;; (defun mechanics ()
;;   "Run mit-scheme with SCMUTILS loaded, to work with (Structure
;; and Interpretation of Classical Mechanics) - The book."
;;   (interactive)
;;   (setenv "MITSCHEME_BAND" "mechanics.com")
;;   (setenv "MITSCHEME_HEAP_SIZE" "100000")
;;   (run-scheme
;;    "/usr/bin/mit-scheme --library /opt/mit-scheme/lib/mit-scheme-x86-64/"))

(use-package geiser
  :commands mechanics
  :config
  (defun mechanics ()
    "Run mit-scheme with SCMUTILS loaded, to work with (Structure
and Interpretation of Classical Mechanics) - The book."
    (interactive)
    (setenv "MITSCHEME_BAND" "mechanics.com")
    (setenv "MITSCHEME_HEAP_SIZE" "100000")
    (let ((geiser-repl-skip-version-check-p t))
      (run-geiser 'mit))))


;;;################################################################
;; ** JULIA
(use-package julia-mode
  :ensure t
  :bind (:map julia-mode-map
              ("`" . my/julia-latexsub-or-indent))
  :config
  (defun my/julia-latexsub-or-indent ()
    (interactive)
    (require 'cdlatex nil t)
    (cl-letf (((symbol-function 'texmathp)
               (lambda () t)))
      (cdlatex-math-symbol)
      (julia-latexsub-or-indent))))

(use-package julia-repl
  :ensure t
  :commands julia-repl-mode
  :config
  (julia-repl-set-terminal-backend 'vterm))

(use-package eglot-jl
  :ensure t
  :commands eglot-jl-init
  :config
  (cl-defmethod project-root ((project (head julia)))
    (cdr project))
  ;; Workaround until LanguageServer.jl is fixed
  (setq eglot-jl-language-server-project
        (dir-concat user-cache-directory "eglot-jl-project")))
;; ** ESS
;; Need this for ob-julia
(use-package ess-julia
  :ensure ess
  :after ob-julia
  ;; :bind (:map ess-julia-mode-map
  ;;        ("`" . my/ess-julia-cdlatex-symbol)
  ;;        :map inferior-ess-julia-mode-map
  ;;        ("`" . my/ess-julia-cdlatex-symbol))
  :mode ("\\.jl\\'" . ess-julia-mode)
  :config
  (setq inferior-julia-args "-t8")
  (defun my/ess-julia-cdlatex-symbol ()
    (interactive)
    (require 'cdlatex)
    (cl-letf (((symbol-function 'texmathp)
               (lambda () t)))
      (cdlatex-math-symbol))
    (call-interactively 'completion-at-point)
    (forward-sexp))
  (define-key ess-julia-mode-map (kbd "`") 'my/ess-julia-cdlatex-symbol)
  (define-key inferior-ess-julia-mode-map (kbd "`") 'my/ess-julia-cdlatex-symbol))

;; ** CIDER
(use-package cider
  :defer
  :init
  (dolist (mode '(cider-mode-hook cider-repl-mode-hook))
    (add-hook mode #'my/cider-comp-styles)
    (add-hook mode #'company-mode)
    (add-hook mode #'smartparens-mode))
  :config
  (defun my/cider-comp-styles ()
    (make-variable-buffer-local 'completion-styles)
    (add-to-list 'completion-styles 'basic)))
;; * PLUGINS
;;;################################################################

;; ** FLYSPELL
;;;----------------------------------------------------------------
(use-package flyspell
  :commands flyspell-mode
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              ("C-," . nil)
              ("C-; C-;" . 'flyspell-correct-word-before-point)
              ("C-; n" . 'flyspell-goto-next-error)))

;;;----------------------------------------------------------------
;; ** EMBRACE
;;;----------------------------------------------------------------
(use-package embrace
  :ensure t
  :hook ((org-mode . embrace-org-mode-hook)
         (org-mode . my/embrace-latex-mode-hook-extra)
         (LaTeX-mode . embrace-LaTeX-mode-hook)
         (LaTeX-mode . my/embrace-latex-mode-hook-extra))
  :bind (:map prog-mode-map
              ("M-s a" . embrace-add)
              ("M-s c" . embrace-change)
              ("M-s d" . embrace-delete)
         :map org-mode-map
              ("M-s a" . embrace-add)
              ("M-s c" . embrace-change)
              ("M-s d" . embrace-delete)
         :map text-mode-map
              ("M-s a" . embrace-add)
              ("M-s c" . embrace-change)
              ("M-s d" . embrace-delete))
  :config
  ;; Monkey patching: Expand region goes haywire sometimes
  (defun embrace--get-region-overlay (open close)
    (let ((bounds (or (embrace--fallback-re-search open close)
                      (embrace--expand-region-research open close))))
      (when bounds
        (make-overlay (car bounds) (cdr bounds) nil nil t))))

  (defun  embrace--fallback-re-search (open close)
    (my/find-bounds-of-regexps open close))
  
  (defun my/embrace-latex-mode-hook-extra ()
    (add-to-list 'embrace-semantic-units-alist '(?E . er/mark-LaTeX-inside-environment))
    (add-to-list 'embrace-semantic-units-alist '(?e . LaTeX-mark-environment))
    (add-to-list 'embrace-semantic-units-alist '(?$ . er/mark-LaTeX-math))
    (embrace-add-pair-regexp ?m "\\\\[a-z*]+{" "}" #'my/embrace-latex-macro-read-function
                              (embrace-build-help "\\macro{" "}"))
    (embrace-add-pair-regexp ?e "\\\\begin{[a-z*]+}" "\\\\end{[a-z*]+}"
                              (lambda ()
                                (let ((env (completing-read "Env: "
                                                            (mapcar #'car
                                                                    LaTeX-environment-list))))
                                  (cons (format "\\begin{%s}" env)
                                        (format "\\end{%s}" env))))
                              (embrace-build-help "\\begin{.}" "\\end{.}"))
    (embrace-add-pair-regexp 36 "\\$" "\\$" nil)
    (embrace-add-pair-regexp ?d "\\\\left\\\\*[{([|<]" "\\\\right\\\\*[]})|>]"
                              (lambda ()
                                (let* ((env (read-char "Delim type: "))
                                       (env-pair (pcase env
                                                   ((or 40 41) '("(" . ")"))
                                                   ((or 91 93) '("[" . "]"))
                                                   ((or 123 125) '("\\{" . "\\}"))
                                                   ((or 60 62) '("<" . ">"))
                                                   (124 '("|" . "|")))))
                                  (cons (format "\\left%s " (car env-pair))
                                        (format " \\right%s" (cdr env-pair)))))
                              (embrace-build-help "\\left." "\\right.")
                              ))

  (defun my/embrace-latex-macro-read-function ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\\\[[{(|]"
         :right-regexp "\\\\[]})|]"))

  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (assoc-default char embrace--pairs-list)))
        (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                  (funcall (embrace-pair-struct-read-function pair)))))
            real-pair
          (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
      (cons char char)))

  (defun +evil--embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair char))
              (text "\\%s")) ;; (if (sp-point-in-string) "\\\\%s" "\\%s")
          (cons (format text (car pair))
                (format text (cdr pair))))))))
;;;----------------------------------------------------------------
;; ** STROKES
;;;----------------------------------------------------------------
(use-package strokes
  :bind ("<down-mouse-9>" . strokes-do-stroke)
  :config
  (setq strokes-file (dir-concat user-cache-directory "strokes"))
  (setq strokes-use-strokes-buffer t))

;;;----------------------------------------------------------------
;; ** ERRORS
;;;----------------------------------------------------------------

;; This code makes it easy to repeat navigation to the next/previous error.
;; Emacs 28 has repeat-mode that does this by default.
(use-package simple
  :if (version<= emacs-version "28.0")
  :bind (("M-g n" . my/next-error)
         ("M-g p" . my/next-error)
         ;; ("M-n" . next-error)
         ;; ("M-p" . next-error)
         )
  :config
  (defun my/next-error (&optional arg reset)
    "`next-error' with easier cycling through errors."
    (interactive "P")
    (let* ((ev last-command-event)
           (echo-keystrokes nil)
           (num (pcase ev
                  (?p -1)
                  (_  1))))
      (next-error (if arg (* arg num) num)
                  reset)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "n") 'my/next-error)
         (define-key map (kbd "p") 'my/next-error)
         map)))))

;;;----------------------------------------------------------------
;; ** DUMB-JUMP
;;;----------------------------------------------------------------
;; Even dumber jump
(use-package buffer-local-xref
  ;; :after xref
  :init (add-hook 'xref-backend-functions #'buffer-local-xref-activate 95)
  ;; :hook (org-mode . (lambda ()
  ;;                     (add-hook 'xref-backend-functions
  ;;                               #'buffer-local-xref-activate
  ;;                               30 t)))
  )

(use-package dumb-jump
  :ensure t
  ;; :after xref
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (add-to-list 'dumb-jump-project-denoters ".project")
  ;; :hook (org-mode . (lambda ()
  ;;                     (add-hook 'xref-backend-functions
  ;;                               #'dumb-jump-xref-activate
  ;;                               20 t)))
  )

;;;----------------------------------------------------------------
;; ** UNDO-TREE
;;;----------------------------------------------------------------
(use-package undo-tree
  :disabled
  :defer
  :config (setq undo-tree-enable-undo-in-region  t))

;;;----------------------------------------------------------------
;; ** FLYMAKE
;;;----------------------------------------------------------------
(use-package flymake
  :defer
  :config
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

(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config (setq flymake-diagnostic-at-point-display-diagnostic-function
                'flymake-diagnostic-at-point-display-minibuffer))

(use-package package-lint-flymake
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

(use-package flymake-proselint
  :ensure t
  :after flymake
  :hook ((markdown-mode org-mode text-mode) . flymake-proselint-setup))

;;;----------------------------------------------------------------
;; ** BROWSE-URL
;;;----------------------------------------------------------------
(use-package browse-url
  :commands (browse-url-at-point-umpv browse-url-umpv)
  :config
  (when IS-LINUX
    (defun browse-url-umpv (url &optional single)
      (start-process "mpv" nil (if single "mpv" "umpv")
                     (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv (url)
      (browse-url-umpv url t))
    
    (defun browse-url-at-point-umpv (&optional single)
      "Open link in mpv"
      (interactive "P")
      (let ((browse-url-browser-function
             (if single
                 (lambda (url &optional _new-window) (browse-url-umpv url t))
               #'browse-url-umpv)))
        (browse-url-at-point)))

    (setq browse-url-generic-program "/usr/bin/qutebrowser")
    ;; (setq browse-url-browser-function
    ;;       '(("https:\\/\\/www\\.youtu\\.*be." . browse-url-umpv)
    ;;         ("." . browse-url-generic)))
    ))

;;;----------------------------------------------------------------
;; ** TRANSIENT
;;;----------------------------------------------------------------

(use-package transient
  :defines toggle-modes
  :bind (("<f8>"  . toggle-modes)
         ("C-c t" . toggle-modes))
  :config
  (transient-bind-q-to-quit)
  (setq transient-display-buffer-action '(display-buffer-below-selected))
  (setq transient-history-file (dir-concat user-cache-directory "transient/history.el")
        transient-levels-file (dir-concat user-cache-directory "transient/levels.el")
        transient-values-file (dir-concat user-cache-directory "transient/values.el"))
  (transient-define-prefix toggle-modes ()
    "Turn on and off various frequently used modes."
    
    [:pad-keys t
     ["Appearance"
      ("t" "color theme" my/toggle-theme)
      ("B" "BIG mode"    presentation-mode)
      ;; ("M" "smart modeline" ignore)
      ("8" "pretty symbols" (lambda () (interactive)
                              (if (derived-mode-p 'org-mode)
                                  (org-toggle-pretty-entities)
                                (call-interactively
                                 #'prettify-symbols-mode))))
      ("vl" "visual lines" visual-line-mode)
      ("vt" "trunc lines" toggle-truncate-lines)]

     ["Editing"
      ("r" "read only" read-only-mode)
      ("n" "line numbers" display-line-numbers-mode)
      ("M-q" "auto fill" auto-fill-mode)
      (";" "flyspell" flyspell-mode)
      ("V" "view mode" view-mode)
      ("o" "outline" outline-minor-mode)]

     ["Highlight"
      ("hl" "line" hl-line-mode)
      ("hp" "paren" show-paren-mode)
      ("hw" "whitespace" whitespace-mode)
      ("hd" "delimiters" rainbow-delimiters-mode)
      ("hr" "rainbow" rainbow-mode)]

     ["Code"
      ("c" "completion" corfu-mode)
      ("a" "autocomp" (lambda () (interactive)
                        (setq-local corfu-auto (not corfu-auto))
                        (message "corfu-auto is now %s" corfu-auto))
       :transient t)
      ;; ("a"
      ;;  :description "autocomp?"
      ;;  :class transient-lisp-variable
      ;;  :variable corfu-auto
      ;;  :reader (lambda () (interactive)
      ;;            (setq corfu-auto
      ;;                  (not corfu-auto))))
      ("g" "vc gutter" diff-hl-mode)
      ("f" "flymake" flymake-mode)
      ("e" "elec pair" electric-pair-mode)
      ("p" "smartparens" smartparens-mode)]]))

;;;----------------------------------------------------------------
;; ** HYDRAS
;;;----------------------------------------------------------------
(use-package hydra
  :defer
  :ensure t
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
  (:keymaps 'smerge-mode-map
            "C-c s" 'hydra-smerge/body)
  (:keymaps 'space-menu-window-map
            "u" '(hydra-winner/body
                  :wk "winner-mode"))
  (:states '(motion)
           "C-w u" 'hydra-winner/body)
  ("C-c <tab>" 'hydra-outline/body)
  (:keymaps 'space-menu-map
            "t" 'hydra-toggle-menu/body)
  (:keymaps 'space-menu-map
            :prefix "f"
            "=" 'hydra-ediff/body)
  )

;;;----------------------------------------------------------------
;; ** TABS!TABS!TABS!
;; *** TAB-BAR
;;;----------------------------------------------------------------
(use-package tab-bar
  :if (not (version-list-<
            (version-to-list emacs-version)
            '(27 0 1 0)))
  :after cus-face
  :defer
  :bind-keymap ("H-t" . tab-prefix-map)
  :bind
  (("C-M-<tab>" . tab-bar-switch-to-next-tab)
   ("C-M-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
   ("H-<tab>" . tab-bar-switch-to-next-tab)
   ("H-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
   ("s-u" . tab-bar-history-back)
   ;; ("C-c u" . tab-bar-history-back)
   ;; ("s-S-U" . tab-bar-history-forward)
   :map tab-prefix-map
   ("h" . my/tab-bar-show-hide-tabs)
   ("H-t" . tab-bar-select-tab-by-name))

  :config
  (tab-bar-history-mode 1)
  (when (version< "28.0" emacs-version)
    (defun tab-bar-format-menu-bar ()
      "Produce the Menu button for the tab bar that shows the menu bar."
      `((menu-bar menu-item (propertize " 𝝺 " 'face 'tab-bar-tab-inactive)
                  tab-bar-menu-bar :help "Menu Bar")))
    (defun tab-bar-tab-name-format-default (tab i)
      (let ((current-p (eq (car tab) 'current-tab)))
        (propertize
         (concat " "
                 (if tab-bar-tab-hints (format "%d " i) "")
                 (alist-get 'name tab)
                 (or (and tab-bar-close-button-show
                          (not (eq tab-bar-close-button-show
                                   (if current-p 'non-selected 'selected)))
                          tab-bar-close-button)
                     "")
                 " ")
         'face (funcall tab-bar-tab-face-function tab))))
    (setq tab-bar-format '(tab-bar-format-menu-bar
                           ;; tab-bar-format-history
                           tab-bar-format-tabs
                           tab-bar-separator
                           tab-bar-format-add-tab
                           tab-bar-format-align-right
                           tab-bar-format-global)
          tab-bar-close-button-show nil))
  
  (setq  tab-bar-close-last-tab-choice 'tab-bar-mode-disable
         tab-bar-show                   (when (version< "28.0" emacs-version) 1)
         tab-bar-tab-name-truncated-max 14
         tab-bar-new-tab-choice        'ibuffer
         tab-bar-tab-name-function '(lambda nil
                                      "Use directory as tab name."
                                      (let ((dir (expand-file-name
                                                  (or (if (fboundp 'project-root)
                                                          (project-root (project-current)))
                                                      default-directory))))
                                        (substring dir (1+ (string-match "/[^/]+/$" dir)) -1 )))
         ;; tab-bar-select-tab-modifiers  '(meta)
         ;; tab-bar-tab-name-function 'tab-bar-tab-name-truncated
         ;; tab-bar-tab-name-function '(lambda nil (upcase (tab-bar-tab-name-truncated)))
         )

  (setq tab-bar-select-tab-modifiers '(meta hyper))

  (defun my/tab-bar-show-hide-tabs ()
    "Show or hide tabs."
    (interactive)
    (setq tab-bar-show (if tab-bar-show nil 1)))
   ;; (custom-set-faces
   ;; '(tab-bar ((t (:inherit nil :height 1.1))))
   ;; '(tab-bar-tab-inactive ((t (:inherit tab-bar :weight normal :height 0.9))))
   ;; '(tab-bar-tab ((t (:inherit tab-bar :underline t :weight bold))))
   ;; )

  (advice-add 'tab-bar-rename-tab
              :after
              (defun my/tab-bar-name-upcase (_name &optional _arg)
                "Upcase current tab name"
                (let* ((tab (assq 'current-tab (frame-parameter nil 'tabs)))
                       (tab-name (alist-get 'name tab)))
                  (setf (alist-get 'name tab) (upcase tab-name)
                        (alist-get 'explicit-name tab) t)))))

;; Show a list of the tabs in the echo area when switching tabs. Disabled since
;; I've taken to showing the tab-bar instead
(use-package tab-bar-echo-area
  :if (version< emacs-version "28.0")
  :ensure
  :after tab-bar
  :init
  (if (version< emacs-version "28.0")
      (defvar tab-bar-format nil "Format for tab-bar-echo-area-mode"))
  :config
  (tab-bar-echo-area-mode 1))
;;;----------------------------------------------------------------
;; *** +EYEBROWSE+
;;;----------------------------------------------------------------

;; This is superceded by native tabs (tab-bar-mode) in Emacs 27. I keep this
;; around in case I find myself using Emacs 26.3 or lower.
(use-package eyebrowse
  :disabled
  :if (version-list-<
       (version-to-list emacs-version)
       '(27 0 1 0))
  :hook (after-init . eyebrowse-mode)
  :init (setq eyebrowse-keymap-prefix (kbd "C-x t"))
  ;; :bind ("C-c C-w c" . eyebrowse-create-window-config)
  ;; :commands eyebrowse-create-window-config
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
              '(:eval (my-title-bar-format))))))

;;;----------------------------------------------------------------
;; ** HIGHLIGHTS
;;;----------------------------------------------------------------
;; Flash lines
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my/recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my/recenter-and-pulse-line))
  :init
  (add-hook 'after-make-frame-functions
            (defun my/pulse-type (_frame)
              (when window-system (setq pulse-flag t))))
  
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

    (defun my/pulse-momentary-upper-bound (&rest _)
      "Pulse the upper scrolling bound of the screen."
      (save-excursion
        (move-to-window-line next-screen-context-lines)
        (my/pulse-momentary-line)))

    (defun my/pulse-momentary-lower-bound (&rest _)
      "Pulse the lower scrolling bound of the screen."
      (save-excursion
        (move-to-window-line (- next-screen-context-lines))
        (my/pulse-momentary-line)))

    (advice-add 'scroll-up-command   :after #'my/pulse-momentary-upper-bound)
    (advice-add 'scroll-down-command :after #'my/pulse-momentary-lower-bound)

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
                   goto-last-change))
      (advice-add cmd :after #'my/pulse-momentary))))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :ensure
  :defer
  ;; :custom-face
  ;; (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  ;; (diff-hl-insert ((t (:background nil))))
  ;; (diff-hl-delete ((t (:background nil))))
  :hook ((dired-mode . diff-hl-dired-mode))
  :init
  (setq diff-hl-draw-borders t)
  (dolist (mode-hook +addons-enabled-modes)
    (add-hook mode-hook #'diff-hl-mode))
  :bind
  (:map diff-hl-command-map
   ("n" . diff-hl-next-hunk)
   ("p" . diff-hl-previous-hunk)
   ("[" . nil)
   ("]" . nil)
   ("DEL"   . diff-hl-revert-hunk)
   ("<delete>" . diff-hl-revert-hunk)
   ("SPC" . diff-hl-mark-hunk)
   :map vc-prefix-map
   ("n" . diff-hl-next-hunk)
   ("p" . diff-hl-previous-hunk)
   ("DEL"   . diff-hl-revert-hunk)
   ("<delete>" . diff-hl-revert-hunk)
   ("SPC" . diff-hl-mark-hunk))
  :general
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

;;;----------------------------------------------------------------
;; ** WALLABAG
;;;----------------------------------------------------------------

(use-package wallabag
  :disabled
  :defer t
  :load-path "plugins/wallabag/"
  :commands wallabag
  :config
  (setq wallabag-host my-wallabag-host)
  (setq wallabag-username my-wallabag-username)
  (with-eval-after-load 'embark
    (define-key embark-url-map (kbd "R")
      (defun embark-wallabag (url)
        (wallabag-post-entry url)))))

;;;----------------------------------------------------------------
;; ** +NAV-FLASH+
;;;----------------------------------------------------------------
(use-package nav-flash :disabled)

;;;----------------------------------------------------------------
;; ** HIDESHOW (built in)
;;;----------------------------------------------------------------
(use-package hideshow ; built-in
  :commands (hs-cycle
             hs-global-cycle)
  :bind (:map prog-mode-map
              ("C-<tab>" . hs-cycle)
              ("<backtab>" . hs-global-cycle)
              ("C-S-<iso-lefttab>" . hs-global-cycle))
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'hideshow-set-up-overlay-fn)

  (defface hideshow-folded-face
    `((t (:inherit font-lock-comment-face :weight light)))
    "Face to hightlight `hideshow' overlays."
    :group 'hideshow)
  
  (defun hideshow-set-up-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put
       ov 'display (propertize "  [...]  " 'face 'hideshow-folded-face))))
  
  (dolist (hs-command (list #'hs-cycle
                            #'hs-global-cycle))
    (advice-add hs-command :before
                (lambda (&optional end) "Advice to ensure `hs-minor-mode' is enabled"
                  (unless (bound-and-true-p hs-minor-mode)
                    (hs-minor-mode +1)))))

  (defun hs-cycle (&optional level)
    (interactive "p")
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;;TODO: Fix this case. `hs-show-block' needs to be called twice to
           ;;open all folds of the parent block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level)))
  
  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))  
  
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

;;;----------------------------------------------------------------
;; ** EDIFF (built-in)
;;;----------------------------------------------------------------
(use-package ediff
  :defer t
  :functions ediff-setup-windows-plain
  :hook ((ediff-prepare-buffer       . my/ediff-expand-outlines)
         (ediff-before-setup         . my/ediff-save-wconf-h)
         ((ediff-quit ediff-suspend) . my/ediff-restore-wconf-h))
  :init
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defun my/ediff-expand-outlines ()
    "If outline minor mode is active, expand the ediff buffers
fully before starting comparison."
    (when outline-minor-mode
      (outline-show-all)))
  (defvar my/ediff-saved-wconf nil)
  (defun my/ediff-save-wconf-h ()
    (setq my/ediff-saved-wconf (current-window-configuration)))
  (defun my/ediff-restore-wconf-h ()
    (when (window-configuration-p my/ediff-saved-wconf)
      (set-window-configuration my/ediff-saved-wconf))))

;;;----------------------------------------------------------------
;; ** HELPFUl
;;;----------------------------------------------------------------
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
            "C-f" 'describe-face
            "." '(helpful-at-point :wk "help at point")
            "v" '(helpful-variable :wk "describe variable")
            "f" '(helpful-callable :wk "describe function")
            "k" '(helpful-key :wk "describe keybind")
            "C" '(helpful-command :wk "describe command"))
  ;; (:keymaps 'space-menu-help-map
  ;;           :wk-full-keys nil
  ;;           "f" '(helpful-callable :wk "Describe function")
  ;;           "v" '(helpful-variable :wk "Describe variable")
  ;;           "k" '(helpful-key :wk "Describe keybind")
  ;;           "." '(helpful-at-point :wk "Help at point")
  ;;           "C" '(helpful-command :wk "Describe command"))

)

(use-package emacs
  :bind (("C-h A" . info-apropos)
         ("C-h C-a" . customize-apropos)))

;;;----------------------------------------------------------------

;; ** VERSION CONTROL
;;;----------------------------------------------------------------
(use-package vc
  :defer
  :config
  (setq vc-follow-symlinks t)
  (setq vc-find-revision-no-save t)

  (use-package log-view
    :config
    (defun my/vc-print-log (&optional arg)
      "Like `vc-print-log' but for a custom fileset.

With optional prefix ARG (\\[universal-argument]), query for a
number to limit the log to.  Then prompt the user for matching
files in the `default-directory'.  A literal space delimits
multiple files (inserting a space will renew the prompt, asking
for another file match).

In a `dired-mode' buffer, print log for the file at point, or any
marked files, except for when a double prefix argument is passed.
A single prefix arg still provides for a limit to the log.

If a double prefix ARG is passed, prompt for a limit and produce
a log that covers all files in the present directory."
      (interactive "P")
      (let* ((lim (if arg
                      (read-number "Limit log to N entries: " 5)
                    20))
             (dir default-directory)
             (dotless directory-files-no-dot-files-regexp)
             (files (directory-files dir nil dotless t))
             (crm-separator " ")
             (set (cond
                   ((equal arg '(16))
                    files)
                   ((eq major-mode 'dired-mode)
                    (dired-get-marked-files t nil))
                   (t
                    (completing-read-multiple
                     "Select files in current dir: " files nil t))))
             (backend (vc-backend set)))
        (vc-print-log-internal backend set nil nil lim 'with-diff)))

    (defun my/vc-git-expanded-log-entry (revision)
      "Expand git commit message for REVISION."
      (with-temp-buffer
        (apply 'vc-git-command t nil nil (list "log" revision "--stat" "-1" "--"))
        (goto-char (point-min))
        (unless (eobp)
          (while (re-search-forward "^" nil t)
            (replace-match "  ")
            (forward-line))
          (concat "\n" (buffer-string)))))

    (add-hook 'vc-git-log-view-mode-hook
              (defun my/vc-git-expand-function ()
                "Set `log-view-expanded-log-entry-function' for `vc-git'"
                (setq-local log-view-expanded-log-entry-function
                            #'my/vc-git-expanded-log-entry)))

    ;; (defun my/log-view-extract-commit ()
    ;;   "Kill commit from around point in `vc-print-log'."
    ;;   (interactive)
    ;;   (let ((commit (cadr (log-view-current-entry (point) t))))
    ;;     (kill-new (format "%s" commit))
    ;;     (message "Copied: %s" commit)))

    ;; (defvar my/vc-shell-output "*vc-shell-output*"
    ;;   "Name of buffer for VC-related shell output.")

    ;; (defun my/log-view-create-patch ()
    ;;   "Create patch for commit at point in `log-view'."
    ;;   (interactive)
    ;;   (let* ((commit (cadr (log-view-current-entry (point) t)))
    ;;          (vc-dir (or (vc-root-dir) default-directory))
    ;;          (dirs (list "~/" "~/Desktop/" vc-dir))
    ;;          (out-dir ;; (read-directory-name "Output directory: ")
    ;;           (completing-read "Output directory: " dirs))
    ;;         (buf (get-buffer-create my/vc-shell-output)))
    ;;     (shell-command
    ;;      (format "git format-patch -1 %s -o %s" commit out-dir) buf)
    ;;     (message "Prepared patch for `%s' and sent it to %s"
    ;;              (propertize commit 'face 'bold)
    ;;              (propertize out-dir 'face 'success))))

    ;; From https://protesilaos.com/codelog/2021-07-24-emacs-misc-custom-commands/
    (defun my/diff-buffer-dwim (&optional arg)
  "Diff buffer with its file's last saved state, or run `vc-diff'.
With optional prefix ARG (\\[universal-argument]) enable
highlighting of word-wise changes (local to the current buffer)."
  (interactive "P")
  (let ((buf))
    (if (buffer-modified-p)
        (progn
          (diff-buffer-with-file (current-buffer))
          (setq buf "*Diff*"))
      (vc-diff)
      (setq buf "*vc-diff*"))
    (when arg
      (with-current-buffer (get-buffer buf)
        (unless diff-refine
          (setq-local diff-refine 'font-lock))))))

    :bind-keymap ("H-v" . vc-prefix-map)
    :bind (("C-x v C-l" . my/vc-print-log)
           :map vc-prefix-map
           ("=" . my/diff-buffer-dwim)
           :map log-view-mode-map
           ("<tab>" . log-view-toggle-entry-display)
           ("<return>" . log-view-find-revision)
           ;; ("c" . my/log-view-create-patch)
           ;; ("w" . my/log-view-extract-commit)
           ("s" . vc-log-search)
           ("O" . vc-log-outgoing)
           ("I" . vc-log-incoming)
           ("F" . vc-update)
           ("+" . vc-update)
           ("P" . vc-push))))

(use-package vc-dir
  :after vc
  :config
  (defun my/vc-dir (&optional arg)
    "Run `vc-dir' for the current project or directory.
With optional ARG (\\[universal-argument]), use the present
working directory, else default to the root of the current
project, as defined by `vc-root-dir'."
    (interactive "P")
    (let ((dir (if arg default-directory (vc-root-dir))))
      (vc-dir dir)))

  ;; Hide unregistered files
  (defun my/vc-dir-hide-unregistered ()
    "Hide unregistered files in a vc-dir."
    (vc-dir-hide-state 'unregistered))

  (add-hook 'vc-dir-mode-hook #'my/vc-dir-hide-unregistered 90)
  :bind
  (("C-x v d" . my/vc-dir)
   :map vc-dir-mode-map
   ("F" . vc-update)
   ("k" . vc-dir-clean-files)))

(use-package vc-git
  :after vc
  :config
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (if (version<= "28" emacs-version)
      (setq vc-git-revision-complete-only-branches nil))
  (setq vc-git-root-log-format
        '("%d%h %ad %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          "^\\(?:[*/\\| ]+ \\)?\
\\(?2: ([^)]+)\\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date)))))

(use-package vc-annotate
  :after vc
  :config
  (setq vc-annotate-display-mode 'scale)
  :bind (:map vc-annotate-mode-map
        ("<tab>" . vc-annotate-toggle-annotation-visibility)))

(use-package smerge-mode
  :defer
  :config
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

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
  ;; (define-key magit-mode-map (kbd "C-TAB") nil)
  ;; (define-key magit-mode-map (kbd "C-<tab>") nil)
  ;; (dolist (keymap (list magit-diff-mode-map magit-log-mode-map))
  ;;   (define-key keymap (kbd "C-TAB") nil)
  ;;   (define-key keymap (kbd "C-<tab>") nil))
  )

;; Misc git functions
(use-package emacs
  :config
  ;; From http://xenodium.com/emacs-clone-git-repo-from-clipboard/
  ;; Auto-git-clone url in clipboard
  (defun my/git-clone-clipboard-url ()
    "Clone git URL in clipboard asynchronously and open in dired when finished."
    (interactive)
    (cl-assert (or (string-match-p "^git@github.com" (current-kill 0))
                   (string-match-p "^git@.*\\.org" (current-kill 0)) 
                   (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)))
               nil
               "No URL in clipboard")
    (let* ((url (current-kill 0))
           (download-dir (expand-file-name "~/.local/share/git/"))
           (project-dir (concat (file-name-as-directory download-dir)
                                (file-name-base url)))
           (default-directory download-dir)
           (command (format "git clone %s" url))
           (buffer (generate-new-buffer (format "*%s*" command)))
           (proc))
      (when (file-exists-p project-dir)
        (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
            (delete-directory project-dir t)
          (user-error "Bailed")))
      (switch-to-buffer buffer)
      (setq proc (start-process-shell-command
                  (shell-quote-argument (nth 0 (split-string command)))
                  buffer command))
      (with-current-buffer buffer
        (setq default-directory download-dir)
        (shell-command-save-pos-or-erase)
        (require 'shell)
        (shell-mode)
        (view-mode +1))
      (set-process-sentinel proc (lambda (process state)
                                   (let ((output (with-current-buffer (process-buffer process)
                                                   (buffer-string))))
                                     (kill-buffer (process-buffer process))
                                     (if (= (process-exit-status process) 0)
                                         (progn
                                           (message "finished: %s" command)
                                           (dired project-dir))
                                       (user-error (format "%s\n%s" command output))))))
      (set-process-filter proc #'comint-output-filter))))
;;;----------------------------------------------------------------
;; ** WHICH-KEY
;;;----------------------------------------------------------------
(use-package which-key
  :ensure t
  :defer 1
  :general
  (:keymaps 'help-map
   "h" 'which-key-show-major-mode
   "C-k" 'which-key-show-major-mode)
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
  (which-key-setup-side-window-bottom)
  ;; (which-key-setup-side-window-right-bottom)
  (add-hook 'which-key-init-buffer-hook
            (lambda () (setq-local line-spacing 3)))

  (advice-add 'which-key-mode :after
              (lambda (_arg)
                (when (featurep 'embark)
                  (setq prefix-help-command
                        #'embark-prefix-help-command))))
  
  (which-key-mode +1)
  :diminish "")

;;;----------------------------------------------------------------
;; ** CALC
;;;----------------------------------------------------------------
(use-package calc
  :bind (("C-x c" . calc)
         ("H-S-c" . calc)
         ("H-*" . calc-dispatch)
         ("C-S-e" . latex-math-from-calc))
  :config
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (let ((lang (when (member major-mode '(org-mode latex-mode))
                  'latex)))
      (cond ((region-active-p)
             (let* ((beg (region-beginning))
                    (end (region-end))
                    (string (buffer-substring-no-properties beg end)))
               (kill-region beg end)
               (insert (calc-eval `(,string calc-language ,lang
                                            calc-prefer-frac t
                                            calc-angle-mode rad)))))
            (t (let ((l (thing-at-point 'line)))
                 (end-of-line 1) (kill-line 0)
                 (insert (calc-eval `(,l
                                      calc-language ,lang
                                      calc-prefer-frac t
                                      calc-angle-mode rad)))))))))

(use-package calctex
  :disabled
  :load-path "~/.local/share/git/calctex/calctex"
  :load-path "~/.local/share/git/calctex/calctex-contrib"
  :after calc
  :config
  (setq calctex-additional-latex-packages "
\\usepackage[usenames]{xcolor}
\\usepackage{soul}
\\usepackage{adjustbox}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{siunitx}
\\usepackage{cancel}
\\usepackage{mathtools}
\\usepackage{mathalpha}
\\usepackage{xparse}
\\usepackage{arevmath}"
        calctex-additional-latex-macros
        (concat calctex-additional-latex-macros
                "\n\\let\\evalto\\Rightarrow"))
  (let ((vendor-folder (concat (file-name-as-directory (getenv "HOME"))
                               ".local/share/git/calctex/vendor/")))
    (setq calctex-dvichop-bin (concat vendor-folder "texd/dvichop")
          calctex-dvichop-sty (concat vendor-folder "texd/dvichop")
          calctex-imagemagick-enabled-p nil))
  (unless (file-exists-p calctex-dvichop-bin)
    (message "CalcTeX: Building dvichop binary")
    (let ((default-directory (file-name-directory calctex-dvichop-bin)))
      (call-process "make" nil nil nil))))

;;;----------------------------------------------------------------
;; ** ISEARCH
;;;----------------------------------------------------------------
(require 'setup-isearch)

;;;----------------------------------------------------------------
;; ** ABBREV MODE
;;;----------------------------------------------------------------
(use-package abbrev
  :defer
  :config
  ;; (setq abbrev-file-name (expand-file-name (dir-concat user-cache-directory "abbvev-defs")))
  (setq abbrev-file-name (dir-concat user-emacs-directory "abbrev_defs"))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;;----------------------------------------------------------------
;; ** SMARTPARENS-MODE
;;;----------------------------------------------------------------
(use-package elec-pair
       :defer
       :config
       (setq electric-pair-inhibit-predicate
             `(lambda (c)
                (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))
       ;; (electric-pair-mode +1)
       )
(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-M-<up>"      . sp-raise-sexp)
        ("C-<right>"     . sp-forward-slurp-sexp)
        ("C-<left>"      . sp-backward-slurp-sexp)
        ("M-<right>"     . sp-forward-barf-sexp)
        ("M-<left>"      . sp-backward-barf-sexp)
        ("C-k"           . sp-kill-hybrid-sexp)
        ("C-x C-t"       . sp-transpose-hybrid-sexp)
        ("C-M-n"         . sp-next-sexp)
        ("C-M-p"         . sp-previous-sexp)
        ("C-<backspace>" . sp-backward-kill-word))
  :init
  (add-hook 'smartparens-enabled-hook
            (lambda ()
              "Disable \\[electric-pair-mode] when \[[smartparens-mode]] is enabled."
              (electric-pair-local-mode -1)))
  (add-hook 'smartparens-disabled-hook
            (lambda ()
              "Enable \\[electric-pair-mode] when \[[smartparens-mode]] is disabled."
              (electric-pair-local-mode +1)))
  :config
  ;; (require 'smartparens-config)
  (sp-with-modes sp-lisp-modes
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)))

;;;----------------------------------------------------------------
;; ** EXPAND-REGION
;;;----------------------------------------------------------------
(use-package expand-region
  :ensure t
  :commands expand-region
  :bind ("C-," . 'er/expand-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode)
  (set-default 'er--show-expansion-message t)
  (setq expand-region-show-usage-message nil)
  
  (defun my/find-bounds-of-regexps (open close)
    (let ((start (point))
          (parity 0)
          (open-close (concat "\\(?:" open "\\|" close "\\)")))
      (save-excursion
        (while (and (not (= parity -1))
                    (re-search-backward open-close nil t))
          (if (looking-at open)
              (setq parity (1- parity))
            (setq parity (1+ parity))))
        (push-mark)
        (goto-char start)
        (while (and (not (= parity 0))
                    (re-search-forward open-close nil t))
          (if (looking-back close)
              (setq parity (1+ parity))
            (setq parity (1- parity))))
        (when (= parity 0) (cons (mark) (point))))))

  (use-package outline
    :hook (outline-minor-mode . er/add-outline-mode-expansions)
    :config
    (defun er/add-outline-mode-expansions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (add-to-list 'er/try-expand-list 'outline-mark-subtree)))
  
  (use-package latex
    :defer
    :config
    (add-hook 'LaTeX-mode-hook 'er/set-latex-mode-expansions 90)
    (defun er/mark-latex-text-sentence ()
      (unless(texmathp) (er/mark-text-sentence)))
    (defun er/mark-latex-text-paragraph ()
      (unless (texmathp) (er/mark-text-paragraph)))
    (defun er/mark-latex-inside-pairs ()
      (unless (texmathp) (er/mark-inside-pairs)))
    (defun er/mark-latex-outside-pairs ()
      (unless (texmathp) (er/mark-outside-pairs)))
    (defun er/mark-latex-outside-delimiters ()
      (destructuring-bind (beg . end )
          (my/find-bounds-of-regexps "\\\\left\\\\*[{([|<]"
                                     "\\\\right\\\\*[]})|>]")
        (set-mark (save-excursion
                    (goto-char beg)
                    (skip-chars-forward er--space-str)
                    (point)))
        (goto-char end)
        (skip-chars-backward er--space-str)
        (exchange-point-and-mark)))
    (defun er/mark-latex-inside-delimiters ()
      (when (texmathp)
        (destructuring-bind (beg . end)
            (my/find-bounds-of-regexps "\\\\left\\\\*[{([|<]"
                                       "\\\\right\\\\*[]})|>]")
          (set-mark (save-excursion
                      (goto-char beg)
                      (skip-chars-forward er--space-str)
                      (forward-char 6)
                      (point)))
          (goto-char end)
          (skip-chars-backward er--space-str)
          (backward-char 7))
        (exchange-point-and-mark)))
    (defun er/set-latex-mode-expansions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (setq er/try-expand-list
            '(er/mark-word er/mark-symbol er/mark-symbol-with-prefix
              er/mark-next-accessor  er/mark-inside-quotes er/mark-outside-quotes
              er/mark-latex-inside-pairs er/mark-latex-outside-pairs
              er/mark-latex-inside-delimiters er/mark-latex-outside-delimiters
              er/mark-comment er/mark-url er/mark-email ;er/mark-defun
              er/mark-latex-text-sentence er/mark-latex-text-paragraph))
      (er/add-latex-mode-expansions)
      ;;  LaTeX-mark-environment LaTeX-mark-section
      ;;  er/mark-LaTeX-inside-environment er/mark-LaTeX-math
      ;;  er/mark-method-call 
)))

;;;----------------------------------------------------------------
;; ** AVY
;;;----------------------------------------------------------------
(use-package avy
  :ensure t
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.35)
  (setq avy-keys '(?a ?s ?d ?f ?g ?j ?l ?\; ;?x
                   ?v ?b ?n ?, ?/ ?u ?p ?e ?.
                   ?c ?q ?2 ?3 ?'))
  (setq avy-dispatch-alist '((?m . avy-action-mark)
                             (?  . avy-action-mark-to-char)
                             (?i . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?o . avy-action-embark)
                             (?= . avy-action-define)
                             (67108925 . avy-action-tuxi)
                             ;; (?W . avy-action-tuxi)
                             (?h . avy-action-helpful)
                             (?x . avy-action-exchange)
                             
                             (11 . avy-action-kill-line)
                             (25 . avy-action-yank-line)
                             
                             (?w . avy-action-easy-copy)
                             ;; (134217847  . avy-action-easy-copy)
                             (?k . avy-action-kill-stay)
                             (?y . avy-action-yank)
                             (?t . avy-action-teleport)
                             
                             (?W . avy-action-copy-whole-line)
                             (?K . avy-action-kill-whole-line)
                             (?Y . avy-action-yank-whole-line)
                             (?T . avy-action-teleport-whole-line)))
  
  (defun avy-action-easy-copy (pt)
        (require 'easy-kill)
        (goto-char pt)
        (cl-letf (((symbol-function 'easy-kill-activate-keymap)
                   (lambda ()
                     (let ((map (easy-kill-map)))
                       (set-transient-map
                        map
                        (lambda ()
                          ;; Prevent any error from activating the keymap forever.
                          (condition-case err
                              (or (and (not (easy-kill-exit-p this-command))
                                       (or (eq this-command
                                               (lookup-key map (this-single-command-keys)))
                                           (let ((cmd (key-binding
                                                       (this-single-command-keys) nil t)))
                                             (command-remapping cmd nil (list map)))))
                                  (ignore
                                   (easy-kill-destroy-candidate)
                                   (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
                                     (easy-kill-save-candidate))))
                            (error (message "%s:%s" this-command (error-message-string err))
                                   nil)))
                        (lambda ()
                          (let ((dat (ring-ref avy-ring 0)))
                            (select-frame-set-input-focus
                             (window-frame (cdr dat)))
                            (select-window (cdr dat))
                            (goto-char (car dat)))))))))
          (easy-kill)))
  
  (defun avy-action-exchange (pt)
  "Exchange sexp at PT with the one at point."
  (set-mark pt)
  (transpose-sexps 0))
  
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-define (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
            #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (dictionary-search-dwim))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  
  (defun avy-action-tuxi (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
            #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (google-search-at-point))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-kill-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  
  (defun my/avy-goto-char-this-window (&optional arg)
    "Goto char in this window with hints."
    (interactive "P")
    (let ((avy-all-windows)
          (current-prefix-arg (if arg 4)))
      (call-interactively 'avy-goto-word-1)))
  
  (defun my/avy-isearch (&optional arg)
    "Goto isearch candidate in this window with hints."
    (interactive "P")
    (let ((avy-all-windows)
          (current-prefix-arg (if arg 4)))
      (call-interactively 'avy-isearch)))
  
  (defun my/avy--read-char-2 (char1 char2)
    "Read two characters from the minibuffer."
    (interactive (list (let ((c1 (read-char "char 1: " t)))
                         (if (memq c1 '(? ?\b))
                             (keyboard-quit)
                           c1))
                       (let ((c2 (read-char "char 2: " t)))
                         (cond ((eq c2 ?)
                                (keyboard-quit))
                               ((memq c2 '(8 127))
                                (keyboard-escape-quit)
                                (call-interactively 'my/avy-next-char-2))
                               (t
                                c2)))))

    (when (eq char1 ?) (setq char1 ?\n))
    (when (eq char2 ?) (setq char2 ?\n))
    (string char1 char2))

  (defun my/avy-next-char-2 (&optional str2 arg)
    "Go to the next occurrence of two characters"
    (interactive (list
                  (call-interactively 'my/avy--read-char-2)
                  current-prefix-arg))
    (let* ((ev last-command-event)
           (echo-keystrokes nil))
      (push-mark (point) t)
      (if (search-forward str2 nil t
                           (+ (if (looking-at (regexp-quote str2))
                                  1 0)
                              (or arg 1)))
           (backward-char 2)
        (pop-mark)))

    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd ";") (lambda (&optional arg) (interactive)
                                   (my/avy-next-char-2 str2 arg)))
       (define-key map (kbd ",") (lambda (&optional arg) (interactive)
                                   (my/avy-previous-char-2 str2 arg)))
       map)))

  (defun my/avy-previous-char-2 (&optional str2 arg)
    "Go to the next occurrence of two characters"
       (interactive (list
                  (call-interactively 'my/avy--read-char-2)
                  current-prefix-arg))
       (let* ((ev last-command-event)
              (echo-keystrokes nil))
         (push-mark (point) t)
         (unless (search-backward str2 nil t (or arg 1))
           (pop-mark)))

    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd ";") (lambda (&optional arg) (interactive)
                                   (my/avy-next-char-2 str2 arg)))
       (define-key map (kbd ",") (lambda (&optional arg) (interactive)
                                   (my/avy-previous-char-2 str2 arg)))
       map)))
  
  (defun my/avy-copy-line-no-prompt (arg)
    (interactive "p")
    (avy-copy-line arg)
    (beginning-of-line)
    (zap-to-char 1 32)
    (delete-forward-char 1)
    (move-end-of-line 1))

  :general
  ("C-'"        '(my/avy-goto-char-this-window :wk "Avy goto char")
   "M-s j"      '(avy-goto-char-2            :wk "Avy goto char 2")
   "M-s y"      '(avy-copy-line              :wk "Avy copy line above")
   "M-s M-y"    '(avy-copy-region            :wk "Avy copy region above")
   "M-s M-k"    '(avy-kill-whole-line        :wk "Avy copy line as kill")
   "M-j"        '(avy-goto-char-timer        :wk "Avy goto char timer")
   "M-s C-w"    '(avy-kill-region            :wk "Avy kill region")
   "M-s M-w"    '(avy-kill-ring-save-region  :wk "Avy copy as kill")
   "M-s t"      '(avy-move-line              :wk "Avy move line")
   "M-s M-t"    '(avy-move-region            :wk "Avy move region")
   "M-s s"      '(my/avy-next-char-2         :wk "Avy snipe forward")
   "M-s r"      '(my/avy-previous-char-2     :wk "Avy snipe backward")
   "M-g l"      '(avy-goto-end-of-line       :wk "Avy goto line")
   "M-s z"      '(my/avy-copy-line-no-prompt :wk "Avy copy and zap"))
  ;; (:states '(normal visual)
  ;;  :prefix "g"
  ;;  "s" 'avy-goto-char-timer)
  :bind (:map isearch-mode-map
         ("C-'" . my/avy-isearch)
         ("M-j" . my/avy-isearch)))

;;;----------------------------------------------------------------
;; ** IY-GO-TO-CHAR
;;;----------------------------------------------------------------
(use-package iy-go-to-char
  :disabled
  :bind (("M-j" . iy-go-to-char)
         ("M-r" . iy-go-to-char-key-backward)))

;;;----------------------------------------------------------------
;; ** WRAP-REGION MODE
;;;----------------------------------------------------------------
(use-package wrap-region
  :ensure t
  :init (wrap-region-mode 1))
;; (add-hook 'text-mode-hook 'wrap-region-mode)

;;;----------------------------------------------------------------
;; ** ORG-MODE
;;;----------------------------------------------------------------
(load-library "setup-org")

;; --------------------
;; #+INCLUDE: "./lisp/setup-org.org" :minlevel 2
;; --------------------

;;;----------------------------------------------------------------
;; ** ORG-ADDONS
;;;----------------------------------------------------------------
;; *** ANKI
(use-package setup-anki
  :after (org-capture org))

;; *** ROAM
(use-package setup-roam)
;;;################################################################
;; ** TRAMP
;;;----------------------------------------------------------------
;; Tramp ssh'es into root@host to edit files. The emacs sudo, kindof.
(use-package tramp
  :defer
  :config
  (setq remote-file-name-inhibit-cache 86400)
  (setq tramp-persistency-file-name
        (dir-concat user-cache-directory "tramp"))
  (setq tramp-verbose 1)
  (with-eval-after-load 'vc
    (setq vc-ignore-dir-regexp
          (format "%s\\|%s"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))))
;;;----------------------------------------------------------------
;; ** DOT MODE
(use-package dot-mode
  :ensure t
  :commands dot-mode
  :bind (:map dot-mode-map
         ("C-c ." . nil)
         ("C-M-." . nil))
  :hook ((prog-mode conf-mode text-mode tex-mode) . 'dot-mode-on)
  ;; :bind ("C-." . (lambda () (interactive)
  ;;                  (dot-mode 1)
  ;;                  (message "Dot mode activated.")))
  )
;; ** BOOKMARKS
(use-package bookmark
  :config
  (setq bookmark-default-file (dir-concat user-cache-directory "bookmarks")))
;; * COMPLETION
;;;################################################################

;;;----------------------------------------------------------------
;; ** DABBREV
;;;----------------------------------------------------------------

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

;;;----------------------------------------------------------------
;; ** HIPPIE-EXPAND
;;;----------------------------------------------------------------
;; Supercharge the way hippie-expand behaves, expand as little as
;; possible
(setq hippie-expand-try-functions-list 
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;;----------------------------------------------------------------
;; ** COMPANY-MODE
;;;----------------------------------------------------------------
(use-package company
  :disabled
  :ensure t
  :defer 3
  :general
  ("M-s <tab>"      'company-yasnippet)
  
  (:keymaps   'company-active-map
  "C-p"       nil
  "C-n"       nil
  "C-;"       'company-other-backend
  "C-w"       nil
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
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        ;;company-tooltip-flip-when-above t
        ;; company-transformers '(company-sort-by-occurrence)
        ;; company-transformers '(company-sort-by-backend-importance)
        ;; company-transformers '(company-sort-by-statistics)
        company-global-modes '(latex-mode matlab-mode emacs-lisp-mode lisp-interaction-mode
                                          python-mode sh-mode fish-mode conf-mode text-mode org-mode)
        company-auto-commit nil
        company-backends '((company-files company-capf))) ;;company-keywords
  (setq tab-always-indent 'complete)
  
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (make-local-variable 'company-idle-delay)
                               (setq-local company-idle-delay 0.5)))
  
  (use-package eldoc
    :config
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort))
  
  (use-package company-tng
    :ensure company
    :config (company-tng-mode))
  
  (global-company-mode))

;; Not needed. the capf backend handles completion just fine
(use-package company-auctex
  :disabled
  :defer t
  :config
  (add-to-list 'company-backends 'company-auctex)
  (company-auctex-init))

(use-package company-prescient
  :disabled
  :after company
  :defer 3
  ;; :ensure t
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

;;;----------------------------------------------------------------
;; ** CORFU + CAPE
(load-library "setup-corfu")
;; ** YASNIPPET
;;;----------------------------------------------------------------

(use-package yasnippet
  :ensure t
  ;; :defer 5
  ;; :after warnings
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :config
  ;; (use-package yasnippet-snippets
  ;;   :ensure t)
  ;; (yas-reload-all)
  ;; Redefine yas expand key from TAB because company-mode uses TAB.

  ;; (push '(yasnippet backquote-change) warning-suppress-types)

  ;; Don't throw a warning if lisp code in a snippet modifies the
  ;; buffer. We need this for auto expanded snippets in latex/org.

  (let ((ydus yas--default-user-snippets-dir))
    (and (member ydus yas-snippet-dirs)
         (yas-load-directory ydus)))

  (setq yas-wrap-around-region t
        yas-triggers-in-field t)

  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  (add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets)

(with-eval-after-load 'company
  ;; (defun my/yas-company-next-field ()
  ;;     "company-complete-common or yas-next-field-or-maybe-expand."
  ;;     (interactive)
  ;;     (if company-candidates (company-complete-common)
  ;;       (yas-next-field-or-maybe-expand)))

  ;;   (define-key yas-keymap [tab] #'my/yas-company-next-field)
  ;;   (define-key yas-keymap (kbd "TAB") #'my/yas-company-next-field)

;;;###autoload
    (defun my/yas-company-cancel ()
      "company-abort or yas-abort-snippet."
      (interactive)
      (if company-candidates
          (company-abort)
        (yas-abort-snippet)))

    (define-key yas-keymap (kbd "C-g") #'my/yas-company-cancel))


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

(use-package yasnippet-snippets
  :ensure t)

(use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change) warning-suppress-types
                :test 'equal))

(use-package yasnippet
  :defer
  :config
  (use-package cdlatex
    :if (featurep 'cdlatex)
    :hook ((cdlatex-tab . yas-expand)
           (cdlatex-tab . cdlatex-in-yas-field))
    :bind (:map yas-keymap
                ("TAB" . yas-next-field-or-cdlatex)
                ([tab] . yas-next-field-or-cdlatex))
    :config
    ;; Allow cdlatex tab to work inside Yas fields
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    ;; (add-hook 'cdlatex-tab-hook #'yas-expand)
    ;; (add-hook 'cdlatex-tab-hook #'cdlatex-in-yas-field)
    ;; (define-key yas-keymap (kbd "TAB")
    ;; (define-key yas-keymap [tab] 'yas-next-field-or-cdlatex)
    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))



;;;----------------------------------------------------------------
;; ** M O V E C
;;;----------------------------------------------------------------
(load-library "setup-marginalia")
(load-library "setup-orderless")
(load-library "setup-vertico")
(load-library "setup-embark")
(load-library "setup-consult")

;;;----------------------------------------------------------------
;; *** MARGINALIA
;;;----------------------------------------------------------------

;; #+INCLUDE: "./lisp/setup-marginalia.org" :minlevel 2
;;;----------------------------------------------------------------
;; *** ORDERLESS
;;;----------------------------------------------------------------

;; #+INCLUDE: "./lisp/setup-orderless.org" :minlevel 2
;;;----------------------------------------------------------------
;; *** VERTICO
;;;----------------------------------------------------------------

;; #+INCLUDE: "./lisp/setup-vertico.org" :minlevel 2
;;;----------------------------------------------------------------
;; *** EMBARK
;;;----------------------------------------------------------------

;; #+INCLUDE: "./lisp/setup-embark.org" :minlevel 2
;;;----------------------------------------------------------------
;; *** CONSULT
;;;----------------------------------------------------------------

;; #+INCLUDE: "./lisp/setup-consult.org" :minlevel 2

;; *** ELMO

;; Embark-live-mode
(use-package elmo
  :disabled
  :load-path "plugins/elmo/"
  :commands elmo-mode
  :after embark
  :init (elmo-mode 1)
  :bind (:map elmo-minibuffer-local-completion-map
         ("C-<tab>" . #'embark-act-with-completing-read))
  :config
  (setq elmo-always-show-list
        '(consult-line consult-outline 
          ;; consult-line-symbol-at-point 
          consult-completion-in-region             
          consult-imenu consult-imenu-all
          consult-xref consult-org-heading
          embark-completing-read-prompter
          embark-act-with-completing-read
          embark-act
          embark-prefix-help-command
          consult-yank-pop)))

;;;----------------------------------------------------------------
;; ** +IVY COUNSEL SWIPER+
;;;----------------------------------------------------------------
(use-package setup-ivy :disabled)

;;; Bibtex management from ivy. Call ivy-bibtex.
(use-package ivy-bibtex
  :disabled
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
  :disabled
  :after ivy
  :general
  (:keymaps 'space-menu-map
    "Y" '(ivy-youtube :wk "Youtube search"))
  :config
  (setq ivy-youtube-history-file
        (dir-concat user-cache-directory "ivy-youtube-history"))
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

;;;----------------------------------------------------------------

;; * APPLICATIONS
;; ** DIRED
;;;----------------------------------------------------------------
(require 'setup-dired nil t)
(use-package emacs
  :after dired
  :config
  (use-package ffmpeg-crop
    :load-path "plugins/ffmpeg-crop/"
    :commands (ffmpeg-crop ffmpeg-crop-dired)))

;; ** ERC
(use-package erc
  :commands (erc-tls erc)
  :bind (:map erc-mode-map
         ("\M-(" . insert-parentheses-sentence))
  :config
  (setq erc-server "irc.karthinks.com"
        erc-port 7078
        erc-nick "karthik"
        erc-user-full-name "Karthik"
        erc-prompt-for-password nil
        ;; erc-track-shorten-start 6
        ;; erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters"
        ;;                                "#org-mode" "#emacs"))
        erc-kill-buffer-on-part t
        erc-lurker-threshold-time 1800
        erc-hide-list '("NICK")
        erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477")
        ;; erc-track-visibility t
        ;; erc-track-when-inactive t
        erc-format-nick-function 'erc-format-@nick
        erc-auto-query 'bury
        erc-keywords nil))

(use-package erc-image
  :disabled
  :after erc
  :ensure
  :init
  (setq erc-image-inline-rescale 350)
  (add-to-list 'erc-modules 'image)
  (push 'button erc-modules)
  (push 'completion erc-modules)
  (erc-update-modules)
  :config
  (defun erc-image-create-image (file-name)
    "Create an image suitably scaled according to the setting of
'ERC-IMAGE-RESCALE."
    (let* ((positions (window-inside-absolute-pixel-edges))
           (width (- (nth 2 positions) (nth 0 positions)))
           (height (- (nth 3 positions) (nth 1 positions)))
           (image (create-image file-name))
           (dimensions (image-size image t))
           (imagemagick-p (and (fboundp 'imagemagick-types) 'imagemagick)))
                                        ; See if we want to rescale the image
      (if (and erc-image-inline-rescale
               (not (image-multi-frame-p image)))
          ;; Rescale based on erc-image-rescale
          (cond (;; Numeric: scale down to that size
                 (numberp erc-image-inline-rescale)
                 (let ((max-height (min (cdr dimensions)
                                        erc-image-inline-rescale
                                        (floor (* width (cdr dimensions))
                                               (car dimensions)))))
                   (if (> (floor (* max-height (car dimensions))
                                 (cdr dimensions))
                          width)
                       (create-image file-name imagemagick-p nil :width width)
                     (create-image file-name imagemagick-p nil :height max-height))))
                (;; 'window: scale down to window size, if bigger
                 (eq erc-image-inline-rescale 'window)
                 ;; But only if the image is greater than the window size
                 (if (or (> (car dimensions) width)
                         (> (cdr dimensions) height))
                     ;; Figure out in which direction we need to scale
                     (if (> width height)
                         (create-image file-name imagemagick-p nil :height  height)
                       (create-image file-name imagemagick-p nil :width width))
                   ;; Image is smaller than window, just give that back
                   image))
                (t (progn (message "Error: none of the rescaling options matched") image)))
        ;; No rescale
        image))))

(use-package erc-hl-nicks
  :ensure
  :after erc
  :hook (erc-mode . erc-hl-nicks-mode))


;; ** EMAIL
;;;----------------------------------------------------------------

;; Left unchecked, every program grows to the point where it can be
;; used to manage your email.
(load-library "setup-email")

;;;----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-email.org" :minlevel 3
;;;----------------------------------------------------------------

;;;----------------------------------------------------------------
;; ** ELFEED
;;;----------------------------------------------------------------

(load-library "setup-elfeed")

;;;----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-elfeed.org" :minlevel 2
;;;----------------------------------------------------------------

;;;----------------------------------------------------------------
;; ** EWW
;;;----------------------------------------------------------------
(require 'setup-eww)

;;;----------------------------------------------------------------
;; ** NOV.EL
;;;----------------------------------------------------------------
(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook ((nov-mode . my/nov-font-setup)
         (nov-mode . er/add-text-mode-expansions))
  :config
  (setq nov-text-width 80
        nov-save-place-file (dir-concat user-cache-directory "nov-places"))
  (defun my/nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Noto Serif"
                             :height 1.0)))


;;;################################################################

;; ** LOOK UP

;; *** GOOGLE ANSWERS
;; Query Google's knowledge graph. This is the answer that shows up before the
;; first result in Google searches. For this purpose we use tuxi, an external
;; tool that queries Google.
(use-package emacs
  :config
  (defvar google-search-history nil
    "List of queries to google-search-string.")
  (defun google-search-string (search-string)
    "Read SEARCH-STRING from the minibuffer and call the shell
command tuxi on it."
    (interactive (list (read-string "Google: " nil
                                    google-search-history
                                    (thing-at-point 'sexp))))
    (unless (executable-find "tuxi")
      (user-error "Cannot find shell command: tuxi"))
    (let ((search-output (string-trim-right
                          (shell-command-to-string
                           (concat
                            "tuxi -r "
                            (shell-quote-argument search-string))))))
      (with-current-buffer (get-buffer-create "*Tuxi Output*")
        (goto-char (point-max))
        (unless (bobp) (insert "\n\n* * *\n"))
        (insert (capitalize search-string) ":\n\n")
        (push-mark)
        (insert search-output)
        (let ((lines (count-lines (or (mark) (point-min)) (point-max))))
          (if (<= lines 1)
              (message search-output)
            (let ((win (display-buffer (current-buffer))))
              (set-window-start win (mark))
              (set-window-parameter win 'window-height (min lines 10))
              (goto-address-mode 1)))))))
  (defun google-search-at-point (&optional beg end)
    "Call the shell command tuxi on the symbol at point. With an
active region use it instead."
    (interactive "r")
    (if-let ((search-string (if (use-region-p)
                                (buffer-substring-no-properties beg end)
                              (thing-at-point 'symbol))))
        (google-search-string search-string)
      ;; (message "No symbol to search for at point!")
      (call-interactively #'google-search-string)))
  :bind (:map help-map
              ("g" . google-search-string)
              ("C-=" . google-search-at-point)))

;; *** DICTIONARY
(use-package sdcv
  :disabled
  :ensure nil
  :commands (sdcv-search-input)
  :bind (("C-x M-=" . sdcv-search-input)
         :map sdcv-mode-map
              ("M-n" . sdcv-next-dictionary)
              ("M-p" . sdcv-previous-dictionary)))

(use-package dictionary
  :ensure t
  :commands (dictionary-lookup-definition dictionary-search)
  :config
  (define-key help-map (kbd "C-d") 'apropos-documentation)
  (setq dictionary-use-single-buffer t)
  (defun dictionary-search-dwim (&optional arg)
    "Search for definition of word at point. If region is active,
search for contents of region instead. If called with a prefix
argument, query for word to search."
    (interactive "P")
    (if arg
        (dictionary-search nil)
      (if (use-region-p)
          (dictionary-search (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
        (if (thing-at-point 'word)
            (dictionary-lookup-definition)
          (dictionary-search-dwim '(4))))))
  :bind (("C-M-=" . dictionary-search-dwim)
         :map help-map
         ("=" . dictionary-search-dwim)
         ("d" . dictionary-search)))

;; * PROJECTS
(load-library "setup-project")

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-project.org" :minlevel 2
;; ---------------------------

;;;----------------------------------------------------------------
;; ** RG, GREP AND WGREP
(use-package rg
  :disabled ;;consult-ripgrep handles this
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")

  (rg-define-search my/rg-vc-or-dir
    "RipGrep in project root or present directory."
    :query ask
    :format regexp
    :files "everything"
    :dir (or (project-root (project-current))
             (vc-root-dir)              ; search root project dir
             default-directory)         ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (rg-define-search my/rg-ref-in-dir
    "RipGrep for thing at point in present directory."
    :query point
    :format regexp
    :files "everything"
    :dir default-directory
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun my/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind (("M-s M-g" . my/rg-vc-or-dir)
         ("M-s M-." . my/rg-ref-in-dir)
         :map rg-mode-map
         ("s" . my/rg-save-search-as-name)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))

(use-package wgrep
  :ensure t
  :commands wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode))
  :general
  (:states '(normal visual motion)
   :keymaps '(grep-mode-map rg-mode-map)
           "i" 'wgrep-change-to-wgrep-mode))

;; * VISUALS AND PRESENTATION
;; ** MONOCLE-MODE
(use-package emacs
  :bind (("H-m" . my/monocle-mode)
         ("C-x C-m" . my/monocle-mode))
  :config 
  (defvar my/window-configuration nil
    "Current window configuration.
Intended for use by `my/monocle-mode.")

  (define-minor-mode my/monocle-mode
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    :lighter " [M]"
    :global nil
    (let ((win my/window-configuration))
      (if (one-window-p)
          (when win
            (set-window-configuration win))
        (setq my/window-configuration (current-window-configuration))
        (when (window-parameter nil 'window-slot)
            (let ((buf (current-buffer)))
              (other-window 1)
              (switch-to-buffer buf)))
        (delete-other-windows)))))
;; ** MIXED-PITCH-MODE
;;;----------------------------------------------------------------
(use-package mixed-pitch
  :defer
  :ensure t
  :config (add-to-list 'mixed-pitch-fixed-pitch-faces 'line-number))

;;;----------------------------------------------------------------
;; ** OLIVETTI
(use-package olivetti
  :commands (my/olivetti-mode)
  :ensure t
  :config
  (setq olivetti-body-width 0.8
        olivetti-minimum-body-width 80
        olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode my/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.

Fringes are disabled. The modeline is hidden, except for
`prog-mode' buffers (see `my/mode-line-hidden-mode'). The default
typeface is set to a proportionately spaced family, except for
programming modes (see `my/variable-pitch-mode'). The cursor
becomes a blinking bar. Evil-mode (if bound) is disabled."
    :init-value nil
    :global nil
    (if my/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (unless (derived-mode-p 'prog-mode)
            (my/mode-line-hidden-mode 1)
            (mixed-pitch-mode 1))
          (if (bound-and-true-p evil-mode)
              (evil-emacs-state))
          (setq-local cusor-type '(bar . 2)))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (mixed-pitch-mode -1)
      (unless (derived-mode-p 'prog-mode)
        (my/mode-line-hidden-mode -1))
      (when (and (bound-and-true-p evil-mode)
                 (evil-emacs-state-p))
        (evil-exit-emacs-state))
      (kill-local-variable 'cursor-type)))

  (define-minor-mode my/reader-mode
    "Mode to read a buffer in style. Pop it out into a frame,
turn on `view-mode', and `my/olivetti-mode', which in turn hides
the mode-line and switches to `variable-pitch-mode'."
    :init-value
    :global-nil
    (if my/reader-mode
        (progn
          (make-frame '((name . "dropdown_reader")))
          (my/olivetti-mode 1)
          (view-mode 1)
          (if (equal major-mode 'org-mode)
              (org-show-all)))
      (view-mode -1)
      (my/olivetti-mode -1)
      (delete-frame)))

  :bind
  ("C-c O" . my/olivetti-mode)
  ("C-c R" . my/reader-mode))


;; ** PRESENTATION (BIG) MODE
(use-package presentation
  :ensure t
  :commands presentation-mode
  :config
  (setq presentation-default-text-scale 1.50
        presentation-mode-lighter " BIG"
        presentation-keep-last-text-scale nil))
;; ** SCREENCAST
;; Presentation-mode will embiggen everything. Keycast-mode shows the keys being
;; pressed. Gif-screencast will screenshot each user action and compile them
;; into a gif.
(use-package keycast
  :ensure t
  :commands keycast-mode
  :config
  (setq keycast-separator-width 1)
  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing!")))
  
  (defun store-action-key+cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd)
    cmd)

  (defun store-action-key-no-cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd))
  
  (defun keycast-capture-avy-dispatch (char)
    (if-let ((cmd (assoc char avy-dispatch-alist)))
        (setq keycast--this-command-keys (make-vector 1 char)
              keycast--this-command (cdr cmd))))
  
  (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)
  ;; (advice-add 'avy-goto-char-timer :filter-return #'store-action-key+cmd)
  (advice-add 'avy-handler-default :before #'keycast-capture-avy-dispatch)

  (defun force-keycast-update (&rest _)
    (force-mode-line-update t))

  (dolist (cmd '(embark-act embark-become))
    (advice-add cmd :before #'force-keycast-update)))

;; Using a screen recorder instead. gif-screencast misses keystrokes.
(use-package gif-screencast
  :disabled 
  :commands (gif-screencast gif-screencast-stop)
  :config
  (define-minor-mode my/screencast-mode
    "Minor mode to record screencasts from emacs."
    :global t
    :init-value nil
    (if my/screencast-mode
        (progn
          (menu-bar-mode -1)
          (presentation-mode 1)
          (keycast-mode 1)
          (gif-screencast))
      (gif-screencast-stop)
      (keycast-mode -1)
      ;; (menu-bar-mode +1)
      (presentation-mode -1)))
  :bind
  ("C-c S" . my/screencast-mode))

;; * NAVIGATION
(use-package emacs
  :config
  (setq view-read-only t))

;;;################################################################
;; * MODELINE
;;;################################################################

(define-minor-mode my/mode-line-hidden-mode
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if my/mode-line-hidden-mode
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      (force-mode-line-update)))

;;----------------------------------------------------------------
;; ** +EXPERIMENTAL MODELINES+
;;----------------------------------------------------------------

;; A few custom modelines I've tried in the past only to rediscover the merits
;; of the original design.

(use-package telephone-line
  :disabled
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1))

(use-package spaceline
  :disabled
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'contour
        spaceline-buffer-encoding-abbrev-p nil
        spaceline-buffer-size-p nil
        spaceline-line-column-p t)
  (spaceline-emacs-theme))

(use-package doom-modeline
  :disabled
  :init (doom-modeline-mode 1))

;;----------------------------------------------------------------
;; ** SMART MODE LINE
;;----------------------------------------------------------------

;; Smart mode line hews close to Emacs' default modeline set up. The only change
;; we make is to disable display of the global-mode-string when on Emacs 28 or
;; higher, we show this info in the less crowded tab-bar instead.

(use-package smart-mode-line
  :ensure t
  :commands sml/setup
  :init
  (setq sml/theme nil)
  (sml/setup)
  (unless (version< emacs-version "28.0")
    (setq mode-line-misc-info
          '((which-function-mode
            (which-func-mode
             ("" which-func-format " ")))
           ;; (global-mode-string
           ;;  ("" global-mode-string))
            ))))

;; Some advice to add support for Evil to smart-mode-line, long since
;; deprecated.

(use-package smart-mode-line
  :defines sml/fix-mode-line-a
  :disabled
  :config
    
  (defun sml/fix-mode-line-a (_theme &rest _args)
    "Advice to `load-theme' to fix the mode-line height after activating/deactivating theme"
    (set-face-attribute 'mode-line nil
                        :box `(:line-width 3 :color ,(plist-get
                                                      (custom-face-attributes-get 'mode-line nil)
                                                      :background))))

  (advice-add 'disable-theme :after #'sml/fix-mode-line-a)
  (advice-add 'load-theme :after #'sml/fix-mode-line-a)

  (custom-set-faces
   '(mode-line ((t (:box (:line-width 4 :color ))))))

          (lexical-let ((default-color (cons (face-background 'mode-line)
                                             (face-foreground 'mode-line))))
            (add-hook 'post-command-hook
                      (lambda ()
                        (let ((color (cond ((minibufferp) default-color)
                                           ((evil-insert-state-p) '("DarkGoldenrod2" . "black"))
                                           ((evil-emacs-state-p)  '("SkyBlue2" . "black"))
                                           ;; ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                           (t default-color))))
                          (set-face-background 'mode-line (car color))
                          (set-face-foreground 'mode-line (cdr color)))))))
    
;; ** MINOR MODE HIDING

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generally unhelpful and only add confusing visual clutter.
(setq mode-line-default-help-echo nil
      show-help-function nil)

(defvar mode-line-cleaner-alist
  `((company-mode . " ⇝")
    (corfu-mode . " ⇝")
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
    (dot-mode . "")
    (scheme-mode . " SCM")
    (matlab-mode . "M")
    (org-mode . " ORG";; "⦿"
              )
    (valign-mode . "")
    (eldoc-mode . "")
    (org-cdlatex-mode . "")
    (org-indent-mode . "")
    (org-roam-mode . "")
    (visual-line-mode . "")
    (latex-mode . "TeX")
    ;; (projectile-mode . " ϸ")
    (outline-minor-mode . " [o]";; " ֍"
                        )
    (hs-minor-mode . "")
    (matlab-functions-have-end-minor-mode . "")
    (org-roam-ui-mode . " UI")
    ;; Evil modes
    (evil-traces-mode . "")
    (latex-extra-mode . "")
    (strokes-mode . "")
    (flymake-mode . "fly")
    (god-mode . ,(propertize "God" 'face 'success))
    (gcmh-mode . ""))
  "Alist for `clean-mode-line'.

  ; ;; When you add a new element to the alist, keep in mind that you
  ; ;; must pass the correct minor/major mode symbol and a string you
  ; ;; want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
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

;; (display-time-mode 1)

(use-package custom
  ;; :general
  :commands my/toggle-theme
  :config
  (setq custom-theme-directory (expand-file-name "lisp" user-emacs-directory))

  (defun my/toggle-theme (theme)
    "Swap color themes. With prefix arg, don't disable the
currently loaded theme first."
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

;;;################################################################
;; * MINIBUFFER
;;;################################################################
(use-package minibuffer
  :config
 (require 'setup-minibuffer nil t))

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;################################################################
;; * FONTS AND COLORS
;;;################################################################
(use-package cus-face
  :config
  (cond (IS-LINUX
         (set-fontset-font t 'unicode "Symbola" nil 'prepend)
         (custom-set-faces
          '(variable-pitch ((t (:family "Ubuntu" :height 110))))
          ;; '(default ((t (:family "Ubuntu Mono" :foundry "PfEd"
          ;;                        :slant normal :weight normal
          ;;                        :height 125 :width normal))))
          ;; '(default ((t (:family "Iosevka" :foundry "PfEd"
          ;;                        :slant normal :weight normal
          ;;                        :height 110 :width normal))))
          '(default ((t (:family "FantasqueSansMono" :foundry "PfEd"
                                 :slant normal :weight normal
                                 :height 120 :width normal))))
          ))
        (IS-WINDOWS
         (custom-set-faces
          '(default ((t (:family "Consolas" :foundry "outline"
                                 :slant normal :weight normal
                                 :height 120 :width normal)))))))

  ;; (custom-set-faces  '(region ((t (:inverse-video t))))
  ;;                    '(font-lock-comment-face ((t (:foreground "IndianRed3")))))
  ;; (add-to-list 'default-frame-alist '(alpha 96 90))

  (use-package dracula-theme
    :disabled
    :defer
    :config
    (custom-theme-set-faces 'dracula
                            '(aw-background-face
                              ((t (:background "#282a36" :inverse-video nil :weight normal))))
                            '(aw-leading-char-face
                              ((t (:foreground "#bd93f9" :height 2.5 :weight normal))))))

  (use-package dichromacy-theme
    :disabled
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
                            )))

;;   (use-package gruvbox-theme
;;     :disabled
;;     :defer
;;     :config
;;     (custom-theme-set-faces 'gruvbox
;;                             '(aw-leading-char-face
;;                               ((t (:height 2.5 :weight normal))))
;;                             '(org-level-1 ((t (:height 1.3 :foreground "#83a598" :inherit (bold) ))))
;;                             '(org-level-2 ((t (:height 1.1 ;; 

;; Protesilaos Stavrou's excellent high contrast themes, perfect for working in
;; bright sunlight (especially on a dim laptop screen).
(use-package modus-themes
  ;; :ensure
  :defer
  ;; :config
  ;; (custom-set-faces '(aw-background-face ((t nil))))
  ;; (custom-set-faces '(aw-leading-char-face ((t nil))))
  ;; (custom-set-faces '(org-level-1 ((t nil))))
  ;; (custom-set-faces '(org-level-2 ((t nil))))
  ;; (custom-set-faces '(org-level-3 ((t nil))))
  ;; (custom-set-faces '(tab-bar-tab ((t nil))))
  ;; (custom-set-faces '(tab-bar-tab-inactive ((t nil))))
  ;; (custom-set-faces '(tab-bar ((t nil))))
  ;; (custom-set-faces '(hl-line ((t nil))))
  ;; (custom-set-faces '(default ((t nil))))
  :init
  (setq modus-themes-org-blocks nil
        modus-themes-intense-hl-line t
        modus-themes-org-blocks 'grayscale
        modus-themes-fringes 'subtle
        modus-themes-scale-headings t
        modus-themes-section-headings nil
        modus-themes-variable-pitch-headings nil
        modus-themes-intense-paren-match t
        modus-themes-bold-constructs t
        modus-themes-completions 'opinionated
        modus-themes-diffs 'desaturated ;'fg-only-deuteranopia
        modus-themes-syntax nil ;'faint
        modus-themes-links '(faint neutral-underline)
        modus-themes-hl-line '(intense)
        modus-themes-prompts '(bold background)
        modus-themes-mode-line '(accented borderless)
        ;; modus-themes-org-habit 'simplified
        modus-themes-subtle-line-numbers t
        modus-themes-tabs-accented t
        modus-themes-inhibit-reload t
        modus-themes-paren-match '(underline)
        modus-themes-region '(no-extend accented)
        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch scale-title))
          (header-date . (bold-today grayscale scale))
          (scheduled . rainbow)
          (habit . traffic-light-deuteranopia))
        modus-themes-headings  '((t . (background overline rainbow)))
        modus-themes-variable-pitch-ui nil
        modus-themes-scale-headings t)
  ;; (setq modus-themes-operandi-color-overrides
  ;;       '((bg-main . "#ededed")))
  (setq modus-themes-vivendi-color-overrides
        '((bg-main . "#100b17")
          (bg-dim . "#161129")
          (bg-alt . "#181732")
          (bg-hl-line . "#191628")
          (bg-active . "#282e46")
          (bg-inactive . "#1a1e39")
          (bg-region . "#393a53")
          (bg-header . "#202037")
          (bg-tab-bar . "#262b41")
          (bg-tab-active . "#120f18")
          (bg-tab-inactive . "#3a3a5a")
          (fg-unfocused . "#9a9aab"))))


;; Henrik Lissner's Doom themes are a mainstay, mostly doom-rouge:
;;
;; [[file:/img/dotemacs/doom-rouge-demo.png]]
(use-package doom-themes
  :ensure t
  :defer
  :init
  (defun my/doom-theme-settings (theme &rest args)
    "Additional face settings for doom themes"
    (when (member theme '(doom-iosvkem doom-rouge))
      (dolist (face-spec
               '((aw-background-face (:background "#061229" :inverse-video nil :weight normal)
                                     ace-window)
                 (org-level-1        (:height 1.20 :inherit outline-1) org)
                 (org-level-2        (:height 1.15 :inherit outline-2) org)
                 (org-level-3        (:height 1.10 :inherit outline-3) org)
                 (hl-line            (:background "#1f2a3f") hl-line)
                 (tab-bar            (:background "black" :height 1.0 :foreground "white")
                                     tab-bar)
                 (tab-bar-tab
                  (:foreground "#B16E75" :bold t :height 1.10 :background "#172030")
                  tab-bar)
                 (tab-bar-tab-inactive
                  (:inherit 'mode-line-inactive :height 1.10 :background "black")
                  tab-bar)))
        (cl-destructuring-bind (face spec library) face-spec
          (if (featurep library)
              (apply #'set-face-attribute face nil spec)
            (with-eval-after-load library
              (when (member 'doom-rouge custom-enabled-themes)
                  (apply #'set-face-attribute face nil spec))))))))

  (advice-add 'load-theme :after #'my/doom-theme-settings)
  
  :config
  (doom-themes-org-config)
  (use-package doom-rouge-theme
    :config
    (setq doom-rouge-padded-modeline t))

  (use-package doom-iosvkem-theme
    :disabled
    ;; :custom-face
    ;; (default ((t (:background "#061229"))))
    :config
    (setq doom-Iosvkem-brighter-comments nil
          doom-Iosvkem-comment-bg nil
          doom-Iosvkem-brighter-modeline nil)))

(use-package moody
  :disabled
  :after (modus-themes smart-mode-line)
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-sml/mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;;################################################################
;; * EVIL-MODE
;;;################################################################
;;(require 'setup-evil)

;;;################################################################
;; * MISC SETTINGS
;;;################################################################
;; Settings that I'm not sure where to put:
(use-package shr
  :defer
  :config
  (setq shr-image-animate nil
        shr-width 66))

(use-package url
  :defer
  :config
  (setq url-configuration-directory (dir-concat user-cache-directory "url/")))

(use-package request
  :defer
  :config
  (setq request-storage-directory (dir-concat user-cache-directory "request/")))

(use-package semantic
  :defer
  :config
  (setq semanticdb-default-save-directory (dir-concat user-cache-directory "semanticdb/")))

(use-package srecode
  :defer
  :config
  (setq srecode-map-save-file (dir-concat user-cache-directory "srecode-map.el")))

;; * LOCAL-VARIABLES
;; Local Variables:
;; outline-regexp: ";; \\*+"
;; page-delimiter: ";; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; no-native-compile: t
;; End:
