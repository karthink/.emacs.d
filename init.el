;; -*- lexical-binding: t -*-

;; #+options: prop:t
;; #+begin_quote
;;                                       my dot emacs grows
;;
;;                                       one day i look inside it
;;
;;                                       singularity
;; #+end_quote

;; * PACKAGE MANAGEMENT

;; ** ELPACA
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" "~/.local/share/git/"))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; (push 'transient elpaca-ignored-dependencies)
;; (setq elpaca-ignored-dependencies
;;       (delete 'seq elpaca-ignored-dependencies))
;; (push 'transient elpaca-ignored-dependencies)
(push 'notmuch elpaca-ignored-dependencies)
(push 'elnode elpaca-ignored-dependencies)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(elpaca-wait)

(use-package elpaca-ui
  :bind (:map elpaca-ui-mode-map
         ("p" . previous-line)
         ("F" . elpaca-ui-mark-pull))
  :after popper
  :init
  (add-to-list 'popper-reference-buffers
               'elpaca-log-mode)
  (setf (alist-get '(major-mode . elpaca-log-mode)
                   display-buffer-alist
                   nil nil #'equal)
        '((display-buffer-at-bottom
          display-buffer-in-side-window)
          (side . bottom)
          (slot . 49)
          (window-height . 0.4)
          (body-function . select-window))))

;; ;; ** STRAIGHT!
;; (defvar bootstrap-version)
;; ;; cache directory
;; (defconst user-repos-directory "~/.local/share/git/"
;;   "Location where cloned repos are stored.")
;; (defconst user-build-directory
;;   (if (= emacs-major-version 29) "build29" "build"))
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (unless (file-directory-p user-repos-directory)
;;     (make-directory user-repos-directory t))
;;   (setq straight-base-dir user-repos-directory
;;         straight-build-dir user-build-directory)
;;   (setq straight-check-for-modifications '(find-when-checking
;;                                            check-on-save)
;;         straight-vc-git-default-clone-depth 2)
;;   (load bootstrap-file nil 'nomessage))

;; (setf (alist-get 'stable straight-profiles)
;;       (expand-file-name "stable.el" user-emacs-directory))

;;; ** PACKAGE.EL
;;; "Activate" packages, /i.e./ autoload commands, set paths, info-nodes and so
;;; on. Set load paths for ELPA packages. This is unnecessary in Emacs 27 with an
;;; early-init.el, but I haven't checked.
;;; (package-initialize)
;;; (package-activate-all)
;;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ; 
;;; Package repositories that are no longer used or included by default:
;;; #+begin_src emacs-lisp
;;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;;; #+end_src
  ; 
;;; This is a workaround for a bug that's probably been fixed by now!
;;; #+begin_src emacs-lisp
;;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;; #+end_src

;; ** USE PACKAGE

;; =use-package= is a neat wrapper over =with-eval-after-load= and =require=, a
;; judicious combination of which helps with lazy loading code. It does a lot
;; more besides, like simplify code to add hooks, bind keys and generate
;; autoloads.
;;
;; The one thing it's not is a package manager!

;;; (unless (package-installed-p 'use-package)
;;;   (package-refresh-contents)
;;;   (package-install 'use-package))
;; (straight-use-package 'use-package)

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        ;use-package-ignore-unknown-keywords t
        use-package-minimum-reported-time 0.01
        use-package-expand-minimally t
        use-package-enable-imenu-support t)
  (when init-file-debug
    (setq use-package-expand-minimally nil
          use-package-verbose t
          use-package-compute-statistics t
          debug-on-error t))
  (require 'use-package))

;;; (require 'bind-key)

;;; (use-package package
;;;   :hook (package-menu-mode . hl-line-mode))

;; * PATHS

;; I avoid defining too many custom helpers, =dir-concat= is an exception. Emacs
;; 28 provides =file-name-concat=, but I'm on 27.2 some of the time.
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
      :ensure t
      ;; :hook (after-init . gcmh-mode)
      :config
      (defun gcmh-register-idle-gc ()
        "Register a timer to run `gcmh-idle-garbage-collect'.
Cancel the previous one if present."
        (unless (eq this-command 'self-insert-command)
          (let ((idle-t (if (eq gcmh-idle-delay 'auto)
		            (* gcmh-auto-idle-delay-factor gcmh-last-gc-time)
		          gcmh-idle-delay)))
            (if (timerp gcmh-idle-timer)
                (timer-set-time gcmh-idle-timer idle-t)
              (setf gcmh-idle-timer
	            (run-with-timer idle-t nil #'gcmh-idle-garbage-collect))))))
      (setq gcmh-idle-delay 'auto  ; default is 15s
            gcmh-high-cons-threshold (* 32 1024 1024)
            gcmh-verbose nil
            gc-cons-percentage 0.2)
      (gcmh-mode 1))
  (error (setq gc-cons-threshold (* 16 1024 1024)
               gc-cons-percentage 0.2)))

;; setup-core is the first of many concerns to be shunted into its own file.
;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-core.org"
;; ----------------------------------------------------------------

;; * DAEMON
;;;################################################################

;; Hack: When starting a server, silently load all the "heavy" libraries and
;; goodies I use. There are more elegant approaches, such as incremental
;; deferring, but this is good enough. A regular (non-daemon) Emacs session
;; still launches in ~0.3 seconds.
(when (daemonp)
  (defvar pulse-flag t)
  (add-hook
   'after-init-hook
   (defun my/load-packages-eagerly ()
     (add-hook 'server-visit-hook
               (lambda () (when (and (equal default-directory
                                       temporary-file-directory)
                                (equal major-mode 'text-mode)
                                (fboundp 'markdown-mode))
                       (markdown-mode))))
     (run-at-time 1 nil
                  (lambda () 
                    (when (fboundp 'pdf-tools-install) (pdf-tools-install t))
                    (load-library "pulse")
                    (when (string-suffix-p "server" server-name)
                      (let ((after-init-time (current-time)))
                         (dolist (lib '("org" "ob" "ox" "ol" "org-roam"
                                       "org-capture" "org-agenda" "org-fragtog"
                                       "org-gcal" "latex" "reftex" "cdlatex"
                                       "consult" "helpful" "elisp-mode"
                                       "notmuch" "elfeed" "simple"
                                       "expand-region" "embrace"
                                       "ace-window" "avy" "yasnippet"
                                       "magit" "modus-themes" "diff-hl"
                                       "dired" "ibuffer" "pdf-tools"
                                       "emacs-wm"))
                          (with-demoted-errors "Error: %S" (load-library lib)))
                         (with-temp-buffer (org-mode))
                        (let ((elapsed (float-time (time-subtract (current-time)
                                                                  after-init-time))))
                          (message "[Pre-loaded packages in %.3fs]" elapsed)))))))))

;;;################################################################
;; * PERSONAL INFO
;;;################################################################
(with-demoted-errors "Error (personal info): %S"
  (load-library "personal")
  (setq user-full-name my-full-name)
  (setq user-mail-address my-email-address))

(use-package org
  :defer
  :ensure `(org
            :remotes ("tecosaur"
                      :repo "https://git.tecosaur.net/tec/org-mode.git"
                      :branch "dev")
            :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi"))
            :build t
            :pre-build
            (progn
              (with-temp-file "org-version.el"
               (require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                (insert
                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                 "(provide 'org-version)\n")))
              (require 'elpaca-menu-org)
              (elpaca-menu-org--build))
            :pin nil))

;;;################################################################
;; * MODELINE
;;;################################################################
(load (expand-file-name "lisp/setup-modeline" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-modeline.org" :minlevel 2
;; ----------------------------------------------------------------

;;;################################################################
;; * MINIBUFFER
;;;################################################################
(use-package minibuffer
  :config
  (load (expand-file-name "lisp/setup-minibuffer" user-emacs-directory)))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-minibuffer.org" :minlevel 2
;; ---------------------------

;;;################################################################
;; * UI
;;;################################################################

;; Miscellaneous UI preferences.
(load (expand-file-name "lisp/setup-ui" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-ui.org"
;; ----------------------------------------------------------------

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
(load (expand-file-name "lisp/setup-keybinds" user-emacs-directory))

(elpaca-wait)

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-keybinds.org" :minlevel 2
;; ----------------------------------------------------------------

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
(setq use-short-answers t)
;; Move the mouse away if the cursor gets close
;; (mouse-avoidance-mode 'animate)

;; highlight the current line, as in Matlab
;; (global-hl-line-mode)

;; Confirm when killing Emacs
(setq confirm-kill-emacs (lambda (prompt)
                           (y-or-n-p-with-timeout prompt 2 nil)))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

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

;; Frame title
(setq frame-title-format
      '(""
        (:eval
         (if (and (boundp 'org-roam-directory)
              (string-match-p org-roam-directory (or buffer-file-name "")))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "roam:"
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))))

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

;;;######################################################################
;; * INTERFACING WITH THE OS
;;;######################################################################

(if IS-WINDOWS
    (setq shell-file-name "C:/cygwin/cygwin.bat"))

(use-package auth-source-pass
  :defer
  :config (auth-source-pass-enable))

;; Consult clipboard before primary selection
;; http://www.gnu.org/software/emacs/manual/
;; html_node/emacs/Clipboard.html
(use-package select
  :config
  (setq select-enable-clipboard t))

;; Howard Abrams' piper lets you mix Shell and Emacs commands through "pipes"
;; using Emacs buffers. A neat idea but I never use it.
(use-package piper
  :disabled
  :bind ("C-x |" . piper)
  :config
  (defun +piper-start (&optional arg)
    "Start piper. With prefix ARG, start piper on current buffer"
    (interactive "P")
    (if arg (piper) (piper-user-interface))))

;; For easy sharing of files/text.
(use-package 0x0
  :ensure t
  :commands (0x0-upload 0x0-dwim)
  :bind ("C-x M-U" . 0x0-dwim))

;; Upload to my webserver
(use-package emacs
  :defer
  :bind ("C-x U" . pastebin-buffer)
  :commands (pastebin-buffer pastebin-file)
  :config
  (defun pastebin-buffer (&optional buf)
    (interactive (list (current-buffer)))
    (with-current-buffer buf
      (let ((upload-file-name
             (if current-prefix-arg
                 (read-string "Name of uploaded buffer: ")
                 (buffer-name)))
            (htmlized-buffer
             (if (use-region-p)
                 (progn
                   (deactivate-mark)
                   (htmlize-region (region-beginning) (region-end)))
               (htmlize-buffer))))
        (with-current-buffer htmlized-buffer
          (let* ((bufhash (sha1 (current-buffer)))
                 (ssh-url (format (car (alist-get 'pastebin my-server-url-alist))
                                  (concat (substring bufhash 0 8) "-")
                                  upload-file-name))
                 (web-url (format (cadr (alist-get 'pastebin my-server-url-alist))
                                  (concat (substring bufhash 0 8) "-")
                                  upload-file-name)))
            (write-file ssh-url)
            (message "Wrote file to: %s" web-url)
            (kill-new web-url))))))
  (defun pastebin-file (&optional file)
    (interactive
     (list (cond
            ((derived-mode-p 'dired-mode)
             (dired-file-name-at-point))
            (t (read-file-name "Pastebin file: " nil nil t)))))
    (let* ((filehash (shell-command-to-string (format "sha1sum %s" file)))
           (ssh-url (format (car (alist-get 'pastebin my-server-url-alist))
                            (substring filehash 0 8)
                            (file-name-nondirectory file)))
           (web-url (format (cadr (alist-get 'pastebin my-server-url-alist))
                            (substring filehash 0 8)
                            (file-name-nondirectory file))))
      (htmlize-file file ssh-url)
      (message "Wrote file to: %s" web-url)
      (kill-new web-url))))

;; Emacs is slow sometimes, I try to find out why.
(use-package explain-pause-mode
  :ensure (:host github
           :repo "lastquestion/explain-pause-mode")
  :commands explain-pause-mode
  :config
  (setq explain-pause-alert-style 'silent))

;;;----------------------------------------------------------------
;; ** SHELL AND ESHELL PREFERENCES
;;;----------------------------------------------------------------
;; Settings for shell, eshell, comint and vterm
(load (expand-file-name "lisp/setup-shells" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-shells.org" :minlevel 2
;; ----------------------------------------------------------------

;;;######################################################################
;; * LINE NUMBERS
;;;######################################################################
(line-number-mode 1)

(defvar my/addons-enabled-modes (list 'prog-mode-hook
                                    'conf-unix-mode-hook
                                    'conf-windows-mode-hook
                                    'conf-javaprop-mode-hook
                                    'tex-mode-hook
                                    'text-mode-hook
                                    'message-mode-hook)
  "List of modes where special features (like line numbers)
  should be enabled.")

;; (dolist (mode-hook my/addons-enabled-modes)
;;   (add-hook mode-hook (lambda () "Turn on line numbers for major-mode"
;;                         (interactive)
;;                         (display-line-numbers-mode))))

(setq display-line-numbers-width-start t
      display-line-numbers-type t)

;;;################################################################
;; * EDITING
;;;######################################################################
 (use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode
  :hook ((eww-after-render . my/visual-fill-in-window)
         (eww-after-render . visual-line-mode)
         (notmuch-show-mode . visual-fill-column-mode))
  :config
  (defun my/visual-fill-in-window ()
    (when-let ((win (get-buffer-window (current-buffer))))
      (with-selected-window win
        (visual-fill-column-mode 1))))
  (setq-default visual-fill-column-center-text t
                visual-fill-column-width 94))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package iedit
  :ensure t
  :bind (("C-M-;" . iedit-mode)
         ("M-s n" . my/iedit-1-down)
         ("M-s p" . my/iedit-1-up))
  :config
  (defun my/iedit-1-down (arg)
    (interactive "p")
    (let ((current-prefix-arg '(1)))
      (call-interactively #'iedit-mode)
      (iedit-expand-down-to-occurrence)))
  (defun my/iedit-1-up (arg)
    (interactive "p")
    (let ((current-prefix-arg '(1)))
      (call-interactively #'iedit-mode)
      (iedit-expand-up-to-occurrence))))

(use-package replace
  :defer
  :bind (:map occur-mode-map
         ("C-x C-q" . occur-edit-mode)
         :map query-replace-map
         ("p" . 'backup)))

;; Testing parallel replacements
(use-package query-replace-parallel
  :ensure (:host github :repo "hokomo/query-replace-parallel"
           :files ("query-replace-parallel.el"))
  :bind (("H-%" . query-replace-parallel)
         ("H-M-%" . query-replace-parallel-regexp)))

(require 'better-editing nil t)

(use-package emacs
  :config
  (setq set-mark-command-repeat-pop t)
  (global-set-key (kbd "M-r") ctl-x-r-map)
  (setq undo-limit (* 80 1024 1024))
  :bind
  (("M-z" . zap-to-char-save)
   ("<C-M-backspace>" . backward-kill-sexp)
   :map ctl-x-r-map
   ("a" . append-to-register)))

(use-package expand-region
  :ensure (:remotes ("fork" :host github :repo "karthink/expand-region.el"))
  :commands expand-region
  :bind ("C-," . 'er/expand-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode)
  (set-default 'er--show-expansion-message nil)
  (setq expand-region-show-usage-message nil
        expand-region-fast-keys-enabled nil)
  (defvar expand-region-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "," #'er/expand-region)
      (define-key map "-" #'er/contract-region)
      (define-key map "." #'er/contract-region)
      map))
  ;; Expand 0-9 times by pressing 0-9
  (dotimes (i 9)
    (define-key expand-region-repeat-map
      (kbd (number-to-string i))
      (lambda () (interactive)
        (er/expand-region i)
        (setq this-command 'er/expand-region))))
  (put 'er/expand-region 'repeat-map 'expand-region-repeat-map)
  (put 'er/contract-region 'repeat-map 'expand-region-repeat-map)
  (advice-add 'er--first-invocation
              :override
              (defun my/er--first-invocation ()
                "t if this is the first invocation of er/expand-region or er/contract-region"
                (not (memq last-command
                           '(er/expand-region er/contract-region
                             easy-kill-expand-region easy-kill-contract-region)))))
  
  ;; The default er/save-org-mode-excursion uses org-fold-core-* and is too expensive
  (define-advice er/save-org-mode-excursion
      (:override (action) "ignore-org-fold")
    (org-with-wide-buffer
     (funcall action)))
  ;; The default er/mark-comment is both expensive and incorrect for block
  ;; comments.
  (defun er/mark-comment ()
    "Mark the entire comment around point."
    (interactive)
    (when (er--point-is-in-comment-p)
      (let ((p (point)))
        (while (or (> (skip-syntax-forward " ") 0)
                   (and (er--point-is-in-comment-p) (not (eobp))))
          (forward-char 1))
        (while (not (or (er--point-is-in-comment-p) (bobp)))
          (forward-char -1))
        (set-mark (point))
        (goto-char p)
        (while (or (< (skip-syntax-backward " ") 0)
                   (er--point-is-in-comment-p))
          (forward-char -1))
        (while (not (or (er--point-is-in-comment-p) (eobp)))
          (forward-char 1)))))
  
  (defun my/find-bounds-of-regexps (open close)
    (let ((start (point))
          (parity 0)
          (open-close (concat "\\(?:" open "\\|" close "\\)"))
          end)
      (save-excursion
        (while (and (not (= parity -1))
                    (re-search-backward open-close nil t))
          (if (looking-at open)
              (setq parity (1- parity))
            (setq parity (1+ parity))))
        (setq end (point))
        (goto-char start)
        (while (and (not (= parity 0))
                    (re-search-forward open-close nil t))
          (if (looking-back
               close
               (- (point) (length (match-string-no-properties 0))))
              (setq parity (1+ parity))
            (setq parity (1- parity))))
        (when (= parity 0) (cons end (point))))))

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
      (unless (texmathp) (er/mark-text-sentence)))
    (defun er/mark-latex-text-paragraph ()
      (unless (texmathp) (er/mark-text-paragraph)))
    (defun er/mark-LaTeX-inside-math ()
      "Mark text inside LaTeX math delimiters. See `er/mark-LaTeX-math'
for details."
      (when (texmathp)
          (let* ((string (car texmathp-why))
                 (pos (cdr texmathp-why))
                 (reason (assoc string texmathp-tex-commands1))
                 (type (cadr reason)))
            (cond
             ((eq type 'sw-toggle) ;; $ and $$
              (goto-char pos)
              (set-mark (1+ (point)))
              (forward-sexp 1)
              (backward-char 1)
              (exchange-point-and-mark))
             ((or (eq type 'sw-on)
                  (equal string "Org mode embedded math")) ;; \( and \[
              (re-search-forward texmathp-onoff-regexp)
              (backward-char 2)
              (set-mark (+ pos 2))
              (exchange-point-and-mark))
             (t (error (format "Unknown reason to be in math mode: %s" type)))))))
    ;; ;; FIXME
    ;; (defun er/mark-latex-macro ()
    ;;   (interactive)
    ;;   (when (texmathp)
    ;;     (when (> (point) (mark))
    ;;       (exchange-point-and-mark))
    ;;     (when (looking-back "\\\\[a-zA-Z_]+\\*?{?" (line-beginning-position))
    ;;       (set-mark (save-excursion
    ;;                   (goto-char (match-beginning 0))
    ;;                   (point)))
    ;;       (exchange-point-and-mark))))
    (defun er/mark-latex-inside-pairs ()
      (if (texmathp)
          (cl-destructuring-bind (beg . end)
              (my/find-bounds-of-regexps " *[{([|<]"
                                         " *[]})|>]")
            (when-let ((n (length (match-string-no-properties 0))))
              (set-mark (save-excursion
                          (goto-char beg)
                          (forward-char n)
                          (skip-chars-forward er--space-str)
                          (point)))
              (goto-char end)
              (backward-char n)
              (if (looking-back "\\\\right\\\\*\\|\\\\" (- (point) 7))
                  (backward-char (length (match-string-no-properties 0)))))
            (skip-chars-backward er--space-str)
            (exchange-point-and-mark))
        (er/mark-inside-pairs)))
    (defun er/mark-latex-outside-pairs ()
      (if (texmathp)
          (cl-destructuring-bind (beg . end)
              (my/find-bounds-of-regexps " *[{([|<]"
                                         " *[]})|>]")
            (set-mark (save-excursion
                        (goto-char beg)
                        ;; (forward-char 1)
                        (if (looking-back "\\\\left\\\\*\\|\\\\" (- (point) 6))
                            (backward-char (length (match-string-no-properties 0))))
                        (skip-chars-forward er--space-str)
                        (point)))
            (goto-char end)
            (skip-chars-backward er--space-str)
            ;; (backward-char 1)
            (exchange-point-and-mark))
        (er/mark-outside-pairs)))
    (defun er/set-latex-mode-expansions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (setq er/try-expand-list
            '(er/mark-word er/mark-symbol er/mark-symbol-with-prefix
              er/mark-next-accessor  er/mark-inside-quotes er/mark-outside-quotes
              er/mark-LaTeX-inside-math
              er/mark-latex-inside-pairs er/mark-latex-outside-pairs
              er/mark-comment er/mark-url er/mark-email ;er/mark-defun
              er/mark-latex-text-sentence er/mark-latex-text-paragraph))
      (er/add-latex-mode-expansions))))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . #'easy-kill)
         ([remap mark-sexp]      . #'easy-mark)
         ("M-S-w" . kill-ring-save)
         :map easy-kill-base-map
         ("+" . nil) ("=" . nil)
         ("," . easy-kill-expand-region)
         ("." . easy-kill-contract-region))
  :config
  (setq easy-kill-alist
        '((119 word " ")
          (115 sexp "\n")
          (101 line "\n")
          (108 list "\n")
          (100 defun "\n\n")
          (41 sentence "\n")
          (104 paragraph "\n")
          (62 page "\n")
          (102 filename "\n")
          (68 defun-name " ")
          (98 buffer-file-name)))
  (defun easy-kill-expand-region ()
    "Expand kill according to expand-region."
    (interactive)
    (let* ((thing (easy-kill-get nil))
           (bounds (easy-kill--bounds)))
      (save-mark-and-excursion
        (set-mark (cdr bounds))
        (goto-char (car bounds))
        (er/expand-region 1)
        (deactivate-mark)
        (easy-kill-adjust-candidate thing (point) (mark)))))
  (defun easy-kill-contract-region ()
    "Expand kill according to expand-region."
    (interactive)
    (let* ((thing (easy-kill-get nil))
           (bounds (easy-kill--bounds)))
      (save-mark-and-excursion
        (set-mark (cdr bounds))
        (goto-char (car bounds))
        (er/contract-region 1)
        (deactivate-mark)
        (easy-kill-adjust-candidate thing (point) (mark))))))

(use-package easy-kill
  :after easy-kill
  :config
  ;; Incomplete implementation of using easy-kill to select backwards
  (defun easy-kill-thing-alt (&optional thing n inhibit-handler)
  (interactive
   (list (cl-second (assq (+ last-command-event 32) easy-kill-alist))
         (prefix-numeric-value current-prefix-arg)))
  (let* ((thing (or thing (easy-kill-get thing)))
         (n (or n 1))
         (handler (and (not inhibit-handler)
                       (easy-kill-thing-handler (format "easy-kill-on-%s" thing)
                                                major-mode))))
    (when (easy-kill-get mark)
      (goto-char (easy-kill-get origin)))
    (cond
     (handler (funcall handler n))
     ((or (memq n '(+ -))
          (and (eq thing (easy-kill-get thing))
               (not (zerop n))))
      (easy-kill-thing-backward (pcase n
                                  (`+ 1)
                                  (`- -1)
                                  (_ n))))
     (t (pcase (easy-kill-bounds-of-thing-at-point thing)
          (`nil (easy-kill-echo "No `%s'" thing))
          (`(,start . ,end)
           (easy-kill-adjust-candidate thing start end)
           (unless (zerop n)
             (easy-kill-thing-backward (1- n)))))))
    (when (easy-kill-get mark)
      (easy-kill-adjust-candidate (easy-kill-get thing)))))

(defun easy-kill-thing-backward (n)
  (when (and (easy-kill-get thing) (/= n 0))
     (let* ((step (if (cl-minusp n) -1 +1))
           (thing (easy-kill-get thing))
           (bounds1 (or (easy-kill-pair-to-list
                         (easy-kill-bounds-of-thing-at-point thing))
                        (list (point) (point))))
           (start (easy-kill-get start))
           (end (easy-kill-get end))
           (rear (or (car (cl-set-difference (list start end) bounds1))
                     (pcase step
                       (`-1 end)
                       (`1 start))))
           (new-rear (save-excursion
                        (goto-char rear)
                        (with-demoted-errors "%S"
                          (dotimes (_ (abs n))
                            (easy-kill-thing-backward-1 thing step)))
                        (point))))
      (pcase (and (/= rear new-rear)
                  (sort (cons new-rear bounds1) #'<))
        (`(,start ,_ ,end)
         (easy-kill-adjust-candidate thing start end)
         t)))))

(defun easy-kill-thing-backward-1 (thing &optional n)
  "Easy Kill wrapper for `forward-thing'."
  (pcase (easy-kill-thing-handler
          (format "easy-kill-thing-backward-%s" thing)
          major-mode)
    ((and (pred functionp) fn) (funcall fn n))
    (_ (forward-thing thing (- n)))))

(defun easy-kill-map ()
  "Build the keymap according to `easy-kill-alist'."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map easy-kill-base-map)
    (when easy-kill-unhighlight-key
      (with-demoted-errors "easy-kill-unhighlight-key: %S"
        (define-key map easy-kill-unhighlight-key #'easy-kill-unhighlight)))
    (define-key map "[" (lambda () (interactive)
                          (cl-letf* (((symbol-function 'easy-kill-thing)
                                      #'easy-kill-thing-alt))
                            (call-interactively #'easy-kill-thing-alt))))
    (define-key map "]" #'easy-kill-thing)
    (dolist (c easy-kill-alist)
      ;; (define-key map (vector meta-prefix-char (car c)) #'easy-kill-select)
      (when (<= 97 (car c) 122)
        (define-key map (char-to-string (- (car c) 32))
                    (lambda () (interactive)
                      (cl-letf* (((symbol-function 'easy-kill-thing)
                                  #'easy-kill-thing-alt))
                        (call-interactively #'easy-kill-thing-alt)))))
      (define-key map (char-to-string (car c)) #'easy-kill-thing))
    map)))

(use-package goto-chg
  :ensure t
  :bind (("M-g ;" . goto-last-change)
         ("M-i" . goto-last-change)
         ("M-g M-;" . goto-last-change))
  :config
  (use-package org
    :bind (:map org-mode-map
           ("M-g ;" . org-goto-last-change)
           ("M-i" . org-goto-last-change)
           ("M-g M-;" . org-goto-last-change))
    :config
    (defun org-goto-last-change ()
      (interactive)
      (call-interactively 'goto-last-change)
      (when (org-invisible-p)
        (org-fold-show-context 'link-search)))))

(use-package quail
  :commands my/cdlatex-input-tex
  :config
  (defun my/cdlatex-input-tex ()
  (interactive)
  (require 'cdlatex nil t)
  (let ((cim current-input-method))
    (unless (equal cim "TeX")
      (activate-input-method "TeX"))
    (cl-letf (((symbol-function 'texmathp)
               (lambda () t))
              ((symbol-function 'insert)
               (lambda (symbol)
                 (setq unread-input-method-events
                       (nconc (quail-input-string-to-events symbol)
                              (list 0))))))
      (cdlatex-math-symbol))
    (unless (equal cim "TeX")
      (run-at-time 0 nil (lambda () (activate-input-method cim)))))))

;; Visual indicator when recording macros
(use-package kmacro
  :defer
  :config
  ;; ;; Undo in blocks, borrowed from Omar Antolin Camarena
  ;; (defun my/block-undo (fn &rest args)
  ;;   (let ((marker (prepare-change-group)))
  ;;     (unwind-protect (apply fn args)
  ;;       (undo-amalgamate-change-group marker))))

  ;; (dolist (fn '(kmacro-call-macro
  ;;               kmacro-exec-ring-item
  ;;               dot-mode-execute
  ;;               apply-macro-to-region-lines))
  ;;   (advice-add fn :around #'my/block-undo))
  
  (defsubst my/mode-line-macro-recording ()
    "Display macro being recorded."
    (when (or defining-kbd-macro executing-kbd-macro)
      (let ((sep (propertize " " 'face 'highlight ))
            (vsep (propertize " " 'face '(:inherit variable-pitch))))
        ;; "●"
        (propertize (concat sep "MACRO" vsep
                            (number-to-string kmacro-counter) vsep
                            "▶" sep)
                    'face 'highlight))))
  
  (setq-default mode-line-format
                (cl-pushnew '(:eval (my/mode-line-macro-recording))
                            (default-value 'mode-line-format)
                            :test 'equal)))

;; * MANAGE STATE
;; ** RECENTF
;; Keep track of recently opened files. Also feeds into the list of recent
;; directories used by consult-dir.
(use-package recentf
  :config
  (setq recentf-save-file (dir-concat user-cache-directory "recentf")
        recentf-max-saved-items 200
        recentf-auto-cleanup 300)
  (define-advice recentf-cleanup (:around (fun) silently)
    (let ((inhibit-message t)
          (message-log-max nil))
      (funcall fun)))
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
  :defer
  ;; :hook (kill-emacs . desktop-save-in-desktop-dir)
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

;; ** ACTIVITIES
(use-package activities
  :when (daemonp)
  :ensure t
  :init
  (setq activities-mode-idle-frequency 45)
  (activities-tabs-mode))

(use-package activities
    :custom-face
    (activities-tabs-face ((t nil)))
    :bind
    (("C-x C-a C-j" . activities-resume)
     ("C-x C-a C-z" . activities-suspend)
     ("C-x C-a C-k" . activities-discard)
     ("C-x C-a b" . activities-switch-buffer)
     ("C-x C-a C-s" . activities-define)
     ("C-x C-a g" . activities-revert)
     ("C-x C-a l" . activities-list)))

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
(load (expand-file-name "lisp/better-buffers" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/better-buffers.org" :minlevel 2
;; ----------------------------------------------------------------

;; ** IBUFFER
(load (expand-file-name "lisp/setup-ibuffer" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-ibuffer.org" :minlevel 3
;; ----------------------------------------------------------------

;;;################################################################
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
  ("<f9>" . +make-frame-floating-with-current-buffer))

(use-package window
  :bind (("H-+" . balance-windows-area)
         ;; ("C-x +" . balance-windows-area)
         ("C-x q" . my/kill-buffer-and-window)
         ("ESC M-v" . scroll-other-window-down)
         :map window-prefix-map
         ("1" . my/window-toggle-dedicated))
  :config
  (setq switch-to-prev-buffer-skip nil) ;'this
  (setq truncate-partial-width-windows t)
  (setq other-window-scroll-default
      (lambda ()
        (or (get-mru-window nil nil 'not-this-one-dummy)
            (next-window)
            (next-window nil nil 'visible))))
  (defun my/kill-buffer-and-window ()
    "Kill buffer.

Also kill this window, tab or frame if necessary."
    (interactive)
    (if (one-window-p)
        (progn (kill-buffer)
               (my/delete-window-or-delete-frame))
      (kill-buffer-and-window)))

  ;; quit-window behavior is completely broken
  ;; Fix by adding winner-mode style behavior to quit-window
  (defun my/better-quit-window-save (window)
    (push (window-parameter window 'quit-restore)
          (window-parameter window 'quit-restore-stack))
    window)
  (defun my/better-quit-window-restore (origfn &optional window bury-or-kill)
    (let ((sw (or window (selected-window))))
      (funcall origfn window bury-or-kill)
      (when (eq sw (selected-window))
        (pop (window-parameter nil 'quit-restore-stack))
        (setf (window-parameter nil 'quit-restore)
              (car (window-parameter nil 'quit-restore-stack))))))

  (defun my/window-toggle-dedicated (&optional win)
    (interactive (list (selected-window)))
    (let ((dedicated (window-dedicated-p win)))
      (set-window-dedicated-p win (not dedicated))
      (message "Window marked as %s." (if dedicated "free" "dedicated"))))

  (advice-add 'display-buffer :filter-return #'my/better-quit-window-save)
  (advice-add 'quit-restore-window :around #'my/better-quit-window-restore))

;; setup-windows:
;; ---------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-windows.org" :minlevel 2
;; ---------------------------------------------------------------


;;;----------------------------------------------------------------
;; ** POPPER
;;;----------------------------------------------------------------

;; Designate buffers to popup status and toggle or cycle through them
(use-package popper
  :ensure (:host github :protocol ssh
           :repo "karthink/popper")
  ;; :load-path "plugins/popper/"
  :after (setup-windows setup-project)
  :commands popper-mode
  :bind (("C-`" . popper-toggle)
         ("C-M-`" . popper-cycle)
         ("H-`" . popper-toggle)
         ("H-M-`" . popper-cycle)
         ("H-6" . popper-toggle-type)
         ("C-x 6" . popper-toggle-type)
         ("C-^" . popper-toggle-type)
         ("H-M-k" . popper-kill-latest-popup)
         ("s-M-k" . popper-kill-latest-popup)
         ("M-`" . my/switch-to-other-buffer)
         ("s-n" . my/next-buffer)
         ("s-p" . my/previous-buffer))
  :init
  ;; (setq popper-group-function
  ;;       (lambda ()
  ;;         (let ((tabs (funcall tab-bar-tabs-function)))
  ;;           (alist-get 'name (nth (tab-bar--current-tab-index tabs)
  ;;                                 tabs)))))

  (if (boundp 'elpaca-after-init-hook)
      (add-hook 'elpaca-after-init-hook #'popper-mode)
    (add-hook 'emacs-startup-hook #'popper-mode))

  (setq popper-reference-buffers
        (append my/help-modes-list
                my/man-modes-list
                my/repl-modes-list
                my/repl-names-list
                my/occur-grep-modes-list
                ;; my/man-modes-list
                '(Custom-mode
                  compilation-mode
                  messages-buffer-mode)
                '(("^\\*Warnings\\*$" . hide)
                  ("^\\*Compile-Log\\*$" . hide)
                  "^\\*Matlab Help.*\\*$"
                  ;; "^\\*Messages\\*$"
                  "^\\*Backtrace\\*"
                  "^\\*evil-registers\\*"
                  "^\\*Apropos"
                  "^Calc:"
                  "^\\*eldoc\\*"
                  "^\\*TeX errors\\*"
                  "^\\*ielm\\*"
                  "^\\*TeX Help\\*"
                  "^\\*ChatGPT\\*"
                  "^\\*gptel-ask\\*"
                  "\\*Shell Command Output\\*"
                  ("\\*Async Shell Command\\*" . hide)
                  ("\\*Detached Shell Command\\*" . hide)
                  "\\*Completions\\*"
                  ;; "\\*scratch.*\\*$"
                  "[Oo]utput\\*")))

  (use-package embark
    :defer
    :bind (:map embark-buffer-map
           ("_" . embark-popper-toggle))
    :config
    (defun embark-popper-toggle (buf)
      "Toggle popup status."
      (popper-toggle-type buf)))

  (use-package popper-echo
    :defer 3
    :config
    (defvar popper-echo--propertized-names nil
      "Alist of popup buffer names and their shortened, propertized
display names.")

    (defun popper-message-shorten (full-name)
      (let ((name (file-name-nondirectory full-name)))
        (or (alist-get name popper-echo--propertized-names nil nil #'string=)
            (let ((short-name
                   (cond
                    ((string= "*Messages*" name)
                     (concat (propertize "LOG " 'face 'default)
                             (propertize name 'face 'popper-echo-area-buried)))
                    ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
                     (concat (propertize "HLP " 'face '(:inherit link :underline nil :weight bold))
                             (propertize (match-string 1 name) 'face 'popper-echo-area-buried)))
                    ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
                     (concat (propertize "HLP" 'face '(:inherit link :underline nil :weight bold))
                             (propertize (match-string 1 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^\\*Customize *\\(.*?\\):\\(.*\\)\\*$" name)
                     (concat (propertize
                              (pcase (match-string 1 name)
                                ('"Option" "OPT") ('"Group" "GRP") (s s))
                              'face '(:inherit link :underline nil :weight bold))
                             (propertize (match-string 2 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^\\*\\(e?\\)shell:? ?\\(.*\\)\\*$" name)
                     (concat (if (string-empty-p (match-string 1 name))
                                 (propertize "SH" 'face 'success)
                               (propertize "ESH" 'face 'success))
                             (unless (string-empty-p (match-string 2 name)) " ")
                             (propertize (match-string 2 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^\\*\\(.*?\\)-\\(e?\\)shell\\*$" name)
                     (concat (if (string-empty-p (match-string 2 name))
                                 (propertize "SH" 'face 'success)
                               (propertize "ESH" 'face 'success))
                             (unless (string-empty-p (match-string 1 name)) " ")
                             (propertize (match-string 1 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^[*]?\\(.*?\\) *\\(?:[Oo]utput\\|Command\\)\\*$" name)
                     (concat (propertize "OUT "
                                         'face '(:inherit warning))
                             (propertize (match-string 1 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
                     (concat (propertize "LOG "
                                         ;; '(:inherit link-visited :underline nil)
                                         'face 'default)
                             (propertize (match-string 1 name)
                                         'face 'popper-echo-area-buried)))
                    ((or (string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
                         (string-match
                          "^\\*\\(.*?\\)[ -]?\\(?:byte\\)?[ -]?[Cc]ompil\\(?:e\\|ation\\)\\*$" name))
                     (concat (propertize "COM "
                                         'face '(:inherit link-visited :underline nil :weight normal))
                             (propertize (match-string 1 name) 'face 'popper-echo-area-buried)))
                    ((or (cl-member (buffer-mode (get-buffer full-name))
                                    my/repl-modes-list :test #'eq)
                         (cl-member full-name my/repl-names-list :test #'string-match))
                     (concat (propertize "RPL " 'face 'success) name))
                    (t (propertize name 'face 'popper-echo-area-buried)))))
              (cdar (push (cons name (truncate-string-to-width short-name 15 nil nil t))
                          popper-echo--propertized-names))))))

    (setq popper-echo-transform-function #'popper-message-shorten)
    (setq popper-echo-dispatch-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
          popper-echo-dispatch-actions t)
    ;; (advice-add 'popper-echo :around
    ;;             (defun my/popper-echo-no-which-key (orig-fn)
    ;;               (let ((which-key-show-transient-maps nil))
    ;;                 (funcall orig-fn))))
    ;; (popper-tab-line-mode 1)
    (popper-echo-mode +1))

  :config
  (setq popper-display-control 'user)

  (defun my/popper-switch-to-popup (buf)
    ";TODO: "
    (interactive
     (list
      (let ((pred (lambda (b)
                    (if (consp b) (setq b (car b)))
                    (setq b (get-buffer b))
                    (and (popper-popup-p b)
                         (or (not popper-group-function)
                             (memq (get-buffer "*Messages*")
                                   (mapcar #'cdr
                                           (alist-get (funcall popper-group-function)
                                                      popper-buried-popup-alist
                                                      nil nil 'equal))))))))
        (read-buffer "Switch to popup: " nil t pred))))
    (popper-close-latest)
    (display-buffer buf))

  (defvar popper-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "`") #'popper-cycle)
      (define-key map (kbd "~") #'popper-cycle-backwards)
      (define-key map (kbd "b") #'my/popper-switch-to-popup)
      map))
  (put 'popper-cycle-backwards 'repeat-map 'popper-repeat-map)
  (put 'popper-cycle 'repeat-map 'popper-repeat-map)
  (put 'popper-toggle 'repeat-map 'popper-repeat-map)

  (with-eval-after-load 'activities-tabs
    (setq popper-group-function
          (lambda ()
            (or (and-let* ((act (activities-tabs-current)))
                  (activities-activity-name act))
                (selected-frame)))))

  (setq popper-display-function
        (defun my/popper-select-below (buffer &optional _alist)
          (funcall (if (> (frame-width) 170)
                       ;; #'display-buffer-in-direction
                       #'popper-select-popup-at-bottom
                     #'display-buffer-at-bottom)
                   buffer
                   `((window-height . ,popper-window-height)
                     (direction . below)
                     (body-function . ,#'select-window)))))

  (defun my/switch-to-other-buffer (&optional _arg)
    (interactive)
    (switch-to-buffer (other-buffer)))
  (put 'my/switch-to-other-buffer 'repeat-map
       'my/buffer-cycle-map)

  (defun my/next-buffer (&optional arg)
    "Switch to the next non-popup buffer."
    (interactive "P")
    (if-let (((equal arg '(4)))
             (win (other-window-for-scrolling)))
        (with-selected-window win
          (my/next-buffer)
          (setq prefix-arg current-prefix-arg))
      (dotimes (or (abs (prefix-numeric-value arg)) 1)
        (my/switch-buffer-1 #'next-buffer))))
  (defun my/previous-buffer (&optional arg)
    "Switch to the previous non-popup buffer."
    (interactive "P")
    (if-let (((equal arg '(4)))
             (win (other-window-for-scrolling)))
        (with-selected-window win
          (my/previous-buffer)
          (setq prefix-arg current-prefix-arg))
      (dotimes (or (abs (prefix-numeric-value arg)) 1)
      (my/switch-buffer-1 #'previous-buffer))))
  (defun my/switch-buffer-1 (switch)
    "Switch to the next user buffer in cyclic order.
User buffers are those not starting with *."
    (funcall switch)
    (let ((i 0))
      (while (and (< i 50)
                  (member (buffer-local-value 'popper-popup-status (current-buffer))
                          '(popup user-popup)))
        (setq i (1+ i)) (funcall switch))))
  
  (define-key global-map (kbd "C-x C-p") #'my/previous-buffer)
  (define-key global-map (kbd "C-x C-n") #'my/next-buffer)
  (define-key global-map (kbd "C-x n g") #'set-goal-column)
  (defvar my/buffer-cycle-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'my/next-buffer)
      (define-key map (kbd "p") #'my/previous-buffer)
      (define-key map (kbd "b")
        (defun my/switch-buffer ()
          "Switch to consult-buffer"
               (interactive)
               (run-at-time
                0 nil
                (lambda (&optional arg)
                  (interactive "P")
                  (if-let (((equal arg '(4)))
                           (win (other-window-for-scrolling)))
                      (with-selected-window win (consult-buffer))
                    (consult-buffer)))
                current-prefix-arg)))
      map))
  (map-keymap
   (lambda (_ cmd) (put cmd 'repeat-map 'my/buffer-cycle-map))
   my/buffer-cycle-map))

;;----------------------------------------------------------------
;; ** WINUM
;;----------------------------------------------------------------

;; Add window numbers and use them to switch windows
(use-package winum
  :disabled
  :ensure t
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
    (winum-mode 1))
  :config
  (setq winum-scope 'visible))

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
  :config
  (winner-mode +1))

;;----------------------------------------------------------------
;; ** switchy-window
;;----------------------------------------------------------------
(use-package switchy-window
  :ensure t
  :defer 2
  :init (switchy-window-minor-mode)
  :bind (("M-o" . switchy-window)
         :map other-window-repeat-map
         ("o" . switchy-window))
  :config
  (setq switchy-window-delay 0.75)
  (put 'switchy-window 'repeat-map 'other-window-repeat-map))

(use-package window
  :unless (fboundp 'switchy-window-minor-mode)
  :bind (("M-o" . my/other-window)
         ("M-O" . my/other-window-prev)
         :map other-window-repeat-map
         ("o" . my/other-window)
         ("O" . my/other-window-prev))
  :config
  (defalias 'my/other-window
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive)
        (if (equal last-command 'my/other-window)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))
  (defun my/other-window-prev (&optional arg all-frames)
    (interactive "p")
    (other-window (if arg (- arg) -1) all-frames))
  (put 'my/other-window 'repeat-map 'other-window-repeat-map)
  (put 'my/other-window-prev 'repeat-map 'other-window-repeat-map))

;;----------------------------------------------------------------
;; ** Ace-window
;;----------------------------------------------------------------

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window)
   ("H-o"   . ace-window)
   ("C-M-0" . ace-window-prefix)
   ("C-M-9" . ace-window)
   :map ctl-x-4-map
   ("o" . ace-window-prefix))
  ;; :custom-face
  ;; (aw-leading-char-face ((t (:height 2.5 :weight normal))))
  :defer 2
  :init (ace-window-display-mode 1)
  :custom-face (aw-mode-line-face ((t (:inherit (bold mode-line-emphasis)))))
  :config
  (defun my/aw-take-over-window (window)
    "Move from current window to WINDOW.

Delete current window in the process."
    (let ((buf (current-buffer)))
      (if (one-window-p)
          (delete-frame)
        (delete-window))
      (aw-switch-to-window window)
      (switch-to-buffer buf)))
  (defun ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  (setq aw-swap-invert t)
  (setq aw-dispatch-always t
        aw-scope 'global
        aw-background nil
        aw-display-mode-overlay nil
        aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?p))
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete Window")
          (?x aw-swap-window "Swap Windows")
          (?m my/aw-take-over-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?o aw-flip-window)
          (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?s aw-split-window-vert "Split Vert Window")
          (?v aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help))))

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
  (use-package emacs-wm))

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
  (let* ((re (reb-target-value 'reb-regexp))
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

;;;################################################################
;; * UTILITY
;;;################################################################
;; Count words, print ASCII table, etc
(load (expand-file-name "lisp/utilities" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/utilities.org" :minlevel 2
;; ---------------------------

(use-package async
  :disabled
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
  :ensure (:host github
           :repo "tecosaur/screenshot"
           :build (:not elpaca--byte-compile))
  :commands screenshot)

;; Colorize color names and parens in buffers
(use-package rainbow-mode
  :commands rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :ensure t)

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
  :ensure t
  :config
  (defun my/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (let* ((mode major-mode))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  (setf (alist-get "\\*Scratch for" display-buffer-alist nil nil #'equal)
        '((display-buffer-same-window)))
  :hook (scratch-create-buffer . my/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

(defun delete-window-if-not-single ()
  "Delete window if not the only one."
  (when (not (one-window-p))
    (delete-window)))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :bind (:map prog-mode-map
         ("M-g t" . hl-todo-next)
         ("M-g T" . hl-todo-previous))
  :config
  (defvar-keymap hl-todo-repeat-map
    :repeat t
    "n" #'hl-todo-next
    "p" #'hl-todo-previous)
  (setq hl-todo-wrap-movement t))

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
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (global-set-key [(f9)] 'compile)
  
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
;; ** NIX
(when IS-GUIX
  (load-library "nix-mode-autoloads")
  (load-library "nix-modeline-autoloads"))
;; ** MARKDOWN
;;;----------------------------------------------------------------
(use-package markdown-mode :ensure t :defer)
(use-package edit-indirect :ensure t :defer)

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
  (use-package company-lsp :straight t :commands company-lsp)
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
;; (use-package jsonrpc :ensure (:tag "1.0.25") :defer)
;; (use-package eldoc :ensure t :defer)
(use-package eglot
  ;; :ensure t
  :commands eglot
  :bind (:map eglot-mode-map
         ("C-h ." . eldoc))
  :hook ((eglot-managed-mode . my/eglot-eldoc-settings))
  :config
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  ;; (setq eglot-put-doc-in-help-buffer nil)
  (setq eglot-events-buffer-size 0
        eglot-report-progress nil)
  (setq eglot-extend-to-xref t)
  (add-to-list 'eglot-server-programs
               '(beancount-mode . ("beancount-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))
  (setq-default
   eglot-workspace-configuration
   '(:basedpyright (:typeCheckingMode "recommended")
     :basedpyright.analysis
     (:diagnosticSeverityOverrides
      (:reportUnusedCallResult "none")
      :inlayHints (:callArgumentNames :json-false)))))

;;;----------------------------------------------------------------
;; *** EGLOT-BOOSTER
;;;----------------------------------------------------------------
(use-package eglot-booster
  :ensure (:host github
           :repo "jdtsmith/eglot-booster")
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :init (eglot-booster-mode))

;; (use-package dape
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq dape-window-buffer-arrangement 'right))

;;;----------------------------------------------------------------
;; ** EMACS-LISP
;;;----------------------------------------------------------------
(use-package pp
  :bind (([remap eval-last-sexp] . eval-sexp-maybe-pp)
         ([remap eval-expression] . my/pp-eval-expression))
  ;; :hook (eval-expression-minibuffer-setup . my/eval-with-threading)
  :config
  (defun my/eval-with-threading ()
    "Pre-insert a threading macro for easy chaining"
    (insert "(thread-first )")
    (backward-char 1))
  
  (defun eval-sexp-maybe-pp (&optional arg)
    (interactive "P")
    (if arg
        (let ((current-prefix-arg '4))
          (call-interactively #'pp-eval-last-sexp))
      (call-interactively #'eval-last-sexp)))
  
  (defun my/pp-eval-expression (&optional insert-p)
    "Call `pp-eval-expression' on EXPR. With prefix-arg INSERT-P,
call `eval-expression' instead and insert the result into the
current buffer without truncation."
    (interactive "P")
    (if insert-p
        (let ((current-prefix-arg current-prefix-arg))
          (call-interactively #'eval-expression))
      (call-interactively #'pp-eval-expression)))
  
  (advice-add 'pp-display-expression :after
              (defun my/pp-handle-output-buffer (&rest args)
                (when-let* ((win (get-buffer-window (nth 1 args)))
                            (_ (window-live-p win)))
                  (select-window win)
                  (view-mode 1)))))

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
         ("C-c C-m" . macrostep-expand)
         :map lisp-interaction-mode-map
         ("C-c C-m" . macrostep-expand)))

(use-package elisp-mode
  :defer t
  :config
  ;; From https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
  (advice-add #'calculate-lisp-indent :override #'void~calculate-lisp-indent)
  (defun void~calculate-lisp-indent (&optional parse-start)
    "Add better indentation for quoted and backquoted lists."
    ;; This line because `calculate-lisp-indent-last-sexp` was defined with
    ;; `defvar` with its value ommited, marking it special and only defining it
    ;; locally. So if you don't have this, you'll get a void variable error.
    (defvar calculate-lisp-indent-last-sexp)
    (save-excursion
      (beginning-of-line)
      (let ((indent-point (point))
            state
            ;; setting this to a number inhibits calling hook
            (desired-indent nil)
            (retry t)
            calculate-lisp-indent-last-sexp containing-sexp)
        (cond ((or (markerp parse-start) (integerp parse-start))
               (goto-char parse-start))
              ((null parse-start) (beginning-of-defun))
              (t (setq state parse-start)))
        (unless state
          ;; Find outermost containing sexp
          (while (< (point) indent-point)
            (setq state (parse-partial-sexp (point) indent-point 0))))
        ;; Find innermost containing sexp
        (while (and retry
                    state
                    (> (elt state 0) 0))
          (setq retry nil)
          (setq calculate-lisp-indent-last-sexp (elt state 2))
          (setq containing-sexp (elt state 1))
          ;; Position following last unclosed open.
          (goto-char (1+ containing-sexp))
          ;; Is there a complete sexp since then?
          (if (and calculate-lisp-indent-last-sexp
                   (> calculate-lisp-indent-last-sexp (point)))
              ;; Yes, but is there a containing sexp after that?
              (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                              indent-point 0)))
                (if (setq retry (car (cdr peek))) (setq state peek)))))
        (if retry
            nil
          ;; Innermost containing sexp found
          (goto-char (1+ containing-sexp))
          (if (not calculate-lisp-indent-last-sexp)
              ;; indent-point immediately follows open paren.
              ;; Don't call hook.
              (setq desired-indent (current-column))
            ;; Find the start of first element of containing sexp.
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
            (cond ((looking-at "\\s(")
                   ;; First element of containing sexp is a list.
                   ;; Indent under that list.
                   )
                  ((> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp)
                   ;; This is the first line to start within the containing sexp.
                   ;; It's almost certainly a function call.
                   (if (or
                        ;; Containing sexp has nothing before this line
                        ;; except the first element. Indent under that element.
                        (= (point) calculate-lisp-indent-last-sexp)

                        ;; First sexp after `containing-sexp' is a keyword. This
                        ;; condition is more debatable. It's so that I can have
                        ;; unquoted plists in macros. It assumes that you won't
                        ;; make a function whose name is a keyword.
                        (when-let (char-after (char-after (1+ containing-sexp)))
                          (char-equal char-after ?:))

                        ;; Check for quotes or backquotes around.
                        (let* ((positions (elt state 9))
                               (last (car (last positions)))
                               (rest (reverse (butlast positions)))
                               (any-quoted-p nil)
                               (point nil))
                          (or
                           (when-let (char (char-before last))
                             (or (char-equal char ?')
                                 (char-equal char ?`)))
                           (progn
                             (while (and rest (not any-quoted-p))
                               (setq point (pop rest))
                               (setq any-quoted-p
                                     (or
                                      (when-let (char (char-before point))
                                        (or (char-equal char ?')
                                            (char-equal char ?`)))
                                      (save-excursion
                                        (goto-char (1+ point))
                                        (looking-at-p
                                         "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                             any-quoted-p))))
                       ;; Containing sexp has nothing before this line
                       ;; except the first element.  Indent under that element.
                       nil
                     ;; Skip the first element, find start of second (the first
                     ;; argument of the function call) and indent under.
                     (progn (forward-sexp 1)
                            (parse-partial-sexp (point)
                                                calculate-lisp-indent-last-sexp
                                                0 t)))
                   (backward-prefix-chars))
                  (t
                   ;; Indent beneath first sexp on same line as
                   ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                   ;; almost certainly a function call.
                   (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                       0 t)
                   (backward-prefix-chars)))))
        ;; Point is at the point to indent under unless we are inside a string.
        ;; Call indentation hook except when overridden by lisp-indent-offset
        ;; or if the desired indentation has already been computed.
        (let ((normal-indent (current-column)))
          (cond ((elt state 3)
                 ;; Inside a string, don't change indentation.
                 nil)
                ((and (integerp lisp-indent-offset) containing-sexp)
                 ;; Indent by constant offset
                 (goto-char containing-sexp)
                 (+ (current-column) lisp-indent-offset))
                ;; in this case calculate-lisp-indent-last-sexp is not nil
                (calculate-lisp-indent-last-sexp
                 (or
                  ;; try to align the parameters of a known function
                  (and lisp-indent-function
                       (not retry)
                       (funcall lisp-indent-function indent-point state))
                  ;; If the function has no special alignment
                  ;; or it does not apply to this argument,
                  ;; try to align a constant-symbol under the last
                  ;; preceding constant symbol, if there is such one of
                  ;; the last 2 preceding symbols, in the previous
                  ;; uncommented line.
                  (and (save-excursion
                         (goto-char indent-point)
                         (skip-chars-forward " \t")
                         (looking-at ":"))
                       ;; The last sexp may not be at the indentation
                       ;; where it begins, so find that one, instead.
                       (save-excursion
                         (goto-char calculate-lisp-indent-last-sexp)
                         ;; Handle prefix characters and whitespace
                         ;; following an open paren.  (Bug#1012)
                         (backward-prefix-chars)
                         (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                       (line-beginning-position))
                                         (and containing-sexp
                                              (>= (1+ containing-sexp) (point)))))
                           (forward-sexp -1)
                           (backward-prefix-chars))
                         (setq calculate-lisp-indent-last-sexp (point)))
                       (> calculate-lisp-indent-last-sexp
                          (save-excursion
                            (goto-char (1+ containing-sexp))
                            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                            (point)))
                       (let ((parse-sexp-ignore-comments t)
                             indent)
                         (goto-char calculate-lisp-indent-last-sexp)
                         (or (and (looking-at ":")
                                  (setq indent (current-column)))
                             (and (< (line-beginning-position)
                                     (prog2 (backward-sexp) (point)))
                                  (looking-at ":")
                                  (setq indent (current-column))))
                         indent))
                  ;; another symbols or constants not preceded by a constant
                  ;; as defined above.
                  normal-indent))
                ;; in this case calculate-lisp-indent-last-sexp is nil
                (desired-indent)
                (t
                 normal-indent)))))))

(use-package isayt
  :ensure (:host gitlab :repo "andreyorst/isayt.el" :protocol https)
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . isayt-mode))

;;;----------------------------------------------------------------
;; ** AUCTEX-MODE & ADDITIONS
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-latex" user-emacs-directory))

(use-package writer :commands writer-settings)

(use-package ink
  :disabled
  :load-path "plugins/ink/"
  :after latex
  :commands (ink-make-figure ink-edit-figure))

;; *** BIBTEX
(load (expand-file-name "lisp/setup-cite" user-emacs-directory))

;; *** PDFs
(if IS-GUIX
    (load-library "pdf-tools-autoloads")
  (elpaca pdf-tools))
(use-package pdf-tools
  :commands pdf-tools-install
  :bind (:map pdf-view-mode-map
         ("C-c C-r w" . pdf-view-auto-slice-minor-mode)
         ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
         ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
  :config
  (setf pdf-tools-enabled-modes
        (delq 'pdf-misc-size-indication-minor-mode
              pdf-tools-enabled-modes))
  (setq pdf-view-resize-factor 1.05))

(use-package sow
  :disabled
  :after pdf-tools)

;;;----------------------------------------------------------------
;; ** MATLAB
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-matlab" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-matlab.org" :minlevel 2
;; ---------------------------

;;;----------------------------------------------------------------
;; ** PYTHON-MODE
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-python" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-python.org" :minlevel 2
;; ---------------------------

;;;----------------------------------------------------------------
;; ** GEISER/SCHEME
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-scheme" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-scheme.org" :minlevel 2
;; ---------------------------

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

;;;################################################################
;; ** JULIA
;;;################################################################
(load (expand-file-name "lisp/setup-julia" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-julia.org" :minlevel 2
;; ---------------------------

;;;----------------------------------------------------------------
;; ** CIDER
;;;----------------------------------------------------------------
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

;;;----------------------------------------------------------------
;; ** LUA
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-lua" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-lua.org" :minlevel 2
;; ----------------------------------------------------------------

;;;################################################################
;; ** JSON
(use-package jsonian
  :disabled
  :ensure (:host github :repo "iwahbe/jsonian")
  :init (add-to-list 'magic-fallback-mode-alist
                     '("^[{[]$" . jsonian-mode))
  :after so-long
  :config (jsonian-no-so-long-mode)
  :bind (:map jsonian-mode-map
         ("C-c '" . jsonian-edit-string)
         ("C-c C-w" . jsonian-path)
         ("C-M-e" . jsonian-enclosing-item)))

;; ** PLANTUML
(use-package plantuml-mode
  :ensure t
  :defer
  :init
  (add-hook 'plantuml-mode-hook
          (lambda () (add-hook
                 'completion-at-point-functions
                 'my/plantuml-complete nil t)))
  :config
  ;; Add rudimentary CAPF support to plantuml-mode
  (defun my/plantuml-complete ()
    (unless (seq-contains-p
             [? ?	13 10] (char-before))
      (list (save-excursion
              (re-search-backward
               (rx word-boundary (1+ wordchar)) nil t)
              (point))
            (point)
            plantuml-kwdList))))

;; * PLUGINS
;;;################################################################

;;----------------------------------------------------------------
;; ** STICKER
;;----------------------------------------------------------------
(use-package emacs
  :bind ("C-c p" . sticker)
  :config
  (defvar sticker-list nil)

  (defun sticker (&optional arg label)
    (interactive "P")
    (cl-flet ((store-sticker (beg end)
                (or (assoc (buffer-substring beg end) sticker-list)
                    (push (list (buffer-substring beg end)) sticker-list))
                (deactivate-mark)
                (message "Sticker stored"))
              (new-sticker (l &optional o)
                (unless o
                  (setq o (make-overlay (line-beginning-position)
                                        (line-end-position))))
                (overlay-put o 'evaporate t)
                (overlay-put o 'sticker l)
                (overlay-put
                 o 'after-string
                 (concat " " (if (get-text-property 0 'face l)
                                 l
                               (propertize l 'face '(:inverse-video t)))))
                (push o (alist-get l sticker-list nil nil #'equal)))
              (read-label ()
                (let ((minibuffer-allow-text-properties t))
                  (completing-read "Attach sticker: " sticker-list))))
      (if (use-region-p)
          (store-sticker (region-beginning) (region-end))
        (if-let* ((exist-ov (cdr-safe (get-char-property-and-overlay (point) 'sticker))))
            (if arg
                (new-sticker (read-label) exist-ov)
              (let* ((old-l (overlay-get exist-ov 'sticker))
                     (matches (assoc old-l sticker-list)))
                (delq exist-ov matches)
                (when (= (length matches) 1)
                  (setq sticker-list (delete matches sticker-list)))
                (delete-overlay exist-ov)))
          (new-sticker (or label (read-label))))))))

;;----------------------------------------------------------------
;; ** ULTRA-SCROLL
;;----------------------------------------------------------------
(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :defer
  :init (setq scroll-conservatively 101 ; important!
              scroll-margin 0))

;;----------------------------------------------------------------
;; ** EL-SEARCH MAYBE
;;----------------------------------------------------------------
(use-package el-search
  :ensure t
  :defer
  :config
  (el-search-install-bindings-under-prefix [(meta ?s) ?s]))

;;----------------------------------------------------------------
;; ** SHOW-FONT MAYBE
;;----------------------------------------------------------------
(use-package show-font
  :ensure (:host github :repo "protesilaos/show-font")
  :defer)

;;----------------------------------------------------------------
;; ** BEANCOUNT
;;----------------------------------------------------------------
(use-package beancount
  :ensure (:host github :repo "beancount/beancount-mode")
  :defer)

;;----------------------------------------------------------------
;; ** ENVRC
;;----------------------------------------------------------------
(use-package envrc :ensure t :defer)

;;;----------------------------------------------------------------
;; ** CW
;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-cw" user-emacs-directory))

;;----------------------------------------------------------------
;; ** EDRAW
;;----------------------------------------------------------------
(use-package edraw
  :ensure (el-easydraw :host github
                       :repo "misohena/el-easydraw"
                       :main "edraw.el")
  :defer
  :mode "\\.edraw\\.svg$")

;;----------------------------------------------------------------
;; ** QRENCODE
;;----------------------------------------------------------------
(use-package qrencode :ensure t :defer)

;;----------------------------------------------------------------
;; ** INDENT-BARS MAYBE
;;----------------------------------------------------------------
(use-package indent-bars
  :ensure (:type git :host github
           :repo "jdtsmith/indent-bars")
  :hook ((python-mode julia-mode) . indent-bars-mode)
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.3)
   indent-bars-pattern "."
   indent-bars-width-frac 0.2
   indent-bars-pad-frac 0.2
   indent-bars-zigzag nil
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil))

;;----------------------------------------------------------------
;; ** DETACHED 
;;----------------------------------------------------------------
;; Testing detached
(use-package detached
  :disabled
  :ensure t
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ;; ([remap compile] . detached-compile)
         ;; ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)
           (detached-session-directory
            (file-name-concat user-cache-directory "sessions"))))

;;;----------------------------------------------------------------
;; ** VISIBLE MARK
;;----------------------------------------------------------------
(use-package visible-mark
  :ensure t
  :hook ((text-mode prog-mode conf-mode) . visible-mark-mode)
  :custom-face
  (visible-mark-face1
   ((t (:inherit region :background unspecified))))
  :config
  (setq visible-mark-faces '(visible-mark-face1))
  (setq visible-mark-max 1))

;;----------------------------------------------------------------
;; ** MACRURSORS
;;;----------------------------------------------------------------
(use-package macrursors
  :ensure (:host github :repo "corytertel/macrursors"
           :remotes ("fork" :host github :protocol ssh
                     :repo "karthink/macrursors"
                     :branch "expand-region"))
  :bind-keymap ("C-;" . macrursors-mark-map)
  :bind (("M-n" . macrursors-mark-next-instance-of)
         ("M-p" . macrursors-mark-previous-instance-of)
         :map macrursors-mode-map
         ("C-'" . macrursors-hideshow)
         ("C-;" . nil)
         ("C-; C-;" . macrursors-end)
         ("C-; C-j" . macrursors-end)
         :map isearch-mode-map
         ("C-;" . macrursors-mark-from-isearch)
         ("M-s n" . macrursors-mark-next-from-isearch)
         ("M-s p" . macrursors-mark-previous-from-isearch)
         :map macrursors-mark-map
         ("C-n" . macrursors-mark-next-line)
         ("C-p" . macrursors-mark-previous-line)
         ("C-SPC" . nil)
         ("." . macrursors-mark-all-instances-of)
         ("o" . macrursors-mark-all-instances-of)
         ("SPC" . macrursors-select)
         ("l" . macrursors-mark-all-lists)
         ("s" . macrursors-mark-all-symbols)
         ("w" . macrursors-mark-all-words)
         ("C-M-e" . macrursors-mark-all-sexps)
         ("d" . macrursors-mark-all-defuns)
         ("n" . macrursors-mark-all-numbers)
         (")" . macrursors-mark-all-sentences)
         ("M-e" . macrursors-mark-all-sentences)
         ("e" . macrursors-mark-all-lines))
  :init
  (define-prefix-command 'macrursors-mark-map)
  (use-package macrursors-select
    :bind (:map macrursors-mark-map
           ("C-g" . macrursors-select-clear)))
  (use-package macrursors-select-expand
    :bind
    (:map macrursors-mark-map
     ("," . macrursors-select-expand)
     :map macrursors-select-map
     ("-" . macrursors-select-contract)
     ("." . macrursors-select-contract)
     ("," . macrursors-select-expand)))
  
  (use-package avy
    :bind 
    (:map macrursors-mark-map
     ("j" . my/macrursors-at-avy))
    :config
    (defun my/macrursors-at-avy ()
      (interactive)
      (let* ((avy-all-windows nil)
             (positions (mapcar #'caar (avy--read-candidates))))
        (when positions
          (mapc #'macrursors--add-overlay-at-point positions)
          (macrursors-start)))))
  
  :config
  (dolist (mode '( ;;global-eldoc-mode
                  ;; gcmh-mode corfu-mode font-lock-mode
                  show-paren-mode))
    (add-hook 'macrursors-pre-finish-hook mode)
    (add-hook 'macrursors-post-finish-hook mode))
  (setq ;; macrursors-apply-keys "C-; C-;"
   macrursors-match-cursor-style t)

  (defvar macrursors-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" #'macrursors-mark-next-instance-of)
      (define-key map "p" #'macrursors-mark-previous-instance-of)
      map))
  (map-keymap (lambda (_ cmd)
                (put cmd 'repeat-map 'macrursors-repeat-map))
              macrursors-repeat-map)
  (dolist (cmd '(macrursors-mark-next-from-isearch
                 macrursors-mark-previous-from-isearch
                 macrursors-mark-next-line
                 macrursors-mark-previous-line))
    (put cmd 'repeat-map 'macrursors-repeat-map))

  (setf macrursors-mode-line nil)
  (defsubst my/mode-line-macro-recording ()
    "Display macro being recorded."
    (when (or defining-kbd-macro executing-kbd-macro)
      (let ((sep (propertize " " 'face 'highlight ))
            (vsep (propertize " " 'face '(:inherit variable-pitch))))
        ;; "●"
        (propertize
         (concat
          sep "REC" vsep
          (number-to-string kmacro-counter) vsep "▶" vsep
          (when macrursors-mode
            (if macrursors--overlays
                (format (concat "[%d/%d]" vsep)
                        (1+ (cl-count-if (lambda (p) (< p (point))) macrursors--overlays
                                         :key #'overlay-start))
                        (1+ (length macrursors--overlays)))
              (concat "[1/1]" vsep))))
         'face 'highlight)))))
  
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
  
  (cl-pushnew '(?  . activate-mark)
              (default-value 'embrace-semantic-units-alist))

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

(use-package embrace
  :after org
  :bind (:map org-mode-map
         ("M-s a" . embrace-add)
         ("M-s c" . embrace-change)
         ("M-s d" . embrace-delete)))

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

(use-package simple
  :if (> emacs-major-version 27)
  :hook (next-error . recenter)
  :config
  (setq next-error-message-highlight t
        next-error-found-function #'next-error-quit-window))

(use-package poi
  :demand t
  :bind (:map next-error-repeat-map
         ("`" . poi-action))
  :config
  (advice-add 'next-error :around #'poi-delegate))

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
;; ** UNDO-TREE DONT
;;;----------------------------------------------------------------
(use-package undo-tree
  :disabled
  :defer
  :config (setq undo-tree-enable-undo-in-region  t))

(use-package vundo
  :disabled
  :ensure (:host github :repo "casouri/vundo")
  :bind ("C-x u" . vundo)
  :config
  (set-face-attribute 'vundo-default nil :family "Symbola")
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;;----------------------------------------------------------------
;; ** SPELL CHECKING
;; *** FLYSPELL
;;;----------------------------------------------------------------
(use-package flyspell
  :commands flyspell-mode
  :bind (:map flyspell-mode-map
              ("C-M-i" . nil)
              ("C-;" . nil)
              ("C-," . nil)
              ("C-; C-4" . 'flyspell-auto-correct-previous-word)
              ;; ("C-; n" . 'flyspell-goto-next-error)
              ))

;;;----------------------------------------------------------------
;; *** SPELL-FU DONT
;;----------------------------------------------------------------
;; Disabled while I test Jinx
(use-package spell-fu
  :disabled
  :ensure t
  :commands text-spell-fu-mode
  :config
  (add-hook 'spell-fu-mode-hook
            (poi-register
             #'spell-fu--goto-next-or-previous-error))
  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary
                "en-personal"
                (concat user-cache-directory
                        ".aspell.en.pws")))))
  (defun text-spell-fu-mode ()
    (interactive)
    (setq spell-fu-faces-exclude
          '(font-lock-function-name-face
            font-lock-comment-face
            font-lock-keyword-face
            font-latex-math-face
            font-lock-variable-name-face
            font-lock-type-face
            font-lock-constant-face
            font-latex-sedate-face
            org-block-begin-line
            org-block-end-line
            org-code
            org-date
            org-drawer org-document-info-keyword
            org-ellipsis
            org-link
            org-meta-line
            org-properties
            org-properties-value
            org-special-keyword
            org-src
            org-tag
            org-verbatim))
    (spell-fu-mode)))

;;;----------------------------------------------------------------
;; *** JINX
;;----------------------------------------------------------------
(unless IS-GUIX (elpaca jinx))
(use-package jinx
  ;; :hook ((text-mode prog-mode conf-mode) . my/jinx-mode)
  :commands jinx-mode
  :bind ([remap ispell-word] . jinx-correct)
  :config
  (cl-callf
      (lambda (pl)
        (delete-dups
         (append '(font-lock-constant-face
                   TeX-fold-unfolded-face TeX-fold-folded-face)
                 pl)))
      (alist-get 'tex-mode jinx-exclude-faces))
  (cl-callf
      (lambda (pl)
        (delete-dups
         (append '(org-block)
                 (alist-get 'tex-mode jinx-exclude-faces) pl)))
      (alist-get 'org-mode jinx-exclude-faces))
  (add-hook 'jinx-mode-hook
            (poi-register 'jinx-next 'jinx-correct)))

;;----------------------------------------------------------------
;; ** ELDOC
;;----------------------------------------------------------------
(use-package eldoc
  :hook (ielm-mode . eldoc-mode)
  :config
  (setq eldoc-documentation-strategy
        'eldoc-documentation-compose-eagerly
        eldoc-echo-area-prefer-doc-buffer t))

;;----------------------------------------------------------------
;; ** DEBUGGING SUPPORT
;;----------------------------------------------------------------
;; *** EDEBUG
;;;----------------------------------------------------------------
(use-package edebug
  :defer
  :after eldoc
  :bind (:map edebug-mode-map
         ("A" . my/elisp-add-to-watch))
  :config
  (setq debugger-stack-frame-as-list t)
  (defun my/elisp-add-to-watch (&optional region-start region-end)
    "Add the current variable to the *EDebug* window"
    (interactive "r")
    (let ((statement
           (if (and region-start region-end (use-region-p))
               (buffer-substring region-start region-end)
             (symbol-name (eldoc-current-symbol)))))
      (edebug-visit-eval-list)
      (goto-char (point-max))
      (newline)
      (insert statement)
      (edebug-update-eval-list)
      (edebug-where)))
  
  ;; From jdtsmith: https://gist.github.com/jdtsmith/1fbcacfe677d74bbe510aec80ac0050c
  ;;;; Power debugging
  (defun my/reraise-error (func &rest args)
    "Call function FUNC with ARGS and re-raise any error which occurs.
Useful for debugging post-command hooks and filter functions, which
normally have their errors suppressed."
    (condition-case err
        (apply func args)
      ((debug error) (signal (car err) (cdr err)))))

  (defun toggle-debug-on-hidden-errors (func)
    "Toggle hidden error debugging for function FUNC."
    (interactive "a")
    (cond
     ((advice-member-p #'my/reraise-error func)
      (advice-remove func #'my/reraise-error)
      (message "Debug on hidden errors disabled for %s" func))
     (t
      (advice-add func :around #'my/reraise-error)
      (message "Debug on hidden errors enabled for %s" func)))))

;;----------------------------------------------------------------
;; *** INSPECTOR
;;----------------------------------------------------------------
(use-package inspector
  :ensure (:host github :repo "mmontone/emacs-inspector")
  :hook (inspector-mode . toggle-truncate-lines)
  :bind (("M-I" . inspector-inspect-expression)
         :map lisp-mode-shared-map
         ("C-c C-i" . inspector-inspect-last-sexp)
         :map edebug-mode-map
         ("<remap> <inspector-inspect-expression>" .
          inspector-inspect-edebug-expression)
         :map embark-expression-map
         ("I" . inspector-inspect-expression)
         :map embark-variable-map
         ("I" . inspector-inspect-expression)
         :map embark-defun-map
         ("I" . inspector-inspect-defun))
  :config
  (use-package popper :defer
    :config (add-to-list 'popper-reference-buffers 'inspector-mode))
  (setf (alist-get '(major-mode . inspector-mode)
                   display-buffer-alist
                   nil nil #'equal)
        `((display-buffer-in-side-window)
          (window-height . ,(lambda (win)
                              (fit-window-to-buffer win
                               (floor (frame-height) 2.5))))
          (side . bottom)
          (slot . -4)
          (body-function . ,#'select-window))))

;;----------------------------------------------------------------
;; ** FLYMAKE
;;;----------------------------------------------------------------
(use-package flymake
  :defer
  :init
  (add-hook 'flymake-mode-hook
            (poi-register 'flymake-goto-next-error)))

;; DONT
(use-package flymake-diagnostic-at-point
  :disabled
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config (setq flymake-diagnostic-at-point-display-diagnostic-function
                'flymake-diagnostic-at-point-display-popup))

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'point) 
  ;; 'point to show errors only on point
  ;; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)
        sideline-flymake-show-backend-name t))

(use-package package-lint :ensure t :defer)

(use-package package-lint-flymake
  :ensure t
  :after flymake
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda () 
              (add-hook 'flymake-diagnostic-functions
                        #'package-lint-flymake
                        nil :local))))

(use-package flymake-proselint
  :ensure t
  :after flymake
  :hook ((markdown-mode org-mode text-mode) . flymake-proselint-setup))

;;;----------------------------------------------------------------
;; ** BROWSE-URL
;;;----------------------------------------------------------------
(use-package browse-url
  :commands (browse-url-umpv)
  :config
  (when IS-LINUX
    ;; I don't want unnecessary child processes
    (defun my/systemd-run (&rest process-args)
      (if (executable-find "systemd-run")
          (apply #'start-process (car process-args) nil
                 "systemd-run" "--user"
                 (concat "--unit=emacs_media_"
                         (format-time-string "%m%d%H%M%S_%3N"))
                 "--property=StandardOutput=null"
                 "--property=StandardError=null"
                 "--quiet" process-args)
        (apply #'start-process (car process-args) nil
               "nohup" process-args)))

    (defun browse-url-umpv (url &optional single)
      (my/systemd-run (if single "mpv" "umpv")
                      (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv-enqueue (url &optional _)
      (condition-case nil
          (let ((enqueue-proc
                 (make-network-process
                  :name "mpv" :buffer nil
                  :service "/tmp/mpvsocket"
                  :family 'local :coding 'utf-8
                  :sentinel
                  (lambda (proc _)
                    (unless (= (process-exit-status proc) 0)
                      (browse-url-mpv url))))))
            (process-send-string
             enqueue-proc
             (concat (json-serialize
                      `(:command [ "loadfile" ,url "append-play" ]))
                     "\n")))
        (error (browse-url-mpv url))))

    (defun browse-url-mpv-enqueue (url &optional _)
      (let ((exit-status
             (call-process "umpv_last" nil nil nil
                           (shell-quote-wildcard-pattern url))))
        (message "%S" exit-status)
        (when (not (= exit-status 0))
          (browse-url-mpv url))))

    (defun browse-url-mpv-hd (url &optional _)
      (my/systemd-run
       "mpv" "--profile=protocol-hd-video"
       (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv-audio (url &optional _)
      (my/systemd-run
       "mpv" "--video=no" "--force-window=yes"
       (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv (url &optional _)
      (pcase (prefix-numeric-value current-prefix-arg)
        (4 (browse-url-mpv-hd url))
        (0 (browse-url-mpv-audio url))
        (1 (my/systemd-run "mpv"
            (shell-quote-wildcard-pattern url)))
        (_ (browse-url-mpv-enqueue url))))

    (setq browse-url-new-window-flag t)))

;;;----------------------------------------------------------------
;; ** TRANSIENT
;;;----------------------------------------------------------------

(use-package transient
  :ensure (:host github :repo "magit/transient")
  :defines toggle-modes
  :bind (("<f8>"  . toggle-modes)
         ("C-c b" . toggle-modes))
  :custom
  (transient-levels-file (dir-concat user-cache-directory "transient/levels.el"))
  (transient-values-file (dir-concat user-cache-directory "transient/values.el"))
  :config
  (defun mode-cycle (mode1 mode2)
    "Cycle between mode1, mode2 and neither enabled."
    (lambda ()
      (interactive)
      (cond 
       ((and (boundp (symbol-value mode1))
             (symbol-value mode1))
        (progn (call-interactively (symbol-function mode1))
               (call-interactively (symbol-function mode2))))
       ((and (boundp (symbol-value mode2))
             (symbol-value mode2))
        (call-interactively (symbol-function mode2)))
       (t (call-interactively (symbol-function mode1))))))
  
  (mode-cycle 'electric-pair-mode 'smartparens-mode)
  
  (transient-bind-q-to-quit)
  ;; (setq transient-display-buffer-action
  ;;       '(display-buffer-below-selected (dedicated . t) (inhibit-same-window . t)))
  (setq transient-history-file (dir-concat user-cache-directory "transient/history.el")
        transient-levels-file (dir-concat user-cache-directory "transient/levels.el")
        transient-values-file (dir-concat user-cache-directory "transient/values.el")
        transient-show-popup t)
  (transient-define-prefix toggle-modes ()
    "Turn on and off various frequently used modes."
    
    [;:pad-keys t
     ["Appearance"
      ("t" "color theme" my/toggle-theme)
      ;; ("B" "BIG mode"    presentation-mode)
      ;; ("M" "smart modeline" ignore)
      ("8" "pretty symbols" (lambda () (interactive)
                              (if (derived-mode-p 'org-mode)
                                  (org-toggle-pretty-entities)
                                (call-interactively
                                 #'prettify-symbols-mode))))
      ("ls"
       (lambda () (concat "line spc"
                     (when line-spacing
                       (propertize
                        (format " %.2f" line-spacing)
                        'face 'font-lock-comment-face))))
       (lambda () (interactive)
         (setq line-spacing
               (read-number "Spacing: "))))
      ("vl" "visual lines" visual-line-mode)
      ("vt" "trunc lines" toggle-truncate-lines)
      ("vo" "olivetti"    olivetti-mode)
      ("vf" "visual fill" visual-fill-column-mode)]

     ["Org"
      :if-derived org-mode
      ("om" "Modern" org-modern-mode) 
      ("o\\" "Pretty" org-toggle-pretty-entities)
      ("o/" "Emphasis" (lambda () (interactive)
                            (if (bound-and-true-p org-appear-mode)
                                (progn (org-appear-mode -1)
                                       (setq-local org-hide-emphasis-markers nil))
                              (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
                              ;; (org-appear-mode 1)
                              )))
      ("oi" "Indent" org-indent-mode)
      ("on" "Numbers" org-num-mode)
      ("ol" "LaTeX" org-latex-preview-auto-mode)
      ("ow" "CAPF" my/toggle-writing-capf)]

     ["Markdown"
      :if-derived markdown-mode
      ("o/" "Emphasis" markdown-toggle-markup-hiding)
      ("ou" "url" markdown-toggle-url-hiding)
      ("os" "src" markdown-toggle-fontify-code-blocks-natively)
      ("ow" "CAPF" my/toggle-writing-capf)]

     ["Editing"
      ("r" "read only" read-only-mode)
      ("n" "line numbers" display-line-numbers-mode)
      ("M-q" "auto fill" auto-fill-mode)
      ("fc" "fill column" set-fill-column)
      ("se" (lambda () (if sentence-end-double-space
                    "double spc" "single spc"))
       (lambda () (interactive)
         (setq-local sentence-end-double-space
                     (not sentence-end-double-space))))
      ("i" "ispell" jinx-mode)
      ;; ("V" "view mode" view-mode)
      ("<tab>" "outline" outline-minor-mode
       :if (lambda () (not (derived-mode-p 'outline-mode))))]

     ["Highlight"
      ("hl" "line" hl-line-mode)
      ("hp" "paren" (lambda ()
                        (interactive)
                        (cond 
                         ((bound-and-true-p paren-face-mode)
                          (progn (call-interactively #'paren-face-mode)
                                 (call-interactively #'rainbow-delimiters-mode)))
                         ((bound-and-true-p rainbow-delimiters-mode)
                          (call-interactively #'rainbow-delimiters-mode))
                         (t (call-interactively #'paren-face-mode)))))
      ("hw" "whitespace" whitespace-mode)
      ("hd" "delimiters" rainbow-delimiters-mode)
      ("hr" "rainbow" rainbow-mode)
      ("hc" "cursor" my/hide-cursor-mode)]

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
      ("dd" "debug (err)" toggle-debug-on-error)
      ("dg" "debug (quit)" (lambda () (interactive)
                             (cl-callf not debug-on-quit)
                             (message "Debug on quit %sset"
                                      (if debug-on-quit "" "un"))))
      ("g" "diff-hl" (lambda (&optional arg) (interactive "P")
                         (if (null arg) (diff-hl-mode 'toggle)
                           (let ((ref (read-string "Reference revision for diff-hl: ")))
                             (setq-local diff-hl-reference-revision ref)
                             (diff-hl-mode) (diff-hl-update)
                             (message "Showing changes against %s" ref)))))
      ("fm" "flymake" (lambda (arg) (interactive "P")
                       (if (not arg)
                           (call-interactively #'flymake-mode)
                         (let* ((linters (remq t flymake-diagnostic-functions))
                                (active (completing-read-multiple
                                         "Exclude linters: "
                                         linters nil t)))
                           (dolist (linter active)
                             (remove-hook 'flymake-diagnostic-functions
                                          (intern-soft linter) :local))
                           (call-interactively #'flymake-mode)))))
      ("p" "smartparens" smartparens-mode)]]))

;;;----------------------------------------------------------------
;; ** HYDRAS DONT
;;;----------------------------------------------------------------
(use-package hydra
  :disabled
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

  :bind
  (:map 'smerge-mode-map
   ("C-c s" . hydra-smerge/body)
   ("C-c <tab>" . hydra-outline/body)))

;;;----------------------------------------------------------------
;; ** TABS!TABS!TABS!
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-tabs" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-tabs.org" :minlevel 2
;; ----------------------------------------------------------------

;;;----------------------------------------------------------------
;; ** HIGHLIGHTS
;; *** BEACON DONT
;;----------------------------------------------------------------
(use-package beacon
  :disabled
  :ensure t
  :bind ("C-x l" . beacon-blink)
  :defer 4
  :config
  (setq beacon-blink-delay 0.1)
  (beacon-mode 1))

;;----------------------------------------------------------------
;; *** PULSE
;;----------------------------------------------------------------
(use-package pulse
  :bind ("C-x l" . my/pulse-line)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit next-error))))
  (pulse-highlight-face ((t (:inherit next-error))))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my/recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file) . my/recenter-and-pulse-line))
  :config
  (setq pulse-delay 0.01
        pulse-iterations 15)
  :init
  (add-hook 'server-after-make-frame-hook
            (defun my/pulse-type ()
              (when (and (not pulse-flag)
                         (pulse-available-p))
                (setq pulse-flag t))))
  
  (with-no-warnings
    (defun my/pulse-line (&optional arg)
      "Pulse line at point"
      (interactive "P")
      (let ((pulse-command-advice-flag t))
        (if arg
            (pulse-momentary-highlight-region
             (region-beginning) (region-end))
          (pulse-line-hook-function))))
    
    (defun my/pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line))

    (defun my/pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (pulse-momentary-highlight-one-line)))

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
      (let ((pulse-delay 0.02)
            (pulse-iterations 10))
        (save-excursion
          (move-to-window-line next-screen-context-lines)
          (my/pulse-momentary-line))))

    (defun my/pulse-momentary-lower-bound (&rest _)
      "Pulse the lower scrolling bound of the screen."
      (let ((pulse-delay 0.02)
            (pulse-iterations 10))
        (save-excursion
          (move-to-window-line (- (1+ next-screen-context-lines)))
          (my/pulse-momentary-line))))

    (advice-add 'scroll-up-command   :after #'my/pulse-momentary-upper-bound)
    (advice-add 'scroll-down-command :after #'my/pulse-momentary-lower-bound)

    (dolist (cmd '(;; recenter-top-bottom treemacs-select-window
                   ;; goto-last-change
                   switchy-window windmove-do-window-select
                   ace-window aw--select-window
                   pager-page-down pager-page-up
                   winum-select-window-by-number
                   pop-to-mark-command pop-global-mark
                   symbol-overlay-basic-jump))
      (advice-add cmd :after #'my/pulse-momentary-line))))

;;;----------------------------------------------------------------
;; ** WALLABAG DONT
;;;----------------------------------------------------------------

;; Wallabag is a self-hosted read-it-later service for webpages and articles.
;; There are multiple ways of interacting with Wallabag in Emacs.
;;
;; The "minimal" version, disabled here, is a little library that provides a
;; "post-to-wallabag" command, like a browser bookmarklet:
(use-package wallabag
  :disabled
  :defer t
  ;; :load-path "plugins/wallabag/"
  :commands wallabag-post-entry
  :config
  (setq wallabag-host my-wallabag-host)
  (setq wallabag-username my-wallabag-username)
  (with-eval-after-load 'embark
    (define-key embark-url-map (kbd "R")
      (defun embark-wallabag (url)
        (wallabag-post-entry url)))))

;; The "full" version is a full-fledged Wallabag client, including a local
;; database, reader, Elfeed-style listings and more. With some elbow grease, I
;; can switch seamlessly from Elfeed ↔ Wallabag ↔ EWW ↔ Org, making some
;; progress towards Emacs' promise of integration.

(load (expand-file-name "lisp/setup-wallabag" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-wallabag.org" :minlevel 2
;; ----------------------------------------------------------------

;;;----------------------------------------------------------------
;; ** +NAV-FLASH+ DONT
;;;----------------------------------------------------------------
(use-package nav-flash :disabled)

;;;----------------------------------------------------------------
;; ** FOLDING AND NAVIGATION
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-folds" user-emacs-directory))

;; ------------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-folds.org" :minlevel 2 :only-contents nil
;; ------------------------------------------------------------------

;;;----------------------------------------------------------------
;; ** DIFF 
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-diff" user-emacs-directory))

;;;----------------------------------------------------------------
;; *** INLINE DIFF
;;;----------------------------------------------------------------
(use-package inline-diff
  :ensure (:repo "https://code.tecosaur.net/tec/inline-diff.git")
  :bind (:map inline-diff-overlay-map
         ("M-a" . nil)
         ("M-k" . nil)
         ("SPC" . inline-diff-apply)
         ("DEL" . inline-diff-reject)
         :map inline-diff-repeat-map
         ("M-a" . nil)
         ("M-k" . nil)
         ("SPC" . inline-diff-apply)
         ("DEL" . inline-diff-reject)))

;;;----------------------------------------------------------------
;; ** VERSION CONTROL
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-vc" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-vc.org" :minlevel 2
;; ---------------------------

;;;----------------------------------------------------------------
;; ** WHICH-KEY
;;;----------------------------------------------------------------
(use-package which-key
  :ensure t
  :defer 10
  :bind
  (:map help-map
   ("h" . which-key-show-major-mode))
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
  
  ;; (which-key-mode +1)
  :diminish "")

;;;----------------------------------------------------------------
;; ** CALC
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-calc" user-emacs-directory))

;;;----------------------------------------------------------------
;; ** ISEARCH
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-isearch" user-emacs-directory))

;;;----------------------------------------------------------------
;; ** ABBREV MODE
;;;----------------------------------------------------------------
(use-package abbrev
  :hook ((prog-mode text-mode) . abbrev-mode)
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
       ;; (electric-pair-mode +1)
       (setq electric-pair-inhibit-predicate
             `(lambda (c)
                (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c)))))

(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          fennel-mode scheme-mode
          ielm-mode markdown-mode
          git-commit-mode)
         . smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("M-["           . sp-backward-slurp-sexp)
        ("M-]"           . sp-forward-slurp-sexp)
        ("M-{"           . sp-backward-barf-sexp)
        ("M-}"           . sp-forward-barf-sexp)
        ("M-U"           . sp-raise-sexp)
        ("M-R"           . raise-sexp)
        ("M-C"           . sp-convolute-sexp)
        ("M-D"           . my/sp-duplicate-sexp)
        ("M-J"           . sp-join-sexp)
        ("M-S"           . sp-split-sexp)
        ("C-M-<up>"      . sp-raise-sexp)
        ("C-<right>"     . sp-forward-slurp-sexp)
        ("C-<left>"      . sp-backward-slurp-sexp)
        ("M-<right>"     . sp-forward-barf-sexp)
        ("M-<left>"      . sp-backward-barf-sexp)
        ("M-K"           . sp-kill-hybrid-sexp)
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
  (add-hook 'show-smartparens-mode-hook
            (lambda ()
              "Disable \\[show-paren-mode] when \[[show-smartparens-mode]] is enabled."
              (if show-smartparens-mode
                  (show-paren-mode -1)
                (show-paren-mode 1))))
  :config
  (sp-pair "`" nil :actions :rem)
  (sp-local-pair '(markdown-mode julia-mode)
                 "`" "`" :actions '(insert wrap autoskip))
  (sp-with-modes '(markdown-mode)
    (sp-local-pair "_" "_" :actions '(wrap autoskip))
    (sp-local-pair "*" "*" :actions '(wrap autoskip))
    (sp-local-pair "'" nil :actions '(wrap)))
  (sp-local-pair '(message-mode text-mode notmuch-message-mode)
                 "`" "'" :actions '(insert wrap autoskip))
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "`" "'" :actions '(wrap autoskip))
    (sp-local-pair "'" nil :actions nil))

  (defun my/sp-duplicate-sexp (&optional arg)
    (interactive "p")
    (insert (string-trim-left
             (buffer-substring 
              (save-excursion
                (backward-sexp)
                (point))
              (point)))))
  
  (defvar lisp-navigation-map
    (let ((map (make-sparse-keymap)))
      (pcase-dolist (`(,k . ,f)
                     '(("u" . backward-up-list)
                       ("f" . forward-sexp)
                       ("b" . backward-sexp)
                       ("d" . down-list)
                       ("n" . sp-next-sexp)
                       ("p" . sp-previous-sexp)
                       ("k" . sp-kill-sexp)
                       ("K" . sp-kill-hybrid-sexp)
                       ("]" . sp-forward-slurp-sexp)
                       ("[" . sp-backward-slurp-sexp)
                       ("}" . sp-forward-barf-sexp)
                       ("{" . sp-backward-barf-sexp)
                       ("r" . raise-sexp)
                       ("C" . sp-convolute-sexp)
                       ("D" . my/sp-duplicate-sexp)
                       ("J" . sp-join-sexp)
                       ("S" . sp-split-sexp)
                       ("R" . sp-raise-sexp)
                       ("\\" . indent-region)
                       ("t" . transpose-sexps)
                       ("e" . eval-sexp-maybe-pp)))
        (define-key map (kbd k) f))
      (dolist (n (number-sequence 48 57))
        (define-key map `[,n] 'digit-argument))
      map))

  (map-keymap
   (lambda (_ cmd)
     (put cmd 'repeat-map 'lisp-navigation-map))
   lisp-navigation-map)
  (put 'kill-sexp 'repeat-map 'lisp-navigation-map))

;;;----------------------------------------------------------------
;; ** AVY
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-avy" user-emacs-directory))

;;;----------------------------------------------------------------
;; ** IY-GO-TO-CHAR DONT
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
  :hook (((text-mode tex-mode conf-mode) . wrap-region-mode)
         (wrap-region-mode . (lambda () (wrap-region-remove-wrapper "'"))))
  :config
  (wrap-region-add-wrappers
   '(("/" "/" nil 'org-mode)
     ("+" "+" nil 'org-mode)
     ("=" "=" nil 'org-mode)
     ("~" "~" nil 'org-mode)
     ("*" "*" nil 'markdown-mode)
     ("*" "*" nil 'org-mode)
     ("_" "_" nil '(org-mode markdown-mode)))))

;;;################################################################
;; ** TRAMP
;;;################################################################
;; Tramp ssh'es into root@host to edit files. The emacs sudo, kindof.
(use-package tramp
  :commands (sudo-find-file sudo-this-file)
  :bind ("C-x C-S-f" . sudo-find-file)
  :config
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:abode.karthinks.com:")
                     "direct-async-process" t))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
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
  (defun sudo-this-file ()
    "Open the current file as root."
    (interactive)
    (sudo-find-file (file-truename buffer-file-name)))
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
;;----------------------------------------------------------------
(use-package dot-mode
  :ensure t
  :commands dot-mode
  :bind (:map dot-mode-map
         ("C-c ." . nil)
         ("C-M-." . nil))
  :hook ((prog-mode conf-mode text-mode tex-mode) . 'dot-mode-on))

;;----------------------------------------------------------------
;; ** BOOKMARKS
;;----------------------------------------------------------------
(use-package bookmark
  :config
  (setq bookmark-default-file (dir-concat user-cache-directory "bookmarks")))

;;----------------------------------------------------------------
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
        try-expand-whole-kill
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;;----------------------------------------------------------------
;; ** COMPANY-MODE
;;;----------------------------------------------------------------
(use-package company
  :ensure t
  :bind (:map company-active-map
         ("C-w" . nil)
         ("M-." . company-show-location)))

;;;----------------------------------------------------------------
;; ** YASNIPPET
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-yas" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-yas.org" :minlevel 2
;; ---------------------------

;;;----------------------------------------------------------------
;; ** M O V E C
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-orderless" user-emacs-directory))
(load (expand-file-name "lisp/setup-vertico" user-emacs-directory))
(load (expand-file-name "lisp/setup-marginalia" user-emacs-directory))
(load (expand-file-name "lisp/setup-embark" user-emacs-directory))
(load (expand-file-name "lisp/setup-consult" user-emacs-directory))

;;;----------------------------------------------------------------
;; ** CORFU + CAPE
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-corfu" user-emacs-directory))
(setq tab-always-indent 'complete)

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

;;----------------------------------------------------------------
;; *** ELMO DONT
;;----------------------------------------------------------------

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
;; ** +IVY COUNSEL SWIPER+ DONT
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
  :config
  (defun counsel-spotify-start-search ()
    (interactive)
    (counsel-M-x "counsel-spotify-search-"))
  (setq counsel-spotify-service-name "spotify")
  (setq counsel-spotify-client-id my-counsel-spotify-client-id
        counsel-spotify-client-secret my-counsel-spotify-client-secret
        counsel-spotify-use-notifications nil))

;;;----------------------------------------------------------------
;; * ORG-MODE
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-org" user-emacs-directory))

;; --------------------
;; #+INCLUDE: "./lisp/setup-org.org" :minlevel 2
;; --------------------

;;;----------------------------------------------------------------
;; ** ORG-ADDONS
;;;----------------------------------------------------------------
;; *** ANKI
(load (expand-file-name "lisp/setup-anki" user-emacs-directory))

;;----------------------------------------------------------------
;; *** ROAM
;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-roam" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-roam.org" :minlevel 2
;; ---------------------------

;;################################################################
;; * APPLICATIONS
;;################################################################

;;----------------------------------------------------------------
;; ** PROFILER
;;----------------------------------------------------------------
(use-package profiler
  :bind ("C-<f9>" . my/run-profiler)
  :config
  (defun my/run-profiler ()
    (interactive)
    (if (and (fboundp 'profiler-running-p)
             (profiler-running-p))
        (prog1 (profiler-stop)
          (profiler-report))
      (profiler-reset)
      (profiler-start 'cpu)
      (message "CPU Profiler started."))))

;;----------------------------------------------------------------
;; ** DIRED
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-dired" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-dired.org" :minlevel 2
;; ---------------------------

(use-package emacs
  :after dired
  :config
  (use-package ffmpeg-crop
    :load-path "plugins/ffmpeg-crop/"
    :commands (ffmpeg-crop ffmpeg-crop-dired)))

;;----------------------------------------------------------------
;; ** ERC
;;----------------------------------------------------------------
(use-package erc
  :commands (erc-tls erc)
  :bind (:map erc-mode-map
         ("\M-(" . insert-parentheses-sentence))
  :config
  (setq erc-server "irc.libera.chat"
        erc-port 6667
        erc-nick "karthik"
        erc-user-full-name "Karthik"
        erc-prompt-for-password t
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
  :ensure t
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
  :ensure t
  :after erc
  :hook (erc-mode . erc-hl-nicks-mode))

;;;----------------------------------------------------------------
;; ** EMAIL
;;;----------------------------------------------------------------
;; Left unchecked, every program grows to the point where it can be
;; used to manage your email.
(load (expand-file-name "lisp/setup-email" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-email.org" :minlevel 3
;; ----------------------------------------------------------------

;;;----------------------------------------------------------------
;; ** ELFEED
;;;----------------------------------------------------------------
;;; (load (expand-file-name "lisp/setup-reading" user-emacs-directory))
(load (expand-file-name "lisp/setup-elfeed" user-emacs-directory))

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-elfeed.org" :minlevel 2
;; ----------------------------------------------------------------

;;;----------------------------------------------------------------
;; ** EWW
;;;----------------------------------------------------------------
(require 'setup-eww)

;;;----------------------------------------------------------------
;; ** YTEL DONT
;;;----------------------------------------------------------------
;; ytel provides an elfeed-like interface to search invidious instances for
;; youtube videos. Phew. The churn rate of Invidious urls is quite high, which
;; makes this flaky, but anything's better than the browser interface to
;; Youtube.
;;
;; Replaced by cw
;; (use-package setup-ytel)

;; ----------------------------------------------------------------
;; #+INCLUDE: "./lisp/setup-ytel.org" :minlevel 2
;; ----------------------------------------------------------------

;;;----------------------------------------------------------------
;; ** NOV.EL
;;;----------------------------------------------------------------
(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook ((nov-mode . my/nov-display-setup)
         (nov-mode . er/add-text-mode-expansions))
  :bind (:map nov-mode-map
         ("u" . my/scroll-down-half)
         ("d" . my/scroll-up-half))
  :config
  (use-package setup-reading
    :disabled
    :hook (nov-post-html-render . my/reader-center-images))
  
  (setq nov-text-width 72
        nov-save-place-file (dir-concat user-cache-directory "nov-places"))
  ;; Pinched from https://tecosaur.github.io/emacs-config/config.html
  (defun my/nov-display-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.0
                             :width 'semi-expanded)
    ;; (face-remap-add-relative 'default :height 1.1)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors t)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width (1+ nov-text-width))
    (visual-fill-column-mode 1)))

;;;----------------------------------------------------------------
;; ** LOOK UP
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-lookup" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-lookup.org" :minlevel 2
;; ---------------------------

(use-package emacs
  :bind (("C-c SPC" . my/easy-page))
  :config
  (defvar-keymap my-pager-map
    :doc "Keymap with paging commands"
    "SPC" 'scroll-up-command
    "C-l" 'recenter-top-bottom
    "C-M-v" 'scroll-other-window
    "C-M-S-v" 'scroll-other-window-down
    "d" (lambda ()
          (interactive)
          (pixel-scroll-precision-interpolate
           (- (floor (window-text-height nil t) 2))
           nil 1))

    "u" (lambda ()
          (interactive)
          (pixel-scroll-precision-interpolate
           (floor (window-text-height nil t) 2)
           nil 1))
    "M-o" (if (fboundp 'switchy-window-minor-mode)
              'switchy-window 'my/other-window)
    "S-SPC" 'scroll-down-command)
  (let ((scrolling (propertize  "SCRL" 'face '(:inherit highlight)))
        ml-buffer)
    (defalias 'my/easy-page
      (lambda ()
        (interactive)
        (when (eq (window-buffer (selected-window))
                  (current-buffer))
          (setq ml-buffer (current-buffer))
          (add-to-list 'mode-line-format scrolling)
          (set-transient-map
           my-pager-map t
           (lambda () (with-current-buffer ml-buffer
                   (setq mode-line-format
                         (delete scrolling mode-line-format))))))))))

;;;----------------------------------------------------------------
;; *** gptel
;;;----------------------------------------------------------------
(use-package gptel-rewrite
  :after gptel
  :bind (:map gptel-rewrite-actions-map
         ("C-c C-i" . gptel--rewrite-inline-diff))
  :config
  (defun gptel--rewrite-inline-diff (&optional ovs)
    (interactive (list (gptel--rewrite-overlay-at)))
    (unless (require 'inline-diff nil t)
      (user-error "Inline diffs require the inline-diff package."))
    (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
                ((buffer-live-p ov-buf)))
      (with-current-buffer ov-buf
        (cl-loop for ov in (ensure-list ovs)
                 for ov-beg = (overlay-start ov)
                 for ov-end = (overlay-end ov)
                 for response = (overlay-get ov 'gptel-rewrite)
                 do (delete-overlay ov)
                 (inline-diff-words
                  ov-beg ov-end response)))))
  (when (boundp 'gptel--rewrite-dispatch-actions)
    (add-to-list
     'gptel--rewrite-dispatch-actions '(?i "inline-diff")
     'append)))

;; gptel: A simple LLM client
(use-package gptel
  ;; :straight (:local-repo "~/.local/share/git/gptel/")
  :ensure (:host github :protocol ssh
           :repo "karthink/gptel")
  :commands (gptel gptel-send)
  :hook ((eshell-mode . my/gptel-eshell-keys))
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . gptel-send)
         ("C-c j" . gptel-menu)
         ("C-c C-g" . gptel-abort)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic)
         :map embark-region-map
         ("+" . gptel-add)
         :map this-buffer-file-map
         ("+" . gptel-add))
  :config
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(deepseek-r1-distill-llama-70b
              llama-3.3-70b-versatile llama-3.1-8b-instant
              mixtral-8x7b-32768 gemma-7b-it))

  (defvar gptel--anthropic
    (gptel-make-anthropic "Claude" :key gptel-api-key :stream t))
  (setq-default gptel-model 'claude-3-5-haiku-20241022
                gptel-backend gptel--anthropic
                gptel-display-buffer-action '(pop-to-buffer-same-window))

  (gptel-make-anthropic "Claude-thinking"
    :key #'gptel-api-key-from-auth-source
    :stream t
    :models '(claude-3-7-sonnet-20250219)
    :header (lambda () (when-let* ((key (gptel--get-api-key)))
                    `(("x-api-key" . ,key)
                      ("anthropic-version" . "2023-06-01")
                      ("anthropic-beta" . "pdfs-2024-09-25")
                      ("anthropic-beta" . "output-128k-2025-02-19")
                      ("anthropic-beta" . "prompt-caching-2024-07-31"))))
    :request-params '(:thinking (:type "enabled" :budget_tokens 1024)
                      :max_tokens 2048))

  (defvar gptel--togetherai
    (gptel-make-openai "TogetherAI"
      :host "api.together.xyz"
      :key gptel-api-key
      :stream t
      :models '(mistralai/Mixtral-8x7B-Instruct-v0.1
                codellama/CodeLlama-13b-Instruct-hf
                codellama/CodeLlama-34b-Instruct-hf)))

  (gptel-make-deepseek "Deepseek"
    :key #'gptel-api-key-from-auth-source
    :stream t)

  (gptel-make-perplexity "Perplexity"
    :stream t
    :key #'gptel-api-key-from-auth-source)

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(deepseek/deepseek-r1-distill-llama-70b:free
              deepseek/deepseek-r1-distill-llama-70b:free))

  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key (lambda () (auth-source-pass-get 'secret "api/api.github.com"))
    :models '(DeepSeek-R1 gpt-4o-mini))

  (with-eval-after-load 'gptel-gemini
    (defvar gptel--gemini
      (gptel-make-gemini "Gemini" :key gptel-api-key :stream t)))

  (with-eval-after-load 'gptel-ollama
    (defvar gptel--ollama
      (gptel-make-ollama
          "Ollama"
        :host "192.168.0.59:11434"
        :models '(mistral:latest zephyr:latest openhermes:latest
                  llama3.1:8b llama3.2:3b deepseek-r1:8b
                  (llava:7b :description Llava 1.6: Vision capable model
                   :capabilities (media)
                   :mime-types (image/jpeg image/png)))
        :stream t)))

  (defvar gptel--gpt4all
    (gptel-make-gpt4all
        "GPT4All"
      :protocol "http"
      :host "localhost:4891"
      :models '(mistral-7b-openorca.Q4_0.gguf)))

  (defalias 'my/gptel-easy-page
    (let ((map (make-composed-keymap
                (define-keymap
                  "RET" 'gptel-end-of-response
                  "n"   'gptel-end-of-response
                  "p"   'gptel-beginning-of-response)
                my-pager-map))
          (scrolling
           (propertize  "SCRL" 'face '(:inherit highlight))))
      (require 'pixel-scroll)
      (lambda ()
        (interactive)
        (when (eq (window-buffer (selected-window))
                  (current-buffer))
          (add-to-list 'mode-line-format scrolling)
          (set-transient-map
           map t
           (lambda () (setq mode-line-format
                       (delete scrolling mode-line-format))))))))
  (add-hook 'gptel-pre-response-hook 'my/gptel-easy-page)
  (define-key global-map (kbd "C-c SPC") 'my/gptel-easy-page)

  (auth-source-pass-enable)
  (defun my/gptel-remove-headings (beg end)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward org-heading-regexp end t)
          (forward-line 0)
          (delete-char (1+ (length (match-string 1))))
          (insert-and-inherit "*")
          (end-of-line)
          (skip-chars-backward " \t\r")
          (insert-and-inherit "*")))))
  (add-hook 'gptel-post-response-functions #'my/gptel-remove-headings)

  ;; (with-eval-after-load 'gptel-transient
  ;;   (transient-suffix-put 'gptel-menu (kbd "-m") :key "M")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-c") :key "C")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-n") :key "N")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-t") :key "T"))

  (defun my/gptel-code-infill ()
    "Fill in code at point based on buffer context.  Note: Sends the whole buffer."
    (let ((lang (gptel--strip-mode-suffix major-mode)))
      `(,(format "You are a %s programmer and assistant in a code buffer in a text editor.

Follow my instructions and generate %s code to be inserted at the cursor.
For context, I will provide you with the code BEFORE and AFTER the cursor.


Generate %s code and only code without any explanations or markdown code fences.  NO markdown.
You may include code comments.

Do not repeat any of the BEFORE or AFTER code." lang lang lang)
        nil
        "What is the code AFTER the cursor?"
        ,(format "AFTER\n```\n%s\n```\n"
          (buffer-substring-no-properties
           (if (use-region-p) (max (point) (region-end)) (point))
           (point-max)))
        "And what is the code BEFORE the cursor?"
        ,(format "BEFORE\n```%s\n%s\n```\n" lang
          (buffer-substring-no-properties
           (point-min)
           (if (use-region-p) (min (point) (region-beginning)) (point))))
        ,@(when (use-region-p) "What should I insert at the cursor?"))))

  (setq gptel-directives
        `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications.  Speak in specific,
 topic relevant terminology.  Do NOT hedge or qualify.  Speak directly and be willing to make creative guesses.

Explain your reasoning.  if you don’t know, say you don’t know.  Be willing to reference less reputable sources for
 ideas.

Do NOT summarize your answers.

If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.

 Never apologize.  Ask questions when unsure.")
          (code-infill . ,#'my/gptel-code-infill)
          (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.  Do NOT use markdown backticks (```) to format your response.")
          (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
          (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
          (explain . "Explain what this code does to a novice programmer.")
          (tutor . "You are a tutor and domain expert in the domain of my questions.  You will lead me to discover the answer myself by providing hints.  Your instructions are as follows:
- If the question or notation is not clear to you, ask for clarifying details.
- At first your hints should be general and vague.
- If I fail to make progress, provide more explicit hints.
- Never provide the answer itself unless I explicitly ask you to.  If my answer is wrong, again provide only hints to correct it.
- If you use LaTeX notation, enclose math in \\( and \\) or \\[ and \\] delimiters.")))
  (setq gptel--system-message (alist-get 'default gptel-directives)
        gptel-default-mode 'org-mode)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: "
        (alist-get 'org-mode gptel-response-prefix-alist) "*Response*:\n"
        (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")
  (with-eval-after-load 'gptel-org
    (setq-default gptel-org-branching-context t))

  (defun my/gptel-eshell-send (&optional arg)
    (interactive "P")
    (if (use-region-p)
        (gptel-send arg)
      (push-mark)
      (or (eshell-previous-prompt 0)
          (eshell-previous-prompt 1))
      (activate-mark)
      (gptel-send arg)
      (exchange-point-and-mark)
      (deactivate-mark)))
  (defun my/gptel-eshell-keys ()
    (define-key eshell-mode-map (kbd "C-c <return>")
                #'my/gptel-eshell-send))
  ;; Testing
  (defun gptel-rewrite-commit-message ()
    (when (and (string-match-p "COMMIT_EDITMSG" (buffer-name))
               (derived-mode-p 'text-mode))
      "You are a git expert.  What you write will be passed to git commit -m \"[message]\".
Rewrite the following message."))

  (add-to-list 'popper-reference-buffers "\\*gptel-log\\*")
  (setf (alist-get "\\*gptel-log\\*" display-buffer-alist nil nil #'equal)
        `((display-buffer-reuse-window display-buffer-in-side-window)
          (side . right)
          (window-width . 72)
          (slot . 20)
          (body-function . ,(lambda (win)
                              (select-window win)
                              (my/easy-page)))))

  (cl-pushnew '(:propertize
                (:eval
                 (when (local-variable-p 'gptel--system-message)
                  (concat
                   "["
                   (if-let ((n (car-safe (rassoc gptel--system-message gptel-directives))))
                       (symbol-name n)
                     (gptel--describe-directive gptel--system-message 12))
                   "]")))
                'face 'gptel-rewrite-highlight-face)
              mode-line-misc-info)
  (add-to-list
   'mode-line-misc-info
   '(:eval
     (unless gptel-mode
      (when (and (local-variable-p 'gptel-model)
             (not (eq gptel-model (default-value 'gptel-model))))
       (concat "[" (gptel--model-name gptel-model) "]")))))
  (with-eval-after-load 'gptel-rewrite
    (add-hook 'gptel-rewrite-directives-hook #'gptel-rewrite-commit-message)))

(use-package gptel-ask
  :after gptel
  :bind (:map help-map
         ("C-q" . gptel-ask)
         :map embark-url-map
         ("?" . gptel-kagi-summarize))
  :config
  (defvar gptel--kagi
    (gptel-make-kagi
        "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api/kagi-ai.com")))
    "Kagi source for gptel")

  (defun gptel-kagi-summarize (url)
    (interactive "sSummarize url: ")
    (let ((gptel-backend gptel--kagi)
          (gptel-model "summarize:agnes")
          (gptel-use-curl)
          (gptel-use-context))
      (gptel-request url
        :callback
        (lambda (response info)
          (if response
              (progn
                (gptel--prepare-ask-buffer)
                (let ((scroll-conservatively 0))
                  (with-current-buffer gptel-ask--buffer-name
                    (insert "\n" url "\nSummary:\n\n"
                            response "\n\n----")
                    (display-buffer (current-buffer)))))
            (message "gptel-request failed with message: %s"
                     (plist-get info :status)))))
      (message "Generating summary for: %s" url))))

(use-package gptel-quick
  :ensure (:host github :protocol ssh
           :repo "karthink/gptel-quick")
  :bind (:map embark-general-map
         ("?" . gptel-quick)))

(use-package project
    :after (popper visual-fill-column)
    :bind (:map project-prefix-map
           ("C" . gptel-project))
    :config
    (setf (alist-get ".*Chat.org$" display-buffer-alist nil nil #'equal)
          `((display-buffer-below-selected)
            (window-height . 0.5)
            (body-function . ,#'select-window)))
    (defun gptel-project ()
      "Open the ChatGPT file for the current project."
      (interactive)
      (let ((default-directory (or (project-root (project-current))
                                   default-directory)))
        (find-file "Chat.org")
        (require 'gptel)
        (unless gptel-mode
          (gptel-mode 1))
        (unless visual-fill-column-mode
          (visual-fill-column-mode 1))
        (unless (equal popper-popup-status 'user-popup)
          (popper-toggle-type)))))

;;;----------------------------------------------------------------
;; *** Codeium (testing) DONT
;;;----------------------------------------------------------------
;; (load (expand-file-name "lisp/setup-codeium" user-emacs-directory))

;;;----------------------------------------------------------------
;; *** BROWSER-HIST MAYBE
;;----------------------------------------------------------------
(use-package browser-hist
  :ensure (:host github :repo "agzam/browser-hist.el"
           :remotes ("copy" :repo "karthink/browser-hist.el" :protocol ssh))
  :bind ("M-s U" . browser-hist-search)
  :config
  (setf (alist-get 'firefox browser-hist-db-paths)
        "$HOME/.mozilla/firefox/$USER/places.sqlite"
        browser-hist-default-browser 'firefox
        browser-hist-cache-timeout (* 24 60 60)))

;;----------------------------------------------------------------
;; * PROJECTS
;;;----------------------------------------------------------------
(load (expand-file-name "lisp/setup-project" user-emacs-directory))

;; ---------------------------
;; #+INCLUDE: "./lisp/setup-project.org" :minlevel 2
;; ---------------------------

;;;----------------------------------------------------------------
;; ** RG, GREP AND WGREP
;;;----------------------------------------------------------------

(use-package grep
  :hook ((grep-mode . toggle-truncate-lines)))

;; RG DONT
;; consult-ripgrep handles this better
(use-package rg
  :disabled 
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
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

;; * VISUALS AND PRESENTATION
;;----------------------------------------------------------------
;; ** ISCROLL DONT
;;----------------------------------------------------------------
;; Smooth scrolling through images.  What a pain Emacs' default behavior is here.
(use-package iscroll
  :disabled
  :ensure t
  :hook ((text-mode eww-mode) . iscroll-mode))

;;----------------------------------------------------------------
;; ** MONOCLE-MODE
;;----------------------------------------------------------------
(use-package emacs
  :bind (("H-m" . my/monocle-mode)
         ("C-x C-1" . my/monocle-mode))
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
;;----------------------------------------------------------------
;; ** MIXED-PITCH-MODE
;;;----------------------------------------------------------------
(use-package mixed-pitch
  :ensure t
  :config
  (dolist (face '(line-number org-property-value org-drawer
                  error org-cite corfu-current corfu-default
                  org-meta-line org-tag))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
  (setq mixed-pitch-set-height nil
        mixed-pitch-variable-pitch-cursor nil)
  (defun my/mixed-pitch-spacing ()
    (if mixed-pitch-mode
        (setq line-spacing 0.12)
      (setq line-spacing 0.0))))

;;;----------------------------------------------------------------
;; ** OLIVETTI
;;----------------------------------------------------------------
(use-package olivetti
  :commands (my/olivetti-mode)
  :ensure t
  :config
  (setq-default
   olivetti-body-width 90
   olivetti-minimum-body-width 76
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
            ;; (my/mode-line-hidden-mode 1)
            (mixed-pitch-mode 1))
          (if (bound-and-true-p evil-mode)
              (evil-emacs-state))
          ;; (setq-local line-spacing 0.16)
          ;; (setq-local cursor-type '(bar . 2))
          )
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (mixed-pitch-mode -1)
      (kill-local-variable 'line-spacing)
      ;; (unless (derived-mode-p 'prog-mode)
      ;;   (my/mode-line-hidden-mode -1))
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


;;----------------------------------------------------------------
;; ** PRESENTATION (BIG) MODE DONT
;;----------------------------------------------------------------
;; Turned off since global-text-scale-adjust exists now
(use-package presentation
  :disabled
  :ensure t
  :commands presentation-mode
  :config
  (setq presentation-default-text-scale 1.25
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
  (setf (alist-get 'strokes-do-stroke
                   keycast-substitute-alist)
        '("[mouse]" t))

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

;;----------------------------------------------------------------
;; ** SPACIOUS-PADDING DONT
;;----------------------------------------------------------------
(use-package spacious-padding
  :disabled
  :ensure t
  :defer
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 16
         :header-line-width 4
         :mode-line-width 2
         :tab-width 2
         :right-divider-width 24
         :scroll-bar-width 8)))

;;;################################################################I
;; * NAVIGATION
;;;################################################################I
(use-package emacs
  :config
  (setq view-read-only t))

;;;################################################################I
;; * FONTS AND COLORS
;;;################################################################
(use-package custom
  :commands my/toggle-theme
  :config
  (setq custom-theme-directory (expand-file-name "lisp" user-emacs-directory))

  (defun my/toggle-theme ()
    "Swap color themes. With prefix arg, don't disable the
currently loaded theme first."
    (interactive)
    (if (fboundp 'consult-theme)
        (progn (setq this-command 'consult-theme)
               (call-interactively #'consult-theme))
      (let ((theme (intern (completing-read "Load theme: "
                                            (cons "user" (mapcar #'symbol-name
                                                                 (custom-available-themes)))
                                            nil t))))
        (unless current-prefix-arg
          (mapc #'disable-theme custom-enabled-themes))
        (load-theme theme t))))

  ;; :init
  ;; (load-theme 'smart-mode-line-atom-one-dark)
  ;; (load-theme 'atom-one-dark t)
  )

;; Disabled while I work on the new org latex preview system.
;; FACE-REMAP DONT
(use-package face-remap
  :disabled
  :hook (text-scale-mode . my/text-scale-adjust-latex-previews)
  :config
  (defun my/text-scale-adjust-latex-previews ()
    "Adjust the size of latex preview fragments when changing the
buffer's text scale."
    (pcase major-mode
      ((or 'latex-mode (guard 'org-auctex-mode))
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'category)
                 'preview-overlay)
             (my/zoom-latex-preview ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'org-overlay-type)
                 'org-latex-overlay)
             (my/zoom-latex-preview ov))))))

  (defun my/zoom-latex-preview (ov)
    (overlay-put
     ov 'display
     (cons 'image 
           (plist-put
            (cdr (overlay-get ov 'display))
            :scale (+ 1.0 (* 0.25 text-scale-mode-amount)))))))

(use-package cus-face
  :config
  (pcase-dolist (`(,font           . ,scale)
                 '(("Merriweather" . 0.88)
                   ("IM FELL"      . 1.19)
                   ;; ("Latin Modern$" . 1.05)
                   ("Latin Modern Math" . 1.25)))
    (setf (alist-get font face-font-rescale-alist nil nil #'equal)
          scale))
  
  (cond (IS-LINUX
         (set-fontset-font t 'unicode "Symbola" nil 'prepend)
         (pcase-let ((`(,vp ,fp)
                      (cond
                       ((string= (getenv "XDG_SESSION_TYPE") "wayland")
                        '(120 110))
                       (t '(95 110)))))
           (custom-set-faces
            `(variable-pitch ((t (:family "Merriweather" ;; :height ,vp
                                  :width semi-expanded))))
            `(default ((t (:family "Monospace" :foundry "PfEd"
                           :slant normal :weight normal
                           :height ,fp :width normal)))))))
        (IS-WINDOWS
         (custom-set-faces
          '(default ((t (:family "Consolas" :foundry "outline"
                                 :slant normal :weight normal
                                 :height 120 :width normal)))))))
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
                            ))
  (use-package gruvbox-theme
    :disabled
    :defer
    :config
    (custom-theme-set-faces 'gruvbox-dark-hard
                            ;; '(aw-leading-char-face
                            ;;   ((t (:height 2.5 :weight normal))))
                            '(outline-1 ((t (:height 1.3))))
                            '(outline-2 ((t (:height 1.1)))))
    (custom-theme-set-faces 'gruvbox-dark-hard
                            '(org-level-1 ((t (:inherit outline-1))))
                            '(org-level-2 ((t (:inherit outline-2)))))))
;;----------------------------------------------------------------
;; ** MODUS THEMES
;;----------------------------------------------------------------
(use-package ef-themes
  :ensure t
  :defer
  :config
  (setq ef-themes-headings
        '((0 . (1.50))
          (1 . (1.28))
          (2 . (1.22))
          (3 . (1.17))
          (4 . (1.14))
          (t . (1.1))))
  (defun my/ef-themes-extra-faces ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
      (custom-set-faces
       `(aw-leading-char-face ((,c :foreground ,fg-mode-line
                                :height 1.5 :weight semi-bold))))))

  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-extra-faces))

;; Protesilaos Stavrou's excellent high contrast themes, perfect for working in
;; bright sunlight (especially on my laptop's dim screen).
(use-package modus-themes
  :ensure t
  :defer
  :init
  (setq modus-themes-common-palette-overrides
        `((date-common cyan)   ; default value (for timestamps and more)
          (date-deadline red-warmer)
          (date-event magenta-warmer)
          (date-holiday blue) ; for M-x calendar
          (date-now yellow-warmer)
          (date-scheduled magenta-cooler)
          (date-weekday cyan-cooler)
          (date-weekend blue-faint)
          (mail-recipient fg-main)
          ;; (fg-heading-1 blue-warmer)
          ;; (fg-heading-2 yellow-cooler)
          ;; (fg-heading-3 cyan-cooler)
          ;; (fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          (bg-region bg-sage)
          (fg-region unspecified)
          ;; (comment yellow-cooler)
          ;; (string green-cooler)
          (fringe unspecified) ;; bg-blue-nuanced
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))
  (setq modus-operandi-palette-overrides
        '((bg-mode-line-active bg-blue-intense) ;
          (fg-mode-line-active fg-main)
          (fg-heading-1 "#a01f64")
          (fg-heading-2 "#2f5f9f") ;;"#193668"
          (fg-heading-3 "#1a8388")))
  (setq modus-vivendi-palette-overrides
        `((fg-main "#d6d6d4")
          ;; (bg-main "#121212")
          (bg-region bg-lavender)
          (bg-main "#090909")
          (fg-heading-1 magenta-faint)
          ;; (bg-main "#181A1B")
          (bg-mode-line-active bg-lavender) ;; bg-graph-magenta-1
          (fg-mode-line-active "#ffffff")))
  (setq modus-themes-org-blocks 'gray-background
        modus-themes-bold-constructs t
        modus-themes-prompts '(bold background)
        modus-themes-variable-pitch-ui nil
        modus-themes-headings
        '((0 . (1.35))
          (1 . (1.30))       ;variable-pitch
          (2 . (1.24))       ;variable-pitch
          (3 . (semibold 1.17))
          (4 . (1.14))
          (t . (monochrome)))))


;;----------------------------------------------------------------
;; ** DOOM THEMES
;;----------------------------------------------------------------
;; Henrik Lissner's Doom themes are a mainstay, mostly doom-rouge:
;;
;; [[file:/img/dotemacs/doom-rouge-demo.png]]
(use-package doom-themes
  :ensure t
  :defer
  :custom
  (doom-gruvbox-dark-variant "hard")
  :config
  (add-hook 'enable-theme-functions #'my/doom-theme-settings)
  (defun my/doom-theme-settings (theme &rest args)
    "Additional face settings for doom themes"
    (if (eq theme 'doom-rouge)
        (progn
          (setq window-divider-default-right-width 2
                window-divider-default-bottom-width 2
                window-divider-default-places t)
          (message "Turned on window dividers")
          (window-divider-mode 1))
      (window-divider-mode -1)
      (message "Turned off window dividers"))
    (when (string-match-p "^doom-" (symbol-name theme))
      ;; (when (eq theme 'doom-rouge)
      ;;   (custom-set-faces `(hl-line ((,class :background "#1f2a3f")))))
      ;; Window dividers
      (let ((class '((class color) (min-colors 256))))
        (dolist (face-spec
                 '((aw-leading-char-face (:height 2.0 :foreground unspecified :inherit mode-line-emphasis)
                    ace-window)
                   (aw-background-face (:inherit default :weight normal) ace-window)
                   (outline-1        (:height 1.25) outline)
                   (outline-2        (:height 1.20) outline)
                   (outline-3        (:height 1.16) outline)
                   (outline-4        (:height 1.12) outline)
                   ;; (tab-bar            (:background "black" :height 1.0 :foreground "white")
                   ;;  tab-bar)
                   ;; (tab-bar-tab
                   ;;  (:bold t :height 1.10 :foreground nil :inherit mode-line-emphasis)
                   ;;  tab-bar)
                   ;; (tab-bar-tab-inactive
                   ;;  (:inherit 'mode-line-inactive :height 1.10 :background "black")
                   ;;  tab-bar)
                   ))
          (cl-destructuring-bind (face spec library) face-spec
            (if (featurep library)
                (custom-set-faces `(,face ((,class ,@spec))))
              (with-eval-after-load library
                (when (string-match-p "^doom-" (symbol-name theme))
                  (custom-set-faces `(,face ((,class ,@spec))))))))))))
  (doom-themes-org-config)
  (use-package doom-rouge-theme
    :config
    (setq doom-rouge-padded-modeline nil
          doom-rouge-brighter-comments t
          doom-rouge-brighter-tabs t))

  (use-package doom-iosvkem-theme
    :disabled
    ;; :custom-face
    ;; (default ((t (:background "#061229"))))
    :config
    (setq doom-Iosvkem-brighter-comments nil
          doom-Iosvkem-comment-bg nil
          doom-Iosvkem-brighter-modeline nil)))

;;;################################################################
;; * EVIL-MODE DONT
;;;################################################################
;;;(require 'setup-evil)

;;;################################################################
;; * TERMINAL SETTINGS DONT
;;;################################################################
(use-package term-keys
  :disabled
  :ensure (:host github :repo "CyberShadow/term-keys"))

;;;################################################################
;; * MISC SETTINGS
;;;################################################################
;; Settings that I'm not sure where to put:
(use-package kbd-mode
  :ensure (:host github
           :repo "kmonad/kbd-mode")
  :mode ("\\.kbd\\'" . kbd-mode))

(use-package shr
  :defer
  :config
  (setq shr-image-animate nil
        shr-use-colors nil
        shr-width 78)
  (defun my/shr-image-extra (_spec alt &rest _)
    "Center large images and add the ALT text below."
    (when (and (display-graphic-p) (or (eolp) (bolp) (looking-at " *$")))
      ;; Center image(s)
      (let ((size 0) (lbp (line-beginning-position)))
        (save-restriction
          (narrow-to-region (line-beginning-position) (line-end-position))
          (save-excursion
            (goto-char lbp)
            (unless (and (looking-at " ") (get-text-property (point) 'display))
              (insert " "))
            ;; find combined size of all images on line
            (let ((prop) (img nil))
              (while-let ((prop (text-property-search-forward 
                                 'display nil (lambda (prop val)
                                                (eq (car-safe val) 'image))))
                          (img (image--get-image (1- (point)))))
                (setq size (+ size (car (image-size img t))))))))
        ;; Center all images on line
        (when (> size 0)
          (add-text-properties
           lbp (1+ lbp)
           `(display (space :align-to (- center (0.50 . (,size))))
             font-lock-face default)))
        ;; Alt text
        (when (and (stringp alt) (> (length alt) 2))
          ;; (looking-back
          ;;  (rx bol (* space) (or (literal alt) (seq "*" (* space))))
          ;;  (line-beginning-position))
          (when (eobp)
            (insert " "))
          (put-text-property
           (1- (point)) (point) 'display
           (concat
            "\n"
            (propertize " " 'display `(space :align-to (- center (0.50 . ,(length alt)))))
            (propertize (concat "[" alt "]") 'font-lock-face 'shadow)))))))
  (advice-add 'shr-put-image :after #'my/shr-image-extra)

  (defun my/shr-zoom-image-extra ()
  "Clear any link property and adjust line spacing after zooming in on an image."
  (when (> line-spacing 0)
    (message "Adjusting line spacing for zoomed image.")
    (setq line-spacing 0))
  (let ((prev (previous-single-property-change (point) 'face))
        (buffer-read-only nil))
    (remove-text-properties prev (1+ (point))
                            '(face shr-link))))
  (advice-add 'shr-zoom-image :after #'my/shr-zoom-image-extra)
  (use-package shr-heading
    :hook (eww-mode . shr-heading-setup-imenu)))

(use-package url
  :defer
  :config
  (setq url-configuration-directory (dir-concat user-cache-directory "url/"))
  (setq url-automatic-caching t))

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

;; This is not a literate config tangled from an Org-mode document! So I include
;; some file-specific settings to make it easier to parse. Specifically, the
;; outline that you see in this document is represented in the Lisp files as
;; Org-style collapsible outline headings. See [[*OUTLINE MODE][Outline Mode]].

;; Local Variables:
;; outline-regexp: ";; \\*+"
;; page-delimiter: ";; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:
