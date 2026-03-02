;; -*- lexical-binding: t -*-

;; #+options: prop:t
;; #+begin_quote
;;                                       my dot emacs grows
;;
;;                                       one day i look inside it
;;
;;                                       singularity
;; #+end_quote

;;;################################################################
;; * PACKAGE MANAGEMENT
;;;################################################################
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
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
  :hook (elpaca-log-mode . elpaca-log-update-mode)
  :after popper
  :config (timeout-debounce! 'elpaca-log--follow 0.15)
  :init
  (add-to-list 'popper-reference-buffers
               'elpaca-log-mode)
  (setf (alist-get '(major-mode . elpaca-log-mode)
                   display-buffer-alist
                   nil nil #'equal)
        '((display-buffer-at-bottom
           display-buffer-in-side-window)
          (side . below)
          (slot . 49)
          (window-height . 0.4)
          (body-function . select-window))
        (alist-get "\\*elpaca-diff\\*" display-buffer-alist
                   nil nil #'equal)
        '((display-buffer-reuse-window
           display-buffer-in-atom-window)
          (side . right))))

;;;################################################################
;; * CORE
;;;################################################################
(load (expand-file-name "lisp/setup-core" user-emacs-directory))

;;;################################################################
;; * DAEMON
;;;################################################################

(use-package emacs
  :if (daemonp)
  :hook (after-init-hook . my/load-packages-eagerly)
  :config
  (defun emenu (cmd &rest args)
    (interactive (help-fns--describe-function-or-command-prompt 'is-command))
    (let ((frame (selected-frame)))
      (unwind-protect
          (let ((vertico-multiform-mode)
                (consult-preview-key "M-RET")
                (vertico-count 15)
                (echo-keystrokes 0)
                (display-buffer-overriding-action
                 `((display-buffer-pop-up-frame
                    display-buffer-use-some-frame
                    display-buffer-reuse-window
                    display-buffer-use-some-window)
                   ;; (reusable-frames . nil)
                   ;; (lru-frames . ,(cadr (frame-list-z-order)))
                   (window-min-height . full-height)
                   (frame-predicate
                    . ,(lambda (fr)
                         (not (string-prefix-p
                               "dropdown" (frame-parameter fr 'name)))))
                   (some-window . mru))))
            (if (commandp cmd) (call-interactively cmd) (funcall cmd args)))
        (run-at-time 0 nil #'delete-frame frame))))
  ;; Hack: When starting a server, silently load all the "heavy" libraries and
  ;; goodies I use. There are more elegant approaches, such as incremental
  ;; deferring, but this is good enough. A regular (non-daemon) Emacs session
  ;; still launches in ~0.3 seconds.
  ;; (defvar pulse-flag t)
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
                                      "org-capture" "org-agenda"
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
                         (message "[Pre-loaded packages in %.3fs]" elapsed))))))))

;;;################################################################
;; * TTY SUPPORT
;;;################################################################

(use-package emacs
  :init
  (add-hook 'after-init-hook 'my/terminal-settings)
  (defun my/terminal-settings (&optional force)
    (interactive (list t))
    (remove-hook 'after-init-hook 'my/terminal-settings)
    (when (or (controlling-tty-p) force)
      (setq pulse-flag nil)
      (keymap-global-unset "C-@")       ;conflicts with C-SPC
      (keymap-global-unset "M-[")
      (setq recenter-redisplay nil)
      (with-eval-after-load 'smartparens
        ;; conflicts with focus-in/out events
        (keymap-set smartparens-mode-map "M-[" nil))
      (with-eval-after-load 'popper
        ;; C-` and H-` are not available
        (keymap-global-set "C-M-^" 'popper-toggle))
      (use-package kkp :ensure t :init (global-kkp-mode 1)))))

;;;################################################################
;; * PERSONAL INFO
;;;################################################################
(with-demoted-errors "Error (personal info): %S"
  (load-library "personal")
  (setq user-full-name my-full-name)
  (setq user-mail-address my-email-address))

;;;################################################################
;; * ORG: SPECIAL CASE
;;;################################################################

;; (use-package org
;;   :defer
;;   :ensure `(org :host github :repo "karthink/org-mode" :branch "olp"
;;                :remotes ("github"
;;                          ("tecosaur" :repo "https://code.tecosaur.net/tec/org-mode.git/"
;;                           :branch "dev")
;;                          ("origin" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
;;                           :branch "main"))))

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
;; * SETUP FILES
;;;################################################################
(load (expand-file-name "lisp/setup-minibuffer" user-emacs-directory))
(load (expand-file-name "lisp/setup-ui" user-emacs-directory))
(load (expand-file-name "lisp/setup-tabs" user-emacs-directory))
(load (expand-file-name "lisp/setup-folds" user-emacs-directory))
;; Settings for shell, eshell, comint and vterm
(load (expand-file-name "lisp/setup-shells" user-emacs-directory))
(load (expand-file-name "lisp/better-editing" user-emacs-directory))
(load (expand-file-name "lisp/setup-editing-extra.el" user-emacs-directory))
(load (expand-file-name "lisp/setup-isearch" user-emacs-directory))
(load (expand-file-name "lisp/setup-avy" user-emacs-directory))
(load (expand-file-name "lisp/setup-windows" user-emacs-directory))
(load (expand-file-name "lisp/setup-persistence" user-emacs-directory))
(load (expand-file-name "lisp/better-buffers" user-emacs-directory))
(load (expand-file-name "lisp/setup-ibuffer" user-emacs-directory))
(load (expand-file-name "lisp/setup-lookup" user-emacs-directory))
(load (expand-file-name "lisp/setup-cw" user-emacs-directory))

(load (expand-file-name "lisp/setup-coding" user-emacs-directory))
(load (expand-file-name "lisp/setup-vc" user-emacs-directory))
(load (expand-file-name "lisp/setup-diff" user-emacs-directory))
(load (expand-file-name "lisp/setup-project" user-emacs-directory))
(load (expand-file-name "lisp/setup-elisp" user-emacs-directory))
(load (expand-file-name "lisp/setup-dired" user-emacs-directory))
(load (expand-file-name "lisp/setup-calc" user-emacs-directory))
(load (expand-file-name "lisp/utilities" user-emacs-directory))
(load (expand-file-name "lisp/setup-themes" user-emacs-directory))

(load (expand-file-name "lisp/setup-orderless" user-emacs-directory))
(load (expand-file-name "lisp/setup-vertico" user-emacs-directory))
(load (expand-file-name "lisp/setup-marginalia" user-emacs-directory))
(load (expand-file-name "lisp/setup-embark" user-emacs-directory))
(load (expand-file-name "lisp/setup-consult" user-emacs-directory))

(load (expand-file-name "lisp/setup-gptel" user-emacs-directory))

(load (expand-file-name "lisp/setup-share" user-emacs-directory))
(load (expand-file-name "lisp/setup-wallabag" user-emacs-directory))
(load (expand-file-name "lisp/setup-erc" user-emacs-directory))
(load (expand-file-name "lisp/setup-email" user-emacs-directory))
(load (expand-file-name "lisp/setup-elfeed" user-emacs-directory))
(load (expand-file-name "lisp/setup-eww" user-emacs-directory))

(load (expand-file-name "lisp/setup-latex" user-emacs-directory))
(load (expand-file-name "lisp/setup-writing" user-emacs-directory))
(use-package writer :commands writer-settings)
(load (expand-file-name "lisp/setup-cite" user-emacs-directory))

(load (expand-file-name "lisp/setup-org" user-emacs-directory))
(load (expand-file-name "lisp/setup-anki" user-emacs-directory))
(load (expand-file-name "lisp/setup-roam" user-emacs-directory))

;; Language support
(load (expand-file-name "lisp/setup-matlab" user-emacs-directory))
(load (expand-file-name "lisp/setup-python" user-emacs-directory))
(load (expand-file-name "lisp/setup-scheme" user-emacs-directory))
(load (expand-file-name "lisp/setup-julia" user-emacs-directory))
(load (expand-file-name "lisp/setup-lua" user-emacs-directory))
(load (expand-file-name "lisp/setup-beancount" user-emacs-directory))
;; (load (expand-file-name "lisp/setup-clojure" user-emacs-directory))

(load (expand-file-name "lisp/setup-visual" user-emacs-directory))
(load (expand-file-name "lisp/setup-present" user-emacs-directory))

(load (expand-file-name "lisp/setup-yas" user-emacs-directory))
(load (expand-file-name "lisp/setup-corfu" user-emacs-directory))
(use-package company :ensure t :bind
  ( :map company-active-map
    ("C-n" . nil) ("C-p" . nil) ("C-w" . nil)
    ("M-p" . company-select-previous-or-abort)
    ("M-n" . company-select-next-or-abort)
    ("M-." . company-show-location)))

(use-package markdown-mode :ensure t :defer)
(use-package ffmpeg-crop :load-path "plugins/ffmpeg-crop/"
  :commands (ffmpeg-crop ffmpeg-crop-dired))

;;;################################################################
;; * TODO Extra modes to move
;;;################################################################

;;;----------------------------------------------------------------
;; ** NIX
(when IS-GUIX
  (load-library "nix-mode-autoloads")
  (load-library "nix-modeline-autoloads"))

(use-package nix-mode
  :hook ((nix-mode . electric-pair-local-mode)
         (nix-mode . my/nix-eglot-workspace-configuration))
  :config
  (defun my/nix-eglot-workspace-configuration ()
    (setq eglot-workspace-configuration
          (plist-put
           (bound-and-true-p eglot-workspace-configuration)
           :nixd
           '( :nixpkgs (:expr "import <nixpkgs> { }")
              :formatting (:command [ "alejandra" ])
              ;; :nix.serverPath "nixd"
              ;; :nix.enableLanguageServer t
              :options
              (:nixos
               (:expr "(builtins.getFlake \"/home/karthik/dotnix\").nixosConfigurations.t14.options")
               :home_manager
               (:expr "(builtins.getFlake \"/home/karthik/dotnix\").homeConfigurations.t14.options"))
              :diagnostic (:suppress [ "sema-escaping-with" "sema-extra-with" ]))))))

;;;----------------------------------------------------------------
;; ** JSON
;;;----------------------------------------------------------------
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

;;----------------------------------------------------------------
;; ** PLANTUML
;;----------------------------------------------------------------
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

;;----------------------------------------------------------------
;; ** EDRAW
;;----------------------------------------------------------------
(use-package edraw
  :disabled
  :ensure (el-easydraw :host github
                       :repo "misohena/el-easydraw"
                       :main "edraw.el")
  :defer
  :mode "\\.edraw\\.svg$")

;;;----------------------------------------------------------------
;; ** TRANSIENT
;;;----------------------------------------------------------------
(use-package transient
  :ensure (:host github :repo "magit/transient")
  :defines toggle-modes
  :bind (("<f8>"  . toggle-modes)
         ("C-c b" . toggle-modes))
  :custom
  (transient-levels-file (expand-file-name "transient/levels.el" user-cache-directory))
  (transient-values-file (expand-file-name "transient/values.el" user-cache-directory))
  :config
  (transient-bind-q-to-quit)
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
  
  ;; (mode-cycle 'electric-pair-mode 'smartparens-mode)
  
  ;; (setq transient-display-buffer-action
  ;;       '(display-buffer-below-selected (dedicated . t) (inhibit-same-window . t)))

  (setq transient-history-file (expand-file-name "transient/history.el" user-cache-directory)
        transient-levels-file (expand-file-name "transient/levels.el" user-cache-directory)
        transient-values-file (expand-file-name "transient/values.el" user-cache-directory)
        transient-show-popup t)
  
  (transient-define-prefix toggle-modes ()
    "Turn on and off various frequently used modes."
    
    [;:pad-keys t
     ["Appearance"
      ("t" "color theme" my/toggle-theme :if (lambda () (fboundp 'my/toggle-theme)))
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
      ("vo" "olivetti"    olivetti-mode
       :if (lambda () (fboundp 'olivetti-mode)))
      ("vf" "visual fill" visual-line-fill-column-mode
       :if (lambda () (fboundp 'visual-line-fill-column-mode)))]

     ["Org"
      :if-derived org-mode
      ("om" "Modern" org-modern-mode :if (lambda () (fboundp 'org-modern-mode))) 
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
      ("ol" "LaTeX" org-latex-preview-mode :if (lambda () (fboundp 'org-latex-preview-mode)))
      ("ow" "CAPF" my/toggle-writing-capf :if (lambda () (fboundp 'my/toggle-writing-capf)))]

     ["Markdown"
      :if-derived markdown-mode
      ("o/" "Emphasis" markdown-toggle-markup-hiding)
      ("ou" "url" markdown-toggle-url-hiding)
      ("os" "src" markdown-toggle-fontify-code-blocks-natively)
      ("ow" "CAPF" my/toggle-writing-capf :if (lambda () (fboundp 'my/toggle-writing-capf)))]

     ["Editing"
      ("r" "read only" read-only-mode)
      ("n" "line numbers" display-line-numbers-mode)
      ("M-q" "auto fill" auto-fill-mode)
      ("fc"
       (lambda ()
         (concat "fill column "
                 (propertize (format "%d" fill-column)
                             'face 'font-lock-comment-face)))
       set-fill-column)
      ("se" (lambda () (if sentence-end-double-space
                    "double spc" "single spc"))
       (lambda () (interactive)
         (setq-local sentence-end-double-space
                     (not sentence-end-double-space))))
      ("i" "ispell" jinx-mode :if (lambda () (fboundp 'jinx-mode)))
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
      ("hd" "delimiters" rainbow-delimiters-mode :if (lambda () (fboundp 'rainbow-delimiters-mode)))
      ("hr" "rainbow" rainbow-mode :if (lambda () (fboundp 'rainbow-mode)))
      ("hc" "cursor" my/hide-cursor-mode :if (lambda () (fboundp 'my/hide-cursor-mode)))]

     ["Code"
      ("c" "completion" corfu-mode :if (lambda () (fboundp 'corfu-mode)))
      ("a" "autocomp" (lambda () (interactive)
                        (setq-local corfu-auto (not corfu-auto))
                        (corfu-mode 0) (corfu-mode 1)
                        (message "corfu-auto is now %s" corfu-auto))
       :transient t)
      ("dd" "debug (err)" toggle-debug-on-error)
      ("dg" "debug (quit)" (lambda () (interactive)
                             (cl-callf not debug-on-quit)
                             (message "Debug on quit %sset"
                                      (if debug-on-quit "" "un"))))
      ("g" "diff-hl" (lambda (&optional arg) (interactive "P")
                         (if (null arg) (diff-hl-mode 'toggle)
                           (let ((ref
                                  (vc-read-revision
                                   (format-prompt "Reference revision for diff-hl" "master")
                                   (list buffer-file-name))))
                             (when (string-blank-p ref) (setq ref "master"))
                             (setq-local diff-hl-reference-revision ref)
                             (diff-hl-mode) (diff-hl-update)
                             (message "Showing changes against %s" ref))))
       :if (lambda () (fboundp 'diff-hl-mode)))
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
      ("p" "smartparens" smartparens-mode
       :if (lambda () (fboundp 'smartparens-mode)))]]))

;;;################################################################
;; ** TRAMP
;;;################################################################
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
  (setq remote-file-name-inhibit-cache 86400
        tramp-allow-unsafe-temporary-files t)
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" user-cache-directory))
  (setq tramp-verbose 1)
  (setq remote-file-name-inhibit-locks t
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1024 1024))
  (connection-local-set-profile-variables
   'remote-direct-async-process '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp") 'remote-direct-async-process)
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options))
  (with-eval-after-load 'vc
    (setq vc-ignore-dir-regexp
          (format "%s\\|%s"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))))

;;;; Testing tramp-rpc
(use-package tramp-rpc
  :disabled
  :ensure ( :host github :repo "ArthurHeymans/emacs-tramp-rpc"
            :files "lisp/*.el")
  :after tramp
  :config
  (setq tramp-rpc-deploy-local-cache-directory
        "~/.cache/emacs/tramp-rpc-binaries"))

;;----------------------------------------------------------------
;; ** BOOKMARKS
;;----------------------------------------------------------------
(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" user-cache-directory)))

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

(use-package url
  :defer
  :config
  (setq url-configuration-directory (expand-file-name "url/" user-cache-directory))
  (setq url-automatic-caching t))

(use-package request
  :defer
  :config
  (setq request-storage-directory (expand-file-name "request/" user-cache-directory)))

(use-package semantic
  :defer
  :config
  (setq semanticdb-default-save-directory (expand-file-name "semanticdb/" user-cache-directory)))

(use-package srecode
  :defer
  :config
  (setq srecode-map-save-file (expand-file-name "srecode-map.el" user-cache-directory)))

(use-package auth-source-pass
  :defer
  :config (auth-source-pass-enable))

;; * LOCAL-VARIABLES

;; Local Variables:
;; outline-regexp: ";; \\*+"
;; page-delimiter: ";; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:
