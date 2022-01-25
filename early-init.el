;;; early-init.el -*- lexical-binding: t; -*-

;; early-init.el is run before init.el,
;; - before package initialization, and
;; - before UI initialization

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil
      package-quickstart nil
      load-prefer-newer t)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun my/reset-file-handler-alist ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'my/reset-file-handler-alist 101))

  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) nomessage-remove)
    (advice-remove #'load-file #'load-file@silence)))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
;;; (advice-add #'x-apply-session-resources :override #'ignore)
;; (setq inhibit-x-resources nil)

;; * NATIVE-COMP

;; I'm still using Emacs 27.2, because it's good enough. This code configures
;; the native compiler and is in preparation for 28.1.
;; - Move eln files to a cache dir
;; - Don't bombard the user with warnings
;; - Compile packages on install, not at runtime
(unless (version-list-<
         (version-to-list emacs-version)
         '(28 0 1 0))
  (when (boundp 'native-comp-eln-load-path)
    (add-to-list 'native-comp-eln-load-path
                 (concat "~/.cache/emacs/" "eln-cache/"))
    (setq native-comp-async-report-warnings-errors 'silent
          native-comp-deferred-compilation t)))

;;;################################################################
