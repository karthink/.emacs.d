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
  (let ((orig-handlers (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler orig-handlers))))
    (set-default-toplevel-value 'file-name-handler-alist
                                file-name-handler-alist)
    ;; Reset after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite to preserve any changes made
                 ;; since startup.
                 (delete-dups (append file-name-handler-alist orig-handlers))))
              101))
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda () (setq-default inhibit-redisplay nil
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
(push '(horizontal-scroll-bars) default-frame-alist)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setq frame-inhibit-implied-resize t)
(setq auto-mode-case-fold nil)
(setq jka-compr-verbose nil)
(setq bidi-inhibit-bpa t)

;; NOTE: High memory usage, watch out
;; (setq inhibit-compacting-font-caches t)

;; (advice-add #'x-apply-session-resources :override #'ignore)
(setq inhibit-x-resources t
      inhibit-startup-buffer-menu t
      initial-buffer-choice t)
(setq inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(advice-add #'display-startup-screen :override #'ignore)
(fset #'display-startup-echo-area-message #'ignore)

;; * NATIVE-COMP

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
