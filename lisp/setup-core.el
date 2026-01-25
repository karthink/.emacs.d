;;; Optimizations  -*- lexical-binding: t; -*-

;; Essential settings and core utilities, always loaded. A few of these are
;; performance optimizations copied from Doom Emacs.

;; Load path
(push (expand-file-name "plugins/" user-emacs-directory) load-path)
(push (expand-file-name "lisp/" user-emacs-directory) load-path)
(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by emacs are placed.")

;; A helper to keep track of start-up time:
(eval-when-compile (require 'cl-lib))
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-GUIX    (and IS-LINUX
                          (with-temp-buffer
                            (insert-file-contents "/etc/os-release")
                            (re-search-forward "ID=\\(?:guix\\|nixos\\)" nil t))))

;; Disable bidirectional text rendering for a modest performance boost. Just
;; need to remember to turn it on when displaying a right-to-left language!
(setq-default bidi-display-reordering 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)
(when (> emacs-major-version 27)
  (setq redisplay-skip-fontification-on-input t))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Performance on Windows is considerably worse than elsewhere.
(when IS-WINDOWS
  ;; Reduce the workload when doing file IO
  (setq w32-get-true-file-attributes nil)

  ;; Font compacting can be terribly expensive, especially for rendering icon
  ;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
  ;; been determined.
  (setq inhibit-compacting-font-caches t))

;; Remove command line options that aren't relevant to our current OS; that
;; means less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))
;; (when IS-LINUX (when (fboundp 'pgtk-use-im-context)
;;                  (pgtk-use-im-context nil)))

;; ;; When Emacs loses focus seems like a great time to do some garbage collection
;; ;; all sneaky breeky like, so we can return a fresh(er) Emacs.
;; (add-hook 'focus-out-hook #'garbage-collect)

(condition-case-unless-debug nil
    (use-package gcmh
      :defer 2
      :ensure t
      :diminish
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
      (setq gcmh-idle-delay 'auto       ; default is 15s
            gcmh-high-cons-threshold (* 32 1024 1024)
            gcmh-verbose nil
            gc-cons-percentage 0.2)
      (gcmh-mode 1))
  (error (setq gc-cons-threshold (* 16 1024 1024)
               gc-cons-percentage 0.2)))

;; use-package
(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        ;; use-package-ignore-unknown-keywords t
        use-package-minimum-reported-time 0.01
        use-package-expand-minimally t
        use-package-enable-imenu-support t)
  (when init-file-debug
    (setq use-package-expand-minimally nil
          use-package-verbose t
          use-package-compute-statistics t
          debug-on-error t))
  (require 'use-package))

;; Don't populate the init file with custom-set-variables, create and use a
;; separate file instead.
(use-package cus-edit
  :init
  ;; Get custom-set-variables out of init.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (defun my/cus-edit ()
    (unless (file-exists-p custom-file)
      (make-empty-file custom-file))
    (load-file custom-file))
  :hook (after-init . my/cus-edit))

(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Debounce/throttle utilities
(use-package emacs
  :init
  (defun timeout--throttle-advice (&optional timeout)
    "Return a function that throttles its argument function.

THROTTLE defaults to 1.0 seconds. This is intended for use as
function advice."
    (let ((throttle-timer)
          (timeout (or timeout 1.0))
          (result))
      (lambda (orig-fn &rest args)
        "Throttle calls to this function."
        (if (timerp throttle-timer)
            result
          (prog1
              (setq result (apply orig-fn args))
            (setq throttle-timer
                  (run-with-timer
                   timeout nil
                   (lambda ()
                     (cancel-timer throttle-timer)
                     (setq throttle-timer nil)))))))))

  (defun timeout--debounce-advice (&optional delay default)
    "Return a function that debounces its argument function.

DELAY defaults to 0.50 seconds.  DEFAULT is the immediate return
value of the function when called.

This is intended for use as function advice."
    (let ((debounce-timer nil)
          (delay (or delay 0.50)))
      (lambda (orig-fn &rest args)
        "Debounce calls to this function."
        (if (timerp debounce-timer)
            (timer-set-idle-time debounce-timer delay)
          (prog1 default
            (setq debounce-timer
                  (run-with-idle-timer
                   delay nil
                   (lambda (buf)
                     (cancel-timer debounce-timer)
                     (setq debounce-timer nil)
                     (with-current-buffer buf
                       (apply orig-fn args)))
                   (current-buffer))))))))

  (defun timeout-debounce! (func &optional delay default)
    "Debounce FUNC by DELAY seconds.

This advises FUNC, when called (interactively or from code), to
run after DELAY seconds. If FUNC is called again within this time,
the timer is reset.

DELAY defaults to 0.5 seconds. Using a delay of 0 resets the
function.

DEFAULT is the immediate return value of the function when called."
    (if (and delay (= delay 0))
        (advice-remove func 'debounce)
      (advice-add func :around (timeout--debounce-advice delay default)
                  '((name . debounce)
                    (depth . -99)))))

  (defun timeout-throttle! (func &optional throttle)
    "Throttle FUNC by THROTTLE seconds.

This advises FUNC so that it can run no more than once every
THROTTLE seconds.

THROTTLE defaults to 1.0 seconds. Using a throttle of 0 resets the
function."
    (if (= throttle 0)
        (advice-remove func 'throttle)
      (advice-add func :around (timeout--throttle-advice throttle)
                  '((name . throttle)
                    (depth . -98))))))

(provide 'setup-core)
;; setup-core.el ends here
