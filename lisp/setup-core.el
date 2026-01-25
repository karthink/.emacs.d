;;; Optimizations  -*- lexical-binding: t; -*-

;; Optimizations to make Emacs more responsive. These are mostly copied from
;; Doom Emacs.

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

(provide 'setup-core)
;; setup-core.el ends here
