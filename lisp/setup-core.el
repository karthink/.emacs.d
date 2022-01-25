;;; Optimizations  -*- lexical-binding: t; -*-

;; A helper to keep track of start-up time:
(eval-when-compile (require 'cl-lib))
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;;; (defconst EMACS26+ (> emacs-major-version 25))
;;; (defconst EMACS27+ (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-GUIX    (and IS-LINUX
                          (with-temp-buffer
                            (insert-file-contents "/etc/os-release")
                            (search-forward "ID=guix" nil t))))
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

;; File-name-handler optimization
;; ;; This is consulted on every `require', `load' and various path/io functions.
;; ;; You get a minor speed up by nooping this.
;; (setq file-name-handler-alist nil)
;;
;; (defvar my/initial-file-name-handler-alist file-name-handler-alist)
;; (defun my/restore-file-name-handler-alist-h ()
;;   (setq file-name-handler-alist my/initial-file-name-handler-alist))

;; (add-hook 'emacs-startup-hook #'my/restore-file-name-handler-alist-h)

;; ;; Garbage collector optimization
;; (defvar my/gc-cons-threshold 33554432  ;32mb
;;   "The default value to use for `gc-cons-threshold'. If you experience freezing,
;; decrease this. If you experience stuttering, increase this.")
;; 
;; (defun my/defer-garbage-collection-h ()
;;   "Defer garbage collection. Meant to be added to
;; `minibuffer-setup-hook'."
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my/restore-garbage-collection-h ()
;;   "Restore garbage collection threshold. Meant to be added to
;; `minibuffer-exit-hook'."
;;   ;; Defer it so that commands launched immediately after will enjoy the
;;   ;; benefits.
;;   (run-at-time
;;    1 nil (lambda () (setq gc-cons-threshold my/gc-cons-threshold))))

;; (add-hook 'minibuffer-setup-hook #'my/defer-garbage-collection-h)
;; (add-hook 'minibuffer-exit-hook #'my/restore-garbage-collection-h)

;; ;; Not restoring these to their defaults will cause stuttering/freezes.
;; (add-hook 'emacs-startup-hook #'my/restore-garbage-collection-h)

;; ;; When Emacs loses focus seems like a great time to do some garbage collection
;; ;; all sneaky breeky like, so we can return a fresh(er) Emacs.
;; (add-hook 'focus-out-hook #'garbage-collect)


(provide 'setup-core)
;; setup-core.el ends here
