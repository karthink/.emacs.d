;; -*- lexical-binding: t; -*-

;; Stop the cursor from blinking.
(blink-cursor-mode 0)
;; No fat cursors.
(setq x-stretch-cursor nil)

;; Turn off the menu, tool bars and the scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 1)
;; native linux tooltips are all right
(when IS-LINUX (setq x-gtk-use-system-tooltips nil))
;; Turn on image viewing
;; (auto-image-file-mode t)

;; Get rid of the annoying system beep.
(setq ring-bell-function 'ignore)

;; Show me what I type, immediately.
(setq echo-keystrokes 0.01)

;; Middle-click paste at point, not at cursor.
(setq mouse-yank-at-point t)
;; Mouse available in terminal
;; (add-hook 'tty-setup-hook #'xterm-mouse-mode)

(setq dnd-indicate-insertion-point t
      dnd-scroll-margin 4)

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; Scrolling
(setq scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 3)

;; mouse
;; (setq mouse-wheel-scroll-amount '(t ((shift) . 2))
;;       mouse-wheel-progressive-speed t)

(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :defer
  :init (setq scroll-conservatively 101 ; important!
              scroll-margin 0)
  (ultra-scroll-mode 1))

;;; (setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries t
      indicate-empty-lines nil
      scroll-conservatively 101
      auto-window-vscroll nil)

;;; remove continuation arrow on right fringe
;;; (delq! 'continuation fringe-indicator-alist 'assq)

;; Don't resize emacs in steps.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
;; (setq window-divider-default-places t
;;       window-divider-default-bottom-width 2
;;       window-divider-default-right-width 2)
;; (add-hook 'after-init-hook #'window-divider-mode)

;; No popup dialogs
(setq use-dialog-box nil)

(setq split-width-threshold 140
      split-height-threshold 80)

(setq display-line-numbers-width-start t
      display-line-numbers-type t)

;; For lazy typists
(setq use-short-answers t)

;; Move the mouse away if the cursor gets close
;; (mouse-avoidance-mode 'animate)

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

;; Confirm when killing Emacs
(setq confirm-kill-emacs
      (lambda (prompt) (y-or-n-p-with-timeout prompt 2 nil)))

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

  (defun my/mode-line-macro-recording ()
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

(use-package emacs
  :preface
  (define-minor-mode my/mode-line-hidden-mode
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if my/mode-line-hidden-mode
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      (force-mode-line-update))))

(use-package diminish :ensure t)

(global-prettify-symbols-mode 1)

;;----------------------------------------------------------------
;; *** PULSE
;;----------------------------------------------------------------
(use-package pulse
  :bind ("C-x l" . my/pulse-line)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit next-error))))
  (pulse-highlight-face ((t (:inherit next-error))))
  :hook (((dumb-jump-after-jump imenu-after-jump)
          . my/recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file)
          . my/recenter-and-pulse-line))
  :config
  (setq pulse-delay 0.01
        pulse-iterations 15)
  :init
  ;; (add-hook 'server-after-make-frame-hook
  ;;           (defun my/pulse-type ()
  ;;             (when (and (not pulse-flag)
  ;;                        (pulse-available-p))
  ;;               (setq pulse-flag t))))

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
;; ** WHICH-KEY
;;;----------------------------------------------------------------
(use-package which-key
  :bind
  ( :map help-map
    ("h" . which-key-show-major-mode))
  :config
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

(provide 'setup-ui)
;; setup-ui ends here
