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
  :straight t
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
  (add-to-list 'sml/replacer-regexp-list '("^~/[dD]ocuments/[rR]oam.*/" ":ROAM:"))
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
    (cdlatex-mode . "")
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

(use-package moody
  :disabled
  :after (modus-themes smart-mode-line)
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-sml/mode-line-buffer-identification)
  (moody-replace-vc-mode))

(provide 'setup-modeline)
