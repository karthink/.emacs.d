;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;; ** SHOW-FONT
;;----------------------------------------------------------------
(use-package show-font
  :ensure (:host github :repo "protesilaos/show-font")
  :defer)

;;----------------------------------------------------------------
;; ** FONTS AND COLORS
;;----------------------------------------------------------------
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
      (let ((theme (intern (completing-read
                            "Load theme: "
                            (cons "user" (mapcar #'symbol-name
                                                 (custom-available-themes)))
                            nil t))))
        (unless current-prefix-arg
          (mapc #'disable-theme custom-enabled-themes))
        (load-theme theme t)))))

;; Disabled while I work on the new org latex preview system.
;;----------------------------------------------------------------
;; ** FACE-REMAP DONT
;;----------------------------------------------------------------
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

;;----------------------------------------------------------------
;; ** DEFAULT FONT
;;----------------------------------------------------------------
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
                        '(1.10 100))
                       (t '(1.10 100)))))
           (custom-set-faces
            `(variable-pitch ((t ( :family "Merriweather"
                                   :height ,vp :width semi-expanded))))
            `(default ((t (:family "Monaspace Neon" ;; :foundry "PfEd"
                                   :slant normal :weight medium
                                   :height ,fp :width normal)))))))
        (IS-WINDOWS
         (custom-set-faces
          '(default ((t (:family "Consolas" :foundry "outline"
                                 :slant normal :weight normal
                                 :height 120 :width normal))))))))

;;----------------------------------------------------------------
;; ** MODUS AND EF THEMES
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

(use-package modus-themes
  :ensure t
  :defer
  :init
  (setq modus-themes-common-palette-overrides
        `((date-common cyan)           ; default value (for timestamps and more)
          (date-deadline red-warmer)
          (date-event magenta-warmer)
          (date-holiday blue)           ; for M-x calendar
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
          (1 . (1.30))                  ;variable-pitch
          (2 . (1.24))                  ;variable-pitch
          (3 . (semibold 1.17))
          (4 . (1.14))
          (t . (monochrome)))))


;;----------------------------------------------------------------
;; ** DOOM THEMES
;;----------------------------------------------------------------
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

;;----------------------------------------------------------------
;; ** OTHER THEMES (DONT)
;;----------------------------------------------------------------
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
                            '(org-level-2 ((t (:inherit outline-2))))))


(provide 'setup-themes)
;;; setup-themes.el ends here
