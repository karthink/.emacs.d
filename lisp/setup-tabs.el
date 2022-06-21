(use-package tab-bar
  :if (not (version-list-<
            (version-to-list emacs-version)
            '(27 0 1 0)))
  :after cus-face
  :defer
  :bind-keymap ("H-t" . tab-prefix-map)
  :bind
  (("C-M-<tab>" . tab-bar-switch-to-next-tab)
   ("C-M-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
   ("H-<tab>" . tab-bar-switch-to-next-tab)
   ("H-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
   ("s-u" . tab-bar-history-back)
   ;; ("C-c u" . tab-bar-history-back)
   ;; ("s-S-U" . tab-bar-history-forward)
   :map tab-prefix-map
   ("h" . my/tab-bar-show-hide-tabs)
   ("H-t" . tab-bar-select-tab-by-name))

  :config
  (tab-bar-history-mode 1)
  (when (version< "28.0" emacs-version)
    (defun tab-bar-format-menu-bar ()
      "Produce the Menu button for the tab bar that shows the menu bar."
      `((menu-bar menu-item (propertize " 𝝺 " 'face 'tab-bar-tab-inactive)
                  tab-bar-menu-bar :help "Menu Bar")))
    (defun tab-bar-tab-name-format-default (tab i)
      (let ((current-p (eq (car tab) 'current-tab)))
        (propertize
         (concat " "
                 (if tab-bar-tab-hints (format "%d " i) "")
                 (alist-get 'name tab)
                 (or (and tab-bar-close-button-show
                          (not (eq tab-bar-close-button-show
                                   (if current-p 'non-selected 'selected)))
                          tab-bar-close-button)
                     "")
                 " ")
         'face (funcall tab-bar-tab-face-function tab))))
    (setq tab-bar-format '(tab-bar-format-menu-bar
                           ;; tab-bar-format-history
                           tab-bar-format-tabs
                           tab-bar-separator
                           tab-bar-format-add-tab
                           tab-bar-format-align-right
                           tab-bar-format-global)
          tab-bar-close-button-show nil))
  
  (setq  tab-bar-close-last-tab-choice 'tab-bar-mode-disable
         tab-bar-show                   (when (version< "28.0" emacs-version) 1)
         tab-bar-tab-name-truncated-max 14
         tab-bar-new-tab-choice        'ibuffer
         tab-bar-tab-name-function '(lambda nil
                                      "Use directory as tab name."
                                      (let ((dir (expand-file-name
                                                  (or (if (fboundp 'project-root)
                                                          (project-root (project-current)))
                                                      default-directory))))
                                        (substring dir (1+ (string-match "/[^/]+/$" dir)) -1 )))
         ;; tab-bar-select-tab-modifiers  '(meta)
         ;; tab-bar-tab-name-function 'tab-bar-tab-name-truncated
         ;; tab-bar-tab-name-function '(lambda nil (upcase (tab-bar-tab-name-truncated)))
         )

  (setq tab-bar-select-tab-modifiers '(meta hyper))

  (defun my/tab-bar-show-hide-tabs ()
    "Show or hide tabs."
    (interactive)
    (setq tab-bar-show (if tab-bar-show nil 1)))
   ;; (custom-set-faces
   ;; '(tab-bar ((t (:inherit nil :height 1.1))))
   ;; '(tab-bar-tab-inactive ((t (:inherit tab-bar :weight normal :height 0.9))))
   ;; '(tab-bar-tab ((t (:inherit tab-bar :underline t :weight bold))))
   ;; )

  (advice-add 'tab-bar-rename-tab
              :after
              (defun my/tab-bar-name-upcase (_name &optional _arg)
                "Upcase current tab name"
                (let* ((tab (assq 'current-tab (frame-parameter nil 'tabs)))
                       (tab-name (alist-get 'name tab)))
                  (setf (alist-get 'name tab) (upcase tab-name)
                        (alist-get 'explicit-name tab) t))))
  
  ;; Workaround for wrong tab-bar right alignment with unicode chars
  (advice-add 'tab-bar-format-align-right :override
              (defun my/tab-bar-fix-align-a ()
                "Align the rest of tab bar items to the right."
                (let* ((rest (cdr (memq 'tab-bar-format-align-right
                                        tab-bar-format)))
                       (rest (tab-bar-format-list rest))
                       (rest (mapconcat (lambda (item) (nth 2 item)) rest ""))
                       (hlen  (length rest))
                       (hpos (+ hlen (ceiling
                                      (- (string-bytes rest) hlen) 2)))
                       (str (propertize " " 'display
                                        `(space :align-to (- right ,hpos)))))
                  `((align-right menu-item ,str ignore))))))

;; Show a list of the tabs in the echo area when switching tabs. Disabled since
;; I've taken to showing the tab-bar instead
(use-package tab-bar-echo-area
  :if (version< emacs-version "28.0")
  :straight t
  :after tab-bar
  :init
  (if (version< emacs-version "28.0")
      (defvar tab-bar-format nil "Format for tab-bar-echo-area-mode"))
  :config
  (tab-bar-echo-area-mode 1))

(when (version< "28.0" emacs-version)
  (use-package project-tab-groups
    :disabled
    :straight t
    :defer))

;;;----------------------------------------------------------------
;; *** +EYEBROWSE+
;;;----------------------------------------------------------------

;; This is superceded by native tabs (tab-bar-mode) in Emacs 27. I keep this
;; around in case I find myself using Emacs 26.3 or lower.
(use-package eyebrowse
  :disabled
  :if (version-list-<
       (version-to-list emacs-version)
       '(27 0 1 0))
  :hook (after-init . eyebrowse-mode)
  :init (setq eyebrowse-keymap-prefix (kbd "C-x t"))
  ;; :bind ("C-c C-w c" . eyebrowse-create-window-config)
  ;; :commands eyebrowse-create-window-config
  :config
  (setq eyebrowse-new-workspace (lambda nil "Buffer menu for user buffers" (buffer-menu 1))
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  (define-key eyebrowse-mode-map (kbd "C-M-TAB") 'eyebrowse-next-window-config)
  (define-key eyebrowse-mode-map (kbd "C-M-<tab>") 'eyebrowse-next-window-config)
  (define-key eyebrowse-mode-map (kbd "<C-M-iso-lefttab>") 'eyebrowse-last-window-config)
  ;; (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  ;; (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  ;; (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  ;; (defmacro eyebrowse-remap-tab-keys () )
  ;; (eyebrowse-setup-opinionated-keys)

  ;; Display tab configuration in Emacs title bar
  (defun my-title-bar-format()
    (let* ((current-slot (eyebrowse--get 'current-slot))
           (window-configs (eyebrowse--get 'window-configs))
           (window-config (assoc current-slot window-configs))
           (window-config-name (nth 2 window-config))
           (num-slots (length window-configs)))
      (concat window-config-name " [" (number-to-string current-slot)
              "/" (number-to-string num-slots) "] | " "%b")))

  (if (display-graphic-p)
      (progn
        (setq frame-title-format
              '(:eval (my-title-bar-format))))))


(provide 'setup-tabs)
