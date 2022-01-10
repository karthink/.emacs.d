;; demo.el --- Helpers to demo Emacs features -*- lexical-binding: t; -*-

;;; Commentary:
;; Some helpers to demo Emacs features. Call `demo-transient' to begin.

;;; Code:
(require 'use-package)
(require 'transient)
(require 'calc)

(use-package transient
  :bind ("C-c D" . demo-transient)
  :config
  (eval-when-compile
    (defmacro demo--define-infix (key name description type default
                                      &rest reader)
      "Define infix with KEY, NAME, DESCRIPTION, TYPE, DEFAULT and READER as arguments."
      `(progn
         (defcustom ,(intern (concat "demo-" name)) ,default
           ,description
           :type ,type
           :group 'demo)
         (transient-define-infix ,(intern (concat "demo--set-" name)) ()
                                 "Set `demo-,name' from a popup buffer."
                                 :class 'transient-lisp-variable
                                 :variable ',(intern (concat "demo-" name))
                                 :key ,key
                                 :description ,description
                                 :argument ,(concat "--" name)
                                 :reader (lambda (&rest _) ,@reader))))

    (demo--define-infix
     "A" "aspect-ratio" "Frame aspect ratio"
     '(choice (const :tag "tall" 'tall)
              (const :tag "wide" 'wide)
              (const :tag "unspecified" nil))
     nil
     (intern-soft (completing-read "Aspect ratio" '(tall wide))))

    (demo--define-infix
     "m" "mode-line-p" "mode-line?"
     'boolean nil
     (not demo-mode-line-p))

    (demo--define-infix
     "h" "height" "Height in pixels"
     'integer 1080
     (pcase demo-aspect-ratio
       ('tall 1080)
       ('wide 720)
       (_ (read-number "Height (pixels): "))))

    (demo--define-infix
     "w" "width" "Width in pixels"
     'integer 610
     (pcase demo-aspect-ratio
       ('tall 610)
       ('wide 1280)
       (_ (read-number "Width (pixels): "))))

    (demo--define-infix
     "t" "theme" "Theme"
     'theme 'modus-vivendi
     (intern-soft
      (completing-read "Use theme: "
                       (mapcar #'symbol-name
			       (custom-available-themes))
                       nil t)))

    (demo--define-infix
     "f" "fontsize" "Font size"
     'integer '150
     (read-number "Font size (points): "))

    (demo--define-infix
     "k" "keycast-p" "keycast-mode?"
     'boolean nil
     (not demo-keycast-p))

    (demo--define-infix
     "c" "autocomplete-p" "autocompletion?"
     'boolean nil
     (not demo-autocomplete-p))
    
    (demo--define-infix
     "p" "popper-style" "popup style?"
     '(choice (const :tag "basic" t)
              (const :tag "custom" 'user)
              (const :tag "off" nil))
     t
     (pcase demo-popper-style
       ('t 'user)
       ('user 'nil)
       ('nil 't))))

  (transient-define-prefix demo-transient ()
     "Turn on demo mode"
     ["Size"
      (demo--set-aspect-ratio)
      (demo--set-height)
      (demo--set-width)]
     ["Appearance"
      (demo--set-theme)
      (demo--set-fontsize)]
     ["Modes"
      (demo--set-mode-line-p)
      (demo--set-keycast-p)
      (demo--set-autocomplete-p)
      (demo--set-popper-style)]
     ["Action"
      ("RET" "Toggle demo-mode" demo-mode)])

  (defvar my/frame-name nil)
  (defvar my/current-themes custom-enabled-themes)
  (define-minor-mode demo-mode ()
    :global t
    :keymap nil
    (if demo-mode
        (progn

          ;; Always needed
          (add-hook 'grep-mode-hook 'toggle-truncate-lines)

          ;; Unneeded
          (dolist (mode '(project-x-mode))
            (if (bound-and-true-p mode)
                (funcall (symbol-function mode) 0)))
          
          ;; Popups
          (demo-popper-apply-settings demo-popper-style)
          
          ;; Autocompletion
          (cond
           ((fboundp 'corfu-mode) (corfu-mode (if demo-autocomplete-p 1 0)))
           ((fboundp 'company-mode) (company-mode (if demo-autocomplete-p 1 0))))

          ;; Mode line
          (unless demo-mode-line-p
            (my/mode-line-hidden-mode 1))
          ;; Keycast
          (if (and demo-keycast-p (not my/mode-line-hidden-mode))
            (keycast-mode 1))
          ;; Theme
          (setq my/current-themes custom-enabled-themes)
          (mapc (lambda (theme)
                  (unless (eq demo-theme theme)
                    (disable-theme theme)))
                custom-enabled-themes)
          (unless (member demo-theme custom-enabled-themes)
            (load-theme demo-theme t))
          ;; Font
          (set-face-attribute
           'default nil
           :family "FantasqueSansMono"
           :slant 'normal
           :height demo-fontsize
           :width 'normal)
          ;; Frame size and parameters
          (setq my/frame-name
                (frame-parameter nil 'name))
          (set-frame-parameter nil 'name "emacs-demo")
          (set-frame-size (selected-frame)
                          demo-width
                          demo-height
                          'pixelwise)

          (visual-line-mode 1))

      ;; Restore popup behavior
      (demo-popper-apply-settings 'user)
      
      ;; Restore themes
      (unless (member demo-theme my/current-themes)
        (disable-theme demo-theme))
      (mapc (lambda (theme) (load-theme theme t)) my/current-themes)

      ;; Restore font
      (set-face-attribute
       'default nil
       :family "FantasqueSansMono"
       :slant 'normal
       :height 125
       :width 'normal)
      (set-frame-parameter
       nil
       'name my/frame-name)

      ;; Restore mode-line
      (when demo-mode-line-p
        (my/mode-line-hidden-mode -1))
      (when demo-keycast-p
        (keycast-mode -1))

      ;; Restore frame
      (set-frame-size (selected-frame) 80 26))))

;; Popper mode
(use-package popper
  :after popper
  :config
  (defun demo-popper-apply-settings (style)
    "Apply popper settings for demo"
    (pcase style
      ('t (setq popper-display-control t
                popper-group-function nil)
          (add-to-list 'popper-reference-buffers "^\\*Customize")
          (add-to-list 'popper-reference-buffers "^\\*Finder-package\\*$")
          (add-to-list 'popper-reference-buffers "^\\*Dictionary\\*$")
          (popper-mode 0)
          (popper-mode 1)
          (popper-echo-mode 0)
          ;; (setq popper-window-height 18)
          (setq popper-mode-line '(:eval
                                   (propertize " POP" 'face 'mode-line-emphasis))))
      ('nil (popper-echo-mode 0)
            (popper-mode 0))
      ('user (setq popper-display-control 'user
                   popper-group-function #'my/popper-group-by-heuristic)
             (popper-mode 0)
             (popper-mode 1)
             (popper-echo-mode 1)
             (setq popper-mode-line nil)))))

;; Keycast
(use-package keycast
  :after keycast
  :config
  ;; (keycast-mode 1)
  (setq keycast-window-predicate (lambda ()
                                   (and (keycast--active-frame-p)
                                        (window-at-side-p nil 'top)
                                        (window-at-side-p nil 'right)))))

;; Completion
(use-package emacs
  :config
  (setq completion-in-region-function #'consult-completion-in-region))

;; Elmo
(use-package elmo
  :disabled
  :config
  (setf (alist-get 't embark-collect-initial-view-alist)
        'grid))
(use-package elmo
  :disabled
  :config
  (progn "Elmo demo"
         (setq elmo-height
               (lambda (win)
                 (fit-window-to-buffer
                  win
                  14)))
         (elmo-mode -1)
         (elmo-mode 1)
         (setq mode-line-keycast-format "%s%k %r")
         (defun force-keycast-update (&rest _)
           (while-no-input (redisplay)
                           (force-mode-line-update t)))
         (add-hook 'post-command-hook #'force-keycast-update)))

;; Yas
(use-package emacs
  :config
  :disabled
  (setq yas-triggers-in-field t)
  (setq preview-scale (lambda nil (* 1.25 (funcall (preview-scale-from-face))))))
(use-package emacs
  :disabled
  :config
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'eval-last-sexp))

;; Embark
(use-package emacs
  :disabled
  :config
  (setq embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            ;; embark-vertico-indicator
                            embark-isearch-highlight-indicator)))

;; avy
(use-package emacs
  :disabled
  :config
  (defun avy-action-embark (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
               #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (xref-pulse-momentarily)
        (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (my/pulse-momentary-line)
      (sit-for 0.4)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-stay (pt)
    "Kill sexp at PT."
    (save-excursion
      (goto-char pt)
      (avy-forward-item)
      (pulse-momentary-highlight-region pt (point) 'next-error)
      (sit-for 0.4)
      (kill-region pt (point))
      (just-one-space))
    ;; (message "Killed: %s" (current-kill 0))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-tuxi (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
               #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (my/pulse-momentary)
        (sit-for 0.1)
        (google-search-at-point))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (my/pulse-momentary-line)
      (sit-for 0.33)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-zap-to-char (pt)
    "Kill from point up to PT."
    (if (> pt (point))
        (progn
          (pulse-momentary-highlight-region (point) pt 'next-error)
          (sit-for 0.33)
          (kill-region (point) pt))
      (pulse-momentary-highlight-region pt (point) 'next-error)
      (sit-for 0.33)
      (kill-region pt (point))))

  (defun avy-action-kill-stay (pt)
    "Kill sexp at PT."
    (save-excursion
      (goto-char pt)
      (avy-forward-item)
      (destructuring-bind (beg . end) (bounds-of-thing-at-point 'sexp)
        (pulse-momentary-highlight-region beg end 'next-error))
      (sit-for 0.40)
      (kill-region pt (point))
      (just-one-space))
    ;; (message "Killed: %s" (current-kill 0))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    (my/pulse-momentary)
    t)

  (defun avy-action-copy (pt)
    "Copy sexp starting on PT."
    (save-excursion
      (let (str)
        (goto-char pt)
        (avy-forward-item)
        (setq str (buffer-substring pt (point)))
        (destructuring-bind (beg . end) (bounds-of-thing-at-point 'sexp)
          (pulse-momentary-highlight-region beg end 'next-error))
        (sit-for 0.25)
        (kill-new str)
        ;; (message "Copied: %s" str)

        ))
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat))))

  (defun avy-action-yank (pt)
    "Yank sexp starting at PT at the current point."
    (avy-action-copy pt)
    (yank)
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    (my/pulse-momentary-line)
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (my/pulse-momentary-line)
      (sit-for 0.30)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (setq avy-background t)
  (setq avy-dispatch-alist '((107 . avy-action-kill-stay)
                           (75 . avy-action-kill-whole-line)
                           (121 . avy-action-yank)
                           (89 . avy-action-yank-whole-line)
                           (119 . avy-action-copy)
                           (87 . avy-action-copy-whole-line)
                           (116 . avy-action-teleport)
                           (84 . avy-action-teleport-whole-line)
                           (109 . avy-action-mark)
                           (32 . avy-action-mark-to-char)
                           (122 . avy-action-zap-to-char)
                           (72 . avy-action-helpful)
                           (105 . avy-action-ispell)
                           (61 . avy-action-define)
                           (?G . avy-action-tuxi)
                           (111 . avy-action-embark)))

  (defun my/pulse-momentary (&rest args)
  (pulse-momentary-highlight-region (point) (save-excursion (forward-sexp) (point)) 'next-error))
  (defun avy-show-dispatch-help ()

  (let* ((len (length "avy-action-"))
         (fw (frame-width))
         (raw-strings (mapcar
                   (lambda (x)
                     (format "%2s: %-19s"
                             (propertize
                              (char-to-string (car x))
                              'face 'aw-key-face)
                             (substring (symbol-name (cdr x)) len)))
                   avy-dispatch-alist))
         (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
         (strings-len (length raw-strings))
         (per-row (floor fw max-len))
         display-strings)
    (cl-loop for string in raw-strings
             for N from 1 to strings-len do
             (push (concat string " ") display-strings)
             (when (= (mod N per-row) 0) (push "\n" display-strings)))
    (message "%s" (apply #'concat (nreverse display-strings)))))

  ;; (advice-add 'avy-goto-char-timer :after #'my/pulse-momentary)
  ;; (advice-remove 'avy-goto-char-timer #'my/pulse-momentary)
  )
(use-package org
  :disabled
  :config
  (setq org-image-actual-width 240))

;; Calc
(defun latex-math-from-calc ()
  
      "Evaluate `calc' on the contents of line at point."
      (interactive)
      (cond ((region-active-p)
             (let* ((beg (region-beginning))
                    (end (region-end))
                    (string (buffer-substring-no-properties beg end)))
               (kill-region beg end)
               (insert (calc-eval `(,string calc-language latex
                                            calc-prefer-frac t
                                            calc-angle-mode rad)))))
            (t (let ((l (thing-at-point 'line)))
                 (end-of-line 1) (kill-line 0)
                 (insert (calc-eval `(,l
                                      calc-language latex
                                      calc-prefer-frac t
                                      calc-angle-mode rad)))))))

(defun my/vertico-extensions-demo ()
  (interactive)
  (require 'vertico-flat)
  (require 'vertico-indexed)
  (require 'vertico-grid)
  (require 'vertico-mouse)
  (require 'vertico-unobtrusive)
  (require 'vertico-reverse)
  (require 'vertico-buffer)

  (setq tab-bar-show nil)
  (project-x-mode 0)
  (add-hook 'embark-collect-mode-hook #'hl-line-mode)
  (setq default-directory "~/.emacs.orig/lisp/")
  (setq vertico-count 14)
  (setq vertico-flat-max-lines 2)
  (setq vertico-buffer-display-action
        '(display-buffer-in-direction
          (direction . right)
          (window-width . 0.5)))
  (advice-add 'push-button :after
              (defun my/only-window (&rest _) (delete-other-windows)))
  
  (defun my/activate-line (&rest _)
    (if (member 'highlight
                (mapcar
                 (lambda (ov) (overlay-get ov 'face))
                 (overlays-at (point))))
        (mapc #'delete-overlay (overlays-at (point)))
      (overlay-put
       (make-overlay (line-beginning-position) (line-end-position))
       'face 'highlight)))
  
  (advice-add 'push-button :after #'my/activate-line))

(provide 'demo)
;;; demo.el ends here
