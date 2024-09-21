;; demo.el --- Helpers to demo Emacs features -*- lexical-binding: t; -*-

;;; Commentary:
;; Some helpers to demo Emacs features. Call `demo-transient' to begin.

;;; Code:
(require 'use-package)
(require 'transient)

(keymap-set mode-specific-map "D" #'demo-transient)

(defgroup demo nil
  "Demo mode details."
  :prefix "demo-"
  :group 'convenience)

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
   (intern-soft (completing-read "Aspect ratio: " '(tall wide nil))))

  (demo--define-infix
   "m" "mode-line-p" "mode-line?"
   'boolean t
   (not demo-mode-line-p))

  (demo--define-infix
   "h" "height" "Height in pixels"
   'integer 840
   (pcase demo-aspect-ratio
     ('tall 840)
     ('wide 480)
     (_ (read-number "Height (pixels): "))))

  (demo--define-infix
   "w" "width" "Width in pixels"
   'integer 740
   (pcase demo-aspect-ratio
     ('tall 740)
     ('wide 720)
     (_ (read-number "Width (pixels): "))))

  (demo--define-infix
   "t" "theme" "Theme"
   'theme 'modus-operandi
   (intern-soft
    (completing-read "Use theme: "
                     (mapcar #'symbol-name
			     (custom-available-themes))
                     nil t)))

  (demo--define-infix
   "f" "fontsize" "Font size"
   'integer (face-attribute 'default :height)
   (read-number "Font size (points): "))

  (demo--define-infix
   "k" "keycast-p" "keycast-mode?"
   'boolean t
   (not demo-keycast-p))

  (demo--define-infix
   "c" "autocomplete-p" "autocompletion?"
   'boolean t
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
     ('nil 't)))

  (demo--define-infix
   "vt" "truncate-lines-p"  "truncate lines?"
   'boolean nil
   (not demo-truncate-lines-p))

  (demo--define-infix
   "T" "tab-bar-p"  "Tab Bar?"
   'boolean nil
   (not demo-tab-bar-p)))

(transient-define-prefix demo-transient ()
  "Turn on demo mode"
  [["Size"
    (demo--set-aspect-ratio)
    (demo--set-height)
    (demo--set-width)
    ""
    "Appearance"
    (demo--set-theme)
    (demo--set-fontsize)]
   ["Modes"
    (demo--set-mode-line-p)
    (demo--set-tab-bar-p)
    (demo--set-keycast-p)
    (demo--set-autocomplete-p)
    (demo--set-popper-style)
    (demo--set-truncate-lines-p)]]
  ["Action"
   ("RET" "Toggle demo-mode" demo-mode)])

(define-minor-mode demo-mode
  "Mode for demos."
  :global t
  :keymap nil
  (if demo-mode
      (progn
        ;; Save all current settings
        (put 'demo-aspect-ratio :name (frame-parameter nil 'name))
        (put 'demo-mode-line-p :original (default-value 'mode-line-format))
        (put 'demo-theme :original custom-enabled-themes)
        (put 'demo-fontsize :original (face-attribute 'default :height))
        (put 'demo-keycast-p
             :original
             (or (and (bound-and-true-p keycast-tab-bar-mode) 'tab-bar)
                 (and (bound-and-true-p keycast-mode-line-mode) 'mode-line)))
        (put 'demo-autocomplete-p
             :original (or (bound-and-true-p corfu-mode)
                           (bound-and-true-p company-mode)
                           0))
        (put 'demo-popper-style
             :original (and (bound-and-true-p popper-mode)
                            (bound-and-true-p popper-display-control)))
        (put 'demo-truncate-lines-p :original truncate-lines)
        (put 'demo-tab-bar-p :original (and tab-bar-mode tab-bar-show))
        
        ;; Always needed
        (add-hook 'grep-mode-hook 'toggle-truncate-lines)
        
	;; No emacs messages when turning off screen recording
        (global-set-key (kbd "C-s-r") #'ignore)
        (global-set-key (kbd "C-s-SPC") #'ignore)
        (global-set-key (kbd "s-r") #'ignore)
        (global-set-key (kbd "C-s-t") #'ignore)
        ;; Unneeded
        (when (featurep 'project-x)
	  (dolist (mode '(project-x-mode))
            (if (bound-and-true-p mode)
                (funcall (symbol-function mode) 0)))
          
          (project-x-mode 0))
        
        ;; Popups
        (demo-popper-apply-settings demo-popper-style)
        
        ;; Autocompletion
        (cond
         ((fboundp 'corfu-mode) (corfu-mode (if demo-autocomplete-p 1 0)))
         ((fboundp 'company-mode) (company-mode (if demo-autocomplete-p 1 0))))
        ;; Mode line
        (unless demo-mode-line-p
          (setq-local mode-line-format nil))
        ;; Tab bar
        (if demo-tab-bar-p
            (progn (setq tab-bar-show t)
                   (tab-bar-mode 0)
                   (tab-bar-mode 1))
          (tab-bar-mode 0))
        ;; Keycast
        (when (and demo-keycast-p)
          (require 'keycast)
          (add-to-list 'keycast-substitute-alist '(ignore nil nil))
          (if demo-tab-bar-p
	      (keycast-tab-bar-mode 1)
            (keycast-mode-line-mode 1)))
        
	;; Theme
        (mapc (lambda (theme)
                (unless (equal theme demo-theme)
                  (disable-theme theme)))
              custom-enabled-themes)
        (unless (member demo-theme custom-enabled-themes)
          (load-theme demo-theme t))

	(let ((inhibit-message t))
	  (if demo-truncate-lines-p
	      (toggle-truncate-lines 1)
	    (toggle-truncate-lines -1)
	    (visual-line-mode 1)))
	
	;; Font
        (set-face-attribute
         'default nil
         ;; :family "Monospace"
         ;; :slant 'normal
         :height demo-fontsize
         ;; :width 'normal
         )

        ;; Frame size and parameters
        (set-frame-parameter nil 'name "emacs-demo")
        (let ((frame-resize-pixelwise t)
              (frame-inhibit-implied-resize t))
          (set-frame-size
           (selected-frame) demo-width demo-height
           'pixelwise)))

    ;; Restore popup behavior
    (demo-popper-apply-settings
     (get 'demo-popper-style
          :original))
    
    ;; Restore themes
    (let ((original-themes (get 'demo-theme :original)))
      (mapc (lambda (theme)
              (unless (memq theme original-themes)
                (disable-theme theme)))
            custom-enabled-themes)
      (dolist (theme original-themes)
        (enable-theme theme)))

    ;; Restore autocompletion
    (cond
     ((fboundp 'corfu-mode) (corfu-mode (get 'demo-autocomplete-p :original)))
     ((fboundp 'company-mode) (company-mode (get 'demo-autocomplete-p :original))))
    
    ;; Restore font
    (set-face-attribute
     'default nil
     ;; :family "Monospace"
     ;; :slant 'normal
     ;; :width 'normal
     :height (get 'demo-fontsize :original))
    (set-frame-parameter nil 'name (get 'demo-aspect-ratio :name))

    (pcase (get 'demo-keycast-p :original)
      ('tab-bar (keycast-tab-bar-mode 1)
                (keycast-mode-line-mode -1))
      ('mode-line (keycast-tab-bar-mode -1)
                  (keycast-mode-line-mode 1))
      ('nil (when keycast-tab-bar-mode
              (keycast-tab-bar-mode -1))
            (when keycast-mode-line-mode
              (keycast-mode-line-mode -1))))
    
    ;; Restore mode-line, tab-bar and frame settings
    (kill-local-variable 'mode-line-format)
    (setq mode-line-format (get 'demo-mode-line-p :original))
    (setq tab-bar-show (or (get 'demo-tab-bar-p :original) 1))
    (tab-bar-mode (and (get 'demo-tab-bar-p :original) t))
    (force-mode-line-update)))

;; Org
(use-package org
  :disabled
  :config
  (setq org-latex-preview-default-process 'dvipng)
  (plist-put org-latex-preview-options :scale 1.5)
  (setq org-latex-preview-debounce 0.1)
  (setq org-latex-preview-throttle 0.1))

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
                   popper-group-function nil)
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
  :disabled
  :config
  (setq completion-in-region-function
	(if (featurep 'consult)
	    #'consult-completion-in-region
	  #'completion--in-region)))

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

(provide 'demo)
;;; demo.el ends here
