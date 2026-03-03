;; -*- lexical-binding: t; -*-

;;;; DISPLAY-BUFFER PREFERENCES
(use-package window
  :hook ((help-mode . visual-line-mode)
         (Custom-mode . visual-line-mode)
         (helpful-mode . visual-line-mode))
  :init
  (setq window-combination-resize t
        even-window-sizes 'height-only
        window-sides-vertical nil
        fit-window-to-buffer-horizontally t
        window-resize-pixelwise t
        fit-frame-to-buffer t)
  (setq display-buffer-base-action
        '((display-buffer-reuse-window
           display-buffer-in-previous-window
           display-buffer-pop-up-window
           display-buffer-use-some-window)
          (some-window . mru)
          (reusable-frames . nil)))
  :config
  (defun my/display-buffer-reuse-minor-mode-window (buffer alist)
    (let* ((alist-entry (assq 'reusable-frames alist))
           (alist-mode-entry (assq 'minor-mode alist))
	   (frames (cond (alist-entry (cdr alist-entry))
		         ((if (eq pop-up-frames 'graphic-only)
			      (display-graphic-p)
			    pop-up-frames)
			  0)
		         (display-buffer-reuse-frames 0)
		         (t (last-nonminibuffer-frame))))
           (inhibit-same-window-p (cdr (assq 'inhibit-same-window alist)))
	   (windows (window-list-1 nil 'nomini frames))
           (allowed-modes (if alist-mode-entry
                              (cdr alist-mode-entry)))
           (curwin (selected-window))
           (curframe (selected-frame)))
      (unless (listp allowed-modes)
        (setq allowed-modes (list allowed-modes)))
      (let ((same-mode-same-frame)
            (same-mode-other-frame))
        (dolist (window windows)
          (let ((mode?
                 (with-current-buffer (window-buffer window)
                   (cl-some (lambda (m) (and (boundp m) (symbol-value m) 'same))
                            allowed-modes))))
            (when (and mode? (not (and inhibit-same-window-p (eq window curwin))))
              (push window (if (eq curframe (window-frame window))
                               same-mode-same-frame
                             same-mode-other-frame)))))
        (let ((window (car (nconc same-mode-same-frame
                                  same-mode-other-frame))))
          (when (window-live-p window)
            (prog1 (window--display-buffer buffer window 'reuse alist)
              (unless (cdr (assq 'inhibit-switch-frame alist))
                (window--maybe-raise-frame (window-frame window)))))))))

  (defun buffer-mode (&optional buffer-or-name)
    "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
    (buffer-local-value 'major-mode
                        (if buffer-or-name
                            (get-buffer buffer-or-name)
                          (current-buffer))))

  (defvar my/occur-grep-modes-list '(occur-mode
                                     grep-mode
                                     xref--xref-buffer-mode
                                     ivy-occur-grep-mode
                                     ivy-occur-mode
                                     locate-mode
                                     flymake-diagnostics-buffer-mode
                                     rg-mode)
    "List of major-modes used in occur-type buffers")

  ;; This does not work at buffer creation since the major-mode for
  ;; REPLs is not yet set when `display-buffer' is called, but is
  ;; useful afterwards
  (defvar my/repl-modes-list '(matlab-shell-mode
                               eshell-mode
                               geiser-repl-mode
                               shell-mode
                               eat-mode
                               vterm-mode
                               inferior-python-mode
                               cider-repl-mode
                               fennel-repl-mode
                               jupyter-repl-mode
                               inferior-ess-julia-mode)
    "List of major-modes used in REPL buffers")

  (defvar my/repl-names-list
    '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
      "\\*.*REPL.*\\*"
      "\\*MATLAB\\*"
      "\\*Python\\*"
      "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
      "\\*Inferior .*\\*$"
      "^\\*julia.*\\*$"
      "^\\*cider-repl.*\\*$"
      "\\*ielm\\*"
      "\\*edebug\\*")
    "List of buffer names used in REPL buffers")

  (defvar my/help-modes-list '(helpful-mode
                               help-mode
                               pydoc-mode
                               TeX-special-mode)
    "List of major-modes used in documentation buffers")

  (defvar my/man-modes-list '(Man-mode woman-mode)
    "List of major-modes used in Man-type buffers")

  (defvar my/message-modes-list '(compilation-mode
                                  edebug-eval-mode)
    "List of major-modes used in message buffers")

  (setq display-buffer-alist
        '(("^\\*[Ee]shell [Ee]xport: .*\\*$"
           (display-buffer-reuse-window display-buffer-use-some-window))

          ("^\\*julia\\*"
           (display-buffer-reuse-mode-window
            display-buffer-reuse-window
            display-buffer-in-direction
            display-buffer-in-side-window)
           (body-function . select-window)
           (window-height . .35)
           (window-width .  .40)
           ;; (preserve-size . (nil . t))
           (direction . below)
           (side . bottom)
           (slot . 1))
          ;; ----------------------------------------------------------------
          ;; Windows on top
          ;; ----------------------------------------------------------------

          ("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
           (display-buffer-below-selected
            display-buffer-in-side-window)
           (body-function . select-window)
           (window-height . (lambda (win) (fit-window-to-buffer win nil 12)))
           (side . top)
           (slot . -2)
           (preserve-size . (nil . t))
           (window-parameters . ((mode-line-format . nil))))

          ("\\*Buffer List\\*" (display-buffer-in-side-window)
           (side . top)
           (slot . 0)
           (window-height . shrink-window-if-larger-than-buffer))

          ((lambda (buf act) (member (buffer-mode buf) my/occur-grep-modes-list))
           (display-buffer-reuse-mode-window
            display-buffer-in-direction
            display-buffer-in-side-window)
           (side . top)
           (slot . 5)
           (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
           (direction . above)
           (body-function . select-window))

          ("\\*\\(Flycheck\\|Package-Lint\\).*"
           (display-buffer-in-direction display-buffer-in-side-window)
           (direction . above)
           (window-height . shrink-window-if-larger-than-buffer)
           ;; (window-height . 0.16)
           (side . top)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))

          ;; ----------------------------------------------------------------
          ;; Windows on the side
          ;; ----------------------------------------------------------------

          ((lambda (buf act) (member (buffer-mode buf) my/man-modes-list))
           nil (body-function . select-window))

          ("\\*Faces\\*" (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . -2)
           (window-parameters . ((no-other-window . t))))

          ;; ----------------------------------------------------------------
          ;; Windows at the bottom
          ;; ----------------------------------------------------------------

          ("\\*Backtrace\\*" (display-buffer-in-side-window)
           (window-height . 0.20)
           (side . bottom)
           (slot . -9))

          ((lambda (buf act) (member (buffer-mode buf) my/message-modes-list))
           (display-buffer-at-bottom display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . -6)
           (bump-use-time . t))

          ("\\*Messages\\*"
           (display-buffer-at-bottom display-buffer-in-side-window display-buffer-in-direction)
           (window-height . (lambda (win) (fit-window-to-buffer
                                      win
                                      (floor (frame-height) 5))))
           (side . bottom)
           (direction . below)
           (slot . -6)
           (body-function . select-window)
           (window-parameters . ((split-window . #'ignore))))

          ("\\*\\(?:Warnings\\|Compile-Log\\)\\*" ;\\|Tex Help\\|TeX errors
           (display-buffer-at-bottom display-buffer-in-side-window display-buffer-in-direction)
           (window-height . (lambda (win) (fit-window-to-buffer
                                      win
                                      (floor (frame-height) 5))))
           (side . bottom)
           (direction . below)
           (slot . -5)
           (window-parameters . ((split-window . #'ignore))))

          ("[Oo]utput\\*" display-buffer-in-side-window
           (window-height . (lambda (win)
                              (fit-window-to-buffer win (floor (frame-height) 2.5))))
           (side . bottom)
           (slot . -4)
           (bump-use-time . t))

          ("\\*Async Shell Command\\*" display-buffer-in-side-window
           (window-height . 0.20)
           (side . bottom)
           (slot . -4)
           ;; (preserve-size . (nil . t))
           (bump-use-time . t))

          ("\\*\\(Register Preview\\).*" (display-buffer-in-side-window)
           (window-height . 0.20)       ; See the :hook
           (side . bottom)
           (slot . -3)
           (window-parameters . ((no-other-window . t))))

          ("\\*Completions\\*" (display-buffer-in-side-window)
           (window-height . 0.20)
           (side . bottom)
           (slot . -2))

          ("\\*Apropos\\*" (display-buffer-in-side-window)
           ;; (window-height . 0.40)
           (window-width . 65)
           (side . right)
           (slot . -2)
           (dedicated . t)
           (body-function . select-window))


          ((lambda (buf act) (or (seq-some (lambda (regex) (string-match-p regex buf))
                                      my/repl-names-list)
                            (seq-some (lambda (mode)
                                        (equal
                                         (buffer-mode buf)
                                         mode))
                                      my/repl-modes-list)))
           (display-buffer-reuse-window
            display-buffer-in-direction
            display-buffer-in-side-window)
           (body-function . select-window)
           ;; display-buffer-at-bottom
           (window-height . .35)
           (window-width .  .40)
           ;; (preserve-size . (nil . t))
           (direction . below)
           (side . bottom)
           (slot . 1))

          ((lambda (buf act) (member (buffer-mode buf) my/help-modes-list))
           (display-buffer-reuse-window
            display-buffer-in-side-window
            display-buffer-in-direction)
           (body-function . select-window)
           (window-width . 77)
           ;; (lambda (win) (fit-window-to-buffer win nil nil 75 65))
           (direction . below)
           (side . right)
           (slot . 2)
           (window-parameters . ((split-window . #'ignore))))

          ("^\\*eldoc.*\\*$"
           (display-buffer-reuse-window
            display-buffer-in-direction
            display-buffer-in-side-window)
           (window-width . 82)
           (direction . below)
           (side . below)
           (slot . 2)
           (window-parameters . ((dedicated . t)
                                 (split-window . #'ignore)
                                 (no-other-window . t)
                                 (mode-line-format . none))))

          ((lambda (buf act) (member (buffer-mode buf) '(ibuffer-mode bookmark-bmenu-mode)))
           (display-buffer-below-selected)
           (body-function . select-window)
           (direction . below)
           (window-height . (lambda (win) (fit-window-to-buffer win 30 7)))
           (side . bottom)
           (slot . 2)))))

;;;; MANUAL WINDOW MANAGEMENT
;;;;; Splitting windows, switching buffers
(use-package window
  :bind (("C-x q" . my/kill-buffer-and-window)
         ("ESC M-v" . scroll-other-window-down)
         ("<f9>" . my/make-frame-floating-with-current-buffer)
         ("M-`" . my/switch-to-other-buffer)
         ("C-x C-p" . my/previous-buffer)
         ("C-x C-n" . my/next-buffer)
         ("C-x n g" . set-goal-column)
         ("C-x C-1" . my/monocle-mode)         
         ([remap split-window-below] . my/split-window-below)
         ([remap split-window-right] . my/split-window-right)
         ([remap delete-window] . my/delete-window-or-delete-frame)
         :map window-prefix-map
         ("1" . my/window-toggle-dedicated))
  :config
  (setq switch-to-prev-buffer-skip nil) ;'this
  (setq truncate-partial-width-windows t)
  (setq other-window-scroll-default
        (lambda ()
          (or (get-mru-window nil t 'not-this-one-dummy)
              (next-window)
              (next-window nil nil 'visible))))

  (defun my/split-window-right (&optional size)
    "Split the selected window into two windows, one above the other.
The selected window is below.  The newly split-off window is
below and displays the same buffer.  Return the new window."
    (interactive "P")
    (select-window
     (if size
         (split-window (frame-root-window)
                       (floor (frame-width) 2)
                       t nil)
       (split-window-right size)))
    (when (interactive-p)
      (if (featurep 'consult)
          (consult-buffer)
        (call-interactively #'switch-to-buffer))))

  (defun my/split-window-below (&optional size)
    "Split the selected window into two side-by-side windows.
The selected window is on the left.  The newly split-off window
is on the right and displays the same buffer.  Return the new
window."
    (interactive "P")
    (select-window
     (if size
         (split-window (frame-root-window)
                       (floor (frame-height) 2)
                       nil nil)
       (split-window-below size)))
    (when (interactive-p)
      (if (featurep 'consult)
          (consult-buffer)
        (call-interactively #'switch-to-buffer))))

  (defun my/delete-window-if-not-single ()
    "Delete window if not the only one."
    (when (not (one-window-p))
      (delete-window)))

  (defun my/delete-window-or-delete-frame (&optional window)
    "Delete WINDOW using `delete-window'.
If this is the sole window run `delete-frame' instead. WINDOW
must be a valid window and defaults to the selected one. Return
nil."
    (interactive)
    (condition-case nil
        (delete-window window)
      (error (if (and tab-bar-mode
                      (> (length (funcall tab-bar-tabs-function)) 1))
                 (tab-bar-close-tab)
               (delete-frame)))))

  (defun my/kill-buffer-and-window ()
    "Kill buffer.

Also kill this window, tab or frame if necessary."
    (interactive)
    (if (one-window-p)
        (progn (kill-buffer)
               (my/delete-window-or-delete-frame))
      (kill-buffer-and-window)))

  (defun my/window-toggle-dedicated (&optional win)
    (interactive (list (selected-window)))
    (let ((dedicated (window-dedicated-p win)))
      (set-window-dedicated-p win (not dedicated))
      (message "Window marked as %s." (if dedicated "free" "dedicated"))))

  (defun my/make-frame-floating-with-current-buffer ()
    "Display the current buffer in a new floating frame.

This passes certain parameters to the newly created frame:

- use a different name than the default;
- use a graphical frame;
- do not display the minibuffer.

The name is meant to be used by the external rules of a tiling
window manager to present the frame in a floating state."
    (interactive)
    (let ((buf (current-buffer)))
      (if (not (one-window-p t))
          (delete-window))
      (make-frame '((name . "dropdown_emacs-buffer")
                    (window-system . x)
                    (minibuffer . nil)))
      (with-selected-frame (get-other-frame)
        (switch-to-buffer buf))))

  ;; Buffer switching/cycling with repeat-mode
  (defun my/switch-to-other-buffer (&optional _arg)
    (interactive)
    (switch-to-buffer (other-buffer)))
  (put 'my/switch-to-other-buffer 'repeat-map
       'my/buffer-cycle-map)

  (defun my/next-buffer (&optional arg)
    "Switch to the next non-popup buffer."
    (interactive "P")
    (if-let (((equal arg '(4)))
             (win (other-window-for-scrolling)))
        (with-selected-window win
          (my/next-buffer)
          (setq prefix-arg current-prefix-arg))
      (dotimes (or (abs (prefix-numeric-value arg)) 1)
        (my/switch-buffer-1 #'next-buffer))))

  (defun my/previous-buffer (&optional arg)
    "Switch to the previous non-popup buffer."
    (interactive "P")
    (if-let (((equal arg '(4)))
             (win (other-window-for-scrolling)))
        (with-selected-window win
          (my/previous-buffer)
          (setq prefix-arg current-prefix-arg))
      (dotimes (or (abs (prefix-numeric-value arg)) 1)
        (my/switch-buffer-1 #'previous-buffer))))

  (defun my/switch-buffer-1 (switch)
    "Switch to the next user buffer in cyclic order.
User buffers are those not starting with *."
    (funcall switch)
    (let ((i 0))
      (while (and (< i 50)
                  (member (buffer-local-value 'popper-popup-status (current-buffer))
                          '(popup user-popup)))
        (setq i (1+ i)) (funcall switch))))

  (defvar my/buffer-cycle-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'my/next-buffer)
      (define-key map (kbd "p") #'my/previous-buffer)
      (define-key map (kbd "`") #'my/switch-to-other-buffer)
      (define-key map (kbd "b")
                  (defun my/switch-buffer ()
                    "Switch to consult-buffer"
                    (interactive)
                    (run-at-time
                     0 nil
                     (lambda (&optional arg)
                       (interactive "P")
                       (if-let (((equal arg '(4)))
                                (win (other-window-for-scrolling)))
                           (with-selected-window win (consult-buffer))
                         (consult-buffer)))
                     current-prefix-arg)))
      map))

  (map-keymap
   (lambda (_ cmd) (put cmd 'repeat-map 'my/buffer-cycle-map))
   my/buffer-cycle-map)

  (defvar my/window-configuration nil
    "Current window configuration.
Intended for use by `my/monocle-mode.")

  (define-minor-mode my/monocle-mode
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    :lighter " [M]"
    :global nil
    (let ((win my/window-configuration))
      (if (one-window-p)
          (when win
            (set-window-configuration win))
        (setq my/window-configuration (current-window-configuration))
        (when (window-parameter nil 'window-slot)
          (let ((buf (current-buffer)))
            (other-window 1)
            (switch-to-buffer buf)))
        (delete-other-windows))))

  (defun other-window-prefix-maybe ()
    (interactive)
    (other-window-prefix)
    (setq unread-command-events (list last-command-event))
    (when-let ((seqs (read-key-sequence "[other-window]: "))
               (cmd (key-binding seqs)))
      (setq this-command cmd)
      (call-interactively cmd)))
  (keymap-set ctl-x-4-map "<t>" #'other-window-prefix-maybe)
  
  ;; quit-window behavior is completely broken
  ;; Fix by adding winner-mode style behavior to quit-window
  (defun my/better-quit-window-save (window)
    (push (window-parameter window 'quit-restore)
          (window-parameter window 'quit-restore-stack))
    window)

  (defun my/better-quit-window-restore (origfn &optional window bury-or-kill)
    (let ((sw (or window (selected-window))))
      (funcall origfn window bury-or-kill)
      (when (eq sw (selected-window))
        (pop (window-parameter nil 'quit-restore-stack))
        (setf (window-parameter nil 'quit-restore)
              (car (window-parameter nil 'quit-restore-stack))))))

  (advice-add 'display-buffer :filter-return #'my/better-quit-window-save)
  (advice-add 'quit-restore-window :around #'my/better-quit-window-restore))

;;;;; switchy-window - switching windows
(use-package switchy-window
  :ensure t
  :defer 2
  :init (switchy-window-minor-mode)
  :bind (("M-o" . switchy-window)
         :map other-window-repeat-map
         ("o" . switchy-window))
  :config
  (setq switchy-window-delay 0.75)
  (put 'switchy-window 'repeat-map 'other-window-repeat-map))

(use-package window
  :unless (fboundp 'switchy-window-minor-mode)
  :bind (("M-o" . my/other-window)
         ("M-O" . my/other-window-prev)
         :map other-window-repeat-map
         ("o" . my/other-window)
         ("O" . my/other-window-prev))
  :config
  (defalias 'my/other-window
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive)
        (if (equal last-command 'my/other-window)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))
  (defun my/other-window-prev (&optional arg all-frames)
    (interactive "p")
    (other-window (if arg (- arg) -1) all-frames))
  (put 'my/other-window 'repeat-map 'other-window-repeat-map)
  (put 'my/other-window-prev 'repeat-map 'other-window-repeat-map))

;;;;; Ace-window - switching windows
(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window)
   ("H-o"   . ace-window)
   ("C-M-0" . ace-window-prefix)
   ("C-M-9" . ace-window)
   :map ctl-x-4-map
   ("o" . ace-window-prefix))
  ;; :custom-face
  ;; (aw-leading-char-face ((t (:height 2.5 :weight normal))))
  :defer 2
  :init (ace-window-display-mode 1)
  :custom-face (aw-mode-line-face ((t (:inherit (bold mode-line-emphasis)))))
  :config
  (defun my/aw-take-over-window (window)
    "Move from current window to WINDOW.

Delete current window in the process."
    (let ((buf (current-buffer)))
      (if (one-window-p)
          (delete-frame)
        (delete-window))
      (aw-switch-to-window window)
      (switch-to-buffer buf)))
  (defun ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  (setq aw-swap-invert t)
  (setq aw-dispatch-always t
        aw-scope 'global
        aw-background nil
        aw-display-mode-overlay nil
        aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?p))
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete Window")
          (?x aw-swap-window "Swap Windows")
          (?m my/aw-take-over-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?o aw-flip-window)
          (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?s aw-split-window-vert "Split Vert Window")
          (?v aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help))))

;;;;; Windmove - switching windows
(use-package windmove
  :after window
  :bind (("H-<right>" . windmove-swap-states-right)
         ("H-<down>" . windmove-swap-states-down)
         ("H-<up>" . windmove-swap-states-up)
         ("H-<left>" . windmove-swap-states-left)
         :map other-window-repeat-map
         ("l" . windmove-right)
         ("k" . windmove-up)
         ("h" . windmove-left)
         ("j" . windmove-down))
  :init
  (dolist (cmd '( windmove-left windmove-right
                  windmove-up windmove-down))
    (put cmd 'repeat-map 'other-window-repeat-map)))

;;;;; transpose-frame - rearranging windows
(use-package transpose-frame
  :ensure t
  :bind (("H-\\" . rotate-frame-anticlockwise)
         :map ctl-x-4-map
         ("|" . flop-frame)
         ("_" . flip-frame)
         ("\\" . rotate-frame-anticlockwise)))

;;;;; Resizing windows
(use-package window
  :config
  (defun my/enlarge-window-horizontally (&optional repeat)
    "Enlarge window horizontally by 8% of the frame width."
    (interactive "p")
    (enlarge-window-horizontally (* (or repeat 1)
                                    (floor (frame-width) 20))))
  (defun my/shrink-window-horizontally (&optional repeat)
    "Enlarge window horizontally by 8% of the frame width."
    (interactive "p")
    (shrink-window-horizontally (* (or repeat 1)
                                   (floor (frame-width) 20))))
  (defun my/shrink-window (&optional repeat)
    "Enlarge window horizontally by 8% of the frame height."
    (interactive "p")
    (shrink-window (* (or repeat 1)
                      (floor (frame-height) 20))))
  (defun my/enlarge-window (&optional repeat)
    "Enlarge window horizontally by 8% of the frame height."
    (interactive "p")
    (enlarge-window (* (or repeat 1)
                       (floor (frame-height) 20))))
  :bind
  (("<C-S-right>" . my/enlarge-window-horizontally)
   ("<C-S-left>"  . my/shrink-window-horizontally)
   ("<C-S-up>"    . my/enlarge-window)
   ("<C-S-down>"  . my/shrink-window)))


;;;; POPPER - POPUP MANAGEMENT
;; Designate buffers to popup status and toggle or cycle through them
(use-package popper
  :ensure (:host github :protocol ssh :repo "karthink/popper")
  ;; :load-path "plugins/popper/"
  :after (window)
  :commands popper-mode
  :bind (("C-`" . popper-toggle)
         ("C-M-`" . popper-cycle)
         ("H-`" . popper-toggle)
         ("H-M-`" . popper-cycle)
         ("H-6" . popper-toggle-type)
         ("C-x 6" . popper-toggle-type)
         ("C-^" . popper-toggle-type)
         ("H-M-k" . popper-kill-latest-popup)
         ("s-M-k" . popper-kill-latest-popup)
         ("M-`" . my/switch-to-other-buffer)
         ("s-n" . my/next-buffer)
         ("s-p" . my/previous-buffer))
  :init
  (if (boundp 'elpaca-after-init-hook)
      (add-hook 'elpaca-after-init-hook #'popper-mode)
    (add-hook 'emacs-startup-hook #'popper-mode))

  (setq popper-reference-buffers
        (append my/help-modes-list
                my/man-modes-list
                my/repl-modes-list
                my/repl-names-list
                my/occur-grep-modes-list
                ;; my/man-modes-list
                '(Custom-mode
                  compilation-mode
                  messages-buffer-mode)
                '(("^\\*Warnings\\*$" . hide)
                  ("^\\*Compile-Log\\*$" . hide)
                  ;; "^\\*Matlab Help.*\\*$"
                  ;; "^\\*Messages\\*$"
                  "^\\*Backtrace\\*"
                  "^\\*evil-registers\\*"
                  "^\\*Apropos"
                  "^Calc:"
                  "^\\*eldoc\\*"
                  "^\\*TeX errors\\*"
                  "^\\*ielm\\*"
                  "^\\*TeX Help\\*"
                  "^\\*gptel-ask\\*"
                  "\\*Shell Command Output\\*"
                  "\\*Async Shell Command\\*"
                  ("\\*Detached Shell Command\\*" . hide)
                  "\\*Completions\\*"
                  "^\\*Org QL View:notmuch-links\\*$"
                  ;; "\\*scratch.*\\*$"
                  "[Oo]utput\\*")))

  ;; (setq popper-group-function
  ;;       (lambda ()
  ;;         (let ((tabs (funcall tab-bar-tabs-function)))
  ;;           (alist-get 'name (nth (tab-bar--current-tab-index tabs)
  ;;                                 tabs)))))

  (use-package embark
    :defer
    :bind ( :map embark-buffer-map
            ("_" . embark-popper-toggle-type))
    :config
    (defun embark-popper-toggle-type (buf)
      "Toggle popup status."
      (popper-toggle-type buf)
      (run-at-time
       0 nil
       (lambda () (when-let* ((win (get-buffer-window buf)))
               (select-window win))))))

  (use-package popper-echo
    :defer 3
    :config
    (defvar popper-echo--propertized-names nil
      "Alist of popup buffer names and their shortened, propertized
display names.")

    (defun popper-message-shorten (full-name)
      (let ((name (file-name-nondirectory full-name)))
        (or (alist-get name popper-echo--propertized-names nil nil #'string=)
            (let ((short-name
                   (cond
                    ((string= "*Messages*" name)
                     (concat (propertize "LOG " 'face 'default)
                             (propertize name 'face 'popper-echo-area-buried)))
                    ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
                     (concat (propertize "HLP " 'face '(:inherit link :underline nil :weight bold))
                             (propertize (match-string 1 name) 'face 'popper-echo-area-buried)))
                    ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
                     (concat (propertize "HLP" 'face '(:inherit link :underline nil :weight bold))
                             (propertize (match-string 1 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^\\*Customize *\\(.*?\\):\\(.*\\)\\*$" name)
                     (concat (propertize
                              (pcase (match-string 1 name)
                                ('"Option" "OPT") ('"Group" "GRP") (s s))
                              'face '(:inherit link :underline nil :weight bold))
                             (propertize (match-string 2 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^\\*\\(e?\\)shell:? ?\\(.*\\)\\*$" name)
                     (concat (if (string-empty-p (match-string 1 name))
                                 (propertize "SH" 'face 'success)
                               (propertize "ESH" 'face 'success))
                             (unless (string-empty-p (match-string 2 name)) " ")
                             (propertize (match-string 2 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^\\*\\(.*?\\)-\\(e?\\)shell\\*$" name)
                     (concat (if (string-empty-p (match-string 2 name))
                                 (propertize "SH" 'face 'success)
                               (propertize "ESH" 'face 'success))
                             (unless (string-empty-p (match-string 1 name)) " ")
                             (propertize (match-string 1 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^[*]?\\(.*?\\) *\\(?:[Oo]utput\\|Command\\)\\*$" name)
                     (concat (propertize "OUT "
                                         'face '(:inherit warning))
                             (propertize (match-string 1 name)
                                         'face 'popper-echo-area-buried)))
                    ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
                     (concat (propertize "LOG "
                                         ;; '(:inherit link-visited :underline nil)
                                         'face 'default)
                             (propertize (match-string 1 name)
                                         'face 'popper-echo-area-buried)))
                    ((or (string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
                         (string-match
                          "^\\*\\(.*?\\)[ -]?\\(?:byte\\)?[ -]?[Cc]ompil\\(?:e\\|ation\\)\\*$" name))
                     (concat (propertize "COM "
                                         'face '(:inherit link-visited :underline nil :weight normal))
                             (propertize (match-string 1 name) 'face 'popper-echo-area-buried)))
                    ((or (cl-member (buffer-mode (get-buffer full-name))
                                    my/repl-modes-list :test #'eq)
                         (cl-member full-name my/repl-names-list :test #'string-match))
                     (concat (propertize "RPL " 'face 'success) name))
                    (t (propertize name 'face 'popper-echo-area-buried)))))
              (cdar (push (cons name (truncate-string-to-width short-name 15 nil nil t))
                          popper-echo--propertized-names))))))

    (setq popper-echo-transform-function #'popper-message-shorten)
    (setq popper-echo-dispatch-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
          popper-echo-dispatch-actions t)
    ;; (advice-add 'popper-echo :around
    ;;             (defun my/popper-echo-no-which-key (orig-fn)
    ;;               (let ((which-key-show-transient-maps nil))
    ;;                 (funcall orig-fn))))
    ;; (popper-tab-line-mode 1)
    (popper-echo-mode +1))

  :config
  (setq popper-display-control 'user)

  (defun my/popper-switch-to-popup (buf)
    ";TODO: "
    (interactive
     (list
      (let ((pred (lambda (b)
                    (if (consp b) (setq b (car b)))
                    (setq b (get-buffer b))
                    (and (popper-popup-p b)
                         (or (not popper-group-function)
                             (memq (get-buffer "*Messages*")
                                   (mapcar #'cdr
                                           (alist-get (funcall popper-group-function)
                                                      popper-buried-popup-alist
                                                      nil nil 'equal))))))))
        (read-buffer "Switch to popup: " nil t pred))))
    (popper-close-latest)
    (display-buffer buf))

  (defvar popper-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "`") #'popper-cycle)
      (define-key map (kbd "~") #'popper-cycle-backwards)
      (define-key map (kbd "b") #'my/popper-switch-to-popup)
      map))
  (put 'popper-cycle-backwards 'repeat-map 'popper-repeat-map)
  (put 'popper-cycle 'repeat-map 'popper-repeat-map)
  (put 'popper-toggle 'repeat-map 'popper-repeat-map)

  (with-eval-after-load 'activities-tabs
    (setq popper-group-function
          (lambda ()
            (or (and-let* ((act (activities-tabs-current)))
                  (activities-activity-name act))
                (selected-frame)))))

  (defun my/popper-select-below (buffer &optional _alist)
    (funcall (if (> (frame-width) 170)
                 ;; #'display-buffer-in-direction
                 #'popper-select-popup-at-bottom
               #'display-buffer-at-bottom)
             buffer
             `((window-height . ,popper-window-height)
               (direction . below)
               (body-function . ,#'select-window))))

  (defun my/popper-display-at-bottom-double (buf &optional _alist)
    "Display BUF in two side-by-side windows at the bottom of the frame.
The windows are created as an atomic unit and use `follow-mode' to
display continuous content across both."
    (let* ((inhibit-redisplay t) (win))
      (if (< (frame-width) 200)
          (progn
            (setq win
                  (display-buffer-at-bottom
                   buf `((side . below)
                         (body-function . ,#'select-window)
                         (window-height . 0.40))))
            (with-selected-window win
              (when (bound-and-true-p follow-mode)
                (follow-mode -1))))
        (setq win
              (display-buffer-at-bottom
               buf `((side . below)
                     (body-function . ,#'select-window)
                     (window-height . ,popper-window-height))))
        (display-buffer-in-atom-window
         buf `((side . right) (window . ,win)))
        (with-selected-window win
          (unless (bound-and-true-p follow-mode)
            (follow-mode 1))))
      win))

  (setq popper-display-function #'my/popper-display-at-bottom-double))

(eval-after-load 'org
  (progn 
    (defun +org-fix-delete-other-windows-a (orig-fn &rest args)
      "docstring"
      (interactive "P")
      (if popper-mode 
          (cl-letf (((symbol-function #'delete-other-windows)
                     (symbol-function #'ignore))
                    ((symbol-function #'delete-window)
                     (symbol-function #'ignore)))
            (apply orig-fn args))
        (apply orig-fn args)))

    (dolist (org-function '(org-add-log-note
                            org-capture-place-template
                            org-export--dispatch-ui
                            org-agenda-get-restriction-and-command
                            org-fast-tag-selection
                            org-fast-todo-selection))

      (advice-add org-function :around #'+org-fix-delete-other-windows-a))))

(provide 'setup-windows)
