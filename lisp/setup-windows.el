;;;###autoload
(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))

;;;###autoload
(defun select-buffer-in-side-window (buffer alist)
  "Display buffer in a side window and select it"
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)))

;;;###autoload
(defun fit-buffer-in-side-window (buffer alist)
  "Display the buffer in a side window and resize it to fit"
  (let ((window (display-buffer-in-side-window buffer alist)))
    (shrink-window-if-larger-than-buffer window)))

(defvar +occur-grep-modes-list '(occur-mode
                                 grep-mode
                                 xref--xref-buffer-mode
                                 ivy-occur-grep-mode
                                 locate-mode)
  "List of major-modes used in occur-type buffers")

;; This does not work at buffer creation since the major-mode for
;; REPLs is not yet set when `display-buffer' is called, but is
;; useful afterwards
(defvar +repl-modes-list '(matlab-shell-mode
                           eshell-mode
                           geiser-repl-mode
                           shell-mode
                           inferior-python-mode)
  "List of major-modes used in REPL buffers")

(defvar +repl-names-list '("\\*e*shell\\*"
                           "\\*.*REPL.*\\*"
                           "\\*MATLAB\\*"
                           "\\*Python\\*"
                           "\\*Inferior .*\\*$"
                           "\\*ielm\\*")
  "List of buffer names used in REPL buffers")

(defvar +help-modes-list '(helpful-mode
                           help-mode
                           man-mode
                           woman-mode
                           Info-mode
                           pydoc-mode
                           eww-mode)
  "List of major-modes used in documentation buffers")

(setq display-buffer-alist
      '(
        ;; Windows on top

        ("\\*Buffer List\\*" (display-buffer-in-side-window)
         (side . top)
         (slot . 0)
         (window-height . fit-window-to-buffer)
         (preserve-size . (nil . t)))

        ((lambda (buf act) (member (buffer-mode buf) +occur-grep-modes-list))
         (fit-buffer-in-side-window)
         ;; (window-height . 20)
         (side . top)
         (slot . -1)
         (preserve-size . (nil . t))
         (window-parameters . ((mode-line-format . (" " mode-line-buffer-identification)))))

        ("\\*\\(Flycheck\\|Package-Lint\\).*" (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))

        ;; Windows on the side

        ((lambda (buf act) (eq (buffer-mode buf) 'matlab-shell-help-mode))
         (display-buffer-in-side-window)
         (window-width . 81)
         (side . right)
         (slot . 0)
         (window-parameters . (;; (no-other-window . t)
                               (mode-line-format . (" "
                                                    mode-line-buffer-identification)))))

        ((lambda (buf act) (member (buffer-mode buf) +help-modes-list))
         (display-buffer-in-side-window)
         (window-width . 76)       ; See the :hook
         (side . right)
         (slot . 0)
         (window-parameters . (;; (no-other-window . t)
                               (mode-line-format . (" "
                                                    mode-line-buffer-identification)))))

        ("^\\*\\(?:Wo\\)?Man"
         (display-buffer-in-side-window)
         (window-width . 76)       ; See the :hook
         (side . right)
         (slot . 0)
         (window-parameters . (;; (no-other-window . t)
                               (mode-line-format . (" "
                                                    mode-line-buffer-identification)))))

        ("\\*Faces\\*" (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . -2)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . (" "
                                                    mode-line-buffer-identification)))))

        ("\\*Custom.*" (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . 1))

        ;; Windows at the bottom
        ("\\*scratch\\*"
         select-buffer-in-side-window
         (window-width 35)
         (side . bottom)
         (slot . 0))

        ("\\*\\(Warnings\\|Compile-Log\\|Messages\\)\\*" display-buffer-in-side-window
         (window-height . 0.20)
         (side . bottom)
         (slot . 0)
         (preserve-size . (nil . t))
         (window-parameters . ((no-other-window . t))))

        ("\\*Backtrace\\*" (display-buffer-in-side-window)
         (window-height . 0.20)
         (side . bottom)
         (slot . -2)
         (preserve-size . (nil . t)))

        ("\\*\\(Output\\|Register Preview\\).*" (display-buffer-in-side-window)
         (window-height . 0.16)       ; See the :hook
         (side . bottom)
         (slot . -2)
         (window-parameters . ((no-other-window . t))))

        ("\\*\\(?:Completions\\|Apropos\\)\\*" (display-buffer-in-side-window)
         (window-height . 0.20)
         (side . bottom)
         (slot . -1)
         (window-parameters . ((no-other-window . t))))

        ((lambda (buf act) (seq-some (lambda (regex) (string-match-p regex buf))
                                     +repl-names-list))
         select-buffer-in-side-window
         (window-height . .25)
         (side . bottom)
         (slot . 1)
         (preserve-size . (nil . t)))

        ;; ("\\*[Ss]hell\\*"
        ;;  select-buffer-in-side-window
        ;;  (window-width . 40)
        ;;  (side . left)
        ;;  (slot . -1)
        ;;  (preserve-size . (nil . t))
        ;;  )

        ))

(setq window-combination-resize t
      even-window-sizes 'height-only
      window-sides-vertical nil
      fit-window-to-buffer-horizontally t
      window-resize-pixelwise t)

;;;###autoload
(defun +make-frame-floating-with-current-buffer ()
  "Display the current buffer in a new floating frame.

This passes certain parameters to the newly created frame:

- use a different name than the default;
- use a graphical frame;
- do not display the minibuffer.

The name is meant to be used by the external rules of a tiling
window manager to present the frame in a floating state."
  (interactive)
  (let ((buf (current-buffer)))
    (delete-window)
    (make-frame '((name . "dropdown_emacs-buffer")
                  (window-system . x)
                  (minibuffer . nil)))
    (switch-to-buffer buf)))

;;;###autoload
(defun +display-buffer-at-bottom ()
  "Move the current buffer to the bottom of the frame.  This is
useful to take a buffer out of a side window.

The window parameters of this function are provided mostly for
didactic purposes."
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (delete-window)
      (display-buffer-at-bottom
       buffer `((window-parameters . ((mode-line-format . (" "
                                                           mode-line-buffer-identification)))))))))

(provide 'setup-windows)
