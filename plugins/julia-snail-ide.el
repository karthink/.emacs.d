(require 'julia-snail)
(declare-function my/display-buffer-reuse-minor-mode-window "setup-windows")

(defun julia-snail-ide--org-src-setup ()
  (when (eq major-mode 'julia-mode)
    (setq-local org-src-window-setup 'current-window)))

(defvar julia-snail-ide--orig-disp-buf nil)
(defvar julia-snail-ide--orig-win-conf nil)
(defvar julia-snail-ide-orientation 'right-to-left
  "Right to left to have the source window on the right.")
(define-minor-mode julia-snail-ide-mode
  "Set up an IDE-style window arrangement for a Julia buffer."
  :global nil
  (if julia-snail-ide-mode
      (progn
        ;; Check if julia-snail is available
        (or (featurep 'julia-snail) (user-error "Julia Snail is not available."))
        ;; Save window state
        (setq-local julia-snail-ide--orig-win-conf (current-window-configuration))
        
        ;; Save display-buffer-alist entries
        (dolist (id '("^\\*julia\\* mm" "^\\*julia\\* documentation"))
          (push (cons id 
                      (alist-get id display-buffer-alist nil nil #'equal))
                julia-snail-ide--orig-disp-buf))
        ;; org-specific
        ;; (when (eq major-mode 'org-mode)
        ;;   (setq-local org-src-window-setup 'current-window)
        ;;   (add-hook 'org-src-mode-hook #'julia-snail-ide--org-src-setup))
        ;; Set up display-buffer-alist
        (setf
         (alist-get "^\\*julia\\*" display-buffer-alist nil nil #'equal)
         '((display-buffer-reuse-window
            ;; display-buffer-in-atom-window
            display-buffer-in-direction
            ;; display-buffer-in-side-window
            )
           (body-function . select-window)
           (window-height . .35)
           (window-width .  .40)
           (direction . below)
           ;; (side . bottom)
           ;; (slot . 1)
           ))

        (setf
         (alist-get "^\\*julia\\* mm" display-buffer-alist nil nil #'equal)
         `((display-buffer-reuse-window
            my/display-buffer-reuse-minor-mode-window
            display-buffer-in-direction)
           (body-function . julia-snail-ide--tab-line)
           (minor-mode . julia-snail-multimedia-buffer-mode)
           (window-height . .40)
           (window-width .  .40)
           (dedicated . julia-mm)
           (direction . right)))

        (setf
         (alist-get "^\\*julia\\* documentation" display-buffer-alist nil nil #'equal)
         `((display-buffer-reuse-window
            my/display-buffer-reuse-minor-mode-window
            display-buffer-in-direction)
           (body-function . julia-snail-ide--tab-line)
           (minor-mode . (julia-snail-message-buffer-mode gptel-mode))
           (side . right)
           (window-height . .35)
           (window-width .  .40)
           (dedicated . julia-doc)
           (direction . right)))
        
        ;; Set up window arrangement
        (julia-snail-ide-setup (current-buffer)))
    ;; Restore state
    ;;; org-mode specific
    ;; (remove-hook 'org-src-mode-hook #'julia-snail-ide--org-src-setup)
    ;; (when (eq major-mode 'org-mode) (kill-local-variable 'org-src-window-setup))
    
    ;; General
    (when-let* ((win (window-atom-root (selected-window)))
                (_   (window-parameter win 'window-atom)))
      (set-window-parameter win 'window-atom nil))
    (delete-other-windows)
    
    ;; Restore window conf
    (when (window-configuration-p julia-snail-ide--orig-win-conf)
      (set-window-configuration julia-snail-ide--orig-win-conf))
    
    (dolist (id '("^\\*julia\\* mm" "^\\*julia\\* documentation"))
      (setf (alist-get id display-buffer-alist nil t #'equal)
            (alist-get id julia-snail-ide--orig-disp-buf nil t #'equal)))
    (setq julia-snail-ide--orig-disp-buf nil)))

(defun julia-snail-ide-setup (&optional buf)
  (interactive "b")
  (let* ((all-bufs (buffer-list))
         (buf-window)
         (doc-buffer
          (or (cl-some (lambda (b) (and (buffer-local-value 'julia-snail-message-buffer-mode b)
                                   b))
                       all-bufs)
              (with-current-buffer (get-buffer-create "*julia* documentation: -")
                (julia-snail-message-buffer-mode 1)
                (current-buffer))))
         (mm-buffer
          (or (cl-some (lambda (b) (and (buffer-local-value 'julia-snail-multimedia-buffer-mode b)
                                   b))
                       all-bufs)
              (with-current-buffer (get-buffer-create "*julia* mm")
                (julia-snail-multimedia-buffer-mode 1)
                (current-buffer))))
         (repl-buffer (save-window-excursion
                        (julia-snail)
                        (current-buffer))))
    ;; Get the main code buffer and show it
    (pop-to-buffer (get-buffer buf))
    (setq buf-window (get-buffer-window buf))
    
    ;; If it already is part of a layout, clear it and regenerate
    (when (window-parameter buf-window 'window-atom)
      (set-window-parameter nil 'window-atom nil))
    (condition-case nil (delete-other-windows) (error t))

    ;; Populate the auxiliary buffers in windows
    ;;; documentation

    (display-buffer-in-side-window
     (get-buffer doc-buffer)
     `((side . ,(if (eq julia-snail-ide-orientation 'right-to-left)
                         'left 'right))
       (slot . -65)
       (window-width . 0.32)
       (window-height . 0.66)
       (preserve-size . (t . nil))
       (dedicated . julia-doc)
       (body-function . julia-snail-ide--tab-line)))
    
    ;; ;; Atom window with multimedia
    ;; (display-buffer-in-direction
    ;;  (get-buffer doc-buffer)
    ;;  `((direction . ,(if (eq julia-snail-ide-orientation 'right-to-left)
    ;;                      'left 'right))
    ;;    (window-width . 0.35)
    ;;    (preserve-size . (t . nil))
    ;;    (dedicated . julia-doc)
    ;;    (body-function . julia-snail-ide--tab-line)))
    
    ;; ;; Atom window with whole frame
    ;; (display-buffer-in-atom-window
    ;;  (get-buffer doc-buffer)
    ;;  `((window        . ,buf-window)
    ;;    (side          . ,(if (eq julia-snail-ide-orientation 'right-to-left)
    ;;                          'left 'right))
    ;;    (body-function . julia-snail-ide--tab-line)
    ;;    (window-width  . 0.4)
    ;;    (preserve-size . (t . nil))
    ;;    (dedicated     . julia-doc)))
    
    ;; multimedia
    (display-buffer-in-side-window
     (get-buffer mm-buffer)
     `((side . ,(if (eq julia-snail-ide-orientation 'right-to-left)
                         'left 'right))
       (slot . -60)
       (window-width . 0.32)
       (window-height . 0.34)
       (preserve-size . (t . nil))
       (dedicated . julia-mm)
       (body-function . julia-snail-ide--tab-line)))
    
    ;; ;; Atom window
    ;; (display-buffer-in-atom-window
    ;;  (get-buffer mm-buffer)
    ;;  `((window        . ,(get-buffer-window doc-buffer))
    ;;    (side          . below)
    ;;    (window-width  . 0.35)
    ;;    (window-height . 0.4)
    ;;    (body-function . julia-snail-ide--tab-line)
    ;;    (preserve-size . (t . nil))
    ;;    (dedicated     . julia-mm)))

    ;;; repl
    ;; (display-buffer-in-atom-window
    ;;  (get-buffer repl-buffer)
    ;;  `((window        . ,buf-window)
    ;;    (side          . below)
    ;;    (window-height . 0.3)
    ;;    (dedicated     . julia-repl)))
    
    (display-buffer-in-direction
     (get-buffer repl-buffer)
     `((direction . below)
       (window-height . 0.3)))))

(defun julia-snail-ide--tab-line (win)
  (and (window-live-p win)
       (with-current-buffer (window-buffer win)
         (tab-line-mode 1))))


(provide 'julia-snail-ide)
