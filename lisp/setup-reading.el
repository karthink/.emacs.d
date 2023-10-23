;; Window helpers for "Readers".  -*- lexical-binding: t; -*-

(defvar wallabag-show-entry-switch)

(defun my/reader-display-buffer (buf &optional _)
  (pop-to-buffer buf `((display-buffer-reuse-window display-buffer-in-direction)
                       (direction . ,(if (> (window-width) 130)
                                         'right 'above))
                       (window-height . 0.72)
                       (window-width . 0.64))))

(defvar my/reader-names '("*elfeed-entry*" "*wallabag-entry*"))
(defvar my/reader-modes '(elfeed-show-mode wallabag-show-mode eww-mode))
(defvar my/reader-list-modes '(elfeed-search-mode wallabag-search-mode))

(defvar my/reader-quit-functions
  (cl-pairlis my/reader-modes
              '(kill-buffer-and-window
                wallabag-show-quit-window
                quit-window)))

(defvar my/reader-list-quit-functions
  (cl-pairlis my/reader-list-modes
              '(elfeed-search-quit-window
                wallabag-search-quit-window
                quit-window)))

(defvar my/reader-list-next-prev-functions
  (cl-pairlis my/reader-list-modes
              '((next-line . previous-line)
               (next-line . previous-line))))

(defvar my/reader-list-show-functions
  (cl-pairlis my/reader-list-modes
              '(elfeed-search-show-entry
                wallabag-search-show-entry)))

(defun my/reader-show ()
  (interactive)
  (let ((elfeed-show-entry-switch #'my/reader-display-buffer)
        (wallabag-show-entry-switch #'my/reader-display-buffer)
        (win (selected-window))
        (show-buf))
    (save-excursion
      (call-interactively (alist-get (buffer-mode) my/reader-list-show-functions)))
    (setq show-buf (current-buffer))
    (select-window win)
    (setq-local other-window-scroll-buffer show-buf)))

(defun my/reader-prev (&optional arg)
  (interactive "p")
  (unless (bobp)
    (when-let ((prev-fun (cdr (alist-get (buffer-mode) my/reader-list-next-prev-functions))))
      (dotimes (or arg 1)
        (funcall prev-fun))
      (my/reader-show))))

(defun my/reader-next (&optional arg)
  (interactive "p")
  (unless (eobp)
    (when-let ((next-fun (car (alist-get (buffer-mode) my/reader-list-next-prev-functions))))
      (dotimes (or arg 1)
        (funcall next-fun))
      (my/reader-show))))

(eval-when-compile
    (defmacro with-reader-window (&rest body)
      "Execute BODY with a visible elfeed entry buffer as current."
      (declare (indent defun))
      `(when-let ((win
                   (or
                    (seq-some #'get-buffer-window my/reader-names)
                    (seq-some (lambda (w)
                                (and (memq
                                      (buffer-mode (window-buffer w))
                                      my/reader-modes)
                                 w))
                     (window-list)))))
        (with-selected-window win
         ,@body)
        t)))

(defvar my/reader-pixel-scroll nil
  "Whether reader-mode scrolling should be animated.")

(defun my/reader-scroll-up-command ()
  (interactive)
  (let ((scroll-error-top-bottom nil)
        (pulse-flag))
    (unless
        (with-reader-window
          (if my/reader-pixel-scroll
              (progn
                (pixel-scroll-precision-interpolate
                 (- (* (default-line-height) next-screen-context-lines)
                    (window-text-height nil t))
                 nil 1)
                (my/pulse-momentary-line))
            (condition-case-unless-debug nil
                (scroll-up-command)
              (error (run-at-time 0 nil #'my/reader-next 1)))))
      (my/reader-show))))

(defun my/reader-scroll-down-command ()
  (interactive)
  (let ((scroll-error-top-bottom nil)
        (pulse-flag))
    (with-reader-window
      (if my/reader-pixel-scroll
          (progn
            (pixel-scroll-precision-interpolate
             (- (window-text-height nil t)
                (* (default-line-height) next-screen-context-lines))
             nil 1)
            (my/pulse-momentary-line))
        (condition-case-unless-debug nil
            (scroll-down-command)
          (error (run-at-time 0 nil #'my/reader-prev 1)))))))

(defun my/reader-browse-url (&optional arg)
  "docstring"
  (interactive "P")
  (with-reader-window (my/search-occur-browse-url arg)))

(defun my/reader-push-button (&optional arg)
  "docstring"
  (interactive)
  (with-reader-window (my/avy-link-hint (selected-window))))

(defun my/reader-imenu ()
  (interactive)
  (with-reader-window (consult-imenu)))

(defun my/reader-top ()
  (interactive)
  (or 
   (with-reader-window (beginning-of-buffer))
   (beginning-of-buffer)))

(defun my/reader-bottom ()
  (interactive)
  (or 
   (with-reader-window (end-of-buffer))
   (end-of-buffer))
  (when (eq (line-beginning-position) (point))
    (forward-line -1)))

(defun my/reader-quit-window ()
    (interactive)
    (or (with-reader-window
          (when-let ((quit-fun (alist-get (buffer-mode) my/reader-quit-functions)))
            (call-interactively quit-fun)
            t))
        (call-interactively (alist-get (buffer-mode) my/reader-list-quit-functions))))

(defun my/reader-center-images ()
    "Center images in document. Meant to be added to a post-render
hook."
    (let* ((inhibit-read-only t)
           (pixel-buffer-width (shr-pixel-buffer-width))
           match)
      (save-excursion
        (goto-char (point-min))
        (while (setq match (text-property-search-forward
                            'display nil
                            ;; (lambda (_ p) (eq (car-safe p) 'image))
                            (lambda (_ p) (image-at-point-p))))
          (when-let ((size (car (image-size
                                 (prop-match-value match) 'pixels)))
                     ((> size 150))
                     (ov (make-overlay (1- (prop-match-beginning match))
                          (prop-match-beginning match))))
            (overlay-put
             ov 'after-string
             (propertize 
              " " 'face 'default
              'display `(space :align-to (- center (0.58 . ,(prop-match-value match)))))))))))

(provide 'setup-reading)
