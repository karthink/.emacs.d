;; Window helpers for "Readers".  -*- lexical-binding: t; -*-
(defun my/reader-display-buffer (buf &optional act)
  (pop-to-buffer buf `((display-buffer-reuse-window display-buffer-in-direction)
                       (direction . ,(if (> (window-width) 130)
                                         'right 'above))
                       (window-height . 0.72)
                       (window-width . 0.64))))

(defvar my/reader-names '("*elfeed-entry*" "*wallabag-entry*"))
(defvar my/reader-modes '(elfeed-show-mode wallabag-entry-mode eww-mode))
(defvar my/reader-list-modes '(elfeed-search-mode wallabag-search-mode))

(defvar my/reader-quit-functions
  (cl-pairlis my/reader-modes
              '(kill-buffer-and-window
                wallabag-entry-quit
                quit-window)))

(defvar my/reader-list-quit-functions
  (cl-pairlis my/reader-list-modes
              '(elfeed-search-quit-window
                wallabag-search-quit
                quit-window)))

(defvar my/reader-list-next-prev-functions
  (cl-pairlis my/reader-list-modes
              '((next-line . previous-line)
               (wallabag-next-entry . wallabag-previous-entry))))

(defvar my/reader-list-show-functions
  (cl-pairlis my/reader-list-modes
              '(elfeed-search-show-entry
                wallabag-view)))

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

(defun my/reader-scroll-up-command ()
  (interactive)
  (let ((scroll-error-top-bottom nil))
    (unless
        (with-reader-window
          (condition-case-unless-debug nil
              (scroll-up-command)
            (error (run-at-time 0 nil #'my/reader-next 1))))
      (my/reader-show))))

(defun my/reader-scroll-down-command ()
  (interactive)
  (let ((scroll-error-top-bottom nil))
    (with-reader-window
      (condition-case-unless-debug nil
          (scroll-down-command)
        (error (run-at-time 0 nil #'my/reader-prev 1))))))

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
  (with-reader-window (beginning-of-buffer)))

(defun my/reader-bottom ()
  (interactive)
  (with-reader-window (end-of-buffer)))

(defun my/reader-quit-window ()
    (interactive)
    (or (with-reader-window
          (when-let ((quit-fun (alist-get (buffer-mode) my/reader-quit-functions)))
            (funcall quit-fun)
            t))
        (call-interactively (alist-get (buffer-mode) my/reader-list-quit-functions))))

(provide 'setup-reading)
