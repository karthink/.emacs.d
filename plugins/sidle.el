;;; sidle.el --- Framework for list-and-entry reading layouts -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1") (compat "29.1"))

;;; Commentary:
;; A flexible framework for navigating two-pane "list-and-entry" interfaces
;; like RSS readers, email clients, and read-it-later apps.
;;
;; Register a backend with `sidle-register-backend' and use
;; `sidle-display-buffer' in your package's display logic.

;;; Code:

(require 'seq)
(eval-when-compile (require 'cl-lib))
(require 'compat nil t)

(declare-function pixel-scroll-precision-interpolate "pixel-scroll")

(defvar sidle-backends nil
  "Alist of registered sidle backends.
Elements are of the form (NAME . PROPS).")

(defvar sidle-display-buffer-action
  (lambda (_) `((display-buffer-reuse-window display-buffer-in-direction)
           (direction . ,(if (> (window-width) 130) 'right 'above))
           (window-height . 0.72)
           (window-width . 0.64)))
  "Default display action for entry buffers.
Can be overridden per-backend using the `:display-action' property.")

(defvar sidle-pixel-scroll nil
  "Whether sidle scrolling should use `pixel-scroll-precision-interpolate'.")

;;;###autoload
(defun sidle-register-backend (name &rest props)
  "Register a list-and-entry backend under NAME.

PROPS is a plist defining the backend's behavior.  Common keys:
  :list-mode       - Major mode of the list buffer.
  :entry-condition - Condition to match entry buffers (using `buffer-match-p').
  :display-action  - Display action alist, or a function taking BUF that
                     returns one.  Defaults to `sidle-display-buffer-action'.
  :show            - Function to display the entry.
  :next            - Function to go to the next entry in the list.
  :prev            - Function to go to the previous entry in the list.
  :quit-list       - Function to quit the list window.
  :quit-entry      - Function to quit the entry window.
Arbitrary keys can be added for custom behaviors, invoked via `sidle-invoke'."
  (declare (indent 1))
  (setf (alist-get name sidle-backends) props))

(defun sidle--current-backend ()
  "Return the plist of the active sidle backend based on the current buffer."
  (cl-loop for (_ . props) in sidle-backends
           for list-mode = (plist-get props :list-mode)
           for condition = (plist-get props :entry-condition)
           when (or (and list-mode (derived-mode-p list-mode))
                    (and condition (buffer-match-p condition (current-buffer))))
           return props))

(defun sidle-get-entry-window ()
  "Find the visible entry window for the current backend."
  (when-let* ((props (sidle--current-backend))
              (cond (plist-get props :entry-condition)))
    (cl-loop for w in (window-list nil 'no-minibuf)
             when (buffer-match-p cond (window-buffer w))
             return w)))

(defmacro sidle-with-entry-window (&rest body)
  "Execute BODY with the entry window selected.
Returns t if the entry window was found, nil otherwise."
  (declare (indent 0) (debug t))
  `(when-let* ((win (sidle-get-entry-window)))
     (with-selected-window win
       ,@body)
     t))

(defun sidle-invoke (behavior &rest args)
  "Invoke BEHAVIOR (a keyword like :next) for the current backend.
If the configured value is a function, call it with ARGS.
If it is not a function (e.g. static data), return it directly."
  (when-let* ((props (sidle--current-backend))
              (val (plist-get props behavior)))
    (if (functionp val)
        (apply val args)
      val)))

(defun sidle-display-buffer (buf &optional action)
  "Display BUF using backend override, falling back to defaults.
ACTION is an optional standard display action to merge/override."
  (let* ((backend-action (sidle-invoke :display-action buf))
         (final-action (or action backend-action
                           (if (functionp sidle-display-buffer-action)
                               (funcall sidle-display-buffer-action buf)
                             sidle-display-buffer-action))))
    (pop-to-buffer buf final-action)))

(defun sidle-show ()
  "Show the current entry using the active backend."
  (interactive)
  (let ((win (selected-window)))
    (when-let* ((fn (plist-get (sidle--current-backend) :show)))
      (call-interactively fn))
    (select-window win)
    (when-let* ((entry-win (sidle-get-entry-window)))
      (setq-local other-window-scroll-buffer (window-buffer entry-win)))))

(defun sidle-next (&optional arg)
  "Move to the next item ARG times and show it."
  (interactive "p")
  (unless (eobp)
    (when-let* ((fn (plist-get (sidle--current-backend) :next)))
      (dotimes (_ (or arg 1)) (call-interactively fn)))
    (sidle-show)))

(defun sidle-prev (&optional arg)
  "Move to the previous item ARG times and show it."
  (interactive "p")
  (unless (bobp)
    (when-let* ((fn (plist-get (sidle--current-backend) :prev)))
      (dotimes (_ (or arg 1)) (call-interactively fn)))
    (sidle-show)))

(defun sidle-quit (&optional all)
  "Quit the current sidle context.

If the entry window is visible, quits it.  Otherwise quits the list.  If
prefix arg ALL is supplied, quits both."
  (interactive "P")
  (let ((props (sidle--current-backend)))
    (unless (and (sidle-with-entry-window
                   (when-let* ((quit-entry (plist-get props :quit-entry)))
                     (call-interactively quit-entry)))
                 (not all))
      (when-let* ((quit-list (plist-get props :quit-list)))
        (call-interactively quit-list)))))

(defun sidle-scroll-up-command ()
  "Scroll the entry window up, or move to the next item at the end."
  (interactive)
  (let ((scroll-error-top-bottom nil))
    (unless
        (sidle-with-entry-window
          (if sidle-pixel-scroll
              (pixel-scroll-precision-interpolate
               (- (* (default-line-height) next-screen-context-lines)
                  (window-text-height nil t))
               nil 1)
            (condition-case-unless-debug nil
                (scroll-up-command)
              (error (run-at-time 0 nil #'sidle-next 1)))))
      (sidle-show))))

(defun sidle-scroll-down-command ()
  "Scroll the entry window down, or move to the prev item at the start."
  (interactive)
  (let ((scroll-error-top-bottom nil))
    (sidle-with-entry-window
      (if sidle-pixel-scroll
          (pixel-scroll-precision-interpolate
           (- (window-text-height nil t)
              (* (default-line-height) next-screen-context-lines))
           nil 1)
        (condition-case-unless-debug nil
            (scroll-down-command)
          (error (run-at-time 0 nil #'sidle-prev 1)))))))

(defun sidle-top ()
  "Go to the top of the buffer (prefers entry window if visible)."
  (interactive)
  (unless (sidle-with-entry-window (goto-char (point-min)))
    (goto-char (point-min))))

(defun sidle-bottom ()
  "Go to the bottom of the buffer (prefers entry window if visible)."
  (interactive)
  (unless (sidle-with-entry-window (goto-char (point-max)))
    (goto-char (point-max))
    (when (eq (line-beginning-position) (point))
      (forward-line -1))))

;;; FIXME Extras, to be moved out of the package

(declare-function consult-imenu "consult")
(declare-function my/avy-link-hint "setup-avy")
(declare-function my/search-occur-browse-url "setup-isearch")

(defun sidle-imenu ()
  "Open imenu in a side entry window."
  (interactive)
  (sidle-with-entry-window (consult-imenu)))

(defun sidle-browse-url (&optional arg)
  "Browse URL with optional argument in side entry window.

If ARG is true use the generic http browser."
  (interactive "P")
  (sidle-with-entry-window
    (my/search-occur-browse-url arg)))

(defun sidle-push-button (&optional _)
  "Push button with optional argument in side entry window."
  (interactive)
  (sidle-with-entry-window
    (my/avy-link-hint (selected-window))))

(provide 'sidle)
;;; sidle.el ends here
