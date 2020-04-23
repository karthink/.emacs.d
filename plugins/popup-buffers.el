;;; POPUP-BUFFERS.el
;;;
;;; A hacked together solution to handle annoying popups in Emacs.
;;; Only works well in conjunction with some system to handle window
;;; creation and placement, like shackle.el. This plugin only handles
;;; summoning and dismissing windows defined by the user as "popups".
;;;
;;; VARIABLES:
;;; popup-buffers-reference-buffer-list
;;;
;;; A list of regexp strings to match buffer names that you want
;;; classified as a "popup".
;;;
;;; COMMANDS:
;;;
;;; popup-buffers-close-latest
;;; popup-buffers-open-latest
;;; popup-buffers-toggle-latest
;;;
;;; TODO: Handle buffer deletion carefully. Killed buffers should be
;;; wiped from popup-buffers-open-buffer-window-alist, by hooking into
;;; kill-buffer-hook
;;;
;;; TODO: Add support for popup classification based on other
;;; attributes, such as major-mode
;;;
;;; by Karthik Chikmagalur

(defgroup popup nil
  "Provide functions for easy access to popup windows"
  :group 'convenience)

(defcustom popup-buffers-reference-buffer-list nil
  "List of buffer names whose windows are treated as popups."
:type 'editable-list
:group 'popup) 

(defcustom popup-buffers-reference-modes-list nil
 "List of buffer major-modes whose buffers are treated as popups"
  :type 'editable-list
  :group 'popup)

(defvar popup-buffers-open-buffer-window-alist nil
  "Alist of currently live (window . buffer)s that are treated as popups")

(defvar popup-buffers-buried-buffer-window-alist nil
  "Alist of currently buried (window . buffer)s that are treated as popups")

(defvar popup-buffers--toggle-state nil
  "Current state of latest popup. Aternates between nil and t")

;; (defvar popup-buffers-old-window-list nil
;;   "List of displayed windows that is updated when the window configuration changes")

;; (defun popup-buffers-add-window-buffer-to-popups-list ()
;;   "Add newly created buffer to `popup-buffers-open-buffer-window-alist' if it matches any of the buffer names in `popup-buffers-reference-buffer-list'"
;;   (let ((new-buffer-name (buffer-name (car (last (buffer-list))))))
;;     ;; (message "Running popup-buffers-add-window-buffer-to-popups-list")
;;     (if (seq-some (lambda (buf-regexp)
;;                           (string-match-p buf-regexp new-buffer-name))
;;                   popup-buffers-reference-buffer-list)
;;         (progn
;;           ;; (message "Adding new pair to popups list")
;;           (push (cons (get-buffer-window new-buffer-name) (get-buffer new-buffer-name)) 
;;                 popup-buffers-open-buffer-window-alist)))))

;; ;; Start by adding the *Messages* buffer to the state of popup-buffers:
;; (defun popup-buffers-init ()
;;   "Initialize popup-buffers"
;;   (interactive)
;;   ;; Start by adding the *Messages* buffer to the popup-list.
;;   (let* ((messages-buffer (get-buffer "*Messages*"))
;;          (messages-window (get-buffer-window messages-buffer)))
;;     (if messages-buffer
;;         (if messages-window
;;             ;; The messages buffer is being displayed:
;;             (push (cons messages-window messages-buffer)
;;                   popup-buffers-open-buffer-window-alist)
;;           ;; The messages buffer is currently buried:
;;           (save-excursion
;;             (with-current-buffer-window messages-buffer () ()
;;                                         (push (cons (get-buffer-window messages-buffer) . messages-buffer)
;;                                               popup-buffers-buried-buffer-window-alist)))
;;           ))))

;;;###autoload
(defun popup-buffers-find-open-popups ()
  "Find open popup windows in the frame and TODO make a list sorting them by active time"
  ;; use buffer-display-time to get timestamps
  (let* ((open-windows (window-list))
         (open-buffers (mapcar #'window-buffer open-windows))
         (open-popups nil))
    (dolist (b open-buffers open-popups)
      (if (or (seq-some (lambda (buf-regexp)
                          (string-match-p buf-regexp (buffer-name b)))
                        popup-buffers-reference-buffer-list)
              (member (buffer-local-value 'major-mode b) popup-buffers-reference-modes-list))
          (push (cons (get-buffer-window b) b)
                open-popups)))))

;;;###autoload
(defun popup-buffers-update-open-popups ()
  (setq popup-buffers-open-buffer-window-alist
        (popup-buffers-find-open-popups)))

;;;###autoload
(defun popup-buffers-update-closed-popups ()
  (mapcar (lambda (buf)
	    (let ((win (get-buffer-window buf)))
	    (if (and (buffer-live-p buf)
		     (not (window-live-p win))
		     (not (window-minibuffer-p win)))
		buf)))
	  (buffer-list)))

;;; HOOKS
;; Add popup list maintenance to `window-configuration-change-hook',
;; (add-hook 'window-configuration-change-hook 'popup-buffers-update-open-popups)

;; Add popup list maintenance to `make-frame-finish-functions',
;; (add-hook 'after-make-frame-functions 'popup-buffers-update-open-popups)

;; TODO Remove current buffer from the list of buried popups (will be added to `kill-buffer-hook')

;; (defun popup-buffers-update-killed-popups ()
;;   )
;; (add-hook 'kill-buffer-hook 'popup-buffers-update-killed-popups)popup-buffers-close-latest(defun popup-buffers-close-latest ()

(defun popup-buffers-close-latest () 
  "Close the last opened popup"
  (interactive)
  (unless (null popup-buffers-open-buffer-window-alist)
    (let* ((latest-popup (car popup-buffers-open-buffer-window-alist))
           (win (car latest-popup))
           (buf (cdr latest-popup)))
      (when (window-parent win)
        ;;only close window when window has a parent:
        (if (not (seq-some (lambda (item) (eq buf (cdr item)))
                           popup-buffers-buried-buffer-window-alist))
            ;; buffer doesn't already exist in the buried popup list
            (push latest-popup popup-buffers-buried-buffer-window-alist))
        (with-selected-window win
          (bury-buffer buf)
          (delete-window win))))))

(defun popup-buffers-open-latest ()
  "Open the last closed popup"
  (interactive)
  (unless (null popup-buffers-buried-buffer-window-alist)
    (let* ((new-popup (pop popup-buffers-buried-buffer-window-alist))
           (buf (cdr new-popup)))
      (if (buffer-live-p buf)
          (display-buffer buf)
        (popup-buffers-open-latest)))
    ))

(defun popup-buffers-bury-all ()
  (cl-do () ((null popup-buffers-open-buffer-window-alist) t)
    (popup-buffers-close-latest)))

(defun popup-buffers-raise-all ()
  (cl-do () ((null popup-buffers-buried-buffer-window-alist) t)
    (popup-buffers-open-latest)))

;;;###autoload
(defun popup-buffers-toggle-latest ()
  "Toggle visibility of the last opened popup window"
  (interactive)
  (if popup-buffers-open-buffer-window-alist
      (popup-buffers-close-latest)
    (if popup-buffers--toggle-state
        (progn (setq popup-buffers--toggle-state (not popup-buffers--toggle-state))
               (popup-buffers-close-latest))
      (popup-buffers-open-latest))))

;;;###autoload
(defun popup-buffers-cycle (&optional arg)
  "Cycle visibility of popup windows one at a time. With a prefix argument, cycle in the opposite direction."
  (interactive "p")
  ;; (setq popup-buffers--toggle-state
  ;;       (not popup-buffers--toggle-state))
  (if (and (not (null (cdr popup-buffers-buried-buffer-window-alist)))
           (equal last-command 'popup-buffers-cycle))
      ;; cycle through buffers: rest of logic
      (progn (popup-buffers-close-latest)
             (let ((bufs popup-buffers-buried-buffer-window-alist)) 
               (setq popup-buffers-buried-buffer-window-alist
                     (nconc (last bufs) (butlast bufs)))
               (popup-buffers-open-latest)))
    ;; starting new cycle, so bury everything first.
    (if (null popup-buffers-open-buffer-window-alist)
        (popup-buffers-open-latest)
      (progn (popup-buffers-bury-all))
      )))

;;;###autoload
(define-minor-mode popup-buffers-mode
  "To be added"
  :global t
  :version "0.1"
  :lighter " POP "
  :group 'popup
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-`") 'popup-buffers-toggle-latest)
	    (define-key map (kbd "M-`") 'popup-buffers-cycle)
	    map)
  (if popup-buffers-mode
   ;; Turning the mode ON
    (progn
      (popup-buffers-update-open-popups)
      (add-hook 'window-configuration-change-hook 'popup-buffers-update-open-popups))
   ;; Turning the mode OFF
    (remove-hook 'window-configuration-change-hook 'popup-buffers-update-open-popups)
   )
  )

(provide 'popup-buffers)
