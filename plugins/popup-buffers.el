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
;;; popup-buffers-latest-close
;;; popup-buffers-latest-open
;;; popup-buffers-toggle-latest
;;;
;;;
;;; by Karthik Chikmagalur

(defvar popup-buffers-reference-buffer-list
  '("^\\*Help\\*"
    "^\\*helpful"
    "^\\*Warnings\\*"
    "^\\*Compile-Log\\*"
    "^\\*Messages\\*"
    "^\\*Backtrace\\*"
    "^\\*evil-registers\\*")
  "List of buffer names whose windows are treated as popups.")

(defvar popup-buffers-open-buffer-window-alist '()
  "List of currently open (window . buffer)s that are treated as popups")

(defvar popup-buffers-buried-buffer-window-alist nil
  "List of currently buried (window . buffer)s that are treated as popups")

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

(defun popup-buffers-find-open-popups ()
  "Find open popup windows in the frame and TODO make a list sorting them by active time"
  (let* ((open-windows (window-list))
         (open-buffers (mapcar #'window-buffer open-windows))
         (open-popups nil))
    (dolist (b open-buffers open-popups)
      (if (seq-some (lambda (buf-regexp)
                      (string-match-p buf-regexp (buffer-name b)))
                    popup-buffers-reference-buffer-list)
          (push (cons (get-buffer-window b) b)
                open-popups)))))

(defun popup-buffers-update-open-popups ()
  (setq popup-buffers-open-buffer-window-alist
        (popup-buffers-find-open-popups)))

;; Add popup maintenance to `window-configuration-change-hook',
(add-hook 'window-configuration-change-hook 'popup-buffers-update-open-popups)

;; TODO Remove current buffer from the list of buried popups (will be added to `kill-buffer-hook')

;; (defun popup-buffers-update-killed-popups ()
;;   )
;; (add-hook 'kill-buffer-hook 'popup-buffers-update-killed-popups)

;;; COMMANDS:

(defun popup-buffers-latest-close ()
  "Close the last opened popup"
  (interactive)
  (unless (null popup-buffers-open-buffer-window-alist)
    (let ((latest-popup (car popup-buffers-open-buffer-window-alist)))
      (push latest-popup popup-buffers-buried-buffer-window-alist)
      (with-selected-window (car latest-popup)
        (bury-buffer)
        (delete-window)))))

(defun popup-buffers-latest-open ()
  "Open the last closed popup"
  (interactive)
  (unless (null popup-buffers-buried-buffer-window-alist)
    (let ((new-popup (pop popup-buffers-buried-buffer-window-alist)))
      (display-buffer (cdr new-popup)))
    ))

(defun popup-buffers-latest-toggle ()
  "Toggle visibility of the last opened popup window"
  (interactive)
  (setq popup-buffers--toggle-state (not popup-buffers--toggle-state))
  (if popup-buffers--toggle-state
      (popup-buffers-latest-open)
    (popup-buffers-latest-close)))

(provide 'popup-buffers)
