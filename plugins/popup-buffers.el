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

(defvar popup-buffers-reference-buffer-list
  '("^\\*Help\\*"
    "^\\*helpful"
    "^\\*Warnings\\*"
    "^\\*Compile-Log\\*"
    "^\\*Messages\\*"
    "^\\*Backtrace\\*"
    "^\\*evil-registers\\*"
    "^\\*Apropos"
    "^Calc:"
    "^\\*ielm\\*"
    "^\\*TeX Help\\*"
    "^\\*eshell\\*"
    "\\*Shell Command Output\\*"
    "\\*Matlab Help\\*"
    "\\*Completions\\*"
    "Output\\*"
    ;; "^\\*"
)
  "List of buffer names whose windows are treated as popups.")

;; (setq popup-buffers-reference-buffer-list
;;         '("^\\*Help\\*"
;;           "^\\*helpful"
;;           "^\\*Warnings\\*"
;;           "^\\*Compile-Log\\*"
;;           "^\\*Messages\\*"
;;           "^\\*Backtrace\\*"
;;           "^\\*evil-registers\\*"
;;           "^\\*Apropos"
;;           "^Calc:"))

(defvar popup-buffers-open-buffer-window-alist '()
  "Alist of currently live (window . buffer)s that are treated as popups")

(defvar popup-buffers-buried-buffer-window-alist nil
  "Alist of currently buried (window . buffer)s that are treated as popups")

(defvar popup-buffers--toggle-state nil
  "Current state of latest popup. Aternates between nil and t")
(defvar popup-buffers--cycle-state nil
  "Current state of popup cycling. Aternates between nil and t")

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
      (if (seq-some (lambda (buf-regexp)
                      (string-match-p buf-regexp (buffer-name b)))
                    popup-buffers-reference-buffer-list)
          (push (cons (get-buffer-window b) b)
                open-popups)))))

(defun popup-buffers-update-open-popups ()
  (setq popup-buffers-open-buffer-window-alist
        (popup-buffers-find-open-popups)))

;;; HOOKS
;; Add popup list maintenance to `window-configuration-change-hook',
(add-hook 'window-configuration-change-hook 'popup-buffers-update-open-popups)

;; Add popup list maintenance to `make-frame-finish-functions',
(add-hook 'after-make-frame-functions 'popup-buffers-update-open-popups)

;; TODO Remove current buffer from the list of buried popups (will be added to `kill-buffer-hook')

;; (defun popup-buffers-update-killed-popups ()
;;   )
;; (add-hook 'kill-buffer-hook 'popup-buffers-update-killed-popups)popup-buffers-close-latest(defun popup-buffers-close-latest ()

(defun popup-buffers-close-latest () 
  "Close the last opened popup"
  (interactive)
  (unless (null popup-buffers-open-buffer-window-alist)
    (let ((latest-popup (car popup-buffers-open-buffer-window-alist)))
      (push latest-popup popup-buffers-buried-buffer-window-alist)
      (with-selected-window (car latest-popup)
        (bury-buffer)
        (delete-window)))))

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
  (setq popup-buffers--toggle-state (not popup-buffers--toggle-state))
  (if popup-buffers-open-buffer-window-alist
      (popup-buffers-close-latest)
    (if popup-buffers--toggle-state
        (popup-buffers-close-latest)
      (popup-buffers-open-latest))))

;;;###autoload
(defun popup-buffers-cycle (&optional arg)
  "Cycle visibility of popup windows one at a time. With a prefix argument, cycle in the opposite direction."
  (interactive "p")
  (if (equal last-command 'popup-buffers-cycle)
      ;; cycle through buffers: rest of logic
      (progn (popup-buffers-close-latest)
             (let ((bufs popup-buffers-buried-buffer-window-alist)) 
               (setq popup-buffers-buried-buffer-window-alist
                     (append (cdr bufs) (cons (car bufs) ())))
               (popup-buffers-open-latest)))
    ;; starting new cycle, so bury everything first.
    (if (null popup-buffers-open-buffer-window-alist)
        (popup-buffers-open-latest)
      (popup-buffers-bury-all))
      )
  )

(provide 'popup-buffers)
