;;; POPUP-BUFFERS.EL - Designate buffers as popups to summon or dismiss easily.
;;;
;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; For a full copy of the GNU General Public License
;;; see <http://www.gnu.org/licenses/>.
;;;
;;; COMMENTARY:
;;; 
;;; A hacked together solution to handle annoying popups in Emacs. Only works
;;; well in conjunction with some system to handle window creation and
;;; placement, like shackle.el. This plugin summons windows defined by
;;; the user as "popups" by simply calling `display-buffer'.
;;;
;;; COMMANDS:
;;;
;;; popup-buffers-toggle-latest : Toggle latest popup
;;; popup-buffers-cycle         : Cycle through all popups, or close all open popups
;;; popup-buffers-open-latest   : Open latest popup
;;; popup-buffers-close-latest  : Close latest popup
;;;
;;; CUSTOMIZATION:
;;;
;;; `popup-buffers-reference-buffers': A list of major modes or regexps whose
;;; corresponding buffer major-modes or regexps (respectively) should be treated
;;; as popups.
;;;
;;; TODO: Handle buffer deletion carefully. Killed buffers should be
;;; wiped from popup-buffers-open-buffer-window-alist, by hooking into
;;; kill-buffer-hook
;;;
;;; TODO: Add popup list maintenance to `make-frame-finish-functions',
;;; (add-hook 'after-make-frame-functions 'popup-buffers-update-open-popups)
;;;
;;; by Karthik Chikmagalur <karthik.chikmagalur@gmail.com>

(require 'cl-seq)

(defgroup popup-buffers nil
  "Provide functions for easy access to popup windows"
  :group 'convenience)

(defcustom popup-buffers-reference-buffers '("*Messages*$")
  "List of buffers to treat as popups. Each entry in the list can be a regexp (string) to match buffer names against, or a major-mode (symbol) to match buffer major-modes against.
Example: 

'(\"*Messages*\" \"Output*$\" help-mode compilation-mode)

Will match against the Messages buffer, any buffer ending in Output*, and all help and compilation buffers."
  :type '(restricted-sexp :match-alternatives (stringp symbolp))
  :group 'popup-buffers)

(defvar popup-buffers-reference-names nil
  "List of buffer names whose windows are treated as popups.")

(defvar popup-buffers-reference-modes nil
 "List of buffer major-modes whose buffers are treated as popups.")

(defvar popup-buffers--user-buffers-alist nil
  "Alist of buffers designated by the user as popups.")

(defvar popup-buffers-open-buffer-window-alist nil
  "Alist of currently live (window . buffer)s that are treated as popups.")

(defvar popup-buffers-buried-buffer-window-alist nil
  "Alist of currently buried (window . buffer)s that are treated as popups.")

(defvar popup-buffers--toggle-state nil
  "Current state of latest popup. Aternates between nil and t.")

;;;###autoload
(defun +display-popup-in-side-window (buffer &optional alist)
  "Display a popup-buffer at the bottom of the screen in a side
window without switching to it."
  (display-buffer-in-side-window
   buffer
   '((side . bottom)
    (slot . 1)
    (window-height . (lambda (win) (fit-window-to-buffer win (/ (frame-height) 3))))
    (window-parameters . ((mode-line-format . (:eval (+helper-window-mode-line-format))))))
   ))

;;;###autoload
(defun popup-buffers-find-open-popups ()
  "Return an alist of (window . buffer) corresponding to open popup buffers."
  ;; TODO: use buffer-display-time to get timestamps
  (let* ((open-buffers (mapcar #'window-buffer (window-list)))
         open-popups)
    (dolist (b open-buffers open-popups)
      (if (or (seq-some (lambda (buf-regexp)
                          (string-match-p buf-regexp (buffer-name b)))
                        popup-buffers-reference-names)
              (member (buffer-local-value 'major-mode b) popup-buffers-reference-modes)
              (member b popup-buffers--user-buffers-alist))
          ;; (add-to-list 'open-popups (cons (get-buffer-window b) b))
          (push (cons (get-buffer-window b) b)
                open-popups)))))

;;;###autoload
(defun popup-buffers-update-open-popups ()
  "Update the list of currently open popups."
  (setq popup-buffers--user-buffers-alist
        (cl-delete-if-not (lambda (buf) (buffer-live-p buf))
                       popup-buffers--user-buffers-alist))
  (setq popup-buffers-open-buffer-window-alist
        (nreverse (popup-buffers-find-open-popups))))

;;;###autoload
(defun popup-buffers-find-buried-popups ()
  "Return an alist of (window . buffer) corresponding to buried
popup buffers."
  (let* ((open-buffers (mapcar #'window-buffer (window-list)))
         buried-popups)
    (dolist (b (cl-set-difference (buffer-list) open-buffers) buried-popups)
     (if (or (seq-some (lambda (buf-regexp)
                          (string-match-p buf-regexp (buffer-name b)))
                        popup-buffers-reference-names)
              (member (buffer-local-value 'major-mode b) popup-buffers-reference-modes)
              (member b popup-buffers--user-buffers-alist))
         (push (cons (get-buffer-window b) b)
               buried-popups)))))

;;;###autoload
(defun popup-buffers-update-buried-popups ()
  "Update the list of currently buried popups. Currently only
called when enabling minor-mode `popup-buffers-mode'."
  (setq popup-buffers-buried-buffer-window-alist
        (popup-buffers-find-buried-popups)))

;;;###autoload
(defun popup-buffers-close-latest ()
  "Close the last opened popup."
  (interactive "P")
  (unless (null popup-buffers-open-buffer-window-alist)
    (cl-destructuring-bind ((win . buf) . rest) popup-buffers-open-buffer-window-alist
      (when (and (window-valid-p win) (window-parent win))
        ;;only close window when window has a parent:
        (when (not (seq-some
                    (lambda (item) (eq buf (cdr item)))
                    popup-buffers-buried-buffer-window-alist))
          ;; buffer doesn't already exist in the buried popup list
          (push (cons win buf) popup-buffers-buried-buffer-window-alist)
          (pop popup-buffers-open-buffer-window-alist))
        (with-selected-window win
          (bury-buffer buf)
          (delete-window win))))))

;;;###autoload
(defun popup-buffers-open-latest ()
  "Open the last closed popup"
  (interactive)
  (unless (null popup-buffers-buried-buffer-window-alist)
    (let* ((new-popup (pop popup-buffers-buried-buffer-window-alist))
           (buf (cdr new-popup)))
      (if (buffer-live-p buf)
          (display-buffer buf)
        (popup-buffers-open-latest)))))

;;;###autoload
(defun popup-buffers-bury-all ()
  (while popup-buffers-open-buffer-window-alist
    (popup-buffers-close-latest)))

;;;###autoload
(defun popup-buffers-raise-all ()
  (while popup-buffers-buried-buffer-window-alist
    (popup-buffers-open-latest)))

;;;###autoload
(defun popup-buffers-toggle-latest (&optional arg)
  "Toggle visibility of the last opened popup window. With a prefix argument, toggle all popup windows"
  (interactive "P")
  (if popup-buffers-open-buffer-window-alist
      (if arg
          (popup-buffers-bury-all)
        (popup-buffers-close-latest))
    (if popup-buffers--toggle-state
        (progn (setq popup-buffers--toggle-state (not popup-buffers--toggle-state))
               (popup-buffers-close-latest))
      (if arg
          (popup-buffers-raise-all)
        (popup-buffers-open-latest)))))

;;;###autoload
(defun popup-buffers-cycle (&optional arg)
  "Cycle visibility of popup windows one at a time. With a prefix argument ARG, cycle in the opposite direction."
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
      (popup-buffers-bury-all))))

;;;###autoload
(defun popup-buffers-raise-popup (&optional buffer)
  "Raise a popup to regular status. If BUFFER is not specified,
raise the current buffer."
  (interactive)
  (let ((buf (get-buffer (or buffer (current-buffer)))))
    (setq popup-buffers--user-buffers-alist
          (delete buf popup-buffers--user-buffers-alist))
    (popup-buffers-update-open-popups)
    (display-buffer-at-bottom
     buf '((window-height . 0.50)
           (window-parameters . ((mode-line-format . mode-line-format)))))))

;;;###autoload
(defun popup-buffers-lower-to-popup (&optional buffer)
  "Turn a regular window into a popup"
  (interactive)
  (let ((buf (get-buffer (or buffer (current-buffer)))))
    (delete-window (get-buffer-window buf t))
    (+display-popup-in-side-window buf)
    (push buf popup-buffers--user-buffers-alist)
    (popup-buffers-update-open-popups)))

;;;###autoload
(define-minor-mode popup-buffers-mode
  "Toggle Popup Buffers mode. When enabled, treat certain buffer windows as popups, a class of window that can be summoned or dismissed with a command. See the customization options for details on how to designate buffer types as popups."
  :global t
  :version "0.15"
  :lighter ""
  :group 'popup-buffers
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-`") 'popup-buffers-toggle-latest)
	    (define-key map (kbd "M-`") 'popup-buffers-cycle)
	    map)
  (if popup-buffers-mode
      ;; Turning the mode ON
      (progn
        (setq popup-buffers-reference-names
              (cl-remove-if-not #'stringp popup-buffers-reference-buffers)
              popup-buffers-reference-modes
              (cl-remove-if-not #'symbolp popup-buffers-reference-buffers))
        (popup-buffers-update-buried-popups)
        (popup-buffers-update-open-popups)
        (add-hook 'window-configuration-change-hook 'popup-buffers-update-open-popups)
        (add-to-list 'display-buffer-alist
                     '((lambda (buf act) (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
                                      (member buffer popup-buffers--user-buffers-alist)))
                       (+display-popup-in-side-window))))
    ;; Turning the mode OFF
    (remove-hook 'window-configuration-change-hook 'popup-buffers-update-open-popups)
    (delete
     '((lambda (buf act) (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
                      (member buffer popup-buffers--user-buffers-alist)))
       (+display-popup-in-side-window))
     display-buffer-alist)))

(provide 'popup-buffers)
