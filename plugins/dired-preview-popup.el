;;; dired-preview-popup.el --- See dired-preview previews in popups  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur


;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (dired-preview "0.6.0"))
;; Keywords: convenience, multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use `dired-preview' with previews in a child-frame.  I could never find a
;; place to put the preview on the frame that wouldn't annoy me, so I went this
;; route.

;;; Code:
(require 'dired-preview)
(eval-when-compile (require 'cl-lib))

(defcustom dired-preview-popup-width 0.45
  "Fraction of frame width occupied by popup."
  :type 'float
  :group 'dired-preview-popup)

(defcustom dired-preview-popup-height 0.88
  "Fraction of frame height occupied by popup."
  :type 'float
  :group 'dired-preview-popup)

(defun dired-preview-popup--action-alist ()
  "Return a `display-buffer' action-alist to view Dired previews in a popup."
  (let* ((parent (window-frame))
         (fw (frame-width))
         (fph (frame-pixel-height))
         (fpw (frame-pixel-width))
         ;; window geometry
         (wpw-char (floor fpw fw))
         (wph-char (line-pixel-height))
         ;; popup geometry
         (pixel-width (round (* dired-preview-popup-width fpw)))
         (pixel-height (round (* dired-preview-popup-height fph)))
         (margin (floor (- fph pixel-height) 2))
         xpos ypos)
    (let ((x (+ (car (posn-x-y (posn-at-point)))
                (car (window-inside-pixel-edges)))))
      (if (< x (floor fpw 2))           ;cursor is in left half of the screen
          (setq xpos (- margin))        ;Show child frame on right side
        (setq xpos margin))             ;Else on left side
      (setq ypos margin))               ;Center vertically
    
    `((display-buffer-use-some-frame display-buffer-in-child-frame)
      (body-function . ,(lambda (w)
                          (let* ((fr (window-frame w)))
                            (when-let* ((parent (frame-parent fr)))
                              (unless (frame-parameter fr 'dired-preview)
                                (set-frame-parameter fr 'dired-preview t))
                              (set-frame-position (window-frame w) xpos ypos)
                              (redirect-frame-focus fr parent)
                              (with-current-buffer (window-buffer w)
                                (setq tab-line-format nil truncate-lines t))
                              (select-frame-set-input-focus parent)))))
      (frame-predicate . ,#'frame-parent)
      (reusable-frames . visible)
      (child-frame-parameters
       (minibuffer . ,(minibuffer-window parent))
       (width . ,(floor pixel-width wpw-char))
       (height . ,(floor pixel-height wph-char))
       (border-width . 0) (undecorated . t)
       (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
       (tab-bar-lines-keep-state . t) (no-other-frame . t)
       (unsplittable . t) (desktop-dont-save . t)
       (inhibit-double-buffering . t)))))

(defun dired-preview-popup--get-windows ()
  "Get all `dired-previews' of window."
  (seq-filter #'dired-preview--window-parameter-p
              (cl-loop for fr in (visible-frame-list)
                       nconc (window-list fr))))

(defun dired-preview-popup-display-file (file)
  "Display preview of FILE if appropriate.

Display in a child-frame."
  (dired-preview--delete-windows)
  (when-let* ((buffer (dired-preview--get-preview-buffer file)))
    (let ((display-buffer-alist)        ;dangerous but necessary
          (inhibit-redisplay t)
          (x-fast-protocol-requests t)
          (x-gtk-resize-child-frames nil)
          (before-make-frame-hook)
          (after-make-frame-functions)
          (window-min-height 10)
          (window-min-width 20))
      (dired-preview--display-buffer buffer))
    (dired-preview--rename-buffer buffer)
    (when-let* ((window (get-buffer-window buffer 'visible)))
      (dired-preview--set-window-parameters window t)
      (run-hooks 'dired-preview-hook))))


(defun dired-preview-popup--hide-previews ()
  "Currently unused."
  (mapc #'make-frame-invisible
        (seq-filter (lambda (fr) (and (eq (frame-parent fr) (selected-frame))
                                 (frame-parameter fr 'dired-preview)))
                    (frame-list))))

(defun dired-preview-popup--close-previews ()
  "Close all child-frame previews."
  (mapc #'delete-frame
        (seq-filter (lambda (fr) (and (eq (frame-parent fr) (selected-frame))
                                 (frame-parameter fr 'dired-preview)))
                    (frame-list))))

;;;###autoload
(define-minor-mode dired-preview-popup-mode
  "Minor-mode to preview `dired-preview' previews in a popup.

I can never find a place to put the previews in the frame."
  :lighter ""
  (cond
   (dired-preview-popup-mode
    (setq dired-preview-display-action-alist #'dired-preview-popup--action-alist)
    (advice-add 'dired-preview--close-previews :after #'dired-preview-popup--close-previews)
    (advice-add 'dired-preview--delete-windows :override #'ignore)
    (advice-add 'dired-preview-display-file :override #'dired-preview-popup-display-file)
    (advice-add 'dired-preview--get-windows :override #'dired-preview-popup--get-windows))
   (t (setq dired-preview-display-action-alist #'dired-preview-display-action-alist-dwim)
      (advice-remove 'dired-preview--close-previews #'dired-preview-popup--close-previews)
      (advice-remove 'dired-preview--delete-windows #'ignore)
      (advice-remove 'dired-preview-display-file #'dired-preview-popup-display-file)
      (advice-remove 'dired-preview--get-windows #'dired-preview-popup--get-windows))))


(provide 'dired-preview-popup)
;;; dired-preview-popup.el ends here
