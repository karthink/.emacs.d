;;; setup-reading.el --- Support for reading in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur(defvar wombag-show-entry-switch) <karthikchikmagalur@gmail.com>
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

;;

;;; Code:


;;----------------------------------------------------------------
;; ** PDFs
;;----------------------------------------------------------------
(cond
 (IS-GUIX (load-library "pdf-tools-autoloads"))
 (IS-LINUX (elpaca pdf-tools))
 (IS-MAC))

(use-package pdf-tools
  :commands pdf-tools-install
  :bind (:map pdf-view-mode-map
              ("C-c C-r w" . pdf-view-auto-slice-minor-mode)
              ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
              ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
  :config
  (setf pdf-tools-enabled-modes
        (delq 'pdf-misc-size-indication-minor-mode
              pdf-tools-enabled-modes))
  (setq pdf-view-resize-factor 1.05))

;;;----------------------------------------------------------------
;; ** ORG-NOTER
;;;----------------------------------------------------------------
(use-package org-noter
  :ensure t
  :bind (:map org-noter-notes-mode-map
              ("C-c ." . my/org-noter-associate-this-entry))
  :config
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil
        org-noter-doc-split-fraction '(0.4 . 0.5)
        org-noter-hide-other nil
        org-noter-disable-narrowing t
        org-noter-use-indirect-buffer nil
        ;; This is buggy
        org-noter-prefer-root-as-file-level nil)

  (remove-hook 'org-noter--show-arrow-hook
               #'org-noter-pdf--show-arrow)

  (defun my/org-noter-associate-this-entry (&optional arg)
    "Associate the current Org entry with the current org-noter location.

With prefix arg, use a precise location."
    (interactive "P")
    (org-noter--with-valid-session
     (let* ((precise-info (if arg (org-noter--get-precise-info) 'interactive))
            (location (org-noter--doc-approx-location precise-info)))
       (org-entry-put nil org-noter-property-note-location
                      (org-noter--pretty-print-location location)))))

  (defun my/org-noter--stay (orig-fn)
    "After a sync action, stay in the doc or notes window.

Whichever was already active."
    (let ((win (selected-window))
          (inhibit-redisplay t))
      (funcall orig-fn)
      (unless (eq (selected-window) win)
        (select-window (org-noter--get-notes-window)))))

  (dolist (sym '(org-noter-sync-next-note
                 org-noter-sync-prev-note
                 org-noter-sync-current-note
                 org-noter-sync-next-page-or-chapter
                 org-noter-sync-prev-page-or-chapter
                 org-noter-sync-current-page-or-chapter))
    (advice-add sym :around #'my/org-noter--stay)))

;;----------------------------------------------------------------
;; ** SHR
;;----------------------------------------------------------------
(use-package shr
  :defer
  :config
  (setq shr-image-animate nil
        shr-use-colors nil
        shr-width 78)
  (defun my/shr-image-extra (_spec alt &rest _)
    "Center large images and add the ALT text below."
    (when (and (display-graphic-p) (or (eolp) (bolp) (looking-at " *$")))
      ;; Center image(s)
      (let ((size 0) (lbp (line-beginning-position)))
        (save-restriction
          (narrow-to-region (line-beginning-position) (line-end-position))
          (save-excursion
            (goto-char lbp)
            (unless (and (looking-at " ") (get-text-property (point) 'display))
              (insert " "))
            ;; find combined size of all images on line
            (let ((prop) (img nil))
              (while-let ((prop (text-property-search-forward
                                 'display nil (lambda (prop val)
                                                (eq (car-safe val) 'image))))
                          (img (image--get-image (1- (point)))))
                (setq size (+ size (car (image-size img t))))))))
        ;; Center all images on line
        (when (> size 0)
          (add-text-properties
           lbp (1+ lbp)
           `(display (space :align-to (- center (0.50 . (,size))))
                     font-lock-face default)))
        ;; Alt text
        (when (and (stringp alt) (> (length alt) 2))
          ;; (looking-back
          ;;  (rx bol (* space) (or (literal alt) (seq "*" (* space))))
          ;;  (line-beginning-position))
          (when (eobp)
            (insert " "))
          (put-text-property
           (1- (point)) (point) 'display
           (concat
            "\n"
            (propertize " " 'display `(space :align-to (- center (0.50 . ,(length alt)))))
            (propertize (concat "[" alt "]") 'font-lock-face 'shadow)))))))
  (advice-add 'shr-put-image :after #'my/shr-image-extra)

  (defun my/shr-zoom-image-extra ()
    "Clear any link property and adjust line spacing after zooming in on an image."
    (when (> line-spacing 0)
      (message "Adjusting line spacing for zoomed image.")
      (setq line-spacing 0))
    (let ((prev (previous-single-property-change (point) 'face))
          (buffer-read-only nil))
      (remove-text-properties prev (1+ (point))
                              '(face shr-link))))
  (advice-add 'shr-zoom-image :after #'my/shr-zoom-image-extra)
  (use-package shr-heading
    :hook (eww-mode . shr-heading-setup-imenu)))

;;----------------------------------------------------------------
;; ** NOV.EL
;;----------------------------------------------------------------
(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook ((nov-mode . my/nov-display-setup)
         (nov-mode . er/add-text-mode-expansions))
  :bind (:map nov-mode-map
              ("u" . my/scroll-down-half)
              ("d" . my/scroll-up-half))
  :config
  (use-package setup-reading
    :disabled
    :hook (nov-post-html-render . my/reader-center-images))

  (setq nov-text-width 72
        nov-save-place-file (expand-file-name "nov-places" user-cache-directory))
  ;; Pinched from https://tecosaur.github.io/emacs-config/config.html
  (defun my/nov-display-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.0
                             :width 'semi-expanded)
    ;; (face-remap-add-relative 'default :height 1.1)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors t)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width (1+ nov-text-width))
    (visual-fill-column-mode 1)))

;;----------------------------------------------------------------
;; ** Reading support
;;----------------------------------------------------------------
(defun my/reader-display-buffer (buf &optional _)
  (pop-to-buffer buf `((display-buffer-reuse-window display-buffer-in-direction)
                       (direction . ,(if (> (window-width) 130)
                                         'right 'above))
                       (window-height . 0.72)
                       (window-width . 0.64))))

(defvar my/reader-names '("*elfeed-entry*" "*wombag-entry*"))
(defvar my/reader-modes '(elfeed-show-mode wombag-show-mode eww-mode))
(defvar my/reader-list-modes '(elfeed-search-mode wombag-search-mode))

(defvar my/reader-quit-functions
  (cl-pairlis my/reader-modes
              '(kill-buffer-and-window
                wombag-show-quit-window
                quit-window)))

(defvar my/reader-list-quit-functions
  (cl-pairlis my/reader-list-modes
              '(elfeed-search-quit-window
                wombag-search-quit-window
                quit-window)))

(defvar my/reader-list-next-prev-functions
  (cl-pairlis my/reader-list-modes
              '((next-line . previous-line)
               (next-line . previous-line))))

(defvar my/reader-list-show-functions
  (cl-pairlis my/reader-list-modes
              '(elfeed-search-show-entry
                wombag-search-show-entry)))

(defun my/reader-show ()
  (interactive)
  (let ((elfeed-show-entry-switch #'my/reader-display-buffer)
        (wombag-show-entry-switch #'my/reader-display-buffer)
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
            (overlay-put ov 'evaporate t)
            (overlay-put
             ov 'after-string
             (propertize
              " " 'face 'default
              'display `(space :align-to (- center (0.58 . ,(prop-match-value match)))))))))))

(provide 'setup-reading)
;;; setup-reading.el ends here
