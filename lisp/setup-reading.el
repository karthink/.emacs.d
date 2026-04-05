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
        shr-discard-aria-hidden t
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
