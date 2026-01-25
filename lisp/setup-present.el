;;; setup-present.el --- screencasting config           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

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

;; keycast, org-tree-slide and more

;;; Code:

;;----------------------------------------------------------------
;; ** PRESENTATION (BIG) MODE DONT
;;----------------------------------------------------------------
;; Turned off since global-text-scale-adjust exists now
(use-package presentation
  :disabled
  :ensure t
  :commands presentation-mode
  :config
  (setq presentation-default-text-scale 1.25
        presentation-mode-lighter " BIG"
        presentation-keep-last-text-scale nil))

;; ** SCREENCAST
;; Presentation-mode will embiggen everything. Keycast-mode shows the keys being
;; pressed. Gif-screencast will screenshot each user action and compile them
;; into a gif.
(use-package keycast
  :ensure t
  :defer
  :config
  (setq keycast-separator-width 1)
  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing!")))
  (setf (alist-get 'strokes-do-stroke
                   keycast-substitute-alist)
        '("[mouse]" t))

  (defun store-action-key+cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd)
    cmd)

  (defun store-action-key-no-cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd))
  
  (defun keycast-capture-avy-dispatch (char)
    (if-let ((cmd (assoc char avy-dispatch-alist)))
        (setq keycast--this-command-keys (make-vector 1 char)
              keycast--this-command (cdr cmd))))
  
  (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)
  ;; (advice-add 'avy-goto-char-timer :filter-return #'store-action-key+cmd)
  (advice-add 'avy-handler-default :before #'keycast-capture-avy-dispatch)

  (defun force-keycast-update (&rest _)
    (force-mode-line-update t))

  (dolist (cmd '(embark-act embark-become))
    (advice-add cmd :before #'force-keycast-update)))

;; Using a screen recorder instead. gif-screencast misses keystrokes.
(use-package gif-screencast
  :disabled 
  :commands (gif-screencast gif-screencast-stop)
  :config
  (define-minor-mode my/screencast-mode
    "Minor mode to record screencasts from emacs."
    :global t
    :init-value nil
    (if my/screencast-mode
        (progn
          (menu-bar-mode -1)
          (presentation-mode 1)
          (keycast-mode 1)
          (gif-screencast))
      (gif-screencast-stop)
      (keycast-mode -1)
      ;; (menu-bar-mode +1)
      (presentation-mode -1)))
  :bind
  ("C-c S" . my/screencast-mode))

;;;----------------------------------------------------------------
;; ** ORG-TREE-SLIDE
;;;----------------------------------------------------------------
;; Presentations from within org-mode.
(use-package org-tree-slide
  :ensure t
  :after org
  :commands my/org-presentation-mode
  ;; :hook (org-tree-slide-after-narrow . my/org-tree-slide-enlarge-latex-preview)
  :config
  (setq org-tree-slide-never-touch-face nil
        org-tree-slide-skip-outline-level 8
        org-tree-slide-heading-emphasis nil
        org-tree-slide-cursor-init nil
        org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message
        (propertize "ORG PRESENTATION STARTED" 'face 'success)
        org-tree-slide-deactivate-message
        (propertize "ORG PRESENTATION STOPPED" 'face 'error))

  ;; (defun my/org-tree-slide-enlarge-latex-preview ()
  ;;   (dolist (ov (overlays-in (point-min) (point-max)))
  ;;     (if (eq (overlay-get ov 'org-overlay-type)
  ;;             'org-latex-overlay)
  ;;         (overlay-put
  ;;          ov 'display
  ;;          (cons 'image
  ;;                (plist-put
  ;;                 (cdr (overlay-get ov 'display))
  ;;                 :scale (+ 1.0 (* 0.2 text-scale-mode-amount))))))))

  (defvar olivetti-style)
  (define-minor-mode my/org-presentation-mode
    "Parameters for plain text presentations with `org-mode'."
    :init-value nil
    :global nil
    (if my/org-presentation-mode
        (progn
          (unless (eq major-mode 'org-mode)
            (user-error "Not in an Org buffer"))
          (setq-local org-hide-emphasis-markers t)
          (org-tree-slide-mode 1)
          (setq olivetti-style nil)
          (setq line-spacing 0.12)
          ;; (setq olivetti-margin-width 14)
          ;; (setq olivetti-body-width 0.7)
          (text-scale-increase 6)
          (my/olivetti-mode 1))
      (org-tree-slide-mode -1)
      ;; (kill-local-variable 'org-hide-emphasis-markers)
      (my/olivetti-mode -1)
      (text-scale-decrease 6)
      (text-scale-mode -1)))

  (defvar-keymap my/org-tree-slide--header-keymap
    :doc "Keymap for actions in the slide header"
    "SPC" 'org-tree-slide-move-next-tree
    "n"   'org-tree-slide-move-next-tree
    "S-SPC" 'org-tree-slide-move-previous-tree
    "p"     'org-tree-slide-move-previous-tree
    "<mouse-1>" 'org-tree-slide-move-next-tree
    "<mouse-3>" 'org-tree-slide-move-previous-tree)

  (define-advice org-tree-slide--set-slide-header (:after (_) set-keymap)
    (overlay-put org-tree-slide--header-overlay
                 'keymap my/org-tree-slide--header-keymap))

  :bind (("C-c P"      . my/org-presentation-mode)
         :map org-tree-slide-mode-map
         ("<next>" . org-tree-slide-move-next-tree)
         ("<prior>" . org-tree-slide-move-previous-tree)
         ("<home>" . 'org-tree-slide-display-header-toggle)
         ("<C-down>"  . org-tree-slide-display-header-toggle)
         ("<C-right>" . org-tree-slide-move-next-tree)
         ("<C-left>"  . org-tree-slide-move-previous-tree)))

(provide 'setup-present)
;;; setup-present.el ends here
