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
  :hook ((keycast-tab-bar-mode
          keycast-mode-line-mode
          keycast-log-mode) .
          'my/keycast-with-embark-and-avy)
  :config
  (setq keycast-separator-width 1)
  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing!")))
  (dolist (input '( dslide-deck-forward dslide-deck-backward dslide-deck-stop
                    "<switch-frame>"))
    (add-to-list 'keycast-substitute-alist `(,input nil nil)))
  (setf (alist-get 'strokes-do-stroke
                   keycast-substitute-alist)
        '("[mouse]" t))

  (defun my/keycast-with-embark-and-avy ()
    (cond
     ((or keycast-mode-line-mode keycast-tab-bar-mode keycast-log-mode)
      (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)
      ;; (advice-add 'avy-goto-char-timer :filter-return #'store-action-key+cmd)
      (advice-add 'avy-handler-default :before #'keycast-capture-avy-dispatch)
      (dolist (cmd '(embark-act embark-become))
        (advice-add cmd :before #'force-keycast-update)))
     (t (advice-remove 'embark-keymap-prompter #'store-action-key+cmd)
        (advice-remove 'avy-handler-default #'keycast-capture-avy-dispatch)
        (dolist (cmd '(embark-act embark-become))
          (advice-remove cmd #'force-keycast-update)))))

  (defun store-action-key+cmd (cmd)
    (setq this-command cmd
          keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd)
    cmd)

  (defun store-action-key-no-cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd))
  
  (defun keycast-capture-avy-dispatch (char)
    (if-let ((cmd (assoc char avy-dispatch-alist)))
        (setq keycast--this-command-keys (make-vector 1 char)
              keycast--this-command (cdr cmd))))
  
  (defun force-keycast-update (&rest _) (keycast--update)))

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

(use-package dslide
  :ensure (:remotes ("karthink" :repo "karthink/dslide"))
  :defer
  :bind ( :map dslide-mode-map
          ("<down>" . nil)
          ("C-<down>" . dslide-deck-stop))
  :config
  (setq dslide-slide-in-blank-lines 2)
  (setq dslide-breadcrumb-separator " » ")
  (setq dslide-breadcrumb-separator-style 'append) ;or 'separate
  (remove-hook 'dslide-start-hook #'dslide-cursor-hide)
  (remove-hook 'dslide-present-hook #'dslide-cursor-hide)
  (remove-hook 'dslide-develop-hook #'dslide-cursor-hide)
  (defclass dslide-action-reveal (dslide-action)
    ((overlays :initform nil)
     (elements :initform (paragraph link)    ;elements to hide
               :initarg :elements))
    "Hide all paragraphs and reveal them one by one.")

  (cl-defmethod dslide-begin :after ((obj dslide-action-reveal))
    (oset obj overlays
          (dslide-section-map
           obj (oref obj elements)
           (lambda (el)
             (unless (and (eq (org-element-type el) 'src-block)
                          (string-match-p
                           ":exports +none"
                           (org-element-property :parameters el))))
             (dslide-hide-element el (oref obj inline))))))

  (cl-defmethod dslide-final :after ((obj dslide-action-reveal))
    (mapc #'delete-overlay (oref obj overlays)))

  (cl-defmethod dslide-forward ((obj dslide-action-reveal))
    (when-let* ((next-para (dslide-section-next obj (oref obj elements) nil t))
                (progress (org-element-property :begin next-para)))
      (let ((item-overlays (seq-intersection (oref obj overlays)
                                             (overlays-at progress))))
        (oset obj overlays (seq-difference (oref obj overlays)
                                           item-overlays))
        (when dslide-slide-in-effect
          (if (oref obj inline)
              (mapc #'dslide-animation-peel item-overlays)
            (dslide-animation-setup (org-element-property :begin next-para)
                                    (org-element-property :end next-para))
            ;; Because the user might add items etc, and to avoid the need for
            ;; keys matching items to our overlays, we intersect overlays we are
            ;; managing with overlays found at point, which could include overlays
            ;; from some other action
            (mapc #'delete-overlay item-overlays))))
      ;; return progress
      progress))

  (cl-defmethod dslide-backward ((obj dslide-action-reveal))
    (when-let ((previous-para (dslide-section-previous obj (oref obj elements)))
               (progress (org-element-property :begin previous-para)))
      (push (dslide-hide-element previous-para (oref obj inline))
            (oref obj overlays))
      ;; return progress
      progress)))

(provide 'setup-present)
;;; setup-present.el ends here
