;;; setup-visual.el --- visual affordances in the buffer  -*- lexical-binding: t; -*-

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

;;

;;; Code:
;;----------------------------------------------------------------
;; ** Color things
;;;----------------------------------------------------------------
;; Colorize color names and parens in buffers
(use-package rainbow-mode
  :commands rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :ensure t)

;;----------------------------------------------------------------
;; ** MIXED-PITCH-MODE
;;;----------------------------------------------------------------
(use-package mixed-pitch
  :ensure t
  :config
  (dolist (face '(line-number org-property-value org-drawer
                              error org-cite corfu-current corfu-default
                              org-meta-line org-tag))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
  (setq mixed-pitch-set-height nil
        mixed-pitch-variable-pitch-cursor nil)
  (defun my/mixed-pitch-spacing ()
    (if mixed-pitch-mode
        (setq line-spacing 0.12)
      (setq line-spacing 0.0))))

;;----------------------------------------------------------------
;; ** VISUAL-FILL
;;;----------------------------------------------------------------
(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode
  :hook ((eww-after-render . my/visual-fill-in-window)
         (eww-after-render . visual-line-mode)
         (notmuch-show-mode . visual-fill-column-mode))
  :config
  (defun my/visual-fill-in-window ()
    (when-let ((win (get-buffer-window (current-buffer))))
      (with-selected-window win
        (visual-fill-column-mode 1))))
  (setq-default visual-fill-column-center-text t
                visual-fill-column-width 94))

;;;----------------------------------------------------------------
;; ** OLIVETTI
;;----------------------------------------------------------------
(use-package olivetti
  :commands (my/olivetti-mode)
  :ensure t
  :config
  (setq-default
   olivetti-body-width 90
   olivetti-minimum-body-width 76
   olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode my/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.

Fringes are disabled. The modeline is hidden, except for
`prog-mode' buffers (see `my/mode-line-hidden-mode'). The default
typeface is set to a proportionately spaced family, except for
programming modes (see `my/variable-pitch-mode'). The cursor
becomes a blinking bar. Evil-mode (if bound) is disabled."
    :init-value nil
    :global nil
    (if my/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (unless (derived-mode-p 'prog-mode)
            ;; (my/mode-line-hidden-mode 1)
            (mixed-pitch-mode 1))
          (if (bound-and-true-p evil-mode)
              (evil-emacs-state))
          ;; (setq-local line-spacing 0.16)
          ;; (setq-local cursor-type '(bar . 2))
          )
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (mixed-pitch-mode -1)
      (kill-local-variable 'line-spacing)
      ;; (unless (derived-mode-p 'prog-mode)
      ;;   (my/mode-line-hidden-mode -1))
      (when (and (bound-and-true-p evil-mode)
                 (evil-emacs-state-p))
        (evil-exit-emacs-state))
      (kill-local-variable 'cursor-type)))

  (define-minor-mode my/reader-mode
    "Mode to read a buffer in style. Pop it out into a frame,
turn on `view-mode', and `my/olivetti-mode', which in turn hides
the mode-line and switches to `variable-pitch-mode'."
    :init-value
    :global-nil
    (if my/reader-mode
        (progn
          (make-frame '((name . "dropdown_reader")))
          (my/olivetti-mode 1)
          (view-mode 1)
          (if (equal major-mode 'org-mode)
              (org-show-all)))
      (view-mode -1)
      (my/olivetti-mode -1)
      (delete-frame)))

  :bind
  ("C-c O" . my/olivetti-mode)
  ("C-c R" . my/reader-mode))

;;----------------------------------------------------------------
;; ** INDENT-BARS MAYBE
;;----------------------------------------------------------------
(use-package indent-bars
  :ensure (:type git :host github
           :repo "jdtsmith/indent-bars")
  :hook ((python-mode julia-mode) . indent-bars-mode)
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.3)
   indent-bars-pattern "."
   indent-bars-width-frac 0.2
   indent-bars-pad-frac 0.2
   indent-bars-zigzag nil
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil))

;;----------------------------------------------------------------
;; ** ISCROLL DONT
;;----------------------------------------------------------------
;; Smooth scrolling through images.  What a pain Emacs' default behavior is here.
(use-package iscroll
  :disabled
  :ensure t
  :hook ((text-mode eww-mode) . iscroll-mode))

;;----------------------------------------------------------------
;; ** SPACIOUS-PADDING DONT
;;----------------------------------------------------------------
(use-package spacious-padding
  :disabled
  :ensure t
  :defer
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 16
           :header-line-width 4
           :mode-line-width 2
           :tab-width 2
           :right-divider-width 24
           :scroll-bar-width 8)))

(provide 'setup-visual)
;;; setup-visual.el ends here
