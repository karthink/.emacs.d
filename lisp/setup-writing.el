;;; setup-writing.el --- writing support             -*- lexical-binding: t; -*-

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

;;; SPELL CHECKING
;;----------------------------------------------------------------
;;;; FLYSPELL
;;----------------------------------------------------------------
(use-package flyspell
  :commands flyspell-mode
  :bind (:map flyspell-mode-map
              ("C-M-i" . nil)
              ("C-;" . nil)
              ("C-," . nil)
              ("C-; C-4" . 'flyspell-auto-correct-previous-word)
              ;; ("C-; n" . 'flyspell-goto-next-error)
              )
  :init (add-hook 'flyspell-mode-hook
                  (poi-register 'flyspell-goto-next-error
                                'flyspell-auto-correct-word)))

;;----------------------------------------------------------------
;;;; SPELL-FU DONT
;;----------------------------------------------------------------
;; Disabled while I test Jinx
(use-package spell-fu
  :disabled
  :ensure t
  :commands text-spell-fu-mode
  :config
  (add-hook 'spell-fu-mode-hook
            (poi-register
             #'spell-fu--goto-next-or-previous-error))
  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary
                "en-personal"
                (concat user-cache-directory
                        ".aspell.en.pws")))))
  (defun text-spell-fu-mode ()
    (interactive)
    (setq spell-fu-faces-exclude
          '(font-lock-function-name-face
            font-lock-comment-face
            font-lock-keyword-face
            font-latex-math-face
            font-lock-variable-name-face
            font-lock-type-face
            font-lock-constant-face
            font-latex-sedate-face
            org-block-begin-line
            org-block-end-line
            org-code
            org-date
            org-drawer org-document-info-keyword
            org-ellipsis
            org-link
            org-meta-line
            org-properties
            org-properties-value
            org-special-keyword
            org-src
            org-tag
            org-verbatim))
    (spell-fu-mode)))

;;----------------------------------------------------------------
;;;; JINX
;;----------------------------------------------------------------
(when IS-GUIX (require 'jinx-autoloads))
(cond
 (IS-MAC)
 (t
  (use-package jinx
    ;; :hook ((text-mode prog-mode conf-mode) . my/jinx-mode)
    :commands jinx-mode
    :bind ([remap ispell-word] . jinx-correct)
    :config
    (cl-callf
        (lambda (pl)
          (delete-dups
           (append '( font-lock-constant-face font-lock-comment-face
                      TeX-fold-unfolded-face TeX-fold-folded-face)
                   pl)))
        (alist-get 'tex-mode jinx-exclude-faces))
    (cl-callf
        (lambda (pl)
          (delete-dups
           (append '(org-block)
                   (alist-get 'tex-mode jinx-exclude-faces) pl)))
        (alist-get 'org-mode jinx-exclude-faces))
    (add-hook 'jinx-mode-hook
              (poi-register 'jinx-next 'jinx-correct)))))

;;----------------------------------------------------------------
;;; FLYMAKE
;;----------------------------------------------------------------
(use-package flymake-proselint
  :disabled
  :ensure t
  :after flymake
  :hook ((markdown-mode org-mode text-mode) . flymake-proselint-setup))

(use-package flymake-vale
  :disabled
  :ensure (:host github :repo "tpeacock19/flymake-vale")
  :after flymake
  :hook ((markdown-mode org-mode text-mode) . flymake-vale-load))

(provide 'setup-writing)
;;; setup-writing.el ends here
