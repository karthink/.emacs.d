;;; setup-coding.el --- general coding support       -*- lexical-binding: t; -*-

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
;; ** FLYMAKE
;;----------------------------------------------------------------
(use-package flymake
  :defer
  :init
  (add-hook 'flymake-mode-hook
            (poi-register 'flymake-goto-next-error
                          'my/flymake-action))
  :config
  (defun my/flymake-action ()
    (interactive)
    (when (and (fboundp 'eglot-managed-p)
               (eglot-managed-p))
      (call-interactively #'eglot-code-actions))))

;; DONT
(use-package flymake-diagnostic-at-point
  :disabled
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config (setq flymake-diagnostic-at-point-display-diagnostic-function
                'flymake-diagnostic-at-point-display-popup))

(use-package sideline-flymake
  :ensure t
  :diminish sideline-mode
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'point)
  ;; 'point to show errors only on point
  ;; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)
        sideline-flymake-show-backend-name t))


;;----------------------------------------------------------------
;; ** DUMB-JUMP
;;----------------------------------------------------------------
;; Even dumber jump
(use-package buffer-local-xref
  ;; :after xref
  :init (add-hook 'xref-backend-functions #'buffer-local-xref-activate 95)
  ;; :hook (org-mode . (lambda ()
  ;;                     (add-hook 'xref-backend-functions
  ;;                               #'buffer-local-xref-activate
  ;;                               30 t)))
  )

(use-package dumb-jump
  :disabled
  :ensure t
  ;; :after xref
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (add-to-list 'dumb-jump-project-denoters ".project")
  ;; :hook (org-mode . (lambda ()
  ;;                     (add-hook 'xref-backend-functions
  ;;                               #'dumb-jump-xref-activate
  ;;                               20 t)))
  )

;;----------------------------------------------------------------
;; ** EGLOT - LSP
;;----------------------------------------------------------------
;; (use-package jsonrpc :ensure (:tag "1.0.25") :defer)
;; (use-package eldoc :ensure t :defer)

(use-package eglot
  ;; :ensure t
  :commands eglot
  :bind (:map eglot-mode-map ("C-h ." . eldoc))
  :hook ((eglot-managed-mode . my/eglot-eldoc-settings))
  :config
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  ;; (setq eglot-put-doc-in-help-buffer nil)
  ;; (setq eglot-events-buffer-size 0
  ;;       eglot-report-progress nil)
  (setq eglot-extend-to-xref t))

;;----------------------------------------------------------------
;; *** EGLOT-BOOSTER
;;----------------------------------------------------------------
(use-package eglot-booster
  :ensure (:host github
           :repo "jdtsmith/eglot-booster")
  :if (executable-find "emacs-lsp-booster")
  :custom (eglot-booster-io-only t)
  :after eglot
  :init (eglot-booster-mode))

;; (use-package dape
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq dape-window-buffer-arrangement 'right))

;;----------------------------------------------------------------
;; ** LSP SUPPORT
;;----------------------------------------------------------------
(use-package lsp-mode
  :disabled
  ;; :hook (python-mode . lsp-deferred)
  :bind (("C-c C-d" . lsp-describe-thing-at-point))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-auto-configure t
        lsp-enable-symbol-highlighting nil
        lsp-pyls-plugins-rope-completion-enabled t)
  (use-package company-lsp :straight t :commands company-lsp)
  (add-to-list 'lsp-language-id-configuration '(matlab-mode . "matlab"))
  (use-package lsp-ui
    :disabled t
    :commands lsp-ui-mode
    :config
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    (setq lsp-ui-sideline-enable nil
          lsp-ui-doc-enable nil
          lsp-ui-flycheck-enable nil
          lsp-ui-imenu-enable t
          lsp-ui-sideline-ignore-duplicate t)))

;;----------------------------------------------------------------
;; ** hl-todo
;;----------------------------------------------------------------
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :bind (:map prog-mode-map
              ("M-g t" . hl-todo-next)
              ("M-g T" . hl-todo-previous))
  :config
  (defvar-keymap hl-todo-repeat-map
    :repeat t
    "n" #'hl-todo-next
    "p" #'hl-todo-previous)
  (setq hl-todo-wrap-movement t))

;;----------------------------------------------------------------
;; ** ENVRC
;;----------------------------------------------------------------
(use-package envrc :ensure t :defer)

(provide 'setup-coding)
;;; setup-coding.el ends here
