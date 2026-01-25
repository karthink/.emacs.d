;; -*- lexical-binding: t; -*-

;;; MANAGE STATE
;;;; SAVE-PLACE
(save-place-mode 1)
(setq save-place-file (expand-file-name "places" user-cache-directory))

;;;; RECENTF
;; Keep track of recently opened files. Also feeds into the list of recent
;; directories used by consult-dir.
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-cache-directory)
        recentf-max-saved-items 200
        recentf-auto-cleanup 300)
  (define-advice recentf-cleanup (:around (fun) silently)
    (let ((inhibit-message t)
          (message-log-max nil))
      (funcall fun)))
  (recentf-mode 1))

;;;; SAVEHIST
;; Save history across various prompts
(use-package savehist
  :defer 2
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "savehist" user-cache-directory))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

;;;; DESKTOP
;; Save and resume Emacs sessions.
(use-package desktop
  :disabled
  ;; :hook (kill-emacs . desktop-save-in-desktop-dir)
  :config
  ;; (when (daemonp)
  ;;   (defun my/restore-desktop (frame)
  ;;     "Restores desktop and cancels hook after first frame opens."
  ;;     (with-selected-frame frame
  ;;       (desktop-save-mode 1)
  ;;       (desktop-read)
  ;;       (remove-hook 'after-make-frame-functions 'my/restore-desktop)))
  ;;   (add-hook 'after-make-frame-functions 'my/restore-desktop))
  (setq desktop-auto-save-timeout 300
        desktop-path `(,(expand-file-name "desktop" user-cache-directory))
        desktop-dirname (expand-file-name "desktop" user-cache-directory)
        desktop-base-file-name "desktop"
        desktop-restore-forces-onscreen nil
        desktop-globals-to-clear nil
        desktop-load-locked-desktop t
        desktop-missing-file-warning nil
        desktop-restore-eager 20
        desktop-restore-frames t
        desktop-save 'ask-if-new))

;;;; ACTIVITIES
(use-package activities
  :when (daemonp)
  :ensure t
  :init
  (setq activities-mode-idle-frequency 10)
  (activities-tabs-mode))

(use-package activities
    :custom-face
    (activities-tabs-face ((t nil)))
    :bind
    (("C-x C-a C-j" . activities-resume)
     ("C-x C-a C-z" . activities-suspend)
     ("C-x C-a C-k" . activities-discard)
     ("C-x C-a b" . activities-switch-buffer)
     ("C-x C-a C-s" . activities-define)
     ("C-x C-a g" . activities-revert)
     ("C-x C-a l" . activities-list)))

;;;; UNDO HISTORY

;; The =undo-fu-session= package saves and restores the undo states of buffers
;; across Emacs sessions.
(use-package undo-fu-session
  :ensure t
  :hook ((prog-mode conf-mode text-mode tex-mode) . undo-fu-session-mode)
  :config
  (setq undo-fu-session-directory
        (expand-file-name "undo-fu-session/" user-cache-directory)))

(provide 'setup-persistence)
