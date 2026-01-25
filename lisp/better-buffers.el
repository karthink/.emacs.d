;;######################################################################
;; * BETTER BUFFERS
;;######################################################################
;;(require 'use-package nil t)
;; Collection of commands to make handling buffers/files less painful

;;----------------------------------------------------------------------
;; ** SAVE AND BACKUP
;;----------------------------------------------------------------------
;; Put backups elsewhere:
(setq auto-save-interval 2400)
(setq auto-save-timeout 300)
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" user-cache-directory))
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-cache-directory)))
      backup-by-copying t               ; Use copies
      version-control t                 ; Use version numbers on backups
      delete-old-versions t             ; Automatically delete excess backups
      kept-new-versions 10              ; Newest versions to keep
      kept-old-versions 5)              ; Old versions to keep

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;----------------------------------------------------------------------
;; ** BUFFER NAMING
;;----------------------------------------------------------------------
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;;----------------------------------------------------------------------
;; ** AUTO REVERT
;;----------------------------------------------------------------------
(use-package autorevert
  :diminish auto-revert-mode
  :hook ((prog-mode text-mode tex-mode org-mode conf-mode)
         . auto-revert-mode))

(setq undo-limit (* 80 1024 1024))

(use-package emacs
  :bind
  (("C-x k" . my/kill-current-buffer)
   ("<f7>" . my/hide-cursor-mode)
   :map ctl-x-x-map
   ("z" . bury-buffer)
   ("R" . rename-visited-file))
  :config
  (defun my/kill-current-buffer (&optional arg)
    "Kill current buffer, DWIM.

- If prefix argument ARG is set, call `kill-buffer' instead.
- If in a server buffer with active clients, call `server-edit'.
- Otherwise, call `kill-current-buffer'."
    (interactive "P")
    (cond (arg
           (call-interactively #'kill-buffer))
          ((and (bound-and-true-p server-process)
                server-buffer-clients)
           (cl-letf ((inhibit-message t)
                     ((symbol-function 'y-or-n-p) #'always))
             (server-edit)))
          ((kill-current-buffer))))

  (defvar-local hide-cursor--original nil)
  (define-minor-mode my/hide-cursor-mode
    "Hide or show the cursor.

When the cursor is hidden `scroll-lock-mode' is enabled, so that
the buffer works like a pager."
    :global nil
    :lighter "H"
    (if my/hide-cursor-mode
        (progn
          (scroll-lock-mode 1)
          (setq-local hide-cursor--original
                      cursor-type)
          (setq-local cursor-type nil))
      (scroll-lock-mode -1)
      (setq-local cursor-type (or hide-cursor--original
                                  t))))

  (defun my/scroll-up-half ()
    (interactive)
    (scroll-up-command
     (floor
      (- (window-height)
         next-screen-context-lines)
      2)))

  (defun my/scroll-down-half ()
    (interactive)
    (scroll-down-command
     (floor
      (- (window-height)
         next-screen-context-lines)
      2))))

;;;----------------------------------------------------------------
;; * VIEW-MODE
;;----------------------------------------------------------------
(use-package emacs
  :config
  (setq view-read-only t))

(provide 'better-buffers)

