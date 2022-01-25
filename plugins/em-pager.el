;;; +eshell.el -- My eshell extras -*- lexical-binding: t -*-
;;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a

(defun eshell-pager-quit ()
  "Better cancel command."
  (interactive)
  (eshell-pager-post)
  (eshell-interrupt-process)
  (goto-char (point-max))
  (keyboard-quit))

(defvar-local eshell-pager-start nil)

(defun eshell-pager-reposition ()
  (eshell-postoutput-scroll-to-bottom)
  (when eshell-pager-start
    (dolist (win (get-buffer-window-list (current-buffer)))
      (when (< eshell-pager-start (window-start win))
        (add-hook 'post-command-hook #'eshell-pager-scroll -99 'local)
        (with-selected-window win
          (goto-char eshell-pager-start)
          (set-window-start nil eshell-pager-start))))))

(defun eshell-pager-pre ()
  (run-at-time 0 nil (lambda ()
                       (add-hook 'post-command-hook #'eshell-pager-reset -99 'local)))
  (setq eshell-pager-start (save-excursion
                              (goto-char eshell-last-input-start)
                              (line-beginning-position))))

(defun eshell-pager-post ()
  (remove-hook 'post-command-hook #'eshell-pager-scroll 'local)
  (remove-hook 'post-command-hook #'eshell-pager-reset 'local)
  (setq eshell-pager-start nil))

(defun eshell-pager-reset ()
  ;; Reset when sending input
  (when (memq this-command (list #'eshell-send-input #'self-insert-command))
    (eshell-pager-post))
  ;; HACK: Sometimes this-command is nil when quitting?!
  (unless this-command
    (with-local-quit (eshell-pager-quit))))

(defun eshell-pager-scroll ()
  (setq eshell-pager-start (window-start)))

(provide 'em-pager)
