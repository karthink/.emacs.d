;; -*- lexical-binding: t; -*-

(require 'gptel)
(require 'gptel-transient)

(defvar gptel-ask--buffer-name "*gptel-ask*"
  "Name for one-off queries.")

(defcustom gptel-ask-display-buffer-action
  '((display-buffer-reuse-window
     display-buffer-in-side-window)
    (side . right)
    (slot . 10)
    (window-width . 0.25)
    (window-parameters (no-delete-other-windows . t))
    (bump-use-time . t))
  "Display buffer action for showing the gptel-ask buffer."
  :type 'display-buffer--action-custom-type
  :group 'emacs)

;;; gptel-ask

(setf (alist-get gptel-ask--buffer-name display-buffer-alist
                 nil nil #'equal)
      gptel-ask-display-buffer-action)

(defun gptel--prepare-ask-buffer ()
  (unless (buffer-live-p gptel-ask--buffer-name)
    (with-current-buffer (get-buffer-create gptel-ask--buffer-name)
      (when (fboundp 'markdown-mode)
        (markdown-mode)
        (setq-local markdown-hide-markup t))
      (setq header-line-format
            (list '(:eval (gptel-backend-name gptel-backend))
                  ": " gptel-ask--buffer-name))
      (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll nil t)
      (add-hook 'gptel-post-response-functions
                (lambda (beg end)
                  (save-excursion
                    (gptel-end-of-response)
                    (goto-char end)
                    (insert "\n\n----")))
                nil t)))
  (if-let ((win (get-buffer-window gptel-ask--buffer-name))
           ((window-live-p win)))
      (with-selected-window win
        (goto-char (point-max))
        (ensure-empty-lines 0))
    (with-current-buffer (get-buffer gptel-ask--buffer-name)
      (goto-char (point-max))
      (ensure-empty-lines 0))))

(defun gptel-ask (&optional arg)
  (interactive "P")
  (gptel--prepare-ask-buffer)
  (gptel--suffix-send (list "m" (if arg "e" (concat "b" gptel-ask--buffer-name)))))

(provide 'gptel-ask)
