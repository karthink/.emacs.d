;; -*- lexical-binding: t; -*-

(require 'gptel)

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
                    (unless (looking-at-p "[\n[:blank:]]+----")
                      (insert "\n\n----"))))
                nil t)))
  (if-let ((win (get-buffer-window gptel-ask--buffer-name))
           ((window-live-p win)))
      (with-selected-window win
        (goto-char (point-max))
        (ensure-empty-lines 0))
    (with-current-buffer (get-buffer gptel-ask--buffer-name)
      (goto-char (point-max))
      (ensure-empty-lines 0))))

;;;###autoload
(defun gptel-ask (&optional arg)
  (interactive "P")
  (let ((gptel-backend (default-value 'gptel-backend))
        (gptel-model (default-value 'gptel-model)))
    (gptel--prepare-ask-buffer)
    (letrec ((prompt
              (minibuffer-with-setup-hook
                  (lambda () (add-hook 'completion-at-point-functions
                                  #'gptel-preset-capf nil t)
                    (use-local-map
                     (define-keymap :parent (current-local-map)
                       "<tab>" #'completion-at-point)))
                (read-string
                 (format "Ask %s: " (gptel-backend-name gptel-backend))
                 (and (use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))))))
             (before-resp
              (lambda () (remove-hook 'gptel-pre-response-hook before-resp)
                (when (eq (current-buffer) (get-buffer gptel-ask--buffer-name))
                  (with-current-buffer gptel-ask--buffer-name
                    (goto-char (point-max))
                    (insert-before-markers prompt)
                    (display-buffer gptel-ask--buffer-name))))))
      (add-hook 'gptel-pre-response-hook before-resp)
      (gptel-request prompt
        :buffer (get-buffer-create gptel-ask--buffer-name)
        :position (with-current-buffer gptel-ask--buffer-name (point-max))
        :stream gptel-stream
        :transforms gptel-prompt-transform-functions
        :fsm (gptel-make-fsm :handlers gptel-send--handlers)
        :system (default-value 'gptel--system-message)))))

;;;###autoload
(defun gptel-ask-notify (prompt)
  (interactive
   (list (minibuffer-with-setup-hook
             (lambda () (add-hook 'completion-at-point-functions
                             #'gptel-preset-capf nil t)
               (use-local-map
                (define-keymap :parent (current-local-map)
                  "<tab>" #'completion-at-point)))
           (read-string
            (format "Ask %s: " (gptel-backend-name gptel-backend))))))
  (gptel--prepare-ask-buffer)
  (with-current-buffer gptel-ask--buffer-name
    (insert prompt))
  (let ((inhibit-message t))
    (gptel-with-preset 'default
      (gptel-request prompt
        :system "Answer directly and in brief.
If you don't know say you don't know."
        :transforms gptel-prompt-transform-functions
        :callback
        (lambda (resp info)
          (pcase resp
            ((pred stringp)
             (notifications-notify
              :title "gptel response"
              :body (string-fill resp 66)
              :timeout (* 48 (length resp)))
             (with-current-buffer gptel-ask--buffer-name
               (insert gptel-response-separator resp "\n\n----")))
            (`(tool-call . ,args)
             (cl-loop
              for (tool-spec arg-values process-tool-result) in args
              do (if (gptel-tool-async tool-spec)
                     (apply (gptel-tool-function tool-spec)
                            process-tool-result arg-values)
                   (let ((result
                          (condition-case errdata
                              (apply (gptel-tool-function tool-spec) arg-values)
                            (error (mapconcat #'gptel--to-string errdata " ")))))
                     (funcall process-tool-result result)))))
            ('nil (notifications-notify
                   :title "gptel response error!"
                   :body (prin1-to-string (plist-get info :error))))))))))

(with-eval-after-load 'gptel-transient
  (transient-append-suffix 'gptel-menu '(1 2 "k")
    '("a" "\"Ask\" buffer" "a"))

  (define-advice gptel--suffix-send (:filter-args (args) ask-buffer)
    ";TODO: "
    (when (member "a" (car args))
      (with-current-buffer (get-buffer-create gptel-ask--buffer-name)
        (goto-char (point-max)))
      (setcar args (cons (concat "b" gptel-ask--buffer-name) (car args))))
    args))

(provide 'gptel-ask)
