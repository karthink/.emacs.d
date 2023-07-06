(use-package beframe
  :straight t
  :hook (beframe-mode . my/consult-beframe-settings)
  :config
  (setq beframe-functions-in-frames
        '(project-switch-project)
        beframe-rename-function #'ignore)
  (defvar consult--source-beframe
      (list :name     "Frame Buffers"
            :narrow   ?w
            :category 'buffer
            :face     'consult-buffer
            :history  'beframe-history
            :default  t
            :preview-key "M-RET"
            :state    #'consult--buffer-state
            :items    #'beframe-buffer-names
            :action   #'switch-to-buffer)

      "Set workspace buffer list for consult-buffer.")
  (defun my/consult-switch-beframe (buf-name &optional norecord)
    (cl-loop with buffer = (get-buffer buf-name)
             for frame in (frame-list)
             for frame-buffers = (beframe--frame-buffers frame)
             when (and (not (frame-parent frame))
                       (not (frame-parameter frame 'explicit-name))
                       (memq buffer frame-buffers)
                       (not (eq frame (selected-frame))))
             do
             (select-frame-set-input-focus frame)
             and return
             (display-buffer
              buffer
              '((display-buffer-reuse-window
                 display-buffer-use-some-window)
                . ((body-function . select-window))))
             finally return
             (switch-to-buffer buffer norecord)))
  (use-package embark
    :defer
    :config
    (define-key embark-buffer-map (kbd "fu")
      (defun my/beframe-unassume-buffer (buf)
        (interactive "bUnassume: ")
        (beframe--unassume
         (list (get-buffer buf)))))
    (define-key embark-buffer-map (kbd "fa")
      (defun my/beframe-assume-buffer (buf)
        (interactive "bAssume: ")
        (beframe--assume
         (list (get-buffer buf))))))

  (defun my/consult-beframe-settings ()
    "Deactivate isolated buffers when not using beframe."
    (require 'consult)
    (cond (beframe-mode
           ;; hide full buffer list (still available with "b")
           (consult-customize consult--source-buffer :hidden t :default nil
                              :preview-key "M-RET")
           (add-to-list 'consult-buffer-sources 'consult--source-beframe)
           (setq consult--buffer-display #'my/consult-switch-beframe))
          (t
           ;; reset consult-buffer to show all buffers 
           (consult-customize consult--source-buffer
                              :hidden nil :default t :preview-key "M-RET"
                              :action nil)
           (setq consult-buffer-sources
                 (remove #'consult--source-beframe consult-buffer-sources))
           (setq consult--buffer-display #'switch-to-buffer)))))

(provide 'setup-beframe)
