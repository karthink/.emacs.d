(use-package project
    :init
    (setq project-switch-commands
          '((?f "Find file" project-find-file)
            (?g "Find regexp" project-find-regexp)
            (?d "Dired" project-dired)
            (?b "Buffer" project-switch-to-buffer)
            (?q "Query replace" project-query-replace-regexp)
            (?v "VC-Dir" project-vc-dir)
            (?k "Kill buffers" project-kill-buffers)
            (?! "Shell command" project-shell-command)
            (?e "Eshell" project-eshell)))
    
    (cl-defgeneric project-root (project)
      "Return root directory of the current project.

It usually contains the main build file, dependencies
configuration file, etc. Though neither is mandatory.

The directory name must be absolute."
      (car project))
    
    :config
    (setq project-list-file (dir-concat user-cache-directory "projects"))

    ;; Use =fd= instead of =find= in non-VC projects (if available)
    (when (executable-find "fd")
      (defun my/project-files-in-directory (dir)
        "Use `fd' to list files in DIR`"
        (let* ((default-directory dir)
               (localdir (file-local-name (expand-file-name dir)))
               (command (format "fd -t f -0 . %s" localdir)))
          (project--remote-file-names
           (sort (split-string (shell-command-to-string command) "\0" t)
                 #'string<))))

      (cl-defmethod project-files ((project (head vc)) &optional dirs)
        (mapcan
         (lambda (dir)
           (let (backend)
             (if (and (file-equal-p dir (cdr project))
                      (setq backend (vc-responsible-backend dir))
                      (cond
                       ((eq backend 'Hg))
                       ((and (eq backend 'Git)
                             (or
                              (not project-vc-ignores)
                              (version<= "1.9" (vc-git--program-version)))))))
                 (project--vc-list-files dir backend project-vc-ignores)
               (my/project-files-in-directory dir)
               )))
         (or dirs
             (list (project-root project))))))
    
    (defun my/project-remove-project ()
      "Remove project from `project--list' using completion."
      (interactive)
      (project--ensure-read-project-list)
      (let* ((projects project--list)
             (dir (completing-read "REMOVE project from list: " projects nil t)))
        (setq project--list (delete (assoc dir projects) projects))))

    (setq project-window-list-file (dir-concat user-cache-directory "project-window-list"))

    :bind-keymap ("H-p" . project-prefix-map)
    :bind (("C-x p q" . project-query-replace-regexp) ; C-x p is `project-prefix-map'
           ("C-x p <delete>" . my/project-remove-project)
           ("C-x p DEL" . my/project-remove-project)
           ;; ("M-s p" . my/project-switch-project)
           ;; ("M-s f" . my/project-find-file-vc-or-dir)
           ("M-s L" . find-library)))

(use-package project-x
  :load-path "~/.local/share/git/project-x/"
  :after project
  :config
  (setq project-x-window-list-file (dir-concat user-cache-directory "project-window-list")
        project-x-save-interval 600)
  (project-x-mode 1))

(provide 'setup-project)
