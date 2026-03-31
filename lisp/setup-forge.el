;; -*- lexical-binding: t; -*-

;; (if IS-GUIX
;;     (load-library "forge-autoloads")
;;   (elpaca forge)
;;   (elpaca orgit-forge))
(use-package forge
  :ensure t
  :defer
  :config
  (remove-hook 'forge-post-mode-hook 'turn-on-flyspell)
  (add-hook 'forge-post-mode-hook #'jinx-mode)
  ;; FIXME `forge-bug-reference-setup' should do this but doesn't
  (add-hook 'forge-post-mode-hook
            (lambda () (add-hook 'completion-at-point-functions
                            #'forge-topic-completion-at-point nil t)))
  (auth-source-pass-enable)
  (setq forge-bug-reference-remote-files nil
        forge-database-file
        (expand-file-name "forge-database.sqlite" user-cache-directory)
        forge-owned-accounts '(("karthink"))))

(use-package orgit :ensure t :defer)

(use-package orgit-forge :ensure t :defer)

(use-package pr-review :ensure t :defer)

(use-package git-link
  :ensure t
  :defer
  :config
  (defun git-link-tecosaur (hostname dirname filename branch commit start end)
    (format "https://%s/%s/src/%s/%s"
            hostname
            dirname
            (if commit
                (format "commit/%s" commit)
              (format "branch/%s" branch))
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-%s" start end)
                                (format "L%s" start)))))))
  (setf (alist-get "git.tecosaur.net" git-link-remote-alist
                   nil nil #'equal)
        (list 'git-link-tecosaur)))

(provide 'setup-forge)
