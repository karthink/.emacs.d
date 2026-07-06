;; -*- lexical-binding: t; -*-

;; (if IS-GUIX
;;     (load-library "forge-autoloads")
;;   (elpaca forge)
;;   (elpaca orgit-forge))
(use-package forge
  :ensure t
  :defer
  :bind ( :map forge-pullreq-section-map
          ("M-RET" . my/pr-review-at-point))
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
        forge-owned-accounts '(("karthink")))
  (defun my/pr-review-at-point ()
    (interactive)
    (require 'pr-review)
    (if-let* ((url (pr-review--find-url-in-buffer)))
        (pr-review url current-prefix-arg))
    (message "No PR URL found at point")))

(use-package orgit :ensure t :defer)

(use-package orgit-forge :ensure t :defer)

(use-package pr-review
  :ensure t
  :defer
  :config
  (define-advice pr-review--find-url-in-buffer
      (:after-until () magit-buffer)
    (and-let* ((_ (featurep 'forge))
               (target (forge--browse-target)))
      (if (stringp target) target (forge-get-url target)))))

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

(use-package remoto
  :ensure (:host github :repo "agzam/remoto.el")
  :defer)

(provide 'setup-forge)
