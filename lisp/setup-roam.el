(use-package org-roam
      :ensure t
      :init (setq org-roam-v2-ack t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert-immediate)
             ("C-c n I" . org-roam-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (setq org-roam-directory (file-truename "~/Documents/roam/"))
      (setq org-roam-completion-everywhere t)
      (setq org-roam-dailies-directory "journal")
      
      (defun org-roam-node-insert-immediate (arg &rest args)
        (interactive "P")
        (let ((args (cons arg args))
              (org-roam-capture-templates
               (list (append (car org-roam-capture-templates)
                             '(:immediate-finish t)))))
          (apply #'org-roam-node-insert args)))

      (org-roam-setup))
;; If using org-roam-protocol
;; (require 'org-roam-protocol)

(provide 'setup-roam)
