(use-package org-roam
      :ensure t
      :init (setq org-roam-v2-ack t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (setq org-roam-directory (file-truename "~/Documents/roam/"))
      (setq org-roam-completion-everywhere t)
      (setq org-roam-dailies-directory "journal")
      (org-roam-setup))
;; If using org-roam-protocol
;; (require 'org-roam-protocol)
