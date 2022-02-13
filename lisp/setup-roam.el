(use-package org-roam
      :straight t
      :init (setq org-roam-v2-ack t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ("C-c n r" . my/org-roam-node-from-cite)
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
      
      ;; (("d" "default" plain "%?" :target
      ;; (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      ;; :unnarrowed t))
      (setq org-roam-capture-templates
            '(("d" "default" plain "%?"
               :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                  "#+title: ${title}\n")
               :immediate-finish t
               :unnarrowed t)
              ("r" "reference" plain "%?"
               :if-new
               (file+head "reference/${title}.org" "#+title: ${title}\n")
               :immediate-finish t
               :unnarrowed t)))
      
      (cl-defmethod org-roam-node-type ((node org-roam-node))
        "Return the TYPE of NODE."
        (condition-case nil
            (file-name-nondirectory
             (directory-file-name
              (file-name-directory
               (file-relative-name (org-roam-node-file node) org-roam-directory))))
          (error "roam")))
      
      (setq org-roam-node-display-template
            (concat "${type:10}" "${title:*} "
                    (propertize "${tags:30}" 'face 'org-tag)))
      
      (defun my/org-roam-node-from-cite (keys-entries)
        "Create node from cite entry."
        (interactive (list
                      (let ((citar-bibliography (concat
                                                 (file-name-as-directory org-roam-directory)
                                                 "biblio.bib")))
                        (citar-select-ref :multiple nil :rebuild-cache t))))
        (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                    "${author} :: ${title}")))
          (org-roam-capture- :templates
                             '(("r" "reference" plain "%?" :if-new
                                (file+head "reference/${citekey}.org"
                                           ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                                :immediate-finish t
                                :unnarrowed t))
                             :info (list :citekey (car keys-entries))
                             :node (org-roam-node-create :title title)
                             :props '(:finalize find-file))))

      (org-roam-setup))
;; If using org-roam-protocol
;; (require 'org-roam-protocol)

(provide 'setup-roam)
