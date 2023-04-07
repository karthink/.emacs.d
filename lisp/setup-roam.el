;;; setup-roam

(use-package org-roam
      :straight t
      :init (setq org-roam-v2-ack t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ("C-c n r" . my/org-roam-node-from-cite))
      :config
      (setf (alist-get "^\\*org-roam\\*$" display-buffer-alist
                       nil nil #'equal)
            '((display-buffer-reuse-window
               display-buffer-reuse-mode-window
               display-buffer-below-selected)
              (window-height . (lambda (win)
                                 (fit-window-to-buffer
                                  win 30)))))
      (setq org-roam-directory (file-truename "~/Documents/roam/"))
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

      (org-roam-db-autosync-enable))
;; If using org-roam-protocol
;; (require 'org-roam-protocol)

(use-package org-roam
  :after (org-roam embark)
  :config
  (add-to-list 'embark-keymap-alist
               '(org-roam-node . embark-org-roam-node-map))

  (defvar embark-org-roam-node-map
    (let ((map (make-sparse-keymap)))
      (define-key map "i" #'org-roam-node-insert)
      (define-key map "f" #'org-roam-node-find)
      (define-key map "o" (my/embark-ace-action org-roam-node-find))
      (define-key map "2" (my/embark-split-action org-roam-node-find my/split-window-below))
      (define-key map "3" (my/embark-split-action org-roam-node-find my/split-window-right))
      map)))

(use-package org-roam-dailies
  :after org-roam
  :commands org-roam-dailies-goto-today
  :bind-keymap ("C-c n j" . org-roam-dailies-map)
  :config
  (setq org-roam-dailies-directory "journal")
  ;; Redefine org-roam-dailies-map, the default bindings are terrible.
  (pcase-dolist
      (`(,key ,cmd)
       '(("j" org-roam-dailies-goto-today)
         ("n" org-roam-dailies-goto-next-note)
         ("p" org-roam-dailies-goto-previous-note)
         ("r" org-roam-dailies-capture-today)
         ;; ("" org-roam-dailies-capture-date)
         ("c" org-roam-dailies-goto-date)
         ("." org-roam-dailies-find-directory)
         ("f" org-roam-dailies-goto-tomorrow)
         ("b" org-roam-dailies-goto-yesterday)))
    (define-key org-roam-dailies-map (kbd key) cmd))
  (defvar org-roam-dailies-repeat-map
    (let ((map (make-sparse-keymap)))
      (prog1 map
        (pcase-dolist
            (`(,key ,cmd)
             '(("n" org-roam-dailies-goto-next-note)
               ("p" org-roam-dailies-goto-previous-note)
               ("." org-roam-dailies-find-directory)))
          (define-key map key cmd)
          (put cmd 'repeat-map 'org-roam-dailies-repeat-map))))))

(use-package org-roam-ui
  :straight t
  :defer
  :config
  (setq org-roam-ui-sync-theme t))

(provide 'setup-roam)
