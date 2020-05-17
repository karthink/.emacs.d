;;;; Org mode
;;(require 'use-package nil t)

;;----------------------------------------------------------------------
;; ORG
;;----------------------------------------------------------------------
(use-package org
  :defer 10
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("<f5>" . org-capture)
         ("<f6>" . org-agenda))

  :hook ((org-mode . org-toggle-pretty-entities)
         (org-mode . turn-on-org-cdlatex)
         (org-mode . visual-line-mode))
  :init
  (add-hook 'org-load-hook
            '(lambda nil
               (define-key org-mode-map (kbd "C-c C-S-l") 'org-toggle-link-display)
               (define-key org-mode-map (kbd "<C-tab>") nil)
               ;; (define-key org-mode-map (kbd "<C-S-tab>") (lambda () (other-window -1)))
               ;; Org-cdlatex options
               (define-key org-cdlatex-mode-map (kbd "$") 'cdlatex-dollar)
               ))
  ;; Pretty symbols
  ;; (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  ;; Org LaTeX options
  ;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  ;; Enable longline-truncation in org-mode buffers
  ;; (add-hook 'org-mode-hook 'toggle-truncate-lines)

  :config
  ;; General preferences
  (setq-default org-adapt-indentation nil 
                org-cycle-include-plain-lists t 
                org-fontify-done-headline t 
                org-fontify-quote-and-verse-blocks t 
                org-fontify-whole-heading-line t 
                org-footnote-auto-label 'plain 
                org-hidden-keywords nil 
                org-hide-emphasis-markers nil 
                org-hide-leading-stars t 
                org-image-actual-width nil 
                org-pretty-entities-include-sub-superscripts t 
                org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)) 
                org-startup-folded t 
                org-startup-indented t 
                org-startup-with-inline-images nil 
                org-tags-column 0
                org-use-sub-superscripts t
                org-pretty-entities-include-sub-superscripts t
                org-latex-listings t
                org-special-ctrl-a/e t
                org-special-ctrl-k t
                org-log-done 'time
                org-catch-invisible-edits 'smart
                org-use-speed-commands t
                ;; org-eldoc-breadcrumb-separator " → " 
                ;; org-hide-leading-stars-before-indent-mode t 
                ;; org-indent-indentation-per-level 2 
                ;; org-indent-mode-turns-on-hiding-stars t 
                ;; org-list-description-max-indent 4 
                ;; org-pretty-entities nil 
                ;; org-priority-faces '((?a . error) (?b . warning) (?c . success)) 
                ;; org-entities-user '(("flat" "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯")) 
                ;; org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)") (sequence "[ ](T)" "[-](p)" "[?](m)" "|" "[X](D)") (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)")) 
                ;; org-todo-keyword-faces '(("[-]" :inherit (font-lock-constant-face bold)) ("[?]" :inherit (warning bold)) ("WAITING" :inherit bold) ("LATER" :inherit (warning bold))) 
                ;; org-use-sub-superscripts '{}
                )

  ;; My defaults
  (setq org-agenda-files '("~/do.org" "~/org/schedule.org")
        org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . "zathura %s"))
        ;; Larger equations
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Make org use `display-buffer' like every other Emacs citizen.
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

;;;###autoload
  (defun org-cdlatex-pbb (&rest _arg)
    "Execute `cdlatex-pbb' in LaTeX fragments.
  Revert to the normal definition outside of these fragments."
    (interactive "P")
    (if (org-inside-LaTeX-fragment-p)
        (call-interactively 'cdlatex-pbb)
      (let (org-cdlatex-mode)
        (call-interactively (key-binding (vector last-input-event))))))

  (define-key org-cdlatex-mode-map (kbd "(") #'org-cdlatex-pbb)
  (define-key org-cdlatex-mode-map (kbd "[") #'org-cdlatex-pbb)
  (define-key org-cdlatex-mode-map (kbd "{") #'org-cdlatex-pbb)

  ;; `org-pretty-entities-mode' does not work with `org-cdlatex' when
  ;; entering sub/superscripts. The following modification to the
  ;; function `org-raise-scripts' fixes the problem.
;;;###autoload
  (defun org-raise-scripts (limit)
    "Add raise properties to sub/superscripts."
    (when (and org-pretty-entities org-pretty-entities-include-sub-superscripts
               (re-search-forward
                (if (eq org-use-sub-superscripts t)
                    org-match-substring-regexp
                  org-match-substring-with-braces-regexp)
                limit t))
      (let* ((pos (point)) table-p comment-p
             (mpos (match-beginning 3))
             (emph-p (get-text-property mpos 'org-emphasis))
             (link-p (get-text-property mpos 'mouse-face))
             (keyw-p (eq 'org-special-keyword (get-text-property mpos 'face))))
        (goto-char (point-at-bol))
        (setq table-p (looking-at-p org-table-dataline-regexp)
              comment-p (looking-at-p "^[ \t]*#[ +]"))
        (goto-char pos)
        ;; Handle a_b^c
        (when (member (char-after) '(?_ ?^)) (goto-char (1- pos)))
        (unless (or comment-p emph-p link-p keyw-p)
          (put-text-property (match-beginning 3) (match-end 0)
                             'display
                             (if (equal (char-after (match-beginning 2)) ?^)
                                 (nth (if table-p 3 1) org-script-display)
                               (nth (if table-p 2 0) org-script-display)))
          (add-text-properties (match-beginning 2) (match-end 2)
                               (list 'invisible t))
          ;;;; Do NOT hide the {}'s. This is what causes org-cdlatex-tab to fail.
          ;; (when (and (eq (char-after (match-beginning 3)) ?{)
          ;; 	   (eq (char-before (match-end 3)) ?}))
          ;;   (add-text-properties (match-beginning 3) (1+ (match-beginning 3))
          ;; 		       (list 'invisible t))
          ;;   (add-text-properties (1- (match-end 3)) (match-end 3)
          ;; 		       (list 'invisible t)))
          )
        t)))

;; (add-hook 'org-mode-hook '(lambda ()
;;			   (define-key org-mode-map (kbd "C-;") 'org-complete))

  (add-to-list 'org-structure-template-alist '("ma" . "src matlab"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  
  )

(use-package org-agenda
  :after org
  :commands org-agenda
  :config
  (setq org-agenda-files '("~/do.org"
                           "~/.local/share/org/schedule.org"))
  )

(use-package org-agenda
  :after org
  :defer
  :commands (org-agenda)
  :config
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window))

(use-package org-capture
  :after org
  :defer 
  :commands (org-capture make-orgcapture-frame)
  :config
 (add-to-list 'org-capture-after-finalize-hook
             (defun org-capture-after-delete-frame ()
               "If this is a dedicated org-capture frame, delete it after"
               (if (and (equal (frame-parameter nil 'name)
                               "dropdown_org-capture")
                        (not (eq this-command 'org-capture-refile)))
                   (delete-frame))))

 (setq org-capture-templates
        (append org-capture-templates 
                 `(("t" "TODO items")
                   
                   ("tr" "TODO research"
                    entry
                    (file+olp "~/do.org" "Research")
                    "* TODO %? :research: \n  DEADLINE: %^{Do by}t\n  %a\n  %x\n"
                    :prepend t
                    )

                   ("tg" "TODO general"
                    entry
                    (file+olp "~/do.org" "Tasks")
                    "* TODO %?\n  %a\n"
                    :kill-buffer t
                    :prepend t
                    )

                   ("tc" "Config projects"
                    entry
                    (file+olp "~/do.org" "Configuration")
                    "* TODO %? :config:\n  %a\n  %x\n"
                    :kill-buffer t
                    :prepend t
                    )

                   ("tp" "Other Projects"
                    entry
                    (file+olp "~/do.org" "Other Projects")
                    "* %? :projects:\n  %a\n  %x\n"
                    :prepend t
                    :kill-buffer t
                    )
                   )))
;;;###autoload
  (defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "dropdown_org_capture") (window-system . x)))
    (select-frame-by-name "dropdown_org_capture")
    (org-capture)
    (delete-other-windows)
    )
  )


(use-package ox
  :after org
  :commands org-export-dispatch
  :config
  (setq org-html-htmlize-output-type 'css)
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("IEEEtran"
                   "\\documentclass[conference]{IEEEtran}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (setq org-export-with-LaTeX-fragments t))

;;----------------------------------------------------------------------
;; ORG-BABEL
;;----------------------------------------------------------------------
(use-package ob-octave-fix
   :after ob-octave)

(use-package ob
  :after org
  :defer
  :config
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (matlab . t)
                                   (python . t)
                                   (R . t)
                                   (shell . t)
                                   (scheme . t))))


(use-package org-babel-eval-in-repl
  :disabled
  :ensure t
  :after ob
  :init
  ;; (define-key org-mode-map (kbd "C-<return>") 'ober-eval-in-repl)
  ;; (define-key org-mode-map (kbd "C-c C-c") 'ober-eval-block-in-repl)
  (defun org-ctrl-return-around (org-fun &rest args)
    "Run `ober-eval-in-repl' if in source code block and `org-insert-heading-respect-content' otherwise."
    (if (org-in-block-p '("src" "example"))
        (ober-eval-in-repl)
      (apply org-fun args)))
  (advice-add 'org-insert-heading-respect-content :around #'org-ctrl-return-around)
  (defun org-meta-return-around (org-fun &rest args)
    "Run `ober-eval-block-in-repl' if in source code block or example block and `org-meta-return' otherwise."
    (if (org-in-block-p '("src" "example"))
        (ober-eval-block-in-repl)
      (apply org-fun args)))
  (advice-add 'org-meta-return :around #'org-meta-return-around)
  :config
  (add-to-list 'ober-org-babel-type-list
               '("scheme" . (eval-in-repl-geiser eir-eval-in-geiser)))
  (setq eir-jump-after-eval nil))

;; ## Usage
;; (with-eval-after-load "ob"
;;   (require 'org-babel-eval-in-repl)
;;   (define-key org-mode-map (kbd "C-<return>") 'ober-eval-in-repl)
;;   (define-key org-mode-map (kbd "C-c C-c") 'ober-eval-block-in-repl))

;; ## Recommended config (optional):
;; (with-eval-after-load "eval-in-repl"
;;   (setq eir-jump-after-eval nil))

;;----------------------------------------------------------------------
;; ORG-BULLETS
;;----------------------------------------------------------------------
(use-package org-bullets
  :ensure t
  :after org
  :defer 5
  :hook (org-mode . org-bullets-mode))

;;----------------------------------------------------------------------
;; OX-HUGO
;;----------------------------------------------------------------------
(use-package ox-hugo
  :ensure t
  :after (ox org-capture)
  :config
  (setq org-hugo-section "blog")
  (with-eval-after-load 'org-capture

  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "posts.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template)))))

;;----------------------------------------------------------------------
;; OL-NOTMUCH
;;----------------------------------------------------------------------
(use-package ol-notmuch
  :after (notmuch org))

;;----------------------------------------------------------------------
;; ORG-GCAL
;;----------------------------------------------------------------------
(use-package org-gcal
  :after org
  :ensure t
  :commands (org-gcal-sync org-gcal-fetch)
  :init
  (setq org-gcal-client-id my-org-gcal-client-id
        org-gcal-client-secret my-org-gcal-client-secret
        org-gcal-file-alist `((,my-email-address . ,(concat
                                                    (file-name-as-directory org-directory)
                                                    "schedule.org"))))
  )

;;----------------------------------------------------------------------
;; ORG-REVEAL
;;----------------------------------------------------------------------
(use-package org-re-reveal
  :disabled
  :ensure t
  :commands (org-re-reveal-export-to-html
             org-re-reveal-export-to-html-and-browse)
  :config
  (setq org-re-reveal-root "file:///home/karthik/.local/share/git/reveal.js"
        org-re-reveal-subtree-with-title-slide t)
  (add-to-list 'org-structure-template-alist '("R" "#+REVEAL_HTML: ?\n"))
  (use-package org-re-reveal-ref
    :disabled t
    :init (setq org-ref-default-bibliography '("~/Documents/research/control_systems.bib")))
  )

(use-package ox-reveal
  :disabled 
  :ensure t
  :init
  (setq org-reveal-root "file:///home/karthik/.local/share/git/reveal.js")
  (setq org-reveal-hlevel 2))

;;----------------------------------------------------------------------
;; DRAWING INTEGRATION
;;----------------------------------------------------------------------
(use-package inkscape-figures
  :after org
  :bind (:map org-mode-map
              ("C-c i" . #'+inkscape-figures-create-at-point-org)
              ("C-c e" . #'+inkscape-figures-edit)))

(provide 'setup-org)

;;----------------------------------------------------------------------
;; ORG-ROAM
;;----------------------------------------------------------------------
(use-package org-roam
  :after org
  :commands org-roam-mode
  :defer
  :config
  (setq org-roam-directory (dir-concat org-directory "roam"))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

  (use-package company-org-roam
  :load-path "~/.local/share/git/company-org-roam/"
  :after (company org org-roam)
  :init
  (add-hook 'org-roam-mode-hook (lambda ()
                                  (make-local-variable 'company-backends)
                                  (push 'company-org-roam company-backends)
                                  )))
