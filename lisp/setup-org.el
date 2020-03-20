;;;; Org mode

(require 'use-package nil t)

;;----------------------------------------------------------------------
;; ORG
;;----------------------------------------------------------------------
(use-package org
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("<f5>" . org-capture)
         ("<f6>" . org-agenda))

  :init
  (add-hook 'org-load-hook
            '(lambda nil
               (define-key org-mode-map (kbd "C-c C-S-l") 'org-toggle-link-display)
               (define-key org-mode-map (kbd "<C-tab>") 'other-window)
               (define-key org-mode-map (kbd "<C-S-tab>") (lambda () (other-window -1)))
               ;; Org-cdlatex options
               (define-key org-cdlatex-mode-map (kbd "$") 'cdlatex-dollar)))

  (setq org-directory "~/.local/share/org")

  ;; Pretty symbols
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities)

  ;; Org LaTeX options
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

  ;; Enable longline-truncation in org-mode buffers
  (add-hook 'org-mode-hook 'toggle-truncate-lines)


  :config
  (setq org-agenda-files '("~/do.org" "~/.local/share/org/schedule.org"))
  (setq org-log-done 'time)
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . "zathura %s")))

;; (global-set-key "\C-cb" 'org-iswitchb)

  ;; Hide all stars except the last one on each line:
  (setq org-hide-leading-stars 1)

  ;; Avoid invisible edits
  (setq org-catch-invisible-edits 'show)
  ;; (setq org-catch-invisible-edits 'smart)


;;; Org-agenda mode
  ;; (defvar org-agenda-files nil)
  ;; (setq org-agenda-files (cons "~/notes/" org-agenda-files))
  ;; (setq org-agenda-restore-windows-after-quit 1)

;;(add-hook 'org-mode-hook '(lambda ()
;;			   (define-key org-mode-map (kbd "C-;") 'org-complete))
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
;; Completion

  )

(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
        (append  org-capture-templates 
                 `(("t" "TODO items")
                   
                   ("tr" "TODO research"
                    entry
                    (file+olp "~/do.org" "Research")
                    "* TODO %? :research: \n  DEADLINE: %^{Do by}t\n  %a\n  %x\n"
                    :prepend t)

                   ("tg" "TODO general"
                    entry
                    (file+olp "~/do.org" "Tasks")
                    "* TODO %?\n  %a\n"
                    :kill-buffer t
                    :prepend t
                    ))))
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
;; ORG-BABEL-EVAL-IN-REPL
;;----------------------------------------------------------------------
(use-package org-babel-eval-in-repl
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
  :hook (org-mode . org-bullets-mode))

;;----------------------------------------------------------------------
;; OX-HUGO
;;----------------------------------------------------------------------
(use-package ox-hugo
  :ensure t
  :after ox
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
(with-eval-after-load 'notmuch
  (require 'ol-notmuch nil t))

;;----------------------------------------------------------------------
;; ORG-GCAL
;;----------------------------------------------------------------------
(use-package org-gcal
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
  :disabled t
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

;; (use-package ox-reveal
;;   ;; :ensure t
;;   :init
;;   (setq org-reveal-root "file:///home/karthik/.local/share/git/reveal.js")
;;   (setq org-reveal-hlevel 2))

;; Some formatting
;; (setq org-blank-before-new-entry
;;       '((heading . t) (plain-list-item . nil)))

(provide 'setup-org)
