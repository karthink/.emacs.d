;;;; Org mode
;;(require 'use-package nil t)

;;----------------------------------------------------------------------
;; ORG
;;----------------------------------------------------------------------
(use-package org
  :defer 10
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("<f5>"  . org-capture)
         ("<f6>"  . org-agenda)
         :map org-mode-map
         ("C-c C-x +" . my/org-strike-through-heading)
         ("C-,"   . nil)
         ("C-'"   . nil))

  :hook ((org-mode . org-toggle-pretty-entities)
         (org-mode . turn-on-org-cdlatex)
         (org-mode . visual-line-mode))
  ;; :custom-face 
  ;; (org-level-1 ((t (:height 1.2 :inherit (outline-1 variable-pitch)))))
  ;; (org-level-2 ((t (:height 1.1 :inherit (outline-2 variable-pitch)))))
  ;; (org-document-title ((t (:height 1.5))))
  :init
  (add-hook 'org-load-hook
            '(lambda nil
               (define-key org-mode-map (kbd "C-c C-S-l") 'org-toggle-link-display)
               ;; (define-key org-mode-map (kbd "<C-tab>") nil)
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
                org-image-actual-width 400 
                org-pretty-entities-include-sub-superscripts t 
                ;; org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 3)) 
                org-refile-targets '((nil :level . 1) (org-agenda-files :todo . "PROJECT"))
                org-refile-target-verify-function nil
                org-refile-use-outline-path nil
                org-refile-use-cache nil
                org-refile-allow-creating-parent-nodes t
                org-startup-folded t 
                org-startup-indented t 
                org-startup-with-inline-images nil 
                org-use-tag-inheritance nil
                org-tags-column -80
                org-use-sub-superscripts t
                org-pretty-entities-include-sub-superscripts t
                org-latex-listings t
                org-special-ctrl-a/e t
                org-special-ctrl-k t
                org-log-done 'time
                org-catch-invisible-edits 'smart
                org-use-speed-commands t
                org-speed-commands-user '(("z" . org-add-note))
                org-highlight-latex-and-related '(native)
                org-imenu-depth 7
                org-id-link-to-org-use-id 'create-if-interactive
                org-extend-today-until 3
                org-id-locations-file "~/.cache/emacs/org-id-locations"
                org-default-notes-file "~/org/do.org"
                org-M-RET-may-split-line '((headline) (default . t))
                org-fast-tag-selection-single-key 'expert
                org-return-follows-link t
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
  
  (defun save-org-mode-files ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'org-mode)
          (if (and (buffer-modified-p) (buffer-file-name))
              (save-buffer))))))

  (run-with-idle-timer 120 t 'save-org-mode-files)

  (setq org-todo-keyword-faces
        '(;; ("TODO"    :foreground "#6e90c8" :weight bold)
          ("WAITING" :foreground "red" :weight bold)
          ("MAYBE"   :foreground "#6e8996" :weight bold)
          ("PROJECT" :foreground "#088e8e" :weight bold)))

  ;; My defaults
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . "zathura %s"))
        ;; Larger equations
        org-format-latex-options (plist-put org-format-latex-options :scale 1.65))

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

  (defun my/org-strike-through-heading (&optional arg)
    "Strike through heading text of current Org item."
    (interactive "P")
    (save-excursion
      (unless (org-at-heading-p)
        (org-previous-visible-heading 1))
      (when (org-at-heading-p)
        (let ((org-special-ctrl-a/e t))
          (org-beginning-of-line)
          (insert "+")
          (org-end-of-line)
          (insert "+")))))
;; (add-hook 'org-mode-hook '(lambda ()
;;			   (define-key org-mode-map (kbd "C-;") 'org-complete))

  (add-to-list 'org-structure-template-alist '("ma" . "src matlab"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  
  )


(use-package org-src
  :after org
  :config
  (setq-default
   org-src-tab-acts-natively t))

(use-package org-clock
  :defer
  :after org
  :config
  (setq-default org-clock-idle-time 60
                org-clock-out-remove-zero-time-clocks t
                org-clock-mode-line-total 'today))

(use-package org-habit
  :after org-agenda
  :config
  (setq org-habit-preceding-days 42)
  
  (defvar my/org-habit-show-graphs-everywhere nil
    "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil variable to show
consistency graphs in all Org mode agendas.")

  (defun my/org-agenda-mark-habits ()
    "Mark all habits in current agenda for graph display.

This function enforces `my/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `my/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
    (when (and my/org-habit-show-graphs-everywhere
               (not (get-text-property (point) 'org-series)))
      (let ((cursor (point))
            item data) 
        (while (setq cursor (next-single-property-change cursor 'org-marker))
          (setq item (get-text-property cursor 'org-marker))
          (when (and item (org-is-habit-p item)) 
            (with-current-buffer (marker-buffer item)
              (setq data (org-habit-parse-todo item))) 
            (put-text-property cursor
                               (next-single-property-change cursor 'org-marker)
                               'org-habit-p data))))))

  (advice-add #'org-agenda-finalize :before #'my/org-agenda-mark-habits))


(use-package org-agenda
  :after org
  :commands org-agenda
  :hook (org-agenda-finalize . hl-line-mode)
  :bind (:map org-agenda-mode-map
              ("D" . org-agenda-day-view)
              ("W" . org-agenda-week-view)
              ("w" . org-agenda-refile))
  :config
  (setq org-agenda-files '("~/Documents/org/do.org"
                           "~/Documents/org/gmail-cal.org"
                           "~/Documents/org/ucsb-cal.org"
                           "~/karthinks/posts.org"))
  (setq-default
   org-agenda-span 2
   org-agenda-restore-windows-after-quit t
   org-agenda-window-setup 'current-window
   org-stuck-projects '("TODO=\"PROJECT\"" ("TODO" "DEFERRED") nil "")
   org-agenda-use-time-grid nil
   org-agenda-todo-ignore-scheduled nil
   org-agenda-text-search-extra-files nil ;'(agenda-archives)
   org-agenda-tags-column 'auto
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-show-all-dates nil
   org-agenda-inhibit-startup t
   org-agenda-include-diary nil
   org-agenda-follow-indirect t
   org-agenda-default-appointment-duration 60)

  (defun org-todo-age (&optional pos)
    (if-let* ((entry-age (org-todo-age-time pos))
              (days (time-to-number-of-days entry-age)))
        (cond
         ((< days 1)   "today")
         ((< days 7)   (format "%dd" days))
         ((< days 30)  (format "%.1fw" (/ days 7.0)))
         ((< days 358) (format "%.1fM" (/ days 30.0)))
         (t            (format "%.1fY" (/ days 365.0))))
      ""))

  (defun org-todo-age-time (&optional pos)
    (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
      (when stamp
        (time-subtract (current-time)
                       (org-time-string-to-time stamp)))))

  (defun org-current-is-todo ()
    (member (org-get-todo-state) '("TODO" "STARTED")))
  
  (defun my/org-agenda-should-skip-p ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (when (or (org-get-scheduled-time (point))
              (org-get-deadline-time (point)))
      (setq should-skip-entry t))
    (when (/= (point)
              (save-excursion
                (org-goto-first-child)
                (point)))
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (and (org-current-is-todo)
                   (not (org-get-scheduled-time (point)))
                   (not (org-get-deadline-time (point))))
          (setq should-skip-entry t))))
    should-skip-entry))
  
  (defun my/org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (when (my/org-agenda-should-skip-p)
    (or (outline-next-heading)
        (goto-char (point-max)))))
  
  (setq org-agenda-custom-commands

        '(("n" "Project Next Actions" alltodo ""
           ((org-agenda-overriding-header "Project Next Actions")
            (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))

          ("P" "All Projects" tags "TODO=\"PROJECT\"&LEVEL>1"
           ((org-agenda-overriding-header "All Projects")))

          ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
           ((org-agenda-overriding-header "Uncategorized items")))

          ("W" "Waiting tasks" tags "W-TODO=\"DONE\"|TODO={WAITING\\|DELEGATED}"
           ((org-agenda-overriding-header "Waiting/delegated tasks:")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
            (org-agenda-sorting-strategy '(todo-state-up priority-down category-up))))

          ("D" "Deadlined tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
           ((org-agenda-overriding-header "Deadlined tasks: ")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
            (org-agenda-sorting-strategy '(category-up))))

          ("S" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{APPT\\|DONE\\|CANCELED\\|NOTE\\|PROJECT}&STYLE<>\"habit\""
           ((org-agenda-overriding-header "Scheduled tasks: ")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled))
            (org-agenda-sorting-strategy '(category-up))
            (org-agenda-prefix-format "%-11c%s ")))
          
          ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT\\|DEFERRED\\|MAYBE}"
           ((org-agenda-overriding-header "Unscheduled tasks: ")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
            (org-agenda-files '("~/org/do.org"))))

          ("~" "Maybe tasks" tags "TODO=\"MAYBE\""
           ((org-agenda-overriding-header "Maybe tasks:")
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
            ;; (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
            ))

          ("K" "Habits" tags "STYLE=\"habit\""
           ((my/org-habit-show-graphs-everywhere t)
            (org-agenda-overriding-header "Habits:")
            (org-habit-show-all-today t)))
          
          )))

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

 (add-to-list 'org-capture-templates `("t" "Add task"
                    entry
                    (file+headline "~/do.org" "Tasks")
                    "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:
%a\n%x\n"
                    :prepend t))

 ;; (setq org-capture-templates
;;         (append org-capture-templates 
;;                  `(("t" "TODO items")
                   
;;                    ("tr" "TODO research"
;;                     entry
;;                     (file+olp "~/do.org" "Research")
;;                     "* TODO %? :research:
;; SCHEDULED: %^{Do by}t
;; :PROPERTIES:
;; :ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
;; :END:
;; %a\n%x\n"
;;                     :prepend t)

;;                    ("tg" "Add task" entry
;;                     (file+headline "~/do.org" "Tasks")
;;                     "* TODO %?
;; :PROPERTIES:
;; :ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
;; :END:
;; %a
;; %x\n" :kill-buffer t :prepend t)

;;                    ("tc" "Config projects"
;;                     entry
;;                     (file+headline "~/do.org" "Configuration")
;;                     "* TODO %? :config:
;; :PROPERTIES:
;; :ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
;; :END:
;; %a\n%x\n"
;;                     :kill-buffer t :prepend t)

;;                    ("tp" "Other Projects"
;;                     entry
;;                     (file+headline "~/do.org" "Other Projects")
;;                     "* %? :project:
;; :PROPERTIES:
;; :ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
;; :END:
;; %a\n%x\n"
;;                     :prepend t :kill-buffer t))))
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

(use-package valign
  :disabled
  :if (file-exists-p "~/.local/share/git/valign")
  :load-path "~/.local/share/git/valign/"
  :hook (org-mode . valign-mode)
  :after org
  )

(use-package themed-ltximg
  :after org)

;;----------------------------------------------------------------------
;; ORG-DOWNLOAD
;;----------------------------------------------------------------------
(use-package org-download
  :ensure t
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :config
  ;; (setq org-download-backend (if IS-LINUX "curl" "url-retrieve"))
  (setq org-download-heading-lvl nil)
  (setq org-download-backend 'curl)
  (setq org-download-image-attr-list
        '("#+attr_html: :width 70% :align center"
         "#+attr_org: :width 100px"
         "#+attr_latex: :width 0.6\textwidth")))

;;----------------------------------------------------------------------
;; ORG-BABEL
;;----------------------------------------------------------------------
(use-package ob-octave-fix
   :after ob-octave)

(use-package ob
  :after org
  :defer
  :config
  (setq org-src-window-setup 'split-window-below
        org-confirm-babel-evaluate nil
        org-export-use-babel nil)
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (matlab . t)
                                   (octave . t)
                                   (python . t)
                                   (R . t)
                                   (shell . t)
                                   (scheme . t)
                                   (ditaa . t)))
  (setq org-ditaa-jar-path "/usr/bin/ditaa")
  (defun my/org-babel-goto-tangle-file ()
    (if-let* ((args (nth 2 (org-babel-get-src-block-info t)))
              (tangle (alist-get :tangle args)))
        (unless (equal "no" tangle)
          (find-file tangle)
          t)))
  (add-hook 'org-open-at-point-functions 'my/org-babel-goto-tangle-file))


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
  :hook (org-mode . org-bullets-mode))

;;----------------------------------------------------------------------
;; OX-HUGO
;;----------------------------------------------------------------------
(use-package ox-hugo
  :ensure t
  :after (org-capture)
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
                   "\n:PROPERTIES:"
                   ,(concat "\n:EXPORT_FILE_NAME: " fname)
                   "\n:ID: "
                   ,(shell-command-to-string "uuidgen")
                   ":END:"
                   "\n%?\n")          ;Place the cursor here finally
                 "")))

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
  :ensure
  :after org
  :commands (org-gcal-sync org-gcal-fetch my/org-gcal-sync-maybe)
  :hook (org-agenda-mode . my/org-gcal-sync-maybe)
  :config
  (setq org-gcal-dir "~/.cache/emacs/org-gcal/")
  ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  (setq org-gcal-client-id my-org-gcal-client-id
        org-gcal-client-secret my-org-gcal-client-secret
        org-gcal-up-days 180
        org-gcal-remove-api-cancelled-events t
        org-gcal-notify-p nil
        org-gcal-file-alist
        `((,my-email-address . ,(concat
                                 (file-name-as-directory org-directory)
                                 "gmail-cal.org"))
          (,(car my-alt-email-addresses) . ,(concat
                                             (file-name-as-directory org-directory)
                                             "ucsb-cal.org")))
        org-gcal-recurring-events-mode 'top-level)

  (defvar my/org-gcal--last-sync-time 0
    "Last time `org-gcal-sync' was run.")
  (defun my/org-gcal-sync-maybe (&optional skip-export silent)
  "Import events from calendars if more than 30 minutes have
 passed since last import.
Export the ones to the calendar if unless
SKIP-EXPORT.  Set SILENT to non-nil to inhibit notifications."
  (interactive)
  (if-let ((now (float-time (current-time)))
           (sync-p (or (< (- now my/org-gcal--last-sync-time)
                          3600)
                       ;; (seq-some (lambda (cal-file-pair)
                       ;;             (< (- now
                       ;;                   (float-time (file-attribute-modification-time
                       ;;                                (file-attributes (cdr cal-file-pair)))))
                       ;;                1800))
                       ;;    org-gcal-file-alist)
                       )))
      (message "Did not check for calendar updates. `M-x org-gcal-sync' to force check.")
    (org-gcal-sync skip-export silent)
    (message "Updated gcal.")
    (setq my/org-gcal--last-sync-time now))))

;;----------------------------------------------------------------------
;; ORG-REVEAL
;;----------------------------------------------------------------------
(use-package org-re-reveal
  ;; :disabled
  :ensure t
  :after ox
  :commands (org-re-reveal-export-to-html
             org-re-reveal-export-to-html-and-browse)
  :config
  (setq org-re-reveal-root "/home/karthik/.local/share/git/reveal.js"
        org-re-reveal-subtree-with-title-slide t)
  (add-to-list 'org-structure-template-alist '("R" . "#+REVEAL_HTML: ?\n"))
  (use-package org-re-reveal-ref
    :disabled
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

;;----------------------------------------------------------------------
;; ORG-REF
;;----------------------------------------------------------------------
(use-package org-ref
  :disabled
  :defer
  :config
  ;; (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f")
        )
  (setq org-latex-pdf-process
        (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq org-ref-bibliography-notes nil
        org-ref-default-bibliography reftex-default-bibliography
        org-ref-pdf-directory "~/Documents/research/lit/")
  )
;;----------------------------------------------------------------------
;; MY ORG PROJECTS
;;----------------------------------------------------------------------
(use-package ox-publish
  :defer
  :config
  (setq org-publish-project-alist
        '(
          ("dai-wiki"
           :base-directory "~/Documents/abode/dai/"
           :base-extension "org"
           :publishing-directory "~/Documents/abode/dai"
           :remote-directory "root@abode.karthinks.com:/var/www/abode/dai"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           :completion-function (list my/org-publish-rsync)
           )
          ("dai" :components ("dai-wiki"))
          ("cyclostationarity"
           :base-directory "~/Dropbox/KarthikBassam/Cyclostationarity"
           :base-extension "org"
           :publishing-directory "~/Dropbox/KarthikBassam/Cyclostationarity"
           :remote-directory "root@abode.karthinks.com:/var/www/abode/cyclostationarity"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :auto-preamble t
           :completion-function (list my/org-publish-rsync-cyclostationarity)
           )
          )
        )

  (defun my/org-publish-rsync (project-plist)
    "Sync output of org project to a remote server using RSYNC. All files and folders except for ORG files will be synced."
    (if (executable-find "rsync")
        (let* ((basedir (expand-file-name
                         (file-name-as-directory
                          (plist-get project-plist :base-directory))))
               (destdir (plist-get project-plist :remote-directory)))
          (start-process "rsync-project-dai" "*project-dai-output*"
                         "rsync" "-a" "-v" "--exclude=*.org" "--delete"
                         basedir destdir))

      (display-warning 'org-publish
                       "Could not find RSYNC in PATH. Project not uploaded to server."
                       :warning)))

  (defun my/org-publish-rsync-cyclostationarity (project-plist)
    "Sync output of org project to a remote server using RSYNC. All files and folders except for ORG files will be synced."
    (if (executable-find "rsync")
        (let* ((basedir (expand-file-name
                         (file-name-as-directory
                          (plist-get project-plist :base-directory))))
               (destdir (plist-get project-plist :remote-directory)))
          (start-process "rsync-project-cyclostationarity" "*project-cyclostationarity-output*"
                         "rsync" "-a" "-v" 
                         "--include=*.html"
                         "--include=/figures/***"
                         "--exclude=*"
                         "--delete"
                         basedir destdir))

      (display-warning 'org-publish
                       "Could not find RSYNC in PATH. Project not uploaded to server."
                       :warning)))
  )

;;----------------------------------------------------------------------
;; ORG-ROAM - Disabled for now
;;----------------------------------------------------------------------
(use-package org-roam
  :disabled
  :after org
  :commands (org-roam-mode org-roam-find-file)
  :init
  (setq org-roam-directory (dir-concat org-directory "roam"))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :config
  (setq org-roam-capture-templates '(("d" "default" plain
                                     #'org-roam-capture--get-point
                                     "%?"
                                     :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                     :head "#+TITLE: ${title}\n#+ROAM_TAGS:\n#+ROAM_KEY:\n"
                                     :unnarrowed t))))

  (use-package company-org-roam
  :disabled
  :load-path "~/.local/share/git/company-org-roam/"
  :after (company org org-roam)
  :init
  (add-hook 'org-roam-mode-hook (lambda ()
                                  (make-local-variable 'company-backends)
                                  (push 'company-org-roam company-backends)
                                  )))

;;----------------------------------------------------------------
;; ORG-TREE-SLIDE - Presentations from within org-mode
;;----------------------------------------------------------------
(use-package org-tree-slide
  :ensure t
  :after org
  :commands my/org-presentation-mode
  :config
  (setq org-tree-slide-never-touch-face nil
        org-tree-slide-skip-outline-level 8
        org-tree-slide-heading-emphasis nil
        org-tree-slide-cursor-init t
        org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message
        (propertize "ORG PRESENTATION STARTED" 'face 'success)
        org-tree-slide-deactivate-message
        (propertize "ORG PRESENTATIONS STOPPED" 'face 'error))

  (define-minor-mode my/org-presentation-mode
    "Parameters for plain text presentations with `org-mode'."
    :init-value nil
    :global nil
    (if my/org-presentation-mode
        (progn
          (unless (eq major-mode 'org-mode)
            (user-error "Not in an Org buffer"))
          (org-tree-slide-mode 1)
          (my/olivetti-mode 1)
          ;; (org-indent-mode 1)
          (text-scale-increase 3))
      (org-tree-slide-mode -1)
      (my/olivetti-mode -1)
      ;; (org-indent-mode)
      (text-scale-decrease 3)
      (text-scale-mode -1)))

  :bind (("C-c P"      . my/org-presentation-mode)
         :map org-tree-slide-mode-map
         ("<next>" . org-tree-slide-move-next-tree)
         ("<prior>" . org-tree-slide-move-previous-tree)
         ("<home>" . 'org-tree-slide-display-header-toggle)
         ("<C-down>"  . org-tree-slide-display-header-toggle)
         ("<C-right>" . org-tree-slide-move-next-tree)
         ("<C-left>"  . org-tree-slide-move-previous-tree))
  )

;;----------------------------------------------------------------
;; ORG-MIME - Compose HTML emails in org-mode
;;----------------------------------------------------------------
(use-package org-mime
  :ensure t
  :after (notmuch org)
  :defer 2
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))
  
  ;; (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart)
  ;; (setq org-mime-export-ascii 'latin1)
  (setq org-mime-export-ascii 'utf-8)
  (add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                           "#E6E1DC" "#232323"))))
  (add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))))

;;----------------------------------------------------------------
;; ORG-LINK-CUSTOMIZE - Store customize links with org
;;----------------------------------------------------------------
(use-package ol
  :after org
  :config
  (org-link-set-parameters "customize-option"
                           :follow #'org-store-link:customize-open-option
                           :store #'org-store-link:customize-store-link-option)

  (org-link-set-parameters "customize-group"
                           :follow #'org-store-link:customize-open-group
                           :store #'org-store-link:customize-store-link-group)

  (defun org-store-link:customize-open-option (path)
    "Visit the customize option at PATH."
    (customize-option (intern path)))

  (defun org-store-link:customize-open-group (path)
    "Visit the customize group at PATH."
    (customize-group (intern path)))

  (defun org-store-link:customize-store-link-option ()
    "Store a link to a customize option."
    (org-store-link:customize--store-link 'option))

  (defun org-store-link:customize-store-link-group ()
    "Store a link to a customize group."
    (org-store-link:customize--store-link 'group))

  (defun org-store-link:customize--store-link (type)
    "Store a link to a customize TYPE window."
    (when (memq major-mode '(Custom-mode))
      (let* ((page (org-store-link:customize-get-page-name))
             (page-type (org-store-link:customize-get-page-type))
             (link-type (symbol-name page-type))
             (link (format "customize-%s:%s" link-type page))
             (description (format "Customize %s for %s" link-type page)))
        (when (eq page-type type)
          (org-store-link-props
           :type (format "customize-%s" link-type)
           :link link
           :description description)))))

  (defun org-store-link:customize-get-page-type ()
    "Extract the page type (group or option) from the buffer name."
    (if (string-match "Customize \\(\\S-+\\):" (buffer-name))
        (pcase (match-string 1 (buffer-name))
          ("Group" 'group)
          ("Option" 'option))
      (error "Cannot create link to this customize page")))

  (defun org-store-link:customize-get-page-name ()
    "Extract the page name from the buffer name."
    (if (string-match ": \\(.+\\)\\*" (buffer-name))
        (let* ((str (downcase (match-string 1 (buffer-name)))))
          (replace-regexp-in-string " " "-" str))
      (error "Cannot create link to this customize page"))))

(provide 'setup-org)

