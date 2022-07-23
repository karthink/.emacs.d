;;;; Org mode

;;;----------------------------------------------------------------
;; ** ORG BEHAVIOR
;;;----------------------------------------------------------------
;; Org settings to do with its default behavior
(use-package org
  :straight (:type built-in)
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("<f5>"  . org-capture)
         ("\C-cr" . org-capture)
         ("<f6>"  . org-agenda)
         :map org-mode-map
         ("C-c C-x +" . my/org-strike-through-heading)
         ("C-,"   . nil)
         ("C-'"   . nil)
         ("C-c C-M-l" . org-toggle-link-display)
         ("C-S-<right>" . nil)
         ("C-S-<left>" . nil))

  :hook ((org-mode . turn-on-org-cdlatex)
         (org-cdlatex-mode . my/org-cdlatex-settings)
         (org-mode . er/add-latex-in-org-mode-expansions))
  
  :config
  ;; General preferences
  (setq-default org-adapt-indentation nil 
                org-cycle-include-plain-lists t 
                org-footnote-auto-label 'confirm
                org-image-actual-width 400 
                ;; org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 3)) 
                org-refile-targets '((nil :level . 1) (org-agenda-files :todo . "PROJECT"))
                org-refile-target-verify-function nil
                org-refile-use-outline-path t
                org-refile-use-cache t
                org-refile-allow-creating-parent-nodes t
                org-outline-path-complete-in-steps nil
                org-use-tag-inheritance nil
                org-tags-column 0
                org-use-sub-superscripts t
                org-special-ctrl-a/e t
                org-special-ctrl-k t
                org-log-done 'time
                org-catch-invisible-edits 'smart
                org-use-speed-commands t
                org-speed-commands-user '(("z" . org-add-note)
                                          ("4" . org-tree-to-indirect-buffer)
                                          ("S" . org-tree-to-indirect-buffer))
                org-imenu-depth 7
                org-id-link-to-org-use-id 'create-if-interactive
                org-extend-today-until 3
                org-id-locations-file (dir-concat user-cache-directory "org-id-locations")
                org-default-notes-file "~/org/do.org"
                org-M-RET-may-split-line '((headline) (default . nil))
                org-fast-tag-selection-single-key 'expert
                ;; org-indent-indentation-per-level 2 
                org-return-follows-link t)
  
  ;; (defun save-org-mode-files ()
  ;;   (dolist (buf (buffer-list))
  ;;     (with-current-buffer buf
  ;;       (when (eq major-mode 'org-mode)
  ;;         (if (and (buffer-modified-p) (buffer-file-name))
  ;;             (save-buffer))))))

  ;; (run-with-idle-timer 120 t 'save-org-mode-files)

  ;; My defaults
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . "zathura %s")))

  ;; Make org use `display-buffer' like every other Emacs citizen.
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  (defun er/add-latex-in-org-mode-expansions ()
    ;; Make Emacs recognize \ as an escape character in org
    (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
    ;; Paragraph end at end of math environment
    (setq paragraph-start (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; (setq paragraph-separate (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; Latex mode expansions
    (with-eval-after-load 'expand-region
      (set (make-local-variable 'er/try-expand-list)
           (append (remove #'er/mark-method-call er/try-expand-list)
                   '(LaTeX-mark-environment 
                     er/mark-LaTeX-inside-math
                     er/mark-latex-inside-delimiters
                     er/mark-latex-outside-delimiters
                     er/mark-LaTeX-math)))))
  
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

  (advice-add 'org-show-set-visibility
              :after (defun my/org-set-visibility-local-tree (detail)
                       "Expand local tree after setting visibility."
                       (when (eq detail 'local)
                         (org-ctrl-c-tab) (org-show-entry))))
  
  (add-to-list 'org-structure-template-alist '("ma" . "src matlab"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (defun my/org-cdlatex-settings ()
    (define-key org-cdlatex-mode-map (kbd "$") 'cdlatex-dollar)
    (ad-unadvise #'texmathp)
    (defadvice texmathp (around org-math-always-on activate)
      "Always return t in Org buffers.
This is because we want to insert math symbols without dollars even outside
the LaTeX math segments.  If Org mode thinks that point is actually inside
an embedded LaTeX fragment, let `texmathp' do its job.
`\\[org-cdlatex-mode-map]'"
      (interactive)
      (let (p)
        (cond
         ((not (derived-mode-p 'org-mode)) ad-do-it)
         ((eq this-command 'cdlatex-math-symbol)
	  (setq ad-return-value t
	        texmathp-why '("cdlatex-math-symbol in org-mode" . 0)))
         (t
	  (let ((p (org-inside-LaTeX-fragment-p)))
            (if (and p (cl-member (car p) (plist-get org-format-latex-options :matchers)
                                  :test (lambda (a b) (string-match (regexp-quote b) a))))
	        ad-do-it
	      ad-do-it
              ;; (setq ad-return-value t
	      ;;       texmathp-why '("Org mode embedded math" . 0))
	      ;; (when p ad-do-it)
              )))))))

  ;; From https://github.com/jkitchin/scimax
  ;; Numbered equations all have (1) as the number for fragments with vanilla
  ;; org-mode. This code injects the correct numbers into the previews so they
  ;; look good.
  (defun my/org-renumber-environment (orig-func &rest args)
    "A function to inject numbers in LaTeX fragment previews."
    (let ((results '())
	  (counter -1)
	  (numberp))
      (setq results (cl-loop for (begin .  env) in
			     (org-element-map (org-element-parse-buffer) 'latex-environment
			       (lambda (env)
			         (cons
			          (org-element-property :begin env)
			          (org-element-property :value env))))
			     collect
			     (cond
			      ((and (string-match "\\\\begin{equation}" env)
			            (not (string-match "\\\\tag{" env)))
			       (cl-incf counter)
			       (cons begin counter))
			      ((string-match "\\\\begin{align}" env)
			       (prog2
			           (cl-incf counter)
			           (cons begin counter)
			         (with-temp-buffer
			           (insert env)
			           (goto-char (point-min))
			           ;; \\ is used for a new line. Each one leads to a number
			           (cl-incf counter (count-matches "\\\\$"))
			           ;; unless there are nonumbers.
			           (goto-char (point-min))
			           (cl-decf counter (count-matches "\\nonumber")))))
			      (t
			       (cons begin nil)))))

      (when (setq numberp (cdr (assoc (point) results)))
        (setf (car args)
	      (concat
	       (format "\\setcounter{equation}{%s}\n" numberp)
	       (car args)))))

    (apply orig-func args))

  (defun my/toggle-latex-equation-numbering ()
    "Toggle whether LaTeX fragments are numbered."
    (interactive)
    (if (not (get 'my/org-renumber-environment 'enabled))
        (progn
	  (advice-add 'org-create-formula-image :around #'my/org-renumber-environment)
	  (put 'my/org-renumber-environment 'enabled t)
	  (message "Latex numbering enabled"))
      (advice-remove 'org-create-formula-image #'my/org-renumber-environment)
      (put 'my/org-renumber-environment 'enabled nil)
      (message "Latex numbering disabled.")))

  ;; From the Org manual
  (defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo))

;;;----------------------------------------------------------------
;; ** ORG-APPEARANCE
;;;----------------------------------------------------------------

;; Org settings to do with its default appearance
(use-package org
  :straight (:type built-in)
  :hook ((org-mode . org-toggle-pretty-entities)
         (org-mode . visual-line-mode))
  :config
  (setq-default
   org-fontify-done-headline t
   org-ellipsis " ▼ "
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-startup-folded t
   org-startup-indented nil
   org-startup-with-inline-images nil
   org-highlight-latex-and-related '(native)
   org-indent-mode-turns-on-hiding-stars nil
   org-use-sub-superscripts '{}
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   ;; org-priority-faces '((?a . error) (?b . warning) (?c . success))
   
  ;; Display preferences for latex previews
  ;; Larger equations
   org-format-latex-options
   (progn (plist-put org-format-latex-options :background 'default)
          (plist-put org-format-latex-options :scale 1.75)))
  
  ;; Pretty symbols
  ;; (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  ;; Org LaTeX options
  ;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  ;; Enable longline-truncation in org-mode buffers
  ;; (add-hook 'org-mode-hook 'toggle-truncate-lines)

  ;; Keyword faces for reftex labels and references in Org
  (font-lock-add-keywords
   'org-mode
   '(("\\(\\(?:\\\\\\(?:label\\|ref\\)\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face)))))

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (defun my/org-appear-markers ()
    (setq-local
     org-hide-emphasis-markers
     (if org-appear-mode t nil)))
   ;; (setq org-hide-emphasis-markers t)
   (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t))

;; TDOO Disabled while I test org-modern
(use-package org-bullets
  :disabled
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode))

;; TDOO Disabled while I test org-modern
;; Prettified symbols in org
(use-package org
  :straight (:type built-in)
  :disabled
  :hook (org-mode . my/org-prettify-symbols)
  :config
  (setq org-todo-keyword-faces
        '(;; ("TODO"    :foreground "#6e90c8" :weight bold)
          ("WAITING" :foreground "red" :weight bold)
          ("MAYBE"   :foreground "#6e8996" :weight bold)
          ("PROJECT" :foreground "#088e8e" :weight bold)))

  (defun my/org-prettify-symbols ()
    (setq prettify-symbols-alist
          (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                  '(;; ("#+begin_src" . ?)
                    (":properties:" . "")
                    ;; ("#+end_src" . ?)
                    ("#+begin_src" . ?)
                    ;; ("#+begin_src" . ?)
                    ;; ("#+end_src" . ?)
                    ;; ("#+begin_src" . "")
                    ("#+end_src" . "―")
                    ("#+begin_example" . ?)
                    ("#+end_example" . ?)
                    ("scheduled:" . ?)
                    ("deadline:" . ?)
                    ;; ("#+header:" . ?)
                    ;; ("#+name:" . ?﮸)
                    ;; ("#+results:" . ?)
                    ;; ("#+call:" . ?)
                    ;; (":properties:" . ?)
                    ;; (":logbook:" . ?)
                    (":end:" . "―")
                    ("#+attr_latex:"    . "🄛")
                    ("#+attr_html:"     . "🄗")
                    ("#+attr_org:"      . "⒪")
                    ("#+begin_quote:"   . "❝")
                    ("#+end_quote:"     . "❞"))))
    (prettify-symbols-mode 1)))

(use-package org-modern
  :straight (:host github
             :repo "minad/org-modern")
  :after org
  :hook (org-modern-mode-hook . my/org-modern-spacing)
  :config
  (defun my/org-modern-spacing ()
    (setq-local line-spacing
                (if org-modern-mode
                    0.1 0.0)))
  (setq org-modern-todo nil
        org-modern-hide-stars nil))

;; *** Modify latex previews to respect the theme
(use-package themed-ltximg
  :after org)

;;;----------------------------------------------------------------------
;; ** ORG FEATURES
;;;----------------------------------------------------------------------

;; *** ORG-SRC
(use-package org-src
  :after org
  :config
  (setq-default
   org-src-tab-acts-natively t))

;; *** ORG-CLOCK
(use-package org-clock
  :defer
  :after org
  :config
  (setq-default org-clock-idle-time 60
                org-clock-out-remove-zero-time-clocks t
                org-clock-mode-line-total 'today))

;; *** ORG-HABIT
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

;; *** ORG-AGENDA
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
                           "~/Documents/org/ucsb-cal.org"))
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

  (advice-add 'org-agenda-do-tree-to-indirect-buffer :after
     (defun my/org-agenda-collapse-indirect-buffer-tree (arg)
       (with-current-buffer org-last-indirect-buffer
         (org-ctrl-c-tab) (org-show-entry))))

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
    (when (and (not should-skip-entry)
               (save-excursion
                 (outline-up-heading 1 t)
                 (not (member (org-get-todo-state)
                              '("PROJECT" "TODO")))))
      (setq should-skip-entry t))
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

          ("i" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
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
          
          ("o" "Overview"
           ((tags-todo "*"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                (org-agenda-skip-function
                 `(org-agenda-skip-entry-if
                   'notregexp ,(format "\\[#%s\\]" ;;(char-to-string org-priority-highest)
                                       "\\(?:A\\|B\\|C\\)")))
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "⛤ Important\n")))
            (agenda ""
                    ((org-agenda-overriding-header "\n🕐 Today\n")
                     (org-agenda-span 1)
                     (org-deadline-warning-days 0)
                     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                     (org-agenda-block-separator nil)))
            (agenda "" ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\n📅 Next three days\n")))
            (agenda ""
                    ((org-agenda-time-grid nil)
                     (org-agenda-start-on-weekday nil)
                     ;; We don't want to replicate the previous section's
                     ;; three days, so we start counting from the day after.
                     (org-agenda-start-day "+3d")
                     (org-agenda-span 14)
                     (org-agenda-show-all-dates nil)
                     (org-deadline-warning-days 0)
                     (org-agenda-block-separator nil)
                     (org-agenda-entry-types '(:deadline))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "\n🞜 Upcoming deadlines (+14d)\n")))
            (tags "TODO=\"PROJECT\"&LEVEL>1&CATEGORY=\"Research\""
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\n⨕ Research\n")))
            ;; (alltodo ""
            ;;  ((org-agenda-overriding-header "Project Next Actions")
            ;;   (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))
            (todo "WAITING"
                  ((org-agenda-overriding-header "\n💤 On Hold\n")
                   (org-agenda-block-separator nil))))))))

;; *** ORG-CAPTURE
(use-package org-capture
  :after org
  :defer
  :commands (org-capture make-orgcapture-frame)
  :hook ((org-capture-prepare-finalize . org-id-get-create)
         (org-capture-after-finalize   . org-capture-after-delete-frame))
  :config
  (defun org-capture-after-delete-frame ()
    "If this is a dedicated org-capture frame, delete it after"
    (if (and (equal (frame-parameter nil 'name)
                    "dropdown_org-capture")
             (not (eq this-command 'org-capture-refile)))
        (delete-frame)))
  
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title))
           (commentp (y-or-n-p "Enable comments? ")))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   "\n:PROPERTIES:"
                   ,(concat "\n:EXPORT_FILE_NAME: " fname
                     "\n:EXPORT_HUGO_CUSTOM_FRONT_MATTER: :comments "
                     (if commentp "true" "false"))
                   "\n:END:"
                   "\n%?\n") 
                 ""))) 
  
  (pcase-dolist
      (`(,key . ,template)
       '(("t" "Add task" entry (file+headline "~/do.org" "Tasks")
          "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n%a\n%x\n" :prepend t)
         ("c" "Add calendar entry" entry (file "~/org/gmail-cal.org")
          "* %?\n%^{LOCATION}p\n:%(progn (require 'org-gcal) (symbol-value 'org-gcal-drawer-name)):
%a\n:END:")
         ("h" "Hugo post")
         ("hb" "Hugo Blog section"
          entry
          ;; It is assumed that below file is present in `org-directory'
          ;; and that it has a "Blog Ideas" heading. It can even be a
          ;; symlink pointing to the actual location of all-posts.org!
          (file+olp "posts.org" "Blog Ideas")
          (function org-hugo-new-subtree-post-capture-template)
          :kill-buffer t)
         ("hs" "Hugo Software section"
          entry
          ;; It is assumed that below file is present in `org-directory'
          ;; and that it has a "Blog Ideas" heading. It can even be a
          ;; symlink pointing to the actual location of all-posts.org!
          (file+olp "posts.org" "Software")
          (function org-hugo-new-subtree-post-capture-template)
          :kill-buffer t)))
    (setf (alist-get key org-capture-templates nil nil #'equal)
          template))
  
  (defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "dropdown_org_capture") (window-system . x)))
    (select-frame-by-name "dropdown_org_capture")
    (org-capture)
    (delete-other-windows)))

;; *** ORG-CRYPT
(use-package org-crypt
  :hook (org-mode . my/org-encrypt-entries)
  :config
  (defun my/org-encrypt-entries ()
    (add-hook 'before-save-hook
              'org-encrypt-entries
              nil t))
  (setq org-crypt-key user-full-name))

;; *** ORG-EXPORT (OX)
(use-package ox
  :after org
  :commands org-export-dispatch
  :config
  (setq org-html-htmlize-output-type 'css)
  (with-eval-after-load 'ox-latex
    (setq org-latex-caption-above nil)
    (setq org-latex-default-packages-alist
          (delete '("" "hyperref" nil) org-latex-default-packages-alist))
    (push '("hidelinks" "hyperref" nil) (cdr (last org-latex-default-packages-alist)))
    (when (executable-find "latexmk")
      (setq org-latex-pdf-process
            '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")))
    (add-to-list 'org-latex-classes
                 '("IEEEtran"
                   "\\documentclass[conference]{IEEEtran}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (setq org-export-with-LaTeX-fragments t
        org-latex-prefer-user-labels t
        org-latex-hyperref-template nil)
  
  ;; Minted
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist
               '("" "minted"))
  (add-to-list 'org-latex-packages-alist
               '("" "xcolor"))
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("linenos" "true")))
  
  ;; (add-to-list 'org-latex-packages-alist '("" "listings"))
  ;; (add-to-list 'org-latex-packages-alist '("" "color"))

  (defun my/org-export-ignore-headlines (data backend info)
    "Remove headlines tagged \"ignore\" retaining contents and promoting children.
Each headline tagged \"ignore\" will be removed retaining its
contents and promoting any children headlines to the level of the
parent."
    (org-element-map data 'headline
      (lambda (object)
        (when (member "ignore" (org-element-property :tags object))
          (let ((level-top (org-element-property :level object))
                level-diff)
            (mapc (lambda (el)
                    ;; recursively promote all nested headlines
                    (org-element-map el 'headline
                      (lambda (el)
                        (when (equal 'headline (org-element-type el))
                          (unless level-diff
                            (setq level-diff (- (org-element-property :level el)
                                                level-top)))
                          (org-element-put-property el
                                                    :level (- (org-element-property :level el)
                                                              level-diff)))))
                    ;; insert back into parse tree
                    (org-element-insert-before el object))
                  (org-element-contents object)))
          (org-element-extract-element object)))
      info nil)
    data)

  (add-hook 'org-export-filter-parse-tree-functions 'my/org-export-ignore-headlines)
  
  )

;; *** ORG-CITE
(use-package org
  :defer
  :config
  (use-package oc
    :after org
    :when (version<= "9.5" org-version)
    :commands org-cite-insert
    :config
    (use-package ox-html :defer :config
      (use-package oc-csl))
    (use-package ox-latex :defer :config
      (use-package oc-biblatex))
    (when-let*
        ((dir "~/.local/share/Zotero/styles/")
         (_ (file-directory-p dir)))
      (setq org-cite-csl-styles-dir dir))
    (setq org-cite-export-processors
          '((latex biblatex)
            (html csl)
            (t basic)))
    (use-package citar
      :config
      (setq
       org-cite-insert-processor 'citar
       org-cite-follow-processor 'citar
       org-cite-activate-processor 'citar
       org-cite-global-bibliography citar-bibliography))))

;;;----------------------------------------------------------------------
;; ** ORG-DOWNLOAD
;;;----------------------------------------------------------------------
(use-package org-download
  :straight t
  :after org
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :config
  ;; (setq org-download-backend (if IS-LINUX "curl" "url-retrieve"))
  (setq org-download-heading-lvl nil)
  (setq org-download-backend 'curl)
  (setq-default org-download-image-dir "./figures")
  (setq org-download-image-attr-list
        '("#+attr_html: :width 70% :align center"
          "#+attr_latex: :width 0.6\\textwidth")))

;;;----------------------------------------------------------------
;; ** ORG-BABEL
;;;----------------------------------------------------------------
(use-package ob-octave-fix
  :after ob-octave)

(use-package ob-julia
  ;; Source: https://git.nixo.xyz/nixo/ob-julia.git
  :disabled
  :straight (ob-julia :host github :repo "nixo/ob-julia")
  :requires ess
  :defer)

(use-package ob-julia
  :straight (ob-julia :host github :repo "nico202/ob-julia"
                      :files ("*.el" "julia"))
  :requires (ess ess-julia)
  :defer)

(use-package ess
  :straight t
  :after ob-julia
  :config
  (use-package ess-julia))

(use-package ob
  :after org
  :commands org-babel-execute-src-block
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :config
  (setq org-src-window-setup 'split-window-below
        org-confirm-babel-evaluate nil
        org-export-use-babel t)
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (matlab . t)
                               (octave . nil)
                               (python . t)
                               (R . nil)
                               (shell . t)
                               (scheme . nil)
                               (ditaa . nil)
                               (julia . nil)
                               (jupyter . nil)))
  (setq org-ditaa-jar-path "/usr/bin/ditaa")
  (defun my/org-babel-goto-tangle-file ()
    (if-let* ((args (nth 2 (org-babel-get-src-block-info t)))
              (tangle (alist-get :tangle args)))
        (unless (equal "no" tangle)
          (find-file tangle)
          t)))
  ;; (add-hook 'org-babel-after-execute-hook (lambda () (when org-inline-image-overlays
  ;;                                                 (org-redisplay-inline-images))))
  (add-hook 'org-open-at-point-functions 'my/org-babel-goto-tangle-file))

(use-package org-babel-eval-in-repl
  :disabled
  :straight t
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

;;;----------------------------------------------------------------
;; ** OX-HUGO
;;;----------------------------------------------------------------
(use-package ox-hugo
  :straight t
  :after (ox)
  :config
  (setq org-hugo-section "blog")
  (add-to-list 'org-hugo-special-block-type-properties
               '("sidenote" . (:trim-pre t :trim-post t)))
  (setq org-hugo-paired-shortcodes "%sidenote")
  (define-minor-mode my/org-hugo-mode
    "Helper mode for org-hugo previews."
    :keymap (make-sparse-keymap)
    :init-value nil)
  (defun my/org-hugo-preview (&optional arg)
    (interactive "P")
    (pcase-let* ((sec nil)
                 (`(,sec . ,title) 
                 (save-excursion
                   (org-previous-visible-heading 1)
                   (let ((title (org-element-property
                                 :EXPORT_FILE_NAME
                                 (org-element-at-point))))
                     (while (and (not sec) (org-up-heading-safe))
                       (setq sec (org-element-property
                                  :EXPORT_HUGO_SECTION
                                  (org-element-at-point))
                             title (or title
                                       (org-element-property
                                        :EXPORT_FILE_NAME
                                        (org-element-at-point)))))
                     (cons sec title)))))
      (if-let ((_ title)
               (url (concat
                     "http://localhost:1313/"
                     (if sec (downcase sec) "blog")
                     "/" title)))
          (progn (save-buffer)
                 (unless org-hugo-auto-export-mode
                   (org-hugo-export-wim-to-md))
                 (if arg
                     (progn (other-window 1) (eww url) (other-window -1))
                   (browse-url url)))
        (message "No preview url found.")))))

;;;----------------------------------------------------------------
;; ** OL-NOTMUCH
;;;----------------------------------------------------------------
(use-package ol-notmuch
  :after (notmuch org))

;;;----------------------------------------------------------------
;; ** ORG-GCAL
;;;----------------------------------------------------------------
(use-package org-gcal
  :straight t
  :after org
  :commands (org-gcal-sync org-gcal-fetch my/org-gcal-sync-maybe)
  :hook (org-agenda-mode . my/org-gcal-sync-maybe)
  :config
  (setq org-gcal-dir (dir-concat user-cache-directory "org-gcal/"))
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

;;;----------------------------------------------------------------
;; ** ORG-REVEAL
;;;----------------------------------------------------------------
(use-package org-re-reveal
  ;; :disabled
  :straight t
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
  :straight t
  :init
  (setq org-reveal-root "file:///home/karthik/.local/share/git/reveal.js")
  (setq org-reveal-hlevel 2))

;;;----------------------------------------------------------------
;; ** +INKSCAPE-FIGURES+
;;;----------------------------------------------------------------
(use-package inkscape-figures
  :disabled
  :after org
  :bind (:map org-mode-map
              ("C-c i" . #'+inkscape-figures-create-at-point-org)
              ("C-c e" . #'+inkscape-figures-edit)))

;;;----------------------------------------------------------------
;; ** ORG-REF
;;;----------------------------------------------------------------
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

;;;----------------------------------------------------------------
;; ** MY ORG PROJECTS
;;;----------------------------------------------------------------
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
          ("abode"
           :base-directory "~/Documents/abode/"
           :base-extension "org"
           :publishing-directory "~/Documents/abode"
           :remote-directory "root@abode.karthinks.com:/var/www/abode"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :auto-preamblle t
           :completion-function (list my/org-publish-rsync)
           )
          )
        )

  (defun my/org-publish-rsync (project-plist)
    "Sync output of org project to a remote server using RSYNC.

 All files and folders except for ORG files will be synced."
    (if (executable-find "rsync")
        (let* ((basedir (expand-file-name
                         (file-name-as-directory
                          (plist-get project-plist :base-directory))))
               (destdir (plist-get project-plist :remote-directory)))
          (start-process "rsync-project-abode" "*project-abode-output*"
                         "rsync" "-a" "-v" "--exclude=*.org" "--delete"
                         basedir destdir))

      (display-warning 'org-publish
                       "Could not find RSYNC in PATH. Project not uploaded to server."
                       :warning)))

  (defun my/org-publish-rsync-cyclostationarity (project-plist)
    "Sync output of org project to a remote server using RSYNC.

 All files and folders except for ORG files will be synced."
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

;;;----------------------------------------------------------------
;; ** ORG-TREE-SLIDE
;;;----------------------------------------------------------------
;; Presentations from within org-mode.
(use-package org-tree-slide
  :straight t
  :after org
  :commands my/org-presentation-mode
  :hook (org-tree-slide-after-narrow . my/org-tree-slide-enlarge-latex-preview)
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
  
  (defun my/org-tree-slide-enlarge-latex-preview ()
    (dolist (ov (overlays-in (point-min) (point-max)))
      (if (eq (overlay-get ov 'org-overlay-type)
              'org-latex-overlay)
          (overlay-put
           ov 'display
           (cons 'image 
                 (plist-put
                  (cdr (overlay-get ov 'display))
                  :scale (+ 1.0 (* 0.2 text-scale-mode-amount))))))))

  (define-minor-mode my/org-presentation-mode
    "Parameters for plain text presentations with `org-mode'."
    :init-value nil
    :global nil
    (if my/org-presentation-mode
        (progn
          (unless (eq major-mode 'org-mode)
            (user-error "Not in an Org buffer"))
          (org-tree-slide-mode 1)
          (setq-local org-hide-emphasis-markers t)
          (my/olivetti-mode 1)
          ;; (org-indent-mode 1)
          (text-scale-increase 3))
      (org-tree-slide-mode -1)
      (kill-local-variable 'org-hide-emphasis-markers)
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

;;;----------------------------------------------------------------
;; ** ORG-MIME 
;;;----------------------------------------------------------------
;; Compose HTML emails in org-mode.
(use-package org-mime
  :straight t
  :after (notmuch org)
  :defer 2
  :config
  (setq org-mime-export-options '(:with-latex dvipng
                                  :section-numbers nil
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

;;;----------------------------------------------------------------
;; ** ORG-LINK-CUSTOMIZE
;;;----------------------------------------------------------------
;; Store customize links with org.
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
          (org-link-store-props
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

;;;----------------------------------------------------------------
;; ** ORG-FRAGTOG (seamless latex fragment preview)
;;;----------------------------------------------------------------
(use-package org-fragtog
  :disabled
  :after org)

;;;----------------------------------------------------------------
;; ** ORG-NOTER
;;;----------------------------------------------------------------
(use-package org-noter
  :straight t
  :defer
  :config
  (setq org-noter-always-create-frame t
        org-noter-kill-frame-at-session-end t))

;;;----------------------------------------------------------------
;; ** ORG-ALERT
;;;----------------------------------------------------------------
(use-package org-alert
  :disabled
  :straight t
  :after org-agenda
  :config
  (setq org-alert-style 'notifications))

;;;----------------------------------------------------------------
;; ** ORG-XOURNALPP
;;;----------------------------------------------------------------
(use-package org-xournalpp
  :straight (:host gitlab
             :repo "vherrmann/org-xournalpp"
             :files ("*.el" "resources"))
  :after org
  :commands org-xournalpp-insert-new-image
  :config
  (setq org-xournalpp-export-dir "figures/"
        org-xournalpp-export-overwrite? t
        org-xournalpp-path-default "figures/sketch"
        org-xournalpp-image-type 'png))

;;;----------------------------------------------------------------
;; ** TOC-ORG
;;;----------------------------------------------------------------
(use-package toc-org :straight t :defer)

;;;----------------------------------------------------------------
;; ** Integration for RefTeX
;;;----------------------------------------------------------------
(use-package consult-reftex
  :disabled
  :after (org latex reftex)
  :bind (:map org-mode-map
         ("C-c )" . 'consult-reftex-insert-reference)))

(provide 'setup-org)

