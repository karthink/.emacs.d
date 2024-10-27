;;;; Org mode  -*- lexical-binding: t; -*-

;;;----------------------------------------------------------------
;; ** ORG BEHAVIOR
;;;----------------------------------------------------------------
;; Org settings to do with its default behavior
(use-package org
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
         ("C-S-<left>" . nil)
         ("C-M-e" . my/org-end-of-defun)
         :map org-cdlatex-mode-map
         ("`" . nil)
         (";" . cdlatex-math-symbol))

  :hook ((org-mode . turn-on-org-cdlatex)
         (org-cdlatex-mode . my/org-cdlatex-settings)
         (org-mode . er/add-latex-in-org-mode-expansions))
  :config
  ;; General preferences
  (setq-default org-adapt-indentation nil 
                org-cycle-include-plain-lists t 
                org-footnote-auto-label 'confirm
                org-image-actual-width nil
                ;; org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 3)) 
                org-refile-targets '((nil :maxlevel . 2)
                                     (org-agenda-files :maxlevel . 3)
                                     (org-agenda-files :todo . "PROJECT"))
                org-refile-target-verify-function nil
                org-refile-use-outline-path t
                org-refile-use-cache t
                org-refile-allow-creating-parent-nodes t
                org-outline-path-complete-in-steps nil
                org-use-tag-inheritance t
                org-tags-column 0
                org-special-ctrl-a/e t
                org-special-ctrl-k t
                org-use-fast-todo-selection 'expert
                org-log-done 'time
                org-catch-invisible-edits 'smart
                org-use-speed-commands t
                org-imenu-depth 7
                org-log-into-drawer t
                org-extend-today-until 3
                org-default-notes-file "~/org/do.org"
                org-M-RET-may-split-line '((headline) (default . nil))
                org-fast-tag-selection-single-key 'expert
                org-link-elisp-confirm-function nil
                org-export-backends '(ascii html latex)
                org-yank-image-save-method "figures"
                ;; org-indent-indentation-per-level 2 
                org-return-follows-link t)
  
  ;; My defaults
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . "zathura %s")))

  ;; Make org use `display-buffer' like every other Emacs citizen.
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  (dolist (cmds '(("z" . org-add-note)
                  ("F" . org-babel-next-src-block)
                  ("B" . org-babel-previous-src-block)
                  ("4" . org-tree-to-indirect-buffer)
                  ("S" . org-tree-to-indirect-buffer)))
    (cl-pushnew cmds org-speed-commands))

  (unless transient-mark-mode
    (advice-add 'org-mark-element :after #'my/activate-mark)
    (advice-add 'org-mark-subtree :after #'my/activate-mark))

  (add-hook 'org-metareturn-hook
            (defun my/auto-checkbox (&rest _)
              (when (org-at-item-checkbox-p)
                ;; Checkbox: Insert new item with checkbox.
                (org-insert-todo-heading nil) t)))

  (remove-hook 'org-cycle-hook
               #'org-cycle-optimize-window-after-visibility-change)

  (when (>= emacs-major-version 28)
    (defvar org-link-navigation-map
            (let ((map (make-sparse-keymap)))
              (define-key map (kbd "n") #'org-next-link)
              (define-key map (kbd "p") #'org-previous-link)
              map))
    (map-keymap
     (lambda (_ cmd)
       (put cmd 'repeat-map 'org-link-navigation-map))
     org-link-navigation-map))
  
  (defun my/org-beginning-of-defun (&optional arg)
    ";TODO: "
    (interactive "p")
    (if (not (texmathp))
        (org-backward-element)
      (let ((lx (save-mark-and-excursion
                  (LaTeX-backward-environment arg)
                  (point)))
            (beg (org-element-begin (org-element-context))))
        (if (> beg lx) (goto-char beg)
          (run-at-time 0 nil #'goto-char lx)
          lx))))

  (defun my/org-end-of-defun (&optional arg)
    ";TODO: "
    (interactive "p")
    (if (not (texmathp))
        (if (not (org-at-heading-p))
	    (org-forward-element)
	  (org-forward-element)
	  (forward-char -1))
      (goto-char (min (save-mark-and-excursion
                        (LaTeX-forward-environment (or arg 1))
                        (point))
                      (org-element-end (org-element-context))))))

  (defun er/add-latex-in-org-mode-expansions ()
    ;; Make Emacs recognize \ as an escape character in org
    (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
    ;; Paragraph end at end of math environment
    (setq paragraph-start (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; (setq paragraph-separate (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; Better forward/backward defun in Org
    (setq-local beginning-of-defun-function 'my/org-beginning-of-defun)
    ;; Latex mode expansions
    (with-eval-after-load 'expand-region
      (set (make-local-variable 'er/try-expand-list)
           (append (cl-set-difference er/try-expand-list
                                      '(er/mark-method-call
                                        er/mark-inside-pairs
                                        er/mark-outside-pairs))
                   '(LaTeX-mark-environment 
                     er/mark-LaTeX-inside-math
                     er/mark-latex-inside-pairs
                     er/mark-latex-outside-pairs
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

  (advice-add 'org-fold-show-set-visibility
              :after (defun my/org-set-visibility-local-tree (detail)
                       "Expand local tree after setting visibility."
                       (when (eq detail 'local)
                         (org-ctrl-c-tab) (org-show-entry))))
  
  (add-to-list 'org-structure-template-alist '("ma" . "src matlab"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))

  (defun my/org-cdlatex-settings ()
    (define-key org-cdlatex-mode-map (kbd "$") 'cdlatex-dollar)
    ;; (ad-unadvise #'texmathp)
    (advice-remove 'texmathp #'org--math-always-on))

  ;; From the Org manual
  (defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;; Disabling this to try to use cookies with PROJECT.
  ;; (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
  )

;; Custom DWIM functionality for `org-ctrl-c-ctrl-c'
(use-package org
  :after (org ox-latex org-latex-preview)
  :config
  (defvar my/org-output-buffers
    '("*Org Preview LaTeX Output*"
      "*Org Preview Convert Output*"
      "*Org PDF LaTeX Output*"
      "*Org Preview Preamble Precompilation*"
      "*Org LaTeX Precompilation*"))
  (defun my/org-output-next-buffer ()
    "Cycle through Org output buffers"
    (let* ((org-buffer (current-buffer))
           (bufnames
            (cl-sort
              (cl-remove-if-not
               #'buffer-live-p
               (cl-mapcar #'get-buffer my/org-output-buffers))
              #'> :key #'buffer-modified-tick))
           (next-output-buffer
            (lambda () (interactive)
              (setq bufnames
                    (nconc (cdr bufnames) (list (car bufnames))))
              (switch-to-buffer (car bufnames))))
           (back-to-org
            (lambda () (interactive)
              (while (memq (current-buffer) bufnames)
                (quit-window))
              (pop-to-buffer org-buffer))))
      (when bufnames
        (pop-to-buffer (car bufnames))
        (set-transient-map
         (define-keymap
           "C-c C-c" next-output-buffer
           "<remap> <keyboard-quit>" back-to-org)
         (lambda () (eq (current-buffer) (car bufnames)))))))
  (add-hook 'org-ctrl-c-ctrl-c-final-hook
            #'my/org-output-next-buffer 99))

;; Auto insertion for Org
(use-package org
  :after (org autoinsert)
  :config
  (define-auto-insert 'org-mode
    '(nil
      ;;----------------------------------------------------------------
      ;; Basic details
      "#+title: " (read-string "Title: ") "\n"
      "#+author: " (user-full-name) "\n"
      (if (y-or-n-p "Add date? ") (concat "#+date: " (format-time-string "%Y-%m-%d") "\n"))
      ;;----------------------------------------------------------------
      ;; Export options completion
      "#+options: "
      '(setq v1
       (append
        ;; Hard-coded OPTION items always available.
        '("H:" "\\n:" "num:" "timestamp:" "arch:" "author:" "c:"
          "creator:" "date:" "d:" "email:" "*:" "e:" "::" "f:"
          "inline:" "tex:" "p:" "pri:" "':" "-:" "stat:" "^:" "toc:"
          "|:" "tags:" "tasks:" "<:" "todo:")
        ;; OPTION items from registered backends.
        (let (items)
         (dolist (backend (bound-and-true-p
        		   org-export-registered-backends))
          (dolist (option (org-export-backend-options backend))
           (let ((item (nth 2 option)))
            (when item (push (concat item ":") items)))))
         items)))
      ((completing-read "Options: " v1) str " ") "\n"
      "#+exclude_tags: noexport ignore\n"
      ;;----------------------------------------------------------------
      ;; Startup options completion
      "#+startup: " ((completing-read "Startup: " org-startup-options nil t) str " ") "\n"

      ;; Org-babel setup
      '(setq v1
        '(("julia" .
           "#+property: header-args:julia :session *julia* :async yes :exports results")
          ("matlab" .
           (concat "#+property: header-args:matlab :session *MATLAB* :results output"
            " :exports both :eval never-export :noweb yes"))))
      (let ((lang (completing-read "Add language header props?: " v1 nil t)))
       (unless (string-empty-p lang)
        (concat (cdr (assoc lang v1)) "\n")))
      ;;----------------------------------------------------------------
      ;; HTML export setup
      (if (y-or-n-p "HTML setup? ")
          (concat "#+html_HEAD: <link rel=\"stylesheet\" type=\"text/css\" "
           "href=\"https://gongzhitaao.org/orgcss/org.css\"/>\n"
           "# #+setupfile: https://fniessen.github.io/org-html-themes/setup/theme-readtheorg.setup\n"))
      ;;----------------------------------------------------------------
      ;; LaTeX and latex previews setup
      (when (y-or-n-p "LaTeX setup? ")
       (concat
        "#+latex_class: " (completing-read "Class: " org-latex-classes) "\n"
        (when (y-or-n-p "Bibliography? ")
         (concat
          "#+bibliography: "
          (progn (require 'reftex-cite)
           (car reftex-default-bibliography))
          "\n"))
        "#+latex_header: \\usepackage[margin=1in]{geometry}\n"
        "#+latex_class_options: [10pt]\n"
        "#+latex_header: \\usepackage{arydshln}\n"
        "#+latex_header: \\usepackage{bbm}\n"
        "#+latex_header: \\usepackage{amsthm}\n"
        "#+latex_header: \\newtheorem{theorem}{Theorem}\n"
        "#+latex_header: \\newtheorem{lemma}{Lemma}[theorem]\n"
        "#+latex_header: \\newcommand{\\abs}[1]{\\left| #1 \\right|}\n"
        "#+latex_header: \\usepackage{cancel}\n"))
      '(setq v1 (progn (require 'tex)
                 (TeX-search-files-by-type 'texinputs 'global t t)))
      ((completing-read "Packages: " v1 nil t) str
       & '(progn (beginning-of-line) (insert "#+latex_header: \\usepackage{")
                 (end-of-line)       (insert "}\n")))
      _)))

(use-package org-id
  :after org
  :config
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id
        org-id-link-consider-parent-id t
        org-id-search-archives nil))

(use-package org-fold
  :after org
  :config
  (setf (alist-get 'agenda org-fold-show-context-detail)
        'local))

;; From alphapapa's unpackaged: https://github.com/alphapapa/unpackaged.el#org-return-dwim
(use-package org
  :bind (:map org-mode-map
         ("RET" . my/org-return-dwim))
  :config
  
  (defun my/org-element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (my/org-element-descendant-of type parent))))

  (defun my/org-return-dwim (&optional default)
    "A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
    ;; Inspired by John Kitchin:
    ;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
    (interactive "P")
    (if default
        (org-return)
      (cond
       ;; Act depending on context around point.
       
       ((and (eq 'link (car (org-element-context)))
             org-return-follows-link)
        ;; Link: Open it.
        (org-open-at-point-global))

       ;; ((or (eq
       ;;       (get-char-property (min (1+ (point)) (point-max)) 'org-overlay-type)
       ;;       'org-latex-overlay)
       ;;      (let ((context (org-element-context)))
       ;;        (and (memq (org-element-type context)
       ;;                   '(latex-fragment latex-environment))
       ;;             (eq (point)
       ;;                 (save-excursion
       ;;                   (goto-char (org-element-property :end context))
       ;;                   (skip-chars-backward "\n\r\t ")
       ;;                   (point))))))
       ;;  (org-latex-preview))

       ((org-at-heading-p)
        ;; Heading: Move to position after entry content.
        ;; NOTE: This is probably the most interesting feature of this function.
        (let ((heading-start (org-entry-beginning-position)))
          (goto-char (org-entry-end-position))
          (cond ((and (org-at-heading-p)
                      (= heading-start (org-entry-beginning-position)))
                 ;; Entry ends on its heading; add newline after
                 (end-of-line)
                 (insert "\n\n"))
                (t
                 ;; Entry ends after its heading; back up
                 (forward-line -1)
                 (end-of-line)
                 (when (org-at-heading-p)
                   ;; At the same heading
                   (forward-line)
                   (insert "\n")
                   (forward-line -1))
                 ;; FIXME: looking-back is supposed to be called with more arguments.
                 (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
                   (insert "\n"))
                 (forward-line -1)))))

       ((org-in-item-p)
        ;; Plain list.  Yes, this gets a little complicated...
        (let ((context (org-element-context)))
          (if (or (eq 'plain-list (car context))  ; First item in list
                  (and (eq 'item (car context))
                       (not (eq (org-element-property :contents-begin context)
                                (org-element-property :contents-end context))))
                  (my/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
              ;; Non-empty item: Add new item.
              (if (org-at-item-checkbox-p)
                  (org-insert-todo-heading nil)
                (org-insert-item))
            ;; Empty item: Close the list.
            ;; TODO: Do this with org functions rather than operating on the
            ;; text. Can't seem to find the right function.
            (delete-region (line-beginning-position) (line-end-position))
            (insert "\n"))))

       ((when (fboundp 'org-inlinetask-in-task-p)
          (org-inlinetask-in-task-p))
        ;; Inline task: Don't insert a new heading.
        (org-return))

       ((org-at-table-p)
        (cond ((save-excursion
                 (beginning-of-line)
                 ;; See `org-table-next-field'.
                 (cl-loop with end = (line-end-position)
                          for cell = (org-element-table-cell-parser)
                          always (equal (org-element-property :contents-begin cell)
                                        (org-element-property :contents-end cell))
                          while (re-search-forward "|" end t)))
               ;; Empty row: end the table.
               (delete-region (line-beginning-position) (line-end-position))
               (org-return))
              (t
               ;; Non-empty row: call `org-return'.
               (org-return))))
       (t
        ;; All other cases: call `org-return'.
        (org-return))))))

;;;----------------------------------------------------------------
;; ** ORG-APPEARANCE
;;;----------------------------------------------------------------

;; Org settings to do with its default appearance
(use-package org
  :hook ((org-mode . org-toggle-pretty-entities)
         (org-mode . visual-line-mode))
  :config
  (setq-default
   org-fontify-done-headline t
   org-ellipsis " ▾"
   ;; org-ellipsis "…"
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-startup-folded t
   org-startup-indented nil
   org-startup-with-inline-images nil
   org-startup-with-latex-preview t
   ;; org-highlight-latex-and-related '(latex entities)
   org-highlight-latex-and-related '(native entities)
   org-indent-mode-turns-on-hiding-stars nil
   org-use-sub-superscripts '{}
   org-pretty-entities nil
   org-image-align 'center
   ;; org-priority-faces '((?a . error) (?b . warning) (?c . success))
   org-pretty-entities-include-sub-superscripts t)
  
  ;; Pretty symbols
  ;; (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  ;; Org LaTeX options
  ;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  ;; Enable longline-truncation in org-mode buffers
  ;; (add-hook 'org-mode-hook 'toggle-truncate-lines)

  ;; Keyword faces for reftex labels and references in Org
  ;; ,(rx
  ;; (group "\\" (or "label" "ref" "cref" "eqref"))
  ;; (minimal-match "{" (group (1+ any)) "}"))
  (font-lock-add-keywords
   'org-mode
   '(("\\(\\(?:\\\\\\(?:label\\|cref\\|ref\\|eqref\\)\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face))))

  (setq org-entities-user '(("nbsp" "~" nil "&nbsp" " " "\x00A0" "∼")))
  
  (defun my/org-raise-scripts-no-braces (_)
    (when (and (eq (char-after (match-beginning 3)) ?{)
	       (eq (char-before (match-end 3)) ?}))
      (remove-text-properties (match-beginning 3) (1+ (match-beginning 3))
		              (list 'invisible nil))
      (remove-text-properties (1- (match-end 3)) (match-end 3)
		              (list 'invisible nil))))

  (advice-add 'org-raise-scripts :after #'my/org-raise-scripts-no-braces)

  (setq org-todo-keyword-faces
        '(;; ("TODO"    :foreground "#6e90c8" :weight bold)
          ("WAITING" :foreground "red" :weight bold)
          ("MAYBE"   :foreground "#6e8996" :weight bold)
          ("PROJECT" :foreground "#088e8e" :weight bold)
          ("SUSPENDED" :foreground "#6e8996" :weight bold))))

;; Settings to do with org-latex-preview
(use-package org-latex-preview
  :after org
  :hook ((org-mode . org-latex-preview-auto-mode)
         (org-mode . my/org-latex-preview-precompile-idle))
  :bind (:map org-mode-map
         ("C-c C-x SPC" . org-latex-preview-clear-cache)
         ("C-c i" . my/org-latex-preview-image-at-point)
         ("M-g m" . my/org-latex-next-env)
         ("M-g M" . my/org-latex-prev-env))
  :config
  ;; (setq org-element-use-cache nil)
  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line
          mwheel-scroll pixel-scroll-precision
          scroll-up-command scroll-down-command
          scroll-other-window scroll-other-window-down))
  (setq-default
   ;; org-latex-preview-header
   ;;    "\\documentclass{article}
   ;; \\usepackage[usenames]{color}
   ;; [DEFAULT-PACKAGES]
   ;; [PACKAGES]
   ;; \\setlength{\\textwidth}{0.8\\paperwidth}
   ;; \\addtolength{\\textwidth}{-2cm}"

   org-format-latex-options
   (progn (plist-put org-format-latex-options :background "Transparent")
          (plist-put org-format-latex-options :scale 1.0)
          (plist-put org-format-latex-options :zoom
                     (- (/ (face-attribute 'default :height) 100.0) 0.025)))

   org-latex-preview-appearance-options
   (progn (plist-put org-latex-preview-appearance-options :scale 1.0)
          (plist-put org-latex-preview-appearance-options :zoom
                     (- (/ (face-attribute 'default :height) 100.0) 0.025)))

   org-latex-preview-numbered nil
   org-latex-preview-live-debounce 0.3
   ;; org-latex-preview-live-throttle 0.5
   org-latex-preview-auto-track-inserts t
   org-latex-preview-live '(block edit-special)
   org-latex-preview-process-active-indicator nil)

  ;; Get the image at point
  (defun my/org-latex-preview-image-at-point (&optional arg)
    (interactive "P")
    (if-let* ((imgpath (thread-first
                         (point) overlays-at last car
                         (overlay-get 'preview-image)
                         cdr (plist-get :file)))
              (_ (file-exists-p imgpath)))
        (prog1 (kill-new imgpath)
            (when arg (start-process "olpsink" nil "dragon" (expand-file-name imgpath)))
            (message "Image path copied to kill-ring."))
      (message "No LaTeX preview image at point!")))
  
  ;; Utility command to navigate math fragments
  (defun my/org-latex-next-env (&optional arg)
    (interactive "p")
    (save-match-data
      (re-search-forward org-latex-preview--tentative-math-re
                         nil t (or arg 1))))

  (defun my/org-latex-prev-env (&optional arg)
    (interactive "p")
    (my/org-latex-next-env (- (or arg 1))))
  
  (defvar-keymap my/org-latex-env-map
    :repeat t
    "m" 'my/org-latex-next-env
    "M" 'my/org-latex-prev-env
    "n" 'my/org-latex-next-env
    "p" 'my/org-latex-prev-env)
  (put 'my/org-latex-next-env
       'repeat-map 'my/org-latex-env-map)
  (put 'my/org-latex-prev-env
       'repeat-map 'my/org-latex-env-map)
  
  (defun my/org-latex-preview-show-error ()
    (display-local-help t))
  (add-hook 'org-ctrl-c-ctrl-c-final-hook
            #'my/org-latex-preview-show-error -10)

  ;; Precompilation freezes emacs, do it in the background when possible.
  (defun my/org-latex-preview-precompile-idle (&optional beg end _)
    (when (and (featurep 'async) (not (or beg end)))
      (run-with-idle-timer
       2 nil #'my/org-latex-preview-precompile-async
       (current-buffer))))

  (advice-add 'org-latex-preview-clear-cache :after
              #'my/org-latex-preview-precompile-idle)

  (defun my/org-latex-preview-precompile-async (&optional org-buf)
    (when (and (buffer-live-p org-buf)
               (window-live-p (get-buffer-window org-buf 'all-frames)))
      (with-current-buffer org-buf
        (when org-latex-preview-process-precompiled
          (let* ((org-location (org-find-library-dir "org"))
                 (compiler-keywords
                  (org-collect-keywords
                   '("LATEX_PREVIEW_COMPILER" "LATEX_COMPILER")
                   '("LATEX_PREVIEW_COMPILER" "LATEX_COMPILER")))
                 (compiler
                  (or (cdr (assoc "LATEX_PREVIEW_COMPILER" compiler-keywords))
                      (and (boundp 'org-latex-preview-compiler)
                           org-latex-preview-compiler)
                      (cdr (assoc "LATEX_COMPILER" compiler-keywords))
                      org-latex-compiler))
                 (header (concat
                          (or org-latex-preview--preamble-content
                              (org-latex-preview--get-preamble))
                          org-latex-preview--include-preview-string))
                 (relative-file-p
                  (string-match-p "\\(?:\\\\input{\\|\\\\include{\\)[^/]" header))
                 (remote-file-p (file-remote-p default-directory))
                 (info (list :latex-compiler compiler
                             :precompile-format-spec
                             (let ((org-tex-compiler
                                    (cdr (assoc compiler org-latex-preview-compiler-command-map))))
                               `((?l . ,org-tex-compiler)
                                 (?L . ,(car (split-string org-tex-compiler)))))))
                 (preamble-hash (thread-first
                                  header
                                  (concat
                                   compiler
                                   (if (not relative-file-p)
                                       "-temp" default-directory))
                                  (sha1))))
            (when (and (equal compiler "pdflatex")
                       (not remote-file-p)
                       (not (cadr (org-persist-read "LaTeX format file cache"
                                                    (list :key preamble-hash)
                                                    nil nil :read-related t))))
              (async-start
               `(lambda ()
                  (add-to-list 'load-path ,org-location)
                  (require 'ox)
                  (org-latex--precompile-preamble
                   ',info ,header
                   ,(expand-file-name preamble-hash temporary-file-directory)))
               `(lambda (dump-file)
                 (let ((inhibit-message t))
                   (org-persist--load-index)
                   (cadr
                    (org-persist-register `(,"LaTeX format file cache"
                                            (file ,dump-file))
                                          (list :key ,preamble-hash)
                                          :write-immediately t))
                   (message "Precompiled in background: %S"
                            (file-name-base dump-file)))))))))))

  ;; org-html-format-latex from Org 9.6 - this is needed because the new version does not work with ox-hugo
  ;; (defun org-html-format-latex (latex-frag processing-type info)
;;     "Format a LaTeX fragment LATEX-FRAG into HTML.
;; PROCESSING-TYPE designates the tool used for conversion.  It can
;; be `mathjax', `verbatim', `html', nil, t or symbols in
;; `org-preview-latex-process-alist', e.g., `dvipng', `dvisvgm' or
;; `imagemagick'.  See `org-html-with-latex' for more information.
;; INFO is a plist containing export properties."
;;     (let ((cache-relpath "") (cache-dir ""))
;;       (unless (or (eq processing-type 'mathjax)
;;                   (eq processing-type 'html))
;;         (let ((bfn (or (buffer-file-name)
;;                        (make-temp-name
;;                         (expand-file-name "latex" temporary-file-directory))))
;;               (latex-header
;;                (let ((header (plist-get info :latex-header)))
;;                  (and header
;;                       (concat (mapconcat
;;                                (lambda (line) (concat "#+LATEX_HEADER: " line))
;;                                (org-split-string header "\n")
;;                                "\n")
;;                               "\n")))))
;;           (setq cache-relpath
;;                 (concat (file-name-as-directory org-preview-latex-image-directory)
;;                         (file-name-sans-extension
;;                          (file-name-nondirectory bfn)))
;;                 cache-dir (file-name-directory bfn))
;;           ;; Re-create LaTeX environment from original buffer in
;;           ;; temporary buffer so that dvipng/imagemagick can properly
;;           ;; turn the fragment into an image.
;;           (setq latex-frag (concat latex-header latex-frag))))
;;       (with-temp-buffer
;;         (insert latex-frag)
;;         (org-format-latex cache-relpath nil nil cache-dir nil
;;                           "Creating LaTeX Image..." nil processing-type)
;;         (buffer-string))))
  )

;; code for centering LaTeX previews -- a terrible idea
(use-package org-latex-preview
  ;; :disabled
  :after org-latex-preview
  :config
  (defun my/org-latex-preview-uncenter (ov)
    ;; (overlay-put ov 'justify (overlay-get ov 'before-string))
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify))
    ;; (overlay-put ov 'justify nil))
    )
  (defun my/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
                  ((or (eq (org-element-type elem) 'latex-environment)
                       (string-match-p "^\\\\\\[" (org-element-property :value elem))))
                  (img (overlay-get ov 'display))
                  (prop `(space :align-to (- center (0.55 . ,img))))
                  (justify (propertize " " 'display prop 'face 'default)))
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
        (progn
          (add-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter nil :local)
          (add-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter nil :local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter))))

(use-package org-appear
  :disabled
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq-default org-hide-emphasis-markers t)
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers nil))

;; TDOO Disabled while I test org-modern
(use-package org-bullets
  :disabled
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

;; TDOO Disabled while I test org-modern
;; Prettified symbols in org
(use-package org
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
  :ensure (:host github
             :repo "minad/org-modern")
  :after org
  :hook ((org-modern-mode . my/org-modern-spacing))
  :config
  (defun my/org-modern-spacing ()
    (setq-local line-spacing
                (if org-modern-mode
                    0.1 0.0)))
  (setq org-modern-todo t
        org-modern-hide-stars nil
        org-modern-horizontal-rule t
        org-modern-star 'replace
        org-modern-keyword "‣ "
        ;; org-modern-block-fringe 0 
        org-modern-table nil))

;; *** Modify latex previews to respect the theme
;; Not needed with the next gen latex previews
(use-package themed-ltximg
  :disabled
  :after org)

;;;----------------------------------------------------------------------
;; ** ORG FEATURES
;;;----------------------------------------------------------------------

(use-package org-footnote
  :defer
  :config
  (setq org-footnote-section nil
        org-footnote-define-inline nil))

;; *** ORG-GOTO
(use-package org-goto
  :defer
  :config
  (setq org-goto-interface 'outline-path-completion))

;; *** ORG-ATTACH
(use-package org-attach
  :defer
  :config
  (setq org-attach-store-link-p 'attached))

;; *** ORG-SRC
(use-package org-src
  :after org
  :config
  (when (fboundp 'LaTeX-mode)
    (setf (alist-get "latex" org-src-lang-modes
                     nil nil #'equal)
          'LaTeX))
  (setq-default
   org-src-tab-acts-natively t
   org-src-preserve-indentation t
   org-src-window-setup 'plain)
  (advice-add 'org-src-font-lock-fontify-block
              :after
              (defun my/no-latex-background-face (lang start end)
                (when (equal lang "latex")
                  (alter-text-property
                   start end 'face
                   (lambda (l) (remove 'org-block l)))))))

(use-package org-src-context
  :defer
  :ensure (:host github
             :repo "karthink/org-src-context")
  :after org-src)

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
              ("w" . org-agenda-refile)
              ("S-SPC" . org-agenda-show-scroll-down))
  :config
  (timeout-debounce! 'org-agenda-do-context-action 0.3 t)
  (setq org-show-notification-timeout 10)
  (setq org-agenda-files '("~/Documents/org/inbox.org"
                           "~/Documents/org/do.org"
                           "~/Documents/org/gmail-cal.org"
                           "~/Documents/org/ucsb-cal.org"))
  (setq-default
   org-agenda-span 2
   org-agenda-restore-windows-after-quit t
   org-agenda-window-setup 'current-window
   org-stuck-projects '("TODO=\"PROJECT\"|TODO=\"SUSPENDED\"" ("TODO" "DEFERRED") nil "")
   org-agenda-use-time-grid nil
   org-agenda-todo-ignore-scheduled nil
   org-agenda-text-search-extra-files nil ;'(agenda-archives)
   org-agenda-tags-column 'auto
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-show-all-dates nil
   org-agenda-inhibit-startup t
   org-agenda-include-diary nil
   org-agenda-follow-indirect nil
   org-agenda-default-appointment-duration 60)

  (advice-add 'org-agenda-do-tree-to-indirect-buffer :after
     (defun my/org-agenda-collapse-indirect-buffer-tree (arg)
       (with-current-buffer org-last-indirect-buffer
         (org-ctrl-c-tab) (org-fold-show-entry 'hide-drawers))))

  (defun my/org-agenda-next-section (arg)
    (interactive "p")
    (when (> arg 0)
      (dotimes (_ arg)
        (when-let ((m (text-property-search-forward 'face 'org-agenda-structure t t)))
          (goto-char (prop-match-beginning m))
          (forward-char 1)))))

  ;; FIXME this is broken
  (defun my/org-agenda-previous-section (arg)
    (interactive "p")
    (when (> arg 0)
      (dotimes (_ arg)
        (when-let ((m (text-property-search-backward 'face 'org-agenda-structure nil nil)))
          (goto-char (prop-match-end m))
          ;; (forward-char 1)
          ))))

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
                 (unless (= (org-outline-level) 1)
                   (outline-up-heading 1 t))
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

          ("P" "All Projects" tags "TODO=\"PROJECT\"&LEVEL>1|TODO=\"SUSPENDED\"" ;|TODO=\"CLOSED\"
           ((org-agenda-overriding-header "All Projects")))

          ("i" "Inbox" tags "CATEGORY=\"Inbox\"&LEVEL=1"
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
            (tags "CATEGORY=\"Inbox\"&LEVEL=1"
             ((org-agenda-block-separator nil)
              (org-agenda-overriding-header "\n📧 Inbox\n")))
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

;; *** APPT (for notifications)
(use-package appt
  :after org-agenda
  :config
  (setq appt-disp-window-function
        (lambda (minutes-to now text)
          (org-show-notification
           (format "In %s minutes:\n%s"
                   minutes-to text)))
        appt-display-interval 15
        appt-display-mode-line nil
        appt-message-warning-time 60)

  (define-advice appt-activate (:after (&optional _arg) hold-your-horses)
    "`appt-activate' is too eager, rein it in."
    (remove-hook 'write-file-functions #'appt-update-list)
    (when (timerp appt-timer)
      (timer-set-time appt-timer (current-time) 600)))

  (define-advice appt-check (:before (&optional _force) read-from-org-agenda)
    "Read events from Org agenda is possible."
    (and (featurep 'org-agenda)
         (ignore-errors
           (let ((inhibit-message t))
             (org-agenda-to-appt t))))))

;; *** ORG-CAPTURE
(use-package org-capture
  :after org
  :defer
  :commands (org-capture make-orgcapture-frame)
  :hook (;; (org-capture-prepare-finalize . org-id-get-create)
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
       '(("r" "Add resource" entry (file "~/org/inbox.org")
          "* TODO [[%c][%^{Title: }]] %^G\n:PROPERTIES:\n:CREATED:  %U\n:END:\n"
          :immediate-finish t)
         ("t" "Add task" entry (file "~/org/inbox.org")
          "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n%a\n%i\n")
         ("m" "Task from email" entry (file "~/org/inbox.org")
          "* TODO %A\n:PROPERTIES:\n:CREATED:  %U\n:END:\n" :immediate-finish t)
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
  (use-package ox-html
    :defer
    :config
    (setq org-html-htmlize-output-type 'css)
    (plist-put org-html-latex-image-options :inline '(svg svg-embed))
    (add-to-list 'org-structure-template-alist
                 '("D" . "details"))
    (add-to-list 'org-structure-template-alist
                 '("S" . "summary")))
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

  (add-hook 'org-export-filter-parse-tree-functions 'my/org-export-ignore-headlines))

(use-package ox-latex
  :after ox
  :defer
  :config
  ;; Temporary fix while ox-latex is borked for previews
  (setf (car org-latex-default-packages-alist)
        '("utf8" "inputenc" t ("pdflatex")))
  ;; General preferences
  (when (executable-find "latexmk")
    (setq org-latex-pdf-process
          '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")))
  (dolist (package '((""           "longtable" nil)
                     (""           "booktabs"  nil)
                     (""           "color"     nil)
                     (""           "cancel"    t)
                     ;; ;DONE: Some documentclasses load these themselves, in
                     ;; ;which case we should add [NO-DEFAULT-PACKAGES] to their
                     ;; ;strings in `org-latex-classes'.
                     ("capitalize" "cleveref"  nil)
                     (""           "amsmath"   t)
                     (""           "amssymb"   t)
                     ))
    (cl-pushnew package org-latex-packages-alist
                :test (lambda (a b) (equal (cadr a) (cadr b)))))
  (let* ((article-sections '(("\\section{%s}"       . "\\section*{%s}")
                             ("\\subsection{%s}"    . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))
    (pcase-dolist (`(,name ,class-string . ,extra)
                   `(("siamart" "\\documentclass{siamart220329}")
                     ("ieeeconf" "\\documentclass{ieeeconf}")
                     ("IEEEtran" "\\documentclass{IEEEtran}")
                     ("ieeecolor" "\\documentclass{ieeecolor}")
                     ("article" "\\documentclass{scrartcl}")
                     ("report" "\\documentclass{scrreprt}")
                     ("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]")
                     ("book" "\\documentclass[twoside=false]{scrbook}"
                      ("\\chapter{%s}" . "\\chapter*{%s}"))))
      (setf (alist-get name org-latex-classes nil nil #'equal)
            (append (list class-string) extra article-sections))))
  (setq
   org-latex-caption-above nil
   org-export-with-LaTeX-fragments t
   org-latex-tables-booktabs t
   org-export-with-smart-quotes t
   org-latex-prefer-user-labels t
   org-latex-reference-command "\\cref{%s}"
   ;; From https://git.tecosaur.net/tec/emacs-config, the default link colors
   ;; are hideous.
   org-latex-hyperref-template
   "\\providecolor{url}{HTML}{0077bb}
\\providecolor{link}{HTML}{882255}
\\providecolor{cite}{HTML}{999933}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=link,
  urlcolor=url,
  citecolor=cite}
\\urlstyle{same}
%% hide links styles in toc
\\NewCommandCopy{\\oldtoc}{\\tableofcontents}
\\renewcommand{\\tableofcontents}{\\begingroup\\hypersetup{hidelinks}\\oldtoc\\endgroup}"
   org-latex-precompile t))

(use-package ox-beamer
  :defer
  :after ox
  :init
  (defun org-beamer-backend-p (info)
    (eq 'beamer (and (plist-get info :back-end)
                     (org-export-backend-name (plist-get info :back-end)))))

  (add-to-list 'org-export-conditional-features
               '(org-beamer-backend-p . beamer) t)
  :config
  (setq org-beamer-theme "metropolis"
        org-beamer-frame-level 2))

;; **** Code block export
;; Code block export preferences.
(use-package org
  :defer
  :config
  
  ;; Engrave faces, or
  (use-package ox
    :if (version<= "9.5.4" org-version)
    :after ox
    :config
    (use-package engrave-faces
      :ensure t
      :config
      ;; (setq org-latex-listings 'engraved)
      (setq org-latex-src-block-backend 'engraved)))

  ;; Minted
  (use-package ox
    :if (version< org-version "9.5.4")
    :after ox
    :config
    (setq org-latex-listings 'minted)
    (add-to-list 'org-latex-packages-alist
                 '("" "minted"))
    (add-to-list 'org-latex-packages-alist
                 '("" "xcolor"))
    (setq org-latex-minted-options
          '(("frame" "lines")
            ("linenos" "true")))))

;; **** OX-CHAMELEON
(use-package ox-chameleon
  :ensure (:host github :repo "tecosaur/ox-chameleon")
  :after ox
  :config
  (add-to-list 'org-latex-packages-alist '("" "scrextend" nil))
  ;; xcolor is added anyway, this is a failsafe
  (add-to-list 'org-latex-packages-alist '("" "xcolor" nil)))

;; *** ORG-CITE
(use-package org
  :defer
  :config
  (use-package oc
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
;; ** ORG-IMAGE-PREVIEW DONT
;;;----------------------------------------------------------------------
;; Disabled while I work on the org-link-preview patch for Org
(use-package org-image-preview
  :disabled
  :ensure (:host github :protocol ssh
           :repo "karthink/org-image-preview")
  :after org
  :bind (:map org-mode-map
         ([remap org-toggle-inline-images] . org-image-preview)
         :map org-link-navigation-map
         ("v" . org-image-preview))
  :config
  (put 'org-image-preview 'repeat-map 'org-link-navigation-map))

;;;----------------------------------------------------------------------
;; ** ORG-DOWNLOAD DONT
;;;----------------------------------------------------------------------
;; Disabled while I work out the kinks with org-yank
(use-package org-download
  :disabled
  :after org
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :config
  ;; (setq org-download-backend (if IS-LINUX "curl" "url-retrieve"))
  (setq-default org-download-heading-lvl nil)
  (setq org-download-backend "curl")
  (setq-default org-download-image-dir "./figures")
  (setq org-download-image-attr-list
        '("#+attr_html: :width 40% :align center"
          "#+attr_latex: :width \\textwidth")))

;;;----------------------------------------------------------------
;; ** ORG-BABEL
;;;----------------------------------------------------------------
(defmacro with-ob-autoload (library &rest forms)
  (declare (indent defun))
  `(use-package ,(intern (concat "ob-" library))
    :autoload (,(intern (concat "org-babel-execute:" library))
               ,(intern (concat "org-babel-expand-body:" library))
               ,(intern (concat "org-babel-" library "-initiate-session")))
    ,@forms))

(use-package ob-julia
  :ensure (:host github :repo "nico202/ob-julia"
           :files ("*.el" "julia")
           :remotes ("fork" :host github :repo "karthink/ob-julia"
                     :branch "main" :protocol ssh))
  :after (ob org)
  :autoload (org-babel-execute:julia org-babel-expand-body:julia
                                     org-babel-prep-session:julia)
  :hook (org-babel-julia-after-async-execute . my/org-redisplay-babel-result)
  :init
  ;; (setq ob-julia-insert-latex-environment-advice nil)
  (add-to-list 'org-structure-template-alist '("j" . "src julia"))
  :config
  (when (featurep 'julia-snail) (require 'ob-julia-snail))
  (setq org-babel-default-header-args:julia
        '((:session . nil)
          (:async   . "yes")))
  (setq org-babel-julia-backend 'julia-snail)
  (when (featurep 'ess)
    (setq ess-eval-visibly 'nowait)
    (defun org-babel-julia-initiate-session (&optional session params)
      "Create or switch to an ESS Julia session.

Return the initialized session, if any."
      (unless (string= session "none")
        (let ((session (or session "*julia*")))
          (if (org-babel-comint-buffer-livep session)
              session
            (save-window-excursion
              (org-babel-prep-session:julia session params))))))))

(with-ob-autoload "octave"
  :config
  (use-package ob-octave-fix :after ob-octave))
(with-ob-autoload "clojure"
  :after (ob cider)
  :config
  (setq org-babel-clojure-backend 'cider))
(with-ob-autoload "svgbob"
  :disabled
  :when (executable-find "svgbob")
  :ensure t
  :config
  (unless (fboundp 'svgbob-mode)
    (add-to-list 'org-src-lang-modes (cons "svgbob" 'artist))))
(with-ob-autoload "python")
(with-ob-autoload "shell")
(with-ob-autoload "scheme")
(with-ob-autoload "ditaa")
(with-ob-autoload "emacs-lisp")

;; ESS DONT
;; Disabled in favor of julia-snail
(use-package ess
  :disabled
  :ensure t
  :after ob-julia
  :config
  (use-package ess-julia))

(use-package org-tempo
  :after org
  :defer 3
  :config
  (pcase-dolist (`(,key ,expansion)
                 '(("n" "name")
                   ("lh" "latex_header")
                   ("lc" "latex_class")
                   ("lco" "latex_class_options")
                   ("ao" "attr_org")
                   ("al" "attr_latex")
                   ("ah" "attr_html")
                   ("cap" "caption")))
    (setf (alist-get key org-tempo-keywords-alist
                     nil nil #'equal)
          expansion)))

(use-package ob
  :commands org-babel-execute-src-block
  :hook (org-babel-after-execute . my/org-redisplay-babel-result)
  :bind (:map org-mode-map
         ("C-M-x" . org-babel-do-key-sequence-in-edit-buffer))
  :config
  (setf (alist-get :eval org-babel-default-header-args)
        "no-export")
  (setq org-confirm-babel-evaluate nil
        org-export-use-babel t)

  (defun my/org-redisplay-babel-result ()
    (save-excursion
      (condition-case err
          (when-let* ((beg (org-babel-where-is-src-block-result))
                      (elem (and (goto-char beg) (org-element-context)))
                      (end (- (org-element-property :end elem)
                              (org-element-property :post-blank elem))))
            (funcall
             (cond
              ((fboundp 'org-link-preview) #'org-link-preview-region)
              ((featurep 'org-image-preview) #'org-image-preview--in-region)
              (t #'org-display-inline-images))
             nil 'refresh beg end))
        (error (message "Could not display images: %S" err)))))

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

;;;----------------------------------------------------------------
;; ** OX-HUGO
;;;----------------------------------------------------------------
(use-package ox-hugo
  :ensure t
  :defer
  :config
  (advice-add 'org-blackfriday--update-ltximg-path
              :around
              (lambda (orig-fn html-string)
                (if (plist-get org-html-latex-image-options :inline)
                    html-string
                  (funcall orig-fn html-string)))
              '((name . inline-image-workaround)))
  (use-package org-glossary
    :ensure (:host github :repo "tecosaur/org-glossary")
    :config
    (setq org-glossary-toplevel-only nil)
    (org-glossary-set-export-spec 'hugo t
      :use "<a class=\"org-gls\" href=\"#gls-%K\">%t</a>"
      :definition "<a name=\"gls-%K\">%t</a>"
      :definition-structure "%d\n\\colon{} %v [%n uses]\n")
    (org-glossary-set-export-spec 'hugo 'glossary
      :heading "*** Glossary")
    (defun org-glossary--expand-print-keyword (backend terms keyword)
      "Call `org-glossary--expand-print' with paramaters and terms based on KEYWORD.
BACKEND is passed through unmodified, but TERMS may be modified depending on
the :consume parameter extracted from KEYWORD."
      (let ((heading (org-element-lineage keyword '(headline org-data)))
            (parameters (org-combine-plists
                         org-glossary-default-print-parameters
                         (org-glossary--parse-print-keyword-value
                          (org-element-property :value keyword)))))
        (while (and heading
                    (not (eq (org-element-type heading) 'org-data))
                    (> (org-element-property :level heading)
                       (plist-get parameters :level)))
          (setq heading (org-element-lineage heading '(headline org-data))))
        (org-glossary--expand-print
         backend
         (org-glossary--extract-uses-in-region
          terms
          (if heading (org-element-property :begin heading) (point-min))
          (if heading (org-element-property :end heading) (point-max))
          (plist-get parameters :type)
          (plist-get parameters :consume))
         parameters))))
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
                     (if sec (downcase sec) "software")
                     "/" title)))
          (progn (save-buffer)
                 (unless (bound-and-true-p org-hugo-auto-export-mode)
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
;; Disabled since org-gcal is broken for me right now.
(use-package org-gcal
  :ensure t
  :after org
  :commands (org-gcal-sync org-gcal-fetch my/org-gcal-sync-maybe)
  ;; :hook (org-agenda-mode . my/org-gcal-sync-maybe)
  :config
  (setq plstore-encrypt-to user-full-name)
  ;; (setq plstore-cache-passphrase-for-symmetric-encryption t)
  (setq org-gcal-dir (dir-concat user-cache-directory "org-gcal/"))
  ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  (setq org-gcal-client-id my-org-gcal-client-id
        org-gcal-client-secret my-org-gcal-client-secret
        org-gcal-up-days 60
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
  
  ;; Without this 'org-gcal is not registered as an oauth provider
  (org-gcal-reload-client-id-secret)

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
                          43200)
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
    :init (setq org-ref-default-bibliography '("~/Documents/research/control_systems.bib"))))

(use-package ox-reveal
  :disabled 
  :ensure t
  :init
  (setq org-reveal-root "file:///home/karthik/.local/share/git/reveal.js")
  (setq org-reveal-hlevel 2))

;;;----------------------------------------------------------------
;; ** +INKSCAPE-FIGURES+ DONT
;;;----------------------------------------------------------------
(use-package inkscape-figures
  :disabled
  :after org
  :bind (:map org-mode-map
              ("C-c i" . #'+inkscape-figures-create-at-point-org)
              ("C-c e" . #'+inkscape-figures-edit)))

;;;----------------------------------------------------------------
;; ** ORG-REF DONT
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
        org-ref-pdf-directory "~/Documents/research/lit/"))

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
           :completion-function (list my/org-publish-rsync-html-and-figures-only)
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
          ("sicm"
           :base-directory "~/Documents/courses/sicm/"
           :base-extension "org"
           :publishing-directory "~/Documents/abode/sicm"
           :remote-directory "root@abode.karthinks.com:/var/www/abode/sicm"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-level 5
           :auto-preamble t
           :completion-function (list my/org-publish-rsync-html-and-figures-only))
          )
        )

  (defun my/org-publish-rsync (project-plist)
    "Sync output of org project to a remote server using RSYNC.

 All files and folders except for ORG files will be synced."
    (if (executable-find "rsync")
        (let* ((basedir (expand-file-name
                         (file-name-as-directory
                          (plist-get project-plist :publishing-directory))))
               (destdir (plist-get project-plist :remote-directory)))
          (start-process "rsync-project-abode" "*project-abode-output*"
                         "rsync" "-a" "-v" "--exclude=*.org" "--delete"
                         basedir destdir))

      (display-warning 'org-publish
                       "Could not find RSYNC in PATH. Project not uploaded to server."
                       :warning)))

  (defun my/org-publish-rsync-html-and-figures-only (project-plist)
    "Sync output of org project to a remote server using RSYNC.

 All files and folders except for ORG files will be synced."
    (if (executable-find "rsync")
        (let* ((basedir (expand-file-name
                         (file-name-as-directory
                          (plist-get project-plist :publishing-directory))))
               (destdir (plist-get project-plist :remote-directory)))
          (message "Running rsync: %s → %s" basedir destdir)
          (start-process "rsync-project-html-and-figures" "*project-rsync-html-output*"
                         "rsync" "-a" "-v" 
                         "--include=*.png"
                         "--include=*.html"
                         "--include=/figures/***"
                         "--exclude=*"
                         "--delete"
                         basedir destdir))

      (display-warning 'org-publish
                       "Could not find RSYNC in PATH. Project not uploaded to server."
                       :warning))))

;;;----------------------------------------------------------------
;; ** ORG-TREE-SLIDE
;;;----------------------------------------------------------------
;; Presentations from within org-mode.
(use-package org-tree-slide
  :ensure t
  :after org
  :commands my/org-presentation-mode
  ;; :hook (org-tree-slide-after-narrow . my/org-tree-slide-enlarge-latex-preview)
  :config
  (setq org-tree-slide-never-touch-face nil
        org-tree-slide-skip-outline-level 8
        org-tree-slide-heading-emphasis nil
        org-tree-slide-cursor-init nil
        org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message
        (propertize "ORG PRESENTATION STARTED" 'face 'success)
        org-tree-slide-deactivate-message
        (propertize "ORG PRESENTATION STOPPED" 'face 'error))
  
  ;; (defun my/org-tree-slide-enlarge-latex-preview ()
  ;;   (dolist (ov (overlays-in (point-min) (point-max)))
  ;;     (if (eq (overlay-get ov 'org-overlay-type)
  ;;             'org-latex-overlay)
  ;;         (overlay-put
  ;;          ov 'display
  ;;          (cons 'image 
  ;;                (plist-put
  ;;                 (cdr (overlay-get ov 'display))
  ;;                 :scale (+ 1.0 (* 0.2 text-scale-mode-amount))))))))

  (defvar olivetti-style)
  (define-minor-mode my/org-presentation-mode
    "Parameters for plain text presentations with `org-mode'."
    :init-value nil
    :global nil
    (if my/org-presentation-mode
        (progn
          (unless (eq major-mode 'org-mode)
            (user-error "Not in an Org buffer"))
          (setq-local org-hide-emphasis-markers t)
          (org-tree-slide-mode 1)
          (setq olivetti-style nil)
          (setq line-spacing 0.12)
          ;; (setq olivetti-margin-width 14)
          ;; (setq olivetti-body-width 0.7)
          (text-scale-increase 6)
          (my/olivetti-mode 1))
      (org-tree-slide-mode -1)
      ;; (kill-local-variable 'org-hide-emphasis-markers)
      (my/olivetti-mode -1)
      (text-scale-decrease 6)
      (text-scale-mode -1)))

  :bind (("C-c P"      . my/org-presentation-mode)
         :map org-tree-slide-mode-map
         ("<next>" . org-tree-slide-move-next-tree)
         ("<prior>" . org-tree-slide-move-previous-tree)
         ("<home>" . 'org-tree-slide-display-header-toggle)
         ("<C-down>"  . org-tree-slide-display-header-toggle)
         ("<C-right>" . org-tree-slide-move-next-tree)
         ("<C-left>"  . org-tree-slide-move-previous-tree)))

;;;----------------------------------------------------------------
;; ** ORG-MIME 
;;;----------------------------------------------------------------
;; Compose HTML emails in org-mode.
(use-package org-mime
  :ensure t
  :after (notmuch org)
  :defer
  :config
  (setq org-mime-export-options '(:with-latex dvipng
                                  :section-numbers nil
                                  :with-author nil
                                  :with-toc nil))
  (setq org-mime-org-html-with-latex-default 'dvipng)
  
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
;; ** ORG-NOTER
;;;----------------------------------------------------------------
(use-package org-noter
  :ensure t
  :bind (:map org-noter-notes-mode-map
         ("C-c ." . my/org-noter-associate-this-entry))
  :config
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil
        org-noter-doc-split-fraction '(0.4 . 0.5)
        org-noter-hide-other nil
        org-noter-disable-narrowing t
        org-noter-use-indirect-buffer nil
        ;; This is buggy
        org-noter-prefer-root-as-file-level nil)

  (remove-hook 'org-noter--show-arrow-hook
               #'org-noter-pdf--show-arrow)

  (defun my/org-noter-associate-this-entry (&optional arg)
    "Associate the current Org entry with the current org-noter location.

With prefix arg, use a precise location."
    (interactive "P")
    (org-noter--with-valid-session
     (let* ((precise-info (if arg (org-noter--get-precise-info) 'interactive))
            (location (org-noter--doc-approx-location precise-info)))
       (org-entry-put nil org-noter-property-note-location
                      (org-noter--pretty-print-location location)))))

  (defun my/org-noter--stay (orig-fn)
    "After a sync action, stay in the doc or notes window.

Whichever was already active."
    (let ((win (selected-window))
          (inhibit-redisplay t))
      (funcall orig-fn)
      (unless (eq (selected-window) win)
        (select-window (org-noter--get-notes-window)))))

  (dolist (sym '(org-noter-sync-next-note
                 org-noter-sync-prev-note
                 org-noter-sync-current-note
                 org-noter-sync-next-page-or-chapter
                 org-noter-sync-prev-page-or-chapter
                 org-noter-sync-current-page-or-chapter))
    (advice-add sym :around #'my/org-noter--stay)))

;;;----------------------------------------------------------------
;; ** ORG-ALERT
;;;----------------------------------------------------------------
(use-package org-alert
  :disabled
  :ensure t
  :after org-agenda
  :config
  (setq org-alert-style 'notifications))

;;;----------------------------------------------------------------
;; ** ORG-XOURNALPP
;;;----------------------------------------------------------------
(use-package org-xournalpp
  :disabled
  :ensure (:host gitlab
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
(use-package toc-org :ensure t :defer)

;;;----------------------------------------------------------------
;; ** Integration for RefTeX
;;;----------------------------------------------------------------
(use-package consult-reftex
  :disabled
  :after (org latex reftex)
  :bind (:map org-mode-map
         ("C-c )" . 'consult-reftex-insert-reference)))

(use-package reftex-xref
  :after (org latex)
  :hook (org-mode . reftex-xref-activate))

;;;----------------------------------------------------------------
;; ** ORG-AUCTEX
;;;----------------------------------------------------------------
(use-package org-auctex
  :disabled
  :load-path "plugins/org-auctex"
  :commands org-auctex-mode
  :after org
  :defer)

;;;----------------------------------------------------------------
;; ** ORGLINK
;;;----------------------------------------------------------------
;; TODO: Orglink is bugged: turning it on changes overlay
;; information across the buffer, causing packages like macrostep,
;; iedit and hl-todo to misbehave.
(use-package orglink
  :disabled
  :ensure t
  :after org
  ;; :hook (prog-mode . orglink-mode)
  :defer)

;;;----------------------------------------------------------------
;; ** ORG-QL
;;;----------------------------------------------------------------
(use-package org-ql
  :ensure t
  :bind (:map mode-specific-map
         ("o s" . org-ql-search)
         ("o v" . org-ql-view)
         ("o l" . org-ql-open-link)
         :map org-agenda-mode-map
         ("C-M-n" . my/org-agenda-next-header)
         ("C-M-p" . my/org-agenda-previous-header)
         ("C-c o w" . org-ql-refile)
         :map org-mode-map
         ("C-c o w" . org-ql-refile)
         ("C-c o f" . org-ql-find))
  :config
  (use-package org-ql-view
    :bind (:map org-ql-view-map
           ("q" . quit-window)))
  (defun my/org-agenda-next-header (&optional arg)
    (interactive "p")
    (dotimes (_ (or arg 1))
      (when (text-property-search-forward
             'face nil
             (lambda (_ val)
               (memq val '(org-super-agenda-header
                           org-agenda-structure
                           org-agenda-structure-secondary)))
             t)
        (forward-line 1))))
  (defun my/org-agenda-previous-header (&optional arg)
    (interactive "p")
    (dotimes (_ (or arg 1))
      (forward-line -1)
      (text-property-search-backward
       'face nil
       (lambda (_ val)
         (memq val '(org-super-agenda-header
                     org-agenda-structure
                     org-agenda-structure-secondary)))
       t)
      (forward-line 1))))

(provide 'setup-org)

