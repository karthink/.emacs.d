(use-package latex
  :after tex
  :ensure (auctex :pre-build (("./autogen.sh")
                              ("./configure"
                               "--without-texmf-dir"
                               "--with-packagelispdir=./"
                               "--with-packagedatadir=./")
                              ("make"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :hook ((LaTeX-mode . electric-pair-mode)
         (LaTeX-mode . my/latex-with-outline))
  :mode ("\\.tex\\'" . latex-mode)
  :defines (TeX-auto-save
            TeX-parse-self
            TeX-electric-escape
            TeX-PDF-mode
            TeX-source-correlate-method
            TeX-newline-function
            TeX-view-program-list
            TeX-view-program-selection
            TeX-mode-map)
  :bind ( :map LaTeX-mode-map
          ("M-RET" . LaTeX-insert-item)
          :map TeX-source-correlate-map     
          ([C-down-mouse-1] . TeX-view-mouse)
          :map TeX-output-mode-map
          ("M-g n" . nil)
          ("M-g p" . nil)
          :map TeX-mode-map
          ([remap next-error] . nil)
          ([remap previous-error] . nil))
  :config
  ;; (add-to-list 'Info-directory-list "/usr/share/texmf-dist/tex/texinfo/")
  (defun my/latex-with-outline ()
    (add-to-list 'minor-mode-overriding-map-alist
                 `(outline-minor-mode . ,outline-minor-mode-map))
    (outline-minor-mode 1))
  
  (use-package embrace
      :bind (:map TeX-mode-map
             ("M-s a" . embrace-add)
             ("M-s c" . embrace-change)
             ("M-s d" . embrace-delete)))

  (add-hook 'LaTeX-mode-hook (poi-register 'TeX-next-error))

  (defvar my-preamble-file (concat (expand-file-name
                                    (file-name-as-directory "~/Documents/"))
                                   "hwstyle.tex")
    "File containing my stock preamble for LaTeX documents")
  ;; (defun TeX-matrix-spacer () (interactive) (insert " & "))
  (defun TeX-insert-smallmatrix () (interactive)
         (insert "[\\begin{smallmatrix}  \\end{smallmatrix}]")
         (backward-char 19))
  (defun TeX-insert-bmatrix () (interactive)
         (insert "\\begin{bmatrix}  \\end{bmatrix}")
         (backward-char 14))

  (setf (alist-get '(major-mode . TeX-special-mode)
                   display-buffer-alist)
        `((display-buffer-at-bottom)
          (window-height . ,(lambda (win) (fit-window-to-buffer win
                                      (floor (frame-height) 3))))))

  (setq
   TeX-auto-save t
   TeX-parse-self t
   TeX-electric-escape nil
   ;; Setting this to t messes up previews
   ;; If previews still don't show disable the hyperref package
   TeX-PDF-mode nil
   TeX-error-overview-open-after-TeX-run nil)
  (setq LaTeX-command "latex")
  (setq-default TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq-default TeX-source-correlate-start-server t)
  (setq TeX-newline-function 'reindent-then-newline-and-indent)
  ;; (setq TeX-PDF-from-DVI "Dvips") ; Set to nil to call pdflatex directly
  (cond ((equal system-type 'cygwin)
         (setq TeX-view-program-list
               '(("Sumatra PDF" ("\"/cygdrive/c/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                                 (mode-io-correlate " -forward-search %b %n ") " %o"))))

         (setq TeX-view-program-selection
               '(((output-dvi has-no-display-manager) "dvi2tty")
                 ((output-dvi style-pstricks) "dvips and gv")
                 (output-dvi "xdvi")
                 (output-pdf "pdf-tools")
                 (output-html "xdg-open"))))
        ((equal system-type 'gnu/linux)
         (setq TeX-view-program-selection
               '(((output-dvi has-no-display-manager) "dvi2tty")
                 ((output-dvi style-pstricks) "dvips and gv")
                 (output-dvi "xdvi")
                 (output-pdf "PDF Tools")
                 ;; (output-pdf "Zathura")
                 (output-html "xdg-open")))))
  (define-advice TeX-active-buffer (:before-while () "livep")
    (buffer-live-p TeX-command-buffer))
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; Some structural navigation tweaks for Latex mode.
(use-package latex
  :defer
  :bind (:map LaTeX-mode-map
         ("C-M-u" . LaTeX-backward-up-list)
         ("C-M-e" . LaTeX-forward-environment)
         ("C-M-a" . LaTeX-backward-environment))
  :config
  ;; Monkey patching: Stop this from marking to the end of the line at the end
  ;; of the env.
  (defun LaTeX-mark-environment (&optional count)
    "Set mark to end of current environment and point to the matching begin.
If prefix argument COUNT is given, mark the respective number of
enclosing environments.  The command will not work properly if
there are unbalanced begin-end pairs in comments and verbatim
environments."
    (interactive "p")
    (setq count (if count (abs count) 1))
    (let ((cur (point)) beg end)
      ;; Only change point and mark after beginning and end were found.
      ;; Point should not end up in the middle of nowhere if the search fails.
      (save-excursion
        (dotimes (_ count) (LaTeX-find-matching-end))
        (setq end (point))
        (goto-char cur)
        (dotimes (_ count) (LaTeX-find-matching-begin))
        (setq beg (point)))
      (push-mark end)
      (goto-char beg)
      (TeX-activate-region)))
  (defun LaTeX-backward-up-list (&optional arg)
    (interactive "p")
    (let ((total (or arg 1)))
      (condition-case at-top-level
          (dotimes (_ arg)
            (up-list -1 t t)
            (setq total (1- total)))
        ('user-error
         (dotimes (_ (max 0 total))
           (LaTeX-find-matching-begin))))))
  (defun LaTeX-forward-environment (&optional N do-push-mark)
    "Move to the \\end of the next \\begin, 
or to the \\end of the current environment
(whichever comes first) N times.

Never goes into deeper environments.

DO-PUSH-MARK defaults to t when interactive,
but mark is only pushed if region isn't active."
    (interactive "p")
    (unless (region-active-p)
      (when do-push-mark (push-mark)))
    (let ((start (point))
          (count (abs N))
          (direction (if (< N 0) -1 1)))
      (while (and (> count 0)
                  (re-search-forward "\\\\\\(begin\\|end\\)\\b"
                                     nil t direction))
        (cl-decf count)
        (if (or (and (> direction 0) (looking-back "begin" (- (point) 7)))
                (looking-at "\\\\end"))
            (unless (funcall (if (> direction 0)
                                 #'LaTeX-find-matching-end
                               #'LaTeX-find-matching-begin))
              (error "Unmatched \\begin?"))
          (when (looking-at "\\[") (forward-sexp 1))
          (when (looking-at "{") (forward-sexp 1))))))
  (defun LaTeX-backward-environment (&optional N do-push-mark)
    "Move to the \\begin of the next \\end,
or to the \\begin of the current environment
(whichever comes first) N times.

Never goes into deeper environments.

DO-PUSH-MARK defaults to t when interactive,
but mark is only pushed if region isn't active."
    (interactive "p")
    (LaTeX-forward-environment (- N) do-push-mark)))

(use-package latex
  :defer
  :if (version<= "28.0" emacs-version)
  :config
  (defvar my/TeX-error-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'TeX-next-error)
      (define-key map "p" 'TeX-previous-error)
      map))
  (put 'TeX-next-error 'repeat-map 'my/TeX-error-map)
  (put 'TeX-previous-error 'repeat-map 'my/TeX-error-map))

(use-package tex-fold
  :after latex
  :defer
  :custom-face
  (TeX-fold-folded-face ((t (:inherit shadow))))
  (TeX-fold-unfolded-face ((t (:background unspecified))))
  :bind (:map text-mode-map
         ("M-g r" . my/next-reference-or-label)
         ("M-g R" . my/previous-reference-or-label)
         :map LaTeX-mode-map
         ("M-g r" . my/next-reference-or-label))
  :config
  (defun my/next-reference-or-label (_arg)
    (interactive "p")
    (let* ((prop))
      (pcase-let
          ((`(,_ . ,ov)
           (get-char-property-and-overlay (point) 'TeX-fold-type)))
        (when ov (TeX-fold-hide-item ov)))
      (save-excursion
        (and (setq prop (text-property-search-forward
                         'face nil
                         (lambda (_ val)
                           (memq val '(font-lock-constant-face org-cite)))
                         t))))
      (if prop
          (progn (goto-char (prop-match-beginning prop))
                 (when (and (derived-mode-p 'org-mode) (org-invisible-p))
                   (org-fold-show-context 'link-search))
                 ;; (when eldoc-mode (eldoc--invoke-strategy t))
                 (pcase-let
                     ((`(,_ . ,ov)
                      (get-char-property-and-overlay (point) 'TeX-fold-type)))
                   (when ov (TeX-fold-show-item ov))))
        (message "No more references/labels."))))
  (defun my/previous-reference-or-label (_arg)
    (interactive "p")
    (let ((p))
      (save-excursion
        (and (text-property-search-backward
              'face nil
              (lambda (_ val)
                (memq val '(font-lock-constant-face org-cite
                            TeX-fold-folded-face)))
              t)
             (setq p (point))))
      (pcase-let
          ((`(,_ . ,ov)
            (get-char-property-and-overlay (point) 'TeX-fold-type)))
        (when ov (TeX-fold-hide-item ov)))
      (when p
        (goto-char p)
        (when (and (derived-mode-p 'org-mode) (org-invisible-p))
          (org-fold-show-context 'link-search))
        ;; (when eldoc-mode (eldoc--invoke-strategy t))
        )
      (pcase-let
          ((`(,_ . ,ov)
            (get-char-property-and-overlay (point) 'TeX-fold-type)))
        (when ov (TeX-fold-show-item ov)))))
  (defvar-keymap my/TeX-ref-map
    :repeat t
    "r" 'my/next-reference-or-label
    "R" 'my/previous-reference-or-label
    "n" 'my/next-reference-or-label
    "p" 'my/previous-reference-or-label)

  (setq ;; TeX-fold-folded-face '((t (:height 1.0 :foreground "SlateBlue1")))
        TeX-fold-auto nil
        TeX-fold-type-list '(macro))    ;do not include "comment", fails in Org mode
  (set-face-attribute 'TeX-fold-folded-face nil :foreground nil :inherit 'shadow)
  ;; Custom folded display for labels and refs
  (defun my/TeX-fold-ref (text)
         (let* ((m (string-match "^\\([^:]+:\\)\\(.*\\)" text))
                (cat (or (match-string 1 text) ""))
                (ref (or (match-string 2 text) text)))
           (setq ref
                 (if (> (length ref) 13)
                     (concat (substring ref 0 6) "..." (substring ref -6))
                     ;; (concat "..." (substring ref -14))
                   ref))
           (concat "[" (propertize cat 'face 'shadow) ref "]")))
  (defun my/TeX-fold-label (&rest texts)
    (cl-loop for text in texts
             for m = (string-match "^\\([^:]+:\\)\\(.*\\)" text)
             for cat = (or (match-string 1 text) "")
             for ref = (or (match-string 2 text) text)
             collect (concat "[" (propertize cat 'face 'shadow) ref "]") into labels
             finally return (mapconcat #'identity labels ",")))
  (setq-default TeX-fold-macro-spec-list
        '(("[f]" ("footnote" "marginpar"))
          (my/TeX-fold-label ("cite"))
          (my/TeX-fold-label ("label"))
          (my/TeX-fold-ref ("ref" "pageref" "eqref" "footref" "cref"))
          ("[i]" ("index" "glossary"))
          ("[1]:||*" ("item"))
          ("..." ("dots"))
          ("(C)" ("copyright"))
          ("(R)" ("textregistered"))
          ("TM"  ("texttrademark"))
          ;; (1 ("part" "chapter" "section" "subsection" "subsubsection"
          ;;     "paragraph" "subparagraph"
          ;;     "part*" "chapter*" "section*" "subsection*" "subsubsection*"
          ;;     "paragraph*" "subparagraph*"
          ;;     "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
          ;;     "textbf" "textsc" "textup"))
          ))
  
  ;; (cl-delete-if (lambda (s) (or (equal s "[l]")
  ;;                          (equal s "[r]")))
  ;;               TeX-fold-macro-spec-list :key #'car)
  ;; (setf (alist-get 'my/TeX-fold-ref TeX-fold-macro-spec-list)
  ;;       '(("ref" "pageref" "eqref" "footref")))
  ;; (setf (alist-get 'my/TeX-fold-label TeX-fold-macro-spec-list)
  ;;       '(("label")))
  )

(use-package latex-extra
  :disabled
  :after latex
  :defines (latex-extra-mode)
  :hook (LaTeX-mode . latex-extra-mode))
;; :config
;; (defface latex/unimportant-latex-face
;;  '((t :height 0.7
;;       :inherit font-lock-comment-face))
;;  "Face used on less relevant math commands.")

;; (font-lock-add-keywords
;;  'latex-mode
;;  `((,(rx (or (and "\\" (or (any ",.!;")
;;                            (and (or "left" "right"
;;                                     "big" "Big")
;;                                 symbol-end)))
;;              (any "_^")))
;;     0 'latex/unimportant-latex-face prepend))
;;  'end)

(use-package preview
  :after latex
  :hook (LaTeX-mode . my/preview-scale-larger)
  :config
  (define-key LaTeX-mode-map (kbd "C-c C-x") preview-map)
  (defun my/preview-scale-larger ()
    "Increase the size of `preview-latex' images"
    (setq preview-scale-function 
          (lambda nil (* 1.25 (funcall (preview-scale-from-face)))))))

(use-package reftex
  :after latex
  :defer 2
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :config
  (setq reftex-default-bibliography '("~/Documents/roam/biblio.bib"))
  (setq reftex-insert-label-flags '("sf" "sfte"))
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-ref-style-default-list '("Default" "AMSmath" "Cleveref"))
  (setq reftex-use-multiple-selection-buffers t))

(use-package consult-reftex
  :ensure (:host github :protocol ssh
           :repo "karthink/consult-reftex")
  ;; :load-path "plugins/consult-reftex/"
  :after (reftex consult embark)
  :bind (:map reftex-mode-map
         ("C-c )"   . consult-reftex-insert-reference)
         ("C-c M-." . consult-reftex-goto-label)
         :map org-mode-map
         ("C-c (" . consult-reftex-goto-label)
         ("C-c )"   . consult-reftex-insert-reference))
  :config
  ;; (define-advice reftex-label (:around (label-fn &rest args) show-labels)
  ;;   (let* ((enable-recursive-minibuffers t)
  ;;          (minibuffer-local-map
  ;;           (make-composed-keymap
  ;;            (define-keymap
  ;;              "C-i" (lambda ()
  ;;                      (interactive)
  ;;                      (when-let
  ;;                          (label
  ;;                           (with-minibuffer-selected-window
  ;;                             (consult-reftex-select-reference current-prefix-arg t)))
  ;;                        (delete-minibuffer-contents)
  ;;                        (insert label))))
  ;;            minibuffer-local-map)))
  ;;     (apply label-fn args)))
  (with-eval-after-load 'embark
    (defun consult-reftex--key-finder ()
      (when (and
             (or (derived-mode-p 'LaTeX-mode)
                 (derived-mode-p 'org-mode))
             (cl-intersection
              '(font-lock-constant-face TeX-fold-unfolded-face)
              (ensure-list (get-char-property (point) 'face))))
        (save-excursion
          (text-property-search-backward
           'face 'font-lock-constant-face
           (lambda (val prop) (memq val (ensure-list prop))))
          (when (looking-back "\\(?:la\\)?\\(?:ref\\|bel\\){" (- (point) 5))
            (backward-char 1)
            (let* ((start (1+ (point)))
                   (end (progn (forward-list)
                               (1- (point)))))
              `(reftex-label
                ,(buffer-substring-no-properties start end)
                ,start .  ,end))))))
    (cl-pushnew 'consult-reftex--key-finder embark-target-finders))
  (setq consult-reftex-preview-function
        #'consult-reftex-preview-make-window
        consult-reftex-preferred-style-order
        '("\\cref" "\\eqref" "\\ref"))
  (consult-customize consult-reftex-insert-reference
                     :preview-key (list :debounce 0.3 'any)))

;; (setq-default TeX-master nil)
(use-package cdlatex
  :after latex
  :ensure t
  ;; :commands turn-on-cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
         (LaTeX-mode . cdlatex-electricindex-mode))
  :bind (:map cdlatex-mode-map
         ("[" . nil) ("(" . nil) ("{" . nil)
         ("<tab>" . cdlatex-tab))
  :defines (cdlatex-math-symbol-prefix cdlatex-command-alist)
  :config
  (setq cdlatex-math-symbol-prefix ?\;)
  (define-key cdlatex-mode-map
              (cdlatex-get-kbd-vector cdlatex-math-symbol-prefix)
              #'cdlatex-math-symbol)
  (dolist (cmd '(("vc" "Insert \\vect{}" "\\vect{?}"
                  cdlatex-position-cursor nil nil t)
                 ("tfr" "Insert \\tfrac{}{}" "\\tfrac{?}{}"
                  cdlatex-position-cursor nil nil t)
                 ("sfr" "Insert \\sfrac{}{}" "\\sfrac{?}{}"
                  cdlatex-position-cursor nil nil t)
                 ("abs" "Insert \\abs{}" "\\abs{?}"
                  cdlatex-position-cursor nil nil t)
                 ("equ*" "Insert equation* env"
                  "\\begin{equation*}\n?\n\\end{equation*}"
                  cdlatex-position-cursor nil t nil)
                 ("sn*" "Insert section* env"
                  "\\section*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("ss*" "Insert subsection* env"
                  "\\subsection*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("sss*" "Insert subsubsection* env"
                  "\\subsubsection*{?}"
                  cdlatex-position-cursor nil t nil)))
    (push cmd cdlatex-command-alist))

  (setq cdlatex-env-alist
        '(("align" "\\begin{align}
?
\\end{align}" "\\\\AUTOLABEL
?")
          ("equation" "\\begin{equation}
?
\\end{equation}" nil)))
  
  (setq cdlatex-math-symbol-alist '((?F ("\\Phi"))
                                    (?o ("\\omega" "\\mho" "\\mathcal{O}"))
                                    (?. ("\\cdot" "\\circ"))
                                    (?6 ("\\partial"))
                                    (?v ("\\vee" "\\forall"))
                                    (?^ ("\\uparrow" "\\Updownarrow" "\\updownarrow"))))
  (setq cdlatex-math-modify-alist '((?k "\\mathfrak" "\\textfrak" t nil nil)
                                    (?b "\\mathbf" "\\textbf" t nil nil)
                                    (?B "\\mathbb" "\\textbf" t nil nil)
                                    (?t "\\text" nil t nil nil)))
  (setq cdlatex-paired-parens "$[{(")
  (cdlatex-reset-mode))

;; Make cdlatex play nice inside org tables
(use-package lazytab
  :load-path "plugins/lazytab/";; 
  :bind (:map orgtbl-mode-map
              ("<tab>" . lazytab-org-table-next-field-maybe)
              ("TAB" . lazytab-org-table-next-field-maybe))
  :after cdlatex
  :demand t
  :config
  (add-hook 'cdlatex-tab-hook #'lazytab-cdlatex-or-orgtbl-next-field 90)
  (dolist (cmd '(("smat" "Insert smallmatrix env"
                  "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                  lazytab-position-cursor-and-edit
                  nil nil t)
                 ("bmat" "Insert bmatrix env"
                  "\\begin{bmatrix} ? \\end{bmatrix}"
                  lazytab-position-cursor-and-edit
                  nil nil t)
                 ("pmat" "Insert pmatrix env"
                  "\\begin{pmatrix} ? \\end{pmatrix}"
                  lazytab-position-cursor-and-edit
                  nil nil t)
                 ("tbl" "Insert table"
                  "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                  lazytab-position-cursor-and-edit
                  nil t nil)))
    (push cmd cdlatex-command-alist))
  (cdlatex-reset-mode))

(provide 'setup-latex)
