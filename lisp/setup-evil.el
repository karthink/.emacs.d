;; -*- lexical-binding: t -*-
(use-package evil-leader
  :ensure
  :commands global-evil-leader-mode
  :init
  (setq evil-want-keybinding nil)
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "cB"
    (lambda () "Byte-compile file"
      (interactive)
      (if buffer-file-name
          (byte-compile-file buffer-file-name)
        (message "Not visiting a file!"))))
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "cL"
    (lambda () "Load current file"
      (interactive)
      (if buffer-file-name
          (load-file buffer-file-name)
        (message "Not visiting a file!"))))

  (evil-leader/set-key-for-mode 'latex-mode
    "cc" 'TeX-command-master
    "ca" 'TeX-command-run-all
    "=" 'reftex-toc
    "("  'reftex-label
    ")" 'reftex-reference
    "[" 'reftex-citation
    "{" 'cdlatex-environment
    "." 'ivy-bibtex)

  (evil-leader/set-key-for-mode 'reftex-toc-mode
    (kbd "SPC") 'reftex-toc-view-line)

  (evil-leader/set-key
    ;; Ace window mode
    "<tab>" 'ace-window
    )

  (evil-leader/set-key-for-mode 'org-mode
    "ce" 'org-export-dispatch
    "/t" 'org-tags-sparse-tree
    "/m" 'org-sparse-tree
    "/." 'org-sparse-tree
    "ol" 'org-insert-link
    "ot" 'org-todo
    "cc" 'org-ctrl-c-ctrl-c
    "oo" 'org-ctrl-c-ctrl-c
    "oc" 'org-ctrl-c-ctrl-c
    "oy" 'org-copy-visible
    "or" 'org-reveal
    "o*" 'org-ctrl-c-star
    "{"  'org-cdlatex-environment-indent
    "op" 'org-set-property
    )
  (let ((counselp (featurep 'counsel)))

    (evil-leader/set-key
      ;; Help
      "hf" (if counselp 'counsel-describe-function 'describe-function)
      "hb" (if counselp 'counsel-descbinds 'describe-bindings)
      "hv" (if counselp 'counsel-describe-variable 'describe-variable)
      "hk" 'describe-key
      "hm" 'describe-mode
      "ha" 'apropos-command
      "hd" 'apropos-documentation
      "hc" 'describe-key-briefly

      ))

  (evil-leader/set-key
    
    ;; Calc
    "*" 'calc-dispatch
    ;; "e" 'find-file
    "u"  'universal-argument

    ;; File commands
    "fz" 'counsel-fzf
    "ff" 'counsel-find-file
    "f." 'counsel-find-file
    "fS" 'sudo-find-file
    "fd" 'dired
    ;; "f~" (lambda () "Find file from ~/" (find-fil))
    "fr" 'counsel-recentf
    "fj" 'counsel-file-jump
    "fg" 'counsel-git
    "f'" 'counsel-bookmark
    "fm" 'counsel-bookmark
    ;; "fE" 'find-file-emacs-config
    "fD" 'find-file-Documents
    "fR" 'find-file-Research
    "fc" 'find-file-config-dirs
    "fC" 'find-file-config-dirs

    ;; Ivy general
    "I" 'ivy-resume
    
    ;; Searching
    "//" 'counsel-grep-or-swiper
    "/q" 'query-replace-regexp
    "/s" 'replace-regexp
    "/r" 'query-replace
    "/S" 'batch-replace-strings
    "/a" 'counsel-ag
    "/g" 'counsel-grep
    "/G" 'counsel-git-grep

    ;; Buffer commands
    "b" 'switch-to-buffer
    "w" 'save-buffer
    "q" 'evil-quit
    "`" 'evil-switch-to-windows-last-buffer
    ;; Navigation commands
    "j" 'ace-jump-mode

    "k" 'kill-this-buffer
    "K" 'kill-buffer-and-window
    "n" (lambda (&optional arg)
          "Next Buffer"
                  (interactive "P")
                  (if arg (previous-user-buffer) (next-user-buffer)))
    "p" (lambda (&optional arg)
          "Previous Buffer"
                  (interactive "P")
                  (if arg (next-user-buffer) (previous-user-buffer)))
    "N" 'next-buffer
    "P" 'previous-buffer
    "B" (lambda ()
          "Open Bibtex file"
          (interactive)
          (find-file-other-window (getenv "BIB")))
    "," 'er/expand-region
    "cn" 'next-error
    "cp" 'previous-error
    "vf" 'ido-find-file-other-window
    "vb" 'ido-switch-buffer-other-window

    )
  :config
  ;; Helper functions
  
;; ;;;###autoload
;;   (defun find-file-system-config ()
;;     "Find file in system config"
;;     (interactive)
;;     (let ((configdir (getenv "CONFIGDIR")))
;;       (if configdir
;;           (counsel-file-jump "" (getenv "CONFIGDIR"))
;;         (message "ENV variable CONFIGDIR not set"))))

;; ;;;###autoload
;;   (defun find-file-emacs-config ()
;;     "Find file in emacs config"
;;     (interactive)
;;     (counsel-file-jump "" (concat
;;                            (file-name-as-directory (getenv "HOME"))
;;                            ".emacs.d")))

;;;###autoload
  (defun find-file-Documents ()
    "Find file in user documents"
    (interactive)
    (counsel-file-jump "" (concat
                           (file-name-as-directory (getenv "HOME"))
                           "Documents")))

;;;###autoload
  (defun find-file-Research ()
    "Find file in user research documents"
    (interactive)
    (counsel-file-jump "" (concat
                           (file-name-as-directory (getenv "HOME"))
                           (file-name-as-directory"Documents")
                           "research")))
  
;;;###autoload
  (defun find-file-config-dirs ()
    "Find files in all system config locations"
    (interactive)
    (counsel-file-jump-multi-dir "" (mapcar (lambda (dir) (concat
                                                      (abbreviate-file-name (file-name-as-directory (getenv "HOME")))
                                                      dir))
                                            '(".local/bin" ".emacs.d" ".config"))))
  
;;;###autoload
  (defun counsel-file-jump-multi-dir (initial-input initial-directories)
    (counsel-require-program find-program)
    (let ((all-files-list nil))
      (ivy-read "Find config file: "
                (dolist (default-directory
                         (if (listp initial-directories)
                             initial-directories
                           (list initial-directories))
                         all-files-list)
                  (setq all-files-list (append
                                        (mapcar (lambda (file)
                                                  (concat
                                                   (file-name-as-directory default-directory)
                                                   file))
                                                (counsel--find-return-list counsel-file-jump-args))
                                        all-files-list)))
                
                :matcher #'counsel--find-file-matcher
                :initial-input initial-input
                :action #'find-file
                :preselect (counsel--preselect-file)
                :require-match 'confirm-after-completion
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'counsel-file-jump))
    )
  
  )

(use-package evil
  :ensure t
  :after evil-leader
  :defines (turn-on-evil-matlab-textobjects-mode turn-on-evil-latex-textobjects-mode)
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq-default evil-symbol-word-search t)
  (setq evil-emacs-state-cursor '(hbar . 4))
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-mode-line-format '(before . mode-line-front-space))
  ;; (add-hook 'evil-jumps-post-jump-hook #'recenter)
  :bind (:map evil-motion-state-map
              ("C-w C-h" . evil-window-left)
              ("C-w C-l" . evil-window-right)
              ("C-w C-k" . evil-window-up)
              ("C-w C-j" . evil-window-down)
              ;; ("C-w C-f" . winner-redo)
              ;; ("C-w C-b" . winner-undo)
              ("C-w C-w" . winner-undo)
              ("C-w |" . toggle-window-split)
              ("C-w f" . find-file-other-window)
              ("C-w b" . switch-to-buffer-other-window)
              ("C-w S-<up>" . evil-window-increase-height)
              ("C-w S-<down>" . evil-window-decrease-height)
              ("C-w S-<left>" . evil-window-decrease-width)
              ("C-w S-<right>" . evil-window-increase-height)
              ;; :map evil-normal-state-map
              ;; ("[o" . open-next-line)
              ;; ("]o" . open-previous-line)
              )
  :config
  (evil-define-key 'normal 'global (kbd "[ SPC") (lambda (&optional arg) (interactive)
                                                   (save-excursion
                                                     (previous-line)
                                                     (end-of-line)
                                                     (open-line arg))))
  (evil-define-key 'normal 'global (kbd "] SPC") (lambda (&optional arg) (interactive)
                                                   (save-excursion
                                                     (end-of-line)
                                                     (open-line arg))))
  (setq evil-normal-state-tag   (propertize " NORMAL " 'face '((bold :background "DarkGoldenrod2" :foreground "black")))
        evil-emacs-state-tag    (propertize " EMACS  " 'face '((bold :background "SkyBlue2"       :foreground "black")))
        evil-insert-state-tag   (propertize " INSERT " 'face '((bold :background "chartreuse3"    :foreground "black")))
        evil-replace-state-tag  (propertize " REPLAC " 'face '((bold :background "chocolate"      :foreground "black")))
        evil-motion-state-tag   (propertize " MOTION " 'face '((bold :background "plum3"          :foreground "black")))
        evil-visual-state-tag   (propertize " VISUAL " 'face '((bold :background "gray"           :foreground "black")))
        evil-operator-state-tag (propertize " OPERAT " 'face '((bold :background "sandy brown"    :foreground "black"))))

  (setq evil-search-module 'evil-search)
  (add-to-list 'evil-emacs-state-modes 'undo-tree-visualizer-mode)
  (evil-mode 1)

;; (fset 'yank-pop #'counsel-yank-pop)

  ;;--------------------
  ;; EVIL KEYBINDS
  ;;--------------------

  ;; (define-key evil-motion-state-map ";" 'evil-repeat-find-char)
  ;; (define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
  (when evil-want-C-u-scroll
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
  (evil-define-key '(normal visual insert) helpful-mode-map "q" 'quit-window)
  (evil-define-key '(normal visual insert) special-mode-map "q" 'quit-window)

  (evil-define-key 'visual 'global (kbd "g-") 'narrow-to-region)
  (evil-define-key '(normal visual) 'global (kbd "g=") 'widen)

  (with-eval-after-load 'dired-sidebar
    (progn
      (evil-define-key* '(normal visual) 'global (kbd "C-w C-d") 'dired-sidebar-toggle-sidebar)))
  (with-eval-after-load 'ivy
    (progn
      (evil-define-key* 'normal 'global (kbd "gI") #'ivy-resume)))

  (with-eval-after-load 'dired-sidebar
    (progn
      (evil-define-key* 'normal 'global (kbd "C-w d") #'dired-sidebar-toggle-sidebar)))

  (with-eval-after-load 'org
    (progn
      (evil-define-key 'normal org-mode-map (kbd "g-") 'org-narrow-to-subtree)))

  (with-eval-after-load 'eyebrowse
    (progn
      (evil-define-key '(visual normal motion) eyebrowse-mode-map (kbd "gt") 'eyebrowse-next-window-config)
      (evil-define-key '(visual normal motion) eyebrowse-mode-map (kbd "gT") 'eyebrowse-last-window-config)
      (evil-define-key '(visual normal motion) eyebrowse-mode-map (kbd "gC") (lambda (arg) "Create or close eyebrowse window config"
                                                                               (interactive "P")
                                                                               (if arg
                                                                                   (eyebrowse-close-window-config)
                                                                                 (eyebrowse-create-window-config))))
      (evil-define-key '(visual normal motion) eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
      (evil-define-key '(visual normal motion) eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
      (evil-define-key '(visual normal motion) eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
      ))

  (with-eval-after-load 'matlab
    (progn
      (evil-define-key '(visual normal) matlab-mode-map (kbd "zb") 'matlab-shell-run-block)
      (evil-define-key '(visual normal) matlab-mode-map (kbd "zr") 'matlab-shell-run-region-or-line)
      (evil-define-key '(normal visual) matlab-mode-map (kbd "[[") #'matlab-backward-section)
      (evil-define-key '(normal visual) matlab-mode-map (kbd "]]") #'matlab-forward-section)))

  (defvar dotemacs--original-mode-line-bg (face-background 'mode-line))
  (defadvice evil-set-cursor-color (after dotemacs activate)
    (cond ((evil-emacs-state-p)
           (set-face-background 'mode-line "#440000"))
          ;; ((evil-insert-state-p)
          ;;  (set-face-background 'mode-line "#002244"))
          ;; ((evil-visual-state-p)
          ;;  (set-face-background 'mode-line "#440044"))
          (t
           (set-face-background 'mode-line dotemacs--original-mode-line-bg))))

  ;; Setup text-objects for use in LaTeX. Putting this here because it doesn't make sense to put this in the AucTeX section without enabling evil-mode first.
  (add-hook 'LaTeX-mode-hook (lambda () (unless (featurep 'evil-latex-textobjects)
                                     (require 'evil-latex-textobjects nil t))
                               (turn-on-evil-latex-textobjects-mode)))

  (add-hook 'matlab-mode-hook (lambda () (unless (featurep 'evil-matlab-textobjects)
                                      (require 'evil-matlab-textobjects nil t))
                                (turn-on-evil-matlab-textobjects-mode)))

  )

;;--------------------
;; OTHER EVIL PACKAGES
;;--------------------
(defvar +evil-addons-enabled-modes (list 'prog-mode-hook
                                         'conf-unix-mode-hook
                                         'conf-windows-mode-hook
                                         'conf-javaprop-mode-hook
                                         'tex-mode-hook
                                         'text-mode-hook
                                         'message-mode-hook)
  "List of modes where evil-mode addons (like commentary and snipe) should be enabled")

;; EVIL-SURROUND
;; c/d/y s {motion}{delimiter} to change/delete/add delimiter around motion.
(use-package evil-surround
  :ensure
  :commands (turn-on-evil-surround-mode
             global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :init
  (dolist (mode-hook +evil-addons-enabled-modes)
    (add-hook mode-hook (lambda () "Turn on evil commentary for mode"
                          (interactive)
                          (turn-on-evil-surround-mode))))
  ;; :config
  ;; (global-evil-surround-mode 1)
  )

;; EVIL-EMBRACE
(use-package evil-embrace
  :ensure t
  ;; :after evil-surround
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  ;; :hook ((ruby-mode enh-ruby-mode) . embrace-ruby-mode-hook)
  ;; :hook ((lisp-mode emacs-lisp-mode clojure-mode racket-mode)
  ;; . +evil-embrace-lisp-mode-hook-h)
  :hook ((org-mode LaTeX-mode latex-mode) . +evil-embrace-latex-mode-hook-h)
  :commands embrace-add-pair embrace-add-pair-regexp
  :init (evil-embrace-enable-evil-surround-integration)
  ;;(setq evil-embrace-show-help-p nil)
  :config
  (setq evil-embrace-show-help-p t)

  (defun +evil-embrace-latex-mode-hook-h ()
    (embrace-add-pair-regexp ?m "\\\\[a-z]+{" "}" #'+evil--embrace-latex
                             (embrace-build-help "\\macro{" "}"))
    (embrace-add-pair-regexp ?e "\\\\begin{[a-z]+}" "\\\\end{[a-z]+}"
                             (lambda ()
                               (let ((env (read-string "Env: ")))
                                 (cons (format "\\begin{%s}" env)
                                       (format "\\end{%s}" env))))
                             (embrace-build-help "\\begin{.}" "\\end{.}"))
    (embrace-add-pair-regexp ?$ "\\$" "\\$" nil))

  (defun +evil-embrace-lisp-mode-hook-h ()
    (push (cons ?f (make-embrace-pair-struct
                    :key ?f
                    :read-function #'+evil--embrace-elisp-fn
                    :left-regexp "([^ ]+ "
                    :right-regexp ")"))
          embrace--pairs-list))

  (defun +evil-embrace-angle-bracket-modes-hook-h ()
    (set (make-local-variable 'evil-embrace-evil-surround-keys)
         (delq ?< evil-embrace-evil-surround-keys))
    (push (cons ?< (make-embrace-pair-struct
                    :key ?<
                    :read-function #'+evil--embrace-angle-brackets
                    :left-regexp "\\[a-z]+<"
                    :right-regexp ">"))
          embrace--pairs-list))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]"))

;;;###autoload
  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
        pair
      (if-let* ((pair (assoc-default char embrace--pairs-list)))
          (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                    (funcall (embrace-pair-struct-read-function pair)))))
              real-pair
            (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
        (cons char char))))

;;;###autoload
  (defun +evil--embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

;;;###autoload
  (defun +evil--embrace-latex ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

;;;###autoload
  (defun +evil--embrace-elisp-fn ()
    "Elisp function support for embrace."
    (cons (format "(%s " (or (read-string "(") "")) ")"))

;;;###autoload
  (defun +evil--embrace-angle-brackets ()
    "Type/generic angle brackets."
    (cons (format "%s<" (or (read-string "") ""))
          ">")))


;; EVIL-COMMENTARY
;; gc{motion} to comment/uncomment
(use-package evil-commentary
  :ensure
  :diminish ""
  :commands evil-commentary-mode
  :init
  (dolist (mode-hook +evil-addons-enabled-modes)
    (add-hook mode-hook (lambda () "Turn on evil commentary for mode"
                          (interactive)
                          (evil-commentary-mode 1))))
  )

;; EVIL-EXCHANGE
;; gx{motion} to select, gx{motion} on second object to exchange
(use-package evil-exchange
  :ensure t
  :init
  (dolist (mode-hook +evil-addons-enabled-modes)
    (add-hook mode-hook (lambda () "turn on evil-exchange-install for mode"
                          (interactive)
                          (evil-exchange-install))))
  )

;; EVIL-LION
;; gl{motion}{char} to align on char
(use-package evil-lion
  :ensure t
  :init
  (dolist (mode-hook +evil-addons-enabled-modes)
    (add-hook mode-hook (lambda () "turn on evil-lion-mode for mode"
                          (interactive)
                          (evil-lion-mode))))
  )

;; EVIL-MATCHIT
;; % to match delimiters, % as text-object to manipulate
(use-package evil-matchit
  :ensure t
  :init
  (dolist (mode-hook +evil-addons-enabled-modes)
    (add-hook mode-hook (lambda () "turn on evil-matchit-mode for mode"
                          (interactive)
                          (evil-matchit-mode +1))))
  )

;; EVIL-NUMBERS
;; + and - to increment/decrement number at point
(use-package evil-numbers
  :ensure t
  ;; :bind (:map evil-normal-state-map
  ;;             ("+" . evil-numbers/inc-at-pt)
  ;;             ("-" . evil-numbers/dec-at-pt))
  
  :init
  (dolist (mode-map (list 'conf-mode-map
                          'prog-mode-map
                          'text-mode-map
                          'message-mode-map))
    (evil-define-key 'normal mode-map (kbd "+") #'evil-numbers/inc-at-pt)
    (evil-define-key 'normal mode-map (kbd "-") #'evil-numbers/dec-at-pt))
  
  )

;; EVIL-RSI
;; C-a, C-e, C-f, C-b, C-d and C-k have same definitions as in emacs mode.
;; C-n and C-p work like in emacs if auto-complete is loaded.
(use-package evil-rsi
  :ensure t
  :config
  (evil-rsi-mode))

;; EVIL-SNIPE
;; s to snipe for next occurrence of chars
;; in operator mode, z or x to operate including/excluding next ocurrence of chars
(use-package evil-snipe
  ;; :after evil-collection
  :ensure t
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-spillover-scope 'whole-visible
        evil-snipe-smart-case t
        evil-snipe-char-fold t)
  (evil-define-key 'normal snipe-local-mode-map "S" 'evil-snipe-S)
  ;; (evil-snipe-mode +1)
  ;; (evil-snipe-override-mode +1)
  (dolist (mode-hook +evil-addons-enabled-modes)
    (add-hook mode-hook (lambda () "turn on turn-on-evil-snipe-mode for mode"
                          (interactive)
                          (turn-on-evil-snipe-mode)
                          (turn-on-evil-snipe-override-mode)
                          (evil-define-key 'normal snipe-local-mode-map "S" 'evil-snipe-S)
                          )))

  :config
  ;; (dolist (mode '(Info-mode calc-mode ibuffer-mode))
  ;;   (push mode evil-snipe-disabled-modes))
  
  ;; (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
  ;; (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S)
  ;; (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  :diminish "")

;; EVIL-SPACE
;; Hit ; or , (originally <SPC>) to repeat last movement.
;; (use-package evil-space
;;   :ensure t
;;   :init
;;   (evil-space-mode)
;;   (setq evil-space-next-key ";")
;;   (setq evil-space-prev-key ","))

;; EVIL-VISUALSTAR
;; Select with visual-mode and hit * or # to find next occurrence
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent t)
  :diminish "")

;; EVIL-PAREDIT
;; (use-package evil-paredit
;;   :ensure t
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

;; ORG-EVIL
(use-package org-evil
  :ensure t)

;;-----------------
;; EVIL-SMARTPARENS
;;-----------------
(use-package evil-smartparens
  :ensure t
  :init
  (add-hook 'lisp-interaction-mode-hook #'evil-smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'evil-smartparens-mode)
  :config
  (evil-define-key 'normal evil-smartparens-mode-map (kbd "(") #'sp-backward-sexp)
  (evil-define-key 'normal evil-smartparens-mode-map (kbd ")") #'sp-forward-sexp)
  (evil-define-key 'visual evil-smartparens-mode-map (kbd "o") nil)
  ;; (evil-define-key 'normal evil-smartparens-mode-map (kbd "J") #'sp-join-sexp)
  )

;; EVIL-OWL
;; (use-package evil-owl
;;   :ensure t
;;   :config
;;   (setq evil-owl-max-string-length 500)
;;   (add-to-list 'display-buffer-alist
;;                '("*evil-owl*"
;;                  (display-buffer-in-side-window)
;;                  (side . bottom)
;;                  (window-height . 0.3)))
;;   (evil-owl-mode)
;;   )

;; EVIL-COLLECTION
(use-package evil-collection
  :ensure t
  :after evil
  :init
  (defvar evil-collection-enabled-mode-list
    '(ag
      (package-menu package)
      help
      helpful
      eshell
      ivy
      custom
      dired
      wdired
      magit
      man
      woman
      diff-mode
      ediff
      calc
      which-key
      reftex
      notmuch
      ibuffer
      )
    "The list of `evil-collection' modules to load. evil-mode bindings will be enabled for these modes. See `evil-collection-mode-list' for the full set of supported modes.")
  :config
  (evil-collection-init evil-collection-enabled-mode-list)
  ;; (evil-collection-setup-minibuffer nil)
  ;; Additional bindings
  (evil-define-key* 'normal process-menu-mode-map
                    "q" #'kill-current-buffer
                    "d" #'process-menu-delete-process)
  
  (with-eval-after-load 'reftex
    (evil-collection-define-key 'normal 'reftex-toc-mode-map
      "<" 'reftex-toc-promote
      ">" 'reftex-toc-demote
      "gF" 'reftex-toc-toggle-file-boundary
      "c" 'reftex-toc-toggle-context
      "i" 'reftex-toc-toggle-index
      "J" 'reftex-toc-jump))

  (with-eval-after-load 'notmuch

    ;; (defun +disable-evil-C-Tab (mode-map)
    ;;   "Disable Control-Tab in evil-mode normal/visual/insert mode-map"
    ;;   (evil-define-key '(normal visual insert) mode-map (kbd "C-TAB") nil)
    ;;   (evil-define-key '(normal visual insert) mode-map (kbd "C-<tab>") nil))

    ;; ;; Disable Control-Tab in all notmuch modes to make it easier to
    ;; ;; switch buffers
    ;; (dolist (mode '((notmuch-hello-mode-hook . notmuch-hello-mode-map)
    ;;                 (notmuch-show-mode-hook . notmuch-show-mode-map)
    ;;                 (notmuch-message-mode-hook . notmuch-message-mode-map)
    ;;                 (notmuch-tree-mode-hook . notmuch-tree-mode-map)
    ;;                 (notmuch-search-mode-hook . notmuch-search-mode-map)))
    ;;   (let ((mode-hook (car mode))
    ;;         (mode-map (cdr mode)))
    ;;     (add-hook mode-hook  
    ;;               (lambda ()
    ;;                 (+disable-evil-C-Tab mode-map)))))

    ;; (dolist (mode-map (list notmuch-hello-mode-map
    ;;                         notmuch-show-mode-map
    ;;                         notmuch-message-mode-map
    ;;                         notmuch-tree-mode-map
    ;;                         notmuch-search-mode-map))
    ;;   (evil-define-key 'normal mode-map (kbd "C-TAB") nil)
    ;;   (evil-define-key 'normal mode-map (kbd "C-<tab>") nil))

    ;; (evil-define-key* 'normal notmuch-hello-mode-map
    ;;                   (kbd "C-TAB") nil
    ;;                   (kbd "C-<tab>") nil)
    
    (fset 'evil-collection-notmuch-search-toggle-delete
          (lambda ()
            "Toggle trash tag for message"
            (interactive)
            (evil-collection-notmuch-toggle-tag "inbox"
                                                "search"
                                                (lambda () (interactive)
                                                  (message "Trashed message")))
            (evil-collection-notmuch-toggle-tag "trash" "search" 'notmuch-search-next-thread)))
    
    (fset 'evil-collection-notmuch-show-toggle-delete
          (lambda ()
            "Toggle deleted tag for message."
            (interactive)
            (evil-collection-notmuch-toggle-tag "inbox"
                                                "show"
                                                (lambda () (interactive)
                                                  (message "Trashed message")))
            (evil-collection-notmuch-toggle-tag "trash" "show")))

    (fset 'evil-collection-notmuch-tree-toggle-delete
          (lambda ()
            "Toggle deleted tag for message."
            (interactive)
            (evil-collection-notmuch-toggle-tag "inbox"
                                                "tree"
                                                (lambda () (interactive)
                                                  (message "Trashed message tree")))
            (evil-collection-notmuch-toggle-tag "trash" "tree")))

    
    (evil-collection-define-key 'normal 'notmuch-common-keymap
      "g?" 'notmuch-help
      "q" 'notmuch-bury-or-kill-this-buffer
      "s" 'notmuch-search
      "S" 'notmuch-tree
      "C" 'notmuch-mua-new-mail           ; like mu4e
      "cc" 'notmuch-mua-new-mail          ; like mu4e
      "gr" 'notmuch-refresh-this-buffer
      "gA" 'notmuch-refresh-all-buffers
      "gR" 'notmuch-poll-and-refresh-this-buffer
      "J" 'notmuch-jump-search)

    (evil-collection-define-key 'normal 'notmuch-hello-mode-map
      "g?" 'notmuch-hello-versions
      (kbd "TAB") 'widget-forward
      (kbd "RET") 'evil-collection-notmuch-hello-ret
      (kbd "S-TAB") 'widget-backward
      (kbd "<C-tab>") 'widget-backward)

    (evil-collection-define-key 'normal 'notmuch-show-mode-map
      "gd" 'goto-address-at-point
      "p" 'notmuch-show-save-attachments  ; like mu4e
      "A" 'notmuch-show-archive-thread-then-next
      "S" 'notmuch-show-filter-thread
      "K" 'notmuch-tag-jump
      "C" 'notmuch-mua-new-mail           ; like mu4e
      "cc" 'notmuch-mua-new-mail          ; like mu4e
      "cR" 'notmuch-show-reply
      "cf" 'notmuch-show-forward-message
      "X" 'notmuch-show-archive-thread-then-exit
      "zv" 'notmuch-tree-from-show-current-query ; like mu4e-conversation
      "<" 'notmuch-show-toggle-thread-indentation
      "a" 'notmuch-show-archive-message-then-next-or-next-thread
      "d" 'evil-collection-notmuch-show-toggle-delete
      "=" 'evil-collection-notmuch-show-toggle-flagged
      "H" 'notmuch-show-toggle-visibility-headers
      "gj" 'notmuch-show-next-open-message
      "gk" 'notmuch-show-previous-open-message
      "]]" 'notmuch-show-next-message
      "[[" 'notmuch-show-previous-message
      (kbd "C-j") 'notmuch-show-next-message
      (kbd "C-k") 'notmuch-show-previous-message
      (kbd "M-j") 'notmuch-show-next-thread-show
      (kbd "M-k") 'notmuch-show-previous-thread-show
      "cr" 'notmuch-show-reply-sender
      (kbd "x") 'notmuch-show-archive-message-then-next-or-exit
      "|" 'notmuch-show-pipe-message
      "*" 'notmuch-show-tag-all
      "-" 'notmuch-show-remove-tag
      "+" 'notmuch-show-add-tag
      (kbd "TAB") 'notmuch-show-next-button
      (kbd "<backtab>") 'notmuch-show-previous-button
      (kbd "RET") 'notmuch-show-toggle-message
      "." 'notmuch-show-part-map)

    (evil-collection-define-key 'normal 'notmuch-tree-mode-map
      "g?" (notmuch-tree-close-message-pane-and 'notmuch-help)
      "q" 'notmuch-tree-quit
      "S" 'notmuch-tree-to-search
      "C" (notmuch-tree-close-message-pane-and 'notmuch-mua-new-mail) ; like mu4e
      "cc" (notmuch-tree-close-message-pane-and 'notmuch-mua-new-mail) ; like mu4e
      "J" (notmuch-tree-close-message-pane-and 'notmuch-jump-search)
      "zv" 'notmuch-search-from-tree-current-query ; like mu4e-conversation
      "cr" (notmuch-tree-close-message-pane-and 'notmuch-show-reply-sender) ; like mu4e
      "cR" (notmuch-tree-close-message-pane-and 'notmuch-show-reply)
      "d" 'evil-collection-notmuch-tree-toggle-delete
      "!" 'evil-collection-notmuch-tree-toggle-unread
      "=" 'evil-collection-notmuch-tree-toggle-flagged
      "K" 'notmuch-tag-jump
      (kbd "RET") 'notmuch-tree-show-message
      [mouse-1] 'notmuch-tree-show-message
      "A" 'notmuch-tree-archive-thread
      "a" 'notmuch-tree-archive-message-then-next
      "s" 'notmuch-tree-to-tree
      "gj" 'notmuch-tree-next-matching-message
      "gk" 'notmuch-tree-prev-matching-message
      "]]" 'notmuch-tree-next-message
      "[[" 'notmuch-tree-prev-message
      (kbd "C-k") 'notmuch-tree-prev-thread
      (kbd "C-j") 'notmuch-tree-next-thread
      "|" 'notmuch-show-pipe-message
      "-" 'notmuch-tree-remove-tag
      "+" 'notmuch-tree-add-tag
      "*" 'notmuch-tree-tag-thread
      "e" 'notmuch-tree-resume-message)

    (dolist (state '(normal visual))
      (evil-collection-define-key state 'notmuch-search-mode-map
        "cC" 'compose-mail-other-frame
        "J" 'notmuch-jump-search
        "S" 'notmuch-search-filter
        "K" 'notmuch-tag-jump
        "o" 'notmuch-search-toggle-order
        "zv" 'notmuch-tree-from-search-current-query
        "*" 'notmuch-search-tag-all
        "a" 'notmuch-search-archive-thread
        "cc" 'compose-mail                ; like mu4e
        "d" 'evil-collection-notmuch-search-toggle-delete
        "!" 'evil-collection-notmuch-search-toggle-unread
        "=" 'evil-collection-notmuch-search-toggle-flagged
        "q" 'notmuch-bury-or-kill-this-buffer
        "cr" 'notmuch-search-reply-to-thread-sender
        "cR" 'notmuch-search-reply-to-thread
        "t" 'notmuch-search-filter-by-tag
        [mouse-1] 'notmuch-search-show-thread
        "-" 'notmuch-search-remove-tag
        "+" 'notmuch-search-add-tag
        (kbd "RET") 'notmuch-search-show-thread))



    )
  
  
  
  )

(provide 'setup-evil)