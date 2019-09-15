(use-package evil-leader
  :ensure
  :commands global-evil-leader-mode
  :init
  (setq evil-want-keybinding nil)
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "B" (lambda () "Byte-compile file"
                                                       (if buffer-file-truename
                                                           (byte-compile-file buffer-true-filename)
                                                         (message "Not visiting a file!"))))
  (evil-leader/set-key-for-mode 'latex-mode "cc" 'TeX-command-master)
  (evil-leader/set-key-for-mode 'latex-mode "ca" 'TeX-command-run-all)
  (evil-leader/set-key-for-mode 'latex-mode "=" 'reftex-toc)
  (evil-leader/set-key-for-mode 'latex-mode "("  'reftex-label)
  (evil-leader/set-key-for-mode 'latex-mode ")" 'reftex-reference)
  (evil-leader/set-key-for-mode 'latex-mode "[" 'reftex-citation)
  (evil-leader/set-key-for-mode 'org-mode
    "o/" 'org-sparse-tree
    "ol" 'org-insert-link
    "ot" 'org-todo
    "oo" 'org-ctrl-c-ctrl-c
    "oc" 'org-ctrl-c-ctrl-c
    "oy" 'org-copy-visible
    "or" 'org-reveal
    
    )
  (evil-leader/set-key
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
    "fE" 'find-file-emacs-config
    "fD" 'find-file-Documents
    "fR" 'find-file-Research
    "fC" 'find-file-system-config
    
    ;; Searching
    "//" 'counsel-grep-or-swiper
    "/q" 'query-replace-regexp
    "/s" 'replace-regexp
    "/r" 'query-replace
    "/S" 'batch-replace-strings
    "/a" 'counsel-ag
    "/g" 'counsel-grep
    
    ;; Buffer commands
    "b" 'switch-to-buffer
    "w" 'save-buffer
    "q" 'evil-quit
    "`" 'evil-switch-to-windows-last-buffer
    ;; Navigation commands
    "j" 'ace-jump-mode

    "k" '(lambda () (interactive) (kill-buffer (current-buffer)))
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
  (defun find-file-system-config ()
    "Find file in system config"
    (interactive)
    (let ((configdir (getenv "CONFIGDIR")))
      (if configdir
          (counsel-file-jump "" (getenv "CONFIGDIR"))
        (message "ENV variable $CONFIGDIR not set"))))

  (defun find-file-emacs-config ()
    "Find file in emacs config"
    (interactive)
    (counsel-file-jump "" (concat
                           (file-name-as-directory (getenv "HOME"))
                           ".emacs.d")))

  (defun find-file-Documents ()
    "Find file in user documents"
    (interactive)
    (counsel-file-jump "" (concat
                           (file-name-as-directory (getenv "HOME"))
                           "Documents")))

  (defun find-file-Research ()
    "Find file in user research documents"
    (interactive)
    (counsel-file-jump "" (concat
                           (file-name-as-directory (getenv "HOME"))
                           (file-name-as-directory"Documents")
                           "research")))
  )

(use-package evil
  :ensure t
  :after evil-leader
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq-default evil-symbol-word-search t)
  (setq evil-emacs-state-cursor '(hbar . 4))
  (setq evil-vsplit-window-right t)
  (setq evil-spilt-window-below t)
  (add-hook 'evil-jumps-post-jump-hook #'recenter)
  :bind (:map evil-motion-state-map
              ("C-w C-h" . evil-window-left)
              ("C-w C-l" . evil-window-right)
              ("C-w C-k" . evil-window-up)
              ("C-w C-j" . evil-window-down)
              ("C-w C-f" . winner-redo)
              ("C-w C-b" . winner-undo)
              ("C-w C-w" . winner-undo)
              ("C-w |" . toggle-window-split)
              ("C-w S-<up>" . evil-window-increase-height)
              ("C-w S-<down>" . evil-window-decrease-height)
              ("C-w S-<left>" . evil-window-decrease-width)
              ("C-w S-<right>" . evil-window-increase-height)
              :map evil-normal-state-map
              ("[o" . open-previous-line)
              ("]o" . open-next-line))
  :config
  
  (evil-mode 1)
  
  ;;-------------------- 
  ;; EVIL KEYBINDS
  ;;-------------------- 
  
  ;; (define-key evil-motion-state-map ";" 'evil-repeat-find-char)
  ;; (define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
  (when evil-want-C-u-scroll
    (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
  (evil-define-key '(normal visual insert) helpful-mode-map "q" 'quit-window)
  (evil-define-key '(normal visual insert) special-mode-map "q" 'quit-window)
  
  (evil-define-key 'visual 'global (kbd "g-") 'narrow-to-region)
  (evil-define-key '(normal visual) 'global (kbd "g=") 'widen)
  
  (progn
    (evil-define-key '(visual normal) matlab-mode-map (kbd "zb") 'matlab-shell-run-block)
    (evil-define-key '(visual normal) matlab-mode-map (kbd "zr") 'matlab-shell-run-region-or-line)
    (evil-define-key '(normal visual) matlab-mode-map (kbd "[[") #'matlab-backward-section)
    (evil-define-key '(normal visual) matlab-mode-map (kbd "]]") #'matlab-forward-section))
  
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

;; EVIL-SURROUND
;; c/d/y s {motion}{delimiter} to change/delete/add delimiter around motion.
(use-package evil-surround
  :ensure
  :commands (turn-on-evil-surround-mode
             global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config
  (global-evil-surround-mode 1))

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
    )

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
          ">"))

  )


;; EVIL-COMMENTARY
;; gc{motion} to comment/uncomment
(use-package evil-commentary
  :ensure
  :diminish ""
  :commands evil-commentary-mode
  :init
  (evil-commentary-mode 1)
  )

;; EVIL-EXCHANGE
;; gx{motion} to select, gx{motion} on second object to exchange
(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

;; EVIL-LION
;; gl{motion}{char} to align on char
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

;; EVIL-MATCHIT
;; % to match delimiters, % as text-object to manipulate
(use-package evil-matchit
  :ensure t
  :init (global-evil-matchit-mode 1))

;; EVIL-NUMBERS
;; + and - to increment/decrement number at point
(use-package evil-numbers
  :ensure t
  :bind (:map evil-normal-state-map
              ("+" . evil-numbers/inc-at-pt)
              ("-" . evil-numbers/dec-at-pt)))
;; EVIL-RSI
;; C-a, C-e, C-f, C-b, C-d and C-k have same definitions as in emacs mode.
;; C-n and C-p work like in emacs if auto-complete is loaded.
(use-package evil-rsi
  :ensure t
  :config
  (evil-rsi-mode)
  :diminish "")

;; EVIL-SNIPE
;; s to snipe for next occurrence of chars
;; in operator mode, z or x to operate including/excluding next ocurrence of chars
(use-package evil-snipe
  :after evil-collection
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :ensure t
  :init
  (setq evil-snipe-spillover-scope 'whole-visible
        evil-snipe-smart-case t
        evil-snipe-char-fold t)
  :config
  ;; (evil-snipe-override-mode +1)
  (evil-snipe-mode 1)
  ;; (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
  ;; (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S)
  (evil-define-key 'normal snipe-local-mode-map "S" 'evil-snipe-S)
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
  ;; (evil-define-key 'normal evil-smartparens-mode-map (kbd "J") #'sp-join-sexp)
  )

;; EVIL-OWL
(use-package evil-owl
  :ensure t
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode)
  )

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
      reftex)
    "The list of `evil-collection' modules to load. evil-mode bindings will be enabled for these modes. See `evil-collection-mode-list' for the full set of supported modes.")
  :config
  (evil-collection-init evil-collection-enabled-mode-list)
  ;; (evil-collection-setup-minibuffer nil)
  ;; Additional bindings
  (evil-define-key* 'normal process-menu-mode-map
                    "q" #'kill-current-buffer
                    "d" #'process-menu-delete-process)
  )

(provide 'setup-evil)
