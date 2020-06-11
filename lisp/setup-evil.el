;; -*- lexical-binding: t -*-
;;(require 'use-package nil t)
(use-package evil-leader
  :disabled
  :commands global-evil-leader-mode
  :init
  (setq evil-want-keybinding nil)
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "cB"
    (defun byte-compile-this-file () "Byte-compile file"
      (interactive)
      (if buffer-file-name
          (byte-compile-file buffer-file-name)
        (message "Not visiting a file!"))))
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "cL"
    (defun load-this-file () "Load current file"
      (interactive)
      (if buffer-file-name
          (load-file buffer-file-name)
        (message "Not visiting a file!"))))

  (evil-leader/set-key
    "f'" 'bookmark-jump ;counsel-bookmark
    "fm" 'bookmark-jump ;counsel-bookmark
    "ff" 'find-file ;counsel-find-file
    "f." 'find-file ;counsel-find-file
    )

  (evil-leader/set-key-for-mode 'latex-mode
    "cc" 'TeX-command-master
    "ca" 'TeX-command-run-all
    "=" 'reftex-toc
    "("  'reftex-label
    ")" 'reftex-reference
    "[" 'reftex-citation
    "{" 'cdlatex-environment)

  (evil-leader/set-key-for-mode 'reftex-toc-mode
    "SPC" 'reftex-toc-view-line)

  (with-eval-after-load 'ace-window 
    (evil-leader/set-key
      ;; Ace window mode
      "<tab>" 'ace-window
      ))

  (evil-leader/set-key-for-mode 'org-mode
    "ce" 'org-export-dispatch
    "/t" 'org-tags-sparse-tree
    "/T" 'org-show-todo-tree
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

  (evil-leader/set-key
    ;; Help
    ;; "hf" (if counselp 'counsel-describe-function 'describe-function)
    ;; "hb" (if counselp 'counsel-descbinds 'describe-bindings)
    ;; "hv" (if counselp 'counsel-describe-variable 'describe-variable)
    "h." 'helpful-at-point
    "hf" 'describe-function
    "hb" 'describe-binddings
    "hv" 'describe-variable
    "ha" 'apropos
    "hk" 'describe-key
    "hm" 'describe-mode
    ;; "ha" (if counselp 'counsel-apropos 'apropos-command)
    "hd" 'apropos-documentation
    "hc" 'describe-key-briefly

    )

  (evil-leader/set-key
    
    ;; Calc
    "*" 'calc-dispatch
    ;; "e" 'find-file
    "u"  'universal-argument

    ;; File commands
    "fS" 'sudo-find-file
    "fd" 'dired

    ;; Searching
    "/q" 'query-replace-regexp
    "/s" 'replace-regexp
    "/r" 'query-replace
    "/S" 'batch-replace-strings

    ;; Buffer commands
    "b" 'switch-to-buffer
    "w" 'save-buffer
    "q" 'evil-quit
    "`" 'evil-switch-to-windows-last-buffer
    ;; Navigation commands
    "j" 'avy-goto-char-timer

    "k" 'kill-this-buffer
    "K" 'kill-buffer-and-window
    ;; "n" (lambda (&optional arg)
    ;;       "Next Buffer"
    ;;               (interactive "P")
    ;;               (if arg (previous-user-buffer) (next-user-buffer)))
    ;; "p" (lambda (&optional arg)
    ;;       "Previous Buffer"
    ;;               (interactive "P")
    ;;               (if arg (next-user-buffer) (previous-user-buffer)))
    "N" 'next-buffer
    "P" 'previous-buffer
    "B" 'ibuffer
        ;; (lambda ()
        ;;   "Open Bibtex file"
        ;;   (interactive)
        ;;   (find-file-other-window (getenv "BIB")))
    "," 'er/expand-region
    "cn" 'next-error
    "cp" 'previous-error
    "vf" 'ido-find-file-other-window
    "vb" 'ido-switch-buffer-other-window

    (with-eval-after-load 'ivy
      (evil-leader/set-key
        ;; Ivy general
        "." 'ivy-resume)
      (evil-leader/set-key-for-mode 'latex-mode
        "]" 'ivy-bibtex))
    
    (with-eval-after-load 'counsel
      (evil-leader/set-key
        ;; Searching
        "//" 'counsel-grep-or-swiper
        "/a" 'counsel-ag
        "/g" 'counsel-grep
        "/G" 'counsel-git-grep

        ;; File commands
        ;; "f~" (lambda () "Find file from ~/" (find-fil))
        ;; "fE" 'find-file-emacs-config
        "fD" 'find-file-Documents
        "fR" 'find-file-Research
        "fc" 'find-file-config-dirs
        "fC" 'find-file-config-dirs
        "fr" 'counsel-recentf
        "fj" 'counsel-file-jump
        "fg" 'counsel-git
        "fl" 'locate ;counsel-locate
        ;; "f'" 'bookmark-jump ;counsel-bookmark
        ;; "fm" 'bookmark-jump ;counsel-bookmark
        ;; "ff" 'find-file ;counsel-find-file
        ;; "f." 'find-file ;counsel-find-file
        "fz" 'counsel-fzf

        ;; Help commands
        "hf" 'counsel-describe-function
        "hb" 'counsel-descbinds
        "hv" 'counsel-describe-variable
        "ha" 'counsel-apropos
        )))

  ;; (with-eval-after-load 'projectile
  ;;   (defun +evil-leader-projectile-map ()
  ;;     "Add evil-leader keybinds for projectile mode"
  ;;     (interactive)
  ;;     (evil-leader/set-key
  ;;       "SPC" 'projectile-command-map
  ;;       "n" 'projectile-next-project-buffer
  ;;       "p" 'projectile-previous-project-buffer))
  ;;   (add-hook 'counsel-projectile-mode-hook #'+evil-leader-projectile-map))

  )

(use-package evil
  :ensure t
  ;; :after evil-leader
  :defines (turn-on-evil-matlab-textobjects-mode turn-on-evil-latex-textobjects-mode)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump (display-graphic-p))
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq-default evil-symbol-word-search t)
  (setq evil-emacs-state-cursor '(hbar . 4))
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-mode-line-format '(before . mode-line-front-space))
  ;; (add-hook 'evil-jumps-post-jump-hook #'recenter)
  :general
  (:states '(motion)
   "g C-f" 'scroll-other-window
   "g C-b" 'scroll-other-window-down)
  ;; TODO: map comma comma to previous match when comma is localleader.
  ;; (localleader-define-key
  ;;   "" nil
  ;;   "," '(evil-repeat-find-char-reverse :wk "last char match"))
  (:keymaps 'space-menu-map
   :wk-full-keys nil
   "TAB" '(evil-switch-to-windows-last-buffer :wk "prev buffer"))
  :bind (:map evil-motion-state-map
              ("[q" . previous-error)
              ("]q" . next-error)
              ("]h" . outline-next-visible-heading)
              ("[h" . 'outline-previous-visible-heading)
         :map evil-window-map
              ("C-h"           . evil-window-left)
              ("C-l"           . evil-window-right)
              ("C-k"           . evil-window-up)
              ("C-j"           . evil-window-down)
              ;; ("C-h"        . evil-window-left)
              ;; ("C-l"        . evil-window-right)
              ;; ("C-k"        . evil-window-up)
              ;; ("C-j"        . evil-window-down)
              ;; ("C-w C-f"    . winner-redo)
              ;; ("C-w C-b"    . winner-undo)
              ("C-w"           . other-window)
              ("|"             . toggle-window-split)
              ("f"             . find-file-other-window)
              ("b"             . switch-to-buffer-other-window)
              ("S-<up>"    . evil-window-increase-height)
              ("S-<down>"  . evil-window-decrease-height)
              ("S-<left>"  . evil-window-decrease-width)
              ("S-<right>" . evil-window-increase-height)
              ;; :map evil-normal-state-map
              ;; ("[o"         . open-next-line)
              ;; ("]o"         . open-previous-line)
           :map evil-insert-state-map
           ("C-w"              . backward-kill-word-or-region))
  :config
  (dolist (mode '(occur-mode
                  ivy-occur-mode
                  ivy-occur-grep-mode
                  diff-mode
                  ;; reftex-toc-mode
                  ) nil)
    (cl-pushnew mode evil-motion-state-modes))

  ;; (defun my-move-key (keymap-from keymap-to key)
  ;;   "Moves key binding from one keymap to another, deleting from the old location. "
  ;;   (define-key keymap-to key (lookup-key keymap-from key))
  ;;   (define-key keymap-from key nil))
  ;; (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  ;; (my-move-key evil-motion-state-map evil-normal-state-map " ")

;  (nconc evil-motion-state-modes '(occur-mode ivy-occur-mode ivy-occur-grep-mode))

  (eval-after-load 'pulse
  (add-hook 'evil-jumps-post-jump-hook #'my/pulse-momentary-line))

  (evil-define-key 'normal 'global (kbd "[ SPC") (lambda (&optional arg) (interactive)
                                                   (save-excursion
                                                     (previous-line)
                                                     (end-of-line)
                                                     (open-line arg))))
  (evil-define-key 'normal 'global (kbd "] SPC") (lambda (&optional arg) (interactive)
                                                   (save-excursion
                                                     (end-of-line)
                                                     (open-line arg))))

  ;; (dolist (state-tag '((" N " . 'evil-normal-state-tag)
  ;;                      (" E " . 'evil-emacs-state-tag)
  ;;                      (" I " . 'evil-insert-state-tag)
  ;;                      (" R " . 'evil-replace-state-tag)
  ;;                      (" M " . 'evil-motion-state-tag)
  ;;                      (" V " . 'evil-visual-state-tag)
  ;;                      (" O " . 'evil-operator-state-tag))) 
  ;;   (if (bound-and-true-p winum-mode)
  ;;       (set (cdr state-tag) nil)
  ;;     (setf (cdr state-tag) (propertize (car state-tag) (+evil-mode-line-faces)))))
  
;; Loading colors in the winum indicator.
  (setq ;; evil-normal-state-tag   (propertize " N " 'face '((bold :background "DarkGoldenrod2" :foreground "black")))
   evil-normal-state-tag   "" ;(propertize " N " 'face '((bold :inherit mode-line )))
   evil-emacs-state-tag    "" ;(propertize " E " 'face '((bold :background "SkyBlue2"       :foreground "black")))
   evil-insert-state-tag   "" ;(propertize " I " 'face '((bold :background "chartreuse3"    :foreground "black")))
   evil-replace-state-tag  "" ;(propertize " R " 'face '((bold :background "chocolate"      :foreground "black")))
   evil-motion-state-tag   "" ;(propertize " M " 'face '((bold :background "plum3"          :foreground "black")))
   evil-visual-state-tag   "" ;(propertize " V " 'face '((bold :background "gray"           :foreground "black")))
   evil-operator-state-tag "" ;(propertize " O " 'face '((bold :background "sandy brown"    :foreground "black")))
   )

  (setq evil-search-module 'evil-search)
  (add-to-list 'evil-emacs-state-modes 'undo-tree-visualizer-mode)
  (add-to-list 'evil-emacs-state-modes 'eww-mode)

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

  (evil-define-key '(normal) 'global (kbd "zd") 'describe-word-at-point)

  (evil-define-key 'visual 'global (kbd "g-") 'narrow-to-region)
  (evil-define-key '(normal visual) 'global (kbd "g=") 'widen)

  (with-eval-after-load 'dired-sidebar
    (progn
      (evil-define-key* '(normal visual) 'global (kbd "C-w d") #'dired-sidebar-toggle-sidebar)
      (evil-define-key 'normal 'dired-sidebar-mode-map (kbd "TAB") 'dired-sidebar-subtree-toggle)
      (evil-define-key 'normal 'dired-sidebar-mode-map "-" 'dired-sidebar-up-directory)
      (evil-define-key 'normal 'dired-sidebar-mode-map "^" 'dired-sidebar-up-directory)
      (evil-define-key 'normal 'dired-sidebar-mode-map (kbd "<mouse-2>") 'dired-sidebar-mouse-subtree-cycle-or-find-file)))

  (with-eval-after-load 'ibuffer-sidebar
    (evil-define-key* '(normal visual) 'global (kbd "C-w C-d") #'+ibuffer-sidebar-toggle))

  ;; (evil-define-key 'normal 'emacs-lisp-mode-map (kbd "(")
  ;;   (defun evil-backward-sexp (&optional arg) (interactive)
  ;;          (if (not (evil-in-comment-p))
  ;;              (backward-sexp arg))))
  ;; (evil-define-key 'normal 'emacs-lisp-mode-map (kbd ")")
  ;;   (defun evil-forward-sexp (&optional arg) (interactive)
  ;;          (if (not (evil-in-comment-p))
  ;;              (forward-sexp arg))))

  (with-eval-after-load 'ivy
    (progn
      (evil-define-key* 'normal 'global (kbd "gI") #'ivy-resume)))

  (with-eval-after-load 'org
    (progn
      (evil-define-key 'normal org-mode-map (kbd "g-") 'org-narrow-to-subtree)
      (evil-define-key 'normal org-mode-map (kbd "gj") 'org-next-visible-heading)
      (evil-define-key 'normal org-mode-map (kbd "gk") 'org-previous-visible-heading)
      ))

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
      (evil-define-key '(visual normal) matlab-mode-map (kbd "zb") #'matlab-shell-run-block)
      (evil-define-key '(visual normal) matlab-mode-map (kbd "zr") #'matlab-shell-run-region-or-line)
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
  
  (add-hook 'org-mode-hook (lambda () (unless (featurep 'evil-latex-textobjects)
                                     (require 'evil-latex-textobjects nil t))
                               (turn-on-evil-latex-textobjects-mode)))

  (add-hook 'matlab-mode-hook (lambda () (unless (featurep 'evil-matlab-textobjects)
                                      (require 'evil-matlab-textobjects nil t))
                                (turn-on-evil-matlab-textobjects-mode)))

  

;;--------------------------------------------------
;; Evil text objects
;;--------------------------------------------------

(evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (evil-range (point-min) (point-max) type))

(evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
  "Text object to select the top-level Lisp form or function definition at
point."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range beg end type)))

(evil-define-text-object +evil:inner-url-txtobj (count &optional _beg _end type)
  "Text object to select the inner url at point.

This excludes the protocol and querystring."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     (save-excursion
       (goto-char beg)
       (re-search-forward "://" end t))
     (save-excursion
       (goto-char end)
       (- (if-let (pos (re-search-backward "[?#]" beg t))
              pos
            end)
          (if (evil-visual-state-p)
              1
            0)))
     type)))

(evil-define-text-object +evil:outer-url-txtobj (count &optional _beg _end type)
  "Text object to select the whole url at point."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     beg (- end (if (evil-visual-state-p) 1 0))
     type)))

(general-def
  :keymaps 'evil-inner-text-objects-map 
  "f" #'+evil:defun-txtobj
  "d" #'+evil:defun-txtobj
  "g" #'+evil:whole-buffer-txtobj
  "u" #'+evil:inner-url-txtobj
  :keymaps 'evil-outer-text-objects-map 
  "f" #'+evil:defun-txtobj
  "d" #'+evil:defun-txtobj
  "g" #'+evil:whole-buffer-txtobj
  "u" #'+evil:outer-url-txtobj)

  (evil-mode 1)
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
  "List of modes where `evil-mode' addons (like commentary and snipe) should be enabled")

(use-package evil-traces
  :ensure
  :after evil-ex
  :defer 5
  :config
  ;; (pushnew! evil-traces-argument-type-alist
  ;;           '(+evil:align . evil-traces-global)
  ;;           '(+evil:align-right . evil-traces-global))
  (evil-traces-mode))

;; EVIL-GOGGLES
(use-package evil-goggles
  :disabled
  :init (evil-goggles-mode)
  :config (setq evil-goggles-duration 0.1
                evil-goggles-lighter ""))

;; EVIL-SURROUND
;; c/d/y s {motion}{delimiter} to change/delete/add delimiter around motion.
(use-package evil-surround
  :defer
  :ensure t
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
  :defer
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
    (embrace-add-pair-regexp ?m "\\\\[a-z*]+{" "}" #'+evil--embrace-latex
                             (embrace-build-help "\\macro{" "}"))
    (embrace-add-pair-regexp ?e "\\\\begin{[a-z*]+}" "\\\\end{[a-z*]+}"
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
         :left-regexp "\\[[{(]|"
         :right-regexp "\\[]})]|"))

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


;; EVIL-NERD-COMMENTER
(use-package evil-nerd-commenter
  :ensure
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general
  (:states '(normal visual)
   "gc" 'evilnc-comment-operator))

;; EVIL-EXCHANGE
;; gx{motion} to select, gx{motion} on second object to exchange
(use-package evil-exchange
  :defer
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
  :after evil
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
  :disabled t
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
  :defer
  :ensure t
  ;; :bind (:map evil-normal-state-map
  ;;             ("+" . evil-numbers/inc-at-pt)
  ;;             ("-" . evil-numbers/dec-at-pt))
  
  :config
  (defun +evil-numbers-add-keybinds (mode-map)
    "Make + and - increment and decrement numbers in evil-normal-state"
    (evil-define-key* 'normal (symbol-value mode-map)
                      (kbd "+") #'evil-numbers/inc-at-pt
                      (kbd "-") #'evil-numbers/dec-at-pt))

  (dolist (mode '((prog-mode-hook         . prog-mode-map)
                  (text-mode-hook         . text-mode-map)
                  (message-mode-hook      . message-mode-map)
                  (conf-unix-mode-hook    . conf-mode-map)
                  (conf-windows-mode-hook . conf-mode-map)
                  (TeX-mode-hook          . TeX-mode-map)
                  ))
      (let ((mode-hook (car mode))
            (mode-map (cdr mode)))
        (add-hook mode-hook
                  (lambda () (+evil-numbers-add-keybinds mode-map)))))
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
  :defer
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

;;EVIL-SPACE
;;Hit ; or , (originally <SPC>) to repeat last movement.
(use-package evil-space
  ;; :ensure t
  :disabled
  :init
  (evil-space-mode)
  (setq evil-space-next-key ";")
  (setq evil-space-prev-key ","))

;; EVIL-VISUALSTAR
;; Select with visual-mode and hit * or # to find next occurrence
(use-package evil-visualstar
  :defer
  :ensure t
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent t)
  :diminish "")

;; EVIL-PAREDIT
(use-package evil-paredit
  ;; :ensure t
  :disabled
  :config
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

;; ORG-EVIL
(use-package org-evil
  :defer 1
  :ensure t
  :after org)

;;-----------------
;; EVIL-SMARTPARENS
;;-----------------
(use-package evil-smartparens
  :ensure
  :after smartparens
  :hook ((emacs-lisp-mode lisp-interaction-mode) . evil-smartparens-mode)
  :config
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
  :defer 3
  :after evil
  :init
  (defvar evil-collection-enabled-mode-list
    '(ag
      (package-menu package)
      help
      helpful
      eshell
      ivy
      epa
      custom
      dired
      wdired
      magit
      man
      woman
      diff-mode
      ediff
      which-key
      reftex
      notmuch
      ibuffer
      (occur replace)
      xref
      doc-view
      view
      )
    "The list of `evil-collection' modules to load. evil-mode bindings will be enabled for these modes. See `evil-collection-mode-list' for the full set of supported modes.")
  :config
  (setq evil-collection-key-blacklist '("C-j" "C-k"))
  (evil-collection-init evil-collection-enabled-mode-list)
  ;; (evil-collection-setup-minibuffer nil)
  ;; Additional bindings
  (evil-define-key* 'normal process-menu-mode-map
                    "q" #'kill-current-buffer
                    "d" #'process-menu-delete-process)
  
  (with-eval-after-load 'dired
    (evil-collection-define-key '(normal visual) 'dired-mode-map (kbd "SPC") nil))

  (with-eval-after-load 'reftex
    (evil-collection-define-key 'normal 'reftex-toc-mode-map
      "<" 'reftex-toc-promote
      ">" 'reftex-toc-demote
      "gF" 'reftex-toc-toggle-file-boundary
      "c" 'reftex-toc-toggle-context
      "i" 'reftex-toc-toggle-index
      "J" 'reftex-toc-jump))

  (with-eval-after-load 'notmuch

    (defun +disable-evil-C-Tab (mode-map)
      "Disable Control-Tab in evil-mode normal/visual/insert mode-map"
      (evil-define-key* '(normal visual insert) (symbol-value mode-map) (kbd "C-TAB") nil)
      (evil-define-key* '(normal visual insert) (symbol-value mode-map) (kbd "C-<tab>") nil)
      (define-key mode-map (kbd "C-<tab>") nil))

    ;; Disable Control-Tab in all notmuch modes to make it easier to
    ;; switch buffers
    (dolist (mode '((notmuch-hello-mode-hook . notmuch-hello-mode-map)
                    (notmuch-show-mode-hook . notmuch-show-mode-map)
                    (notmuch-tree-mode-hook . notmuch-tree-mode-map)
                    (notmuch-search-mode-hook . notmuch-search-mode-map)
                    ;; (custom-mode-hook . custom-mode-map)
                    ))
      (let ((mode-hook (car mode))
            (mode-map (cdr mode)))
        (add-hook mode-hook  
                  (lambda ()
                    (+disable-evil-C-Tab mode-map)))))

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
    
    (evil-collection-define-key 'normal 'notmuch-tree-mode-map
      "j"  'notmuch-tree-next-message
      "k"  'notmuch-tree-prev-message
      "gj" 'notmuch-tree-next-matching-message
      "gk" 'notmuch-tree-prev-matching-message
      "]]" 'notmuch-tree-next-thread
      "[[" 'notmuch-tree-prev-thread)

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

    
    ;; (evil-collection-define-key 'normal 'notmuch-common-keymap
    ;;   "g?" 'notmuch-help
    ;;   "q" 'notmuch-bury-or-kill-this-buffer
    ;;   "s" 'notmuch-search
    ;;   "S" 'notmuch-tree
    ;;   "C" 'notmuch-mua-new-mail           ; like mu4e
    ;;   "cc" 'notmuch-mua-new-mail          ; like mu4e
    ;;   "gr" 'notmuch-refresh-this-buffer
    ;;   "gA" 'notmuch-refresh-all-buffers
    ;;   "gR" 'notmuch-poll-and-refresh-this-buffer
    ;;   "J" 'notmuch-jump-search)

    ;; (evil-collection-define-key 'normal 'notmuch-hello-mode-map
    ;;   "g?" 'notmuch-hello-versions
    ;;   (kbd "TAB") 'widget-forward
    ;;   (kbd "RET") 'evil-collection-notmuch-hello-ret
    ;;   (kbd "S-TAB") 'widget-backward
    ;;   (kbd "<C-tab>") 'widget-backward)

    ;; (evil-collection-define-key 'normal 'notmuch-show-mode-map
    ;;   "gd" 'goto-address-at-point
    ;;   "p" 'notmuch-show-save-attachments  ; like mu4e
    ;;   "A" 'notmuch-show-archive-thread-then-next
    ;;   "S" 'notmuch-show-filter-thread
    ;;   "K" 'notmuch-tag-jump
    ;;   "C" 'notmuch-mua-new-mail           ; like mu4e
    ;;   "cc" 'notmuch-mua-new-mail          ; like mu4e
    ;;   "cR" 'notmuch-show-reply
    ;;   "cf" 'notmuch-show-forward-message
    ;;   "X" 'notmuch-show-archive-thread-then-exit
    ;;   "zv" 'notmuch-tree-from-show-current-query ; like mu4e-conversation
    ;;   "<" 'notmuch-show-toggle-thread-indentation
    ;;   "a" 'notmuch-show-archive-message-then-next-or-next-thread
    ;;   "d" 'evil-collection-notmuch-show-toggle-delete
    ;;   "=" 'evil-collection-notmuch-show-toggle-flagged
    ;;   "H" 'notmuch-show-toggle-visibility-headers
    ;;   "gj" 'notmuch-show-next-open-message
    ;;   "gk" 'notmuch-show-previous-open-message
    ;;   "]]" 'notmuch-show-next-message
    ;;   "[[" 'notmuch-show-previous-message
    ;;   (kbd "C-j") 'notmuch-show-next-message
    ;;   (kbd "C-k") 'notmuch-show-previous-message
    ;;   (kbd "M-j") 'notmuch-show-next-thread-show
    ;;   (kbd "M-k") 'notmuch-show-previous-thread-show
    ;;   "cr" 'notmuch-show-reply-sender
    ;;   (kbd "x") 'notmuch-show-archive-message-then-next-or-exit
    ;;   "|" 'notmuch-show-pipe-message
    ;;   "*" 'notmuch-show-tag-all
    ;;   "-" 'notmuch-show-remove-tag
    ;;   "+" 'notmuch-show-add-tag
    ;;   (kbd "TAB") 'notmuch-show-next-button
    ;;   (kbd "<backtab>") 'notmuch-show-previous-button
    ;;   (kbd "RET") 'notmuch-show-toggle-message
    ;;   "." 'notmuch-show-part-map)

    ;; (evil-collection-define-key 'normal 'notmuch-tree-mode-map
    ;;   "g?" (notmuch-tree-close-message-pane-and 'notmuch-help)
    ;;   "q" 'notmuch-tree-quit
    ;;   "S" 'notmuch-tree-to-search
    ;;   "C" (notmuch-tree-close-message-pane-and 'notmuch-mua-new-mail) ; like mu4e
    ;;   "cc" (notmuch-tree-close-message-pane-and 'notmuch-mua-new-mail) ; like mu4e
    ;;   "J" (notmuch-tree-close-message-pane-and 'notmuch-jump-search)
    ;;   "zv" 'notmuch-search-from-tree-current-query ; like mu4e-conversation
    ;;   "cr" (notmuch-tree-close-message-pane-and 'notmuch-show-reply-sender) ; like mu4e
    ;;   "cR" (notmuch-tree-close-message-pane-and 'notmuch-show-reply)
    ;;   "d" 'evil-collection-notmuch-tree-toggle-delete
    ;;   "!" 'evil-collection-notmuch-tree-toggle-unread
    ;;   "=" 'evil-collection-notmuch-tree-toggle-flagged
    ;;   "K" 'notmuch-tag-jump
    ;;   (kbd "RET") 'notmuch-tree-show-message
    ;;   [mouse-1] 'notmuch-tree-show-message
    ;;   "A" 'notmuch-tree-archive-thread
    ;;   "a" 'notmuch-tree-archive-message-then-next
    ;;   "s" 'notmuch-tree-to-tree
    ;;   "gj" 'notmuch-tree-next-matching-message
    ;;   "gk" 'notmuch-tree-prev-matching-message
    ;;   "]]" 'notmuch-tree-next-message
    ;;   "[[" 'notmuch-tree-prev-message
    ;;   (kbd "C-k") 'notmuch-tree-prev-thread
    ;;   (kbd "C-j") 'notmuch-tree-next-thread
    ;;   "|" 'notmuch-show-pipe-message
    ;;   "-" 'notmuch-tree-remove-tag
    ;;   "+" 'notmuch-tree-add-tag
    ;;   "*" 'notmuch-tree-tag-thread
    ;;   "e" 'notmuch-tree-resume-message)

    ;; (dolist (state '(normal visual))
    ;;   (evil-collection-define-key state 'notmuch-search-mode-map
    ;;     "cC" 'compose-mail-other-frame
    ;;     "J" 'notmuch-jump-search
    ;;     "S" 'notmuch-search-filter
    ;;     "K" 'notmuch-tag-jump
    ;;     "o" 'notmuch-search-toggle-order
    ;;     "zv" 'notmuch-tree-from-search-current-query
    ;;     "*" 'notmuch-search-tag-all
    ;;     "a" 'notmuch-search-archive-thread
    ;;     "cc" 'compose-mail                ; like mu4e
    ;;     "d" 'evil-collection-notmuch-search-toggle-delete
    ;;     "!" 'evil-collection-notmuch-search-toggle-unread
    ;;     "=" 'evil-collection-notmuch-search-toggle-flagged
    ;;     "q" 'notmuch-bury-or-kill-this-buffer
    ;;     "cr" 'notmuch-search-reply-to-thread-sender
    ;;     "cR" 'notmuch-search-reply-to-thread
    ;;     "t" 'notmuch-search-filter-by-tag
    ;;     [mouse-1] 'notmuch-search-show-thread
    ;;     "-" 'notmuch-search-remove-tag
    ;;     "+" 'notmuch-search-add-tag
    ;;     (kbd "RET") 'notmuch-search-show-thread))

    )
  
  
  
  )

(provide 'setup-evil)
