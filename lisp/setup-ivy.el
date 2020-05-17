;; -*- lexical-binding: t; -*-
;;(require 'use-package nil t)
(use-package ivy
  :ensure t
  :general
  (:keymaps 'space-menu-map
            :wk-full-keys t
            "." '(ivy-resume :wk "Resume last search")
            )
  :bind (("C-S-s" . swiper)
         ("M-s O" . swiper)
         :map ivy-minibuffer-map
         ("C-m"   . ivy-alt-done)
         ("C-,"   . ivy-rotate-preferred-builders))
  :init
  (ivy-mode 1)

  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 14
        ivy-read-action-format-function 'ivy-read-action-format-columns
        ivy-re-builders-alist '((read-file-name-internal . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (swiper-isearch . ivy--regex-plus)
                                (counsel-ag . ivy--regex-plus)
                                ;; (counsel-M-x . ivy--regex-fuzzy)
                                (ivy-completion-in-region . ivy--regex-fuzzy)
                                (t . ivy--regex-ignore-order)))
  (setq enable-recursive-minibuffers t)
  ;; (setq ivy-read-action-function 'ivy-read-action-by-key)  

  ;; Ensure a jump point is registered before jumping to new locations with ivy
  (defvar +ivy--origin nil)
  (defun +ivy--record-position-maybe-fn ()
    (with-ivy-window
      (setq +ivy--origin (point-marker))))
  (setq ivy-hooks-alist '((t . +ivy--record-position-maybe-fn)))

  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  ;; (global-set-key (kbd "C-S-s") 'swiper)
  ;; (global-set-key (kbd "M-s O") 'swiper)
  ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  ;; (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)

;;;###autoload
  (defun ivy-save-views ()
    "Save current buffer view to file"
    (interactive)
    (with-temp-file "~/.local/share/ivy/ivy-views"
      (prin1 ivy-views (current-buffer))
      (message "save ivy-views to ~/.local/share/ivy/ivy-views")))

;;;###autoload
  (defun ivy-load-views ()
    (interactive)
    "Load current buffer view from file"
    (setq ivy-views
          (with-temp-buffer
            (insert-file-contents "~/.local/share/ivy/ivy-views")
            (read (current-buffer))))
    (message "load ivy-views"))

;;;###autoload
  (defun +ivy-switch-file-search ()
    "Switch to counsel-fzf, preserving current input."
    (interactive)
    (let ((input (ivy--input)))
      (ivy-quit-and-run (counsel-file-jump input ivy--directory))))

  ;; (define-key ivy-minibuffer-map (kbd "M-s") '+ivy-switch-file-search)

  (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-mark)

  ;; Define a function to replace `counsel--find-return-list' that uses `fd' or `ripgrep' (if available) in place of `find' to find files, and...
;;;###autoload
  (defun +ivy--counsel-file-jump-use-fd-rg-a (&rest args)
    "Change `ivy-counsel-file-jump' to use fd or ripgrep if available"
    (cl-destructuring-bind (find-program . args)
        (cond ((executable-find "fd")
               (cons "fd" (list "--hidden" "-L" "-t" "f"
                                "-E" ".git*"
                                "-E" "#*"
                                "-E" "*~")))
              ((executable-find "rg")
               (cons "rg" (list "--files" "--hidden" "--no-messages")))
              ((cons find-program args)))
      (unless (listp args)
        (user-error "`counsel-file-jump-args' is a list now, please customize accordingly."))
      (counsel--call
       (cons find-program args)
       (lambda ()
         (goto-char (point-min))
         (let ((offset (if (member find-program '("fd" "rg")) 0 2))
               files)
           (while (< (point) (point-max))
             (push (buffer-substring
                    (+ offset (line-beginning-position)) (line-end-position)) files)
             (forward-line 1))
           (nreverse files))))))

  ;; ...then replace counsel--find-return-list
  (advice-add #'counsel--find-return-list :override '+ivy--counsel-file-jump-use-fd-rg-a)

  (with-eval-after-load 'yasnippet
    (defun +ivy-yas-prompt (prompt choices &optional display-fn)
      (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))
    (add-to-list 'yas-prompt-functions #'+ivy-yas-prompt nil #'eq))

;;; ivy-hydra allows additional actions and vim-like navigation on ivy candidates.
;;; Initialize in ivy with C-o
  )

(use-package  ivy-hydra
  :after ivy
  :ensure t
  :commands (ivy-dispatching-done-hydra
             ivy--matcher-desc
             ;ivy-hydra/body
             ivy-hydra-read-action)
  :config
  (setq ivy-read-action-function 'ivy-hydra-read-action))

(use-package counsel
  ;; :disabled
  :ensure t
  :commands counsel-describe-face
  :general
  ("M-x" 'counsel-M-x
   "M-s a" 'counsel-ag
   "M-s G" 'counsel-git-grep
   "C-c C-j" 'counsel-outline)
  (:keymaps 'LaTeX-mode-map
   "C-c C-j" nil)
  (:keymaps 'space-menu-map
   :wk-full-keys t        
   "," '(counsel-switch-buffer :wk "Switch to buffer")
   :prefix "f"
   "x" '(counsel-find-file-extern :wk "xdg-open")
   "f" '(counsel-find-file     :wk "Find file")
   "D" '(find-file-Documents   :wk "Find Document")
   "C" '(find-file-config-dirs :wk "Find user config file")
   "c" '(find-file-config-dirs :wk t)
   "R" '(find-file-Research    :wk "Find Research file")
   "z" '(counsel-fzf           :wk "FZF in this dir")
   "j" '(counsel-file-jump     :wk "Jump to file")
   "g" '(counsel-git           :wk "Find file in git repo")
   "l" '(counsel-locate        :wk "Locate file on system")
   "r" '(counsel-recentf       :wk "Find recent file")
   "m" '(counsel-bookmark      :wk "Jump or set bookmark")
   "'" '(counsel-bookmark      :wk "Jump or set bookmark"))
  (:keymaps 'space-menu-search-map
   :wk-full-keys t
   "i" '(counsel-imenu :wk "imenu")
   "a" '(counsel-ag :wk "Ag in dir")
   "s" '(counsel-grep-or-swiper :wk "Swiper")
   "G" '(counsel-git-grep :wk "Grep through Git"))
  (:keymaps 'counsel-find-file-map
   "M-s" '+ivy-switch-file-search
   "C-s" '+ivy-switch-file-search)
  (:keymaps 'help-map ;'space-menu-help-map
   "f" 'counsel-describe-function
   "b" 'counsel-descbinds
   "v" 'counsel-describe-variable
   "a" 'counsel-apropos)
  (:keymaps 'counsel-find-file-map
            "C-s" '+ivy-switch-file-search)
  ;; :bind (("C-r" . counsel-minibuffer-history)
  ;;        :map counsel-find-file-map
  ;;             ("M-s" . +ivy-switch-file-search)
  ;;             ("C-s" . +ivy-switch-file-search))
  :config
  ;; (dolist (switch-version
  ;;          '((apropos                  . counsel-apropos)
  ;;            (bookmark-jump            . counsel-bookmark)
  ;;            (describe-face            . counsel-faces)
  ;;            (describe-function        . counsel-describe-function)
  ;;            (describe-variable        . counsel-describe-variable)
  ;;            (describe-bindings        . counsel-descbinds)
  ;;            (set-variable             . counsel-set-variable)
  ;;            (execute-extended-command . counsel-M-x)
  ;;            (locate                   . counsel-locate)
  ;;            ;(find-file                . counsel-find-file)
  ;;            (find-library             . counsel-find-library)
  ;;            (info-lookup-symbol       . counsel-info-lookup-symbol)
  ;;            (imenu                    . counsel-imenu)
  ;;            (recentf-open-files       . counsel-recentf)
  ;;            ;; (swiper                . counsel-grep-or-swiper)
  ;;            (evil-ex-registers        . counsel-evil-registers)
  ;;            (yank-pop                 . counsel-yank-pop)
  ;;            (imenu                    . counsel-imenu)) t)
  ;;   (let ((old-cmd (car switch-version))
  ;;         (new-cmd (cdr switch-version)))
  ;;     (defalias old-cmd new-cmd)))

  (with-eval-after-load 'org
    (defalias 'org-goto 'counsel-org-goto))

  (with-eval-after-load 'helpful
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable))

  ;; Configure `counsel-ag'
  (setq counsel-ag-base-command "ag --nocolor --nogroup --hidden -f %s"
        counsel-find-file-ignore-regexp  "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)"
        counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

    ;; Factories
  (defun +ivy-action-reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))

  (defun +ivy-action-given-file (cmd prompt)
    (lambda (source)
      (let* ((enable-recursive-minibuffers t)
             (target (read-file-name (format "%s %s to:" prompt source))))
        (funcall cmd source target 1))))

  ;; (defun +ivy-open-ace-window (arg)
  ;;   "Use `ace-window' on file candidates in ivy."
  ;;   (ace-window t)
  ;;   (let (;; (default-directory (if (eq (vc-root-dir) nil)
  ;;         ;;                        counsel--fzf-dir
  ;;         ;;                      (vc-root-dir)))
  ;;         )
  ;;     (if (> (length (aw-window-list)) 1)
  ;;         (find-file arg)
  ;;       (find-file-other-window arg))
  ;;     (balance-windows (current-buffer))))

  (defun +ivy-ace-window (cmd)
    "Returns a function that runs `ace-window' and then calls
cmd, a function of one argument."
    (lambda (x)
      (ace-window t)
      (funcall cmd x)))

;; ((+ivy-ace-window #'switch-to-buffer) "do.org")

  ;; Configure `counsel-find-file'
  (dolist (ivy-command '(counsel-find-file counsel-file-jump counsel-fzf counsel-git))
    (ivy-add-actions
     ivy-command
     `(("'" counsel-find-file-cd-bookmark-action "cd bookmark")
       ("S" counsel-find-file-as-root "open as root")
       ("+" counsel-find-file-mkdir-action "mkdir")
       ("c" ,(+ivy-action-given-file #'copy-file "Copy file") "copy file")
       ("k" ,(+ivy-action-reloading (lambda (x) (dired-delete-file x 'confirm-each-subdirectory))) "delete")
       ("r" (lambda (path) (rename-file path (read-string "New name: "))) "rename")
       ("R" ,(+ivy-action-reloading (+ivy-action-given-file #'rename-file "Move")) "move")
       ("`" ,(+ivy-ace-window #'find-file) "ace window")
       ("f" find-file-other-frame "other frame")
       ("j" find-file-other-window "other window")
       ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
       ("x" counsel-find-file-extern "xdg-open")
       ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
       ("l" (lambda (path) "Insert org-link with relative path"
              (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
       ("L" (lambda (path) "Insert org-link with absolute path"
              (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")
       )))

  (ivy-add-actions 'ivy-switch-buffer
                   `(("`" ,(+ivy-ace-window #'switch-to-buffer) "ace window")))
  (ivy-add-actions 'counsel-bookmark
                   '(("f" find-file-other-frame "other frame")
                     ("j" find-file-other-window "other window")))

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
    (counsel-file-jump-multi-dir "" (list (abbreviate-file-name (concat
                                                                 (file-name-as-directory (getenv "HOME"))
                                                                 (file-name-as-directory "Documents")
                                                                 "research"))
                                          (abbreviate-file-name (concat
                                                                 (file-name-as-directory (getenv "HOME"))
                                                                 (file-name-as-directory "Dropbox")
                                                                 "KarthikBassam")))))
  
;;;###autoload
  (defun find-file-config-dirs ()
    "Find files in all system config locations"
    (interactive)
    (counsel-file-jump-multi-dir "" `("~/.local/bin" ,user-emacs-directory "~/.config")))

;;;;###autoload
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


(use-package counsel-projectile
  :disabled t
  :ensure t
  :after evil-leader
  :init (counsel-projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "/ g") 'projectile-grep)
  (define-key projectile-command-map (kbd "/ a") 'projectile-ag)
  (define-key projectile-command-map (kbd "/ G") 'counsel-projectile-git-grep))

(use-package ivy-prescient
  :ensure t
  :after (ivy counsel)
  :hook (ivy-mode . ivy-prescient-mode)
  :init
  (ivy-prescient-mode +1)
  :config
  (setq ;; prescient-filter-method
   ;; (if (featurep +fuzzy)
   ;;     '(literal regexp initialism fuzzy)
   ;;   '(literal regexp initialism))
   ivy-prescient-enable-filtering nil  ; we do this ourselves
   ivy-prescient-enable-sorting t
   ivy-prescient-retain-classic-highlighting t
   ;; ivy-prescient-sort-commands (list 'counsel-file-jump
   ;;                                   'counsel-fzf
   ;;                                   'counsel-git
   ;;                                   'counsel-M-x
   ;;                                   'load-theme)
   ;; ivy-re-builders-alist
   ;; '((counsel-ag . +ivy-prescient-non-fuzzy)
   ;;   (counsel-rg . +ivy-prescient-non-fuzzy)
   ;;   (counsel-grep . +ivy-prescient-non-fuzzy)
   ;;   (swiper . +ivy-prescient-non-fuzzy)
   ;;   (swiper-isearch . +ivy-prescient-non-fuzzy)
   ;;   (t . ivy-prescient-re-builder))
   )

  (setq ivy-initial-inputs-alist '((org-refile                . "^")
                                   (org-agenda-refile         . "^")
                                   (org-capture-refile        . "^")
                                   (Man-completion-table      . "^")
                                   (woman                     . "^")
                                   (counsel-M-x               . "")
                                   (counsel-describe-function . "^")
                                   (counsel-describe-variable . "^")
                                   (counsel-org-capture       . "^")))

  ;; (defun +ivy-prescient-non-fuzzy (str)
  ;;   (let ((prescient-filter-method '(literal regexp)))
  ;;     (ivy-prescient-re-builder str)))

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat (expand-file-name
                                     (file-name-as-directory "~/.cache"))
                                    "prescient-save.el"))
  (prescient-persist-mode +1))

;; ivy-rich shows descriptions along with selection candidates in ivy
(use-package ivy-rich
  :ensure t
  :defer 3
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (defun +ivy-rich-describe-variable-transformer (cand)
    "Previews the value of the variable in the minibuffer"
    (let* ((sym (intern cand))
           (val (and (boundp sym) (symbol-value sym)))
           (print-level 3))
      (replace-regexp-in-string
       "[\n\t\^[\^M\^@\^G]" " "
       (cond ((booleanp val)
              (propertize (format "%s" val) 'face
                          (if (null val)
                              'font-lock-comment-face
                            'success)))
             ((symbolp val)
              (propertize (format "'%s" val)
                          'face 'highlight-quoted-symbol))
             ((keymapp val)
              (propertize "<keymap>" 'face 'font-lock-constant-face))
             ((listp val)
              (prin1-to-string val))
             ((stringp val)
              (propertize (format "%S" val) 'face 'font-lock-string-face))
             ((numberp val)
              (propertize (format "%s" val) 'face 'highlight-numbers-number))
             ((format "%s" val)))
       t)))


  ;;   ;; Include variable value in `counsel-describe-variable'

  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'counsel-describe-variable
                   '(:columns
                     ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
                      (+ivy-rich-describe-variable-transformer (:width 36))
                      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))))

  (ivy-rich-mode +1)

  )

(provide 'setup-ivy)
