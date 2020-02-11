(require 'use-package nil t)
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)

  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 14
        ivy-re-builders-alist '((read-file-name-internal . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (swiper-isearch . ivy--regex-plus)
                                (counsel-ag . ivy--regex-plus)
                                (t . ivy--regex-ignore-order)))
  (setq enable-recursive-minibuffers t)
  ;; (setq ivy-initial-inputs-alist nil)

  ;; Ensure a jump point is registered before jumping to new locations with ivy
  (defvar +ivy--origin nil)
  (defun +ivy--record-position-maybe-fn ()
    (with-ivy-window
      (setq +ivy--origin (point-marker))))
  (setq ivy-hooks-alist '((t . +ivy--record-position-maybe-fn)))

  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
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
    "Switch to counsel-file-jump, preserving current input."
    (interactive)
    (let ((input (ivy--input)))
      (ivy-quit-and-run (counsel-file-jump "" ivy--directory))))

  (define-key ivy-minibuffer-map (kbd "M-j") '+ivy-switch-file-search)
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

  (use-package  ivy-hydra
    :ensure t
    :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body)
    :init
    ;; (define-key! ivy-minibuffer-map
    ;;   "C-o" #'ivy-dispatching-done-hydra
    ;;   "M-o" #'hydra-ivy/body)
    :config
    ;; ivy-hydra rebinds this, so we have to do so again
    ;; (define-key ivy-minibuffer-map (kbd "M-o") #'hydra-ivy/body)
    ))

(use-package counsel
  :ensure t
  :commands counsel-describe-face
  :init
  (dolist (switch-version
           '((org-goto . counsel-org-goto)
             (imenu . counsel-imenu)) t)
    (let ((old-cmd (car switch-version))
          (new-cmd (symbol-function (cdr switch-version))))
      (defalias old-cmd new-cmd)))

  ;; (dolist (switch-version
  ;;          '((apropos                  . counsel-apropos)
  ;;            (bookmark-jump            . counsel-bookmark)
  ;;            (describe-face            . counsel-faces)
  ;;            (describe-function        . counsel-describe-function)
  ;;            (describe-variable        . counsel-describe-variable)
  ;;            (describe-bindings        . counsel-descbinds)
  ;;            (set-variable             . counsel-set-variable)
  ;;            (execute-extended-command . counsel-M-x)
  ;;            (find-file                . counsel-find-file)
  ;;            (find-library             . counsel-find-library)
  ;;            (info-lookup-symbol       . counsel-info-lookup-symbol)
  ;;            (imenu                    . counsel-imenu)
  ;;            (recentf-open-files       . counsel-recentf)
  ;;            (org-capture              . counsel-org-capture)
  ;;            (swiper                   . counsel-grep-or-swiper)
  ;;            (evil-ex-registers        . counsel-evil-registers)
  ;;            (yank-pop                 . counsel-yank-pop)
  ;;            (imenu                    . counsel-imenu))
  ;;          t)
  ;;   (let ((old-cmd (car switch-version))
  ;;         (new-cmd (symbol-function (cdr switch-version))))
  ;;     (defalias old-cmd new-cmd)))

  :config
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
       ("f" find-file-other-window "other window")
       ("F" find-file-other-frame "other frame")
       ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
       ("x" counsel-find-file-extern "xdg-open")
       ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
       ;; ("l" (lambda (path) "Insert org-link with relative path"
       ;;        (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
       ;; ("L" (lambda (path) "Insert org-link with absolute path"
       ;;        (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")
       ))))


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
  :after ivy
  :hook (ivy-mode . ivy-prescient-mode)
  :init
  (ivy-prescient-mode +1)
  (setq ;; prescient-filter-method
   ;; (if (featurep +fuzzy)
   ;;     '(literal regexp initialism fuzzy)
   ;;   '(literal regexp initialism))
   ivy-prescient-enable-filtering nil  ; we do this ourselves
   ivy-prescient-enable-sorting t
   ivy-prescient-retain-classic-highlighting t
   ivy-initial-inputs-alist '((org-refile . "^")
                              (org-agenda-refile . "^")
                              (org-capture-refile . "^")
                              (counsel-M-x . "^")
                              (counsel-describe-function . "^")
                              (counsel-describe-variable . "^")
                              (counsel-org-capture . "^")
                              (Man-completion-table . "^")
                              (woman . "^"))
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

  :config
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
