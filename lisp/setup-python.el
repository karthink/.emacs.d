(use-package pyvenv
  :disabled t
  :straight t
  :config
  (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python))

(use-package elpy
  :disabled
  ;; :straight t
  :commands elpy
  ;; :init
  ;; (setq python-shell-interpreter "jupyter"
  ;;       python-shell-interpreter-args "console --simple-prompt"
  ;;       python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;;              "jupyter")
  ;; (advice-add 'python-mode :before 'elpy-enable)
  :config
  ;; (add-hook
  ;;  'python-mode-hook
  ;;  (lambda ()
  ;;    (mapc (lambda (pair) (push pair prettify-symbols-alist))
  ;;          '(;; Syntax
  ;;            ("def" .      #x2131)
  ;;            ("not" .      #x2757)
  ;;            ("in" .       #x2208)
  ;;            ("not in" .   #x2209)
  ;;            ("return" .   #x27fc)
  ;;            ("yield" .    #x27fb)
  ;;            ("for" .      #x2200)
  ;;            ;; Base Types
  ;;            ("int" .      #x2124)
  ;;            ("float" .    #x211d)
  ;;            ("str" .      #x1d54a)
  ;;            ("True" .     #x1d54b)
  ;;            ("False" .    #x1d53d)
  ;;            ;; Mypy
  ;;            ("Dict" .     #x1d507)
  ;;            ("List" .     #x2112)
  ;;            ("Tuple" .    #x2a02)
  ;;            ("Set" .      #x2126)
  ;;            ("Iterable" . #x1d50a)
  ;;            ("Any" .      #x2754)
  ;;            ("Union" .    #x22c3)))))
  )

;;;----------------------------------------------------------------
;; *** JUPYTER
;;;----------------------------------------------------------------
(unless IS-GUIX
  (use-package zmq
    :straight (zmq :host github
                   ;;   :repo "nnicandro/emacs-zmq"
                   :repo "dakra/emacs-zmq"
                   :branch "hidden-visibility"
                   :files ("*.el" "*.so")
                   :pre-build (compile "make"))
    
    ;; (zmq :host github
    ;;      :repo "nnicandro/emacs-zmq"
    ;;      :fork (:host github
    ;;             :repo "dakra/emacs-zmq"
    ;;             :branch "hidden-visibility")
    ;;      :pre-build (compile "make")
    ;;      :files ("*.el" "*.so"))
    :defer
    :init
    (add-to-list 'native-comp-deferred-compilation-deny-list "zmq")
    (use-package jupyter 
      :straight t
      :defer
      :init
      (when (version< "28.0" emacs-version)
        (add-to-list 'native-comp-deferred-compilation-deny-list "jupyter")))
    (use-package ob-jupyter
      :after (jupyter ob)
      :config
      ;; Clean up ob-jupyter source block output
      ;; From Henrik Lissner
      (defun my/org-babel-jupyter-strip-ansi-escapes-block ()
        (when (string-match-p "^jupyter-"
                              (nth 0 (org-babel-get-src-block-info)))
          (unless (or
                   ;; ...but not while Emacs is exporting an org buffer (where
                   ;; `org-display-inline-images' can be awfully slow).
                   (bound-and-true-p org-export-current-backend)
                   ;; ...and not while tangling org buffers (which happens in a temp
                   ;; buffer where `buffer-file-name' is nil).
                   (string-match-p "^ \\*temp" (buffer-name)))
            (save-excursion
              (when-let* ((beg (org-babel-where-is-src-block-result))
                          (end (progn (goto-char beg)
                                      (forward-line)
                                      (org-babel-result-end))))
                (ansi-color-apply-on-region (min beg end) (max beg end)))))))

      (add-hook 'org-babel-after-execute-hook
                #'my/org-babel-jupyter-strip-ansi-escapes-block))))

;;;----------------------------------------------------------------
;; *** CONDA SUPPORT
;;;----------------------------------------------------------------
(unless IS-GUIX 
  (use-package conda
    :commands conda-env-activate
    :hook (eshell-first-time-mode . conda-env-initialize-eshell)
    :straight t
    :config
    (setq conda-anaconda-home "/opt/miniconda3/")
    (setq conda-env-home-directory (expand-file-name "~/.conda/"))
    (add-to-list
     'global-mode-string
     '(:eval
       (list
        (if conda-env-current-name
            (propertize (concat "(" conda-env-current-name ") ")
                        'face 'font-lock-builtin-face)
          ""))))))
;;; (setq conda-env-subdirectory "envs")
;;; (unless (getenv "CONDA_DEFAULT_ENV")
;;;   (conda-env-activate "base"))
;;; if you want interactive shell support, include:
;;; (conda-env-initialize-interactive-shells)
;;; if you want eshell support, include:
;;; if you want auto-activation, include:
;;; (conda-env-autoactivate-mode t)
;;; if you want to automatically activate a conda environment on the opening of a file:
;;; (add-to-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
;;;                                      (conda-env-activate-for-buffer))))

(provide 'setup-python)
