;; -*- lexical-binding: t; -*-
;;;---------------------------------------------------------------- 
;; * OUTLINE MODE
;;;----------------------------------------------------------------
;; :PROPERTIES:
;; :CUSTOM_ID: outline-mode
;; :END:

(use-package outline
  :diminish (outline-minor-mode . " ֍")
  :bind (:map outline-minor-mode-map
              ("TAB" . my/outline-cycle)
              ("<tab>" . my/outline-cycle)
              ("<backtab>" . outline-cycle-buffer)
              ("S-<iso-lefttab>" . outline-cycle-buffer)
              ("C-c C-n" . outline-next-visible-heading)
              ("C-c C-p" . outline-previous-visible-heading)
              ("C-c C-u" . outline-up-heading)
              ("C-c M-<up>" . outline-move-subtree-up)
              ("C-c M-<down>" . outline-move-subtree-down)
              ("C-c M-<left>" . outline-promote)
              ("C-c M-<right>" . outline-demote))
  :config
  (defun my/outline-cycle ()
    (interactive)
    (if (save-excursion (forward-line 0)
                        (looking-at-p outline-regexp))
        (call-interactively #'outline-cycle)
      (let* ((outline-minor-mode nil)
             (cmd (or (key-binding (this-command-keys-vector))
                      (key-binding (key-parse "TAB")))))
        (when cmd
          (setq this-command cmd)
          (call-interactively cmd)))))
  (put 'my/outline-cycle 'repeat-map
       'outline-navigation-repeat-map))

(use-package outline
  :when (version< "28.0" emacs-version)
  :defer
  :bind (:map outline-navigation-repeat-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("C-n" . nil)
              ("C-p" . nil)
              ("C-f" . nil)
              ("C-b" . nil)
              ("M-<left>" . outline-promote)
              ("M-<right>" . outline-demote)
              ("M-<up>" . outline-move-subtree-up)
              ("M-<down>" . outline-move-subtree-down))
  :config
  (dolist (sym '(outline-promote outline-demote outline-cycle
                 outline-move-subtree-up outline-move-subtree-down))
    (put sym 'repeat-map 'outline-navigation-repeat-map)))

;;;----------------------------------------------------------------
;; * HIDESHOW (built in)
;;;----------------------------------------------------------------
(use-package hideshow ; built-in
  :commands (hs-cycle
             hs-global-cycle)
  :bind (:map prog-mode-map
              ("C-<tab>" . hs-cycle)
              ("<backtab>" . hs-global-cycle)
              ("C-S-<iso-lefttab>" . hs-global-cycle))
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'hideshow-set-up-overlay-fn)

  (defface hideshow-folded-face
    `((t (:inherit font-lock-comment-face :weight light)))
    "Face to hightlight `hideshow' overlays."
    :group 'hideshow)
  
  (defun hideshow-set-up-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put
       ov 'display (propertize "  [...]  " 'face 'hideshow-folded-face))))
  
  (dolist (hs-command (list #'hs-cycle
                            #'hs-global-cycle))
    (advice-add hs-command :before
                (lambda (&optional end) "Advice to ensure `hs-minor-mode' is enabled"
                  (unless (bound-and-true-p hs-minor-mode)
                    (hs-minor-mode +1)))))

  (defun hs-cycle (&optional level)
    (interactive "p")
    (save-excursion
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;;TODO: Fix this case. `hs-show-block' needs to be called twice to
             ;;open all folds of the parent block.
             (hs-show-block)
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))
  
  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))  
  
  ;; extra folding support for more languages
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append
           '((vimrc-mode "{{{" "}}}" "\"")
             ;; (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
             ;;            ""
             ;;            "#"
             ;;            +fold-hideshow-forward-block-by-indent-fn nil)
             ;; (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp-fn nil)
             ;; (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
             ;;            "end\\|[]}]"
             ;;            "#\\|=begin"
             ;;            ruby-forward-sexp)
             ;; (enh-ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
             ;;                "end\\|[]}]"
             ;;                "#\\|=begin"
             ;;                enh-ruby-forward-sexp nil)
             (matlab-mode "^\s*if\\|switch\\|case\\|otherwise\\|while\\|^\s*for\\|try\\|catch\\|function"
                          "end"
                          "" (lambda (_arg) (matlab-forward-sexp)))
             (nxml-mode "<!--\\|<[^/>]*[^/]>"
                        "-->\\|</[^/>]*[^/]>"
                        "<!--" sgml-skip-tag-forward nil))
           hs-special-modes-alist
           '((t))))))

(provide 'setup-folds)
