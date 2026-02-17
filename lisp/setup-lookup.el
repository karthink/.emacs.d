;;;----------------------------------------------------------------
;; *** HELPFUl
;;;----------------------------------------------------------------
(use-package emacs
  :bind-keymap ("C-h a" . help-apropos-map)
  :bind (("C-h A" . info-apropos)
         ("C-h C-a" . customize-apropos)
         ("C-h ." . my/describe-symbol-at-point)
         ("C-h C-f". describe-face)
         ("C-h C-k" . describe-keymap))
  :config
  (put 'help-fns-edit-variable 'disabled nil)
  (defvar help-apropos-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") #'apropos)
      (define-key map (kbd "l") #'apropos-library)
      (define-key map (kbd "f") #'apropos-function)
      (define-key map (kbd "x") #'apropos-command)
      (define-key map (kbd "v") #'apropos-variable)
      (define-key map (kbd "V") #'apropos-local-variable)
      (define-key map (kbd "u") #'apropos-user-option)
      (define-key map (kbd "d") #'apropos-documentation)
      (define-key map (kbd "C-f") #'customize-apropos-faces)
      (define-key map (kbd "g") #'customize-apropos-groups)
      (define-key map (kbd "o") #'customize-apropos-options)
      (define-key map (kbd "c") #'customize-apropos)
      (define-key map (kbd "i") #'info-apropos)
      map))
  ;; (define-prefix-command help-apropos-command help-apropos-map)
  (defun my/describe-symbol-at-point (&optional arg)
    "Get help (documentation) for the symbol at point.

ARG does nothing yet."
    (interactive "P")
    (let ((thing (thing-at-point 'filename)))
      (when-let* ((symbol (intern-soft thing)))
        (describe-symbol symbol)))
    (unless (get-buffer-window (help-buffer))
      (display-local-help nil t)))
  (unbind-key "C-h C-h"))

(use-package find-func
  :bind (("C-h M-." . find-function-on-key)))

;; MAYBE
(use-package info-colors
  :ensure t
  :hook (Info-selection . info-colors-fontify-node))

(use-package helpful
  :disabled
  :ensure t
  :commands (helpful-callable helpful-variable)
  ;; :hook (helpful-mode . (lambda () (line-number-mode 0)))
  :init
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h C") #'helpful-command)
  (global-set-key (kbd "C-h .") #'helpful-at-point)
  (global-set-key (kbd "C-h C-.") #'helpful-at-point)
  :bind
  (:map help-map
   ("C-f" . describe-face)
   ("." . helpful-at-point)
   ("v" . helpful-variable)
   ("f" . helpful-callable)
   ("k" . helpful-key)
   ("C" . helpful-command)))

(use-package helpful
  :disabled
  :after (helpful embark)
  :bind (:map embark-become-help-map
         ("f" . helpful-callable)
         ("v" . helpful-variable)
         ("C" . helpful-command)))

(use-package man
  :defer t
  :custom (Man-notify-method 'aggressive))


;;----------------------------------------------------------------
;; ** ELDOC
;;----------------------------------------------------------------
(use-package eldoc
  :hook (ielm-mode . eldoc-mode)
  :config
  (setq eldoc-documentation-strategy
        'eldoc-documentation-compose-eagerly
        eldoc-echo-area-prefer-doc-buffer t))

;;;----------------------------------------------------------------
;; *** DICTIONARY
;;;----------------------------------------------------------------
(use-package sdcv
  :disabled
  ;; :straight nil
  :commands (sdcv-search-input)
  :bind (("C-x M-=" . sdcv-search-input)
         :map sdcv-mode-map
         ("M-n" . sdcv-next-dictionary)
         ("M-p" . sdcv-previous-dictionary)))

(use-package dictionary
  ;; :straight (:type built-in)
  :ensure nil
  :commands (dictionary-lookup-definition dictionary-search)
  :config
  (define-key help-map (kbd "C-d") 'apropos-documentation)
  (setq dictionary-use-single-buffer t)
  (defun dictionary-search-dwim (&optional arg)
    "Search for definition of word at point. If region is active,
search for contents of region instead. If called with a prefix
argument, query for word to search."
    (interactive "P")
    (if arg
        (dictionary-search nil)
      (if (use-region-p)
          (dictionary-search (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
        (if (thing-at-point 'word)
            (dictionary-lookup-definition)
          (dictionary-search-dwim '(4))))))

  (defvar my/dictionary-log-file
    (concat user-cache-directory "dictionary-log")
    "File that tracks looked up words.")
  (advice-add 'dictionary-search :after
              (defun my/dictionary-log-update (word &optional dictionary)
                "Add the looked up WORD to `my/dictionary-log-file'."
                (when word
                  (write-region (concat word "\n") nil
                                my/dictionary-log-file
                                'append))))

  :bind (("C-M-=" . dictionary-search-dwim)
         :map help-map
         ("=" . dictionary-search-dwim)
         ("d" . dictionary-search)))

(provide 'setup-lookup)
