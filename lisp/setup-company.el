(use-package company
  :disabled
  :straight t
  :defer 3
  :bind
  (("M-s <tab>" . company-yasnippet)
   
   :map   company-active-map
   ("C-p"   .    nil)
   ("C-n"   .    nil)
   ("C-;"   .    company-other-backend)
   ("C-w"   .    nil)
   ("C-]"   .    company-show-location)
   ("M-."   .    company-show-location)

   :map   company-search-map
   ([return]   . company-complete-selection)
   ("RET"      . company-complete-selection)
   ("S-SPC"    . company-search-toggle-filtering))

  ;; (:keymaps   'company-active-map
  ;; "<tab>"     'company-complete-common-or-cycle
  ;; "TAB"       'company-complete-common-or-cycle
  ;; "<backtab>" 'company-select-previous
  ;; "S-TAB"     'company-select-previous
  ;; "M-n"        nil
  ;; "M-p"        nil
  ;; "C-n"       'company-select-next
  ;; "C-p"       'company-select-previous)

  :config
  ;; (add-to-list 'company-backends 'company-files)
  ;; (add-to-list 'company-backends 'company-dabbrev)
  ;; (add-to-list 'company-backends 'company-jedi)
  ;; (add-to-list 'company-backends 'company-dict)
  (setq company-idle-delay 0.2
        company-dabbrev-downcase 0
        company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        ;;company-tooltip-flip-when-above t
        ;; company-transformers '(company-sort-by-occurrence)
        ;; company-transformers '(company-sort-by-backend-importance)
        ;; company-transformers '(company-sort-by-statistics)
        company-global-modes '(latex-mode matlab-mode emacs-lisp-mode lisp-interaction-mode
                                          python-mode sh-mode fish-mode conf-mode text-mode org-mode)
        company-auto-commit nil
        company-backends '((company-files company-capf))) ;;company-keywords
  (setq tab-always-indent 'complete)
  
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (make-local-variable 'company-idle-delay)
                               (setq-local company-idle-delay 0.5)))
  
  (use-package eldoc
    :config
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort))
  
  (use-package company-tng
    :straight company
    :config (company-tng-mode))
  
  (global-company-mode))

;; Not needed. the capf backend handles completion just fine
(use-package company-auctex
  :disabled
  :defer t
  :config
  (add-to-list 'company-backends 'company-auctex)
  (company-auctex-init))

(use-package company-prescient
  :disabled
  :after company
  :defer 3
  ;; :straight t
  :init (company-prescient-mode))

(use-package company-statistics
  :disabled
  :after company
  :defer 5
  :straight t
  ;; :hook (after-init . company-statistics-mode)
  :init  (company-statistics-mode)
  :config
  (setq company-statistics-file (concat (expand-file-name
                                         (file-name-as-directory "~/.cache"))
                                        "company-statistics-cache.el")))

(provide 'setup-company)
