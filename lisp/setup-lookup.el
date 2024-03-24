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

With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead."
  (interactive "P")
  (let ((symbol (symbol-at-point)))
    (when symbol
      (describe-symbol symbol)))
  (when arg
    (let ((help (get-buffer-window "*Help*")))
      (when help
        (if (not (eq (selected-window) help))
            (select-window help)
          (select-window (get-mru-window)))))))
  (unbind-key "C-h C-h"))

(use-package find-func
  :bind (("C-h M-." . find-function-on-key)))

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

;;;----------------------------------------------------------------
;; *** GOOGLE ANSWERS
;;;----------------------------------------------------------------
;; Query Google's knowledge graph. This is the answer that shows up before the
;; first result in Google searches. For this purpose we use tuxi, an external
;; tool that queries Google.
(use-package emacs
  :disabled
  :config
  (defvar google-search-history nil
    "List of queries to google-search-string.")
  (defun google-search-string (search-string)
    "Read SEARCH-STRING from the minibuffer and call the shell
command tuxi on it."
    (interactive (list (read-string "Google: " nil
                                    google-search-history
                                    (thing-at-point 'sexp))))
    (unless (executable-find "tuxi")
      (user-error "Cannot find shell command: tuxi"))
    (let ((search-output (string-trim-right
                          (shell-command-to-string
                           (concat
                            "tuxi -r "
                            (shell-quote-argument search-string))))))
      (with-current-buffer (get-buffer-create "*Tuxi Output*")
        (goto-char (point-max))
        (unless (bobp) (insert "\n\n* * *\n"))
        (insert (capitalize search-string) ":\n\n")
        (push-mark)
        (insert search-output)
        (let ((lines (count-lines (or (mark) (point-min)) (point-max))))
          (if (<= lines 1)
              (message search-output)
            (let ((win (display-buffer (current-buffer))))
              (set-window-start win (mark))
              (set-window-parameter win 'window-height (min lines 10))
              (goto-address-mode 1)))))))
  (defun google-search-at-point (&optional beg end)
    "Call the shell command tuxi on the symbol at point. With an
active region use it instead."
    (interactive "r")
    (if-let ((search-string (if (use-region-p)
                                (buffer-substring-no-properties beg end)
                              (thing-at-point 'symbol))))
        (google-search-string search-string)
      ;; (message "No symbol to search for at point!")
      (call-interactively #'google-search-string)))
  :bind (:map help-map
              ("g" . google-search-string)
              ("C-=" . google-search-at-point)))

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
