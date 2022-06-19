;; setup-minibuffer

(use-package minibuffer
  :hook (minibuffer-setup .  cursor-intangible-mode)
  :config
;; Minibuffer completion
  (setq completion-cycle-threshold 2
      completion-flex-nospace nil
      completion-pcm-complete-word-inserts-delimiters nil
      ;;completion-pcm-word-delimiters "-_./:| "
      completion-show-help nil      
      completion-ignore-case nil
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completions-format 'vertical   ; *Completions* buffer
      enable-recursive-minibuffers t
      read-minibuffer-restore-windows t
      read-answer-short t
      resize-mini-windows 'grow-only
      completion-styles '(partial-completion substring initials)
      completion-category-overrides '((file (styles basic-remote partial-completion initials)))
      ;; '((file (styles basic flex substring))
      ;;   (buffer (styles basic flex substring)))
)

  (defun basic-remote-try-completion (string table pred point)
    (and (path-remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (path-remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  
  (defun path-remote-p (path)
    "Return t if PATH is a remote path."
    (string-match-p "\\`/[^/|:]+:" (substitute-in-file-name path)))

(defun my/messageless (fn &rest args)
  "Set `minibuffer-message-timeout' to 0.
Meant as advice around minibuffer completion FN with ARGS."
  (let ((minibuffer-message-timeout 0))
    (apply fn args)))

(advice-add 'minibuffer-force-complete-and-exit :around #'my/messageless)

(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode nil)
;; (define-key minibuffer-local-completion-map (kbd "-") #'minibuffer-complete-word)

(define-key minibuffer-local-completion-map (kbd "?")
  (lambda () (interactive)
    (minibuffer-completion-help)
    (switch-to-completions)))
(define-key completion-list-mode-map "n" 'next-line)
(define-key completion-list-mode-map "p" 'previous-line)
(define-key completion-list-mode-map "n" 'next-line)
(define-key completion-list-mode-map "f" 'next-completion)
(define-key completion-list-mode-map "b" 'previous-completion)
(define-key completion-list-mode-map "M-v" 'my/focus-minibuffer)
(define-key completion-list-mode-map "?" 'my/focus-minibuffer)

(defun my/minibuffer-focus-mini ()
  "Focus the active minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt)))

(provide 'setup-minibuffer)
