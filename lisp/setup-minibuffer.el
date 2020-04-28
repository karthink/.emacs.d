;; Minibuffer completion
(setq completion-cycle-threshold 3
      completion-flex-nospace nil
      completion-pcm-complete-word-inserts-delimiters t
      ;;completion-pcm-word-delimiters "-_./:| "
      completion-show-help nil
      completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completions-format 'vertical   ; *Completions* buffer
      enable-recursive-minibuffers t
      read-answer-short t
      resize-mini-windows 'grow-only
      completion-styles '(partial-completion substring initials)
      ;; completion-category-overrides nil
      ;; '((file (styles basic flex substring))
      ;;   (buffer (styles basic flex substring)))
)
;; Technically, this is not specific to the minibuffer, but I define
;; it here so that you can see how it is also used from inside the
;; "Completions" buffer
;;;###autoload
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

;;;###autoload
(defun my/completions-kill-save-symbol ()
  "Add symbol-at-point to the kill ring.

Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'."
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(define-key minibuffer-local-completion-map (kbd "?")
  (lambda () (interactive)
    (minibuffer-completion-help)
    (switch-to-completions)))
(define-key completion-list-mode-map "h" 'my/describe-symbol-at-point)
(define-key completion-list-mode-map "w" 'my/completions-kill-save-symbol)
(define-key completion-list-mode-map "n" 'next-line)
(define-key completion-list-mode-map "p" 'previous-line)
(define-key completion-list-mode-map "n" 'next-line)
(define-key completion-list-mode-map "f" 'next-completion)
(define-key completion-list-mode-map "b" 'previous-completion)
(define-key completion-list-mode-map "M-v" 'my/focus-minibuffer)
(define-key completion-list-mode-map "?" 'my/focus-minibuffer)
(global-set-key (kbd "C-h .") 'my/describe-symbol-at-point)
(global-set-key (kbd "C-h C-.") (lambda ()
				(interactive)
				(my/describe-symbol-at-point '(4))))

;;;###autoload
(defun my/focus-minibuffer ()
  "Focus the active minibuffer.

Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

;;;###autoload
(defun my/focus-minibuffer-or-completions ()
  "Focus the active minibuffer or the \\*Completions\\*.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`my/focus-minibuffer' and `switch-to-completions' in
succession."
  (interactive)
  (let* ((mini (active-minibuffer-window))
         (completions (get-buffer-window "*Completions*")))
    (cond ((and mini
                (not (minibufferp)))
           (select-window mini nil))
          ((and completions
                (not (eq (selected-window)
                         completions)))
           (select-window completions nil)))))

(provide 'setup-minibuffer)
