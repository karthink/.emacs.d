(define-minor-mode wrap-region-mode
  "Wrap selected text with specified delimiters. Delimiters
  include \", (), [] and {}. When an opening delimiter is typed
  with a region selected, the region will be wrapped between the
  two. With no region highlighted, this mode has no effect."
  :lighter " wrap"
  :version "0.1"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "\"")
              (lambda () (interactive) (wrap-region "\"" "\"")))
            (define-key map (kbd "(")
              (lambda () (interactive) (wrap-region "(" ")")))
            (define-key map (kbd "[")
              (lambda () (interactive) (wrap-region "[" "]")))
            (define-key map (kbd "{")
              (lambda () (interactive) (wrap-region "{" "}")))
            map))

(defun wrap-region (begin-char end-char)
  "Function to insert quotes around a marked region"
  (if (and transient-mark-mode
           (region-active-p))
      (let ((beg (region-beginning))
            (end (region-end)))
        (save-excursion 
          (goto-char beg)
          (insert begin-char)
          (goto-char end)
          (forward-char 1)
          (insert end-char))
        (message (format "%s %s" beg end)))
    (insert begin-char)))

;; (defvar wrap-region-pairs '(("\"" . "\"")
;;                               ("(" . ")")
;;                               ("[" . "]")
;;                               ("{" . "}"))
;;   "Pairs of characters in which to enclose a selected region.")

(provide 'wrap-region)
