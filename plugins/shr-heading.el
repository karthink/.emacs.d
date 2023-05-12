;;; shr-heading.el --- Navigation by heading   -*- lexical-binding: t; -*-

;; From https://github.com/oantolin/emacs-config/blob/master/my-lisp/shr-heading.el
(defun shr-heading-next (&optional arg)
  "Move forward by ARG headings (any h1-h4).
If ARG is negative move backwards, ARG defaults to 1."
  (interactive "p")
  (unless arg (setq arg 1))
  (catch 'return
    (dotimes (_ (abs arg))
      (when (> arg 0) (end-of-line))
      (if-let ((match
                (funcall (if (> arg 0)
                             #'text-property-search-forward
                           #'text-property-search-backward)
                         'face '(shr-h1 shr-h2 shr-h3 shr-h4)
                         (lambda (tags face)
                           (cl-loop for x in (if (consp face) face (list face))
                                    thereis (memq x tags))))))
          (goto-char
           (if (> arg 0) (prop-match-beginning match) (prop-match-end match)))
        (throw 'return nil))
      (when (< arg 0) (beginning-of-line)))
    (beginning-of-line)
    (point)))

(defun shr-heading-previous (&optional arg)
  "Move backward by ARG headings (any h1-h4).
If ARG is negative move forwards instead, ARG defaults to 1."
  (interactive "p")
  (shr-heading-next (- (or arg 1))))

(defun shr-heading--line-at-point ()
  "Return the current line."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun shr-heading-setup-imenu ()
  "Setup imenu for h1-h4 headings in eww buffer.
Add this function to appropriate major mode hooks such as
`eww-mode-hook' or `elfeed-show-mode-hook'."
  (setq-local
   imenu-prev-index-position-function #'shr-heading-previous
   imenu-extract-index-name-function  #'shr-heading--line-at-point))

(add-hook 'eww-mode-hook 'shr-heading-setup-imenu)

(provide 'shr-heading)
