(declare-function xref-make-prompt "xref" (summary location))
(declare-function xref-make-file-location "xref" (file line column))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql matlab-shell)))
  (let ((ident (matlab-read-word-at-point)))
    (and (not (equal ident "")) ident)))

(cl-defmethod xref-backend-definitions ((_backend (eql matlab-shell)) prompt)
  (let* ((file-and-flag (matlab-shell-which-fcn prompt))
         (built-in-p (cdr file-and-flag))
         (file (if built-in-p
                   (replace-regexp-in-string "/@[^/]+" "" (car file-and-flag))
                 (car file-and-flag))))
    (when (and (not (equal file "")) (file-exists-p file))
      (list (xref-make prompt
                       (xref-make-file-location
                        file
                        1 0))))))

(cl-defmethod xref-backend-apropos ((_backend (eql matlab-shell)) pattern)
  (xref-backend-definitions 'matlab-shell pattern))

(defun matlab-shell-xref-activate ()
  "Function to activate xref backend.
Add this function to `xref-backend-functions' for matlab shell to
use xref to find function and variable definitions."
  (and (member major-mode '(matlab-mode matlab-shell-mode org-mode))
       (matlab-shell-active-p)
       'matlab-shell))

(provide 'matlab-xref)
