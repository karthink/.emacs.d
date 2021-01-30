(declare-function xref-make-prompt "xref" (summary location))
(declare-function xref-make-file-location "xref" (file line column))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql buffer-local)))
  (let ((ident (symbol-name (symbol-at-point))))
    (and (not (equal ident "")) ident)))

(cl-defmethod xref-backend-definitions ((_backend (eql buffer-local)) prompt)
  (beginning-of-buffer)
  (search-forward prompt)
  (list (xref-make prompt (xref-make-buffer-location
                           (current-buffer)
                           (point)))))

(cl-defmethod xref-backend-apropos ((_backend (eql buffer-local)) pattern)
  (xref-backend-definitions 'buffer-local pattern))

(defun buffer-local-xref-activate ()
  "Function to activate buffer-local backend.
Add this function to `xref-backend-functions' to use xref to find
function and variable definitions in the same buffer.

This should have the lowest priority among xref backends."
  (and 'buffer-local))

(provide 'buffer-local-xref)
