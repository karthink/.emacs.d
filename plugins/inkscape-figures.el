;; Call the shell command inkscape-figures on symbols in a latex buffer to create an svg with that name (in Inkscape) and save it in the 'figures' directory under the working TeX directory. 

(defvar +inkscape-figures-directory "figures"
  "Directory in TeX project folder used to store figures")

(defun +inkscape-figures-create-at-point (transform &optional silent-p)
  "Create an svg/pdf with the symbol-at-point as name in the
'figures' directory under the current TeX directory and insert
latex code to embed as figure. With prefix arg, do not insert the
code."
  (let ((tex-file (or tex-main-file (buffer-file-name)))
        (figname   (or (symbol-at-point)
                       (completing-read "Figure name: " nil)))
        figbounds)
    (when (symbol-at-point)
      (setq figbounds (bounds-of-thing-at-point 'symbol))
      (delete-region (car figbounds) (cdr figbounds)))
    
    (call-process-shell-command 
     (concat "inkscape-figures create"
             " '" 
             (if (symbolp figname)
                 (symbol-name figname)
               figname) 
             "' '"
             (file-name-directory tex-file)
             (file-name-as-directory +inkscape-figures-directory)
             "'"
             (if silent-p " >/dev/null")
             " 2>/dev/null"
             )
     nil "*inkscape-figures-output*")
    
    (insert (funcall transform figname (with-current-buffer "*inkscape-figures-output*" (buffer-substring (point-min) (point-max)))))
    (kill-buffer "*inkscape-figures-output*")
    ))

(defun +inkscape-figures-latex-transform (figname latex-template)
  "docstring"
  latex-template)

(defun +inkscape-figures-org-transform (figname latex-template)
  "docstring"
  (let ((cwd (file-name-directory (buffer-file-name)))
        (figname-string (if (symbolp figname)
                            (symbol-name figname)
                          figname)))
    (concat (mapconcat (lambda (s) (concat "#+latex: " s))
                       (split-string-and-unquote latex-template "\n" )
                       "\n")
            "\n[["
            cwd
            (file-name-as-directory +inkscape-figures-directory)
            figname-string
            ".svg]]"
            )))

(defun +inkscape-figures-create-at-point-latex (&optional silent-p)
  "docstring"
  (interactive "P")
  (+inkscape-figures-create-at-point #'+inkscape-figures-latex-transform silent-p))

(defun +inkscape-figures-create-at-point-org (&optional silent-p)
  (interactive "P")
  (+inkscape-figures-create-at-point #'+inkscape-figures-org-transform silent-p))

(defun +inkscape-figures-edit ()
  "Edit an svg/pdf in the 'figures' directory under the current TeX directory"
  (interactive)
  (let ((tex-file (or tex-main-file (buffer-file-name))))
    (call-process-shell-command (concat "inkscape-figures edit"
                                 " '"
                                 (file-name-directory tex-file)
                                 (file-name-as-directory +inkscape-figures-directory)
                                 "'"
                                 ))))

(provide 'inkscape-figures)
