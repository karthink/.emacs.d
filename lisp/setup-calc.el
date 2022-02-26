(use-package calc
  :bind (("C-x c" . calc)
         ("H-S-c" . calc)
         ("H-*" . calc-dispatch)
         ("C-S-e" . latex-math-from-calc))
  :config
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (let ((lang (when (member major-mode '(org-mode latex-mode))
                  'latex)))
      (cond ((region-active-p)
             (let* ((beg (region-beginning))
                    (end (region-end))
                    (string (buffer-substring-no-properties beg end)))
               (kill-region beg end)
               (insert (calc-eval `(,string calc-language ,lang
                                            calc-prefer-frac t
                                            calc-angle-mode rad)))))
            (t (let ((l (thing-at-point 'line)))
                 (end-of-line 1) (kill-line 0)
                 (insert (calc-eval `(,l
                                      calc-language ,lang
                                      calc-prefer-frac t
                                      calc-angle-mode rad)))))))))

(use-package calctex
  :straight (calctex :host github :repo "johnbcoughlin/calctex"
                     :files ("*.el" "vendor" "org-calctex/*.el"
                             "calctex/*.el" "calctex-contrib/*.el"))
  :after calc
  :config
  (setq calctex-additional-latex-packages "
\\usepackage[usenames]{xcolor}
\\usepackage{soul}
\\usepackage{adjustbox}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{siunitx}
\\usepackage{cancel}
\\usepackage{mathtools}
\\usepackage{mathalpha}
\\usepackage{xparse}
\\usepackage{arevmath}"
        calctex-additional-latex-macros
        (concat calctex-additional-latex-macros
                "\n\\let\\evalto\\Rightarrow"))
  (let ((vendor-folder (expand-file-name
                        (concat (file-name-as-directory "straight")
                                (file-name-as-directory straight-build-dir)
                                (file-name-as-directory "calctex/vendor"))
                        straight-base-dir)))
    (setq calctex-dvichop-bin (concat vendor-folder "texd/dvichop")
          calctex-dvichop-sty (concat vendor-folder "texd/dvichop")
          calctex-imagemagick-enabled-p nil))
  (unless (file-exists-p calctex-dvichop-bin)
    (message "CalcTeX: Building dvichop binary")
    (let ((default-directory (file-name-directory calctex-dvichop-bin)))
      (compile "make"))))

(provide 'setup-calc)
