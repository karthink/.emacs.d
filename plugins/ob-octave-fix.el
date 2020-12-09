;; Session evaluation of MATLAB in org-babel is broken, this goes some
;; way towards addressing the problem.
;;
;;- I replaced a `delq' with `delete', the `eq' test was failing on
;; blank strings
;;
;;- For the second fix see function. C-s "kludge".

(defun org-babel-octave-evaluate-session
    (session body result-type &optional matlabp)
  "Evaluate BODY in SESSION."
  (let* ((tmp-file (org-babel-temp-file (if matlabp "matlab-" "octave-")))
	 (wait-file (org-babel-temp-file "matlab-emacs-link-wait-signal-"))
	 (full-body
	  (pcase result-type
	    (`output
	     (mapconcat
	      #'org-babel-chomp
	      (list body org-babel-octave-eoe-indicator) "\n"))
	    (`value
	     (if (and matlabp org-babel-matlab-with-emacs-link)
		 (concat
		  (format org-babel-matlab-emacs-link-wrapper-method
			  body
			  (org-babel-process-file-name tmp-file 'noquote)
			  (org-babel-process-file-name tmp-file 'noquote) wait-file) "\n")
	       (mapconcat
		#'org-babel-chomp
		(list (format org-babel-octave-wrapper-method
			      body
			      (org-babel-process-file-name tmp-file 'noquote)
			      (org-babel-process-file-name tmp-file 'noquote))
		      org-babel-octave-eoe-indicator) "\n")))))
	 (raw (if (and matlabp org-babel-matlab-with-emacs-link)
		  (save-window-excursion
		    (with-temp-buffer
		      (insert full-body)
		      (write-region "" 'ignored wait-file nil nil nil 'excl)
		      (matlab-shell-run-region (point-min) (point-max))
		      (message "Waiting for Matlab Emacs Link")
		      (while (file-exists-p wait-file) (sit-for 0.01))
		      "")) ;; matlab-shell-run-region doesn't seem to
		;; make *matlab* buffer contents easily
		;; available, so :results output currently
		;; won't work
		(org-babel-comint-with-output
		    (session
		     (if matlabp
			 org-babel-octave-eoe-indicator
		       org-babel-octave-eoe-output)
		     t full-body)
		  (insert full-body) (comint-send-input nil t)))) results)
    (pcase result-type
      (`value
       (org-babel-octave-import-elisp-from-file tmp-file))
      (`output
       (setq results
	     (if matlabp
		 (cdr (reverse (delete "" (mapcar #'org-strip-quotes
						  (mapcar #'org-trim raw)))))
	       (cdr (member org-babel-octave-eoe-output
			    (reverse (mapcar #'org-strip-quotes
					     (mapcar #'org-trim raw)))))))
       ;; This kludge is to remove the input lines from the output. Because of
       ;; the special way that MATLAB processes bulk comint output (the output
       ;; of each line follows that line) the macro
       ;; `org-babel-comint-with-output' cannot remove the echoed commands. The
       ;; following handles this manually, by splitting both the original input
       ;; (`BODY') and full output (`RESULTS') on newlines, comparing them line
       ;; by line and removing all lines in BODY from RESULTS. Note that RESULTS
       ;; is already a list of strings so additional care is needed.
       (if matlabp
           (let* ((body-lines (split-string body "\n+"))
                  (result-lines (flatten-list
                                 (mapcar
                                  (lambda (entry) (reverse (split-string entry "\n")))
                                  results))))
             (mapconcat
              #'identity
              (reverse (cl-remove-if
                        (lambda (line) (member line body-lines))
                        result-lines)) "\n"))
         results)))))

(provide 'ob-octave-fix)

;; Testing code
;; (remove-car-upto-newline '("test;string\nno 1\nyesyes" "no 2\n\n whatever"))
;; (multi-replace-regexp-in-string '(("\\s-+" . "qq")
;;                                   ("w"    . "\n"))
;;                                 "this is a wery wery blank line")

;; (with-current-buffer (get-buffer-create "*results*")
;;   (delete-region (point-min) (point-max))
;;   (insert "----raw results\n")
;;   (insert (mapconcat #'identity raw "\n"))
;;   (insert "----fullbody\n")
;;   (insert full-body "\n\n")
;;   (insert "----results\n")
;;   (insert results-output))
