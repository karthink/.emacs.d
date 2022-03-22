;; Restrict re-builder matches to region

(defun reb-update-overlays (&optional subexp)
  "Switch to `reb-target-buffer' and mark all matches of `reb-regexp'.
If SUBEXP is non-nil mark only the corresponding sub-expressions."
  (let* ((re (buffer-local-value 'reb-regexp reb-target-buffer))
	 (subexps (reb-count-subexps re))
	 (matches 0)
	 (submatches 0)
	 firstmatch
         here
         start end
         firstmatch-after-here)
    (with-current-buffer reb-target-buffer
      (setq here
            (if reb-target-window
                (with-selected-window reb-target-window (window-point))
              (point))
            start (if (region-active-p) (nth 1 my/re-builder-positions) (nth 0 my/re-builder-positions))
            end   (if (region-active-p) (nth 2 my/re-builder-positions) (point-max)))
      (reb-delete-overlays)
      (goto-char start)
      (while (and (not (eobp))
		  (re-search-forward re end t)
		  (or (not reb-auto-match-limit)
		      (< matches reb-auto-match-limit)))
	(when (and (= 0 (length (match-string 0)))
		   (not (eobp)))
	  (forward-char 1))
	(let ((i 0)
	      suffix max-suffix)
	  (setq matches (1+ matches))
	  (while (<= i subexps)
	    (when (and (or (not subexp) (= subexp i))
		       (match-beginning i))
	      (let ((overlay (make-overlay (match-beginning i)
					   (match-end i)))
		    ;; When we have exceeded the number of provided faces,
		    ;; cycle thru them where `max-suffix' denotes the maximum
		    ;; suffix for `reb-match-*' that has been defined and
		    ;; `suffix' the suffix calculated for the current match.
		    (face
		     (cond
		       (max-suffix
			(if (= suffix max-suffix)
			    (setq suffix 1)
			  (setq suffix (1+ suffix)))
			(intern-soft (format "reb-match-%d" suffix)))
		       ((intern-soft (format "reb-match-%d" i)))
		       ((setq max-suffix (1- i))
			(setq suffix 1)
			;; `reb-match-1' must exist.
			'reb-match-1))))
		(unless firstmatch (setq firstmatch (match-data)))
                (unless firstmatch-after-here
                  (when (> (point) here)
                    (setq firstmatch-after-here (match-data))))
		(setq reb-overlays (cons overlay reb-overlays)
		      submatches (1+ submatches))
		(overlay-put overlay 'face face)
		(overlay-put overlay 'priority i)))
	    (setq i (1+ i))))))
    (let ((count (if subexp submatches matches)))
      (message "%s %smatch%s%s"
	       (if (= 0 count) "No" (int-to-string count))
	       (if subexp "subexpression " "")
	       (if (= 1 count) "" "es")
	       (if (and reb-auto-match-limit
			(= reb-auto-match-limit count))
		   " (limit reached)" "")))
    (when firstmatch
      (store-match-data (or firstmatch-after-here firstmatch))
      (reb-show-subexp (or subexp 0)))))

(provide 'reb-fix)
;;; reb-fix.el ends here
