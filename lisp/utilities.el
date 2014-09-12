;; UTILITIES FOR MISCELLANEOUS TASKS


;;----------------------------------------------------------------------
;; COUNT-WORDS-REGION: USING `while'
;;----------------------------------------------------------------------

(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
;;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)
;;; 2. Run the while loop.
      (while (and (< (point) end)
		  (re-search-forward "\\w+\\W*" end t))
	(setq count (1+ count)))
;;; 3. Send a message to the user.
      (cond ((zerop count)
	     (message
	      "The region does NOT have any words."))
	    ((= 1 count)
	     (message
	      "The region has 1 word."))
	    (t
	     (message
	      "The region has %d words." count))))))

;; count words in region
;; (global-set-key (kbd "C-=") 'count-words-region)


;;----------------------------------------------------------------------
;; PRINT ASCII TABLE
;;----------------------------------------------------------------------
(defun ascii-table ()
  "Display basic ASCII table (0 thru 127)"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion (let ((i -1))
                    (insert "ASCII characters 0 thru 127.\n\n")
                    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
                    (while (< i 31)
                      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                                      (setq i (+ 1  i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)))
                      (setq i (- i 96)))))
  (toggle-read-only 1))

;;----------------------------------------------------------------------
;; INSERT FUNCTION DEFINITION AT POINT
;;----------------------------------------------------------------------
(defun insert-definition-at-point ()
  "Function to find the definition of the defun at point and insert it there."
  (interactive)
  (save-excursion
    (imenu (thing-at-point 'symbol))
    (mark-defun)
    (kill-ring-save (region-beginning)
                    (region-end)))
  (with-temp-buffer
    (yank)
    (beginning-of-buffer)
    (delete-blank-lines) 
    (kill-new (buffer-substring-no-properties
               (point-min)
               (point-max))
              t))
  (beginning-of-line)
  (yank)
  (kill-whole-line)
  (beginning-of-defun))

(global-set-key (kbd "C-x C-M-y") 'insert-definition-at-point)

;;----------------------------------------------------------------------

(provide 'utilities)
