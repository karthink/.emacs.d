;; UTILITIES FOR MISCELLANEOUS TASKS  -*- lexical-binding: t; -*-


;;----------------------------------------------------------------------
;; COUNT-WORDS-REGION: USING `while'
;;----------------------------------------------------------------------

;;;###autoload
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

;;;###autoload
(defun count-words-buffer ()
  "Print number of words in the region."
;;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0)
          (beginning (point-min))
          (end (point-max)))
      (goto-char beginning)
;;; 2. Run the while loop.
      (while (and (< (point) end)
		  (re-search-forward "\\w+\\W*" end t))
	(setq count (1+ count)))
;;; 3. Send a message to the user.
      (cond ((zerop count) (message "No words"))
            ((= 1 count) (message "1 word"))
            (t  (message  (format "%d words" count)))))))

;;----------------------------------------------------------------------
;; PRINT ASCII TABLE
;;----------------------------------------------------------------------
;;;###autoload
(defun ascii-table ()
  "Display basic ASCII table (0 thru 127)"
  (interactive)
  (pop-to-buffer "*ASCII*")
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
  (special-mode))

;;----------------------------------------------------------------------
;; INSERT FUNCTION DEFINITION AT POINT
;;----------------------------------------------------------------------
;;;###autoload
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

;; SVG screenshots
;;----------------------------------------------------------------------
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Puts the filename in the kill ring and displays it using an
X-sink (Executable dragon-drag-and-drop if available)."
  (interactive)
  (let ((filename (concat (file-name-as-directory
                           (concat (getenv "HOME") "/Pictures/screenshots"))
                           (format "%s_%sx%s_emacs.svg"
                                   (format-time-string "%Y-%m-%d-%H%M%S")
                                   (frame-pixel-width)
                                   (frame-pixel-height))))
        (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (if (executable-find "dragon-drag-and-drop")
        (start-process "drag_screenshot_svg" "drag_screenshot_svg"
                     "dragon-drag-and-drop" filename)
      (message "Could not find dragon-drag-and-drop"))
    (message filename)))

(global-set-key (kbd "<M-print>") #'screenshot-svg)

;;----------------------------------------------------------------------
;; TIMEOUT: GENERIC DEBOUNCE & THROTTLE
;;----------------------------------------------------------------------
(defun timeout--throttle-advice (&optional timeout)
  "Return a function that throttles its argument function.

THROTTLE defaults to 1.0 seconds. This is intended for use as
function advice."
  (let ((throttle-timer)
        (timeout (or timeout 1.0))
        (result))
    (lambda (orig-fn &rest args)
      "Throttle calls to this function."
      (if (timerp throttle-timer)
          result
        (prog1
            (setq result (apply orig-fn args))
          (setq throttle-timer
                (run-with-timer
                 timeout nil
                 (lambda ()
                   (cancel-timer throttle-timer)
                   (setq throttle-timer nil)))))))))

(defun timeout--debounce-advice (&optional delay default)
  "Return a function that debounces its argument function.

DELAY defaults to 0.50 seconds.  DEFAULT is the immediate return
value of the function when called.

This is intended for use as function advice."
  (let ((debounce-timer nil)
        (delay (or delay 0.50)))
    (lambda (orig-fn &rest args)
      "Debounce calls to this function."
      (if (timerp debounce-timer)
          (timer-set-idle-time debounce-timer delay)
        (prog1 default
          (setq debounce-timer
                (run-with-idle-timer
                 delay nil
                 (lambda (buf)
                   (cancel-timer debounce-timer)
                   (setq debounce-timer nil)
                   (with-current-buffer buf
                     (apply orig-fn args)))
                 (current-buffer))))))))

;;;###autoload
(defun timeout-debounce! (func &optional delay default)
  "Debounce FUNC by DELAY seconds.

This advises FUNC, when called (interactively or from code), to
run after DELAY seconds. If FUNC is called again within this time,
the timer is reset.

DELAY defaults to 0.5 seconds. Using a delay of 0 resets the
function.

DEFAULT is the immediate return value of the function when called."
  (if (and delay (= delay 0))
      (advice-remove func 'debounce)
    (advice-add func :around (timeout--debounce-advice delay default)
                '((name . debounce)
                  (depth . -99)))))

;;;###autoload
(defun timeout-throttle! (func &optional throttle)
  "Throttle FUNC by THROTTLE seconds.

This advises FUNC so that it can run no more than once every
THROTTLE seconds.

THROTTLE defaults to 1.0 seconds. Using a throttle of 0 resets the
function."
  (if (= throttle 0)
      (advice-remove func 'throttle)
    (advice-add func :around (timeout--throttle-advice throttle)
                '((name . throttle)
                  (depth . -98)))))


(provide 'utilities)
