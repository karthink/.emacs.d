;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------
;;;; BETTER EDITING
;;----------------------------------------------------------------------

;; (flyspell-mode)

(save-place-mode 1)
(setq save-place-file (dir-concat user-cache-directory "places"))
(setq-default fill-column 80)
(setq vc-follow-symlinks t)
(setq scroll-error-top-bottom t)
(setq mark-even-if-inactive t)
(setq kill-whole-line t)
;; (and (require 'use-package nil t)
;;      (use-package visual-fill-column-mode
;;        ;; :straight t
;;        ;; :hook (visual-line-mode . visual-fill-column-mode)
;;        :config
;;        (setq split-window-preferred-function #'visual-fill-column-mode-split-window-sensibly)
;;        (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
;;        ;; :init (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;;        ))

;; Collection of bindings and functions to make editing less painful on Emacs

;; /Enable/DISABLE cua-selection mode, for awesome rectangle selects
;; (Set cua-selection-mode for rectangle editing)
;; (cua-selection-mode 1)
;; (delete-selection-mode 0)

;; Prevent Emacs from bugging me about C-x n n not being
;; user-friendly.
(put 'narrow-to-region 'disabled nil)

;; Better default delete-char
(global-set-key (kbd "C-d") 'delete-forward-char)

;; Better defaults for upcase/downcase
(use-package simple
  :config
  (global-set-key (kbd "M-u") 'upcase-dwim)
  (global-set-key (kbd "M-l") 'downcase-dwim)
  (global-set-key (kbd "M-c") 'capitalize-dwim)
  
  ;; Change case with impunity 
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;; Turn off yucky visual line mode
(visual-line-mode nil)

;; Dynamic abbreviations does not search for case
(setq dabbrev-case-fold-search t)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
      dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")

;; Sentence end need not be "  " (double space)
(setq sentence-end-double-space t)

;; Turn on show-paren-mode. Highlights matching parentheses
;; (show-paren-mode 1)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Turn off transient-mark-mode
;; (transient-mark-mode -1)
;; With tmm turned off, make unit-selections activate mark
;; (defun my/activate-mark (&rest _)
;;   (activate-mark))
;; (dolist (command '(mark-word
;;                    mark-sexp
;;                    mark-paragraph
;;                    mark-defun
;;                    mark-page
;;                    mark-whole-buffer
;;                    rectangle-mark-mode))
;;   (advice-add command :after #'my/activate-mark))

;;; Text mode and Auto Fill mode
; Set default Emacs mode to text-mode. In addition, turn on
; word-wrapping and either auto filling of text or longlines-mode,
; which auto fills in Emacs buffers but not when you copy the text.
(setq default-major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'toggle-word-wrap)

;; Save clipboard before changing it
(setq save-interprogram-paste-before-kill t)

(use-package cua-rect
  :bind ("C-x SPC" . cua-rectangle-mark-mode))
;;----------------------------------------------------------------------
;; KEYBINDINGS
;;----------------------------------------------------------------------

(global-set-key (kbd "M-=") 'count-words)

;; Have C-a toggle between beginning of line and indentation
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

;; Transpose sentences
(global-set-key "\C-x\M-t" 'transpose-sentences)

;;; Autocompletion is power.
;; (global-set-key (kbd "C-;") 'complete-symbol) 

;; Cycle spacing: just-one-space to no-space to original-spacing
(setq cycle-spacing-actions
      '(just-one-space delete-all-space restore))
(defun my/cycle-spacing-impatient (&optional n)
  (interactive "*p")
  "Call `cycle-spacing', but in fast mode."
  ;; (cycle-spacing (if (= n 1) -1 n) preserve-nl-back 'fast)
  (cycle-spacing (if (= n 1) -1 n)))
(global-set-key (kbd "M-\\") #'my/cycle-spacing-impatient)

;;; Alternative to M-x
;; (global-set-key "\C-x\C-m" 'execute-extended-command)
;;(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Better join-lines
(defvar-keymap delete-indentation-map
  :repeat t
  "^" #'my/delete-indentation)
(defun my/delete-indentation (arg)
  "Run `delete-indentation', but backwards."
  (interactive "p")
  (unless (use-region-p)
    (when arg (setq current-prefix-arg (- arg))))
  (call-interactively #'delete-indentation))
(global-set-key (kbd "M-^") #'my/delete-indentation)

;;; Unbind `C-x f'
(global-unset-key "\C-xf")

;;; Easy beginning/end of buffer (think < and >):
(global-set-key (kbd "C-x ,") 'beginning-of-buffer)
;; (global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x .") 'end-of-buffer)
;; (global-set-key (kbd "C-x l") 'end-of-buffer)

;;; keybindings for Vi like o and O
;; autoindent open-*-lines
(global-set-key (kbd "C-S-O") 'open-previous-line)
(global-set-key (kbd "C-o") 'open-next-line)

;; Zapping up to char and Copying to char (intuitive versions)
(global-set-key "\C-z" 'zap-up-to-char)
(global-set-key "\M-z" 'zap-to-char-save)

;; Set keyboard shortcuts for parenthesizing sentences in the
;; following modes
(define-key text-mode-map "\M-(" 'insert-parentheses-sentence)

;; Key to duplicate line
(global-set-key (kbd "C-S-d") 'duplicate-line)

;; Key to kill-whole-line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; Key to kill-whole-sentence
(global-set-key (kbd "M-K") (lambda ()
                              (interactive)
                              (backward-sentence)
                              (kill-sentence)))

;; Key to kill word backwards OR kill region 
(global-set-key (kbd "C-w") 'backward-kill-word-or-region)
(global-set-key (kbd "C-S-w") 'kill-region)

;; Easy keys to traverse paragraphs
(global-set-key (kbd "M-]") 'forward-paragraph)
;; tty emacs has mouse trouble with this binding:
(when window-system
    (global-set-key (kbd "M-[") 'backward-paragraph))

;; Re-search forward
;;; Superceded by iy-go-to-char
;; (global-set-key (kbd "M-s") 'isearch-forward-regexp)

;; Re-search backward
;;; Superceded by iy-go-to-char
;; (global-set-key (kbd "M-r") 'isearch-backward-regexp)

;; Easy regex replace
(defalias 'rr 'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
;; (global-set-key (kbd "H-%") 'replace-string)
;; (global-set-key (kbd "H-M-%") 'replace-regexp)
(global-set-key (kbd "C-x / s") 'replace-regexp)
(global-set-key (kbd "C-x / q") 'query-replace-regexp)
(global-set-key (kbd "C-x / r") 'query-replace)
(global-set-key (kbd "C-x / d") 'delete-matching-lines)
(global-set-key (kbd "C-x / o") 'occur)
(global-set-key (kbd "C-x / /") 'isearch-forward-regexp)
;; Multiple search and replace
(global-set-key (kbd "C-x / Q") 'batch-replace-strings)
;; Replace in whole buffer
(global-set-key (kbd "C-x / W") 'replace-in-buffer)

;; Adding text to and from elsewhere
(global-unset-key (kbd "C-x i"))
(global-set-key (kbd "C-x i b") 'insert-buffer)
(global-set-key (kbd "C-x i a") 'append-to-buffer)
(global-set-key (kbd "C-x i f") 'insert-file)

;; Easy align on regex
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Use M-j to join lines. C-j splits them, so it's all good.
(global-set-key (kbd "M-j") 'join-line)

;; Easily comment and uncomment region
(global-set-key (kbd "C-c ;") 'comment-region)

;; Hippie-expand for better autocompletion
(global-set-key (kbd "M-/") 'hippie-expand)

;; Keybindings for occur inside isearch
;; (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;; Unfill-region
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Register commands
(define-key global-map (kbd "M-m") 'store-register-dwim)
(define-key global-map (kbd "M-'") 'use-register-dwim)

;;----------------------------------------------------------------------

;; FUNCTIONS
;;----------------------------------------------------------------------

;;;###autoload
(defun forward-sexp (&optional arg interactive)
  "Customized: Move forward across one balanced expression (sexp).
With ARG, do it that many times. Negative arg -N means move
backward across N balanced expressions. This command assumes
point is not in a string or comment. Calls
`forward-sexp-function' to do the work, if that is non-nil. If
unable to move over a sexp, signal `scan-error' with three
arguments: a message, the start of the obstacle (usually a
parenthesis or list marker of some kind), and end of the
obstacle. If INTERACTIVE is non-nil, as it is interactively,
go up/down the list instead."
  (interactive "^p\nd")
  (if interactive
      (condition-case _
          (forward-sexp arg nil)
        (scan-error (if (> arg 0)
                        (up-list 1 t t)
                      (up-list -1 t t))))
    (or arg (setq arg 1))
    (if forward-sexp-function
        (funcall forward-sexp-function arg)
      (goto-char (or (scan-sexps (point) arg) (buffer-end arg)))
      (if (< arg 0) (backward-prefix-chars)))))


;;;###autoload
(defun listify (beg end &optional stringify)
  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (whitespace-cleanup)
      (goto-char (point-min))
      (when (bolp) (forward-char 1))
      (insert "(")
      (when stringify
        (while (not (eobp))
          (insert "\"")
          (end-of-line)
          (insert "\"")
          (forward-line 1)))
      (goto-char (point-max))
      (when (bolp) (backward-char 1))
      (insert ")"))))

;;;###autoload
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(defun back-to-indentation-or-beginning () (interactive)
   (if (= (point) (progn (beginning-of-line-text) (point)))
       (beginning-of-line)))

;; (defun beginning-of-line-or-indentation ()
;;   "move to beginning of line, or indentation"
;;   (interactive)
;;   (if (bolp)
;;       (back-to-indentation)
;;     (beginning-of-line)))


;;;###autoload
(defun duplicate-line (&optional arg)
  "Duplicate it. With prefix ARG, duplicate ARG lines following the current one."
  (interactive "p")
  (cl-destructuring-bind (beg . end) (if (region-active-p)
                                      (cons (region-beginning)
                                            (region-end))
                                    (cons (line-beginning-position)
                                          (line-end-position)))
    (copy-region-as-kill beg end)
    (if (region-active-p)
        (progn (dotimes (_ arg)
                 (goto-char (region-end))
                 (yank))
               (exchange-point-and-mark)) 
      (save-excursion
        (dotimes (_ arg)
          (open-previous-line 1)
          (yank)
          (indent-according-to-mode))))))

;; (defun duplicate-line-many-once (&optional arg)
;;   "Duplicate it. With prefix ARG, duplicate ARG lines following the current one. This function is deprecated. Use duplicate-line with a selected region instead"
;;   (interactive "p")
;;   (save-excursion 
;;     (let ((beg (line-beginning-position))
;;           (end 
;;            (progn (forward-line (1- arg)) (line-end-position))))
;;       (copy-region-as-kill beg end)
;;       (end-of-line) (newline)
;;       (yank)))
;;   (next-line arg))

;;; behave like vi's o command
;;;###autoload
(defun open-next-line (arg)
  "Move to the next line and then opens a line.

 See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;;; behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
 
  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))


;;; zap-up-to-char: Leave char unchanged!
;;;###autoload
(defun zap-up-to-char (arg char)
  "Zap up to CHAR, (optional) ARG number of times"
  (interactive "p\ncZap up to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (message (char-to-string char))
  (kill-region (progn 
                 (if (looking-at (char-to-string char))
                     (if (> arg 0) 
                         (forward-char)))
                 (point))
               (progn
                 (search-forward (char-to-string char)
                                 nil nil arg)
                 (cond ((< arg 0) (forward-char))
                       (t (backward-char)))
                 (point))))

;;; zap-to-char, but copy instead of killing
;;;###autoload
(defun zap-to-char-save (arg char)
  "Zap to CHAR, (optional) ARG number of times, but save instead of kill."
  (interactive "p\ncSave to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (save-excursion (message (char-to-string char))
                  (copy-region-as-kill (point) 
                                       (progn
                                         (search-forward 
                                          (char-to-string char) 
                                          nil nil arg)
                                         (point)))))

;;; Modifies kill-ring-save so that with no active mark, the current
;;; line is saved.
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Bind C-w to backward-kill-word ulness region is active
;;;###autoload
(defun backward-kill-word-or-region (&optional arg)
  "Kill word backward if region is inactive; else kill region"
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg))
  )

;;; Modifies kill-region so that with no active mark, the current line
;;; is saved.
;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))

;;;###autoload
(defun insert-parentheses-sentence ()
  "Insert () around the sentence at point."
  (interactive)
  (save-excursion
    (backward-sentence)
    (while (not (looking-at "[a-zA-Z0-9]"))
      (forward-char))
    (insert "(")
    (forward-sentence)
    (insert ")")
    ))

;;;###autoload
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

;;;###autoload
(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

;;;###autoload
(defun isearch-occur ()
  "*Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

;;;###autoload
(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (buffer-file-name)
      (save-buffer args)))
(add-hook 'auto-save-hook 'save-buffer-if-visiting-file)

;;----------------------------------------------------------------------
;; Better mark handling
;;----------------------------------------------------------------------
;; Switch to older buffers with marks with pop-to-buffer
(advice-add 'pop-global-mark :around
            (defun my/pop-global-mark-display-buffer (pgm)
              (interactive)
              (cl-letf (((symbol-function 'switch-to-buffer)
                         #'pop-to-buffer))
                (funcall pgm))))

;;----------------------------------------------------------------------
;; MOVE-LINE-REGION
;;----------------------------------------------------------------------
;; Functions to move blocks of text (or single lines) up and down
;;;###autoload
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          ;; ;; Account for changes to transpose-lines in Emacs 24.3
          ;; (when (and (eval-when-compile
          ;;              (not (version-list-<
          ;;                    (version-to-list emacs-version)
          ;;                    '(24 3 50 0))))
          ;;            (< arg 0))
          ;;   (forward-line -1))
          )
        (forward-line -1))
      (move-to-column column t)))))

;;;###autoload
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

;;;###autoload
(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))


;;----------------------------------------------------------------------
;; Multiple search and replace
;;----------------------------------------------------------------------

;;;###autoload
(defun batch-replace-strings (replacement-alist)
  "Prompt user for pairs of strings to search/replace, then do so in the current buffer"
  (interactive (list (batch-replace-strings-prompt)))
  (dolist (pair replacement-alist)
    (save-excursion
      (replace-string (car pair) (cdr pair) nil (region-beginning) (region-end)))))

;;;###autoload
(defun batch-replace-strings-prompt ()
  "prompt for string pairs and return as an association list"
  (let (from-string
        ret-alist)
    (while (not (string-equal "" (setq from-string (read-string "String to search (RET to stop): "))))
      (setq ret-alist
            (cons (cons from-string (read-string (format "Replace %s with: " from-string)))
                  ret-alist)))
    ret-alist))

;;----------------------------------------------------------------------
;; Replace text in whole buffer 
;;----------------------------------------------------------------------

;; The suggested OLD text is either the current region, or the next
;;  word (as mark-word would select it). The suggested text for the
;;  replacement is the same as the OLD text.

;;;###autoload
(defun replace-in-buffer ()
  (interactive)
  (save-excursion
    (if (equal mark-active nil) (mark-word))
    (setq curr-word (buffer-substring-no-properties (mark) (point)))
    (setq old-string (read-string "OLD string:\n" curr-word))
    (setq new-string (read-string "NEW string:\n" old-string))
    (query-replace old-string new-string nil (point-min) (point-max))
    )
  )
;;----------------------------------------------------------------------

;;;###autoload
(defun store-register-dwim (arg register)
  "Store what I mean in a register.
With an active region, store or append (with \\[universal-argument]) the
contents, optionally deleting the region (with a negative
argument). With a numeric prefix, store the number. With \\[universal-argument]
store the frame configuration. Otherwise, store the point."
  (interactive
   (list current-prefix-arg
         (register-read-with-preview "Store in register: ")))
  (cond
   ((use-region-p)
    (let ((begin (region-beginning))
          (end (region-end))
          (delete-flag (or (equal arg '-)  (equal arg '(-4)))))
      (if (consp arg)
          (append-to-register register begin end delete-flag)
        (copy-to-register register begin end delete-flag t))))
   ((numberp arg) (number-to-register arg register))
   (t (point-to-register register arg))))

;;;###autoload
(defun use-register-dwim (register &optional arg)
  "Do what I mean with a register.
For a window configuration, restore it. For a number or text, insert it.
For a location, jump to it."
  (interactive
   (list (register-read-with-preview "Use register: ")
         current-prefix-arg))
  (condition-case nil
      (jump-to-register register arg)
    (user-error (insert-register register arg))))

(provide 'better-editing)
