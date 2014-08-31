;;----------------------------------------------------------------------
;;;; BETTER EDITING
;;----------------------------------------------------------------------

(flyspell-mode)


;; Collection of bindings and functions to make editing less painful on Emacs

;; /Enable/DISABLE cua-selection mode, for awesome rectangle selects
;; (Set cua-selection-mode for rectangle awesomeness)
;; (cua-selection-mode 1)
(transient-mark-mode t)
(delete-selection-mode 0)

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;; Turn off yucky visual line mode
(visual-line-mode nil)

;; Dynamic abbreviations does not search for case
(setq dabbrev-case-fold-search t)

;; Sentence end need not be "  " (double space)
(setq sentence-end-double-space nil)

;; Turn on show-paren-mode. Highlights matching parentheses
(show-paren-mode 1)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Turn off transient-mark-mode
(transient-mark-mode 1)

;;; Text mode and Auto Fill mode
; Set default Emacs mode to text-mode. In addition, turn on
; word-wrapping and either auto filling of text or longlines-mode,
; which auto fills in Emacs buffers but not when you copy the text.
(setq default-major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'text-mode-hook 'longlines-mode)
(add-hook 'text-mode-hook 'toggle-word-wrap)

;; Modify the way hippie-expand behaves, expand as little as possible
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs  try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-list try-expand-line try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;;----------------------------------------------------------------------
;; KEYBINDINGS
;;----------------------------------------------------------------------

;; Have C-a toggle between beginning of line and indentation
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

;; Transpose sentences
(global-set-key "\C-x\M-t" 'transpose-sentences)

;;; Autocompletion is power.
(global-set-key (kbd "C-;") 'complete-symbol) 

;;; Alternative to M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;; Unbind `C-x f'
(global-unset-key "\C-xf")

;;; Easy beginning/end of buffer (think < and >):
(global-set-key (kbd "C-x ,") 'beginning-of-buffer)
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x .") 'end-of-buffer)
(global-set-key (kbd "C-x l") 'end-of-buffer)

;;; Rebind `C-x C-b' for `buffer-menu'
(global-set-key "\C-x\C-b" 'buffer-menu)

;;; keybindings for Vi like o and O
;; autoindent open-*-lines
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-o") 'open-next-line)

;; Zapping up to char and Copying to char (intuitive versions)
(global-set-key "\C-z" 'zap-up-to-char)
(global-set-key "\M-z" 'zap-to-char-save)

;; goto matching parenthesis. 
;; Note that in the interest of Vi mimicry, this command has taken
;; over the prefix digit argument. The digit argument will still apply
;; provided (point) is not at a paren. :-/
(global-set-key (kbd "C-5") 'goto-match-paren)

;; Set keyboard shortcuts for parenthesizing sentences in the
;; following modes
(define-key text-mode-map "\M-(" 'insert-parentheses-sentence)

;; Key to duplicate line
(global-set-key (kbd "C-S-d") 'duplicate-line)

;; Key to kill-whole-line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; Key to kill word backwards OR kill region 
(global-set-key (kbd "C-w") 'backward-kill-word-or-region)

;; Easy keys to traverse paragraphs
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)

;; Re-search forward
(global-set-key (kbd "M-s") 'isearch-forward-regexp)

;; Re-search backward
(global-set-key (kbd "M-r") 'isearch-backward-regexp)

;; Easy regex replace
(defalias 'rr 'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(global-set-key (kbd "C-x / s") 'replace-regexp)
(global-set-key (kbd "C-x / q") 'query-replace-regexp)
(global-set-key (kbd "C-x / r") 'query-replace)
(global-set-key (kbd "C-x / d") 'delete-matching-lines)
(global-set-key (kbd "C-x / o") 'occur)
(global-set-key (kbd "C-x / /") 'isearch-forward-regexp)

;; Adding text to and from elsewhere
(global-unset-key (kbd "C-x i"))
(global-set-key (kbd "C-x i b") 'insert-buffer)
(global-set-key (kbd "C-x i a") 'append-to-buffer)
(global-set-key (kbd "C-x i f") 'insert-file)

;; Easy align on regex
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Use M-j to join lines. C-j splits them, so it's all good.
(global-set-key (kbd "M-j") 'join-line)

;; Search in adjacent windows without leaving this one:
(global-set-key (kbd "C-M-s") 'isearch-forward-other-buffer)
(global-set-key (kbd "C-M-r") 'isearch-backward-other-buffer)

;; Easily comment and uncomment region
(global-set-key (kbd "C-c ;") 'comment-region)

;; Hippie-expand for better autocompletion
(global-set-key (kbd "M-/") 'hippie-expand)

;; Keybindings for occur inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)


;;----------------------------------------------------------------------
;; FUNCTIONS
;;----------------------------------------------------------------------

(defun back-to-indentation-or-beginning () (interactive)
   (if (= (point) (progn (back-to-indentation) (point)))
       (beginning-of-line)))

;; (defun beginning-of-line-or-indentation ()
;;   "move to beginning of line, or indentation"
;;   (interactive)
;;   (if (bolp)
;;       (back-to-indentation)
;;     (beginning-of-line)))


(defun duplicate-line (&optional arg)
  "Duplicate it. With prefix ARG, duplicate ARG lines following the current one."
  (interactive "p")
  (next-line 
   (save-excursion 
     (let ((beg (line-beginning-position))
           (end (line-end-position)))
       (copy-region-as-kill beg end)
       (dotimes (num arg arg)
         (end-of-line) (newline)
         (yank))))))

(defun duplicate-line-many-once (&optional arg)
  "Duplicate it. With prefix ARG, duplicate ARG lines following the current one."
  (interactive "p")
  (save-excursion 
    (let ((beg (line-beginning-position))
          (end 
           (progn (forward-line (1- arg)) (line-end-position))))
      (copy-region-as-kill beg end)
      (end-of-line) (newline)
      (yank)))
  (next-line arg))

;;; behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.

 See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
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
(defun zap-to-char-save (arg char)
  "Zap to CHAR, (optional) ARG number of times, but save instead of kill."
  (interactive "p\ncSave to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (save-excursion (message (char-to-string char))
                  (copy-region-as-kill (point) (progn
                                                 (search-forward (char-to-string char) 
                                                                 nil nil arg)
                                                 (point)))))

;;; goto matching parenthesis, Vi style. The last statement is a bit conked; also look at the keybinding for this function above.
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise do nothing"
  (interactive "p")
  (cond
   ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
   ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
   (t (digit-argument (or arg 1)))))

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

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun isearch-occur ()
  "*Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun isearch-backward-other-buffer (prefix)
  "Function to isearch-forward in other-window."
  (interactive "P")
  (save-excursion
    (let ((next (if prefix 1 -1)))
    (other-window next)
    (isearch-backward)
    (other-window (- next))
    )))
  
(defun isearch-forward-other-buffer (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (save-excursion
    (let ((next (if prefix 1 -1)))
    (other-window next)
    (isearch-forward)
    (other-window (- next))
    )))

(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (buffer-file-name)
      (save-buffer args)))
(add-hook 'auto-save-hook 'save-buffer-if-visiting-file)

;;----------------------------------------------------------------------

(provide 'better-editing)