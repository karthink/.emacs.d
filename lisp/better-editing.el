;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------
;;;; BETTER EDITING
;;----------------------------------------------------------------------

;; (flyspell-mode)
(setq-default fill-column 80)
(setq vc-follow-symlinks t)
(setq scroll-error-top-bottom t)
(setq tab-always-indent 'complete)
(setq kill-whole-line t)
(setq select-enable-clipboard t)
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
  :disabled
  :bind ("C-x SPC" . cua-rectangle-mark-mode))

(use-package repeat
  :if (version< "28.0" emacs-version)
  :bind ("H-z" . repeat)
  :hook (after-init . my/repeat-mode)
  :config
  (setq repeat-keep-prefix t
        repeat-echo-function #'repeat-echo-mode-line
        repeat-echo-mode-line-string
        (propertize "[R]" 'face 'mode-line-emphasis))
  (defun my/repeat-mode ()
    (let ((inhibit-message t)
          (message-log-max nil))
      (repeat-mode))))

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t))

(use-package imenu
  :hook (imenu-after-jump . my/imenu-show-entry)
  :bind ("M-s i" . imenu)
  :config
  (setq imenu-use-markers t
        imenu-auto-rescan t
        imenu-max-item-length 100
        imenu-use-popup-menu nil
        imenu-eager-completion-buffer t
        imenu-space-replacement " "
        imenu-level-separator "/")

  (declare-function org-at-heading-p "org")
  (declare-function org-show-entry "org")
  (declare-function org-reveal "org")
  (declare-function outline-show-entry "outline")

  (defun my/imenu-show-entry ()
    "Reveal index at point after successful `imenu' execution.
To be used with `imenu-after-jump-hook' or equivalent."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry)
      (org-reveal t))
     ((bound-and-true-p prot-outline-minor-mode)
      (outline-show-entry)))))

(use-package replace
  :defer
  :bind ( :map occur-mode-map
          ("C-x C-q" . occur-edit-mode)
          :map query-replace-map
          ("p" . 'backup)))

(use-package re-builder
  :bind (("C-M-5" . re-builder)
         ("C-M-%" . re-builder)
         :map reb-mode-map
         ("C-c C-k" . reb-quit)
         ("RET" . reb-replace-regexp)
         :map reb-lisp-mode-map
         ("RET" . reb-replace-regexp))
  :config
  ;; reb-fix modifies reb-update-overlays to restrict matches to region
  (use-package reb-fix)
  (defvar my/re-builder-positions nil
    "Store point and region bounds before calling re-builder")
  (advice-add 're-builder
              :before
              (defun my/re-builder-save-state (&rest _)
                "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
                (setq my/re-builder-positions
                      (cons (point)
                            (when (region-active-p)
                              (list (region-beginning)
                                    (region-end)))))))
  (defun reb-replace-regexp (&optional delimited)
    "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
    (interactive "P")
    (reb-update-regexp)
    (let* ((re (reb-target-value 'reb-regexp))
           (replacement (query-replace-read-to
                         re
                         (concat "Query replace"
                                 (if current-prefix-arg
                                     (if (eq current-prefix-arg '-) " backward" " word")
                                   "")
                                 " regexp"
                                 (if (with-selected-window reb-target-window
                                       (region-active-p)) " in region" ""))
                         t))
           (pnt (car my/re-builder-positions))
           (beg (cadr my/re-builder-positions))
           (end (caddr my/re-builder-positions)))
      (with-selected-window reb-target-window
        (goto-char pnt)
        (setq my/re-builder-positions nil)
        (reb-quit)
        (query-replace-regexp re replacement delimited beg end)))))

(use-package emacs
  :no-require t
  :bind (("C-c SPC" . my/easy-page))
  :config
  (defvar-keymap my-pager-map
    :doc "Keymap with paging commands"
    "SPC" 'scroll-up-command
    "C-l" 'recenter-top-bottom
    "C-M-v" 'scroll-other-window
    "C-M-S-v" 'scroll-other-window-down
    "d" (lambda ()
          (interactive)
          (pixel-scroll-precision-interpolate
           (- (floor (window-text-height nil t) 2))
           nil 1))

    "u" (lambda ()
          (interactive)
          (pixel-scroll-precision-interpolate
           (floor (window-text-height nil t) 2)
           nil 1))
    "M-o" (if (fboundp 'switchy-window-minor-mode)
              'switchy-window 'my/other-window)
    "S-SPC" 'scroll-down-command)
  (let ((scrolling (propertize  "SCRL" 'face '(:inherit highlight)))
        ml-buffer)
    (defalias 'my/easy-page
      (lambda ()
        (interactive)
        (when (eq (window-buffer (selected-window))
                  (current-buffer))
          (setq ml-buffer (current-buffer))
          (add-to-list 'mode-line-format scrolling)
          (set-transient-map
           my-pager-map t
           (lambda () (with-current-buffer ml-buffer
                   (setq mode-line-format
                         (delete scrolling mode-line-format))))))))))

;;;----------------------------------------------------------------
;; ** DABBREV
;;;----------------------------------------------------------------

(use-package dabbrev
  :commands (dabbrev-expand dabbrev-completion)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t))

;;;----------------------------------------------------------------
;; ** HIPPIE-EXPAND
;;;----------------------------------------------------------------
;; Supercharge the way hippie-expand behaves, expand as little as
;; possible
(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :preface
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-whole-kill
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))
  :config
  ;; From https://code.tecosaur.net/tec/emacs-config/commit/1e6e64991e
  (defun my/he-subst-suffix-overlap (ins rem)
    "The longest suffix of the string INS that is a prefix of REM.
This is intended to be used when INS is a newly inserted string and REM is the
remainder of the line, to allow for handling potentially duplicated content."
    (let ((len (min (length ins) (length rem))))
      (while (and (> len 0)
                  (not (eq t (compare-strings ins (- len) nil rem 0 len))))
        (setq len (1- len)))
      len))

  (define-advice he-substitute-string (:filter-args (args) suffix-strip)
    "Filter ARG list for `he-substitute-string', truncating duplicated suffix.
ARGS is the raw argument list (STRING &optional TRANS-CASE)."
    (pcase-let* ((`(,ins &optional ,trans-case) args)
                 (rem (save-excursion
                        (goto-char (marker-position he-string-end))
                        (buffer-substring-no-properties
                         (point) (line-end-position))))
                 (ov (my/he-subst-suffix-overlap ins rem)))
      (when (>= ov 0)
        (setq ins (substring ins 0 (- (length ins) ov))))
      (list ins trans-case))))

;;----------------------------------------------------------------
;; ** GREP
;;----------------------------------------------------------------
(use-package grep
  :hook ((grep-mode . toggle-truncate-lines)))

;;----------------------------------------------------------------
;; * COMPILATION
;;----------------------------------------------------------------
(use-package compile
  :defer t
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind (("M-#" . compile)
         :map mode-specific-map
         ("x" . compile))
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error))

;;----------------------------------------------------------------------
;; KEYBINDINGS
;;----------------------------------------------------------------------

;; Hyper bindings for emacs. Why use a pinky when you can use a thumb?
(use-package emacs
  :when IS-LINUX
  :bind-keymap (("H-r" . ctl-x-r-map)
                ("H-4" . ctl-x-4-map)
                ("H-5" . ctl-x-5-map))
  :bind (("M-ESC ESC" . nil)
         ("H-x" . H-x)
         ("H-c" . H-c)
         ("H-z" . repeat)
         ("H-=" . text-scale-increase)
         ("H--" . text-scale-decrease)
         ("H-M--" . shrink-window-if-larger-than-buffer)
         ("H-h" . mark-whole-buffer)
         ("H-M-x" . eval-defun)
         ("C-H-x" . eval-defun)
         ("H-s" . isearch-forward)
         ("H-r" . isearch-backward)
         ("C-M-4" . other-window-prefix)
         ("C-M-1" . same-window-prefix)
         ("C-M-5" . other-frame-prefix)
         ("C-M-6" . other-tab-prefix)
         ("H-v"   . scroll-other-window)
         ("H-V" . scroll-other-window-down)
         ("H-+" . balance-windows-area)
         ("H-0" . my/delete-window-or-delete-frame)
         ("H-1" . delete-other-windows)
         ("H-2" . my/split-window-below)
         ("H-3" . my/split-window-right)
         ("H-k" . my/kill-current-buffer)
         ("H-q" . my/kill-buffer-and-window)
         :map isearch-mode-map
         ("H-s" . isearch-repeat-forward)
         ("H-r" . isearch-repeat-backward)
         ;; :map ctl-x-map
         ;; ("H-s" . save-buffer)
         ;; ("H-e" . eval-last-sexp)
         ;; ("H-c" . save-buffers-kill-terminal)
         ;; ("H-f" . find-file)
         ;; ("H-q" . read-only-mode)
         )
  :config
  (defun hyperify-prefix-key (key)
    (let* ((convert-function
            (lambda (event)
              (vector
               (if (memq 'hyper (event-modifiers event))
                   (event-apply-modifier (event-basic-type event) 'control 26 "C-")
                 event))))
           (first-key-sequence (vconcat key (funcall convert-function (read-event))))
           (command (or (let ((minor-cmd (lookup-key (current-minor-mode-maps) first-key-sequence)))
                          (unless (equal minor-cmd 1) minor-cmd))
                        (let ((local-cmd (lookup-key (current-local-map) first-key-sequence)))
                          (unless (equal local-cmd 1) local-cmd))
                        (lookup-key (current-global-map) first-key-sequence))))
      (catch 'finished
        (while t
          (cond ((commandp command)
                 (call-interactively command)
                 (throw 'finished t))
                ((keymapp command)
                 (setq command (lookup-key command (funcall convert-function (read-event)))))
                (t (error "ABORT")))))))

  (defun H-x ()
    (interactive)
    (hyperify-prefix-key [24]))

  (defun H-c ()
    (interactive)
    (hyperify-prefix-key [3])))

(use-package emacs
  :when IS-MAC
  :bind-keymap (("s-t" . tab-prefix-map)
                ("s-4" . ctl-x-4-map)
                ("s-5" . ctl-x-5-map))
  :bind (("C-M-4" . other-window-prefix)
         ("C-M-1" . same-window-prefix)
         ("C-M-5" . other-frame-prefix)
         ("C-M-6" . other-tab-prefix)
         ("s-0" . my/delete-window-or-delete-frame)
         ("s-1" . delete-other-windows)
         ("s-2" . my/split-window-below)
         ("s-3" . my/split-window-right)
         ("s-k" . my/kill-current-buffer)
         ("s-q" . my/kill-buffer-and-window)))

(global-set-key (kbd "M-r") ctl-x-r-map)
(define-key ctl-x-r-map "a" 'append-to-register)

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
(global-set-key (kbd "C-S-y") 'copy-from-above-command)

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
(global-set-key (kbd "M-W") 'copy-region-as-kill)

;; Easy keys to traverse paragraphs
(global-set-key (kbd "M-]") 'forward-paragraph)
;; tty emacs has mouse trouble with this binding:
(when (or window-system (daemonp))
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

;; Adding text to and from elsewhere
(global-unset-key (kbd "C-x i"))
(global-set-key (kbd "C-x i b") 'insert-buffer)
(global-set-key (kbd "C-x i a") 'append-to-buffer)
(global-set-key (kbd "C-x i f") 'insert-file)
(global-set-key (kbd "C-x i w") 'write-region)

;; Easy align on regex
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Use M-j to join lines. C-j splits them, so it's all good.
(global-set-key (kbd "M-j") 'join-line)

;; Easily comment and uncomment region
(global-set-key (kbd "C-c ;") 'comment-region)

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;; Unfill-region
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Register commands
(define-key global-map (kbd "M-m") 'store-register-dwim)
(define-key global-map (kbd "M-'") 'use-register-dwim)

;; Better mark behavior in tmm
(define-key global-map [remap exchange-point-and-mark]
            'my/exchange-point-and-mark)
(define-key global-map (kbd "C-@")
            (lambda () (interactive)
              (if (display-graphic-p)
                  (activate-mark) (call-interactively #'set-mark-command))))

;;----------------------------------------------------------------------

;; FUNCTIONS
;;----------------------------------------------------------------------

;;;###autoload
(defun my/exchange-point-and-mark (&optional arg)
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive "P")
  (exchange-point-and-mark (if (use-region-p) arg (not arg))))

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
    (if (looking-back "^[ \t]+" (line-beginning-position))
        (delete-region (line-beginning-position) (point))
      (backward-kill-word arg))))

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

(setq set-mark-command-repeat-pop t
      mark-even-if-inactive t)

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
