(require 'embark)

;;; Embark-based completion and selection
;; Customizations to use embark's live-occur as a completion system for Emacs.
;;  Most of this code is copied from or inspired by the work of Protesilaos
;;  Stavrou: https://protesilaos.com/dotemacs/

(defvar elm-always-show-list nil
  "List of commands for which the Embark live completions should
  always pop up immediately.")

(defvar elm-never-show-list nil
  "List of commands for which the Embark live completions should
  never pop up.

It can still be manually shown.")

(defun elm--minimum-input-p ()
  "Test if there are enough characters in the minibuffer.

This is to pop up the Embark live-collect buffer."
  (>= (length
       (buffer-substring-no-properties
        (minibuffer-prompt-end)
        (point-max)))
      3))

(defun elm--wait-for-input (_beg _end _len)
  (when (and (minibufferp)
             (elm--minimum-input-p))
    (remove-hook 'after-change-functions 'elm--wait-for-input t)
    ;; (embark-collect-completions-after-delay)
    (embark-collect-completions-after-delay)))

(defun elm-collect-completions-after-input ()
  "Start `embark-collect-completions' after some minibuffer input.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup soon after you type something in the
minibuffer."
  ;; (setq elm--initial-input-pos (point-max))
  (when minibuffer-completion-table
    (if (member this-command elm-always-show-list)
        (embark-collect-completions)
      (add-hook 'after-change-functions #'elm--wait-for-input nil t))))

(defun elm--minibuffer-local-completion-map ()
  "Hook to `minibuffer-setup-hook'."
  (use-local-map
   (make-composed-keymap elm-minibuffer-local-completion-map (current-local-map))))

(defun elm--embark-collect-mode-map ()
  "Hook to `embark-collect-mode-hook'."
  (use-local-map
   (make-composed-keymap elm-live-collect-map (current-local-map))))

(defun elm-directory--completing-file-p ()
  "Return non-nil when completing file names."
  (eq 'file
      (completion-metadata-get
       (completion-metadata
        (buffer-substring (minibuffer-prompt-end)
                          (max (minibuffer-prompt-end) (point)))
        minibuffer-completion-table
        minibuffer-completion-predicate)
       'category)))

;; Ido-like directory handling
(defun elm-directory-up ()
  "Delete directory before point."
  (interactive)
  (when (and (> (point) (minibuffer-prompt-end))
             (eq (char-before) ?/)
             (elm-directory--completing-file-p))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (delete-region (1+ (point)) (point-max))
        t))))

(defun elm-directory-delete-char ()
  "Delete directory or char before point."
  (interactive)
  (unless (elm-directory-up)
    (call-interactively #'backward-delete-char)))

(defun elm-directory-delete-word ()
  "Delete directory or word before point."
  (interactive)
  (unless (elm-directory-up)
    (let ((pt (point)))
      (forward-word -1)
      (delete-region pt (point)))))

(defun elm-directory-enter ()
  "Enter directory or exit completion with current candidate."
  (interactive)
  (let ((cand (cdr (embark-target-top-minibuffer-completion))))
    (if (and (elm-directory--completing-file-p)
             (string-suffix-p "/" cand))
        (progn (delete-minibuffer-contents)
               (insert (substring-no-properties cand)))
      (minibuffer-force-complete-and-exit))))

(setf  (alist-get "^\\*Embark Collect Completions\\*" display-buffer-alist nil nil 'equal)
       '((display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height .  (lambda (win) (fit-window-to-buffer
                                     win
                                     (floor (frame-height) 3))))
         (side . bottom)
         (slot . 0)
          (window-parameters . ((no-other-window . t)))))

(defun elm-keyboard-quit ()
  "If in an Embark live collect/completions buffer, run
`abort-recursive-edit'. Otherwise run `keyboard-quit'."
  (interactive)
  (if (elm--live-completions-p)
      (if (use-region-p)
          (keyboard-quit)
        (kill-buffer)
        (abort-recursive-edit))
    (keyboard-quit)))

(defun elm-clear-live-buffers ()
  "Remove lingering Embark Collect Completions' buffers.
Add this to `minibuffer-exit-hook'."
  (let* ((buffers (buffer-list))
         (case-fold-search nil)
         (completions
          (cl-remove-if-not (lambda (buf)
                              (string-match "\\*Embark.*Completions.*"
                                            (format "%s" buf)))
                            buffers)))
    (mapc #'kill-buffer completions)))

;; (setq embark-collect-initial-view-alist
;;       '((file           . list)
;;         (project-file   . list)
;;         (virtual-buffer . list)
;;         (buffer         . list)
;;         (consult-multi  . list)
;;         (consult-location . list)
;;         (consult-compile-error . list)
;;         (consult-flymake-error . list)
;;         (symbol         . grid)
;;         (command        . grid)
;;         (imenu          . grid)
;;         (line           . list)
;;         (xref-location  . list)
;;         (kill-ring      . zebra)
;;         (face           . list)
;;         (t              . grid)))

(defun elm--collect-fit-window (&rest _)
  "Fit Embark's live occur window to its buffer.
To be added to `embark-collect-post-revert-hook'."
  (when (derived-mode-p 'embark-collect-mode)
    (fit-window-to-buffer (get-buffer-window)
                          (floor (frame-height) 3) 1)))

(defun elm--live-buffer-p ()
  "Determine presence of a linked live occur buffer."
  (let ((buf embark-collect-linked-buffer))
    (when buf
      (window-live-p (get-buffer-window buf)))))

;; (defun elm--live-buffer-p ()
;; "Determine presence of a linked live occur buffer."
;; (let* ((buf-link embark-collect-linked-buffer)
;;        (buf-name (buffer-name buf-link)))
;;   (when buf-name
;;     (string-match-p elm-collect-window-regexp buf-name))))

;;   (defvar elm-collect-window-regexp
;;   "\\*Embark Collect \\(Live\\|Completions\\).*"
;;   "Regexp to match window names with Embark collections.")

(defun elm--live-completions-p ()
  "Determine whether current collection is for live completions."
  (and (derived-mode-p 'embark-collect-mode)
       (eq embark-collect--kind :completions)))

(defvar elm-live-collect-hook nil
  "Hook that runs after `elm-live-collect-toggle'.")

(defun elm-completions-toggle ()
  "Toggle `embark-collect-completions'."
  (interactive)
  (if (elm--live-buffer-p)
      (kill-buffer embark-collect-linked-buffer)
    (embark-collect-completions)))

;; TODO
(defun elm-collect-toggle-view ()
  (when (eq embark-collect-view 'list)
    (hl-line-mode -1)
    (embark-collect--toggle 'embark-collect-view 'list 'grid)
    )
  )
;; (defun elm-live-occur-toggle ()

;;   "Toggle `embark-live-occur', call `elm-live-occur-hook'."
;;   (interactive)
;;   (if (elm--live-buffer-p)
;;       (kill-buffer embark-occur-linked-buffer)
;;     (embark-live-occur))
;;   (run-hooks 'elm-live-occur-hook))

(setq embark-collect-live-update-delay 0.3)
(setq embark-collect-live-initial-delay 0.3)

(setq embark-candidate-collectors
      (delete 'embark-minibuffer-candidates embark-candidate-collectors))
(add-to-list 'embark-candidate-collectors 'embark-sorted-minibuffer-candidates)

(defun embark-top-sorted-minibuffer-candidates ()
  "Return a sorted list of the top 30 current minibuffer completion candidates.
This using the same sort order that `icomplete' and
`minibuffer-force-complete' use. The intended usage is that you
replace `embark-minibuffer-candidates' with this function in the
list `embark-candidate-collectors'."
  (when (minibufferp)
    (cons
     (completion-metadata-get (embark--metadata) 'category)
     (let ((cacs (completion-all-sorted-completions)))
       (nconc (cl-copy-list (if (listp cacs) (seq-take cacs 30))) nil)))))

;; (defun elm-minibuffer-candidates ()
;;   (seq-take (embark-minibuffer-candidates) 40))

(defun elm-minibuffer-focus-mini ()
  "Focus the active minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))  

;; Move from minibuffer to embark-collect and back
(defun elm-next-line-or-mini (&optional arg)
  "Move to the next line or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction, then it switches to
the minibuffer."
  (interactive "p")
  (if (or (eobp) (eq (point-max)
                     (1+ (line-end-position))
                     ;; (save-excursion (forward-line 1) (point))
                     ))
      (elm-minibuffer-focus-mini)    ; from `setup-minibuffer.el'
    (forward-line (or arg 1)))
  (setq this-command 'next-line))

(defun elm-previous-line-or-mini (&optional arg)
  "Move to the previous line or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction, then it switches to
the minibuffer."
  (interactive "p")
  (let ((num (if arg (- arg)))) ; from `elm-common.el'
    (if (bobp)
        (elm-minibuffer-focus-mini)    ; from `elm-minibuffer.el'
      (forward-line (or num -1)))))

(defun elm--switch-to-completions ()
  "Subroutine for switching to the Embark completions buffer."
  (unless (elm--live-buffer-p)
    (elm-completions-toggle))
  (pop-to-buffer embark-collect-linked-buffer))

(defun elm-switch-to-completions-top ()
  "Switch to the top of Embark's completions buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (elm--switch-to-completions)
  (goto-char (point-min)))

(defun elm-switch-to-completions-bottom ()
  "Switch to the bottom of Embark's completions buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (elm--switch-to-completions)
  (goto-char (point-max))
  (forward-line -1)
  (goto-char (point-at-bol)))

;; Better embark action movements
(defun elm-preview (arg)
  (unless (bound-and-true-p consult--preview-function) ;; Disable preview for Consult commands
    (save-selected-window
      (forward-line arg)
      (embark-dwim 4))))

(defun elm--completions-act (arg)
  "Move ARG lines and perform `embark-default-action'."
  (forward-line arg)
  (embark--act #'embark-default-action (cdr (embark--target))))

(defun elm-completions-act-next (&optional arg)
  "Run default action on next or ARGth Embark target.
This calls `elm--completions-act' and is meant to be
assigned to a key in `embark-collect-mode-map'."
  (interactive "p")
  (elm-preview (or arg 1)))

(defun elm-completions-act-previous (&optional arg)
  "Run default action on previous or ARGth Embark target.
This calls `elm--completions-act' and is meant to be
assigned to a key in `embark-collect-mode-map'."
  (interactive "p")
  (let ((num (if arg (- arg))))
    (elm-preview (or num -1))))

(defun elm-completions-act-current ()
  "Run default action on Embark target without exiting.
Meant to be assigned to a key in `embark-collect-mode-map'."
  (interactive)
  (embark--act #'embark-default-action (cdr (embark--target))))

;; Highlighting selections in embark-collect buffers
(defvar-local elm-collect--overlay nil
  "Text overlay for embark-collect buffers.")

(defun elm--embark-collect-live-setup ()
  "Remove mode-line from live embark-collect buffers and set up
highlighting."
  (when (elm--live-completions-p)
    (setq-local mode-line-format nil))
  (setq elm-collect--overlay (make-overlay 1 1))
  (overlay-put elm-collect--overlay 'face 'highlight)
  (add-hook 'post-command-hook 'elm-collect--live-overlay-update nil t))

(defun elm-collect--live-overlay-update ()
  "Update the overlay in the embark-collect buffer."
  (pcase embark-collect-view
    ('list (hl-line-mode 1))
    ('grid (when (and (overlayp elm-collect--overlay)
                      (get-text-property (point) 'mouse-face))
             (hl-line-mode 0)
             (let ((beg (previous-single-property-change
                         (if (eobp) (point-max) (1+ (point)))
                         'mouse-face nil (point-min)))
                   (end (next-single-property-change (point) 'mouse-face nil (point-max))))
               (move-overlay elm-collect--overlay beg end))))))

;; (defun elm-insert () (interactive)
;;        (when-let ((cand (cdr (embark-target-top-minibuffer-completion))))
;;          (delete-minibuffer-contents)
;;          (insert (substring-no-properties cand))))
(defun elm-insert () (interactive)
       (let ((completion-cycle-threshold t))
         (minibuffer-complete)))

(defvar elm-minibuffer-local-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'elm-switch-to-completions-top)
    (define-key map (kbd "C-p") #'elm-switch-to-completions-bottom)
    (define-key map (kbd "C-l") #'elm-completions-toggle)
    (define-key map (kbd "RET") #'elm-directory-enter)
    (define-key map (kbd "C-j")  (lambda () (interactive)
	        	           (if minibuffer--require-match
	        	               (minibuffer-complete-and-exit)
	        	             (exit-minibuffer))))
    (define-key map (kbd "DEL") #'elm-directory-delete-char)
    (define-key map (kbd "M-DEL") #'elm-directory-delete-word)
    (define-key map (kbd "C-w") #'elm-directory-delete-word)
    (define-key map (kbd "C-M-l") #'embark-export)
    (define-key map (kbd ">") #'embark-become)
    (define-key map (kbd "M-q") #'embark-collect-toggle-view)
    (define-key map (kbd "C-c C-f") #'consult-preview-at-point-mode)
    (define-key map (kbd "<tab>") #'embark-act-with-completing-read)
    (define-key map (kbd "M-i") #'elm-insert)
    map))

(defvar elm-live-collect-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'elm-next-line-or-mini)
    (define-key map (kbd "C-p") #'elm-previous-line-or-mini)
    (define-key map (kbd "C-g") #'elm-keyboard-quit)
    (define-key map (kbd "C-M-l") #'embark-export)
    (define-key map (kbd ">") #'embark-become)
    (define-key map (kbd "M-q") #'embark-collect-toggle-view)
    (define-key map (kbd "C-c C-f") #'consult-preview-at-point-mode)
    map))

;;;###autoload
(define-minor-mode embark-live-mode
  "Embark live based incremental completion system."
  :global t
  :group 'elm
  :lighter ""
  (if embark-live-mode
      (progn
        (add-hook 'minibuffer-setup-hook 'elm-collect-completions-after-input)
        (add-hook 'minibuffer-setup-hook 'elm--minibuffer-local-completion-map)
        (add-hook 'minibuffer-exit-hook 'elm-clear-live-buffers)
        ;; (add-hook 'embark-post-action-hook 'embark-collect--update-linked)
        (add-hook 'embark-collect-post-revert-hook 'elm--collect-fit-window)
        (add-hook 'embark-collect-mode-hook 'elm--embark-collect-mode-map)
        (add-hook 'embark-collect-mode-hook 'elm--embark-collect-live-setup)
        (and (fboundp 'consult-preview-at-point-mode)
             (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))
        (setq resize-mini-windows t))
    (remove-hook 'minibuffer-setup-hook 'elm-collect-completions-after-input)
    (remove-hook 'minibuffer-setup-hook 'elm--minibuffer-local-completion-map)
    (remove-hook 'minibuffer-exit-hook 'elm-clear-live-buffers)
    ;; (remove-hook 'embark-post-action-hook 'embark-collect--update-linked) ;
    (remove-hook 'embark-collect-post-revert-hook 'elm--collect-fit-window)
    (remove-hook 'embark-collect-mode-hook 'elm--embark-collect-live-setup)
    (remove-hook 'embark-collect-mode-hook 'elm--embark-collect-mode-map)
    (remove-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)))

(provide 'elm)
