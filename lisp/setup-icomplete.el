;; -*- lexical-binding: t -*-
;; Icomplete
(use-package icomplete
  ;; :demand
  :disabled
  :after minibuffer
  :config 
  (setq icomplete-delay-completions-threshold 50
        icomplete-max-delay-chars 2
        icomplete-compute-delay 0.2
        ;; icomplete-separator " · "
        ;; icomplete-separator " ┆ "
        ;; icomplete-in-buffer t
        icomplete-separator (propertize " . " 'face 'shadow)
        icomplete-show-matches-on-no-input nil
        icomplete-hide-common-prefix nil
        icomplete-with-completion-tables t
        icomplete-prospects-height 1
        icomplete-tidy-shadowed-file-names t)

  ;; (define-key icomplete-minibuffer-map (kbd "TAB") 'icomplete-force-complete)
  ;; (define-key icomplete-minibuffer-map (kbd "TAB") 'minibuffer-complete)
  :bind (:map icomplete-minibuffer-map
             ( "C-n" . icomplete-forward-completions)
             ( "C-p" . icomplete-backward-completions)
             ( "RET" . icomplete-fido-ret)
             ( "C-k" . icomplete-fido-kill)
             ( "C-j" . (lambda () (interactive)
	        	    (if minibuffer--require-match
	        		(minibuffer-complete-and-exit)
	        	      (exit-minibuffer))))
             ( "DEL" . icomplete-fido-backward-updir)
             ;; ( "C-," . my/icomplete-toggle-completion-styles)
             )
  :config
  (defun icomplete-fido-kill ()
    "Kill line or current completion, like `ido-mode'.
If killing to the end of line make sense, call `kill-line',
otherwise kill the currently selected completion candidate.
Exactly what killing entails is dependent on the things being
completed.  If completing files, it means delete the file.  If
completing buffers it means kill the buffer.  Both actions
require user confirmation."
    (interactive)
    (let ((end (icomplete--field-end)))
      (if (< (point) end)
          (call-interactively 'kill-line)
        (let* ((all (completion-all-sorted-completions))
               (thing (car all))
               (action
                (pcase (icomplete--category)
                  (`buffer
                   (lambda ()
                     (kill-buffer thing)))
                  (`virtual-buffer
                   (lambda ()
                     (let* ((type (elt thing 0))
                            (instance (substring thing 1)))
                       (cond 
                        ((equal (- type consult--special-char) ?b)
                         (kill-buffer (get-buffer instance)))
                        ((equal (- type consult--special-char) ?f)
                         (let* ((dir (file-name-directory (icomplete--field-string)))
                                (path (expand-file-name thing dir)))
                           (when (yes-or-no-p (concat "Delete file " path "? "))
                             (delete-file path) t)))
                        ((or (equal (- type consult--special-char) ?m)
                             (equal (- type consult--special-char) ?v))
                         (when (yes-or-no-p (concat "Delete bookmark" instance "? "))
                           (bookmark-delete instance)
                           (bookmark-save)))))))
                  (`file
                   (lambda ()
                     (let* ((dir (file-name-directory (icomplete--field-string)))
                            (path (expand-file-name thing dir)))
                       (when (yes-or-no-p (concat "Delete file " path "? "))
                         (delete-file path) t)))))))
          (when (let (;; Allow `yes-or-no-p' to work and don't let it
                      ;; `icomplete-exhibit' anything.
                      (enable-recursive-minibuffers t)
                      (icomplete-mode nil))
                  (funcall action))
            (completion--cache-all-sorted-completions
             (icomplete--field-beg)
             (icomplete--field-end)
             (cdr all)))
          (message nil))))))

;;; Icomplete-vertical
(use-package icomplete-vertical
  :disabled
  :demand
  :after (minibuffer icomplete)
  :config
  (setq icomplete-vertical-prospects-height (/ (frame-height) 5))
  (icomplete-vertical-mode -1)
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle)))

;; Icomplete vertical mini
(use-package icomplete-vertical-mini
  :disabled
  :config

;; (define-key icomplete-minibuffer-map (kbd "?")
;;   (lambda () (interactive)
;;     (minibuffer-completion-help)
;;     (switch-to-completions)))
;;(fido-mode -1)
  
  ;; (define-key completion-list-mode-map (kbd "M-o") icomplete-menu-map)

  (defun icomplete-vertical-minibuffer-setup ()
    "Setup minibuffer for a vertical icomplete session.
Meant to be added to `icomplete-minibuffer-setup-hook'."
    (visual-line-mode -1) ; just in case
    (setq truncate-lines t)
    (when (boundp 'auto-hscroll-mode)
      (setq-local auto-hscroll-mode 'current-line))
    (enlarge-window (- icomplete-prospects-height (1- (window-height)))))

  (defun icomplete-vertical-minibuffer-teardown ()
    "Undo minibuffer setup for a vertical icomplete session.
This is used when toggling Icomplete Vertical mode while the
minibuffer is in use."
    (setq truncate-lines nil)
    (enlarge-window (- (1- (window-height)))))

  (defun my/icomplete-toggle-vertical ()
    "Toggle vertical view for `icomplete'.

This is intended as a temporary adjustment of the layout,
possibly to read a list of long names.  It is for this reason
that `my/icomplete-restore-horizontal' exists and is called by
the `minibuffer-exit-hook'."
    (interactive)
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (if (not (eq last-command 'my/icomplete-vertical-mode))
	  ;; (not (string= icomplete-separator "\n "))
          (progn
	    (setq this-command 'my/icomplete-vertical-mode)
            (setq-local icomplete-prospects-height 10)
            (setq-local icomplete-separator "\n ")
	    (icomplete-vertical-minibuffer-setup)
	    )
        (icomplete-vertical-minibuffer-teardown)
        (setq icomplete-prospects-height 2)
        (setq icomplete-separator " ┆ "))))


  ;; (defun my/icomplete-toggle-vertical ()
  ;;     "Toggle vertical view for `icomplete'.

  ;; This is intended as a temporary adjustment of the layout,
  ;; possibly to read a list of long names.  It is for this reason
  ;; that `my/icomplete-restore-horizontal' exists and is called by
  ;; the `minibuffer-exit-hook'.

  ;; NOTE: there still needs to be a way to show the minibuffer input
  ;; on its own line while also displaying the list of candidates."
  ;;     (interactive)
  ;;     (when (and (minibufferp)
  ;;                (bound-and-true-p icomplete-mode))
  ;;       (if (not (string= icomplete-separator "\n "))
  ;;           (progn
  ;;             (setq-local icomplete-prospects-height 10)
  ;;             (setq-local icomplete-separator "\n "))
  ;;         (setq icomplete-prospects-height 1)
  ;;         (setq icomplete-separator " ┆ "))))

  ;; (defun my/icomplete-toggle-flex ()
  ;;   "Toggle between flex and partial-completion (regexp)."
  ;;   (interactive)
  ;;   (when (and (minibufferp)
  ;;              (bound-and-true-p icomplete-mode))
  ;;     (if (not (eq (car completion-styles) 'flex))
  ;;         (progn
  ;;           (setq-local completion-styles '(flex initials substring partial-completion))
  ;;           (message "%s" (propertize "Prioritising FLEX" 'face 'highlight)))
  ;;       (setq-local completion-styles '(partial-completion substring initials flex))
  ;;       (message "%s" (propertize "Prioritising PREFIX REGEXP" 'face 'highlight)))))

  ;; (when (> emacs-major-version 26)
  ;;     (fido-mode 1)
  ;;     (define-key icomplete-fido-mode-map (kbd "C-j") 'icomplete-fido-exit))
  (defun icomplete-fido-backward-updir ()
    "Delete char before or go up directory, like `ido-mode'."
    (interactive)
    (if (and (eq (char-before) ?/)
             (eq (icomplete--category) 'file))
        (zap-up-to-char -1 ?/)
      (call-interactively 'backward-delete-char)))

  (defun icomplete-fido-ret ()
    "Exit minibuffer or enter directory, like `ido-mode'."
    (interactive)
    (let* ((dir (and (eq (icomplete--category) 'file)
                     (file-name-directory (icomplete--field-string))))
           (current (car completion-all-sorted-completions))
           (probe (and dir current
                       (expand-file-name (directory-file-name current) dir))))
      (cond ((and probe (file-directory-p probe) (not (string= current "./")))
             (icomplete-force-complete))
            (t
             (icomplete-force-complete-and-exit)))))

  (defun contrib/completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
 Use as a value for `completion-in-region-function'."
    (if (minibufferp)
        (completion--in-region start end collection predicate)
      (let* ((initial (buffer-substring-no-properties start end))
             (all (completion-all-completions initial collection predicate
                                              (length initial)))
             (completion (cond
                          ((atom all) nil)
                          ((and (consp all) (atom (cdr all))) (car all))
                          (t (let ((completion-in-region-function
                                    #'completion--in-region))
			       (completing-read
                                "Completion: " collection predicate t initial)
                               ;; (icomplete-vertical-do (:height (/ (window-height) 5))
                               ;;   (completing-read
                               ;;    "Completion: " collection predicate t initial))
			       )))))
        (if (null completion)
            (progn (message "No completion") nil)
          (delete-region start end)
          (insert completion)
          t))))

  (setq completion-in-region-function #'contrib/completing-read-in-region))

;; Icomplete-actions - disabled
(use-package icomplete-actions
  :disabled
  :config
  ;;========================================================
  ;; ICOMPLETE-ACTIONS in the minibuffer/completions buffer
  ;;========================================================
  (define-prefix-command 'icomplete-menu-map)
  ;; (define-key icomplete-minibuffer-map (kbd "M-o") icomplete-menu-map)
  (define-key icomplete-menu-map (kbd "w") 'my/icomplete-kill-or-insert-candidate)
  (define-key icomplete-menu-map (kbd "i") 
    (defun my/icomplete-insert-candidate ()
      (interactive)
      (my/icomplete-kill-or-insert-candidate '(4))))
  (define-key icomplete-menu-map (kbd "I") 
    (defun my/icomplete-insert-candidate-finalize ()
      (interactive)
      (my/icomplete-kill-or-insert-candidate '(16))))
  (define-key icomplete-menu-map (kbd "j") (defun my/icomplete-open-other-buffer ()
					     (interactive)
					     (let ((candidate (car completion-all-sorted-completions)))
					       (cond  
						((eq (icomplete--category) 'file) (find-file-other-window candidate) (other-window 1))
						((eq (icomplete--category) 'buffer) (display-buffer candidate) (other-window 1))
						(t nil)
						))))
  (define-key icomplete-menu-map (kbd "J") (lambda () (interactive)
					     (my/icomplete-open-other-buffer)
					     (top-level)))
  (define-key icomplete-menu-map (kbd "h") (defun my/icomplete-help ()
					     (interactive)
					     (let ((candidate (car completion-all-sorted-completions)))
					       (describe-symbol candidate) ;; (if (eq (icomplete--category) nil)
					       ;; ;
					       ;;  )
					       ))
    (define-key icomplete-menu-map (kbd "o") 'icomplete-fido-ret))



  ;; (defvar icomplete-menu-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (set-keymap-parent map icomplete-minibuffer-map)
  ;;     (define-key map (kbd "w") 'my/icomplete-kill-or-insert-candidate)
  ;;     (define-key map (kbd "i") 
  ;;       (defun my/icomplete-insert-candidate ()
  ;; 	(interactive)
  ;; 	(my/icomplete-kill-or-insert-candidate '(4))))
  ;;     (define-key map (kbd "I") 
  ;;       (defun my/icomplete-insert-candidate-finalize ()
  ;; 	(interactive)
  ;; 	(my/icomplete-kill-or-insert-candidate '(16))))
  ;;     (define-key map (kbd "o") 'icomplete-fido-ret)
  ;;     map)
  ;;   "Local keymap for actions on `icomplete' candidates" )

  (defun my/icomplete-kill-or-insert-candidate (&optional arg)
    "Place the matching candidate to the top of the `kill-ring'.
This will keep the minibuffer session active.

With \\[universal-argument] insert the candidate in the most
recently used buffer, while keeping focus on the minibuffer.

With \\[universal-argument] \\[universal-argument] insert the
candidate and immediately exit all recursive editing levels and
active minibuffers.

Bind this function in `icomplete-minibuffer-map'."
    (interactive "*P")
    (let* ((candidate (car completion-all-sorted-completions))
	   (dir (and (eq (icomplete--category) 'file)
                     (file-name-directory (icomplete--field-string))))
	   (candidate-full (concat dir candidate)))
      (when (and (minibufferp)
                 (bound-and-true-p icomplete-mode))
        (cond ((eq arg nil)
               (kill-new candidate-full))
              ((= (prefix-numeric-value arg) 4)
               (with-minibuffer-selected-window (insert candidate-full)))
              ((= (prefix-numeric-value arg) 16)
               (with-minibuffer-selected-window (insert candidate-full))
               (top-level)))))))

(provide 'setup-icomplete)
