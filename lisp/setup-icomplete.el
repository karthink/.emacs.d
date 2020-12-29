(use-package orderless
  :after setup-minibuffer
  :ensure t
  :demand
  :config
  (setq orderless-component-separator #'split-string-and-unquote)
  (setq completion-styles '(orderless partial-completion))
  (setq orderless-matching-styles
        '(orderless-regexp
          orderless-strict-leading-initialism)
        orderless-style-dispatchers
        '(my/orderless-flex-dispatcher
          my/orderless-literal-dispatcher
          my/orderless-initialism-dispatcher))

  (defun my/orderless-flex-dispatcher (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun my/orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun my/orderless-initialism-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))
  
  :bind (:map minibuffer-local-completion-map
              ("SPC" . self-insert-command))
)

(use-package icomplete
  :demand
  :after minibuffer
  :config 
  (setq icomplete-delay-completions-threshold 50
        icomplete-max-delay-chars 2
        icomplete-compute-delay 0.2
        ;; icomplete-separator " · "
        ;; icomplete-separator " ┆ "
        ;; icomplete-in-buffer t
        icomplete-separator (propertize " . " 'face 'shadow)
        icomplete-show-matches-on-no-input t
        icomplete-hide-common-prefix nil
        icomplete-with-completion-tables t
        icomplete-prospects-height 1
        icomplete-tidy-shadowed-file-names t
        )

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
          (message nil)))))
  )

(use-package icomplete-vertical
  :ensure t
  :demand
  :after (minibuffer icomplete)
  :config
  (setq icomplete-vertical-prospects-height (/ (frame-height) 6))
  (icomplete-vertical-mode -1)
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle)
              ("M-q" . icomplete-vertical-toggle)))

(use-package consult
  :after minibuffer
  :commands consult-file-externally
  :config
  (setq consult-line-numbers-widen t)
  (setq consult-preview-buffer nil)
  (setq consult-preview-mark nil)
  (setq consult-preview-line nil)
  (setq consult-preview-outline nil)
  (consult-preview-mode -1)
  
  (defun my/consult-file-jump (initial-input initial-directories)
    "Find any file across INITIAL-DIRECTORIES"
    (let ((all-files-list nil))
      (consult--read "Find File: "
                     (dolist (default-directory
                               (if (listp initial-directories)
                                   initial-directories
                                 (list initial-directories))
                               all-files-list)
                       (let* ((localdir (file-local-name (expand-file-name default-directory)))
                              (command (format "fd -t f -L -0 . %s" localdir)))
                         (setq all-files-list (append
                                               ;; (mapcar (lambda (file)
                                               ;;           (concat
                                               ;;            (file-name-as-directory default-directory)
                                               ;;            (file-name-nondirectory file))))
                                               (split-string (shell-command-to-string command) "\0" t)
                                               all-files-list))))
                     :require-match t
                     :category 'file
                     :history-type 'input
                     :sort t
                     )))

  (use-package org
    :bind (:map org-mode-map
                ("C-c C-j" . consult-outline)))
  
  :bind (("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x r x" . consult-register)
         ("C-x M-:" . consult-complex-command)
         ("M-s M-o" . consult-multi-occur)
         ("C-c C-j" . consult-outline)
         ("C-x r b" . consult-bookmark)
         ("M-s l"   . consult-line)
         ("C-x C-r" . consult-recent-file)
         ("<help> a" . consult-apropos)
         ("M-i" . consult-imenu)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after icomplete
  :config
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  ;; (marginalia-mode 1)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to 
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))


(use-package embark
  :demand
  :hook ((embark-pre-action  . completion--flush-all-sorted-completions)
         (embark-post-action . embark-occur--update-linked))
  :after minibuffer
  :bind (("M-s RET" . embark-act)
         ("M-g o" .   embark-act)
         ("M-g M-o" . embark-act)
         :map minibuffer-local-completion-map
         ("C-o" . embark-act)
         ("C-M-o" . embark-act-noexit)
         ("C-c C-o" . embark-export)
         ("M-s o" . embark-export)
         ;; ("M-v" . embark-switch-to-live-occur)
         :map completion-list-mode-map
         ("C-o" . embark-act)
         ("C-M-o" . embark-act-noexit)
         :map embark-occur-mode-map
         ("C-o" . embark-act)
         ("C-M-o" . embark-act-noexit)
         :map embark-file-map
         ("x" . consult-file-externally)
         ("j" . dired-jump)
         ("S" . sudo-find-file))
  :config
  (setq embark-occur-initial-view-alist
        '((t . list)))

  (add-to-list 'embark-keymap-alist
               '(project-file . embark-file-map))
  (add-to-list 'embark-exporters-alist
               '(project-file . embark-export-dired))
  (add-to-list 'embark-exporters-alist
               '(virtual-buffer . embark-buffer-map))
  (add-to-list 'embark-exporters-alist
               '(virtual-buffer . embark-export-virtual-ibuffer))

  (defun embark-export-virtual-ibuffer (virtual-buffers)
    "docstring"
    (let ((buffers (mapcar (lambda (buf) (substring buf 1))
                           (cl-remove-if-not
                            (lambda (buf) (equal (- (elt buf 0)
                                               consult--special-char)
                                            ?b))
                            virtual-buffers))))
      (ibuffer t "*Embark Export Ibuffer*"
               `((predicate . (member (buffer-name) ',buffers))))))
  
  (define-key embark-file-map (kbd "`") (lambda (f) (interactive)
                                          (ace-window t)
                                          (find-file f)))
  (define-key embark-buffer-map (kbd "`") (lambda (b) (interactive)
                                            (ace-window t)
                                            (switch-to-buffer b)))
  
  (use-package which-key
    :defer
    :config
    (setq embark-action-indicator
          (lambda (map) (let ((which-key-side-window-location 'bottom))
                     (which-key--show-keymap "Embark" map nil nil 'no-paging)
                     #'which-key--hide-popup-ignore-command))
      embark-become-indicator embark-action-indicator)))

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

;;=================================
;; END OF CUSTOMIZATION 
;;=================================
(provide 'setup-icomplete)


;; (defun my/icomplete-show-vertical (&optional str)
;;     "Allow `icomplete' to present results vertically.

;; This is meant to be used by other functions that need to show
;; their results as a vertical list, with an optional string marking
;; the demarcation line.

;; For an interactive version see `my/icomplete-toggle-vertical'."
;;     (when (bound-and-true-p icomplete-mode)
;;       (setq icomplete-prospects-height 10)
;;       (if str
;;           (setq icomplete-separator
;;                 (concat "\n" (propertize str 'face 'shadow) "\n "))
;;         (setq icomplete-separator "\n "))))

;;   (defun my/icomplete-restore-horizontal ()
;;     "Restore `icomplete' to its horizontal layout.

;; This is meant to be run by the `minibuffer-exit-hook'."
;;     (unless (string= icomplete-separator " ┆ ")
;;       (setq icomplete-prospects-height 3)
;;       (setq icomplete-separator " ┆ ")))

;; (add-hook 'icomplete-minibuffer-setup-hook
;;                   #'icomplete-vertical-minibuffer-setup
;;                   5)
