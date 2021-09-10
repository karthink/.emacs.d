(use-package fish-completion
  :ensure
  :when (executable-find "fish")
  :hook ((eshell-mode shell-mode) . fish-completion-mode)
  :config
  (defun fish-completion--list-completions (raw-prompt)
    (mapcar (lambda (e)
              (string-match "\\`\\([^\t]*\\)\t?\\(.*\\)\\'" e)
              (propertize (match-string 1 e) 'fish-completion--annotation (match-string 2 e)))
            (split-string
             (fish-completion--list-completions-with-desc raw-prompt)
             "\n" t)))

  (defun fish-completion--annotate (cand)
    (when-let* ((pos (or (next-single-property-change 0 'fish-completion--annotation cand)
                         0))
                (ann (get-text-property pos 'fish-completion--annotation cand)))
      (concat (propertize " " 'display '(space :align-to center)) ann)))

  (defun fish-completion--provide-annotation-function (table)
    (nconc table (list :annotation-function #'fish-completion--annotate)))

  (advice-add #'pcomplete-completions-at-point
              :filter-return #'fish-completion--provide-annotation-function))

(use-package pcomplete
  :defer
  :config
  (defun pcomplete/xargs ()
    "Completion for `xargs'."
    (while (string-prefix-p "-" (pcomplete-arg 0))
      (funcall pcomplete-default-completion-function))
    (funcall pcomplete-command-completion-function)
    (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	         pcomplete-default-completion-function)))
  (defun pcomplete/time ()
    "Completion for `time'."
    (pcomplete-here (funcall pcomplete-command-completion-function))
    (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	         pcomplete-default-completion-function))))

(use-package eshell
  :bind (("H-<return>" . eshell)
         ("H-!" . eshell-here))
  :hook ((eshell-mode . my/eshell-keys-and-modes)
         (eshell-mode . my/eshell-hist-use-global-history)
         (eshell-pre-command-hook . eshell-save-some-history)
         (eshell-pre-command-hook . my/eshell-history-remove-duplicates)
         (eshell-first-time-mode . my/eshell-first-load-settings))
  :config
  ;; From the Doom emacs config
  (defun my/eshell-default-prompt-fn ()
    "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
    (concat (if (bobp) "" "\n")
            (let ((pwd (eshell/pwd)))
              (propertize (if (equal pwd "~")
                              pwd
                            (abbreviate-file-name pwd))
                          'face 'my/eshell-prompt-pwd))
            (propertize (my/eshell--current-git-branch)
                        'face 'my/eshell-prompt-git-branch)
            (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " "))

  (defun my/eshell--current-git-branch ()
    ;; TODO Refactor me
    (cl-destructuring-bind (status . output)
        (with-temp-buffer (cons
                           (or (call-process "git" nil t nil "symbolic-ref" "-q" "--short" "HEAD")
                               (call-process "git" nil t nil "describe" "--all" "--always" "HEAD")
                               -1)
                           (string-trim (buffer-string))))
      (if (equal status 0)
          (format " [%s]" output)
        "")))

  (defface my/eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
    "TODO"
    :group 'eshell)

  (defface my/eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
    "TODO"
    :group 'eshell)

;; From https://gitlab.com/ambrevar/dotfiles/-/blob/master/.emacs.d/lisp/init-eshell.el
  (defvar my/eshell-history-global-ring nil
    "History ring shared across Eshell sessions.")

  (defun my/eshell-hist-use-global-history ()
    "Make Eshell history shared across different sessions."
    (unless my/eshell-history-global-ring
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq my/eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
    (setq eshell-history-ring my/eshell-history-global-ring))
  
  (defun my/ring-delete-first-item-duplicates (ring)
    "Remove duplicates of last command in history.
Return RING.

This should be faster then `seq-uniq'.  Unlike
`eshell-hist-ignoredups' or `comint-input-ignoredups', it does
not allow duplicates ever.
Surrounding spaces are ignored when comparing."
    (let ((first (ring-ref ring 0))
          (index 1))
      (while (<= index (1- (ring-length ring)))
        (if (string= (string-trim first)
                     (string-trim (ring-ref ring index)))
            ;; REVIEW: We could stop at the first match, it would be faster and it
            ;; would eliminate duplicates if we started from a fresh history.
            ;; From an existing history that would not clean up existing
            ;; duplicates beyond the first one.
            (ring-remove ring index)
          (setq index (1+ index))))
      ring))

  (defun my/eshell-history-remove-duplicates ()
    (my/ring-delete-first-item-duplicates eshell-history-ring))
  
  (defun my/eshell-fish-complete-commands-list ()
  "Gerenate list of appliclable, visible commands by combining
Eshell specific completions with those returned by fish shell.

Falls back to native Eshell completion if fish-completion is not available.

Filenames are always matched by eshell."
  (if (fboundp 'fish-completion--list-completions)
      (let ((filename (pcomplete-arg)) glob-name)
        (if (file-name-directory filename)
            (if eshell-force-execution
                (pcomplete-dirs-or-entries nil #'file-readable-p)
              (pcomplete-executables))
          (if (and (> (length filename) 0)
	           (eq (aref filename 0) eshell-explicit-command-char))
	      (setq filename (substring filename 1)
		    pcomplete-stub filename
		    glob-name t))
          (let ((completions (fish-completion--list-completions filename)))
            ;; Add aliases which are currently visible, and Lisp functions.
	    (pcomplete-uniquify-list
	     (if glob-name
	         completions
	       (setq completions
		     (append (if (fboundp 'eshell-alias-completions)
			         (eshell-alias-completions filename))
			     (eshell-winnow-list
			      (mapcar
			       (function
			        (lambda (name)
			          (substring name 7)))
			       (all-completions (concat "eshell/" filename)
					        obarray #'functionp))
			      nil '(eshell-find-alias-function))
			     completions))
	       (append (and (or eshell-show-lisp-completions
			        (and eshell-show-lisp-alternatives
				     (null completions)))
			    (all-completions filename obarray #'functionp))
                       completions))))))
    (eshell-complete-commands-list)))
  
  (defun my/eshell-first-load-settings ()
    (setq eshell-visual-commands (append eshell-visual-commands
                                         '("btm" "fzf" "pulsemixer" "mpv"
                                           "ncmpcpp" "progress" "julia"))
          ;; eshell-input-filter-functions '(eshell-expand-history-references)
          eshell-hist-ignoredups t
          eshell-destroy-buffer-when-process-dies t
          eshell-directory-name "~/.cache/emacs/eshell/"
          eshell-history-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "history")
          eshell-aliases-file (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "alias")
          eshell-last-dir-ring-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "lastdir")
          eshell-history-size 4096
          eshell-command-completion-function (lambda ()
                                               (pcomplete-here
                                                (my/eshell-fish-complete-commands-list)))
          eshell-glob-case-insensitive t
          eshell-error-if-no-glob t)
    (setq eshell-prompt-regexp "^.* λ "
          eshell-prompt-function #'my/eshell-default-prompt-fn
          eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)))

    (defalias 'eshell/x #'eshell/exit)
    ;; (setq eshell-buffer-shorthand t)
    
    (advice-add 'eshell-life-is-too-much :after #'delete-window-if-not-single)
    (advice-add 'eshell-mark-output :after #'activate-mark)
    
    ;; From
    ;; https://gitlab.com/ambrevar/dotfiles/-/blob/master/.emacs.d/lisp/init-eshell.el
    (setq eshell-input-filter
          (lambda (str)
            (not (or
                  ;; Here we can filter out failing commands.  This is usually a bad
                  ;; idea since a lot of useful commands have non-zero exit codes
                  ;; (including Emacs/Eshell functions).
                  ;; (/= eshell-last-command-status 0)
                  (string= "" str)
                  (string= "cd" str)
                  (string-prefix-p "cd " str)
                  ;; Filter out space-beginning commands from history.
                  (string-prefix-p " " str))))))
  
  (defun my/eshell-keys-and-modes ()
    (setq outline-regexp eshell-prompt-regexp)
    (abbrev-mode 1)
    (define-key eshell-mode-map (kbd "H-<return>") 'my/delete-window-or-delete-frame)
    (define-key eshell-mode-map (kbd "M-r") 'my/eshell-previous-matching-input)
    (define-key eshell-mode-map (kbd "M-s") nil)
    (define-key eshell-mode-map (kbd "C-c M-w") 'eshell-copy-output)
    (define-key eshell-mode-map (kbd "C-c C-l") 'my/eshell-export)
    (define-key eshell-mode-map (kbd "C-c C-SPC") 'eshell-mark-output)
    (setq-local company-minimum-prefix-length 2)
    ;; (setq-local completion-in-region-function #'consult-completion-in-region)
    (setq eshell-cmpl-cycle-cutoff-length 2))
  
  (defun my/eshell-previous-matching-input ()
    (interactive)
    (when-let ((input (completing-read "History element: "
                                  (delete-dups (ring-elements eshell-history-ring))
                                  nil t
                                  (buffer-substring-no-properties
                                   eshell-last-output-end
                                   (point)))))
      (delete-region (save-excursion (eshell-bol)) (point))
      (insert input)))
  
  (defun eshell-copy-output (&optional arg)
  "Copy output of the last command to the kill ring. With prefix
argument arg, Also copy the prompt and input."
  (interactive "P")
  (copy-region-as-kill (if arg (eshell-beginning-of-input)
                         (eshell-beginning-of-output))
                       (eshell-end-of-output))
  (message (if arg "Copied last input and output to kill ring."
             "Copied last output to kill ring.")))
  
  ;; From https://protesilaos.com/dotemacs
  
  (defcustom my/eshell-output-buffer "*Eshell Export*"
    "Name of buffer with the last output of Eshell command.
Used by `my/eshell-export'."
    :type 'string
    :group 'eshell)

  (defun my/eshell-export (&optional arg)
  "Produce a buffer with output of the last Eshell command. 
With optional argument ARG, include the input as well.
If `my/eshell-output-buffer' does not exist, create it. Else
append to it."
  (interactive "P")
  (let ((eshell-output (buffer-substring-no-properties
                        (if arg (save-excursion (goto-char (eshell-beginning-of-input))
                                                (goto-char (point-at-bol)))
                          (eshell-beginning-of-output))
                        (eshell-end-of-output))))
    (with-current-buffer (get-buffer-create my/eshell-output-buffer)
      (goto-char (point-max))
      (unless (eq (point-min) (point-max))
        (insert (format "\n%s\n\n" "* * *")))
      (goto-char (point-at-bol))
      (insert eshell-output)
      (switch-to-buffer-other-window (current-buffer)))))
  
  ;; From http://howardism.org/Technical/Emacs/eshell-fun.html
  (defun eshell-here ()
      "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
      (interactive)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (height (/ (window-total-height) 3))
             (name   (car (last (split-string parent "/" t)))))
        (split-window-vertically (- height))
        (other-window 1)
        (if-let* ((eshell-name (concat "*eshell: " name "*"))
                  (existing-eshell-buffer (get-buffer eshell-name)))
            (switch-to-buffer existing-eshell-buffer)
          (eshell "new")
          (rename-buffer eshell-name)
          (insert (concat "ls"))
          (eshell-send-input))))
  
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                            (completing-read "cd: " eshell-dirs))))))))

(use-package eshell-bookmark
  :ensure
  :hook (eshell-mode . eshell-bookmark-setup))

(use-package em-alias
  :after eshell
  :config
  ;;; If we read the alias list here, it means we make commandline-defined aliases persistent.
  ;; (eshell-read-aliases-list)
  (dolist
      (alias
       '(("l" "ls -1 $*")
         ("la" "ls -lAh $*")
         ("ll" "ls -lh $*")
         ;; TODO: Aliasing eshell/{cp,mv,ln} does not work.
         ;; REVIEW: Eshell/TRAMP's sudo does not work with aliases.
         ;; See #28320, #27168.
         ;; ("ls" "ls -F $*") ; not supported
         ;; ("emacs" "find-file $1")
         ;; ("cp" "eshell/cp -iv $*")
         ;; ("mv" "eshell/mv -iv $*")
         ("ffow" "find-file-other-window $1")
         ("ffof" "find-file-other-frame  $1")
         )) ; TODO: '&&' does not work because mkdir exits with nil?
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

(use-package shell
  :defer
  :config
  (setq async-shell-command-buffer 'new-buffer))

(provide 'setup-shells)
