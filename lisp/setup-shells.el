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
         (eshell-first-time-mode . my/eshell-first-load-settings))
  :config
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
                                           "ncmpcpp" "progress"))
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
          eshell-history-size 2048
          eshell-command-completion-function (lambda ()
                                               (pcomplete-here
                                                (my/eshell-fish-complete-commands-list))))

    (defalias 'eshell/x #'eshell/exit)
    ;; (setq eshell-buffer-shorthand t)
    
    (advice-add 'eshell-life-is-too-much :after #'delete-window-if-not-single)
    
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
    (define-key eshell-mode-map (kbd "H-<return>") 'delete-window)
    (define-key eshell-mode-map (kbd "M-r") 'my/eshell-previous-matching-input)
    (define-key eshell-mode-map (kbd "M-s") nil)
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
    (eshell/cd
     (if regexp (eshell-find-previous-directory regexp)
       (completing-read "cd: " (delete-dups (mapcar 'abbreviate-file-name
                                                    (ring-elements eshell-last-dir-ring)))))))
  
  )

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
