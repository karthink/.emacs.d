;; * SHELL COMPLETION  -*- lexical-binding: t; -*-

;; ** FISH-COMPLETION

;; The =fish-completion= package provides completions (including docstrings) by
;; querying fish. [[https://github.com/minad/marginalia/issues/87][As it turns
;; out]] this can get convoluted, so it's disabled in favor of =pcmpl-args=
;; ([[*PCMPL-ARGS][which see]]).

(use-package fish-completion
  :disabled
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

  (advice-remove 'pcomplete-completions-at-point #'fish-completion--provide-annotation-function)
  (advice-add #'pcomplete-completions-at-point
              :filter-return #'fish-completion--provide-annotation-function)
  
  (with-eval-after-load 'eshell
    (setq eshell-command-completion-function 
          (lambda ()
            (pcomplete-here
             (my/eshell-fish-complete-commands-list))))
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
	        ;; (pcomplete-uniquify-list)
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
                          completions)))))
        (eshell-complete-commands-list)))))

;; ** CAPF-AUTOSUGGEST

;; capf-autosuggest is a good idea, but it's too slow and laggy in practice.
(use-package capf-autosuggest
  ;; :straight t
  :disabled
  :hook ((comint-mode) . capf-autosuggest-mode))


;; ** PCOMPLETE
;; Emacs' Programmable Completion interface. These specific settings are
;; superceded by =pcmpl-args=.
(use-package pcomplete
  :disabled
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

;; ** PCMPL-ARGS
;;;-------------
;; :PROPERTIES:
;; :CUSTOM_ID: pcmpl-args
;; :END:

;; Fully Emacs-native shell completions, including docstrings parsed on-the-fly
;; from man-pages or --help output.
(use-package pcmpl-args
  :disabled
  :ensure t
  :hook ((eshell-mode . my/pcmpl-args-eshell-settings)
         ((eshell-mode shell-mode) . my/pcmpl-args-capf-ensure))
  :config
  (defun my/pcmpl-args-prepare ()
    (let ((pfunc
           (thread-first
             "pcomplete/"
             (concat (car (pcomplete-parse-arguments)))
             (intern))))
      (unless (fboundp pfunc)
        (defalias pfunc 'pcmpl-args-pcomplete-on-man)))
    (list nil :exclusive 'no))
  (defun my/pcmpl-args-capf-ensure ()
    (add-hook 'completion-at-point-functions
              'my/pcmpl-args-prepare -90 t))
  (defun my/pcmpl-args-eshell-settings ()
    (setq-local pcomplete-try-first-hook
                '(eshell-complete-host-reference
                  eshell-complete-history-reference
                  eshell-complete-user-reference
                  eshell-complete-variable-assignment
                  eshell-complete-variable-reference
                  eshell-complete-lisp-symbols
                  t))))

;; * ESHELL

;; ** Eshell built-ins
(use-package eshell
  :hook ((eshell-mode . my/eshell-keys-and-modes)
         (eshell-first-time-mode . my/eshell-first-load-settings))
  :config
  (defun my/eshell-first-load-settings ()
    (setq eshell-visual-commands (append eshell-visual-commands
                                         '("btm" "fzf" "pulsemixer" "mpv"
                                           "ncmpcpp" "progress" "julia"
                                           "ranger" "watch" "bluetoothctl"))
          ;; eshell-input-filter-functions '(eshell-expand-history-references)
          eshell-hist-ignoredups t
          eshell-destroy-buffer-when-process-dies t
          eshell-directory-name (dir-concat user-cache-directory "eshell/")
          eshell-history-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "history")
          eshell-last-dir-ring-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "lastdir")
          eshell-history-size 4096
          eshell-glob-case-insensitive t
          eshell-error-if-no-glob nil)
    (setq eshell-aliases-file
          (concat (file-name-as-directory
                   eshell-directory-name)
                  "alias"))
    (eshell-read-aliases-list))
  
  (defun my/eshell-keys-and-modes ()
    (setq outline-regexp eshell-prompt-regexp)
    (abbrev-mode 1)
    (setq-local imenu-generic-expression
                  '(("λ: " " λ \\(.*\\)" 1)))
    (define-key eshell-mode-map (kbd "H-<return>") 'my/delete-window-or-delete-frame)
    (define-key eshell-hist-mode-map (kbd "M-s") nil)
    (define-key eshell-mode-map (kbd "C-c C-SPC") 'eshell-mark-output)
    (define-key eshell-mode-map (kbd "C-<return>") 'my/eshell-send-detached-input)
    (setq-local company-minimum-prefix-length 2)
    ;; (setq-local completion-in-region-function #'consult-completion-in-region)
    (setq eshell-cmpl-cycle-cutoff-length 2)))

;; ** Eshell paging

;; There was an idea -- use Eshell as a pager (with ~SPC~ and ~S-SPC~) if not at
;; the prompt.
;;
;; It was a bad idea.
(use-package em-pager
  :disabled
  :after eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda () 
              (define-key eshell-mode-map [remap keyboard-quit]
                #'eshell-pager-quit)))
  ;; Setup the pager
  (remove-hook 'eshell-output-filter-functions #'eshell-postoutput-scroll-to-bottom)
  (add-hook 'eshell-output-filter-functions #'eshell-pager-reposition)
  (add-hook 'eshell-pre-command-hook #'eshell-pager-pre)
  (add-hook 'eshell-post-command-hook #'eshell-pager-post))

;; ** Eshell buffer redirection
(use-package eshell
  :defer
  :config
  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun my/eshell-font-lock-and-pop (fun object target)
    (let* ((buf (and (markerp target) (marker-buffer target)))
           (str (and buf (stringp object) (string-match-p "\e\\[" object) object)))
      (funcall fun (if str (ansi-color-apply str) object) target)
      (when buf
        (with-current-buffer buf
          (goto-char (point-min))
          (font-lock-mode)
          (pop-to-buffer buf)))))
  (advice-add 'eshell-output-object-to-target
              :around #'my/eshell-font-lock-and-pop)
  
  ;; From https://emacs.stackexchange.com/questions/42113/customize-eshell-redirection-to-buffer
  (defun my/eshell-syntax-buffer-redirect ()
    "Parse buffer redirection > #buf and >#."
    (when (and (not eshell-current-argument)
               (not eshell-current-quoted)
               ;; Don't overwrite `eshell-parse-special-reference'
               (not (looking-at "#<\\(\\(buffer\\|process\\)\\s-\\)?"))
               (looking-at "#\\(\\S-+\\)?"))
      (goto-char (match-end 0)) ;; Go to the end of the match.
      (list #'get-buffer-create
            (or
             (match-string 1)
             (format "*eshell export: %s*"
                     (replace-regexp-in-string
                      "\\s-*>+\\s-*#.*\\'" ""
                      (buffer-substring-no-properties (line-beginning-position) (point))))))))
  (add-hook 'eshell-parse-argument-hook #'my/eshell-syntax-buffer-redirect))

;; ** Eshell history management
(defvar ISATUIN (executable-find "atuin")
  "Whether atuin is available for shell/eshell history.")
(use-package eshell
  :if (not ISATUIN)
  :hook ((eshell-mode . my/eshell-hist-use-global-history)
         (eshell-pre-command . eshell-save-some-history)
         (eshell-pre-command . my/eshell-history-remove-duplicates))
  :config
  (add-hook 'eshell-expand-input-functions
            'eshell-expand-history-references)
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
                (string-prefix-p " " str)))))
  
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
          (index 1)
          (found))
      (while (and (not found) (<= index (1- (ring-length ring))))
        (if (not (string= (string-trim first)
                          (string-trim (ring-ref ring index))))
            (setq index (1+ index))
          (ring-remove ring index)
          (setq found t)))
      ring))

  (defun my/eshell-history-remove-duplicates ()
    (my/ring-delete-first-item-duplicates eshell-history-ring))
  
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
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-hist-mode-map (kbd "M-r")
                'my/eshell-previous-matching-input))))

(use-package eshell-atuin
  :when ISATUIN
  :ensure (:host github :repo "SqrtMinusOne/eshell-atuin")
  :after eshell
  :hook (eshell-first-time-mode . my/bind-eshell-atuin)
  :preface
  (defun my/bind-eshell-atuin ()
    (keymap-set eshell-hist-mode-map
                "M-r" 'eshell-atuin-history))
  :init (eshell-atuin-mode)
  :config
  (setopt eshell-atuin-save-duration nil ;Requires atuin 18.0+
          eshell-atuin-search-fields '(time duration command directory host)
          eshell-atuin-history-format "%-80c %-40i %>10t %h"))

;; ** Eshell extras

;; Custom functions for use in eshell
(use-package eshell
  :defer
  :config
  (defalias 'eshell/v 'eshell-exec-visual)
  (defalias 'eshell/x #'eshell/exit)

  (defun my/eshell-insert-args (&optional num)
    "Insert the NUMth argument of the previous command.

NUM counts from the end"
    (interactive "p")
    (let ((valid-pos)
          (N (length eshell-last-arguments)))
      (save-excursion
        (beginning-of-line)
        (if (looking-at eshell-prompt-regexp)
            (setq valid-pos t)))
      (if valid-pos
          (insert (substring-no-properties
                   (nth (- N num) eshell-last-arguments)))
        (call-interactively #'xref-find-definitions))))
  
  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun eshell/b (regexp)
    "Output buffer content of buffer matching REGEXP."
    (cl-loop for buf in (buffer-list)
             thereis
             (and (string-match-p regexp (buffer-name buf))
                  (with-current-buffer buf
                    (buffer-substring-no-properties (point-min) (point-max))))))

  ;; From https://github.com/LemonBreezes/.doom.d/blob/master/modules/private/eshell/autoload.el
  (defun eshell/hat (&rest files)
    "Output FILES with highlighting."
    (dolist (f files)
      (eshell-print (my/eshell-file-contents f))))
  
  (defun eshell/view (&optional file)
    (if (or (not file)
            (not (file-exists-p file))
            (file-directory-p file))
        (dired-other-window default-directory)
      (find-file-other-window file)
      (read-only-mode 1)
      (view-mode 1)))
  
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
                       (completing-read "cd: " eshell-dirs)))))))
  
  ;; From https://protesilaos.com/dotemacs
  (defun my/eshell-export-output (&optional arg)
    "Export output of the last command to a buffer.
With prefix ARG, also copy the prompt and input."
    (interactive)
    (let ((orig (current-buffer))
          (beg (if arg (eshell-beginning-of-input)
                 (eshell-beginning-of-output)))
          (end (eshell-end-of-output))
          (buffer (get-buffer-create
                   (format "*eshell export: %s*"
                           (buffer-substring-no-properties
                            (eshell-beginning-of-input)
                            (1- (eshell-beginning-of-output)))))))
      (with-current-buffer buffer
        (font-lock-mode)
        (insert-buffer-substring orig beg end)
        (goto-char (point-min)))
      ;; Taken from `eshell-kill-output'
      (goto-char (eshell-beginning-of-output))
      (insert (format "Exported to %S\n" buffer))
      (delete-region (point) (eshell-end-of-output))
      (goto-char (point-max))
      (pop-to-buffer buffer)))

  (defun my/eshell-copy-output (&optional arg)
  "Copy output of the last command to the kill ring. With prefix
argument arg, Also copy the prompt and input."
  (interactive "P")
  (copy-region-as-kill (if arg (eshell-beginning-of-input)
                         (eshell-beginning-of-output))
                       (eshell-end-of-output))
  (message (if arg "Copied last input and output to kill ring."
             "Copied last output to kill ring.")))
  (add-hook
   'eshell-mode-hook
   (lambda ()
     (define-key eshell-mode-map (kbd "M-.") 'my/eshell-insert-args)
     (define-key eshell-mode-map (kbd "C-c M-w") 'my/eshell-copy-output)
     (define-key eshell-hist-mode-map (kbd "C-c C-l") 'my/eshell-export-output)))
  
  ;;From https://github.com/nbarrientos/dotfiles/.emacs.d/init.el
  (defun my/eshell-send-detached-input (&optional arg)
    "Send the current Eshell input to a compilation buffer.
With universal prefix argument bury the compilation buffer and
send a notification when the process has exited."
    (interactive "p")
    (let* ((cmd (buffer-substring
                 eshell-last-output-end (point-max)))
           (hostname (car (split-string
                           (or
                            (file-remote-p default-directory 'host)
                            (system-name))
                           "\\.")))
           (compile-command nil)
           (compilation-buffer-name-function
            (lambda (_major-mode)
              (format "D# %s (%s)" cmd hostname)))
           (compilation-buffer (compile cmd)))
      (when (equal arg 4)
        (with-current-buffer compilation-buffer
          (switch-to-prev-buffer (get-buffer-window (current-buffer)))
          (setq-local compilation-finish-functions
                      `((lambda (buffer str)
                          (notifications-notify
                           :body ,cmd
                           :timeout 8000
                           :category "detached_process"
                           :actions '("default" "Switch to buffer")
                           :on-action (lambda (id key) (switch-to-buffer-other-window ,(buffer-name compilation-buffer)))
                           :title (format "Process running in '%s' finished!" ,hostname)
                           :urgency (if (string-prefix-p "finished" str) 'normal 'critical)))))))
      (eshell-add-input-to-history cmd)
      (eshell-reset)))
  
  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun my/eshell-buffer-contents (buffer)
    "Return fontified buffer contents for BUFFER."
    (with-current-buffer buffer
      (font-lock-ensure (point-min) (point-max))
      (buffer-string)))

  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun my/eshell-file-contents (file)
    "Return fontified file contents for FILE."
    (let ((buffer (get-file-buffer file)))
      (if buffer
          (my/eshell-buffer-contents buffer)
        (unwind-protect
            (my/eshell-buffer-contents
             (setq buffer
                   (let ((inhibit-message t)
                         (non-essential t)
                         (enable-dir-local-variables nil)
                         (enable-local-variables (and enable-local-variables :safe)))
                     (find-file-noselect file))))
          (when buffer
            (kill-buffer buffer)))))))

;; Better region selection for eshell.
(use-package expand-region
  :defer
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (when (require 'expand-region nil t)
                (make-local-variable 'er/try-expand-list)
                (add-to-list 'er/try-expand-list
                             #'er/mark-eshell-command-and-output))))
  :config
  (defun er/mark-eshell-command-and-output ()
    (when (re-search-forward eshell-prompt-regexp nil t)
      (goto-char (match-beginning 0))
      (push-mark)
      (when (re-search-backward eshell-prompt-regexp nil t)
        (goto-char (match-end 0))))))

;; ** Eshell spawning
;;
;; Calling and exiting eshell
(use-package eshell
  :bind (("H-<return>" . eshell)
         ("H-!" . eshell-here))
  :config
  (advice-add 'eshell-life-is-too-much :after #'delete-window-if-not-single)
  (advice-add 'eshell-mark-output :after #'activate-mark)
  ;; From http://howardism.org/Technical/Emacs/eshell-fun.html
  (defun eshell-here ()
      "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
      (interactive)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (name   (car (last (split-string parent "/" t)))))
        (if-let* ((eshell-name (concat "*eshell: " name "*"))
                  (existing-eshell-buffer (get-buffer eshell-name)))
            (select-window (display-buffer existing-eshell-buffer))
          (select-window (display-buffer (eshell "new")))
          (rename-buffer eshell-name)
          (insert (concat "ls"))
          (eshell-send-input)))))

;; ** Eshell appearance
;;
;; Including the prompt.
(use-package eshell
  :defer
  :config
  (setq eshell-prompt-regexp "^.* λ "
          eshell-prompt-function #'my/eshell-default-prompt-fn)

  ;; From the Doom emacs config
  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)))

  (defun my/eshell-default-prompt-fn ()
    "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
    (concat (if (bobp) "" "\n")
            (when (bound-and-true-p conda-env-current-name)
              (propertize (concat "(" conda-env-current-name ") ")
                          'face 'my/eshell-prompt-git-branch))
            (let ((pwd (eshell/pwd)))
              (propertize (if (equal pwd "~")
                              pwd
                            (abbreviate-file-name pwd))
                          'face 'my/eshell-prompt-pwd))
            (propertize (my/eshell--current-git-branch)
                        'face 'my/eshell-prompt-git-branch)
            (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " "))

  (defsubst my/eshell--current-git-branch ()
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

  (defface my/eshell-prompt-pwd '((t (:inherit font-lock-keyword-face)))
    "TODO"
    :group 'eshell)

  (defface my/eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
    "TODO"
    :group 'eshell))

;; Bookmarks for eshell is built-into Emacs 28.
(use-package eshell-bookmark
  :disabled
  :if (>= emacs-major-version 28)
  :hook (eshell-mode . eshell-bookmark-setup))

;; * COMINT & SHELL
(use-package shell
  :defer
  :config
  (defun zsh-shell-mode-setup ()
    ;; (setq-local shell-prompt-pattern "^[^#$%\n]*[#$%>❯] *" )
    (setq-local comint-process-echoes t))
  (add-hook 'shell-mode-hook #'zsh-shell-mode-setup))

(use-package comint
  :commands (comint-mode shell-command-at-line)
  :bind
  ("C-!" . shell-command-at-line)
  :bind
  (:map comint-mode-map
   ("SPC" . comint-magic-space))
  :config
  ;; Arrange for Emacs to notice password prompts and turn off echoing for them, as follows:
  (setq read-process-output-max (* 1024 64))
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt)

  (setq ansi-color-for-comint-mode t)
  
  ;; Auto-kill buffer and window of comint process when done
  (advice-add 'comint-send-eof :after
              (defun comint-kill-after-finish-a (&rest _args)
                (let (confirm-kill-processes kill-buffer-query-functions)
                  ;; (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
                  (ignore-errors (kill-buffer-and-window)))))

  (defun shell-command-at-line (&optional prefix)
    "Run contents of line around point as a shell command and
replace the line with output. With a prefix argument, append the
output instead."
    (interactive "P")
    (let ((command
            (if (use-region-p)
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))
              (thing-at-point 'line))))
      (cond ((use-region-p)
             (call-interactively #'delete-region))
            ((null prefix)
             (kill-whole-line)
             (indent-according-to-mode))
            (t (newline-and-indent)))
      (insert (string-trim
               (ansi-color-apply
                (shell-command-to-string command))))
      (exchange-point-and-mark))))

(use-package simple
  :config
  (setq async-shell-command-display-buffer nil
        async-shell-command-buffer 'new-buffer))

;; ** COMINT EXTRAS

;; comint-mime and coterm: disabled while I test eat
(use-package comint-mime
  :disabled
  :ensure t
  :hook ((shell-mode . comint-mime-setup)
         (inferior-python-mode . comint-mime-setup)))

;; Disabled while I test eat
(use-package coterm
  :ensure t
  :defer)

;; ;TODO: Check recipe vs elpaca
(use-package eat
  :ensure
  (:host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el")))
  :hook ((eshell-mode . eat-eshell-mode)
         (eat-mode . my/eat-keys))
  :bind (:map eat-semi-char-mode-map
         ("M-v" . eat-emacs-mode)
         :map eat-mode-map
         ("RET" . eat-semi-char-mode))
  :config
  (defun my/eat-keys ()
    (remove-hook 'eat-mode-hook 'my/eat-keys)
    (dolist (key `([?\e ?o] [?\e ?`] ,(kbd "C-`")
                   [?\e 67108960] [C-M-v] [?\e v]))
      (push key eat-semi-char-non-bound-keys))
    (eat-update-semi-char-mode-map)
    (eat-reload))
  (setq eat-kill-buffer-on-exit t))

(use-package eat
  :disabled
  :after project
  :bind ([remap project-shell] . eat-project))

;; Disabled: shelldon. Regular `async-shell-command' does enough for me.
(use-package shelldon
  :disabled
  :ensure t
  :bind (([remap async-shell-command] . my/shelldon-dwim))
  :config
  (defun my/shelldon-dwim (&optional arg)
    (interactive "P")
    (call-interactively
     (if arg #'shelldon-loop #'shelldon)))
  (setf (alist-get "*\\(shelldon.*\\)" display-buffer-alist nil t 'equal)
        `((display-buffer-pop-up-window
           display-buffer-reuse-window
           display-buffer-in-previous-window)
	  (direction . right)
          (window-width . 80)
          (body-function . (lambda (win)
                             (select-window win)
                             (goto-char (point-min))))
          (reusable-frames . visible)))
  (use-package sh-mode
    :bind (:map sh-mode-map
           ("C-c C-c" . shelldon-send-region)
           ("C-c C-n" . shelldon-send-line-at-point))))

;; * VTERM

;; Vterm is a module, so the build declaration takes a little work.
(unless (or IS-GUIX IS-WINDOWS)
  (use-package vterm
    :ensure (:files
             ("*.so" "*.el")
             :pre-build
             (progn (unless (file-directory-p "build")
                      (make-directory "build"))
                    (call-process
                     "sh" nil "*vterm-prepare*" t "-c" 
                     (concat "cd build; "
                             "cmake -G 'Unix Makefiles' .."))
                    (compile "cd build; make")))
    :defer))

(use-package vterm
  :when (not IS-WINDOWS)
  :bind (:map vterm-mode-map
              ("C-c C-p" . my/vterm-previous-prompt)
              ("C-c C-n" . my/vterm-next-prompt)
              ("M-v" . vterm-copy-mode))
  :config
  (add-to-list
   'vterm-eval-cmds
   '("update-pwd" (lambda (path) (setq default-directory path))))
  
  (defun my/vterm-next-prompt (&optional arg)
    "next prompt"
    (interactive "p")
    (re-search-forward term-prompt-regexp nil t arg)
    (when (< arg 0) (goto-char (match-end 0))))
  (defun my/vterm-previous-prompt (&optional arg)
    "previous prompt"
    (interactive "p")
    (beginning-of-line)
    ;; (if (looking-at term-prompt-regexp)
    ;;     (setq arg (1+ arg)))
    (my/vterm-next-prompt (- (or arg 1)))))

(use-package vterm
  :when (not IS-WINDOWS)
  :after project
  :bind (:map project-prefix-map
         ("s" . vterm-project))
  :config
  (defun vterm-project (&optional arg)
    (interactive "P")
    (require 'project)
    (let* ((default-directory (project-root (project-current t)))
           (buffer-name (project-prefixed-buffer-name "vterm")))
      (vterm buffer-name))))


(provide 'setup-shells)
