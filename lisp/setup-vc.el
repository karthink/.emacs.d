;; -*- lexical-binding: t; -*-

;; VC gets the short-shrift in the age of Magit.
(use-package vc
  :defer
  :config
  (setq vc-follow-symlinks t)
  (setq vc-find-revision-no-save t)

  (use-package log-view
    :config
    (defun my/vc-print-log (&optional arg)
      "Like `vc-print-log' but for a custom fileset.

With optional prefix ARG (\\[universal-argument]), query for a
number to limit the log to.  Then prompt the user for matching
files in the `default-directory'.  A literal space delimits
multiple files (inserting a space will renew the prompt, asking
for another file match).

In a `dired-mode' buffer, print log for the file at point, or any
marked files, except for when a double prefix argument is passed.
A single prefix arg still provides for a limit to the log.

If a double prefix ARG is passed, prompt for a limit and produce
a log that covers all files in the present directory."
      (interactive "P")
      (let* ((lim (if arg
                      (read-number "Limit log to N entries: " 5)
                    20))
             (dir default-directory)
             (dotless directory-files-no-dot-files-regexp)
             (files (directory-files dir nil dotless t))
             (crm-separator " ")
             (set (cond
                   ((equal arg '(16))
                    files)
                   ((eq major-mode 'dired-mode)
                    (dired-get-marked-files t nil))
                   (t
                    (completing-read-multiple
                     "Select files in current dir: " files nil t))))
             (backend (vc-backend set)))
        (vc-print-log-internal backend set nil nil lim 'with-diff)))

    (defun my/vc-git-expanded-log-entry (revision)
      "Expand git commit message for REVISION."
      (with-temp-buffer
        (apply 'vc-git-command t nil nil (list "log" revision "--stat" "-1" "--"))
        (goto-char (point-min))
        (unless (eobp)
          (while (re-search-forward "^" nil t)
            (replace-match "  ")
            (forward-line))
          (concat "\n" (buffer-string)))))

    (add-hook 'vc-git-log-view-mode-hook
              (defun my/vc-git-expand-function ()
                "Set `log-view-expanded-log-entry-function' for `vc-git'"
                (setq-local log-view-expanded-log-entry-function
                            #'my/vc-git-expanded-log-entry)))

    ;; (defun my/log-view-extract-commit ()
    ;;   "Kill commit from around point in `vc-print-log'."
    ;;   (interactive)
    ;;   (let ((commit (cadr (log-view-current-entry (point) t))))
    ;;     (kill-new (format "%s" commit))
    ;;     (message "Copied: %s" commit)))

    ;; (defvar my/vc-shell-output "*vc-shell-output*"
    ;;   "Name of buffer for VC-related shell output.")

    ;; (defun my/log-view-create-patch ()
    ;;   "Create patch for commit at point in `log-view'."
    ;;   (interactive)
    ;;   (let* ((commit (cadr (log-view-current-entry (point) t)))
    ;;          (vc-dir (or (vc-root-dir) default-directory))
    ;;          (dirs (list "~/" "~/Desktop/" vc-dir))
    ;;          (out-dir ;; (read-directory-name "Output directory: ")
    ;;           (completing-read "Output directory: " dirs))
    ;;         (buf (get-buffer-create my/vc-shell-output)))
    ;;     (shell-command
    ;;      (format "git format-patch -1 %s -o %s" commit out-dir) buf)
    ;;     (message "Prepared patch for `%s' and sent it to %s"
    ;;              (propertize commit 'face 'bold)
    ;;              (propertize out-dir 'face 'success))))

    ;; From https://protesilaos.com/codelog/2021-07-24-emacs-misc-custom-commands/
    (defun my/diff-buffer-dwim (&optional arg)
  "Diff buffer with its file's last saved state, or run `vc-diff'.
With optional prefix ARG (\\[universal-argument]) enable
highlighting of word-wise changes (local to the current buffer)."
  (interactive "P")
  (let ((buf))
    (if (buffer-modified-p)
        (progn
          (diff-buffer-with-file (current-buffer))
          (setq buf "*Diff*"))
      (vc-diff)
      (setq buf "*vc-diff*"))
    (when arg
      (with-current-buffer (get-buffer buf)
        (unless diff-refine
          (setq-local diff-refine 'font-lock))))))

    :bind-keymap ("H-v" . vc-prefix-map)
    :bind (("C-x v C-l" . my/vc-print-log)
           :map vc-prefix-map
           ("=" . my/diff-buffer-dwim)
           ("C-=" . vc-ediff)
           :map log-view-mode-map
           ("<tab>" . log-view-toggle-entry-display)
           ("<return>" . log-view-find-revision)
           ;; ("c" . my/log-view-create-patch)
           ;; ("w" . my/log-view-extract-commit)
           ("s" . vc-log-search)
           ("O" . vc-log-outgoing)
           ("I" . vc-log-incoming)
           ("F" . vc-update)
           ("+" . vc-update)
           ("P" . vc-push))))

(use-package vc-dir
  :after vc
  :config
  (defun my/vc-dir (&optional arg)
    "Run `vc-dir' for the current project or directory.
With optional ARG (\\[universal-argument]), use the present
working directory, else default to the root of the current
project, as defined by `vc-root-dir'."
    (interactive "P")
    (let ((dir (if arg default-directory (vc-root-dir))))
      (vc-dir dir)))

  ;; Hide unregistered files
  (defun my/vc-dir-hide-unregistered ()
    "Hide unregistered files in a vc-dir."
    (vc-dir-hide-state 'unregistered))

  (add-hook 'vc-dir-mode-hook #'my/vc-dir-hide-unregistered 90)
  :bind
  (("C-x v d" . my/vc-dir)
   :map vc-dir-mode-map
   ("F" . vc-update)
   ("k" . vc-dir-clean-files)))

(use-package vc-git
  :after vc
  :config
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (if (version<= "28" emacs-version)
      (setq vc-git-revision-complete-only-branches nil))
  (setq vc-git-root-log-format
        '("%d%h %ad %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          "^\\(?:[*/\\| ]+ \\)?\
\\(?2: ([^)]+)\\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date)))))

(use-package vc-annotate
  :after vc
  :config
  (setq vc-annotate-display-mode 'scale)
  :bind (:map vc-annotate-mode-map
        ("<tab>" . vc-annotate-toggle-annotation-visibility)))

(use-package smerge-mode
  :defer
  :bind-keymap ("C-c m" . smerge-basic-map)
  :config
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :straight t
  :defer
  ;; :custom-face
  ;; (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  ;; (diff-hl-insert ((t (:background nil))))
  ;; (diff-hl-delete ((t (:background nil))))
  ;; :hook ((dired-mode . diff-hl-dired-mode))
  :init
  (setq diff-hl-draw-borders t)
  (setq-default diff-hl-inline-popup--height 4)
  (dolist (mode-hook my/addons-enabled-modes)
    (add-hook mode-hook #'diff-hl-mode))
  (remove-hook 'text-mode-hook #'diff-hl-mode)
  :bind
  (:map diff-hl-command-map
   ("n" . diff-hl-next-hunk)
   ("p" . diff-hl-previous-hunk)
   ("[" . nil)
   ("]" . nil)
   ("DEL"   . diff-hl-revert-hunk)
   ("<delete>" . diff-hl-revert-hunk)
   ("SPC" . diff-hl-mark-hunk)
   :map vc-prefix-map
   ("n" . diff-hl-next-hunk)
   ("p" . diff-hl-previous-hunk)
   ("DEL"   . diff-hl-revert-hunk)
   ("<delete>" . diff-hl-revert-hunk)
   ("SPC" . diff-hl-mark-hunk))
  :general
  (:states '(normal visual)
           "]d"   'diff-hl-next-hunk
           "[d"   'diff-hl-previous-hunk)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Recenter to location of diff
  (advice-add 'diff-hl-next-hunk :after
              (defun my/diff-hl-recenter
                  (&optional _) (recenter)))

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  ;; (with-no-warnings
  ;;   (defun my-diff-hl-fringe-bmp-function (_type _pos)
  ;;     "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
  ;;     (define-fringe-bitmap 'my-diff-hl-bmp
  ;;       (vector #b11111100) ;(if sys/macp #b11100000 #b11111100)
  ;;       1 8
  ;;       '(center t)))
  ;;   (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)


  ;;   (unless (display-graphic-p)
  ;;     (setq diff-hl-margin-symbols-alist
  ;;           '((insert . " ") (delete . " ") (change . " ")
  ;;             (unknown . " ") (ignored . " ")))
  ;;     ;; Fall back to the display margin since the fringe is unavailable in tty
  ;;     (diff-hl-margin-mode 1)
  ;;     ;; Avoid restoring `diff-hl-margin-mode'
  ;;     (with-eval-after-load 'desktop
  ;;       (add-to-list 'desktop-minor-mode-table
  ;;                    '(diff-hl-margin-mode nil))))
  ;;   )

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package magit
  :defer t
  ;; :commands magit-status
  :straight t
  :bind ("C-x g" . magit-status)
  :hook (magit-diff-visit-file . (lambda ()
                                   (when (and smerge-mode
                                              (fboundp 'hydra-smerge/body))
                                     (hydra-smerge/body))))
  :config
  ;; (define-key magit-mode-map (kbd "C-TAB") nil)
  ;; (define-key magit-mode-map (kbd "C-<tab>") nil)
  ;; (dolist (keymap (list magit-diff-mode-map magit-log-mode-map))
  ;;   (define-key keymap (kbd "C-TAB") nil)
  ;;   (define-key keymap (kbd "C-<tab>") nil))
  (use-package magit-section
    :config
    (setq magit-section-initial-visibility-alist
          '((stashes . hide)
            ([file unstaged status] . hide))
          )
    )
  )

(use-package forge
  :straight t
  :defer
  :config
  (auth-source-pass-enable)
  (setq forge-database-file
        (dir-concat user-cache-directory "forge-database.sqlite")
        forge-owned-accounts '(("karthink"))))

;; Misc git functions
(use-package emacs
  :config
  ;; From http://xenodium.com/emacs-clone-git-repo-from-clipboard/
  ;; Auto-git-clone url in clipboard
  (defun my/git-clone-clipboard-url ()
    "Clone git URL in clipboard asynchronously and open in dired when finished."
    (interactive)
    (cl-assert (or (string-match-p "^git@github.com" (current-kill 0))
                   (string-match-p "^git@.*\\.org" (current-kill 0)) 
                   (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)))
               nil
               "No URL in clipboard")
    (let* ((url (current-kill 0))
           (download-dir (expand-file-name "~/.local/share/git/"))
           (project-dir (concat (file-name-as-directory download-dir)
                                (file-name-base url)))
           (default-directory download-dir)
           (command (format "git clone %s" url))
           (buffer (generate-new-buffer (format "*%s*" command)))
           (proc))
      (when (file-exists-p project-dir)
        (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
            (delete-directory project-dir t)
          (user-error "Bailed")))
      (switch-to-buffer buffer)
      (setq proc (start-process-shell-command
                  (shell-quote-argument (nth 0 (split-string command)))
                  buffer command))
      (with-current-buffer buffer
        (setq default-directory download-dir)
        (shell-command-save-pos-or-erase)
        (require 'shell)
        (shell-mode)
        (view-mode +1))
      (set-process-sentinel proc (lambda (process state)
                                   (let ((output (with-current-buffer (process-buffer process)
                                                   (buffer-string))))
                                     (kill-buffer (process-buffer process))
                                     (if (= (process-exit-status process) 0)
                                         (progn
                                           (message "finished: %s" command)
                                           (dired project-dir))
                                       (user-error (format "%s\n%s" command output))))))
      (set-process-filter proc #'comint-output-filter))))

(provide 'setup-vc)
