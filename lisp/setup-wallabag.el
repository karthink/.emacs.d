;; -*- lexical-binding: t; -*-

(use-package wombag
  ;; :straight (:host github :repo "chenyanming/wallabag.el"
  ;;            :files ("*.el" "*.alist" "*.css"))
  :straight (:local-repo "~/.local/share/git/wombag/")
  :hook ((wombag-pre-html-render . my/wombag-display-settings))
  ;; :bind (
  ;;        ;; :map wombag-show-mode-map
  ;; ;;        ("SPC" . scroll-up-command)
  ;; ;;        ("DEL" . scroll-down-command)
  ;; ;;        ("S-SPC" . scroll-down-command)
  ;; ;;        ("<" . beginning-of-buffer)
  ;; ;;        (">" . end-of-buffer)
  ;;        ;; :map wombag-search-mode-map
  ;; ;;        ("u" . wombag-unmark-at-point)
  ;; ;;        ("G" . wombag-request-new-entries)
  ;; ;;        ("+" . wombag-add-tags)
  ;;        )
  :config
  (use-package setup-reading
    :after wombag-search
    :bind (:map wombag-search-mode-map
           ("RET" . my/reader-show)
           ("M-n" . my/reader-next)
           ("M-p" . my/reader-prev)
           ("q" . my/reader-quit-window)
           ("<" . my/reader-top)
           (">" . my/reader-bottom)
           ("SPC" . my/reader-scroll-up-command)
           ("S-SPC" . my/reader-scroll-down-command)
           ("DEL" . my/reader-scroll-down-command)
           ("M-s i" . my/reader-imenu)
           ("i" . my/reader-imenu)
           ("<tab>" . my/reader-push-button)
           ("M-RET" . wombag-search-show-entry)
           ("E" . my/switch-to-elfeed)
           ("M-s u" . my/reader-browse-url))
    :config
    (add-hook 'wombag-post-html-render-hook #'my/reader-center-images 'append))
  
  (add-hook 'wombag-show-mode-hook (lambda () (let ((pulse-flag))
                                             (unless (bobp)
                                               (pulse-momentary-highlight-one-line)))))
  
  (defsubst wombag-url (url)
      (wombag-add-entry url ""))
  
  (use-package elfeed
    :bind (:map elfeed-search-mode-map
           ("R" . elfeed-post-to-wombag)
           :map elfeed-show-mode-map
           ("R" . elfeed-post-to-wombag))
    :config
    (defun elfeed-post-to-wombag (entries)
      (interactive (list (pcase major-mode
                           ('elfeed-search-mode
                            (elfeed-search-selected))
                           ('elfeed-show-mode
                            (list elfeed-show-entry)))))
      (dolist (entry (ensure-list entries))
        (wombag-url (elfeed-entry-link entry)))))

  (use-package embark
    :bind (:map embark-url-map ("R" . wombag-url)))
  
  (defun my/switch-to-elfeed ()
    (interactive)
    (if-let ((buf (get-buffer "*elfeed-search*")))
        (switch-to-buffer buf)
      (if (featurep 'elfeed)
          (elfeed)
        (message "Elfeed not available."))))
  
  (defun wombag-search-alt-view (&optional lines)
    "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them"
    (lambda (times)
      (interactive "p")
      (forward-line (* times (or lines 0)))
      (recenter)
      (let ((wombag-show-entry-switch #'my/reader-display-buffer))
        (call-interactively #'wombag-view))
      (when-let ((win (get-buffer-window "*wombag-search*")))
        (select-window win)
        (setq-local other-window-scroll-buffer
                    (get-buffer "*wombag-entry*")))))
  
  
  (defun my/wombag-display-settings ()
    (when (require 'visual-fill-column nil t)
                    (setq-local visual-fill-column-center-text t
                                visual-fill-column-width (+ shr-width 6))
                    (setq-local shr-max-image-proportion 0.9)
                    (visual-line-mode 1)
                    (visual-fill-column-mode 1))
    ;; (shr-heading-setup-imenu)
    (setq-local line-spacing 0.08))
  ;; (setq wombag-show-entry-switch #'pop-to-buffer-same-window)
  (setq wombag-host "https://read.karthinks.com")
  (setq wombag-username (auth-source-pass-get "login" "www/read.karthinks.com"))
  (setq wombag-password (auth-source-pass-get 'secret "www/read.karthinks.com"))
  ;; (setq wallabag-clientid (auth-source-pass-get "client_id" "api/wallabag/qutescript"))
  ;; (setq wallabag-secret (auth-source-pass-get 'secret "api/wallabag/qutescript"))
  (setq wombag-client-id (auth-source-pass-get "client_id" "api/wallabag/qutescript"))
  (setq wombag-client-secret (auth-source-pass-get 'secret "api/wallabag/qutescript"))
  ;; optional, auto refresh token, token should refresh every hour
  ;; (run-with-timer 0 3540 'wallabag-request-token) 
  ;; (setq wombag-db-file (expand-file-name "test-wallabag.sqlite" "~/Desktop/"))
  (setq wombag-db-file (expand-file-name "wallabag.sqlite" user-cache-directory))
  )

(provide 'setup-wallabag)
