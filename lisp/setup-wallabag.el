;; -*- lexical-binding: t; -*-

(use-package wallabag
  :straight (:host github :repo "chenyanming/wallabag.el"
             :files ("*.el" "*.alist" "*.css"))
  :hook ((wallabag-post-html-render . my/wallabag-display-settings))
  :bind (:map wallabag-entry-mode-map
         ("SPC" . scroll-up-command)
         ("DEL" . scroll-down-command)
         ("S-SPC" . scroll-down-command)
         ("<" . beginning-of-buffer)
         (">" . end-of-buffer)
         :map wallabag-search-mode-map
         ("E" . my/switch-to-elfeed)
         ("u" . wallabag-unmark-at-point)
         ("G" . wallabag-request-new-entries)
         ("+" . wallabag-add-tags)
         ("M-RET" . wallabag-view))
  :config
  (use-package setup-reading
    :bind (:map wallabag-search-mode-map
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
           ("M-s u" . my/reader-browse-url))
    :config
    (add-hook 'wallabag-post-html-render-hook #'my/reader-center-images 'append))
  
  (defsubst wallabag-url (url)
      (wallabag-add-entry url ""))
  
  (use-package elfeed
    :bind (:map elfeed-search-mode-map
           ("R" . elfeed-post-to-wallabag)
           :map elfeed-show-mode-map
           ("R" . elfeed-post-to-wallabag))
    :config
    (defun elfeed-post-to-wallabag (entries)
      (interactive (list (pcase major-mode
                           ('elfeed-search-mode
                            (elfeed-search-selected))
                           ('elfeed-show-mode
                            (list elfeed-show-entry)))))
      (dolist (entry (ensure-list entries))
        (wallabag-url (elfeed-entry-link entry)))))

  (use-package embark
    :bind (:map embark-url-map ("R" . wallabag-url)))
  
  (defun my/switch-to-elfeed ()
    (interactive)
    (if-let ((buf (get-buffer "*elfeed-search*")))
        (switch-to-buffer buf)
      (if (featurep 'elfeed)
          (elfeed)
        (message "Elfeed not available."))))
  
  (defun wallabag-search-alt-view (&optional lines)
    "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them"
    (lambda (times)
      (interactive "p")
      (forward-line (* times (or lines 0)))
      (recenter)
      (let ((wallabag-show-entry-switch #'my/reader-display-buffer))
        (call-interactively #'wallabag-view))
      (when-let ((win (get-buffer-window "*wallabag-search*")))
        (select-window win)
        (setq-local other-window-scroll-buffer
                    (get-buffer "*wallabag-entry*")))))
  
  
  (defun my/wallabag-display-settings ()
    (when (require 'visual-fill-column nil t)
                    (setq-local visual-fill-column-center-text t
                                visual-fill-column-width (1+ shr-width))
                    (visual-line-mode 1)
                    (visual-fill-column-mode 1))
    (setq-local line-spacing 0.2)
    (shr-heading-setup-imenu))
  (setq wallabag-show-entry-switch #'switch-to-buffer)
  (setq wallabag-host "https://read.karthinks.com")
  (setq wallabag-username (auth-source-pass-get "login" "www/read.karthinks.com"))
  (setq wallabag-password (auth-source-pass-get 'secret "www/read.karthinks.com"))
  (setq wallabag-clientid (auth-source-pass-get "client_id" "api/wallabag/qutescript"))
  (setq wallabag-secret (auth-source-pass-get 'secret "api/wallabag/qutescript"))
  ;; optional, auto refresh token, token should refresh every hour
  ;; (run-with-timer 0 3540 'wallabag-request-token) 
  (setq wallabag-db-file (expand-file-name "wallabag.sqlite" user-cache-directory)))

(provide 'setup-wallabag)
