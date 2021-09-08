;;;** ELFEED  -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------
(use-package elfeed
  :commands (elfeed elfeed-update elfeed-search-bookmark-handler)
  :load-path ("~/.local/share/git/elfeed/"
              "~/.local/share/git/elfeed/web")
  :config
  (setq-default elfeed-db-directory "~/.cache/emacs/elfeed"
                elfeed-save-multiple-enclosures-without-asking t
                elfeed-search-clipboard-type 'CLIPBOARD
                elfeed-search-filter "#50 +unread "
                elfeed-show-entry-switch #'elfeed-display-buffer)
  ;;----------------------------------------------------------------------
  ;;*** Helper functions
  ;;----------------------------------------------------------------------


  (defun elfeed-search-show-entry-pre (&optional lines) 
  "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
    (lambda (times) 
      (interactive "p")
      (forward-line (* times (or lines 0)))
      (recenter)
      (call-interactively #'elfeed-search-show-entry)
      (select-window (previous-window))
      (unless elfeed-search-remain-on-entry (forward-line -1))))

  (general-def :keymaps 'elfeed-search-mode-map
               :states  '(normal visual)
               "gj" (elfeed-search-show-entry-pre 1)
               "gk" (elfeed-search-show-entry-pre -1)
               "]]" (elfeed-search-show-entry-pre 1)
               "[[" (elfeed-search-show-entry-pre -1))
  (general-def :keymaps 'elfeed-search-mode-map
               "M-RET" (elfeed-search-show-entry-pre)
               "w" 'elfeed-search-yank
               "M-n" (elfeed-search-show-entry-pre 1)
               "M-p" (elfeed-search-show-entry-pre -1))
  
  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-in-direction)
                         (direction . above)
                         (window-height . 0.7)))
    ;; (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height))))
    ) 
  
  (advice-add 'elfeed-kill-buffer :after 'delete-window-if-not-single)
  (advice-add 'elfeed-show-entry :after (defun elfeed-visual-lines-a (_entry)
                                          (visual-line-mode 1)))

  (defun elfeed-scroll-up-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-up-command arg)
        (error (elfeed-show-next)))))

  (defun elfeed-scroll-down-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-down-command arg)
        (error (elfeed-show-prev)))))

  (defun elfeed-search-tag-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
MYTAG"
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))

  (general-def :keymaps 'elfeed-search-mode-map
    :states  '(normal visual emacs)
    "f"      (elfeed-search-tag-as 'later)
    "d"      (elfeed-search-tag-as 'junk)
    "l"      (elfeed-search-tag-as 'listen))
  (bind-key "f" (elfeed-search-tag-as 'later) elfeed-search-mode-map)
  (bind-key "u" (elfeed-search-tag-as 'unread) elfeed-search-mode-map)
  (bind-key "l" (elfeed-search-tag-as 'listen) elfeed-search-mode-map)

  (defun elfeed-show-tag-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
MYTAG"
    (lambda ()
      "Toggle a tag on an Elfeed entry being displayed"
      (interactive)
      (elfeed-tag elfeed-show-entry mytag)
      (elfeed-search-update-entry elfeed-show-entry)
      (unless elfeed-search-remain-on-entry (elfeed-show-next))))

  (general-def :keymaps 'elfeed-show-mode-map
    :states '(normal visual emacs)
    "f"     (elfeed-show-tag-as 'later)
    "d"     (elfeed-show-tag-as 'junk))

  (bind-key "f" (elfeed-show-tag-as 'later)  elfeed-show-mode-map)
  (bind-key "u" (elfeed-show-tag-as 'unread) elfeed-show-mode-map)
  (bind-key "l" (elfeed-show-tag-as 'listen) elfeed-show-mode-map)
  
  (setq elfeed-feeds my-elfeed-feeds)

  (defun elfeed-show-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit use-generic-p)
      (add-hook 'eww-after-render-hook 'eww-readable nil t)))

  (defun elfeed-search-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-search-browse-url use-generic-p)
      (add-hook 'eww-after-render-hook 'eww-readable nil t)))

  (defun elfeed-mpv-url (&optional use-single-p)
    "Visit the current entry in umpv or (with prefix arg
USE-SINGLE-P) with mpv."
    (interactive "P")
    (let ((browse-url-browser-function (if use-single-p
                                           (lambda (url &optional _) (browse-url-umpv url t))
                                           #'browse-url-umpv)))
      (pcase major-mode
        ('elfeed-search-mode (elfeed-search-browse-url))
        ('elfeed-show-mode (elfeed-show-visit)))))

  (bind-key "m" #'elfeed-mpv-url elfeed-search-mode-map)
  (bind-key "m" #'elfeed-mpv-url elfeed-show-mode-map)

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "The Linux Experiment"
                                :entry-title "Linux News"
                                :add '(news listen)))
  
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "Skill Up"
                                :entry-title "This Week"
                                :add '(news listen)))

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "TechLinked"
                                :add '(news listen)))

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "ACG"
                                :entry-title "\\(?:Roundup\\|Gaming News\\)"
                                :add '(news listen)))
  
  (add-hook 'elfeed-new-entry-hook
            #'elfeed-declickbait)

  (defun elfeed-declickbait (entry)
    (when-let* ((feed (elfeed-entry-feed entry))
                (feed-url (elfeed-feed-url feed))
                (youtube-p (string-match-p "youtube\\.com" feed-url))
                (title (elfeed-entry-title entry)))
      (setf (elfeed-meta entry :title)
            (let* ((title-no-bang (if (string-match "!+\\?*\\s-*$" title)
                                      (substring title 0 (- (length (match-string 0))))
                                    title))
                   (words (split-string title-no-bang))
                   (case-fold-search nil))
              (string-join (mapcar (lambda (word)
                                     (cond
                                      ((member word '("AND" "OR" "IF" "ON" "IT"
                                                      "TO" "A" "OF" "THE" "VS"
                                                      "IN" "FOR" "AN" "WAS" "IS"))
                                       (downcase word))
                                      ((member word '("WE" "DAY" "HOW" "WHY" "NOW"
                                                      "CUP" "OLD" "NEW" "MY" "TOO"
                                                      "GOT" "GET" "EYE"))
                                       (capitalize word))
                                      ((and (> (length word) 3)
                                            (string-match-p "\\`[A-Z\\.\\?\\!\\':,’\\-]*\\'"
                                                            word))
                                       (capitalize word))
                                      (t word)))
                                   words)
                           " ")))))
;; (dolist (entry (elfeed-feed-entries (with-current-buffer "*elfeed-search*"
;;   (elfeed-entry-feed-id (elfeed-search-selected :ignore-region)))))
;;   (elfeed-declickbait entry))

  (defun my/elfeed-search-by-day (dir)
    (lambda (&optional arg)
      (interactive "p")
      (let* ((entry (elfeed-search-selected :ignore-region))
             (this-day (if entry
                           (elfeed-entry-date entry)
                         (current-time)))
             (next-day (time-add this-day (days-to-time (or arg 1))))
             (next-next-day (time-add next-day (days-to-time (or arg 1))))
             (prev-day (time-subtract this-day (days-to-time (or arg 1))))
             from to)
        (pcase dir
          ('next (setq from next-day
                       to   next-next-day))
          ('prev (setq from prev-day
                       to   this-day))
          (_     (setq from this-day
                       to   next-day)))
        (setq elfeed-search-filter (concat (replace-regexp-in-string
                                            "@[^[:space:]]*" ""
                                            elfeed-search-filter)
                                           "@"  (elfeed-search-format-date from)
                                           "--" (elfeed-search-format-date to)))
        (elfeed-search-update :force))))
  
  (define-key elfeed-search-mode-map (kbd ".") (my/elfeed-search-by-day 'this))
  (define-key elfeed-search-mode-map (kbd "<") (my/elfeed-search-by-day 'next))
  (define-key elfeed-search-mode-map (kbd ">") (my/elfeed-search-by-day 'prev))
        
  (use-package wallabag
    :config 
    (defun elfeed-post-to-wallabag ()
      "Post current entry (or entry at point) to Wallabag"
      (interactive)
      (let ((entries (elfeed-search-selected))
            (links (pcase major-mode
                     ('elfeed-show-mode
                      (list (elfeed-entry-link elfeed-show-entry)))
                     ('elfeed-search-mode
                      (mapcar #'elfeed-entry-link (elfeed-search-selected))))))
        (dolist (link links nil)
          (wallabag-post-entry link)
          (when (eq major-mode 'elfeed-search-mode)
            (elfeed-untag entries 'unread)
            (mapc #'elfeed-search-update-entry entries)
            (unless (or elfeed-search-remain-on-entry
                        (use-region-p))
              (forward-line))))))
    :bind (:map elfeed-show-mode-map
                ("R" . elfeed-post-to-wallabag)
           :map elfeed-search-mode-map
                ("R" . elfeed-post-to-wallabag)))
  
    (defvar elfeed-search-filter-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "+") (lambda () (interactive) (my/elfeed-search-tag-filter "+")))
      (define-key map (kbd "-") (lambda () (interactive) (my/elfeed-search-tag-filter "-")))
      (define-key map (kbd "@") (lambda () (interactive) (my/elfeed-search-tag-filter "@")))
      (define-key map (kbd "RET") 'exit-minibuffer)
      (define-key map (kbd "<tab>") 'minibuffer-force-complete-and-exit)
      (define-key map (kbd "C-g") 'abort-recursive-edit)
      map)
    "Keymap active when entering filter terms in elfeed")

  (defun elfeed-search-live-filter ()
    "Filter the elfeed-search buffer as the filter is written."
    (interactive)
    (unwind-protect
        (let ((elfeed-search-filter-active :live))
          (setq elfeed-search-filter
                (read-from-minibuffer "Filter: " elfeed-search-filter elfeed-search-filter-map)))
      (elfeed-search-update :force)))

  ;; (defun elfeed-search-live-filter ()
  ;;   "Filter the elfeed-search buffer as the filter is written."
  ;;   (interactive)
  ;;   (let* ((keymap (copy-keymap minibuffer-local-map))
  ;;          (prompt "Filter: ")
  ;;          (current-query elfeed-search-filter)
  ;;          (completions
  ;;           (mapcan (lambda (tag) (list (substring-no-properties (concat "+" (symbol-name tag)))
  ;;                                  (substring-no-properties (concat "-" (symbol-name tag)))))
  ;;                   (elfeed-db-get-all-tags)))
  ;;          (minibuffer-completion-table
  ;;           (completion-table-dynamic
  ;;            (lambda (string)
  ;;              (cond
  ;;               ((string-match "\\(+[^[:space:]]*\\|-[^[:space:]]*\\)$" string)
  ;;                (all-completions (match-string-no-properties 1 string) completions))
  ;;               (t (list string)))))))
      
  ;;     (define-key keymap (kbd "TAB") 'minibuffer-complete)
  ;;     (let ((history-delete-duplicates t))
  ;;       (read-from-minibuffer prompt elfeed-search-filter keymap))))

  (defun my/elfeed-search-tag-filter (plus-minus)
    "Filter `elfeed' by tags using completion."
    (let ((elfeed-search-filter-active nil))
      (if (equal plus-minus "@")
          (insert (format "@%s "
                          (replace-regexp-in-string
                           " +" "-"
                           (replace-regexp-in-string
                            " to " "--" 
                            (read-from-minibuffer "Date range: ")))))
        (let* ((db-tags (elfeed-db-get-all-tags))
               (tag (completing-read (format "%s %s" elfeed-search-filter plus-minus) db-tags nil t)))
          (insert (concat plus-minus tag " ")))))
    (elfeed-search-update :force))
  ;;----------------------------------------------------------------------
  ;; Faces
  ;;----------------------------------------------------------------------
  ;; (defface elfeed-entry-read-later
  ;;   '((t :foreground "#b48ead"))
  ;;   "Marks a 'read-later' Elfeed entry.")
  ;; (push '(later elfeed-entry-read-later)
  ;;       elfeed-search-face-alist)
  :general
  (:states '(normal visual)
           :keymaps 'elfeed-search-mode-map
           "SPC" 'space-menu
           "gO"  'elfeed-search-eww-open
           "c"   'elfeed-search-clear-filter
           "gy"  'elfeed-search-yank
           )

  (:states '(normal visual)
           :keymaps 'elfeed-show-mode-map
           "SPC"   'elfeed-scroll-up-command
           "S-SPC" 'elfeed-scroll-down-command
           "gO"    'elfeed-show-eww-open
           "gy"    'elfeed-show-yank
           "<tab>" 'elfeed-show-next-link)
  (:keymaps 'elfeed-show-mode-map
            "SPC" 'elfeed-scroll-up-command
            "S-SPC" 'elfeed-scroll-down-command
            "B" 'elfeed-show-eww-open)
  (:keymaps 'elfeed-search-mode-map
            "B" 'elfeed-search-eww-open)
  )

(use-package wallabag
  :load-path "~/.local/share/git/wallabag"
  :commands wallabag-post-entry
  :config
  (setq wallabag-data-dir "~/.cache/wallabag")
  (let ((creds (split-string (shell-command-to-string
                              "pass show api/wallabag/qutescript"))))
    (setq wallabag-credentials
          `((client-secret  . ,(nth 0 creds))
            (client-id      . ,(nth 2 creds))
            (host           . ,(nth 4 creds))))))

;; Org-Link handling for Elfeed buffers
(use-package ol
  :after (elfeed org)
  :config
  (org-link-set-parameters "elfeed"
                           :follow #'org-elfeed-open
                           :store  #'org-elfeed-store-link)
  
  (defun org-elfeed-store-link ()
    "Store a link to an Elfeed entry."
    (when-let* ((entry (pcase major-mode 
                         ('elfeed-show-mode elfeed-show-entry)
                         ('elfeed-search-mode (elfeed-search-selected :ignore-region))))
           (entry-id (elfeed-entry-id entry))
           (title (elfeed-entry-title entry)))
      (let ((link))
        (org-link-store-props :type "elfeed")
        (setq link (concat "elfeed:id:" (car entry-id) "@@" (cdr entry-id)))
        (org-add-link-props :link link :description title)
        link)))

  (defun org-elfeed-open (link)
    "Follow an elfeed entry specified by LINK."
    (require 'elfeed)
    (let* ((url+title (split-string (substring link 3) "@@"))
                (url (nth 0 url+title))
                (title (nth 1 url+title))
                (entry (elfeed-db-get-entry (cons url title))))
      (elfeed-show-entry entry))))

;;----------------------------------------------------------------------
;;*** Autotagging setup
;;----------------------------------------------------------------------
(use-package elfeed-custom
  :disabled
  :config
  (progn 
    ;; Mark all YouTube entries
    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :feed-url "youtube\\.com"
                                  :add '(video youtube)))
    ;; Avoiding tagging old entries as unread:

    ;; Entries older than 2 weeks are marked as read
    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :before "2 weeks ago"
                                  :remove 'unread))

    ;; Or building your own subset feeds:
    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :feed-url "example\\.com"
                                  :entry-title '(not "something interesting")
                                  :add 'junk
                                  :remove 'unread))))
;; Use `M-x elfeed-apply-hooks-now' to apply elfeed-new-entry-hook to all
;; existing entries. Otherwise hooks will only apply to new entries on
;; discovery.
;; ----------------------------------------------------------------------

(provide 'setup-elfeed)

;; (defun elfeed-search-eww-open (&optional use-generic-p)
;;   "open with eww"
;;   (interactive "P")
;;   (let ((entries (elfeed-search-selected)))
;;     (cl-loop for entry in entries
;;              do (elfeed-untag entry 'unread)
;;              when (elfeed-entry-link entry)
;;              do (eww-browse-url it t))
;;     (mapc #'elfeed-search-update-entry entries)
;;     (unless (use-region-p) (forward-line))))

;; For quick tagging
;; (defvar my-feeds nil)
;; (let ((tags '("always" "rare" "sometimes" "junk"))
;;       history)
;;   (dolist (feed elfeed-feeds)
;;     (let ((choices (completing-read-multiple (format "%s: " feed) tags nil nil nil)))
;;       (add-to-list 'my-feeds (append (list feed) (mapcar 'make-symbol choices)))
;;       (setq tags (delete-dups (append choices tags)))
;;       )
;;     ))

