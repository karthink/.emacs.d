;;;** ELFEED  -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------
(use-package elfeed
  :commands (elfeed elfeed-update elfeed-search-bookmark-handler)
  :load-path ("~/.local/share/git/melpa/elfeed/"
              "~/.local/share/git/melpa/elfeed/web")
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
               "w" 'elfeed-search-yank)

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
    "d"      (elfeed-search-tag-as 'junk))
  (bind-key "f" (elfeed-search-tag-as 'later) elfeed-search-mode-map)
  (bind-key "u" (elfeed-search-tag-as 'unread) elfeed-search-mode-map)

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

    (defvar elfeed-search-filter-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "+") (lambda () (interactive) (my/elfeed-search-tag-filter "+")))
      (define-key map (kbd "-") (lambda () (interactive) (my/elfeed-search-tag-filter "-")))
      (define-key map (kbd "@") (lambda () (interactive) (my/elfeed-search-tag-filter "@")))
      (define-key map (kbd "RET") 'exit-minibuffer)
      (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
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
           "S"   'my/elfeed-search-tag-filter
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
            "B" 'elfeed-search-eww-open
            "S" 'my/elfeed-search-tag-filter)
  )

(use-package elfeed-goodies-split-pane
  :disabled
  :commands elfeed-goodies/split-search-show-entry
  :general
  (:states '(motion)
           :keymaps 'elfeed-search-mode-map
           "RET" 'elfeed-goodies/split-search-show-entry
           "gj" 'elfeed-goodies/split-show-next
           "gk" 'elfeed-goodies/split-show-prev)
)
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

