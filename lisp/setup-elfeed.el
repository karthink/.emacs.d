;;;** ELFEED
;;----------------------------------------------------------------------
(use-package elfeed
  :commands elfeed
  :load-path ("~/.local/share/git/elfeed/"
              "~/.local/share/git/elfeed/web")
  :config
  (setq-default elfeed-db-directory "~/.cache/emacs/elfeed"
                elfeed-save-multiple-enclosures-without-asking t
                elfeed-search-clipboard-type 'CLIPBOARD
                elfeed-search-filter "@1-week-ago #50 +unread "
                elfeed-show-entry-switch #'elfeed-display-buffer)
  ;;----------------------------------------------------------------------
  ;;*** Helper functions
  ;;----------------------------------------------------------------------

  (eval-when-compile
    (defmacro elfeed-search-show-entry-pre (&optional pre-action)
      "Macro to customize Elfeed behavior. Create a function that
calls `PRE-ACTION', then display an elfeed entry without
switching to it."
      `(lambda () (interactive)
         ,pre-action
         (save-current-buffer (call-interactively #'elfeed-search-show-entry))
         (select-window (previous-window))
         (unless elfeed-search-remain-on-entry (forward-line -1)))))

  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height))))) 
  
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

(defmacro elfeed-tag-selection-as (mytag)
  "Tag elfeed entry as MYTAG"
  `(lambda (&optional user-generic-p)
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (funcall (if (elfeed-tagged-p ,mytag entry)
                               #'elfeed-untag #'elfeed-tag)
                           entry ,mytag)
               do (elfeed-untag entry 'unread))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line)))))

(setq elfeed-feeds my-elfeed-feeds)
(eval-after-load 'evil-collection
  (evil-collection-elfeed-setup))

(defun elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit use-generic-p)))

(defun elfeed-search-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-search-browse-url use-generic-p)))

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
    "gj" (elfeed-search-show-entry-pre (progn (forward-line 1) (recenter)))
    "gk" (elfeed-search-show-entry-pre (progn (forward-line -1) (recenter)))
    "]]" (elfeed-search-show-entry-pre (progn (forward-line 1) (recenter)))
    "[[" (elfeed-search-show-entry-pre (progn (forward-line -1) (recenter)))
    "d"  (elfeed-tag-selection-as 'junk)
    "f"  (elfeed-tag-selection-as 'later))

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
            "M-RET" (elfeed-search-show-entry-pre nil))
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

