;; -*- lexical-binding: t -*-

;; Embark for actions
(use-package embark
  :demand
  :ensure (:host github :repo "oantolin/embark"
             ;; :files ("embark.el" "embark.texi" "embark-org.el")
             )
  :after minibuffer
  :hook ((embark-collect-mode . hl-line-mode))
  :bind (("M-SPC" . embark-act)
         ("M-S-SPC" . embark-select)
         ("M-s RET"  . embark-act)
         ;; ("s-o"      . embark-act)
         ("H-SPC"    . my/embark-select)
         ("C-S-SPC"  . my/embark-select)
         ;; ("C-c SPC"  . embark-act)
         ("M-*"      . embark-act-all)
         ("C-c RET"  . embark-dwim)
         ("S-<return>"  . embark-dwim)
         ("C-M-<return>"  . embark-dwim)
         ("C-h b"   . embark-bindings)
         ("C-h C-b" . describe-bindings)
         :map embark-general-map
         ("M-SPC"     . embark-select)
         :map minibuffer-local-completion-map
         ;; ("s-o"      . embark-act)
         ("C-c C-o"  . embark-export)
         ("M-s o"    . embark-export)
         ("H-SPC"    . embark-select)
         ("C->"      . embark-become)
         ("C-*"      . embark-act-all)
         ("M-*"      . embark-act-all)
         :map embark-collect-mode-map
         ("H-SPC" . embark-select)
         ("m"        . embark-select)
         ("o"        . embark-act)
         ("M-*"      . embark-act-all)
         ("M-t"      . toggle-truncate-lines)
         :map embark-file-map
         ("S"        . sudo-find-file)
         ("4"        . find-file-other-window)
         ("5"        . find-file-other-frame)
         ("C-="      . diff)
         ("X"        . embark-drag-and-drop)
         ("U"        . pastebin-file)
         ("M-U"      . 0x0-upload-file)
         :map embark-buffer-map
         ("d"        . diff-buffer-with-file) ;FIXME
         ("l"        . eval-buffer)
         ("4"        . switch-to-buffer-other-window)
         ("5"        . switch-to-buffer-other-frame)
         ("C-="      . diff-buffers)
         ("U"        . pastebin-buffer)
         ("M-U"      . 0x0-dwim)
         :map embark-bookmark-map
         ("4"        . bookmark-jump-other-window)
         ("5"        . bookmark-jump-other-frame)
         :map embark-region-map
         ("U"        . pastebin-buffer)
         ("M-U"      . 0x0-dwim)
         :map embark-url-map
         ("z"        . qrencode-string)
         ("B"        . eww)
         ("M-m"      . browse-url-mpv-enqueue)
         ("m"        . browse-url-mpv))
  :config
  
  (cl-defun embark--verbose-indicator-section-bindings
      (&key bindings &allow-other-keys)
    "Format the BINDINGS section for the indicator buffer."
    (let* ((max-width (apply #'max (cons 0 (mapcar (lambda (x)
                                                     (string-width (car x)))
                                                   bindings))))
           (fmt (format "%%-%ds" (1+ max-width)))
           (num-cols (floor (frame-width) (+ max-width 38)))
           (col 0) (result nil))
      (dolist (binding bindings (string-join (nreverse result)))
        (setq col (1+ col))
        (let ((cmd (caddr binding)))
          (unless (embark--verbose-indicator-excluded-p cmd)
            (let* ((keys (format fmt (car binding)))
                   (doc (embark--function-doc cmd))
                   (doc (and (stringp doc)
                             (truncate-string-to-width doc 38))))
              (push (format "%s%-38s " keys
                            (propertize
                             (car (split-string (or doc "") "\n"))
                             'face 'embark-verbose-indicator-documentation))
                    result)
              (when (= (mod col num-cols) 0) (push "\n" result) (setq col 0))))))))

  (setq embark-keymap-prompter-key "'")
  (setq embark-cycle-key "SPC")
  (setq embark-quit-after-action
        '((kill-buffer . nil)
          (delete-file . nil)
          (delete-directory . nil)
          (copy-file . nil)
          (rename-file . nil)
          (make-directory . nil)
          (t . t)))
  ;; Use Embark instead of `describe-prefix-bindings'
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Embark indicators
  ;; (mapc (lambda (ind) (add-hook 'embark-indicators ind)))
  (setq embark-indicators
        '(;; embark-mixed-indicator
          embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  
  (setq embark-mixed-indicator-delay 0.75)
  ;; (setq embark-verbose-indicator-display-action
  ;;       '(display-buffer-at-bottom
  ;;         (window-height . (lambda (win) (fit-window-to-buffer
  ;;                                    win (floor (frame-height) 
  ;;                                               3))))))

  (setq embark-verbose-indicator-display-action
        `(display-buffer-in-side-window
          (side . bottom)
          (window-height . fit-window-to-buffer)
          (body-function . ,(lambda (win)
                              (with-selected-window win
                               (and mode-line-format
                                (setq-local mode-line-format nil)))))))
  (setf (alist-get 'kill-buffer embark-pre-action-hooks) nil)

  ;; Selection
  (defun my/embark-select ()
    (interactive)
    (prog1 (embark-select)
      (if (minibufferp)
          (when (bound-and-true-p vertico-mode)
            (vertico-next))
        (call-interactively #'next-line))))

  ;; Drag and drop
  (defun embark-drag-and-drop (file)
    (interactive "fDrag and drop: ")
    (start-process "dragon" nil "dragon" (expand-file-name file)))

  (defun embark-attach-file (file)
    "Attach FILE to an  email message.
The message to which FILE is attached is chosen as for `gnus-dired-attach`,
that is: if no message buffers are found a new email is started; if some
message buffer exist you are asked whether you want to start a new email
anyway, if you say no and there is only one message buffer the attachements
are place there, otherwise you are prompted for a message buffer."
    (interactive "fAttach: ")
    (gnus-dired-attach (list file)))
  (define-key embark-file-map (kbd "C-a") #'embark-attach-file)
  
  ;; Extra embark actions
  (defun my/embark-share-file (file)
    "Share file via personal fileserver."
    (let* ((share-urls (alist-get 'share my-server-url-alist))
           (ssh-url (format (car share-urls) (file-name-nondirectory file)))
           (web-url (format (cadr share-urls) (file-name-nondirectory file))))
      (copy-file file ssh-url)
      (message "Copied %s to %s." file web-url)
      (kill-new web-url)))
  (define-key embark-file-map (kbd "M-S") #'my/embark-share-file)

  ;; Dummy function, will be overridden by running `embark-around-action-hooks'
  (defun my/embark-prefix-window () (interactive))

  (setf (alist-get 'my/embark-prefix-window embark-around-action-hooks)
        '(my/embark--call-prefix-action))

  (defvar-keymap my/window-prefix-map
    :doc "Keymap for various window-prefix maps"
    :suppress 'nodigits
    "o" #'ace-window-prefix
    "0" #'ace-window-prefix
    "1" #'same-window-prefix
    "2" #'my/split-window-below
    "3" #'my/split-window-right
    "4" #'other-window-prefix
    "5" #'other-frame-prefix
    "6" #'other-tab-prefix
    "t" #'other-tab-prefix)

  (setf (alist-get 'buffer embark-default-action-overrides) #'switch-to-buffer)
  (setf (alist-get 'file embark-default-action-overrides) #'find-file)
  (setf (alist-get 'bookmark embark-default-action-overrides) #'bookmark-jump)
  (setf (alist-get 'library embark-default-action-overrides) #'find-library)

  ;; Look up the key in `my/window-prefix-map' and call that function first.
  ;; Then run the default embark action.
  (cl-defun my/embark--call-prefix-action (&rest rest &key run type &allow-other-keys)
    (when-let ((cmd (keymap-lookup
                     my/window-prefix-map
                     (key-description (this-command-keys-vector)))))
      (funcall cmd))
    (plist-put rest :action (embark--default-action type))
    (apply run rest))

  (map-keymap (lambda (key cmd)
                (keymap-set embark-general-map (key-description (make-vector 1 key))
                            #'my/embark-prefix-window))
              my/window-prefix-map)

    ;; Embark actions for this buffer/file
    (defun embark-target-this-buffer-file ()
      (cons 'this-buffer-file (buffer-name)))

    (add-to-list 'embark-target-finders #'embark-target-this-buffer-file 'append)
    (unless (member 'embark-target-this-buffer-file embark-target-finders)
      (setq embark-target-finders
            (append (butlast embark-target-finders 2)
                    '(embark-target-this-buffer-file)
                    (last embark-target-finders 2))))

    ;; Embark keymaps
    (add-to-list 'embark-keymap-alist
                 '(project-file . embark-file-map))
    (add-to-list 'embark-exporters-alist
                 '(project-file . embark-export-dired))
    
    ;; Commands to act on current file or buffer.
    (defvar this-buffer-file-map
      (let ((map (make-sparse-keymap)))
        (pcase-dolist
            (`(,key ,command) 
             '(("l" load-file)
               ("b" byte-compile-file)
               ("S" sudo-find-file)
               ("r" rename-file-and-buffer)
               ("d" my/diff-buffer-dwim)
               ("=" ediff-buffers)
               ("C-=" ediff-files)
               ("!" shell-command)
               ("&" async-shell-command)
               ("x" embark-open-externally)
               ("C-a" embark-attach-file)
               ("c" copy-file)
               ("4" clone-indirect-buffer-other-window)
               ("k" kill-buffer)
               ;; ("l" org-store-link)
               ("#" recover-this-file)
               ("z" bury-buffer)
               ("|" embark-shell-command-on-buffer)
               ("U" pastebin-buffer)
               ("M-U" 0x0-dwim)
               ("U" pastebin-buffer)
               ("g" revert-buffer-quick)
               ("u" rename-uniquely)
               ("n" clone-buffer)
               ("M-S" my/embark-share-file)
               ("t" toggle-truncate-lines)))
          (define-key map (kbd key) command))
        map))

    (add-to-list 'embark-keymap-alist '(this-buffer-file . this-buffer-file-map))
    ;; (cl-pushnew 'revert-buffer embark-allow-edit-actions)
    ;; (cl-pushnew 'rename-file-and-buffer embark-allow-edit-actions)
    
    ;; Embark-collect display
    (setf (alist-get "^\\*Embark \\(?:Export\\|Collect\\).*\\*"
                     display-buffer-alist nil nil 'equal)
            '((display-buffer-in-direction)
              (window-height . (lambda (win) (fit-window-to-buffer
                                         win
                                         (floor (frame-height) 3))))
              (direction . below)
              (window-parameters . ((split-window . #'ignore)))))

    (setf (alist-get "^\\*Embark \\(?:Export\\|Collect\\).*Variables\\*"
                     display-buffer-alist nil nil 'equal)
            '((display-buffer-in-side-window)
              (body-function . (lambda (win) (select-window win)))
              (window-width . 74)
              (side . right)
              (slot . 5)
              (window-parameters . ((split-window . #'ignore)))))
    
    ;; Helm style prompter
    (defun with-minibuffer-keymap (keymap)
      (lambda (fn &rest args)
        (minibuffer-with-setup-hook
            (:append (lambda ()
                       (use-local-map
                        (make-composed-keymap keymap (current-local-map)))))
          (apply fn args))))

    (defvar embark-completing-read-prompter-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-<tab>") 'abort-recursive-edit)
        (define-key map (kbd "H-<tab>") 'abort-recursive-edit)
        map))

    (advice-add 'embark-completing-read-prompter :around
                (with-minibuffer-keymap embark-completing-read-prompter-map))
    
    (defun embark-act-with-completing-read (&optional arg)
      (interactive "P")
      (let* ((embark-prompter 'embark-completing-read-prompter)
             (embark-indicators '(embark-minimal-indicator)))
        (embark-act arg)))

    ;; Which-key style indicator
    (use-package which-key
      :after which-key
      :disabled
      :bind
      (:map minibuffer-local-completion-map
       ("C-o"      . embark-minimal-act)
       ("C-M-o"    . embark-minimal-act-noexit)
       :map completion-list-mode-map
       ("C-o"      . embark-minimal-act))
      :config
      (defun embark-minimal-act (&optional arg)
        (interactive "P")
        (let ((embark-indicators
               '(embark-which-key-indicator
                 embark-highlight-indicator
                 embark-isearch-highlight-indicator)))
          (embark-act arg)))
      
      (defun embark-minimal-act-noexit ()
        (interactive)
        (embark-minimal-act 4))

      ;; From the embark wiki
      (add-to-list 'embark-indicators #'embark-which-key-indicator)
      (defun embark-which-key-indicator ()
        "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
        (lambda (&optional keymap targets prefix)
          (if (null keymap)
              (which-key--hide-popup-ignore-command)
            (which-key--show-keymap
             (if (eq (caar targets) 'embark-become)
                 "Become"
               (format "Act on %s '%s'%s"
                       (plist-get (car targets) :type)
                       (embark--truncate-target (plist-get (car targets) :target))
                       (if (cdr targets) "…" "")))
             (if prefix
                 (pcase (lookup-key keymap prefix 'accept-default)
                   ((and (pred keymapp) km) km)
                   (_ (key-binding prefix 'accept-default)))
               keymap)
             nil nil t))))))

(use-package embark-consult
  :ensure (:host github :repo "oantolin/embark"
             :files ("embark-consult.el"))
  :after (embark consult)
  :demand
  :bind (:map embark-become-file+buffer-map
         ("m" . consult-bookmark)
         ("b" . consult-buffer)
         ("j" . consult-find)
         :map embark-consult-search-map
         ("f". consult-fd))
  :config
  
  (add-to-list
   'embark-exporters-alist
   '(consult-flymake-error . embark-export-flymake))
  
  (defun embark-export-flymake (_errors)
    (flymake-show-buffer-diagnostics)))

;;; Embark-Collect overlays
;; Disabled - don't need this with embark-live-mode (elm) active
(use-package embark
  :hook ((embark-collect-mode . my/embark-collect--live-setup))
  :disabled                             
  :config
   ;; Highlighting selections in embark-collect buffers
  (defvar-local my/embark-collect--overlay nil
    "Text overlay for embark-collect buffers.")

  (defun my/embark--live-completions-p ()
  "Determine whether current collection is for live completions."
  (and (derived-mode-p 'embark-collect-mode)
       (eq embark-collect--kind :completions)))

  (defun my/embark-collect--live-setup ()
    "Remove mode-line from live embark-collect buffers and set up
highlighting."
    ;; (when (my/embark--live-completions-p)
    ;;   (my/mode-line-hidden-mode 1))
    (setq my/embark-collect--overlay (make-overlay 1 1))
    (overlay-put my/embark-collect--overlay 'face 'highlight)
    (add-hook 'post-command-hook 'my/embark-collect--live-overlay-update nil t))

  (defun my/embark-collect--live-overlay-update ()
    "Update the overlay in the embark-collect buffer."
    (pcase embark-collect-view
      ('list (hl-line-mode 1))
      ('grid (when (and (overlayp my/embark-collect--overlay)
                        (get-text-property (point) 'mouse-face))
               (hl-line-mode 0)
               (let ((beg (previous-single-property-change
                           (if (eobp) (point-max) (1+ (point)))
                           'mouse-face nil (point-min)))
                     (end (next-single-property-change (point) 'mouse-face nil (point-max))))
                 (move-overlay my/embark-collect--overlay beg end)))))))

;;; Embark-avy
(use-package avy-embark-collect
  :disabled
  :after embark
  :bind (:map minibuffer-local-completion-map
              ("M-j" . avy-embark-collect-choose)
              ("M-RET" . avy-embark-collect-choose)
              ("C-M-o" . avy-embark-collect-act)
              ("C-M-j" . avy-embark-collect-act)))

;; Embark integration for Straight
(use-package embark
  :disabled
  :defer
  :config
  (use-package straight
    :defer
    :config
    (defvar embark-straight-map
      (let ((map (make-sparse-keymap)))
        (pcase-dolist
            (`(,key ,command) 
             '(("u" straight-visit-package-website)
               ("r" straight-get-recipe)
               ("i" straight-use-package)
               ("v" straight-visit-package-website)
               ("c" straight-check-package)
               ("F" straight-pull-package)
               ("f" straight-fetch-package)
               ("p" straight-push-package)
               ("n" straight-normalize-package)
               ("m" straight-merge-package)))
          (define-key map (kbd key) command))
        map))

    (add-to-list 'embark-keymap-alist '(straight . embark-straight-map))

    (add-to-list 'marginalia-prompt-categories '("recipe\\|package" . straight))))

;; Embark integration for Wallabag
(use-package embark
    :bind (:map embark-url-map ("R" . embark-wombag-url))
    :config
    (defun embark-wombag-url (url)
      "Add URL to my Wallabag reading list."
      (if (require 'setup-wallabag nil t)
          (wombag-add-entry url "")
        (message "Could not initialize Wallabag"))))

(provide 'setup-embark)
;; setup-embark.el ends here
