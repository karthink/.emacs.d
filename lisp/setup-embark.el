;; -*- lexical-binding: t -*-

;; Embark for actions
(use-package embark
  :demand
  :straight t
  :after minibuffer
  :bind (("M-s RET"  . embark-act)
         ("s-o"      . embark-act)
         ("s-C-o"    . embark-act-noexit)
         ("H-SPC" . embark-act)
         ("C-h b" . embark-bindings)
         ("C-h C-b" . describe-bindings)
         :map minibuffer-local-completion-map
         ("s-o"      . embark-act)
         ("s-C-o"    . embark-act-noexit)
         ("C-o"      . embark-minimal-act)
         ("C-M-o"    . embark-minimal-act-noexit)
         ("C-c C-o"  . embark-export)
         ("M-s o"    . embark-export)
         ("H-SPC"    . embark-act)
         ("C->"      . embark-become)
         :map completion-list-mode-map
         ("C-o"      . embark-minimal-act)
         :map embark-collect-mode-map
         ("H-SPC" . embark-act)
         ("o"        . embark-act)
         ("O"        . embark-act-noexit)
         ("C-o"      . embark-act)
         ("M-t"      . toggle-truncate-lines)
         ("M-q"      . embark-collect-toggle-view)
         :map embark-file-map
         ("j"        . my/find-file-dir)
         ("S"        . sudo-find-file)
         ("4"        . find-file-other-window)
         ("5"        . find-file-other-frame)
         ("C-="      . diff)
         :map embark-buffer-map
         ("d"        . diff-buffer-with-file) ;FIXME
         ("l"        . eval-buffer)
         ("4"        . switch-to-buffer-other-window)
         ("5"        . switch-to-buffer-other-frame)
         ("C-="      . diff-buffers)
         :map embark-bookmark-map
         ("4"        . bookmark-jump-other-window)
         ("5"        . bookmark-jump-other-frame)
         :map embark-url-map
         ("f"        . browse-url-firefox)
         ("m"        . browse-url-umpv)
         ("C-M-m"      . browse-url-umpv-last)
         ("M"        . browse-url-mpv))
  :config
  (setq embark-cycle-key (kbd "s-o"))
  (setq embark-quit-after-action t)
  ;; Use Embark instead of `describe-prefix-bindings'
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Embark indicators
  (setq embark-indicators '(embark-which-key-indicator
                            ;; embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))
  ;; (add-to-list 'embark-indicators 'embark-mixed-indicator)
  ;; (setq embark-mixed-indicator-delay 0.5)
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom
          (window-height . (lambda (win) (fit-window-to-buffer
                                     win (floor (frame-height) 
                                                3))))))
  (setf (alist-get 'kill-buffer embark-pre-action-hooks) nil)

  ;; Utility commands
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
  
  (defun embark-act-noexit ()
    (interactive)
    (embark-act 4))

  (defun my/find-file-dir (file)
    (interactive (list (read-file-name "Jump to dir of file: ")))
                         (dired (file-name-directory file)))
  
  ;; Extra embark actions
  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))
    
    (defmacro my/embark-split-action (fn split-type) 
      `(defun ,(intern (concat "my/embark-"
                               (symbol-name fn)
                               "-"
                               (car (last  (split-string
                                            (symbol-name split-type) "-"))))) ()
         (interactive)
         (funcall #',split-type)
         (call-interactively #',fn))))

    (define-key embark-file-map (kbd "o") (my/embark-ace-action find-file))
    (define-key embark-file-map (kbd "2") (my/embark-split-action find-file my/split-window-below))
    (define-key embark-file-map (kbd "3") (my/embark-split-action find-file my/split-window-right))
    (define-key embark-buffer-map (kbd "o") (my/embark-ace-action switch-to-buffer))
    (define-key embark-buffer-map (kbd "2") (my/embark-split-action switch-to-buffer my/split-window-below))
    (define-key embark-buffer-map (kbd "3") (my/embark-split-action switch-to-buffer my/split-window-right))
    (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
    (define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump my/split-window-below))
    (define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump my/split-window-right))
    (define-key embark-file-map (kbd "U") '0x0-upload-file)
    (define-key embark-region-map (kbd "U") '0x0-upload-text)
    (define-key embark-buffer-map (kbd "U") '0x0-dwim)

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
    
    (embark-define-keymap this-buffer-file-map
      "Commands to act on current file or buffer."
      ("l" load-file)
      ("b" byte-compile-file)
      ("S" sudo-find-file)
      ("r" rename-file-and-buffer)
      ("d" my/diff-buffer-dwim)
      ("=" ediff-buffers)
      ("C-=" ediff-files)
      ("!" shell-command)
      ("&" async-shell-command)
      ("x" consult-file-externally)
      ("C-a" mml-attach-file)
      ("c" copy-file)
      ("k" kill-buffer)
      ("#" recover-this-file)
      ("z" bury-buffer)
      ("|" embark-shell-command-on-buffer)
      ;; ("l" org-store-link)
      ("U" 0x0-dwim)
      ("g" revert-buffer))

    (add-to-list 'embark-keymap-alist '(this-buffer-file . this-buffer-file-map))
    ;; (cl-pushnew 'revert-buffer embark-allow-edit-actions)
    ;; (cl-pushnew 'rename-file-and-buffer embark-allow-edit-actions)
    
    (use-package helpful
      :defer
      :bind (:map embark-become-help-map
                  ("f" . helpful-callable)
                  ("v" . helpful-variable)
                  ("C" . helpful-command)))

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
             (act (propertize "Act" 'face 'highlight))
             (embark-indicators '(embark-minimal-indicator)))
        (embark-act arg)))

    ;; Which-key style indicator
    (use-package which-key
      :after which-key
      :config
      ;; From the embark wiki
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
             nil nil t)))))
    
    ;; Vertico highlight indicator
    (use-package vertico 
      :defer
      :config
      (defun embark-vertico-indicator ()
        (let ((fr face-remapping-alist))
          (lambda (&optional keymap _targets prefix)
            (when (bound-and-true-p vertico--input)
              (setq-local face-remapping-alist
                          (if keymap
                              (cons '(vertico-current . embark-target) fr)
                            fr))))))
      (add-to-list 'embark-indicators #'embark-vertico-indicator)))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand
  :bind (:map embark-become-file+buffer-map
         ("m" . consult-bookmark)
         ("b" . consult-buffer)
         ("j" . consult-find))
  :config
  
;; (setq embark-collect-initial-view-alist
;;       '((file           . list)
;;         (project-file   . list)
;;         (virtual-buffer . list)
;;         (buffer         . list)
;;         (consult-multi  . list)
;;         (consult-location . list)
;;         (consult-compile-error . list)
;;         (consult-flymake-error . list)
;;         (symbol         . grid)
;;         (command        . grid)
;;         (imenu          . grid)
;;         (line           . list)
;;         (xref-location  . list)
;;         (kill-ring      . zebra)
;;         (face           . list)
;;         (t              . grid)))

  (add-to-list
   'embark-exporters-alist
   '(consult-flymake-error . embark-export-flymake))
  
  (defun embark-export-flymake (_errors)
    (flymake-show-buffer-diagnostics))
  
  (dolist (pair '((consult-fd . list)))
    (add-to-list 'embark-collect-initial-view-alist
                 pair))
  :bind (:map embark-file-map
              ("x" . consult-file-externally)))

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

(provide 'setup-embark)
;; setup-embark.el ends here
