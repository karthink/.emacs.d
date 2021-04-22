;; -*- lexical-binding: t -*-
(use-package orderless
  :after setup-minibuffer
  :ensure t
  :demand
  :config
  (setq orderless-component-separator #'split-string-and-unquote)
  (setq completion-styles '(orderless partial-completion))
  (setq orderless-matching-styles
        '(orderless-regexp
          orderless-strict-leading-initialism)
        orderless-style-dispatchers
        '(my/orderless-flex-dispatcher
          my/orderless-literal-dispatcher
          my/orderless-initialism-dispatcher
          my/orderless-exclude-dispatcher))

  (defun my/orderless-flex-dispatcher (pattern _index _total)
    (when (or (string-suffix-p "`" pattern)
              (string-suffix-p "~" pattern))
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun my/orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun my/orderless-initialism-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-strict-full-initialism . ,(substring pattern 0 -1))))

  (defun my/orderless-exclude-dispatcher (pattern _index _total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))

  :bind (:map minibuffer-local-completion-map
              ("SPC" . self-insert-command)))

(use-package icomplete
  ;; :demand
  :disabled
  :after minibuffer
  :config 
  (setq icomplete-delay-completions-threshold 50
        icomplete-max-delay-chars 2
        icomplete-compute-delay 0.2
        ;; icomplete-separator " · "
        ;; icomplete-separator " ┆ "
        ;; icomplete-in-buffer t
        icomplete-separator (propertize " . " 'face 'shadow)
        icomplete-show-matches-on-no-input nil
        icomplete-hide-common-prefix nil
        icomplete-with-completion-tables t
        icomplete-prospects-height 1
        icomplete-tidy-shadowed-file-names t)

  ;; (define-key icomplete-minibuffer-map (kbd "TAB") 'icomplete-force-complete)
  ;; (define-key icomplete-minibuffer-map (kbd "TAB") 'minibuffer-complete)
  :bind (:map icomplete-minibuffer-map
             ( "C-n" . icomplete-forward-completions)
             ( "C-p" . icomplete-backward-completions)
             ( "RET" . icomplete-fido-ret)
             ( "C-k" . icomplete-fido-kill)
             ( "C-j" . (lambda () (interactive)
	        	    (if minibuffer--require-match
	        		(minibuffer-complete-and-exit)
	        	      (exit-minibuffer))))
             ( "DEL" . icomplete-fido-backward-updir)
             ;; ( "C-," . my/icomplete-toggle-completion-styles)
             )
  :config
  (defun icomplete-fido-kill ()
    "Kill line or current completion, like `ido-mode'.
If killing to the end of line make sense, call `kill-line',
otherwise kill the currently selected completion candidate.
Exactly what killing entails is dependent on the things being
completed.  If completing files, it means delete the file.  If
completing buffers it means kill the buffer.  Both actions
require user confirmation."
    (interactive)
    (let ((end (icomplete--field-end)))
      (if (< (point) end)
          (call-interactively 'kill-line)
        (let* ((all (completion-all-sorted-completions))
               (thing (car all))
               (action
                (pcase (icomplete--category)
                  (`buffer
                   (lambda ()
                     (kill-buffer thing)))
                  (`virtual-buffer
                   (lambda ()
                     (let* ((type (elt thing 0))
                            (instance (substring thing 1)))
                       (cond 
                        ((equal (- type consult--special-char) ?b)
                         (kill-buffer (get-buffer instance)))
                        ((equal (- type consult--special-char) ?f)
                         (let* ((dir (file-name-directory (icomplete--field-string)))
                                (path (expand-file-name thing dir)))
                           (when (yes-or-no-p (concat "Delete file " path "? "))
                             (delete-file path) t)))
                        ((or (equal (- type consult--special-char) ?m)
                             (equal (- type consult--special-char) ?v))
                         (when (yes-or-no-p (concat "Delete bookmark" instance "? "))
                           (bookmark-delete instance)
                           (bookmark-save)))))))
                  (`file
                   (lambda ()
                     (let* ((dir (file-name-directory (icomplete--field-string)))
                            (path (expand-file-name thing dir)))
                       (when (yes-or-no-p (concat "Delete file " path "? "))
                         (delete-file path) t)))))))
          (when (let (;; Allow `yes-or-no-p' to work and don't let it
                      ;; `icomplete-exhibit' anything.
                      (enable-recursive-minibuffers t)
                      (icomplete-mode nil))
                  (funcall action))
            (completion--cache-all-sorted-completions
             (icomplete--field-beg)
             (icomplete--field-end)
             (cdr all)))
          (message nil))))))

(use-package icomplete-vertical
  :disabled
  :ensure t
  :demand
  :after (minibuffer icomplete)
  :config
  (setq icomplete-vertical-prospects-height (/ (frame-height) 5))
  (icomplete-vertical-mode -1)
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle)))

(use-package consult
  :ensure t
  :after minibuffer
  :config
  (setq consult-narrow-key "<")
  (setq consult-line-numbers-widen t)
  (setq consult-preview-buffer nil)
  (setq consult-preview-mark nil)
  (setq consult-preview-line nil)
  (setq consult-preview-outline nil)
  (setq consult-preview-key nil)
  (setq consult-project-root-function (lambda () "Return current project root"
                                        (project-root (project-current))))
  (setq consult-find-command "fd --hidden -t f -t d -t l --follow ARG OPTS")
                           ;; "find . -not ( -wholename */.* -prune ) -ipath *ARG* OPTS"
  (defun consult-buffer-other-tab ()
  "Variant of `consult-buffer' which opens in other frame."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-tab))
    (consult-buffer)))
  
;;   (defun my/consult-find-multi-dir (dirlist &optional prompt initial)
;;     "Search for regexp with find in directories in DIRLIST with INITIAL input.

;; The find process is started asynchronously, similar to `consult-find'."
;;     (interactive "P")
;;     (let ((consult-find-command (concat "fd --hidden -t f -t d -t l --follow "
;;                                         (mapconcat (lambda (dir)
;;                                                      (concat "--search-path "
;;                                                              (file-name-as-directory dir)))
;;                                                    '("~/Documents" "~/Dropbox") " ")
;;                                         " ARG")))
;;       (consult--find (or prompt "Find: ") consult-find-command initial)))

  (use-package org
    :bind (:map org-mode-map
                ("C-c C-j" . consult-outline)))

  :bind (("C-x b"   . consult-buffer)
         ("C-x H-r" . consult-recent-file)
         ("C-x M-:" . consult-complex-command)
         ("M-s M-o" . consult-multi-occur)
         ("M-X" . consult-mode-command)
         ("C-c C-j" . consult-outline)
         ("M-s M-j" . consult-outline)
         ("M-s l"   . consult-line)
         ("M-s f"   . consult-find)
         ("M-s M-f" . consult-locate)
         ("M-s g"   . consult-ripgrep)
         ("M-s G"   . consult-git-grep)
         ("C-x C-r" . consult-recent-file)
         ("<help> a" . consult-apropos)
         ("M-s i" . consult-imenu)
         ("s-b" . consult-buffer)
         ;; ("H-b" . consult-buffer)
         ("M-m" . consult-register-store)
         ("M-s k l" . consult-focus-lines)
         ("M-'" . consult-register-load)
         :map ctl-x-r-map
         ("b" . consult-bookmark)
         ("x" . consult-register)
         :map ctl-x-4-map
         ("b" . consult-buffer-other-window)
         :map ctl-x-5-map
         ("b" . consult-buffer-other-frame)
         :map tab-prefix-map
         ("b" . consult-buffer-other-tab)
         :map space-menu-file-map
         ("l" . consult-locate)))

(use-package embark-consult
  :ensure t
  :after consult
  :demand
  :bind (:map embark-collect-mode-map
         ("C-c C-f" . my/embark-consult-preview-toggle))
  :config
  (setf (alist-get 'consult-location
                   embark-collect-initial-view-alist)
        'list
        (alist-get ?b
                   embark-become-file+buffer-map)
        'consult-buffer)
  
  (defun my/embark-consult-preview-toggle ()
  "Toggle preview mode for Embark's Consult collections."
  (interactive)
  (when (featurep 'embark-consult)
    (require 'embark-consult)
    (if (and (bound-and-true-p embark-consult-preview-minor-mode)
             (derived-mode-p 'embark-collect-mode))
        (progn
          (remove-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)
          (embark-consult-preview-minor-mode -1))
      (add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)
      (embark-consult-preview-minor-mode 1))))

  :bind (:map embark-file-map
        ("x" . consult-file-externally))
  ;; :demand t ; only necessary if you have the hook below
  ;; ;; if you want to have consult previews as you move around an
  ;; ;; auto-updating embark collect buffer
  ;; :hook
  ;; (embark-collect-mode . embark-consult-preview-minor-mode)
  )

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :after embark
  :init (marginalia-mode 1)
  :config
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  ;; (marginalia-mode 1)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to 
  ;; switch between the annotators.
  (add-to-list 'marginalia-prompt-categories '("\\burl\\b" . url))
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light)))


(use-package embark
  :demand
  :ensure t
  :after minibuffer
  :bind (("M-s RET"  . embark-act)
         ("M-g RET"  . embark-act)
         ("M-g o"    . embark-act)
         ("M-g M-o"  . embark-act)
         ("s-o"      . embark-act)
         ("H-SPC" . embark-act)
         :map minibuffer-local-completion-map
         ("s-o"      . embark-act)
         ("C-o"      . embark-act)
         ("C-c C-o"  . embark-export)
         ("M-s o"    . embark-export)
         ("H-o"      . embark-export)
         ("M-q"      . embark-collect-toggle-view)
         ("H-SPC"    . embark-act)
         ("C->"      . embark-become)
         :map completion-list-mode-map
         ("C-o"      . embark-act)
         :map embark-collect-mode-map
         ("M-s o"    . embark-export)
         ("H-SPC" . embark-act)
         ("o"        . embark-act)
         ("O"        . embark-act-noexit)
         ("C-o"      . embark-act)
         ("M-g o"    . embark-act)
         ("M-t"      . toggle-truncate-lines)
         ("M-q"      . embark-collect-toggle-view)
         :map embark-file-map
         ("j"        . dired-jump)
         ("S"        . sudo-find-file)
         ("4"        . find-file-other-window)
         ("5"        . find-file-other-frame)
         :map embark-buffer-map
         ("d"        . diff-buffer-with-file) ;FIXME
         ("l"        . eval-buffer)
         ("4"        . switch-to-buffer-other-window)
         ("5"        . switch-to-buffer-other-frame)
         :map embark-bookmark-map
         ("4"        . bookmark-jump-other-window)
         ("5"        . bookmark-jump-other-frame)
         :map embark-url-map
         ("f"        . browse-url-firefox))
  :config
  (setq embark-quit-after-action t)
  ;; (setq embark-collect-initial-view-alist
  ;;       '((t . list)))
  (add-to-list 'embark-keymap-alist
               '(project-file . embark-file-map))
  ;; (add-to-list 'embark-keymap-alist
  ;;              '(virtual-buffer . embark-buffer-map))
  (add-to-list 'embark-exporters-alist
               '(project-file . embark-export-dired))
  ;; (add-to-list 'embark-exporters-alist
  ;;              '(virtual-buffer . embark-export-virtual-ibuffer))

  ;; (defun embark-export-virtual-ibuffer (virtual-buffers)
  ;;   "docstring"
  ;;   (let ((buffers (mapcar (lambda (buf) (substring buf 1))
  ;;                          (cl-remove-if-not
  ;;                           (lambda (buf) (equal (- (elt buf 0)
  ;;                                              consult--special-char)
  ;;                                           ?b))
  ;;                           virtual-buffers))))
  ;;     (ibuffer t "*Embark Export Ibuffer*"
  ;;              `((predicate . (member (buffer-name) ',buffers))))))

  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (aw-switch-to-window (aw-select nil))
         (call-interactively (symbol-function ',fn))))
    
    (defmacro my/embark-split-action (fn split-type) 
      `(defun ,(intern (concat "my/embark-"
                               (symbol-name fn)
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
  
  
  
  (use-package which-key
    :defer
    :config
    (setq embark-action-indicator
          (lambda (map &optional target) (let ((which-key-side-window-location '(right bottom)))
                     (which-key--show-keymap "Embark" map nil nil 'no-paging)
                     #'which-key--hide-popup-ignore-command))
          embark-become-indicator embark-action-indicator))
  (use-package helpful
    :bind (:map embark-become-help-map
                ("f" . helpful-callable)
                ("v" . helpful-variable)
                ("C" . helpful-command)))
  )

(use-package avy-embark-collect
  :ensure t
  :after embark
  :bind (:map minibuffer-local-completion-map
              ("M-j" . avy-embark-collect-choose)
              ("M-RET" . avy-embark-collect-choose)
              ("C-M-o" . avy-embark-collect-act)
              ("C-M-j" . avy-embark-collect-act)))

(use-package embark
  ;; Customizations to use embark's live-occur as a completion system for Emacs.
  ;;  Most of this code is copied from or inspired by the work of Protesilaos
  ;;  Stavrou: https://protesilaos.com/dotemacs/
  ;;  :disabled
  :hook ((embark-post-action . embark-collect--update-linked)
         ;; (embark-pre-action  . completion--flush-all-sorted-completions)
         (embark-collect-post-revert . my/embark--collect-fit-window)
         (minibuffer-setup . embark-collect-completions-after-input)
         (embark-collect-mode . my/embark-collect--live-setup)
         (minibuffer-exit . my/embark-clear-live-buffers))
  :bind (:map embark-collect-mode-map
              ("C-M-n" . my/embark-completions-act-next)
              ("C-M-p" . my/embark-completions-act-previous)
              ("C-M-m" . my/embark-completions-act-current)
              ("C-n" . my/embark-next-line-or-mini)
              ("C-p" . my/embark-previous-line-or-mini)
              ("C-g" . my/embark-keyboard-quit)
              :map minibuffer-local-completion-map
              ("C-n" . my/embark-switch-to-completions-top)
              ("C-p" . my/embark-switch-to-completions-bottom)
              ("C-l" . my/embark-completions-toggle)
              ("RET" . minibuffer-force-complete-and-exit)
              ("C-k" . icomplete-fido-kill)
              ("C-j" . (lambda () (interactive)
	        	  (if minibuffer--require-match
	        	      (minibuffer-complete-and-exit)
	        	    (exit-minibuffer))))
              ("DEL" . icomplete-fido-backward-updir)
              :map minibuffer-local-must-match-map
              ("RET" . minibuffer-force-complete-and-exit))
  :config

  (setf  (alist-get "\\*Embark Collect .*\\*" display-buffer-alist nil nil 'equal)
         '((display-buffer-in-side-window)
           (window-height .  (lambda (win) (fit-window-to-buffer
                                      win
                                      (floor (frame-height) 3))))
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t)))))

  (setf   (alist-get "\\*Embark Collect\\*" display-buffer-alist nil nil 'equal)
          '((display-buffer-in-direction)
            (window-height . (lambda (win) (fit-window-to-buffer
                                      win
                                      (floor (frame-height) 3))))
            (direction . below)
            (window-parameters . ((split-window . #'ignore)))))

  (defun my/embark-keyboard-quit ()
    "If in an Embark live collect/completions buffer, run
`abort-recursive-edit'. Otherwise run `keyboard-quit'."
    (interactive)
    (if (my/embark--live-completions-p)
        (if (use-region-p)
            (keyboard-quit)
          (kill-buffer)
          (abort-recursive-edit))
      (keyboard-quit)))

  (defun my/embark-clear-live-buffers ()
  "Remove lingering Embark Collect Completions' buffers.
Add this to `minibuffer-exit-hook'."
  (let* ((buffers (buffer-list))
         (case-fold-search nil)
         (completions
          (cl-remove-if-not (lambda (buf)
                              (string-match "\\*Embark.*Completions.*"
                                            (format "%s" buf)))
                            buffers)))
    (mapc #'kill-buffer completions)))

  (setq embark-collect-initial-view-alist
        '((file           . list)
          (project-file   . list)
          (virtual-buffer . list)
          (buffer         . list)
          (consult-multi  . list)
          (consult-location . list)
          (symbol         . grid)
          (command        . grid)
          (imenu          . grid)
          (line           . list)
          (xref-location  . list)
          (kill-ring      . zebra)
          (face           . list)
          (t              . grid)))
  
  (defun my/embark--collect-fit-window (&rest _)
    "Fit Embark's live occur window to its buffer.
To be added to `embark-collect-post-revert-hook'."
    (when (derived-mode-p 'embark-collect-mode)
      (fit-window-to-buffer (get-buffer-window)
                            (floor (frame-height) 3) 1)))

  (defun my/embark--live-buffer-p ()
    "Determine presence of a linked live occur buffer."
    (let ((buf embark-collect-linked-buffer))
      (when buf
        (window-live-p (get-buffer-window buf)))))

  ;; (defun my/embark--live-buffer-p ()
  ;; "Determine presence of a linked live occur buffer."
  ;; (let* ((buf-link embark-collect-linked-buffer)
  ;;        (buf-name (buffer-name buf-link)))
  ;;   (when buf-name
  ;;     (string-match-p my/embark-collect-window-regexp buf-name))))

  ;;   (defvar my/embark-collect-window-regexp
  ;;   "\\*Embark Collect \\(Live\\|Completions\\).*"
  ;;   "Regexp to match window names with Embark collections.")
    
  (defun my/embark--live-completions-p ()
  "Determine whether current collection is for live completions."
  (and (derived-mode-p 'embark-collect-mode)
       (eq embark-collect--kind :completions)))
  
  (defvar my/embark-live-collect-hook nil
    "Hook that runs after `my/embark-live-collect-toggle'.")
  
  (defun my/embark-completions-toggle ()
    "Toggle `embark-collect-completions'."
    (interactive)
    (if (my/embark--live-buffer-p)
        (kill-buffer embark-collect-linked-buffer)
      (embark-collect-completions)))

  ;; TODO
  (defun my/embark-collect-toggle-view ()
      (when (eq embark-collect-view 'list)
        (hl-line-mode -1)
        (embark-collect--toggle 'embark-collect-view 'list 'grid)
        
        )
      )
  ;; (defun my/embark-live-occur-toggle ()

  ;;   "Toggle `embark-live-occur', call `my/embark-live-occur-hook'."
  ;;   (interactive)
  ;;   (if (my/embark--live-buffer-p)
  ;;       (kill-buffer embark-occur-linked-buffer)
  ;;     (embark-live-occur))
  ;;   (run-hooks 'my/embark-live-occur-hook))

  (setq embark-live-collect-update-delay 0.30)
  (setq embark-live-collect-initial-delay 0.30)

  (setq embark-candidate-collectors
        (delete 'embark-minibuffer-candidates embark-candidate-collectors))
  (add-to-list 'embark-candidate-collectors 'embark-sorted-minibuffer-candidates)
  ;; (defun my/embark-minibuffer-candidates ()
  ;;   (seq-take (embark-minibuffer-candidates) 40))

  ;; Move from minibuffer to embark-collect and back
  (defun my/embark-next-line-or-mini (&optional arg)
    "Move to the next line or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction, then it switches to
the minibuffer."
    (interactive "p")
    (if (or (eobp) (eq (point-max)
                       (1+ (line-end-position))
                       ;; (save-excursion (forward-line 1) (point))
                       ))
        (my/minibuffer-focus-mini)    ; from `setup-minibuffer.el'
      (forward-line (or arg 1)))
    (setq this-command 'next-line))

  (defun my/embark-previous-line-or-mini (&optional arg)
    "Move to the previous line or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction, then it switches to
the minibuffer."
    (interactive "p")
    (let ((num (if arg (- arg)))) ; from `my/common.el'
      (if (bobp)
          (my/minibuffer-focus-mini)    ; from `my/minibuffer.el'
        (forward-line (or num -1)))))

  (defun my/embark--switch-to-completions ()
    "Subroutine for switching to the Embark completions buffer."
    (unless (my/embark--live-buffer-p)
      (my/embark-completions-toggle))
    (pop-to-buffer embark-collect-linked-buffer))

  (defun my/embark-switch-to-completions-top ()
    "Switch to the top of Embark's completions buffer.
Meant to be bound in `minibuffer-local-completion-map'."
    (interactive)
    (my/embark--switch-to-completions)
    (goto-char (point-min)))

  (defun my/embark-switch-to-completions-bottom ()
    "Switch to the bottom of Embark's completions buffer.
Meant to be bound in `minibuffer-local-completion-map'."
    (interactive)
    (my/embark--switch-to-completions)
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (point-at-bol)))

  ;; Better embark action movements
  (defun my/embark--completions-act (arg)
    "Move ARG lines and perform `embark-default-action'."
    (forward-line arg)
    (embark--act #'embark-default-action (cdr (embark--target))))

  (defun my/embark-completions-act-next (&optional arg)
    "Run default action on next or ARGth Embark target.
This calls `my/embark--completions-act' and is meant to be
assigned to a key in `embark-collect-mode-map'."
    (interactive "p")
    (my/embark--completions-act (or arg 1)))

  (defun my/embark-completions-act-previous (&optional arg)
    "Run default action on previous or ARGth Embark target.
This calls `my/embark--completions-act' and is meant to be
assigned to a key in `embark-collect-mode-map'."
    (interactive "p")
    (let ((num (if arg (- arg))))
      (my/embark--completions-act (or num -1))))

  (defun my/embark-completions-act-current ()
    "Run default action on Embark target without exiting.
Meant to be assigned to a key in `embark-collect-mode-map'."
    (interactive)
    (embark--act #'embark-default-action (cdr (embark--target))))

  ;; Highlighting selections in embark-collect buffers
  (defvar-local my/embark-collect--overlay nil
    "Text overlay for embark-collect buffers.")

  (defun my/embark-collect--live-setup ()
    "Remove mode-line from live embark-collect buffers and set up
highlighting."
    (when (my/embark--live-completions-p)
      (my/mode-line-hidden-mode 1))
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
  

(use-package icomplete-vertical-mini
  :disabled
  :config

;; (define-key icomplete-minibuffer-map (kbd "?")
;;   (lambda () (interactive)
;;     (minibuffer-completion-help)
;;     (switch-to-completions)))
;;(fido-mode -1)
  
  ;; (define-key completion-list-mode-map (kbd "M-o") icomplete-menu-map)

  (defun icomplete-vertical-minibuffer-setup ()
    "Setup minibuffer for a vertical icomplete session.
Meant to be added to `icomplete-minibuffer-setup-hook'."
    (visual-line-mode -1) ; just in case
    (setq truncate-lines t)
    (when (boundp 'auto-hscroll-mode)
      (setq-local auto-hscroll-mode 'current-line))
    (enlarge-window (- icomplete-prospects-height (1- (window-height)))))

  (defun icomplete-vertical-minibuffer-teardown ()
    "Undo minibuffer setup for a vertical icomplete session.
This is used when toggling Icomplete Vertical mode while the
minibuffer is in use."
    (setq truncate-lines nil)
    (enlarge-window (- (1- (window-height)))))

  (defun my/icomplete-toggle-vertical ()
    "Toggle vertical view for `icomplete'.

This is intended as a temporary adjustment of the layout,
possibly to read a list of long names.  It is for this reason
that `my/icomplete-restore-horizontal' exists and is called by
the `minibuffer-exit-hook'."
    (interactive)
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (if (not (eq last-command 'my/icomplete-vertical-mode))
	  ;; (not (string= icomplete-separator "\n "))
          (progn
	    (setq this-command 'my/icomplete-vertical-mode)
            (setq-local icomplete-prospects-height 10)
            (setq-local icomplete-separator "\n ")
	    (icomplete-vertical-minibuffer-setup)
	    )
        (icomplete-vertical-minibuffer-teardown)
        (setq icomplete-prospects-height 2)
        (setq icomplete-separator " ┆ "))))


  ;; (defun my/icomplete-toggle-vertical ()
  ;;     "Toggle vertical view for `icomplete'.

  ;; This is intended as a temporary adjustment of the layout,
  ;; possibly to read a list of long names.  It is for this reason
  ;; that `my/icomplete-restore-horizontal' exists and is called by
  ;; the `minibuffer-exit-hook'.

  ;; NOTE: there still needs to be a way to show the minibuffer input
  ;; on its own line while also displaying the list of candidates."
  ;;     (interactive)
  ;;     (when (and (minibufferp)
  ;;                (bound-and-true-p icomplete-mode))
  ;;       (if (not (string= icomplete-separator "\n "))
  ;;           (progn
  ;;             (setq-local icomplete-prospects-height 10)
  ;;             (setq-local icomplete-separator "\n "))
  ;;         (setq icomplete-prospects-height 1)
  ;;         (setq icomplete-separator " ┆ "))))

  ;; (defun my/icomplete-toggle-flex ()
  ;;   "Toggle between flex and partial-completion (regexp)."
  ;;   (interactive)
  ;;   (when (and (minibufferp)
  ;;              (bound-and-true-p icomplete-mode))
  ;;     (if (not (eq (car completion-styles) 'flex))
  ;;         (progn
  ;;           (setq-local completion-styles '(flex initials substring partial-completion))
  ;;           (message "%s" (propertize "Prioritising FLEX" 'face 'highlight)))
  ;;       (setq-local completion-styles '(partial-completion substring initials flex))
  ;;       (message "%s" (propertize "Prioritising PREFIX REGEXP" 'face 'highlight)))))

  ;; (when (> emacs-major-version 26)
  ;;     (fido-mode 1)
  ;;     (define-key icomplete-fido-mode-map (kbd "C-j") 'icomplete-fido-exit))
  (defun icomplete-fido-backward-updir ()
    "Delete char before or go up directory, like `ido-mode'."
    (interactive)
    (if (and (eq (char-before) ?/)
             (eq (icomplete--category) 'file))
        (zap-up-to-char -1 ?/)
      (call-interactively 'backward-delete-char)))

  (defun icomplete-fido-ret ()
    "Exit minibuffer or enter directory, like `ido-mode'."
    (interactive)
    (let* ((dir (and (eq (icomplete--category) 'file)
                     (file-name-directory (icomplete--field-string))))
           (current (car completion-all-sorted-completions))
           (probe (and dir current
                       (expand-file-name (directory-file-name current) dir))))
      (cond ((and probe (file-directory-p probe) (not (string= current "./")))
             (icomplete-force-complete))
            (t
             (icomplete-force-complete-and-exit)))))

  (defun contrib/completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
 Use as a value for `completion-in-region-function'."
    (if (minibufferp)
        (completion--in-region start end collection predicate)
      (let* ((initial (buffer-substring-no-properties start end))
             (all (completion-all-completions initial collection predicate
                                              (length initial)))
             (completion (cond
                          ((atom all) nil)
                          ((and (consp all) (atom (cdr all))) (car all))
                          (t (let ((completion-in-region-function
                                    #'completion--in-region))
			       (completing-read
                                "Completion: " collection predicate t initial)
                               ;; (icomplete-vertical-do (:height (/ (window-height) 5))
                               ;;   (completing-read
                               ;;    "Completion: " collection predicate t initial))
			       )))))
        (if (null completion)
            (progn (message "No completion") nil)
          (delete-region start end)
          (insert completion)
          t))))

  (setq completion-in-region-function #'contrib/completing-read-in-region))

(use-package icomplete-actions
  :disabled
  :config
  ;;========================================================
  ;; ICOMPLETE-ACTIONS in the minibuffer/completions buffer
  ;;========================================================
  (define-prefix-command 'icomplete-menu-map)
  ;; (define-key icomplete-minibuffer-map (kbd "M-o") icomplete-menu-map)
  (define-key icomplete-menu-map (kbd "w") 'my/icomplete-kill-or-insert-candidate)
  (define-key icomplete-menu-map (kbd "i") 
    (defun my/icomplete-insert-candidate ()
      (interactive)
      (my/icomplete-kill-or-insert-candidate '(4))))
  (define-key icomplete-menu-map (kbd "I") 
    (defun my/icomplete-insert-candidate-finalize ()
      (interactive)
      (my/icomplete-kill-or-insert-candidate '(16))))
  (define-key icomplete-menu-map (kbd "j") (defun my/icomplete-open-other-buffer ()
					     (interactive)
					     (let ((candidate (car completion-all-sorted-completions)))
					       (cond  
						((eq (icomplete--category) 'file) (find-file-other-window candidate) (other-window 1))
						((eq (icomplete--category) 'buffer) (display-buffer candidate) (other-window 1))
						(t nil)
						))))
  (define-key icomplete-menu-map (kbd "J") (lambda () (interactive)
					     (my/icomplete-open-other-buffer)
					     (top-level)))
  (define-key icomplete-menu-map (kbd "h") (defun my/icomplete-help ()
					     (interactive)
					     (let ((candidate (car completion-all-sorted-completions)))
					       (describe-symbol candidate) ;; (if (eq (icomplete--category) nil)
					       ;; ;
					       ;;  )
					       ))
    (define-key icomplete-menu-map (kbd "o") 'icomplete-fido-ret))



  ;; (defvar icomplete-menu-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (set-keymap-parent map icomplete-minibuffer-map)
  ;;     (define-key map (kbd "w") 'my/icomplete-kill-or-insert-candidate)
  ;;     (define-key map (kbd "i") 
  ;;       (defun my/icomplete-insert-candidate ()
  ;; 	(interactive)
  ;; 	(my/icomplete-kill-or-insert-candidate '(4))))
  ;;     (define-key map (kbd "I") 
  ;;       (defun my/icomplete-insert-candidate-finalize ()
  ;; 	(interactive)
  ;; 	(my/icomplete-kill-or-insert-candidate '(16))))
  ;;     (define-key map (kbd "o") 'icomplete-fido-ret)
  ;;     map)
  ;;   "Local keymap for actions on `icomplete' candidates" )

  (defun my/icomplete-kill-or-insert-candidate (&optional arg)
    "Place the matching candidate to the top of the `kill-ring'.
This will keep the minibuffer session active.

With \\[universal-argument] insert the candidate in the most
recently used buffer, while keeping focus on the minibuffer.

With \\[universal-argument] \\[universal-argument] insert the
candidate and immediately exit all recursive editing levels and
active minibuffers.

Bind this function in `icomplete-minibuffer-map'."
    (interactive "*P")
    (let* ((candidate (car completion-all-sorted-completions))
	   (dir (and (eq (icomplete--category) 'file)
                     (file-name-directory (icomplete--field-string))))
	   (candidate-full (concat dir candidate)))
      (when (and (minibufferp)
                 (bound-and-true-p icomplete-mode))
        (cond ((eq arg nil)
               (kill-new candidate-full))
              ((= (prefix-numeric-value arg) 4)
               (with-minibuffer-selected-window (insert candidate-full)))
              ((= (prefix-numeric-value arg) 16)
               (with-minibuffer-selected-window (insert candidate-full))
               (top-level)))))))

(provide 'setup-icomplete)
