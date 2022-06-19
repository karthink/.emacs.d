(use-package general
  ;; :preface (setq use-package-ignore-unknown-keywords t)
  :straight t
  :demand t
  :commands (general-def general-define-key)
  :init
  (defvar general-leader "SPC"
    "Leader key for Evil")
  (defvar general-leader-alt "M-SPC"
    "Leader key for Emacs and Evil Insert states")
  (defvar general-localleader ","
    "Local leader key for major-mode specific commands")
  (defvar general-localleader-alt "M-SPC ,"
    "Local leader key for major-mode specific commands for Emacs and Evil Insert states.")

  ;; With evil-mode
  (general-define-key
   :states '(normal visual emacs)
   :prefix general-leader
   :non-normal-prefix general-leader-alt
   :prefix-command 'space-menu
   :prefix-map 'space-menu-map
   :wk "Leader key for emacs")

  (general-create-definer leader-define-key
    :states '(normal visual motion emacs)
    :prefix general-leader
    :non-normal-prefix general-leader-alt)
  (general-create-definer localleader-define-key
    :states '(normal visual motion emacs)
    :prefix general-localleader
    :non-normal-prefix general-localleader-alt)

  (general-def
    :states '(motion)
    :prefix general-leader-alt
    "" 'space-menu)
  ;; Pure emacs
  ;; (general-define-key
  ;;   ;; :states '(normal motion visual emacs)
  ;;   ;; :prefix general-leader
  ;;   ;; :non-normal-prefix general-leader-alt
  ;;   :prefix general-leader-alt
  ;;   :prefix-command 'space-menu
  ;;   :prefix-map 'space-menu-map
  ;;   :wk "Leader key for emacs")

  ;;  (general-create-definer leader-define-key
  ;;    ;; :states '(normal visual motion emacs)
  ;;    ;; :non-normal-prefix general-leader-alt
  ;;    :prefix general-leader-alt)

  ;;  (general-create-definer localleader-define-key
  ;;    ;; :states '(normal visual motion emacs)
  ;;    ;; :non-normal-prefix general-localleader-alt
  ;;    :prefix general-localleader-alt)

  (general-def
    :keymaps 'space-menu-map
    :wk-full-keys nil
    ;; unbind SPC and give it a title for which-key (see echo area)
    ;;"x" '(Control-X-prefix :wk "C-x")
    "SPC" 'scroll-other-window
    "M-SPC" '(scroll-other-window :wk nil)
    "M-S-SPC" '(scroll-other-window-down :wk nil)
    "S-SPC" 'scroll-other-window-down
    "z" '(repeat-complex-command :wk "M-x again")
    "x" '(execute-extended-command :wk "M-x")
    "f" '(:prefix-command space-menu-file
                          :prefix-map space-menu-file-map
                          :wk "files") ;; :wk "file")
    "q" '(:ignore t :wk "quit")
    "b" '(:ignore t)
    "g" '(vc-prefix-map :wk "git/VC")
    "c" '(:ignore t) ;; :wk "code")
    "k" '(kill-this-buffer :wk "Kill buffer")
    "/" '(:prefix-command space-menu-search
                          :prefix-map space-menu-search-map
                          :wk "search")
    "b" '(:prefix-command space-menu-buffer
                          :prefix-map space-menu-buffer-map
                          :wk "buffers")
    "p" '(project-prefix-map :wk "project")
    "h" '(help-command :wk "help")
    ;; "h" '(:prefix-command space-menu-help
    ;;       :prefix-map space-menu-help-map
    ;;       :wk "help")
    "w" '(:prefix-command space-menu-window
                          :prefix-map space-menu-window-map
                          :wk "window")
    )

  (general-def
    :keymaps 'space-menu-map
    :wk-full-keys nil
    "s" '(space-menu-search :wk "search"))

  (general-def
    :keymaps 'space-menu-buffer-map
    :wk-full-keys nil
    "g" '(revert-buffer         :wk "revert buffer")
    "b" '(switch-to-buffer      :wk "switch to buffer")
    "d" '(kill-buffer           :wk "delete buffers")
    "k" '(kill-this-buffer      :wk "kill buffer")
    "z" '(bury-buffer           :wk "bury buffer")
    "[" '(previous-buffer       :wk "prev buffer")
    "]" '(next-buffer           :wk "next buffer")
    "=" '(diff-buffer-with-file :wk "diff against file")
    "C-o" '(display-buffer      :wk "display buffer")
    )

  (general-def
    :keymaps 'space-menu-window-map
    :wk-full-keys nil
    "S" 'window-configuration-to-register
    "J" '(jump-to-register :wk "window config jump")
    "k" '(delete-window :wk "delete window")
    "K" '(kill-buffer-and-window :wk "kill buf and win"))

  (general-def
    :keymaps 'space-menu-search-map
    :wk-full-keys nil
    "o" '(occur :wk "occur")
    "." '(isearch-forward-symbol-at-point :wk "search thing-at-pt")
    "h" '(highlight-regexp :wk "highlight regexp")
    "_" '(isearch-forward-symbol :wk "search for symbol")
    "f" '(grep-find :wk "grep through find")
    "b" '(batch-replace-strings :wk "batch-replace")
    "i" '(imenu :wk "imenu"))

  ;; (general-def
  ;;   :keymaps 'space-menu-project-map
  ;;   :wk-full-keys nil
  ;;   "f" '(project-find-file :wk "find file in proj")
  ;;   "q" '(project-query-replace-regexp :wk "query replace in proj")
  ;;   "g" '(project-search :wk "grep in proj")
  ;;   "o" '(project-find-regexp :wk "occur in proj"))

  (general-def
    :keymaps 'space-menu-map
    :wk-full-keys nil
    "," '(switch-to-buffer   :wk "switch buffer")
    ";" '(eval-expression    :wk "eval expr")
    "u" '(universal-argument :wk "universal arg")
    )

  (general-def
    :keymaps 'space-menu-map
    :wk-full-keys nil
    :prefix "q"
    "q" '(save-buffers-kill-terminal :wk "quit emacs (eject!)")
    "f" '(delete-frame               :wk "quit frame")
    "d" '(server-edit                :wk "done with buffer"))

  (general-def
    :keymaps 'space-menu-file-map
    :wk-full-keys nil
    "s" '(save-buffer       :wk "Save file")
    "w" '(write-file        :wk "Save as?")
    "S" '(save-some-buffers :wk "Save bufferS")
    "U" '(sudo-find-file    :wk "Sudo find file")
    "^" '(sudo-this-file    :wk "Sudo THIS file")
    "f" '(find-file         :wk "Find file")
    "l" '(locate            :wk "Locate file on system")
    "'" '(bookmark-jump     :wk "Jump to bookmark")
    "m" '(bookmark-set      :wk "Set bookmark")
    "." '(find-file         :wk "Find file (FIXME)")
    )

  (localleader-define-key
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "g" '(nil            :wk "goto")
    "gl" '(find-library  :wk "Find Library")
    "gv" '(find-variable :wk "Find Variable")
    "gf" '(find-function :wk "Find Function")
    "x" '(eval-defun     :wk "Eval defun")
    "l" '(load-library   :wk "Load library")
    "b" '(eval-buffer    :wk "Eval Buffer")
    "r" '(eval-region    :wk "Eval Region")
    "B" `(,(defun byte-compile-this-file () "Byte-compile file"
                  (interactive)
                  (if buffer-file-name
                      (byte-compile-file
                       buffer-file-name)
                    (message "Not visiting a file!")))
          :wk "Byte-compile file")
    "L" `(,(defun load-this-file () "Load current file"
                  (interactive)
                  (if buffer-file-name
                      (load-file
                       buffer-file-name)
                    (message "Not visiting a file!")))
          :wk "Load this file"))
  (general-def :keymaps 'space-menu-help-map
    "m" '(describe-mode :wk "describe mode")))

(use-package god-mode
  :disabled
  :init
  (setq which-key--god-mode-support-enabled t)
  (setq god-mode-enable-function-key-translation nil)
  (defun my-god-mode-update-cursor ()
    (setq cursor-type (if (or god-local-mode)
                          'hollow
                        'box)))
  (global-set-key (kbd "<escape>") #'god-local-mode)
  ;; (global-set-key (kbd "S-SPC") #'god-local-mode)
  (add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor))

;; Hyper bindings for emacs. Why use a pinky when you can use a thumb?
(use-package emacs
  :bind-keymap (("H-f" . space-menu-file-map)
                ("H-b" . space-menu-buffer-map)
                ("H-r" . ctl-x-r-map))
  :bind (("M-ESC ESC" . nil)
         ("H-x" . H-x)
         ("H-c" . H-c)
         ("H-z" . repeat)
         ("H-=" . text-scale-increase)
         ("H--" . text-scale-decrease)
         ("H-M--" . shrink-window-if-larger-than-buffer)
         ("H-h" . mark-whole-buffer)
         ("H-M-x" . eval-defun)
         ("H-s" . isearch-forward)
         ("H-r" . isearch-backward)
         ("H-q" . kill-buffer-and-window)
         :map isearch-mode-map
         ("H-s" . isearch-repeat-forward)
         ("H-r" . isearch-repeat-backward)
         ;; :map ctl-x-map
         ;; ("H-s" . save-buffer)
         ;; ("H-e" . eval-last-sexp)
         ;; ("H-c" . save-buffers-kill-terminal)
         ;; ("H-f" . find-file)
         ;; ("H-q" . read-only-mode)
         )
  :config
  (defun hyperify-prefix-key (key)
    (let* ((convert-function
	    (lambda (event)
	      (vector
	       (if (memq 'hyper (event-modifiers event))
		   (event-apply-modifier (event-basic-type event) 'control 26 "C-")
	         event))))
	   (first-key-sequence (vconcat key (funcall convert-function (read-event))))
	   (command (or (let ((minor-cmd (lookup-key (current-minor-mode-maps) first-key-sequence)))
                          (unless (equal minor-cmd 1) minor-cmd))
                        (let ((local-cmd (lookup-key (current-local-map) first-key-sequence)))
                          (unless (equal local-cmd 1) local-cmd))
                        (lookup-key (current-global-map) first-key-sequence))))
      (catch 'finished
        (while t
	  (cond ((commandp command)
	         (call-interactively command)
	         (throw 'finished t))
	        ((keymapp command)
	         (setq command (lookup-key command (funcall convert-function (read-event)))))
	        (t (error "ABORT")))))))

  (defun H-x ()
    (interactive)
    (hyperify-prefix-key [24]))

  (defun H-c ()
    (interactive)
    (hyperify-prefix-key [3])))

(use-package repeat
  :if (version< "28.0" emacs-version)
  :bind ("H-z" . repeat)
  :hook (after-init . my/repeat-mode)
  :config
  (defun my/repeat-mode ()
    (let ((inhibit-message t)
          (message-log-max nil))
      (repeat-mode)))
  
  (use-package which-key
    :after which-key
    :config
    ;; (advice-add 'repeat-post-hook :after
    ;;             (defun my/which-key-repeat ()
    ;;               (when-let ((cmd (or this-command real-this-command))
    ;;                          (keymap (repeat--command-property 'repeat-map)))
    ;;                 (run-at-time
    ;;                  which-key-idle-delay nil
    ;;                  (lambda () 
    ;;                    (which-key--create-buffer-and-show
    ;;                     nil (symbol-value keymap)))))))

    (defun my/which-key-repeat-mode-dispatch ()
      (interactive)
      (setq this-command last-command)
      (when-let (keymap (repeat--command-property 'repeat-map))
        (which-key--create-buffer-and-show
         nil (symbol-value keymap))))
    
    ;; (defun my/which-key-repeat-mode-binding ()
    ;;   (when repeat-mode
    ;;     (when-let* ((rep-map-sym (or repeat-map (repeat--command-property 'repeat-map)))
    ;;                 (keymap (and (symbolp rep-map-sym) (symbol-value rep-map-sym))))
    ;;       (set-transient-map
    ;;        (make-composed-keymap
    ;;         (let ((map (make-sparse-keymap)))
    ;;           (define-key map (kbd "C-h") #'my/which-key-repeat-mode-dispatch)
    ;;           map)
    ;;         keymap)))))
    
    (defun my/which-key-repeat-mode-binding ()
      (when repeat-mode
        (when-let* ((rep-map-sym (or repeat-map (repeat--command-property 'repeat-map)))
                    (keymap (and (symbolp rep-map-sym) (symbol-value rep-map-sym))))
          (set-transient-map
           (let ((map (make-sparse-keymap)))
             (set-keymap-parent map keymap)
             (define-key map (kbd "<f1>") #'my/which-key-repeat-mode-dispatch)
             map)))))
    (advice-add 'repeat-post-hook :after #'my/which-key-repeat-mode-binding)))



(provide 'setup-keybinds)
