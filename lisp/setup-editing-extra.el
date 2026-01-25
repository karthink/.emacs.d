;; -*- lexical-binding: t; -*-

;; Hyper bindings for emacs. Why use a pinky when you can use a thumb?
(use-package emacs
  :when IS-LINUX
  :bind-keymap (;; ("H-f" . space-menu-file-map)
                ;; ("H-b" . space-menu-buffer-map)
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
         ("C-H-x" . eval-defun)
         ("H-s" . isearch-forward)
         ("H-r" . isearch-backward)
         ("H-q" . kill-buffer-and-window)
         ("C-M-4" . other-window-prefix)
         ("C-M-1" . same-window-prefix)
         ("C-M-5" . other-frame-prefix)
         ("C-M-6" . other-tab-prefix)
         ("H-v"   . scroll-other-window)
         ("H-V" . scroll-other-window-down)
         ("H-+" . balance-windows-area)
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
  (defun other-window-prefix-maybe ()
    (interactive)
    (other-window-prefix)
    (setq unread-command-events (list last-command-event))
    (when-let ((seqs (read-key-sequence "[other-window]: "))
               (cmd (key-binding seqs)))
      (setq this-command cmd)
      (call-interactively cmd)))
  (keymap-set ctl-x-4-map "<t>" #'other-window-prefix-maybe)
  
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
  (setq repeat-keep-prefix t
        repeat-echo-function #'repeat-echo-mode-line
        repeat-echo-mode-line-string
        (propertize "[R]" 'face 'mode-line-emphasis))
  (defun my/repeat-mode ()
    (let ((inhibit-message t)
          (message-log-max nil))
      (repeat-mode))))

(use-package repeat-help
  :ensure (:host github :protocol ssh
           :repo "karthink/repeat-help")
  ;; :load-path "plugins/repeat-help/"
  :hook (repeat-mode . repeat-help-mode)
  :config
  (setq repeat-help-key "<f1>"
        repeat-help-popup-type 'embark))

(use-package repeat-help
  :disabled                             ;doesn't work with repeat-help yet
  :config
  ;; From JDTSmith https://gist.github.com/jdtsmith/a169362879388bc1bdf2bbb977782d4f
  (let ((orig (default-value 'repeat-echo-function))
	rcol ccol in-repeat)
    (setq
     repeat-echo-function
     (lambda (map)
       (if orig (funcall orig map))
       (unless rcol (setq rcol (face-foreground 'error)))
       (if map
	   (unless in-repeat		; new repeat sequence
	     (setq in-repeat t
		   ccol (face-background 'cursor))
	     (set-frame-parameter nil 'my/repeat-cursor ccol))
	 (setq in-repeat nil)
	 (set-frame-parameter nil 'my/repeat-cursor nil))
       (set-cursor-color (if map rcol ccol))))

    (add-function
     :after after-focus-change-function
     (let ((sym 'my/remove-repeat-cursor-color-on-focus-change))
       (defalias sym
	 (lambda ()
	   (when in-repeat
	     (dolist (frame (frame-list))
	       (when-let ((col (frame-parameter frame 'my/repeat-cursor)))
		 (with-selected-frame frame
		   (set-cursor-color col)))))))
       sym))))



(provide 'setup-keybinds)
