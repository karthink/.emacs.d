;; This makes ido-find-file-other-window,
;; ido-switch-buffer-other-window, et. al obsolete. It’s a much better
;; abstraction, and I believe it should become apart of ido mode,
;; because any command that uses ido-completing-read can benefit from
;; it without any additional effort, including textmate.el’s
;; textmate-goto-symbol.

(defun ido-invoke-in-other-window ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'other)
  (ido-exit-minibuffer))

(defun ido-invoke-in-horizontal-split ()
  "signals ido mode to split horizontally and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

(defun ido-invoke-in-vertical-split ()
  "signals ido mode to split vertically and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

(defun ido-invoke-in-new-frame ()
  "signals ido mode to create a new frame after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defadvice ido-read-internal (around ido-read-internal-with-minibuffer-other-window activate)
  (let* (ido-exit-minibuffer-target-window
         (this-buffer (current-buffer))
         (result ad-do-it))
    (cond
     ((equal ido-exit-minibuffer-target-window 'other)
      (if (= 1 (count-windows))
          (split-window-horizontally-and-switch)
        (other-window 1)))
     ((equal ido-exit-minibuffer-target-window 'horizontal)
      (split-window-horizontally-and-switch))

     ((equal ido-exit-minibuffer-target-window 'vertical)
      (split-window-vertically-and-switch))
     ((equal ido-exit-minibuffer-target-window 'frame)
      (make-frame)))
    (switch-to-buffer this-buffer) ;; why? Some ido commands, such as textmate.el's textmate-goto-symbol don't switch the current buffer
    result))

;; (defadvice ido-init-completion-maps (after ido-init-completion-maps-with-other-window-keys activate)
;;   (mapcar (lambda (map)
;;             (define-key map (kbd "C-o") 'ido-invoke-in-other-window)
;;             (define-key map (kbd "C-2") 'ido-invoke-in-vertical-split)
;;             (define-key map (kbd "C-3") 'ido-invoke-in-horizontal-split)
;;             (define-key map (kbd "C-4") 'ido-invoke-in-other-window)
;;             (define-key map (kbd "C-5") 'ido-invoke-in-new-frame))
;;           (list ido-buffer-completion-map
;;                 ido-common-completion-map
;;                 ido-file-completion-map
;;                 ido-file-dir-completion-map)))

(provide 'ido-other-window)