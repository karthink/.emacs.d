;;----------------------------------------------------------------------
;; BETTER BUFFERS
;;----------------------------------------------------------------------

;; Collection of commands to make handling buffers less painful

;; Iswitchb Mode; Superceded by ido-mode
;; (iswitchb-mode 1)

;; KEYBINDINGS

;; C-x C-b to ibuffer instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Keys to traverse buffers
(global-set-key (kbd "<C-M-return>") 'ido-display-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer) ; Ctrl+PageDown
(global-set-key (kbd "C-<next>") 'next-buffer) ; Ctrl+PageUp

(global-set-key (kbd "<C-tab>") 'other-window)  ; Ctrl+Tab
(global-set-key (kbd "<C-S-iso-lefttab>") 
                (lambda () (interactive) (other-window -1))) ;Ctrl+Shift+Tab

;;; Set keys to scroll buffer while centering
;;; location on screen.
(global-set-key "\M-]" 'scroll-buffer-down)
(global-set-key "\M-[" 'scroll-buffer-up)

;; FUNCTIONS

(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("C-n" . iswitchb-next-match)
          ("C-p"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; Swap windows if there are two of them
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

;; Skip to next/previous user buffer
(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

;;; scroll-buffer: Functions to do exactly that.

(defun scroll-buffer-down (&optional arg)
  "Scroll buffer by (optional) ARG paragraphs."
  (interactive "p")
  (forward-paragraph arg)
  (recenter))

(defun scroll-buffer-up (&optional arg)
  "Scroll buffer by (optional) ARG paragraphs."
  (interactive "p")
  (backward-paragraph arg)
  (recenter))

;; Never understood why Emacs doesn't have this function.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." 
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1) (rename-buffer new-name) (set-visited-file-name new-name) (set-buffer-modified-p nil))))))

;; Never understood why Emacs doesn't have this function, either.
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1) (delete-file filename) (set-visited-file-name newname) (set-buffer-modified-p nil) t))))

;;----------------------------------------------------------------------

(provide 'better-buffers)