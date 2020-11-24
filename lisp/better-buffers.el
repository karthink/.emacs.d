;;######################################################################
;; BETTER BUFFERS
;;######################################################################
;;(require 'use-package nil t)
;; Collection of commands to make handling buffers less painful

;;----------------------------------------------------------------------
;; KEYBINDINGS
;;----------------------------------------------------------------------

;;; Use C-` or ` to dismiss *Help* and *info* windows. If there are
;;; no *Help*/*info* windows open, C-` will cycle between this buffer
;;; and (other-buffer) instead, and ` will self-insert.
;; (global-set-key (kbd "C-`") 'bbuf-dismiss-or-switch)

;; (global-set-key (kbd "`") (lambda () (interactive)
;;                             (bbuf-dismiss-or-insert "`")))


(global-set-key (kbd "<C-delete>")
                (lambda () (interactive)
                  (funcall (if (string= (buffer-name) "*scratch*")
                               'bury-buffer
                             (lambda ()
                               (and (buffer-file-name) (save-buffer))
                               (kill-buffer)
                               (delete-window))))))


(use-package ibuffer
  :defer t
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  ;; (setq ibuffer-use-header-line t)
  ;; (setq ibuffer-display-summary nil)
  ;; (setq ibuffer-use-other-window nil)
  ;; (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  ;; (setq ibuffer-saved-filter-groups nil)

  (defun my/buffers-major-mode (&optional arg)
    "Select buffers that match the current buffer's major mode.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion."
    (interactive "P")
    (let* ((major major-mode)
           (prompt "Buffers for ")
           (mode-string (format "%s" major))
           (mode-string-pretty (propertize mode-string 'face 'success)))
      (if arg
          (ibuffer t (concat "*" prompt mode-string "*")
                   (list (cons 'used-mode major)))
        (switch-to-buffer
         (read-buffer
          (concat prompt mode-string-pretty ": ") nil t
          (lambda (pair) ; pair is (name-string . buffer-object)
            (with-current-buffer (cdr pair) (derived-mode-p major))))))))

  (defun my/buffers-vc-root (&optional arg)
    "Select buffers that match the present `vc-root-dir'.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion.

When no VC root is available, use standard `switch-to-buffer'."
    (interactive "P")
    (let* ((root (vc-root-dir))
           (prompt "Buffers for VC ")
           (vc-string (format "%s" root))
           (vc-string-pretty (propertize vc-string 'face 'success)))
      (if root
          (if arg
              (ibuffer t (concat "*" prompt vc-string "*")
                       (list (cons 'filename (expand-file-name root))))
            (switch-to-buffer
             (read-buffer
              (concat prompt vc-string-pretty ": ") nil t
              (lambda (pair) ; pair is (name-string . buffer-object)
                (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
        (call-interactively 'switch-to-buffer))))

  :hook ((ibuffer-mode . hl-line-mode)
         (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root))
  :general
  (:keymaps 'space-menu-buffer-map
   :wk-full-keys nil
   "i" '(ibuffer :wk "ibuffer"))
  ("C-x C-b" 'ibuffer)
  ("M-s b" 'my/buffers-major-mode)
  ("M-s v" 'my/buffers-vc-root)
  ;; :bind (("C-x C-b" . ibuffer))
         ;; :map ibuffer-mode-map
         ;; ("* f" . ibuffer-mark-by-file-name-regexp)
         ;; ("* g" . ibuffer-mark-by-content-regexp) ; "g" is for "grep"
         ;; ("* n" . ibuffer-mark-by-name-regexp)
         ;; ("s n" . ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
         ;; ("/ g" . ibuffer-filter-by-content)
         )

(use-package ibuffer-vc
  :ensure t
  :after (ibuffer vc)
  :general
  (:keymaps 'ibuffer-mode-map
   :states 'normal
   "s V" 'ibuffer-vc-set-filter-groups-by-vc-root
   "s <backspace>" 'ibuffer-clear-filter-groups)
  (:keymaps 'ibuffer-mode-map
   "/ V" 'ibuffer-vc-set-filter-groups-by-vc-root
   "/ <backspace>" 'ibuffer-clear-filter-groups))

;; Keys to traverse buffers
;; (global-set-key (kbd "<C-M-return>") 'ido-display-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer) ; Ctrl+PageDown
(global-set-key (kbd "C-<next>") 'next-buffer) ; Ctrl+PageUp

;; (global-set-key (kbd "<C-tab>") 'other-window)  ; Ctrl+Tab
;; (global-set-key (kbd "<C-S-iso-lefttab>") 
;;                 (lambda () (interactive) (other-window -1)))
                                        ;Ctrl+Shift+Tab

;;; Cycle buffers forward. (Backward with prefix arg) 
;; (global-set-key (kbd "M-`")
;;                 (lambda (&optional arg)
;;                   (interactive "P")
;;                   (if arg (next-user-buffer) (previous-user-buffer))))

;;; Set keys to scroll buffer while centering
;;; location on screen.
(global-set-key "\M-]" 'scroll-buffer-down)
(global-set-key "\M-[" 'scroll-buffer-up)

;;; Toggle window split between horizontal and vertical
(define-key ctl-x-4-map "t" 'toggle-window-split)
(define-key ctl-x-4-map "|" 'toggle-window-split)

;;----------------------------------------------------------------------
;; FUNCTIONS
;;----------------------------------------------------------------------

;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K) 
;;           (let* ((key (car K)) (fun (cdr K)))
;;             (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;;         '(("C-n" . iswitchb-next-match)
;;           ("C-p"  . iswitchb-prev-match)
;;           ("<up>"    . ignore             )
;;           ("<down>"  . ignore             ))))

;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;;###autoload
(defun toggle-window-split ()
  (interactive)
  "Toggles the window split between horizontal and vertical when
the fram has exactly two windows."
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; Swap windows if there are two of them
;;;###autoload
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
;;;###autoload
(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    ;; (cond ((and (string-equal "*scratch*" (buffer-name))
    ;;             (buffer-modified-p))
    ;;        (setq i (1+ i))
    ;;        (next-buffer)))
    (while (and (not (and (string-equal "*scratch*" (buffer-name))
                          (buffer-modified-p)))
                (string-match "^*" (buffer-name))
                (< i 50))
      (setq i (1+ i)) (next-buffer) )))

;;;###autoload
(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*scratch*" (buffer-name)))
                (string-match "^*" (buffer-name))
                (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

;;; scroll-buffer: Functions to do exactly that.

;;;###autoload
(defun scroll-buffer-down (&optional arg)
  "Scroll buffer by (optional) ARG paragraphs."
  (interactive "p")
  (forward-paragraph arg)
  (recenter))

;;;###autoload
(defun scroll-buffer-up (&optional arg)
  "Scroll buffer by (optional) ARG paragraphs."
  (interactive "p")
  (backward-paragraph arg)
  (recenter))

;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." 
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

;;;###autoload
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
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil) t))))

;;----------------------------------------------------------------------
;; DISMISS-WINDOW 
;;----------------------------------------------------------------------
;; Code to dismiss *Help* windows and other popups by saving and
;; restoring window configurations.
;; DEPRECATED: winner mode handles this better

;; (defvar bbuf-window-configuration nil
;;   "Variable to store a window configuration to restore later.
;;   Will be updated when a *Help* window springs up.")

;; (defvar bbuf-bury-buffer-list '("*help*"
;;                                 "*info*"
;;                                 "*compile-log*"
;;                                 "*apropos*"
;;                                 "*backtrace*"
;;                                 "*warning*"
;;                                 "*Warning*"
;;                                 "*Completions*"
;;                                 "*helpful")
;;   "List of buffer names that will be buried (with respective
;;   windows deleted) by bbuf-dismiss-windows")

;; ;;; Save window-configuration to bbuf-window-configuration
;; ;;; when a *Help* window pops up. 
;; ;;; (But only when there are no pre-existing *Help* buffers)
;; (add-hook 'help-mode-hook
;;           (lambda () 
;;             (bbuf-save-window-configuration "*Help*")))

;; (add-hook 'compilation-finish-functions
;;           (lambda ()
;;             (bbuf-save-window-configuration "*Compile-Log*")))

;; (add-hook 'apropos-mode-hook
;;           (lambda ()
;;             (bbuf-save-window-configuration "*Apropos*")))


;; (defun bbuf-save-window-configuration (buffer-name)
;;   "if buffer-name is not one of the currently displayed buffers,
;; save the current window configuration"
;;   (if (not (member buffer-name
;;                    (mapcar (lambda (w) (buffer-name 
;;                                         (window-buffer w))) 
;;                            (window-list))))
;;       (setq bbuf-window-configuration 
;;             (current-window-configuration))))

;; (defun bbuf-dismiss-windows (no-dismiss-window-function)
;;   "Restore the window configuration to the one just before
;; certain windows/buffers are created. The windows/buffers to
;; dismiss are given by buffer names in
;; bbuf-bury-buffer-list. If there are no windows to
;; dismiss, run no-dismiss-window-function instead.

;; Typically, running this function will bury any open *Help* buffer
;; and dismiss its window."
;;   (let ((buf (window-buffer (next-window))))
;;     (if (member (downcase (buffer-name buf))
;;                 bbuf-bury-buffer-list)
;;         (progn (bury-buffer buf)
;;                (set-window-configuration
;;                 bbuf-window-configuration))
;;       (funcall no-dismiss-window-function))))

;; (defun bbuf-dismiss-or-switch (arg)
;;   "Restore window configuration or cycle (current buffer)"
;;   (interactive "P")
;;   (bbuf-dismiss-windows 
;;    (lambda () (switch-to-buffer 
;;           (other-buffer (current-buffer) arg)))))

;; (defun bbuf-dismiss-or-insert (char)
;;   "Restore window configuration or insert a character"
;;   (bbuf-dismiss-windows
;;    (lambda () (insert char))))
;; ;;----------------------------------------------------------------------

(provide 'better-buffers)

