;; ----------------------------
;; Completion
;; ----------------------------
(setq user-emacs-directory "~/.emacs.d.orig")
(defun dir-concat (dir file) (concat (file-name-as-directory dir)
				     file))
(push (dir-concat user-emacs-directory "plugins/") load-path)
(push (dir-concat user-emacs-directory "lisp/") load-path)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/atom-one-dark-theme-20190705.554/")

;(add-to-list 'load-path "~/.local/share/git/melpa/embark/")
;(require 'embark)

(require 'cl-seq)
(require 'setup-completion nil t)
(require 'setup-minibuffer nil t)
(when (require 'icomplete nil t)
 (require 'setup-icomplete nil t)
 (icomplete-mode 1))

;; ----------------------------
;; Windows
;; ----------------------------
(require 'setup-windows nil t)
(when (require 'popup-buffers nil t) (popup-buffers-mode 1))

(setq popup-buffers-reference-modes-list
      (append my/help-modes-list
              my/repl-modes-list
              my/occur-grep-modes-list))
(setq popup-buffers-reference-buffer-list
      '("^\\*Warnings\\*"
        "^\\*Compile-Log\\*"
        "^\\*Messages\\*"
        "^\\*Backtrace\\*"
        "^\\*evil-registers\\*"
        "^\\*Apropos"
        "^Calc:"
        "^\\*ielm\\*"
        "^\\*TeX Help\\*"
        "\\*Shell Command Output\\*"
        "\\*Async Shell Command\\*"
        "\\*Completions\\*"
        "Output\\*"
        "\\*scratch\\*"))

;; ----------------------------
;; UI and stuff
;; ----------------------------

(scroll-bar-mode 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.01)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;; ----------------------------
;; Editing
;; ----------------------------
(setq scroll-conservatively 1)
(setq scroll-preserve-screen-position t)
(electric-pair-mode 1)

(global-set-key (kbd "M-SPC")
		(defun my/cycle-spacing-impatient (&optional n preserve-nl-back)
		  (interactive "*p")
		  (cycle-spacing (if (= n 1) -1 n) preserve-nl-back 'fast)))

(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)

(global-set-key
 (kbd "C-w")
 (defun backward-kill-word-or-region (&optional arg)
   "Kill word backwards unless region is active,
kill region instead"
   (interactive)
   (if (region-active-p)
       (kill-region (region-beginning)
				      (region-end))
     (backward-kill-word (or arg 1)))))

(advice-add 'kill-ring-save :around
	    (defun kill-ring-save-advice (fun &rest args)
	      "Save line to kill ring if region is inactive"
	      (interactive)
	      (if mark-active
		  (funcall fun (region-beginning) (region-end))
		(funcall fun (line-beginning-position)
			 (line-beginning-position 2)))))

(global-set-key (kbd "M-O") #'other-window)

(global-set-key (kbd "M-o") (defun open-line-above (&optional arg)
			      (interactive)
			      (beginning-of-line)
			      (open-line (or arg 1))
			      (indent-according-to-mode)))
(global-set-key (kbd "C-o") (defun open-line-below (&optional arg)
			      (interactive)
			      (end-of-line)
			      (open-line (or arg 1))
			      (forward-line)
			      (indent-according-to-mode)
			      ))
(global-set-key (kbd "C-a")
		(defun back-to-indentation-or-beginning () (interactive)
		       (if (= (point) (progn (back-to-indentation) (point)))
			   (beginning-of-line))))

;; Buffers
(when (require 'windmove nil t)
  (windmove-default-keybindings))
(global-set-key (kbd "C-x k") #'kill-current-buffer)

;; Occur:
(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)

;; History
(when (require 'savehist nil t)
  (setq savehist-file (dir-concat user-cache-directory "savehist")
	history-length 10000
	history-delete-duplicates nil
	savehist-save-minibuffer-history t)
  (savehist-mode 1))
