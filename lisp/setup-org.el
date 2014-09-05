;;;; Org mode

;; The following lines are always needed.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-load-hook 
          '(lambda nil 
             (define-key org-mode-map (kbd "<C-tab>") 'other-window)
             (define-key org-mode-map (kbd "<C-S-tab>") (lambda () (other-window -1)))))

;; Completion
;;(add-hook 'org-mode-hook '(lambda () 
;;			   (define-key org-mode-map (kbd "C-;") 'org-complete))

;; Enable longline-truncation in org-mode buffers
(add-hook 'org-mode-hook 'toggle-truncate-lines)
;; Hide all stars except the last one on each line:
(setq org-hide-leading-stars 1)        

;;; Org-agenda mode
;; (defvar org-agenda-files nil) 
;; (setq org-agenda-files (cons "~/notes/" org-agenda-files))
;; (setq org-agenda-restore-windows-after-quit 1)

;;; Org-export options
(setq org-export-with-LaTeX-fragments t)

;; Some formatting
;; (setq org-blank-before-new-entry
;;       '((heading . t) (plain-list-item . nil)))

;;; Org LaTeX options
;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;;; Org-Remember: Org-mode with Remember-mode 

;; (org-remember-insinuate)
;; (setq org-directory "~/doodles")
;; (setq org-default-notes-file 
;;       (expand-file-name (concat org-directory "tasks.org")))
;; ;; (setq org-remember-default-headline "stuff")

;; ;;Templates for org-remember:
;; (setq org-remember-templates
;;       (quote (("Journal" ?j
;;                "* %^{Title}\n  %U\n  %?\n  %i\n"
;;                "journal.org" top
;;                )
;;               ("Notes" ?n
;;                "* %?\n  "
;;                "tasks.org" bottom
;;                ))))

;; (defun make-remember-frame ()
;;   "Turn the current frame into a small popup frame for remember mode;
;; this is meant to be called with 
;;      emacsclient -c -e '(make-remember-frame)'"
;;   (modify-frame-parameters nil
;;     '( (name . "*Remember*") ;; must be same as in mode-hook below  
;;        (width .  80)
;;        (height . 14)
;;        (vertical-scroll-bars . nil)
;;        (menu-bar-lines . nil)
;;        (tool-bar-lines . nil)))
;;   (org-remember)
;;   (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only....
;;   (delete-other-windows))

;; when we're in such a remember-frame, close it when done.
;; (add-hook 'org-remember-mode-hook
;;   (lambda()
;;     (define-key org-remember-mode-map (kbd "C-c C-c")
;;       '(lambda()(interactive)
;;          (let ((remember-frame-p 
;;                  (string= (frame-parameter nil 'name) "*Remember*")))
;;            (when remember-frame-p (make-frame-invisible))  ;; hide quickly
;;            (org-remember-finalize)
;;            (when remember-frame-p (delete-frame)))))
;;     (define-key org-remember-mode-map (kbd "C-c C-k") 
;;       '(lambda() (interactive)
;;          (let ((remember-frame-p 
;;                  (string= (frame-parameter nil 'name) "*Remember*")))
;;            (when remember-frame-p (make-frame-invisible))  ;; hide quickly
;;            (org-kill-note-or-show-branches)
;;            (when remember-frame-p (delete-frame)))))
;;       ))

(define-key org-mode-map (kbd "C-c r") nil)
(provide 'setup-org)


