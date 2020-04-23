;; ----------------------------
;; Completion
;; ----------------------------

(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
      dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")

;(setq dabbrev-backward-only nil)
;(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search t)
;(setq dabbrev-case-replace nil)
;(setq dabbrev-check-other-buffers t)
;(setq dabbrev-eliminate-newlines nil)
;(setq dabbrev-upcase-means-case-search t)

(when (require 'hippie-expand nil t)
  (setq hippie-expand-try-functions-list 
	'(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))
(global-set-key (kbd "M-/") #'hippie-expand))



(provide 'setup-completion)
