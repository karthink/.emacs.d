;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "better-buffers" "better-buffers.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from better-buffers.el

(autoload 'toggle-window-split "better-buffers" "\


\(fn)" t nil)

(autoload 'swap-windows "better-buffers" "\
If you have 2 windows, it swaps them.

\(fn)" t nil)

(autoload 'next-user-buffer "better-buffers" "\
Switch to the next user buffer in cyclic order.

User buffers are those not starting with *.

\(fn)" t nil)

(autoload 'previous-user-buffer "better-buffers" "\
Switch to the previous user buffer in cyclic order.

User buffers are those not starting with *.

\(fn)" t nil)

(autoload 'scroll-buffer-down "better-buffers" "\
Scroll buffer by (optional) ARG paragraphs.

\(fn &optional ARG)" t nil)

(autoload 'scroll-buffer-up "better-buffers" "\
Scroll buffer by (optional) ARG paragraphs.

\(fn &optional ARG)" t nil)

(autoload 'rename-file-and-buffer "better-buffers" "\
Renames both current buffer and file it's visiting to NEW-NAME.

\(fn NEW-NAME)" t nil)

(autoload 'move-buffer-file "better-buffers" "\
Moves both current buffer and file it's visiting to DIR.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads nil "better-editing" "better-editing.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from better-editing.el

(autoload 'unfill-paragraph "better-editing" "\
Takes a multi-line paragraph and makes it into a single line of text.

\(fn &optional REGION)" t nil)

(autoload 'duplicate-line "better-editing" "\
Duplicate it. With prefix ARG, duplicate ARG lines following the current one.

\(fn &optional ARG)" t nil)

(autoload 'open-next-line "better-editing" "\
Move to the next line and then opens a line.

 See also `newline-and-indent'.

\(fn ARG)" t nil)

(autoload 'zap-up-to-char "better-editing" "\
Zap up to CHAR, (optional) ARG number of times

\(fn ARG CHAR)" t nil)

(autoload 'zap-to-char-save "better-editing" "\
Zap to CHAR, (optional) ARG number of times, but save instead of kill.

\(fn ARG CHAR)" t nil)

(autoload 'goto-match-paren "better-editing" "\
Go to the matching parenthesis if on parenthesis, otherwise do nothing

\(fn ARG)" t nil)

(autoload 'backward-kill-word-or-region "better-editing" "\
Kill word backward if region is inactive; else kill region

\(fn &optional ARG)" t nil)

(autoload 'insert-parentheses-sentence "better-editing" "\
Insert () around the sentence at point.

\(fn)" t nil)

(autoload 'uniquify-all-lines-region "better-editing" "\
Find duplicate lines in region START to END keeping first occurrence.

\(fn START END)" t nil)

(autoload 'uniquify-all-lines-buffer "better-editing" "\
Delete duplicate lines in buffer and keep first occurrence.

\(fn)" t nil)

(autoload 'isearch-occur "better-editing" "\
*Invoke `occur' from within isearch.

\(fn)" t nil)

(autoload 'isearch-backward-other-buffer "better-editing" "\
Function to isearch-forward in other-window.

\(fn PREFIX)" t nil)

(autoload 'isearch-forward-other-buffer "better-editing" "\
Function to isearch-backward in other-window.

\(fn PREFIX)" t nil)

(autoload 'save-buffer-if-visiting-file "better-editing" "\
Save the current buffer only if it is visiting a file

\(fn &optional ARGS)" t nil)

(autoload 'move-text-internal "better-editing" "\


\(fn ARG)" nil nil)

(autoload 'move-text-down "better-editing" "\
Move region (transient-mark-mode active) or current line
  arg lines down.

\(fn ARG)" t nil)

(autoload 'move-text-up "better-editing" "\
Move region (transient-mark-mode active) or current line
  arg lines up.

\(fn ARG)" t nil)

(autoload 'batch-replace-strings "better-editing" "\
Prompt user for pairs of strings to search/replace, then do so in the current buffer

\(fn REPLACEMENT-ALIST)" t nil)

(autoload 'batch-replace-strings-prompt "better-editing" "\
prompt for string pairs and return as an association list

\(fn)" nil nil)

(autoload 'replace-in-buffer "better-editing" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "better-editing" '("open-previous-line" "back-to-indentation-or-beginning" "qrr" "newline-and-indent")))

;;;***

;;;### (autoloads nil "personal" "personal.el" (0 0 0 0))
;;; Generated autoloads from personal.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "personal" '("my-")))

;;;***

;;;### (autoloads nil "setup-core" "setup-core.el" (0 0 0 0))
;;; Generated autoloads from setup-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-core" '("doom-" "IS-")))

;;;***

;;;### (autoloads nil "setup-dired" "setup-dired.el" (0 0 0 0))
;;; Generated autoloads from setup-dired.el

(autoload 'ora-ediff-files "setup-dired" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "setup-email" "setup-email.el" (0 0 0 0))
;;; Generated autoloads from setup-email.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-email" '("cg-feed-msmtp")))

;;;***

;;;### (autoloads nil "setup-evil" "setup-evil.el" (0 0 0 0))
;;; Generated autoloads from setup-evil.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-evil" '(#("+evil-addons-enabled-modes" 0 26 (fontified nil)))))

;;;***

;;;### (autoloads nil "setup-keybinds" "setup-keybinds.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from setup-keybinds.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-keybinds" '("incremental-escape" "doom-l")))

;;;***

;;;### (autoloads nil "setup-org" "setup-org.el" (0 0 0 0))
;;; Generated autoloads from setup-org.el

(autoload '+org-prettify-symbols "setup-org" "\
Set `prettify-symbols-alist' to display LaTeX code as pretty symbols in org-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "utilities" "utilities.el" (0 0 0 0))
;;; Generated autoloads from utilities.el

(autoload 'count-words-region "utilities" "\
Print number of words in the region.

\(fn BEGINNING END)" t nil)

(autoload 'count-words-buffer "utilities" "\
Print number of words in the region.

\(fn)" nil nil)

(autoload 'ascii-table "utilities" "\
Display basic ASCII table (0 thru 127)

\(fn)" t nil)

(autoload 'insert-definition-at-point "utilities" "\
Function to find the definition of the defun at point and insert it there.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "wrap-region" "wrap-region.el" (0 0 0 0))
;;; Generated autoloads from wrap-region.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wrap-region" '("wrap-region")))

;;;***

;;;### (autoloads nil nil ("backup.el" "setup-helm.el" "setup-ivy.el"
;;;;;;  "setup-scheme.el") (0 0 0 0))

;;;***

(provide 'setup-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
