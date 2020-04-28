;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../hamrick-init" "../hamrick-init.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ../hamrick-init.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../hamrick-init" '("custom-file" "my-" "risky-local-variable-p")))

;;;***

;;;### (autoloads nil "../init" "../init.el" (0 0 0 0))
;;; Generated autoloads from ../init.el

(autoload 'shell-command-at-line "../init" "\
Run contents of line around point as a shell command and replace the line with output. With a prefix argument, append the output instead

\(fn &optional PREFIX)" t nil)

(autoload 'sudo-this-file "../init" "\
Open the current file as root." t nil)

(autoload 'describe-word "../init" "\
Briefly describe WORD entered by user. With PREFIX argument,
  show verbose descriptions with hyperlinks.

\(fn WORD &optional PREFIX)" t nil)

(autoload 'describe-word-at-point "../init" "\
Briefly describe word at point. With PREFIX argument, show
  verbose descriptions with hyperlinks.

\(fn &optional PREFIX)" t nil)

(autoload 'mechanics "../init" "\
Run mit-scheme with SCMUTILS loaded, to work with (Structure and Interpretation of Classical Mechanics) - The book" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../init" '("+addons-enabled-modes" "auto-byte-recompile" "calc-on-line" "clean-mode-line" "delete-window-if-not-single" "dir-concat" "mode-line-cleaner-alist" "my-evil-leader" "sudo-find-file")))

;;;***

;;;### (autoloads nil "better-buffers" "better-buffers.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from better-buffers.el

(autoload 'toggle-window-split "better-buffers" nil t nil)

(autoload 'swap-windows "better-buffers" "\
If you have 2 windows, it swaps them." t nil)

(autoload 'next-user-buffer "better-buffers" "\
Switch to the next user buffer in cyclic order.

User buffers are those not starting with *." t nil)

(autoload 'previous-user-buffer "better-buffers" "\
Switch to the previous user buffer in cyclic order.

User buffers are those not starting with *." t nil)

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
Insert () around the sentence at point." t nil)

(autoload 'uniquify-all-lines-region "better-editing" "\
Find duplicate lines in region START to END keeping first occurrence.

\(fn START END)" t nil)

(autoload 'uniquify-all-lines-buffer "better-editing" "\
Delete duplicate lines in buffer and keep first occurrence." t nil)

(autoload 'isearch-occur "better-editing" "\
*Invoke `occur' from within isearch." t nil)

(autoload 'isearch-forward-other-buffer "better-editing" "\
Function to isearch-forward in other-window.

\(fn PREFIX)" t nil)

(autoload 'isearch-backward-other-buffer "better-editing" "\
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
prompt for string pairs and return as an association list" nil nil)

(autoload 'replace-in-buffer "better-editing" nil t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "better-editing" '("back-to-indentation-or-beginning" "newline-and-indent" "open-previous-line" "qrr")))

;;;***

;;;### (autoloads nil "icomplete-vertical" "icomplete-vertical.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from icomplete-vertical.el

(defvar icomplete-vertical-mode nil "\
Non-nil if Icomplete-Vertical mode is enabled.
See the `icomplete-vertical-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `icomplete-vertical-mode'.")

(custom-autoload 'icomplete-vertical-mode "icomplete-vertical" nil)

(autoload 'icomplete-vertical-mode "icomplete-vertical" "\
Display icomplete candidates vertically.

If called interactively, enable Icomplete-Vertical mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'icomplete-vertical-toggle "icomplete-vertical" "\
Toggle Icomplete Vertical mode without echo area message." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "icomplete-vertical" '("icomplete-vertical-")))

;;;***

;;;### (autoloads nil "milky-theme" "milky-theme.el" (0 0 0 0))
;;; Generated autoloads from milky-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "milky-theme" '("milky")))

;;;***

;;;### (autoloads nil "personal" "personal.el" (0 0 0 0))
;;; Generated autoloads from personal.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "personal" '("my-")))

;;;***

;;;### (autoloads nil "pretty-latex" "pretty-latex.el" (0 0 0 0))
;;; Generated autoloads from pretty-latex.el

(autoload 'prettify-symbols-latex-symbols "pretty-latex" "\
List of pretty symbols for latex-mode" nil nil)

;;;***

;;;### (autoloads nil "setup-core" "setup-core.el" (0 0 0 0))
;;; Generated autoloads from setup-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-core" '("doom-" "IS-")))

;;;***

;;;### (autoloads nil "setup-dired" "setup-dired.el" (0 0 0 0))
;;; Generated autoloads from setup-dired.el

(autoload 'ora-ediff-files "setup-dired" nil t nil)

;;;***

;;;### (autoloads nil "setup-email" "setup-email.el" (0 0 0 0))
;;; Generated autoloads from setup-email.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-email" '("cg-feed-msmtp")))

;;;***

;;;### (autoloads nil "setup-evil" "setup-evil.el" (0 0 0 0))
;;; Generated autoloads from setup-evil.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-evil" '("+evil-addons-enabled-modes")))

;;;***

;;;### (autoloads nil "setup-icomplete" "setup-icomplete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from setup-icomplete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-icomplete" '("contrib/completing-read-in-region" "icomplete-" "my/icomplete-")))

;;;***

;;;### (autoloads nil "setup-keybinds" "setup-keybinds.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from setup-keybinds.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-keybinds" '("incremental-escape" "doom-l")))

;;;***

;;;### (autoloads nil "setup-minibuffer" "setup-minibuffer.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from setup-minibuffer.el

(autoload 'my/describe-symbol-at-point "setup-minibuffer" "\
Get help (documentation) for the symbol at point.

With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead.

\(fn &optional ARG)" t nil)

(autoload 'my/completions-kill-save-symbol "setup-minibuffer" "\
Add symbol-at-point to the kill ring.

Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'." t nil)

(autoload 'my/focus-minibuffer "setup-minibuffer" "\
Focus the active minibuffer.

Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer." t nil)

(autoload 'my/focus-minibuffer-or-completions "setup-minibuffer" "\
Focus the active minibuffer or the \\*Completions\\*.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`my/focus-minibuffer' and `switch-to-completions' in
succession." t nil)

;;;***

;;;### (autoloads nil "setup-windows" "setup-windows.el" (0 0 0 0))
;;; Generated autoloads from setup-windows.el

(autoload 'buffer-mode "setup-windows" "\
Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode.

\(fn &optional BUFFER-OR-NAME)" nil nil)

(autoload 'select-buffer-in-side-window "setup-windows" "\
Display buffer in a side window and select it

\(fn BUFFER ALIST)" nil nil)

(autoload 'fit-buffer-in-side-window "setup-windows" "\
Display the buffer in a side window and resize it to fit

\(fn BUFFER ALIST)" nil nil)

(autoload '+make-frame-floating-with-current-buffer "setup-windows" "\
Display the current buffer in a new floating frame.

This passes certain parameters to the newly created frame:

- use a different name than the default;
- use a graphical frame;
- do not display the minibuffer.

The name is meant to be used by the external rules of a tiling
window manager to present the frame in a floating state." t nil)

(autoload '+display-buffer-at-bottom "setup-windows" "\
Move the current buffer to the bottom of the frame.  This is
useful to take a buffer out of a side window.

The window parameters of this function are provided mostly for
didactic purposes." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup-windows" '("+help-modes-list" "+occur-grep-modes-list" "+repl-")))

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

;;;### (autoloads nil nil ("backup.el" "setup-anki.el" "setup-completion.el"
;;;;;;  "setup-helm.el" "setup-isearch.el" "setup-ivy.el" "setup-org.el"
;;;;;;  "setup-scheme.el" "setup-ui.el") (0 0 0 0))

;;;***

(provide 'setup-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
