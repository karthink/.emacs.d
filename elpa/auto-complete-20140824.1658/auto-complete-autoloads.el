;;; auto-complete-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-auto-complete-mode auto-complete-mode auto-complete)
;;;;;;  "auto-complete" "auto-complete.el" (21515 63638))
;;; Generated autoloads from auto-complete.el

(autoload 'auto-complete "auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

(autoload 'auto-complete-mode "auto-complete" "\
AutoComplete mode

\(fn &optional ARG)" t nil)

(defvar global-auto-complete-mode nil "\
Non-nil if Global-Auto-Complete mode is enabled.
See the command `global-auto-complete-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-auto-complete-mode'.")

(custom-autoload 'global-auto-complete-mode "auto-complete" nil)

(autoload 'global-auto-complete-mode "auto-complete" "\
Toggle Auto-Complete mode in every possible buffer.
With prefix ARG, turn Global-Auto-Complete mode on if and only if
ARG is positive.
Auto-Complete mode is enabled in all buffers where
`auto-complete-mode-maybe' would do it.
See `auto-complete-mode' for more information on Auto-Complete mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ac-config-default) "auto-complete-config" "auto-complete-config.el"
;;;;;;  (21515 63639))
;;; Generated autoloads from auto-complete-config.el

(autoload 'ac-config-default "auto-complete-config" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("auto-complete-pkg.el") (21515 63639 66779))

;;;***

(provide 'auto-complete-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-complete-autoloads.el ends here
