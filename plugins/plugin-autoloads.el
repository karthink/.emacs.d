;;; plugin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "popup-buffers" "popup-buffers.el" (24232 59297
;;;;;;  969479 985000))
;;; Generated autoloads from popup-buffers.el

(autoload '+display-popup-in-side-window "popup-buffers" "\


\(fn BUFFER &optional ALIST)" nil nil)

(autoload 'popup-buffers-find-open-popups "popup-buffers" "\
Find open popup windows in the frame and TODO make a list sorting them by active time. old-list has the open-popups from the last call." nil nil)

(autoload 'popup-buffers-update-open-popups "popup-buffers" nil nil nil)

(autoload 'popup-buffers-update-closed-popups "popup-buffers" nil nil nil)

(autoload 'popup-buffers-close-latest "popup-buffers" "\
Close the last opened popup" t nil)

(autoload 'popup-buffers-open-latest "popup-buffers" "\
Open the last closed popup" t nil)

(autoload 'popup-buffers-bury-all "popup-buffers" nil nil nil)

(autoload 'popup-buffers-raise-all "popup-buffers" nil nil nil)

(autoload 'popup-buffers-toggle-latest "popup-buffers" "\
Toggle visibility of the last opened popup window" t nil)

(autoload 'popup-buffers-cycle "popup-buffers" "\
Cycle visibility of popup windows one at a time. With a prefix argument, cycle in the opposite direction.

\(fn &optional ARG)" t nil)

(autoload 'popup-buffers-raise-popup "popup-buffers" "\
Raise a popup to regular status

\(fn &optional BUFFER)" t nil)

(autoload 'popup-buffers-lower-to-popup "popup-buffers" "\
Turn a regular window into a popup

\(fn &optional BUFFER)" t nil)

(defvar popup-buffers-mode nil "\
Non-nil if Popup-Buffers mode is enabled.
See the `popup-buffers-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `popup-buffers-mode'.")

(custom-autoload 'popup-buffers-mode "popup-buffers" nil)

(autoload 'popup-buffers-mode "popup-buffers" "\
To be added

If called interactively, enable Popup-Buffers mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "popup-buffers" '("popup-buffers-")))

;;;***

(provide 'plugin-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; plugin-autoloads.el ends here
