;;; plugin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "company-matlab" "company-matlab.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from company-matlab.el

(autoload 'company-matlab "company-matlab" "\
A `company-mode' completion back-end for matlab-mode and matlab-shell-mode.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-matlab" '("company-matlab-")))

;;;***

;;;### (autoloads nil "evil-matlab-textobjects" "evil-matlab-textobjects.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matlab-textobjects.el

(autoload 'evil-matlab-textobjects-mode "evil-matlab-textobjects" "\
Minor mode for latex-specific text objects in evil.

If called interactively, enable Evil-Matlab-Textobjects mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

Installs the following additional text objects:
\\<evil-latex-textobjects-outer-map>
  \\[evil-matlab-textobjects-a-math]	Display math		\\=\\[ .. \\=\\]
  \\[evil-matlab-textobjects-a-dollar]	Inline math		$ .. $
  \\[evil-matlab-textobjects-a-macro]	TeX macro		\\foo{..}
  \\[evil-matlab-textobjects-an-env]	LaTeX environment	\\begin{foo}..\\end{foo}

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-matlab-textobjects-mode "evil-matlab-textobjects" "\
Enable evil-matlab-textobjects-mode in current buffer." t nil)

(autoload 'turn-off-evil-matlab-textobjects-mode "evil-matlab-textobjects" "\
Disable evil-matlab-textobjects-mode in current buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-matlab-textobjects" '("evil-matlab-textobjects-")))

;;;***

;;;### (autoloads nil "inkscape-figures" "inkscape-figures.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from inkscape-figures.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inkscape-figures" '("+inkscape-figures-")))

;;;***

;;;### (autoloads nil "iscroll" "iscroll.el" (0 0 0 0))
;;; Generated autoloads from iscroll.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "iscroll" '("iscroll-")))

;;;***

;;;### (autoloads nil "math" "math.el" (0 0 0 0))
;;; Generated autoloads from math.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "math" '("Mathematica-search-path" "backward-incarnations" "check-" "emacs-version-18" "find-math-error" "get-math-completion" "indent-cookie-p" "kill-" "math" "metered-process-send-string" "old-kill-math-cell" "set-math-" "skip-over-white-lines" "start-math")))

;;;***

;;;### (autoloads nil "mathematica" "mathematica.el" (0 0 0 0))
;;; Generated autoloads from mathematica.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mathematica" '("mathematica")))

;;;***

;;;### (autoloads nil "ob-octave-fix" "ob-octave-fix.el" (0 0 0 0))
;;; Generated autoloads from ob-octave-fix.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-octave-fix" '("multi-replace-regexp-in-string" "org-babel-octave-evaluate-session" "remove-car-upto-newline")))

;;;***

;;;### (autoloads nil "ol" "ol.el" (0 0 0 0))
;;; Generated autoloads from ol.el

(autoload 'org-next-link "ol" "\
Move forward to the next link.
If the link is in hidden text, expose it.  When SEARCH-BACKWARD
is non-nil, move backward.

\(fn &optional SEARCH-BACKWARD)" t nil)

(autoload 'org-previous-link "ol" "\
Move backward to the previous link.
If the link is in hidden text, expose it." t nil)

(autoload 'org-toggle-link-display "ol" "\
Toggle the literal or descriptive display of links." t nil)

(autoload 'org-store-link "ol" "\
Store a link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  A single
`\\[universal-argument]' negates `org-context-in-file-links' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix ARG forces storing a link for each line in the
active region.

Assume the function is called interactively if INTERACTIVE? is
non-nil.

\(fn ARG &optional INTERACTIVE\\=\\?)" t nil)

(autoload 'org-insert-link "ol" "\
Insert a link.  At the prompt, enter the link.

Completion can be used to insert any of the link protocol prefixes in use.

The history can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press `RET' at the prompt), the link defaults to the most recently
stored link.  As `SPC' triggers completion in the minibuffer, you need to
use `M-SPC' or `C-q SPC' to force the insertion of a space character.

You will also be prompted for a description, and if one is given, it will
be displayed in the buffer instead of the link.

If there is already a link at point, this command will allow you to edit
link and description parts.

With a `\\[universal-argument]' prefix, prompts for a file to link to.  The file name can be
selected using completion.  The path to the file will be relative to the
current directory if the file is in the current directory or a subdirectory.
Otherwise, the link will be the absolute path as completed in the minibuffer
\(i.e. normally ~/path/to/file).  You can configure this behavior using the
option `org-link-file-path-type'.

With a `\\[universal-argument] \\[universal-argument]' prefix, enforce an absolute path even if the file is in
the current directory or below.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix negates `org-link-keep-stored-after-insertion'.

If the LINK-LOCATION parameter is non-nil, this value will be used as
the link location instead of reading one interactively.

If the DESCRIPTION parameter is non-nil, this value will be used as the
default description.  Otherwise, if `org-link-make-description-function'
is non-nil, this function will be called with the link target, and the
result will be the default link description.  When called non-interactively,
don't allow to edit the default description.

\(fn &optional COMPLETE-FILE LINK-LOCATION DESCRIPTION)" t nil)

(autoload 'org-insert-all-links "ol" "\
Insert all links in `org-stored-links'.
When a universal prefix, do not delete the links from `org-stored-links'.
When `ARG' is a number, insert the last N link(s).
`PRE' and `POST' are optional arguments to define a string to
prepend or to append.

\(fn ARG &optional PRE POST)" t nil)

(autoload 'org-insert-last-stored-link "ol" "\
Insert the last link stored in `org-stored-links'.

\(fn ARG)" t nil)

(autoload 'org-insert-link-global "ol" "\
Insert a link like Org mode does.
This command can be called in any mode to insert a link in Org syntax." t nil)

(autoload 'org-update-radio-target-regexp "ol" "\
Find all radio targets in this file and update the regular expression.
Also refresh fontification if needed." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol" '("org-")))

;;;***

;;;### (autoloads nil "ol-notmuch" "ol-notmuch.el" (0 0 0 0))
;;; Generated autoloads from ol-notmuch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-notmuch" '("org-notmuch-")))

;;;***

;;;### (autoloads nil "sweet-kill" "sweet-kill.el" (0 0 0 0))
;;; Generated autoloads from sweet-kill.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sweet-kill" '("haiku-emacs")))

;;;***

;;;### (autoloads nil "themed-ltximg" "themed-ltximg.el" (0 0 0 0))
;;; Generated autoloads from themed-ltximg.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "themed-ltximg" '("my/org-latex-")))

;;;***

(provide 'plugin-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; plugin-autoloads.el ends here
