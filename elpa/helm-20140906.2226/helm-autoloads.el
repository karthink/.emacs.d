;;; helm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-other-buffer helm helm-debug-open-last-log
;;;;;;  helm-define-key-with-subkeys helm-multi-key-defun helm-define-multi-key)
;;;;;;  "helm" "helm.el" (21517 3090))
;;; Generated autoloads from helm.el

(autoload 'helm-define-multi-key "helm" "\
In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function run sequentialy each time the key KEY is pressed.
If DELAY is specified switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list are functions with no args.
e.g
  (defun foo ()
    (message \"Run foo\"))
  (defun bar ()
    (message \"Run bar\"))
  (defun baz ()
    (message \"Run baz\"))

\(helm-define-multi-key global-map \"<f5> q\" '(foo bar baz) 2)

Each time \"<f5> q\" is pressed the next function is executed, if you wait
More than 2 seconds, next hit will run again the first function and so on.

\(fn KEYMAP KEY FUNCTIONS &optional DELAY)" nil nil)

(autoload 'helm-multi-key-defun "helm" "\
Define NAME as a multi-key command running FUNS.
After DELAY seconds the FUNS list is reinitialised.
See `helm-define-multi-key'.

\(fn NAME DOCSTRING FUNS &optional DELAY)" nil (quote macro))

(autoload 'helm-define-key-with-subkeys "helm" "\
Allow defining a KEY without having to type its prefix again on next calls.
Arg MAP is the keymap to use, SUBKEY is the initial long keybinding to
call COMMAND.
Arg OTHER-SUBKEYS is an unquoted alist specifying other short keybindings
to use once started.
e.g:

\(helm-define-key-with-subkeys global-map
      (kbd \"C-x v n\") ?n 'git-gutter:next-hunk ((?p . 'git-gutter:previous-hunk)))


In this example, `C-x v n' will run `git-gutter:next-hunk' subsequent hit on \"n\"
will run this command again and subsequent hit on \"p\" will run `git-gutter:previous-hunk'.

Arg MENU is a string to display in minibuffer to describe SUBKEY and OTHER-SUBKEYS.
Arg EXIT-FN specify a function to run on exit.

Any other keys pressed run their assigned command defined in MAP and exit the loop.

\(fn MAP KEY SUBKEY COMMAND &optional OTHER-SUBKEYS MENU EXIT-FN)" nil (quote macro))

(autoload 'helm-debug-open-last-log "helm" "\
Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to `helm-debug-buffer' .

\(fn)" t nil)

(autoload 'helm "helm" "\
Main function to execute helm sources.

Keywords supported:
:sources :input :prompt :resume :preselect
:buffer :keymap :default :history :allow-nest

Extra LOCAL-VARS keywords are supported, see below.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
\(&optional sources input prompt resume
            preselect buffer keymap default history).

Basic keywords are the following:

:sources

A list of sources used for this session.  It also accepts a
symbol, interpreted as a variable of a helm source
i.e (a symbol can be passed instead of a list of sources).
It also accepts an alist representing a helm source, which is
detected by (assq 'name ANY-SOURCES).
NOTE: In this case the source is embedded in the helm command and
have no symbol name, so it is not reachable from outside.
It will be referenced in `helm-sources' as a whole alist.

:input

Temporary value of `helm-pattern', ie. initial input of minibuffer.

:prompt

Prompt other than \"pattern: \".

:resume

If t, Resurrect previously instance of `helm'.  Skip the initialization.
If 'noresume, this instance of `helm' cannot be resumed.

:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.

:buffer

`helm-buffer' instead of *helm*.

:keymap

`helm-map' for current `helm' session.

:default

A default argument that will be inserted in minibuffer with \\<minibuffer-local-map>\\[next-history-element].
When nil or not present `thing-at-point' will be used instead.
If `helm-maybe-use-default-as-input' is non--nil display will be
updated using :default arg as input unless :input is specified,
which in this case will take precedence on :default
This is a string or a list, in this case the car of the list will
be used as initial default input, but you will be able to cycle in this
list with \\<minibuffer-local-map>\\[next-history-element].

:history

By default all minibuffer input is pushed to `minibuffer-history',
if an argument HISTORY is provided, input will be pushed to HISTORY.
History element should be a symbol.

:allow-nest

Allow running this helm command within a running helm session.

Of course, conventional arguments are supported, the two are same.

\(helm :sources sources :input input :prompt prompt :resume resume
       :preselect preselect :buffer buffer :keymap keymap :default default
       :history history)

and

\(helm sources input prompt resume preselect buffer keymap default history)

are the same.

However the use of non keyword args is deprecated and should not be used.

Other keywords are interpreted as local variables of this helm session.
The `helm-' prefix can be omitted.  For example,

\(helm :sources 'helm-source-buffers-list
       :buffer \"*buffers*\" :candidate-number-limit 10)

means starting helm session with `helm-source-buffers'
source in *buffers* buffer and set variable `helm-candidate-number-limit'
to 10 as session local variable.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)" nil nil)

(autoload 'helm-other-buffer "helm" "\
Simplified interface of `helm' with other `helm-buffer'.
Call `helm' with only ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

;;;***

;;;### (autoloads (helm-reset-adaptive-history) "helm-adaptive" "helm-adaptive.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-adaptive.el

(autoload 'helm-reset-adaptive-history "helm-adaptive" "\
Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-apt) "helm-apt" "helm-apt.el" (21517 3089))
;;; Generated autoloads from helm-apt.el

(autoload 'helm-apt "helm-apt" "\
Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-bbdb) "helm-bbdb" "helm-bbdb.el" (21517 3089))
;;; Generated autoloads from helm-bbdb.el

(autoload 'helm-bbdb "helm-bbdb" "\
Preconfigured `helm' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-filtered-bookmarks helm-pp-bookmarks helm-bookmarks)
;;;;;;  "helm-bookmark" "helm-bookmark.el" (21517 3089))
;;; Generated autoloads from helm-bookmark.el

(autoload 'helm-bookmarks "helm-bookmark" "\
Preconfigured `helm' for bookmarks.

\(fn)" t nil)

(autoload 'helm-pp-bookmarks "helm-bookmark" "\
Preconfigured `helm' for bookmarks (pretty-printed).

\(fn)" t nil)

(autoload 'helm-filtered-bookmarks "helm-bookmark" "\
Preconfigured helm for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded
only if external library addressbook-bookmark.el is available.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-buffers-list) "helm-buffers" "helm-buffers.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-buffers.el

(autoload 'helm-buffers-list "helm-buffers" "\
Preconfigured `helm' to list buffers.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-colors) "helm-color" "helm-color.el" (21517
;;;;;;  3090))
;;; Generated autoloads from helm-color.el

(autoload 'helm-colors "helm-color" "\
Preconfigured `helm' for color.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-M-x) "helm-command" "helm-command.el" (21517
;;;;;;  3089))
;;; Generated autoloads from helm-command.el

(autoload 'helm-M-x "helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

Unlike regular `M-x' emacs vanilla `execute-extended-command' command,
the prefix args if needed, are passed AFTER starting `helm-M-x'.

You can get help on each command by persistent action.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-configuration) "helm-config" "helm-config.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-config.el

(autoload 'helm-configuration "helm-config" "\
Customize `helm'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-dabbrev) "helm-dabbrev" "helm-dabbrev.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-dabbrev.el

(autoload 'helm-dabbrev "helm-dabbrev" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-complex-command-history helm-timers helm-locate-library
;;;;;;  helm-manage-advice helm-apropos helm-lisp-completion-or-file-name-at-point
;;;;;;  helm-lisp-indent helm-complete-file-name-at-point helm-lisp-completion-at-point)
;;;;;;  "helm-elisp" "helm-elisp.el" (21517 3090))
;;; Generated autoloads from helm-elisp.el

(autoload 'helm-lisp-completion-at-point "helm-elisp" "\
Helm lisp symbol completion at point.

\(fn)" t nil)

(autoload 'helm-complete-file-name-at-point "helm-elisp" "\
Complete file name at point.

\(fn &optional FORCE)" t nil)

(autoload 'helm-lisp-indent "helm-elisp" "\
Not documented

\(fn)" t nil)

(autoload 'helm-lisp-completion-or-file-name-at-point "helm-elisp" "\
Complete lisp symbol or filename at point.
Filename completion happen if string start after or between a double quote.

\(fn)" t nil)

(autoload 'helm-apropos "helm-elisp" "\
Preconfigured helm to describe commands, functions, variables and faces.

\(fn)" t nil)

(autoload 'helm-manage-advice "helm-elisp" "\
Preconfigured `helm' to disable/enable function advices.

\(fn)" t nil)

(autoload 'helm-locate-library "helm-elisp" "\
Not documented

\(fn)" t nil)

(autoload 'helm-timers "helm-elisp" "\
Preconfigured `helm' for timers.

\(fn)" t nil)

(autoload 'helm-complex-command-history "helm-elisp" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-list-elisp-packages) "helm-elisp-package"
;;;;;;  "helm-elisp-package.el" (21517 3089))
;;; Generated autoloads from helm-elisp-package.el

(autoload 'helm-list-elisp-packages "helm-elisp-package" "\
Not documented

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-elscreen) "helm-elscreen" "helm-elscreen.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-elscreen.el

(autoload 'helm-elscreen "helm-elscreen" "\
Preconfigured helm to list elscreen.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-emms) "helm-emms" "helm-emms.el" (21517 3089))
;;; Generated autoloads from helm-emms.el

(autoload 'helm-emms "helm-emms" "\
Preconfigured `helm' for emms sources.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-eshell-history helm-esh-pcomplete) "helm-eshell"
;;;;;;  "helm-eshell.el" (21517 3090))
;;; Generated autoloads from helm-eshell.el

(autoload 'helm-esh-pcomplete "helm-eshell" "\
Preconfigured helm to provide helm completion in eshell.

\(fn)" t nil)

(autoload 'helm-eshell-history "helm-eshell" "\
Preconfigured helm for eshell history.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-calcul-expression helm-eval-expression-with-eldoc
;;;;;;  helm-eval-expression) "helm-eval" "helm-eval.el" (21517 3090))
;;; Generated autoloads from helm-eval.el

(autoload 'helm-eval-expression "helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'helm-eval-expression-with-eldoc "helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'helm-calcul-expression "helm-eval" "\
Preconfigured helm for `helm-source-calculation-result'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-run-external-command) "helm-external" "helm-external.el"
;;;;;;  (21517 3091))
;;; Generated autoloads from helm-external.el

(autoload 'helm-run-external-command "helm-external" "\
Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'.

\(fn PROGRAM)" t nil)

;;;***

;;;### (autoloads (helm-recentf helm-for-files helm-find-files helm-find
;;;;;;  helm-browse-project) "helm-files" "helm-files.el" (21517
;;;;;;  3090))
;;; Generated autoloads from helm-files.el

(autoload 'helm-browse-project "helm-files" "\
Browse files and see status of project with its vcs.
Only hg and git are supported for now.
Fall back to `helm-find-files' if directory is not under
control of one of those vcs.
Need dependencies:
<https://github.com/emacs-helm/helm-ls-git.git>
and
<https://github.com/emacs-helm/helm-mercurial-queue/blob/master/helm-ls-hg.el>.

\(fn)" t nil)

(autoload 'helm-find "helm-files" "\
Preconfigured `helm' for the find shell command.

\(fn ARG)" t nil)

(autoload 'helm-find-files "helm-files" "\
Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'helm-for-files "helm-files" "\
Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'.

\(fn)" t nil)

(autoload 'helm-recentf "helm-files" "\
Preconfigured `helm' for `recentf'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-firefox-bookmarks) "helm-firefox" "helm-firefox.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-firefox.el

(autoload 'helm-firefox-bookmarks "helm-firefox" "\
Preconfigured `helm' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-ucs helm-select-xfont) "helm-font" "helm-font.el"
;;;;;;  (21517 3089))
;;; Generated autoloads from helm-font.el

(autoload 'helm-select-xfont "helm-font" "\
Preconfigured `helm' to select Xfont.

\(fn)" t nil)

(autoload 'helm-ucs "helm-font" "\
Preconfigured helm for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-gentoo) "helm-gentoo" "helm-gentoo.el" (21517
;;;;;;  3090))
;;; Generated autoloads from helm-gentoo.el

(autoload 'helm-gentoo "helm-gentoo" "\
Preconfigured `helm' for gentoo linux.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-do-pdfgrep helm-do-zgrep helm-do-grep helm-grep-mode-jump-other-window
;;;;;;  helm-grep-mode-jump-other-window-backward helm-grep-mode-jump-other-window-forward
;;;;;;  helm-grep-mode-jump helm-gm-precedent-file helm-gm-next-file
;;;;;;  helm-grep-mode helm-grep-run-save-buffer helm-goto-next-file
;;;;;;  helm-goto-precedent-file) "helm-grep" "helm-grep.el" (21517
;;;;;;  3090))
;;; Generated autoloads from helm-grep.el

(autoload 'helm-goto-precedent-file "helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-goto-next-file "helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-grep-run-save-buffer "helm-grep" "\
Run grep save results action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-grep-mode "helm-grep" "\
Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}

\(fn)" t nil)

(autoload 'helm-gm-next-file "helm-grep" "\
Not documented

\(fn)" t nil)

(autoload 'helm-gm-precedent-file "helm-grep" "\
Not documented

\(fn)" t nil)

(autoload 'helm-grep-mode-jump "helm-grep" "\
Not documented

\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window-forward "helm-grep" "\
Not documented

\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window-backward "helm-grep" "\
Not documented

\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window "helm-grep" "\
Not documented

\(fn)" t nil)

(autoload 'helm-do-grep "helm-grep" "\
Preconfigured helm for grep.
Contrarily to Emacs `grep', no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can also use wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep (recurse).
The prefix arg can be passed before or after start file selection.
See also `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-do-zgrep "helm-grep" "\
Preconfigured helm for zgrep.

\(fn)" t nil)

(autoload 'helm-do-pdfgrep "helm-grep" "\
Preconfigured helm for pdfgrep.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-describe-helm-attribute helm-semantic-help
;;;;;;  helm-color-help helm-imenu-help helm-M-x-help helm-el-package-help
;;;;;;  helm-apt-help helm-top-help helm-moccur-help helm-buffers-ido-virtual-help
;;;;;;  helm-esh-help helm-bookmark-help helm-ucs-help helm-etags-help
;;;;;;  helm-pdfgrep-help helm-grep-help helm-generic-file-help helm-read-file-name-help
;;;;;;  helm-ff-help helm-buffer-help helm-help) "helm-help" "helm-help.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-help.el

(autoload 'helm-help "helm-help" "\
Help of `helm'.

\(fn)" t nil)

(autoload 'helm-buffer-help "helm-help" "\
Help command for helm buffers.

\(fn)" t nil)

(autoload 'helm-ff-help "helm-help" "\
Help command for `helm-find-files'.

\(fn)" t nil)

(autoload 'helm-read-file-name-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-generic-file-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-grep-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-pdfgrep-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-etags-help "helm-help" "\
The help function for etags.

\(fn)" t nil)

(autoload 'helm-ucs-help "helm-help" "\
Help command for `helm-ucs'.

\(fn)" t nil)

(autoload 'helm-bookmark-help "helm-help" "\
Help command for bookmarks.

\(fn)" t nil)

(autoload 'helm-esh-help "helm-help" "\
Help command for `helm-find-files-eshell-command-on-file'.

\(fn)" t nil)

(autoload 'helm-buffers-ido-virtual-help "helm-help" "\
Help command for ido virtual buffers.

\(fn)" t nil)

(autoload 'helm-moccur-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-top-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-apt-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-el-package-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-M-x-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-imenu-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-color-help "helm-help" "\
Not documented

\(fn)" t nil)

(autoload 'helm-semantic-help "helm-help" "\
Not documented

\(fn)" t nil)

(defvar helm-buffer-mode-line-string '("Buffer(s)" "\\<helm-buffer-map>\\[helm-buffer-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct" "String displayed in mode-line in `helm-source-buffers-list'"))

(defvar helm-color-mode-line-string '("Colors" "\\<helm-color-map>\\[helm-color-help]:Help/\\[helm-color-run-insert-name]:Insert name/\\[helm-color-run-insert-rgb]:Insert RGB/with shift: Kill"))

(defvar helm-buffers-ido-virtual-mode-line-string '("Killed Buffer(s)" "\\<helm-buffers-ido-virtual-map>\\[helm-buffers-ido-virtual-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct" "String displayed in mode-line in `helm-source-buffers-list'"))

(defvar helm-ff-mode-line-string "\\<helm-find-files-map>\\[helm-ff-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct" "\
String displayed in mode-line in `helm-source-find-files'")

(defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-read-file-name-help]:Help \\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct" "\
String displayed in mode-line in `helm-source-find-files'.")

(defvar helm-generic-file-mode-line-string "\\<helm-generic-files-map>\\[helm-generic-file-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in Locate.")

(defvar helm-grep-mode-line-string "\\<helm-grep-map>\\[helm-grep-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in `helm-do-grep'.")

(defvar helm-pdfgrep-mode-line-string "\\<helm-pdfgrep-map>\\[helm-pdfgrep-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in `helm-do-pdfgrep'.")

(defvar helm-etags-mode-line-string "\\<helm-etags-map>\\[helm-etags-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct" "\
String displayed in mode-line in `helm-etags-select'.")

(defvar helm-ucs-mode-line-string "\\<helm-ucs-map>\\[helm-ucs-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct" "\
String displayed in mode-line in `helm-ucs'.")

(defvar helm-bookmark-mode-line-string '("Bookmark(s)" "\\<helm-bookmark-map>\\[helm-bookmark-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct") "\
String displayed in mode-line in `helm-source-buffers-list'")

(defvar helm-occur-mode-line "\\<helm-map>\\[helm-help]:Help \\<helm-occur-map>\\[helm-occur-run-query-replace-regexp]:Query replace regexp \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-moccur-mode-line "\\<helm-moccur-map>\\[helm-moccur-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-comp-read-mode-line "\\<helm-comp-read-map>\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct")

(defvar helm-top-mode-line "\\<helm-top-map>\\[helm-top-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-apt-mode-line "\\<helm-apt-map>\\[helm-apt-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-el-package-mode-line "\\<helm-el-package-map>\\[helm-el-package-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-M-x-mode-line "\\<helm-M-x-map>\\[helm-M-x-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-imenu-mode-line "\\<helm-imenu-map>\\[helm-imenu-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-semantic-mode-line "\\<helm-semantic-map>\\[helm-semantic-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(autoload 'helm-describe-helm-attribute "helm-help" "\
Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol.

\(fn HELM-ATTRIBUTE)" t nil)

;;;***

;;;### (autoloads (helm-imenu) "helm-imenu" "helm-imenu.el" (21517
;;;;;;  3089))
;;; Generated autoloads from helm-imenu.el

(autoload 'helm-imenu "helm-imenu" "\
Preconfigured `helm' for `imenu'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-info-at-point) "helm-info" "helm-info.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-info.el

(autoload 'helm-info-at-point "helm-info" "\
Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-locate helm-locate-read-file-name) "helm-locate"
;;;;;;  "helm-locate.el" (21517 3090))
;;; Generated autoloads from helm-locate.el

(autoload 'helm-locate-read-file-name "helm-locate" "\
Not documented

\(fn PROMPT)" nil nil)

(autoload 'helm-locate "helm-locate" "\
Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it
if it doesn't exists.
Many databases can be used: navigate and mark them.
See also `helm-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-man-woman) "helm-man" "helm-man.el" (21517
;;;;;;  3091))
;;; Generated autoloads from helm-man.el

(autoload 'helm-man-woman "helm-man" "\
Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-match-plugin-mode) "helm-match-plugin" "helm-match-plugin.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-match-plugin.el

(defvar helm-match-plugin-mode nil "\
Non-nil if Helm-Match-Plugin mode is enabled.
See the command `helm-match-plugin-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-match-plugin-mode'.")

(custom-autoload 'helm-match-plugin-mode "helm-match-plugin" nil)

(autoload 'helm-match-plugin-mode "helm-match-plugin" "\
Add more flexible regexp matching for helm.
See `helm-mp-matching-method' for the behavior of each method.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-comint-input-ring helm-minibuffer-history
;;;;;;  helm-mini helm-stumpwm-commands helm-ratpoison-commands helm-eev-anchors
;;;;;;  helm-insert-latex-math helm-world-time helm-browse-menubar)
;;;;;;  "helm-misc" "helm-misc.el" (21517 3090))
;;; Generated autoloads from helm-misc.el

(autoload 'helm-browse-menubar "helm-misc" "\
Helm interface to the menubar using lacarte.el.

\(fn)" t nil)

(autoload 'helm-world-time "helm-misc" "\
Preconfigured `helm' to show world time.

\(fn)" t nil)

(autoload 'helm-insert-latex-math "helm-misc" "\
Preconfigured helm for latex math symbols completion.

\(fn)" t nil)

(autoload 'helm-eev-anchors "helm-misc" "\
Preconfigured `helm' for eev anchors.

\(fn)" t nil)

(autoload 'helm-ratpoison-commands "helm-misc" "\
Preconfigured `helm' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'helm-stumpwm-commands "helm-misc" "\
Not documented

\(fn)" t nil)

(autoload 'helm-mini "helm-misc" "\
Preconfigured `helm' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'helm-minibuffer-history "helm-misc" "\
Preconfigured `helm' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'helm-comint-input-ring "helm-misc" "\
Predefined `helm' that provide completion of `comint' history.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-mode) "helm-mode" "helm-mode.el" (21517 3090))
;;; Generated autoloads from helm-mode.el

(cl-defun helm-comp-read (prompt collection &key test initial-input default preselect (buffer "*Helm Completions*") must-match reverse-history (requires-pattern 0) history input-history (case-fold helm-comp-read-case-fold-search) (del-input t) (persistent-action nil) (persistent-help "DoNothing") (mode-line helm-comp-read-mode-line) (keymap helm-comp-read-map) (name "Helm Completions") candidates-in-buffer exec-when-only-one quit-when-no-cand (volatile t) sort (fc-transformer 'helm-cr-default-transformer) marked-candidates nomark (alistp t)) "Read a string in the minibuffer, with helm completion.\n\nIt is helm `completing-read' equivalent.\n\n- PROMPT is the prompt name to use.\n\n- COLLECTION can be a list, vector, obarray or hash-table.\n  It can be also a function that receives three arguments:\n  the values string, predicate and t. See `all-completions' for more details.\n\nKeys description:\n\n- TEST: A predicate called with one arg i.e candidate.\n\n- INITIAL-INPUT: Same as input arg in `helm'.\n\n- PRESELECT: See preselect arg of `helm'.\n\n- DEFAULT: This option is used only for compatibility with regular\n  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').\n\n- BUFFER: Name of helm-buffer.\n\n- MUST-MATCH: Candidate selected must be one of COLLECTION.\n\n- REVERSE-HISTORY: When non--nil display history source after current\n  source completion.\n\n- REQUIRES-PATTERN: Same as helm attribute, default is 0.\n\n- HISTORY: A list containing specific history, default is nil.\n  When it is non--nil, all elements of HISTORY are displayed in\n  a special source before COLLECTION.\n\n- INPUT-HISTORY: A symbol. the minibuffer input history will be\n  stored there, if nil or not provided, `minibuffer-history'\n  will be used instead.\n\n- CASE-FOLD: Same as `helm-case-fold-search'.\n\n- DEL-INPUT: Boolean, when non--nil (default) remove the partial\n  minibuffer input from HISTORY is present.\n\n- PERSISTENT-ACTION: A function called with one arg i.e candidate.\n\n- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.\n\n- MODE-LINE: A string or list to display in mode line.\n  Default is `helm-comp-read-mode-line'.\n\n- KEYMAP: A keymap to use in this `helm-comp-read'.\n  (the keymap will be shared with history source)\n\n- NAME: The name related to this local source.\n\n- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'\n  to non--nil. (possibles values are t or nil).\n\n- VOLATILE: Use volatile attribute (enabled by default).\n\n- SORT: A predicate to give to `sort' e.g `string-lessp'.\n\n- FC-TRANSFORMER: A `filtered-candidate-transformer' function.\n\n- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.\n\n- NOMARK: When non--nil don't allow marking candidates.\n\n- ALISTP: (default is non--nil) See `helm-comp-read-get-candidates'.\n\n- CANDIDATES-IN-BUFFER: when non--nil use a source build with\n  `helm-candidates-in-buffer' which is much faster.\n  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.\n\nAny prefix args passed during `helm-comp-read' invocation will be recorded\nin `helm-current-prefix-arg', otherwise if prefix args were given before\n`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.\nThat's mean you can pass prefix args before or after calling a command\nthat use `helm-comp-read' See `helm-M-x' for example." (when (get-buffer helm-action-buffer) (kill-buffer helm-action-buffer)) (let ((action-fn `(("Sole action (Identity)" lambda (candidate) (if ,marked-candidates (helm-marked-candidates) (identity candidate)))))) (when (eq must-match 'confirm-after-completion) (setq must-match 'confirm)) (let* ((minibuffer-completion-confirm must-match) (must-match-map (when must-match (let ((map (make-sparse-keymap))) (define-key map (kbd "RET") 'helm-confirm-and-exit-minibuffer) map))) (loc-map (if must-match-map (make-composed-keymap must-match-map (or keymap helm-map)) (or keymap helm-map))) (helm-read-file-name-mode-line-string (replace-regexp-in-string "helm-maybe-exit-minibuffer" "helm-confirm-and-exit-minibuffer" helm-read-file-name-mode-line-string)) (get-candidates (lambda nil (let ((cands (helm-comp-read-get-candidates collection test sort alistp))) (setq helm-cr-unknown-pattern-flag nil) (unless (or (eq must-match t) (string= helm-pattern "") (assoc helm-pattern cands) (assoc (intern helm-pattern) cands) (member helm-pattern cands)) (setq cands (append (list helm-pattern) cands)) (setq helm-cr-unknown-pattern-flag t)) (if (and default (not (string= default ""))) (delq nil (cons default (delete default cands))) cands)))) (history-get-candidates (lambda nil (let ((all (helm-comp-read-get-candidates history test nil alistp))) (when all (delete "" (helm-fast-remove-dups (if (and default (not (string= default ""))) (delq nil (cons default (delete default all))) all) :test 'equal)))))) (src-hist `((name \, (format "%s History" name)) (candidates \, history-get-candidates) (filtered-candidate-transformer lambda (candidates sources) (cl-loop for i in candidates for cand = (replace-regexp-in-string "\\s\\" "" i) do (set-text-properties 0 (length cand) nil cand) collect cand)) (persistent-action \, persistent-action) (persistent-help \, persistent-help) (keymap \, loc-map) (mode-line \, mode-line) (action \, action-fn))) (src `((name \, name) (candidates \, get-candidates) (filtered-candidate-transformer \, fc-transformer) (requires-pattern \, requires-pattern) (persistent-action \, persistent-action) (persistent-help \, persistent-help) (keymap \, loc-map) (mode-line \, mode-line) (action \, action-fn))) (src-1 `((name \, name) (init lambda nil (helm-init-candidates-in-buffer 'global (funcall ',get-candidates))) (candidates-in-buffer) (filtered-candidate-transformer \, fc-transformer) (requires-pattern \, requires-pattern) (persistent-action \, persistent-action) (persistent-help \, persistent-help) (keymap \, loc-map) (mode-line \, mode-line) (action \, action-fn))) (src-list (list src-hist (if candidates-in-buffer src-1 (if volatile (append src '((volatile))) src)))) (helm-execute-action-at-once-if-one exec-when-only-one) (helm-quit-if-no-candidate quit-when-no-cand) result) (when nomark (setq src-list (cl-loop for src in src-list collect (cons '(nomark) src)))) (when reverse-history (setq src-list (nreverse src-list))) (setq result (helm :sources src-list :input initial-input :default default :preselect preselect :prompt prompt :resume 'noresume :case-fold-search case-fold :keymap loc-map :history (and (symbolp input-history) input-history) :buffer buffer)) (when (and result history del-input) (cond ((and (symbolp history) (not (symbolp (symbol-value history)))) (helm-aif (symbol-value history) (setcar it result))) ((consp history) (setcar history result)) (t (set history (list result))))) (or result (when (and (eq helm-exit-status 0) (eq must-match 'confirm)) (if (and (string= helm-pattern "") default) default (identity helm-pattern))) (unless (or (eq helm-exit-status 1) must-match) default) (keyboard-quit)))))

(defvar helm-mode nil "\
Non-nil if Helm mode is enabled.
See the command `helm-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.")

(custom-autoload 'helm-mode "helm-mode" nil)

(autoload 'helm-mode "helm-mode" "\
Toggle generic helm completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use helm interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `helm-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `helm-completing-read-handlers-alist'
with a nil value.

Note: This mode is incompatible with Emacs23.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-wikipedia-suggest helm-yahoo-suggest helm-google-suggest
;;;;;;  helm-surfraw) "helm-net" "helm-net.el" (21517 3089))
;;; Generated autoloads from helm-net.el

(autoload 'helm-surfraw "helm-net" "\
Preconfigured `helm' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'helm-google-suggest "helm-net" "\
Preconfigured `helm' for google search with google suggest.

\(fn)" t nil)

(autoload 'helm-yahoo-suggest "helm-net" "\
Preconfigured `helm' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

(autoload 'helm-wikipedia-suggest "helm-net" "\
Preconfigured `helm' for Wikipedia lookup with Wikipedia suggest.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-org-headlines helm-org-keywords) "helm-org"
;;;;;;  "helm-org.el" (21517 3090))
;;; Generated autoloads from helm-org.el

(autoload 'helm-org-keywords "helm-org" "\
Preconfigured `helm' for org keywords.

\(fn)" t nil)

(autoload 'helm-org-headlines "helm-org" "\
Preconfigured helm to show org headlines.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-multi-occur-from-isearch helm-multi-occur
;;;;;;  helm-occur-from-isearch helm-occur helm-regexp helm-moccur-mode
;;;;;;  helm-moccur-run-save-buffer helm-occur-match-plugin-mode)
;;;;;;  "helm-regexp" "helm-regexp.el" (21517 3089))
;;; Generated autoloads from helm-regexp.el

(defvar helm-occur-match-plugin-mode t "\
Non-nil if Helm-Occur-Match-Plugin mode is enabled.
See the command `helm-occur-match-plugin-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-occur-match-plugin-mode'.")

(custom-autoload 'helm-occur-match-plugin-mode "helm-regexp" nil)

(autoload 'helm-occur-match-plugin-mode "helm-regexp" "\
Turn On/Off `helm-match-plugin-mode' only for `helm-m/occur'.

\(fn &optional ARG)" t nil)

(autoload 'helm-moccur-run-save-buffer "helm-regexp" "\
Run grep save results action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-moccur-mode "helm-regexp" "\
Major mode to provide actions in helm moccur saved buffer.

Special commands:
\\{helm-moccur-mode-map}

\(fn)" t nil)

(autoload 'helm-regexp "helm-regexp" "\
Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'helm-occur "helm-regexp" "\
Preconfigured helm for Occur.

\(fn)" t nil)

(autoload 'helm-occur-from-isearch "helm-regexp" "\
Invoke `helm-occur' from isearch.

\(fn)" t nil)

(autoload 'helm-multi-occur "helm-regexp" "\
Preconfigured helm for multi occur.

  BUFFERS is a list of buffers to search through.
With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling `helm-multi-occur'
or during the buffer selection.

\(fn BUFFERS)" t nil)

(autoload 'helm-multi-occur-from-isearch "helm-regexp" "\
Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

\(fn &optional _ARG)" t nil)

;;;***

;;;### (autoloads (helm-show-kill-ring helm-register helm-all-mark-rings
;;;;;;  helm-global-mark-ring helm-mark-ring) "helm-ring" "helm-ring.el"
;;;;;;  (21517 3089))
;;; Generated autoloads from helm-ring.el

(autoload 'helm-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-global-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'helm-all-mark-rings "helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-register "helm-ring" "\
Preconfigured `helm' for Emacs registers.

\(fn)" t nil)

(autoload 'helm-show-kill-ring "helm-ring" "\
Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-semantic-or-imenu helm-semantic) "helm-semantic"
;;;;;;  "helm-semantic.el" (21517 3091))
;;; Generated autoloads from helm-semantic.el

(autoload 'helm-semantic "helm-semantic" "\
Preconfigured `helm' for `semantic'.

\(fn)" t nil)

(autoload 'helm-semantic-or-imenu "helm-semantic" "\
Run `helm' with `semantic' or `imenu'.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-xrandr-set helm-list-emacs-process helm-top
;;;;;;  helm-top-run-sort-by-user helm-top-run-sort-by-mem helm-top-run-sort-by-cpu
;;;;;;  helm-top-run-sort-by-com) "helm-sys" "helm-sys.el" (21517
;;;;;;  3090))
;;; Generated autoloads from helm-sys.el

(autoload 'helm-top-run-sort-by-com "helm-sys" "\
Not documented

\(fn)" t nil)

(autoload 'helm-top-run-sort-by-cpu "helm-sys" "\
Not documented

\(fn)" t nil)

(autoload 'helm-top-run-sort-by-mem "helm-sys" "\
Not documented

\(fn)" t nil)

(autoload 'helm-top-run-sort-by-user "helm-sys" "\
Not documented

\(fn)" t nil)

(autoload 'helm-top "helm-sys" "\
Preconfigured `helm' for top command.

\(fn)" t nil)

(autoload 'helm-list-emacs-process "helm-sys" "\
Preconfigured `helm' for emacs process.

\(fn)" t nil)

(autoload 'helm-xrandr-set "helm-sys" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-etags-select) "helm-tags" "helm-tags.el"
;;;;;;  (21517 3090))
;;; Generated autoloads from helm-tags.el

(autoload 'helm-etags-select "helm-tags" "\
Preconfigured helm for etags.
If called with a prefix argument or if any of the tag files have
been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories, by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-yank-text-at-point helm-w32-shell-execute-open-file
;;;;;;  helm-quit-and-find-file helm-display-all-sources helm-show-all-in-this-source-only)
;;;;;;  "helm-utils" "helm-utils.el" (21517 3089))
;;; Generated autoloads from helm-utils.el

(autoload 'helm-show-all-in-this-source-only "helm-utils" "\
Show only current source of this helm session with all its candidates.
With a numeric prefix arg show only the ARG number of candidates.

\(fn ARG)" t nil)

(autoload 'helm-display-all-sources "helm-utils" "\
Display all sources previously hidden by `helm-set-source-filter'.

\(fn)" t nil)

(autoload 'helm-quit-and-find-file "helm-utils" "\
Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory.

\(fn)" t nil)

(autoload 'helm-w32-shell-execute-open-file "helm-utils" "\
Not documented

\(fn FILE)" t nil)

(autoload 'helm-yank-text-at-point "helm-utils" "\
Yank text at point in `helm-current-buffer' into minibuffer.
If `helm-yank-symbol-first' is non--nil the first yank
grabs the entire symbol.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-w3m-bookmarks) "helm-w3m" "helm-w3m.el" (21517
;;;;;;  3091))
;;; Generated autoloads from helm-w3m.el

(autoload 'helm-w3m-bookmarks "helm-w3m" "\
Preconfigured `helm' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-yaoddmuse-emacswiki-post-library helm-yaoddmuse-emacswiki-edit-or-view
;;;;;;  helm-yaoddmuse-cache-pages) "helm-yaoddmuse" "helm-yaoddmuse.el"
;;;;;;  (21517 3089))
;;; Generated autoloads from helm-yaoddmuse.el

(autoload 'helm-yaoddmuse-cache-pages "helm-yaoddmuse" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(autoload 'helm-yaoddmuse-emacswiki-edit-or-view "helm-yaoddmuse" "\
Preconfigured `helm' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'helm-yaoddmuse-emacswiki-post-library "helm-yaoddmuse" "\
Preconfigured `helm' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-aliases.el" "helm-pkg.el" "helm-plugin.el"
;;;;;;  "helm-source.el") (21517 3091 172340))

;;;***

(provide 'helm-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-autoloads.el ends here
