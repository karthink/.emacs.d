; math.el, a mode package for Mathematica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (c) 1990, 1991, 1992, 1993, 1994 Hewlett-Packard Company, 
;; all rights reserved.
;; 
;;                             LEGAL NOTICE
;; 
;; This math-mode package is experimental and HP shall have no obligation to
;; maintain or support it.  HP makes no express or implied warranty of any
;; kind with respect to this software, and HP shall not be liable for any
;; direct, indirect, special, incidental or consequential damages (whether
;; based on contract, tort or any other legal theory) arising in any way from
;; use of the software.
;; 
;; Everyone is granted permission to copy, modify and redistribute this
;; math-mode package, provided:
;;  1.  All copies contain this copyright notice.
;;  2.  All modified copies shall carry a prominant notice stating who
;;      made the last modification and the date of such modification.
;;  3.  No charge is made for this software or works derived from it.  
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	The procedure start-math-process was derived from the 
;;	procedure make-shell, part of the Gnu Emacs file shell.el, by
;;	permission of the Free Software Foundation.  Shell.el is
;;	Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation,
;;	Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Author: David Jacobson, jacobson@hpl.hp.com
;;      Assumes GNU Emacs version 18.54 or later
;;      Version info is in math-version-string defined after history box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LCD Archive Entry:
;; math|David Jacobson|jacobson@hpl.hp.com|
;; Mode package for running Mathematica
;; $Date: 1994/10/18 15:14:56 $|$Revision: 1.106 $|~/modes/math.el.Z|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2/17/1991 Dan Dill dan@chem.bu.edu
;;   Add math-send-filter-active and math-send-filter-status, 
;;   for use with tex-mma
;;
;; 5/1/1991 Dan Dill dan@chem.bu.edu
;;   Add math-remote-host and math-remote-shell and modified 
;;   start-buffer-process, to run Mathematica remotely
;;
;; 5/8/1991 David Jacobson jacobson@hplabs.hp.com
;;   Add math-timeout and improve documentation, add checking
;;   for incomplete cells to check-math-syntax
;;
;; 11/17/1991 David Jacobson jacobson@hpl.hp.com
;;  Adding indent cookie crumb rejection
;;
;; 11/27/1991 David Jacobson jacobson@hpl.hp.com
;;  Fix path.  Delete goto-math-line.  Fix up find-math-ehrror
;;
;; 11/30/1991 Dan Dill dan@chem.bu.edu
;;  Adapt indent cookie crumb rejection changes for use with tex-mma
;;
;; 12/5/91 David Jacobson jacobson@hpl.hp.com
;;  Functions math and start-math no longer call math-mode if it was already
;;  in math-mode.  (math-mode initializes a lot of variables, including
;;  all local variables.)  Small changes to start-buffer-process to avoid 
;;  munging state if the process is already running.
;;
;; 12/10/91 David Jacobson jacobson@hpl.hp.com
;;  Add kill-math-cell.  Add installation instructions.  Fix documentation.
;; 
;; 12/12/91 Dan Dill dan@chem.bu.edu
;;  Synchronize with tex-mma by setting math-send-filter-active to nil
;;  in math-send-filter when the input prompt has been received.
;;
;; 12/17/91 David Jacobson jacobson@hpl.hp.com
;;  Set mark when C-c C-y copies cell.  Fix bug causing
;;  C-c C-y and empty response in last cell of buffer that
;;  was not its own math process buffer to get wrong cell.
;;  Rename start-buffer-process to start-math-process.
;;  Add more documentation.  Add math-mode-load-hook.
;;
;; 12/31/91, 1/7/92 David Jacobson jacobson@hpl.hp.com
;;  Cause the DISPLAY variable to be set when using a remote host.
;;
;; 1/20/92 David Jacobson jacobson@hpl.hp.com
;;  Adjust legal wording.
;; 
;; 4/2/92 David Jacobson jacobson@hpl.hp.com
;;  Add math-transform-float and math-transform-floats-in-region
;;
;; 5/16/92 David Jacobson jacobson@hpl.hp.com
;;  Fix math-copy-region so that if the input string ends in a "-" 
;;  the cell is deleted.
;;
;; 6/3/92 David Jacobson jacobson@hpl.hp.com
;;  Fix to use fancy math indent cookies.
;;
;; 6/4/92 David Jacobson jacobson@hpl.hp.com
;;  Fix cell sending system to that it gets responses to interrupts
;;  and Input[...] right.  Major changes to math-identify-cell.
;;  Also fix so that no extra space is inserted after response
;;  to Input.
;; 
;; 6/22/92 David Jacobson jacobson@hpl.hp.com
;;  Remove references to c-process-filter in math-help-filter.
;;  Fix math-copy-cell to add a backslash if the first line
;;  is blank.  This sometimes happens when you copy an Out cell.
;;  Fix math-identify-cell to search back to a blank line before the
;;  beginning if it is an Out cell and also to include all the
;;  text to the beginning of the next In cell.
;;
;; 6/23/92 David Jacobson jacobson@hpl.hp.com
;;  Make "--" at end of math-copy-cell delete to end of buffer.
;;
;; 7/17/92 David Jacobson jacobson@hpl.hp.com
;;  Twiddle on-line documentation.  Make C-c C-k run old-kill-math-cell,
;;  which is now an alias for kill-math-cell.  Twiddle key binding
;;  details.
;;
;; 7/23/92 David Jacobson jacobson@hpl.hp.com
;;  Add math-remove-symbol
;;
;; 7/28/92 David Jacobson jacobson@hpl.hp.com
;;  Make check-math-syntax warn on backslash whitespace eol.
;;  Add math-remote-user.  Make backquote be a valid statement ender.
;;
;; 8/1/92 David Jacobson jacobson@hpl.hp.com
;;  Fix check-math-syntax to leave cursor at point of error.
;;  Also make it detect negative paren depth.
;; 
;; 8/12/92 David Jacobson jacobson@hpl.hp.com
;;  Fix math-identify-cell to work around emacs bug.  Now C-c C-y
;;  will properly grab cells at the beginning of the buffer.  (Important
;;  when the buffer is a .m file rather than the *math* buffer.)
;;
;; 8/24/92 David Jacobson jacobson@hpl.hp.com
;;  Add experimental math-electric-char facility.  This is subject to 
;;  change at any moment.  
;;
;; 9/28/92 David Jacobson jacobson@hpl.hp.com
;;  Add /usr/bsd/rsh as possible remote shell program.
;;
;; 6/6/93 David Jacobson jacobson@hpl.hp.com
;;  Add defvars for math-send-state and math-completion-symbol.
;;  Comment out deep-copy-keymap and change all instances of
;;  deep-copy-keymap to copy-keymap.
;;  Add constant emacs-version-18 that is t if running v. 18, and nil
;;  otherwise.  Use this constant to select either screen-width (v18)
;;  or frame-width (v19).  
;;
;; 12/12/93 David Jacobson jacobson@hpl.hp.com
;;  The last version breaks on Gnu Emacs 19.20 and following.  The 
;;  following changes address that problem.
;;  Change all references to shell-mode to comint-mode, shell-mode-map to
;;  comint-mode-map, etc.  The function unread-command-char is now
;;  obsolete.  Rewrite math-isearch-backwards to not depend on it.
;;  Add function math-insert-in-regexp-search-ring to facilitate this.
;;  Move (provide 'math) to the end of the file.
;;  Fix spelling error in parse-partial-sexp-ignore-comments.
;;  Fix semi-bug in math-electric-char-bounce-p: some things
;;  that were passed as parameters were being accessed by the names
;;  of the local vars in the calling procedure.
;;
;; 12/13/93 David Jacobson jacobson@hpl.hp.com
;;  Eliminate math-insert-in-regexp-search-string and rewrite
;;  math-isearch-backwards to not need it to avoid copyright hassles 
;;  with FSF.  Add some functionality in the process.  It now
;;  avoids cluttering the regexp-search-ring.  Eliminate some
;;  lies in the comments regarding assumptions about Emacs' 
;;  output process.  Explain the indent cookies.
;;
;; 12/15/93 David Jacobson jacobson@hpl.hp.com
;;  Add LCD archive entry comment.  
;;  No other changes, even to math-version string.
;;
;; 03/12/94 Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;;  Added mathematica-state and the comment strings definition.
;;
;; 03/29/94 David Jacobson jacobson@hpl.hp.com
;;  Removed printing of "*" is message area.
;;
;; 06/20/94 David Jacobson jacobson@hpl.hp.com
;;  Make it work on Gnu Emacs 19.23 and following.
;;  Split math-start-process into 2 halves, one for Gnu Emacs v18 and
;;  one for Gnu Emacs v 19.  The version 19 part does not call the
;;  "env" program as it no longer exists.  Instead it sets up evnironment
;;  variables with the variable process-envoronment.  Also hack mode
;;  message regarding sending of interrupts.
;;
;; 06/21/94 David Jacobson jacobson@hpl.hp.com
;;  Fix LCD archive entry.  math-version-string left unchanged
;;
;; 10/18/94 David Jacobson jacobson@hpl.hp.com
;;  Fix recommended installation comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst math-version-string
  "Mathematica mode experimental version of October 18, 1994 for Mathematica version 2.2."
  "String describing this version of math.el.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation and use:
;; 
;; For initial tryout save this file somewhere, start emacs and type 
;;     M-x load-file <<the full path name of this file>>
;; Then type 
;;     M-x math
;; a window should be created and a Mathematica process created in it.
;; Type in a cell and "submit" it with ESC-RET.  
;; You can see a lot more information and instructoins by typing 
;; C-h m (DEL m for HP keyboard users).  
;; You can use C-h f to find more information on each command.
;; 
;; For full installation, this file should be installed as math.el 
;; in whatever directory you put your local or personal emacs lisp files.
;; It should be byte-compiled.  You should add the following to 
;; your .emacs file:
;;  
;;  (autoload 'math "math" "Starts Mathematica" t)
;;  (autoload 'math-mode "math" 
;;    "Mode for editing Mathematica.  Loading will result in more info." t)
;;  (setq auto-mode-alist (cons '("\\.m\\'" . math-mode) auto-mode-alist))
;;
;; and add the following to your init.m file
;;
;;  If[Environment["MATHINDENTCOOKIE"] =!= $Failed,
;;    $BatchInput=False;
;;    If[NameQ["System`Private`$IndentSuffix"],
;;      ToExpression[
;;        "System`Private`$IndentSuffix = Environment[\"MATHINDENTCOOKIE\"]"];
;;      Print[" ", Environment["MATHINDENTCOOKIEMSG"]]]]
;;  
;; It will usually work without this, but this makes it do better at
;; identifying when the system is waiting for more input.  
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; LIMITATIONS
;; 
;; A nasty problem arises because's in Versions 2.0 and up Mathematica will 
;; under certain conditions put out a message before a cell has been 
;; completed, then expect the remainder of the cell to be sent.  In most 
;; other conditions, unexpected output indicates an error of one sort or
;; another has occurred.  Two such messages are General::spell,
;; indicating a suspected spelling error, and Syntax::newl, indicating
;; that a newline has been interpreted as a multiplication operator.
;; 
;; There are several approaches to this problem:
;; 1.  Avoid the constructs that lead to it.
;; 2.  Turn off the messages.
;; 3.  Persuade WRI to change the interface.
;; 4.  Workaround it.  When it occurs
;;      a. Clear the input by typing ESC RET.  (Moving to a blank line
;;               might make you feel better, but shouldn't be necessary.  
;;               Math-mode keeps track of where the last output ended.)
;;               This will send a newline to the Mathematica process 
;;               and cause it to flush its input and reissue the prompt.
;;               (This step is not always necessary.)
;;      b. Cut away the message and new prompt. (Or hit the key for 
;;               undo one or two times.)
;;      c. Fix the input, if necessary (it is not necessary for 
;;               General::Spell), and resubmit the cell.
;;
;; The whole system for running Mathematica on a remote host is rather
;; unreliable.  If the built-in facilities don't work, try setting 
;; math-process-string to point to a shell script (or compiled program,for
;; that matter), that you can then work with freely.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Note for people running older versions of Gnu Emacs.  
;;
;;  On sufficiently old versions of Gnu Emacs, the function copy-keymap
;;  did only a "shallow" copy, i.e. it only copied the top level of the
;;  key map.  To overcome this, this code used to have a function 
;;  deep-copy-keymap.  Unfortunately, deep-copy-keymap is incompatible 
;;  with Lucid Emacs.  Since it is now also unnecessary, on June 6, 1993 I 
;;  commented out the code for deep-copy-keymap and changed all instances
;;  of deep-copy-keymap to copy keymap.  If this causes trouble on your 
;;  machine, uncomment deep-copy-keymap and change all instances of
;;  copy-keymap to deep-copy-keymap.  Here are two ways of telling if
;;  you need deep-copy-keymap.
;;  Look at the documentation for copy-keymap. If it looks like this: 
;;    "Return a copy of the keymap KEYMAP.
;;     The copy starts out with the same definitions of KEYMAP,
;;     but changing either the copy or KEYMAP does not affect the other.
;;     Any key definitions that are subkeymaps are recursively copied."
;;  you are probably ok.  Also you can first start Mathematica.  Then 
;;  run m-x shell.  Then look at the shell keybindings with C-h b.  
;;  If you see any math-mode commands in there, you definitely need
;;  deep-copy-keymap.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defconst emacs-version-18 (string= (substring emacs-version 0 3) "18.")
"t if version 18 of emacs, else nil")

(if emacs-version-18 (require 'shell) (require 'comint))

(defvar Mathematica-search-path 
  (list nil (getenv "HOME") "/usr/local/math/StartUp" "/usr/local/math/Packages")
  "*A list of directories in which to look for files.  
Use nil for the current directory.")

(defvar math-process-string "/usr/local/bin/math"
  "*A string to pass to the unix exec function to start Mathematica")

(defvar math-process-buffer 
  "*math*"
  "The buffer normally running Mathematica.  Certain commands
(e.g. math-complete-symbol) will go to this buffer to find a Mathematica 
process.  This can be locally set with set-math-process-buffer.")

; (defconst math-header-re (concat "^" (regexp-quote "Copyright 1988-91 Wolfram Research, Inc."))
;   "A regexp that will match somewhere in the Mathematica preamble")

(defvar math-header-re nil
  "A regexp that matches some line in the Mathematic preamble.  Nil if 
Mathematica is not started.")

(defvar math-remote-host nil
  "*If non-nil, use as remote host name with `math-remote-shell' to run
Mathematica remotely.  See also variables math-remote-user and 
math-display-var")

(defvar math-remote-user nil
   "*If non-nil, use as user-ID on remote host.  Effective only if 
math-remote-host is non-nil.  Uses the \"-l\" flag to rsh/remsh")

(defvar math-display-var nil 
  "*If set to a string, the DISPLAY environment variable is set to this
value.  If nil, the DISPLAY environment variable is set the the local
DISPLAY environment variable.  If math-display-var is neither nil nor
a string, the DISPLAY environment variable will not be set.")

(defconst math-indent-cookie "|===indent==="
  "This string is put in the evironment variable MATHINDENTCOOKIE, and
in version 2.1 and greater and the init.m file is set up properly, this
is assigned to the Mathematica variable `System`Private`$IndentSuffix.  
The output parser looks for this value to know to send the next line.")

(defconst math-indent-cookie-message "-- indent cookies enabled --"
  "This string in the startup messages indicates that indent cookies
are being used.")

(defvar math-indent-cookies nil
  "A buffer specific variable that is t if non-blank math indent cookies
are being used.")


(defvar math-timeout nil 
"*If non-nil, use as a timeout between lines of input.  Nil is
recommended, unless you are having trouble with the system hanging,
particularly on syntax errors.  Nil causes input to be waited for
using accept-process-output.  This means the next line of output will
be sent when math-mode receives the indentation prompt from
Mathematica.  If non-nil math-mode will wait only math-timeout seconds
between sending lines of input. Usually this needs to be set when
using a remote host.  1 is recommended in this case. (See
math-remote-host and math-remote-shell.)")

(defvar math-remote-shell 
  (cond ((file-exists-p "/usr/ucb/rsh")	"/usr/ucb/rsh")
	((file-exists-p "/usr/bsd/rsh")	"/usr/bsd/rsh")
	((file-exists-p "/usr/bin/remsh") "/usr/bin/remsh"))
  "*String used with `math-remote-host' to run Mathematica remotely.")

(defconst math-valid-cell-ending-re 
  ".*\\([])}\"A-Za-z0-9!_`'$%#]\\|\\+\\+\\|--\\|=[ \t]*\\.\\|[^/];\\|\\b[0-9]+\\.\\|[^&]&\\)[ \t]*$"
  "An re that matches lines that can validly end complete cells.
This is not perfect.")
;;; The "[^/];" is because "/;" means a condition follows, so it cannot 
;;; be the end of a cell.  The "=[ \t]*\\." and "\\b[0-9]+\\." allow periods 
;;; only in "=." and at the end of mumbers, disallowing a 
;;; variable followed by a period (Dot[]).  "[^&]&" allows "&" but not "&&".  
;;; It will mess up if "&" appears
;;; as the first character on a line by itself, but then the end of the
;;; previous line probably made a valid prefix, which is an error anyway.

(defvar math-send-state 'normal 
"The state of the finite state machine that controls interaction with 
Mathematica.  See the description in a giant comment block in the source 
code.")

(defvar math-send-filter-status 'normal
  "Status of last input to Mathematica:
  `normal' means no problems detected;
  `input-prompt' means Mathematica input prompt received
  `premature-output' means part of cell not sent due to unexpected output;
  `blank-line-added' means line inserted to separate input from output;
  `incomplete-cell' means an incomplete cell was detected;
  `syntax-error' means a syntax error was detected.")

(defvar mathematica-state ""
  "Current state of Mathematica:
  `' means no process yet;
  `Starting' means the Mathematica process is starting;
  `Done' means Mathematica is idle;
  `Computing' means Mathematica is busy;
  `Printing' means Mathematica is still busy, but has printed something;
  `No Process' means Mathematica has exited or has been terminated.")

(defvar math-send-filter-active nil
  "Status of Mathematica process filter: t if enabled, else nil.")

(defvar math-last-output-column 8
  "The column at which the last \"normal\" output ended.  If 
math-indent-cookies is false, indent cookies are considered complete if 
they are this long.")

(defvar math-completion-symbol nil 
"A global variable for communicating between math-help-filter and the 
function math-complete-symbol.")

(defvar math-mode-map nil) 

;;; deep-copy-keymap was written by Daniel LaLiberte.  
;;; It is necessary for some older versions of Gnu Emacs.  If 
;;; copy-keymap on your system does not do a deep copy (ie copy
;;; all levels, remove the semicolons commenting out the following
;;; code, and replace all instances of copy-keymap in this file
;;; with deep-copy-keymap.  This deep-copy-keymap code is known
;;; to be incompatible with Lucid emacs, but isn't necessary 
;;; for it either.

; (defun deep-copy-keymap (keymap)
;   "Return a deep copy of KEYMAP.  That is, all levels are copied,
; not just the top level."
;   (if (not (keymapp keymap))
;       keymap
;     (cond
; 
;      ((listp keymap)
;       (let ((new-keymap (copy-alist keymap)))
; 	(setq keymap (cdr new-keymap))
; 	(while keymap
; 	  (let ((binding (car keymap)))
; 	    (if (keymapp (cdr binding))
; 		(setcdr binding (copy-keymap (cdr binding))))
; 	    )
; 	  (setq keymap (cdr keymap))
; 	  )
; 	new-keymap
; 	))
; 
;       ((vectorp keymap)
;        (let ((i 0)
; 	     (n (length keymap))
; 	     (new-keymap (copy-sequence keymap)))
; 	 (while (< i n)
; 	   (if (keymapp (aref keymap i))
; 	       (aset new-keymap i (copy-keymap (aref keymap i))))
; 	   (setq i (1+ i)))
; 	 new-keymap
; 	 )))))

(defun math-version ()
  "Display string indentifying math.el."
  (interactive)
  (with-output-to-temp-buffer "*Help*" (print-help-return-message))
  (let ((home-buffer (current-buffer)))
    (pop-to-buffer "*Help*")
    (insert math-version-string)
    (insert "\n")
    (pop-to-buffer home-buffer))
  (bury-buffer "*Help*"))

(if math-mode-map
    nil
  (setq math-mode-map (copy-keymap (if emacs-version-18 
				       shell-mode-map
				     comint-mode-map)))
  (define-key math-mode-map "\C-m" 'newline) 
					; The shell-mode-mode
					; sets this to shell-send-input.
					; We change it to 'newline.  We
					; ought to undefine it, so that
					; Emacs will find the 'newline
					; in the global keymap, but there
					; is no easy way to do this.
  (define-key math-mode-map "\M-\C-m" 'math-send-input)
  ;; \C-c\C-c is set to interrupt-shell-subjob in the shell-mode-mode
  ;; The name is deceptive; it sends a SIGINT signal (control C) to 
  ;; whatever process is running in the current buffer
  (define-key math-mode-map "\C-c9" 'kill-9-process)
  (define-key math-mode-map "\C-cv" 'math-version)
  (define-key math-mode-map "\C-he" 'math-help) ; e-xpression in help menu
  (define-key math-mode-map "\C-hE" 'math-extra-help) ; E-xpression in help menu
  (define-key math-mode-map "\C-c\C-f" 'math-edit-function)
  (define-key math-mode-map "\M-\t"   'math-complete-symbol)
  (define-key math-mode-map "\C-c\C-y" 'math-copy-cell)
  (define-key math-mode-map "\C-c\C-e" 'find-math-error)
  (define-key math-mode-map "\C-c\C-r" 'math-isearch-backward)
  (define-key math-mode-map "\M-k" 'kill-math-cell) ; overrides kill-sentence
  (define-key math-mode-map "\C-c\C-k" 'old-kill-math-cell)
  ;; old-kill-math-cell is effectively just an alias for kill-math-cell
  ;; but it is named differently so the preferred binding will show up
  ;; for describe mode
  (define-key math-mode-map "\C-c\C-x" 'math-transform-float)
  (define-key math-mode-map "\C-c\C-v" 'math-remove-symbol)
  )

(defvar math-mode-syntax-table nil
  "Syntax table used while in math mode.")

(if math-mode-syntax-table
    ()
  (setq math-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?% "." math-mode-syntax-table)
  (modify-syntax-entry ?& "." math-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" math-mode-syntax-table) ;allow for (* comment *)
  (modify-syntax-entry ?+ "." math-mode-syntax-table)
  (modify-syntax-entry ?- "." math-mode-syntax-table)
  (modify-syntax-entry ?/ "." math-mode-syntax-table)
  (modify-syntax-entry ?< "." math-mode-syntax-table)
  (modify-syntax-entry ?= "." math-mode-syntax-table)
  (modify-syntax-entry ?> "." math-mode-syntax-table)
  (modify-syntax-entry ?_ "." math-mode-syntax-table)
  (modify-syntax-entry ?\| "." math-mode-syntax-table)
  (modify-syntax-entry ?\` "_" math-mode-syntax-table) ; Mathematica context symbol
  (modify-syntax-entry ?\( "()1" math-mode-syntax-table) ;allow for (* comment *)
  (modify-syntax-entry ?\) ")(4" math-mode-syntax-table)) ;allow for (* comment *)

;;; math-send-input sends a chunk of text to Mathematica.  It
;;; interacts tightly with math-send-filter using the buffer-specific
;;; variable math-send-state and synchronizes though sending output
;;; and the accept-process-output (but see below) command.  

;;; If the Mathematica process does not send a prompt at all, the
;;; accept-process-output hangs and the only solution is to kill the
;;; mathexe process or Emacs.  This can happen if you use an Input[""]
;;; (a rather perverse thing to do).  An alternative is to replace
;;; (accept-process-output process) with (sleep-for 10).  It appears that
;;; arrival of any output causes Emacs to pop immediately out of a
;;; sleep-for.  But this could mess up if you have more than one active
;;; process running at a time or if you strike a key.  
;;; Of course, we could use a while loop and have the filter set 
;;; math-send-state to another value when it has actually gotten 
;;; something.  I tried this but ran into unknown trouble and have 
;;; not followed up on it.

;;; The variable math-send-state has the following interpretatons:
;;; starting-up               Mathematica is just starting up.
;;;                           Snarf up the first line and watch
;;;                           for the math-indent-cookie-message.
;;;
;;; non-last-line             We are in the middle of sending a
;;;                           multi-line input.  Watch for
;;;                           errors and output other than indent
;;;                           cookies.  Math-send-input exits its
;;;                           loop as soon as the state is not
;;;                           non-last-line.
;;;
;;; last-line                 The last line has been sent, still watch
;;;                           for syntax error messages.  (Approprate
;;;                           for 1.2 only.)  Also insert
;;;                           a blank like (and warn about it) if the
;;;                           output contains any non-whitespace
;;;                           characters before a newline.
;;;
;;; last-line-no-blank        Same as last-line, except it doesn't
;;;                           force a blank line after the cell.
;;;
;;; throw-away-prompt         A syntax error has been detected and a 
;;;                           newline sent to Mathematica to flush its
;;;                           input buffer.  Normally it will come
;;;                           back with a new prompt.  If the next
;;;                           output looks like a prompt, throw it
;;;                           away and give a syntax error message.
;;;                           Throw away indent cookies.
;;;                           Otherwise display the discarded
;;;                           material in a warning message.
;;;                           This is only for compatibility with 1.2.
;;;
;;; premature-output          Output that was not an indent cookie
;;;                           was detected in the non-last-line state.
;;;                           Stop sending.  Watch for indent cookies.
;;;                           Transition to normal whan In or Out are
;;;                           detected.
;;;
;;; normal                    Just post the output.
;;;

;;; Note: the syntax error detection code and the correspoinding 
;;; throw-away-prompt state were important in 1.2.  They should
;;; be unnecessary in 2.0, but were left for compatibility.
;;; No attempt has been made in upgrading to 2.0 to maintain full backward
;;; compatibility, but the basic cell-submission stuff is so important 
;;; that backward compatibility is attempted here.
;;; With version 2.1 indent cookie recognition has been added.
;;; 
;;; The indent cookie thing watches for indent cookies (normally  
;;; "|===indent===", but changable) to be output after each line of a
;;; a multi-line cell.  When one is found, it is discarded, and the next line
;;; is sent.  Some systems may not support indent cookies.  In this case,
;;; we just watch for a blank line ending at the same column as 
;;; as the end of the last input prompt.  It's not completely reliable, but
;;; works most of the time.  When looking for an indent cookie or spaces,
;;; we don't actually put it in the buffer, but accumulate it in the
;;; variable math-partial-output.  (It actually is in the variable
;;; string during most of the execution of math-send-filter.)


(defvar math-partial-output "" 
 "Accumulates output until it can be determined that it is not an indent 
cookie")

(defun indent-cookie-p (string &optional ignorestate)
  "Checks STRING to see if it is an indent cookie.  Returns
t if probably a cookie, nil if definitely not, and 'partial otherwise.
It is not really a predicate of STRING, since it also depends on 
math-send-state.  If optional IGNORESTATE is true it ignores the 
math-send-state."
  (if math-indent-cookies
      (let (tail)
	(cond ((not (or 
		     ignorestate 
		     (memq math-send-state 
			   '(last-line last-line-no-blank non-last-line premature-output))))
	       nil)
	      ((not (string-match "\\`\\([ \t]*\\)[^ \t]" string)) 'partial)
	      ((string= (setq tail (substring string (match-end 1)))
			math-indent-cookie) t)
	      ((>= (length tail) (length math-indent-cookie)) nil)
	      ((string= tail (substring math-indent-cookie 
					0 
					(length tail)))
	       'partial)
	      (t nil)))
    ;; now for the math-indent-cookies not true case
    (if (or ignorestate (memq math-send-state '(last-line last-line-no-blank non-last-line)))
	(if (string-match "\\` +\\'" string)
	    (let ((len (length string)))
	      (cond ((< len math-last-output-column) 'partial)
		    ((= len math-last-output-column) t)
		    (t nil)))  ; too long
	  nil)  ; non-blank
      nil)) ; not correct state
  )
	    
(defvar math-indent-cookie-pending nil "t if we can't decide if we have
an indent cookie.")

(defun math-send-input ()
  "Send input to Mathematica.
At end of buffer, sends last \"cell\" to Mathematica.  When not at end, 
copies current \"cell\" to the end of the buffer and sends it.  Also
sends input for interrupt and Input[].  Warning: A multi-line input
to Input[\"\"] will cause deadlock."
  (interactive "*")
  (let ((process (or (get-buffer-process (current-buffer))
		     (error "Current buffer has no process")))
	bpt2
	ept2
	begpkt
	endpkt
	copy
	)
    ;; Find beginning of "cell"
    (let* ((cellinfo (math-identify-cell 
		      (point) 'submit (process-mark process)))
	   (bpt (car cellinfo))
	   (ept (nth 1 cellinfo))
	   )
      (check-math-syntax bpt ept)
      (goto-char ept)
      ;; Move to line beyond cell, adding newline if necessary.
      (forward-line 1)
      (if (or (not (bolp)) 
	      (= (point) bpt)) ; make null cells contain a newline
	  (newline))
      (setq copy (buffer-substring bpt (point)))
      ;; If we are \"near\" the end of the buffer, we don't copy the data down
      ;; there, but we kill excess white space.  Otherwise, we go there and 
      ;; copy the data.
      (if (looking-at "\\s *\\'")
	  (progn
	    (replace-match "")
	    (setq bpt2 bpt)
	    (setq ept2 (point))
	    (setq math-last-input-end (point)))
	(push-mark bpt)  ; This sets the mark when copying down a cell
	(goto-char (point-max))
	(forward-line 0)
	(if (or (eolp) (looking-at "^[ \t]*In\\[[0-9]+\\]:=\\s *$"))
	    (end-of-line)
	  (end-of-line)
	  (newline))
	(setq bpt2 (point))
	(insert copy)
	(setq ept2 (point))
	(setq math-last-input-end (point)))
      (goto-char bpt2)
      (setq math-partial-output "") 
      (setq math-indent-cookie-pending nil)
      (setq math-send-filter-status 'normal) ; For single line input without filter
                                             ; ..
      (set-process-filter process 'math-send-filter)
      ;; math-send-state is a global variable
      (setq math-send-state 'non-last-line)
      (setq begpkt bpt2) ; point
      (setq mathematica-state "Computing")
      ;; (message "*")
      (unwind-protect
	(while (eq math-send-state 'non-last-line)
	  (goto-char begpkt)
	  (forward-line 1)
	  (setq endpkt (point))
	  ;; set flag to exit loop and tell math-send-filter to
	  ;; deal with next output as non-intermediate lines.
	  ;; If this line likely came from an Input[...] have it
	  ;; not force a blank line.
	  (if (= endpkt ept2) 
	      (setq math-send-state (if (nth 2 cellinfo)
					'last-line-no-blank
				      'last-line)))
	  (metered-process-send-string 
	   process (buffer-substring begpkt endpkt))
	  (if (eq math-send-state 'non-last-line)
	      (if math-timeout
		  (sleep-for math-timeout)
		(accept-process-output process)
		(while math-indent-cookie-pending
		  (accept-process-output process))))
	  (setq begpkt endpkt) ; advance to next line
	  ) ; end while
	;; unwind-protect tail; here for future use
	))))

(defun check-dangling-indent-cookie (proc)
  ;; proc can be nil
  (if (eq (indent-cookie-p 
	   (buffer-substring (save-excursion 
			       (forward-line 0)
			       (point))
			     (point))
	   t)  ; ignore state
	  t)
      (progn
	(setq math-send-state 'normal)
	(setq math-send-filter-status 'incomplete-cell);????
	(if (and proc math-indent-cookies) ; without cookies it is too 
					; unreliable to send the newline
	    (progn
	      (newline 2)
	      (ding t)
	      (message "Incomplete cell!  Newline sent to clear input.")
	      (process-send-string proc "\n"))
	  (ding t)
	  (message "Incomplete cell?  Clear input with ESC RET.")
	  )
	t)
    nil) ;end if
)

(defun math-send-filter (proc procstring)
  (let ((cbuf (current-buffer))
	(save-match-data (match-data))
	(string (concat math-partial-output procstring))
	incomplete-cell
	cookie-status)
    (unwind-protect
	(progn
	  (setq math-partial-output "")
	  (setq math-indent-cookie-pending nil) ; assume for now this ouput
					;completes an indent cookie
	  (set-buffer (process-buffer proc))
	  (setq cookie-status (indent-cookie-p string))
	  (cond 
	   ;; cond branch: can not tell if it is or is not an indent cookie
	   ((eq cookie-status 'partial)
	    (setq math-partial-output string)
	    (setq math-indent-cookie-pending t))
	   ;; if state is starting-up
	   ;; check for math-send-cookie-message in startup strings
	   ;; snarf up beginning string
	   ;; exit to normal when In[...] is found
	   ;; We insert and search in the buffer since we might get more
	   ;; than one line at a time.
	   ((eq math-send-state 'starting-up)
	    (let ((beg (point-max))
		  end)
	      (goto-char beg)
	      (insert string)
	      (set-marker (process-mark proc) (point))
	      (setq end (point)) 
	      (goto-char beg)
	      (forward-line 0)
	      (if (and (not math-header-re)
		       (looking-at "^.*\n"))
		  (setq math-header-re 
			(concat "^" (regexp-quote
				     (buffer-substring (match-beginning 0)
						       (match-end 0))))))
	      (if (and (not math-indent-cookies)
		       (re-search-forward math-indent-cookie-message
					  end t))
		  (setq math-indent-cookies t))
	      (goto-char end)
	      (forward-line 0)
	      (if (looking-at "^[ \t]*In\\[[0-9]+\\]")
		  (setq math-send-state 'normal))
	      (goto-char end)))
	   ;; cond branch: a <retype-line error> 
	   ;; retained for version 1.2 compatibility
	   ((and
	     (memq math-send-state '(non-last-line last-line last-line-no-blank))
	     (string-match "\\`\\([ \t]*\\)\\^ <retype line>" string))
	    (let ((tpt (point))
		  error-column
		  indent-column
		  (tail-string (substring string (match-end 0))))
	      (goto-char tpt)
	      (insert (substring string 0 (match-end 1)))
	      (setq error-column (current-column))
	      (delete-region tpt (point))
	      (indent-to-column (- error-column math-last-output-column))
	      (insert "^--error\n")
	      (backward-char 9)
	      (previous-line 1)
	      ;; Display any unexpected output.  I don't know how to 
	      ;; test this code.
	      (if (string-match "\\S " tail-string)
		  (save-excursion
		    (goto-char (point-max))
		    (insert tail-string)
		    (set-marker (process-mark proc) (point)))))  ; end of let
	    (setq math-send-state 'throw-away-prompt)
	    (message "Syntax error") ; live dangerously here, but sometimes we 
					; don't get a prompt back from 
					; Mathematica
	    (process-send-string proc "\n")
	    (setq math-send-filter-status 'syntax-error)
	    )
	   ;; cond branch: snarf up indent strings
	   ;; Whether or not this branch is taken when math-send-state
	   ;; is throw-away-prompt depends on the OS.  On some systems
	   ;; the indent cookie comes out with the "^ <retype-line>" 
	   ;; (in 1.2 only)
	   ;; and the indent cookie is inserted by the tail-string
	   ;; procesing above.  On others it comes out separately and is 
	   ;; handled here.
	   ((and
	     (memq math-send-state '(non-last-line throw-away-prompt))
	     (eq cookie-status t))) ; do nothing 
	   ;; cond branch: unexpected output
	   ((eq math-send-state 'non-last-line)
	    (insert 
"-------- Unexpected output appeared here; rest of cell not sent --------\n"
)
	    (goto-char (point-max))
	    (insert string)
	    (setq math-last-output-column (current-column))
	    (set-marker (process-mark proc) (point))
	    (setq math-send-state 'premature-output)
	    (setq math-send-filter-status 'premature-output)
	    ;; the following if checks for a dangling indent cookie right
	    ;; after unexpected output.  But it probably never happens
	    ;; since the unexpected output is probably several i/o chunks
	    ;; long and the state is normal by the time any indent cookie 
	    ;; finally arrives.  Thus the message is almost always sent.
	    (if (not (check-dangling-indent-cookie proc))
					; check-dangling-indent-cookie
					; generates its own message
		(progn
		  (ding t)
		  ;; (message "")
		  ))) 
	   ;; cond branch: throw away unwanted prompt
	   ((eq math-send-state 'throw-away-prompt)
	     (setq math-send-state 'normal)
	     (if (string-match "\\`[ \t]*In\\[[0-9]+\\]:= \\'" string)
		 (message "Syntax error")
	       (message "Syntax error, discarding prompt(?): %s" 
			string))
	     (setq math-send-filter-status 'syntax-error)
	    )
	   ;; cond branch: last line has been sent, make sure a blank line
	   ;; follows the In... stuff.  See further comments.
	   ((memq math-send-state '(last-line last-line-no-blank))
	    (goto-char (point-max))
	    ;; except in the case of an indent cookie the string
	    ;; that was received is inserted after this big cond.
	    (cond ((string-match "\\`\\s *\n" string); blank with newline 
		   (setq math-send-state 'normal)
		   ;; (message "") 
		   )                ; clear "*" in message area
		  ((eq cookie-status t)
					; give help message
		   (setq math-send-filter-status 'incomplete-cell)
		   (setq math-send-state 'normal)
		   (setq incomplete-cell t)
		   (process-send-string proc "\n")
		   (newline)
		   (ding t) ; don't do anything funny
		   (message "Incomplete cell!  Newline sent to clear input.")
		   )
		  ((string-match "\\`[ \t]*In\\[[0-9]+\\]:=" string)
		   ;; (message "") ; clear "*" in message area.  
		   (setq math-send-state 'normal)
		   (setq math-send-filter-status 'normal)
		   )
		  ;; cond branch.  Output was cell sent in response to
		  ;; an Input[...] (sometimes)
		  ((eq math-send-state 'last-line-no-blank)
		   (setq math-send-filter-status 'normal)
		   (setq math-send-state 'normal))
		  ;; cond brach
		  (t
		   ;; non-blank, but not an In[] prompt.  Probably either
		   ;; output of a Mathematica Print[...] or an error message.
		   ;; Add a blank line to separate cells.
		   (newline)
		   (message "newline inserted by Emacs' math-mode")
		   (setq math-send-filter-status 'blank-line-added)
		   (setq math-send-state 'normal)
		   )
		  )
	    (if (not incomplete-cell)
		(progn
		  (insert string)
		  (set-marker (process-mark proc) (point))
		  (setq math-last-output-column (current-column)))))
	   ;; cond branch. premature output
	   ((eq math-send-state 'premature-output)
	    (setq math-send-filter-status 'normal)
	    (goto-char (point-max))
	    (insert string)
	    (setq math-last-output-column (current-column))
	    (set-marker (process-mark proc) (point))
	    (if (save-excursion
		  (forward-line 0)
		  (looking-at "\\(^[ \t]*In\\[[0-9]+\\]:= ?\\)\\|\\(^[ \t]*Out\\[[0-9]+\\]\\(//[^=]*\\)?= ?\\)"))
		(setq math-send-state 'normal)
	      (check-dangling-indent-cookie proc)))
	   (t
	    (setq math-send-filter-status 'normal)
	    (goto-char (point-max))
	    (insert string)
	    (setq math-last-output-column (current-column))
	    (set-marker (process-mark proc) (point))
	    ;; at one time a check was made here for dangling indent cookies.
	    ;; but only if math-indent-cookies was true, since otherwise
	    ;; graphics resulted in spurious warnings.  However
	    ;; this code was deleted since with math-indent-cookies true
	    ;; you can see an indent cookie anyway, and if it happens when the
	    ;; math-send-state is 'normal, there is something seriously wrong
	    ;; anyway.
	    )) ; finish off t branch and entire cond
	  (set-buffer-modified-p (buffer-modified-p)) ;Update modeline
	  (save-excursion
	    (forward-line 0)
	    (if (not (looking-at "^[ \t]*In\\[[0-9]+\\]:= "))
		(setq mathematica-state "Printing")
	      (setq math-send-filter-active nil) ; Synchronize with tex-mma
	      (setq mathematica-state "Done"))
	    (set-buffer-modified-p (buffer-modified-p)) ;Update modeline
	    )
	  ) ; end of prgn
      ;; safely exit the filter
      ;; unwind-protect tail
      (set-buffer cbuf)
      (store-match-data save-match-data))))

(defun math-sentinel (process msg)
  (cond ((eq (process-status process) 'exit)
	 (setq mathematica-state "No Process")
	 (let ((p (process-buffer process))
	       (b (current-buffer)))
	   (set-buffer p)
	   (goto-char (point-max))
	   (insert "Process " (process-name process) " " msg)
	   (set-buffer b)))))

(defun math-mode ()
  "Major mode for interacting with Mathematica and editing .m files.

\\[math] starts Mathematica.  (See below for starting Mathematica on a 
remote host.)

\\[math-send-input] tries to identify stuff following last \"In[...]:=\" 
or blank line or the last output and sends it.  To clear out
Mathmatica after an error occurs, move point two lines below last
printing character and type \\[math-send-input].  Warning: do not
use Input[\"\"], and type in a mult-line reply; deadlock results.

\\[math-copy-cell] will copy a previous cell to the end of the buffer.
It prompts for the number of the cell to copy.  Blank is previous cell.
There are many more (and very useful) options.
Type \\[describe-key] \\[math-copy-cell] to see the full details.

\\[math-help] gives help on a Mathematica symbol.  With wildcards
it lists all matching symbols.  
\\[math-extra-help] or C-u \\[math-help] give more verbose help.
These functions use Mathematica's ? and ?? operations.

\\[math-complete-symbol] will complete the symbol near point.

\\[math-isearch-backward] does a backward regexp i-search, 
initialized to find In[...].

\\[kill-math-cell] kills the cell near point.  With prefix arg kills
this and subsequent cells.

\\[find-math-error] when typed after <<filename has returned a
syntax error will goto the error.  (Depends on Mathematica-search-path.)

\\[math-transform-float] converts thing near point like 6.02E23 to 6.02*10^23.
Type \\[describe-key] \\[math-transform-float] for more details.
\\[math-transform-floats-in-region] does it to all floats in the region

\\[interrupt-shell-subjob] interrupts Mathematica (v. 18)
\\[comint-interrupt-subjob] interrupts Mathematica (v. 19)
\\[kill-9-process] kills (-9) the Mathematica process.

\\[start-math] starts a Mathematica process in the current buffer.

\\[math-version] identifies this version of Mathematica mode.

\\[math-remove-symbol] creates a cell Remove[<<symbol>>], where <<symbol>> 
is the word near point. 

Most entries from the Emacs' shell mode are available as well.

If you are not in a buffer running Mathematica, \\[math-help], \\[math-extra-help], 
\\[math-complete-symbol], and \\[math-copy-cell] use or copy to the 
buffer *math*.  \\[math-help], \\[math-extra-help], and \\[math-complete-symbol]
all send input to Mathematica: chaos may ensue if you do this while Mathmatica
is busy with other work---no check is made.  You can change the buffer/process
these commands use with \\[set-math-process-buffer].

\\[set-math-electric-char] will cause selected characters to flash the 
paren-like character of the enclosing expression.  See its documentation.
This is experimental.  

Entry to this mode calls the value of math-mode-hook with no args,
if that value is non-nil.  Loading the file runs  math-mode-load-hook.

If variable math-remote-host is non-nil, \\[math] will start
Mathematica on host math-remote-host.
If you have trouble
with math-mode hanging for multi-line input, see the help on the variable
math-timeout.  Due to a Mathematica feature you should make sure that 
$BatchInput = False.  
Interrupts are not available when using a remote host, and synchronization
is sometimes incorrect; sorry.  See also the variables math-remote-user, 
math-display-var, and math-remote-shell."


  (interactive)
  (if emacs-version-18 (shell-mode) (comint-mode))
  (kill-all-local-variables)
  (setq major-mode 'math-mode)
  (setq mode-name "Mathematica")
  ;(setq mode-line-process '(": %s"))
  (setq mode-line-process '(": " mathematica-state))
  (use-local-map math-mode-map)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end "*)")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (set-syntax-table math-mode-syntax-table)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'math-last-output-column)
  (make-local-variable 'math-partial-output)
  (setq math-partial-output "")
  (make-local-variable 'math-send-state)
  (setq math-send-state 'normal)
  (make-local-variable 'doing-math-complete-symbol)
  (setq doing-math-complete-symbol nil)
  (make-local-variable 'math-indent-cookies)
  (make-local-variable 'math-indent-cookie-pending)
  ; Position of end of last input to Mathematica
  (make-local-variable 'math-last-input-end)

  (run-hooks 'math-mode-hook))


(defun math ()
  "Run Mathematica, input and output via buffer *math*."
  (interactive)
  (pop-to-buffer (start-math-process
		  "*math*" "math" math-process-string))
  ;; We don't make this one local.  That way if the
  ;; user changes the name of the buffer, say by writing
  ;; it to a file, math-process-buffer still points
  ;; to the right place.
  (setq math-process-buffer (current-buffer))
  (set-process-filter (get-buffer-process math-process-buffer) 
		      'math-send-filter)
  (set-process-sentinel (get-buffer-process math-process-buffer)
			'math-sentinel))

(defun start-math ()
  "Starts a Mathematica process in the current buffer."
  (interactive "*")
  (start-math-process (current-buffer) "math" math-process-string)
  (make-local-variable 'math-process-buffer)
  (setq math-process-buffer (current-buffer))
  (set-process-filter (get-buffer-process math-process-buffer) 
		      'math-send-filter)
  (set-process-sentinel (get-buffer-process math-process-buffer)
			'math-sentinel))


(defun math-complete-symbol ()
  "Complete the symbol preceeding point."
  (interactive "*")
  (let ((process (get-buffer-process math-process-buffer))
	sent-successfully)
    (if	(not (and process (memq (process-status process) '(run stop))))
	(error "No math process running in buffer %s" math-process-buffer))
    (setq math-completion-symbol (math-symbol-around-point))
    (unwind-protect
	(let ((cbuf (current-buffer)))
	  (set-buffer (get-buffer-create " Mathwork"))
	  (erase-buffer)
	  (set-buffer cbuf)
	  (setq doing-math-complete-symbol t)
	  (set-process-filter process 'math-help-filter)
	  (process-send-string process (concat 
"Scan[Print,Names[\"" math-completion-symbol "**\"]];Out[--$Line];\n"))
	  (setq sent-successfully t))
      (if (not sent-successfully)
	  (progn
	    (setq doing-math-complete-symbol nil)
	    (setq math-send-state 'normal) ; IS THIS RIGHT
	    (set-process-filter process 'math-send-filter))))))
	   
		      
(defun math-symbol-around-point ()
 "Return the symbol around the point as a string."
 (save-excursion
   (let (beg)
     (if (not (eobp)) (forward-char 1))
     (if (not (re-search-backward "\\w\\|\\s_" nil t))
	 ""
       (forward-char 1)
       (backward-sexp)
       (setq beg (point))
       (forward-sexp)
       (buffer-substring beg (point))))))

(defun math-extra-help () 
  "Like math-help with a prefix arg"
  (interactive)
  (let ((current-prefix-arg (list 1))
	(prefix-arg (list 1)))          ; I'm hacking.  
					; current-prefix-arg makes M-X ... work
                                        ; prefix-arg makes it work when bound to a key
					; I'm sure RMS had something else in mind.
    (call-interactively 'math-help)))

(defun math-help (symbol arg)
  "Display what Mathematica knows about SYMBOL.  
With prefix arg (2nd arg when called from a program) it gives more info."
  (interactive  ; read a word, using the word around point as the default
   (let ((enable-recursive-minibuffers t)
	 (try-word (math-symbol-around-point))
	 val)
     (if (string-equal try-word "")
	 (setq val (read-string "Mathematica symbol: "))
       (setq val (read-string (format "Mathematica symbol (default %s): "
				      try-word)))
       (if (string-equal val "")
	   (setq val try-word)))
     (if (string-equal val "")
	 (error "No symbol read"))
     (list val current-prefix-arg)))
  (let ((process (get-buffer-process math-process-buffer))
	sent-successfully)
    (if	(not (and process (memq (process-status process) '(run stop))))
      (error "No math process running in buffer %s" math-process-buffer))
    (unwind-protect
	(progn
	  (with-output-to-temp-buffer "*Help*"
	    (print-help-return-message))
	  (set-process-filter process 'math-help-filter)
	  (process-send-string process (concat (if arg "??" "?") symbol "\n"))
	  (setq sent-successfully t))
      (if (not sent-successfully) (set-process-filter process 
						      'math-send-filter)))))


(defun math-edit-function (symbol arg)
  "Display all of SYMBOL's definitions in InputForm"
  (interactive  ; read a word, using the word around point as the default
   (let ((enable-recursive-minibuffers t)
	 (try-word (math-symbol-around-point))
	 val)
     (if (string-equal try-word "")
	 (setq val (read-string "Mathematica symbol: "))
       (setq val (read-string (format "Mathematica symbol (default %s): "
				      try-word)))
       (if (string-equal val "")
	   (setq val try-word)))
     (if (string-equal val "")
	 (error "No symbol read"))
     (list val current-prefix-arg)))
  (let ((sent-successfully)
	(process (get-buffer-process math-process-buffer))
	(cbuf (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer (get-buffer-create " Mathwork"))
	  (erase-buffer)
	  (set-buffer cbuf)
	  (set-process-filter process 'math-edit-function-filter)
	  (process-send-string process 
			       (concat "Print[HoldForm[Clear[" symbol 
				       "]]];Definition[" symbol 
				       "]//InputForm\nPrint[\"asdfasdfasdfasdf\"];Out[$Line -= 2];\n"))
	  (setq sent-successfully t))
      (if (not sent-successfully)
	  (set-process-filter process 'math-send-filter)))))

(defun math-edit-function-filter (proc string)
  (let ((cbuf (current-buffer))
	(save-match-data (match-data))
	)
    (unwind-protect
	(progn
	  (set-buffer " Mathwork")
	  (goto-char (point-max))
	  (insert string)
	  (forward-line 0)
	  (if (and (looking-at "[ \t]*In\\[[0-9]+\\]" )
		    (re-search-backward "\\(\\S \\)\\s *In\\[[0-9]+\\]:= asdfasdfasdfasdf" nil t))
	      (let ((separator "")) ; separator puts things 
		; like (*=============*) between the defintions.
		; but the more I looked at it the more less-cluttered looked
		; better.  Feel free to change it to whatever you like.
		(set-process-filter proc 'math-send-filter)
		(delete-region (match-end 1) (point-max))
		(goto-char (point-max))
		(if (not (re-search-backward "Out\\[[0-9]+\\]//InputForm=" nil t))
		    (error "Math mode internal error"))
		(delete-region (match-beginning 0) (match-end 0))
		(if (looking-at "\\s *\\'") ; blank to eob
		    (progn ; extract name of function
		      (goto-char (point-min))
		      (re-search-forward "Clear\\[\\([^]]+\\)\\]")
		      (error "Function %s not defined" 
			     (buffer-substring 
			      (match-beginning 1) (match-end 1)))))
		(if (looking-at "\\s *Attributes\\[.+\\] =")
		    (progn 
		      (forward-sexp)
		      (end-of-line)
		      (newline)))
		(goto-char (point-min))
		(insert "(")
		(goto-char (point-max))
		(insert "\n;" separator ")\n")
		(goto-char (point-min))
		(replace-regexp "^\\([ \t]*\n\\)+" (concat ";" separator "\n"))
		(goto-char (point-max))
		(set-buffer cbuf)
		(insert-buffer " Mathwork")
		(forward-line 2)))
	  )
      ;; Unwind protect tail
      (set-buffer cbuf)
      (store-match-data save-match-data))))



(defun math-help-filter (proc string)
  (let ((cbuf (current-buffer))
	(save-match-data (match-data))
	(local-doing-math-complete-symbol doing-math-complete-symbol))
    ;; doing-math-complete-symbol is buffer-local and we are going
    ;; to switch buffers.
    (unwind-protect
	(progn
	  (if local-doing-math-complete-symbol
	      (set-buffer " Mathwork")
	    (set-buffer "*Help*"))
	  (goto-char (point-max))
	  (insert string)
	  (beginning-of-line)
	  (if (looking-at "^[ \t]*In\\[[0-9]+\\]:=")
	      (progn
		(delete-region (point) (point-max))
		(bury-buffer (current-buffer))
		(if local-doing-math-complete-symbol
		    (progn
		      (set-buffer cbuf)
		      ;; we are back to the original buffer, so this is ok
		      (setq doing-math-complete-symbol nil)
		      (insert (get-math-completion math-completion-symbol)))
		  (goto-char (point-min))))))
      (set-buffer cbuf)
      (store-match-data save-match-data))))

(defun check-math-syntax (pmin pmax) 
"Checks for various constructs likely to cause errors in Mathematica.
Asks for confirmation if one is found, if negative, an error is 
signalled.  The conditons checked for are
1.  line terminated with backslash-whitespace
2.  complete statement before end of cell
3.  unterminated string
4.  unclosed comment
5.  mismatched parens
6.  possible incomplete cell"
  (interactive "r")
  (let ((pt (point))
	possibleerr)
    (save-restriction
      (narrow-to-region pmin pmax)
      (goto-char pmin)
      (if (re-search-forward "\\\\[ \t]+$" nil t) 
	  ;; final t ==> on failure leave point untouched
	  (setq possibleerr
		"Line ends with backslash-whitespace, submit anyway? "))
      ;; point is at pmin; skip while loop if we already found an error
      (while (and (not possibleerr)
		  (not (eobp)))
	(end-of-line)
	(let ((parsestate (parse-partial-sexp (point-min) (point) -1)))
					; -1 ==> stop at neg paren depth
	  (if (not (looking-at "\\s *\\'")) ; not just all white space to eob
	      (cond  
	       ((< (nth 0 parsestate) 0) ; negative paren depth
		(setq possibleerr
		      "Mismatched parens, submit anyway? "))
	       ((and ; make sure this could NOT end a valid expression
		 (= (nth 0 parsestate) 0) ; zero paren depth
		 (not (nth 3 parsestate)) ; not in a string
		 (not (nth 4 parsestate)) ; not in a comment
		 (progn
		   (forward-line 0)
		   (looking-at math-valid-cell-ending-re)))
	        (setq possibleerr 
		      "Possible complete statement before end, submit anyway? ")
		(end-of-line)))
	    ;; we are at the end of the statement
	    (cond 
	     ((nth 3 parsestate)
	      (setq possibleerr
		    "Apparently unterminated string, submit anyway? "))
	     ((nth 4 parsestate)
	      (setq possibleerr 
		    "Apparently unclosed comment, submit anyway? "))
	     ((not (zerop (nth 0 parsestate)))
	      (setq possibleerr
		    "Apparently mismatched parens, submit anyway? "))
	     ((save-excursion
		(forward-line 0)
		(not (looking-at math-valid-cell-ending-re)))
	      (setq possibleerr 
		    "Possible incomplete cell, submit anyway? "))))
	  (if (not possibleerr) (forward-line 1)))))
    (if (and possibleerr (not (y-or-n-p possibleerr)))
	(progn
	  (error "Cancelled")
	  (setq math-send-filter-status 'syntax-error) 
	  )
      (goto-char pt))))


(defun start-math-process (bufferid procname program &optional startfile &rest switches)
  ;; A munged version of make-shell
  ;; Make-shell is part of Gnu Emacs, Copyright (C) 1985, 1986, 1987, 1988 
  ;; Free Software Foundation,Inc.
  ;; Used with permission.
  ;; bufferid can be a buffer or the name of a buffer
  ;; startfile is now ignored.  
  ;; It wouldn't have worked with Mathematica anyway.
  (let ((buffer (get-buffer-create bufferid))
	(disp (cond
		((eq math-display-var nil) (getenv "DISPLAY"))
		((stringp math-display-var) math-display-var)
		(t nil)))
	proc proc-args status)
    (setq proc (get-buffer-process buffer))
    (if proc (setq status (process-status proc)))
    (save-excursion
      (set-buffer buffer)
      (if (memq status '(run stop))
	  nil
	(if proc (delete-process proc))
	(message "Starting Mathematica...")
	(setq mathematica-state "Starting")
	(if (not (eq major-mode 'math-mode))
	    (math-mode))
	(setq math-indent-cookies nil)
	(setq math-header-re nil)
	(setq math-send-state 'starting-up)
	(if emacs-version-18
	    (progn
	      (if math-remote-host
		  (progn
		    (setq 
		     proc-args 
		     (delq nil
			   (list 
			    procname
			    buffer
			    math-remote-shell
			    math-remote-host
			    (if math-remote-user "-l")
			    math-remote-user ; if nil will be deleted
			    "sh"
			    "-c"
			    (mapconcat 
			     'identity
			     (delq nil
				   (list
				    "\""
				    (if disp (format "DISPLAY=%s" disp))
				    (format
				     "TERMCAP=emacs:co#%d:tc=unknown:"
				     (if emacs-version-18 (screen-width) (frame-width)))
				    "TERM=emacs"
				    "EMACS=t"
				    (format "MATHINDENTCOOKIE='%s'" 
					    math-indent-cookie)
				    (format "MATHINDENTCOOKIEMSG='%s'" 
					    math-indent-cookie-message)
				    program
				    switches
				    "\"")) " "))))); have mapconcat put spaces between
		;; The local case
		(setq 
		 proc-args
		 (append
		  (list
		   procname
		   buffer
		   (concat exec-directory "env"))
		  (delq nil 
			(list
			 (if disp (format "DISPLAY=%s" disp))
			 (format
			  "TERMCAP=emacs:co#%d:tc=unknown:"
			  (if emacs-version-18 (screen-width) (frame-width)))
			 "TERM=emacs"
			 "EMACS=t"
			 (format "MATHINDENTCOOKIE=%s" 
				 math-indent-cookie)
			 (format "MATHINDENTCOOKIEMSG=%s" 
				 math-indent-cookie-message)
			 "-" ; suppress other environment variables
			 program
			 switches)))))
	      (setq proc (apply 'start-process proc-args)))
	  ;; Now for the version 19 case
	  (progn
	    (if math-remote-host
		(progn
		  (setq 
		   proc-args 
		   (delq nil
			 (list 
			  procname
			  buffer
			  math-remote-shell
			  math-remote-host
			  (if math-remote-user "-l")
			  math-remote-user ; if nil will be deleted
			  "sh"
			  "-c"
			  (mapconcat 
			   'identity
			   (delq nil
				 (list
				  "\""
				  (if disp (format "DISPLAY=%s" disp))
				  (format
				   "TERMCAP=emacs:co#%d:tc=unknown:"
				   (if emacs-version-18 (screen-width) (frame-width)))
				  "TERM=emacs"
				  "EMACS=t"
				  (format "MATHINDENTCOOKIE='%s'" 
					  math-indent-cookie)
				  (format "MATHINDENTCOOKIEMSG='%s'" 
					  math-indent-cookie-message)
				  program
				  switches
				  "\"")) " "))))
		  (setq proc (apply 'start-process proc-args)))
	      ;; The local case
	      (let ((process-environment 
		     (append process-environment
			     (list
			      (format "MATHINDENTCOOKIE=%s" 
				      math-indent-cookie)
			      (format "MATHINDENTCOOKIEMSG=%s" 
				      math-indent-cookie-message)))))
		(if disp 
		    (setenv "DISPLAY" disp))
		(setq proc 
		      (comint-exec-1 procname buffer program switches))))))
	(goto-char (point-max))
	(set-marker (process-mark proc) (point))
	))
    buffer))

(defun backward-incarnations (inc)
  "Moves back ARG incarnations of Mathematica, as recognized
by math-header-re."
  (if inc
      (let ((count (cond ((numberp inc) inc)
			 ((equal inc '(4)) 1)
			 ((equal inc '(16)) 2)
			 ((equal inc '(64)) 3)
			 ((equal inc '(256)) 4)
			 ((equal inc '(1024)) 5)
			 (t (error "I'm too lazy to count that many prefix keys")))))
		(re-search-backward math-header-re nil nil count))))







(defun math-copy-cell (numberstring incarnations pt)
  "Copies the cell beginning In[<CELLNUMBER>] to the end of the buffer.  
With CELLNUMBER of empty string and point at or after last In[...]:= 
(and if buffer is its own math-process-buffer)
copies previous In cell to end of buffer.  With point before last In[...]:= 
copies cell near point (In, Out, or just a block of text) to end of buffer.  
If CELLNUMBER is followed by \"-\", even if otherwise blank, the designated
cell is deleted.  If by \"--\" all subsequent cells are deleted.
With an explicit CELLNUMBER, a prefix arg will skip back prefix arg 
incarnations before searching for In[<CELLNUMBER>].  C-u's count in unary.  
When called from a program, CELLNUMBER must be a string, second arg is 
INCARNATIONS back and third is POINT to begin search at."
  (interactive "sCell number (default is cell near point):  \nP\nd")
  (let (killflag)
    (cond ((string-match "--\\'" numberstring)
	   (setq killflag 'all)
	   (setq numberstring (substring numberstring 0 -2)))
	  ((string-match "-\\'" numberstring)
	   (setq killflag t)
	   (setq numberstring (substring numberstring 0 -1))))
    (cond  ((zerop (length numberstring))
	    (goto-char (point-max))
	    (if (and
		 (equal (get-buffer math-process-buffer) ; get-buffer is safe
					; in wierd cases
			(current-buffer))  ; copy to ourself?
		 (re-search-backward "^[ \t]*In\\[[0-9]+\\]:=" nil t)
		 (>= pt (point))) ; in or after last cell
	      (progn
		(re-search-backward "^[ \t]*In\\[[0-9]+\\]:=") ; back up to previous one
		(while (and (not (bobp))
			    (or (looking-at ; reject ones without any useful text 
				 "^[ \t]*In\\[[0-9]+\\]:=\\s *\\(\\'\\|\n\\s *$\\)")))
		  (re-search-backward "^[ \t]*In\\[[0-9]+\\]:=")))
	      (goto-char pt))) ; else branch: do current cell
	   (t
	    (goto-char (point-max))
	    (backward-incarnations incarnations)
	    (re-search-backward (concat "^[ \t]*In\\[" numberstring "\\]:="))))
    (if (interactive-p) (push-mark))
    (let* ((cellinfo (math-identify-cell (point) 'copy))
	   (copy (buffer-substring (car cellinfo) (nth 1 cellinfo)))
	   insert-point)
      (if killflag (kill-math-cell (car cellinfo) (eq killflag 'all)))
      (if (not (equal (get-buffer math-process-buffer)
		      (current-buffer)))
	  (pop-to-buffer math-process-buffer))
      (goto-char (point-max))
      (re-search-backward "\\S ")
      (forward-line 0)
      (if (looking-at "^[ \t]*In\\[[0-9]+\\]:=\\s *$")
	  (end-of-line)
	(goto-char (point-max)))
      (setq insert-point (point))
      (insert copy)
      (save-excursion ; patch up cells that begin with a blank line
	(goto-char insert-point)
	(if (looking-at "[ \t]*\n")
	    (progn
	      (end-of-line)
	      (insert "\\")))))))


;;; The following code works, and is not dependent on isearch internals, 
;;; but with version 19, it is too slow and
;;; keeps diddling with the minibuffer in an annoying way.


; (defun math-isearch-backward ()
;   "Does a backward regexp i-search, initialized to find In[...]:="
;   (interactive)
;   (if emacs-version-18
;       (progn
; 	(setq search-last-regexp "^[ \t]*In\\[[0-9]+\\]:=\\s *")
; 	(setq unread-command-char search-reverse-char)
; 	(isearch-backward-regexp))
;     (setq unread-command-events
; 	  ;; tricky: append will break up a string into a list of chars.
; 	  (append
; 	   ;; In v.19 interactive searches have different semantics 
; 	   ;; than programmed searches.  So we need a different string.  Bzzzt!
; 	   "^[\^q \t]*In\\[[0-9]+\\]:= *"
; 	   unread-command-events))
;     (isearch-backward-regexp)))


(defun math-isearch-backward ()
  "Does a backward regexp i-search, initialized to find In[...]:="
  (interactive)
  (if emacs-version-18
      (progn
	(setq search-last-regexp "^[ \t]*In\\[[0-9]+\\]:=\\s *")
	(setq unread-command-char search-reverse-char)
	(isearch-backward-regexp))
    ;; And now the version 19 stuff.
    (let ((mib-regexp "^ *In\\[[0-9]+\\]:= *")
	  (save-regexp-search-ring regexp-search-ring))
      (setq regexp-search-ring (cons mib-regexp regexp-search-ring))
      (setq unread-command-events
	    (append
	     (where-is-internal 
	      'isearch-repeat-backward nil isearch-mode-map 1)
	     unread-command-events))
      (isearch-backward-regexp)
      (cond 
       ;; if the user just did ^r's, restore the ring.
       ((string= (car regexp-search-ring) mib-regexp)
	(setq regexp-search-ring save-regexp-search-ring))
       ;; but if the usr did something extra, splice around mib-regexp.
       ;; but play it safe
       ((and (cdr regexp-search-ring)
	     (string= (car (cdr regexp-search-ring)) mib-regexp))
	(setq regexp-search-ring (cons (car regexp-search-ring)
				       (cdr (cdr regexp-search-ring))))))
)))


(defun math-identify-cell (pt mode &optional possiblebndy)
  "Finds cell around POS.  MODE can be one of 'submit, 'copy, or
'kill.  'submit searches for cells beginning with In blank or
Interrupt>, 'copy with In Out or blank line, and 'kill with just In or
Out.  A string crossing optional POSSIBLEBNDY (usually the process
mark) will result in query as to include characters before
POSSIBLEBNDY.  Returns a list of the buffer position of the beginning
and end of the cell and non-nil if the string was truncated at POSSIBLEBNDY."
  (save-excursion
    (let (bpt ept tpt inputresponse)
      (goto-char pt)
      ;; back up at most one blank line looking for input
      (end-of-line)
      (if (re-search-backward  
	   (cond 
	    ((eq mode 'copy)
	     "\\(^[ \t]*In\\[[0-9]+\\]:= ?\\)\\|\\(^\\s *\n\\)\\|\\(^[ \t]*Out\\[[0-9]+\\]\\(//[^=]*\\)?= ?\\)")
	    ((eq mode 'submit)
	     "\\(^[ \t]*In\\[[0-9]+\\]:= ?\\)\\|\\(Interrupt> ?\\)\\|\\(^\\s *\n\\)")
	    ((eq mode 'kill) "\\(^[ \t]*In\\[[0-9]+\\]:= ?\\)\\|\\(^[ \t]*Out\\[[0-9]+\\]\\(//[^=]*\\)?= ?\\)"))
	   nil 1)
	  (goto-char (match-end 0)) ; search succeeded
	(goto-char (point-min))) ; search failed
      ;; used to be no 'if' around re-search-forward and then
      ;; (goto-char (or (match-end 0) (point-min)))
      ;; but a bug in emacs causes (match-end 0) to be set even if the
      ;; search failed.
      (setq tpt (point)) ; place from which to initiate search for end of cell
      ;; for 'kill, back up to blank line before the beginning of an Out cell.
      (if (eq mode 'kill)
	  (if (match-beginning 2) ; Out...
	      (let ((savept (point)))
		(forward-line 0)
		(re-search-backward "\\(^[ \t]*$\\)\\|^[ \t]*In\\[[0-9]+\\]\\|^[ \t]*Out\\[[0-9]+\\]"
				    nil t) ; see if we find a blank before 
					;anything else
		(if (match-beginning 1) ; found a blank first
		    (forward-line 1) ; move off blank
		  (goto-char savept)
		  (forward-line 0)))
	    (forward-line 0))) ; if kill but not Out, also go to bol
      (setq bpt (point)) ; beginning of cell
      (goto-char tpt)
      (if (re-search-forward 
	   (cond ((memq mode '(submit copy))
		  "^\\s *$\\|^[ \t]*Out\\[[0-9]+\\][^=\n]*=\\|^[ \t]*In\\[[0-9]+\\]:=")
		 (t "^[ \t]*In\\[[0-9]+\\]:=")
		 ); end of cond
	   nil 1)
	  ;; If it matches, we have found the beginning of a line
	  ;;  following the cell.  If not kill, back up one character.  
	  ;; If it doesn't match we are at eob and end of cell.
	  (goto-char (max (- (match-beginning 0) (if (eq mode 'kill) 0 1)) 
			  bpt)))
      (setq ept (point))
      ;; Take care of boundary crossing problems
      (if (and possiblebndy (< bpt possiblebndy) (< possiblebndy ept))
	  (progn 
	    (goto-char possiblebndy)
	    (unwind-protect
		(progn
		  (insert "==>")
		  (if (y-or-n-p "Use only chars after ==> in buffer ")
		      (progn
			(setq bpt possiblebndy)
			(setq inputresponse t))))
	      (delete-backward-char 3))))
      (list bpt ept inputresponse))))



(defun kill-math-cell (pt arg) "Kills the cell around POINT.  
If it is an In[...] cell, the following Out[...] cell is also killed.
With prefix ARG kills to almost eob"
;; we should fix this so a numeric arg kills that many cells.
  (interactive "*d\nP")
  (let* ((region-info (math-identify-cell pt 'kill))
	 (kill-beg (car region-info))
	 kill-end
	)
    (if arg
	(progn
	  (goto-char (point-max))
	  (if (re-search-backward "In\\[[0-9]+\\]:= ?" nil t)
	      (setq kill-end (match-beginning 0)) ; kill to about eob
	    (setq kill-end (point-max)))) ; obscure case
      (setq kill-end (car (cdr region-info)))) ; normal case
    (kill-region kill-beg kill-end)
    (re-search-forward 
     "\\(^[ \t]*In\\[[0-9]+\\]:= ?\\)\\|\\(^[ \t]*Out\\[[0-9]+\\]\\(//[^=]*\\)?= ?\\)"
     nil t)))


(defun old-kill-math-cell ()
  (interactive)
  (error "kill-math-cell is now on ESC k"))


(defun get-math-completion (prefix)
  "Returns string to insert to complete a Mathematica symbol
  Designed to be called as in (insert (get-math-completion word))"
  (let ((cbuf (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer " Mathwork")
	  (goto-char (point-min))
	  (let (alist)
	    (while (looking-at "\\S +")
	      (setq alist (cons (list (buffer-substring (match-beginning 0) (match-end 0))) alist))
	      (forward-line 1))
	    (set-buffer cbuf)
	    (let ((t-c-result  (and alist (try-completion prefix alist))))
	      ; try-completion barfs on a nil alist, so we help it out
	      (cond ((eq t-c-result t) 
		     (message "%s is complete" prefix)
		     "")
		    ((eq t-c-result nil)
		     (message "No match found")
		     "")
		    ((not (string= prefix t-c-result))
		     (substring t-c-result (length prefix)))
		    (t (with-output-to-temp-buffer "*Help*"
			 (display-completion-list 
			  (all-completions prefix alist))
			 (print-help-return-message))
		       "")))))
      (set-buffer cbuf); unwind-protect exit
   )))

(defun kill-9-process ()
  "Kills the process in the current buffer as in kill -9."
  (interactive)
  (kill-process (get-buffer-process (current-buffer))))

(defun metered-process-send-string (process string)
  "The same semantics as process-send-string, except the
string is broken into small enough chunks to not mess up emacs."
  (let ((p 0)
	(len (length string)))
    (while (< p len)
      (process-send-string process
			   (substring string p (setq p (min len (+ p 80))))))))



(defun skip-over-white-lines ()
  ;; it might be possible to do this with 
  ;; (re-search-forward "\\(^\\s *\n\\)*")
  ;; but this works.
  (while (and 
	  (not (eobp))
	  (looking-at "^\\s *$") ; blank line
	  (zerop (forward-line)))))

(defun find-math-error ()
  "Searches for the last \"syntax error in\" message; goes to indicated line
in the indicated file.  It uses the symbol Mathematica-search-path rather 
than going to all the work to discover the real real search path."
  (interactive)
  (let (filename
	linenumber
	raw-filename
	(math-search-path Mathematica-search-path))
    (save-excursion
      (re-search-backward "Syntax::sntx:")
      (forward-line 0)
      (if (not (looking-at ".*(line \\([0-9]+\\) of \"\\([^\"\n \t]+\\)\")$"))
	  (error "Cannot parse error line"))
      (setq raw-filename (buffer-substring (match-beginning 2) (match-end 2)))
      (setq linenumber (string-to-int 
			(buffer-substring (match-beginning 1) (match-end 1)))))
    (while (not filename)
      (setq filename (expand-file-name raw-filename (car math-search-path)))
      (if (not (file-readable-p filename))
	  (progn (setq filename nil)
		 (setq math-search-path (cdr math-search-path))
		 (if (null math-search-path)
		     (error "File %s not found" raw-filename)))))
    (find-file-other-window filename)
    (goto-line linenumber)))


(defun set-math-process-buffer (buffer)
  "Sets the buffer in/to which to evaluate/copy Mathematica
code.  (You only need to use this function if you want a buffer 
other than *math*.)"
  (interactive "bMathematica buffer: ")
  (make-local-variable 'math-process-buffer)
  ;; The following trick will use the buffer itself if
  ;; it is defined.  That way if the user eventually 
  ;; changes the name, say by writing it out, this local
  ;; math-process-buffer will still point to the right place.  
  ;; But if the buffer does not yet exist, it will still work.
  (setq math-process-buffer (or (get-buffer buffer) buffer)))

(defun math-transform-float (&optional forcedecimal)
  "Converts a float near point in Fortran/C style to math style. 
With optional prefix ARG, forces a decimal point."
  (interactive "*P")
  ;; Parens are necessary.  Otherwise consider
  ;; 5^3e2 ==> 5^3*10^2.
  ;; It is important that something of the form -3.5e6 be transformed
  ;; to -(3.5*10^6).  Otherwise condsider 3-1e3==>3(-1*10^3).
  (let ((pt (point))
	(eolpoint (progn (end-of-line) (point)))
	temp)
    (forward-line 0)
    (while 
	(and
	 (setq temp 
	       (re-search-forward 
"\\(^\\|[^0-9.]\\)\\(\\([0-9]+\\.[0-9]*\\|[0-9]*\\.[0-9]+\\)\\|[0-9]+\\)[eE]\\([-+]?[0-9]+\\)" 
;;; bol or a character that can't be in a float followed by 
;;; significant, which is d+.d* or d*.d+ or d+  The first two are in the
;;; third paren group to detect the presence of a decimal point
;;; then an optional e or E and finally the exponent.

		eolpoint t)) ;no error
	 (<= (match-end 0) (1- pt)))
      ;; empty while body
      )
    (if (and temp
	     (<= (match-beginning 2) pt))
	(if (and forcedecimal 
		 (not (match-beginning 3))) ; no decimal point present
	    (replace-match "(\\2.*10^\\4)" t)
	  (replace-match "(\\2*10^\\4)" t))
      (goto-char pt)
      (error "No float found"))))


(defun math-transform-floats-in-region (pt1 pt2 &optional forcedecimal)
  "Converts all Fortran/C floats in REGION to Mathematica style.
   Optional non-nil prefix ARG forces decimal points."
  (interactive "*rP")
  ;; Parens are necessary.  Otherwise consider
  ;; 5^3e2 ==> 5^3*10^2.
  ;; It is important that something of the form -3.5e6 be transformed
  ;; to -(3.5*10^6).  Otherwise condsider 3-1e3==>3(-1*10^3).
  (save-excursion
    (save-restriction
      (narrow-to-region pt1 pt2)
	(goto-char pt1)
	(if (not forcedecimal)
	    (replace-regexp 
	     "\\(^\\|[^0-9.]\\)\\(\\([0-9]+\\.[0-9]*\\|[0-9]*\\.[0-9]+\\)\\|[0-9]+\\)[eE]\\([-+]?[0-9]+\\)" 
	     "\\1(\\2*10^\\4)")
	  (replace-regexp 
	   "\\(^\\|[^0-9.]\\)\\([0-9]+\\.[0-9]*\\|[0-9]*\\.[0-9]+\\)[eE]\\([-+]?[0-9]+\\)" 
	   "\\1(\\2*10^\\3)")
	  (goto-char pt1)
	  (replace-regexp 
	   "\\(^\\|[^0-9.]\\)\\([0-9]+\\)[eE]\\([-+]?[0-9]+\\)" 
	   "\\1(\\2.*10^\\3)")
	  ))))

(defun math-remove-symbol ()
  "Take the word near point and create a Remove[<<word>>] cell.  This is 
useful when a spelling error has occurred."
  (interactive)
  (let ((symbol (math-symbol-around-point)))
    (push-mark)
    (goto-char (point-max))
    (insert "Remove[" symbol "]")))

(defun math-electric-char-bounce-p (parenpt commapt)
  "A function that accepts the buffer position of an opening PAREN
and a the buffer position of a COMMA (or other character) and returns 
non-nil if the paren should be flashed.  At the moment this is called, the 
comma has temporarily been replaced with a right paren."
;;; This function is experimental and subject to moment-by-moment 
;;; hacking.  Right now it always flashes if the comma is more than 50
;;; characters away and never if it less than 10.  In between
;;; it flashs only if there are doubly nested parens in between.
;;; I'm not really sure what I like here.  If you really dislike how 
;;; this works try your own function, and kindly let me know what you do 
;;; like.
  (or (> (- commapt parenpt) 50)
      (and (> (- commapt parenpt) 10)
	   (>= (car (parse-partial-sexp parenpt commapt 3 nil)) 3))))

(defun math-electric-self-insert ()
  (interactive "*")
  (self-insert-command 1)
  (let (pt matchpt visible do-it)
    (save-excursion
      (insert ")")
      (setq pt (point))
      (backward-sexp)
      (setq matchpt (point))
      (setq visible (pos-visible-in-window-p))
      (setq do-it (math-electric-char-bounce-p matchpt pt))
      (goto-char pt)
      (delete-backward-char 1)
      (if do-it
	  (if visible
	      (progn (goto-char matchpt)
		     (sit-for 1))
	    (message "Continues %s" 
		     (progn 
		       (goto-char matchpt)
		       (forward-line 0)
		       (buffer-substring (point) (progn 
						   (end-of-line)
						   (point))))))))))

      
      
(defun set-math-electric-char (char target-value)
  "Turns CHAR to an electric-char if VALUE is non-nil, othewise turns it
to a normal character.  An electric char will flash the beginning of the 
enclosing expression."
  (interactive "cKey to be made electric/unelectric \nSt for electric, nil for unelectric ")
  (define-key math-mode-map (char-to-string char) 
    (if target-value 
	'math-electric-self-insert
	'self-insert-command)))


(run-hooks 'math-mode-load-hook)

(provide 'math)