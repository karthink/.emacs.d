;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This mathematica.el defines mathematica-mode version 2.1.0. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mathematical.el, A Mathematica interface through GNU Emacs
;;; Copyright (C) 2002  Jim Pivarski
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mathematica is (C) Copyright 1988-1999 Wolfram Research, Inc.
;;;
;;; Protected by copyright law and international treaties.
;;;
;;; Unauthorized reproduction or distribution subject to severe civil
;;; and criminal penalties.
;;;
;;; Mathematica is a registered trademark of Wolfram Research.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; How to use this package:
;;; 
;;;   1) Save the file `mathematica.el' to some convenient directory.
;;;
;;;   2) Insert the following in your `.emacs' or `.xemacs/init.el'
;;;      initialization file:
;;;
;;;         (load-file "<convenient-directory-name>/mathematica.el")
;;;
;;;   2.1) If you want to make interaction (slightly) faster, run the
;;;        command M-x byte-compile-file RET mathematica.el RET.
;;;
;;;   2.2) This package uses the command-line interface to
;;;        Mathematica. If Mathematica is installed in the default
;;;        way, you can invoke this on the shell with the command name
;;;        `math'. If you need to type something different to start up
;;;        the text-only interface, such as
;;;        `/usr/local/mathematica/Executables/Linux/math', you need
;;;        to put the following in your `.emacs' or `.xemacs/init.el'
;;;        initialization file:
;;;
;;;         (setq mathematica-command-line "/usr/local/mathematica/Executables/Linux/math")
;;;
;;;        This command line is simply called, so you can include
;;;        arguments.
;;;
;;;   3) Start a Mathematica in Emacs process either by visiting a
;;;      file which ends in a `.m' suffix or by M-x mathematica RET.
;;;
;;;   4) Each Mathematica process is associated with one working
;;;      (interaction) buffer and one log buffer. The working buffer
;;;      is labeled `Mathematica [status...]' and the log buffer is
;;;      labeled `Mathematica Log'. If you want to only look at the
;;;      interaction buffer, type C-x 1. If you want to see both, type
;;;      C-c s.
;;;
;;;   4.1) Each interaction buffer is associated with exactly one
;;;        Mathematica process. Each Mathematica process is associated
;;;        with exactly one interaction buffer and one log buffer.
;;;        Commands and variables in one buffer WILL NOT AFFECT
;;;        commands and variables in another buffer. This was done on
;;;        purpose.
;;;
;;;   5) Type a mathematica command in the interaction buffer,
;;;      delimited from other commands and text by an empty line. To
;;;      execute it, put the cursor anywhere in the paragraph and type
;;;      C-j.
;;;
;;;   6) If the command is evaluated quickly, you will see the output
;;;      inserted below the command with an Out[] line number.
;;;
;;;   7) If the command takes a long time, you will see
;;;      `[Calculating...]' under the command, and when the result
;;;      arrives, it will be put where the `[Calculating...]' message
;;;      is. While Mathematica is calculating, you can continue typing
;;;      (or do whatever you want with Emacs). Just don't mess with
;;;      the `[Calculating...]' message and the output will go to the
;;;      right place. It won't overwrite your other work or go in the
;;;      wrong buffer or anything like that.
;;;
;;;   7.1) You can even send other commands while Mathematica is
;;;        calculating. Mathematica will handle them as soon as it is
;;;        finished with the one it's on. Every output will go to the
;;;        right `[Calculating...]' message.
;;;
;;;   8) If you want to abort a calculation, type C-c a. Mathematica
;;;      will abort the command it's working on and all commands that
;;;      have been sent to it while it was busy.
;;;
;;;   9) It's nice to know the following Emacs commands for getting
;;;      around paragraphs (command and output blocks): M-a goes to
;;;      the beginning of paragraph, M-e goes to the end, M-k kills
;;;      the next paragraph (e.g. unwanted output). Up and down arrows
;;;      while holding down shift skip paragraphs.
;;;
;;;   9.1) It's nice to know the following Emacs commands for getting
;;;        around parenthesis trees (since Mathematica is almost as
;;;        paren-crazy as lisp): C-M-f and C-M-b goes forward or
;;;        backward on the same parenthesis level (VERY useful for
;;;        adding arguments to a deeply-nested function). C-M-u and
;;;        C-M-d step up and down the parenthesis tree. All of this
;;;        takes into account parenthesis type: (), {} or [].
;;;
;;;   10) Emacs will complain if you try to quit without killing the
;;;       attached Mathematica processes. You can kill the one
;;;       associated with the current interaction buffer with C-c k,
;;;       but it is better to send the Mathematica command `Exit'.
;;;
;;;   11) If you have killed the Mathematica process associated with a
;;;       given interaction buffer and would like to restart it (a
;;;       good way of being certain there are no stray variables
;;;       messing with your calculations), type C-c r. If you type
;;;       this while there is a live Mathematica process attached,
;;;       this process is first killed.
;;;
;;;   APPENDIXES
;;;
;;;   A1) An alternative to C-j is the key sequence C-u C-j. This will
;;;       execute a paragraph in the same way, but when the output
;;;       comes up, the point will be set to the end of the output and
;;;       the mark will be set to the beginning. Then you can toggle
;;;       between looking at the beginning and end of a long output by
;;;       C-x x (interchange point and mark) and kill the output with
;;;       C-w (kill region between point and mark). This is the way I
;;;       used to do Mathematica interaction, but it's not really
;;;       convenient for small commands, keeping track of buffer
;;;       positions with the mark stack and especially for commands
;;;       that take a long time, since having the point change while
;;;       you're typing something else is atrocious.
;;;
;;;   A2) Keep in mind that while you can do multiple statements in a
;;;       single execution with Mathematica, they all need to end with
;;;       a semicolon except for the last one. A common mistake (for
;;;       me, at least) is to work on two commands independantly with
;;;       no semicolons at the end, so that their output can be seen,
;;;       then join them into a block for easy evaluation, forgetting
;;;       to put a semi-colon between them. Mathematica tries to
;;;       multiply them together, which is often wrong or confusing or
;;;       a Mathematica syntax error.
;;;
;;;   A3) Mathematica's default truncation at 78 characters can make
;;;       output hard to read, particularly tables. To update
;;;       Mathematica on your current window width, type `C-c w'.
;;;
;;;   A4) This package checks for balanced parentheses before sending
;;;       Mathematica the command since Mathematica would otherwise
;;;       sit and wait for a continuation that closes the paren stack.
;;;       This confuses the user (who is meanwhile staring at a
;;;       `[Calculating...]' message with no indication that
;;;       Mathematica is idle) and it confuses mathematica-mode, which
;;;       gets an off-by-one error in identifying which command goes
;;;       with which `[Calculating...]' message. THEREFORE, I avoid
;;;       the whole mess by only sending Mathematica paren-balanced
;;;       expressions.
;;;
;;;   A5) Try the tab key! I hope you like the indentation scheme. It
;;;       is meant to help keep track of parens, so that you know
;;;       what's an argument of what. Close parens are electric
;;;       (automatically indent).
;;;
;;;   A6) It sometimes happens that paren-match blinking slows down
;;;       the execution of a keyboard macro. You can turn off
;;;       paren-match blinking by:
;;;           M-: (setq blink-matching-delay 0) RET
;;;       When you want to turn it on again (it's a wonderful
;;;       feature), do this:
;;;           M-: (setq blink-matching-delay 1) RET
;;;
;;;   A7) Here are all of the commands that you can invoke:
;;;
;;;     key             binding
;;;     ---             -------
;;;     C-j             mathematica-execute
;;;     
;;;     C-c k           mathematica-kill-this-kernel
;;;     C-c r           mathematica-restart-kernel
;;;     C-c s           mathematica-split-screen
;;;     C-c a           mathematica-abort-calculation
;;;     C-c w           mathematica-send-window-width
;;;     
;;;     M-x mathematica-mode-copying   for a copyright message
;;;     M-x mathematica-mode-warantee  for a no-warantee message
;;;
;;;   A8) Here are all the variables you can set in your `.emacs' if
;;;       you wish:
;;;
;;;     variable                   		    default value
;;;     --------                   		    -------------
;;;     mathematica-command-line                    "math"
;;;     mathematica-always-start-kernel-with-mode   nil
;;;     mathematica-split-on-startup                nil

(setq auto-mode-alist
      (append
       '(
 	 ("\\.m\\'" . mathematica-mode)   ; .m -> mathematica plain-text
	 )
       auto-mode-alist))

;;; These variables you can change

(defvar mathematica-command-line "math"
  "How to access the command-line interface to Mathematica on your system."
  )

(defvar mathematica-always-start-kernel-with-mode nil
  "If t, a Mathematica kernel will be started every time you enter
Mathematica Mode (either by M-x mathematica-mode RET or by visiting a
.m file)."
  )

;; added by p.weitershausen@physik.tu-dresden.de
(defvar mathematica-never-start-kernel-with-mode nil
  "If t, a Mathematica kernel will never be started when you enter
Mathematica Mode (either by M-x mathematica-mode RET or by visiting a
.m file)."
  )
;; end addition by p.weitershausen@physik.tu-dresden.de

(defvar mathematica-split-on-startup nil
  "If t, entering Mathematica mode will split the screen to show you
the kernel starting up."
  )

;;; The rest of these variables are internal

(defvar mathematica-status ()
  "A word or two describing the state of the Mathematica kernel
associated with this buffer (local variable)."
  )

(defvar mathematica-kernel-workbuf ()
  "An association list connecting Mathematica processes with working
buffers."
  )

(defvar mathematica-kernel-marks ()
  "An association list connecting Mathematica processes with the mark
queue."
  )

(defvar mathematica-waiting-for-abort-message nil
  "A set of Mathematica processes which are waiting for an interrupt
message."
  )

(defvar mathematica-waiting-for-abort-message2 nil
  "A set of Mathematica processes which are waiting for the second
part of an interrupt message." )

(defvar mathematica-mode-map ()
  "Keymap used in Mathematica mode."
  )
(if mathematica-mode-map
    ()
  (setq mathematica-mode-map (make-sparse-keymap))
  (define-key mathematica-mode-map "\C-j" 'mathematica-execute)
  (define-key mathematica-mode-map "\M-\C-m" 'mathematica-execute)
  (define-key mathematica-mode-map "\C-ca" 'mathematica-abort-calculation)
  (define-key mathematica-mode-map "\C-cs" 'mathematica-split-screen)
  (define-key mathematica-mode-map "\C-cr" 'mathematica-restart-kernel)
  (define-key mathematica-mode-map "\C-ck" 'mathematica-kill-this-kernel)
  (define-key mathematica-mode-map "\C-cw" 'mathematica-send-window-width)
  (define-key mathematica-mode-map "\177" 'backward-delete-char-untabify)
  (define-key mathematica-mode-map ")" 'mathematica-electric-paren)
  (define-key mathematica-mode-map "]" 'mathematica-electric-braket)
  (define-key mathematica-mode-map "}" 'mathematica-electric-brace) 
 )

(defvar mathematica-mode-abbrev-table nil
  "Abbrev table in use in Mathematica-mode buffers."
  )

(defvar mathematica-mode-syntax-table nil "")
(if (not mathematica-mode-syntax-table)
    (let ((i 0))
      (setq mathematica-mode-syntax-table (make-syntax-table))

      ;; white space
      (modify-syntax-entry ?  " " mathematica-mode-syntax-table)  
      (modify-syntax-entry ?\t " " mathematica-mode-syntax-table)
      (modify-syntax-entry ?\f " " mathematica-mode-syntax-table)
      (modify-syntax-entry ?\n " " mathematica-mode-syntax-table)
      (modify-syntax-entry ?\^m " " mathematica-mode-syntax-table)
      
      ;; comments and parens
      (modify-syntax-entry ?( "()1b" mathematica-mode-syntax-table)  
      (modify-syntax-entry ?) ")(4b" mathematica-mode-syntax-table)
      (modify-syntax-entry ?* "_ 23b" mathematica-mode-syntax-table)

      ;; pure parens
      (modify-syntax-entry ?[ "(]" mathematica-mode-syntax-table)
      (modify-syntax-entry ?] ")[" mathematica-mode-syntax-table)
      (modify-syntax-entry ?{ "(}" mathematica-mode-syntax-table)
      (modify-syntax-entry ?} "){" mathematica-mode-syntax-table)

      ;; punctuation
      (modify-syntax-entry ?= "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?: "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?% "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?< "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?> "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?& "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?| "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?_ "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?/ "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?! "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?@ "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?# "." mathematica-mode-syntax-table)
      (modify-syntax-entry ?\' "." mathematica-mode-syntax-table)

      ;; quotes
      (modify-syntax-entry ?\\ "\\" mathematica-mode-syntax-table)
      (modify-syntax-entry ?\" "\"" mathematica-mode-syntax-table)

      ;; for Mathematica numbers, the following would be better as
      ;; parts of symbols
      (modify-syntax-entry ?- "_" mathematica-mode-syntax-table)
      (modify-syntax-entry ?. "_" mathematica-mode-syntax-table)
      (modify-syntax-entry ?\` "_" mathematica-mode-syntax-table)
      (modify-syntax-entry ?^ "_" mathematica-mode-syntax-table)

      (modify-syntax-entry ?$ "_" mathematica-mode-syntax-table)
      (modify-syntax-entry ?+ "_" mathematica-mode-syntax-table)

      ;; create an abbrev table for mathematica mode
      (define-abbrev-table 'mathematica-mode-abbrev-table ())

      ) ; end of let
  )

(defvar mathematica-font-lock-keywords
  '(
    ("^In\[[0-9]+\]:=" . font-lock-keyword-face)
    ("^Out\[[0-9]+\]=" . font-lock-keyword-face)
    ("^Out\[[0-9]+\]//[A-Za-z][A-Za-z0-9]*=" . font-lock-keyword-face)
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t]*[\[][ \t]*[\[]" 1 "default")
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t]*[\[]" 1 font-lock-function-name-face)
    ("//[ \t\f\n]*\\([A-Za-z][A-Za-z0-9]*\\)" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t\f\n]*/@" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t\f\n]*//@" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9]*\\)[ \t\f\n]*@@" 1 font-lock-function-name-face)
    ("_[) \t]*\\?\\([A-Za-z][A-Za-z0-9]*\\)" 1 font-lock-function-name-face)
    ("\\(&&\\)" 1 "default")
    ("&" . font-lock-function-name-face)
    ("\\\\[[A-Za-z][A-Za-z0-9]*\]" . font-lock-constant-face )
    ("$[A-Za-z0-9]+" . font-lock-variable-name-face )
    ("\\([A-Za-z0-9]+\\)[ \t]*\\->" 1 font-lock-type-face )
    ("<<[ \t\f\n]*[A-Za-z][A-Za-z0-9]*`[ \t\f\n]*[A-Za-z][A-Za-z0-9]*[ \t\f\n]*`"
     . font-lock-type-face )
    ("[A-Za-z][A-Za-z0-9]*::[A-Za-z][A-Za-z0-9]*" . font-lock-warning-face)
    ("\\[Calculating\\.\\.\\.\\]" . font-lock-warning-face)
    ("\\[Mathematica.*\\]" . font-lock-warning-face)
    ("^Interrupt>" . font-lock-warning-face)
    ("-Graphics-" . font-lock-type-face)
    ("-DensityGraphics-" . font-lock-type-face)
    ("-ContourGraphics-" . font-lock-type-face)
    ("-SurfaceGraphics-" . font-lock-type-face)
    ("-Graphics3D-" . font-lock-type-face)
    ("-GraphicsArray-" . font-lock-type-face)
    ("-Sound-" . font-lock-type-face)
    ("-CompiledCode-" . font-lock-type-face)
    )
  )

(defun mathematica-log-mode ()
  "Major mode for viewing Mathematica interaction logs in Emacs."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'mathematica-log-mode)
  (setq mode-name "Mathematica Log")
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mathematica-font-lock-keywords nil t))

  (setq buffer-read-only t)
  )

(defun mathematica-mode ()
  "Major mode for editing Mathematica plain text files (.m) in Emacs.
Commands:
\\{mathematica-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (use-local-map mathematica-mode-map)

  (setq major-mode 'mathematica-mode)
  (setq mode-name "Mathematica")

  (make-local-variable 'mathematica-status)
  (setq mathematica-status "not running")
  (setq mode-line-process '(" is " mathematica-status "."))

  (setq local-abbrev-table mathematica-mode-abbrev-table)
  (set-syntax-table mathematica-mode-syntax-table)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mathematica-indent-line)
;  (make-local-variable 'comment-indent-function)
;  (setq comment-indent-function 'mathematica-indent-comment)

  (make-local-variable 'comment-start)
  (setq comment-start "(*")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*")

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mathematica-font-lock-keywords nil t))

  (make-local-variable 'kill-buffer-hook)
  (setq kill-buffer-hook 'mathematica-kill-this-kernel)

;; added by p.weitershausen@physik.tu-dresden.de
;;   (if (or mathematica-always-start-kernel-with-mode
;; 	  (y-or-n-p "Start a Mathematica kernel for evaluations? ")
;; 	  ) ; end of or
  (if (and
       (not mathematica-never-start-kernel-with-mode)
       (or mathematica-always-start-kernel-with-mode
	   (y-or-n-p "Start a Mathematica kernel for evaluations? "))
       ) ; end of logical expression
;; end addition by p.weitershausen@physik.tu-dresden.de
      (progn
	(mathematica-internal-start-kernel (current-buffer))
	(if mathematica-split-on-startup
	    (mathematica-split-screen)
	  ) ; end if
	) ; end of progn
    (message "You can start a Mathematica kernel with C-c r or M-x mathematica-restart-kernel.")
    ) ; end if
  )

(defun mathematica ()
  "Start a Mathematica process in a new buffer."
  (interactive)
  (let ((oldval mathematica-always-start-kernel-with-mode))
    (setq mathematica-always-start-kernel-with-mode t)
    (switch-to-buffer (generate-new-buffer "tmp.m"))
    (mathematica-mode)
    (setq mathematica-always-start-kernel-with-mode oldval)
    ) ; end let
  )

(defun mathematica-internal-start-kernel (workbuf)
  "Internal function for starting Mathematica kernels."
  (mathematica-cleanup-zombies)
  (if (bufferp workbuf)
      (let ((kernel) (oldbuf))
        (if (rassoc workbuf mathematica-kernel-workbuf)
            (message
             (format "There is already a kernel associated with \"%s\"."
                     (buffer-name workbuf)))
          (progn
            (message "Starting Mathematica kernel...")
            (setq kernel (start-process
                          (format "mathematica<%s>" (buffer-name workbuf))
                          (format "*mathematica<%s>*" (buffer-name workbuf))
                          mathematica-command-line))
            (message "Starting Mathematica kernel... done!")

	    (setq oldbuf (current-buffer))
	    (set-buffer (process-buffer kernel))
	    (mathematica-log-mode)
	    (set-buffer oldbuf)

            (setq mathematica-kernel-workbuf
                  (append mathematica-kernel-workbuf
                          (cons (cons kernel workbuf) nil)
                          ) ; end append
                  ) ; end setq

            (setq mathematica-kernel-marks
                  (append mathematica-kernel-marks
                          (cons (cons kernel []) nil)
                          ) ; end append
                  ) ; end setq

            (set-process-filter kernel 'mathematica-filter)
            (set-process-sentinel kernel 'mathematica-sentinel)

	    (if (processp kernel) (setq mathematica-status "starting up"))

            kernel
            ) ; end of "starting Mathematica" progn
          ) ; end if process already exists
        ) ; end let
    (message "argument is not a buffer!")
    ) ; end if
  )

(defun mathematica-split-screen ()
  "Splits the screen into work buffer above, log buffer below"
  (interactive)
  (let ((kernel (mathematica-kernel-from-workbuf (current-buffer))))
    (if kernel
	(set-window-buffer (split-window) (process-buffer kernel))
      (message "This buffer has no Mathematica kernel!")
      ) ; end if kernel
    ) ; end let
  )

(defun mathematica-kill-this-kernel ()
  "Kills the Mathematica kernel associated with this working buffer."
  (interactive)
  (let ((kernel (mathematica-kernel-from-workbuf (current-buffer)))
	(isreal))
    (setq isreal (processp kernel))
    (mathematica-kill-kernel kernel)
    (if isreal (message "Mathematica process killed."))
    (setq mathematica-status "not running")
    ) ; end let
  )

(defun mathematica-kill-kernel (kernel)
  "Internal function for killing a Mathematica kernel."
  (if (processp kernel)
      (progn
        (setq mathematica-kernel-workbuf
              (delete (assoc kernel mathematica-kernel-workbuf)
                      mathematica-kernel-workbuf)
              ) ; end of setq

        (setq mathematica-kernel-marks
              (delete (assoc kernel mathematica-kernel-marks)
                      mathematica-kernel-marks)
              ) ; end of setq

        (delete-process kernel)

        ) ; end of progn
    (mathematica-cleanup-zombies)
    ) ; end if kernel is still alive
  )

(defun mathematica-restart-kernel ()
  "Restarts the Mathematica kernel associated with this buffer."
  (interactive)
  (let ((kernel (mathematica-kernel-from-workbuf (current-buffer))))
    (if (processp kernel)
        (mathematica-kill-kernel kernel)
      nil)
    ) ; end of let
  (mathematica-internal-start-kernel (current-buffer))
  )

(defun mathematica-send-window-width ()
  "Tell Mathematica the current window width, so that output is properly aligned."
  (interactive)

  (let ((command (format "SetOptions[$Output, PageWidth -> %d, PageHeight -> Infinity];"
			 (- (window-width) 2)))
	(start-mark) (end-mark)
	(oldbuf (current-buffer))
	(oldpos (point))
	)
    (let ((kernel (mathematica-kernel-from-workbuf oldbuf)))
      (if (processp kernel)
	  (progn
	    (goto-char (point-max))
	    (insert "\n\n")
	    (setq start-mark (point-marker))
	    (insert "[Calculating...]")
	    (setq end-mark (point-marker))
	    (goto-char oldpos)

	    ;; insert the input and marks into the queue corresponding
	    ;; to this kernel
	    (if (= (mathematica-marks-length kernel) 0)
		(progn
		  (set-buffer (process-buffer kernel))
		  (goto-char (point-max))
		  (setq buffer-read-only nil)
		  (insert command)
		  (setq buffer-read-only t)
		  (goto-char (point-max))
		  (set-marker (process-mark kernel) (point))
		  (mathematica-insert-marks kernel start-mark end-mark t)
		  ) ; end progn
	      (mathematica-insert-marks kernel start-mark end-mark t command)
	      ) ; end if
	    
	    (process-send-string kernel (format "%s\n" command))
	    (setq mathematica-status "working")
	    (message command)
	    ) ; end progn
	(error "This buffer has no Mathematica kernel!")
	) ; end if
      ) ; end let kernel
    ) ; end let everything else
  )

(defun mathematica-execute (arg)
  "Executes a paragraph of code with the Mathematica kernel associated
with this buffer. If an arg is passed, the output will set position
and mark."
  (interactive "P")
  
  (if (null arg) (setq arg t) (setq arg nil)) ; (I had this backward at first.)

  (let ((kernel (mathematica-kernel-from-workbuf (current-buffer))) (oldpos (make-marker)))

    (if (processp kernel)
        (let ((start) (end) (kernel) (input) (oldbuf (current-buffer))
              (tmpbuf (generate-new-buffer "*mathematica-temp*"))
              (start-mark) (end-mark))

	  (set-marker oldpos (point))
	  (undo-boundary)

          ;; find the command and make a "[Calculating...]" notice
          (if (re-search-backward "[\f\n][ \t]*[\f\n]" nil t)
              nil
            (goto-char (point-min))
            ) ; end if
          (skip-chars-forward " \t\f\n")
          (setq start (point))
          (if (re-search-forward "[\f\n][ \t]*[\f\n]" nil t)
              nil
            (goto-char (point-max))
            ) ; end if
          (skip-chars-backward " \t\f\n")
          (setq end (point))

          ;; get the input
          (setq input (buffer-substring start end))

          ;; format it properly
          (set-buffer tmpbuf)
          (insert input)
          (goto-char (point-max))
          (beginning-of-line)
          (while (> (point) (point-min))
            (previous-line 1)
            (end-of-line)
            (insert " \\")
            (beginning-of-line)
            )

	  ;; I have to resolve the disagreement between Emacs and
	  ;; Mathematica about the use of "\[" as an open paren
	  (setq input
		(format "%s\n" (buffer-substring (point-min) (point-max))))

	  (beginning-of-buffer)
	  (while (search-forward "\\(" nil t) (replace-match "(" nil t))
	  (beginning-of-buffer)
	  (while (search-forward "\\)" nil t) (replace-match ")" nil t))
	  (beginning-of-buffer)
	  (while (search-forward "\\[" nil t) (replace-match "[" nil t))
	  (beginning-of-buffer)
	  (while (search-forward "\\]" nil t) (replace-match "]" nil t))
	  (beginning-of-buffer)
	  (while (search-forward "\\{" nil t) (replace-match "{" nil t))
	  (beginning-of-buffer)
	  (while (search-forward "\\}" nil t) (replace-match "}" nil t))
	  (beginning-of-buffer)
	  (while (search-forward "\\\"" nil t)
	    (let ((number-of-quotes) (characters (append (buffer-substring (point-min) (point)) nil)))
	      (setq number-of-quotes (1- (- (length characters) (length (delq ?\" characters)))))
	      (if (= (% number-of-quotes 2) 0)
		  (replace-match "\"" nil t)
		(replace-match "" nil t)
		) ; end if
	      ) ; end let
	    ) ; end while

	  (let ((number-of-quotes) (characters (append (buffer-substring (point-min) (point-max)) nil)))
	    (setq number-of-quotes (- (length characters) (length (delq ?\" characters))))
	    (if (= (% number-of-quotes 2) 1)
		(progn
		  (set-buffer oldbuf)
		  (kill-buffer tmpbuf)
		  (goto-char oldpos)
		  (error "The quotes in this expression are not balanced")
		  ) ; end progn
	      ) ; end if
	    ) ; end let

	  (if (not (= (car (parse-partial-sexp (point-min) (point-max))) 0))
	      (progn
		(set-buffer oldbuf)
		(kill-buffer tmpbuf)
		(goto-char oldpos)
		(error "The parentheses in this expression are not balanced")
		) ; end progn
	    )

	  (set-buffer oldbuf)
          (kill-buffer tmpbuf)
	  (goto-char end)

	  (insert "\n\n")
	  (setq start-mark (point-marker))
	  (insert "[Calculating...]")
	  (setq end-mark (point-marker))

	  ;; insert the input and marks into the queue corresponding
	  ;; to this kernel
	  (setq kernel (mathematica-kernel-from-workbuf oldbuf))
	  (if (= (mathematica-marks-length kernel) 0)
	      (progn
		(set-buffer (process-buffer kernel))
		(goto-char (point-max))
		(setq buffer-read-only nil)
		(insert input)
		(setq buffer-read-only t)
		(goto-char (point-max))
		(set-marker (process-mark kernel) (point))
		(mathematica-insert-marks kernel start-mark end-mark arg)
		) ; end progn
	    (mathematica-insert-marks kernel start-mark end-mark arg input)
	    ) ; end if

	  (set-buffer oldbuf)
	  
          ;; send the command to Mathematica
	  (process-send-string kernel input)
	  (setq mathematica-status "working")

	  (goto-char oldpos)

          ) ; end let
      (error "This buffer has no Mathematica kernel!")
      ) ; end of if
    ) ; end of let
  )

(defun mathematica-filter (process output)
  "Puts the Mathematica output where it needs to go."
  (let ((oldbuf (current-buffer)) (maybe-more-output t))
    (progn
      (set-buffer (mathematica-workbuf-from-kernel process))
      (setq mathematica-status "spewing output into the log")
      (set-buffer oldbuf)

      ;; always put new output at the end
      (set-buffer (marker-buffer (process-mark process)))
      (goto-char (point-max))
      (setq buffer-read-only nil)
      (insert output)
      (setq buffer-read-only t)

      ;; but maybe this isn't all of it
      (while maybe-more-output
	(set-buffer (marker-buffer (process-mark process)))
	(goto-char (process-mark process))

	;; handle (and hide) Interrupt> messages
	(if (and (memq process mathematica-waiting-for-abort-message)
		 (re-search-forward "Interrupt> " nil t)
		 ) ; end of and
	    (progn
	      (set-buffer (mathematica-workbuf-from-kernel process))
	      (setq mathematica-status "trying to abort")
	      (set-buffer (marker-buffer (process-mark process)))

	      (process-send-string process "a\n")
	      (setq mathematica-waiting-for-abort-message
		    (delq process mathematica-waiting-for-abort-message)
		    ) ; end setq
	      (setq mathematica-waiting-for-abort-message2
		    (cons process mathematica-waiting-for-abort-message2)
		    ) ; end setq
	      (set-marker (process-mark process) (point))
	      (while (> (mathematica-marks-length process) 1)
		(let ((twomarks (mathematica-pop-marks process)) (cursorposition (make-marker)))
		  (set-buffer (marker-buffer (elt twomarks 0)))
		  (set-marker cursorposition (point))
		  (undo-boundary)
		  (goto-char (marker-position (elt twomarks 0)))
		  (delete-region (marker-position (elt twomarks 0))
				 (marker-position (elt twomarks 1)))
		  (insert "[Mathematica aborted this and the next calculation.]")
		  (if (elt twomarks 2) (goto-char cursorposition))
		  ) ; end let
		) ; end while
	      ) ; end progn
	  ) ; end if

	(if (re-search-forward "In\[[0-9]+\]:= " nil t)
	    (let ((newmark) (start) (end) (wholeoutput) (twomarks) (empty))
	      (set-buffer (mathematica-workbuf-from-kernel process))
	      (setq mathematica-status "sitting idle")
	      (set-buffer (marker-buffer (process-mark process)))

	      ;; this is where I would like to put the new mark,
	      ;; once I'm done with the old one
	      (setq newmark (point))

	      ;; I want the end of the wholeoutput to be at the
	      ;; beginning of this phrase
	      (re-search-backward "In\[[0-9]+\]:= " nil t)
	      (backward-char 2)
	      (setq end (point))
	      
	      ;; back to the beginning of the output
	      (goto-char (process-mark process))
	      (if (looking-at "[\r\n]") (forward-char))
	      (setq start (point))

; 	      ;; special case for abort messages (we don't want to see
; 	      ;; all the Interrupt> lines)
	      (if (and (memq process mathematica-waiting-for-abort-message2)
		       (re-search-forward "Out\[[0-9]+\]=" nil t))
		  (progn
		    (setq start (match-beginning 0))
		    (setq mathematica-waiting-for-abort-message2
			  (delq process mathematica-waiting-for-abort-message2)
			  ) ; end setq
		    ) ; end progn
		) ; end if

	      ;; finally set the new marker
	      (set-marker (process-mark process) newmark)
	      
	      ;; check to see if the output is empty
	      (skip-chars-forward " \t\n\r")
	      (setq empty (> (point) end))

	      ;; update the log point
	      (goto-char (point-max))

	      ;; copy the output to a string
	      (setq wholeoutput (buffer-substring start end))

	      ;; pop a set of marks from the input queue
	      (setq twomarks (mathematica-pop-marks process))
	      (if twomarks
		  (let ((m1 (elt twomarks 0)) (m2 (elt twomarks 1))
			(arg (elt twomarks 2)) (cursorposition (make-marker)))
		    ;; twomarks may contain an input string
		    (if (= (length twomarks) 4)
			(let ((input (elt twomarks 3)))
			  ;; enter the input at the appropriate In[]:= point
			  (goto-char start)
			  (setq buffer-read-only nil)
			  (insert input)
			  (setq buffer-read-only t)
			  ) ; end of let
		      nil
		      ) ; end of if
		    
		    ;; now we want to write over "[Calculating...]"
		    (set-buffer (marker-buffer m1))
		    (set-marker cursorposition (point))
		    (undo-boundary)
		    (goto-char (marker-position m1))
		    (delete-region (marker-position m1)
				   (marker-position m2))
		    (if (not empty)
			(progn
			  (insert wholeoutput)
			  (if (not arg) (insert "\n"))
			  ) ; end progn
		      (if (not (> (+ (point) 2) (point-max)))
			  (delete-char 2)
			) ; end if not at end of buffer
		      ) ; end if not empty

		    (if arg
			(goto-char cursorposition)
		      (push-mark (marker-position m1))
		      ) ; end if arg is not null
		    ) ; end of let
		) ; end of if twomarks
	      
	      ) ; end of let
	  (setq maybe-more-output nil) ; this is the case where
				       ; we're simply waiting for
				       ; more data
	  ) ; end of if
	) ; end while looking for more output lines
      ) ; end of progn
    (set-buffer oldbuf)
    ) ; end of let
  )

(defun mathematica-sentinel (process event)
  (let (twomarks m1 m2)
    (setq twomarks (mathematica-pop-marks process))
    (if twomarks
	(let ((m1 (elt twomarks 0)) (m2 (elt twomarks 1))
	      (arg (elt twomarks 2)) (cursorposition (make-marker)))
	  ;; now we want to write over "[Calculating...]"
	  (set-buffer (marker-buffer m1))
	  (set-marker cursorposition (point))
	  (undo-boundary)
	  (goto-char (marker-position m1))
	  (delete-region (marker-position m1)
			 (marker-position m2))
	  (insert (format "[Mathematica %s" event))
	  (delete-char -1)
	  (insert ".]")
	  (end-of-line)
	  (if (= (point) (point-max))
	      (insert "\n")
	    (forward-char 1)
	    )
	  (if arg
	      (goto-char cursorposition)
	    (push-mark (marker-position m1))
	    ) ; end if

	  (if (not (string-equal (process-status process) "run"))
	      (setq mathematica-status "not running")
	    (setq mathematica-status (format "returning \"%s\"" (substring event 0 -1)))
	    ) ; end if

	  ) ; end of let
      ) ; end of if twomarks
    ) ; end of let

  (let ((oldbuf (current-buffer)))
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (insert (format "\n * * * Mathematica %s" event))
    (delete-char -1)
    (insert ". * * *\n\n")
    (setq buffer-read-only t)
    (set-marker (process-mark process) (point))
    (set-buffer oldbuf)
    ) ; end of let
  
  (mathematica-cleanup-zombies)
  )

(defun mathematica-abort-calculation ()
  "Abort the current Mathematica calculation, if there is any."
  (interactive)
  (let ((kernel (mathematica-kernel-from-workbuf (current-buffer))))
    (interrupt-process kernel)
    (setq mathematica-waiting-for-abort-message
	  (cons kernel mathematica-waiting-for-abort-message)
	  ) ; end setq
    ) ; end of let
  )

(defun mathematica-kernel-from-workbuf (workbuf)
  "Internal function for getting the kernel from a working buffer."
  (car (rassoc workbuf mathematica-kernel-workbuf))
  )

(defun mathematica-workbuf-from-kernel (kernel)
  "Internal function for getting the working buffer from a kernel."
  (cdr (assoc kernel mathematica-kernel-workbuf))
  )

(defun mathematica-insert-marks (kernel m1 m2 arg &optional input)
  "Internal function which inserts inserts [m1 m2 input] into the
queue corresponding to \"kernel\"."
  (let ((oldarray (cdr (assoc kernel mathematica-kernel-marks)))
        (newarray))
    (if input
	(setq newarray (vconcat (vector (vector m1 m2 arg input)) oldarray))
      (setq newarray (vconcat (vector (vector m1 m2 arg)) oldarray))
      ) ; end if input

    ;; first completely get rid of the old entry
    (setq mathematica-kernel-marks
          (delete (assoc kernel mathematica-kernel-marks)
                  mathematica-kernel-marks)
          ) ; end of setq

    ;; then put it back with the new values
    (setq mathematica-kernel-marks
          (append mathematica-kernel-marks
                  (cons (cons kernel newarray) nil)
                  ) ; end append
          ) ; end setq

    ) ; end of let
  )

(defun mathematica-pop-marks (kernel)
  "Internal function which takes [m1 m2 input] off the other end of
the queue corresponding to \"kernel\" and returns them."
  (let ((oldarray (cdr (assoc kernel mathematica-kernel-marks)))
        (newarray []) (i 0))

    (if (= (length oldarray) 0)
        nil
      (progn
        ;; first completely get rid of the old entry
        (setq mathematica-kernel-marks
              (delete (assoc kernel mathematica-kernel-marks)
                      mathematica-kernel-marks)
              ) ; end of setq

        (while (< i (1- (length oldarray)))
          (setq newarray (vconcat newarray (vector (elt oldarray i))))
          (setq i (1+ i))
          ) ; end of while

        ;; then put it back with the new values
        (setq mathematica-kernel-marks
              (append mathematica-kernel-marks
                      (cons (cons kernel newarray) nil)
                      ) ; end append
              ) ; end setq

        (elt oldarray (1- (length oldarray)))
        ) ; end of progn
      ) ; end of if
    ) ; end of let
  )

(defun mathematica-marks-length (kernel)
  "Internal function which returns the length of the marks queue."
  (length (cdr (assoc kernel mathematica-kernel-marks)))
  )

(defun mathematica-cleanup-zombies ()
  "Removes items from \"mathematica-kernel-workbuf\" and
\"mathematica-kernel-marks\" that no longer exist."
  (interactive)

  ;; first do it for mathematica-kernel-workbuf
  (let ((tmp (copy-alist mathematica-kernel-workbuf)) (i 0))
    (setq mathematica-kernel-workbuf nil)
    (while (< i (length tmp))
      (if (string= "run" (process-status (car (nth i tmp))))
          (setq mathematica-kernel-workbuf
                (append mathematica-kernel-workbuf (cons (nth i tmp) nil))
                ) ; end of setq
        nil
        ) ; end if
      (setq i (1+ i))
      ) ; end while
    ) ; end let

  ;; then for mathematica-kernel-marks
  (let ((tmp (copy-alist mathematica-kernel-marks)) (i 0))
    (setq mathematica-kernel-marks nil)
    (while (< i (length tmp))
      (if (string= "run" (process-status (car (nth i tmp))))
          (setq mathematica-kernel-marks
                (append mathematica-kernel-marks (cons (nth i tmp) nil))
                ) ; end of setq
        nil
        ) ; end if
      (setq i (1+ i))
      ) ; end while
    ) ; end let
  )

(defun mathematica-indent-samelinep (first second)
  "Determines if the two points belong to the same line."
  (let ((limit (- second first)) (same-line))
    (save-excursion
      (if (re-search-forward "[\f\n]" limit t)
	  (setq same-line nil)
	(setq same-line t)
	) ; end of if
      ) ; end of excursion
    ) ; end of let
  )

(defun mathematica-indent-determine-in-comment ()
  "Returns the beginning of the comment, or nil."
  (save-excursion
    (let ((here (point)) (no-open nil) (first-open) (no-close nil) (first-close))

      (if (search-backward "(*" nil t)
	  (setq first-open (point))
	(setq no-open t)
	) ; end if

      (goto-char here)
      (if (search-backward "*)" nil t)
	  (setq first-close (point))
	(setq no-close t)
	) ; end if
      
      (cond ((and no-open no-close) nil)
	    ((and (not no-open) no-close) first-open)
	    ((and no-open (not no-close)) nil)
	    ((and (not no-open) (not no-close))
	     (if (> first-open first-close) first-open nil)
	     )
	    ) ; end cond
      ) ; end let
    ) ; end excursion
  )

(defun mathematica-indent-determine-unbalanced ()
  "Returns the beginning of the open paren or nil. Assumes not in
comment."
  (save-excursion
    (let ((toplevel nil) (home nil))
      (condition-case nil
	  (while (not home)
	    (up-list -1)
	    (if (and (<= (+ (point) 2) (point-max))
		     (string=
		      (format "%s" (buffer-substring (point) (+ (point) 2)))
		      "(*")
		     ) ; end of and
		(setq home nil)
	      (setq home t)
	      ) ; end if this open paren is the start of a comment
	    ) ; end while looking for an unbalanced open paren
	(error (setq toplevel (point)))
	) ; end condition-case
      (if toplevel nil (point))
      ) ; end let
    ) ; end excursion
  )

(defun mathematica-indent-stepthrough-comments ()
  "Moves the point backward through comments and non-eoln whitespace."
  (let ((home nil))
    (while (not home)
      (skip-chars-backward " \t")
      (setq home t) ; tenative assumtion
      (if (and (>= (- (point) 2) (point-min))
	       (string=
		(format "%s" (buffer-substring (- (point) 2) (point)))
		"*)")
	       ) ; end of and
	  (if (search-backward "(*" nil t)
	      (setq home nil)
	    nil
	    ) ; end if comment has a beginning
	) ; end if we stopped at the end of a comment
      ) ; end loop between comments and whitespace
    ) ; end of let
  )

(defun mathematica-indent-line-emptyp ()
  "Returns t if the line the point is on is empty, nil if not."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at "[\f\n]")
    ) ; end excursion
  )

(defun mathematica-indent-determine-prevline ()
  "Returns the meaningful end of the previous line (is it a
semicolon?), under the assumtion that you're not in a comment or
unbalanced parens."
  (save-excursion
    (let ((home nil) (meaningful-end))
      (while (not home)
	(beginning-of-line)
	(if (= (point) (point-min))
	    (progn ; There's nowhere to go. You're quite done.
	      (setq meaningful-end nil)
	      (setq home t)
	      ) ; end of progn
	  (progn

	    (backward-char 1)
	    (mathematica-indent-stepthrough-comments)
	
	    (if (mathematica-indent-line-emptyp)
		(progn ; we're done, there is no previous line
		  (setq meaningful-end nil)
		  (setq home t)
		  ) ; end progn
	      (progn
		(setq meaningful-end (point))
		(beginning-of-line)
		(if (= meaningful-end (point))
		    (setq home nil) ; there was nothing on this line but
				    ; comments
		  (setq home t) ; this is a good line
		  )
		) ; end progn
	      ) ; end if-else line empty

	    ) ; end line empty progn
	  ) ; end line empty if-else

	) ; end while
      
      meaningful-end
      ) ; end let
    ) ; end excursion
  )

(defun mathematica-indent-determine-indent ()
  "Returns the indentation of the line the point is on."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)
    ) ; end excursion
  )

(defun mathematica-indent-calculate (start)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (skip-chars-forward " \t")

    (let ((start-char) (start-close-paren ? )
	  (in-comment) (in-unbalanced) (prevline))
      (if (not (= (point) (point-max)))
	  (progn
	    (setq start-char (char-after))
	    (cond ((= start-char ?\)) (setq start-close-paren ?\())
		  ((= start-char ?\]) (setq start-close-paren ?\[))
		  ((= start-char ?}) (setq start-close-paren ?{))
		  ) ; end of cond
	    ) ; end of progn
	nil
	) ; end if you're not at the very end of the buffer

      (setq in-comment (mathematica-indent-determine-in-comment))
      (if in-comment ; in-comment = the position of the opening of the comment
	  (let ((tmp (+ in-comment 2)) (tmp-column))
	    (goto-char tmp)
	    (setq tmp-column (current-column))

	    (skip-chars-forward " \t")
	    (if (looking-at "[\f\n]") ; first comment line has nothing
				      ; but "(*"
		(1+ tmp-column) ; return one space after the "(*"
	      (current-column)
	      ) ; end if
	    ) ; end let in-comment

	(progn ; from now on, you're not in a comment
	  (setq in-unbalanced (mathematica-indent-determine-unbalanced))
	  (if in-unbalanced ; in-unbalanced = the opening paren
	      (progn
		(goto-char in-unbalanced)
		(if (= (char-after) start-close-paren)
		    (current-column)
		  (let ((tmp in-unbalanced)) 
		    (forward-char 1)
		    (skip-chars-forward " \t")
		    (if (looking-at "[\f\n]")
			(+ (mathematica-indent-determine-indent) 4)
		      (current-column)
		      ) ; end if unbalanced-paren ends the line
		    ) ; end let unbalanced-paren isn't immediately matched
		  ) ; end if immediate match
		) ; end progn unbalanced-paren
	    
	    (progn ; from now on, you're not in a comment or
		   ; unbalanced paren (you're at toplevel)
	      (setq prevline (mathematica-indent-determine-prevline))
	      (if prevline
		  (progn ; prevline = end of the last line
		    (goto-char prevline)
		    (if (= (char-before) ?\;)
			0 ; a fully top-level command
		      4 ; a continuation of a toplevel command

		      ) ; end if last line ended in a semicolon
		    ) ; end progn there was a last line
		0 ; if there's no previous line (in this execution
		  ; block) don't indent
		) ; end prevline if-else
	      ) ; end at toplevel progn

	    ) ; end unbalanced if-else
	  ) ; end non-comment progn
	) ; end in-comment if-else
      ) ; end outermost let
    ) ; end excursion
  )

(defun mathematica-indent-line ()
  "Indent current line as Mathematica code."
  (interactive)
  (let ((indent (mathematica-indent-calculate (point))) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	nil
      (progn
	(delete-region beg (point))
	(indent-to indent)
	) ; end of progn
      ) ; end if there is nothing to shift
  
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))
      nil
      ) ; end if we need to move the cursor
    ) ; end of let
  )

(defun mathematica-electric-paren (arg)
  "Indents on closing a paren."
  (interactive "p")
  (let ((start (point)))
    (if (not arg) (setq arg 1) nil)
    (let ((i 0)) (while (< i arg) (insert ")") (setq i (1+ i))))
    (save-excursion
      (goto-char start)
      (skip-chars-backward " \t")
      (if (= (current-column) 0)
	  (mathematica-indent-line)
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )

(defun mathematica-electric-braket (arg)
  "Indents on closing a braket."
  (interactive "p")
  (let ((start (point)))
    (if (not arg) (setq arg 1) nil)
    (let ((i 0)) (while (< i arg) (insert "]") (setq i (1+ i))))
    (save-excursion
      (goto-char start)
      (skip-chars-backward " \t")
      (if (= (current-column) 0)
	  (mathematica-indent-line)
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )

(defun mathematica-electric-brace (arg)
  "Indents on closing a brace."
  (interactive "p")
  (let ((start (point)))
    (if (not arg) (setq arg 1) nil)
    (let ((i 0)) (while (< i arg) (insert "}") (setq i (1+ i))))
    (save-excursion
      (goto-char start)
      (skip-chars-backward " \t")
      (if (= (current-column) 0)
	  (mathematica-indent-line)
	nil
	) ; end if
      ) ; end excursion
    ) ; end let
  (blink-matching-open)
  )

(defun mathematica-mode-copying ()
  "Get a buffer describing the copyright."
  (interactive)
  
  (let ((buffer (get-buffer-create "COPYING")))
    (set-buffer buffer)

    (insert "mathematical.el, A Mathematica interface through GNU Emacs
Copyright (C) 2002  Jim Pivarski

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

===============================================================

Mathematica is (C) Copyright 1988-1999 Wolfram Research, Inc.

Protected by copyright law and international treaties.

Unauthorized reproduction or distribution subject to severe civil
and criminal penalties.

Mathematica is a registered trademark of Wolfram Research.
")
    (setq buffer-read-only t)
    (switch-to-buffer buffer)

    ) ; end let

  )

(defun mathematica-mode-no-warantee ()
  "Get a buffer describing the lack of warantee."
  (interactive)

  (mathematica-mode-warantee)
  )

(defun mathematica-mode-warantee ()
  "Get a buffer describing the lack of warantee."
  (interactive)
  
  (let ((buffer (get-buffer-create "NO-WARANTEE")) (pos))
    (set-buffer buffer)

    (insert "GNU GENERAL PUBLIC LICENSE

Preamble

The licenses for most software are designed to take away your freedom to
share and change it. By contrast, the GNU General Public License is intended
to guarantee your freedom to share and change free software--to make sure
the software is free for all its users. This General Public License applies
to most of the Free Software Foundation's software and to any other program
whose authors commit to using it. (Some other Free Software Foundation
software is covered by the GNU Library General Public License instead.) You
can apply it to your programs, too.

When we speak of free software, we are referring to freedom, not price. Our
General Public Licenses are designed to make sure that you have the freedom
to distribute copies of free software (and charge for this service if you
wish), that you receive source code or can get it if you want it, that you
can change the software or use pieces of it in new free programs; and that
you know you can do these things.

To protect your rights, we need to make restrictions that forbid anyone to
deny you these rights or to ask you to surrender the rights. These
restrictions translate to certain responsibilities for you if you distribute
copies of the software, or if you modify it.

For example, if you distribute copies of such a program, whether gratis or
for a fee, you must give the recipients all the rights that you have. You
must make sure that they, too, receive or can get the source code. And you
must show them these terms so they know their rights.

We protect your rights with two steps: (1) copyright the software, and (2)
offer you this license which gives you legal permission to copy, distribute
and/or modify the software.

Also, for each author's protection and ours, we want to make certain that
everyone understands that there is no warranty for this free software. If
the software is modified by someone else and passed on, we want its
recipients to know that what they have is not the original, so that any
problems introduced by others will not reflect on the original authors'
reputations.

Finally, any free program is threatened constantly by software patents. We
wish to avoid the danger that redistributors of a free program will
individually obtain patent licenses, in effect making the program
proprietary. To prevent this, we have made it clear that any patent must be
licensed for everyone's free use or not licensed at all.

The precise terms and conditions for copying, distribution and modification
follow.

TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

0. This License applies to any program or other work which contains a notice
placed by the copyright holder saying it may be distributed under the terms
of this General Public License. The \"Program\", below, refers to any such
program or work, and a \"work based on the Program\" means either the Program
or any derivative work under copyright law: that is to say, a work
containing the Program or a portion of it, either verbatim or with
modifications and/or translated into another language. (Hereinafter,
translation is included without limitation in the term \"modification\".) Each
licensee is addressed as \"you\".

Activities other than copying, distribution and modification are not covered
by this License; they are outside its scope. The act of running the Program
is not restricted, and the output from the Program is covered only if its
contents constitute a work based on the Program (independent of having been
made by running the Program). Whether that is true depends on what the
Program does.

1. You may copy and distribute verbatim copies of the Program's source code
as you receive it, in any medium, provided that you conspicuously and
appropriately publish on each copy an appropriate copyright notice and
disclaimer of warranty; keep intact all the notices that refer to this
License and to the absence of any warranty; and give any other recipients of
the Program a copy of this License along with the Program.

You may charge a fee for the physical act of transferring a copy, and you
may at your option offer warranty protection in exchange for a fee.

2. You may modify your copy or copies of the Program or any portion of it,
thus forming a work based on the Program, and copy and distribute such
modifications or work under the terms of Section 1 above, provided that you
also meet all of these conditions:

   * a) You must cause the modified files to carry prominent notices stating
     that you changed the files and the date of any change.

   * b) You must cause any work that you distribute or publish, that in
     whole or in part contains or is derived from the Program or any part
     thereof, to be licensed as a whole at no charge to all third parties
     under the terms of this License.

   * c) If the modified program normally reads commands interactively when
     run, you must cause it, when started running for such interactive use
     in the most ordinary way, to print or display an announcement including
     an appropriate copyright notice and a notice that there is no warranty
     (or else, saying that you provide a warranty) and that users may
     redistribute the program under these conditions, and telling the user
     how to view a copy of this License. (Exception: if the Program itself
     is interactive but does not normally print such an announcement, your
     work based on the Program is not required to print an announcement.)

These requirements apply to the modified work as a whole. If identifiable
sections of that work are not derived from the Program, and can be
reasonably considered independent and separate works in themselves, then
this License, and its terms, do not apply to those sections when you
distribute them as separate works. But when you distribute the same sections
as part of a whole which is a work based on the Program, the distribution of
the whole must be on the terms of this License, whose permissions for other
licensees extend to the entire whole, and thus to each and every part
regardless of who wrote it.

Thus, it is not the intent of this section to claim rights or contest your
rights to work written entirely by you; rather, the intent is to exercise
the right to control the distribution of derivative or collective works
based on the Program.

In addition, mere aggregation of another work not based on the Program with
the Program (or with a work based on the Program) on a volume of a storage
or distribution medium does not bring the other work under the scope of this
License.

3. You may copy and distribute the Program (or a work based on it, under
Section 2) in object code or executable form under the terms of Sections 1
and 2 above provided that you also do one of the following:

   * a) Accompany it with the complete corresponding machine-readable source
     code, which must be distributed under the terms of Sections 1 and 2
     above on a medium customarily used for software interchange; or,

   * b) Accompany it with a written offer, valid for at least three years,
     to give any third party, for a charge no more than your cost of
     physically performing source distribution, a complete machine-readable
     copy of the corresponding source code, to be distributed under the
     terms of Sections 1 and 2 above on a medium customarily used for
     software interchange; or,

   * c) Accompany it with the information you received as to the offer to
     distribute corresponding source code. (This alternative is allowed only
     for noncommercial distribution and only if you received the program in
     object code or executable form with such an offer, in accord with
     Subsection b above.)

The source code for a work means the preferred form of the work for making
modifications to it. For an executable work, complete source code means all
the source code for all modules it contains, plus any associated interface
definition files, plus the scripts used to control compilation and
installation of the executable. However, as a special exception, the source
code distributed need not include anything that is normally distributed (in
either source or binary form) with the major components (compiler, kernel,
and so on) of the operating system on which the executable runs, unless that
component itself accompanies the executable.

If distribution of executable or object code is made by offering access to
copy from a designated place, then offering equivalent access to copy the
source code from the same place counts as distribution of the source code,
even though third parties are not compelled to copy the source along with
the object code.

4. You may not copy, modify, sublicense, or distribute the Program except as
expressly provided under this License. Any attempt otherwise to copy,
modify, sublicense or distribute the Program is void, and will automatically
terminate your rights under this License. However, parties who have received
copies, or rights, from you under this License will not have their licenses
terminated so long as such parties remain in full compliance.

5. You are not required to accept this License, since you have not signed
it. However, nothing else grants you permission to modify or distribute the
Program or its derivative works. These actions are prohibited by law if you
do not accept this License. Therefore, by modifying or distributing the
Program (or any work based on the Program), you indicate your acceptance of
this License to do so, and all its terms and conditions for copying,
distributing or modifying the Program or works based on it.

6. Each time you redistribute the Program (or any work based on the
Program), the recipient automatically receives a license from the original
licensor to copy, distribute or modify the Program subject to these terms
and conditions. You may not impose any further restrictions on the
recipients' exercise of the rights granted herein. You are not responsible
for enforcing compliance by third parties to this License.

7. If, as a consequence of a court judgment or allegation of patent
infringement or for any other reason (not limited to patent issues),
conditions are imposed on you (whether by court order, agreement or
otherwise) that contradict the conditions of this License, they do not
excuse you from the conditions of this License. If you cannot distribute so
as to satisfy simultaneously your obligations under this License and any
other pertinent obligations, then as a consequence you may not distribute
the Program at all. For example, if a patent license would not permit
royalty-free redistribution of the Program by all those who receive copies
directly or indirectly through you, then the only way you could satisfy both
it and this License would be to refrain entirely from distribution of the
Program.

If any portion of this section is held invalid or unenforceable under any
particular circumstance, the balance of the section is intended to apply and
the section as a whole is intended to apply in other circumstances.

It is not the purpose of this section to induce you to infringe any patents
or other property right claims or to contest validity of any such claims;
this section has the sole purpose of protecting the integrity of the free
software distribution system, which is implemented by public license
practices. Many people have made generous contributions to the wide range of
software distributed through that system in reliance on consistent
application of that system; it is up to the author/donor to decide if he or
she is willing to distribute software through any other system and a
licensee cannot impose that choice.

This section is intended to make thoroughly clear what is believed to be a
consequence of the rest of this License.

8. If the distribution and/or use of the Program is restricted in certain
countries either by patents or by copyrighted interfaces, the original
copyright holder who places the Program under this License may add an
explicit geographical distribution limitation excluding those countries, so
that distribution is permitted only in or among countries not thus excluded.
In such case, this License incorporates the limitation as if written in the
body of this License.

9. The Free Software Foundation may publish revised and/or new versions of
the General Public License from time to time. Such new versions will be
similar in spirit to the present version, but may differ in detail to
address new problems or concerns.

Each version is given a distinguishing version number. If the Program
specifies a version number of this License which applies to it and \"any
later version\", you have the option of following the terms and conditions
either of that version or of any later version published by the Free
Software Foundation. If the Program does not specify a version number of
this License, you may choose any version ever published by the Free Software
Foundation.

10. If you wish to incorporate parts of the Program into other free programs
whose distribution conditions are different, write to the author to ask for
permission. For software which is copyrighted by the Free Software
Foundation, write to the Free Software Foundation; we sometimes make
exceptions for this. Our decision will be guided by the two goals of
preserving the free status of all derivatives of our free software and of
promoting the sharing and reuse of software generally.

")
    (setq pos (point))
    (insert "NO WARRANTY

11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR
THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO
THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM
PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
CORRECTION.

12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING
OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO
LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR
THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER
PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGES.

===============================================================

mathematical.el, A Mathematica interface through GNU Emacs
Copyright (C) 2002  Jim Pivarski

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

===============================================================

Mathematica is (C) Copyright 1988-1999 Wolfram Research, Inc.

Protected by copyright law and international treaties.

Unauthorized reproduction or distribution subject to severe civil
and criminal penalties.

Mathematica is a registered trademark of Wolfram Research.
")
    (goto-char pos)

    (setq buffer-read-only t)
    (switch-to-buffer buffer)

    (recenter 1)

    ) ; end let

  )

