;;; multiple-cursors-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (mc/edit-beginnings-of-lines mc/edit-ends-of-lines
;;;;;;  mc/edit-lines) "mc-edit-lines" "mc-edit-lines.el" (21507
;;;;;;  302))
;;; Generated autoloads from mc-edit-lines.el

(autoload 'mc/edit-lines "mc-edit-lines" "\
Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

What is done with lines which are not long enough is governed by
`mc/edit-lines-empty-lines'.  The prefix argument ARG can be used
to override this.  If ARG is a symbol (when called from Lisp),
that symbol is used instead of `mc/edit-lines-empty-lines'.
Otherwise, if ARG negative, short lines will be ignored.  Any
other non-nil value will cause short lines to be padded.

\(fn &optional ARG)" t nil)

(autoload 'mc/edit-ends-of-lines "mc-edit-lines" "\
Add one cursor to the end of each line in the active region.

\(fn)" t nil)

(autoload 'mc/edit-beginnings-of-lines "mc-edit-lines" "\
Add one cursor to the beginning of each line in the active region.

\(fn)" t nil)

;;;***

;;;### (autoloads (mc/mark-sgml-tag-pair mc/add-cursor-on-click mc/mark-all-symbols-like-this-in-defun
;;;;;;  mc/mark-all-words-like-this-in-defun mc/mark-all-like-this-in-defun
;;;;;;  mc/mark-all-dwim mc/mark-all-like-this-dwim mc/mark-more-like-this-extended
;;;;;;  mc/mark-all-in-region-regexp mc/mark-all-in-region mc/mark-all-symbols-like-this
;;;;;;  mc/mark-all-words-like-this mc/mark-all-like-this mc/skip-to-previous-like-this
;;;;;;  mc/skip-to-next-like-this mc/unmark-previous-like-this mc/unmark-next-like-this
;;;;;;  mc/mark-previous-lines mc/mark-next-lines mc/mark-previous-symbol-like-this
;;;;;;  mc/mark-previous-word-like-this mc/mark-previous-like-this
;;;;;;  mc/mark-next-symbol-like-this mc/mark-next-word-like-this
;;;;;;  mc/mark-next-like-this) "mc-mark-more" "mc-mark-more.el"
;;;;;;  (21507 302))
;;; Generated autoloads from mc-mark-more.el

(autoload 'mc/mark-next-like-this "mc-mark-more" "\
Find and mark the next part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

\(fn ARG)" t nil)

(autoload 'mc/mark-next-word-like-this "mc-mark-more" "\
Not documented

\(fn ARG)" t nil)

(autoload 'mc/mark-next-symbol-like-this "mc-mark-more" "\
Not documented

\(fn ARG)" t nil)

(autoload 'mc/mark-previous-like-this "mc-mark-more" "\
Find and mark the previous part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

\(fn ARG)" t nil)

(autoload 'mc/mark-previous-word-like-this "mc-mark-more" "\
Not documented

\(fn ARG)" t nil)

(autoload 'mc/mark-previous-symbol-like-this "mc-mark-more" "\
Not documented

\(fn ARG)" t nil)

(autoload 'mc/mark-next-lines "mc-mark-more" "\
Not documented

\(fn ARG)" t nil)

(autoload 'mc/mark-previous-lines "mc-mark-more" "\
Not documented

\(fn ARG)" t nil)

(autoload 'mc/unmark-next-like-this "mc-mark-more" "\
Deselect next part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/unmark-previous-like-this "mc-mark-more" "\
Deselect prev part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/skip-to-next-like-this "mc-mark-more" "\
Skip the current one and select the next part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/skip-to-previous-like-this "mc-mark-more" "\
Skip the current one and select the prev part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/mark-all-like-this "mc-mark-more" "\
Find and mark all the parts of the buffer matching the currently active region

\(fn)" t nil)

(autoload 'mc/mark-all-words-like-this "mc-mark-more" "\
Not documented

\(fn)" t nil)

(autoload 'mc/mark-all-symbols-like-this "mc-mark-more" "\
Not documented

\(fn)" t nil)

(autoload 'mc/mark-all-in-region "mc-mark-more" "\
Find and mark all the parts in the region matching the given search

\(fn BEG END)" t nil)

(autoload 'mc/mark-all-in-region-regexp "mc-mark-more" "\
Find and mark all the parts in the region matching the given regexp

\(fn BEG END)" t nil)

(autoload 'mc/mark-more-like-this-extended "mc-mark-more" "\
Like mark-more-like-this, but then lets you adjust with arrows key.
The adjustments work like this:

   <up>    Mark previous like this and set direction to 'up
   <down>  Mark next like this and set direction to 'down

If direction is 'up:

   <left>  Skip past the cursor furthest up
   <right> Remove the cursor furthest up

If direction is 'down:

   <left>  Remove the cursor furthest down
   <right> Skip past the cursor furthest down

The bindings for these commands can be changed. See `mc/mark-more-like-this-extended-keymap'.

\(fn)" t nil)

(autoload 'mc/mark-all-like-this-dwim "mc-mark-more" "\
Tries to guess what you want to mark all of.
Can be pressed multiple times to increase selection.

With prefix, it behaves the same as original `mc/mark-all-like-this'

\(fn ARG)" t nil)

(autoload 'mc/mark-all-dwim "mc-mark-more" "\
Tries even harder to guess what you want to mark all of.

If the region is active and spans multiple lines, it will behave
as if `mc/mark-all-in-region'. With the prefix ARG, it will call
`mc/edit-lines' instead.

If the region is inactive or on a single line, it will behave like 
`mc/mark-all-like-this-dwim'.

\(fn ARG)" t nil)

(autoload 'mc/mark-all-like-this-in-defun "mc-mark-more" "\
Mark all like this in defun.

\(fn)" t nil)

(autoload 'mc/mark-all-words-like-this-in-defun "mc-mark-more" "\
Mark all words like this in defun.

\(fn)" t nil)

(autoload 'mc/mark-all-symbols-like-this-in-defun "mc-mark-more" "\
Mark all symbols like this in defun.

\(fn)" t nil)

(autoload 'mc/add-cursor-on-click "mc-mark-more" "\
Add a cursor where you click.

\(fn EVENT)" t nil)

(autoload 'mc/mark-sgml-tag-pair "mc-mark-more" "\
Mark the tag we're in and its pair for renaming.

\(fn)" t nil)

;;;***

;;;### (autoloads (mc/mark-pop) "mc-mark-pop" "mc-mark-pop.el" (21507
;;;;;;  302))
;;; Generated autoloads from mc-mark-pop.el

(autoload 'mc/mark-pop "mc-mark-pop" "\
Add a cursor at the current point, pop off mark ring and jump
to the popped mark.

\(fn)" t nil)

;;;***

;;;### (autoloads (mc/sort-regions mc/reverse-regions mc/insert-numbers)
;;;;;;  "mc-separate-operations" "mc-separate-operations.el" (21507
;;;;;;  302))
;;; Generated autoloads from mc-separate-operations.el

(autoload 'mc/insert-numbers "mc-separate-operations" "\
Insert increasing numbers for each cursor, starting at 0 or ARG.

\(fn ARG)" t nil)

(autoload 'mc/reverse-regions "mc-separate-operations" "\
Not documented

\(fn)" t nil)

(autoload 'mc/sort-regions "mc-separate-operations" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (set-rectangular-region-anchor) "rectangular-region-mode"
;;;;;;  "rectangular-region-mode.el" (21507 302))
;;; Generated autoloads from rectangular-region-mode.el

(autoload 'set-rectangular-region-anchor "rectangular-region-mode" "\
Anchors the rectangular region at point.

Think of this one as `set-mark' except you're marking a rectangular region. It is
an exceedingly quick way of adding multiple cursors to multiple lines.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("mc-cycle-cursors.el" "mc-hide-unmatched-lines-mode.el"
;;;;;;  "multiple-cursors-core.el" "multiple-cursors-pkg.el" "multiple-cursors.el")
;;;;;;  (21507 302 559497))

;;;***

(provide 'multiple-cursors-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multiple-cursors-autoloads.el ends here
