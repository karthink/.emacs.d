INTRODUCTION:

    Quack enhances Emacs support for Scheme programming.

    Install Quack rather than following non-Quack-based tutorials on how to
    set up Emacs for Scheme.

    The name "Quack" was a play on "DrScheme".

    Quack is dedicated to Yosh, naturally.

COMPATIBILITY:

    GNU Emacs 23 and 22 -- Yes.  Quack is now developed under GNU Emacs 23
    on a GNU/Linux system, which is the preferred platform for Quacksmokers.
    Quack should work under GNU Emacs 23 on any Un*x-like OS.  Reportedly,
    Quack also works with GNU Emacs 22 on Apple Mac OS X and Microsoft
    Windows (NT, 2000, XP), but the author has no means of testing on those
    platforms.

    GNU Emacs 21 -- Probably, but no longer tested.

    GNU Emacs 20 -- Probably mostly.  When last tested. Some of the menus do
    not work properly, due to a bug in easymenu.el (which the FSF will not
    fix, since they no longer support Emacs 20).  Nested block comments are
    not fontified correctly.  Pretty-lambda does not work.  Quack runs less
    efficiently in 20 than 21, due to the lack of standard hash tables.

    XEmacs 21 -- Probably mostly, but no longer tested.  Block comment
    fontification is not yet supported under XEmacs 21, due to what appears
    to be a bug in 21.4 font-lock.  Pretty-lambda does not work.  XEmacs
    Quacksmokers who always want the latest and greatest Quack should
    consider GNU Emacs 21 -- Quack treats XEmacs like a high-maintenance
    redheaded stepchild.

INSTALLATION:

    To install, put this file (`quack.el') somewhere in your Emacs load
    path, and add the following line to your `.emacs' file:

        (require 'quack)

    If you don't know what your Emacs load path is, try invoking the command
    "C-h v load-path RET" or consulting the Emacs manual.

    Note to advanced Emacsers: Byte-compiled `quack.elc' files generally are
    *not* portable between Emacs implementations, nor between different
    versions of the same implementation.

    You will also need the GNU `wget' program, which Quack uses for
    downloading SRFI indexes.  This popular program is included in most
    GNU/Linux distributions and is available for most other platforms.

    Note to PLT Scheme users: If you do not already have the PLT manuals
    installed, they can be downloaded from
    `http://download.plt-scheme.org/doc/' and installed in your PLT `doc'
    collection.  If Quack is not finding installed PLT manuals, then be sure
    that the `quack-pltcollect-dirs' variable contains the appropriate
    collection directory (if it does not, then either set the `PLTHOME'
    and/or `PLTCOLLECTS' environment variables appropriately, or set
    `quack-pltcollect-dirs').

KEY BINDINGS:

    The key bindings that Quack adds to `scheme-mode' include:

        C-c C-q m   View a manual in your Web browser.
        C-c C-q k   View the manual documentation for a keyword
                    (currently only works for PLT manuals).
        C-c C-q s   View an SRFI.
        C-c C-q r   Run an inferior Scheme process.
        C-c C-q f   Find a file using context of point for default.
        C-c C-q l   Toggle `lambda' syntax of `define'-like form.
        C-c C-q t   Tidy the formatting of the buffer.

    One additional command that does not currently have a standard binding
    is `quack-dired-pltcollect', which prompts for a PLT collection name and
    creates a Dired buffer on the collection's directory.  (A future version
    of Quack may integrate this functionality into a more generalized
    documentation navigation interface.)

RELEASE ANNOUNCEMENTS EMAIL:

    To receive email notification when a new Quack version is released, ask
    neil@neilvandyke.org to add you to the moderated `scheme-announce' list.

HISTORY:

    Version 0.47 (2012-11-15):
        * Added indent for `call-with-' file variants and semaphore.
        * Added font and indent for `with-handlers*', `define-runtime-path',
          `match-let'.

    Version 0.46 (2012-06-20):
        * Added indent for `letrec-values'.
        * Corrected date on history for version 0.45.

    Version 0.45 (2012-06-18):
        * Added a bunch of indent rules for Scribble definition forms
          and Racket sequence/iterator stuff, plus Overeasy `test-section'.

    Version 0.44 (2012-04-11):
        * Added indent and fontify for `struct', `module+', `module*'.
        * Changed intent for `module' from `defun' to 2.
        * Added fontify for `define-syntax-class',
          `define-splicing-syntax-class', `begin-for-syntax'.
        * Changed `define-struct' fontify.

    Version 0.43 (2011-08-23):
        * Add indent and fontify for "syntax-parse".
        * Added another compile error regexp for Racket backtraces.

    Version 0.42 (2011-07-30):
        * Added compile error regexp for "raco".

    Version 0.41 (2011-06-04)
        * Added `sxml-match' to `scheme-indent-function'.

    Version 0.40 (2010-12-22)
        * Added indent rules for Racket `let:', `let*:', and `match'.  And
          a provisional rule for `define:'.

    Version 0.39 (2010-10-18)
        * Renamed "typed/scheme" to "typed/racket".

    Version 0.38 (2010-10-14)
        * Replaced old PLT Scheme programs in `quack-programs' with Racket.
        * Added Racket ".rkt" and ".rktd" filename extensions.
        * Added some Racket keywords for fontifying.

    Version 0.37 (2009-06-29)
        * Disabled highlighting of "Compilation started at" lines.

    Version 0.36 (2009-05-27)
        * Made `#:' ``colon keywords'' fontify in PLT-ish mode.
        * Added PLT `r6rs' and `typed-scheme' languages to `quack-programs'.

    Version 0.35 (2009-02-24)
        * Added `interpreter-mode-alist' support, so Scheme scripts with "#!"
          start in `scheme-mode'.
        * Added PLT `parameterize-break'.
        * Improved `compile' mode for PLT 4.x tracebacks when there is only
          file, line, and column, but no additional information.

    Version 0.34 (2009-02-19)
        * Added fontify and indent support for PLT `define/kw', `lambda/kw',
         `parameterize*'.
        * Fontify Unix "#!" cookie in PLT-ish font-lock.
        * Changed reference to `quack-announce' email list to
          `scheme-announce'.
        * Added PLT `default-load-handler' to
         `quack-compilation-error-regexp-alist-additions'
        * Changed some face ":height" attributes.

    Version 0.33 (2008-07-31)
        * Added handlers for some PLT 4.0.1 "setup-plt" messages.

    Version 0.32 (2008-06-19)
        * Added to `quack-programs'.
        * Updated compatibility comments.
        * Added indent rule for `for/fold'.

    Version 0.31 (2008-05-03)
        * Added `defvar' for `quack-pltish-font-lock-keywords', so that the
          GNU Emacs 22.1 compiler doesn't complain about assignment to a free
          variable.
        * Changed banner regexp for MzScheme for v3.99.x.
        * Set `dynamic-wind' `scheme-indent-function to 0, when the default
          is 3.  It was just taking up too much space.  DrScheme's
          indentation seems to be equivalent -1, so there is precedent for
          something different.  We generally respect Emacs indentation
          convention.
        * Added fontifying and indent for PLT `define-for-syntax',
          `define-values-for-syntax', `quasisyntax', `quasisyntax/loc',
          `syntax', `syntax/loc', `define-parameters'.
        * Advise `scheme-interactively-start-process' for GNU Emacs 22.
        * Removed TODO comment that mentioned using `(current-eventspace
          (make-eventspace))' under `mred', as Robby Findler has indicated
          that is not good advice.

    Version 0.30 (2007-06-27)
        * Emacs 22 compatibility change: `string-to-number' instead of
          `string-to-int'.  Thanks to Charles Comstock.

    Version 0.29 (2006-11-12)
        * Fixed `quack-bar-syntax-string', which caused vertical bar
          characters to be treated as whitespace.  Thanks to Eric Hanchrow
          for reporting.

    Version 0.28 (2005-05-14)
        * Added `quack-smart-open-paren-p'.
        * Changed `scheme-indent-function' for `parameterize' from `defun'
          to `1'.
        * In `quack-pltish-keywords-to-fontify': added `quasiquote',
          `unquote', and `unquote-splicing'.
        * Added ".mzschemerc" to `auto-mode-alist'.
        * Added a little extra threesemi fontification for Funcelit and
          similar Texinfo markup formats.

    Version 0.27 (2004-12-19)
        * For Gambit-C, added REPL banner fontifying, `quack-manuals' entry,
          and "gsi ~~/syntax-case.scm -" `quack-programs' entry.
        * Changed "[PLT]" prefix on PLT manuals to "PLT", to make it easier
          to type.
        * Minor changes to reflect "MIT Scheme" becoming "MIT/GNU Scheme".

    Version 0.26 (2004-07-14)
        * Added fontifying of a bunch of "define-"* syntax from Chicken.

    Version 0.25 (2004-07-09)
        * Added `define-record-type' to `quack-pltish-keywords-to-fontify'.
        * Added "csi -hygienic" to `quack-programs'.
        * In `quack-manuals', replaced PLT-specific `r5rs' and `t-y-scheme'
          with generic ones.
        * Updated URL in `quack-manuals' for 3rd ed. of `tspl'.
        * `quack-view-manual' completions no longer include symbols.
        * `quack-view-manual' completion default is now "R5RS".

    Version 0.24 (2004-05-09)
        * Made `quack-pltish-keywords-to-fontify' and
          `quack-emacs-keywords-to-fontify' custom changes update
          immediately.  Bug reported by Taylor Campbell.
        * Removed some non-syntax names from
          `quack-pltish-keywords-to-fontify'.
        * Documentation changes.

    Version 0.23 (2003-11-11)
        * `quack-local-keywords-for-remote-manuals-p' can now have the value
          of the symbol `always', to work around a defect in some versions
          of Microsoft Windows.  Thanks to Bill Clementson.
        * `quack-w3m-browse-url-other-window' no longer splits a `*w3m*'
          buffer.
        * Added indent and `quack-pltish-keywords-to-fontify' rules for
          `c-lambda' and `c-declare'.

    Version 0.22 (2003-07-03)
        * `quack-newline-behavior' controls the RET key behavior in Scheme
          buffers.
        * In `quack-manuals', added Chez Scheme, and updated Chicken.
        * Added error message navigation to `compile' for PLT `setup-plt'.
        * Partial fix for Quack global menu disappearing from the main menu
          bar in XEmacs.  Thought it used to work, but it doesn't in XEmacs
          21.4.12.

    Version 0.21 (2003-05-28)
        * `quack-find-file' is faster in many cases due to fix to
          `quack-backward-sexp'.
        * Added auto-mode-alist for `.ccl', `.stk', and `.stklos' files.
        * Indent rule additions/changes for `chicken-setup' and `unit/sig'.

    Version 0.20 (2003-05-04)
        * Added indent and fontify for SRFI-8 "receive".
        * Added indent and fontify for additional PLT syntax.
        * Added `quack-fontify-threesemi-p'.
        * `quack-tidy-buffer' sets `fill-prefix' to nil when running.
        * Added messages to `run-scheme', if only to get rid of annoying
          "Mark set" message.
        * Added "mzscheme -M errortrace" to `quack-programs'.
        * `quack-dired-pltcollect' prompt defaults to `mzlib'.
        * "Update SRFI Index" menu item has moved to top of menu, mainly to
          avoid usability issue in a particular Emacs menu implementation.
        * Several code quality improvements sent by Stefan Monnier will be
          in the next release.

    Version 0.19 (2003-03-04)
        * Commands such as `scheme-load-file' now start a Scheme process if
          none is found.
        * Bugfix for using `match-string-no-properties' when we meant
          `quack-match-string-no-properties'.  (Thanks to Noel Welsh.)

    Version 0.18 (2003-05-02)
        * Removed uses of `(regexp-opt LIST t)', since XEmacs21 does not
          create match data.  (Thanks to Garrett Mitchener for debugging.)
        * Added to `quack-programs' and `quack-manuals'.
        * Added pretty-case-lambda.
        * Changed PLT documentation URL function.

    Version 0.17 (2003-01-03)
        * Pretty-lambda is supported well under GNU Emacs 21, when using PLT
          Style fontification.  Enable via the Options menu.  (Based on
          approach by Stefan Monnier; suggested by Ray Racine.)
        * Various faces now have separate defaults for `light' and `dark'
          backgrounds, so may now look better on dark backgrounds.
          (Suggested by Eli Barzilay.)
        * `quack-find-file' now respects `insert-default-directory' when
          there is no default file.  (Thanks to Eli Barzilay.)
        * Most of the special w3m support has been moved to a separate
          package, `w3mnav' (`http://www.neilvandyke.org/w3mnav/').
          `quack-w3m-browse-url-other-window' has been added.

    Version 0.16 (2002-12-16)
        * `quack-insert-closing' now calls `blink-paren-function'.  (Thanks
          to Guillaume Marceau and Steve Elkins for reporting this.)
        * Now uses PLT 202 manuals.  Added "PLT Framework" manual.
        * Added `quack-pltish-module-defn-face'.
        * Added some PLTish font-lock keywords.

    Version 0.15 (2002-11-21)
        * "Keywords" are now fontified in PLT Style fontification mode.
        * Definition names are now blue by default in PLT Style.
        * Symbol literals with vertical bars are now fontified in PLT Style.
        * New `quack-manuals-webjump-sites' function for people who prefer
          to use the `webjump' package for invoking manuals.
        * New `quack-quiet-warnings-p' option.
        * New `quack-pltish-class-defn-face' face.

    Version 0.14 (2002-10-18)
        * Fix for `quack-view-manual' interactive prompting (thanks to Marko
          Slyz for reporting this).
        * `quack-emacsw3m-go-next' and `quack-emacsw3m-go-prev' now work
          with GTK reference documentation (not that this has anything to do
          with Scheme).
        * Added SLIB to `quack-manuals'.
        * Added comment about installing PLT manuals (thanks to Marko).
        * We now call the canonical version of Emacs "GNU Emacs," instead of
          "FSF Emacs".

    Version 0.13 (2002-09-21)
        * Bugfix: No longer drop SRFI index entries on the floor.

    Version 0.12 (2002-09-20)
        * New "View SRFI" menu.  Select "Update SRFI Index" if the submenus
          "Draft", "Final", and "Withdrawn" are disabled.
        * Most options are now settable via "Options" menu.
        * PLT collections are no longer scanned when building "View Manuals"
          menu.
        * "View Keyword Docs..." back on Scheme Mode menu in addition to
          Quack menu.
        * Various `defcustom' variables have been made to dynamically update
          relevant program state when changed.
        * Under GNU Emacs 20, dynamic menus still do not work -- they now
          display, but do not perform the selected action.  Will do more
          debugging after this release.
        * '[' and ']' keys work in emacs-w3m of MIT Scheme manuals.

    Version 0.11 (2002-09-17)
        * Menus now work under XEmacs.  Also now partly broken for Emacs 20.
        * New global "Quack" menu.  Disable with `quack-global-menu-p'.
        * New "View Manual" submenu under GNU Emacs 21 and XEmacs (GNU Emacs
          20 is stuck with the old "View Manual..." menu item).
        * Fix for `quack-pltcollects-alist' to include PLT `doc' collection,
          which was preventing local manuals from being used.
        * `quack-manuals' now includes `t-y-scheme'.
        * `quack-view-in-different-browser' command that spawns alternative
          Web browser from the special emacs-w3m support, bound to `B'.  For
          when you normally view manuals in an Emacs window, but
          occasionally want to view a particular page in normal Web browser.
        * More `scheme-indent-function' properties set.
        * `quack-about' command.
        * Fix to `quack-keyword-at-point'.

    Version 0.10 (2002-09-11)
        * `quack-view-srfi' now prompts with completion, including titles
          for all SRFIs.  The SRFI titles are fetched from the official SRFI
          Web site using the GNU Wget program, and cached locally.
        * `quack-view-srfi' also now defaults to the SRFI number at or near
          the point.
        * `quack-dir' variable specifies a directory where Quack should
          store its persistent data files (e.g., cached SRFI indexes), and
          defaults to "~/.quack/".
        * New `quack-tidy-buffer' command.  [C-c C-q t] is now bound to
          this; [C-c C-q l] ("l" as in "lambda) is now the official binding
          for `quack-toggle-lambda'.
        * `quack-find-file' now recognizes PLT `dynamic-require' form.
        * Fix to make `quack-looking-at-backward' preserve match data.
        * Fix for benign bug in `quack-parent-sexp-search'.

    Version 0.9 (2002-09-04)
        * Quack now works under XEmacs 21, except no menus are currently
          defined (that will come in a later version) and block comments
          aren't fontified.
        * `quack-toggle-lambda' command toggles a `define' form between
          explicit and implicit `lambda' syntax.
        * `quack-dired-pltcollect' feature prompts for a PLT collection name
          and creates a Dired on the collection.
        * `)' and `]' keys are bound to insert a closing character that
          agrees with the opening character of the sexp.
        * Nested `#|' comment blocks are now fontified mostly correctly
          under GNU Emacs 21.
        * Fix to `quack-parent-sexp-search'.
        * Fix for PLT manual keywords lookup under Emacs 20.
        * `quack-manuals' URLs for assorted implementation manuals now point
          to canonical Web copies.
        * No longer warns about PLT manual keywords file found without HTML.
        * `find-file' key bindings are automatically remapped to
          `quack-find-file' in Scheme buffers.
        * Both PLT-style and Emacs-style fontification now work with the
          `noweb-mode' package.  Tested under GNU Emacs 21 with
          Debian `nowebm' package version 2.10c-1.
        * Added to `quack-emacsish-keywords-to-fontify'.
        * Disabled fontification of named `let'.
        * Renamed "collect" in PLT identifiers to "pltcollect".
        * `auto-mode-alist' set more aggressively.

    Version 0.8 (2002-08-25)
        * PLT package file viewing mode.  This is mainly used to easily
          inspect a ".plt" package before installing it via DrScheme or
          "setup-plt".
        * No longer warns about `font-lock-keywords' when `noweb-mode'
          package is installed.

    Version 0.7 (2002-08-22)
        * Now works on GNU Emacs 20 (though people are still encouraged to
          upgrade to GNU Emacs 21 if they are able).
        * `quack-manuals' now includes MIT Scheme and Chicken manuals
          (currently where Debian GNU/Linux puts them).
        * `quack-view-srfi' command.
        * Named-`let' name is fontified like a PLTish definition name.
        * `define-record' and `define-opt' fontified.
        * Scheme Mode is forced in `auto-mode-alist' for ".sch" files.
        * Fix to `quack-backward-sexp'.
        * `quack-warning' messages get your attention.
        * `quack-pltrequire-at-point-data-1' search depth limited.

    Version 0.6 (2002-08-20)
        * `quack-find-file' now supports multi-line PLT `require' forms.
        * When `emacs-w3m' is used, the keys "[", "]", and "t" are bound to
          navigate through PLT manuals like in Info mode.
        * Names highlighted in PLT-style fontification of `defmacro',
          `defmacro-public', `defsyntax'.
        * Advised `run-scheme' no longer prompts when there is already a
          running Scheme.
        * "csi" (Chicken interpreter) added to `quack-programs' default.
        * Forces `auto-mode-alist' for ".scm" files to `scheme-mode'
          (two can play at that game, `bee-mode'!).
        * To-do comments moved from the top of the file to throughout code.

    Version 0.5 (2002-08-15)
        * New `quack-find-file' permits quick navigation to files indicated
          by a PLT Scheme `require' form under the point.  Currently only
          works when the "(require" string is on the same line as point.
        * Improved PLT-style fontification.  Most noticeable difference is
          that names in many definition forms are boldfaced.  See
          `quack-pltish-fontify-definition-names-p' option.
        * `quack-collects-alist' added.
        * "~/plt/" has been removed from `quack-collect-dirs' default.
        * Unnecessary syntax table settings have been removed.
        * Reduced memory usage in some cases, via explicit GC calls.

    Version 0.4 (2002-08-07)
        * Functionality adapted from author's `giguile.el' package:
            - Enhanced `run-scheme' behavior.  `quack-run-mzscheme',
              `quack-run-mred', and `quack-remove-run-scheme-menu-item-p'
              are obsolete.
            - Enhanced `switch-to-scheme' behavior.
            - Options menu.
            - Indent rules for a few Guile-isms.
        * Inferior Scheme Mode now uses the preferred fontification method.
        * Now uses the PLT-bundled version of R5RS manual, which permits
          keyword searching.
        * `quack-banner-face' for the MzScheme/MrEd banner in REPL buffer.
        * This code includes a start on toolbars and XEmacs21 portability,
          but neither feature is yet functional.

    Version 0.3 (2002-08-01)
        * PLT-style fontification added, except for quoted lists.  Emacs-
          style fontification still available; see `quack-fontify-style'.
        * `emacs-w3m' package support for lightweight viewing of PLT manuals
          in Emacs window.  If you install the `emacs-w3m' package, then you
          can change the new `quack-browse-url-browser-function' option to
          use it.
        * Quack menu items added to Scheme Mode menu.  "Run Scheme" item
          is removed by default; see `quack-remove-run-scheme-menu-item-p'.
        * MrEd REPL supported with `quack-run-mred'.
        * Better default for `quack-collect-dirs'.
        * More `scheme-indent-function' settings.
        * Bugfix for `quack-prompt-for-kwmatch-choice'.
        * Bugfix for font-lock keywords getting set too early.
        * Now byte-compiles without warnings/errors.

    Version 0.2 (2002-07-28)
        * Manual keywords lookup.
        * Other minor changes.

    Version 0.1 (2002-07-18)
        * Initial release.

ADMONISHMENT TO IMPRESSIONABLE YOUNG SCHEME STUDENTS:

    Quack should by no means be construed as a model of good programming,
    much less of good software engineering.  Emacs is by nature a complex
    system of interacting kludges.  To get Emacs to do useful new things is
    to artfully weave one's extensions into a rich tapestry of sticky duct
    tape.  Also, Quack usually only got hacked on when I was stuck in a busy
    lobby for an hour with a laptop and unable to do real work.
