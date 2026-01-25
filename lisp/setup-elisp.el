;;; setup-elisp.el --- elisp editing and debugging setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package pp
  :bind (([remap eval-last-sexp] . eval-sexp-maybe-pp)
         ([remap eval-expression] . my/pp-eval-expression))
  ;; :hook (eval-expression-minibuffer-setup . my/eval-with-threading)
  :config
  (defun my/eval-with-threading ()
    "Pre-insert a threading macro for easy chaining"
    (insert "(thread-first )")
    (backward-char 1))
  
  (defun eval-sexp-maybe-pp (&optional arg)
    (interactive "P")
    (if arg
        (let ((current-prefix-arg '4))
          (call-interactively #'pp-eval-last-sexp))
      (call-interactively #'eval-last-sexp)))
  
  (defun my/pp-eval-expression (&optional insert-p)
    "Call `pp-eval-expression' on EXPR. With prefix-arg INSERT-P,
call `eval-expression' instead and insert the result into the
current buffer without truncation."
    (interactive "P")
    (if insert-p
        (let ((current-prefix-arg current-prefix-arg))
          (call-interactively #'eval-expression))
      (call-interactively #'pp-eval-expression)))
  
  (advice-add 'pp-display-expression :after
              (defun my/pp-handle-output-buffer (&rest args)
                (when-let* ((win (get-buffer-window (nth 1 args)))
                            (_ (window-live-p win)))
                  (select-window win)
                  (view-mode 1)))))

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c C-m" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c C-m" . macrostep-expand)))

(use-package elisp-mode
  :disabled
  :defer t
  :config
  ;; From https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
  (advice-add #'calculate-lisp-indent :override #'void~calculate-lisp-indent)
  (defun void~calculate-lisp-indent (&optional parse-start)
    "Add better indentation for quoted and backquoted lists."
    ;; This line because `calculate-lisp-indent-last-sexp` was defined with
    ;; `defvar` with its value ommited, marking it special and only defining it
    ;; locally. So if you don't have this, you'll get a void variable error.
    (defvar calculate-lisp-indent-last-sexp)
    (save-excursion
      (beginning-of-line)
      (let ((indent-point (point))
            state
            ;; setting this to a number inhibits calling hook
            (desired-indent nil)
            (retry t)
            calculate-lisp-indent-last-sexp containing-sexp)
        (cond ((or (markerp parse-start) (integerp parse-start))
               (goto-char parse-start))
              ((null parse-start) (beginning-of-defun))
              (t (setq state parse-start)))
        (unless state
          ;; Find outermost containing sexp
          (while (< (point) indent-point)
            (setq state (parse-partial-sexp (point) indent-point 0))))
        ;; Find innermost containing sexp
        (while (and retry
                    state
                    (> (elt state 0) 0))
          (setq retry nil)
          (setq calculate-lisp-indent-last-sexp (elt state 2))
          (setq containing-sexp (elt state 1))
          ;; Position following last unclosed open.
          (goto-char (1+ containing-sexp))
          ;; Is there a complete sexp since then?
          (if (and calculate-lisp-indent-last-sexp
                   (> calculate-lisp-indent-last-sexp (point)))
              ;; Yes, but is there a containing sexp after that?
              (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                              indent-point 0)))
                (if (setq retry (car (cdr peek))) (setq state peek)))))
        (if retry
            nil
          ;; Innermost containing sexp found
          (goto-char (1+ containing-sexp))
          (if (not calculate-lisp-indent-last-sexp)
              ;; indent-point immediately follows open paren.
              ;; Don't call hook.
              (setq desired-indent (current-column))
            ;; Find the start of first element of containing sexp.
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
            (cond ((looking-at "\\s(")
                   ;; First element of containing sexp is a list.
                   ;; Indent under that list.
                   )
                  ((> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp)
                   ;; This is the first line to start within the containing sexp.
                   ;; It's almost certainly a function call.
                   (if (or
                        ;; Containing sexp has nothing before this line
                        ;; except the first element. Indent under that element.
                        (= (point) calculate-lisp-indent-last-sexp)

                        ;; First sexp after `containing-sexp' is a keyword. This
                        ;; condition is more debatable. It's so that I can have
                        ;; unquoted plists in macros. It assumes that you won't
                        ;; make a function whose name is a keyword.
                        (when-let (char-after (char-after (1+ containing-sexp)))
                          (char-equal char-after ?:))

                        ;; Check for quotes or backquotes around.
                        (let* ((positions (elt state 9))
                               (last (car (last positions)))
                               (rest (reverse (butlast positions)))
                               (any-quoted-p nil)
                               (point nil))
                          (or
                           (when-let (char (char-before last))
                             (or (char-equal char ?')
                                 (char-equal char ?`)))
                           (progn
                             (while (and rest (not any-quoted-p))
                               (setq point (pop rest))
                               (setq any-quoted-p
                                     (or
                                      (when-let (char (char-before point))
                                        (or (char-equal char ?')
                                            (char-equal char ?`)))
                                      (save-excursion
                                        (goto-char (1+ point))
                                        (looking-at-p
                                         "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                             any-quoted-p))))
                       ;; Containing sexp has nothing before this line
                       ;; except the first element.  Indent under that element.
                       nil
                     ;; Skip the first element, find start of second (the first
                     ;; argument of the function call) and indent under.
                     (progn (forward-sexp 1)
                            (parse-partial-sexp (point)
                                                calculate-lisp-indent-last-sexp
                                                0 t)))
                   (backward-prefix-chars))
                  (t
                   ;; Indent beneath first sexp on same line as
                   ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                   ;; almost certainly a function call.
                   (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                       0 t)
                   (backward-prefix-chars)))))
        ;; Point is at the point to indent under unless we are inside a string.
        ;; Call indentation hook except when overridden by lisp-indent-offset
        ;; or if the desired indentation has already been computed.
        (let ((normal-indent (current-column)))
          (cond ((elt state 3)
                 ;; Inside a string, don't change indentation.
                 nil)
                ((and (integerp lisp-indent-offset) containing-sexp)
                 ;; Indent by constant offset
                 (goto-char containing-sexp)
                 (+ (current-column) lisp-indent-offset))
                ;; in this case calculate-lisp-indent-last-sexp is not nil
                (calculate-lisp-indent-last-sexp
                 (or
                  ;; try to align the parameters of a known function
                  (and lisp-indent-function
                       (not retry)
                       (funcall lisp-indent-function indent-point state))
                  ;; If the function has no special alignment
                  ;; or it does not apply to this argument,
                  ;; try to align a constant-symbol under the last
                  ;; preceding constant symbol, if there is such one of
                  ;; the last 2 preceding symbols, in the previous
                  ;; uncommented line.
                  (and (save-excursion
                         (goto-char indent-point)
                         (skip-chars-forward " \t")
                         (looking-at ":"))
                       ;; The last sexp may not be at the indentation
                       ;; where it begins, so find that one, instead.
                       (save-excursion
                         (goto-char calculate-lisp-indent-last-sexp)
                         ;; Handle prefix characters and whitespace
                         ;; following an open paren.  (Bug#1012)
                         (backward-prefix-chars)
                         (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                       (line-beginning-position))
                                         (and containing-sexp
                                              (>= (1+ containing-sexp) (point)))))
                           (forward-sexp -1)
                           (backward-prefix-chars))
                         (setq calculate-lisp-indent-last-sexp (point)))
                       (> calculate-lisp-indent-last-sexp
                          (save-excursion
                            (goto-char (1+ containing-sexp))
                            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                            (point)))
                       (let ((parse-sexp-ignore-comments t)
                             indent)
                         (goto-char calculate-lisp-indent-last-sexp)
                         (or (and (looking-at ":")
                                  (setq indent (current-column)))
                             (and (< (line-beginning-position)
                                     (prog2 (backward-sexp) (point)))
                                  (looking-at ":")
                                  (setq indent (current-column))))
                         indent))
                  ;; another symbols or constants not preceded by a constant
                  ;; as defined above.
                  normal-indent))
                ;; in this case calculate-lisp-indent-last-sexp is nil
                (desired-indent)
                (t
                 normal-indent)))))))

(use-package isayt
  :diminish
  :ensure (:host gitlab :repo "andreyorst/isayt.el" :protocol https)
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . isayt-mode))


(provide 'setup-elisp)
;;; setup-elisp.el ends here
