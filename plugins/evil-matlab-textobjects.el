;;; evil-latex-textobjects.el --- LaTeX text objects for evil

;; Copyright (C) 2015  Hans-Peter Deifel

;; Author: Hans-Peter Deifel <hpd@hpdeifel.de>
;; Keywords: tex, wp, convenience, vi, evil
;; Version: 1.0-git
;; Package-Requires: ((evil "1.0") (auctex "11.88"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Provides a minor mode that installs several additional LaTeX
;;; specific text objects for evil-mode:
;;; 
;;;  \	Display math		\[ .. \]
;;;  $	Inline math		$ .. $
;;;  m	TeX macro		\foo{..}
;;;  e	LaTeX environment	\begin{foo}..\end{foo}
;;;
;;; To enable this mode in LaTeX buffers, add this to your init file:
;;;
;;; (require 'evil-latex-textobjects)
;;; (add-hook 'LaTeX-mode-hook 'turn-on-evil-latex-textobjects-mode)

;;; Code:

(require 'evil)
;; (require 'matlab-mode)

;; (evil-define-text-object evil-latex-textobjects-inner-dollar (count &optional beg end type)
;;   "Select inner dollar"
;;   :extend-selection nil
;;   (evil-select-quote ?$ beg end type count nil))

;; (evil-define-text-object evil-latex-textobjects-a-dollar (count &optional beg end type)
;;   "Select a dollar"
;;   :extend-selection t
;;   (evil-select-quote ?$ beg end type count t))

;; (evil-define-text-object evil-latex-textobjects-inner-math (count &optional beg end type)
;;   "Select innter \\[ \\] or \\( \\)."
;;   :extend-selection nil
;;   (evil-select-paren "\\\\\\[\\|\\\\(" "\\\\)\\|\\\\\\]" beg end type count nil))

;; (evil-define-text-object evil-latex-textobjects-a-math (count &optional beg end type)
;;   "Select a \\[ \\] or \\( \\)."
;;   :extend-selection nil
;;   (evil-select-paren "\\\\\\[\\|\\\\(" "\\\\)\\|\\\\\\]" beg end type count t))

;; (defun evil-latex-textobjects-macro-beginning ()
;;   "Return (start . end) of the macro-beginning to the left of point.

;; If no enclosing macro is found, return nil.
;; For example for \macro{foo|bar} it returns the start and end of \"\macro{\""
;;   (let ((beg (TeX-find-macro-start)))
;;     (when beg
;;       (save-excursion
;;         (goto-char beg)
;;         (forward-char)                  ; backslash
;;         (skip-chars-forward "A-Za-z@*") ; macro-name
;;         (when (looking-at "{\\|\\[")
;;           (forward-char))                ; opening brace
;;         (cons beg (point))))))

;; (defun evil-latex-textobjects-macro-end ()
;;   "Return (start . end) of the end of the enclosing macro.

;; If no such macro can be found, return nil"
;;   (let ((end (TeX-find-macro-end)))
;;     (when end
;;       (save-excursion
;;         (goto-char end)
;;         (when (looking-back "}\\|\\]")
;;           (backward-char))               ; closing brace
;;         (cons (point) end)))))

;; TODO Support visual selection
;; TODO Support count

;; (evil-define-text-object evil-latex-textobjects-a-macro (count &optional beg end type)
;;   "Select a TeX macro"
;;   :extend-selection nil
;;   (let ((beg (evil-latex-textobjects-macro-beginning))
;;         (end (evil-latex-textobjects-macro-end)))
;;     (if (and beg end)
;;         (list (car beg) (cdr end))
;;       (error "No enclosing macro found"))))

;; (evil-define-text-object evil-latex-textobjects-inner-macro (count &optional beg end type)
;;   "Select inner TeX macro"
;;   :extend-selection nil
;;   (let ((beg (evil-latex-textobjects-macro-beginning))
;;         (end (evil-latex-textobjects-macro-end)))
;;     (cond
;;      ((or (null beg) (null end))
;;       (error "No enclosing macro found"))
;;      ((= (cdr beg) (car end))           ; macro has no content
;;       (list (1+ (car beg))              ; return macro boundaries excluding \
;;             (cdr beg)))
;;      (t (list (cdr beg) (car end))))))

;; (defun evil-latex-textobjects-env-beginning ()
;;   "Return (start . end) of the \\begin{foo} to the left of point."
;;   (let (beg)
;;     (save-excursion
;;       (LaTeX-find-matching-begin)       ; we are at backslash
;;       (setq beg (point))
;;       (skip-chars-forward "^{")         ; goto opening brace
;;       (forward-sexp)                    ; goto closing brace
;;       ;; Count the newline after \begin{foo} to the environment header
;;       ;; Without this, delete-inner-env would unexpectedly move the end
;;       ;; to the same line as the beginning
;;       ;; (when (looking-at "[[:blank:]]*$")
;;       ;;   (message "Newline")
;;       ;;   (forward-line 1))
;;       (cons beg (point)))))

;; (defun evil-latex-textobjects-env-end ()
;;   "Return (start . end) of the \\end{foo} to the right of point."
;;   (let (end)
;;     (save-excursion
;;       (LaTeX-find-matching-end)         ; we are at closing brace
;;       (setq end (point))
;;       (backward-sexp)                   ; goto opening brace
;;       (search-backward "\\")            ; goto backslash
;;       (cons (point) end))))

;; Section text objects
;; --------------------
(defun evil-matlab-textobjects-section-beginning ()
  "Return (start . end) of the section delimiter %% to the left of point."
  (save-excursion
    (let ((beg (re-search-backward "^%%" nil t)))
      (if (null beg)
          (progn (setq beg (point-min))
                 (cons beg beg))
        (cons beg (progn (beginning-of-line 2)
                         (point))))))
  )
(defun evil-matlab-textobjects-section-end ()
  "Return (start . end) of the section delimiter %% to the right of point."
  (save-excursion
    (let ((end (re-search-forward "^%%" nil t)))
      (if (null end)
          (progn (setq end (point-max))
                 (cons end end))
       (cons (progn (end-of-line 0) (point))
             (match-beginning 0)) 
        )
      )))

(evil-define-text-object evil-matlab-textobjects-a-section (count &optional beg end type)
  "Select a Matlab section"
  :extend-selection nil
  (let ((beg (evil-matlab-textobjects-section-beginning))
        (end (evil-matlab-textobjects-section-end)))
    (list (car beg) (cdr end))))
(evil-define-text-object evil-matlab-textobjects-inner-section (count &optional beg end type)
  "Select a Matlab section"
  :extend-selection nil
  (let ((beg (evil-matlab-textobjects-section-beginning))
        (end (evil-matlab-textobjects-section-end)))
    (list (cdr beg) (car end))))

;; Command text objects
;; --------------------
(defun evil-matlab-textobjects-command-beginning ()
  "Return position of the beginning of the matlab command to the left of point."
  (save-excursion
    (let ((beg (save-excursion (matlab-beginning-of-command)
                               (point))))
      beg))
  )
(defun evil-matlab-textobjects-command-end ()
  "Return position of the end of the matlab command to the right of point."
  (save-excursion
    (let ((end (save-excursion (matlab-end-of-command)
                               (point))))
      end))
  )

(evil-define-text-object evil-matlab-textobjects-inner-command (count &optional beg end type)
  "Select a Matlab section"
  :extend-selection nil
  (let ((beg (evil-matlab-textobjects-command-beginning))
        (end (evil-matlab-textobjects-command-end)))
    (list beg end)))
(evil-define-text-object evil-matlab-textobjects-a-command (count &optional beg end type)
  "Select a Matlab section"
  :extend-selection nil
  (let ((beg (evil-matlab-textobjects-command-beginning))
        (end (evil-matlab-textobjects-command-end)))
    (list beg end)))

;; Function text objects
;; ---------------------
;; TODO

;; s-exp text objects
;; ---------------------
;; TODO

;; Matlab-textobjects keymap
;; ------------------------- 
(defvar evil-matlab-textobjects-outer-map (make-sparse-keymap))
(defvar evil-matlab-textobjects-inner-map (make-sparse-keymap))

(set-keymap-parent evil-matlab-textobjects-outer-map evil-outer-text-objects-map)
(set-keymap-parent evil-matlab-textobjects-inner-map evil-inner-text-objects-map)

(define-key evil-matlab-textobjects-inner-map "s" 'evil-matlab-textobjects-inner-command)
(define-key evil-matlab-textobjects-outer-map "s" 'evil-matlab-textobjects-a-command)
;; (define-key evil-latex-textobjects-inner-map "\\" 'evil-latex-textobjects-inner-math)
;; (define-key evil-latex-textobjects-outer-map "\\" 'evil-latex-textobjects-a-math)
;; (define-key evil-latex-textobjects-outer-map "m" 'evil-latex-textobjects-a-macro)
;; (define-key evil-latex-textobjects-inner-map "m" 'evil-latex-textobjects-inner-macro)
(define-key evil-matlab-textobjects-outer-map "S" 'evil-matlab-textobjects-a-section)
(define-key evil-matlab-textobjects-inner-map "S" 'evil-matlab-textobjects-inner-section)

;;;###autoload
(define-minor-mode evil-matlab-textobjects-mode
  "Minor mode for latex-specific text objects in evil.

Installs the following additional text objects:
\\<evil-latex-textobjects-outer-map>
  \\[evil-latex-textobjects-a-math]\tDisplay math\t\t\\=\\[ .. \\=\\]
  \\[evil-latex-textobjects-a-dollar]\tInline math\t\t$ .. $
  \\[evil-latex-textobjects-a-macro]\tTeX macro\t\t\\foo{..}
  \\[evil-latex-textobjects-an-env]\tLaTeX environment\t\\begin{foo}..\\end{foo}"
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

(evil-define-key 'operator evil-matlab-textobjects-mode-map
  "a" evil-matlab-textobjects-outer-map
  "i" evil-matlab-textobjects-inner-map)

(evil-define-key 'visual evil-matlab-textobjects-mode-map
  "a" evil-matlab-textobjects-outer-map
  "i" evil-matlab-textobjects-inner-map)

;;;###autoload
(defun turn-on-evil-matlab-textobjects-mode ()
  "Enable evil-matlab-textobjects-mode in current buffer."
  (interactive "")
  (evil-matlab-textobjects-mode 1))

;;;###autoload
(defun turn-off-evil-matlab-textobjects-mode ()
  "Disable evil-matlab-textobjects-mode in current buffer."
  (interactive "")
  (evil-matlab-textobjects-mode -1))


(provide 'evil-matlab-textobjects)

;;; evil-latex-textobjects.el ends here
