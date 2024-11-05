;;; latex-latex-preview.el --- latex-mode previews using org-latex-preview  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: tex

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

;; Plugging in org-latex-preview into LaTeX-mode.

;;; Code:
(require 'org-latex-preview)
(require 'tex)

(setf (alist-get 'LaTeX-mode org-latex-preview-interfaces)
      `(:preamble ,#'latex-latex-preview--make-preamble
        :section  ,#'latex-latex-preview-section-bounds
        :context  ,#'latex-latex-preview-context
        :type     ,#'latex-latex-preview-type
        :property ,#'latex-latex-preview-property))

(defun latex-latex-preview--make-preamble ()
  "Slurp the preamble from the `TeX-master-file'.
Code stolen from `TeX-region-create'."
  (save-excursion
    (save-restriction
      (set-buffer                       ; TeX master
       (find-file-noselect (TeX-master-file TeX-default-extension)))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (not (re-search-forward TeX-header-end nil t))
              ""
            (re-search-backward TeX-header-end nil t)
            (string-replace
             "\\pdfoutput=1"            ; HACK
             ""
             (buffer-substring-no-properties (point-min) (point)))))))))

(defconst latex-latex-preview--start-strings
  '(("\\$\\$" . "$$")
    ("\\$" . "$")
    ("\\\\(" . "\\(")
    ("\\\\\\[" . "\\[")))

(defun latex-latex-preview-context ()
  (save-match-data
    (save-excursion
      (let ((opener))
        (when (or (texmathp)
                  (setq opener (cl-some (lambda (pair)
                                        (and (looking-at (car pair))
                                             (cdr pair)))
                                      latex-latex-preview--start-strings))
                  (progn (beginning-of-line)
                         (skip-chars-forward " \t")
                         (looking-at "\\\\begin\\|\\\\end")))
          (pcase-let*
              ((begin (or (and (car texmathp-why)
                               (cdr texmathp-why))
                          (point)))
               (`(,end . ,type)
                (progn
                  (goto-char (1+ begin))
                  (pcase (or (car texmathp-why) opener)
                    ("$"   (cons (search-forward "$" nil t)   'latex-fragment))
                    ("\\(" (cons (search-forward "\\)" nil t) 'latex-fragment))
                    ("$$"  (cons (search-forward "$$" nil t)  'latex-environment))
                    ("\\[" (cons (search-forward "\\]" nil t) 'latex-environment))
                    (_     (cons (LaTeX-find-matching-end)    'latex-environment)))))
               (element `(:begin ,begin)))
            (when end
              (plist-put element :type type)
              (plist-put element :end end))
            (plist-put element :buffer (current-buffer))
            element))))))

(defun latex-latex-preview-property (prop element)
  (if (eq prop :value)
      (buffer-substring-no-properties
       (plist-get element :begin)
       (plist-get element :end))
    (plist-get element prop)))

(defun latex-latex-preview-type (element)
  (plist-get element :type))

(defun latex-latex-preview-section-bounds ()
  (save-excursion
    (LaTeX-mark-section)
    (prog1 (list (region-beginning) (region-end))
      (deactivate-mark)
      (pop-mark))))

(provide 'latex-latex-preview)
;;; latex-latex-preview.el ends here
