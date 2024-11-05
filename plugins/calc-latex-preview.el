;;; calc-preview.el --- org-latex-preview interface for calc  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: hypermedia, tex

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
(require 'calc)
(require 'calc-lang)
(require 'org-latex-preview)

;; (math-format-stack-value (nth (calc-locate-cursor-element (point)) calc-stack))
;; math-format-stack-value: entry -> string
;; calc-locate-cursor-element: pt -> index of current entry in calc-stack
;; (cadr (nth n calc-stack)) -> height of nth entry in calc-stack
;; calc-cursor-stack-index : index -> (goto calc-stack's index'th entry)
;; calc-substack-height: index -> height above bottom of calc-stack's index'th entry's beginning

;;; org-latex-preview interface for calc
(setf (alist-get 'calc-mode org-latex-preview-interfaces)
      `(:preamble   ,#'calc-latex-preview--preamble
        :context    ,#'calc-latex-preview--context
        :type       ,#'calc-latex-preview--type
        :property   ,#'calc-latex-preview--property))

(defun calc-latex-preview--preamble ()
  (with-temp-buffer
    (insert (org-latex-preview--get-preamble))
    (goto-char (point-min))
    (search-forward "\\documentclass" nil t)
    (insert "[fleqn]")
    (goto-char (point-max))
    (insert "\\setlength{\\mathindent}{20pt}"
            "\\setlength{\\abovedisplayskip}{4pt}"
            "\\setlength{\\belowdisplayskip}{8pt}")
    (buffer-string)))

(defun calc-latex-preview--context ()
  ";TODO: "
  (when (> (length calc-stack) 1)
    (let* ((index (calc-locate-cursor-element (point)))
           (formula-height (cadr (nth index calc-stack)))
           begin end value)
      (unless (= index 0)
        (save-excursion
          (let ((calc-line-numbering)
                (calc-line-breaking)
                (calc-language 'latex)
                (calc-language-option 1))
            (setq value
                  (concat                 ; NOTE: (calc-top ...) is slower
                   "\\["
                   (math-format-stack-value (nth index calc-stack))
                   "\\]"))
            ;; `math-format-stack-value' modifies the stack metadata.  Because we
            ;; set `calc-line-breaking' to nil, the height of the "formula cell"
            ;; as recorded in `calc-stack' is now wrong.  Reset it to
            ;; `formula-height', captured above. Truly, this is inspired design.
            (setf (cadr (nth index calc-stack)) formula-height))
          (calc-cursor-stack-index (1- index))
          (setq end (1- (point)))
          (calc-cursor-stack-index index)
          (setq begin (if calc-line-numbering
                          (+ (point) 4)
                        (point))))
        (list :type 'latex-environment
              :begin begin :end end :value value)))))

(defun calc-latex-preview--type (_element)
  ";TODO: "
  (unless (save-excursion (forward-line)
                          (eobp))
    'latex-environment))

(defun calc-latex-preview--property (prop element)
  (plist-get element prop))

;;; LaTeX preview generator

(defun calc-latex-preview--refresh (&rest _)
  ;; Move up the stack, collecting fragments to preview
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (when-let
        ((elements
          (cl-loop
           for entry in calc-stack do
           (forward-line (- (cadr entry)))
           collect (calc-latex-preview--context) into elements
           if (bobp) return elements)))
      (when (car-safe elements)
        (org-latex-preview--place-from-elements
         org-latex-preview-process-default elements)))))

(defun calc-latex-preview--hide-text (ov)
  (prog1 ov
    (unless (overlay-get ov 'display)
      (overlay-put ov 'display " "))))

(define-minor-mode calc-latex-preview-mode
  "Preview LaTeX in the Emacs calculator."
  :lighter " 🦄"
  (if (eq major-mode 'calc-mode)
      (if calc-latex-preview-mode
          (progn (calc-latex-language 1)
                 (org-latex-preview-auto-mode 1)
                 (calc-latex-preview--refresh)
                 (setq-local org-latex-preview-numbered nil)
                 (add-hook 'org-latex-preview-overlay-update-functions
                           #'calc-latex-preview--hide-text nil 'local)
                 (advice-add 'calc-pop :after 'calc-latex-preview--refresh)
                 (advice-add 'calc-push-list :after 'calc-latex-preview--refresh)
                 (advice-add 'calc-refresh :after 'calc-latex-preview--refresh)
                 (calc-refresh))
        (remove-hook 'org-latex-preview-overlay-update-functions
                     #'calc-latex-preview--hide-text)
        (advice-remove 'calc-pop 'calc-latex-preview--refresh)
        (advice-remove 'calc-push-list 'calc-latex-preview--refresh)
        (advice-remove 'calc-refresh 'calc-latex-preview--refresh)
        (org-latex-preview-clear-overlays)
        (org-latex-preview-auto-mode 0))
    (user-error "Calc preview mode can only be used in Calc buffers.")))

(provide 'calc-latex-preview)
;;; calc-latex-preview.el ends here
