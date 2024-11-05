;;; prog-latex-preview.el --- LaTeX previews for programming modes  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'org-latex-preview)
(require 'syntax)

(setf (alist-get 'prog-mode org-latex-preview-interfaces)
      `(:context  ,#'prog-latex-preview--context
        :type     ,#'prog-latex-preview--type
        :property ,#'prog-latex-preview--property
        :preamble ,#'prog-latex-preview--preamble
        :section  ,(lambda () (list (point-min) (point-max)))))

(defun prog-latex-preview--preamble ()
  (with-temp-buffer
    (org-latex-preview--get-preamble)))

(defun prog-latex-preview--type (element)
  (plist-get element :type))

(defun prog-latex-preview--property (prop element)
  (plist-get element prop))

(defsubst prog-latex-preview--comment-p ()
  (or (nth 3 (syntax-ppss))
      (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face)
            '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defsubst prog-latex-preview--sanitize (val)
  (if comment-start
      (string-replace comment-start "" val)
    val))

(defun prog-latex-preview--context ()
  ";TODO: "
  (when (prog-latex-preview--comment-p)
    (let (begin end)
      (save-excursion
        (catch 'failed
          (when (eq (char-before) ?\\)
            (forward-char))
          (when (eq (char-after) ?\\)
            (forward-char 2))
          (or 
           (looking-back "\\\\\\[\\|\\\\(" (- (point) 2))
           (while (not (looking-back "\\\\\\[\\|\\\\(" (- (point) 2)))
             (if (or (not (prog-latex-preview--comment-p))
                     (bobp))
                 (throw 'failed nil)
               (backward-char))))
          (setq begin (- (point) 2))
          (pcase (match-string 0)
            ("\\["
             (setq type 'latex-environment)
             (while (not (looking-at "\\]"))
               (if (or (not (prog-latex-preview--comment-p))
                       (eobp))
                   (throw 'failed nil))
               (forward-char))
             (setq end (+ (point) 1)))

            ("\\("
             (setq type 'latex-fragment)
             (while (not (looking-at "\\\\)"))
               (if (or (not (prog-latex-preview--comment-p))
                       (eobp))
                   (throw 'failed nil))
               (forward-char))
             (setq end (+ (point) 2))))
          
          ;; (cond
          ;;  ((looking-back "\\$")
          ;;   (setq begin (1- (point)))
          ;;   (if (looking-at "\\$")      ;displaymath begin
          ;;       (progn
          ;;         (setq type 'latex-environment)
          ;;         (while (not (looking-at "\\$\\$"))
          ;;           (if (or (not (prog-latex-preview--comment-p))
          ;;                   (eobp))
          ;;               (throw 'failed nil)
          ;;             (forward-char)))
          ;;         (setq end (+ (point) 2)))
          ;;     (setq type 'latex-fragment)
          ;;     (while (not (looking-at "\\$"))
          ;;       (if (or (not (prog-latex-preview--comment-p))
          ;;               (eobp))
          ;;           (throw 'failed nil)
          ;;         (forward-char)))
          ;;     (setq end (1+ (point)))))
          ;;  ((looking-back "\\\\\\[" (- (point) 2))        ;displaymath begin
          ;;   (setq begin (- (point) 2)
          ;;         type  'latex-environment)
          ;;   (while (not (looking-at "\\]"))
          ;;     (if (or (not (prog-latex-preview--comment-p))
          ;;             (eobp))
          ;;         (throw 'failed nil))
          ;;     (forward-char))
          ;;   (setq end (1+ (point))))
           
          ;;  ((looking-back "\\\\(" (- (point) 2))      ;inlinemath begin
          ;;   (setq begin (- (point) 2)
          ;;         type  'latex-fragment)
          ;;   (while (not (looking-at "\\\\)"))
          ;;     (if (or (not (prog-latex-preview--comment-p))
          ;;             (eobp))
          ;;         (throw 'failed nil))
          ;;     (forward-char))
          ;;   (setq end (+ (point) 2))))
          
          (and begin end
               (list :begin begin :end end :type type
                     :value (prog-latex-preview--sanitize
                             (buffer-substring-no-properties
                              begin end)))))))))





(provide 'prog-latex-preview)
;;; prog-latex-preview.el ends here
