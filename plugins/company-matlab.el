;;; company-matlab.el --- A matlab-mode and matlab-shell mode completion back-end

;;; Commentary:
;; Copyright (C) 2009 David Engster
;; Copyright (C) 2015 Andrzej Pronobis
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
;; The following code is largely based on the company-matlab-shell.el mode.

(require 'cl-lib)
(condition-case nil
    (require 'company)
  (error nil))
(require 'matlab)


(defun company-matlab-get-completions (cmd)
  "Retrieve completions for the given command CMD."
  (let* ((pt-start (save-excursion (matlab-beginning-of-command) (point)))
         (pt-shell nil)
         (cmd (buffer-substring-no-properties pt-start (point)))
         (completions nil)
         (pre-shell-line nil)
         (post-shell-line nil)
         (shell-buf (get-buffer (concat "*" matlab-shell-buffer-name "*")))
         (inhibit-quit nil))            ; For some reason this is set and produces error
    ;; Remove everything before ;
    (setq cmd (replace-regexp-in-string ".*;" "" cmd))
    ;; Replace every ' with ''''
    (setq cmd (replace-regexp-in-string "'" "''''" cmd))
    (with-current-buffer shell-buf
      ;; Remember shell cursor position
      (setq pt-shell (point))
      (setq pre-shell-line (save-excursion
                             (let ((inhibit-field-text-motion t))
                               (beginning-of-line))
                             (point)))
      ;; Get completions
      (setq completions (let ((comint-inhibit-carriage-motion t))
                          (matlab-shell-completion-list cmd))))
    ;; Restore point if it was misplaced in console
    (dolist (win (get-buffer-window-list shell-buf nil t))
      (with-selected-window win
        (setq post-shell-line (save-excursion
                                (let ((inhibit-field-text-motion t))
                                  (beginning-of-line))
                                (point)))
        (goto-char (+ post-shell-line (- pt-shell pre-shell-line)))))
    (mapcar 'car completions)))

(defun company-matlab-grab-command ()
  "Return the symbol to complete together with length of all relevant context."
  (let* ((symbol (company-grab-symbol))
         (pt-beg (save-excursion (matlab-beginning-of-command) (point)))
         (pt-end (point))
         (len (- pt-end pt-beg)))
    (when (and (> len 0)
               (not (eq (preceding-char) ?\s)))  ; Do not complete just after space
      (cons symbol len))))

(defun company-matlab-prefix ()
  "Check if all conditions are met and provide prefix."
  (and (derived-mode-p 'matlab-mode 'matlab-shell-mode)
       (not (matlab-cursor-in-string t))
       (not (matlab-cursor-in-comment))
       (matlab-shell-active-p)
       (or (company-matlab-grab-command) 'stop)))

;;;###autoload
(defun company-matlab (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for matlab-mode and matlab-shell-mode."
  (interactive (list 'interactive))
  (cl-case command
    ('interactive (company-begin-backend 'company-matlab))
    ('prefix (company-matlab-prefix))
    ('candidates (company-matlab-get-completions arg))
    ('sorted t)))

(provide 'company-matlab)
;;; company-matlab.el ends here
