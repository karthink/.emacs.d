;;; poi.el --- Jump to points of interest            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience

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
(require 'simple)

(defcustom poi-functions nil
    "Additional functions to use as `next-error-function'."
    :group 'next-error
    :type 'hook)

(defvar-local poi--choice nil)

(defun poi-action ()
  "Perform the logical action at point of interest."
  (interactive)
  (and-let* ((poi--choice)
             (action-fn (get poi--choice 'poi-action)))
    (call-interactively action-fn)))

(defun poi-next (arg)
  (interactive "p")
  (let ((final-point (if (> arg 0) (point-max) (point-min)))
        (comparison (if (> arg 0) #'< #'>))
        next-point (here (point)))
    (run-hook-wrapped
     'poi-functions
     (lambda (func)
       (prog1 nil
         (save-excursion
           (condition-case nil (funcall func arg)
             (user-error nil)
             (:success
              (when (and (funcall comparison (point) (or next-point final-point))
                         (funcall comparison here (point)))
                (setq poi--choice func
                      next-point (point)))))))))
    (if next-point (goto-char next-point) (ding t))))

(defun poi-previous (arg)
  (interactive "p")
  (poi-next (- arg)))

;;;###autoload
(defun poi-register (fn &optional action-fn)
  "Add FUN to `poi-functions'."
  (lambda ()
    (if (memq fn poi-functions)
        (remove-hook 'poi-functions fn 'local)
      (add-hook 'poi-functions fn nil 'local))
    (when action-fn (put fn 'poi-action action-fn))))

;;;; Integration with `next-error'
(defun poi-delegate (orig-fn &optional arg reset)
  "If a `next-error' source is visible, run `next-error'.

Otherwise jump to the closest point of interest."
  (if poi-functions
      (if-let* ((buf (ignore-errors
                       (next-error-find-buffer
                        t nil (lambda () (not (memq next-error-function
                                               '(org-occur-next-match
                                                 vterm-next-error-function)))))))
                ((get-buffer-window buf)))
          (funcall orig-fn arg reset)
        (setq arg (or arg 1))
        (poi-next arg))
    (funcall orig-fn arg reset)))

(provide 'poi)
;;; poi.el ends here
