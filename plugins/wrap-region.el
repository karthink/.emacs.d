;;; wrap-region.el --- Wrap text with punctation or tag

;; Copyright (C) 2008-2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.2.0
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/wrap-region

;; This file is NOT part of GNU Emacs.


;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; wrap-region is a minor mode that wraps a region with
;; punctuations. For tagged markup modes, such as HTML and XML, it
;; wraps with tags.
;;
;; To use wrap-region, make sure that this file is in Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require wrap-region:
;;   (require 'wrap-region)

;; To start wrap-region:
;;   (wrap-region-mode t) or M-x wrap-region-mode
;;
;; If you only want wrap-region active in some mode, use hooks:
;;   (add-hook 'ruby-mode-hook 'wrap-region-mode)
;;
;; Or if you want to activate it in all buffers, use the global mode:
;;   (wrap-region-global-mode t)

;; To wrap a region, select that region and hit one of the punctuation
;; keys. In "tag-modes"" (see `wrap-region-tag-active-modes'), "<" is
;; replaced and wraps the region with a tag. To activate this behavior
;; in a mode that is not default:
;;   (add-to-list 'wrap-region-tag-active-modes 'some-tag-mode)
;;
;; `wrap-region-table' contains the default punctuations
;; that wraps. You can add and remove new wrappers by using the
;; functions `wrap-region-add-wrapper' and
;; `wrap-region-remove-wrapper' respectively.
;;   (wrap-region-add-wrapper "#" "#")
;;   (wrap-region-remove-wrapper "(")
;;
;; Some modes may have conflicting key bindings with wrap-region. To
;; avoid conflicts, the list `wrap-region-except-modes' contains names
;; of modes where wrap-region should not be activated (note, only in
;; the global mode). You can add new modes like this:
;;   (add-to-list 'wrap-region-except-modes 'conflicting-mode)


;;; Code:

(defvar wrap-region-mode-map (make-sparse-keymap)
  "Keymap for `wrap-region-mode'.")

(defvar wrap-region-table
  (let ((table (make-hash-table :test 'equal)))
    (puthash "\"" "\"" table)
    (puthash "'"  "'"  table)
    (puthash "("  ")"  table)
    (puthash "{"  "}"  table)
    (puthash "["  "]"  table)
    (puthash "<"  ">"  table)
    table)
  "Table with wrapper pairs.")


(defvar wrap-region-tag-active-modes '(html-mode sgml-mode rhtml-mode)
  "List of modes that uses tags.")

(defvar wrap-region-except-modes '(calc-mode dired-mode)
  "A list of modes in which `wrap-region-mode' should not be activated.")

(defvar wrap-region-hook nil
  "Called when `wrap-region-mode' is turned on.")

(defvar wrap-region-before-wrap-hook nil
  "Called before wrapping.")

(defvar wrap-region-after-wrap-hook nil
  "Called after wrapping.")


(defun wrap-region-trigger ()
  "Called when trigger key is pressed."
  (interactive)
  (let ((key (char-to-string last-input-event)))
    (if (region-active-p)
        (if (wrap-region-insert-tag-p key)
            (wrap-region-with-tag)
          (wrap-region-with-punctuations key (gethash key wrap-region-table)))
      (wrap-region-fallback key))))

(defun wrap-region-insert-tag-p (key)
  "Checks if tag should be inserted or not."
  (and (member major-mode wrap-region-tag-active-modes) (equal key "<")))

(defun wrap-region-with-tag ()
  "Wraps region with tag."
  (let* ((tag (read-string "Enter Tag (with optional attributes): "))
         (split (split-string tag " "))
         (tag-name (car split))
         (left (concat "<" tag ">"))
         (right (concat "</" tag-name ">")))
    (wrap-region-with left right)))

(defun wrap-region-with-punctuations (left right)
  "Wraps region with LEFT and RIGHT punctuations."
  (wrap-region-with left right))

(defun wrap-region-with (left right)
  "Wraps region with LEFT and RIGHT."
  (run-hooks 'wrap-region-before-wrap-hook)
  (let ((beg (region-beginning)) (end (region-end)))
    (save-excursion
      (goto-char beg)
      (insert left)
      (goto-char (+ end (length left)))
      (insert right)))
  (run-hooks 'wrap-region-after-wrap-hook))

(defun wrap-region-fallback (key)
  "Executes function that KEY was bound to before `wrap-region-mode'."
  (let ((wrap-region-mode nil))
    (call-interactively
     (key-binding
      (read-kbd-macro key)))))

(defun wrap-region-add-wrapper (left right)
  "Adds LEFT and RIGHT as new wrapper pair."
  (puthash left right wrap-region-table)
  (wrap-region-define-wrapper left))

(defun wrap-region-remove-wrapper (left)
  "Removed LEFT as wrapper."
  (remhash left wrap-region-table)
  (wrap-region-unset-key left))

(defun wrap-region-define-keys ()
  "Defines key bindings for `wrap-region-mode'."
  (maphash
   (lambda (left _)
     (wrap-region-define-wrapper left))
   wrap-region-table))

(defun wrap-region-define-wrapper (key)
  "Defines KEY as wrapper."
  (wrap-region-define-key key 'wrap-region-trigger))

(defun wrap-region-unset-key (key)
  "Remove KEY from `wrap-region-mode-map'."
  (wrap-region-define-key key nil))

(defun wrap-region-define-key (key fn)
  "Binds KEY to FN in `wrap-region-mode-map'."
  (define-key wrap-region-mode-map (edmacro-parse-keys key) fn))


;;;###autoload
(define-minor-mode wrap-region-mode
  "Wrap region with stuff."
  :init-value nil
  :lighter " wr"
  :keymap wrap-region-mode-map
  (when wrap-region-mode
    (wrap-region-define-keys)
    (run-hooks 'wrap-region-hook)))

;;;###autoload
(defun turn-on-wrap-region-mode ()
  "Turn on `wrap-region-mode'"
  (interactive)
  (unless (member major-mode wrap-region-except-modes)
    (wrap-region-mode +1)))

;;;###autoload
(defun turn-off-wrap-region-mode ()
  "Turn off `wrap-region-mode'"
  (interactive)
  (wrap-region-mode -1))

;;;###autoload
(define-globalized-minor-mode wrap-region-global-mode
  wrap-region-mode
  turn-on-wrap-region-mode)

(provide 'wrap-region)

;;; wrap-region.el ends here
