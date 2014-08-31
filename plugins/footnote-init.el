;;; footnote-init.el --- Reads in existing footnotes 
;; Version 1.1 
;; Copyright (C) 2007  Andreas Roehler 
;; Author: Andreas Roehler <andreas.roeh...@easy-emacs.de> 
;; Keywords: wp, mail, news 
;; This file is free software; you can redistribute it and/or modify 
;; it under the terms of the GNU General Public License as published by 
;; the Free Software Foundation; either version 2, or (at your option) 
;; any later version. 
;; This file is distributed in the hope that it will be useful, 
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; GNU General Public License for more details. 
;; You should have received a copy of the GNU General Public License 
;; along with GNU Emacs; see the file COPYING.  If not, write to 
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
;; Boston, MA 02110-1301, USA. 
;;; Commentary: Provides a `footnote-init' usable as 
;;; footnote-mode-hook, which reads in existing 
;;; footnodes. Customize `footnote-mode-hook' with this 
;;; function should do the right thing when switching 
;;; footnote-mode on. 
;; Changes to previous version: Bugs fixed which 
;; occurred if `footnote-init' was called with buffer 
;; without footnotes. 
;;; Code: 
(require 'footnote) 
(defvar footnote-regexp nil 
 "`footnote-regexp' in effect after `footnote-init' is called 
Possible values are defined in footnote-style-alist: 
`footnote-numeric-regexp', footnote-english-lower-regexp etc. ") 
(defun footnote-init (&optional ispec) 
   "Let's footnote take notice of already existing footnotes" 
   (interactive "p") 
   (unless footnote-mode (footnote-mode)) 
   (let ((documents-footnote-style (footnote-what-style ispec))) 
      (setq footnote-regexp (concat (nth 2 documents-footnote-style) "+")) 
      (footnote-init-markers t) 
      (Footnote-set-style (nth 0 documents-footnote-style)))) 
(defun footnote-what-style (&optional ispec) 
   "Returns style in effect according footnote-style-alist: 
default is (numeric Footnote-numeric ,footnote-numeric-regexp)" 
   (interactive "p") 
   (let((length-fn-start-tag (length footnote-start-tag)) 
           (length-fn-end-tag (length footnote-end-tag)) 
           style-in-effect found) 
      (save-excursion 
         (goto-char (point-min)) 
         (when 
         (search-forward footnote-start-tag nil t 1) 
      (goto-char (match-beginning 0)) 
      (setq found (buffer-substring-no-properties (+ length-fn-start-tag 
(point)) (- (re-search-forward footnote-end-tag (line-end-position) t 1) 
length-fn-end-tag))) 
      (dolist (styles footnote-style-alist) 
         (when 
               (string-match (nth 2 styles) found) 
            (setq style-in-effect styles)))) 
         (when ispec 
      (message "Footnote style in effect: %s"   style-in-effect))) 
      style-in-effect)) 
(defun footnote-init-markers (&optional ispec) 
   " " 
   (interactive "p") 
   (save-excursion 
      (let ((count 0) 
         (footnote-section-tag-pos 
           (or 
            (search-forward footnote-section-tag nil t 1) 
            (re-search-forward footnote-section-tag-regexp nil t 1)))) 
         (goto-char (point-min)) 
         (when footnote-section-tag-pos 
      (while (re-search-forward (concat footnote-start-tag footnote-regexp 
footnote-end-tag) footnote-section-tag-pos t 1) 
         (setq count (1+ count)) 
         (Footnote-insert-pointer-marker count (point))) 
      (setq count 0) 
      (while (re-search-forward (concat footnote-start-tag footnote-regexp 
footnote-end-tag) nil t 1) 
         (setq count (1+ count)) 
         (Footnote-insert-text-marker count (point))) 
      (when ispec 
         (message "footnote-pointer-marker-alist: 
%s\nfootnote-text-marker-alist:      %s" footnote-pointer-marker-alist 
footnote-text-marker-alist)) 
      footnote-pointer-marker-alist 
      footnote-text-marker-alist)))) 
(provide 'footnote-init) 
;;; footnote-init.el ends here 