;;; setup-publish.el --- Config for publishing from Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: hypermedia

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

;;;----------------------------------------------------------------
;; ** OX-HUGO
;;;----------------------------------------------------------------
(use-package ox-hugo
  :ensure t
  :defer
  :config
  (advice-add 'org-blackfriday--update-ltximg-path
              :around
              (lambda (orig-fn html-string)
                (if (plist-get org-html-latex-image-options :inline)
                    html-string
                  (funcall orig-fn html-string)))
              '((name . inline-image-workaround)))
  (use-package org-glossary
    :ensure (:host github :repo "tecosaur/org-glossary")
    :config
    (setq org-glossary-toplevel-only nil)
    (org-glossary-set-export-spec 'hugo t
      :use "<a class=\"org-gls\" href=\"#gls-%K\">%t</a>"
      :definition "<a name=\"gls-%K\">%t</a>"
      :definition-structure "%d\n\\colon{} %v [%n uses]\n")
    (org-glossary-set-export-spec 'hugo 'glossary
      :heading "*** Glossary")
    (defun org-glossary--expand-print-keyword (backend terms keyword)
      "Call `org-glossary--expand-print' with paramaters and terms based on KEYWORD.
BACKEND is passed through unmodified, but TERMS may be modified depending on
the :consume parameter extracted from KEYWORD."
      (let ((heading (org-element-lineage keyword '(headline org-data)))
            (parameters (org-combine-plists
                         org-glossary-default-print-parameters
                         (org-glossary--parse-print-keyword-value
                          (org-element-property :value keyword)))))
        (while (and heading
                    (not (eq (org-element-type heading) 'org-data))
                    (> (org-element-property :level heading)
                       (plist-get parameters :level)))
          (setq heading (org-element-lineage heading '(headline org-data))))
        (org-glossary--expand-print
         backend
         (org-glossary--extract-uses-in-region
          terms
          (if heading (org-element-property :begin heading) (point-min))
          (if heading (org-element-property :end heading) (point-max))
          (plist-get parameters :type)
          (plist-get parameters :consume))
         parameters))))
  (setq org-hugo-section "blog")
  (add-to-list 'org-hugo-special-block-type-properties
               '("sidenote" . (:trim-pre t :trim-post t)))
  (setq org-hugo-paired-shortcodes "%sidenote")
  (define-minor-mode my/org-hugo-mode
    "Helper mode for org-hugo previews."
    :keymap (make-sparse-keymap)
    :init-value nil)
  (defun my/org-hugo-preview (&optional arg)
    (interactive "P")
    (pcase-let* ((sec nil)
                 (`(,sec . ,title)
                 (save-excursion
                   (org-previous-visible-heading 1)
                   (let ((title (org-element-property
                                 :EXPORT_FILE_NAME
                                 (org-element-at-point))))
                     (while (and (not sec) (org-up-heading-safe))
                       (setq sec (org-element-property
                                  :EXPORT_HUGO_SECTION
                                  (org-element-at-point))
                             title (or title
                                       (org-element-property
                                        :EXPORT_FILE_NAME
                                        (org-element-at-point)))))
                     (cons sec title)))))
      (if-let ((_ title)
               (url (concat
                     "http://localhost:1313/"
                     (if sec (downcase sec) "software")
                     "/" title)))
          (progn (save-buffer)
                 (unless (bound-and-true-p org-hugo-auto-export-mode)
                   (org-hugo-export-wim-to-md))
                 (if arg
                     (progn (other-window 1) (eww url) (other-window -1))
                   (browse-url url)))
        (message "No preview url found.")))))

;;;----------------------------------------------------------------
;; ** ORG-REVEAL
;;;----------------------------------------------------------------
(use-package org-re-reveal
  ;; :disabled
  :ensure t
  :after ox
  :commands (org-re-reveal-export-to-html
             org-re-reveal-export-to-html-and-browse)
  :config
  (setq org-re-reveal-subtree-with-title-slide t)
  (pcase-dolist (`(,key ,expansion)
                 '(("col" "col") ("row" "row")
                   ("card" "card") ("cell" "cell")))
    (setf (alist-get key org-structure-template-alist
                     nil nil #'equal)
          expansion))
  (pcase-dolist (`(,key ,expansion) '(("rh" "reveal_html")
                                      ("R" "reveal")
                                      ("ar" "attr_reveal")))
    (setf (alist-get key org-tempo-keywords-alist
                     nil nil #'equal)
          expansion))
  (use-package org-re-reveal-ref
    :disabled
    :init (setq org-ref-default-bibliography '("~/Documents/research/control_systems.bib"))))

(use-package org-re-reveal-citeproc
  :ensure t
  :after org-re-reveal)

(use-package ox-reveal
  :disabled
  :ensure t
  :init
  (setq org-reveal-root "file:///home/karthik/.local/share/git/reveal.js")
  (setq org-reveal-hlevel 2))


;;;----------------------------------------------------------------
;; ** MY ORG PROJECTS
;;;----------------------------------------------------------------
(use-package ox-publish
  :defer
  :config
  (setq org-publish-project-alist
        '(
          ("dai-wiki"
           :base-directory "~/Documents/abode/dai/"
           :base-extension "org"
           :publishing-directory "~/Documents/abode/dai"
           :remote-directory "root@abode.karthinks.com:/var/www/abode/dai"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4           ; Just the default for this project.
           :auto-preamble t
           :completion-function (list my/org-publish-rsync)
           )
          ("dai" :components ("dai-wiki"))
          ("cyclostationarity"
           :base-directory "~/Dropbox/KarthikBassam/Cyclostationarity"
           :base-extension "org"
           :publishing-directory "~/Dropbox/KarthikBassam/Cyclostationarity"
           :remote-directory "root@abode.karthinks.com:/var/www/abode/cyclostationarity"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :auto-preamble t
           :completion-function (list my/org-publish-rsync-html-and-figures-only)
           )
          ("abode"
           :base-directory "~/Documents/abode/"
           :base-extension "org"
           :publishing-directory "~/Documents/abode"
           :remote-directory "root@abode.karthinks.com:/var/www/abode"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :auto-preamblle t
           :completion-function (list my/org-publish-rsync)
           )
          ("sicm"
           :base-directory "~/Documents/courses/sicm/"
           :base-extension "org"
           :publishing-directory "~/Documents/abode/sicm"
           :remote-directory "root@abode.karthinks.com:/var/www/abode/sicm"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-level 5
           :auto-preamble t
           :completion-function (list my/org-publish-rsync-html-and-figures-only))
          )
        )

  (defun my/org-publish-rsync (project-plist)
    "Sync output of org project to a remote server using RSYNC.

 All files and folders except for ORG files will be synced."
    (if (executable-find "rsync")
        (let* ((basedir (expand-file-name
                         (file-name-as-directory
                          (plist-get project-plist :publishing-directory))))
               (destdir (plist-get project-plist :remote-directory)))
          (start-process "rsync-project-abode" "*project-abode-output*"
                         "rsync" "-a" "-v" "--exclude=*.org" "--delete"
                         basedir destdir))

      (display-warning 'org-publish
                       "Could not find RSYNC in PATH. Project not uploaded to server."
                       :warning)))

  (defun my/org-publish-rsync-html-and-figures-only (project-plist)
    "Sync output of org project to a remote server using RSYNC.

 All files and folders except for ORG files will be synced."
    (if (executable-find "rsync")
        (let* ((basedir (expand-file-name
                         (file-name-as-directory
                          (plist-get project-plist :publishing-directory))))
               (destdir (plist-get project-plist :remote-directory)))
          (message "Running rsync: %s → %s" basedir destdir)
          (start-process "rsync-project-html-and-figures" "*project-rsync-html-output*"
                         "rsync" "-a" "-v"
                         "--include=*.png"
                         "--include=*.html"
                         "--include=/figures/***"
                         "--exclude=*"
                         "--delete"
                         basedir destdir))

      (display-warning 'org-publish
                       "Could not find RSYNC in PATH. Project not uploaded to server."
                       :warning))))

(provide 'setup-publish)
;;; setup-publish.el ends here
