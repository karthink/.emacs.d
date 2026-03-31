;; -*- lexical-binding: t; -*-

(use-package google
  :when IS-LINUX
  :config
  (require 'prodfs)
  (prodfs-enable-automatic-start)
  (prodfs-enable-file-handler))

(use-package org-node
  :defer t
  :config
  (require 'org-node)
  (defvar waymo/abbreviations nil)

  ;; TODO: thing-at-pt might be too slow, get the symbol directly with skip-chars?
  (defun waymo/update-abbreviations ()
    (if-let* ((entry (gethash "Waymo Terms and Concepts"
                              org-node--candidate<>entry))
              (id (org-mem-entry-id entry)))
        (let* ((file (or (gethash id org-id-locations)
                         (let ((entry (org-mem-entry-by-id id)))
                           (and entry (org-mem-entry-file-truename entry)))
                         (error "Abbreviations Update: unknown ID: %s" id)))
               (buffer
                (or (find-buffer-visiting file)
                    (and (file-exists-p file)
                         (find-file-noselect file))
                    (error "Abbreviations Update: File not on disk nor visited by any buffer: %s"
                           file)))
               (pos))
          (with-current-buffer buffer
            (save-excursion
              (without-restriction
                (setq pos (or (org-find-property "ID" id)
                              (error "Abbreviations Update: Could not find ID \"%s\" in buffer %s"
                                     id buffer)))
                (goto-char pos)
                (org-map-entries
                 (lambda ()
                   (when (looking-at org-complex-heading-regexp)
                     (goto-char (match-beginning 4))
                     (when-let* ((word (thing-at-point 'symbol))
                                 (_ (string-match "[A-Z]+" word)))
                       (setf (alist-get word waymo/abbreviations
                                        nil nil #'string-equal)
                             (progn (forward-symbol 1)
                                    (skip-syntax-forward ".-")
                                    (buffer-substring-no-properties
                                     (point) (line-end-position)))))))))))
          (let ((inhibit-message t))
            (message "Updated waymo/abbreviations")))
      (user-error "Abbreviations Update: could not find entry \"Waymo Terms and Concepts\"!")))

  (defun waymo/find-abbreviations (callback)
    (when waymo/abbreviations
      (when-let* ((word (thing-at-point 'symbol))
                  (_ (string-match-p "[A-Z]+" word))
                  (abbr (assoc word waymo/abbreviations)))
        (funcall callback (concat (car abbr) ": " (cdr abbr))))))

  (defun waymo/org-mode-setup ()
    (add-hook 'eldoc-documentation-functions
              'waymo/find-abbreviations nil t)
    (bug-reference-mode 1)
    (unless waymo/abbreviations (waymo/update-abbreviations))
    (eldoc-mode 1)))

(use-package org
  :defer
  :config
  (defun my/org-https-preview-handler (ov path node) t)

  (defun my/org-https-gdoc-handler (ov path _node)
    (when (string-prefix-p "//docs.google.com" path)
      (overlay-put ov 'before-string ;; "🖺"
                   (propertize "." 'display (list 'image :type 'png
                                                  :file "~/Documents/roam/gdocs-logo.png"
                                                  :height (cons 1.0 'em)
                                                  :ascent 'center)))))

  (add-function :before (symbol-function 'my/org-https-preview-handler)
                #'my/org-https-gdoc-handler)

  (org-link-set-parameters "https" :preview #'my/org-https-preview-handler))

(provide 'setup-waymo)
