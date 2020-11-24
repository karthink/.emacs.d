;; Make org latex previews be consistent with the Emacs theme. This will
;; generate a separate set of images for each theme used, based on the theme's
;; background color.

(defun my/org-latex-set-options ()
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

(defvar my/org-latex-latex-preview-has-been-called nil
  "Tracks if org-latex-preview has ever been called (updated locally).")

(defadvice org-latex-preview (before my/latex-fragments-advice activate)
  "Keep Org LaTeX fragments in a directory with background color name."
  (if (not my/org-latex-latex-preview-has-been-called) (my/org-latex-set-options))
  (setq-local my/org-latex-latex-preview-has-been-called t)
  (my/org-latex-set-directory-name-to-background))

;; (defadvice load-theme (after my/load-theme-advice-for-latex activate)
;;   "Conditionally update Org LaTeX fragments for current background."
;;   (if my/org-latex-latex-preview-has-been-called (my/org-latex-update-fragments-for-background)))

;; (defadvice disable-theme (after my/disable-theme-advice-for-latex activate)
;;   "Conditionally update Org LaTeX fragments for current background."
;;   (if my/org-latex-latex-preview-has-been-called (my/org-latex-update-fragments-for-background)))

(defun my/org-latex-set-directory-name-to-background ()
  "Set Org LaTeX directory name to background color name: c_Red_Green_Blue."
  (setq org-preview-latex-image-directory
        (concat "ltximg/c"
                (let ((color (color-values (alist-get 'background-color (frame-parameters)))))
                  (apply 'concat (mapcar (lambda (x) (concat "_" x)) (mapcar 'int-to-string color))))
                "/")))

;; (defun my/org-latex-update-fragments-for-background ()
;;   "Remove Org LaTeX fragment layout, switch directory for background, turn fragments back on."
;;   ;; removes latex overlays in the whole buffer
;;   (org-clear-latex-preview)

;;   ;; background directory switch
;;   (my/org-latex-set-directory-name-to-background)

;;   ;; recreate overlay
;;   ;; Argument '(16) is same as prefix C-u C-u,
;;   ;; means create images in the whole buffer instead of just the current section.
;;   ;; For many new images this will take time.
;;   ;; (org-toggle-latex-fragment '(16)))
;; )

(provide 'themed-ltximg)
