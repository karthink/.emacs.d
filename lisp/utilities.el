;; UTILITIES FOR MISCELLANEOUS TASKS  -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------
;; PRINT ASCII TABLE
;;----------------------------------------------------------------------
(defun ascii-table ()
  "Display basic ASCII table (0 thru 127)"
  (interactive)
  (pop-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion
    (let ((i -1))
      (insert "ASCII characters 0 thru 127.\n\n")
      (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
      (while (< i 31)
        (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                        (setq i (+ 1  i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)))
        (setq i (- i 96)))))
  (special-mode))

;;----------------------------------------------------------------
;; ** QRENCODE
;;----------------------------------------------------------------
(use-package qrencode :ensure t :defer)


;;----------------------------------------------------------------
;; ** TEMPORARY BUFFERS and INDIRECT EDITING
;;----------------------------------------------------------------
(use-package scratch
  :ensure t
  :config
  (defun my/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (unless (derived-mode-p
             'text-mode 'prog-mode 'conf-mode 'tex-mode)
      (condition-case nil
          (let ((pick
                 (read-multiple-choice
                  "Switch major mode?"
                  '((?o "org") (?m "markdown")
                    (?l "lisp-interaction") (?e "elisp")
                    (?  "Continue")))))
            (pcase (car pick)
              (?o (org-mode)) (?m (markdown-mode))
              (?l (lisp-interaction-mode)) (?e (emacs-lisp-mode)))
            (read-only-mode 0))
        (quit nil)))
    (let* ((mode major-mode))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  (setf (alist-get "\\*Scratch for" display-buffer-alist nil nil #'equal)
        '((display-buffer-same-window)))
  :hook (scratch-create-buffer . my/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

(use-package edit-indirect
  :ensure t
  :bind ( :map mode-specific-map
          ("'" . my/edit-indirect-region)
          :map edit-indirect-mode-map
          ("C-c C-c" . nil))
  :config
  (setq edit-indirect-guess-mode-function
        'my/edit-indirect-guess-mode-function)
  (defun my/edit-indirect-region (rb re disp)
    (interactive (list (region-beginning) (region-end) t))
    (edit-indirect-region rb re disp))
  (defun my/edit-indirect-guess-mode-function (parent rb re)
    "Heuristic to set major mode when using edit-indirect"
    (let ((pmode (buffer-local-value 'major-mode parent)))
      (cond
       ((with-current-buffer parent
          (save-excursion
            (goto-char rb)
            (skip-chars-forward "\n\r\t ")
            (memq (char-after) '(40 59))))
        (lisp-interaction-mode))
       ((provided-mode-derived-p pmode 'message-mode) (org-mode))
       (t (normal-mode))))))

;;----------------------------------------------------------------
;; ** STICKER
;;----------------------------------------------------------------
(use-package emacs
  :bind ("C-c p" . sticker)
  :config
  (defvar sticker-list nil)

  (defun sticker-next (&optional arg)
    (interactive "p")
    (let ((func (if (< arg 0)
                    'previous-single-char-property-change
                  'next-single-char-property-change)))
      (dotimes (_ (abs arg))
        (goto-char (funcall func (point) 'sticker))
        (forward-line 0))))

  (add-hook 'poi-functions #'sticker-next)

  (defun sticker (&optional arg label)
    (interactive "P")
    (cl-flet ((store-sticker (beg end)
                (or (assoc (buffer-substring beg end) sticker-list)
                    (push (list (buffer-substring beg end)) sticker-list))
                (deactivate-mark)
                (message "Sticker stored"))
              (new-sticker (l &optional o)
                (unless o
                  (setq o (make-overlay (line-beginning-position)
                                        (line-end-position))))
                (overlay-put o 'evaporate t)
                (overlay-put o 'sticker l)
                (overlay-put
                 o 'after-string
                 (concat " " (if (get-text-property 0 'face l)
                                 l
                               (propertize l 'face '(:inverse-video t)))))
                (push o (alist-get l sticker-list nil nil #'equal)))
              (read-label ()
                (let ((minibuffer-allow-text-properties t))
                  (completing-read "Attach sticker: " sticker-list))))
      (if (use-region-p)
          (store-sticker (region-beginning) (region-end))
        (if-let* ((exist-ov (cdr-safe (get-char-property-and-overlay (point) 'sticker))))
            (if arg
                (new-sticker (read-label) exist-ov)
              (let* ((old-l (overlay-get exist-ov 'sticker))
                     (matches (assoc old-l sticker-list)))
                (delq exist-ov matches)
                (when (= (length matches) 1)
                  (setq sticker-list (delete matches sticker-list)))
                (delete-overlay exist-ov)))
          (new-sticker (or label (read-label))))))))

;;;----------------------------------------------------------------
;; ** STROKES
;;;----------------------------------------------------------------
(use-package strokes
  :diminish 'stokes-mode
  :bind ("<down-mouse-9>" . strokes-do-stroke)
  :config
  (setq strokes-file (expand-file-name "strokes" user-cache-directory))
  (setq strokes-use-strokes-buffer t))



(provide 'utilities)
