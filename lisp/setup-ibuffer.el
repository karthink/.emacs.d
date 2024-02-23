;; -*- lexical-binding: t; -*-

(use-package ibuffer
  :defer t
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)
  (add-to-list 'ibuffer-help-buffer-modes 'Man-mode)
  ;; (setq ibuffer-display-summary nil)
  ;; (setq ibuffer-use-other-window nil)
  ;; (setq ibuffer-movement-cycle nil)
  ;; (setq ibuffer-default-shrink-to-minimum-size t)
  ;; (setq ibuffer-saved-filter-groups nil)

  (defun ibuffer-visit-buffer-other-window (&optional noselect)
  "In ibuffer, visit this buffer. If `ace-window' is available, use
it to select window for visiting this buffer.`"
  (interactive "P")
  (let ((buf (ibuffer-current-buffer t))
        (window 
         (if (fboundp 'aw-select)
             (aw-select "Select Window")
           (next-window))))
    (bury-buffer (current-buffer))
    (if noselect
        (save-window-excursion (select-window window)
                               (switch-to-buffer buf))
      (select-window window) (switch-to-buffer buf))))
  
  (defun my/buffers-major-mode (&optional arg)
    "Select buffers that match the current buffer's major mode.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion."
    (interactive "P")
    (let* ((major major-mode)
           (prompt "Buffers for ")
           (mode-string (format "%s" major))
           (mode-string-pretty (propertize mode-string 'face 'success)))
      (if arg
          (ibuffer t (concat "*" prompt mode-string "*")
                   (list (cons 'used-mode major)))
        (switch-to-buffer
         (read-buffer
          (concat prompt mode-string-pretty ": ") nil t
          (lambda (pair) ; pair is (name-string . buffer-object)
            (with-current-buffer (cdr pair) (derived-mode-p major))))))))

  (defun my/buffers-vc-root (&optional arg)
    "Select buffers that match the present `vc-root-dir'.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion.

When no VC root is available, use standard `switch-to-buffer'."
    (interactive "P")
    (let* ((root (vc-root-dir))
           (prompt "Buffers for VC ")
           (vc-string (format "%s" root))
           (vc-string-pretty (propertize vc-string 'face 'success)))
      (if root
          (if arg
              (ibuffer t (concat "*" prompt vc-string "*")
                       (list (cons 'filename (expand-file-name root))))
            (switch-to-buffer
             (read-buffer
              (concat prompt vc-string-pretty ": ") nil t
              (lambda (pair) ; pair is (name-string . buffer-object)
                (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
        (call-interactively 'switch-to-buffer))))

  :hook ((ibuffer-mode . hl-line-mode)
         ;; (ibuffer-mode . my/ibuffer-project-generate-filter-groups)
         ;; (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root)
         )
  :bind (("C-x C-b" . ibuffer)
         ("M-s b" . my/buffers-major-mode)
         ("M-s v" . my/buffers-vc-root)
         :map ibuffer-mode-map
         ("M-o" . nil)))

(use-package ibuffer-project
  :ensure t
  :after (ibuffer project)
  :hook ((ibuffer ibuffer-mode) . my/ibuffer-project-generate-filter-groups)
  :config
  (setq ibuffer-project-use-cache t
        ibuffer-project-root-functions
        '(((lambda (dir)
             (project-root (project-current nil dir))) . "Project")))
  (defun my/ibuffer-project-generate-filter-groups ()
    (setq ibuffer-filter-groups
          (ibuffer-project-generate-filter-groups))))

;; Superceded by =ibuffer-project=, since it covers vc repositories and more
;; besides.
(use-package ibuffer-vc
  :disabled
  :after (ibuffer vc)
  :bind
  (:map ibuffer-mode-map
   ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
   ("/ <backspace>" . ibuffer-clear-filter-groups)))

(provide 'setup-ibuffer)
;;; setup-ibuffer.el ends here
