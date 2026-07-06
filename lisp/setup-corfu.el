;; -*- lexical-binding: t; -*-

(use-package completion-preview
  :bind ( :map completion-preview-active-mode-map
          ("M-n" . #'completion-preview-next-candidate)
          ("M-p" . #'completion-preview-prev-candidate))
  :hook (((prog-mode text-mode tex-mode ielm-mode) . completion-preview-mode)
         (minibuffer-setup . my/completion-preview-in-minibuffer))
  :config
  (setq completion-preview-minimum-symbol-length 3
        completion-preview-message-format nil
        completion-preview-sort-function #'minibuffer-sort-by-history)
  (dolist (cmd '(org-self-insert-command org-delete-backward-char))
    (add-to-list 'completion-preview-commands cmd))

  (defun my/completion-preview-in-minibuffer ()
    "Enable Completion Preview in the minibuffer if Vertico is not active."
    (unless (or (bound-and-true-p vertico--input)
                (memq this-command '(org-ql-find))
                (memq (current-local-map) (list read-passwd-map)))
      (completion-preview-mode 1)))

  ;; From https://github.com/agzam/.doom.d/blob/main/modules/custom/completion/config.el
  (defvar completion-preview-echo-max 5
    "Maximum number of completion-preview candidates shown per echo-area page.")
  (defvar completion-preview--echo-shown nil
  "Non-nil while the echo-area candidate list is on screen.")
  (defface completion-preview-echo-number '((t :foreground "orange"))
    "Face for the index number shown before each echo-list candidate."
    :group 'completion-preview)
  (defvar completion-preview-echo-number-height 1.1
    "Height multiplier applied to the superscript echo-list index numbers.")
  (defconst completion-preview--superscripts ["⁰" "¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹"]
    "Superscript glyphs for digits 0-9.")

  (defun completion-preview--superscript (n)
    "Return the natural number N rendered with superscript digits."
    (mapconcat (lambda (c) (aref completion-preview--superscripts (- c ?0)))
               (number-to-string n) ""))

  (defun completion-preview--echo-string ()
    "Return the echo string for the current preview page, or nil when inactive.a
Shows the page of up to `completion-preview-echo-max' candidates containing
the current one (highlighted).  Each candidate is prefixed with its 1-based
on-page index as an orange superscript (the key that inserts it, M-1..M-N),
and a leading/trailing arrow marks more candidates before/after the page."
    (when (bound-and-true-p completion-preview--overlay)
      (let* ((ov completion-preview--overlay)
             (common (or (overlay-get ov 'completion-preview-common) ""))
             (sufs (overlay-get ov 'completion-preview-suffixes))
             (idx (or (overlay-get ov 'completion-preview-index) 0))
             (total (length sufs))
             (size completion-preview-echo-max)
             (start (* (/ idx size) size))
             (end (min total (+ start size)))
             (cands (cl-loop for i from start below end
                             for num = (propertize
                                        (completion-preview--superscript (1+ (- i start)))
                                        'face `((:height ,completion-preview-echo-number-height)
                                                completion-preview-echo-number))
                             for cand = (substring-no-properties
                                         (concat common (nth i sufs)))
                             collect (concat num (if (= i idx)
                                                     (propertize cand 'face 'highlight)
                                                   cand)))))
        (concat (and (< 0 start) "← ")
                (mapconcat #'identity cands "  ")
                (and (< end total) " →")))))

  (defun completion-preview-echo-candidates (&rest _)
    "Echo the current page of completion-preview candidates."
    (if-let* ((str (completion-preview--echo-string)))
        (progn
          (setq completion-preview--echo-shown t)
          (let ((message-log-max nil)) (message "%s" str)))
      (completion-preview-echo-clear)))

  (defun completion-preview-echo-clear (&rest _)
    "Clear the echoed candidate list once the preview is gone."
    (when (and completion-preview--echo-shown
               (not (bound-and-true-p completion-preview--overlay)))
      (setq completion-preview--echo-shown nil)
      (let ((message-log-max nil)) (message nil))))

  (defun completion-preview-insert-indexed (n)
    "Complete with the Nth (1-based) candidate of the visible echo page.
The inline-preview analog of `+corfu-insert-indexed': one press inserts.
Mirrors `completion-preview-insert' (which inserts the shown text and runs
the capf :exit-function) but targets the chosen index directly."
    (when (bound-and-true-p completion-preview--overlay)
      (let* ((ov completion-preview--overlay)
             (base (or (overlay-get ov 'completion-preview-base) ""))
             (beg (overlay-get ov 'completion-preview-beg))
             (end (overlay-get ov 'completion-preview-end))
             (sufs (overlay-get ov 'completion-preview-suffixes))
             (common (or (overlay-get ov 'completion-preview-common) ""))
             (idx (or (overlay-get ov 'completion-preview-index) 0))
             (efn (plist-get (overlay-get ov 'completion-preview-props) :exit-function))
             (size completion-preview-echo-max)
             (target (+ (* (/ idx size) size) (1- n))))
        (when (< target (length sufs))
          (let* ((cand (concat common (nth target sufs)))
                 (skip (- end beg))
                 (visible (if (<= 0 skip (length cand)) (substring cand skip) "")))
            (completion-preview-active-mode -1)
            (goto-char end)
            (insert-and-inherit visible)
            (when (functionp efn)
              (funcall efn (concat base cand) 'finished)))))))

  (defun completion-preview-next-candidate-guard-a (orig &rest args)
    "Hide the preview instead of throwing if cycling hits a stale overlay.
`completion-preview-next-candidate' runs an unguarded `buffer-substring' on
the overlay's stored integer positions; they go stale in buffers rewritten
under it (e.g. eca-chat streaming)."
    (condition-case nil
        (apply orig args)
      (args-out-of-range
       (when (bound-and-true-p completion-preview-active-mode)
         (completion-preview-active-mode -1)))))

  ;; M+number completes with the Nth candidate of the visible page, like
  ;; `+corfu-insert-indexed'.  Bound only here, so `digit-argument' stays
  ;; intact when no preview is shown.
  (defun completion-preview-echo-key-setup ()
    (cond
     (completion-preview-echo-mode
      (keymap-set completion-preview-active-mode-map
                  "M-1" (lambda () (interactive)
                          (completion-preview-insert-indexed 1)))
      (keymap-set completion-preview-active-mode-map
                  "M-2" (lambda () (interactive)
                          (completion-preview-insert-indexed 2)))
      (keymap-set completion-preview-active-mode-map
                  "M-3" (lambda () (interactive)
                          (completion-preview-insert-indexed 3)))
      (keymap-set completion-preview-active-mode-map
                  "M-4" (lambda () (interactive)
                          (completion-preview-insert-indexed 4)))
      (keymap-set completion-preview-active-mode-map
                  "M-5" (lambda () (interactive)
                          (completion-preview-insert-indexed 5))))
     (t (dolist (key '("M-1" "M-2" "M-3" "M-4" "M-5"))
          (keymap-unset completion-preview-active-mode-map key)))))

  (define-minor-mode completion-preview-echo-mode
    "Show completion previews in the echo area."
    :global t
    :lighter ""
    (unless completion-preview-mode
      (message "`completion-preview-echo-mode' requires `completion-preview-mode' \
to be active.")
      (setq completion-preview-echo-mode nil))
    (cond
     (completion-preview-echo-mode
      ;; Popup-less candidate list: echo the current page of candidates.
      ;; `completion-preview--update' is the per-keystroke convergence point;
      ;; guard it since it is a private symbol.
      (completion-preview-echo-key-setup)
      (when (fboundp 'completion-preview--update)
        (advice-add 'completion-preview--update :after
                    #'completion-preview-echo-candidates))
      (advice-add 'completion-preview-next-candidate :after
                  #'completion-preview-echo-candidates)
      (advice-add 'completion-preview-active-mode :after
                  #'completion-preview-echo-clear)
      (advice-add 'completion-preview-next-candidate :around
                  #'completion-preview-next-candidate-guard-a))
     (t (completion-preview-echo-key-setup)
        (advice-remove 'completion-preview--update #'completion-preview-echo-candidates)
        (advice-remove 'completion-preview-next-candidate #'completion-preview-echo-candidates)
        (advice-remove 'completion-preview-active-mode #'completion-preview-echo-clear)
        (advice-remove 'completion-preview-next-candidate
                       #'completion-preview-next-candidate-guard-a)))))

(use-package corfu
  :ensure (:host github :repo "minad/corfu")
  :hook (;; ((prog-mode text-mode tex-mode ielm-mode) . corfu-mode)
         ((shell-mode eshell-mode) . my/corfu-shell-settings)
         ;; (minibuffer-setup . my/corfu-enable-always-in-minibuffer)
         )
  :bind ( :map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ("RET" . nil)
          ("M-RET" . corfu-insert)
          ("M-." . corfu-show-location)
          ("M-h" . nil)
          ([remap next-line] . nil)
          ([remap previous-line] . nil)
          ("M-." . corfu-info-location)
          ("C-h" . corfu-info-documentation))
  :config
  (setq corfu-auto-prefix 4
        corfu-auto-delay 0.07
        corfu-count 8
        corfu-auto  t
        corfu-cycle t
        corfu-quit-no-match 'separator
        corfu-preselect 'prompt
        corfu-scroll-margin 5)

  ;; Extensions
  (use-package corfu-info
    :bind (:map corfu-map ("M-g" . nil)))
  (use-package corfu-history :defer 3 :config (corfu-history-mode 1))
  (use-package corfu-popupinfo
    :bind ( :map corfu-map
            ([remap corfu-info-documentation] . corfu-popupinfo-toggle))
    :config
    (setq corfu-popupinfo-hide nil
          corfu-popupinfo-delay '(2 . 0.2))
    (corfu-popupinfo-mode 1))
  (use-package corfu-quick
    :bind (:map corfu-map ("'" . corfu-quick-complete))
    :config (setq corfu-quick1 "asdfghjkl;"))

  ;; Corfu in the minibuffer
  (defvar my-corfu-minibuffer-exclude-modes (list read-passwd-map)
    "Minibuffer-local keymaps for which Corfu should be disabled.")
  (defvar my-corfu-minibuffer-exclude-commands
    '(org-ql-find)
    "Minibuffer commands for which Corfu should be disabled.")
  (defun my/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (or (bound-and-true-p vertico--input)
                (memq this-command my-corfu-minibuffer-exclude-commands)
                (memq (current-local-map)
                      my-corfu-minibuffer-exclude-modes))
      (if (and (not (display-graphic-p))
               (fboundp 'consult-completion-in-region))
          (setq-local completion-in-region-function
                      #'consult-completion-in-region)
        ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
        (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                    corfu-popupinfo-delay nil)
        (corfu-mode 1))))

  (use-package consult
    :bind ( :map corfu-map
            ("M-m" . corfu-move-to-minibuffer)
            ("C-<tab>" . corfu-move-to-minibuffer))
    :config
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
         (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
           (consult-completion-in-region beg end table pred)))))
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

  ;; Corfu in the shell
  (defun my/corfu-shell-settings ()
    (setq-local corfu-quit-no-match t
                corfu-auto nil)
    (setq-local corfu-map (copy-keymap corfu-map)
                completion-cycle-threshold nil)
    (define-key corfu-map "\r" #'corfu-insert-and-send)
    (corfu-mode))
  (defun corfu-insert-and-send ()
    (interactive)
    ;; 1. First insert the completed candidate
    (corfu-insert)
    ;; 2. Send the entire prompt input to the shell
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((derived-mode-p 'comint-mode)
      (comint-send-input))))

  ;; Faster corfu
  ;; Disabled -- interferes with dynamic completion tables
  (use-package orderless
    :disabled
    :after orderless
    :hook (corfu-mode . my/corfu-comp-style)
    :config
    (defun my/corfu-comp-style ()
      "Set/unset a fast completion style for corfu"
      (if corfu-mode
          (setq-local completion-styles '(orderless-fast))
        (kill-local-variable 'completion-styles)))))

(use-package kind-icon
  :disabled
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  ;; (setq nerd-icons-corfu-mapping
  ;;       '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
  ;;         (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
  ;;         ;; ...
  ;;         (t :style "cod" :icon "code" :face font-lock-warning-face)))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :after (corfu orderless)
  :commands my/toggle-writing-capf
  :defines my/toggle-writing-capf
  :bind (("C-$" . cape-dict)
         ("C-S-f" . cape-file)
         ("C-M-/" . cape-dabbrev)
         :map corfu-map
         ("M-/" . cape-dabbrev)
         ("C-x C-f" . cape-file))
  ;; :hook ((text-mode conf-mode) . my/text-mode-capfs)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-file 85)
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev 90)
  :config
  (defvar my/writing-capf (cape-capf-super #'cape-dabbrev #'cape-dict))
  (defun my/toggle-writing-capf ()
    "Turn on CAPFs suitable for writing."
    (interactive)
    (if (memq my/writing-capf completion-at-point-functions)
        (progn
          (kill-local-variable 'corfu-count)
          (kill-local-variable 'corfu-auto-prefix)
          (kill-local-variable 'completion-styles)
          (remove-hook 'completion-at-point-functions
                       my/writing-capf 'local)
          (message "Turned off writing CAPFs."))
      (setq-local corfu-count 5 corfu-auto-prefix 5)
      (add-hook 'completion-at-point-functions
                my/writing-capf nil 'local)
      (setq-local completion-styles '(basic emacs22))
      (message "Turned on writing CAPFs.")))

  ;; Cape-dict settings

  ;; Use Peter Norvig's 300,000 most used English words as the word list
  (let ((wordlist (expand-file-name
                   "wordlist.txt" user-cache-directory)))
    (setq cape-dict-file
          (cond
           ((or (file-readable-p wordlist)
                (and (url-copy-file "https://norvig.com/ngrams/count_1w.txt"
                                    wordlist)
                     (shell-command
                      (concat "sed -re 's_[ \t0-9]*__g' -e '/^.{1,4}$/d' -i "
                              wordlist))))
            (message "Downloaded wordlist")
            wordlist)
           ((getenv "WORDLIST"))
           (t "/usr/share/dict/words"))))
  (setq ispell-alternate-dictionary cape-dict-file)
  (when (bound-and-true-p text-mode-ispell-word-completion)
    (setq text-mode-ispell-word-completion nil))

  (setf cape-dict-limit 4
        (alist-get 'cape-dict completion-category-defaults)
        '((styles basic)))

  ;; Cape-dabbrev settings
  (defun my/text-mode-capfs ()
    (add-hook 'completion-at-point-functions 'cape-dabbrev 85 t))
  (setf cape-dabbrev-check-other-buffers nil
        (alist-get 'cape-dabbrev completion-category-defaults)
        '((styles orderless-fast)))

  (when (< emacs-major-version 29)
    (use-package pcomplete
      :defer
      :config
      ;; Silence the pcomplete capf, no errors or messages!
      (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
      ;; Ensure that pcomplete does not write to the buffer
      ;; and behaves as a pure `completion-at-point-function'.
      (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))))

(provide 'setup-corfu)
;;; setup-corfu.el ends here
