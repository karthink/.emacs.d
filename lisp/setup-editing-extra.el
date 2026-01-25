;;; setup-editing-extra.el --- Advanced editing configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

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

;; Navigation and editing

;;; Code:

;;;; Navigating to errors
(use-package simple
  :if (version<= emacs-version "28.0")
  :bind (("M-g n" . my/next-error)
         ("M-g p" . my/next-error)
         ;; ("M-n" . next-error)
         ;; ("M-p" . next-error)
         )
  :config
  (defun my/next-error (&optional arg reset)
    "`next-error' with easier cycling through errors."
    (interactive "P")
    (let* ((ev last-command-event)
           (echo-keystrokes nil)
           (num (pcase ev
                  (?p -1)
                  (_  1))))
      (next-error (if arg (* arg num) num)
                  reset)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "n") 'my/next-error)
         (define-key map (kbd "p") 'my/next-error)
         map)))))

(use-package simple
  :if (> emacs-major-version 27)
  :hook (next-error . recenter)
  :config
  (setq next-error-message-highlight t
        next-error-found-function #'next-error-quit-window))

(use-package poi
  :demand t
  :bind (:map next-error-repeat-map
         ("`" . poi-action))
  :config
  (advice-add 'next-error :around #'poi-delegate))

;;;; Better repeat-mode
(use-package repeat-help
  :ensure ( :host github :protocol ssh
            :repo "karthink/repeat-help")
  ;; :load-path "plugins/repeat-help/"
  :hook (repeat-mode . repeat-help-mode)
  :config
  (setq repeat-help-key "<f1>"
        repeat-help-popup-type 'embark))

(use-package repeat-help
  :disabled                             ;doesn't work with repeat-help yet
  :config
  ;; From JDTSmith https://gist.github.com/jdtsmith/a169362879388bc1bdf2bbb977782d4f
  (let ((orig (default-value 'repeat-echo-function))
        rcol ccol in-repeat)
    (setq
     repeat-echo-function
     (lambda (map)
       (if orig (funcall orig map))
       (unless rcol (setq rcol (face-foreground 'error)))
       (if map
           (unless in-repeat		; new repeat sequence
             (setq in-repeat t
                   ccol (face-background 'cursor))
             (set-frame-parameter nil 'my/repeat-cursor ccol))
         (setq in-repeat nil)
         (set-frame-parameter nil 'my/repeat-cursor nil))
       (set-cursor-color (if map rcol ccol))))

    (add-function
     :after after-focus-change-function
     (let ((sym 'my/remove-repeat-cursor-color-on-focus-change))
       (defalias sym
         (lambda ()
           (when in-repeat
             (dolist (frame (frame-list))
               (when-let ((col (frame-parameter frame 'my/repeat-cursor)))
                 (with-selected-frame frame
                   (set-cursor-color col)))))))
       sym))))

;;;; dot-mode
(use-package dot-mode
  :ensure t
  :diminish " ·"
  :commands dot-mode
  :bind ( :map dot-mode-map
          ("C-c ." . nil)
          ("C-M-." . nil))
  :hook ((prog-mode conf-mode text-mode tex-mode) . 'dot-mode-on))

;;;; abbrevs
(use-package abbrev
  :diminish
  :hook ((prog-mode text-mode) . abbrev-mode)
  :config
  ;; (setq abbrev-file-name (expand-file-name "abbvev-defs" user-cache-directory))
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;;; better marks
(use-package visible-mark
  :ensure t
  :hook ((text-mode prog-mode conf-mode) . visible-mark-mode)
  :custom-face
  (visible-mark-face1
   ((t (:inherit region :background unspecified))))
  :config
  (setq visible-mark-faces '(visible-mark-face1))
  (setq visible-mark-max 1))

;;;; expand-region
(use-package expand-region
  :ensure (:remotes ("fork" :host github :repo "karthink/expand-region.el"))
  :commands expand-region
  :bind ("C-," . 'er/expand-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode)
  (set-default 'er--show-expansion-message nil)
  (setq expand-region-show-usage-message nil
        expand-region-fast-keys-enabled nil)
  (defvar expand-region-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "," #'er/expand-region)
      (define-key map "-" #'er/contract-region)
      (define-key map "." #'er/contract-region)
      map))
  ;; Expand 0-9 times by pressing 0-9
  (dotimes (i 9)
    (define-key expand-region-repeat-map
                (kbd (number-to-string i))
                (lambda () (interactive)
                  (er/expand-region i)
                  (setq this-command 'er/expand-region))))
  (put 'er/expand-region 'repeat-map 'expand-region-repeat-map)
  (put 'er/contract-region 'repeat-map 'expand-region-repeat-map)
  (advice-add 'er--first-invocation
              :override
              (defun my/er--first-invocation ()
                "t if this is the first invocation of er/expand-region or er/contract-region"
                (not (memq last-command
                           '(er/expand-region er/contract-region
                                              easy-kill-expand-region easy-kill-contract-region)))))

  ;; The default er/save-org-mode-excursion uses org-fold-core-* and is too expensive
  (define-advice er/save-org-mode-excursion
      (:override (action) "ignore-org-fold")
    (org-with-wide-buffer
     (funcall action)))
  ;; The default er/mark-comment is both expensive and incorrect for block
  ;; comments.
  (defun er/mark-comment ()
    "Mark the entire comment around point."
    (interactive)
    (when (er--point-is-in-comment-p)
      (let ((p (point)))
        (while (or (> (skip-syntax-forward " ") 0)
                   (and (er--point-is-in-comment-p) (not (eobp))))
          (forward-char 1))
        (while (not (or (er--point-is-in-comment-p) (bobp)))
          (forward-char -1))
        (set-mark (point))
        (goto-char p)
        (while (or (< (skip-syntax-backward " ") 0)
                   (er--point-is-in-comment-p))
          (forward-char -1))
        (while (not (or (er--point-is-in-comment-p) (eobp)))
          (forward-char 1)))))

  (defun my/find-bounds-of-regexps (open close)
    (let ((start (point))
          (parity 0)
          (open-close (concat "\\(?:" open "\\|" close "\\)"))
          end)
      (save-excursion
        (while (and (not (= parity -1))
                    (re-search-backward open-close nil t))
          (if (looking-at open)
              (setq parity (1- parity))
            (setq parity (1+ parity))))
        (setq end (point))
        (goto-char start)
        (while (and (not (= parity 0))
                    (re-search-forward open-close nil t))
          (if (looking-back
               close
               (- (point) (length (match-string-no-properties 0))))
              (setq parity (1+ parity))
            (setq parity (1- parity))))
        (when (= parity 0) (cons end (point))))))

  (use-package outline
    :hook (outline-minor-mode . er/add-outline-mode-expansions)
    :config
    (defun er/add-outline-mode-expansions ()
      (make-variable-buffer-local 'er/try-expand-list)
      (add-to-list 'er/try-expand-list 'outline-mark-subtree))))

;;;; easy-kill and easy-mark
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . #'easy-kill)
         ([remap mark-sexp]      . #'easy-mark)
         ("M-S-w" . kill-ring-save)
         :map easy-kill-base-map
         ("+" . nil) ("=" . nil)
         ("," . easy-kill-expand-region)
         ("." . easy-kill-contract-region))
  :config
  (setq easy-kill-alist
        '((119 word " ")
          (115 sexp "\n")
          (101 line "\n")
          (108 list "\n")
          (100 defun "\n\n")
          (41 sentence "\n")
          (104 paragraph "\n")
          (62 page "\n")
          (102 filename "\n")
          (68 defun-name " ")
          (98 buffer-file-name)))
  (defun easy-kill-expand-region ()
    "Expand kill according to expand-region."
    (interactive)
    (let* ((thing (easy-kill-get nil))
           (bounds (easy-kill--bounds)))
      (save-mark-and-excursion
        (set-mark (cdr bounds))
        (goto-char (car bounds))
        (er/expand-region 1)
        (deactivate-mark)
        (easy-kill-adjust-candidate thing (point) (mark)))))
  (defun easy-kill-contract-region ()
    "Expand kill according to expand-region."
    (interactive)
    (let* ((thing (easy-kill-get nil))
           (bounds (easy-kill--bounds)))
      (save-mark-and-excursion
        (set-mark (cdr bounds))
        (goto-char (car bounds))
        (er/contract-region 1)
        (deactivate-mark)
        (easy-kill-adjust-candidate thing (point) (mark))))))

(use-package easy-kill
  :after easy-kill
  :config
  ;; Incomplete implementation of using easy-kill to select backwards
  (defun easy-kill-thing-alt (&optional thing n inhibit-handler)
    (interactive
     (list (cl-second (assq (+ last-command-event 32) easy-kill-alist))
           (prefix-numeric-value current-prefix-arg)))
    (let* ((thing (or thing (easy-kill-get thing)))
           (n (or n 1))
           (handler (and (not inhibit-handler)
                         (easy-kill-thing-handler (format "easy-kill-on-%s" thing)
                                                  major-mode))))
      (when (easy-kill-get mark)
        (goto-char (easy-kill-get origin)))
      (cond
       (handler (funcall handler n))
       ((or (memq n '(+ -))
            (and (eq thing (easy-kill-get thing))
                 (not (zerop n))))
        (easy-kill-thing-backward (pcase n
                                    (`+ 1)
                                    (`- -1)
                                    (_ n))))
       (t (pcase (easy-kill-bounds-of-thing-at-point thing)
            (`nil (easy-kill-echo "No `%s'" thing))
            (`(,start . ,end)
             (easy-kill-adjust-candidate thing start end)
             (unless (zerop n)
               (easy-kill-thing-backward (1- n)))))))
      (when (easy-kill-get mark)
        (easy-kill-adjust-candidate (easy-kill-get thing)))))

  (defun easy-kill-thing-backward (n)
    (when (and (easy-kill-get thing) (/= n 0))
      (let* ((step (if (cl-minusp n) -1 +1))
             (thing (easy-kill-get thing))
             (bounds1 (or (easy-kill-pair-to-list
                           (easy-kill-bounds-of-thing-at-point thing))
                          (list (point) (point))))
             (start (easy-kill-get start))
             (end (easy-kill-get end))
             (rear (or (car (cl-set-difference (list start end) bounds1))
                       (pcase step
                         (`-1 end)
                         (`1 start))))
             (new-rear (save-excursion
                         (goto-char rear)
                         (with-demoted-errors "%S"
                           (dotimes (_ (abs n))
                             (easy-kill-thing-backward-1 thing step)))
                         (point))))
        (pcase (and (/= rear new-rear)
                    (sort (cons new-rear bounds1) #'<))
          (`(,start ,_ ,end)
           (easy-kill-adjust-candidate thing start end)
           t)))))

  (defun easy-kill-thing-backward-1 (thing &optional n)
    "Easy Kill wrapper for `forward-thing'."
    (pcase (easy-kill-thing-handler
            (format "easy-kill-thing-backward-%s" thing)
            major-mode)
      ((and (pred functionp) fn) (funcall fn n))
      (_ (forward-thing thing (- n)))))

  (defun easy-kill-map ()
    "Build the keymap according to `easy-kill-alist'."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map easy-kill-base-map)
      (when easy-kill-unhighlight-key
        (with-demoted-errors "easy-kill-unhighlight-key: %S"
          (define-key map easy-kill-unhighlight-key #'easy-kill-unhighlight)))
      (define-key map "[" (lambda () (interactive)
                            (cl-letf* (((symbol-function 'easy-kill-thing)
                                        #'easy-kill-thing-alt))
                              (call-interactively #'easy-kill-thing-alt))))
      (define-key map "]" #'easy-kill-thing)
      (dolist (c easy-kill-alist)
        ;; (define-key map (vector meta-prefix-char (car c)) #'easy-kill-select)
        (when (<= 97 (car c) 122)
          (define-key map (char-to-string (- (car c) 32))
                      (lambda () (interactive)
                        (cl-letf* (((symbol-function 'easy-kill-thing)
                                    #'easy-kill-thing-alt))
                          (call-interactively #'easy-kill-thing-alt)))))
        (define-key map (char-to-string (car c)) #'easy-kill-thing))
      map)))

;;;; goto-last-change
(use-package goto-chg
  :ensure t
  :bind (("M-g ;" . goto-last-change)
         ("M-i" . goto-last-change)
         ("M-g M-;" . goto-last-change))
  :config
  (use-package org
    :bind (:map org-mode-map
                ("M-g ;" . org-goto-last-change)
                ("M-i" . org-goto-last-change)
                ("M-g M-;" . org-goto-last-change))
    :config
    (defun org-goto-last-change ()
      (interactive)
      (call-interactively 'goto-last-change)
      (when (org-invisible-p)
        (org-fold-show-context 'link-search)))))

;;;; macrursors
;;----------------------------------------------------------------
;; ** MACRURSORS
;;;----------------------------------------------------------------
(use-package macrursors
  :ensure ( :host github :protocol ssh
            :repo "karthink/macrursors"
            :branch "expand-region")
  :bind-keymap ("C-;" . macrursors-mark-map)
  :bind (("M-n" . macrursors-mark-next-instance-of)
         ("M-p" . macrursors-mark-previous-instance-of)
         ("C-M-;" . macrursors-mark-all-instances-of)
         :map macrursors-mode-map
         ("C-'" . macrursors-hideshow)
         ("C-;" . nil)
         ("C-; C-;" . macrursors-end)
         ("C-; C-j" . macrursors-end)
         :map isearch-mode-map
         ("C-;" . macrursors-mark-from-isearch)
         ("M-s n" . macrursors-mark-next-from-isearch)
         ("M-s p" . macrursors-mark-previous-from-isearch)
         :map macrursors-mark-map
         ("C-n" . macrursors-mark-next-line)
         ("C-p" . macrursors-mark-previous-line)
         ("C-SPC" . nil)
         ("." . macrursors-mark-all-instances-of)
         ("o" . macrursors-mark-all-instances-of)
         ("SPC" . macrursors-select)
         ("l" . macrursors-mark-all-lists)
         ("s" . macrursors-mark-all-symbols)
         ("w" . macrursors-mark-all-words)
         ("C-M-e" . macrursors-mark-all-sexps)
         ("d" . macrursors-mark-all-defuns)
         ("n" . macrursors-mark-all-numbers)
         (")" . macrursors-mark-all-sentences)
         ("M-e" . macrursors-mark-all-sentences)
         ("e" . macrursors-mark-all-lines))
  :init
  (define-prefix-command 'macrursors-mark-map)
  (use-package macrursors-select
    :bind (:map macrursors-mark-map
           ("C-g" . macrursors-select-clear)))
  (use-package macrursors-select-expand
    :bind
    (:map macrursors-mark-map
     ("," . macrursors-select-expand)
     :map macrursors-select-map
     ("-" . macrursors-select-contract)
     ("." . macrursors-select-contract)
     ("," . macrursors-select-expand)))

  (use-package avy
    :bind
    (:map macrursors-mark-map
     ("j" . my/macrursors-at-avy))
    :config
    (defun my/macrursors-at-avy ()
      (interactive)
      (let* ((avy-all-windows nil)
             (avy-timeout-seconds 0.40)
             (positions (mapcar #'caar (avy--read-candidates))))
        (when positions
          (when-let* ((buf (overlay-buffer mouse-secondary-overlay))
                      ((eq buf (current-buffer))))
            (setq positions
                  (cl-delete-if-not
                   (lambda (p) (<= (overlay-start mouse-secondary-overlay)
                              p (overlay-end mouse-secondary-overlay)))
                   positions))))
        (when positions
          (push-mark)
          (goto-char (car positions))
          (mapc #'macrursors--add-overlay-at-point (cdr positions))
          (macrursors-start)))))

  :config
  (dolist (mode '( ;;global-eldoc-mode
                  ;; gcmh-mode corfu-mode font-lock-mode
                  show-paren-mode))
    (add-hook 'macrursors-pre-finish-hook mode)
    (add-hook 'macrursors-post-finish-hook mode))
  (setq ;; macrursors-apply-keys "C-; C-;"
   macrursors-match-cursor-style t)

  (defvar macrursors-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" #'macrursors-mark-next-instance-of)
      (define-key map "p" #'macrursors-mark-previous-instance-of)
      map))
  (map-keymap (lambda (_ cmd)
                (put cmd 'repeat-map 'macrursors-repeat-map))
              macrursors-repeat-map)
  (dolist (cmd '(macrursors-mark-next-from-isearch
                 macrursors-mark-previous-from-isearch
                 macrursors-mark-next-line
                 macrursors-mark-previous-line))
    (put cmd 'repeat-map 'macrursors-repeat-map))

  (setf macrursors-mode-line nil)
  (defsubst my/mode-line-macro-recording ()
    "Display macro being recorded."
    (when (or defining-kbd-macro executing-kbd-macro)
      (let ((sep (propertize " " 'face 'highlight ))
            (vsep (propertize " " 'face '(:inherit variable-pitch))))
        ;; "●"
        (propertize
         (concat
          sep "REC" vsep
          (number-to-string kmacro-counter) vsep "▶" vsep
          (when macrursors-mode
            (if macrursors--overlays
                (format (concat "[%d/%d]" vsep)
                        (1+ (cl-count-if (lambda (p) (< p (point))) macrursors--overlays
                                         :key #'overlay-start))
                        (1+ (length macrursors--overlays)))
              (concat "[1/1]" vsep))))
         'face 'highlight)))))

;;;; embrace
(use-package embrace
  :ensure t
  :hook ((org-mode . embrace-org-mode-hook)
         (org-mode . my/embrace-latex-mode-hook-extra)
         (LaTeX-mode . embrace-LaTeX-mode-hook)
         (LaTeX-mode . my/embrace-latex-mode-hook-extra))
  :bind (:map prog-mode-map
              ("M-s a" . embrace-add)
              ("M-s c" . embrace-change)
              ("M-s d" . embrace-delete)
              :map text-mode-map
              ("M-s a" . embrace-add)
              ("M-s c" . embrace-change)
              ("M-s d" . embrace-delete))
  :config
  ;; Monkey patching: Expand region goes haywire sometimes
  (defun embrace--get-region-overlay (open close)
    (let ((bounds (or (embrace--fallback-re-search open close)
                      (embrace--expand-region-research open close))))
      (when bounds
        (make-overlay (car bounds) (cdr bounds) nil nil t))))

  (defun  embrace--fallback-re-search (open close)
    (my/find-bounds-of-regexps open close))

  (cl-pushnew '(?  . activate-mark)
              (default-value 'embrace-semantic-units-alist))

  (defun my/embrace-latex-mode-hook-extra ()
    (add-to-list 'embrace-semantic-units-alist '(?E . er/mark-LaTeX-inside-environment))
    (add-to-list 'embrace-semantic-units-alist '(?e . LaTeX-mark-environment))
    (add-to-list 'embrace-semantic-units-alist '(?$ . er/mark-LaTeX-math))
    (embrace-add-pair-regexp ?m "\\\\[a-z*]+{" "}" #'my/embrace-latex-macro-read-function
                             (embrace-build-help "\\macro{" "}"))
    (embrace-add-pair-regexp ?e "\\\\begin{[a-z*]+}" "\\\\end{[a-z*]+}"
                             (lambda ()
                               (let ((env (completing-read "Env: "
                                                           (mapcar #'car
                                                                   LaTeX-environment-list))))
                                 (cons (format "\\begin{%s}" env)
                                       (format "\\end{%s}" env))))
                             (embrace-build-help "\\begin{.}" "\\end{.}"))
    (embrace-add-pair-regexp 36 "\\$" "\\$" nil)
    (embrace-add-pair-regexp ?d "\\\\left\\\\*[{([|<]" "\\\\right\\\\*[]})|>]"
                             (lambda ()
                               (let* ((env (read-char "Delim type: "))
                                      (env-pair (pcase env
                                                  ((or 40 41) '("(" . ")"))
                                                  ((or 91 93) '("[" . "]"))
                                                  ((or 123 125) '("\\{" . "\\}"))
                                                  ((or 60 62) '("<" . ">"))
                                                  (124 '("|" . "|")))))
                                 (cons (format "\\left%s " (car env-pair))
                                       (format " \\right%s" (cdr env-pair)))))
                             (embrace-build-help "\\left." "\\right.")
                             ))

  (defun my/embrace-latex-macro-read-function ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\\\[[{(|]"
         :right-regexp "\\\\[]})|]"))

  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (assoc-default char embrace--pairs-list)))
        (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                  (funcall (embrace-pair-struct-read-function pair)))))
            real-pair
          (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
      (cons char char)))

  (defun +evil--embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair char))
              (text "\\%s")) ;; (if (sp-point-in-string) "\\\\%s" "\\%s")
          (cons (format text (car pair))
                (format text (cdr pair))))))))

(use-package embrace
  :after org
  :bind (:map org-mode-map
         ("M-s a" . embrace-add)
         ("M-s c" . embrace-change)
         ("M-s d" . embrace-delete)))

;;;; wrap-region
(use-package wrap-region
  :ensure t
  :diminish
  :hook (((text-mode tex-mode conf-mode) . wrap-region-mode)
         (wrap-region-mode . (lambda () (wrap-region-remove-wrapper "'"))))
  :config
  (wrap-region-add-wrappers
   '(("/" "/" nil 'org-mode)
     ("+" "+" nil 'org-mode)
     ("=" "=" nil 'org-mode)
     ("~" "~" nil 'org-mode)
     ("*" "*" nil 'markdown-mode)
     ("*" "*" nil 'org-mode)
     ("_" "_" nil '(org-mode markdown-mode)))))

;;;; smartparens
(use-package elec-pair
  :defer
  :config
  ;; (electric-pair-mode +1)
  (setq electric-pair-inhibit-predicate
        `(lambda (c)
           (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c)))))

(use-package smartparens
  :ensure t
  :diminish "()"
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          fennel-mode scheme-mode
          ielm-mode markdown-mode
          eval-expression-minibuffer-setup
          git-commit-mode)
         . smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("M-["           . sp-backward-slurp-sexp)
        ("M-]"           . sp-forward-slurp-sexp)
        ("M-{"           . sp-backward-barf-sexp)
        ("M-}"           . sp-forward-barf-sexp)
        ("M-U"           . sp-raise-sexp)
        ("M-R"           . raise-sexp)
        ("M-C"           . sp-convolute-sexp)
        ("M-D"           . my/sp-duplicate-sexp)
        ("M-J"           . sp-join-sexp)
        ("M-S"           . sp-split-sexp)
        ("C-M-<up>"      . sp-raise-sexp)
        ("C-<right>"     . sp-forward-slurp-sexp)
        ("C-<left>"      . sp-backward-slurp-sexp)
        ("M-<right>"     . sp-forward-barf-sexp)
        ("M-<left>"      . sp-backward-barf-sexp)
        ("M-K"           . sp-kill-hybrid-sexp)
        ("C-x C-t"       . sp-transpose-hybrid-sexp)
        ("C-M-n"         . sp-next-sexp)
        ("C-M-p"         . sp-previous-sexp)
        ("C-<backspace>" . sp-backward-kill-word))
  :init
  (add-hook 'smartparens-enabled-hook
            (lambda ()
              "Disable \\[electric-pair-mode] when \[[smartparens-mode]] is enabled."
              (electric-pair-local-mode -1)))
  (add-hook 'smartparens-disabled-hook
            (lambda ()
              "Enable \\[electric-pair-mode] when \[[smartparens-mode]] is disabled."
              (electric-pair-local-mode +1)))
  (add-hook 'show-smartparens-mode-hook
            (lambda ()
              "Disable \\[show-paren-mode] when \[[show-smartparens-mode]] is enabled."
              (if show-smartparens-mode
                  (show-paren-mode -1)
                (show-paren-mode 1))))
  :config
  (add-to-list 'sp-lisp-modes 'minibuffer-mode)
  (sp-pair "`" nil :actions :rem)
  (sp-local-pair '(markdown-mode julia-mode)
                 "`" "`" :actions '(insert wrap autoskip))
  (sp-with-modes '(markdown-mode)
    (sp-local-pair "_" "_" :actions '(wrap autoskip))
    (sp-local-pair "*" "*" :actions '(wrap autoskip))
    (sp-local-pair "'" nil :actions '(wrap)))
  (sp-local-pair '(message-mode text-mode notmuch-message-mode)
                 "`" "'" :actions '(insert wrap autoskip))
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "`" "'" :actions '(wrap autoskip))
    (sp-local-pair "'" nil :actions nil))

  (defun my/sp-duplicate-sexp (&optional arg)
    (interactive "p")
    (insert (string-trim
             (buffer-substring
              (save-excursion
                (backward-sexp)
                (point))
              (point)))))

  (defvar lisp-navigation-map
    (let ((map (make-sparse-keymap)))
      (pcase-dolist (`(,k . ,f)
                     '(("u" . backward-up-list)
                       ("f" . forward-sexp)
                       ("b" . backward-sexp)
                       ("d" . down-list)
                       ("n" . sp-next-sexp)
                       ("p" . sp-previous-sexp)
                       ("k" . sp-kill-sexp)
                       ("K" . sp-kill-hybrid-sexp)
                       ("]" . sp-forward-slurp-sexp)
                       ("[" . sp-backward-slurp-sexp)
                       ("}" . sp-forward-barf-sexp)
                       ("{" . sp-backward-barf-sexp)
                       ("r" . raise-sexp)
                       ("C" . sp-convolute-sexp)
                       ("D" . my/sp-duplicate-sexp)
                       ("J" . sp-join-sexp)
                       ("S" . sp-split-sexp)
                       ("R" . sp-raise-sexp)
                       ("\\" . indent-region)
                       ;; ("e" . eval-sexp-maybe-pp)
                       ("t" . transpose-sexps)))
        (define-key map (kbd k) f))
      (dolist (n (number-sequence 48 57))
        (define-key map `[,n] 'digit-argument))
      map))

  (map-keymap
   (lambda (_ cmd)
     (put cmd 'repeat-map 'lisp-navigation-map))
   lisp-navigation-map)
  (put 'kill-sexp 'repeat-map 'lisp-navigation-map))

;;;; cdlatex-style unicode input
(use-package quail
  :disabled
  :commands my/cdlatex-input-tex
  :config
  (defun my/cdlatex-input-tex ()
    (interactive)
    (require 'cdlatex nil t)
    (let ((cim current-input-method))
      (unless (equal cim "TeX")
        (activate-input-method "TeX"))
      (cl-letf (((symbol-function 'texmathp)
                 (lambda () t))
                ((symbol-function 'insert)
                 (lambda (symbol)
                   (setq unread-input-method-events
                         (nconc (quail-input-string-to-events symbol)
                                (list 0))))))
        (cdlatex-math-symbol))
      (unless (equal cim "TeX")
        (run-at-time 0 nil (lambda () (activate-input-method cim)))))))

;;;; Replacements
(use-package query-replace-parallel
  :ensure (:host github :repo "hokomo/query-replace-parallel"
                 :files ("query-replace-parallel.el"))
  :bind (("H-%" . query-replace-parallel)
         ("H-M-%" . query-replace-parallel-regexp)))
;;;; wgrep
(use-package wgrep
  :ensure t
  :commands wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

;;;; vundo
(use-package vundo
  :disabled
  :ensure (:host github :repo "casouri/vundo")
  :bind ("C-x u" . vundo)
  :config
  (set-face-attribute 'vundo-default nil :family "Symbola")
  (setq vundo-glyph-alist vundo-unicode-symbols))

(provide 'setup-editing-extra)
;;; setup-editing-extra.el ends here
