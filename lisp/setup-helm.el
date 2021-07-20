(use-package helm-config
  :demand)

(use-package helm
  :init (helm-mode 1)
  :after helm-config
  :config
  (unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  (setq
   helm-locate-command "plocate %s -i --regex %s"
   helm-follow-mode-persistent t
   helm-show-completion-display-function nil
   helm-dwim-target 'completion
   helm-grep-save-buffer-name-no-confirm t

   helm-M-x-fuzzy-match t
   helm-apropos-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-completion-in-region-fuzzy-match t
   helm-eshell-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-locate-library-fuzzy-match t
   helm-recentf-fuzzy-match t

   ;; To prevent M-s f from directly going to symbol at point if in same buffer.
   ;; helm-imenu-execute-action-at-once-if-one nil
   ;; Use woman instead of man.
   ;; helm-man-or-woman-function nil

   ;; https://github.com/emacs-helm/helm/issues/1910
   helm-buffers-end-truncated-string "…"
   
   ;; helm-reuse-last-window-split-state t
   ;; helm-display-header-line nil
   ;; helm-findutils-search-full-path t
   ;; helm-completion-mode-string ""
   ;; helm-echo-input-in-header-line t
   ;; helm-use-frame-when-more-than-two-windows nil
   ;; helm-buffer-max-length 22

   helm-window-show-buffers-function 'helm-window-mosaic-fn
   helm-window-prefer-horizontal-split 'decide)
  
  :bind (([remap execute-extended-command] . helm-M-x)
         ([remap find-file] . helm-find-files)
         ([remap switch-to-buffer] . helm-mini)
         ("s-b" . helm-mini)
         ([remap bookmark-jump] . helm-filtered-bookmarks)
         ([remap bookmark-set] . helm-filtered-bookmarks)
         ([remap query-replace-regexp] . helm-regexp)
         ([remap yank-pop] . helm-show-kill-ring)
         ("M-s /" . helm-regexp)
         ("M-s i" . helm-semantic-or-imenu)
         ("M-s I" . helm-imenu-in-all-buffers)
         ("M-s M-l" . helm-locate)
         ("M-s g" . helm-do-grep-ag)
         ("M-s G" . helm-grep-do-git-grep)
         ("M-s f" . helm-find)
         ("C-h M-g" . helm-google-suggest)
         ("M-s SPC" . helm-all-mark-rings)
         ([remap occur] . helm-occur)
         ([remap list-buffers] . helm-buffers-list)
         ([remap dabbrev-expand] . helm-dabbrev)
         ([remap execute-extended-command] . helm-M-x)
         ([remap apropos-command] . helm-apropos)
         :map helm-map
         ("C-w" . backward-kill-word-or-region)
         ("C-M-n" . helm-next-source)
         ("C-M-p" . helm-previous-source))
)

(use-package helm-eshell
  :after eshell
  :config
  (setq helm-esh-pcomplete-build-source-fn #'helm-fish-completion-make-eshell-source))

(use-package wgrep-helm
  :after helm-grep
  ;; :hook (wgrep-setup . #'wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key (kbd "C-x C-q")))

(use-package helm-bookmark
  :after (helm helm-buffer)
  :config
  (setq helm-mini-default-sources `(helm-source-buffers-list
                                  helm-source-recentf
                                  ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                  helm-source-bookmarks
                                  helm-source-bookmark-set
                                  helm-source-buffer-not-found)))

(with-eval-after-load 'man
  (define-key Man-mode-map (kbd "M-s i") 'helm-imenu))
(with-eval-after-load 'woman
  (define-key Man-mode-map (kbd "M-s i") 'helm-imenu))

(use-package helm-switch-to-repl
  :after (helm-buffer)
  :config
  (helm-switch-to-repl-setup))

(use-package helm-descbinds
  :after helm
  :bind ([remap describe-bindings] . helm-descbinds))

(use-package helm-fish-completion
  :after (eshell helm)
  :config
  (define-key shell-mode-map (kbd "<tab>") 'helm-fish-completion))

(use-package eshell
  :after helm
  :hook (eshell-mode . eshell-helm-setup)
  :config
  (defun eshell-helm-setup ()
    (define-key eshell-mode-map [remap completion-at-point] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-s") nil) ; Useless when we have 'helm-eshell-history.
    (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))

(use-package helm-sly
  :after sly
  :config
;; If you want fuzzy-matching:
  (setq helm-completion-in-region-fuzzy-match t))

(use-package sly
  :hook (sly-mrepl . helm-sly-disable-internal-completion))

;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
;; (setq helm-command-prefix-key "C-c h")

;; (require 'helm-config)
;;(require 'helm-eshell)
;;(require 'helm-files)
;;(require 'helm-grep)

; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

; (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
; (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
; (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

;; (setq
;;  helm-google-suggest-use-curl-p t
;;  helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
;;  helm-quick-update t ; do not display invisible candidates
;;  helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
;;  helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
;;  helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

;;  helm-split-window-default-side 'other ;; open helm buffer in another window
;;  helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
;;  ;; helm-buffers-favorite-modes (append helm-buffers-favorite-modes
;;  ;;                                     '(picture-mode artist-mode))
;;  helm-candidate-number-limit 30 ; limit the number of displayed canidates
;;  helm-M-x-requires-pattern 1     ; show all candidates when set to 0
;;  helm-ff-file-name-history-use-recentf t
;;  helm-move-to-line-cycle-in-source t ; move to end or beginning of source
;;                                         ; when reaching top or bottom of source.
;;  ido-use-virtual-buffers t      ; Needed in helm-buffers-list
;;  helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
;;                                         ; useful in helm-mini that lists buffers
;;  )

;; Save current position to mark ring when jumping to a different place
;; (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
