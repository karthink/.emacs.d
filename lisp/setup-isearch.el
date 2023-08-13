;;(require 'use-package nil t)
(use-package isearch
  :diminish
  :config
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)

  (defvar isearch-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "s" 'isearch-repeat-forward)
      (define-key map "r" 'isearch-repeat-backward)
      map))
  (put 'isearch-repeat-forward  'repeat-map 'isearch-repeat-map)
  (put 'isearch-repeat-backward 'repeat-map 'isearch-repeat-map)
  
  (defun my/isearch-mark-and-exit ()
    "Mark the current search string and exit the search."
    (interactive)
    (push-mark isearch-other-end t 'activate)
    (setq deactivate-mark nil)
    (activate-mark)
    (isearch-done))

  (defun my/isearch-other-end ()
    "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
    (interactive)
    (isearch-done)
    (when isearch-other-end
      (goto-char isearch-other-end)))

  (defun my/isearch-forward-symbol-at-point (&optional arg)
    (interactive "p")
    (let ((arg (or arg 1)))
      (isearch-forward-symbol-at-point arg)))
  
  (defun my/isearch-backward-symbol-at-point (&optional arg)
    (interactive "p")
    (let ((arg (or arg 1)))
      (isearch-forward-symbol-at-point (- arg))))
  
;;   (defun my/isearch-abort ()
;;     "Remove non-matching `isearch' input, reverting to previous
;; successful search and continuing with the search.

;; This is a modified variant of the original `isearch-abort',
;; mapped to C-g which will remove the failed match if any and only
;; afterwards exit the search altogether."
;;     (interactive)
;;     (discard-input)
;;     (while (or (not isearch-success) isearch-error)
;;       (isearch-pop-state))
;;     (isearch-update))

  (defun my/isearch-query-replace-symbol-at-point ()
    "Run `query-replace-regexp' for the symbol at point."
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

;;;###autoload
  (defun my/isearch-forward-other-buffer (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))
          ))))
  
;;;###autoload
  (defun my/isearch-backward-other-buffer (prefix)
    "Function to isearch-backward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix 1 -1)))
          (other-window next)
          (isearch-backward)
          (other-window (- next))
          ))))

  (defvar my/search-url-regexp
    (concat
     "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
     "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
     "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
     (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	   (punct "!?:;.,"))
       (concat
        "\\(?:"
        ;; Match paired parentheses, e.g. in Wikipedia URLs:
        ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
        "[" chars punct "]+" "(" "[" chars punct "]+" ")"
        "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
        "\\|"
        "[" chars punct "]+" "[" chars "]"
        "\\)"))
     "\\)")
    "Regular expression that matches URLs.
Copy of variable `browse-url-button-regexp'.")

  (autoload 'goto-address-mode "goto-addr")

;;;###autoload
  (defun my/search-occur-urls ()
    "Produce buttonised list of all URLs in the current buffer."
    (interactive)
    (add-hook 'occur-hook #'goto-address-mode)
    (occur my/search-url-regexp "\\&")
    (remove-hook 'occur-hook #'goto-address-mode))

;;;###autoload
  (defun my/search-occur-browse-url (&optional use-generic-p)
    "Point browser at a URL in the buffer using completion.
Which web browser to use depends on the value of the variable
`browse-url-browser-function'.

Also see `my/search-occur-url'."
    (interactive "P")
    (let ((match nil)
          (matches nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp my/search-url-regexp nil t)
          (push (match-string-no-properties 0) matches)))
      (save-excursion
        (goto-char (point-min))
        (while (setq match (text-property-search-forward 'shr-url nil nil))
          (push (prop-match-value match) matches)))
      (let ((url (completing-read "Browse URL: " matches nil t)))
        (if use-generic-p
            (browse-url-generic url)
          (browse-url url)))))

  :bind (("M-s M-o"     . multi-occur)
         ("M-s %"       . my/isearch-query-replace-symbol-at-point)
         ("M-s u"       . my/search-occur-browse-url)
         ("M-s M-u"     . my/search-occur-urls)
         ("C-M-s"       . my/isearch-forward-other-buffer)
         ("C-M-r"       . my/isearch-backward-other-buffer)
         ("M-s ."           . my/isearch-forward-symbol-at-point)
         ("M-s ,"           . my/isearch-backward-symbol-at-point)
         :map minibuffer-local-isearch-map
         ("M-/"         . isearch-complete-edit)
         :map isearch-mode-map
         ("M-<"         . isearch-beginning-of-buffer)
         ("M->"         . isearch-end-of-buffer)
         ("M-s >"       . isearch-end-of-buffer)
         ("M-s <"       . isearch-beginning-of-buffer)
         ("M-/"         . isearch-complete)
         ("C-SPC"       . my/isearch-mark-and-exit)
         ("<C-return>"  . my/isearch-other-end)
         ("C-w"         . nil)
         ("M-e"         . nil)
         ("M-w"         . isearch-yank-word-or-char)))

(provide 'setup-isearch)
