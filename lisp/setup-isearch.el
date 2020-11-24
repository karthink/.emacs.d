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

  (defun my/isearch-mark-and-exit ()
    "Mark the current search string and exit the search."
    (interactive)
    (push-mark isearch-other-end t 'activate)
    (setq deactivate-mark nil)
    (isearch-done))

  (defun my/isearch-other-end ()
    "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
    (interactive)
    (isearch-done)
    (when isearch-other-end
      (goto-char isearch-other-end)))

  (defun my/isearch-abort ()
    "Remove non-matching `isearch' input, reverting to previous
successful search and continuing with the search.

This is a modified variant of the original `isearch-abort',
mapped to C-g which will remove the failed match if any and only
afterwards exit the search altogether."
    (interactive)
    (discard-input)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state))
    (isearch-update))

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

  :bind (("M-s M-o"    . multi-occur)
         ("M-s %"      . my/isearch-query-replace-symbol-at-point)
         :map minibuffer-local-isearch-map
         ("M-/"        . isearch-complete-edit)
         :map isearch-mode-map
         ("M-/"        . isearch-complete)
         ("C-SPC"      . my/isearch-mark-and-exit)
         ("DEL"        . my/isearch-abort)
         ("<C-return>" . my/isearch-other-end)
         ("C-M-s"      . my/isearch-forward-other-buffer)
         ("C-M-r"      . my/isearch-backward-other-buffer)))

(provide 'setup-isearch)
