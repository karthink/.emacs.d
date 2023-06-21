;; Adapted from Protesilaos Stavrou's dotfiles: https://protesilaos.com/dotemacs

(declare-function bookmark-make-record-default "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))
(declare-function bookmark-get-bookmark-record "bookmark" (bmk))

(use-package eww
  :bind (("M-s W" . eww-search-words)
         :map eww-mode-map
         ("D" . eww-download)
         ("d" . my/scroll-up-half)
         ("u" . my/scroll-down-half)
         ("U" . eww-up-url)
         ("^" . eww-up-url)
         ("i" . imenu))
  :hook (eww-after-render . (lambda () (setq line-spacing 0.1)))
  :config
  (setq eww-browse-url-new-window-is-tab nil)
  (use-package setup-reading
    :config
    (add-hook 'eww-after-render-hook 'my/reader-center-images 99)))

(use-package eww
  :when (< emacs-major-version 28)
  :hook (eww-mode . prot-eww--set-bookmark-handler)
  :defer
  :config
  (defcustom prot-eww-bookmark-link t
    "Control the behaviour of bookmarking inside EWW buffers.

If non-nil bookmark the button at point, else the current page's
URL.  Otherwise only target the current page.

This concerns the standard bookmark.el framework, so it applies
to commands `bookmark-set' and `bookmark-set-no-overwrite'."
    :type 'boolean
    :group 'eww)

  (defun prot-eww--bookmark-make-record ()
    "Return a bookmark record.
If `prot-eww-bookmark-link' is non-nil and point is on a link button,
return a bookmark record for that link.  Otherwise, return a bookmark
record for the current EWW page."
    (let* ((button (and prot-eww-bookmark-link
                        (button-at (point))))
           (url (if button
                    (button-get button 'shr-url)
                  (plist-get eww-data :url))))
      (unless url
        (error "No link found; cannot bookmark this"))
      (let* ((title (if button
                        url
                      (concat "(EWW) " (plist-get eww-data :title))))
             (pos (if button nil (point)))
             (defaults (delq nil (list title url))))
        `(,title
          ,@(bookmark-make-record-default 'no-file)
          (eww-url . ,url)
          (filename . ,url) ; This is a hack to get Marginalia annotations
          (position . ,pos)
          (handler . prot-eww-bookmark-jump)
          (defaults . ,defaults)))))

  ;; FIXME 2021-09-08: Why `bookmark-bmenu-other-window' does not work as
  ;; intended?  It is bound to `o' in `bookmark-bmenu-mode-map' (C-x r l).

  ;; TODO 2021-09-08: Restore position of point.  We would need a
  ;; `with-current-buffer' for this, but the tricky part is to get the
  ;; correct one that the bookmark handler visits at the moment.

;;;###autoload
  (defun prot-eww-bookmark-jump (bookmark)
    "Jump to BOOKMARK using EWW.
This implements the handler function interface for the record
type returned by `prot-eww--bookmark-make-record'."
    (let* ((file (bookmark-prop-get bookmark 'eww-url))
           (buf (eww file)))
      (bookmark-default-handler
       `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

  (defun prot-eww--set-bookmark-handler ()
    "Set appropriate `bookmark-make-record-function'.
Intended for use with `eww-mode-hook'."
    (setq-local bookmark-make-record-function #'prot-eww--bookmark-make-record)))

(provide 'setup-eww)


