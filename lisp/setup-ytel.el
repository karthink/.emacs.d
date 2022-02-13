;; Utilities for opening youtube videos with mpv using ytel
(use-package ytel
  :straight t
  :commands ytel
  :bind (:map ytel-mode-map
              ("x" . ytel-watch-browse-url)
              ("m" . ytel-watch-umpv)
              ("w" . ytel-url-kill-new)
              ("f" . ytel-search-next-page)
              ("b" . ytel-search-previous-page))
  :hook ((ytel-mode . toggle-truncate-lines)
         (ytel-mode . hl-line-mode))
  :config
  (defun ytel ()
  "Enter ytel."
  (interactive)
  (pop-to-buffer (ytel-buffer))
  (unless (eq major-mode 'ytel-mode)
    (ytel-mode))
  (when (seq-empty-p ytel-search-term)
    (call-interactively #'ytel-search)))
  
  (add-to-list 'display-buffer-alist
               '((lambda (buf act) (equal (buffer-mode buf)
                                     'ytel-mode))
                 ;; "^\\*ytel\\*$"
                 (+select-buffer-in-side-window
                  display-buffer-in-side-window)
                 (window-height . 0.33)
                 (slot . 10)
                 (side . bottom)))
  (setq ytel-invidious-api-url "https://invidious.osi.kr/")
  (defvar ytel-invidious-api-alt-urls
    (mapcar (lambda (url) (concat "https://" url))
            '("inv.riverside.rocks" "invidious.silkky.cloud"
             "invidious-us.kavin.rocks" "ytb.trom.tf"
             "yewtu.be" "invidious.namazso.eu" "invidious.kavin.rocks"
             "ytprivate.com" "invidious.kavin.rocks"
             "vid.puffyan.us" "invidious.exonip.de"
             "invidious.snopyta.org" "y.com.cm"))
    "Alternate invidious URLs for ytel.")
  
  (defun ytel-video-url ()
    (let* ((video (ytel-get-current-video))
           (id (ytel-video-id video)))
      (concat "https://youtube.com/watch?v=" id)))
    
  (defun ytel-watch-umpv (&optional arg)
    (interactive "P")
    (browse-url-umpv (ytel-video-url) arg)
    (message "Playing video with mpv.")
    (forward-line))

  (defun ytel-watch-browse-url (&optional arg)
    (interactive "P")
    (browse-url (ytel-video-url) arg)
    (forward-line))
  
  (defun ytel-url-kill-new ()
    (interactive)
    (kill-new (ytel-video-url))
    (message "Copied url to kill ring.")
    (forward-line)))

;; ytel-show is useful for digging through video descriptions and, occasionally,
;; comments.
(use-package ytel-show
  :after ytel
  :straight
  (ytel-show :type git :host github :repo "xFA25E/ytel-show")
  :bind (:map ytel-mode-map ("RET" . ytel-show)
         :map ytel-show-mode-map
         ("TAB" . shr-next-link)
         ("<backtab>" . shr-previous-link))
  :config
  (setq ytel-show-image-max-width 50
        ytel-show-image-max-height 50))

(provide 'setup-ytel)
;;; setup-ytel.el ends here
