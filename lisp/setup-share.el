;; -*- lexical-binding: t; -*-

;; SVG screenshots
;;----------------------------------------------------------------------
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Puts the filename in the kill ring and displays it using an
X-sink (Executable dragon-drag-and-drop if available)."
  (interactive)
  (let ((filename (concat (file-name-as-directory
                           (concat (getenv "HOME") "/Pictures/screenshots"))
                          (format "%s_%sx%s_emacs.svg"
                                  (format-time-string "%Y-%m-%d-%H%M%S")
                                  (frame-pixel-width)
                                  (frame-pixel-height))))
        (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (if (executable-find "dragon-drag-and-drop")
        (start-process "drag_screenshot_svg" "drag_screenshot_svg"
                       "dragon-drag-and-drop" filename)
      (message "Could not find dragon-drag-and-drop"))
    (message filename)))

(global-set-key (kbd "<M-print>") #'screenshot-svg)

;; For easy sharing of files/text.
(use-package 0x0
  :ensure t
  :commands (0x0-upload 0x0-dwim)
  :bind ("C-x M-U" . 0x0-dwim))

;; Upload to my webserver
(use-package emacs
  :when (boundp 'my-server-url-alist)
  :bind ("C-x U" . pastebin-buffer)
  :commands (pastebin-buffer pastebin-file)
  :config
  (defun pastebin-buffer (&optional buf)
    (interactive (list (current-buffer)))
    (with-current-buffer buf
      (let ((upload-file-name
             (if current-prefix-arg
                 (read-string "Name of uploaded buffer: ")
               (buffer-name)))
            (htmlized-buffer
             (if (use-region-p)
                 (progn
                   (deactivate-mark)
                   (htmlize-region (region-beginning) (region-end)))
               (htmlize-buffer))))
        (with-current-buffer htmlized-buffer
          (let* ((bufhash (sha1 (current-buffer)))
                 (ssh-url (format (car (alist-get 'pastebin my-server-url-alist))
                                  (concat (substring bufhash 0 8) "-")
                                  upload-file-name))
                 (web-url (format (cadr (alist-get 'pastebin my-server-url-alist))
                                  (concat (substring bufhash 0 8) "-")
                                  upload-file-name)))
            (write-file ssh-url)
            (message "Wrote file to: %s" web-url)
            (kill-new web-url))))))
  
  (defun pastebin-file (&optional file)
    (interactive
     (list (cond
            ((derived-mode-p 'dired-mode)
             (dired-file-name-at-point))
            (t (read-file-name "Pastebin file: " nil nil t)))))
    (let* ((filehash (shell-command-to-string (format "sha1sum %s" file)))
           (ssh-url (format (car (alist-get 'pastebin my-server-url-alist))
                            (substring filehash 0 8)
                            (file-name-nondirectory file)))
           (web-url (format (cadr (alist-get 'pastebin my-server-url-alist))
                            (substring filehash 0 8)
                            (file-name-nondirectory file))))
      (htmlize-file file ssh-url)
      (message "Wrote file to: %s" web-url)
      (kill-new web-url))))

(use-package screenshot
  :disabled
  :ensure ( :host github
            :repo "tecosaur/screenshot"
            :build (:not elpaca--byte-compile))
  :commands screenshot)


