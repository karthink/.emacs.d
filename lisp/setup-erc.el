;;; setup-erc.el --- ERC settings                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: comm

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

;;

;;; Code:
(use-package erc
  :commands (erc-tls erc)
  :bind (:map erc-mode-map
              ("\M-(" . insert-parentheses-sentence))
  :config
  (setq erc-server "irc.libera.chat"
        erc-port 6667
        erc-nick "karthik"
        erc-user-full-name "Karthik"
        erc-prompt-for-password t
        ;; erc-track-shorten-start 6
        ;; erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters"
        ;;                                "#org-mode" "#emacs"))
        erc-kill-buffer-on-part t
        erc-lurker-threshold-time 1800
        erc-hide-list '("NICK")
        erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477")
        ;; erc-track-visibility t
        ;; erc-track-when-inactive t
        erc-format-nick-function 'erc-format-@nick
        erc-auto-query 'bury
        erc-keywords nil))

(use-package erc-image
  :disabled
  :after erc
  :ensure t
  :init
  (setq erc-image-inline-rescale 350)
  (add-to-list 'erc-modules 'image)
  (push 'button erc-modules)
  (push 'completion erc-modules)
  (erc-update-modules)
  :config
  (defun erc-image-create-image (file-name)
    "Create an image suitably scaled according to the setting of
'ERC-IMAGE-RESCALE."
    (let* ((positions (window-inside-absolute-pixel-edges))
           (width (- (nth 2 positions) (nth 0 positions)))
           (height (- (nth 3 positions) (nth 1 positions)))
           (image (create-image file-name))
           (dimensions (image-size image t))
           (imagemagick-p (and (fboundp 'imagemagick-types) 'imagemagick)))
                                        ; See if we want to rescale the image
      (if (and erc-image-inline-rescale
               (not (image-multi-frame-p image)))
          ;; Rescale based on erc-image-rescale
          (cond (;; Numeric: scale down to that size
                 (numberp erc-image-inline-rescale)
                 (let ((max-height (min (cdr dimensions)
                                        erc-image-inline-rescale
                                        (floor (* width (cdr dimensions))
                                               (car dimensions)))))
                   (if (> (floor (* max-height (car dimensions))
                                 (cdr dimensions))
                          width)
                       (create-image file-name imagemagick-p nil :width width)
                     (create-image file-name imagemagick-p nil :height max-height))))
                (;; 'window: scale down to window size, if bigger
                 (eq erc-image-inline-rescale 'window)
                 ;; But only if the image is greater than the window size
                 (if (or (> (car dimensions) width)
                         (> (cdr dimensions) height))
                     ;; Figure out in which direction we need to scale
                     (if (> width height)
                         (create-image file-name imagemagick-p nil :height  height)
                       (create-image file-name imagemagick-p nil :width width))
                   ;; Image is smaller than window, just give that back
                   image))
                (t (progn (message "Error: none of the rescaling options matched") image)))
        ;; No rescale
        image))))

(use-package erc-hl-nicks
  :ensure t
  :after erc
  :hook (erc-mode . erc-hl-nicks-mode))

(provide 'setup-erc)
;;; setup-erc.el ends here
