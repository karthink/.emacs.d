;; -*- lexical-binding: t -*-
(require 'use-package nil t)
;; Outgoing email
(setq mail-host-address "gmail.com"
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
      notmuch-fcc-dirs nil
      )

;; Choose account label to feed msmtp -a option based on From header in Message buffer;
;; This function must be added to message-send-mail-hook for on-the-fly change of From address
;; before sending message since message-send-mail-hook is processed right before sending message.
(defun cg-feed-msmtp ()
  (if (message-mail-p)
      (save-excursion
        (let* ((from
                (save-restriction
                  (message-narrow-to-headers)
                  (message-fetch-field "from")))
               (account
                (cond
                 ;; I use email address as account label in ~/.msmtprc
                 ((string-match my-email-address from) my-email-dir)
                 ((seq-some (lambda (x) (string-match x from))
                            my-alt-email-addresses) my-alt-email-dir))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))
;; the original form of this script did not have the ' before "a" which causes a very difficult to track bug --frozencemetery
(add-hook 'message-send-mail-hook #'cg-feed-msmtp)

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)

;;----------------------------------------------------------------------
;; NOTMUCH
;;----------------------------------------------------------------------
(use-package notmuch
  :ensure t
  :commands notmuch
  :hook (notmuch-message-mode . turn-off-auto-fill)
  :config
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-show-logo nil
        notmuch-column-control 0.6
        notmuch-message-headers-visible nil
        notmuch-search-oldest-first nil
        notmuch-saved-searches my-notmuch-saved-searches
        message-kill-buffer-on-exit t
        ;; notmuch-search-result-format
        ;; '(("date" . "%12s ")
        ;;   ("count" . "%-7s ")
        ;;   ("authors" . "%-30s ")
        ;;   ("subject" . "%-72s ")
        ;;   ("tags" . "(%s)"))
        ;; notmuch-tag-formats
        ;; '(("unread" (propertize tag 'face 'notmuch-tag-unread)))
        notmuch-hello-sections
        '(notmuch-hello-insert-saved-searches
          notmuch-hello-insert-alltags))

  ;; (defun +disable-C-tab (mode-map)
  ;;   "Disable Control-Tab in mode-map"
  ;;   (define-key mode-map (kbd "C-TAB") nil)
  ;;   (define-key mode-map (kbd "C-<tab>") nil))

  ;; (dolist (mode '((notmuch-hello-mode-hook . notmuch-hello-mode-map)
  ;;                 (notmuch-show-mode-hook . notmuch-show-mode-map)
  ;;                 (notmuch-message-mode-hook . notmuch-message-mode-map)
  ;;                 (notmuch-tree-mode-hook . notmuch-tree-mode-map)
  ;;                 (notmuch-search-mode-hook . notmuch-search-mode-map)))
  ;;   (let ((mode-hook (car mode))
  ;;         (mode-map (cdr mode)))
  ;;     (add-hook mode-hook
  ;;               (lambda ()
  ;;                 (+disable-C-tab mode-map)))))


  ;; (define-key notmuch-common-keymap (kbd "C-TAB") nil)
  ;; (define-key notmuch-common-keymap (kbd "C-<tab>") nil)


  ;; (add-hook 'notmuch-show-hook #'my-next-unread)
  ;; (defun my-next-unread ()
  ;;   (interactive)
  ;;   (let ((init (point)))
  ;;     (catch 'break
  ;;       (while t
  ;;         (when (member "unread" (notmuch-show-get-tags))
  ;;           (let ((props (notmuch-show-get-message-properties)))
  ;;             (notmuch-show-message-visible props t)
  ;;             (notmuch-show-mark-read)
  ;;             (throw 'break t)))
  ;;         (when (not (notmuch-show-goto-message-next))
  ;;           (message "No more unread messages.")
  ;;           (goto-char init)
  ;;           (throw 'break t))))))
  )

(use-package counsel-notmuch
  :ensure t
  :commands counsel-notmuch)

(provide 'setup-email)
