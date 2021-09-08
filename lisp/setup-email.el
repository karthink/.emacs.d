;; -*- lexical-binding: t -*-
;;(require 'use-package nil t)
;; Outgoing email
(use-package sendmail
  :after (message notmuch)
  :config
  (setq mail-host-address "gmail.com"
        send-mail-function 'sendmail-send-it
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header)

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
  ;; the original form of this script did not have the ' before "a" which causes
  ;; a very difficult to track bug --frozencemetery
  (add-hook 'message-send-mail-hook #'cg-feed-msmtp)
  )

;;----------------------------------------------------------------------
;; NOTMUCH
;;----------------------------------------------------------------------
(use-package notmuch
  :commands notmuch
  :hook ((notmuch-message-mode . turn-off-auto-fill)
         (notmuch-mua-send . notmuch-mua-attachment-check))
  :config
  (setq-default notmuch-search-oldest-first nil)
  (setq notmuch-show-logo nil
        notmuch-fcc-dirs nil
        notmuch-column-control 0.6
        notmuch-message-headers-visible nil
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
        notmuch-always-prompt-for-sender t
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-hello-sections
        '(notmuch-hello-insert-saved-searches
          notmuch-hello-insert-alltags))

  (setq notmuch-search-line-faces '(("flagged"   . (:inherit notmuch-search-flagged-face
                                                    :foreground "IndianRed"))
                                    ("ucsb"      . (:foreground "MediumPurple"))
				    ("important" . (:foreground "CornflowerBlue"))
                                    ("unread"    . (:inherit notmuch-search-unread-face
                                                    ;; :background "gray16"
                                                    ))))
  
  (define-key notmuch-search-mode-map (kbd "d")
    (defun my/notmuch-toggle-trash ()
      (interactive)
      (let ((taglist (if (member "trash" (notmuch-search-get-tags))
                         '("+inbox" "-trash")
                       '("-inbox" "+trash"))))
        (notmuch-search-tag taglist)
        (notmuch-search-next-thread))))

  (defun notmuch-search-make-tagger (tag)
    (lambda () (interactive)
      (let ((taglist (if (member tag (notmuch-search-get-tags))
                         (list (concat "-" tag))
                       (list (concat "+" tag)))))
        (notmuch-search-tag taglist)
        (notmuch-search-next-thread))))
  
  (defun notmuch-show-make-tagger (tag)
    (lambda () (interactive)
      (let ((taglist (if (member tag (notmuch-show-get-tags))
                         (list (concat "-" tag))
                       (list (concat "+" tag)))))
        (notmuch-show-tag taglist)
        (notmuch-show-next-message))))
  
  (defun notmuch-tree-make-tagger (tag)
    (lambda (&optional all) (interactive "P")
      (let ((taglist (if (member tag (notmuch-tree-get-tags))
                         (list (concat "-" tag))
                       (list (concat "+" tag)))))
        (if all
            (notmuch-tree-tag-thread taglist)
          (notmuch-tree-tag taglist))
        (notmuch-tree-next-thread))))

  (define-key notmuch-search-mode-map (kbd "f") (notmuch-search-make-tagger "flagged"))
  (define-key notmuch-show-mode-map (kbd "f") (notmuch-show-make-tagger "flagged"))
  (define-key notmuch-tree-mode-map (kbd "f") (notmuch-tree-make-tagger "flagged"))

  
  ;; (define-key notmuch-show-mode-map "`" 'notmuch-show-apply-tag-macro)
  ;; (define-key notmuch-search-mode-map "`" 'notmuch-show-apply-tag-macro)

  ;; (define-prefix-command notmuch-tagger-map)
  ;; (let ((map notmuch-tagger-map))
  ;;   (define-key map "")
  ;;   )

  ;; (defun notmuch-show-apply-tag-macro (key)
  ;;   (interactive "k")
  ;;   (let ((macro (assoc key notmuch-show-tag-macro-alist)))
  ;;     (apply 'notmuch-show-tag-message (cdr macro))))
  
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

(use-package notmuch-bookmarks
:after notmuch
:config
(notmuch-bookmarks-mode))

(use-package counsel-notmuch
  :disabled
  :commands counsel-notmuch)

(provide 'setup-email)
