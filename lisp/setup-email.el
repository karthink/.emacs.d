;; -*- lexical-binding: t -*-
;; 
;; * SENDMAIL
;;
;; We use msmtp. Also of note is automatic selection of the
;; right from address when replying to email.
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
                              my-alt-email-addresses)
                    my-alt-email-dir))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))
  ;; the original form of this script did not have the ' before "a" which causes
  ;; a very difficult to track bug --frozencemetery
  (add-hook 'message-send-mail-hook #'cg-feed-msmtp)
  )

;;* NOTMUCH

(use-package notmuch
  :straight (:type built-in)
  :commands notmuch
  :bind (("C-x m" . notmuch-mua-new-mail)
         ("C-x M-m" . notmuch-jump-search)
         :map notmuch-search-mode-map
         ("RET"   . notmuch-tree-from-search-thread)
         ("M-RET" . notmuch-search-show-thread)
         :map notmuch-tree-mode-map
         ("M-s u" . my/notmuch-tree-browse-url)
         ("<tab>" . my/notmuch-tree-message-push-button)
         ("S-SPC" . notmuch-tree-scroll-message-window-back))
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

  (setq notmuch-search-line-faces
        '(("flagged"   . (:inherit notmuch-search-flagged-face
                          :foreground "IndianRed"))
          ("ucsb"      . (:foreground "MediumPurple"))
	  ("important" . (:foreground "CornflowerBlue"))
          ("unread"    . (:inherit notmuch-search-unread-face))))
  
  ;; Hide patches and diffs in notmuch-show by default. Note: To turn them into
  ;; attachments from inline components you can customize
  ;; `mm-inline-override-types' instead. This method simply toggles their inline
  ;; display.
  (advice-add 'notmuch-show-insert-bodypart :filter-args 'my/notmuch-hide-content)

  (defvar my/notmuch-hide-content-types '("text/x-patch" "text/x-diff"))

  (defun my/notmuch-hide-content (args)
    (cl-destructuring-bind (msg part depth . hide) args
      (list msg part depth
            (if-let ((mime-type (notmuch-show-mime-type part))
                     (_ (seq-some (lambda (type) (notmuch-match-content-type mime-type type))
                                  my/notmuch-hide-content-types)))
                t (car hide)))))
  
  (defun my/notmuch-tree-browse-url (&optional arg)
    (interactive "P")
    (when (window-live-p notmuch-tree-message-window)
      (with-selected-window notmuch-tree-message-window
        (my/search-occur-browse-url arg))))
  
  (defun notmuch-search-make-tagger (&rest tags)
    (lambda () (interactive)
      (let ((taglist (mapcar (lambda (tag)
                               (if (member tag (notmuch-search-get-tags))
                                   (concat "-" tag)
                                 (concat "+" tag)))
                             tags)))
        (notmuch-search-tag taglist)
        (message "Tagged: %s" (mapconcat #'identity taglist " "))
        (notmuch-search-next-thread))))
  
  (defun notmuch-show-make-tagger (&rest tags)
    (lambda () (interactive)
      (let ((taglist (mapcar (lambda (tag)
                               (if (member tag (notmuch-show-get-tags))
                                   (concat "-" tag)
                                 (concat "+" tag)))
                             tags)))
        (notmuch-show-tag taglist)
        (message "Tagged: %s" (mapconcat #'identity taglist " "))
        (notmuch-show-next-message))))
  
  (defun notmuch-tree-make-tagger (&rest tags)
    (lambda (&optional all) (interactive "P")
      (let ((taglist (mapcar (lambda (tag)
                               (if (member tag (notmuch-tree-get-tags))
                                   (concat "-" tag)
                                 (concat "+" tag)))
                             tags)))
        (if all
            (notmuch-tree-tag-thread taglist)
          (notmuch-tree-tag taglist))
        (message "Tagged: %s" (mapconcat #'identity taglist " "))
        (notmuch-tree-next-thread))))

  (define-key notmuch-search-mode-map (kbd "f") (notmuch-search-make-tagger "flagged"))
  (define-key notmuch-show-mode-map (kbd "f") (notmuch-show-make-tagger "flagged"))
  (define-key notmuch-tree-mode-map (kbd "f") (notmuch-tree-make-tagger "flagged"))

  (define-key notmuch-search-mode-map (kbd "d") (notmuch-search-make-tagger "trash" "inbox"))
  (define-key notmuch-show-mode-map (kbd "d") (notmuch-show-make-tagger "trash" "inbox"))
  (define-key notmuch-tree-mode-map (kbd "d") (notmuch-tree-make-tagger "trash" "inbox"))

  (defun my/notmuch-tree-message-push-button ()
    (interactive)
    (when (window-live-p notmuch-tree-message-window)
      (my/avy-link-hint notmuch-tree-message-window)))

 ;;; (define-key notmuch-show-mode-map "`" 'notmuch-show-apply-tag-macro)
  ;;; (define-key notmuch-search-mode-map "`" 'notmuch-show-apply-tag-macro)
    ;
  ;;; (define-prefix-command notmuch-tagger-map)
  ;;; (let ((map notmuch-tagger-map))
  ;;;   (define-key map "")
  ;;;   )
    ;
  ;;; (defun notmuch-show-apply-tag-macro (key)
  ;;;   (interactive "k")
  ;;;   (let ((macro (assoc key notmuch-show-tag-macro-alist)))
  ;;;     (apply 'notmuch-show-tag-message (cdr macro))))
    ;
  ;;; (defun +disable-C-tab (mode-map)
  ;;;   "Disable Control-Tab in mode-map"
  ;;;   (define-key mode-map (kbd "C-TAB") nil)
  ;;;   (define-key mode-map (kbd "C-<tab>") nil))
    ;
  ;;; (dolist (mode '((notmuch-hello-mode-hook . notmuch-hello-mode-map)
  ;;;                 (notmuch-show-mode-hook . notmuch-show-mode-map)
  ;;;                 (notmuch-message-mode-hook . notmuch-message-mode-map)
  ;;;                 (notmuch-tree-mode-hook . notmuch-tree-mode-map)
  ;;;                 (notmuch-search-mode-hook . notmuch-search-mode-map)))
  ;;;   (let ((mode-hook (car mode))
  ;;;         (mode-map (cdr mode)))
  ;;;     (add-hook mode-hook
  ;;;               (lambda ()
  ;;;                 (+disable-C-tab mode-map)))))
    ;
    ;
  ;;; (define-key notmuch-common-keymap (kbd "C-TAB") nil)
  ;;; (define-key notmuch-common-keymap (kbd "C-<tab>") nil)
    ;
    ;
  ;;; (add-hook 'notmuch-show-hook #'my-next-unread)
  ;;; (defun my-next-unread ()
  ;;;   (interactive)
  ;;;   (let ((init (point)))
  ;;;     (catch 'break
  ;;;       (while t
  ;;;         (when (member "unread" (notmuch-show-get-tags))
  ;;;           (let ((props (notmuch-show-get-message-properties)))
  ;;;             (notmuch-show-message-visible props t)
  ;;;             (notmuch-show-mark-read)
  ;;;             (throw 'break t)))
  ;;;         (when (not (notmuch-show-goto-message-next))
  ;;;           (message "No more unread messages.")
  ;;;           (goto-char init)
  ;;;           (throw 'break t))))))
  )

(use-package notmuch-bookmarks
:after notmuch
:config
(notmuch-bookmarks-mode))

;; * CONSULT-NOTMUCH

;; [[https://melpa.org/#/consult-notmuch][consult-notmuch]] provides consult
;; commands for live-searching a notmuch database.
;;
;; I use a couple of date-oriented custom notmuch search commands instead of the
;; default, bound to my "search map" (=M-s=) prefix:

(use-package consult-notmuch
  :straight t
  :after consult
  :bind (("M-s M-m" . consult-notmuch-latest-tree)
         ("M-s m" . consult-notmuch-latest))
  :config
  (defun consult-notmuch-latest (&optional arg)
    (interactive "P")
    (let ((consult-async-input-debounce 0.6)
          (consult-async-input-throttle 0.7))
      (consult-notmuch
       (unless arg "tag:inbox date:1d.."))))
  (defun consult-notmuch-latest-tree (&optional arg)
    (interactive "P")
    (let ((consult-async-input-debounce 0.6)
          (consult-async-input-throttle 0.7))
      (consult-notmuch-tree
       (unless arg "tag:inbox date:1d..")))))

;; Add a notmuch buffer-source to =consult-buffer=. This is provided by
;; consult-notmuch.

(use-package consult
  :after (notmuch consult-notmuch)
  :config
  (add-to-list 'consult-buffer-sources 'consult-notmuch-buffer-source))

;; Embark actions on =consult-notmuch= search results to add/remove tags, and an
;; Embark exporter that displays search results in a =notmuch-search= buffer.
;;
;; This code [[https://codeberg.org/jao/consult-notmuch/pulls/6][will be part of
;; consult-notmuch]] soon.

(use-package embark
  :after (notmuch embark)
  :config
  (defun embark-notmuch-make-tagger (tags)
    "Make a function to tag a message with TAGS."
    (lambda (msg)
      "Tag a notmuch message using Embark."
      (when-let ((thread-id (consult-notmuch--thread-id msg)))
        (notmuch-tag (concat "(" thread-id ")")
                     (split-string tags)))))
  (defun embark-export-consult-notmuch (msgs)
    "Create a notmuch search buffer listing messages."
    (notmuch-search
     (concat "("
             (mapconcat #'consult-notmuch--thread-id msgs " ")
             ")")))
  
  (defun embark-notmuch-tag (msg)
    (when-let* ((thread-id (consult-notmuch--thread-id msg))
                (tags (get-text-property 0 'tags msg))
                (tag-changes (notmuch-read-tag-changes
                              tags "Tags: "
                             "+")))
      (notmuch-tag (concat "(" thread-id ")")
                   tag-changes)))
  
  (defvar embark-notmuch-map 
    (let ((map (make-sparse-keymap))) 
      (define-key map (kbd "d") (embark-notmuch-make-tagger "+trash -inbox"))
      (define-key map (kbd "a") (embark-notmuch-make-tagger "-inbox"))
      (define-key map (kbd "f") (embark-notmuch-make-tagger "+flagged"))
      (define-key map (kbd "+") 'embark-notmuch-tag)
      (define-key map (kbd "-") 'embark-notmuch-tag)
      map)
    "Keymap for actions on Notmuch entries.")
  (add-to-list 'embark-keymap-alist '(notmuch-result . embark-notmuch-map))
  (add-to-list 'embark-exporters-alist '(notmuch-result . embark-export-consult-notmuch)))

(provide 'setup-email)
