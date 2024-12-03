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
        sendmail-program "msmtp"
        mail-specify-envelope-from t
        message-kill-buffer-on-exit t
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
                   ;; I use email address as account label in ~/.msmtprc
                  (alist-get from my-alt-email-dirs nil nil #'string-match-p)))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))
  ;; the original form of this script did not have the ' before "a" which causes
  ;; a very difficult to track bug --frozencemetery
  (add-hook 'message-send-mail-hook #'cg-feed-msmtp))

;;* NOTMUCH

(use-package notmuch
  ;; :straight (:type built-in)
  :ensure nil
  :commands notmuch
  :bind (("C-x m" . notmuch-mua-new-mail)
         ("C-x M-m" . notmuch-jump-search)
         :map notmuch-message-mode-map
         ("C-c C-s" . nil)
         :map notmuch-search-mode-map
         ("RET"   . notmuch-tree-from-search-thread)
         ("M-RET" . notmuch-search-show-thread)
         :map notmuch-tree-mode-map
         ("h" . my/notmuch-tree-show-hide-header)
         ("M-s u" . my/notmuch-tree-browse-url)
         ("<tab>" . my/notmuch-tree-message-push-button)
         ("S-SPC" . notmuch-tree-scroll-message-window-back))
  :hook ((notmuch-message-mode . turn-off-auto-fill)
         (notmuch-search-mode . (lambda () (setq line-spacing 0.15)))
         (notmuch-mua-send . notmuch-mua-attachment-check))
  :config
  (setq-default notmuch-search-oldest-first nil
                notmuch-show-indent-content nil)
  (setq notmuch-fcc-dirs '(("contact@" . "sent +sent"))
        notmuch-show-logo nil
        notmuch-column-control 0.6
        notmuch-message-headers-visible nil
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

  (pcase-dolist (`(,key . ,ops)
                 '(("d" ("+trash" "-inbox") "Delete")
                   ("m" ("+money") "Money")
                   ("b" ("+bills" "-inbox") "Bills")
                   ("H" ("+medical") "Medical")
                   ("I" ("+insurance") "Insurance")
                   ("X" ("+tax") "Tax")
                   ("p" ("+purchases") "Purchases")))
    (setf (alist-get key notmuch-tagging-keys nil nil #'equal) ops))
  
  ;; Hide patches and diffs in notmuch-show by default. Note: To turn them into
  ;; attachments from inline components you can customize
  ;; `mm-inline-override-types' instead. This method simply toggles their inline
  ;; display.
  (advice-add 'notmuch-show-insert-bodypart :filter-args 'my/notmuch-hide-content)

  (defvar my/notmuch-hide-content-types '("text/x-patch" "text/x-diff"))

  (defun my/notmuch-tree-show-hide-header ()
    "Show or hide mail headers."
    (interactive)
    (when (window-live-p notmuch-tree-message-window)
      (with-selected-window notmuch-tree-message-window
        (notmuch-show-toggle-visibility-headers))))

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

  (advice-add 'notmuch-tree-next-thread
              :after (lambda (&rest _) (recenter)))

  (defun my/notmuch-tree-show-message-split-sensibly (orig-fn)
    "Split window sensibly when showing messages in notmuch-tree."
    (cl-letf (((symbol-function 'split-window-vertically)
               (lambda (_) (if (> (frame-width) 160)
                            (split-window-horizontally
                             (* 2 (floor (window-width) 5)))
                          (split-window-below
                           (/ (window-height) 4))))))
      (funcall orig-fn)))
  (advice-add 'notmuch-tree-show-message-in
              :around 'my/notmuch-tree-show-message-split-sensibly)

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
      (my/avy-link-hint notmuch-tree-message-window))))

;; Use corfu in notmuch buffers
(use-package notmuch-address
  :when (or (daemonp) (display-graphic-p))
  :defer
  :config
  (setq notmuch-address-use-company nil
        notmuch-address-selection-function #'ignore)
  (define-advice notmuch-address-setup (:after () use-corfu)
    (add-hook 'completion-at-point-functions
              (cape-company-to-capf 'notmuch-company)
              nil t)))

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
  :ensure t
  :after consult
  :bind (("M-s M-m" . consult-notmuch-latest-tree)
         ("M-s m" . consult-notmuch-latest))
  :config
  (consult-customize
   consult-notmuch-latest consult-notmuch-latest-tree
   consult-notmuch consult-notmuch-tree
   (list :debounce 0.4 'any))
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

;; From Yantar92's config:
;; https://github.com/yantar92/emacs-config/blob/master/config.org#browse-mailing-list-archive-urls-using-my-local-notmuch-archives
(use-package browse-url
  :after notmuch
  :init
  (defun yant/browse-url-orgmode-ml (url &optional new-window)
    "Open an orgmode list url using notmuch."
    (let ((id (and (or (string-match "^https?://orgmode\\.org/list/\\([^/]+\\)" url)
                       (string-match "^https?://list\\.orgmode\\.org/\\(?:orgmode/\\)?\\([^/]+\\)" url))
                   (match-string 1 url))))
      (when id
        (unless (notmuch-show (format "id:%s" id))
          (browse-url-default-browser url new-window)))))
  :config
  (add-to-list 'browse-url-handlers
               '("^https?://orgmode\\.org/list/\\([^/]+\\)"
                 . yant/browse-url-orgmode-ml))
  (add-to-list 'browse-url-handlers
               '("^https?://list\\.orgmode\\.org/\\(?:orgmode/\\)?\\([^/]+\\)"
                 . yant/browse-url-orgmode-ml)))

;; Commands to go to the web version of mailing list URLs.
(use-package notmuch
  :bind (:map notmuch-tree-mode-map
         ("x" . my/notmuch-browse-externally)
         ("C-c C-w" . my/notmuch-kill-web-url)
         :map notmuch-show-mode-map
         ("x" . my/notmuch-browse-externally)
         ("C-c C-w" . my/notmuch-kill-web-url))
  :config
  (defvar my-notmuch-web-urls-alist
    '(("<emacs-orgmode@gnu.org>"      . "https://list.orgmode.org/")
      ("<emacs-devel@gnu.org>"        . "https://yhetil.org/emacs-devel/")
      ("<comment@noreply.github.com>" . "https://github.com/")
      ("<mention@noreply.github.com>" . "https://github.com/"))
    "List of email addresses and web links for corresponding mailing lists.")
  (defun my/notmuch-browse-externally ()
    "If a web url is found for this email, open it in a browser."
    (interactive)
    (when-let ((url (my/notmuch-web-url)))
      (browse-url-default-browser url)))
  (defun my/notmuch-kill-web-url ()
    "If a web url is found for this email, add it to the kill-ring."
    (interactive)
    (when-let ((url (my/notmuch-web-url)))
      (message "Killed url: %s" url)
      (kill-new url)))
  (defun my/notmuch-web-url ()
    "Create a web link based on the message ID.

Uses entries in `my-notmuch-web-urls-alist'."
    (let* ((id (thread-last
                 (notmuch-show-get-message-id)
                 (string-remove-prefix "id:")
                 (string-remove-prefix "<")
                 (string-remove-suffix ">")))
           (to (concat (notmuch-show-get-to)
                       (notmuch-show-get-cc)))
          (addr-match
           (seq-find (lambda (email)
                       (string-match-p (regexp-quote email) to))
                     (map-keys my-notmuch-web-urls-alist))))
      (when addr-match
        (concat (map-elt my-notmuch-web-urls-alist addr-match)
                id)))))


(provide 'setup-email)
