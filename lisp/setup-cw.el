(use-package cw
  :ensure (:host github :repo "karthink/consult-web-mini"
           :protocol ssh)
  :bind (:map help-map
         ("C-s" . cw-search)
         ("C-l" . cw-search-local))
  :init
  (with-eval-after-load 'embark
    (setf (alist-get 'consult-web embark-keymap-alist)
          'cw-embark-general-actions-map)

    (defvar-keymap cw-embark-general-actions-map
      :doc "Keymap for cw-embark"
      :parent embark-general-map)
    
    (defmacro with-cw-url (cand func &rest args)
      `(when-let ((url (and (stringp ,cand) (get-text-property 0 :url ,cand))))
        (,func url ,@args)))
    (when (fboundp 'browse-url-mpv)
      (keymap-set cw-embark-general-actions-map
                  "w" (defun cw-embark-copy (cand)
                        (with-cw-url cand kill-new)))
      
      (keymap-set cw-embark-general-actions-map
                  "m" (defun cw-embark-mpv (cand)
                        (when-let ((url (and (stringp cand) (get-text-property 0 :url cand))))
                          (browse-url-mpv url)))))
    (when (fboundp 'browse-url-mpv-enqueue)
      (keymap-set cw-embark-general-actions-map
                  "M-m" (defun cw-embark-mpv-enqueue (cand)
                          (when-let ((url (and (stringp cand) (get-text-property 0 :url cand))))
                            (browse-url-mpv-enqueue url)))))
    (defvar-keymap cw-embark-become-map
      :doc "Keymap for switching between cw commands."
      :parent embark-meta-map
      "l" #'cw-search-local
      "s" #'cw-search)
    (add-to-list 'embark-become-keymaps 'cw-embark-become-map))

  (with-eval-after-load 'vertico-multiform
    (setf (alist-get 'cw-search vertico-multiform-commands nil t) '(reverse)
          (alist-get 'cw-search-local vertico-multiform-commands nil t) '(reverse)))

  :config
  (setq cw-brave-api-key
        (lambda ()
          (let ((message-log-max nil)
                (inhibit-message t))
            (auth-source-pass-get 'secret "api/api.search.brave.com/search")))))
