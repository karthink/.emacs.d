;;  simplenote.el --- Interact with simple-note.appspot.com

;; Copyright (C) 2009, 2010 Konstantinos Efstathiou <konstantinos@efstathiou.gr>

;; Author: Konstantinos Efstathiou <konstantinos@efstathiou.gr>
;; Keywords: simplenote

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


;;; Code:



(require 'cl)
(require 'url)
(require 'json)
(require 'widget)

(defcustom simplenote-directory (expand-file-name "~/.simplenote/")
  "Simplenote directory."
  :type 'directory
  :safe 'stringp
  :group 'simplenote)

(defcustom simplenote-email nil
  "Simplenote account email."
  :type 'string
  :safe 'stringp
  :group 'simplenote)

(defcustom simplenote-password nil
  "Simplenote account password."
  :type 'string
  :safe 'stringp
  :group 'simplenote)

(defcustom simplenote-notes-mode 'text-mode
  "The mode used for editing notes opened from Simplenote.

Since notes do not have file extensions, the default mode must be
set via this option.  Individual notes can override this setting
via the usual `-*- mode: text -*-' header line."
  :type 'function
  :group 'simplenote)

(defvar simplenote-mode-hook nil)

(put 'simplenote-mode 'mode-class 'special)


;;; Simplenote authentication

(defun simplenote-encode-post-data (string)
  (concat (base64-encode-string string) "\n"))

(defvar simplenote-key nil)
(make-variable-buffer-local 'simplenote-key)

(defvar simplenote-email-was-read-interactively nil)
(defvar simplenote-password-was-read-interactively nil)

(defun simplenote-email ()
  (when (not simplenote-email)
    (setq simplenote-email (read-string "Simplenote email: "))
    (setq simplenote-email-was-read-interactively t))
  simplenote-email)

(defun simplenote-password ()
  (when (not simplenote-password)
    (setq simplenote-password (read-passwd "Simplenote password: "))
    (setq simplenote-password-was-read-interactively t))
  simplenote-password)

(defun simplenote-get-token (email password)
  (let ((url "https://simple-note.appspot.com/api/login") 
        (url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (simplenote-encode-post-data (format "email=%s&password=%s"
                                        (url-hexify-string email)
                                        (url-hexify-string password)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (when (eql url-http-response-status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (buffer-substring (1+ (point)) (point-max))))))

(defun simplenote-token ()
  (interactive)
  (let ((token (simplenote-get-token (simplenote-email) (simplenote-password))))
    (if token
        (message "Simplenote authentication succeeded")
      (if simplenote-email-was-read-interactively
          (setq simplenote-email nil))
      (if simplenote-password-was-read-interactively
          (setq simplenote-password nil))
      (message "Simplenote authentication failed"))
    token))


;;; API calls for index and notes

(defun simplenote-get-index (token email)
  (let (url status headers data index)
    (setq url (format "https://simple-note.appspot.com/api/index?auth=%s&email=%s"
                      (url-hexify-string token)
                      (url-hexify-string email)))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq headers (buffer-substring (point-min) (point)))
        (setq data (buffer-substring (1+ (point)) (point-max)))
        (setq index (json-read-from-string data))))
    index))

(defun simplenote-get-note (key token email)
  (let (url status headers data note-key note-modifydate note-createdate note-deleted)
    (setq url (format
               "https://simple-note.appspot.com/api/note?key=%s&auth=%s&email=%s"
               (url-hexify-string key)
               (url-hexify-string token)
               (url-hexify-string email)))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq headers (buffer-substring (point-min) (point)))
        (setq data (decode-coding-string
                    (buffer-substring (1+ (point)) (point-max))
                    'utf-8))
        (string-match "^note-key: \\(.*\\)$" headers)
        (setq note-key (match-string 1 headers))
        (string-match "^note-modifydate: \\(.*\\)$" headers)
        (setq note-modifydate (date-to-time (match-string 1 headers)))
        (string-match "^note-createdate: \\(.*\\)$" headers)
        (setq note-createdate (date-to-time (match-string 1 headers)))
        (string-match "^note-deleted: \\(.*\\)$" headers)
        (setq note-deleted (match-string 1 headers))))
    (values data note-key note-createdate note-modifydate note-deleted)))

(defun simplenote-mark-note-as-deleted (key token email)
  (let (url)
    (setq url (format
               "https://simple-note.appspot.com/api/delete?key=%s&auth=%s&email=%s"
               (url-hexify-string key)
               (url-hexify-string token)
               (url-hexify-string email)))
    (with-current-buffer (url-retrieve-synchronously url)
      (eql url-http-response-status 200))))

(defun simplenote-update-note (key text token email &optional modifydate)
  (let (url url-request-method url-request-data status note-key)
    (if modifydate
        (setq url (format
                   "https://simple-note.appspot.com/api/note?key=%s&auth=%s&email=%s&modify=%s"
                   (url-hexify-string key)
                   (url-hexify-string token)
                   (url-hexify-string email)
                   (url-hexify-string (format-time-string "%Y-%m-%d %H:%M:%S" modifydate))))
      (setq url (format
                 "https://simple-note.appspot.com/api/note?key=%s&auth=%s&email=%s"
                 (url-hexify-string key)
                 (url-hexify-string token)
                 (url-hexify-string email))))
    (setq url-request-method "POST")
    (setq url-request-data (simplenote-encode-post-data text))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq note-key (buffer-substring (1+ (point)) (point-max)))))
    note-key))

(defun simplenote-create-note (text token email &optional createdate)
  (let (url url-request-method url-request-data status headers note-key)
    (if createdate
        (setq url (format
                   "https://simple-note.appspot.com/api/note?auth=%s&email=%s&create=%s"
                   (url-hexify-string token)
                   (url-hexify-string email)
                   (url-hexify-string (format-time-string "%Y-%m-%d %H:%M:%S" createdate))))
      (setq url (format
                 "https://simple-note.appspot.com/api/note?auth=%s&email=%s"
                 (url-hexify-string token)
                 (url-hexify-string email))))
    (setq url-request-method "POST")
    (setq url-request-data (simplenote-encode-post-data text))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq note-key (buffer-substring (1+ (point)) (point-max)))))
    note-key))


;;; Push and pull buffer as note

(defun simplenote-file-mtime-gmt (path)
  (let (mtime tz-offset)
    (setq mtime (nth 5 (file-attributes path)))
    (setq tz-offset (nth 8 (decode-time mtime)))
    (time-add mtime (butlast (seconds-to-time (- tz-offset))))))

(defun simplenote-push-buffer ()
  (interactive)
  (let (modifydate success)
    (save-buffer)
    (setq modifydate (simplenote-file-mtime-gmt (buffer-file-name)))
    (setq success (simplenote-update-note simplenote-key
                                          (encode-coding-string (buffer-string) 'utf-8 t)
                                          (simplenote-token)
                                          (simplenote-email)
                                          modifydate))
    (if success
        (message "Pushed note %s" simplenote-key)
      (message "Failed to push note %s" simplenote-key))))

(defun simplenote-create-note-from-buffer ()
  (interactive)
  (let (createdate key)
    (save-buffer)
    (setq createdate (simplenote-file-mtime-gmt (buffer-file-name)))
    (setq key (simplenote-create-note (encode-coding-string (buffer-string) 'utf-8 t)
                                      (simplenote-token)
                                      (simplenote-email)
                                      createdate))
    (if key
        (progn
          (setq simplenote-key key)
          (message "Created note %s" key)
          (add-file-local-variable 'simplenote-key key)
          (simplenote-push-buffer))
      (message "Failed to create new note"))))

(defun simplenote-pull-buffer ()
  (interactive)
  (multiple-value-bind (data note-key note-createdate note-modifydate note-deleted)
      (simplenote-get-note simplenote-key
                           (simplenote-token)
                           (simplenote-email))
    (if data
        (progn
          (erase-buffer)
          (insert data)
          (message "Pulled note %s" simplenote-key))
      (message "Failed to pull note %s" simplenote-key))))


;;; Browser helper functions

(defun simplenote-trash-dir ()
  (file-name-as-directory (concat (file-name-as-directory simplenote-directory) "trash")))

(defun simplenote-notes-dir ()
  (file-name-as-directory (concat (file-name-as-directory simplenote-directory) "notes")))

(defun simplenote-new-notes-dir ()
  (file-name-as-directory (concat (file-name-as-directory simplenote-directory) "new")))

(defun simplenote-setup ()
  (interactive)
  (when (not (file-exists-p simplenote-directory))
    (make-directory simplenote-directory t))
  (when (not (file-exists-p (simplenote-notes-dir)))
    (make-directory (simplenote-notes-dir) t))
  (when (not (file-exists-p (simplenote-trash-dir)))
    (make-directory (simplenote-trash-dir) t))
  (when (not (file-exists-p (simplenote-new-notes-dir)))
    (make-directory (simplenote-new-notes-dir) t)))

(defun simplenote-filename-for-note (key)
  (concat (simplenote-notes-dir) key))

(defun simplenote-filename-for-note-marked-deleted (key)
  (concat (simplenote-trash-dir) key))

(defun simplenote-file-contents (file)
  (let (temp-buffer contents)
    (setq temp-buffer (get-buffer-create " *simplenote-temp*"))
    (with-current-buffer temp-buffer
      (insert-file-contents file nil nil nil t)
      (setq contents (encode-coding-string (buffer-string) 'utf-8 t)))
    (kill-buffer " *simplenote-temp*")
    contents))

(defvar simplenote-note-head-size 78
  "Length of note headline in the notes list.")

(defun simplenote-note-headline (text)
  "The first non-empty line of a note."
  (let ((begin (string-match "^.+$" text)))
    (when begin
      (substring text begin (min (match-end 0)
                                 (+ begin simplenote-note-head-size))))))

(defun simplenote-note-headrest (text)
  "Text after the first non-empty line of a note, to fill in the list display."
  (let* ((headline (simplenote-note-headline text))
         (text (replace-regexp-in-string "\n" " " text))
         (begin (when headline (string-match (regexp-quote headline) text))))
    (when begin (substring text (match-end 0)
                           (min (length text)
                                (+ (match-end 0) (- simplenote-note-head-size (length headline))))))))

(defun simplenote-open-note (file)
  "Opens FILE in a new buffer, setting its mode, and returns the buffer.

The major mode of the resulting buffer will be set to
`simplenote-notes-mode' but can be overridden by a file-local
setting."
  (prog1 (find-file file)
    ;; Don't switch mode when set via file cookie
    (when (eq major-mode (default-value 'major-mode))
      (funcall simplenote-notes-mode))
    ;; Refresh notes display after save
    (add-hook 'after-save-hook 
              (lambda () (save-excursion (simplenote-browser-refresh)))
              nil t)))


;; Simplenote sync

(defun simplenote-sync-notes ()
  "Synchronize local notes with the simplenote server."
  (interactive)

  (let (index index-keys files files-marked-deleted)

    ;; Try to download the index. If this fails then the connection is broken or
    ;; authentication failed. Abort sync.
    (setq index (simplenote-get-index (simplenote-token) (simplenote-email)))
    (if (not index)
        (message "Could not retrieve the index. Aborting sync.")

      (setq keys-in-index (mapcar '(lambda (e) (cdr (assoc 'key e))) index))
      (setq files (directory-files (simplenote-notes-dir) t "^[a-zA-Z0-9_\\-]+$"))
      (setq files-marked-deleted (directory-files (simplenote-trash-dir) t "^[a-zA-Z0-9_\\-]+$"))

      ;; If a file has been marked locally as deleted then sync the deletion and
      ;; delete from the file system provided that the corresponding key is in
      ;; the index. If the key is not in the index just delete the local file.
      (loop for file in files-marked-deleted do
            (let (key success)
              (setq key (file-name-nondirectory file))
              (if (member key keys-in-index)
                (progn
                  (setq success (simplenote-mark-note-as-deleted key
                                                                 (simplenote-token)
                                                                 (simplenote-email)))
                  (when success
                    (message "Marked note %s as deleted on the server" key)
                    (message "Deleting local file %s" file)
                    (delete-file file)
                    (setq keys-in-index (delete key keys-in-index))
                    (setq index (delete-if
                                 '(lambda (e) (equal key (cdr (assoc 'key e))))
                                 index))))
                (message "Local file %s has been marked deleted locally and does not appear in the index. Deleting." file)
                (delete-file file))))
      
      ;; Loop over all notes in the index.
      (loop for elem across index do
            (let (key deleted modify file note-text note-key temp-buffer)
              (setq key (cdr (assoc 'key elem)))
              (setq deleted (eq (cdr (assoc 'deleted elem)) t))
              (setq modify (date-to-time (cdr (assoc 'modify elem))))
              (setq file (simplenote-filename-for-note key))
              ;; Remove the file corresponding to this index element from the
              ;; list of files. At the end of the loop `files` will contain only
              ;; those files that (1) have not been marked locally as deleted
              ;; and (2) they are not contained in the index.
              (setq files (delete file files))
              (when (not deleted)
                ;; Download
                (when (or (not (file-exists-p file))
                          (time-less-p (nth 5 (file-attributes file)) modify))
                  (message "Downloading note %s from Simplenote" key)
                  (multiple-value-bind (note-text note-key note-createdate
                                                  note-modifydate note-deleted)
                      (simplenote-get-note key (simplenote-token) (simplenote-email))
                    (if note-text
                        (progn
                          (message "Downloaded note %s" key)
                          (write-region note-text nil file nil)
                          (set-file-times file note-modifydate))
                      (message "Failed to download note %s" key))))
                ;; Upload
                (when (and (file-exists-p file)
                           (time-less-p modify (nth 5 (file-attributes file))))
                  (message "Uploading note %s to Simplenote" key)
                  (setq note-text (simplenote-file-contents file))
                  (setq note-key (simplenote-update-note key
                                                         note-text
                                                         (simplenote-token)
                                                         (simplenote-email)
                                                         (simplenote-file-mtime-gmt file)))
                  (if note-key
                      (message "Uploaded note %s" note-key)
                    (message "Failed to upload note %s" note-key))))
              ;; If a note in the index is marked as deleted and the
              ;; corresponding local file exists then delete the file.
              (when (and deleted (file-exists-p file))
                (message "Note %s has been marked deleted on the server. Deleting local file %s" key file)
                (delete-file file))))
      
      ;; If a file is not in the index then delete it from the file system.
      (loop for file in files do
            (let (key)
              (setq key (file-name-nondirectory file))
              (if (member key keys-in-index)
                  (message "Key %s is not supposed to be in the index." key)
                (message "The note %s has not been found in the index. Deleting file %s" key file)
                (delete-file file))))
      
      ;; If a new file has been locally created then create a new note on the
      ;; server and rename the local file after getting the key of the new note
      ;; from the server.
      (loop for file in (directory-files (simplenote-new-notes-dir) t "^note-[0-9]+$") do
            (let (text note-key mod-time)
              (setq text (simplenote-file-contents file))
              (setq mod-time (nth 5 (file-attributes file)))
              (setq note-key (simplenote-create-note text
                                                     (simplenote-token)
                                                     (simplenote-email)
                                                     (simplenote-file-mtime-gmt file)))
              (when note-key
                (message "Created new note on the server with key %s" note-key)
                (let (new-filename)
                  (setq new-filename (simplenote-filename-for-note note-key))
                  (rename-file file new-filename)
                  (set-file-times new-filename mod-time)))))
      
      ;; Refresh the browser
      (save-excursion
        (simplenote-browser-refresh)))))


;;; Simplenote browser

(defvar simplenote-mode-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map (kbd "g") 'simplenote-sync-notes)
    (define-key map (kbd "q") 'quit-window)
    map))

(defun simplenote-mode ()
  "Browse and edit Simplenote notes locally and sync with the server.

\\{simplenote-mode-map}"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map simplenote-mode-map)
  (simplenote-menu-setup)
  (setq major-mode 'simplenote-mode
        mode-name "Simplenote")
  (run-mode-hooks 'simplenote-mode-hook))

(defun simplenote-browse ()
  (interactive)
  (when (not (file-exists-p simplenote-directory))
      (make-directory simplenote-directory t))
  (switch-to-buffer "*Simplenote*")
  (simplenote-mode)
  (goto-char 1))

(defun simplenote-browser-refresh ()
  (interactive)
  (when (get-buffer "*Simplenote*")
    (set-buffer "*Simplenote*")
    (simplenote-menu-setup)))


(defun simplenote-menu-setup ()
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Buttons
  (widget-create 'link
                 :format "%[%v%]"
                 :help-echo "Synchronize with the Simplenote server"
                 :notify (lambda (widget &rest ignore)
                           (simplenote-sync-notes)
                           (simplenote-browser-refresh))
                 "Sync with server")
  (widget-insert "  ")
  (widget-create 'link
                 :format "%[%v%]"
                 :help-echo "Create a new note"
                 :notify (lambda (widget &rest ignore)
                           (let (buf)
                             (setq buf (simplenote-create-note-locally))
                             (simplenote-browser-refresh)
                             (switch-to-buffer buf)))
                 "Create new note")
  (widget-insert "\n\n")
  ;; New notes list
  (let ((new-notes (directory-files (simplenote-new-notes-dir) t "^note-[0-9]+$")))
    (when new-notes
      (widget-insert "== NEW NOTES\n\n")
      (mapc 'simplenote-new-note-widget new-notes)))
  ;; Other notes list
  (let (files)
    (setq files (append
                 (mapcar '(lambda (file) (cons file nil))
                         (directory-files (simplenote-notes-dir) t "^[a-zA-Z0-9_\\-]+$"))
                 (mapcar '(lambda (file) (cons file t))
                         (directory-files (simplenote-trash-dir) t "^[a-zA-Z0-9_\\-]+$"))))
    (when files
      (setq files (sort files '(lambda (p1 p2) (simplenote-file-newer-p (car p1) (car p2)))))
      (widget-insert "== NOTES\n\n")
      (mapc 'simplenote-other-note-widget files)))
  (use-local-map simplenote-mode-map)
  (widget-setup))

(defun simplenote-file-newer-p (file1 file2)
  (let (time1 time2)
    (setq time1 (nth 5 (file-attributes file1)))
    (setq time2 (nth 5 (file-attributes file2)))
    (time-less-p time2 time1)))
  
(defun simplenote-new-note-widget (file)
  (let* ((modify (nth 5 (file-attributes file)))
         (modify-string (format-time-string "%Y-%m-%d %H:%M:%S" modify))
         (note (decode-coding-string (simplenote-file-contents file) 'utf-8 t))
         (headline (simplenote-note-headline note))
         (shorttext (simplenote-note-headrest note)))
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%v%]"
                   :tag file
                   :help-echo "Edit this note"
                   :notify (lambda (widget &rest ignore)
                             (simplenote-open-note (widget-get widget :tag)))
                   headline)
    (widget-insert shorttext "\n")
    (widget-insert "  " modify-string "\t                                      \t")
    (widget-create 'link
                   :tag file
                   :value "Edit"
                   :format "%[%v%]"
                   :help-echo "Edit this note"
                   :notify (lambda (widget &rest ignore)
                             (simplenote-open-note (widget-get widget :tag)))
                    "Edit")
    (widget-insert " ")
    (widget-create 'link
                   :format "%[%v%]"
                   :tag file
                   :help-echo "Permanently remove this file"
                   :notify (lambda (widget &rest ignore)
                             (delete-file (widget-get widget :tag))
                             (simplenote-browser-refresh))
                   "Remove")
    (widget-insert "\n\n")))

(defun simplenote-other-note-widget (pair)
  (let* ((file (car pair))
         (deleted (cdr pair))
         (key (file-name-nondirectory file))
         (modify (nth 5 (file-attributes file)))
         (modify-string (format-time-string "%Y-%m-%d %H:%M:%S" modify))
         (note (decode-coding-string (simplenote-file-contents file) 'utf-8 t))
         (headline (simplenote-note-headline note))
         (shorttext (simplenote-note-headrest note)))
    (widget-create 'link 
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%v%]"
                   :tag file
                   :help-echo "Edit this note"
                   :notify (lambda (widget &rest ignore)
                             (simplenote-open-note (widget-get widget :tag)))
                   headline)
    (widget-insert shorttext "\n")
    (widget-insert "  " modify-string "\t" (propertize key 'face 'shadow) "\t")
    (widget-create 'link
                   :tag file
                   :value "Edit"
                   :format "%[%v%]"
                   :help-echo "Edit this note"
                   :notify (lambda (widget &rest ignore)
                             (simplenote-open-note (widget-get widget :tag)))
                    "Edit")
    (widget-insert " ")
    (widget-create 'link
                   :format "%[%v%]"
                   :tag key
                   :help-echo (if deleted
                                  "Mark this note as not deleted"
                                "Mark this note as deleted")
                   :notify (if deleted
                               simplenote-undelete-me
                             simplenote-delete-me)
                   (if deleted
                       "Undelete"
                     "Delete"))
    (widget-insert "\n\n")))

(setq simplenote-delete-me
      (lambda (widget &rest ignore)
        (simplenote-mark-note-for-deletion (widget-get widget :tag))
        (widget-put widget :notify simplenote-undelete-me)
        (widget-value-set widget "Undelete")
        (widget-setup)))

(setq simplenote-undelete-me
  (lambda (widget &rest ignore)
    (simplenote-unmark-note-for-deletion (widget-get widget :tag))
    (widget-put widget :notify simplenote-delete-me)
    (widget-value-set widget "Delete")
    (widget-setup)))

(defun simplenote-mark-note-for-deletion (key)
  (rename-file (simplenote-filename-for-note key)
               (simplenote-filename-for-note-marked-deleted key)))

(defun simplenote-unmark-note-for-deletion (key)
  (rename-file (simplenote-filename-for-note-marked-deleted key)
               (simplenote-filename-for-note key)))

(defun simplenote-create-note-locally ()
  (let (new-filename counter)
    (setq counter 0)
    (setq new-filename (concat (simplenote-new-notes-dir) (format "note-%d" counter)))
    (while (file-exists-p new-filename)
      (setq counter (1+ counter))
      (setq new-filename (concat (simplenote-new-notes-dir) (format "note-%d" counter))))
    (write-region "New note" nil new-filename nil)
    (simplenote-browser-refresh)
    (simplenote-open-note new-filename)))


(provide 'simplenote)

;;; simplenote.el ends here
