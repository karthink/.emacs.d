;;; moviedata.el --- Fetch movie details as Org properties  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: multimedia, hypermedia

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
(require 'plz)
(require 'auth-source-pass)

(defvar moviedata-post-insert-hook nil
  "Hook run after inserting movie data in an Org file.")

(defun moviedata (type query)
  (interactive (list (completing-read "Type: " '("movie" "tv") nil t)
                     (read-string "Query: ")))
  (moviedata-fetch
   query
   :type type
   :marker (set-marker (make-marker) (point))))

(cl-defun moviedata-fetch (query &key (type "movie") marker)
  "docstring"
  (unless (string-empty-p query)
    (let* ((query-string (url-build-query-string `(("query" ,query))))
           (full-query (concat "https://api.themoviedb.org/3/search/" type "?" query-string)))
      (plz 'get full-query
        :headers `(("Authorization" . ,(format "Bearer %s" (auth-source-pass-get "token" "api/api.themoviedb.org")))
                   ("accept" . "application/json"))
        :as (lambda () (json-parse-buffer :object-type 'plist))
        :else (lambda (resp) (message "%S" resp))
        :then (lambda (resp)
                (message "\"%s\": Response received, processing" query)
                (cl-loop for entry across (plist-get resp :results)
                         for num upto 3
                         for id = (plist-get entry :id)
                         do
                         (moviedata-add-availability id type entry marker (= num 0))))))))

(defun moviedata-add-availability (id type entry marker &optional primary)
  (let* ((full-query (concat "https://streaming-availability.p.rapidapi.com/shows/"
                             (and type (concat type "/"))
                             (cl-etypecase id
                               (number (number-to-string id))
                               (string id))
                             "?country=us")))
    (plz 'get full-query
      :headers `(("X-RapidAPI-Key" . ,(auth-source-pass-get 'secret "api/rapidapi.com")))
      :as (lambda () (json-parse-buffer :object-type 'plist))
      :else (lambda (err) (message "%S" err))
      :then (lambda (resp)
              ;; (with-current-buffer (get-buffer "*scratch*")
              ;;   (erase-buffer)
              ;;   (print resp (current-buffer))
              ;;   (pp-buffer))
              (message "%d: Availability data gathered" id)
              (plist-put entry :streamingOptions (map-nested-elt resp '(:streamingOptions :us)))
              (plist-put entry :poster_path
                         (or (map-nested-elt resp '(:imageSet :horizontalPoster :w480))
                             (map-nested-elt resp '(:imageSet :verticalPoster   :w480))))
              (moviedata-insert-as-org entry marker primary)))))

(defun moviedata-insert-as-org (entry marker &optional primary)
  (when (buffer-live-p (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (if (org-at-heading-p)
           (goto-char (org-entry-end-position))
         (kill-region
          (point) (save-excursion (org-end-of-subtree t t))))
       (when-let* ((title (or (plist-get entry :title)
                              (plist-get entry :name))))
         (save-excursion (insert "\n"))
         (unless primary
           (org-insert-heading nil t (1+ (org-current-level)))
           (insert title)
           (forward-line) (insert "\n\n"))
         (org-entry-put (point) "GENRE_IDS" (prin1-to-string (plist-get entry :genre_ids)))
         ;; Insert link to thumbnail
         (when-let* ((poster (plist-get entry :poster_path)))
           (if (and primary (buffer-file-name))
               (let* ((attach-dir (org-attach-dir 'get-create))
                      (attach-file (expand-file-name
                                    (concat (string-replace " " "-" (downcase title)) ".jpg")
                                    attach-dir)))
                 (url-copy-file poster attach-file 'overwrite)
                 (run-hook-with-args 'org-attach-after-change-hook attach-dir)
                 (org-attach-tag)
                 (insert "[[file:" (file-relative-name attach-file) "]]\n\n"))
             (insert "[[" poster "][Poster]]\n\n")))
         ;; Add overview
         (insert (plist-get entry :overview) "\n\n")
         ;; Add availability table
         (when (> (length (plist-get entry :streamingOptions)) 0)
           (cl-loop for service across (plist-get entry :streamingOptions)
                    for name = (map-nested-elt service '(:service :name))
                    for url =  (plist-get service :link)
                    for type = (plist-get service :type)
                    for quality = (plist-get service :quality)
                    for addon-type = (map-nested-elt service '(:addon :name))
                    for price = (map-nested-elt service '(:price :formatted))
                    initially do
                    (insert "| Link | Price | Requirement | Quality |\n|-\n")
                    do
                    (insert (format "| [[%s][%s]] | %s | %s | %s |\n"
                                    url name (or price "N/A")
                                    (if addon-type (concat "addon: " addon-type) type)
                                    (or (and quality (upcase quality)) "SD")))
                    finally do (forward-line -1) (org-table-align) (forward-line 1)))
         ;; Move insertion marker down
         (move-marker marker (point))
         (insert "\n")
         (run-hooks 'moviedata-post-insert-hook))))))

;; (moviedata-add-availability 155 "movie" nil nil)
;; (moviedata-add-availability "tt0068646" nil nil)

(provide 'moviedata)
;;; moviedata.el ends here
