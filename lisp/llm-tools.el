;;; llm-tools.el --- Some LLM tools for gptel        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur;; -*- lexical-binding: t; -*- <karthikchikmagalur@gmail.com>
;; Keywords:

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

;; Adds the following gptel tools.
;; System:
;; - "execute_bash"           : Execute a Bash command.
;;
;; Web:
;; - "search_web"             : Search the web for the first five results to a query.
;; - "read_url"               : Fetch and read the contents of a URL.
;; - "get_youtube_meta"       : Find the description and video transcript for a youtube video.
;;
;; Emacs:                       Currently WIP
;; - "append_to_buffer"       : Append text to an Emacs buffer.
;; - "open_file_or_dir"       : Open a file or directory in Emacs.
;; - "read_buffer"            : Return the contents of an Emacs buffer.
;; - "modify_buffer"          : Modify buffer contents using unified diff format.
;;
;; Filesystem:
;; - "make_directory"  : Create a new directory.
;; - "glob_files"      : Find files matching a glob pattern
;; - "grep_files"      : Grep for text in file(s).
;; - "read_file_lines" : Read a specific line range from a file.
;; - "insert_in_file"  : Insert text at a specific line number in a file.
;; - "edit_files"      : Replace text in file(s) using string match or unified diff.
;; - "write_file"      : Create a new file with content.

;;; Code:



(require 'gptel)
(require 'eww)
(require 'flymake)
(require 'url-http)

(defvar url-http-end-of-headers)

;;; System tools
(gptel-make-tool
 :name "execute_bash"
 :function (lambda (command)
             "Execute a bash command and return its output.
COMMAND is the bash command string to execute."
             (with-temp-buffer
               (let* ((exit-code (call-process "bash" nil (current-buffer) nil "-c" command))
                      (output (buffer-string)))
                 (if (zerop exit-code)
                     output
                   (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s" exit-code output)))))
 :description "Execute Bash commands to inspect files and system state.

This tool provides access to a Bash shell with GNU coreutils (or equivalents)
available. You can use any standard Linux commands including: cd, ls, file, cat,
grep, awk, sed, head, tail, wc, find, sort, uniq, cut, tr, and more.

PURPOSE:
- Efficiently inspect files and system state WITHOUT consuming excessive
tokens. This is preferred over reading entire large files.
- Run tests, check your work or otherwise close the loop to verify changes you make.
- Modify files or system state as appropriate, using cp, mv, rm, patch,
git subcommands (git log, commit, branch and more) and so on.

BEST PRACTICES:
- Use pipes to combine commands: 'cat file.log | grep ERROR | tail -20'
- For large files, use head/tail: 'head -50 file.txt' or 'tail -100 file.log'
- Use grep with context: 'grep -A 5 -B 5 pattern file.txt'
- Check file sizes first: 'wc -l file.txt' before reading
- Use file command to identify file types: 'file *'
- Combine with other tools: 'find . -name \"*.el\" | head -10'

EXAMPLES:
- List files with details: 'ls -lah /path/to/dir'
- Print lines 25-35 of a long file/stream: 'sed -n \"25,35p\" app.log'
- Find recent errors: 'grep -i error /var/log/app.log | tail -20'
- Check file type: 'file document.pdf'
- Count lines: 'wc -l *.txt'
- Search with context: 'grep -A 3 \"function foo\" script.sh'

The command will be executed in the current working directory. Output is
returned as a string. Long outputs should be filtered/limited using pipes."
 :args '((:name "command"
          :type string
          :description "The Bash command to execute. Can include pipes and standard shell operators. Example: 'ls -la | head -20' or 'grep -i error app.log | tail -50'"))
 :confirm t
 :include t
 :category "system")

;; "Execute Bash commands to inspect files and system state.

;; This tool provides access to a Bash shell with GNU coreutils (or equivalents)
;; available. You can use any standard Linux commands including: cd, ls, file, cat,
;; grep, awk, sed, head, tail, wc, find, sort, uniq, cut, tr, and more.

;; PURPOSE:
;; - Efficiently inspect files and system state WITHOUT consuming excessive
;; tokens. This is preferred over reading entire large files.
;; - Modify files or system state as appropriate, using cp, mv, rm, patch,
;; git subcommands (git log, commit, branch and more) and so on.

;; BEST PRACTICES:
;; - Use pipes to combine commands: 'cat file.log | grep ERROR | tail -20'
;; - For large files, use head/tail: 'head -50 file.txt' or 'tail -100 file.log'
;; - Use grep with context: 'grep -A 5 -B 5 pattern file.txt'
;; - Check file sizes first: 'wc -l file.txt' before reading
;; - Use file command to identify file types: 'file *'
;; - Combine with other tools: 'find . -name \"*.el\" | head -10'

;; EXAMPLES:
;; - List files with details: 'ls -lah /path/to/dir'
;; - Print lines 25-35 of a long file/stream: 'sed -n \"25,35p\" app.log'
;; - Find recent errors: 'grep -i error /var/log/app.log | tail -20'
;; - Check file type: 'file document.pdf'
;; - Count lines: 'wc -l *.txt'
;; - Search with context: 'grep -A 3 \"function foo\" script.sh'

;; The command will be executed in the current working directory. Output is
;; returned as a string. Long outputs should be filtered/limited using pipes."

;;; Web tools

;;;; Web search
(defvar gptel-agent-brave-key #'gptel-agent-brave-key-auth-source
  "API key for Brave web search.

Can be a string (the key) or a function that returns the string.")

(defun gptel-agent-brave-key-auth-source ()
  "Lookup API key for Brave web search from auth source.

Expects the host and user to be \"api.search.brave.com\" and \"search\"
respectively."
  (let ((message-log-max nil)
        (inhibit-message t)
        (secret (plist-get (car (auth-source-search
                                 :host "api.search.brave.com"
                                 :user "search"
                                 :require '(:secret)))
                           :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun gptel-agent--fetch-with-timeout (url url-cb main-cb fail-msg &rest args)
  "Fetch URL and call URL-CB in the result buffer.

Call TOOL-CB if there is an error or a timeout.  TOOL-CB and ARGS are
passed to URL-CB.  FAILED-MSG is a fragment used for messaging."
  (let* ((timeout 30) timer done
         (proc-buffer
          (url-retrieve
           url (lambda (status)
                 (setq done t)
                 (when timer (cancel-timer timer))
                 (if-let* ((err (plist-get status :error)))
                     (funcall main-cb
                              (format "Error: %s failed with error: %S" fail-msg err))
                   (apply url-cb main-cb args))
                 (kill-buffer (current-buffer)))
           args 'silent)))
    (setq timer
          (run-at-time
           timeout nil
           (lambda (buf cb)
             (unless done
               (setq done t)
               (let ((kill-buffer-query-functions)) (kill-buffer buf))
               (funcall
                cb (format "Error: %s timed out after %d seconds."
                           fail-msg timeout))))
           proc-buffer main-cb))
    proc-buffer))

(defun gptel-agent--web-search-brave (tool-cb query &optional count)
  "Return a JSON array of COUNT web search results for QUERY.

Callback CB is called with the result."
  (let* ((brave-url "https://api.search.brave.com/res/v1/web/search")
         (brave-url-string
          (lambda (q) (concat brave-url "?"
                         (url-build-query-string
                          `(("q" ,(url-hexify-string q))
                            ("count" ,(format "%s" (or count 5)))
                            ("page" ,(format "%s" 0)))))))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("User-Agent" . "Emacs:gptel-agent/0.1")
            ("Accept" . "application/json")
            ("Accept-Encoding" . "gzip")
            ("X-Subscription-Token" . ,(if (functionp gptel-agent-brave-key)
                                           (funcall gptel-agent-brave-key)
                                         gptel-agent-brave-key)))))
    (gptel-agent--fetch-with-timeout
     (funcall brave-url-string query)
     (lambda (cb)
       (goto-char url-http-end-of-headers)
       (condition-case nil
           (let ((attrs (json-parse-buffer :object-type 'plist)))
             (if-let* ((err (plist-get attrs :error)))
                 (funcall tool-cb (list :error err :type (plist-get attrs :type)))
               (let* ((raw-results (map-nested-elt attrs '(:web :results)))
                      (result
                       (mapconcat
                        (lambda (item)
                          (concat (map-elt item :url) "\n\n"
                                  (map-elt item :title) "\n"
                                  (map-elt item :description) "\n"))
                        raw-results "\n----\n")))
                 (funcall cb result))))
         (error (funcall cb (list :type "parse error"
                                  :error "Could not parse API response")))))
     tool-cb (format  "Web search for \"%s\"" query))))

(gptel-make-tool
 :name "search_web_brave"
 :function 'gptel-agent--web-search-brave
 :description "Search the web for the first five results to a query.  The query can be an arbitrary string.  Returns the top five results from the search engine as a plist of objects.  Each object has the keys `:url`, `:title` and `:description` for the corresponding search result.

If required, consider using the url as the input to the `read_url` tool to get the contents of the url.  Note that this might not work as the `read_url` tool does not handle javascript-enabled pages."
 :args `((:name "query" :type string :description "The natural language search query, can be multiple words."))
 :async t
 :category "web")
;; Search the web with brave, top 5 results with links:1 ends here

;;;; Read URLs
(list
 :function (lambda (url)
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min)) (forward-paragraph)
               (let ((dom (libxml-parse-html-region (point) (point-max))))
                 (run-at-time 0 nil #'kill-buffer (current-buffer))
                 (with-temp-buffer
                   (eww-score-readability dom)
                   (shr-insert-document (eww-highest-readability dom))
                   (buffer-substring-no-properties (point-min) (point-max))))))
 :name "read_url_sync"
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
                     :type "string"
                     :description "The URL to read"))
 :category "web")

(gptel-make-tool
 :function (lambda (cb url)
             (url-retrieve
              url
              (lambda (status)
                (if (plist-get status :error)
                    (format "Error: Request failed with data:\n%S"
                            (plist-get status :error))
                  (goto-char (point-min)) (forward-paragraph)
                  (condition-case errdata
                      (let ((dom (libxml-parse-html-region (point) (point-max)))
                            (url-buffer (current-buffer)))
                        (run-at-time 0 nil #'kill-buffer url-buffer)
                        (with-temp-buffer
                          (eww-score-readability dom)
                          (shr-insert-document (eww-highest-readability dom))
                          (funcall
                           cb (buffer-substring-no-properties
                               (point-min) (point-max)))))
                    (error (format "Error: Request failed with error data:\n%S"
                                   errdata)))))))
 :name "read_url"
 :description "Fetch and read the contents of a URL"
 :args '(( :name "url"
           :type "string"
           :description "The URL to read"))
 :async t
 :include t
 :confirm t
 :category "web")
;; Read a non-js webpage with eww:1 ends here

;;;; Fetch youtube transcript
(gptel-make-tool
 :name "get_youtube_meta"
 :function #'my/gptel-youtube-metadata
 :description "Find the description and video transcript for a youtube video.  Returns a markdown formatted string containing two sections:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
 :args '((:name "url"
                :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
                :type "string"))
 :category "web"
 :async t
 :include t)

(defun my/yt--parse-caption-xml (xml-string)
  "Parse YouTube caption XML and return DOM structure."
  (with-temp-buffer
    (insert xml-string)
    (goto-char (point-min))
    ;; Clean up the XML
    (dolist (reps '(("\n" . " ")
                    ("&amp;" . "&")
                    ("&quot;" . "\"")
                    ("&#39;" . "'")
                    ("&lt;" . "<")
                    ("&gt;" . ">")))
      (save-excursion
        (while (search-forward (car reps) nil t)
          (replace-match (cdr reps) nil t))))
    (libxml-parse-xml-region (point-min) (point-max))))

(defun my/yt--format-timestamp (seconds)
  "Format SECONDS as MM:SS timestamp."
  (format "%d:%02d" (floor seconds 60) (mod seconds 60)))

(defun my/yt--format-captions-as-paragraphs (caption-dom &optional chunk-time)
  "Format caption DOM as paragraphs with timestamps.
CHUNK-TIME is the number of seconds per paragraph (default 30)."
  (when (and (listp caption-dom)
             (eq (car-safe caption-dom) 'transcript))
    (let ((chunk-time (or chunk-time 30))
          (result "")
          (current-para "")
          (para-start-time 0))
      (dolist (elem (cddr caption-dom)) ;; Process each text element
        (when (and (listp elem) (eq (car elem) 'text))
          (let* ((attrs (cadr elem))
                 (text (caddr elem))
                 (start (string-to-number (cdr (assoc 'start attrs))))
                 ;; Check if we've crossed into a new chunk-time boundary
                 (should-chunk (and (> (abs (- start para-start-time)) 3)
                                    (not (= (floor para-start-time chunk-time)
                                            (floor start chunk-time))))))
            (when (and should-chunk (> (length current-para) 0))
              ;; Add completed paragraph
              (setq result (concat result
                                   (format "[%s]\n%s\n\n"
                                           (my/yt--format-timestamp para-start-time)
                                           (string-trim current-para))))
              (setq current-para "")
              (setq para-start-time start))

            (when text
              (setq current-para (concat current-para " " text))))))

      ;; Add final paragraph
      (when (> (length current-para) 0)
        (setq result (concat result
                             (format "[%s]\n%s\n\n"
                                     (my/yt--format-timestamp para-start-time)
                                     (string-trim current-para)))))
      result)))

(defun my/yt--fetch-watch-page (callback video-id)
  "Step 1: Fetch YouTube watch page and extract INNERTUBE_API_KEY.
Calls CALLBACK with error or proceeds to fetch InnerTube data."
  (url-retrieve
   (format "https://youtube.com/watch?v=%s" video-id)
   (lambda (status callback video-id)
     (if-let ((error (plist-get status :error)))
         (funcall callback (format "Error fetching page: %s" error))
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (let* ((html (buffer-substring (point) (point-max)))
              (api-key (and (string-match
                             "\"INNERTUBE_API_KEY\":\"\\([a-zA-Z0-9_-]+\\)"
                             html)
                            (match-string 1 html))))
         (if api-key
             (my/yt--fetch-innertube callback video-id api-key)
           (funcall callback "Error: Could not extract API key")))))
   (list callback video-id)))

(defun my/yt--fetch-innertube (callback video-id api-key)
  "Step 2: Fetch video metadata from YouTube InnerTube API.
Calls CALLBACK with error or proceeds to fetch captions."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")
           ("Accept-Language" . "en-US")))
        (url-request-data
         (encode-coding-string
          (json-encode
           `((context . ((client . ((clientName . "ANDROID")
                                    (clientVersion . "20.10.38")))))
             (videoId . ,video-id)))
          'utf-8)))
    (url-retrieve
     (format "https://www.youtube.com/youtubei/v1/player?key=%s" api-key)
     (lambda (status callback)
       (if-let ((error (plist-get status :error)))
           (funcall callback (format "Error fetching metadata: %s" error))
         (goto-char (point-min))
         (search-forward "\n\n" nil t)
         (let* ((json-data (ignore-errors
                             (json-parse-buffer :object-type 'plist)))
                (video-details (plist-get json-data :videoDetails))
                (description (plist-get video-details :shortDescription))
                (caption-tracks (map-nested-elt
                                 json-data
                                 '(:captions
                                   :playerCaptionsTracklistRenderer
                                   :captionTracks))))
           (my/yt--fetch-captions callback description caption-tracks))))
     (list callback))))

(defun my/yt--fetch-captions (callback description caption-tracks)
  "Step 3: Find and fetch English captions.
Calls CALLBACK with formatted result containing description and transcript."
  (if (not caption-tracks)
      (funcall callback
               (format "# Description\n\n%s\n\n# Transcript\n\nNo transcript available."
                       (or description "No description available.")))
    (let ((en-caption
           (cl-find-if
            (lambda (track)
              (string-match-p "^en" (or (plist-get track :languageCode) "")))
            caption-tracks)))
      (if (not en-caption)
          (funcall callback
                   (format "# Description\n\n%s\n\n# Transcript\n\nNo English transcript available."
                           (or description "No description available.")))
        (let ((base-url (replace-regexp-in-string
                         "&fmt=srv3" ""
                         (plist-get en-caption :baseUrl))))
          (url-retrieve
           base-url
           (lambda (status callback description)
             (if-let ((error (plist-get status :error)))
                 (funcall callback
                          (format "# Description\n\n%s\n\n# Transcript\n\nError fetching transcript: %s"
                                  (or description "No description available.")
                                  error))
               (goto-char (point-min))
               (search-forward "\n\n" nil t)
               (let* ((xml-string (buffer-substring (point) (point-max)))
                      (caption-dom (my/yt--parse-caption-xml xml-string))
                      (formatted-transcript
                       (my/yt--format-captions-as-paragraphs caption-dom 30)))
                 (funcall callback
                          (format "# Description\n\n%s\n\n# Transcript\n\n%s"
                                  (or description "No description available.")
                                  (or formatted-transcript "Error parsing transcript."))))))
           (list callback description)))))))

(defun my/gptel-youtube-metadata (callback url)
  "Fetch YouTube metadata and transcript for URL, calling CALLBACK with result.
CALLBACK is called with a markdown-formatted string containing the video
description and transcript formatted as timestamped paragraphs."
  (if-let* ((video-id
             (and (string-match
                   (rx bol (opt "http" (opt "s") "://")
                       (opt "www.") "youtu" (or ".be" "be.com") "/"
                       (opt "watch?v=")
                       (group (one-or-more (not (any "?&")))))
                   url)
                  (match-string 1 url))))
      (my/yt--fetch-watch-page callback video-id)
    (funcall callback "Error: Invalid YouTube URL")))

;;; Emacs tools
(gptel-make-tool
 :function (lambda (buffer text)
             (with-current-buffer (get-buffer-create buffer)
               (save-excursion
                 (goto-char (point-max))
                 (insert text)))
             (format "Appended text to buffer %s" buffer))
 :name "append_to_buffer"
 :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
 :args (list '(:name "buffer"
                     :type "string"
                     :description "The name of the buffer to append text to.")
             '(:name "text"
                     :type "string"
                     :description "The text to append to the buffer."))
 :category "emacs")

(gptel-make-tool
 :function (lambda (filename)
             (find-file-other-window filename)
             (format "Opened file or directory %s" filename))
 :name "open_file_or_dir"
 :description "Open a file or directory in this Emacs session."
 :args (list '(:name "file"
                     :type "string"
                     :description "The file or directory to open in the Emacs session."))
 :category "emacs")

;; buffer retrieval tool
(gptel-make-tool
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Error: buffer %s is not live" buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :name "read_buffer"
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
                     :type "string"
                     :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")

(gptel-make-tool
 :function (lambda (buffer start-line end-line)
             "Extract a line range from a buffer"
             (unless (get-buffer buffer)
               (error "Error: buffer %s is not live" buffer))
             (with-current-buffer buffer
               (let ((start (line-beginning-position start-line))
                     (end (line-end-position end-line)))
                 (buffer-substring-no-properties start end))))
 :name "read_buffer_lines"
 :description "extract a line range from a buffer"
 :args (list '(:name "buffer" :type "string" :description "the name of the buffer")
             '(:name "start_line" :type "integer" :description "the line to start reading from")
             '(:name "end_line" :type "integer" :description "the line up to which to read"))
 :confirm (lambda (_ start end) (> (- end start) 100))
 :category "emacs")

(defun gptel--tool-flymake-diagnostics ()
  "Collect all flymake errors across all open buffers in the current project.

Errors with low severity are not collected."
  (let ((project (project-current)))
    (unless project
      (error "Not in a project.  Cannot collect flymake diagnostics"))
    (let ((results '()))
      (dolist (diag (flymake--project-diagnostics project))
        (let ((severity (flymake--diag-type diag)))
          (when (memq severity '(:error :warning))
            (with-current-buffer (flymake-diagnostic-buffer diag)
              (let* ((beg (flymake--diag-beg diag))
                     (line-num (line-number-at-pos beg))
                     (line-text (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))
                (push (format "File: %s:%d\nSeverity: %s\nMessage: %s\n---\n%s"
                              (buffer-file-name)
                              line-num
                              severity
                              (flymake--diag-text diag)
                              line-text)
                      results))))))
      (string-join (nreverse results) "\n\n"))))

(gptel-make-tool
 :name "code_diagnostics"
 :description "Collect all code diagnostics with severity high/medium across all open buffers in the current project."
 :function #'gptel--tool-flymake-diagnostics
 :args nil
 :category "code"
 :include t
 :confirm t)

(defun modify-buffer-apply-diff (buffer diff)
  "Apply unified format DIFF to BUFFER."
  (message "Applying diff %s to buffer %s" diff buffer)
  (if-let ((buf (get-buffer buffer)))
      (with-current-buffer buf
        ;; Find the first @@ and ignore everything before it
        (if-let ((first-hunk-pos (string-match "^@@ .*@@\n" diff)))
            (dolist (hunk (split-string (substring diff first-hunk-pos) "^@@ .*@@\n" t))
              (let (before after)
                (dolist (line (split-string hunk "\n" t))
                  (cond
                   ((string-prefix-p " " line)
                    (push (substring line 1) before)
                    (push (substring line 1) after))
                   ((string-prefix-p "-" line)
                    (push (substring line 1) before))
                   ((string-prefix-p "+" line)
                    (push (substring line 1) after))))
                (setq before (string-join (nreverse before) "\n")
                      after (string-join (nreverse after) "\n"))
                (goto-char (point-min))
                (if (search-forward before nil t)
                    (replace-match after)
                  (message "Hunk not found in buffer %s" buffer))))
          (message "No hunks found in diff"))
        (format "Applied changes to buffer %s" buffer))
    (error "Buffer %s not found" buffer)))

(gptel-make-tool
 :name "modify_buffer"
 :description "Modify buffer contents using unified diff format"
 :args (list '(:name "buffer"
                     :type "string"
                     :description "The name of the buffer to modify")
             '(:name "diff"
                     :type "string"
                     :description "The changes to apply in unified diff format (with @@ hunks and +/- lines)"))
 :category "emacs"
 :function #'modify-buffer-apply-diff)

;;; Filesystem tools
;;;; Make directories
(gptel-make-tool
 :name "make_directory"
 :description "Create a new directory with the given name in the specified parent directory"
 :function (lambda (parent name)
             (condition-case nil
                 (progn
                   (make-directory (expand-file-name name parent) t)
                   (format "Directory %s created/verified in %s" name parent))
               (error (format "Error creating directory %s in %s" name parent))))
 :args (list '( :name "parent"
                :type "string"
                :description "The parent directory where the new directory should be created, e.g. /tmp")
             '( :name "name"
                :type "string"
                :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem"
 :confirm t)

;;;; Writing to files
(defun gptel--tool-fix-patch-headers ()
  "Fix line numbers in hunks in diff at point."
  ;; Find and process each hunk header
  (while (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) +\\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
    (let ((hunk-start (line-beginning-position))
          (orig-line (string-to-number (match-string 1)))
          (new-line (string-to-number (match-string 3)))
          (orig-count 0)
          (new-count 0))

      ;; Count lines in this hunk until we hit the next @@ or EOF
      (goto-char hunk-start)
      (forward-line 1)
      (save-match-data
        (while (and (not (eobp))
                    (not (looking-at-p "^@@")))
          (cond
           ;; Removed lines (not ---)
           ((looking-at-p "^-[^-]")
            (cl-incf orig-count))
           ;; Added lines (not +++)
           ((looking-at-p "^\\+[^+]")
            (cl-incf new-count))
           ;; Context lines (space at start)
           ((looking-at-p "^ ")
            (cl-incf orig-count)
            (cl-incf new-count)))
          (forward-line 1)))

      ;; Replace the hunk header with corrected counts
      (goto-char hunk-start)
      (delete-line)
      (insert (format "@@ -%d,%d +%d,%d @@\n"
                      orig-line orig-count new-line new-count)))))

(defun gptel--tool-edit-files (path &optional old-str new-str-or-diff diffp)
  "Replace text in file(s) at PATH using either string matching or unified diff.

This function supports two distinct modes of operation:

1. STRING REPLACEMENT MODE (DIFFP is nil or :json-false):
   - Searches for OLD-STR in the file at PATH
   - Replaces it with NEW-STR-OR-DIFF
   - Requires OLD-STR to match exactly once (uniquely) in the file
   - Only works on single files, not directories

2. DIFF/PATCH MODE (when DIFFP is non-nil and not :json-false):
   - Applies NEW-STR-OR-DIFF as a unified diff using the `patch` command
   - Works on both single files and directories
   - OLD-STR is ignored in this mode
   - NEW-STR-OR-DIFF can contain the diff in fenced code blocks
     (=diff or =patch)
   - Uses the -N (--forward) option to ignore already-applied patches

PATH - File or directory path to modify (must be readable)
OLD-STR - (String mode only) Exact text to find and replace
NEW-STR-OR-DIFF - Replacement text (string mode) or unified diff (diff mode)
DIFFP - If non-nil (and not :json-false), use diff/patch mode

Error Conditions:
  - PATH not readable
  - (String mode) PATH is a directory
  - (String mode) OLD-STR not found in file
  - (String mode) OLD-STR matches multiple times (ambiguous)
  - (Diff mode) patch command fails (exit status non-zero)

Returns:
  Success message string describing what was changed

Signals:
  error - On any failure condition (caught and displayed by gptel)"
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))

  (if (or (eq diffp :json-false) old-str)
      ;; Replacement by Text
      (progn
        (when (file-directory-p path)
          (error "Error: String replacement is intended for single files, not directories (%s)"
                 path))
        (with-temp-buffer
          (insert-file-contents path)
          (if (search-forward old-str nil t)
              (if (save-excursion (search-forward old-str nil t))
                  (error "Error: Match is not unique.
Consider providing more context for the replacement, or a unified diff")
                ;; TODO: More robust backspace escaping
                (replace-match (string-replace  "\\" "\\\\" new-str-or-diff))
                (write-region nil nil path)
                (format "Successfully replaced %s (truncated) with %s (truncated)"
                        (truncate-string-to-width old-str 20 nil nil t)
                        (truncate-string-to-width new-str-or-diff 20 nil nil t)))
            (error "Error: Could not find old_str \"%s\" in file %s"
                   (truncate-string-to-width old-str 20) path))))
    ;; Replacement by Diff
    (unless (executable-find "patch")
      (error "Error: Command \"patch\" not available, cannot apply diffs.
Use string replacement instead"))
    (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
           (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
           (target-file (expand-file-name path))
           (exit-status -1)             ; Initialize to a known non-zero value
           (result-output "")
           (result-error ""))
      (unwind-protect
          (let ((default-directory (file-name-directory path))
                (patch-options    '("--forward" "--verbose")))

            (with-temp-message
                (format "Applying diff to: `%s` with options: %s"
                        target-file patch-options)
              (with-temp-buffer
                (insert new-str-or-diff)
                ;; Insert trailing newline, required by patch
                (unless (eq (char-before (point-max)) ?\n)
                  (goto-char (point-max))
                  (insert "\n"))
                (goto-char (point-min))
                ;; Remove code fences, if present
                (when (looking-at-p "^ *```diff\n")
                  (save-excursion
                    (delete-line)
                    (goto-char (point-max))
                    (forward-line -1)   ;guaranteed to be at a blank newline
                    (when (looking-at-p "^ *```") (delete-line))))
                ;; Fix line numbers in hunk headers
                (gptel--tool-fix-patch-headers)

                ;; Pass buffer *names* to call-process-region
                (setq exit-status
                      (apply #'call-process-region (point-min) (point-max)
                             "patch" nil
                             (list out-buf-name err-buf-name) ; stdout/stderr buffer names
                             nil patch-options))))

            ;; Retrieve content from buffers using their names
            (let ((stdout-buf (get-buffer out-buf-name))
                  (stderr-buf (get-buffer err-buf-name)))
              (when stdout-buf
                (with-current-buffer stdout-buf
                  (setq result-output (buffer-string))))
              (when stderr-buf
                (with-current-buffer stderr-buf
                  (setq result-error (buffer-string)))))

            (if (= exit-status 0)
                (format "Diff successfully applied to %s.
Patch command options: %s
Patch STDOUT:\n%s
Patch STDERR:\n%s"
                        target-file patch-options result-output result-error)
              ;; Signal an Elisp error, which gptel will catch and display.
              ;; The arguments to 'error' become the error message.
              (error "Error: Failed to apply diff to %s (exit status %s).
Patch command options: %s
Patch STDOUT:\n%s
Patch STDERR:\n%s"
                     target-file exit-status patch-options
                     result-output result-error)))
        (let ((stdout-buf-obj (get-buffer out-buf-name)) ;Clean up
              (stderr-buf-obj (get-buffer err-buf-name)))
          (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))
          (when (buffer-live-p stderr-buf-obj) (kill-buffer stderr-buf-obj)))))))

(gptel-make-tool
 :name "edit_files"
 :description
 "Replace text in one or more files.

To edit a single file, provide the file `path`.

For the replacement, there are two methods:
- Short replacements: Provide both `old_str` and `new_str`, in which case `old_str` needs to exactly match one unique section of the original file, including any whitespace.  Make sure to include enough context that the match is not ambiguous.  The entire original string will be replaced with `new str`.
- Long or involved replacements: set the `diff` parameter to true and provide a unified diff in `new_str`. `old_str` can be ignored.

To edit multiple files,
- provide the directory path,
- set the `diff` parameter to true
- and provide a unified diff in `new_str`.

Diff instructions:

- The diff must be provided within fenced code blocks (=diff or =patch) and be in unified format.
- The LLM should generate the diff such that the file paths within the diff (e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'path'.

To simply insert text at some line, use the \"insert_in_file\" instead."
 :function #'gptel--tool-edit-files
 :args '(( :name "path"
           :description "File path or directory to edit"
           :type string)
         ( :name "old_str"
           :description "Original string to replace.  If providing a unified diff, this should be false"
           :type string
           :optional t)
         ( :name "new_str"
           :description "Replacement string OR unified diff text"
           :type string)
         ( :name "diff"
           :description "Whether the replacement is a string or a diff.  `true` for a diff, `false` otherwise."
           :type boolean))
 :category "filesystem"
 :confirm t
 :include t)

(defun gptel--tool-insert-in-file (path line-number new-str)
  "Insert NEW-STR at LINE-NUMBER in file at PATH.

LINE-NUMBER conventions:
- 0 inserts at the beginning of the file
- -1 inserts at the end of the file
- N > 1 inserts before line N"
  (unless (file-readable-p path)
    (error "Error: File %s is not readable" path))

  (when (file-directory-p path)
    (error "Error: Cannot insert into directory %s" path))

  (with-temp-buffer
    (insert-file-contents path)

    (pcase line-number
      (0 (goto-char (point-min)))       ; Insert at the beginning
      (-1 (goto-char (point-max)))      ; Insert at the end
      (_ (goto-char (point-min))
         (forward-line line-number)))   ; Insert before line N

    ;; Insert the new string
    (insert new-str)

    ;; Ensure there's a newline after the inserted text if not already present
    (unless (or (string-suffix-p "\n" new-str) (eobp))
      (insert "\n"))

    ;; Write the modified content back to the file
    (write-region nil nil path)

    (format "Successfully inserted text at line %d in %s" line-number path)))

(gptel-make-tool
 :name "insert_in_file"
 :description "Insert `new_str` after `line_number` in file at `path`.

Use this tool for purely additive actions: adding text to a file at a
specific location with no changes to the surrounding context."
 :function #'gptel--tool-insert-in-file
 :args '(( :name "path"
           :description "Path of file to edit."
           :type string)
         ( :name "line_number"
           :description "The line number at which to insert `new_str`, with
- 0 to insert at the beginning, and
- -1 to insert at the end."
           :type integer)
         ( :name "new_str"
           :description "String to insert at `line_number`.")))

(gptel-make-tool
 :name "write_file"
 :description "Create a new file with the specified content.
Overwrites an existing file, so use with care!
Consider using the more granular tools \"insert_in_file\" or \"edit_files\" first."
 :function (lambda (path filename content)
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :args (list '( :name "path"
                :type "string"
                :description "The directory where to create the file, \".\" is the current directory.")
             '( :name "filename"
                :type "string"
                :description "The name of the file to create.")
             '( :name "content"
                :type "string"
                :description "The content to write to the file"))
 :category "filesystem"
 :confirm t)

(gptel-make-tool
 :name "glob_files"
 :description "Recursively find files matching a provided glob pattern.

- Supports glob patterns like \"*.md\" or \"*test*.py\".
  The glob applies to the basename of the file (with extension).
- Returns matching file paths at all depths sorted by modification time.
  Limit the depth of the search by providing the `depth` argument.
- When you are doing an open ended search that may require multiple rounds
  of globbing and grepping, use the \"task\" tool instead
- You can call multiple tools in a single response.  It is always better to
  speculatively perform multiple searches in parallel if they are potentially useful."
 :function (lambda (pattern &optional path depth)
             (if path
                 (unless (and (file-readable-p path) (file-directory-p path))
                   (error "Error: path %s is not readable" path))
               (setq path "."))
             (with-temp-buffer
               (let* ((args (list "-l" "-f" "-i" "-I" ".git"
                                  "--sort=mtime" "--ignore-case"
                                  "--prune" "-P" pattern))
                      (args (if (natnump depth)
                                (nconc args '("-L" (number-to-string depth)))
                              args))
                      (exit-code (apply #'call-process "tree" nil t nil args)))
                 (when (/= exit-code 0)
                   (goto-char (point-min))
                   (insert (format "glob_files failed with exit code %d\n.STDOUT:\n\n"
                                   exit-code))))
               (buffer-string)))
 :args '(( :name "pattern"
           :type string
           :description "Glob pattern to match, for example \"*.el\".")
         ( :name "path"
           :type string
           :description "Directory to search in.  Supports relative paths and defaults to \".\""
           :optional t)
         ( :name "depth"
           :description "Limit directory depth of search, 1 or higher. Defaults to no limit."
           :type integer
           :optional t))
 :category "filesystem")

;;;; Read files or directories
(defun gptel--tool-read-file-lines (filename start-line end-line)
  "Return lines START-LINE to END-LINE fom FILENAME."
  (unless (file-readable-p filename)
    (error "Error: File %s is not readable" filename))

  (when (file-directory-p filename)
    (error "Error: Cannot insert into directory %s" filename))

  (if (and (not start-line) (not end-line)) ;read full file
      (if (> (file-attribute-size (file-attributes filename))
             (* 512 1024))
          (error "Error: File is too large (> 512 KB).
Please specify a line range to read.")
        (with-temp-buffer
          (insert-file-contents filename)
          (buffer-string)))
    ;; TODO: Handle nil start-line OR nil end-line
    (cl-decf start-line)
    (let* ((file-size (nth 7 (file-attributes filename)))
           (chunk-size (min file-size (* 512 1024)))
           (byte-offset 0) (line-offset (- end-line start-line)))
      (with-temp-buffer
        ;; Go to start-line
        (while (and (> start-line 0)
                    (< byte-offset file-size))
          (insert-file-contents-literally
           filename nil byte-offset (+ byte-offset chunk-size))
          (setq byte-offset (+ byte-offset chunk-size))
          (setq start-line (forward-line start-line))
          (when (eobp)
            (if (/= (line-beginning-position) (line-end-position))
                ;; forward-line counted 1 extra line
                (cl-incf start-line))
            (delete-region (point-min) (line-beginning-position))))

        (delete-region (point-min) (point))

        ;; Go to end-line, forward by line-offset
        (cl-block nil
          (while (> line-offset 0)
            (setq line-offset (forward-line line-offset))
            (when (and (eobp) (/= (line-beginning-position) (line-end-position)))
              ;; forward-line counted 1 extra line
              (cl-incf line-offset))
            (if (= line-offset 0)
                (delete-region (point) (point-max))
              (if (>= byte-offset file-size)
                  (cl-return)
                (insert-file-contents-literally
                 filename nil byte-offset (+ byte-offset chunk-size))
                (setq byte-offset (+ byte-offset chunk-size))))))

        (buffer-string)))))

(gptel-make-tool
 :name "read_file_lines"
 :description "Read file contents between specified line numbers `start_line` and `end_line`,
with both ends included.

Consider using the \"grep_files\" tool to find the right range to read first.

Reads the whole file if the line range is not provided.

Files over 512 KB in size can only be read by specifying a line range."
 :function #'gptel--tool-read-file-lines
 :args '(( :name "file_path"
           :type string
           :description "The path to the file to be read."
           :type string)
         ( :name "start_line"
           :type integer
           :description "The line to start reading from, defaults to the start of the file"
           :optional t)
         ( :name "end_line"
           :type integer
           :description "The line up to which to read, defaults to the end of the file."
           :optional t))
 :category "filesystem"
 :confirm (lambda (_ start end) (or (not start) (not end) (> (- end start) 100)))
 :include t)

(defun gptel--tool-grep (regex path &optional glob context-lines)
  "Search for REGEX in file or directory at PATH using ripgrep.

REGEX is a PCRE-format regular expression to search for.
PATH can be a file or directory to search in.

Optional arguments:
GLOB restricts the search to files matching the glob pattern.
  Examples: \"*.el\", \"*.md\", \"*.rs\"
CONTEXT-LINES specifies the number of lines of context to show
  around each match (0-15 inclusive, defaults to 0).

Returns a string containing matches grouped by file, with line numbers
and optional context. Results are sorted by modification time."
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))
  (with-temp-buffer
    (let* ((args
            (delq nil (list "--sort=modified"
                            (and (natnump context-lines)
                                 (format "--context=%d" context-lines))
                            (and glob (format "--glob=%s" glob))
                            ;; "--files-with-matches"
                            ;; "--max-count=10"
                            "--heading"
                            "--line-number"
                            "-e" regex
                            (expand-file-name (substitute-in-file-name path)))))
           (exit-code (apply #'call-process "rg" nil '(t t) nil args)))
      (when (/= exit-code 0)
        (goto-char (point-min))
        (insert (format "Error: search failed with exit-code %d.  Tool output:\n\n" exit-code)))
      (buffer-string))))

(gptel-make-tool
 :name "grep_files"
 :description "Search for text in file(s) at `path`.

Use this tool to find relevant parts of files to read.

Returns a list of matches prefixed by the line number, and grouped by file.  Can search an individual file (if providing a file path) or a directory.  Consider using this tool to find the right line range for the \"read_file_lines\" tool.

When searching directories, optionally restrict the types of files in the search with a `glob`.  Can request context lines around each match using the `context_lines` parameters."
 :function #'gptel--tool-grep
 :args '(( :name "regex"
           :description "Regular expression to search for in file contents."
           :type string)
         ( :name "path"
           :description "File or directory to search in."
           :type string)
         ( :name "glob"
           :description "Optional glob to restrict file types to search for.
Only required when path is a directory.
Examples: *.md, *.rs"
           :type string
           :optional t)
         ( :name "context_lines"
           :description "Number of lines of context to retrieve around each match (0-15 inclusive).
Optional, defaults to 0."
           :optional t
           :type integer
           :maximum 15))
 :category "filesystem")

;;; Task tool (sub-agent)

(defvar gptel--tool-agent-registry
  '(("general-purpose"
     :system "You are a general-purpose research and execution agent. Your role is to autonomously complete complex, multi-step tasks that require exploration and investigation.

Core responsibilities:
- Search through codebases systematically to find relevant information
- Read and analyze files to understand implementations
- Use tools efficiently to gather comprehensive information
- Execute multi-step workflows without user intervention
- Return complete, well-organized findings in a single response

Tool usage guidelines:
- Use glob_files to find files by name patterns
- Use grep_files to search file contents
- Use read_file_lines to examine files in detail
- Use search_web and read_url for documentation or external resources
- Call tools in parallel when operations are independent

Output requirements:
- Provide file paths with line numbers (e.g., src/main.rs:142)
- Include relevant code snippets to support findings
- Organize information logically (by feature, by file, by pattern, etc.)
- Be thorough but concise - focus on actionable information
- If uncertain, explore multiple approaches before concluding

Remember: You run autonomously and cannot ask follow-up questions. Make reasonable assumptions and be comprehensive in your investigation."
     :tools ("filesystem" "web")
     :backend "Gemini"
     :model gemini-flash-latest)
    ("researcher"
     :system "You are a read-only research agent specialized in analyzing code and understanding implementations without making any modifications.

Core responsibilities:
- Analyze code structure and architecture
- Trace execution flows and dependencies
- Document patterns and approaches used
- Identify potential issues or improvements
- Provide detailed technical analysis

Tool usage guidelines:
- Use glob_files to discover relevant files
- Use grep_files to search for patterns and usage
- Use the context flag in grep_files to see surrounding code
- Use read_file_lines to examine implementations in detail
- NEVER write to or edit files (you are read-only)
- Search systematically - check multiple locations

Analysis approach:
- Start broad, then narrow down to specifics
- Look at multiple examples to identify patterns
- Consider edge cases and error handling
- Note relationships between components
- Trace data flow and control flow

Output requirements:
- Provide file paths with line numbers
- Include relevant code excerpts
- Explain how things work, not just what exists
- Compare different approaches found
- Highlight notable design decisions or patterns
- Organize findings logically

Remember: You are read-only and autonomous. Explore thoroughly and return comprehensive analysis in one response."
     :tools ("read_file_lines" "glob_files" "grep_files" "web")
     :backend "Gemini"
     :model gemini-flash-latest)))

;; - Use Bash for system operations when needed

(gptel-make-tool
 :name "task_agent"
 :description "Launch a specialized agent to handle complex, multi-step tasks autonomously.  \
Agents run independently and return results in one message.  \
Use for open-ended searches, complex research, or when uncertain about finding results in first few tries."
 :function #'gptel--tool-make-task
 :args '(( :name "subagent_type"
           :type string
           :enum ["general-purpose" "researcher"]
           :description "The type of specialized agent to use for this task")
         ( :name "description"
           :type string
           :description "A short (3-5 word) description of the task")
         ( :name "prompt"
           :type "string"
           :description "The detailed task for the agent to perform autonomously.  \
Should include exactly what information the agent should return."))
 :category "agent"
 :async t
 :confirm nil
 :include t)

(defun gptel--tool-make-task (main-cb agent-type description prompt)
  ";TODO: "
  (gptel-with-preset
      (nconc (list :include-reasoning nil
                   :use-tools t
                   :use-context nil)
             (cdr (assoc agent-type gptel--tool-agent-registry)))
    (gptel-request prompt
      :callback
      (lambda (resp info)
        (pcase resp
          ('nil
           (funcall main-cb
                    (format "Error: Task %s could not finish task %s.

Error details: %S"
                            agent-type description (plist-get info :error))))
          (`(tool-call . ,calls)
           (pcase-dolist (`(,tool ,args ,cb) calls)
             (if (gptel-tool-async tool)
                 (apply (gptel-tool-function tool) cb args)
               (let ((result
                      (condition-case errdata
                          (apply (gptel-tool-function tool) args)
                        (error (mapconcat #'gptel--to-string errdata " ")))))
                 (funcall cb result)))))
          ((pred stringp)
           (funcall main-cb (format "%s result for task: %s\n\n%s"
                                    (capitalize agent-type) description resp))))))))

(provide 'llm-tools)
;;; llm-tools.el ends here

;; Local Variables:
;; elisp-flymake-byte-compile-load-path: ("~/.local/share/git/elpaca/repos/gptel/" "~/.local/share/git/elpaca/repos/transient/lisp" "~/.local/share/git/elpaca/repos/compat/")
;; End:
