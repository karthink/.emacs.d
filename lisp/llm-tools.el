;; -*- lexical-binding: t; -*-
(require 'gptel)
(require 'eww)

;; [[file:../../Documents/org/data/8c/5d1967-1a60-4ed9-8440-9ae07450a7f5/tool-test.org::*Search the web with brave, top 5 results with links][Search the web with brave, top 5 results with links:1]]
;;;; Web tools
(defun my/gptel-brave-search (callback query &optional count)
  "Return a JSON array of COUNT web search results for QUERY.

CALLBACK is called on the result."
  (let* ((brave-url "https://api.search.brave.com/res/v1/web/search")
         (brave-api-key
          (lambda () (let ((message-log-max nil)
                      (inhibit-message t))
                  (auth-source-pass-get 'secret "api/api.search.brave.com/search"))))
         (brave-url-string
          (lambda (q) (concat brave-url "?"
                         (url-build-query-string
                          `(("q" ,(url-hexify-string q))
                            ("count" ,(format "%s" (or count 5)))
                            ("page" ,(format "%s" 0)))))))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
            ("Accept" . "application/json")
            ("Accept-Encoding" . "gzip")
            ("X-Subscription-Token" . ,(let ((key brave-api-key))
                                         (if (functionp key) (funcall key) key))))))
    (url-retrieve (funcall brave-url-string query)
                  (lambda (_)
                    (goto-char url-http-end-of-headers)
                    (when-let* ((attrs (ignore-errors (json-parse-buffer :object-type 'plist)))
                                (raw-results (map-nested-elt attrs '(:web :results)))
                                (annotated-results
                                 (vconcat
                                  (mapcar
                                   (lambda (item)
                                     (let* ((title (map-elt item :title))
                                            (url (map-elt item :url))
                                            (desc (map-elt item :description)))
                                       (list :url url :title title :description desc)))
                                   raw-results))))
                      (funcall callback annotated-results)))
                  nil 'silent)))

(gptel-make-tool
 :name "search_web"
 :function 'my/gptel-brave-search
 :description "Search the web for the first five results to a query.  The query can be an arbitrary string.  Returns the top five results from the search engine as a plist of objects.  Each object has the keys `:url`, `:title` and `:description` for the corresponding search result.

If required, consider using the url as the input to the `read_url` tool to get the contents of the url.  Note that this might not work as the `read_url` tool does not handle javascript-enabled pages."
 :args `((:name "query" :type string :description "The natural language search query, can be multiple words."))
 :async t
 :category "web")
;; Search the web with brave, top 5 results with links:1 ends here

;; [[file:../../Documents/org/data/8c/5d1967-1a60-4ed9-8440-9ae07450a7f5/tool-test.org::*Read a non-js webpage with eww][Read a non-js webpage with eww:1]]
(gptel-make-tool
 :function (lambda (url)
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min)) (forward-paragraph)
               (let ((dom (libxml-parse-html-region (point) (point-max))))
                 (run-at-time 0 nil #'kill-buffer (current-buffer))
                 (with-temp-buffer
                   (eww-score-readability dom)
                   (shr-insert-document (eww-highest-readability dom))
                   (buffer-substring-no-properties (point-min) (point-max))))))
 :name "read_url"
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
               :type "string"
               :description "The URL to read"))
 :category "web")

;; (gptel-make-tool
;;  :function (lambda (cb url)
;;  	    (url-retrieve
;;               url (lambda (status)
;;                     (goto-char (point-min)) (forward-paragraph)
;;                     (delete-region (point-min) (point))
;;                     ;; (shr-render-region (point-min) (point-max))
;;                     (funcall
;;                      cb (buffer-substring-no-properties
;;                          (point-min) (point-max))))))
;;  :name "read_url"
;;  :description "Fetch and read the contents of a URL"
;;  :args (list '(:name "url"
;;  	      :type "string"
;;  	      :description "The URL to read"))
;;  :async t)
;; Read a non-js webpage with eww:1 ends here

;; [[file:../../Documents/org/data/8c/5d1967-1a60-4ed9-8440-9ae07450a7f5/tool-test.org::*Read a youtube transcript and metadata][Read a youtube transcript and metadata:1]]
(gptel-make-tool
 :name "get_youtube_meta"
 :function #'my/gptel-youtube-metadata
 :description "Find the description and video transcript for a youtube video.  Return a JSON object containing two fields:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
 :args '((:name "url"
          :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
          :type "string"))
 :category "web"
 :async t
 :include t)

(defun my/gptel-youtube-metadata (callback url)
  (let* ((video-id
          (and (string-match
                (concat
                 "^\\(?:http\\(?:s?://\\)\\)?\\(?:www\\.\\)?\\(?:youtu\\(?:\\(?:\\.be\\|be\\.com\\)/\\)\\)"
                 "\\(?:watch\\?v=\\)?" "\\([^?&]+\\)")
                url)
               (match-string 1 url)))
         (dir (file-name-concat temporary-file-directory "yt-dlp" video-id)))
    (if (file-directory-p dir) (delete-directory dir t))
    (make-directory dir t)
    (let ((default-directory dir) (idx 0)
          (data (list :description nil :transcript nil)))
      (make-process :name "yt-dlp"
                    :command `("yt-dlp" "--write-description" "--skip-download" "--output" "video" ,url)
                    :sentinel (lambda (proc status)
                                (cl-incf idx)
                                (let ((default-directory dir))
                                  (when (file-readable-p "video.description")
                                    (plist-put data :description
                                               (with-temp-buffer
                                                 (let ((coding-system-for-read 'utf-8-unix))
                                                   (insert-file-contents "video.description")
                                                   (buffer-string))))))
                                (when (= idx 2)
                                  (funcall callback data)
                                  (delete-directory dir t))))
      (make-process :name "yt-dlp"
                    :command `("yt-dlp" "--skip-download" "--write-auto-subs" "--sub-langs"
                               "en,-live_chat" "--convert-subs" "srt" "--output" "video" ,url)
                    :sentinel (lambda (proc status)
                                (cl-incf idx)
                                (let ((default-directory dir))
                                  (when (file-readable-p "video.en.srt")
                                    (plist-put data :transcript
                                               (with-temp-buffer
                                                 (let ((coding-system-for-read 'utf-8-unix))
                                                   (insert-file-contents "video.en.srt")
                                                   (buffer-string))))))
                                (when (= idx 2)
                                  (funcall callback data)
                                  (delete-directory dir t)))))))
;; Read a youtube transcript and metadata:1 ends here

;; [[file:../../Documents/org/data/8c/5d1967-1a60-4ed9-8440-9ae07450a7f5/tool-test.org::*emacs tools][emacs tools:1]]
;;;; Emacs tools
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

;; Message buffer logging tool
;; (gptel-make-tool
;;  :function (lambda (text)
;;              (message "%s" text)
;;              (format "Message sent: %s" text))
;;  :name "echo_message"
;;  :description "Send a message to the *Messages* buffer"
;;  :args (list '(:name "text"
;;                :type "string"
;;                :description "The text to send to the messages buffer"))
;;  :category "emacs")

;; buffer retrieval tool
(gptel-make-tool
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "error: buffer %s is not live." buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :name "read_buffer"
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
               :type "string"
               :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")

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
;; emacs tools:1 ends here

;; [[file:../../Documents/org/data/8c/5d1967-1a60-4ed9-8440-9ae07450a7f5/tool-test.org::*filesystem tools][filesystem tools:1]]
;;;; Filesystem tools
(gptel-make-tool
 :function (lambda (directory)
	     (mapconcat #'identity
                        (directory-files directory)
                        "\n"))
 :name "list_directory"
 :description "List the contents of a given directory"
 :args (list '(:name "directory"
	       :type "string"
	       :description "The path to the directory to list"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (parent name)
             (condition-case nil
                 (progn
                   (make-directory (expand-file-name name parent) t)
                   (format "Directory %s created/verified in %s" name parent))
               (error (format "Error creating directory %s in %s" name parent))))
 :name "make_directory"
 :description "Create a new directory with the given name in the specified parent directory"
 :args (list '(:name "parent"
	       :type "string"
	       :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name"
	       :type "string"
	       :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem"
 :confirm t)

(gptel-make-tool
 :function (lambda (path filename content)
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :name "create_file"
 :description "Create a new file with the specified content"
 :args (list '(:name "path"
	       :type "string"
	       :description "The directory where to create the file")
             '(:name "filename"
	       :type "string"
	       :description "The name of the file to create")
             '(:name "content"
	       :type "string"
	       :description "The content to write to the file"))
 :category "filesystem"
 :confirm t)

(gptel-make-tool
 :function (lambda (filepath)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name filepath))
	       (buffer-string)))
 :name "read_file"
 :description "Read and display the contents of a file"
 :args (list '(:name "filepath"
	       :type "string"
	       :description "Path to the file to read.  Supports relative paths and ~."))
 :category "filesystem")

(gptel-make-tool
 :name "apply_diff_fenced"
 :description (concat
               "Applies a diff (patch) to a specified file using fenced diff content. This is the PREFERRED method for modifying files as it is more token-efficient."
               "The diff must be provided within fenced code blocks (=diff or =patch) and be in unified format. "
               "The LLM should generate the diff such that the file paths within the diff "
               "(e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'file_path' argument and chosen 'patch_options'. "
               "Common 'patch_options' include: '' (empty, if paths in diff are exact or relative to current dir of file_path), "
               "'-p0' (if diff paths are full or exactly match the target including prefixes like 'a/'), "
               "'-p1' (if diff paths have one leading directory to strip, e.g., diff has 'a/src/file.c' and you want to patch 'src/file.c' from project root). "
               "Default options are '-N' (ignore already applied patches).")
 :args (list
        '(:name "file_path"
                :type string
                :description "The path to the file that needs to be patched.")
        '(:name "diff_content"
                :type string
                :description "The diff content within fenced code blocks (=diff or =patch) in unified format.")
        '(:name "patch_options"
                :type string
                :optional t
                :description "Optional: Additional options for the 'patch' command (e.g., '-p1', '-p0', '-R'). Defaults to '-N'. Prepend other options if needed, e.g., '-p1 -N'.")
        '(:name "working_dir"
                :type string
                :optional t
                :description "Optional: The directory in which to interpret file_path and run patch. Defaults to the current buffer's directory if not specified."))
 :category "filesystem"
 :function
 (lambda (file_path diff_content &optional patch_options working_dir)
   ;; Extract diff content from fenced blocks
   (let ((extracted-diff 
          (if (string-match "=\\(?:diff\\|patch\\)?\n\\(\\(?:.\\|\n\\)*?\\)\n=" diff_content)
              (match-string 1 diff_content)
            ;; If no fenced block found, try to use content as-is but warn
            (progn
              (message "Warning: No fenced diff block found, using content as-is")
              diff_content))))
     
     ;; Continue with original logic using extracted diff
     (setq diff_content extracted-diff))
   
   (let ((original-default-directory default-directory)
         (user-patch-options (if (and patch_options (not (string-empty-p patch_options)))
                                 (split-string patch_options " " t)
                               nil))
         ;; Combine user options with -N, ensuring -N is there.
         ;; If user provides -N or --forward, use their version. Otherwise, add -N.
         (base-options '("-N"))
         (effective-patch-options '()))

     (if user-patch-options
         (if (or (member "-N" user-patch-options) (member "--forward" user-patch-options))
             (setq effective-patch-options user-patch-options)
           (setq effective-patch-options (append user-patch-options base-options)))
       (setq effective-patch-options base-options))

     (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
            (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
            (target-file nil)
            (exit-status -1) ; Initialize to a known non-zero value
            (result-output "")
            (result-error ""))
       (unwind-protect
           (progn
             (when (and working_dir (not (string-empty-p working_dir)))
               (setq default-directory (expand-file-name working_dir)))

             (setq target-file (expand-file-name file_path))

             (unless (file-exists-p target-file)
               ;; Use error to signal failure, which gptel should catch.
               (error "File to patch does not exist: %s" target-file))

             (with-temp-message (format "Applying diff to: `%s` with options: %s" target-file effective-patch-options)
               (with-temp-buffer
                 (insert diff_content)
                 (unless (eq (char-before (point-max)) ?\n)
                   (goto-char (point-max))
                   (insert "\n"))

                 ;; Pass buffer *names* to call-process-region
                 (setq exit-status (apply #'call-process-region
                                          (point-min) (point-max)
                                          "patch"       ; Command
                                          nil           ; delete region (no)
                                          (list out-buf-name err-buf-name) ; stdout/stderr buffer names
                                          nil           ; display (no)
                                          (append effective-patch-options (list target-file))))))

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
                 (format "Diff successfully applied to %s.\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                         target-file effective-patch-options result-output result-error)
               ;; Signal an Elisp error, which gptel will catch and display.
               ;; The arguments to 'error' become the error message.
               (error "Failed to apply diff to %s (exit status %s).\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                      target-file exit-status effective-patch-options result-output result-error)))
         ;; Cleanup clause of unwind-protect
         (setq default-directory original-default-directory)
         (let ((stdout-buf-obj (get-buffer out-buf-name))
               (stderr-buf-obj (get-buffer err-buf-name)))
           (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))
           (when (buffer-live-p stderr-buf-obj) (kill-buffer stderr-buf-obj)))))))
 :include t)
;; filesystem tools:1 ends here

(provide 'llm-tools)
