;; -*- lexical-binding: t; -*-

;;---------------------------------------------------------
;; * gptel general behavior
;;---------------------------------------------------------
(use-package gptel
  ;; :straight (:local-repo "~/.local/share/git/gptel/")
  :ensure (:host github :protocol ssh :repo "karthink/gptel")
  :commands (gptel gptel-send)
  :hook ((gptel-pre-response . my/gptel-easy-page)
         (gptel-mode . (lambda ()
                         (setq-local gptel-cache '(message))
                         (add-hook 'before-save-hook #'my/gptel-assign-filename
                                   nil 'local))))
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . gptel-send)
         ("C-c j" . gptel-menu)
         ("C-c C-g" . gptel-abort)
         ("C-c SPC" . my/gptel-easy-page)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic)
         :map embark-region-map
         ("+" . gptel-add)
         :map this-buffer-file-map
         ("+" . gptel-add))
  :config
  (auth-source-pass-enable)
  (setq-default gptel-model 'gpt-4.1-nano
                gptel-backend gptel--openai
                gptel-display-buffer-action '(pop-to-buffer-same-window))

  (defalias 'my/gptel-easy-page
    (let ((map (make-composed-keymap
                (define-keymap
                  "RET" 'gptel-end-of-response
                  "n"   'gptel-end-of-response
                  "p"   'gptel-beginning-of-response)
                my-pager-map))
          (scrolling
           (propertize  "SCRL" 'face '(:inherit highlight))))
      (require 'pixel-scroll)
      (lambda ()
        (interactive)
        (when (eq (window-buffer (selected-window))
                  (current-buffer))
          (add-to-list 'mode-line-format scrolling)
          (set-transient-map
           map t
           (lambda () (setq mode-line-format
                       (delete scrolling mode-line-format))))))))

  (defun my/gptel-mode-add-prop-line ()
    "Ensure that this file opens with `gptel-mode' enabled."
    (save-excursion
      (let ((enable-local-variables t)) ; Ensure we can modify local variables
        (if (and (save-excursion (goto-char (point-min)) (looking-at ".*-\\*-")))
            ;; If there's a -*- line
            ;; First remove any existing eval, then add the new one
            (modify-file-local-variable-prop-line 'eval nil 'delete))
        ;; Always add our eval
        (add-file-local-variable-prop-line
         'eval '(and (fboundp 'gptel-mode) (gptel-mode 1))))))

  (defvar my/gptel-chat-directory
    (file-name-as-directory
     (file-name-concat (xdg-data-home) "gptel-chat")))

  (defun gptel-resume (chat)
    "Resume previous gptel chat stored in `my/gptel-chat-directory'."
    (interactive (list (read-file-name
                        "Resume chat: " my/gptel-chat-directory nil t)))
    (find-file chat))

  (defun my/gptel-assign-filename ()
    (unless (or (buffer-file-name) current-prefix-arg)
      (make-directory my/gptel-chat-directory t)
      (my/gptel-mode-add-prop-line)
      (setq buffer-file-name
            (file-name-concat
             my/gptel-chat-directory
             (concat
              (format-time-string "%Y%m%d%H%M%2S-")
              (file-name-sans-extension
               (replace-regexp-in-string " " "-" (buffer-name)))
              (pcase major-mode
                ('org-mode ".org") ('markdown-mode ".md") (_ ".txt")))))
      (rename-buffer (file-name-nondirectory buffer-file-name) t)))

  (defun my/gptel-remove-headings (beg end)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward org-heading-regexp end t)
          (forward-line 0)
          (delete-char (1+ (length (match-string 1))))
          (insert-and-inherit "*")
          (end-of-line)
          (skip-chars-backward " \t\r")
          (insert-and-inherit "*")))))
  
  (defun my/gptel-latex-preview (beg end)
    (when (derived-mode-p 'org-mode)
      (org-latex-preview--preview-region 'dvisvgm beg end)))
  (add-hook 'gptel-post-response-functions #'my/gptel-latex-preview)
  (add-hook 'gptel-post-response-functions #'my/gptel-remove-headings)

  (define-advice gptel-api-key-from-auth-source
      (:around (func &rest args) silence)
    (let ((inhibit-message t)) (apply func args)))

  ;; (with-eval-after-load 'gptel-transient
  ;;   (transient-suffix-put 'gptel-menu (kbd "-m") :key "M")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-c") :key "C")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-n") :key "N")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-t") :key "T"))

  (setq gptel--system-message (alist-get 'default gptel-directives)
        gptel-default-mode 'org-mode)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: "
        (alist-get 'org-mode gptel-response-prefix-alist) "*Response*:\n"
        (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")
  (with-eval-after-load 'gptel-org
    (setq-default gptel-org-branching-context t))

  (add-to-list 'popper-reference-buffers "\\*gptel-log\\*")
  (setf (alist-get "\\*gptel-log\\*" display-buffer-alist nil nil #'equal)
        `((display-buffer-reuse-window display-buffer-in-side-window)
          (side . right)
          (window-width . 72)
          (slot . 20)
          (body-function . ,(lambda (win)
                              (select-window win)
                              (my/easy-page))))))

;; Mode line status indicators
;; ---------------------------
(use-package gptel
  :disabled
  :config
  ;; Show model but only when it differs from the global value
  (cl-pushnew
   '(:propertize
     (:eval
      (when (local-variable-p 'gptel--system-message)
        (concat
         "["
         (if-let* ((n (car-safe (rassoc gptel--system-message gptel-directives))))
             (gptel--model-name n)
           (gptel--describe-directive gptel--system-message 12))
         "]")))
     'face 'gptel-rewrite-highlight-face)
   mode-line-misc-info)
  (add-to-list
   'mode-line-misc-info
   '(:eval
     (unless gptel-mode
       (when (and (local-variable-p 'gptel-model)
                  (not (eq gptel-model (default-value 'gptel-model))))
         (concat "[" (gptel--model-name gptel-model) "]")))))

  ;; Variable to hold status indicator
  (defvar-local gptel--mode-line-format " ")

  (defun gptel--mode-line-format (msg &optional face)
    (setq gptel--mode-line-format
          (if face (propertize msg 'face face) msg)))

  ;; Update according to current state
  (defun gptel--mode-line-update (fsm)
    (let ((buf (plist-get (gptel-fsm-info fsm) :buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (pcase (gptel-fsm-state fsm)
            ('WAIT (gptel--mode-line-format "↑" 'warning))
            ('TYPE (gptel--mode-line-format "↓" 'success))
            ('TOOL (gptel--mode-line-format "🔨" 'warning))
            ('ERRS (gptel--mode-line-format "❌" 'error))
            (_     (gptel--mode-line-format " ")))))))

  ;; Add to `gptel-send' FSM handlers
  (dolist (state '(WAIT TYPE TOOL ERRS DONE))
    (cl-pushnew 'gptel--mode-line-update
                (alist-get state gptel-send--handlers)))

  ;; ;; Optional: Also add to `gptel-request' FSM handlers
  ;; ;; (i.e. for all gptel commands, not just `gptel-send')
  ;; (dolist (state '(WAIT TYPE TOOL ERRS DONE))
  ;;   (cl-pushnew 'gptel--mode-line-update
  ;;               (alist-get state gptel-request--handlers)))

  ;; Add indicator to the `mode-line-misc-info'
  (cl-pushnew '(:eval gptel--mode-line-format) mode-line-misc-info
              :test #'equal))

;;---------------------------------------------------------
;; * gptel LLM backends
;;---------------------------------------------------------
(use-package gptel
  :after gptel
  :config
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '( openai/gpt-oss-120b openai/gpt-oss-20b
               deepseek-r1-distill-llama-70b gemma-7b-it
               llama-3.3-70b-versatile llama-3.1-8b-instant))

  (gptel-make-xai "xai"
    :stream t
    :key #'gptel-api-key-from-auth-source)

  (defvar gptel--anthropic
    (gptel-make-anthropic "Claude" :key gptel-api-key :stream t))

  (gptel-make-preset 'think
    :request-params '( :thinking (:type "enabled" :budget_tokens 768)
                       :max_tokens 2048))

  (gptel-make-anthropic "Claude-thinking"
    :key #'gptel-api-key-from-auth-source
    :stream t
    :models '(claude-sonnet-4-20250514 claude-3-7-sonnet-20250219)
    :request-params '( :thinking (:type "enabled" :budget_tokens 1024)
                       :max_tokens 2048))

  (defvar gptel--togetherai
    (gptel-make-openai "TogetherAI"
      :host "api.together.xyz"
      :key gptel-api-key
      :stream t
      :models '(mistralai/Mixtral-8x7B-Instruct-v0.1
                codellama/CodeLlama-13b-Instruct-hf
                codellama/CodeLlama-34b-Instruct-hf)))

  (gptel-make-openai "Deepseek"
    :host "api.deepseek.com"
    :models '((deepseek-reasoner
               :capabilities (tool reasoning)
               :context-window 64 :input-cost 0.55 :output-cost 2.19)
              (deepseek-chat
               :capabilities (tool)
               :context-window 64
               :input-cost 0.27
               :output-cost 1.1))
    :key #'gptel-api-key-from-auth-source
    :stream t)

  (gptel-make-perplexity "Perplexity"
    :stream t
    :key #'gptel-api-key-from-auth-source)

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(openai/gpt-oss-120b
              openai/gpt-oss-20b:free
              deepseek/deepseek-r1-distill-llama-70b:free
              deepseek/deepseek-r1-distill-llama-70b:free))

  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key (lambda () (auth-source-pass-get 'secret "api/api.github.com"))
    :models '(DeepSeek-R1 gpt-4o-mini))

  (gptel-make-openai "llamacpp-cube"
    :host "bazzite.local:8080"
    :protocol "http"
    :models '( qwen2.5-coder-0.5b qwen2.5-coder-1.5b
               qwen3-1.7b qwen3-0.6b qwen3-4b gemma-3-1b)
    :stream t)

  (gptel-make-openai "llamacpp-t14"
    :host "localhost:8080"
    :protocol "http"
    :models '( qwen2.5-coder-0.5b qwen2.5-coder-1.5b
               qwen3-1.7b qwen3-0.6b qwen3-4b gemma-3-1b)
    :stream t)

  (defvar gptel--gemini
    (gptel-make-gemini "Gemini" :key gptel-api-key :stream t))

  (with-eval-after-load 'gptel-ollama
    (defvar gptel--ollama
      (gptel-make-ollama
          "Ollama"
        :host "192.168.1.11:11434"
        :models '( qwen3:4b llama3.1:8b qwen3:8b
                   (llava:7b :description Llava 1.6: Vision capable model
                             :capabilities (media)
                             :mime-types ("image/jpeg" "image/png")))
        :stream t)))

  (defvar gptel--gpt4all
    (gptel-make-gpt4all
        "GPT4All"
      :protocol "http"
      :host "localhost:4891"
      :models '(mistral-7b-openorca.Q4_0.gguf))))

;;================================================================
;; * Directives and presets
;;================================================================

;;----------------------------------------------------------------
;; ** gptel-prompts: Get directives from a prompts directory
;;----------------------------------------------------------------
(use-package gptel-prompts
  :ensure (:host github :repo "jwiegley/gptel-prompts")
  :after gptel
  :config
  (when (file-directory-p gptel-prompts-directory)
    (gptel-prompts-update)))

;;----------------------------------------------------------------
;; ** gptel presets
;;----------------------------------------------------------------
(use-package gptel
  :after gptel
  :config
  (gptel-make-preset 'default
    :description "My default settings for gptel"
    :system 'default
    :backend "ChatGPT"
    :model 'gpt-4.1-mini
    :tools nil :temperature nil :stream t
    :include-reasoning 'ignore)

  (gptel-make-preset 'nostream
    :description "No streaming"
    :stream nil)

  (gptel-make-preset 'prog
    :description "Claude Sonnet, with context, generates only code"
    :backend "Claude" :model 'claude-3-7-sonnet-20250219 :system 'programmer
    :tools nil :stream t :temperature nil :max-tokens nil :use-context 'system
    :include-reasoning nil)

  (gptel-make-preset 'sonar-gen
    :description "Sonar (non pro) with default instructions"
    :system 'default
    :backend "Perplexity"
    :model 'sonar :stream nil
    :tools nil :temperature 0.66)

  (gptel-make-preset 'cliwhiz
    :description "Haiku, no context, generates CLI commands" :backend "Claude"
    :model 'claude-3-5-haiku-20241022 :system 'cliwhiz :tools nil :stream t
    :temperature 0.66 :max-tokens nil :use-context 'nil :include-reasoning nil)

  (gptel-make-preset 'websearch
    :description "Add basic web search tools"
    :pre (lambda () (require 'llm-tools))
    :tools '(:append ("search_web" "read_url" "get_youtube_meta"))
    :system '(:append "\n\nUse the provided tools to search the web for up-to-date information."))

  (gptel-make-preset 'nixos
    :description "Add NixOS tools (minus darwin)"
    :pre (lambda () (gptel-mcp-connect '("nixos") 'sync))
    :system '(:append "\n\nUse the provided NixOS tools to look for up-to-date information and\
 examine the state of my system")
    :tools '( :append ("mcp-nixos")
              :function (lambda (tools)
                          (cl-delete-if
                           (lambda (tool)
                             (string-match-p "darwin" (gptel-tool-name tool)))
                           (mapcan #'gptel-get-tool tools)))))
  (gptel-make-preset 'files
    :pre (lambda () (require 'llm-tools))
    :description "Add filesystem tools"
    :tools '(:append ("read_file_or_directory" "read_file_lines"
                      "write_file" "make_directory"
                      "search_in_files" "replace_in_files")))

  (gptel-make-preset 'files-ro
    :pre (lambda () (require 'llm-tools))
    :description "Add read-only filesystem tools"
    :tools '(:append ("read_file_or_directory"
                      "read_file_lines"
                      "search_in_files")))

  (gptel-make-preset 'shell
    :pre (lambda () (require 'llm-tools))
    :description "Add Bash as a tool"
    :tools  '(:append ("execute_bash"))
    :system '(:append "Use the execute_bash tool to introspect and change the state of the system."))

  (gptel-make-preset 'tutor
    :description "Get Claude Sonnet to teach using hints"
    :system 'tutor
    :backend "Claude"
    :model 'claude-3-7-sonnet-20250219
    :tools nil :temperature 0.7
    :max-tokens 600)

  (gptel-make-preset 'pro
    :description "Sonnet 4, no-holds bared"
    :system 'default
    :backend "Claude"
    :model 'claude-sonnet-4-20250514
    :tools nil
    :include-reasoning nil)

  (gptel-make-preset 'json
    :description "JIT only: use JSON schema following @json cookie"
    :schema '(:eval (buffer-substring-no-properties
                     (point) (point-max)))
    :post (lambda () (delete-region (point) (point-max)))
    :include-reasoning nil)

  (defun my/gptel-windows-on-frame ()
    "Return all windows on frame that aren't gptel chat buffers."
    (delq (and-let* ((current-buf (window-buffer (selected-window)))
                     ((buffer-local-value 'gptel-mode current-buf)))
            (selected-window))
          (window-list)))

  (gptel-make-preset 'visible-buffers
    :description "Include the full text of all buffers visible in the frame."
    :context
    '(:eval (mapcar #'window-buffer (my/gptel-windows-on-frame))))

  (gptel-make-preset 'visible-text
    :description "Include visible text from all windows in the frame."
    :context
    '(:eval (mapcar (lambda (win) ;; Create (<buffer> :bounds ((start . end)))
                      `(,(window-buffer win)
                        :bounds ((,(window-start win) . ,(window-end win)))))
                    (my/gptel-windows-on-frame))))

  (gptel-make-preset 'explain
    :description "Deepseek-R1, explains the prompt text"
    :backend "Deepseek" :model 'deepseek-reasoner :system 'explain :tools nil
    :stream t :temperature nil :max-tokens nil
    :use-context 'system :include-reasoning nil))

;;================================================================
;; * gptel addons and other integration
;;================================================================

;;----------------------------------------------------------------
;; ** gptel-rewrite: rewrite regions of text
;;----------------------------------------------------------------
(use-package gptel-rewrite
  :after gptel
  :bind (:map gptel-rewrite-actions-map
         ("C-c C-i" . gptel--rewrite-inline-diff))
  :config
  (add-hook 'poi-functions
            (lambda (arg) (interactive "p")
              (dotimes (_ (abs arg))
                (if (> arg 0)
                    (gptel--rewrite-next)
                  (gptel--rewrite-previous)))))
  (defun gptel--rewrite-inline-diff (&optional ovs)
    (interactive (list (gptel--rewrite-overlay-at)))
    (unless (require 'inline-diff nil t)
      (user-error "Inline diffs require the inline-diff package"))
    (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
                ((buffer-live-p ov-buf)))
      (with-current-buffer ov-buf
        (cl-loop for ov in (ensure-list ovs)
                 for ov-beg = (overlay-start ov)
                 for ov-end = (overlay-end ov)
                 for response = (overlay-get ov 'gptel-rewrite)
                 do (delete-overlay ov)
                 (inline-diff-words
                  ov-beg ov-end response)))))

  (when (boundp 'gptel--rewrite-dispatch-actions)
    (add-to-list
     'gptel--rewrite-dispatch-actions '(?i "inline-diff")
     'append))

  (setf (alist-get 'infill gptel-directives) #'my/gptel-code-infill)
  (defun my/gptel-code-infill ()
    "Fill in code at point based on buffer context.  Note: Sends the whole buffer."
    (let ((lang (gptel--strip-mode-suffix major-mode)))
      `(,(format "You are a %s programmer and assistant in a code buffer in a text editor.

Follow my instructions and generate %s code to be inserted at the cursor.
For context, I will provide you with the code BEFORE and AFTER the cursor.


Generate %s code and only code without any explanations or markdown code fences.  NO markdown.
You may include code comments.

Do not repeat any of the BEFORE or AFTER code." lang lang lang)
        nil
        "What is the code AFTER the cursor?"
        ,(format "AFTER\n```\n%s\n```\n"
          (buffer-substring-no-properties
           (if (use-region-p) (max (point) (region-end)) (point))
           (point-max)))
        "And what is the code BEFORE the cursor?"
        ,(format "BEFORE\n```%s\n%s\n```\n" lang
          (buffer-substring-no-properties
           (point-min)
           (if (use-region-p) (min (point) (region-beginning)) (point))))
        ,@(when (use-region-p) "What should I insert at the cursor?")))))

;;----------------------------------------------------------------
;; ** gptel-ask: persistent side-buffer for one-off queries
;;----------------------------------------------------------------
(use-package gptel-ask
  :after gptel
  :bind (:map help-map
         ("C-q" . gptel-ask)
         :map embark-url-map
         ("?" . gptel-kagi-summarize))
  :config
  (defvar gptel--kagi
    (gptel-make-kagi
        "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api/kagi-ai.com")))
    "Kagi source for gptel")

  (setf (alist-get "^\\*gptel-ask\\*" display-buffer-alist
                   nil nil #'equal)
        '((display-buffer-reuse-window display-buffer-in-side-window)
          (side . right) (slot . 10) (window-width . 0.25)
          (window-parameters (no-delete-other-windows . t))
          (post-command-select-window . t)
          (bump-use-time . t)))

  (defun gptel-kagi-summarize (url)
    (interactive "sSummarize url: ")
    (let ((gptel-backend gptel--kagi)
          (gptel-model "summarize:agnes")
          (gptel-use-curl)
          (gptel-use-context))
      (gptel-request url
        :callback
        (lambda (response info)
          (if response
              (progn
                (gptel--prepare-ask-buffer)
                (let ((scroll-conservatively 0))
                  (with-current-buffer gptel-ask--buffer-name
                    (insert "\n" url "\nSummary:\n\n"
                            response "\n\n----")
                    (display-buffer (current-buffer)))))
            (message "gptel-request failed with message: %s"
                     (plist-get info :status)))))
      (message "Generating summary for: %s" url))))

;;----------------------------------------------------------------
;; ** gptel-quick: describe thing at point
;;----------------------------------------------------------------
(use-package gptel-quick
  :ensure (:host github :protocol ssh
           :repo "karthink/gptel-quick")
  :bind (:map embark-general-map
         ("?" . gptel-quick)))

;;----------------------------------------------------------------
;; ** flymake-gptel
;;----------------------------------------------------------------
(use-package flymake-gptel
  :ensure (:host github :protocol ssh
           :repo "karthink/flymake-gptel")
  :bind (:map flymake-gptel-repeat-map
         ("SPC" . flymake-gptel-accept-and-next))
  :config
  (put 'flymake-gptel-accept-and-next
       'repeat-map 'flymake-gptel-repeat-map)
  (defun flymake-gptel-accept-and-next ()
    (interactive)
    (flymake-gptel-accept (point))
    (flymake-gptel-next)))

;;----------------------------------------------------------------
;; ** Project-specific chat file
;;----------------------------------------------------------------
(use-package project
  :after (popper visual-fill-column)
  :bind (:map project-prefix-map
         ("C" . gptel-project))
  :config
  (setf (alist-get ".*Chat.org$" display-buffer-alist nil nil #'equal)
        `((display-buffer-in-side-window)
          (window-width . 0.3)
          (side . right)
          (body-function . ,#'select-window)))
  (defun gptel-project ()
    "Open the ChatGPT file for the current project."
    (interactive)
    (let ((default-directory (or (project-root (project-current))
                                 default-directory)))
      (find-file "Chat.org")
      (require 'gptel)
      (unless gptel-mode
        (gptel-mode 1))
      (unless visual-fill-column-mode
        (visual-fill-column-mode 1))
      (unless (equal popper-popup-status 'user-popup)
        (popper-toggle-type)))))

(use-package macher
  :ensure (:host github :repo "kmontag/macher")
  :after gptel
  :defer
  ;; :hook
  ;; Add the current file to the gptel context when making macher requests.
  ;; (macher-before-send
  ;;  .
  ;;  (lambda ()
  ;;    (when-let* ((filename (buffer-file-name))
  ;;                ((not (file-directory-p filename))))
  ;;      (gptel-add-file filename))))
  :config
  (setf (alist-get "\\*macher:.*\\*" display-buffer-alist
                   nil nil #'equal)
        '((display-buffer-in-side-window)
          (side . right)
          (slot . 22)
          (window-width . 84)))
  (setf (alist-get "\\*macher-patch:.*\\*" display-buffer-alist
                   nil nil #'equal)
        '((display-buffer-in-side-window)
          (side . right)
          (slot . 23)
          (window-width . 84)))

  ;; Customize patch display action. The 'macher-context' struct
  ;; contains data from the current request, including the contents of
  ;; any files that were edited.
  ;; (setopt macher-patch-ready-function
  ;;         (lambda (macher-context)
  ;;           (ediff-patch-file nil (current-buffer))))
  (macher-install))

;;----------------------------------------------------------------
;; ** Fake eshell integration (pretty silly)
;;----------------------------------------------------------------
(use-package gptel
  :hook ((eshell-mode . my/gptel-eshell-keys))
  :config
  (defun my/gptel-eshell-send (&optional arg)
    (interactive "P")
    (if (use-region-p)
        (gptel-send arg)
      (push-mark)
      (or (eshell-previous-prompt 0)
          (eshell-previous-prompt 1))
      (activate-mark)
      (gptel-send arg)
      (exchange-point-and-mark)
      (deactivate-mark)))
  (defun my/gptel-eshell-keys ()
    (define-key eshell-mode-map (kbd "C-c <return>")
                #'my/gptel-eshell-send)))

;;----------------------------------------------------------------
;; ** Actual eshell integration, currently non-functional
;;----------------------------------------------------------------
(use-package gptel-eshell
  :ensure (:host github :protocol ssh
           :repo "karthink/gptel-eshell")
  :defer)

;;----------------------------------------------------------------
;; ** git commit headings
;;----------------------------------------------------------------
(use-package gptel
  :after (gptel git-commit)
  :hook ((git-commit-setup . my/gptel-commit-summary))
  :config
  (gptel-make-preset 'commit-summary
    :description "For generating commit message summaries"
    :system (lambda () (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name
                      "commit-summary.txt" user-emacs-directory))
                    (buffer-string)))
    :backend "ChatGPT"
    :model 'gpt-4.1-nano
    :include-reasoning nil
    :tools nil)
  (defun my/gptel-commit-summary ()
    "Insert a commit message header line in the format I use, followed by a
standard magit (GNU style) changelog.

Don't get the LLM to write the commit message itself, because it's bad
at inferring my intent.

Intended to be placed in `git-commit-setup-hook'."
    (gptel-with-preset 'commit-summary
      (let ((commit-buffer (current-buffer))) ;commit message buffer

        (when (looking-at-p "[\n[:blank:]]+") ;Heuristic for blank message
          (with-temp-buffer
            (vc-git-command             ;insert diff
             (current-buffer) 1 nil
             "diff-index" "--exit-code" "--patch"
             (and (magit-anything-staged-p) "--cached")
             "HEAD" "--")

            (gptel-request nil          ;Run request on diff buffer contents
              :context commit-buffer
              :callback
              (lambda (resp info)
                (if (not (stringp resp))
                    (message "Git commit summary generation failed")
                  (with-current-buffer (plist-get info :context)
                    (save-excursion
                      (goto-char (point-min))
                      (insert resp "\n\n")
                      (magit-generate-changelog))))))))))))

;;----------------------------------------------------------------
;; ** org export gptel chat buffers
;;----------------------------------------------------------------
(use-package gptel
  :defer
  :after ox-html
  :config
  (defun my/org-html-filter-tool-block (tree backend info)
    "Rewrite #+begin_tool blocks as <details><summary>...</summary><pre>...</pre></details>."
    (org-element-map tree 'special-block
      (lambda (blk)
        (when (string= (org-element-property :type blk) "tool")
          (let* ((params (org-element-property :parameters blk))
                 (summary-blk
                  (org-element-create
                   'special-block
                   '(:type "summary"
                           :begin nil :end nil :contents-begin nil :contents-end nil
                           :parameters nil :parent blk)
                   params))
                 (body (org-element-interpret-data (org-element-contents blk)))
                 (code-blk
                  (org-element-create
                   'export-block
                   `(:type "HTML" :parent ,blk
                           :value ,(format "<pre>%s</pre>"
                                           (org-html-encode-plain-text (string-trim body))))
                   nil)))
            (org-element-put-property blk :type "details")
            (org-element-set-contents blk (list summary-blk code-blk)))))
      info)
    tree)

  (defun my/org-html-highlight-@prefix (text _backend info)
    "Wrap @words in <span class=\"at-prefix\">…</span> during Org export."
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "@\\([^[:blank:]]+\\)\\_>" nil t)
        (replace-match "<span class=\"at-prefix\">@\\1</span>"))
      ;; (goto-char (point-min))
      ;; (when-let* ((prompt-prefix
      ;;              (alist-get 'org-mode gptel-prompt-prefix-alist))
      ;;             (_ (not (string-empty-p prompt-prefix))))
      ;;   (while (search-forward prompt-prefix nil t)
      ;;     (forward-line 0) (insert "<hr>")))
      (buffer-string))
    ;; (setq text
    ;;       (replace-regexp-in-string
    ;;        "@\\([^[:blank:]]+\\)\\_>"
    ;;        "<span class=\"at-prefix\">@\\1</span>"
    ;;        text))
    )

  (defun my/org-export-gptel-chat
      (&optional async subtreep visible-only body-only ext-plist)
    (interactive)
    (let* ((extension ".html")
           (file (org-export-output-file-name extension subtreep))
           (org-export-coding-system org-html-coding-system))
      (org-export-to-file 'gptel-chat file
        async subtreep visible-only body-only ext-plist)))

  (org-export-define-derived-backend 'gptel-chat 'html
    :menu-entry
    '(?g "Export gptel chat (HTML)"
         ((?g "As HTML file" my/org-export-gptel-chat)
          (?o "As HTML file and open"
              (lambda (a s v b)
                (if a
                    (my/org-export-gptel-chat t s v b)
                  (org-open-file (my/org-export-gptel-chat nil s v b)))))))
    :filters-alist
    '((:filter-parse-tree my/org-html-filter-tool-block
                          my/org-export-ignore-headlines)
      (:filter-plain-text my/org-html-highlight-@prefix))
    :options-alist
    '((:html-doctype "HTML_DOCTYPE" nil "html5")
      (:html-html5-fancy nil "html5-fancy" t)
      (:html-head "HTML_HEAD_EXTRA" nil
                  "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://orgmode.org/worg/style/worg.css\"/>
<style>
details {
  border: 1px solid #cccccc; border-radius: 4px;
  padding: 0.75em 1em; margin: 1em 0;
  background: #fafbff; position: relative;
  transition: box-shadow 0.2s; box-shadow: 0 1px 3px rgba(0,0,0,0.03); }
details[open] { box-shadow: 0 2px 10px rgba(0,0,0,0.08); }
summary {
  font-weight: 200; cursor: pointer; outline: none;
  position: relative; padding-left: 1.4em; }
span.at-prefix {
  background: #ede2ff; color: #581997; padding: 0.1em 0.4em;
  border-radius: 0.25em; font-family: monospace; font-size: 120%; }
h2 code { font-family: inherit; font-size: inherit; font-weight: inherit; }
.note { font-size: 0.9em; border-radius: 4px;
        padding: 0.1em 0.75em; background: #f09fa9a1; }
</style>"
                  newline))))

;;================================================================
;; * Tools and MCP
;;================================================================

(use-package llm-tool-collection
  :ensure (:host github :repo "skissue/llm-tool-collection")
  :config (mapcar (apply-partially #'apply #'gptel-make-tool)
                  (llm-tool-collection-get-all))
  :defer)

(use-package ragmacs
  :ensure (:host github :repo "positron-solutions/ragmacs")
  :after gptel
  :defer
  :init
  (gptel-make-preset 'introspect
    :pre (lambda () (require 'ragmacs))
    :description "Introspect Emacs with Ragmacs"
    :system
    "You are pair programming with the user in Emacs and on Emacs.

Your job is to dive into Elisp code and understand the APIs and
structure of elisp libraries and Emacs.  Use the provided tools to do
so, but do not make duplicate tool calls for information already
available in the chat.

<tone>
1. Be terse and to the point.  Speak directly.
2. Explain your reasoning.
3. Do NOT hedge or qualify.
4. If you don't know, say you don't know.
5. Do not offer unprompted advice or clarifications.
6. Never apologize.
7. Do NOT summarize your answers.
</tone>

<code_generation>
When generating code:
1. Create a plan first: list briefly the design steps or ideas involved.
2. Use the provided tools to check that functions or variables you use
in your code exist.
3. Also check their calling convention and function-arity before you use
them.
</code_generation>

<formatting>
1. When referring to code symbols (variables, functions, tags etc)
enclose them in markdown quotes.
  Examples: `read_file`, `getResponse(url, callback)`
  Example: `<details>...</details>`
2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
</formatting>"
    :cache '(tool)
    :tools '("introspection")))

(use-package mcp
  :after gptel
  :ensure (:host github :repo "lizqwerscott/mcp.el")
  :config
  (require 'gptel-integrations)
  (setq mcp-hub-servers
        `(("github"
           :command "github-mcp-server"
           :args ("stdio")
           :env (:GITHUB_PERSONAL_ACCESS_TOKEN
                 ,(auth-source-pass-get 'secret "api/api.github.com")))
          ("filesystem"
           :command "mcp-server-filesystem"
           :args (,(expand-file-name "~/dotnix/")))
          ("deepwiki" :url "https://mcp.deepwiki.com/sse")
          ("memory" :command "mcp-server-memory")
          ("sequential-thinking"
           :command "uvx"
           :args ("--from" "git+https://github.com/arben-adm/mcp-sequential-thinking"
                  "--with" "portalocker" "mcp-sequential-thinking"))
          ("nixos" :command "uvx" :args ("mcp-nixos"))
          ("brave"
           :command "mcp-server-brave-search"
           :args nil
           :env (:BRAVE_API_KEY ,(auth-source-pass-get
                                  'secret "api/api.search.brave.com/search"))))))

(provide 'setup-gptel)

;; Local Variables:
;; outline-regexp: ";; \\*+"
;; End:
