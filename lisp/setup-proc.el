;;; setup-proc.el --- Config for common OS interactions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: multimedia, convenience

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

;;;----------------------------------------------------------------
;; ** BROWSE-URL & MPV
;;;----------------------------------------------------------------
(use-package browse-url
  :commands (browse-url-umpv)
  :config
  (when IS-LINUX
    ;; I don't want unnecessary child processes
    (defun my/systemd-run (&rest process-args)
      (if (executable-find "systemd-run")
          (apply #'start-process (car process-args) nil
                 "systemd-run" "--user"
                 (concat "--unit=emacs_media_"
                         (format-time-string "%m%d%H%M%S_%3N"))
                 "--property=StandardOutput=null"
                 "--property=StandardError=null"
                 "--quiet" process-args)
        (apply #'start-process (car process-args) nil
               "nohup" process-args)))

    (defun browse-url-umpv (url &optional single)
      (my/systemd-run (if single "mpv" "umpv")
                      (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv-enqueue (url &optional _)
      (condition-case nil
          (let ((enqueue-proc
                 (make-network-process
                  :name "mpv" :buffer nil
                  :service "/tmp/mpvsocket"
                  :family 'local :coding 'utf-8
                  :sentinel
                  (lambda (proc _)
                    (unless (= (process-exit-status proc) 0)
                      (browse-url-mpv url))))))
            (process-send-string
             enqueue-proc
             (concat (json-serialize
                      `(:command [ "loadfile" ,url "append-play" ]))
                     "\n")))
        (error (browse-url-mpv url))))

    (defun browse-url-mpv-enqueue (url &optional _)
      (let ((exit-status
             (call-process "umpv_last" nil nil nil
                           (shell-quote-wildcard-pattern url))))
        (message "%S" exit-status)
        (when (not (= exit-status 0))
          (browse-url-mpv url))))

    (defun browse-url-mpv-hd (url &optional _)
      (my/systemd-run
       "mpv" "--profile=protocol-hd-video"
       (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv-audio (url &optional _)
      (my/systemd-run
       "mpv" "--video=no" "--force-window=yes"
       (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv (url &optional _)
      (pcase (prefix-numeric-value current-prefix-arg)
        (4 (browse-url-mpv-hd url))
        (0 (browse-url-mpv-audio url))
        (1 (my/systemd-run "mpv"
                           (shell-quote-wildcard-pattern url)))
        (_ (browse-url-mpv-enqueue url))))

    (setq browse-url-new-window-flag t)))


;;;----------------------------------------------------------------
;; *** BROWSER-HIST MAYBE
;;----------------------------------------------------------------
(use-package browser-hist
  :when IS-LINUX
  :ensure (:host github :repo "agzam/browser-hist.el"
           :remotes ("copy" :repo "karthink/browser-hist.el" :protocol ssh))
  :bind ("M-s U" . browser-hist-search)
  :config
  (setf (alist-get 'firefox browser-hist-db-paths)
        "$HOME/.mozilla/firefox/$USER/places.sqlite"
        browser-hist-default-browser 'firefox
        browser-hist-cache-timeout (* 24 60 60)))

;;----------------------------------------------------------------
;; ** OCR and dictation
;;----------------------------------------------------------------
;; *** TESSERACT
;;----------------------------------------------------------------
(use-package emacs
  :when (and IS-LINUX (executable-find "tesseract"))
  :config
  (defun tesseract-file (&optional files)
    "Run tesseract on FILE, with output to kill ring.

If no FILE is specified get its path from the kill ring."
    (interactive (list
                  (cond ((derived-mode-p 'dired-mode) (dired-get-marked-files))
                        (t (read-file-name "File to tesseract: ")))))
    (let ((output-buf (generate-new-buffer " tesseract-out" t)))
      (dolist (f (ensure-list files))
        (if (not (file-readable-p f))
            (message "File %s not readable"
                     (truncate-string-to-width f 40))
          (let ((status (call-process "tesseract" nil
                                      `(,output-buf nil) t f "-")))
            (if (not (= status 0))
                (message "Tesseract failed on %s!" f)))))
      (with-current-buffer output-buf
        (kill-new (buffer-string))
        (kill-buffer (current-buffer))
        (message "Copied Tesseract output to kill-ring"))))

  (defun tesseract-clipboard ()
    "Run tesseract on file path from kill-ring."
    (interactive)
    (tesseract-file (string-trim (current-kill 0 t)))))

;;;----------------------------------------------------------------
;; *** WHISPER
;;;----------------------------------------------------------------
(use-package whisper
  :unless IS-MAC
  :ensure (:host github :repo "natrys/whisper.el")
  :bind (("C-c C-SPC"   . whisper-run)
         ("C-c C-S-SPC" . my/whisper-run-remote))
  :config
  (defun my/whisper-run-remote ()
    "Run whisper transcription via Openai"
    (interactive)
    (if (process-live-p whisper--recording-process)
        (interrupt-process whisper--recording-process)
      (setq whisper-openai-api-key
            (auth-source-pass-get 'secret "api/openai.com")
            whisper-server-mode 'openai)
      (letrec ((restore
                (lambda () (remove-hook 'whisper-after-transcription-hook
                                   restore)
                  (setq whisper-openai-api-key nil
                        whisper-server-mode 'local))))
        (add-hook 'whisper-after-transcription-hook
                  restore)))
    (whisper-run))
  (setq whisper-model "base.en"
        whisper-use-threads 14
        whisper-server-mode 'local)
  (when IS-GUIX                         ;Whisper managed by NixOS
    (require 'xdg)
    (define-advice whisper--find-whispercpp-server
        (:before-until () whisper-from-system)
      (executable-find "whisper-server"))
    ;; NOTE: Ensure models are in $XDG_CACHE_HOME/whisper.cpp/models/
    (setq whisper-install-whispercpp nil
          whisper-install-directory (xdg-cache-home))))

;;----------------------------------------------------------------
;; ** DETACHED
;;----------------------------------------------------------------
;; Testing detached
(use-package detached
  :disabled
  :ensure t
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ;; ([remap compile] . detached-compile)
         ;; ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)
           (detached-session-directory
            (expand-file-name "sessions" user-cache-directory))))

(provide 'setup-proc)
;;; setup-proc.el ends here
