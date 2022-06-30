;; emacs-i3.el -*- lexical-binding: t -*-
;; From Pavel Korytov, https://sqrtminusone.xyz/posts/2021-10-04-emacs-i3/

(require 'windmove)
(require 'transpose-frame)

;;----------------------------------------------------------------
;; ** i3 integration
;;----------------------------------------------------------------

(eval-when-compile
  (defmacro i3-msg (&rest args)
    `(start-process "emacs-i3-windmove" nil "i3-msg" ,@args)))

(defun my/emacs-i3-windmove (direction)
  (let ((other-window (windmove-find-other-window direction)))
    (if (or (null other-window) (window-minibuffer-p other-window))
        (i3-msg "focus" (symbol-name direction))
      (windmove-do-window-select direction))))

(defun my/emacs-i3-direction-exists-p (dir)
  (seq-some (lambda (dir)
          (let ((win (windmove-find-other-window dir)))
            (and win (not (window-minibuffer-p win)))))
        (pcase dir
          ('width '(left right))
          ('height '(up down)))))

(defun my/emacs-i3-move-window (dir &rest _)
  (let ((other-window (windmove-find-other-window dir))
        (other-direction (my/emacs-i3-direction-exists-p
                          (pcase dir
                            ('up 'width)
                            ('down 'width)
                            ('left 'height)
                            ('right 'height)))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (other-direction
      (evil-move-window dir))
     (t (i3-msg "move" (symbol-name dir))))))

(defun my/emacs-i3-integration (command)
  (pcase command
    ((rx bos "focus")
     (my/emacs-i3-windmove
      (intern (elt (split-string command) 1))))
    ((rx bos "move")
      (my/emacs-i3-move-window
      (intern (elt (split-string command) 1))))
    ((rx bos "resize")
     (my/emacs-i3-resize-window
       (intern (elt (split-string command) 2))
       (intern (elt (split-string command) 1))
       (string-to-number (elt (split-string command) 3))))
    ("layout toggle stacking tabbed split" (transpose-frame))
    ("split h" (evil-window-split))
    ("split v" (evil-window-vsplit))
    ("kill" (evil-quit))
    (- (i3-msg command))))

;;----------------------------------------------------------------
;; ** qtile integration
;;----------------------------------------------------------------

(defvar qtile-directions-alist
  '(("left"          . left)
    ("right"         . right)
    ("up"            . up)
    ("down"          . down)
    ("shuffle_left"  . left)
    ("shuffle_right" . right)
    ("shuffle_up"    . up)
    ("shuffle_down"  . down)))

(defun qtile-move (&rest args)
  "FIXME"
  (pcase (nth 0 args)
    ("move" (qtile--windmove
             (assoc (nth 1 args) qtile-directions-alist)))
    ("swap" (qtile--swap-window
             (assoc (nth 1 args) qtile-directions-alist)))))

(defmacro qtile-cmd (&rest args)
  `(start-process "emacs-qtile-windmove" nil
    "qtile" "cmd-obj" "-o" "layout" "-f" ,@args))

(defun qtile--windmove (direction)
  "FIXME"
  (let ((other-window (windmove-find-other-window (cdr direction))))
    (if (or (null other-window) (window-minibuffer-p other-window))
        ;; (qtile-cmd (car direction))
        (qtile--net-call "layout" (car direction))
      (windmove-do-window-select (cdr direction)))))

(defun qtile--direction-exists-p (dir)
  (seq-some (lambda (dir)
          (let ((win (windmove-find-other-window dir)))
            (and win (not (window-minibuffer-p win)))))
        (pcase dir
          ('width '(left right))
          ('height '(up down)))))

(defun qtile--swap-window (direction &rest _)
  (let ((other-window (windmove-find-other-window (cdr direction))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (t ;; (qtile-cmd (car direction))
      (qtile--net-call "layout" (car direction))))))


(defvar qtile--socket-dir
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name "~/.cache/qtile")))

(defvar qtile--socket
  (let ((f (file-name-concat
            qtile--socket-dir
            (concat "qtilesocket."
                    (or (getenv "DISPLAY")
                        (getenv "WAYLAND_DISPLAY"))))))
    (if-let ((_ (string-match-p ":" f))
             (f-link (expand-file-name
                      (make-temp-name "qtile-emacs-")
                      temporary-file-directory)))
 ;; (file-name-concat qtile--socket-dir "qtile-emacs")        
        (prog1  f-link
          (unless (file-symlink-p f-link)
            (make-symbolic-link f f-link)
            (add-hook 'kill-emacs-hook
                      (lambda ()
                        "Clean up socket connection to Qtile"
                        (delete-file f-link)))))
      f)))

(defun qtile--net-call (object command &rest args)
  (let ((emacs-qtile-proc 
         (make-network-process :name "emacs-qtile-proc"
                               :buffer "*qtile-net-proc*"
                               :service qtile--socket
                               :family 'local
                               :coding 'utf-8)))
    (process-send-string
     emacs-qtile-proc
     (concat
      (json-serialize `[[[,object :null]] ,command  [,@args] nil ])
      "\n"))
    ;; (qtile-socat object command)
    (process-send-eof emacs-qtile-proc)))

;;;; Socat process test
;; (defun qtile--ipc-call (object command &rest args)
;;   (let ((emacs-qtile-proc 
;;          (make-process :name "emacs-qtile-proc"
;;                        :buffer nil
;;                        :command `("socat" "-"
;;                                   ,(concat "UNIX-CONNECT:"
;;                                     qtile--socket))
;;                        :coding 'utf-8)))
;;     (process-send-string
;;      emacs-qtile-proc
;;      (concat
;;       (json-serialize `[[[,object :null]] ,command  [,@args] nil ])
;;       "\n"))
;;     ;; (qtile-socat object command)
;;     (process-send-eof emacs-qtile-proc)))

;;;; network-process + tq test
;;
;; (tq-enqueue
;;  qtile--queue
;;  (json-serialize [[["layout" :null]] "right" [] nil])
;;  "" nil (lambda (c ans) (prin1 ans (current-buffer))))
;;
;;
;; (defvar qtile--queue
;;   (tq-create
;;    (make-network-process :name "qtile-socket-q"
;;                          :family 'local
;;                          :service "/home/karthik/.cache/qtile/qtiletest")))

(provide 'emacs-i3)
