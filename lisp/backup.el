;;----------------------------------------------------------------------
;; REMEMBER-MODE
;;----------------------------------------------------------------------
;; Customizations to remember-mode so I can use it as my journal/notes.

(add-hook 'remember-mode-hook 
          '(lambda nil
             (setq remember-data-file (expand-file-name 
                                       "~/doodles/notes.txt"))
             (setq remember-leader-text "")))

(defadvice remember-finalize (after remember-mode-add-delimiter activate compile)
  (let ( (buf (find-file remember-data-file)) )
    (save-excursion
      (goto-char (point-max))
      (insert "\n%\n"))
    (save-buffer)
    (kill-buffer buf)
    ))

(defun remember-journal ()
               "Run (remember) with my journal as the data file."
               (interactive)
               (setq remember-data-file 
                     (expand-file-name 
                      "~/doodles/journal.txt"))
               (remember)
               (setq remember-data-file "~/doodles/notes.txt"))

