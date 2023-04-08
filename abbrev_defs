;;-*-coding: utf-8;-*-
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("lx" "LaTeX" nil :count 11)
   ))

(define-abbrev-table 'eshell-mode-abbrev-table
  '(
    ("aur" "sudo aura" nil :count 6)
    ("dof" "dired-other-frame" nil :count 3)
    ("dow" "dired-other-window" nil :count 3)
    ("ec" "emacsclient -c" nil :count 3)
    ("et" "emacsclient -t" nil :count 3)
    ("ffof" "find-file-other-frame" nil :count 5)
    ("ffot" "find-file-other-tab" nil :count 1)
    ("ffow" "find-file-other-window" nil :count 10)
    ("jcu" "journalctl --user" nil :count 1)
    ("la" "ls -lAh" nil :count 2)
    ("ll" "ls -l" nil :count 4)
    ("pac" "sudo pacman" nil :count 29)
    ("qb" "qutebrowser" nil :count 3)
    ("qnb" "qutebrowser --temp-basedir --config ~/.config/qutebrowser/config.py --set \"colors.webpage.darkmode.enabled\" \"False\"" nil :count 4)
    ("qrdecode" "zbarimg" nil :count 2)
    ("qutenb" "qutebrowser --temp-basedir --config ~/.config/qutebrowser/config.py --set \"colors.webpage.darkmode.enabled\" \"False\"" nil :count 6)
    ("scu" "systemctl --user" nil :count 29)
    ("sjc" "sudo journalctl" nil :count 1)
    ("ssc" "sudo systemctl" nil :count 7)
    ("tri" "trizen -S" nil :count 5)
    ("xo" "xdg-open" nil :count 5)
    ("yt" "youtube-dl -w -c -o '%(title)s.%(ext)s' -f 'bestvideo[height<=?1080]+bestaudio/best' --add-metadata" nil :count 3)
    ("yta" "youtube-dl -w -c -r 1M -o '%(playlist_index)s-%(title)s.%(ext)s' --add-metadata  -f 'bestaudio[ext=m4a]'" nil :count 2)
   ))

(define-abbrev-table 'notmuch-message-mode-abbrev-table
  '(
    ("K" "Karthik" nil :count 9)
    ("k" "Karthik" nil :count 11)
   ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("hte" "the" nil :count 3)
    ("rl" "Rayleigh" nil :count 2)
    ("teh" "the" nil :count 2)
    ("yt" "Youtube" nil :count 7)
   ))

(define-abbrev-table 'shell-mode-abbrev-table
  '(
    ("jcu" "journalctl --user" nil :count 1)
    ("la" "ls -lAh" nil :count 1)
    ("ll" "ls -l" nil :count 1)
    ("pac" "sudo pacman" nil :count 1)
    ("scu" "systemctl --user" nil :count 4)
    ("sjc" "sudo journalctl" nil :count 1)
    ("ssc" "sudo systemctl" nil :count 1)
    ("tri" "trizen -S" nil :count 1)
   ))

(define-abbrev-table 'text-mode-abbrev-table
  '(
    ("lx" "LaTeX" nil :count 4)
   ))

