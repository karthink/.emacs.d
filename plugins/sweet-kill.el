;; Set of defadvices and functions to sweeten Emacs messages. I expect to get better at this soon.

(defadvice save-buffers-kill-terminal (before sweet-kill activate)
  (delete-other-windows)
  (switch-to-buffer "*Exit*")
  (delete-region (point-min) (point-max))
  (let ((h (1- (/ (window-height) 2))) (l (length haiku-emacs)))
    (open-line (1- (/ (window-height) 2)))
    (goto-line (1- (/ (window-height) 2)))
    (mapc (lambda (x)
            (insert x)
            (center-line)
            (open-line 1)
            (forward-line))
          (nth (random l) haiku-emacs))))
          
(provide 'sweet-kill)

;(setq l (length haiku-emacs))
;(nth (random l) haiku-emacs)

(defvar haiku-emacs 
  
  '(("my dot emacs grows"
    "one day i look inside it"
    "singularity")

   ("emacs starts, i wait"
    "while the lisp libraries load"
    "never close emacs")

   ("the sound of typing"
    "it can only mean one thing"
    "dot emacs expands")

   ("beyond the dreams of"
    "avarice, gnu emacs is"
    "a hidden treasure"

    "-- LuisFernandes")

   ("Oort is so awesome"
    "deuglifies Outlook crap"
    "`W k' rocks"

    "-- EdwardOConnor")

   ("Great clouds overhead"
    "Tiny black birds rise and fall"
    "Snow covers Emacs"

    "-- AlexSchroeder")

   ("hacking on Smyrno"
    "\"error in process filter\""
    "something is b0rken"

    "-- EdwardOConnor")

   ("treeless quiet field"
    "sudden bud: EmacsWiki"
    "now he{ar,re} the birds sing"

    "-- ttn")

   ("an emacs user's"
    "fingers dance on the keyboard;"
    "a nerd pianist"

    "-- ErikBourget")

   ("five syllables twice"
    "seven (*ELISP*) syllables here"
    "bad *EMACS* haiku"

    "-- BrianTempleton")

   ("(defun undos (buf)"
    "  \"DOS newlines are pathetic.\""
    "  (interactive \"b\")"
    "  (replace-string \"^M\" \"\n\"))"

    "-- TreyBelew")

   ("The file was open."
    "flying in a sparrow stole"
    "a parenthesis")

   ("The day went away."
    "The file still puts its weight on"
    "the tired mode-line.")

   ("On a cloudy day"
    "you hear the cons cells whisper:"
    "\"We are lost and gone.\"")

   ("A message, a string"
    "remind me of my sweet love."
    "Good bye, my buffers.")

   ("Hot night in summer:"
    "Hush, you quibbling characters!"
    "Do not wake her up!")

   ("A bright, busy day."
    "The windows watch a thousand"
    "wild cursors dancing.")

   ("Oh, why don't you are"
    "a lake, a stream, a meadow"
    "this morning, Emacs?")

   ("The friends chat gaily,"
    "I stand up to join their talk."
    "My save-excursion.")

   ("First snow, then silence."
    "This thousand dollar screen dies"
    "so beautifully.")

   ("Why the japanese"
    "think seventeen syllables"
    "are enough is a")

   ("In vain does the chicken cross the road; Emacs is on both sides. – Ludovic Brenta.")

   ("A newbie asked on the newsgroup: “What is the meaning of Emacs”?"
    "“What is the value of one parenthesis evaluating?”, answered the old masters.")

   ("An old master asked on the newsgroup: What is the best question a newbie can ask about Emacs?"
    "The newbie answered: What is Emacs?")

   ("A newbie asked on the newsgroup: Why is Emacs not using a popular extension language such as Perl?"
    "The master answered: The river sparkles and dances on the rocks. At night, it flows deep and strong.")

   ("A newbie asked on the newsgroup: What is the true nature of the lambda expression?"
    "A parenthesis holds its value, answered the old masters."))

  "A list of witty quotes for mirthful occasions!")   

