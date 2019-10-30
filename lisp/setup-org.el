;;;; Org mode

;; The following lines are always needed.
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-load-hook
          '(lambda nil
	     (define-key org-mode-map (kbd "C-c C-S-l") 'org-toggle-link-display)
             (define-key org-mode-map (kbd "<C-tab>") 'other-window)
             (define-key org-mode-map (kbd "<C-S-tab>") (lambda () (other-window -1)))
             ;; Org-cdlatex options
             (define-key org-cdlatex-mode-map (kbd "$") 'cdlatex-dollar)))

;;(add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
(setq org-directory "~/.local/share/org")
(setq org-agenda-files '("~/do.org" "~/.local/share/org/schedule.org"))
(setq org-log-done 'time)
(setq org-file-apps '((auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'" . "zathura %s")))
;; Completion
;;(add-hook 'org-mode-hook '(lambda ()
;;			   (define-key org-mode-map (kbd "C-;") 'org-complete))

;; Enable longline-truncation in org-mode buffers
(add-hook 'org-mode-hook 'toggle-truncate-lines)

;; Hide all stars except the last one on each line:
(setq org-hide-leading-stars 1)

;; Avoid invisible edits
(setq org-catch-invisible-edits 'show)
;; (setq org-catch-invisible-edits 'smart)

;;; Org-agenda mode
;; (defvar org-agenda-files nil)
;; (setq org-agenda-files (cons "~/notes/" org-agenda-files))
;; (setq org-agenda-restore-windows-after-quit 1)

;;; Org-export options
(setq org-export-with-LaTeX-fragments t)
(setq org-html-htmlize-output-type 'css)
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("IEEEtran"
                 "\\documentclass[conference]{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


(use-package ox-hugo
  :ensure t
  :after ox
  :config
  (setq org-hugo-section "blog")
  (with-eval-after-load 'org-capture

  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "posts.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template)))))

;;----------------------------------------------------------------------
;; ORG-GCAL
;;----------------------------------------------------------------------
(use-package org-gcal
  :ensure t
  :commands (org-gcal-sync org-gcal-fetch)
  :init
  (setq org-gcal-client-id my-org-gcal-client-id
        org-gcal-client-secret my-org-gcal-client-secret
        org-gcal-file-alist `((,my-email-address . ,(concat
                                                    (file-name-as-directory org-directory)
                                                    "schedule.org"))))
  )


;;----------------------------------------------------------------------
;; ORG-REVEAL
;;----------------------------------------------------------------------
(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root "file:///home/karthik/.local/share/git/reveal.js")
  (setq org-reveal-hlevel 2))

;; Some formatting
;; (setq org-blank-before-new-entry
;;       '((heading . t) (plain-list-item . nil)))

;;; Org LaTeX options
;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;;; Org-Remember: Org-mode with Remember-mode

;; (org-remember-insinuate)
;; (setq org-directory "~/doodles")
;; (setq org-default-notes-file
;;       (expand-file-name (concat org-directory "tasks.org")))
;; ;; (setq org-remember-default-headline "stuff")

;; ;;Templates for org-remember:
;; (setq org-remember-templates
;;       (quote (("Journal" ?j
;;                "* %^{Title}\n  %U\n  %?\n  %i\n"
;;                "journal.org" top
;;                )
;;               ("Notes" ?n
;;                "* %?\n  "
;;                "tasks.org" bottom
;;                ))))

;; (defun make-remember-frame ()
;;   "Turn the current frame into a small popup frame for remember mode;
;; this is meant to be called with
;;      emacsclient -c -e '(make-remember-frame)'"
;;   (modify-frame-parameters nil
;;     '( (name . "*Remember*") ;; must be same as in mode-hook below
;;        (width .  80)
;;        (height . 14)
;;        (vertical-scroll-bars . nil)
;;        (menu-bar-lines . nil)
;;        (tool-bar-lines . nil)))
;;   (org-remember)
;;   (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only....
;;   (delete-other-windows))

;; when we're in such a remember-frame, close it when done.
;; (add-hook 'org-remember-mode-hook
;;   (lambda()
;;     (define-key org-remember-mode-map (kbd "C-c C-c")
;;       '(lambda()(interactive)
;;          (let ((remember-frame-p
;;                  (string= (frame-parameter nil 'name) "*Remember*")))
;;            (when remember-frame-p (make-frame-invisible))  ;; hide quickly
;;            (org-remember-finalize)
;;            (when remember-frame-p (delete-frame)))))
;;     (define-key org-remember-mode-map (kbd "C-c C-k")
;;       '(lambda() (interactive)
;;          (let ((remember-frame-p
;;                  (string= (frame-parameter nil 'name) "*Remember*")))
;;            (when remember-frame-p (make-frame-invisible))  ;; hide quickly
;;            (org-kill-note-or-show-branches)
;;            (when remember-frame-p (delete-frame)))))
;;       ))

;; (define-key org-mode-map (kbd "C-c r") nil)

(add-hook 'org-mode-hook #'+org-prettify-symbols)

;;;###autoload
(defun +org-prettify-symbols ()
  "Set `prettify-symbols-alist' to display LaTeX code as pretty symbols in org-mode."
  (interactive)
  (setq prettify-symbols-alist '(("\\alpha" . 945)
                                 ("\\beta" . 946)
                                 ("\\gamma" . 947)
                                 ("\\delta" . 948)
                                 ("\\epsilon" . 1013)
                                 ("\\zeta" . 950)
                                 ("\\eta" . 951)
                                 ("\\theta" . 952)
                                 ("\\iota" . 953)
                                 ("\\kappa" . 954)
                                 ("\\lambda" . 955)
                                 ("\\mu" . 956)
                                 ("\\nu" . 957)
                                 ("\\xi" . 958)
                                 ("\\pi" . 960)
                                 ("\\rho" . 961)
                                 ("\\sigma" . 963)
                                 ("\\tau" . 964)
                                 ("\\upsilon" . 965)
                                 ("\\phi" . 981)
                                 ("\\chi" . 967)
                                 ("\\psi" . 968)
                                 ("\\omega" . 969)
                                 ("\\Gamma" . 915)
                                 ("\\Delta" . 916)
                                 ("\\Lambda" . 923)
                                 ("\\Phi" . 934)
                                 ("\\Pi" . 928)
                                 ("\\Psi" . 936)
                                 ("\\Sigma" . 931)
                                 ("\\Theta" . 920)
                                 ("\\Upsilon" . 933)
                                 ("\\Xi" . 926)
                                 ("\\Omega" . 937)
                                 ("\\Box" . 9633)
                                 ("\\Bumpeq" . 8782)
                                 ("\\Cap" . 8914)
                                 ("\\Cup" . 8915)
                                 ("\\Diamond" . 9671)
                                 ("\\Downarrow" . 8659)
                                 ("\\H{o}" . 337)
                                 ("\\Im" . 8465)
                                 ("\\Join" . 8904)
                                 ("\\Leftarrow" . 8656)
                                 ("\\Leftrightarrow" . 8660)
                                 ("\\Ll" . 8920)
                                 ("\\Lleftarrow" . 8666)
                                 ("\\Longleftarrow" . 8656)
                                 ("\\Longleftrightarrow" . 8660)
                                 ("\\Longrightarrow" . 8658)
                                 ("\\Lsh" . 8624)
                                 ("\\Re" . 8476)
                                 ("\\Rightarrow" . 8658)
                                 ("\\Rrightarrow" . 8667)
                                 ("\\Rsh" . 8625)
                                 ("\\Subset" . 8912)
                                 ("\\Supset" . 8913)
                                 ("\\Uparrow" . 8657)
                                 ("\\Updownarrow" . 8661)
                                 ("\\Vdash" . 8873)
                                 ("\\Vert" . 8214)
                                 ("\\Vvdash" . 8874)
                                 ("\\aleph" . 8501)
                                 ("\\amalg" . 8720)
                                 ("\\angle" . 8736)
                                 ("\\approx" . 8776)
                                 ("\\approxeq" . 8778)
                                 ("\\ast" . 8727)
                                 ("\\asymp" . 8781)
                                 ("\\backcong" . 8780)
                                 ("\\backepsilon" . 8717)
                                 ("\\backprime" . 8245)
                                 ("\\backsim" . 8765)
                                 ("\\backsimeq" . 8909)
                                 ("\\backslash" . 92)
                                 ("\\barwedge" . 8892)
                                 ("\\because" . 8757)
                                 ("\\beth" . 8502)
                                 ("\\between" . 8812)
                                 ("\\bigcap" . 8898)
                                 ("\\bigcirc" . 9711)
                                 ("\\bigcup" . 8899)
                                 ("\\bigstar" . 9733)
                                 ("\\bigtriangledown" . 9661)
                                 ("\\bigtriangleup" . 9651)
                                 ("\\bigvee" . 8897)
                                 ("\\bigwedge" . 8896)
                                 ("\\blacklozenge" . 10022)
                                 ("\\blacksquare" . 9642)
                                 ("\\blacktriangle" . 9652)
                                 ("\\blacktriangledown" . 9662)
                                 ("\\blacktriangleleft" . 9666)
                                 ("\\blacktriangleright" . 9656)
                                 ("\\bot" . 8869)
                                 ("\\bowtie" . 8904)
                                 ("\\boxminus" . 8863)
                                 ("\\boxplus" . 8862)
                                 ("\\boxtimes" . 8864)
                                 ("\\bullet" . 8226)
                                 ("\\bumpeq" . 8783)
                                 ("\\cap" . 8745)
                                 ("\\cdots" . 8943)
                                 ("\\centerdot" . 183)
                                 ("\\checkmark" . 10003)
                                 ("\\chi" . 967)
                                 ("\\cdot" . 8901)
                                 ("\\cdots" . 8943)
                                 ("\\circ" . 8728)
                                 ("\\circeq" . 8791)
                                 ("\\circlearrowleft" . 8634)
                                 ("\\circlearrowright" . 8635)
                                 ("\\circledR" . 174)
                                 ("\\circledS" . 9416)
                                 ("\\circledast" . 8859)
                                 ("\\circledcirc" . 8858)
                                 ("\\circleddash" . 8861)
                                 ("\\clubsuit" . 9827)
                                 ("\\coloneq" . 8788)
                                 ("\\complement" . 8705)
                                 ("\\cong" . 8773)
                                 ("\\coprod" . 8720)
                                 ("\\cup" . 8746)
                                 ("\\curlyeqprec" . 8926)
                                 ("\\curlyeqsucc" . 8927)
                                 ("\\curlypreceq" . 8828)
                                 ("\\curlyvee" . 8910)
                                 ("\\curlywedge" . 8911)
                                 ("\\curvearrowleft" . 8630)
                                 ("\\curvearrowright" . 8631)
                                 ("\\dag" . 8224)
                                 ("\\dagger" . 8224)
                                 ("\\daleth" . 8504)
                                 ("\\dashv" . 8867)
                                 ("\\ddag" . 8225)
                                 ("\\ddagger" . 8225)
                                 ("\\ddots" . 8945)
                                 ("\\diamond" . 8900)
                                 ("\\diamondsuit" . 9826)
                                 ("\\divideontimes" . 8903)
                                 ("\\doteq" . 8784)
                                 ("\\doteqdot" . 8785)
                                 ("\\dotplus" . 8724)
                                 ("\\dotsquare" . 8865)
                                 ("\\downarrow" . 8595)
                                 ("\\downdownarrows" . 8650)
                                 ("\\downleftharpoon" . 8643)
                                 ("\\downrightharpoon" . 8642)
                                 ("\\ell" . 8467)
                                 ("\\emptyset" . 8709)
                                 ("\\eqcirc" . 8790)
                                 ("\\eqcolon" . 8789)
                                 ("\\eqslantgtr" . 8925)
                                 ("\\eqslantless" . 8924)
                                 ("\\equiv" . 8801)
                                 ("\\exists" . 8707)
                                 ("\\fallingdotseq" . 8786)
                                 ("\\flat" . 9837)
                                 ("\\forall" . 8704)
                                 ("\\frown" . 8994)
                                 ("\\ge" . 8805)
                                 ("\\geq" . 8805)
                                 ("\\geqq" . 8807)
                                 ("\\geqslant" . 8805)
                                 ("\\gets" . 8592)
                                 ("\\gg" . 8811)
                                 ("\\ggg" . 8921)
                                 ("\\gimel" . 8503)
                                 ("\\gnapprox" . 8935)
                                 ("\\gneq" . 8809)
                                 ("\\gneqq" . 8809)
                                 ("\\gnsim" . 8935)
                                 ("\\gtrapprox" . 8819)
                                 ("\\gtrdot" . 8919)
                                 ("\\gtreqless" . 8923)
                                 ("\\gtreqqless" . 8923)
                                 ("\\gtrless" . 8823)
                                 ("\\gtrsim" . 8819)
                                 ("\\gvertneqq" . 8809)
                                 ("\\hbar" . 8463)
                                 ("\\heartsuit" . 9829)
                                 ("\\hookleftarrow" . 8617)
                                 ("\\hookrightarrow" . 8618)
                                 ("\\iff" . 8660)
                                 ("\\imath" . 305)
                                 ("\\in" . 8712)
                                 ("\\infty" . 8734)
                                 ("\\int" . 8747)
                                 ("\\intercal" . 8890)
                                 ("\\langle" . 10216)
                                 ("\\lbrace" . 123)
                                 ("\\lbrack" . 91)
                                 ("\\lceil" . 8968)
                                 ("\\ldots" . 8230)
                                 ("\\le" . 8804)
                                 ("\\leadsto" . 8605)
                                 ("\\leftarrow" . 8592)
                                 ("\\leftarrowtail" . 8610)
                                 ("\\leftharpoondown" . 8637)
                                 ("\\leftharpoonup" . 8636)
                                 ("\\leftleftarrows" . 8647)
                                 ("\\leftrightarrow" . 8596)
                                 ("\\leftrightarrows" . 8646)
                                 ("\\leftrightharpoons" . 8651)
                                 ("\\leftrightsquigarrow" . 8621)
                                 ("\\leftthreetimes" . 8907)
                                 ("\\leq" . 8804)
                                 ("\\leqq" . 8806)
                                 ("\\leqslant" . 8804)
                                 ("\\lessapprox" . 8818)
                                 ("\\lessdot" . 8918)
                                 ("\\lesseqgtr" . 8922)
                                 ("\\lesseqqgtr" . 8922)
                                 ("\\lessgtr" . 8822)
                                 ("\\lesssim" . 8818)
                                 ("\\lfloor" . 8970)
                                 ("\\lhd" . 9665)
                                 ("\\rhd" . 9655)
                                 ("\\ll" . 8810)
                                 ("\\llcorner" . 8990)
                                 ("\\lnapprox" . 8934)
                                 ("\\lneq" . 8808)
                                 ("\\lneqq" . 8808)
                                 ("\\lnsim" . 8934)
                                 ("\\longleftarrow" . 8592)
                                 ("\\longleftrightarrow" . 8596)
                                 ("\\longmapsto" . 8614)
                                 ("\\longrightarrow" . 8594)
                                 ("\\looparrowleft" . 8619)
                                 ("\\looparrowright" . 8620)
                                 ("\\lozenge" . 10023)
                                 ("\\lq" . 8216)
                                 ("\\lrcorner" . 8991)
                                 ("\\ltimes" . 8905)
                                 ("\\lvertneqq" . 8808)
                                 ("\\maltese" . 10016)
                                 ("\\mapsto" . 8614)
                                 ("\\measuredangle" . 8737)
                                 ("\\mho" . 8487)
                                 ("\\mid" . 8739)
                                 ("\\models" . 8871)
                                 ("\\mp" . 8723)
                                 ("\\multimap" . 8888)
                                 ("\\nLeftarrow" . 8653)
                                 ("\\nLeftrightarrow" . 8654)
                                 ("\\nRightarrow" . 8655)
                                 ("\\nVDash" . 8879)
                                 ("\\nVdash" . 8878)
                                 ("\\nabla" . 8711)
                                 ("\\napprox" . 8777)
                                 ("\\natural" . 9838)
                                 ("\\ncong" . 8775)
                                 ("\\ne" . 8800)
                                 ("\\nearrow" . 8599)
                                 ("\\neg" . 172)
                                 ("\\neq" . 8800)
                                 ("\\nequiv" . 8802)
                                 ("\\newline" . 8232)
                                 ("\\nexists" . 8708)
                                 ("\\ngeq" . 8817)
                                 ("\\ngeqq" . 8817)
                                 ("\\ngeqslant" . 8817)
                                 ("\\ngtr" . 8815)
                                 ("\\ni" . 8715)
                                 ("\\nleftarrow" . 8602)
                                 ("\\nleftrightarrow" . 8622)
                                 ("\\nleq" . 8816)
                                 ("\\nleqq" . 8816)
                                 ("\\nleqslant" . 8816)
                                 ("\\nless" . 8814)
                                 ("\\nmid" . 8740)
                                 ("\\notin" . 8713)
                                 ("\\nparallel" . 8742)
                                 ("\\nprec" . 8832)
                                 ("\\npreceq" . 8928)
                                 ("\\nrightarrow" . 8603)
                                 ("\\nshortmid" . 8740)
                                 ("\\nshortparallel" . 8742)
                                 ("\\nsim" . 8769)
                                 ("\\nsimeq" . 8772)
                                 ("\\nsubset" . 8836)
                                 ("\\nsubseteq" . 8840)
                                 ("\\nsubseteqq" . 8840)
                                 ("\\nsucc" . 8833)
                                 ("\\nsucceq" . 8929)
                                 ("\\nsupset" . 8837)
                                 ("\\nsupseteq" . 8841)
                                 ("\\nsupseteqq" . 8841)
                                 ("\\ntriangleleft" . 8938)
                                 ("\\ntrianglelefteq" . 8940)
                                 ("\\ntriangleright" . 8939)
                                 ("\\ntrianglerighteq" . 8941)
                                 ("\\nvDash" . 8877)
                                 ("\\nvdash" . 8876)
                                 ("\\nwarrow" . 8598)
                                 ("\\odot" . 8857)
                                 ("\\oint" . 8750)
                                 ("\\ominus" . 8854)
                                 ("\\oplus" . 8853)
                                 ("\\oslash" . 8856)
                                 ("\\otimes" . 8855)
                                 ("\\par" . 8233)
                                 ("\\parallel" . 8741)
                                 ("\\partial" . 8706)
                                 ("\\perp" . 8869)
                                 ("\\pitchfork" . 8916)
                                 ("\\prec" . 8826)
                                 ("\\precapprox" . 8830)
                                 ("\\preceq" . 8828)
                                 ("\\precnapprox" . 8936)
                                 ("\\precnsim" . 8936)
                                 ("\\precsim" . 8830)
                                 ("\\prime" . 8242)
                                 ("\\prod" . 8719)
                                 ("\\propto" . 8733)
                                 ("\\qed" . 8718)
                                 ("\\qquad" . 10722)
                                 ("\\quad" . 9251)
                                 ("\\rangle" . 10217)
                                 ("\\rbrace" . 125)
                                 ("\\rbrack" . 93)
                                 ("\\rceil" . 8969)
                                 ("\\rfloor" . 8971)
                                 ("\\rightarrow" . 8594)
                                 ("\\rightarrowtail" . 8611)
                                 ("\\rightharpoondown" . 8641)
                                 ("\\rightharpoonup" . 8640)
                                 ("\\rightleftarrows" . 8644)
                                 ("\\rightleftharpoons" . 8652)
                                 ("\\rightrightarrows" . 8649)
                                 ("\\rightthreetimes" . 8908)
                                 ("\\risingdotseq" . 8787)
                                 ("\\rtimes" . 8906)
                                 ("\\times" . 215)
                                 ("\\sbs" . 65128)
                                 ("\\searrow" . 8600)
                                 ("\\setminus" . 8726)
                                 ("\\sharp" . 9839)
                                 ("\\shortmid" . 8739)
                                 ("\\shortparallel" . 8741)
                                 ("\\sim" . 8764)
                                 ("\\simeq" . 8771)
                                 ("\\smallamalg" . 8720)
                                 ("\\smallsetminus" . 8726)
                                 ("\\smallsmile" . 8995)
                                 ("\\smile" . 8995)
                                 ("\\spadesuit" . 9824)
                                 ("\\sphericalangle" . 8738)
                                 ("\\sqcap" . 8851)
                                 ("\\sqcup" . 8852)
                                 ("\\sqsubset" . 8847)
                                 ("\\sqsubseteq" . 8849)
                                 ("\\sqsupset" . 8848)
                                 ("\\sqsupseteq" . 8850)
                                 ("\\square" . 9633)
                                 ("\\squigarrowright" . 8669)
                                 ("\\star" . 8902)
                                 ("\\straightphi" . 966)
                                 ("\\subset" . 8834)
                                 ("\\subseteq" . 8838)
                                 ("\\subseteqq" . 8838)
                                 ("\\subsetneq" . 8842)
                                 ("\\subsetneqq" . 8842)
                                 ("\\succ" . 8827)
                                 ("\\succapprox" . 8831)
                                 ("\\succcurlyeq" . 8829)
                                 ("\\succeq" . 8829)
                                 ("\\succnapprox" . 8937)
                                 ("\\succnsim" . 8937)
                                 ("\\succsim" . 8831)
                                 ("\\sum" . 8721)
                                 ("\\supset" . 8835)
                                 ("\\supseteq" . 8839)
                                 ("\\supseteqq" . 8839)
                                 ("\\supsetneq" . 8843)
                                 ("\\supsetneqq" . 8843)
                                 ("\\surd" . 8730)
                                 ("\\swarrow" . 8601)
                                 ("\\therefore" . 8756)
                                 ("\\thickapprox" . 8776)
                                 ("\\thicksim" . 8764)
                                 ("\\to" . 8594)
                                 ("\\top" . 8868)
                                 ("\\triangle" . 9653)
                                 ("\\triangledown" . 9663)
                                 ("\\triangleleft" . 9667)
                                 ("\\trianglelefteq" . 8884)
                                 ("\\triangleq" . 8796)
                                 ("\\triangleright" . 9657)
                                 ("\\trianglerighteq" . 8885)
                                 ("\\twoheadleftarrow" . 8606)
                                 ("\\twoheadrightarrow" . 8608)
                                 ("\\ulcorner" . 8988)
                                 ("\\uparrow" . 8593)
                                 ("\\updownarrow" . 8597)
                                 ("\\upleftharpoon" . 8639)
                                 ("\\uplus" . 8846)
                                 ("\\uprightharpoon" . 8638)
                                 ("\\upuparrows" . 8648)
                                 ("\\urcorner" . 8989)
                                 ("\\u{i}" . 301)
                                 ("\\vDash" . 8872)
                                 ("\\varepsilon" . 949)
                                 ("\\varphi" . 966)
                                 ("\\varprime" . 8242)
                                 ("\\varpropto" . 8733)
                                 ("\\varrho" . 1009)
                                 ("\\varsigma" 962)
                                 ("\\vartriangleleft" . 8882)
                                 ("\\vartriangleright" . 8883)
                                 ("\\vdash" . 8866)
                                 ("\\vdots" . 8942)
                                 ("\\vee" . 8744)
                                 ("\\veebar" . 8891)
                                 ("\\vert" . 124)
                                 ("\\wedge" . 8743)
                                 ("\\wp" . 8472)
                                 ("\\wr" . 8768)
                                 ("\\Bbb{N}" . 8469)
                                 ("\\Bbb{P}" . 8473)
                                 ("\\Bbb{Q}" . 8474)
                                 ("\\Bbb{R}" . 8477)
                                 ("\\Bbb{Z}" . 8484)
                                 ("--" . 8211)
                                 ("---" . 8212)
                                 ("\\ordfeminine" . 170)
                                 ("\\ordmasculine" . 186)
                                 ("\\lambdabar" . 411)
                                 ("\\celsius" . 8451)
                                 ("\\textmu" . 181)
                                 ("\\textfractionsolidus" . 8260)
                                 ("\\textbigcircle" . 8413)
                                 ("\\textmusicalnote" . 9834)
                                 ("\\textdied" . 10013)
                                 ("\\textcolonmonetary" . 8353)
                                 ("\\textwon" . 8361)
                                 ("\\textnaira" . 8358)
                                 ("\\textpeso" . 8369)
                                 ("\\textlira" . 8356)
                                 ("\\textrecipe" . 8478)
                                 ("\\textinterrobang" . 8253)
                                 ("\\textpertenthousand" . 8241)
                                 ("\\textbaht" . 3647)
                                 ("\\textnumero" . 8470)
                                 ("\\textdiscount" . 8274)
                                 ("\\textestimated" . 8494)
                                 ("\\textopenbullet" . 9702)
                                 ("\\textlquill" . 8261)
                                 ("\\textrquill" . 8262)
                                 ("\\textcircledP" . 8471)
                                 ("\\textreferencemark" . 8251)))
(prettify-symbols-mode +1))




(provide 'setup-org)


