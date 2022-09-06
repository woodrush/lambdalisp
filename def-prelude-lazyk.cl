(def-lazy *cons2 (lambda (x y z) (cons x (cons y z))))
(def-lazy *sym2 (lambda (a b) (cons t (cons t (cons nil (cons t (do (a) (b) nil)))))))
(def-lazy *char3 (lambda (a b c) (cons t (cons nil (do (a) (b) (c) nil)))))
(def-lazy "*11" (*cons2 nil nil))
(def-lazy "*10" (*cons2 nil t))
(def-lazy "*01" (*cons2 t nil))
(def-lazy "*00" (*cons2 t t))


(def-lazy "*<" (do ("*00") ("*11") ("*11") ("*00") nil)  )
(def-lazy "*>" (do ("*00") ("*11") ("*11") ("*10") nil)  )
(def-lazy "*=" (do ("*00") ("*11") ("*11") ("*01") nil)  )
(def-lazy "*%" (*sym2 ("*01") ("*01"))         )
(def-lazy "*/" (*sym2 ("*11") ("*11"))         )
(def-lazy "**" (*sym2 ("*10") ("*10"))         )
(def-lazy "*+" (*sym2 ("*10") ("*11"))         )
(def-lazy "*-" (*sym2 ("*11") ("*01"))         )
(def-lazy "*&" (do ("*00") ("*10") ("*01") ("*10") nil)  )
(def-lazy "*y" (*char3 ("*11") ("*10") ("*01")) )
(def-lazy "*h" (*char3 ("*10") ("*10") ("*00")) )
(def-lazy "*k" (*char3 ("*10") ("*10") ("*11")) )
(def-lazy "*v" (*char3 ("*11") ("*01") ("*10")) )
(def-lazy "*f" (*char3 ("*10") ("*01") ("*10")) )
(def-lazy "*b" (*char3 ("*10") ("*00") ("*10")) )
(def-lazy "*g" (*char3 ("*10") ("*01") ("*11")) )
(def-lazy "*l" (*char3 ("*10") ("*11") ("*00")) )
(def-lazy "*m" (*char3 ("*10") ("*11") ("*01")) )
(def-lazy "*p" (*char3 ("*11") ("*00") ("*00")) )
(def-lazy "*s" (*char3 ("*11") ("*00") ("*11")) )
(def-lazy "*u" (*char3 ("*11") ("*01") ("*01")) )
(def-lazy "*c" (*char3 ("*10") ("*00") ("*11")) )
(def-lazy "*i" (*char3 ("*10") ("*10") ("*01")) )
(def-lazy "*q" (*char3 ("*11") ("*00") ("*01")) )
(def-lazy "*a" (*char3 ("*10") ("*00") ("*01")) )
(def-lazy "*d" (*char3 ("*10") ("*01") ("*00")) )
(def-lazy "*e" (*char3 ("*10") ("*01") ("*01")) )
(def-lazy "*n" (*char3 ("*10") ("*11") ("*10")) )
(def-lazy "*o" (*char3 ("*10") ("*11") ("*11")) )
(def-lazy "*r" (*char3 ("*11") ("*00") ("*10")) )
(def-lazy "*t" (*char3 ("*11") ("*01") ("*00")) )
(def-lazy "*#" (do ("*00") ("*10") ("*00") ("*11") nil)  )
(def-lazy "*@" (do ("*01") ("*00") ("*00") ("*00") nil)  )
(def-lazy "*," (*sym2 ("*11") ("*00"))         )
(def-lazy "*`" (*char3 ("*10") ("*00") ("*00")) )
(def-lazy "*'" (*sym2 ("*01") ("*11"))         )
(def-lazy "*\\n" (do ("*00") ("*00") ("*10") ("*10") nil)  )
(def-lazy "*tilde" (do ("*01") ("*11") ("*11") ("*10") nil)  )
(def-lazy "*w" (*char3 ("*11") ("*01") ("*11")) )
(def-lazy "*\"" (*sym2 ("*00") ("*10"))         )
(def-lazy "*0" (do ("*00") ("*11") ("*00") ("*00") nil)  )
(def-lazy "*1" (do ("*00") ("*11") ("*00") ("*01") nil)  )
(def-lazy "* " (*sym2 ("*00") ("*00"))         )
(def-lazy "*(" (*sym2 ("*10") ("*00"))         )
(def-lazy "*)" (*sym2 ("*10") ("*01"))         )

(def-lazy **prelude**
  ((string-concatenator stdin) "*)" "*)" "*)" "*r" "*t" "*s" "*t" "*e" "*r" "*)" "*l" "*i" "*n" "*)" "*t" "* " "*r" "*t" "*s" "*t" "*e" "*r" "* " "*t" "*n" "*i" "*r" "*p" "*(" "* " "*n" "*g" "*o" "*r" "*p" "*(" "* " "*n" "*o" "*i" "*t" "*p" "*o" "* " "*f" "*i" "*(" "*)" "*)" "*)" "*t" "*e" "*r" "* " "*r" "*d" "*c" "*(" "* " "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*r" "*t" "*s" "*t" "*e" "*r" "*)" "*t" "*e" "*r" "* " "*r" "*a" "*c" "*(" "* " "*+" "*(" "* " "*r" "*t" "*s" "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*m" "*o" "*r" "*f" "*-" "*n" "*r" "*u" "*t" "*e" "*r" "*(" "*)" "*)" "*(" "* " "*t" "*e" "*r" "* " "*q" "*e" "*(" "* " "*f" "*i" "*(" "* " "*p" "*o" "*o" "*l" "*(" "*)" "*\"" "*\"" "* " "*r" "*t" "*s" "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*)" "*r" "*t" "*s" "* " "*r" "*t" "*s" "*r" "*d" "*c" "*(" "* " "*r" "*t" "*s" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*)" "*)" "*t" "*e" "*r" "*)" "*r" "*t" "*s" "* " "*r" "*t" "*s" "*r" "*a" "*c" "*(" "* " "*s" "*n" "*o" "*c" "*(" "* " "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "* " "*t" "*(" "*)" "*)" "*)" "*)" "*)" "*t" "*e" "*r" "*)" "*m" "*e" "*t" "*i" "* " "*r" "*t" "*s" "*(" "* " "*s" "*n" "*o" "*c" "*(" "* " "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*l" "*i" "*n" "* " "*m" "*e" "*t" "*i" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*)" "*s" "*g" "*r" "*a" "* " "*r" "*d" "*c" "*(" "* " "*s" "*g" "*r" "*a" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*s" "*g" "*r" "*a" "* " "*r" "*a" "*c" "*(" "* " "*m" "*e" "*t" "*i" "* " "*q" "*t" "*e" "*s" "*(" "* " "*n" "*g" "*o" "*r" "*p" "*(" "* " "*s" "*g" "*r" "*a" "* " "*f" "*i" "*(" "*)" "*\"" "*a" "*\"" "*)" "*r" "*t" "*s" "* " "*r" "*t" "*s" "*r" "*a" "*c" "*(" "* " "*q" "*e" "*(" "*(" "*)" "*)" "*)" "*t" "*e" "*r" "* " "*e" "*n" "*i" "*l" "*w" "*e" "*n" "* " "*s" "*n" "*o" "*c" "*(" "* " "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*\"" "*%" "*\"" "*)" "*r" "*t" "*s" "* " "*r" "*t" "*s" "*r" "*a" "*c" "*(" "* " "*q" "*e" "*(" "*(" "* " "*d" "*n" "*o" "*c" "*(" "*)" "*)" "*r" "*t" "*s" "* " "*r" "*t" "*s" "*r" "*d" "*c" "*(" "* " "*r" "*t" "*s" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*\"" "*tilde" "*\"" "*)" "*r" "*t" "*s" "* " "*r" "*t" "*s" "*r" "*a" "*c" "*(" "* " "*q" "*e" "*(" "*(" "*)" "*)" "*m" "*o" "*r" "*f" "*-" "*n" "*r" "*u" "*t" "*e" "*r" "*(" "*)" "*\"" "*\"" "* " "*r" "*t" "*s" "* " "*q" "*e" "*(" "*(" "* " "*d" "*n" "*o" "*c" "*(" "* " "*p" "*o" "*o" "*l" "*(" "*)" "*\"" "*\\n" "*\"" "* " "*e" "*n" "*i" "*l" "*w" "*e" "*n" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*(" "* " "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*s" "*g" "*r" "*a" "* " "*t" "*s" "*e" "*r" "*&" "* " "*r" "*t" "*s" "* " "*n" "*o" "*i" "*t" "*p" "*o" "*(" "* " "*t" "*a" "*m" "*r" "*o" "*f" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*p" "**" "* " "*r" "*t" "*s" "*(" "*)" "*p" "**" "*(" "* " "*g" "*n" "*i" "*r" "*t" "*s" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*r" "*e" "*d" "*a" "*e" "*r" "*-" "*p" "*r" "*a" "*h" "*s" "* " "*\"" "*#" "*\"" "* " "*r" "*e" "*t" "*c" "*a" "*r" "*a" "*h" "*c" "*-" "*o" "*r" "*c" "*a" "*m" "*-" "*t" "*e" "*s" "*(" "*)" "*)" "*)" "*d" "*a" "*e" "*r" "*(" "*)" "*)" "*d" "*a" "*e" "*r" "*(" "*)" "*r" "*a" "*h" "*c" "*-" "*d" "*a" "*e" "*r" "*(" "* " "*n" "*g" "*o" "*r" "*p" "*(" "*)" "*)" "*r" "*a" "*h" "*c" "*-" "*k" "*e" "*e" "*p" "*(" "* " "*\"" "*'" "*\"" "* " "*q" "*e" "*(" "* " "*f" "*i" "*(" "*)" "*r" "*a" "*h" "*c" "*(" "* " "*r" "*e" "*d" "*a" "*e" "*r" "*-" "*p" "*r" "*a" "*h" "*s" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*l" "*," "* " "*f" "*," "* " "*s" "*n" "*o" "*c" "*(" "* " "*l" "*a" "*v" "*e" "*(" "*`" "*)" "*l" "* " "*f" "*(" "* " "*e" "*c" "*u" "*d" "*e" "*r" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*)" "*l" "* " "*r" "*d" "*c" "*(" "* " "*l" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*t" "*e" "*r" "*)" "*l" "* " "*r" "*a" "*c" "*(" "* " "*s" "*n" "*o" "*c" "*(" "* " "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*l" "*i" "*n" "*)" "*t" "*e" "*r" "* " "*n" "*r" "*u" "*t" "*e" "*r" "*(" "*)" "*l" "* " "*m" "*o" "*t" "*a" "*(" "* " "*f" "*i" "*(" "* " "*p" "*o" "*o" "*l" "*(" "*)" "*)" "*(" "* " "*t" "*e" "*r" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*l" "*(" "* " "*e" "*s" "*r" "*e" "*v" "*e" "*r" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*l" "*i" "*n" "*)" "*)" "*)" "*p" "**" "* " "*r" "*d" "*c" "*(" "* " "*f" "* " "*r" "*a" "*c" "*p" "*a" "*m" "*(" "*)" "*)" "*p" "**" "* " "*r" "*a" "*c" "*(" "* " "*f" "*(" "* " "*s" "*n" "*o" "*c" "*(" "* " "*p" "**" "* " "*f" "*i" "*(" "*)" "*p" "**" "* " "*f" "*(" "* " "*r" "*a" "*c" "*p" "*a" "*m" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*y" "*e" "*k" "* " "*t" "*e" "*g" "*'" "* " "*e" "*l" "*b" "*a" "*t" "*h" "*s" "*a" "*h" "*(" "*)" "*e" "*l" "*b" "*a" "*t" "*h" "*s" "*a" "*h" "* " "*y" "*e" "*k" "*(" "* " "*h" "*s" "*a" "*h" "*t" "*e" "*g" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*e" "*u" "*l" "*a" "*v" "*," "*)" "*)" "*e" "*c" "*a" "*l" "*p" "* " "*r" "*d" "*c" "*(" "* " "*r" "*a" "*c" "*(" "*," "* " "*t" "*e" "*s" "*'" "*)" "*)" "*)" "*e" "*c" "*a" "*l" "*p" "* " "*r" "*d" "*c" "*(" "* " "*r" "*d" "*c" "*(" "* " "*r" "*a" "*c" "*(" "*," "*(" "*`" "*)" "*e" "*u" "*l" "*a" "*v" "*," "* " "*e" "*c" "*a" "*l" "*p" "*," "* " "*q" "*t" "*e" "*s" "*(" "*`" "*)" "*e" "*c" "*a" "*l" "*p" "* " "*m" "*o" "*t" "*a" "*(" "* " "*f" "*i" "*(" "*)" "*e" "*u" "*l" "*a" "*v" "* " "*e" "*c" "*a" "*l" "*p" "*(" "* " "*f" "*t" "*e" "*s" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "**" "*e" "*l" "*b" "*a" "*t" "*-" "*h" "*s" "*a" "*h" "*-" "*e" "*k" "*a" "*m" "*(" "*)" "*p" "**" "* " "*t" "*s" "*e" "*r" "*&" "*(" "* " "*e" "*l" "*b" "*a" "*t" "*-" "*h" "*s" "*a" "*h" "*-" "*e" "*k" "*a" "*m" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*)" "*)" "*l" "*i" "*n" "*)" "*)" "*e" "*u" "*l" "*a" "*v" "* " "*r" "*a" "*c" "*(" "* " "*y" "*e" "*k" "* " "*r" "*e" "*t" "*t" "*e" "*s" "*(" "*)" "*t" "*e" "*s" "*'" "* " "*e" "*d" "*o" "*m" "* " "*q" "*e" "*(" "* " "*f" "*i" "*(" "*)" "*y" "*e" "*k" "* " "*r" "*e" "*t" "*t" "*e" "*g" "*(" "*)" "*t" "*e" "*g" "*'" "* " "*e" "*d" "*o" "*m" "* " "*q" "*e" "*(" "* " "*f" "*i" "*(" "*)" "*e" "*u" "*l" "*a" "*v" "* " "*t" "*s" "*e" "*r" "*&" "* " "*y" "*e" "*k" "* " "*e" "*d" "*o" "*m" "*(" "* " "*a" "*d" "*b" "*m" "*a" "*l" "*(" "*)" "*)" "*)" "*e" "*l" "*b" "*a" "*t" "*h" "*s" "*a" "*h" "*)" "*e" "*u" "*l" "*a" "*v" "* " "*y" "*e" "*k" "* " "*s" "*n" "*o" "*c" "*(" "* " "*s" "*n" "*o" "*c" "*(" "* " "*e" "*l" "*b" "*a" "*t" "*h" "*s" "*a" "*h" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*e" "*u" "*l" "*a" "*v" "* " "*y" "*e" "*k" "*(" "* " "*r" "*e" "*t" "*t" "*e" "*s" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*)" "*t" "*s" "*i" "*l" "*h" "*s" "*a" "*h" "* " "*r" "*d" "*c" "*(" "* " "*t" "*s" "*i" "*l" "*h" "*s" "*a" "*h" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*)" "*)" "*t" "*s" "*i" "*l" "*h" "*s" "*a" "*h" "* " "*r" "*a" "*c" "*(" "* " "*r" "*d" "*c" "*(" "* " "*r" "*e" "*t" "*t" "*e" "*g" "* " "*m" "*o" "*r" "*f" "*-" "*n" "*r" "*u" "*t" "*e" "*r" "*(" "*)" "*)" "*)" "*t" "*s" "*i" "*l" "*h" "*s" "*a" "*h" "* " "*r" "*a" "*c" "*(" "* " "*r" "*a" "*c" "*(" "* " "*y" "*e" "*k" "* " "*q" "*e" "*(" "* " "*f" "*i" "*(" "*)" "*)" "*l" "*i" "*n" "* " "*r" "*e" "*t" "*t" "*e" "*g" "* " "*m" "*o" "*r" "*f" "*-" "*n" "*r" "*u" "*t" "*e" "*r" "*(" "*)" "*t" "*s" "*i" "*l" "*h" "*s" "*a" "*h" "* " "*m" "*o" "*t" "*a" "*(" "* " "*f" "*i" "*(" "* " "*p" "*o" "*o" "*l" "*(" "*)" "*e" "*l" "*b" "*a" "*t" "*h" "*s" "*a" "*h" "* " "*t" "*s" "*i" "*l" "*h" "*s" "*a" "*h" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*y" "*e" "*k" "*(" "* " "*r" "*e" "*t" "*t" "*e" "*g" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*l" "*i" "*n" "* " "*e" "*l" "*b" "*a" "*t" "*h" "*s" "*a" "*h" "*(" "*(" "* " "*t" "*e" "*l" "*(" "*)" "*(" "* " "**" "*e" "*l" "*b" "*a" "*t" "*-" "*h" "*s" "*a" "*h" "*-" "*e" "*k" "*a" "*m" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*p" "**" "* " "*r" "*t" "*s" "*(" "*)" "*p" "**" "*(" "* " "*g" "*n" "*i" "*r" "*t" "*s" "*-" "*o" "*t" "*-" "*e" "*t" "*i" "*r" "*w" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*s" "*g" "*r" "*a" "*@" "*," "* " "*+" "*(" "*`" "*)" "*s" "*g" "*r" "*a" "* " "*t" "*s" "*e" "*r" "*&" "* " "*p" "**" "*(" "* " "*e" "*t" "*a" "*n" "*e" "*t" "*a" "*c" "*n" "*o" "*c" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "*f" "*-" "*t" "*s" "*e" "*t" "*," "* " "*l" "*," "* " "*m" "*e" "*t" "*i" "*," "* " "**" "*n" "*o" "*i" "*t" "*i" "*s" "*o" "*p" "*(" "*`" "*)" "*f" "*-" "*t" "*s" "*e" "*t" "* " "*t" "*s" "*e" "*t" "* " "*l" "* " "*m" "*e" "*t" "*i" "*(" "* " "*n" "*o" "*i" "*t" "*i" "*s" "*o" "*p" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*)" "*l" "* " "*r" "*d" "*c" "*(" "* " "*l" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*i" "* " "*1" "* " "*+" "*(" "* " "*i" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*)" "*i" "* " "**" "*n" "*o" "*i" "*t" "*i" "*s" "*o" "*p" "* " "*m" "*o" "*r" "*f" "*-" "*n" "*r" "*u" "*t" "*e" "*r" "*(" "*)" "*)" "*l" "* " "*r" "*a" "*c" "*(" "* " "*m" "*e" "*t" "*i" "* " "*f" "*-" "*t" "*s" "*e" "*t" "*(" "* " "*f" "*i" "*(" "*)" "*)" "*l" "*i" "*n" "* " "**" "*n" "*o" "*i" "*t" "*i" "*s" "*o" "*p" "* " "*m" "*o" "*r" "*f" "*-" "*n" "*r" "*u" "*t" "*e" "*r" "*(" "*)" "*l" "* " "*m" "*o" "*t" "*a" "*(" "* " "*f" "*i" "*(" "* " "*p" "*o" "*o" "*l" "*(" "*)" "*0" "* " "*i" "* " "*q" "*t" "*e" "*s" "*(" "*)" "*f" "*-" "*t" "*s" "*e" "*t" "* " "*l" "* " "*m" "*e" "*t" "*i" "*(" "* " "**" "*n" "*o" "*i" "*t" "*i" "*s" "*o" "*p" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*p" "**" "*," "*)" "*(" "* " "*m" "*o" "*r" "*f" "*-" "*n" "*r" "*u" "*t" "*e" "*r" "*(" "*`" "*)" "*p" "**" "*(" "* " "*n" "*r" "*u" "*t" "*e" "*r" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*)" "*)" "*l" "* " "*r" "*d" "*c" "*(" "* " "*h" "*t" "*g" "*n" "*e" "*l" "*(" "* " "*1" "* " "*+" "*(" "* " "*0" "*)" "*l" "* " "*m" "*o" "*t" "*a" "*(" "* " "*f" "*i" "*(" "*)" "*l" "*(" "* " "*h" "*t" "*g" "*n" "*e" "*l" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*y" "*d" "*o" "*b" "*@" "*," "*)" "*t" "*s" "*i" "*l" "*l" "* " "*r" "*e" "*p" "*l" "*e" "*h" "*(" "*@" "*," "* " "*n" "*g" "*o" "*r" "*p" "*(" "*`" "*)" "*)" "*l" "*i" "*n" "*)" "*)" "*)" "*s" "*m" "*e" "*t" "*i" "* " "*r" "*d" "*c" "*(" "* " "*r" "*e" "*p" "*l" "*e" "*h" "*(" "*)" "*)" "*s" "*m" "*e" "*t" "*i" "* " "*r" "*a" "*c" "*(" "* " "*n" "*u" "*f" "*e" "*d" "*'" "* " "*s" "*n" "*o" "*c" "*(" "* " "*s" "*n" "*o" "*c" "*(" "* " "*s" "*m" "*e" "*t" "*i" "* " "*f" "*i" "*(" "*)" "*s" "*m" "*e" "*t" "*i" "*(" "* " "*r" "*e" "*p" "*l" "*e" "*h" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*y" "*d" "*o" "*b" "* " "*t" "*s" "*e" "*r" "*&" "* " "*t" "*s" "*i" "*l" "*l" "*(" "* " "*s" "*l" "*e" "*b" "*a" "*l" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "*r" "*t" "*s" "*'" "*)" "*p" "**" "* " "*e" "*p" "*y" "*t" "*(" "* " "*q" "*e" "*(" "*)" "*p" "**" "*(" "* " "*p" "*g" "*n" "*i" "*r" "*t" "*s" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*q" "**" "* " "*p" "**" "* " "*=" "*(" "*)" "*q" "**" "* " "*p" "**" "* " "*q" "*e" "*(" "* " "*r" "*o" "*(" "*)" "*q" "**" "* " "*p" "**" "*(" "* " "*l" "*a" "*u" "*q" "*e" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*t" "* " "*l" "*i" "*n" "* " "*p" "**" "* " "*f" "*i" "*(" "*)" "*p" "**" "*(" "* " "*t" "*o" "*n" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*a" "**" "*)" "*)" "*b" "**" "* " "*r" "*o" "* " "*y" "*l" "*p" "*p" "*a" "*(" "* " "*t" "* " "*a" "**" "* " "*f" "*i" "*(" "* " "*b" "**" "* " "*f" "*i" "*(" "*)" "*b" "**" "* " "*t" "*s" "*e" "*r" "*&" "* " "*a" "**" "*(" "* " "*r" "*o" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*p" "**" "*)" "*l" "*i" "*n" "*)" "*q" "**" "* " "*d" "*n" "*a" "* " "*y" "*l" "*p" "*p" "*a" "*(" "* " "*p" "**" "* " "*f" "*i" "*(" "* " "*q" "**" "* " "*f" "*i" "*(" "*)" "*q" "**" "* " "*t" "*s" "*e" "*r" "*&" "* " "*p" "**" "*(" "* " "*d" "*n" "*a" "* " "*n" "*u" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*l" "*i" "*n" "*)" "*)" "*b" "**" "* " "*d" "*n" "*o" "*c" "*'" "* " "*s" "*n" "*o" "*c" "*(" "*)" "*)" "*a" "**" "* " "*r" "*d" "*c" "*(" "* " "*n" "*g" "*o" "*r" "*p" "*'" "* " "*s" "*n" "*o" "*c" "*(" "*)" "*a" "**" "* " "*r" "*a" "*c" "*(" "* " "*f" "*i" "*'" "* " "*t" "*s" "*i" "*l" "*(" "* " "*a" "**" "* " "*f" "*i" "*(" "*)" "*b" "**" "* " "*t" "*s" "*e" "*r" "*&" "* " "*a" "**" "*(" "* " "*o" "*r" "*c" "*a" "*m" "*(" "* " "*d" "*n" "*o" "*c" "* " "*r" "*a" "*v" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*l" "*i" "*n" "*)" "*)" "*)" "*l" "*i" "*n" "*)" "*)" "*q" "**" "* " "*r" "*d" "*c" "*(" "* " "*t" "*s" "*i" "*l" "*'" "* " "*s" "*n" "*o" "*c" "*(" "* " "*s" "*n" "*o" "*c" "*(" "*)" "*q" "**" "* " "*r" "*a" "*c" "*(" "* " "*s" "*n" "*o" "*c" "*(" "* " "*s" "*n" "*o" "*c" "*'" "* " "*s" "*n" "*o" "*c" "*(" "* " "*q" "**" "* " "*f" "*i" "*(" "*)" "*q" "**" "* " "*t" "*s" "*e" "*r" "*&" "*(" "* " "*o" "*r" "*c" "*a" "*m" "*(" "* " "*t" "*s" "*i" "*l" "* " "*r" "*a" "*v" "*f" "*e" "*d" "*(" "*)" "*)" "*e" "*u" "*l" "*a" "*v" "*," "* " "*e" "*m" "*a" "*n" "*," "* " "*r" "*a" "*v" "*f" "*e" "*d" "*(" "*`" "*)" "*e" "*u" "*l" "*a" "*v" "* " "*e" "*m" "*a" "*n" "*(" "* " "*r" "*e" "*t" "*e" "*m" "*a" "*r" "*a" "*p" "*f" "*e" "*d" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*)" "*y" "*d" "*o" "*b" "*@" "*," "* " "*e" "*m" "*a" "*n" "*," "* " "*k" "*c" "*o" "*l" "*b" "*(" "* " "*s" "*g" "*r" "*a" "*," "* " "*a" "*d" "*b" "*m" "*a" "*l" "*(" "* " "*e" "*m" "*a" "*n" "*," "* " "*q" "*t" "*e" "*s" "*(" "*`" "*)" "*y" "*d" "*o" "*b" "* " "*t" "*s" "*e" "*r" "*&" "* " "*s" "*g" "*r" "*a" "* " "*e" "*m" "*a" "*n" "*(" "* " "*n" "*u" "*f" "*e" "*d" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "*(" "*)" "*)" "*)" "*)" "*)" "*y" "*d" "*o" "*b" "*@" "*," "* " "*e" "*m" "*a" "*n" "*," "* " "*k" "*c" "*o" "*l" "*b" "*(" "* " "*s" "*g" "*r" "*a" "*," "* " "*o" "*r" "*c" "*a" "*m" "*(" "* " "*e" "*m" "*a" "*n" "*," "* " "*r" "*a" "*v" "*f" "*e" "*d" "*(" "*`" "*)" "*y" "*d" "*o" "*b" "* " "*t" "*s" "*e" "*r" "*&" "* " "*s" "*g" "*r" "*a" "* " "*e" "*m" "*a" "*n" "*(" "* " "*o" "*r" "*c" "*a" "*m" "*(" "* " "*o" "*r" "*c" "*a" "*m" "*f" "*e" "*d" "* " "*r" "*a" "*v" "*f" "*e" "*d" "*(" "* " "*n" "*g" "*o" "*r" "*p" "*(" nil))
