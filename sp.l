(load "ruby.l")

# abbrevation for common pattern in this code
(de sh (@Str . Rest)
   (macro
      (call 'sh "-c"
         (text @Str
            ^ Rest ))))


### sonic pi tool
#
# pass ruby code (as string) to sonic pi server
#
# -- see https://github.com/emlyn/sonic-pi-tool

# (spt-eval "play 66, sustain: 2")
(de spt-eval (Str)
   (sh "sonic-pi-tool.py eval \"@1\"" Str))

# (spt-eval-file "somefile.rb")
(de spt-eval-file (Str)
   (sh "sonic-pi-tool.py eval-file @1" Str))

(de spt-run-file (File)
   (sh "sonic-pi-tool.py run-file @1" File))

(de spt-start-server ()
   (sh "sonic-pi-tool.py start-server") )

(de spt-logs ()
   (sh "sonic-pi-tool.py logs") )

# very important
(de stop () (sh "sonic-pi-tool.py stop"))



# sonic picolisp code
#
# a little bit of preprocessing and fancy printing allows
# picolisp to generate ruby code to send to the sonic pi
# server

(de sp-run Xs
   (out (tmp "sonic-pi")
      (ruby-run Xs))
   (spt-eval-file (tmp "sonic-pi")) )

(de sp-debug Xs
   (ruby-run Xs) )

(de sp-check Xs
   (pipe
      (out '(ruby "-c")
         (ruby-run Xs) )
      (line T) ) )

# even more convenient!
#
#  (sp ...) -- send to sonic pi
#  (sp T ...) -- print to stdout (debug)
#  (sp ! ...) -- debug with ruby formatter
#  (sp foo ...) -- define lisp function
#  (foo) -- send to sonic pi
#  (foo T) -- debug
#  (foo !)

(de sp Lst
   (cond
      ((=T (car Lst))
         (macro (nil (sp-debug ^ (cdr Lst)))) )
      ((= '! (car Lst))
         (macro (sp-check ^ (cdr Lst))) )
      ((sym? (car Lst))
         (let [(@Nm . Rst) Lst]
            (macro
               (de @Nm X
                  (let [(Y) X]
                     ((case Y
                        ("T" 'sp-debug)
                        ("!" 'sp-check)
                        (T 'sp-run) )
                      ^ Rst ) ) ) ) ) )
      (T (macro (sp-run ^ Lst))) ) )
