# pass ruby code (as string) to sonic pi server

# (spt-eval "play 66, sustain: 2")
(de spt-eval (Str)
   (sh "sonic-pi-tool.py eval \"@1\"" Str))

# (spt-eval-file "somefile.rb")
(de spt-eval-file (Str)
   (sh "sonic-pi-tool.py eval-file @1" Str))

(de spt-run-file (File)
   (sh "sonic-pi-tool.py run-file @1" File))

# very important
(de stop () (sh "sonic-pi-tool.py stop"))

# -- see https://github.com/emlyn/sonic-pi-tool


# sonic picolisp code
#
# a little bit of preprocessing and fancy printing allows
# picolisp to generate ruby code to send to the sonic pi
# server

(de spl Xs
   (out (tmp "sonic-pi")
      (spl-run-forms (mapcar spl-preprocess Xs)) )
   (spt-eval-file (tmp "sonic-pi")) )

(de spl-debug Xs
   (spl-run-forms (mapcar spl-preprocess Xs)) )

(de spl-run-forms (Xs)
   (for X Xs
      (ifn (member (car X) *SplFns)
         (eval X)
         (spl-run-fn X) )
      (prin ";") ) )

(de spl-run-fn (Form)
   (let [(Car . Cdr) Form]
      (prog
         (prin (pl-to-rb Car) (char 40))
         (for (I . X) Cdr
            (let Next (car (nth Cdr (+ I 1)))
               (cond
                  ((pair X)
                     (if (or (num? (car X)) (str? (car X)) (pre? ":" (car X)))
                        (prin "[" (glue "," X) "]")  # arrays
                        (if (member (car X) *SplFns)
                           (spl-run-fn X)
                           (eval X) ) ) )
                  ((pre? "." X) (prin X))
                  ((= (last (chop X)) ":")
                     (prin X " ") )
                  (T (prin X)) )
               # print commas unless it's an 'opt:' or '.function'
               (cond
                  ((or (= (last (chop X)) ":") (pre? "." Next)) NIL)
                  ((and (=1 I) (> (length Cdr) 1))
                     (prin ", ") )
                  ((and (> I 1) (not (= I (length Cdr))))
                     (prin ", ") ) ) ) )
         (prin (char 41)) ) ) )

(scl 4) # seems the best for sonic-pi

### sonic picolisp preprocessor
#
# - numbers to strings (0.125 -> "0.125")
# - pl symbols to rb symbols (do-something -> do_something)

(de spl-preprocess (X)
   (recur (X Acc)
      (ifn X
         (flip Acc)
         (cond
            # format numbers for printing (damnit picolisp)
            ((and (num? (car X)) (>= (length @) *Scl))
               (recurse (cdr X) (cons (format (car X) *Scl) Acc)) )
            # dashes to underscores
            ((sym? (car X))
               (let Sym
                     (if (and (not (spl-fn? (car X))) (sub? "-" (car X)))
                        (any (pl-to-rb (car X)))
                        (car X) )
                  (recurse (cdr X) (cons Sym Acc)) ) )
            # recurse down car / cdr
            ((pair (car X))
                  (recurse (cdr X) (cons (recurse (car X) NIL) Acc)) )
            (T (recurse (cdr  X) (cons (car X) Acc))) ) ) ) )

(de spl-fn? (Sym)
   (member Sym (append *SplBlockFns *SplFns)) )

(de spl-opt? (Sym)
   (= (last (chop Sym)) ":") )

# prototype block functions
(de live-loop Lst
   (let [(Nm Args . Rst) Lst]
      (prin "live_loop :" Nm " do |" (glue "," (mapc lowc1 Args)) "| ")
      (spl-run-forms Rst)
      (prin "end")))

# current
#
#  (with-fx :reverb (play 60))
#  (with-fx :reverb (mix: 2) (fx)
#     (play 60)
#     (control fx mix: 0.2)
#     (play 60) )

# ideal?
#
#  (with-fx :reverb (fx)  mix: 2  sustain: 4
#     (play 60) )

# damnit ruby syntax
(de with-fx Lst
   (let [(Nm . Rst) Lst]
      (prin "with_fx " Nm)
      (let Do NIL
         (for X Rst
            (cond
               ((pair X)
                  (cond
                     ((spl-opt? (car X))
                        (prog
                           (prin ", ")
                           (spl-run-args X) ) )
                     ((spl-fn? (car X))
                        (prog
                           (unless Do (prin " do "))
                           (on Do)
                           (macro (spl-run-forms '(( ^ X))) )) )
                     (T (prog
                           (unless Do (prin " do "))
                           (on Do)
                           (prin "|" (glue "," X) "| ") ) ) ) ) ) ) )
      (prin "end") ) )

# mostly the same as 'spl-run-fn'
(de spl-run-args (Xs)
   (for (I . X) Xs
      (let Next (car (nth Xs (+ I 1)))
         (cond
            ((pair X)
               (if (or (num? (car X)) (str? (car X)) (pre? ":" (car X)))
                  (prin "[" (glue "," X) "]")  # arrays
                  (if (member (car X) *SplFns)
                     (spl-run-fn X)
                     (eval X) ) ) )
            ((pre? "." X) (prin X))
            ((= (last (chop X)) ":")
               (prin X " ") )
            (T (prin X)) )
         # print commas unless it's an 'opt:' or '.function'
         (cond
            ((or (= (last (chop X)) ":") (pre? "." Next)) NIL)
            ((and (=1 I) (> (length Xs) 1))
               (prin ", ") )
            ((and (> I 1) (not (= I (length Xs))))
               (prin ", ") ) ) ) ) )

(de times Lst
   (let [(N . Rst) Lst]
      (prin N ".times do ")
      (spl-run-forms Rst)
      (prin "end")))

(setq *SplBlockFns
   (quote
      live-loop with-fx times
) )

(setq *SplFns
   (quote
      choose chord chord-degree chord-invert
      control cue dice doubles factor? halves
      hz-to-midi knit load-buffer
      load-sample load-samples load-synthdefs
      midi-notes midi-to-hz note note-range
      octs one-in pitch-to-ratio play
      play-chord play-pattern
      play-pattern-timed quantise ramp
      rand-back rand-i rand-reset rand-skip
      ratio-to-pitch rdist reset-mixer!
      rest? ring rrand rrand-i rt sample
      sample-duration
      sample-free sample-free-all scale
      set-cent-tuning! set-control-delta!
      set-mixer-control! set-schedule-ahead-time!
      set-volume! shuffle sleep spread stretch
      sync-bpm synth synth-names
      tick-reset tick-reset-all tick-set
      use-bpm use-bpm-mul use-cent-tuning
      use-octave use-random-seed use-sample-bpm
      use-sample-defaults use-sample-pack
      use-timing-guarantees use-transpose
      use-tuning vector vt
      # need to figure out namespacing
      sync rand inc dec kill line look pick range
) )




# utils

# picolisp symbol to ruby symbol
(de pl-to-rb (Sym) (pack (replace (chop Sym) "-" "_")))

# convert first char of string to lowercase
(de lowc1 (Str)
   (let C (chop Str)
      (pack
         (cons
            (lowc (car C))
            (cdr C) ) ) ) )


# abbrevation for a common pattern in this code
(de sh (@Str . Rest)
   (macro
      (call 'sh "-c"
         (text @Str
            ^ Rest ))))


# tests
(test '(play 60 release: "1.5000") (spl-preprocess '(play 60 release: 1.5)))

# should play a lame industrial beat in c minor
(de spl-sound-check ()
   (spl
      (live-loop notz () (play (chord :c3 :minor7).choose) (sleep 0.5))
      (live-loop padz () (sync :notz) (synth :prophet notes: (chord :c4 :minor7) release: 3) (sleep 4))
      (live-loop bazz () (sync :notz) (synth :tb303 note: :c2 release: 3 amp: 0.4) (sleep 4))
      (live-loop bd () (sync :notz) (sample :bd_haus) (sleep 1))
) )
