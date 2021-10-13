### utils
(de spl-fn? (Sym)
   (member Sym (append *SplBlockFns *SplFns)) )

(de spl-opt? (Sym)
   (= (last (chop Sym)) ":") )

(de spl-array? (X)
   (or (num? (car X)) (str? (car X)) (pre? ":" (car X))) )

(de spl-fn-args (Xs)
   (prin "|" (glue "," Xs) "|") )

# picolisp symbol to ruby symbol
(de pl-to-rb (Sym) (pack (replace (chop Sym) "-" "_")))

# convert first char of string to lowercase
(de lowc1 (Str)
   (let C (chop Str)
      (pack
         (cons
            (lowc (car C))
            (cdr C) ) ) ) )

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

# very important
(de stop () (sh "sonic-pi-tool.py stop"))



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

# even more convenient!
#
#  (sp ...) -- send to sonic pi
#  (sp T ...) -- print to stdout (debug)
(de sp Lst
   (if (=T (car Lst))
      (macro (t (spl-debug ^ (cdr Lst))))
      (macro (spl ^ Lst)) ) )

(de spl-run-forms (Xs)
   (for X Xs
      (cond
         ((= (car X) '=)
            (spl-run-var X) )
         ((or (member (car X) *SplBlockFns) (sub? ".times" (car X)))
            (spl-run-block-fn X) )
         (T (spl-run-fn X)) )
      (prin ";") ) )

(de spl-run-fn (Form)
   (let [(Car . Cdr) Form]
      (prog
         (prin (pl-to-rb Car) (char 40))
         (for (I . X) Cdr
            (let [Next (car (nth Cdr (+ I 1)))
                  Prev (car (nth Cdr (- I 1))) ]
               (cond
                  ((pair X)
                     (if (spl-array? X)
                        # arrays
                        (prog
                           (if (spl-array? Prev) (prin ", ") (prin " "))
                           (prin "[" (glue "," X) "]") )
                        # lists
                        (cond
                           # variables
                           ((= (car X) '=)
                              (prog
                                 (unless (=1 I) (prin " "))
                                 (spl-run-var X)
                                 (prin ";") ) )
                           # ((member (car X) *SplFns)
                           #    (prog
                           #       (unless (=1 I) (prin " "))
                           #       (spl-run-fn X) ) )
                           ((or (member (car X) *SplBlockFns) (sub? ".times" (car X)))
                              (prog
                                 (unless (=1 I) (prin " "))
                                 (spl-run-block-fn X) ) )
                           # (T (eval X))
                           (T (prog
                                 (unless (=1 I) (prin " "))
                                 (spl-run-fn X) ) ) ) ) )
                  # .functions
                  ((pre? "." X) (prin X))
                  ((sub? "." Prev) (prin "," X))
                  # arg list, e.g. |x| or |x y|
                  ((sub? "||" (pack (sort (chop X))))
                     (prin " " (lowc1 X)) )
                  ((pre? "|" X)
                     (prog (on ArgsOn) (prin " " (lowc1 X))) )
                  ((= (last (chop X)) "|")
                     (prog (off ArgsOn) (prin " " (lowc1 X))) )
                  # opts
                  ((= (last (chop X)) ":")
                     (prin ", " X) )
                  ((pre? ":" Prev) (prin "," X))
                  # numbers
                  ((format Prev *Scl) (prin "," X))
                  ((spl-opt? Prev) (prin " " X))
                  # ((and Prev (sym? Prev)) (prin "," X))
                  (T (prin (unless (=1 I) " ") X) ) ) ) )
         (prin (char 41)) ) ) )

(de spl-run-var (Form)
   (++ Form)  # discard '=' sym
   (let [(Car . Cdr) Form]
      (prog
         (prin (pl-to-rb Car) " = ")
         (for (I . X) Cdr
            (let [Next (car (nth Cdr (+ I 1)))
                  Prev (car (nth Cdr (- I 1))) ]
               (cond
                  ((pair X)
                     (if (spl-array? X)
                        # arrays
                        (prog
                           (if (spl-array? Prev) (prin ", ") (prin " "))
                           (prin "[" (glue "," X) "]") )
                        # lists
                        (cond
                           # variables
                           ((= (car X) '=)
                              (prog
                                 (unless (=1 I) (prin " "))
                                 (spl-run-var X)
                                 (prin ";") ) )
                           # functions
                           # ((member (car X) *SplFns)
                           #    (prog
                           #       # (prin " ")
                           #       (unless (=1 I) (prin " "))
                           #       (spl-run-fn X) ) )
                           # blocks
                           ((or (member (car X) *SplBlockFns) (sub? ".times" (car X)))
                              (prog
                                 # (prin " ")
                                 (unless (=1 I) (prin " "))
                                 (spl-run-block-fn X) ) )
                           # # lisp, most likely
                           # (T (eval X))
                           (T
                              (prog
                                 # (prin " ")
                                 (unless (=1 I) (prin " "))
                                 (spl-run-fn X) ) ) ) ) )
                  # .functions
                  ((pre? "." X) (prin X))
                  ((pre? "." Prev) (prin "," X))
                  # arg list, e.g. |x| or |x y|
                  ((sub? "||" (pack (sort (chop X)))) (prin " " X))
                  ((pre? "|" X) (prog (on ArgsOn) (prin " " X)))
                  ((= (last (chop X)) "|") (prog (off ArgsOn) (prin " " X)))
                  # opts
                  ((= (last (chop X)) ":")
                     (prin ", " X) )
                  # numbers
                  ((format Prev *Scl) (prin "," X))
                  (T (prin (unless (=1 I) " ") X)) ) ) ) ) ) )

(de spl-run-block-fn (Form)
   (let [(Car . Cdr) Form]
      (prog
         (prin (pl-to-rb Car))
         (let ArgsOn NIL
            (for (I . X) Cdr
               (if ArgsOn (prin ","))
               (let [Next (car (nth Cdr (+ I 1)))
                     Prev (car (nth Cdr (- I 1))) ]
                  (cond
                     ((pair X)
                        (if (spl-array? X)
                           # arrays
                           (prog
                              (if (spl-array? Prev) (prin ", ") (prin " "))
                              (prin "[" (glue "," X) "]") )
                           # lists
                           (cond
                              # variables
                              ((= (car X) '=)
                                 (prog
                                    (unless (=1 I) (prin " "))
                                    (spl-run-var X)
                                    (prin ";") ) )
                              # ((member (car X) *SplFns)
                              #    (prog
                              #       (prin " ")
                              #       (spl-run-fn X)
                              #       (unless (spl-opt? Prev) (prin ";")) ) )
                              # ((member (car X) *SplBlockFns)
                              ((or (member (car X) *SplBlockFns) (sub? ".times" (car X)))
                                 (prog
                                    (prin " ")
                                    (spl-run-block-fn X)
                                    (prin ";") ) )
                              # (T (eval X))
                              (T (prog
                                 (prin " ")
                                 (spl-run-fn X)
                                 (unless (spl-opt? Prev) (prin ";")) ) ) ) ) )
                     # .functions
                     ((pre? "." X) (prin X))
                     # arg list, e.g. |x| or |x y|
                     ((sub? "||" (pack (sort (chop X)))) (prin " " (lowc1 X)))
                     ((pre? "|" X) (prog (on ArgsOn) (prin " " (lowc1 X))))
                     ((= (last (chop X)) "|") (prog (off ArgsOn) (prin " " (lowc1 X))))
                     # opts
                     ((= (last (chop X)) ":")
                        (prin ", " X) )
                     # # local vars (via |...|) -- this feels like a hack
                     # ((upp? (car (chop X)))
                     #    (prin (lowc1 X) ",") )
                     # numbers
                     # ((format Prev *Scl) (prin "," X))
                     (T (prin " " X)) )
                  ) ) )
         (prin " end") ) ) )

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
               ((and (num? (car X)) (or (>= (length (car X)) *Scl) (< 99 (car X) 1000)))
                  (recurse (cdr X) (cons (format (car X) *Scl) Acc)) )
               # dashes to underscores
               ((sym? (car X))
                  (let Sym
                        (if (and (> (length (car X)) 1) (sub? "-" (car X)))
                           (any (pl-to-rb (car X)))
                           (car X) )
                     (recurse (cdr X) (cons Sym Acc)) ) )
               # recurse down car / cdr
               ((pair (car X))
                     (recurse (cdr X) (cons (recurse (car X) NIL) Acc)) )
               (T (recurse (cdr  X) (cons (car X) Acc))) ) ) ) )


(scl 4) # seems the best for sonic-pi

(setq *SplBlockFns
   (quote
      at
      block_duration block_slept?
      define defonce
      comment
      in_thread
      live_loop
      ndefine
      on
      # times # 5.times do nonsense
      uncomment
      with_arg_bpm_scaling with_arg_checks
      with_bpm with_bpm_mul with_cent_tuning
      with_cue_logging with_debug
      with_fx
      with_merged_sample_defaults
      with_merged_synth_defaults
      with_octave with_random_seed
      with_sample_bpm with_sample_defaults with_sample_pack
      with_synth with_synth_defaults
      with_timing_guarantees
      with_transpose with_tuning ) )

(setq *SplFns
   (quote
      all_sample_names assert assert_equal
      beat bools bt
      choose chord chord_degree chord_invert
      chord_names control cue current_arg_checks
      current_beat_duration current_bpm current_cent_tuning
      current_debug current_octave current_random_seed
      current_sample_defaults current_sample_pack
      current_sched_ahead_time current_synth current_synth_defaults
      current_transpose current_volume
      dec degree density dice doubles factor? fx_names
      halves hz_to_midi
      inc inspect
      kill knit line load_buffer load_example
      load_sample load_samples load_synthdefs look
      midi_notes midi_to_hz
      note note_info note_range
      octs one_in
      pick pitch_to_ratio
      play play_chord play_pattern play_pattern_timed
      print puts
      quantise
      ramp rand_back rand_i rand_reset rand_skip
      range ratio_to_pitch rdist reset_mixer!
      rest? ring rrand rrand_i rt
      sample sample_buffer sample_duration
      sample_free sample_free_all sample_groups
      sample_info sample_loaded? sample_names sample_paths
      scale scale_names
      set_cent_tuning! set_control_delta!
      set_mixer_control! set_schedule_ahead_time!
      set_volume! shuffle sleep spark spark_graph
      spread status stop stretch
      sync_bpm synth synth_names
      tick tick_reset tick_reset_all tick_set
      use_arg_bpm_scaling use_arg_checks
      use_bpm use_bpm_mul use_cent_tuning
      use_cue_logging use_debug
      use_merged_sample_defaults
      use_merged_synth_defaults
      use_octave use_random_seed use_sample_bpm
      use_sample_defaults use_sample_pack
      use_synth
      use_timing_guarantees use_transpose
      use_tuning
      vector version vt
      wait ) )



### examples / tests

# define new example
#
#  (sp-example ex-blah
#     (live-loop :blah do
#        ... ) )
#
# call it
#
#  (ex-blah) -- send to sonic pi
#  (ex-blah T) -- debug

(de sp-example Lst
   (let [(@Nm . Rst) Lst]
      (macro
         (de @Nm (Flg)
            ((ifn Flg 'spl 'spl-debug)
               ^ Rst ) ) ) ) )

(def 'spex sp-example)

(test '(do_something 60 release: "1.5000") (spl-preprocess '(do-something 60 release: 1.5)))

# should play a lame industrial beat in c minor
(de spl-sound-check ()
   (spl
      (live-loop :notz do
         (play (chord :c3 :minor7).choose)
         (sleep 0.5) )
      (live-loop :padz do
         (sync :notz)
         (synth :prophet notes: (chord :c4 :minor7) release: 3)
         (sleep 4) )
      (live-loop :bazz do
         (sync :notz)
         (synth :tb303 note: :c2 release: 3 amp: 0.4)
         (sleep 4) )
      (live-loop :bd do
         (sync :notz)
         (sample :bd_haus)
         (sleep 1) ) ) )


(de sound-check-amen-dnb ()
   (spl-debug
      (use-sample-bpm :loop-amen)
      (with-fx :rlpf  cutoff: 10 cutoff-slide: 4  do |c|
         (live-loop :dnb do
            (sample :bass-dnb-f amp: 5)
            (sample :loop-amen amp: 5)
            (sleep 1)
            (control c cutoff: (rrand 40 120) cutoff-slide: (rrand 1 4)) ) ) ) )


(de ex-basic-variable ()
   (spl
      (= s (synth :prophet note: 60 release: 5 cutoff: 60))
      (sleep 1)
      (control s cutoff: 80)
      (sleep 1)
      (control s cutoff: "100") ) )  # bug with 0.01 and 100, etc

(de ex-basic-functions ()
   (spl
      (define :ratio do |note x y|
         (play (hz-to-midi (midi-to-hz note) * x / y)) )
      (ratio :c4 1 1)
      (ratio :c4 5 4)
      (ratio :c4 3 2)
      (ratio :c4 7 4) ) )

# BROKEN
(de ex-layering-functions ()
   (spl-debug
      (define :just-7th-chord do |note|
         (ratio note 1 1)
         (ratio note 5 4)
         (ratio note 3 2)
         (ratio note 7 4) )
      (sleep 1)
      (just-7th-chord :d4) ) )


(de ex-ambient-experiment ()
   (spl
      (use-synth :hollow)
      (with-fx :reverb mix: 0.7 do
         (live-loop :note1 do
            (play [:D4 :E4].choose attack: 6 release: 6)
            (sleep 8) )
         (live-loop :note2 do
            (play [:Fs4 :G4].choose attack: 4 release: 5)
            (sleep 8) )
         (live-loop :note3 do
            (play [:A4 :Cs5].choose attack: 5 release: 5)
            (sleep 8) ) ) ) )

# lower volume a bit, kinda loud
(de ex-jungle ()
   (spl
      (with-fx :lpf  cutoff: 90  do
         (with-fx :reverb  mix: 0.5  do
            (with-fx :compressor  pre-amp: 20  do
               (with-fx :distortion  distort: 0.4  do
                  (live-loop :jungle do
                     (use-random-seed 667)
                     (times 4
                        (sample :loop-amen
                           beat-stretch: 1
                           rate: [1 1 1 -1].choose / 2
                           finish: 0.5
                           amp: 0.1 )
                        (sample :loop-amen
                           beat-stretch: 1 )
                        (sleep 1) ) ) ) ) ) ) ) )

(de ex-fm-noise ()
   (spl
      (use-synth :fm)
      (live-loop :sci-fi do
         (= player
            (play (chord :Eb3 :minor).choose
               divisor: 0.0100
               div-slide: (rrand 0 10)
               depth: (rrand "0.001" 2)
               attack: 0.01
               release: (rrand 0 5)
               amp: 0.5 ) )
         (control player divisor: (rrand "0.001" 50))
         (sleep [0.5 1 2].choose) ) ) )

(de ex-reich-phase ()
   (spl
      (= reich-notes (ring :E4 :Fs4 :B4))
      (live-loop :slower do
         (play reich-notes.tick release: 0.1)
         (sleep 0.3) )
      (live-loop :faster do
         (play reich-notes.tick release: 0.1)
         (sleep 0.296) ) ) )


(de ex-tron-bike ()
   (spl
      (use-random-seed 10)
      (= notes (ring :b1 :b2 :e1 :e2 :b3 :e3))
      (live-loop :tron do
         (with-synth :dsaw do
            (with-fx :slicer phase: [0.25 0.125].choose do
               (with-fx :reverb room: 0.5 mix: 0.3 do
                  (= n1 (chord notes.choose :minor).choose)
                  (= n2 (chord notes.choose :minor).choose)
                  (= p (play n1 amp: 2 release: 8 note-slide: 4 cutoff: 30 cutoff-slide: 4 detune: (rrand 0 0.2)))
                  (control p note: n2 cutoff: (rrand 80 120)) ) ) )
         (sleep 8) ) ) )

(sp-example ex-turbo-test
   (play 60)
   (sleep 1)
   (play 64) )


# Broken
(spex ex-acid
   (use-debug false)
   (load-sample :bd-fat)

   (8.times do
      (sample :bd-fat  amp: (line 0 5 steps: 8).tick)
      (sleep 0.5) )

   (live-loop :drums do
      (sample :bd-fat amp: 5)
      (sleep 0.5) )

   (live-loop :acid do
      (cue :foo)
      (4.times do |i|
         (use-random-seed 667)
         (16.times do
            (use-synth :tb303)
            (play (chord :e3 :minor).choose
               attack: 0
               release: 0.1
               cutoff: (rrand-i 50 90) + i * 10 )
            (sleep 0.125) ) ) )

      (cue :bar)
      (32.times do |i|
         (use-synth :tb303)
         (play (chord :a3 :minor).choose
            attack: 0
            release: 0.1
            cutoff: (rrand-i 70 98) + i
            res: (rrand 0.9 0.95) )
         (sleep 0.125) )

      (cue :quux)
      (in-thread do
         (use-random-seed 668)
         (with-fx :echo  phase: 0.125  do
            (16.times do
               (use-synth :tb303)
               (play (chord :a :minor).choose
                  attack: 0
                  release: 0.1
                  cutoff: (rrand 50 100))
               (sleep 0.25) ) ) )

      (sleep 4)
)

# kinda works
(spex ex-ambient

   (load-samples (sample-names :ambi))
   (sleep 2)

   (with_fx :reverb mix: 0.8 do
     (live_loop :foo do
       # try changing the sp_ vars..
       (= sp-name (choose (sample_names :ambi)))
       # sp_name = choose sample_names :drum
       (= sp-time [1 2].choose)
       #sp_time = 0.5
       (= sp-rate 1)
       #sp_rate = 4

       (= s (sample sp-name
               amp: 2
               cutoff: (rrand 70 130)
               rate: sp-rate * (choose [0.5 1])
               pan: (rrand -1 1)
               pan-slide: sp-time ) )
       (control s pan: (rrand 1 1))
       (sleep sp-time) ) ) )
