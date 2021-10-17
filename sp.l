### utils
(de spl-fn? (Sym)
   (member Sym (append *SplBlockFns *SplFns)) )

(de spl-opt? (Sym)
   (= (last (chop Sym)) ":") )

(de spl-array? (X)
   (and (pair X) (or (num? (car X)) (str? (car X)) (pre? ":" (car X)))) )

# (de spl-fn-args (Xs)
#    (prin "|" (glue "," Xs) "|") )

# (de spl-fn-args (Xs)
#    (prin "|")
#    (for X Xs
#       (let [Next (car (nth Cdr (+ I 1)))     # formatting often depends on
#             Prev (car (nth Cdr (- I 1))) ]   # what comes before or after
#          (prin (if (member '= (list Prev X Next)) " " ",") X) ) )
#    (prin "|") )

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

(de spl-debug! Xs
   (let File (tmp "sonic-pi")
      (out File
         (spl-run-forms (mapcar spl-preprocess Xs)) )
      # ruby formatter (catch errors, etc)
      (sh "cat @1 | rufo" File) ) )

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
         (macro (t (spl-debug ^ (cdr Lst)))) )
      ((= '! (car Lst))
         (macro (t (spl-debug! ^ (cdr Lst)))) )
      ((sym? (car Lst))
         (let [(@Nm . Rst) Lst]
            (macro
               (de @Nm X
                  (let [(Y) X]
                     ((case Y
                        ("T" 'spl-debug)
                        ("!" 'spl-debug!)
                        (T 'spl) )
                      ^ Rst ) ) ) ) ) )
      (T (macro (spl ^ Lst))) ) )

(de spl-run-forms (Xs)
   (for X Xs
      (cond
         ((= (car X) '=)
            (spl-run-var X) )
         ((or (member (car X) *SplBlockFns) (sub? "times" (car X)))
            (spl-run-block-fn X) )
         ((sub? "each" X)
            (spl-run-block-fn (cons NIL X)) )
         (T (spl-run-fn X)) )
      (prin ";") ) )

# getting messy...
(de spl-run-fn (Form)
   (let [(Car . Cdr) Form
         Opts NIL ] # flag for start of optional params
      (cond
         ((sub? "each" Form)
            (spl-run-block-fn (cons NIL Form)) )
         ((= Car 'if)
            (spl-run-block-fn Form) )
         (T
            (prog
               (if (member 'if Form)
                  (prin (pl-to-rb Car) " ")  # hack for inline 'if'
                  (prin (pl-to-rb Car) (char 40)) )
               (for (I . X) Cdr
                  (let [Next (car (nth Cdr (+ I 1)))     # formatting often depends on
                        Prev (car (nth Cdr (- I 1))) ]   # what comes before or after
                     (cond
                        ((pair X)
                           # refactor this mess into a single 'cond'
                           (if (and (=1 (length X)) (atom Prev) (sub? "." Prev) (not (str? Prev)))  # prev .method?
                              (print X)   # assume simple list, print it (eg. '[...].shuffle.take(3)')
                              (if (spl-array? X)
                                 # arrays
                                 (prog
                                    # (if (spl-array? Prev) (prin ", ") (prin " "))
                                    (unless (=1 I) (prin (if Opts " " ",")))
                                    (prin "[" (glue "," X) "]") )
                                 # lists
                                 (cond
                                    # variables
                                    ((= (car X) '=)
                                       (prog
                                          (unless (=1 I) (prin " "))
                                          (spl-run-var X)
                                          (prin ";") ) )
                                    # do / end block fns
                                    ((or (member (car X) *SplBlockFns) (sub? "times" (car X)))
                                       (prog
                                          (unless (=1 I) (prin " "))
                                          (spl-run-block-fn X) ) )
                                    ((sub? "each" X)
                                       (spl-run-block-fn (cons NIL X)) )
                                    (T (prog
                                          (unless (=1 I)
                                             # (prin " ")
                                             (prin
                                                (if (or (spl-opt? Prev) (member 'if Cdr))
                                                   " "
                                                   "," ) ) )
                                          (spl-run-fn X) ) ) ) ) ) )
                        # inline conditionals
                        ((member X '(if unless when while))
                           (prog
                              (ifn Opts (on Opts))  # similar behavior, hijacking 'Opts' flg
                              (prin " " X) ) )
                        # .methods
                        ((pre? "." X) (prin X))
                        # opts
                        ((= (last (chop X)) ":")
                           (prog
                              (ifn Opts (on Opts))
                              (prin (unless (=1 I) ", ") X) ) )
                        # previous .method
                        ((and (atom Prev) (sub? "." Prev) (not (str? Prev)))
                           (prin (if Opts " " ",") X) )
                        (Opts (prin " " X))
                        # arg list, e.g. |x| or |x y|
                        ((sub? "||" (pack (sort (chop X))))
                           (prin " " X) )
                        ((pre? "|" X)
                           (prog (on ArgsOn) (prin " " X)) )
                        ((= (last (chop X)) "|")
                           (prog (off ArgsOn) (prin " " X)) )
                        # ruby symbols
                        ((pre? ":" Prev) (prin "," X))
                        ((pre? ":" X) (prin (if Opts " " (unless (=1 I) ",")) X))
                        # numbers
                        ((member X '(* / + - %)) (prin " " X))  # inline maths
                        ((member Prev '(* / + - %)) (prin " " X))
                        ((format Prev *Scl) (prin "," X))
                        ((and (format X *Scl) Prev (sym? Prev))
                           (prin "," X) )
                        ((spl-opt? Prev) (prin " " X))
                        (T (prin (unless (=1 I) " ") X) ) ) ) )
               # (if (= Car 'if) (prin " end"))
               (ifn (member 'if Form) (prin (char 41))) ) ) ) ) )

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
                           ((or (member (car X) *SplBlockFns) (sub? "times" (car X)))
                              (prog
                                 # (prin " ")
                                 (unless (=1 I) (prin " "))
                                 (spl-run-block-fn X) ) )
                           (T
                              (prog
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
         (let [ArgsOn NIL
               Do NIL ]   # has 'do' been encountered in current form?
            (for (I . X) Cdr
               (let [Next (car (nth Cdr (+ I 1)))
                     Prev (car (nth Cdr (- I 1))) ]
                  # (if ArgsOn (prin ","))
                  (when ArgsOn
                     (prin (if (member '= (list X Prev)) " " ",")) )
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
                              ((or (member (car X) *SplBlockFns) (sub? "times" (car X)))
                                 (prog
                                    (prin " ")
                                    (spl-run-block-fn X)
                                    (prin ";") ) )
                              (T (prog
                                    (prin " ")
                                    (spl-run-fn X)
                                    (when Do
                                       (prin (if (spl-opt? Prev) " " ";")) ) ) ) ) ) )
                     ((= X 'do) (prog (on Do) (prin " " X)))
                     # .functions
                     ((pre? "." X) (prin X))
                     # arg list, e.g. |x| or |x y|
                     ((sub? "||" (pack (sort (chop X)))) (prin " " X))
                     ((pre? "|" X) (prog (on ArgsOn) (prin " " X)))
                     ((= (last (chop X)) "|") (prog (off ArgsOn) (prin " " X)))
                     # opts
                     ((= (last (chop X)) ":")
                        (prin ", " X) )
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
               ((and (num? (car X)) (or (>= (length (car X)) *Scl) (< 131 (car X) 1000)))
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

# 'times' / 'each' handled separately
(setq *SplBlockFns
   (quote
      at
      block_duration block_slept?
      define defonce
      comment
      in_thread
      live_loop
      loop
      ndefine
      on
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

(test '(do_something 60 release: "1.5000") (spl-preprocess '(do-something 60 release: 1.5)))


# should play a lame industrial beat in c minor
(sp ex-lame-industrial
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
      (sleep 1) ) )


(sp ex-filtered-dnb
   (use-sample-bpm :loop-amen)
   (with-fx :rlpf  cutoff: 10 cutoff-slide: 4  do |c|
      (live-loop :dnb do
         (sample :bass-dnb-f amp: 5)
         (sample :loop-amen amp: 5)
         (sleep 1)
         (control c cutoff: (rrand 40 120) cutoff-slide: (rrand 1 4)) ) ) )


(sp ex-basic-variable
   (= s (synth :prophet note: 60 release: 5 cutoff: 60))
   (sleep 1)
   (control s cutoff: 80)
   (sleep 1)
   (control s cutoff: 100) )


(sp ex-basic-functions
   (define :ratio do |note x y|
      (play (hz-to-midi (midi-to-hz note) * x / y)) )
   (ratio :c4 1 1)
   (ratio :c4 5 4)
   (ratio :c4 3 2)
   (ratio :c4 7 4) )


# HACK -- can't come up with a rule to get a comma after
# 'note' in the (ruby) output of the following 'ratio' calls.
# use '$var' in such a case
(sp ex-layering-functions
   (define :just-7th-chord do |note|
      (ratio note 1 1)
      (ratio note 5 4)
      (ratio note 3 2)
      (ratio note 7 4) )
   (just-7th-chord :d4) )


(sp ex-ambient-experiment
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
         (sleep 8) ) ) )


(sp ex-jungle
   (with-fx :lpf  cutoff: 90  do
      (with-fx :reverb  mix: 0.5  do
         (with-fx :compressor  pre-amp: 20  do
            (with-fx :distortion  distort: 0.4  do
               (live-loop :jungle do
                  (use-random-seed 667)
                  (4.times do
                     (sample :loop-amen
                        beat-stretch: 1
                        rate: [1 1 1 -1].choose / 2
                        finish: 0.5
                        amp: 0.1 )
                     (sample :loop-amen
                        beat-stretch: 1 )
                     (sleep 1) ) ) ) ) ) ) )


(sp ex-fm-noise
   (use-synth :fm)
   (live-loop :sci-fi do
      (= p
         (play (chord :Eb3 :minor).choose
            divisor: "0.01"
            div-slide: (rrand 0 10)
            depth: (rrand "0.001" 2)
            attack: "0.01"
            release: (rrand 0 5)
            amp: 0.5 ) )
      (control p divisor: (rrand "0.001" 50))
      (sleep [0.5 1 2].choose) ) )


(sp ex-reich-phase
   (= reich-notes (ring :E4 :Fs4 :B4))
   (live-loop :slower do
      (play reich-notes.tick release: 0.1)
      (sleep 0.3) )
   (live-loop :faster do
      (play reich-notes.tick release: 0.1)
      (sleep 0.296) ) )

(sp ex-echo-drama
   (use-synth :tb303)
   (use-bpm 45)
   (use-random-seed 3)
   (use-debug false)
   (with-fx :reverb do
      (with-fx :echo  delay: 0.5 decay: 0.4  do
         (live-loop :echoes do
            (play (chord [:b1 :b2 :e1 :e2 :b3 :e3].choose :minor).choose
               cutoff: (rrand 40 100)
               amp: 0.5
               attack: 0
               release: (rrand 1 2)
               cutoff-max: 120 )
            (sleep [0.25 0.5 0.5 0.5 1 1].choose) ) ) ) )


(sp ex-tron-bike
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
      (sleep 8) ) )


(sp ex-turbo-test
   (play 60)
   (sleep 1)
   (play 64) )


(sp ex-acid

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
            (sleep 0.125) ) )

      (cue :bar)
      (32.times do |i|
         (use-synth :tb303)
         (play (chord :a3 :minor).choose
            attack: 0
            release: 0.1
            cutoff: (rrand-i 70 98) + i
            res: (rrand 0.9 0.95) )
         (sleep 0.125) )

      (cue :baz)
      (with-fx :reverb  mix: 0.3  do |r|
         (32.times do |m|
            (control r  mix: (rrand 0.2 0.8))
            (use-synth :prophet)
            (play (chord :e :minor).choose
               attack: 0
               release: 0.08
               cutoff: (rrand-i 110 130) )
            (sleep 0.125) ) )

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

      (sleep 4) )
)


(sp ex-ambient

   (load-samples (sample-names :ambi))
   (sleep 2)

   (with-fx :reverb mix: 0.8 do
     (live-loop :foo do
       # try changing the sp- vars..
       (= sp-name (choose (sample_names :ambi)))
       # (= sp-name (choose (sample_names :drum)))
       (= sp-time [1 2].choose)
       # (= sp-time 0.5)
       (= sp-rate 1)
       # (= sp-rate 4)

       (= s (sample sp-name
               amp: 2
               cutoff: (rrand 70 130)
               rate: sp-rate * (choose [0.5 1])
               pan: (rrand -1 1)
               pan-slide: sp-time ) )
       (control s pan: (rrand 1 1))
       (sleep sp-time) ) ) )


(sp ex-idm-bb
   (live-loop :idm-bb do
      (= n [1 2 4 8 16].choose)
      (sample :drum-heavy-kick  amp: 2)
      (sample :ambi-drone
         rate: [0.25 0.5 0.125 1].choose
         amp: 0.25 if (one-in 8) )
      (sample :ambi-lunar-land
         rate: [0.5 0.125 1 -1 -0.5].choose
         amp: 0.25 if (one-in 8) )
      (sample :loop-amen
         attack: 0
         release: 0.05
         start: 1 - 1.0 / n
         rate: [1 1 1 1 1 1 -1].choose )
      (sleep (sample-duration :loop-amen) / n) ) )


(sp ex-chord-inversions
   ([1 3 6 4].each do |d|
      ((range -3 3).each do |i|
         (play-chord (chord-degree d :c :major 3 invert: i))
         (sleep 0.25) ) ) )

(sp ex-nested-iterations
   (4.times do
      (sample :drum-heavy-kick)
      (2.times do
         (sample :elec-blip2 rate: 2)
         (sleep 0.25) )
      (sample :elec-snare)
      (4.times do
         (sample :drum-tom-mid-soft)
         (sleep 0.125) ) ) )

(sp ex-basic-function-params

   (define :chord-player do |root repeats|
      (repeats.times do
         (play (chord root :minor) release: 3)
         (sleep 0.5) ) )

   (chord-player :e3 2)
   (sleep 0.5)
   (chord-player :a3 3)
   (chord-player :g3 4) )

(sp ex-cue-names
   (in-thread do
      (loop do
         (cue [:foo :bar :baz].choose)
         (sleep 0.5) ) )
   (in-thread do
      (loop do
         (sync :foo)
         (sample :elec-beep) ) )
   (in-thread do
      (loop do
         (sync :bar)
         (sample :elec-blip) ) )
   (in-thread do
      (loop do
         (sync :baz)
         (sample :elec-blip2) ) ) )


(sp ex-rand-surfer
   (live-loop :rand-surfer do
      (use-random-seed 1)
      (use-synth :dsaw)
      (= notes (scale :e2 :minor-pentatonic num-octaves: 2))
      (16.times do
         (play notes.choose release: 0.1 cutoff: (rrand 70 120))
         (sleep 0.125) ) ) )


(sp ex-looper-trooper
   (with-fx :reverb room: 1 do
      (live-loop :time do
         (synth :prophet release: 8 note: :e1 cutoff: 90 amp: 3)
         (sleep 8) ) )
   (live-loop :machine do
      (sample :loop-garzul rate: 0.5 finish: 0.25)
      (sample :loop-garzul beat-stretch: 4 amp: 1)
      (sleep 4) )
   (live-loop :kik do
      (sample :bd-haus amp: 2)
      (sleep 0.5) )
   (with-fx :echo do
      (live-loop :vortex do
         (= notes (scale :e3 :minor-pentatonic num-octaves: 3))
         (16.times do
            (play notes.choose release: 0.1 amp: 0.5)
            (sleep 0.125) ) ) ) )


(sp ex-magpi-coded-beats
   (use-bpm 100)
   (live-loop :amen-break do
      (= p [0.125 0.25 0.5].choose)
      (with-fx :slicer phase: p wave: 0 mix: (rrand 0.7 1) do
         (= r [1 1 1 -1].choose)
         (sample :loop-amen beat-stretch: 2 rate: r amp: 2) )
      (sleep 2) )
   (live-loop :bass-drum do
      (sample :bd-haus cutoff: 70 amp: 1.5)
      (sleep 0.5) )
   (live-loop :landing do
      (= bass-line (knit :e1 3 [:c1 :c2].choose 1))
      (with-fx :slicer phase: [0.25 0.5].choose invert-wave: 1 wave: 0 do
         (= s (synth :square note: bass-line.tick sustain: 4 cutoff: 60))
         (control s cutoff-slide: 4 cutoff: 120)
         (sleep 4) ) ) )


[sp ex-monday-blues
   (use-debug false)
   (load-samples [:drum-heavy-kick :drum-snare-soft])
   (live-loop :drums do
      # (puts "slow drums")
      (6.times do
         (sample :drum-heavy-kick rate: 0.8)
         (sleep 0.5) )
      # (puts "fast drums")
      (8.times do
         (sample :drum-heavy-kick rate: 0.8)
         (sleep 0.125) ) )
   (live-loop :synths do
      (use-synth :mod-saw)
      (use-synth-defaults
         amp: 0.5
         attack: 0
         sustain: 1
         release: 0.25
         mod-range: 12
         mod-phase: 0.5
         mod-invert-wave: 1 )
      (= notes
         (ring :F :C :D :D :G :D :D) )
      (notes.each do |n|
         (tick)
         (play (note n octave: 1) cutoff: (line 90 130 steps: 32).look) )
      (sleep 1) )
   (live-loop :snr delay: 12.5 do
      (sample :drum-snare-soft )
      (sleep 1) )
]


# this works, but something sounds ever so slightly different.
# there's some kind of resonant hum that seems to grow over
# time and occasionally clash with the chord changes. I don't
# notice it when loading the actual example in sonic pi

[sp ex-cloud-beat
   (use-bpm 100)

   # HISS
   (live-loop :hiss-loop do
      (sample :vinyl-hiss amp: 2)
      (sleep (sample-duration :vinyl-hiss)) )

   (define :hihat do
      (use-synth :pnoise)
      (with-fx :hpf cutoff: 120 do
         (play release: "0.01" amp: 13) ) )


   (live-loop :hihat-loop do
      (= divisors (ring 2 4 2 2 2 2 2 6))
      (divisors.tick.times do
         (hihat)
         (sleep 1.0 / divisors.look) ) )

   (live-loop :snare-loop do
      (sleep (ring 2.5 3).tick)
      (with-fx :lpf cutoff: 100 do
         (sample :sn-dub sustain: 0 release: 0.05 amp: 3) )
      (sleep (ring 1.5 1).look) )

   (define :bassdrum do |note1 duration note2 = note1|
      (use-synth :sine)
      (with-fx :hpf cutoff: 100 do
         (play note1 + 24  amp: 40 release: 0.01) )
      (with-fx :distortion distort: 0.1 mix: 0.3 do
         (with-fx :lpf cutoff: 26 do
            (with-fx :hpf cutoff: 55 do
               (= bass (play note1 amp: 85 release: duration note-slide: duration))
               (control bass note: note2) ) ) )
      (sleep duration) )

   # UGLY 'if', but it works
   (live-loop :bass-loop do
      (bassdrum 36 1.5)
      (if (bools 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0).tick
         then
            (bassdrum 36 0.5 40);   # semi-colons are gross but necessary for now
            (bassdrum 38 1 10);
         else
            (bassdrum 36 1.5) )
      (bassdrum 36 1.0 (ring 10 10 10 40).look) )

   (= chord1 (chord :c4  :maj9 num-octaves: 2))
   (= chord2 (chord :es4 :maj9 num-octaves: 2))
   (= chord3 (chord :b3  :maj9 num-octaves: 2))
   (= chord4 (chord :d4  :maj9 num-octaves: 2))

   (= chord-low1 (chord :c2  :maj9))
   (= chord-low2 (chord :es2 :maj9))
   (= chord-low3 (chord :b1  :maj9))
   (= chord-low4 (chord :d2  :maj9))

   (= chord-high chord1)
   (= chord-low  chord-low1)

   (live-loop :chord-selector delay: -0.5 do
      (= chord-high (knit chord1 2 chord2 2 chord3 4 chord4 4).tick)
      (= chord-low  (knit chord-low1 2 chord-low2 2 chord-low3 4 chord-low4 4).look)
      (sleep 8) )

   (define :chord-player do |the-chord|
      (use-synth :blade)
      (the-chord.each do |note|
         (play note
            attack: (rand 4)
            release: (rand 6..8)
            cutoff: (rand 50..85)
            vibrato-rate: (rand 0.01..2)
            amp: 0.55 ) ) )

   (with-fx :reverb room: 0.99 mix: 0.7 do
      (live-loop :chord-loop do
         (chord-player chord-high.pick(6))
         (chord-player chord-low.take(3))
         (sleep 8) ) )

]
