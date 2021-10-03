# currently using 'spl%' as the front-end to sonic pl syntax,
# paired with the redefinitions of 'spl-run-[...]' functions
# defined towards the end of the file.  most things are
# working, see the examples at the end of the file. code is a
# mess - too many things commented out, functions redefined.



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


# using 'spl%' below, but not ready to delete this

# sonic picolisp code
#
# a little bit of preprocessing and fancy printing allows
# picolisp to generate ruby code to send to the sonic pi
# server

# (de spl Xs
#    (out (tmp "sonic-pi")
#       (spl-run-forms (mapcar spl-preprocess Xs)) )
#    (spt-eval-file (tmp "sonic-pi")) )

# (de spl-debug Xs
#    (spl-run-forms (mapcar spl-preprocess Xs)) )

# (de spl-run-forms (Xs)
#    (for X Xs
#       (ifn (member (car X) *SplFns)
#          (eval X)
#          (spl-run-fn X) )
#       (prin ";") ) )

# (de spl-run-fn (Form)
#    (let [(Car . Cdr) Form]
#       (prog
#          (prin (pl-to-rb Car) (char 40))
#          (for (I . X) Cdr
#             (let Next (car (nth Cdr (+ I 1)))
#                (cond
#                   ((pair X)
#                      (if (or (num? (car X)) (str? (car X)) (pre? ":" (car X)))
#                         (prin "[" (glue "," X) "]")  # arrays
#                         (if (member (car X) *SplFns)
#                            (spl-run-fn X)
#                            (eval X) ) ) )
#                   ((pre? "." X) (prin X))
#                   ((= (last (chop X)) ":")
#                      (prin X " ") )
#                   (T (prin X)) )
#                # print commas unless it's an 'opt:' or '.function'
#                (cond
#                   ((or (= (last (chop X)) ":") (pre? "." Next)) NIL)
#                   ((and (=1 I) (> (length Cdr) 1))
#                      (prin ", ") )
#                   ((and (> I 1) (not (= I (length Cdr))))
#                      (prin ", ") ) ) ) )
#          (prin (char 41)) ) ) )

(de spl-array? (X)
   (or (num? (car X)) (str? (car X)) (pre? ":" (car X))) )

(scl 4) # seems the best for sonic-pi

### sonic picolisp preprocessor
#
# - numbers to strings (0.125 -> "0.125")
# - pl symbols to rb symbols (do-something -> do_something)

# (de spl-preprocess (X)
#    (recur (X Acc)
#       (ifn X
#          (flip Acc)
#             (cond
#                # format numbers for printing (damnit picolisp)
#                ((and (num? (car X)) (>= (length @) *Scl))
#                   (recurse (cdr X) (cons (format (car X) *Scl) Acc)) )
#                # dashes to underscores
#                ((sym? (car X))
#                   (let Sym
#                         (if (and (not (spl-fn? (car X))) (sub? "-" (car X)))
#                            (any (pl-to-rb (car X)))
#                            (car X) )
#                      (recurse (cdr X) (cons Sym Acc)) ) )
#                # recurse down car / cdr
#                ((pair (car X))
#                      (recurse (cdr X) (cons (recurse (car X) NIL) Acc)) )
#                (T (recurse (cdr  X) (cons (car X) Acc))) ) ) ) )

(de spl-fn? (Sym)
   (member Sym (append *SplBlockFns *SplFns)) )

(de spl-opt? (Sym)
   (= (last (chop Sym)) ":") )

## prototype block functions
#(de live-loop Lst
#   (let [(Nm Args . Rst) Lst]
#      (prin "live_loop " Nm " do ")
#      (spl-fn-args Args)
#      (prin " ")
#      (spl-run-forms Rst)
#      (prin "end") ) )

## current
##
##  (with-fx :reverb (play 60))
##  (with-fx :reverb (mix: 2) (fx)
##     (play 60)
##     (control fx mix: 0.2)
##     (play 60) )

## damnit ruby syntax
#(de with-fx Lst
#   (let [(Nm . Rst) Lst]
#      (prin "with_fx " Nm)
#      (let Do NIL
#         (for X Rst
#            (cond
#               ((pair X)
#                  (cond
#                     ((spl-opt? (car X))
#                        (prog
#                           (prin ", ")
#                           (spl-run-opts X) ) )
#                     ((spl-fn? (car X))
#                        (prog
#                           (unless Do (prin " do "))
#                           (on Do)
#                           (macro (spl-run-forms '(( ^ X))) )) )
#                     (T (prog
#                           (unless Do (prin " do "))
#                           (on Do)
#                           (prin "|" (glue "," X) "| ") ) ) ) ) ) ) )
#      (prin "end") ) )

(de spl-fn-args (Xs)
   (prin "|" (glue "," (mapc lowc1 Xs)) "|") )

# # mostly the same as 'spl-run-fn'
# (de spl-run-opts (Xs)
#    (for (I . X) Xs
#       (let Next (car (nth Xs (+ I 1)))
#          (cond
#             ((pair X)
#                (if (or (num? (car X)) (str? (car X)) (pre? ":" (car X)))
#                   (prin "[" (glue "," X) "]")  # arrays
#                   (if (member (car X) *SplFns)
#                      (spl-run-fn X)
#                      (eval X) ) ) )
#             ((pre? "." X) (prin X))
#             ((= (last (chop X)) ":")
#                (prin X " ") )
#             (T (prin X)) )
#          # print commas unless it's an 'opt:' or '.function'
#          (cond
#             ((or (= (last (chop X)) ":") (pre? "." Next)) NIL)
#             ((and (=1 I) (> (length Xs) 1))
#                (prin ", ") )
#             ((and (> I 1) (not (= I (length Xs))))
#                (prin ", ") ) ) ) ) )

# (de times Lst
#    (let [(N . Rst) Lst]
#       (prin N ".times do ")
#       (spl-run-forms Rst)
#       (prin "end")))

# (setq *SplBlockFns
#    (quote
#       live-loop with-fx times
# ) )

# (setq *SplFns
#    (quote
#       choose chord chord-degree chord-invert
#       control cue dice doubles factor? halves
#       hz-to-midi knit load-buffer
#       load-sample load-samples load-synthdefs
#       midi-notes midi-to-hz note note-range
#       octs one-in pitch-to-ratio play
#       play-chord play-pattern
#       play-pattern-timed quantise ramp
#       rand-back rand-i rand-reset rand-skip
#       ratio-to-pitch rdist reset-mixer!
#       rest? ring rrand rrand-i rt sample
#       sample-duration
#       sample-free sample-free-all scale
#       set-cent-tuning! set-control-delta!
#       set-mixer-control! set-schedule-ahead-time!
#       set-volume! shuffle sleep spread stretch
#       sync-bpm synth synth-names
#       tick-reset tick-reset-all tick-set
#       use-bpm use-bpm-mul use-cent-tuning
#       use-octave use-random-seed use-sample-bpm
#       use-sample-defaults use-sample-pack
#       use-synth
#       use-timing-guarantees use-transpose
#       use-tuning vector vt
#       # need to figure out namespacing
#       sync rand inc dec kill line look pick range
# ) )


# alternate
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
   (spl%
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
   (spl-debug%
      (use-sample-bpm :loop-amen)
      (with-fx :rlpf cutoff: 10 cutoff-slide: 4 do |c|
         (live-loop :dnb do
            (sample :bass-dnb-f amp: 5)
            (sample :loop-amen amp: 5)
            (sleep 1)
            (control c cutoff: (rrand 40 120) cutoff-slide: (rrand 1 4)) ) ) ) )



# alternate implementation
(de spl% Xs
   (out (tmp "sonic-pi")
      (spl-run-forms% (mapcar spl-preprocess Xs)) )
   (spt-eval-file (tmp "sonic-pi")) )

(de spl-debug% Xs
   (spl-run-forms% (mapcar spl-preprocess Xs)) )

(de spl-run-forms% (Xs)
   (for X Xs
      (cond
         ((= (car X) '=)
            (spl-run-var X) )
         ((member (car X) *SplFns)
            (spl-run-fn X) )
         ((member (car X) *SplBlockFns)
            (spl-run-block-fn X) )
         (T (eval X)) )
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
                           ((member (car X) *SplFns)
                              (prog
                                 # (prin " ")
                                 (unless (=1 I) (prin " "))
                                 (spl-run-fn X) ) )
                           ((member (car X) *SplBlockFns)
                              (prog
                                 # (prin " ")
                                 (unless (=1 I) (prin " "))
                                 (spl-run-block-fn X) ) )
                           (T (eval X)) ) ) )
                  # .functions
                  ((pre? "." X) (prin X))
                  # arg list, e.g. |x| or |x y|
                  ((sub? "||" (pack (sort (chop X))))
                     (prin " " X))
                  ((pre? "|" X)
                     (prog (on ArgsOn) (prin " " X)))
                  ((= (last (chop X)) "|")
                     (prog (off ArgsOn) (prin " " X)))
                  # opts
                  ((= (last (chop X)) ":")
                     (prin ", " X) )
                  ((pre? ":" Prev) (prin "," X))
                  # numbers
                  ((format Prev *Scl) (prin "," X))
                  (T (prin (unless (=1 I) " ") X)) ) ) )
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
                           ((member (car X) *SplFns)
                              (prog
                                 # (prin " ")
                                 (unless (=1 I) (prin " "))
                                 (spl-run-fn X) ) )
                           # blocks
                           ((member (car X) *SplBlockFns)
                              (prog
                                 # (prin " ")
                                 (unless (=1 I) (prin " "))
                                 (spl-run-block-fn X) ) )
                           # lisp, most likely
                           (T (eval X)) ) ) )
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
                              ((member (car X) *SplFns)
                                 (prog
                                    (prin " ")
                                    (spl-run-fn X)
                                    (unless (spl-opt? Prev) (prin ";")) ) )
                              ((member (car X) *SplBlockFns)
                                 (prog
                                    (prin " ")
                                    (spl-run-block-fn X)
                                    (prin ";") ) )
                              (T (eval X)) ) ) )
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

(de times Lst
   (let [(N . Rst) Lst]
      (prin N ".times do ")
      (spl-run-forms% Rst)
      (prin "end")))


# examples / tests
(de ex-basic-variable ()
   (spl%
      (= s (synth :prophet note: 60 release: 5 cutoff: 60))
      (sleep 1)
      (control s cutoff: 80)
      (sleep 1)
      (control s cutoff: "100") ) )  # bug with 0.01 and 100, etc

(de ex-ambient-experiment ()
   (spl%
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
   (spl%
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
   (spl%
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
   (spl%
      (= reich-notes (ring :E4 :Fs4 :B4))
      (live-loop :slower do
         (play reich-notes.tick release: 0.1)
         (sleep 0.3) )
      (live-loop :faster do
         (play reich-notes.tick release: 0.1)
         (sleep 0.296) ) ) )
