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


### tests
(test '(do_something 60 release: "1.5000") (spl-preprocess '(do-something 60 release: 1.5)))
