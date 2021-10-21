(scl 4) # seems the best for sonic-pi

# NOTE - currently not using 'ruby-load' because it
# messes with the numbers (damnit picolisp)

# USAGE
#
# write picolisp ruby code in a normal '.l' file, then
#
#  : (ruby-load "file.l")
#  -> ((...)(...)...)
#
#  : (ruby-run @)
#  -> ... prints ruby code

(de ruby-load (File)
   (ruby-transform
      (ruby-read File) ) )

(def 'RUBY_READSTRING "._-+=!?<>$%*':/\|~[]\^")
# (def 'RUBY_READSTRING ".:_-|")

(de ruby-read (File)
   (in File
      (make
         (while (read RUBY_READSTRING)
            (link @) ) ) ) )

# transform pl ruby code to something the interpreter can understand
(de ruby-transform (Lst)
   (and
      (_transform Lst)
      (mapcar pack @)
      (glue " " @)
      (any @) ) )


(de _transform (X)
   (recur (X Acc)
      (ifn X
         (flip Acc)
         (case (car X)
            ("{" (recurse (cdr X) (cons (sym "{") Acc)))
            ("}" (recurse (cdr X) (cons (sym "}") Acc)))
            ###
            (T
               (cond
                  # negative numbers
                  ((= (car X) "-")
                     (if (num? (cadr X))
                        (recurse (cddr X) (cons (pack (car X) (cadr X)) Acc))
                        (recurse (cdr X) (cons (car X) Acc)) ) )
                  # (3.times do ...), etc
                  ((num? (car X))
                     (if (member  (cadr X) '(times upto downto step))
                        (recurse (cddr X) (cons (pack (car X) "." (cadr X)) Acc))
                        (recurse (cdr X) (cons (car X) Acc)) ) )
                  # "pass through" transient symbols
                  ((pair (car X))
                     (recurse (cdr X) (cons (sym (pack (car X))) Acc)) )
                  (T
                     (recurse (cdr X) (cons (car X) Acc)) ) ) ) ) ) ) )


(de ruby-syntax-check-file (File)
   (pipe
      (out '(ruby "-c")
         # (ruby-run (ruby-load File)) )
         (ruby-run (in File (read))) )
      (line T) ) )

(de ruby-test ()
   (ruby-syntax-check-file "ruby-test.l") )

# (ruby-test "(3.times { (play 60) (sleep 1) })"
#    '(3.times "{" (play 60) (sleep 1) "}") )



(de ruby-run (Xs)
   (for X (ruby-prep Xs)
      (cond
         ((= (car X) '=)
            (ruby-run-var X) )
         ((or (sub? "do" X) (sub? "{" X) (sub? "then" X) (sub? "when" X))
            (ruby-run-block
               # (if (sub? "each" X) (cons NIL X) X) ) )
               (if (pair (car X)) (cons NIL X) X) ) ) # first elem is an array
         (T (ruby-run-fn X)) )
      (prin ";") ) )

### ruby-prep -- the pl ruby preprocessor
#
# - numbers to strings (0.125 -> "0.125")
# - pl symbols to rb symbols (do-something -> do_something)

(de ruby-prep (X)
   (recur (X Acc)
      (ifn X
         (flip Acc)
            (cond
               # format numbers for printing (damnit picolisp)
               ((and (num? (car X)) (or (>= (length (car X)) *Scl) (< 131 (car X) 1000)))
                  (recurse (cdr X) (cons (format (car X) *Scl) Acc)) )
               # # attempt to sort out the 'ruby-transform' number issues. still broken
               # ((num? (car X))
               #    (let N (format (car X) *Scl)
               #       (cond
               #          ((sub? ".0000" N)
               #             (recurse (cdr X) (cons (any (pack (car (split (chop N) ".")))) Acc)) )
               #          ((or (>= (length (car X)) *Scl) (< 131 (car X) 1000))
               #             (recurse (cdr X) (cons (format (car X) *Scl) Acc)) )
               #          (T
               #             (recurse (cdr X) (cons (car X) Acc)) ) ) ) )
               ((str? (car X))
                  (recurse (cdr X) (cons (sym (pack (car X))) Acc)) )
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


# getting messy...
(de ruby-run-fn (Form)
   (let [(Car . Cdr) Form
         Opts NIL ] # flag for start of optional params
      (cond
         ((sub? "each" Form)
            (ruby-run-block (cons NIL Form)) )
         ((= Car 'if)
            (ruby-run-block Form) )
         (T
            (prog  # hack for inline 'if'
               (if (member 'if Form)
                  (prin (pl-to-rb Car) " ")
                  (prin (pl-to-rb Car) (char 40)) )
               (for (I . X) Cdr
                  (let [Next (car (nth Cdr (+ I 1)))     # formatting often depends on
                        Prev (car (nth Cdr (- I 1))) ]   # what comes before or after
                     (cond
                        ((pair X)
                           (cond
                              # prev .method? (eg. '[...].shuffle.take(3)')
                              ((and (atom Prev) (sub? "." Prev) (not (str? Prev)))
                                 (ruby-run-fn (cons NIL X)) )
                              # arrays
                              ((ruby-array? X)
                                 (prog
                                    (unless (=1 I) (prin (if Opts " " ",")))
                                    (prin "[" (glue "," X) "]") ) )
                              # variables
                              ((= (car X) '=)
                                 (prog
                                    (unless (=1 I) (prin " "))
                                    (ruby-run-var X)
                                    (prin ";") ) )
                              # blocks
                              ((or (sub? "do" X) (sub? "{" X))
                                 (prog
                                    (unless (=1 I) (prin " "))
                                    (ruby-run-block
                                       (if (sub? "each" X) (cons NIL X) X) ) ) )
                              ((sub? "each" X)
                                 (ruby-run-block (cons NIL X)) )
                              (T (prog
                                    (unless (=1 I)
                                       # (prin " ")
                                       (prin
                                          (if (or (ruby-opt? Prev) (member 'if Cdr))
                                             " "
                                             "," ) ) )
                                             (ruby-run-fn X) ) ) ) )
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
                        ((ruby-opt? Prev) (prin " " X))
                        (T (prin (unless (=1 I) " ") X) ) ) ) )
               (ifn (member 'if Form) (prin (char 41))) ) ) ) ) )

(de ruby-run-var (Form)
   (++ Form)  # discard '=' sym
   (let [(Car . Cdr) Form]
      (prog
         (prin (pl-to-rb Car) " = ")
         (for (I . X) Cdr
            (let [Next (car (nth Cdr (+ I 1)))
                  Prev (car (nth Cdr (- I 1))) ]
               (cond
                  ((pair X)
                     (cond
                        # prev .method? (eg. '[...].shuffle.take(3)')
                        ((and (atom Prev) (sub? "." Prev) (not (str? Prev)))
                           (ruby-run-fn (cons NIL X)) )
                        # arrays
                        ((ruby-array? X)
                           (prog
                              (if (ruby-array? Prev) (prin ", ") (prin " "))
                              (prin "[" (glue "," X) "]") ) )
                        # blocks
                        ((or (sub? "do" X) (sub? "{" X))
                           (prog
                              (unless (=1 I) (prin " "))
                              (ruby-run-block
                                 (if (sub? "each" X) (cons NIL X) X) ) ) )
                        (T
                           (prog
                              (unless (=1 I) (prin " "))
                              (ruby-run-fn X) ) ) ) )
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

(de ruby-run-block (Form)
   (let [(Car . Cdr) Form
         ArgsOn NIL
         Do NIL ]   # has {'do'|'then'|'when'} been encountered in current form?
      (prog
         (prin (pl-to-rb Car))
         (for (I . X) Cdr
            (let [Next (car (nth Cdr (+ I 1)))
                  Prev (car (nth Cdr (- I 1))) ]
               (when ArgsOn
                  (prin (if (member '= (list X Prev)) " " ",")) )
               (cond
                  ((pair X)
                     (cond
                        # prev .method? (eg. '[...].shuffle.take(3)')
                        ((and (atom Prev) (sub? "." Prev) (not (str? Prev)))
                           (ruby-run-fn (cons NIL X)) )
                        # arrays
                        ((ruby-array? X)
                           (prog
                              (if (ruby-array? Prev) (prin ", ") (prin " "))
                              (prin "[" (glue "," X) "]") ) )
                        # variables
                        ((= (car X) '=)
                           (prog
                              (unless (=1 I) (prin " "))
                              (ruby-run-var X)
                              (prin ";") ) )
                        # blocks
                        ((or (sub? "do" X) (sub? "{" X))
                           (prog
                              (prin " ")
                              (ruby-run-block
                                 (if (sub? "each" X) (cons NIL X) X) )
                              (prin ";") ) )
                        # functions
                        (T (prog
                              (prin " ")
                              (ruby-run-fn X)
                              (when Do
                                 (prin
                                    (if (or (ruby-opt? Prev) (member Next '(&& ||)))
                                       " "
                                       ";" ) ) ) ) ) ) )
                  ((member X '(do "{"))
                     (prog (on Do) (prin " do")) )
                  ((member X '(then when))
                     (prog (on Do) (prin " " X)) )
                  ((member X '("{" "}")) NIL)
                  # .methods
                  ((pre? "." X) (prin X))
                  # local arg list, e.g. |x| or |x y|
                  ((sub? "||" (pack (sort (chop X)))) (prin " " X))
                  ((pre? "|" X) (prog (on ArgsOn) (prin " " X)))
                  ((= (last (chop X)) "|") (prog (off ArgsOn) (prin " " X)))
                  # opts
                  ((= (last (chop X)) ":")
                     (prin ", " X) )
                  (T (prin " " X)) )
               ) )
         (if Do (prin " end")) ) ) )


# picolisp symbol to ruby symbol
(de pl-to-rb (Sym) (pack (replace (chop Sym) "-" "_")))

(de ruby-opt? (Sym)
   (= (last (chop Sym)) ":") )

(de ruby-array? (X)
   (and (pair X) (or (num? (car X)) (str? (car X)) (pre? ":" (car X)))) )

 # (eg. '[...].shuffle.take(3)')
 # NOTE - implicit 'Prev' arg, for use in 'for' loops above
(de ruby-prev-method? (X)
   (and
      (=1 (length X))
      (atom Prev)
      (sub? "." Prev)
      (not (str? Prev)) ) )

