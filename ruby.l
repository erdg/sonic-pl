(scl 4) # seems the best for sonic-pi

# NOTE - numbers are a bit funky. integers 100-999 should be written as e.g.
# '100.0' or '999.0'. best practice is to make everything a float (except for
# 'times' loops, etc)

# currently not using 'ruby-load' because numbers are even more buggy


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


# top level
(de ruby-run (Xs)
   (for X (ruby-prep Xs)
      (cond
         ((= (car X) '=)
            (ruby-run-var X) )
         ((ruby-block? X)
            (ruby-run-block
               (if (pair (car X))  # array first?
                  (cons NIL X)
                  X ) ) )
         ((ruby-array? X)
            (ruby-run-array X) )
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
               # format numbers for printing (damnit picolisp!)
               # this represents the best compromise
               ((and (num? (car X)) (or (>= (length (car X)) *Scl) (< 99 (car X) 1000)))
                  (recurse (cdr X) (cons (format (car X) *Scl) Acc)) )
               # transients symbols pass through
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

(de ruby-run-fn (Form)
   (let [(Car . Cdr) Form
         Opts NIL ] # flag for start of optional params
      (cond
         ((ruby-block? Form)
            (ruby-run-block
               (if (pair (car Form))
                  (cons NIL Form)
                  Form ) ) )
         (T
            (prog  # hack for inline modifiers
               (if (contains? '(if unless while until) Form)
                  (prin (pl-to-rb Car) " ")
                  (prin (pl-to-rb Car) (char 40)) )
               (for (I . X) Cdr
                  (let [Next (car (nth Cdr (+ I 1)))     # formatting often depends on
                        Prev (car (nth Cdr (- I 1))) ]   # what comes before or after
                     (cond
                        ((pair X)
                           (cond
                              # prev .method()
                              ((ruby-dot-method? Prev)
                                 (ruby-run-fn (cons NIL X)) )
                              # variables
                              ((= (car X) '=)
                                 (prog
                                    (unless (=1 I) (prin " "))
                                    (ruby-run-var X)
                                    (prin ";") ) )
                              ((ruby-block? X)
                                 (ruby-run-block
                                    (if (pair (car X))
                                       (cons NIL X)
                                       X ) ) )
                              ((ruby-array? X)
                                 (prog
                                    (unless (=1 I)
                                       (unless (= Prev 'get)
                                          (prin (if Opts " " ",")) ) )
                                    (ruby-run-array X) ) )
                              (T (prog
                                    (unless (=1 I)
                                       (prin
                                          (if
                                             (or
                                                (ruby-opt? Prev)
                                                (contains? '(if unless until while) Cdr)
                                                (member Prev '(* / + - %)) )
                                             " "
                                             "," ) ) )
                                    (ruby-run-fn X) ) ) ) )
                        # inline modifier
                        ((member X '(if unless when while))
                           (prog
                              (ifn Opts (on Opts))  # similar behavior, hijacking 'Opts' flg
                              (prin " " X) ) )
                        # .method()
                        ((ruby-dot-method? X) (prin X))
                        # opts
                        ((ruby-opt? X)
                           (prog
                              (ifn Opts (on Opts))
                              (prin (unless (=1 I) ", ") X) ) )
                        # prev .method()
                        ((ruby-dot-method? Prev)
                           (prin (if Opts " " ",") X) )
                        (Opts (prin " " X))
                        # arg list, e.g. |x y|
                        ((sub? "||" (pack (sort (chop X))))
                           (prin " " X) )
                        ((pre? "|" X)
                           (prog (on ArgsOn) (prin " " X)) )
                        ((= (last (chop X)) "|")
                           (prog (off ArgsOn) (prin " " X)) )
                        # ruby symbols
                        ((pre? ":" Prev) (prin "," X))
                        ((pre? ":" X) (prin (if Opts " " (unless (=1 I) ",")) X))
                        # numbers / maths
                        ((member X '(* / + - %)) (prin " " X))  # inline maths
                        ((format Prev *Scl) (prin "," X))
                        ((member Prev '(* / + - %)) (prin " " X))
                        ((and (format X *Scl) Prev (sym? Prev))
                           (prin "," X) )
                        ((ruby-opt? Prev) (prin " " X))
                        (T (prin (unless (=1 I) " ") X) ) ) ) )
               (ifn (contains? '(if unless while until) Form) (prin (char 41))) ) ) ) ) )

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
                        # prev .method()
                        ((ruby-dot-method? Prev)
                           (ruby-run-fn (cons NIL X)) )
                        # arrays
                        ((ruby-array? X)
                           (prog
                              (if (ruby-array? Prev) (prin ",") (prin " "))
                              (ruby-run-array X) ) )
                        # blocks
                        ((ruby-block? X)
                           (prog
                              (unless (=1 I) (prin " "))
                              (ruby-run-block
                                 (if (pair X) (cons NIL X) X) ) ) )
                        (T
                           (prog
                              (unless (=1 I) (prin " "))
                              (ruby-run-fn X) ) ) ) )
                  # .method()
                  ((pre? "." X) (prin X))
                  ((pre? "." Prev) (prin "," X))
                  # block arg list, e.g. |x y|
                  ((sub? "||" (pack (sort (chop X)))) (prin " " X))
                  ((pre? "|" X) (prog (on ArgsOn) (prin " " X)))
                  ((= (last (chop X)) "|") (prog (off ArgsOn) (prin " " X)))
                  # opts
                  ((ruby-opt? X)
                     (prin ", " X) )
                  # numbers
                  ((format Prev *Scl) (prin "," X))
                  (T (prin (unless (=1 I) " ") X)) ) ) ) ) ) )

(de ruby-run-array (Arr)
   (prin "[")
   (for (I . X) Arr
      (let [Next (car (nth Arr (+ I 1)))  # formatting often depends on
            Prev (car (nth Arr (- I 1)))  # what comes before or after
            Opts NIL ]
         (cond
            ((pair X)
               (cond
                  # prev .method? (eg. '[...].shuffle.take(3)')
                  ((ruby-dot-method? Prev)
                     (ruby-run-fn (cons NIL X)) )
                  # arrays
                  ((ruby-array? X)
                     (unless (=1 I) (prin ","))
                     (ruby-run-array X) )
                  (T (prog
                        (unless (=1 I)
                           (prin
                              (if (or (ruby-opt? Prev) (member 'if Arr))
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
            ((ruby-dot-method? Prev)
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
            ((format Prev *Scl) (prin "," X))
            ((member Prev '(* / + - %)) (prin " " X))
            ((and (format X *Scl) Prev (sym? Prev))
               (prin "," X) )
            ((ruby-opt? Prev) (prin " " X))
            (T (prin (unless (=1 I) " ") X) ) ) ) )
   (prin "]") )

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
                        # prev .method() call
                        ((ruby-dot-method? Prev)
                           (ruby-run-fn (cons NIL X)) )
                        # arrays
                        ((ruby-array? X)
                           (prog
                              (if (ruby-array? Prev) (prin ",") (prin " "))
                              (ruby-run-array X) ) )
                        # variables
                        ((= (car X) '=)
                           (prog
                              (unless (=1 I) (prin " "))
                              (ruby-run-var X)
                              (prin ";") ) )
                        # blocks
                        ((ruby-block? X)
                           (prog
                              (prin " ")
                              (ruby-run-block
                                 (if (pair (car X))
                                    (cons NIL X)
                                    X ) )
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
                  # .method() call
                  ((pre? "." X) (prin X))
                  # arg list, e.g. |x y|
                  ((sub? "||" (pack (sort (chop X)))) (prin " " X))
                  ((pre? "|" X) (prog (on ArgsOn) (prin " " X)))
                  ((= (last (chop X)) "|") (prog (off ArgsOn) (prin " " X)))
                  # opts
                  ((ruby-opt? X) (prin ", " X))
                  (T (prin " " X)) )
               ) )
         (if Do (prin " end")) ) ) )

# utils

# picolisp symbol to ruby symbol
(de pl-to-rb (Sym) (pack (replace (chop Sym) "-" "_")))

(de ruby-block? (Form)
   (contains? '(do "{" then else elseif) Form) )

(de ruby-opt? (Sym)
   (= (last (chop Sym)) ":") )

(de ruby-array? (X)
   (ifn (pair X)
      NIL
      (if (pair (car X))
         (ruby-array? @)
         (or (num? (car X)) (str? (car X)) (pre? ":" (car X))) ) ) )

# .method() call syntax (eg. 'something.take(3)')
(de ruby-dot-method? (X)
   (and
      (atom X)
      (sub? "." X)
      (not (str? X)) ) )

# are any members of L1 present in the top level of L2
(de contains? (L1 L2)
   (not
      (fully '((X) (= X NIL))
         (make
            (for L L1
               (link (member L L2)) ) ) ) ) )


# shell helper
#  (sh "ruby -c @1" (tmp "test.rb"))
(de sh (@Str . Rest)
   (macro
      (call 'sh "-c"
         (text @Str
            ^ Rest ) ) ) )


# tests
(de ruby-check-file (File)
   (pipe
      (out '(ruby "-c")
         # (ruby-run (ruby-load File)) )
         (ruby-run (in File (read))) )
      (line T) ) )

(de ruby-test ()
   (ruby-check-file "ruby-test.l") )

# interactive ruby
(de irb Xs
   (sh "ruby -e '@1'"
      (pipe
         (ruby-run Xs)
         (line T) ) ) )

(de ruby-unit-test (Result . Xs)
   (sh "ruby -e '@1' > @2"
      (pipe
         (ruby-run Xs)
         (line T) )
      (tmp "ruby-results") )
   (test Result
      (in (tmp "ruby-results")
         (make
            (until (eof)
               (link (line T)) ) ) ) ) )
