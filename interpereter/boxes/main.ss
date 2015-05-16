; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

(load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/chez-init.ss") 
;(load "chez-init.ss")

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/interpreter.ss")
    (load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/datatypes.ss")
    (load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/parse.ss")
    (load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/env.ss")
    
    (load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/datatypes.ss")
    (load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/parse.ss")
    (load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/env.ss")
    (load "C:/Users/lehmandr/Documents/PLCinterpreter/interpereter/interpreter.ss")
    
    ;(load "datatypes.ss")
    ;(load "parse.ss")
    ;(load "env.ss")
    ;(load "interpreter.ss")
    ;(load "datatypes.ss")
    ;(load "parse.ss")
    ;(load "env.ss")
    ;(load "interpreter.ss")
    ))

(load-all)

(define l load-all) ; even easier!



