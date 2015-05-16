; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                                   
; Claude Anderson.  Last modified April, 2014

;; Project modified by Tim Anderson, Daniel Lehman
;; Note:  this passes all tests when ran on our computers

(load "chez-init.ss") 
;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/chez-init.ss")

(define load-all ; make it easy to reload the files
    (lambda ()
        (load "datatypes.ss")
        ;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/datatypes.ss")
        (load "parse.ss")
        ;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/parse.ss")
        (load "env.ss")
        ;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/env.ss")
        (load "interpreter.ss")
        ;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/interpreter.ss")

        (load "datatypes.ss")
        ;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/datatypes.ss")
        (load "parse.ss")
        ;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/parse.ss")
        (load "env.ss")
        ;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/env.ss")
        (load "interpreter.ss")))
        ;(load "C:/Users/anderst4/Documents/Courses/CSSE304-2/PLCinterpreter/interpereter/boxes/interpreter.ss")))

(load-all)

(define l load-all) ; even easier!




