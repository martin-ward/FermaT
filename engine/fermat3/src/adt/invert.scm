;;; Scheme translation of WSL code
(define (/foreach-invert-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (member (@V (@I)) /vars))
   (for-in /assign /assigns 
    (cond
     ((and (= (@ST (@Get_n /assign 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n /assign 1)) (@V (@I))))
      (set! /new (@Invert (@I) (@V (@Get_n /assign 1)) (@Get_n /assign 2))))))
   (cond
    ((null? /new)
     (@Print_WSL (@Make //T_/Assignment '() /assigns) "")
     (@Print_WSL (@I) "")
     (error "@Invert_All: cannot invert!"))
    (#t
     (@Paste_Over /new))))))

(define /%const__invert__1 (@Make 220 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 205 1 '()))))
(define /%const__invert__2 (@Make 220 '() (list (@Make 205 1 '()) (@Make 207 (@Make_Name "v") '()))))
(define /%const__invert__3 (@Make 221 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 205 1 '()))))
(define /%const__invert__4 (@Make 221 '() (list (@Make 205 1 '()) (@Make 207 (@Make_Name "v") '()))))
(define /%const__invert__5 (@Make 221 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 207 (@Make_Name "a") '()) (@Make 207 (@Make_Name "b") '()) (@Make 207 (@Make_Name "c") '()))))
(define /%const__invert__6 (@Make 221 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 207 (@Make_Name "b") '()) (@Make 207 (@Make_Name "v") '()) (@Make 207 (@Make_Name "c") '()))))
(define /%const__invert__7 (@Make 220 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 207 (@Make_Name "b") '()) (@Make 207 (@Make_Name "v") '()) (@Make 207 (@Make_Name "c") '()))))
(define /%const__invert__8 (@Make 222 '() (list (@Make 205 2 '()) (@Make 207 (@Make_Name "v") '()))))
(define /%const__invert__9 (@Make 222 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 207 (@Make_Name "a") '()) (@Make 207 (@Make_Name "b") '()))))
(define /%const__invert__10 (@Make 223 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 207 (@Make_Name "a") '()) (@Make 207 (@Make_Name "b") '()))))
(define /%const__invert__11 (@Make 223 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 207 (@Make_Name "v") '()) (@Make 207 (@Make_Name "b") '()) (@Make 207 (@Make_Name "c") '()))))
(define /%const__invert__12 (@Make 220 '() (list (@Make 222 '() (list (@Make 205 2 '()) (@Make 207 (@Make_Name "v") '()))) (@Make 205 1 '()))))
(define /%const__invert__13 (@Make 220 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 222 '() (list (@Make 205 2 '()) (@Make 220 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 207 (@Make_Name "b") '()))))) (@Make 207 (@Make_Name "c") '()))))
(define /%const__invert__14 (@Make 220 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 207 (@Make_Name "b") '()))))
(define /%const__invert__15 (@Make 251 '() (list (@Make 207 (@Make_Name "v") '()))))
(define /%const__invert__16 (@Make 223 '() (list (@Make 205 1 '()) (@Make 207 (@Make_Name "v") '()))))
(define /%const__invert__17 (@Make 223 '() (list (@Make 205 1 '()) (@Make 221 '() (list (@Make 205 1 '()) (@Make 223 '() (list (@Make 205 1 '()) (@Make 221 '() (list (@Make 205 1 '()) (@Make 207 (@Make_Name "v") '()))))))))))
(define /%const__invert__18 (@Make 223 '() (list (@Make 205 1 '()) (@Make 220 '() (list (@Make 222 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 205 2 '()))) (@Make 205 1 '()))))))
(define /%const__invert__19 (@Make 220 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 222 '() (list (@Make 205 2 '()) (@Make 207 (@Make_Name "v") '()))))))
(define /%const__invert__20 (@Make 220 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 207 (@Make_Name "v") '()))))
(define /%const__invert__21 (@Make 221 '() (list (@Make 205 1 '()) (@Make 222 '() (list (@Make 207 (@Make_Name "v") '()) (@Make 205 2 '()))))))
(define /%const__invert__22 (@Make 207 (@Make_Name "v") '()))
; Implementation of @Invertible?(assigns) and @Invert(x, v, exp) 
; @Invert(x, v, exp), where exp contains one occurrence of v 
; returns an expression exp2 which inverts the effect of exp, such that: 
; x := exp; x := exp2 is equivalent to SKIP 
; In other words, exp with v replaced by exp2 will simplify to x 
; If this is not possible, then @Invert returns < > 
; For example, @Invert(x, v, 2*v - 1) returns (x + 1)/2 
; @Invert(x, v, 3 - 2*v) returns (3 - x)/2 
; Note: this only works with simple variables. We can extend it to 
; more general expressions by first replacing the expression 
; with a simple variable. 
; x is an item, v is a symbol and exp is an expression. 
(define (@Invert /x /v-par /exp-par)
 (let ((/exp-save /exp)
       (/v-save /v)
       (//R '())
       (/pos 0)
       (//S/T (@ST /exp-par))
       (funct-result '()))
  (set! /exp /exp-par)
  (set! /v /v-par)
  (cond
   ((= //S/T //T_/Variable)
    (cond
     ((equal? (@V /exp) /v)
      (set! //R /x))))
   ((and (= //S/T //T_/Concat) (= (@ST (@Get_n /exp 1)) //T_/Sequence) (= (@Size /exp) 2))
    (set! /pos (@Invert_Find_Var /v (@Cs /exp)))
    (cond
     ((= /pos 2)
      (let ((/new-save /new))
       (set! /new /x)
       (for /i 1 (@Size (@Get_n (@Get_n /exp 1) 1)) 1 
        (set! /new (@Make //T_/Tail '() (list /new))))
       (set! //R (@Invert /new /v (@Get_n /exp 2)))
       (set! /new /new-save)))))
   ((and (not (= //S/T //T_/Plus)) (not (= //S/T //T_/Minus)) (not (= //S/T //T_/Times)) (not (= //S/T //T_/Divide)) (not (= //S/T //T_/Negate)) (not (= //S/T //T_/Invert)))
    (set! //R '()))
   (#t
    (set! /pos (@Invert_Find_Var /v (@Cs /exp)))
    (cond
     ((> /pos 0)
      (let ((/new-save /new)
            (/rest (concat (@Get_L /exp 1 (- /pos 1)) (@Get_L /exp (+ /pos 1) (@Size /exp)))))
       (set! /new '())
       ; The first component of a T_Minus or T_Divide is special 
       (cond
        ((= //S/T //T_/Plus)
         (set! /new (@Make //T_/Minus '() (cons /x /rest))))
        ((and (= //S/T //T_/Minus) (= /pos 1))
         (set! /new (@Make //T_/Plus '() (cons /x /rest))))
        ((and (= //S/T //T_/Minus) (> /pos 1))
         (set! /new (@Make //T_/Minus '() (cons (car /rest) (cons /x (cdr /rest))))))
        ((= //S/T //T_/Times)
         (set! /new (@Make //T_/Divide '() (cons /x /rest))))
        ((and (= //S/T //T_/Divide) (= /pos 1))
         (set! /new (@Make //T_/Times '() (cons /x /rest))))
        ((and (= //S/T //T_/Divide) (> /pos 1))
         (set! /new (@Make //T_/Divide '() (cons (car /rest) (cons /x (cdr /rest))))))
        ((= //S/T //T_/Negate)
         (set! /new (@Make //T_/Negate '() (list /x))))
        ((= //S/T //T_/Invert)
         (set! /new (@Make //T_/Invert '() (list /x)))))
       (set! //R (@Invert /new /v (@Get_n /exp /pos)))
       (set! /new /new-save))))))
  (set! funct-result //R)
  (set! /exp /exp-save)
  (set! /v /v-save)
  funct-result))

; Find the component which contains v 
; If there isn't one, return 0. 
; If there are more than one, return -1. 
; otherwise, return the position of the component. 
(define (@Invert_Find_Var /v-par /comps)
 (let ((/v-save /v)
       (/pos 0)
       (/comp '())
       (/i 0)
       (funct-result '()))
  (set! /v /v-par)
  (for-in /comp /comps 
   (begin
    (set! /i (+ /i 1))
    (cond
     ((member /v (@Variables /comp))
      (cond
       ((= /pos 0)
        (set! /pos /i))
       (#t
        (set! /pos (- 1))))))))
  (set! funct-result /pos)
  (set! /v /v-save)
  funct-result))

; Check that all assignments to any of the given vars are invertible 
(define (@Invertible? /vars-par /assigns-par)
 (let ((/assigns-save /assigns)
       (/vars-save /vars)
       (//O/K 1)
       (/assign-save /assign)
       (/x (@Make //T_/Variable (@Make_Name "x") '()))
       (funct-result '()))
  (set! /assigns /assigns-par)
  (set! /vars /vars-par)
  (set! /assign '())
  (while (and (= //O/K 1) (not (null? /assigns))) 
   (begin
    (set! /assign (car /assigns))
    (set! /assigns (cdr /assigns))
    (cond
     ((and (not (null? (intersection-n (@Assigned (@Get_n /assign 1)) /vars))) (not (= (@ST (@Get_n /assign 1)) //T_/Var_/Lvalue)))
      (set! //O/K 0))
     ((and (member (@V (@Get_n /assign 1)) /vars) (null? (@Invert /x (@V (@Get_n /assign 1)) (@Get_n /assign 2))))
      (set! //O/K 0)))))
  (set! funct-result (= //O/K 1))
  (set! /assigns /assigns-save)
  (set! /vars /vars-save)
  (set! /assign /assign-save)
  funct-result))

; Replace each assigned variable in the current item with its inverse expression 
(define (@Invert_All /vars-par /assigns-par)
 (let ((/assigns-save /assigns)
       (/vars-save /vars))
  (set! /assigns /assigns-par)
  (set! /vars /vars-par)
  (@Foreach_Global_Var /foreach-invert-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /assigns /assigns-save)
  (set! /vars /vars-save)))

(cond
 (#f
  (set! /tests (list /%const__invert__1 /%const__invert__2 /%const__invert__3 /%const__invert__4 /%const__invert__5 /%const__invert__6 /%const__invert__7 /%const__invert__8 /%const__invert__9 /%const__invert__10 /%const__invert__11 /%const__invert__12 /%const__invert__13 /%const__invert__14 /%const__invert__15 /%const__invert__16 /%const__invert__17 /%const__invert__18 /%const__invert__19 /%const__invert__20 /%const__invert__21 /%const__invert__22))
  (for-in /exp /tests 
   (begin
    (display-list-flush "x := ")
    (@New_Program /exp)
    (@Checkpoint "")
    (set! /exp (@Invert (@Make //T_/Variable (@Make_Name "x") '()) (@Make_Name "v") /exp))
    (cond
     ((null? /exp)
      (display-list "-- not invertible"))
     (#t
      (@Paste_Over /exp)
      (display-list-flush "v := ")
      (@Checkpoint "")))
    (display-list "")))
  #t))
#t
