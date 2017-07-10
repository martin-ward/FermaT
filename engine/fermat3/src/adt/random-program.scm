;;; Scheme translation of WSL code
;
;==========================================================================
;FermaT Transformation System
;Copyright (C) 2001 Software Migrations Limited.
;Email: martin@gkc.org.uk
;
;This program is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;==========================================================================
; ----------------------------------------------------------------------- 
; The following functions are used to generate random programs and to     
; apply random transformations to them at random positions.               
; ----------------------------------------------------------------------- 
;
;       @New_Program(@CRP_Item(T_Statements, 5, 0));
;       @Clear_State;
; 
(define (@CRP_Item //Type //Dep //In_/A_/S)
 (let ((//Result '())
       (//S 0)
       (//Seq '())
       (//Syn '())
       (//Val '()))
  (cond
   ((= //Type //T_/Statement)
    (set! //Result (@CRP_Stat //Dep //In_/A_/S)))
   ((= //Type //T_/Lvalue)
    (set! //Result (@CRP_Lvalue //Dep)))
   ((= //Type //T_/Expression)
    (set! //Result (@CRP_Expn 3)))
   ((= //Type //T_/Condition)
    (set! //Result (@CRP_Cond 3)))
   (#t
    (set! //Syn (@Syntax //Type))
    (cond
     ((@List_Type? //Type)
      (set! //S (+ (@Random 5) 1))
      (for //I 1 //S 1 
       (set! //Seq (cons (@CRP_Item (car //Syn) (- //Dep 1) //In_/A_/S) //Seq))))
     (#t
      (while (not (null? //Syn)) 
       (begin
        (set! //Seq (concat //Seq (list (@CRP_Item (car //Syn) (- //Dep 1) //In_/A_/S))))
        (set! //Syn (cdr //Syn))))))
    (set! //Result (@Make //Type '() //Seq))))
  //Result))

(define (@CRP_Stat //Dep //In_/A_/S)
 (let ((//Result '())
       (//R (@Random (if (> //Dep 0) 20 8)))
       (//S 0)
       (//Seq '())
       (//Temp '()))
  (cond
   ((= //R 1)
    (set! //Result (@Make //T_/Skip '() '())))
   ((= //R 2)
    (set! //Result (@Make //T_/Exit (@Random 4) '())))
   ((= //R 3)
    (cond
     ((= //In_/A_/S 1)
      (set! //Result (@Make //T_/Call (@Make //T_/Name (@CRP_Name //Dep) '()) '())))
     (#t
      (set! //Result (@Make //T_/Skip '() '())))))
   ((<= //R 8)
    (set! //Result (@CRP_Item (@Pick (list //T_/Abort //T_/Assert //T_/Assignment //T_/Pop //T_/Push)) (- //Dep 1) //In_/A_/S)))
   ((<= //R 14)
    (set! //Result (@CRP_Item (@Pick (list //T_/Cond //T_/Cond //T_/Floop //T_/Var //T_/While)) (- //Dep 1) //In_/A_/S)))
   ((= //R 15)
    (set! //Result (@Make //T_/Comment "comment..." '())))
   ((= //R 16)
    (set! //Result (@Make //T_/For '() (list (@Make //T_/Var_/Lvalue (@CRP_Name //Dep) '()) (@CRP_Expn //Dep) (@CRP_Expn //Dep) (@CRP_Expn //Dep) (@CRP_Item //T_/Statements (- //Dep 1) //In_/A_/S)))))
   ((= //R 17)
    (set! //Result (@Make (@Pick (list //T_/Proc_/Call //T_/A_/Proc_/Call)) '() (list (@Make //T_/Name (@CRP_Name //Dep) '()) (@Make //T_/Expressions '() (@CRP_Expns (- //Dep 1) 0 4)) (@Make //T_/Lvalues '() (@CRP_Lvalues (- //Dep 1) 0 4))))))
   ((= //R 18)
    (set! //Result (@Make //T_/X_/Proc_/Call '() (list (@Make //T_/Name (@CRP_Name //Dep) '()) (@Make //T_/Expressions '() (@CRP_Expns (- //Dep 1) 0 4))))))
   ((= //R 19)
    (let ((/names (list "A1" "B2" "C3" "D4" "E5" "F6"))
          (/body '())
          (//S (@Random 6)))
     (for //I 1 //S 1 
      (begin
       (set! /body (cons (@Make //T_/Action '() (list (@Make //T_/Name (@Make_Name (car /names)) '()) (@CRP_Item //T_/Statements (- //Dep 1) 1))) /body))
       (set! /names (cdr /names))))
     (set! //Result (@Make //T_/A_/S '() (list (@Make //T_/Name (@Make_Name "A1") '()) (@Make //T_/Actions '() /body))))))
   ((= //R 20)
    (let ((/names (list "A1" "B2" "C3" "D4" "E5" "F6"))
          (/body '())
          (//S (@Random 6)))
     (for //I 1 //S 1 
      (begin
       (set! /body (cons (@Make //T_/Proc '() (list (@Make //T_/Name (@Make_Name (car /names)) '()) (@Make //T_/Lvalues '() (@CRP_Lvalues (- //Dep 1) 0 4)) (@Make //T_/Lvalues '() (@CRP_Lvalues (- //Dep 1) 0 4)) (@CRP_Item //T_/Statements (- //Dep 1) //In_/A_/S))) /body))
       (set! /names (cdr /names))))
     (set! //Result (@Make //T_/Where '() (list (@CRP_Item //T_/Statements (- //Dep 1) //In_/A_/S) (@Make //T_/Definitions '() /body)))))))
  //Result))

(define (@CRP_Name //Dep)
 
 (@Make_Name (@Pick (list "A1" "B2" "C3" "D4" "E5" "F6"))))

(define (@CRP_Lvalue //Dep)
 (let ((//Result '())
       (//R (@Random (if (< //Dep 0) 5 10))))
  (cond
   ((<= //R 5)
    (set! //Result (@CRP_Var //T_/Var_/Lvalue)))
   ((<= //R 7)
    (set! //Result (@Make //T_/Aref_/Lvalue '() (list (@CRP_Var //T_/Var_/Lvalue) (@Make //T_/Expressions '() (@CRP_Expns (- //Dep 1) 1 3))))))
   ((= //R 8)
    (set! //Result (@Make //T_/Struct_/Lvalue (@CRP_Name (- //Dep 1)) (list (@CRP_Var //T_/Var_/Lvalue)))))
   ((= //R 9)
    (set! //Result (@Make //T_/Sub_/Seg_/Lvalue '() (list (@CRP_Var //T_/Var_/Lvalue) (@CRP_Expn (- //Dep 1)) (@CRP_Expn (- //Dep 1))))))
   (#t
    (set! //Result (@Make //T_/Rel_/Seg_/Lvalue '() (list (@CRP_Var //T_/Var_/Lvalue) (@CRP_Expn (- //Dep 1)) (@CRP_Expn (- //Dep 1)))))))
  //Result))

; Generate an array or structure reference, or a MOD: 
(define (@CRP_Array //Dep)
 (let ((//Result '())
       (//R (@Random 7)))
  (cond
   ((<= //R 2)
    (set! //Result (@Make //T_/Aref '() (list (@CRP_Var //T_/Variable) (@Make //T_/Expressions '() (@CRP_Expns (- //Dep 1) 1 3))))))
   ((<= //R 4)
    (set! //Result (@Make //T_/Struct '() (list (@Make //T_/Name (@CRP_Name (- //Dep 1)) '()) (@CRP_Var //T_/Variable)))))
   ((= //R 5)
    (set! //Result (@Make //T_/Sub_/Seg '() (list (@CRP_Var //T_/Variable) (@CRP_Expn (- //Dep 1)) (@CRP_Expn (- //Dep 1))))))
   ((= //R 6)
    (set! //Result (@Make //T_/Mod '() (list (@CRP_Expn (- //Dep 1)) (@CRP_Expn (- //Dep 1))))))
   (#t
    (set! //Result (@Make //T_/Rel_/Seg '() (list (@CRP_Var //T_/Variable) (@CRP_Expn (- //Dep 1)) (@CRP_Expn (- //Dep 1)))))))
  //Result))

; Possibly empty sequences of items: 
(define (@CRP_Expns //Dep /from /to)
 (let ((//R (- (+ /from (@Random (+ (- /to /from) 1))) 1))
       (//Result '()))
  (while (> //R 0) 
   (begin
    (set! //Result (cons (@CRP_Expn //Dep) //Result))
    (set! //R (- //R 1))))
  //Result))

(define (@CRP_Lvalues //Dep /from /to)
 (let ((//R (- (+ /from (@Random (+ (- /to /from) 1))) 1))
       (//Result '()))
  (while (> //R 0) 
   (begin
    (set! //Result (cons (@CRP_Lvalue //Dep) //Result))
    (set! //R (- //R 1))))
  //Result))

(define (@CRP_Conds //Dep /from /to)
 (let ((//R (- (+ /from (@Random (+ (- /to /from) 1))) 1))
       (//Result '()))
  (while (> //R 0) 
   (begin
    (set! //Result (cons (@CRP_Cond //Dep) //Result))
    (set! //R (- //R 1))))
  //Result))

(define (@CRP_Var /type)
 
 (@Make /type (@Make_Name (@Pick (list "A" "B" "C" "I" "J" "X" "Y"))) '()))

(define (@CRP_Expn //Dep)
 (let ((//Result '())
       (//R (@Random (if (> //Dep 0) 8 3)))
       (//Temp '()))
  (cond
   ((= //R 1)
    (set! //Result (@CRP_Var //T_/Variable)))
   ((= //R 2)
    (set! //Result (@Make //T_/Number (@Pick (list 0 1 2 3 4 5 10 20 50 100)) '())))
   ((= //R 3)
    (set! //Result (@Make //T_/String (@Pick (list "foo" "bar" "baz")) '())))
   ((= //R 4)
    (set! //Result (@Make (@Pick (list //T_/Plus //T_/Minus //T_/Times //T_/Divide //T_/Max //T_/Min //T_/Exponent)) '() (@CRP_Expns //Dep 2 5))))
   ((= //R 5)
    (set! //Result (@Make (@Pick (list //T_/Abs //T_/Sgn //T_/Int //T_/Frac //T_/Negate //T_/Invert)) '() (list (@CRP_Expn (- //Dep 1))))))
   ((= //R 6)
    (set! //Result (@Make (@Pick (list //T_/X_/Funct_/Call //T_/Funct_/Call)) '() (list (@Make //T_/Name (@CRP_Name //Dep) '()) (@Make //T_/Expressions '() (@CRP_Expns (- //Dep 1) 0 4))))))
   ((= //R 7)
    (set! //Result (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() (@CRP_Expns (- //Dep 1) 0 4))))))
   (#t
    (set! //Result (@CRP_Array //Dep))))
  //Result))

(define (@CRP_Cond //Dep)
 (let ((//Result '())
       (//R (@Random (if (> //Dep 0) 12 6))))
  (cond
   ((<= //R 6)
    (set! //Result (@Make (@Pick (list //T_/Equal //T_/Not_/Equal //T_/Less //T_/Greater //T_/Less_/Eq //T_/Greater_/Eq)) '() (list (@CRP_Expn (- //Dep 1)) (@CRP_Expn (- //Dep 1))))))
   ((<= //R 7)
    (set! //Result (@Make //T_/Not '() (list (@CRP_Cond (- //Dep 1))))))
   ((<= //R 9)
    (set! //Result (@Make (@Pick (list //T_/And //T_/Or)) '() (@CRP_Conds (- //Dep 1) 2 5))))
   ((<= //R 11)
    (set! //Result (@Make (@Pick (list //T_/X_/B/Funct_/Call //T_/B/Funct_/Call)) '() (list (@Make //T_/Name (@CRP_Name //Dep) '()) (@Make //T_/Expressions '() (@CRP_Expns (- //Dep 1) 0 4))))))
   (#t
    (set! //Result (@Make (@Pick (list //T_/True //T_/False)) '() '()))))
  //Result))

; Pick a random element from the given sequence: 
(define (@Pick /seq)
 
 (wsl-ref /seq (@Random (gen-length /seq))))

; ----------------------------------------------------------------------- 

