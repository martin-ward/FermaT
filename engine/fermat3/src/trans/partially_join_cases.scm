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
(define (@Partially_Join_Cases_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Cond))
   (@Fail "Not an IF statement"))
  (#t
   (@Down)
   ; to first guarded 
   (let ((//S1 (last-1 (@Cs (@Get_n (@I) 2))))
         (//S2 '()))
    (@Right)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (set! //S2 (last-1 (@Cs (@Get_n (@I) 2))))
      (cond
       ((and (not (= (@ST //S1) //T_/Comment)) (@Equal? //S1 //S2))
        (@Pass)
        (set! /fl_flag1 1))
       (#t
        (set! //S1 //S2)
        (cond
         ((not (@Right?))
          (@Fail "The `If' statement does not have two adjacent, and partially repeated, guards.")
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0)))))))))))

(define (@Partially_Join_Cases_Code //Data)
 (let ((//G1 '())
       (//G/G '())
       (//S1 '())
       (//S/S '())
       (/posn (@Posn))
       (//B '())
       (//B1 '())
       (//S '())
       (/n 0)
       (/tmp '())
       (/add_assertions 0))
  (cond
   ((> (@String_To_Num (@String //Data)) 0)
    (set! /add_assertions 1)))
  (@Down)
  ; to first guarded 
  (set! //S1 (last-1 (@Cs (@Get_n (@I) 2))))
  (set! //G1 (@I))
  (set! /n (@Posn_n))
  (@Right)
  (while (or (= //T_/Comment (@ST //S1)) (not (@Equal? //S1 (last-1 (@Cs (@Get_n (@I) 2)))))) 
   (begin
    (set! //S1 (last-1 (@Cs (@Get_n (@I) 2))))
    (set! //G1 (@I))
    (set! /n (@Posn_n))
    (@Right)))
  ; See how many more guardeds have S1 as their last statement: 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (set! //G/G (concat //G/G (list (@I))))
    (cond
     ((not (@Right?))
      (@Delete)
      (set! /fl_flag1 1))
     (#t
      (@Delete)
      (cond
       ((not (@Equal? //S1 (last-1 (@Cs (@Get_n (@I) 2)))))
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0)))))))
  (@To /n)
  ; Calculate B and put the reversed statement lists into SS, taking off S1: 
  (set! //B (@Get_n //G1 1))
  (set! //S/S (list (cdr (reverse (@Cs (@Get_n //G1 2))))))
  (for-in //G //G/G 
   (begin
    (set! //B (@Or //B (@Get_n //G 1)))
    (set! //S/S (cons (cdr (reverse (@Cs (@Get_n //G 2)))) //S/S))))
  (set! //S/S (reverse //S/S))
  ; Put the common statements into S 
  (set! //S (list //S1))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (set! /tmp //S/S)
    (cond
     ((null? (car //S/S))
      (set! /fl_flag1 1))
     (#t
      (set! //S1 (car (car //S/S)))
      (while (and (not (null? /tmp)) (not (null? (car /tmp))) (@Equal? (car (car /tmp)) //S1)) 
       (set! /tmp (cdr /tmp)))
      (cond
       ((and (not (null? /tmp)) (null? (car /tmp)))
        (set! /fl_flag1 1))
       ((and (not (null? /tmp)) (not (@Equal? (car (car /tmp)) //S1)))
        (set! /fl_flag1 1))
       (#t
        ; Add S1 to S and remove from the front of each element of SS 
        (set! //S (cons //S1 //S))
        (set! /tmp //S/S)
        (set! //S/S '())
        (while (not (null? /tmp)) 
         (begin
          (set! //S/S (cons (cdr (car /tmp)) //S/S))
          (set! /tmp (cdr /tmp))))
        (set! //S/S (reverse //S/S))
        (set! /fl_flag1 0)))))))
  ; Construct the inner guardeds from GG and SS 
  (set! /tmp (cons //G1 //G/G))
  (set! //G/G '())
  (while (not (null? /tmp)) 
   (begin
    (cond
     ((null? (cdr /tmp))
      (set! //B1 (@Make //T_/True '() '())))
     (#t
      (set! //B1 (@Get_n (car /tmp) 1))))
    (set! //B1 (@Simplify_Cond (@Mth_Simplify_Using //B1 //B 20)))
    (cond
     ((= /add_assertions 1)
      (set! //S1 (cons (@Make //T_/Assert '() (list (@Get_n (car /tmp) 1))) (reverse (car //S/S)))))
     (#t
      (set! //S1 (reverse (car //S/S)))))
    (cond
     ((null? //S1)
      (set! //S1 (list (@Skip)))))
    (set! //G/G (cons (@Make //T_/Guarded '() (list //B1 (@Make //T_/Statements '() //S1))) //G/G))
    (set! /tmp (cdr /tmp))
    (set! //S/S (cdr //S/S))))
  (set! //G/G (reverse //G/G))
  (@Paste_Over (@Make //T_/Guarded '() (list //B (@Make //T_/Statements '() (cons (@Make //T_/Cond '() //G/G) //S)))))
  (@Down)
  ; to the inner IF 
  (@Trans //T/R_/Simplify_/Item "")
  (@Goto /posn)
  (@Fix_Cond)
  (@Trans //T/R_/Simplify_/Item "")))

(define (@Partially_Join_Cases_Code_orig //Data)
 (let ((//G1 '())
       (//B1 '())
       (//B2 '())
       (//S1 '())
       (//S2 '())
       (//B '())
       (//S '())
       (/posn (@Posn)))
  (@Down)
  ; to first guarded 
  (set! //S1 (last-1 (@Cs (@Get_n (@I) 2))))
  (set! //G1 (@I))
  (@Right)
  (set! //S2 (last-1 (@Cs (@Get_n (@I) 2))))
  (while (not (@Equal? //S1 //S2)) 
   (begin
    (set! //S1 //S2)
    (set! //G1 (@I))
    (@Right)
    (set! //S2 (last-1 (@Cs (@Get_n (@I) 2))))))
  (@Pass)
  ; G1 contains the guardeds up to and including the first one to be joined. 
  ; The next guarded is selected 
  (set! //B1 (@Get_n //G1 1))
  (set! //B2 (@Get_n (@I) 1))
  (set! //B (@Or //B1 //B2))
  ; Put the common statements into S 
  (set! //S1 (reverse (@Cs (@Get_n //G1 2))))
  (set! //S2 (reverse (@Cs (@Get_n (@I) 2))))
  (while (and (not (null? //S1)) (not (null? //S2)) (@Equal? (car //S1) (car //S2))) 
   (begin
    (set! //S (cons (car //S1) //S))
    (set! //S1 (cdr //S1))
    (set! //S2 (cdr //S2))))
  (set! //S1 (reverse //S1))
  (set! //S2 (reverse //S2))
  ; Delete this guarded and modify the previous one: 
  (@Delete)
  (@Left)
  (@Down)
  (@Paste_Over //B)
  (@Right)
  (@Paste_Over (@Make 17 '() (append (list (@Make 114 '() (list (@Make 7 '() (list //B1 (@Make 17 '() (append (list (@Make 109 '() (list //B1))) //S1)))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (append (list (@Make 109 '() (list //B2))) //S2))))))) //S)))
  (@Down)
  ; to the inner IF 
  (@Trans //T/R_/Simplify_/Item "")
  (@Goto /posn)
  (@Fix_Cond)
  (@Trans //T/R_/Simplify_/Item "")))

#t
