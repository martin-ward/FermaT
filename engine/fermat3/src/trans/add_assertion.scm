;;; Scheme translation of WSL code
(define /%const__add_assertion__1 (@Make 6 '() (list (@Make 506 -1 '()) (@Make 217 -2 '()))))
(define /%const__add_assertion__2 (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()) (@Make 109 '() (list (@Make 305 -3 '()))))))))
(define /%const__add_assertion__3 (@Make 109 '() (list (@Make 309 '() '()))))
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
(define (@Add_Assertion_Test)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   ;We can add an assertion after an assignment 
   ; which assigns to some new variables.
   (cond
    ((not (null? (@Set_Difference (@Assigned (@I)) (@Used (@I)))))
     (@Pass))
    (#t
     (@Fail "The assignment should assign to new variables."))))
  ((= (@ST (@I)) //T_/Cond)
   ;We can add an assertion after an `If' statement if the guards of the
   ;latter either end with assertions or do not terminate (are improper).
   (let ((//Found 0))
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((not (@Is_Improper?))
        (@Down_Last)
        (@Down_Last)
        (cond
         ((= (@ST (@I)) //T_/Assert)
          (set! //Found 1))
         (#t
          (set! //Found 2)))
        (@Up)
        (@Up)))
      (cond
       ((= //Found 2)
        (set! /fl_flag1 1))
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)
    (cond
     ((= //Found 1)
      (@Pass))
     (#t
      (@Fail "No reachable assertion in the `IF' statement.")))))
  ((or (= (@ST (@I)) //T_/Assert) (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/Abort))
   (@Pass))
  ((= (@GT (@I)) //T_/Condition)
   (cond
    ((and (or (= (@ST (@Parent)) //T_/Assert) (= (@ST (@Parent)) //T_/Guarded) (= (@ST (@Parent)) //T_/While)) (not (= (@ST (@I)) //T_/True)) (not (= (@ST (@I)) //T_/False)))
     (@Pass))
    (#t
     (@Fail "Condition is not in a guard or `While' loop."))))
  ((and (= (@GT (@I)) //T_/Statement) (not-member 0 (@TVs)))
   (@Pass))
  (#t
   (@Fail "Cannot add an assertion after the selected type of item."))))

(define (@Add_Assertion_Code //Data)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (let ((//A '())
       (//B '())
       (//C '())
       (//S '())
       (//Assd_/Vars (@Assigned (@I))))
  (cond
   ((= (@ST (@I)) //T_/Assignment)
    (@Down)
    (set! //A (@Make //T_/True '() '()))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__add_assertion__1 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__/E_save //E)
               (/__/V_save //V))
          (set! //E (vector-ref /__/Match_array 1))
          (set! //V (vector-ref /__/Match_array 0))
          (cond
           ((null? (intersection-n (@Used (@I)) //Assd_/Vars))
            (set! //A (@And //A (@Make 313 '() (list (@Var_To_Expn //V) (@Var_To_Expn //E)))))))
          (set! //E /__/E_save)
          (set! //V /__/V_save)))))
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)
    (@Paste_After (@Make 109 '() (list //A)))
    (@Right))
   ((= (@ST (@I)) //T_/Cond)
    (@Down)
    (set! //A (@Make //T_/False '() '()))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((not (@Is_Improper?))
        (let ((/__/O/K 1))
         (set! /__/O/K (@New_Match  /%const__add_assertion__2 (@I) /__/O/K))
         (cond
          ((= /__/O/K 1)
           (let ((/__/C_save //C)
                 (/__/S_save //S)
                 (/__/B_save //B))
            (set! //C (vector-ref /__/Match_array 2))
            (set! //S (vector-ref /__/Match_array 1))
            (set! //B (vector-ref /__/Match_array 0))
            (set! //A (@Or //A //C))
            (set! //C /__/C_save)
            (set! //S /__/S_save)
            (set! //B /__/B_save)))))))
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)
    (@Paste_After (@Make 109 '() (list //A)))
    (@Right))
   ((= (@ST (@I)) //T_/While)
    (@Down)
    (set! //A (@Not (@I)))
    (@Up)
    (@Paste_After (@Make 109 '() (list //A)))
    (@Right))
   ((= (@ST (@I)) //T_/Assert)
    (@Paste_After (@I))
    (@Right))
   ((= (@ST (@I)) //T_/Abort)
    (@Paste_After /%const__add_assertion__3)
    (@Right))
   ((and (= (@GT (@I)) //T_/Condition) (not (= (@ST (@I)) //T_/True)) (not (= (@ST (@I)) //T_/False)))
    (set! //A (@I))
    (@Up)
    (cond
     ((= (@ST (@I)) //T_/Assert)
      (@Paste_After (@Make 109 '() (list //A)))
      (@Right))
     (#t
      (@Down_Last)
      (@Down)
      (@Paste_Before (@Make 109 '() (list //A))))))
   ((and (= (@GT (@I)) //T_/Statement) (not-member 0 (@TVs)))
    (set! //B (@Make //T_/False '() '()))
    (@Paste_After (@Make 109 '() (list //B)))
    (@Right)))))

