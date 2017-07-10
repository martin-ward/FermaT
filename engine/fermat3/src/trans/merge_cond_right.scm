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
; Merge a Cond with a subsequent one which uses the same (or inverted) test 
; provided the variables in the test are not modified 
(define (@Merge_Cond_Right_Test)
 ; Look for a pair of binary IF statements with the same (or opposite) test 
 ; where the code in the first statement doesn't change the test. 
 (cond
  ((not (= (@ST (@I)) //T_/Cond))
   (@Fail "Current item is not a Cond (IF statement)"))
  ((not (= (@Size (@I)) 2))
   (@Fail "IF statement must have exactly two branches"))
  ((and (null? (intersection-n (@Variables (@Get_n (@Get_n (@I) 1) 1)) (union-n (@Assigned (@Get_n (@Get_n (@I) 1) 2)) (@Assigned (@Get_n (@Get_n (@I) 2) 2))))) (null? (intersection-n //Call_/Types_/Set (@Stat_Types (@I)))))
   (let ((/n (@Posn_n))
         (//B (@Get_n (@Get_n (@I) 1) 1))
         (/not/B (@Not (@Get_n (@Get_n (@I) 1) 1))))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((not (@Right?))
       (@Fail "No suitable Cond found")
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (cond
        ((or (= (@ST (@I)) //T_/Comment) (= (@ST (@I)) //T_/Skip))
         (set! /fl_flag1 0))
        ((and (= (@ST (@I)) //T_/Cond) (= (@Size (@I)) 2))
         (cond
          ((or (@Equal? /not/B (@Get_n (@Get_n (@I) 1) 1)) (@Equal? //B (@Get_n (@Get_n (@I) 1) 1)))
           (@Pass))
          (#t
           (@Fail "The next Cond does not have the same (or opposite) test")))
         (set! /fl_flag1 1))
        (#t
         (@Fail "No suitable Cond found")
         (set! /fl_flag1 1))))))
    (@To /n)))
  (#t
   (@Fail "The condition could be modified in the body"))))

(define (@Merge_Cond_Right_Code //Data)
 (let ((//B (@Get_n (@Get_n (@I) 1) 1))
       (/not/B (@Not (@Get_n (@Get_n (@I) 1) 1)))
       (/n (@Posn_n))
       (//S1 '())
       (//S2 '()))
  (@Right)
  (while (not (= (@ST (@I)) //T_/Cond)) 
   (@Right))
  (cond
   ((@Equal? //B (@Get_n (@Get_n (@I) 1) 1))
    (set! //S1 (@Cs (@Get_n (@Get_n (@I) 1) 2)))
    (set! //S2 (@Cs (@Get_n (@Get_n (@I) 2) 2))))
   ((@Equal? /not/B (@Get_n (@Get_n (@I) 1) 1))
    (set! //S2 (@Cs (@Get_n (@Get_n (@I) 1) 2)))
    (set! //S1 (@Cs (@Get_n (@Get_n (@I) 2) 2))))
   (#t
    (error "@Merge_Cond_Right_Code: next Cond has a different condition!")))
  (cond
   ((not (null? //S1))
    (@Delete)
    (@To /n)
    (@Down)
    (@Down_Last)
    (@Down_Last)
    ; to end of THEN 
    (@Splice_After //S1)
    (@Up)
    (@Up)
    (@Right)
    (@Down_Last)
    (@Down_Last)
    ; to end of ELSE 
    (@Splice_After //S2)
    (@Up)
    (@Up)
    (@Up)
    ; back to IF 
   ))))

#t
