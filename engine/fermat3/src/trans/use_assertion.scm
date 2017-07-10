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
(define (@Use_Assertion_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Assert))
   (@Fail "The selected item is not an assertion."))
  ((not (@Right?))
   (@Fail "No statements after the assertion"))
  (#t
   (@Pass))))

(define (@Use_Assertion_Code //Data)
 (let ((//A (@Get_n (@I) 1)))
  (cond
   ((@Right?)
    (@Right)
    (@UA_Process //A)))))

; Try to simplify current and subsequent items using given assertion: 
(define (@UA_Process //A)
 (let ((//B '()))
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    ; Process the components of the currently selected item: 
    (cond
     ((or (= (@ST (@I)) //T_/Cond) (= (@ST (@I)) //T_/D_/If))
      (@Down)
      ; to first guarded 
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (set! //B (@Get_n (@I) 1))
        (cond
         ((@Implies? //A //B)
          (@Delete_Rest)
          (@Down_To 2)
          (@Down)
          (@UA_Process (@And //A //B))
          (@Up)
          (@Up)
          (set! /fl_flag1 1))
         ((= (@ST (@And //A //B)) //T_/False)
          (@Delete)
          ; Check if there are further guardeds: 
          (cond
           ((> (@Posn_n) (@Size (@Parent)))
            (set! /fl_flag1 1))
           (#t
            (set! /fl_flag1 0))))
         (#t
          (@Down)
          (@Paste_Over (@Simplify_Using (@I) //A 20))
          (@Right)
          (@Down)
          (let ((/posn (@Posn)))
           (@UA_Process (@And //A //B))
           (@Goto /posn))
          (@Up)
          (@Up)
          (cond
           ((not (@Right?))
            (set! /fl_flag1 1))
           (#t
            (@Right)
            (set! /fl_flag1 0)))))))
      (@Up)
      (cond
       ((not (@Cs? (@I)))
        (@Clever_Delete))
       ((= (@Size (@I)) 1)
        (@Splice_Over (@Cs (@Get_n (@Get_n (@I) 1) 2))))))
     ((= (@ST (@I)) //T_/While)
      (cond
       ((= (@ST (@And //A (@Get_n (@I) 1))) //T_/False)
        (@Paste_Over (@Skip)))
       (#t
        (@UA_While //A))))
     ((= (@ST (@I)) //T_/Assignment)
      (@Down)
      (cond
       ((null? (intersection-n (@Assigned (@I)) (@Used (@I))))
        (set! //B (@Make //T_/Equal '() (list (@Lvalue_To_Expn (@Get_n (@I) 1)) (@Get_n (@I) 2))))
        (cond
         ((@Implies? //A //B)
          (@Delete)))))
      (while (@Right?) 
       (begin
        (@Right)
        (cond
         ((null? (intersection-n (@Assigned (@I)) (@Used (@I))))
          (set! //B (@Make //T_/Equal '() (list (@Lvalue_To_Expn (@Get_n (@I) 1)) (@Get_n (@I) 2))))
          (cond
           ((@Implies? //A //B)
            (@Delete)))))))
      (@Up)
      (cond
       ((= (@Size (@I)) 0)
        (@Paste_Over (@Skip))))))
    ; Update assertion with the effect of executing the current item 
    (cond
     ((not (null? (intersection-n (@Assigned (@I)) (@Used //A))))
      (set! /fl_flag2 1))
     ((@Right?)
      (@Right)
      (set! /fl_flag2 0))
     (#t
      (set! /fl_flag2 1)))))))

; See if the assertion can simplify the WHILE loop 
; If no variables in the assertion are assigned in the loop, 
; or if the assertion is invariant over the loop, 
; then use the assertion on the loop body. 
(define (@UA_While //A)
 (cond
  ((null? (intersection-n (@Assigned (@I)) (@Used //A)))
   ; Variables in the assertion are not modified in the loop 
   (@Down)
   (@Paste_Over (@Simplify_Using (@I) //A 20))
   (@Right)
   (@Down)
   ; to first statement in body 
   (@UA_Process //A))
  ((@Set_Subset? (@Stat_Types (@Get_n (@I) 2)) //W/P_/Types_/Set)
   (cond
    ((@Implies? //A (@WP (@Get_n (@I) 2) //A))
     (@Down_To 2)
     (@Down)
     ; to first statement in body 
     (@UA_Process //A))))
  (#t
   ; Loop modifies the assertion, so we cannot process it 
  )))

#t
