;;; Scheme translation of WSL code
(define (/foreach-double_to_single_loop-1 //Depth //A/S_/Type)
 (cond
  ((member //Depth (@Gen_TVs (@I) //A/S/Type))
   (set! //N (+ //N 1)))))

(define (/foreach-double_to_single_loop-2 //Depth //A/S_/Type)
 (cond
  ((and (= //Depth 0) (= //R 1))
   (@Down)
   ; to first statement 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (member 1 (@Gen_TVs (@I) //A/S/Type)) (@Right?))
       ; If the next statement is an EXIT, then we are OK 
       (@Right)
       (cond
        ((and (= (@ST (@I)) //T_/Exit) (> (@V (@I)) 0))
         (set! //N (- 1)))
        (#t
         (set! //N 0)))
       (@Left)
       (cond
        ((= //N 0)
         (cond
          ((= (@ST (@I)) //T_/Cond)
           (@GCR_Cond_Fix)))
         (@Ateach_Terminal /foreach-double_to_single_loop-1 0 (@AS_Type) 1)
         (cond
          ((null? (@Program))
           (@New_Program (@Skips))))
         (cond
          ((> //N 1)
           (set! //R 0)
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0))))
        (#t
         (set! /fl_flag1 0))))
      (#t
       (set! /fl_flag1 0)))
     (cond
      ((= /fl_flag1 0)
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))))
   (@Up))))

(define (/foreach-double_to_single_loop-3 //Depth //A/S_/Type)
 ; NB: Process the statements from left to right 
 ; so as to avoid absorbing in (and missing) 
 ; some statements which need processing. 
 (@Down_Last)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (begin
   (cond
    ((and (= //Depth 1) (member 1 (@TVs)) (@Trans? //T/R_/Fully_/Absorb_/Right))
     (display-list-flush "a")
     (cond
      ((= (@ST (@I)) //T_/Cond)
       (@GCR_Cond_Fix)))
     (@Trans //T/R_/Fully_/Absorb_/Right "")))
   (cond
    ((not (@Left?))
     (set! /fl_flag1 1))
    (#t
     (@Left)
     (set! /fl_flag1 0))))))

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
(define (@Double_To_Single_Loop_Test)
 (let ((//A/S (@AS_Type)))
  (cond
   ((not (and (= (@ST (@I)) //T_/Floop) (= (@Size (@Get_n (@I) 1)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Floop)))
    (@Fail "Not a double loop"))
   ((@Gen_Can_Reduce? (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1) //A/S)
    (@Pass))
   (#t
    (@Fail "Body cannot be easily reduced")))))

; Return TRUE if the given item can be reduced: 
(define (@Gen_Can_Reduce? //I //A/S/Type-par)
 (let ((//A/S/Type-save //A/S/Type)
       (//N-save //N)
       (//R-save //R)
       (funct-result '()))
  (set! //A/S/Type //A/S/Type-par)
  (set! //N 0)
  (set! //R 1)
  (cond
   ((@Gen_Reducible? //I //A/S/Type)
    (set! //R 1))
   (#t
    (@Edit)
    (@New_Program //I)
    (@Ateach_Stats /foreach-double_to_single_loop-2 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Undo_Edit)))
  (set! funct-result (= //R 1))
  (set! //A/S/Type //A/S/Type-save)
  (set! //N //N-save)
  (set! //R //R-save)
  funct-result))

(define (@Double_To_Single_Loop_Code //Data)
 (cond
  ((@Trans? //T/R_/Remove_/Dummy_/Loop)
   (@Trans //T/R_/Remove_/Dummy_/Loop ""))
  (#t
   (let ((//A/S (@AS_Type)))
    (@Down)
    (@Down)
    ; to inner loop 
    (cond
     ((@Trans? //T/R_/Remove_/Dummy_/Loop)
      (@Trans //T/R_/Remove_/Dummy_/Loop ""))
     (#t
      (cond
       ((@Trans? //T/R_/Delete_/Unreachable_/Code)
        (@Trans //T/R_/Delete_/Unreachable_/Code "")))
      (cond
       ((not (@Gen_Reducible? (@Get_n (@I) 1) //A/S))
        (display-list-flush " Using absorption to reduce loop body...")
        (@Ateach_Stats /foreach-double_to_single_loop-3 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))
        (display-list "")))
      (@Splice_Over (@Cs (@Increment (@Get_n (@I) 1) //A/S (- 1) 1)))
      (@Up)
      (@Up)))))))

; Fix a Cond statement to be more suitable for absorbing 
(define (@GCR_Cond_Fix)
 (cond
  ((and (= (@ST (@I)) //T_/Cond) (> (@Size (@I)) 2) (member 1 (@Gen_TVs (@Get_n (@I) 1) //A/S/Type)) (@Gen_Proper? (@Get_n (@I) 2) //A/S/Type) (@Gen_Proper? (@Get_n (@I) 3) //A/S/Type))
   (cond
    ((and (@Right?) (= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Exit) (> (@V (@Get_n (@Parent) (+ (@Posn_n) 1))) 0))
     #t)
    ((@Trans? //T/R_/Elsif_/To_/Else_/If)
     ; Combine the ELSIF clauses so that we can absorb 
     (@Trans //T/R_/Elsif_/To_/Else_/If "")
     (@Down_To 2)
     (@Down_Last)
     (@Down_Last)
     (@Paste_After (@Skip))
     (@Up)
     (@Up)
     (@Up))))))

#t
