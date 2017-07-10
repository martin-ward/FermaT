;;; Scheme translation of WSL code
(define (/foreach-force_double_to_single_loop-1 //Depth //A/S_/Type)
 (@Down)
 (cond
  ((and (member 1 (@TVs)) (@Trans? //T/R_/Fully_/Absorb_/Right))
   (@Trans //T/R_/Fully_/Absorb_/Right "")))
 (while (@Right?) 
  (begin
   (@Right)
   (cond
    ((and (member 1 (@TVs)) (@Trans? //T/R_/Fully_/Absorb_/Right))
     (@Trans //T/R_/Fully_/Absorb_/Right ""))))))

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
(define (@Force_Double_To_Single_Loop_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Floop))
   (@Fail "Not a do-loop"))
  ((@Trans? //T/R_/Double_/To_/Single_/Loop)
   (@Fail "Can be done more simply"))
  (#t
   (@Down)
   (@Down)
   (cond
    ((and (= (@ST (@I)) //T_/Floop) (not (@Right?)))
     (@Pass))
    (#t
     (@Fail "Not a double loop"))))))

(define (@Force_Double_To_Single_Loop_Code //Data)
 ;Down to inner loop
 (@Down)
 (@Down)
 (cond
  ((@Trans? //T/R_/Delete_/Unreachable_/Code)
   (@Trans //T/R_/Delete_/Unreachable_/Code "")))
 ;Down to loop body (Statement_s)
 (@Down)
 (while (not (@Is_Reducible?)) 
  (begin
   ;Up to inner loop
   (@Up)
   (let ((//A/S/Type (@AS_Type)))
    (@Foreach_Stats /foreach-force_double_to_single_loop-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips)))))
   ;Back down to statement_s
   (@Down)))
 (@Up)
 (@Splice_Over (@Cs (@Increment (@Get_n (@I) 1) (@AS_Type) (- 1) 1)))
 (@Up)
 (@Up))

