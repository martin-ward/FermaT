;;; Scheme translation of WSL code
(define (/foreach-remove_dummy_loop-1 //Depth //A/S_/Type)
 ; NB: Process the statements from left to right so as to avoid 
 ; absorbing in (and missing) some statements which need processing. 
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

(define (/foreach-remove_dummy_loop-2 //Depth //A/S_/Type)
 (@Down)
 (while (and (not (@Gen_Improper? (@I) //A/S_/Type)) (@Right?)) 
  (@Right))
 (cond
  ((@Gen_Improper? (@I) //A/S_/Type)
   (cond
    ((@Right?)
     (@Delete_Rest))))))

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
(define (@Remove_Dummy_Loop_Test)
 (let ((//A/S (@AS_Type)))
  (cond
   ((not (= (@Spec_Type (@Item)) //T_/Floop))
    (@Fail "Not a do-loop"))
   ((not (@Gen_Improper? (@Get_n (@I) 1) //A/S))
    (@Fail "Loop body is not improper"))
   ((@Gen_Can_Reduce? (@Get_n (@I) 1) //A/S)
    (@Pass))
   (#t
    (@Fail "Body cannot be easily reduced")))))

(define (@Remove_Dummy_Loop_Code //Data)
 (cond
  ((not (@Is_Dummy?))
   ; Need to do some absorbing first 
   (display-list-flush " Using absorption to reduce loop body...")
   (@Foreach_Stats /foreach-remove_dummy_loop-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (display-list "")))
 (@Foreach_Stats /foreach-remove_dummy_loop-2 0 (@AS_Type) 0)
 (cond
  ((null? (@Program))
   (@New_Program (@Skips))))
 (@Splice_Over (@Cs (@Increment (@Get_n (@I) 1) (@AS_Type) (- 1) 1))))

#t
