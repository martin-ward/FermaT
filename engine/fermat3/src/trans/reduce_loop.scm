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
(define (@Reduce_Loop_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Floop))
   (@Fail "Selected item is not a DO...OD loop."))
  ((@Gen_Reducible? (@Get_n (@I) 1) (@AS_Type))
   (@Fail "Loop body is already reducible."))
  (#t
   (@Pass))))

(define (@Reduce_Loop_Code //Data)
 (let ((/as (@AS_Type))
       (/posn (@Posn))
       (/calls (@MP_Proc_Calls))
       (/call_n 1)
       (/orig (@Program)))
  (@Down)
  ; to statement sequence 
  (let ((/-result- (@MR_Process  /as /calls /call_n)))
   (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-)))
  (@Up)
  ; If the loop body is now reducible, then remove or convert the loop 
  (cond
   ((@Gen_Reducible? (@Get_n (@I) 1) /as)
    (cond
     ((@Gen_Improper? (@Get_n (@I) 1) /as)
      (@Trans //T/R_/Remove_/Dummy_/Loop ""))
     ((@Set_Subset? (@Gen_TVs (@I) /as) (list 0))
      ; Loop is equivalent to ABORT! 
     )
     (#t
      (display-list "The loop is neither proper nor improper.")))
    (cond
     ((@Trans? //T/R_/Delete_/All_/Skips)
      (@Trans //T/R_/Delete_/All_/Skips ""))))
   (#t
    (display-list "Unable to reduce the loop, sorry.")
    (@New_Program /orig)
    (@Goto /posn)))))

#t
