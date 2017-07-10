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
(define (@Substitute_And_Delete_List_Test)
 (cond
  ((= (@Spec_Type (@Item)) //T_/Action)
   (@Pass))
  (#t
   (@Fail "Selected item is not an Action."))))

(define (@Substitute_And_Delete_List_Code //Data)
 (let ((//Orig_/Pos (@Posn))
       (/as '())
       (/span (@Span))
       (/n (last-1 (@Posn))))
  (@Up)
  (@Up)
  (set! /as (@Posn))
  (for /i /span 0 (- 1) 
   (begin
    (@Goto /as)
    (@Down_Last)
    (@Down_To (+ /n /i))
    (display-list "Testing action " (@V (@Get_n (@Item) 1)))
    (cond
     ((and (@Trans? //T/R_/Substitute_/And_/Delete) (= (@Call_Freq (@V (@Get_n (@Item) 1)) (@Get (@Program) /as)) 1))
      (display-list "Deleting " (@V (@Get_n (@Item) 1)))
      (@Trans //T/R_/Substitute_/And_/Delete "")))))))

