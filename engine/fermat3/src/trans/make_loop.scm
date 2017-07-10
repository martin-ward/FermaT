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
(define (@Make_Loop_Test)
 (cond
  ((and (not (= (@GT (@I)) //T_/Statement)) (not (= (@GT (@I)) //T_/Statements)))
   (@Fail "Selected item is not a statement or statement sequence"))
  ((and (not (equal? (@AS_Type) "Reg")) (member //T_/Call (@Stat_Types (@I))))
   (@Fail "Item contains calls in a non-regular system"))
  (#t
   (@Pass))))

(define (@Make_Loop_Code //Data)
 (let ((//S (@Increment (@I) (@AS_Type) 1 0)))
  (cond
   ((= (@GT (@I)) //T_/Statements)
    (set! //S (@Cs //S))
    (@Paste_Over (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() //S)))))))
   (#t
    (@Paste_Over (@Make 133 '() (list (@Make 17 '() //S))))))))

#t
