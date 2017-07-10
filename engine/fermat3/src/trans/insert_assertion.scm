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
(define (@Insert_Assertion_Test)
 (cond
  ((or (= (@ST (@I)) //T_/Cond) (= (@ST (@I)) //T_/D_/If) (= (@ST (@I)) //T_/D_/Do) (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/Guarded))
   (@Pass))
  (#t
   (@Fail "The selected item is not of a valid type."))))

(define (@Insert_Assertion_Code //Data)
 (let ((//A '())
       (//Else_/Cond (@Make //T_/False '() '())))
  (cond
   ((= (@ST (@I)) //T_/Cond)
    (@Down)
    (while (@Right?) 
     (begin
      (@Down)
      (set! //A (@I))
      (@Right)
      (@Down)
      (@Paste_Before (@Make 109 '() (list //A)))
      (set! //Else_/Cond (@Or //Else_/Cond //A))
      (@Up)
      (@Up)
      (@Right)))
    (@Down_Last)
    (@Down)
    (set! //Else_/Cond (@Not //Else_/Cond))
    (@Paste_Before (@Make 109 '() (list //Else_/Cond)))
    (@Up)
    (@Up)
    (@Up))
   ((or (= (@ST (@I)) //T_/D_/If) (= (@ST (@I)) //T_/D_/Do))
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (@Down)
      (set! //A (@I))
      (@Right)
      (@Down)
      (@Paste_Before (@Make 109 '() (list //A)))
      (@Up)
      (@Up)
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0))))))
   (#t
    (@Down)
    (set! //A (@I))
    (@Right)
    (@Down)
    (@Paste_Before (@Make 109 '() (list //A)))))))

#t
