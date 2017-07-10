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
;Written by Tim.
;Almost entirely rewritten by Eddy, Apr. 1997.
(define (@Apply_To_Right_Test)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (= (@GT (@I)) //T_/Statement))
   (@Fail "Selected item is not a statement."))
  ((not (@Right?))
   (@Fail "There is no following statement."))
  ((and (not (= (@Spec_Type (@I)) //T_/Assert)) (not (@Trans? //T/R_/Add_/Assertion)))
   (@Fail "The item was not an assertion and it was not possible to add one."))
  (#t
   (@Edit_Parent)
   (cond
    ((not (= (@ST (@I)) //T_/Assert))
     (@Trans //T/R_/Add_/Assertion "")))
   (let ((//B1 (@Get_n (@I) 1)))
    (@Right)
    (cond
     ((or (= (@ST (@I)) //T_/Cond) (= (@ST (@I)) //T_/D_/If))
      (@Apply_To_Right_Test_Cond //B1))
     ((= (@ST (@I)) //T_/D_/Do)
      (@Apply_To_Right_Test_D_Do //B1))
     ((= (@ST (@I)) //T_/While)
      (cond
       ((@Implies? //B1 (@Not (@Get_n (@I) 1)))
        (@Pass))
       (#t
        (@Fail "The assertion does not affect the condition."))))
     (#t
      (@Fail "The following statement is not of a suitable type.")))
    (@Left)
    (@Undo_Edit)))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (@Apply_To_Right_Test_Cond //B1)
 (@Down)
 ;Consider each guard in turn until there are no more clauses.   
 ;For each clause determine whether the assertion allows the     
 ;guard's condition to be simplified.                            
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (begin
   (@Down)
   (cond
    ((or (@Implies? //B1 (@I)) (@Implies? //B1 (@Not (@I))) (= (@ST (@And //B1 (@I))) //T_/False))
     (@Pass)
     (@Up)
     (set! /fl_flag1 1))
    (#t
     (@Up)
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))))
 (@Up)
 (cond
  ((not (@Passed?))
   (@Fail "Application of the selected item causes no change"))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (@Apply_To_Right_Test_D_Do //B1)
 ; If the item implies that all the guards in a D_Do are false when the   
 ; loop is entered, (i.e. none will be executed) the loop is equivalent to SKIP 
 (@Down)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (begin
   (@Down)
   (cond
    ((or (@Implies? //B1 (@Not (@I))) (= (@ST (@And //B1 (@I))) //T_/False))
     (@Up)
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))
    (#t
     (@Fail "At least one guard cannot be shown to be false")
     (@Up)
     (set! /fl_flag1 1)))))
 (@Up)
 (cond
  ((not (@Failed?))
   (@Pass))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (@Apply_To_Right_Code //Data)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (let ((//B1 '()))
  (cond
   ((= (@ST (@I)) //T_/Assert)
    (set! //B1 (@Get_n (@I) 1))
    (@Right))
   (#t
    (@Trans //T/R_/Add_/Assertion "")
    (set! //B1 (@Get_n (@I) 1))
    (@Delete)))
  (cond
   ((or (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/D_/Do))
    (@Paste_Over (@Make //T_/Skip '() '())))
   (#t
    ; Item is not a WHILE or D_Do, so it must be a Cond or a D_If
    (@Down)
    (@Down)
    (cond
     ((@Implies? //B1 (@I))
      (@Paste_Over (@Make //T_/True '() '())))
     ((or (= //T_/False (@ST (@And //B1 (@I)))) (@Implies? //B1 (@Not (@I))))
      (@Paste_Over (@Make //T_/False '() '()))))
    (@Up)
    (while (@Right?) 
     (begin
      (@Right)
      (@Down)
      (cond
       ((@Implies? //B1 (@I))
        (@Paste_Over (@Make //T_/True '() '())))
       ((or (= //T_/False (@ST (@And //B1 (@I)))) (@Implies? //B1 (@Not (@I))))
        (@Paste_Over (@Make //T_/False '() '()))))
      (@Up)))
    (@Up)
    (cond
     ((@Trans? //T/R_/Simplify_/Item)
      (@Trans //T/R_/Simplify_/Item "")))))
  (@Left)))

#t
