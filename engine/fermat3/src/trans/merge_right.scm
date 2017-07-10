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
(define (@Merge_Right_Test)
 (cond
  ((and (= (@Gen_Type (@Item)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (= (@Gen_Type (@Item)) //T_/Statement))
   (@Fail "The selected item is not a Statement."))
  ((not (@Right?))
   (@Fail "There is no statement to the Right of this one."))
  (#t
   (@Right)
   (cond
    ((@Trans? //T/R_/Absorb_/Left)
     (@Pass))
    ((= (@Spec_Type (@Item)) //T_/Assignment)
     ;Can we merge two assignment?
     (let ((//A (@Assigned (@Item)))
           (//U (@Used (@Item))))
      (@Left)
      (cond
       ((and (= (@Spec_Type (@Item)) //T_/Assignment) (null? (intersection-n //A (@Variables (@Item)))) (null? (intersection-n //U (@Assigned (@Item)))))
        (@Pass))
       (#t
        (@Fail "The assignments were too complex (for this version).")))))
    (#t
     (@Fail "It was not possible to `Absorb Left' on the previous statement."))))))

(define (@Merge_Right_Code //Data)
 (cond
  ((and (= (@Gen_Type (@Item)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (@Right)
 (cond
  ((@Trans? //T/R_/Absorb_/Left)
   (@Trans //T/R_/Absorb_/Left ""))
  (#t
   ;We must be merging two assignments.
   (@Left)
   (let ((//C (@Components (@Item))))
    (@Delete)
    (@Down)
    (@Splice_Before //C)
    (@Up)))))

