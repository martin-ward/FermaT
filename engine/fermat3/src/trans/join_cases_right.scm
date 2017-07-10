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
(define (@Join_Cases_Right_Test)
 (let ((/bad_types (@Make_Set (list //T_/X_/Proc_/Call //T_/Proc_/Call //T_/M/W_/Proc_/Call)))
       (/funct_types (@Make_Set (list //T_/Funct_/Call //T_/X_/Funct_/Call //T_/M/W_/Funct_/Call))))
  (cond
   ((not (= (@ST (@I)) //T_/Guarded))
    (@Fail "Current item is not a Guarded"))
   ((not (@Right?))
    (@Fail "There is no Guarded to the right of this one"))
   ((not (@Equal? (@Get_n (@Get_n (@I) 2) 1) (@Get_n (@Get_n (@Get_n (@Parent) (+ (@Posn_n) 1)) 2) 1)))
    (@Fail "Initial statements in the two clauses do not match"))
   ((not (null? (intersection-n (@Spec_Types (@Get_n (@I) 1)) /funct_types)))
    (@Fail "Function calls in first condition"))
   ((not (null? (intersection-n (@Spec_Types (@Get_n (@Get_n (@Parent) (+ (@Posn_n) 1)) 1)) /funct_types)))
    (@Fail "Function calls in second condition"))
   ((not (null? (intersection-n (@Assigned (@Get_n (@I) 2)) (@Used (@Get_n (@I) 1)))))
    (@Fail "Assigned variable is referenced in first condition"))
   ((not (null? (intersection-n (@Assigned (@Get_n (@I) 2)) (@Used (@Get_n (@Get_n (@Parent) (+ (@Posn_n) 1)) 1)))))
    (@Fail "Assigned variable is referenced in second condition"))
   ((or (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Skip) (not (null? (intersection-n /bad_types (@Stat_Types (@Get_n (@I) 2))))))
    (@Fail "Won't take out skips, can't take out proc calls"))
   (#t
    (@Pass)))))

(define (@Join_Cases_Right_Code //Data)
 (let ((//B1 (@Get_n (@I) 1))
       (//S1 (@Cs (@Get_n (@I) 2)))
       (//B2 '())
       (//S2 '())
       (//B '())
       (//S '()))
  (@Right)
  (set! //B2 (@Get_n (@I) 1))
  (set! //S2 (@Cs (@Get_n (@I) 2)))
  (@Delete)
  (@Left)
  (set! //B (@Or //B1 //B2))
  (@Paste_Over (@Make 7 '() (list //B (@Make 17 '() (list (@Make 114 '() (list (@Make 7 '() (list //B1 (@Make 17 '() //S1))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() //S2))))))))))
  (@Down_To 2)
  (@Down)
  (cond
   ((not (@Trans? //T/R_/Separate_/Left))
    (@Checkpoint "zzz-join_cases_right_error.wsl")
    (display-list "Posn = " (@Posn))
    (error "Separate_Left failed in @Join_Cases_Right_Code"))
   (#t
    (@Trans //T/R_/Separate_/Left "")))
  (@Up)
  (@Up)
  ; back to guarded 
 ))

#t
