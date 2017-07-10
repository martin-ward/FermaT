;;; Scheme translation of WSL code
(define (/foreach-var_pars_to_val_pars-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Proc_/Call)
   (cond
    ((not (null? (gethash /new_vals (@V (@Get_n (@I) 1)))))
     (set! /new (concat (@Cs (@Get_n (@I) 2)) (my-map @Lvalue_To_Expn (@Cs (@SS_Filter (@Get_n (@I) 3) (gethash /new_vals (@V (@Get_n (@I) 1))))))))
     (@Down_To 2)
     (@Paste_Over (@Make //T_/Expressions '() /new)))))))

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
(define (@Var_Pars_To_Val_Pars_Test)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Pass))
  (#t
   (@Fail "Select a WHERE clause to process all the procedures."))))

(define (@Var_Pars_To_Val_Pars_Code //Data)
 (let ((/new_vals-save /new_vals)
       (/extra '())
       (/new-save /new))
  (set! /new_vals (hash-table))
  (set! /new '())
  (@Down_Last)
  (@Down)
  ; to first declaration 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (cond
     ((= (@ST (@I)) //T_/Proc)
      (set! /extra (@Set_Difference (@Elements (@Get_n (@I) 3)) (@Elements (@Get_n (@I) 2))))
      ; Record which var pars need to be copied: 
      (puthash /new_vals (@V (@Get_n (@I) 1)) (@SS_Keep (@Cs (@Get_n (@I) 3)) /extra))
      (set! /new (concat (@Cs (@Get_n (@I) 2)) (@Cs (@SS_Filter (@Get_n (@I) 3) (gethash /new_vals (@V (@Get_n (@I) 1)))))))
      (@Down_To 2)
      (@Paste_Over (@Make //T_/Lvalues '() /new))
      (@Up)))
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  ; Now fix all the proc calls correspondingly 
  (@Up)
  (@Up)
  (@Foreach_Statement /foreach-var_pars_to_val_pars-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /new_vals /new_vals-save)
  (set! /new /new-save)))

#t
