;;; Scheme translation of WSL code
(define (/foreach-all_proc_stacks_to_pars-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Down_To 2)
   (@Down)
   ; to first defn 
   (cond
    ((= //T_/Proc (@ST (@I)))
     (cond
      ((@Trans? //T/R_/Stack_/To_/Par)
       (@Pass)))))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((= //T_/Proc (@ST (@I)))
       (cond
        ((@Trans? //T/R_/Stack_/To_/Par)
         (@Pass)))))))
   (@Up)
   (@Up))))

(define (/foreach-all_proc_stacks_to_pars-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Down_To 2)
   (@Down)
   ; to first defn 
   (cond
    ((= //T_/Proc (@ST (@I)))
     (cond
      ((@Trans? //T/R_/Stack_/To_/Par)
       (@Trans //T/R_/Stack_/To_/Par "")))))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((= //T_/Proc (@ST (@I)))
       (cond
        ((@Trans? //T/R_/Stack_/To_/Par)
         (@Trans //T/R_/Stack_/To_/Par "")))))))
   (@Up)
   (@Up))))

;
;==========================================================================
;FermaT Transformation System
;Copyright (C) 2015 Software Migrations Limited.
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
(define (@All_Proc_Stacks_To_Pars_Test)
 (@Foreach_Statement /foreach-all_proc_stacks_to_pars-1 0 (@AS_Type) 0)
 (cond
  ((null? (@Program))
   (@New_Program (@Skips))))
 (cond
  ((not (@Passed?))
   (@Fail "No suitable procedure definitions found"))))

(define (@All_Proc_Stacks_To_Pars_Code //Data)
 (@Foreach_Statement /foreach-all_proc_stacks_to_pars-2 0 (@AS_Type) 0)
 (cond
  ((null? (@Program))
   (@New_Program (@Skips)))))

#t
