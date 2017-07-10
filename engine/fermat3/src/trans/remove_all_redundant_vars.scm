;;; Scheme translation of WSL code
(define (/foreach-remove_all_redundant_vars-1 //Depth //A/S_/Type)
 (cond
  ((and (or (= (@ST (@I)) //T_/Var) (= (@ST (@I)) //T_/M/W_/Funct) (= (@ST (@I)) //T_/M/W_/B/Funct)) (@Trans? //T/R_/Remove_/Redundant_/Vars))
   (@Pass))))

(define (/foreach-remove_all_redundant_vars-2 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/Var) (= (@ST (@I)) //T_/M/W_/Funct) (= (@ST (@I)) //T_/M/W_/B/Funct))
   (cond
    ((@Trans? //T/R_/Remove_/Redundant_/Vars)
     (@Trans //T/R_/Remove_/Redundant_/Vars ""))))))

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
(define (@Remove_All_Redundant_Vars_Test)
 (cond
  ((and (not (= (@GT (@I)) //T_/Statement)) (not (= (@GT (@I)) //T_/Statements)))
   (@Fail "The selected item is not a statement or sequence "))
  (#t
   (@Ateach_Statement /foreach-remove_all_redundant_vars-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (cond
    ((not (@Passed?))
     (@Fail "No VAR statement has redundant variables"))))))

(define (@Remove_All_Redundant_Vars_Code //Data)
 (@Ateach_Statement /foreach-remove_all_redundant_vars-2 0 (@AS_Type) 0)
 (cond
  ((null? (@Program))
   (@New_Program (@Skips)))))

#t
