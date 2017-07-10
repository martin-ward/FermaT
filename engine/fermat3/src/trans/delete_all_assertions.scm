;;; Scheme translation of WSL code
(define (/foreach-delete_all_assertions-1 //Depth //A/S_/Type)
 (cond
  ((= (@Spec_Type (@Item)) //T_/Assert)
   (@Delete))))

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
(define (@Delete_All_Assertions_Test)
 (cond
  ((member //T_/Assert (@Stat_Types (@Item)))
   (@Pass))
  (#t
   (@Fail "The selected item did not include a `ASSERT' statement."))))

(define (@Delete_All_Assertions_Code //Data)
 (@Foreach_Statement /foreach-delete_all_assertions-1 0 (@AS_Type) 0)
 (cond
  ((null? (@Program))
   (@New_Program (@Skips)))))

