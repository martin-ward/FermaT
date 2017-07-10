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
(set! //T/R_/Delete_/Redundant_/Statement (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/Delete_/Redundant_/Statement 1) "Delete Redundant Statement")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Delete_/Redundant_/Statement 1) "Delete_Redundant_Statement")
(vector-set! //T/Rs_/Test (- //T/R_/Delete_/Redundant_/Statement 1) (funct (@Delete_Redundant_Statement_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Delete_/Redundant_/Statement 1) (funct (@Delete_Redundant_Statement_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Delete_/Redundant_/Statement 1) (list "Delete"))
(vector-set! //T/Rs_/Help (- //T/R_/Delete_/Redundant_/Statement 1) "Delete Redundant Statement checks whether
the current statement is `Redundant' (because it calls nothing external and the variables
it modifies will all be assigned again before their values are accessed). If so,
it deletes the Statement. ")
(vector-set! //T/Rs_/Prompt (- //T/R_/Delete_/Redundant_/Statement 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Delete_/Redundant_/Statement 1) "")
