;;; Scheme translation of WSL code
;
;==========================================================================
;FermaT Transformation System
;Copyright (C) 2015 Software Migrations Limited.
;Email: martin@gkc.org.uk
;
;This program is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software FTo_Vaoundation; either version 3 of the License, or
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
(set! //T/R_/Array_/To_/Vars (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/Array_/To_/Vars 1) "Array_To_Vars")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Array_/To_/Vars 1) "Array_To_Vars")
(vector-set! //T/Rs_/Test (- //T/R_/Array_/To_/Vars 1) (funct (@Array_To_Vars_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Array_/To_/Vars 1) (funct (@Array_To_Vars_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Array_/To_/Vars 1) (list "Rewrite" "L_to_R" "R_to_L"))
(vector-set! //T/Rs_/Help (- //T/R_/Array_/To_/Vars 1) "Convert a local variable array to a set of local variables")
(vector-set! //T/Rs_/Prompt (- //T/R_/Array_/To_/Vars 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Array_/To_/Vars 1) "")
