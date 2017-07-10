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
(set! //T/R_/Elsif_/To_/Else_/If (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/Elsif_/To_/Else_/If 1) "`Elsif' To `Else If'")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Elsif_/To_/Else_/If 1) "Elsif_To_Else_If")
(vector-set! //T/Rs_/Test (- //T/R_/Elsif_/To_/Else_/If 1) (funct (@Elsif_To_Else_If_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Elsif_/To_/Else_/If 1) (funct (@Elsif_To_Else_If_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Elsif_/To_/Else_/If 1) (list "Rewrite"))
(vector-set! //T/Rs_/Help (- //T/R_/Elsif_/To_/Else_/If 1) "This transformation will replace an `Elsif' clause in an `If' statement with an `Else' clause which itself contains an `If' statement.  

The transformation can be selected with either the `If' statement, or the `Elsif' clause selected.")
(vector-set! //T/Rs_/Prompt (- //T/R_/Elsif_/To_/Else_/If 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Elsif_/To_/Else_/If 1) "")
