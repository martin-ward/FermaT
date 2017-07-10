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
(set! //T/R_/Align_/Nested_/Statements (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/Align_/Nested_/Statements 1) "Align Nested Statements")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Align_/Nested_/Statements 1) "Align_Nested_Statements")
(vector-set! //T/Rs_/Test (- //T/R_/Align_/Nested_/Statements 1) (funct (@Align_Nested_Statements_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Align_/Nested_/Statements 1) (funct (@Align_Nested_Statements_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Align_/Nested_/Statements 1) (list "Rewrite" "L_to_R" "R_to_L"))
(vector-set! //T/Rs_/Help (- //T/R_/Align_/Nested_/Statements 1) "This transformation takes a guarded clause whose first statement is a `If' and integrates it with the outer condition by absorbing the other guarded statements into the inner `If', and then modifying its conditions appropriately.  This is the converse of `Partially Join Cases'.")
(vector-set! //T/Rs_/Prompt (- //T/R_/Align_/Nested_/Statements 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Align_/Nested_/Statements 1) "")
