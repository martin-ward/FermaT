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
(set! //T/R_/Expand_/And_/Separate (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/Expand_/And_/Separate 1) "Expand And Separate")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Expand_/And_/Separate 1) "Expand_And_Separate")
(vector-set! //T/Rs_/Test (- //T/R_/Expand_/And_/Separate 1) (funct (@Expand_And_Separate_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Expand_/And_/Separate 1) (funct (@Expand_And_Separate_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Expand_/And_/Separate 1) (list "Reorder"))
(vector-set! //T/Rs_/Help (- //T/R_/Expand_/And_/Separate 1) "Expand And Separate will expand the selected IF statement to include all the following statements, then separate all possible statements from the resulting IF. This is probably only useful if the IF includes a CALL, EXIT etc. which is duplicated in the following statements, otherwise it will probably achieve nothing.")
(vector-set! //T/Rs_/Prompt (- //T/R_/Expand_/And_/Separate 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Expand_/And_/Separate 1) "")
