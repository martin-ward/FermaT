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
(set! //T/R_/While_/To_/Abort (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/While_/To_/Abort 1) "While_To_Abort")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/While_/To_/Abort 1) "While_To_Abort")
(vector-set! //T/Rs_/Test (- //T/R_/While_/To_/Abort 1) (funct (@While_To_Abort_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/While_/To_/Abort 1) (funct (@While_To_Abort_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/While_/To_/Abort 1) (list "Simplify" "L_to_R" "R_to_L"))
(vector-set! //T/Rs_/Help (- //T/R_/While_/To_/Abort 1) "This transformation replaces a non-terminating while loop with a conditional abort")
(vector-set! //T/Rs_/Prompt (- //T/R_/While_/To_/Abort 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/While_/To_/Abort 1) "")
