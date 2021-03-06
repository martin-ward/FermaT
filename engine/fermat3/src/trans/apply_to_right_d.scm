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
(cond
 ((null? //T/R_/Apply_/To_/Right)
  (set! //T/R_/Apply_/To_/Right (@New_TR_Number))))
(vector-set! //T/Rs_/Name (- //T/R_/Apply_/To_/Right 1) "Apply To Right")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Apply_/To_/Right 1) "Apply_To_Right")
(vector-set! //T/Rs_/Test (- //T/R_/Apply_/To_/Right 1) (funct (@Apply_To_Right_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Apply_/To_/Right 1) (funct (@Apply_To_Right_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Apply_/To_/Right 1) (list "Use/Apply"))
(vector-set! //T/Rs_/Help (- //T/R_/Apply_/To_/Right 1) "This transformation will apply the current program item to the one to its immediate right.  For example, if the current item is an assertion and the next item is an `IF' statement, then the transformation will attempt to simplify the conditions using the assertions.")
(vector-set! //T/Rs_/Prompt (- //T/R_/Apply_/To_/Right 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Apply_/To_/Right 1) "")
