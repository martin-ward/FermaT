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
(set! //T/R_/Separate_/Left (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/Separate_/Left 1) "Separate_Left")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Separate_/Left 1) "Separate_Left")
(vector-set! //T/Rs_/Test (- //T/R_/Separate_/Left 1) (funct (@Separate_Left_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Separate_/Left 1) (funct (@Separate_Left_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Separate_/Left 1) (list "Reorder"))
(vector-set! //T/Rs_/Help (- //T/R_/Separate_/Left 1) "Separate_Left will take code out to the left of the selected structure. As much code as possible will be taken out; if all the statements are taken out then the original containing structure will be removed")
(vector-set! //T/Rs_/Prompt (- //T/R_/Separate_/Left 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Separate_/Left 1) "")
