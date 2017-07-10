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
(set! //T/R_/Join_/All_/Cases (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/Join_/All_/Cases 1) "Join All Cases")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Join_/All_/Cases 1) "Join_All_Cases")
(vector-set! //T/Rs_/Test (- //T/R_/Join_/All_/Cases 1) (funct (@Join_All_Cases_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Join_/All_/Cases 1) (funct (@Join_All_Cases_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Join_/All_/Cases 1) (list "Rewrite" "Join"))
(vector-set! //T/Rs_/Help (- //T/R_/Join_/All_/Cases 1) "This transformation will join any guards in an `If' statement which contain the same sequence of statements (thus reducing their number) by changing the conditions of all the guards as appropriate.")
(vector-set! //T/Rs_/Prompt (- //T/R_/Join_/All_/Cases 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Join_/All_/Cases 1) "")
