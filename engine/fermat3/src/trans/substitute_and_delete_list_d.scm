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
(set! //T/R_/Substitute_/And_/Delete_/List (@New_TR_Number))
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Substitute_/And_/Delete_/List 1) "Substitute_And_Delete_List")
(vector-set! //T/Rs_/Test (- //T/R_/Substitute_/And_/Delete_/List 1) (funct (@Substitute_And_Delete_List_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Substitute_/And_/Delete_/List 1) (funct (@Substitute_And_Delete_List_Code)))
(vector-set! //T/Rs_/Name (- //T/R_/Substitute_/And_/Delete_/List 1) "Substitute and Delete List")
(vector-set! //T/Rs_/Keywords (- //T/R_/Substitute_/And_/Delete_/List 1) (list "Hidden" "Rewrite"))
(vector-set! //T/Rs_/Help (- //T/R_/Substitute_/And_/Delete_/List 1) "Substitute and Delete List will replace all calls to any action
within the selected list of actions with the corresponding definition
and delete the definition. Actions which are called more than  once
will not be affected.")
(vector-set! //T/Rs_/Prompt (- //T/R_/Substitute_/And_/Delete_/List 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Substitute_/And_/Delete_/List 1) "")
