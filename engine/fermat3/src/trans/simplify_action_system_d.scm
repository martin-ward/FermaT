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
(set! //T/R_/Simplify_/Action_/System (@New_TR_Number))
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Simplify_/Action_/System 1) "Simplify_Action_System")
(vector-set! //T/Rs_/Test (- //T/R_/Simplify_/Action_/System 1) (funct (@Simplify_Action_System_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Simplify_/Action_/System 1) (funct (@Simplify_Action_System_Code)))
(vector-set! //T/Rs_/Name (- //T/R_/Simplify_/Action_/System 1) "Simplify Action System")
(vector-set! //T/Rs_/Keywords (- //T/R_/Simplify_/Action_/System 1) (list "Simplify"))
(vector-set! //T/Rs_/Help (- //T/R_/Simplify_/Action_/System 1) "Simplify action system will attempt to remove actions and calls from an action system by successively applying simplifying transformations. As many of the actions as possible will be eliminated without making the program significantly larger.")
(vector-set! //T/Rs_/Prompt (- //T/R_/Simplify_/Action_/System 1) "Enter names of any actions in which recursion should NOT be removed, in the form Name1 Name2 ...")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Simplify_/Action_/System 1) "Expression")
