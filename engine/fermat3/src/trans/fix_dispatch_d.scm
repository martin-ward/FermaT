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
(set! //T/R_/Fix_/Dispatch (@New_TR_Number))
(vector-set! //T/Rs_/Name (- //T/R_/Fix_/Dispatch 1) "Fix Dispatch")
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Fix_/Dispatch 1) "Fix_Dispatch")
(vector-set! //T/Rs_/Test (- //T/R_/Fix_/Dispatch 1) (funct (@Fix_Dispatch_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Fix_/Dispatch 1) (funct (@Fix_Dispatch_Code)))
(vector-set! //T/Rs_/Keywords (- //T/R_/Fix_/Dispatch 1) (list "Hidden"))
(vector-set! //T/Rs_/Help (- //T/R_/Fix_/Dispatch 1) "This transformation will search for simple
procedures in Assembler code and convert them to WSL PROCs.
A simple procedure is a collection of actions with a single entry
point and possibly multiple exit points. All the exits are calls to
dispatch (ie normal returns), or calls to an action which must lead
to an ABEND (ie error returns).")
(vector-set! //T/Rs_/Prompt (- //T/R_/Fix_/Dispatch 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Fix_/Dispatch 1) "")
