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
(set! //T/R_/Force_/Double_/To_/Single_/Loop (@New_TR_Number))
(vector-set! //T/Rs_/Proc_/Name (- //T/R_/Force_/Double_/To_/Single_/Loop 1) "Force_Double_To_Single_Loop")
(vector-set! //T/Rs_/Test (- //T/R_/Force_/Double_/To_/Single_/Loop 1) (funct (@Force_Double_To_Single_Loop_Test)))
(vector-set! //T/Rs_/Code (- //T/R_/Force_/Double_/To_/Single_/Loop 1) (funct (@Force_Double_To_Single_Loop_Code)))
(vector-set! //T/Rs_/Name (- //T/R_/Force_/Double_/To_/Single_/Loop 1) "Force Double - Single Loop")
(vector-set! //T/Rs_/Keywords (- //T/R_/Force_/Double_/To_/Single_/Loop 1) (list "Rewrite"))
(vector-set! //T/Rs_/Help (- //T/R_/Force_/Double_/To_/Single_/Loop 1) "Force Double - Single Loop will convert a double nested loop to a single loop, regardless of any increase in program size which this causes")
(vector-set! //T/Rs_/Prompt (- //T/R_/Force_/Double_/To_/Single_/Loop 1) "")
(vector-set! //T/Rs_/Data_/Gen_/Type (- //T/R_/Force_/Double_/To_/Single_/Loop 1) "")
