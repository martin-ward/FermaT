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
(define (@Reduce_Multiple_Loops_Test)
 (cond
  ((not (= (@Spec_Type (@Item)) //T_/Floop))
   (@Fail "This transformation must be applied to a `Do-Od' loop."))
  ((or (@Trans? //T/R_/Double_/To_/Single_/Loop) (@Trans? //T/R_/Remove_/Dummy_/Loop))
   (@Pass))
  (#t
   (@Fail "It is not possible to remove any of the loops."))))

(define (@Reduce_Multiple_Loops_Code //Data)
 (while (@Trans? //T/R_/Double_/To_/Single_/Loop) 
  (@Trans //T/R_/Double_/To_/Single_/Loop ""))
 (while (@Trans? //T/R_/Remove_/Dummy_/Loop) 
  (@Trans //T/R_/Remove_/Dummy_/Loop "")))

