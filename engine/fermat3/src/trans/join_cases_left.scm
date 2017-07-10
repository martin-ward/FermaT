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
(define (@Join_Cases_Left_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Guarded))
   (@Fail "Current item is not a Guarded"))
  ((not (@Left?))
   (@Fail "There is no Guarded to the left of this one"))
  (#t
   (@Left)
   (cond
    ((not (@Trans? //T/R_/Join_/Cases_/Right))
     (@Fail (@Fail_Message)))
    (#t
     (@Pass)))
   (@Right))))

(define (@Join_Cases_Left_Code //Data)
 (@Left)
 (@Trans //T/R_/Join_/Cases_/Right //Data))

#t
