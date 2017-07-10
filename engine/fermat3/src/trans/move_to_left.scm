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
(define (@Move_To_Left_Test)
 (cond
  ((and (= (@Gen_Type (@Item)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (@Left?))
   (@Fail "There is no statement to the left of this one."))
  (#t
   (@Left)
   (cond
    ((@Trans? //T/R_/Move_/To_/Right)
     (@Pass))
    (#t
     (@Fail "Could not move the previous item to the right."))))))

(define (@Move_To_Left_Code //Data)
 (cond
  ((and (= (@Gen_Type (@Item)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (@Left)
 (@Trans //T/R_/Move_/To_/Right "")
 (@Left))

