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
(define (@Loop_Doubling_Test)
 (cond
  ((= (@ST (@I)) //T_/Floop)
   (@Pass))
  (#t
   (@Fail "Selected item is not an Floop"))))

(define (@Loop_Doubling_Code //Data)
 (let ((//S (concat (@Cs (@Get_n (@I) 1)) (@Cs (@Get_n (@I) 1)))))
  (@Paste_Over (@Make 133 '() (list (@Make 17 '() //S))))))

#t
