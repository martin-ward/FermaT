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
(define (@Separate_Both_Test)
 (cond
  ((or (= (@ST (@I)) //T_/Cond) (= (@ST (@I)) //T_/Assignment))
   (cond
    ((or (@Trans? //T/R_/Separate_/Right) (@Trans? //T/R_/Separate_/Left))
     (@Pass))
    (#t
     (@Fail "Can't take out in either direction"))))
  (#t
   (@Fail "Not a Cond or assignment statement"))))

(define (@Separate_Both_Code //Data)
 (cond
  ((@Trans? //T/R_/Separate_/Left)
   (@Trans //T/R_/Separate_/Left "")))
 (cond
  ((@Trans? //T/R_/Separate_/Right)
   (@Trans //T/R_/Separate_/Right ""))))

