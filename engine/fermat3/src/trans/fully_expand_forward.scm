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
(define (@Fully_Expand_Forward_Test)
 (cond
  ((@Trans? //T/R_/Expand_/Forward)
   (@Pass))
  (#t
   (@Fail "Cannot Expand Forward"))))

(define (@Fully_Expand_Forward_Code //Data)
 (while (@Trans? //T/R_/Expand_/Forward) 
  (@Trans //T/R_/Expand_/Forward "")))

#t
