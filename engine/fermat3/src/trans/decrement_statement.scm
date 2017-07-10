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
; If the item can be decremented and the decremented item can be incremented 
; then the transformation is OK 
(define (@Decrement_Statement_Test)
 (cond
  ((not (= (@GT (@I)) //T_/Statement))
   (@Fail "Current item is not a statement"))
  ((not (@Up?))
   (@Fail "Statement is not in an enclosing loop"))
  ((not (@Is_Improper?))
   (@Fail "Statement cannot be decremented"))
  (#t
   (let ((/orig (@I)))
    ; Note: a decrement of a single statement is always a single statement 
    (@Splice_Over (@Increment (@I) (@AS_Type) (- 1) 0))
    (cond
     ((@Trans? //T/R_/Increment_/Statement)
      (@Pass))
     (#t
      (@Fail (@Fail_Message))))
    (@Paste_Over /orig)))))

(define (@Decrement_Statement_Code //Data)
 (@Splice_Over (@Increment (@I) (@AS_Type) (- 1) 0)))

#t
