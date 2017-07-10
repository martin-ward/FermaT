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
(define (@Expand_Forward_Test)
 (cond
  ((not (member (@Spec_Type (@Item)) (list //T_/Cond //T_/D_/If)))
   (@Fail "Not an IF statement"))
  ((not (@Right?))
   (@Fail "There is no statement after this one"))
  (#t
   (@Pass))))

(define (@Expand_Forward_Code //Data)
 (let ((//A/S_/Type (@AS_Type)))
  (@Gen_Expand_Forward //A/S_/Type)))

(define (@Gen_Expand_Forward //A/S/Type)
 (let ((//S '())
       (//P (@Posn)))
  (@Right)
  (set! //S (@Item))
  (@Left)
  (@Down)
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (@Down_Last)
    (@Down_Last)
    (cond
     ((not (@Gen_Improper? (@Parent) //A/S/Type))
      (cond
       ((= (@Spec_Type (@Item)) //T_/Skip)
        (@Paste_Over //S))
       (#t
        (@Paste_After //S)))))
    (@Up)
    (@Up)
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@Up)
  (@Right)
  (@Delete)
  (@Goto //P)))

