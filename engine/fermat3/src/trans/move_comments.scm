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
(define (@Move_Comments_Test)
 (cond
  ((not (= (@Spec_Type (@Item)) //T_/A_/S))
   (@Fail "Not Action System"))
  (#t
   (@Pass))))

(define (@Move_Comments_Code //Data)
 (let ((//A/S_/Size (@Size (@Get_n (@Item) 2))))
  (@Down_Last)
  (@Down)
  (for //I 1 //A/S_/Size 1 
   (let ((//Actn (@Posn)))
    (@Down_Last)
    (@Down_Last)
    (while (and (@Left?) (not (= (@Spec_Type (@Item)) //T_/Call))) 
     (@Left))
    (let ((//S '()))
     (while (and (= (@Spec_Type (@Item)) //T_/Call) (@Right?) (member (@Spec_Type (@Get_n (@Parent) (+ (@Posn_n) 1))) (list //T_/Skip //T_/Comment))) 
      (begin
       (@Cut)
       (set! //S (@Buffer))
       (@Paste_After //S)
       (@Right))))
    (@Goto //Actn)
    (cond
     ((@Right?)
      (@Right)))))))

