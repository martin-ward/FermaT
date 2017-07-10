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
(define (@Collapse_All_Systems_Test)
 (cond
  ((and (= (@Spec_Type (@Item)) //T_/Where) (equal? (@Posn) (list 1)))
   (@Pass))
  (#t
   (@Fail "Not a suitable structure"))))

(define (@Collapse_All_Systems_Code //Data)
 (let ((/n 0))
  (@Goto (list 1 2))
  (set! /n (@Size (@Item)))
  (@Goto (list 1 1 1))
  (cond
   ((= (@Spec_Type (@Item)) //T_/A_/S)
    (display-list " ")
    (display-list "+++++++++++++++++++++++++++++++++++++++++ ")
    (display-list "Collapsing Action System " (@Value (@Item)))
    (display-list "+++++++++++++++++++++++++++++++++++++++++ ")
    (@Trans //T/R_/Collapse_/Action_/System "")))
  (for /i 1 /n 1 
   (begin
    (@Goto (list 1 2 /i 3 1 2 1))
    (cond
     ((= (@Spec_Type (@Item)) //T_/A_/S)
      (display-list " ")
      (display-list "+++++++++++++++++++++++++++++++++++++++++ ")
      (display-list "Collapsing Action System " (@Value (@Item)))
      (display-list "+++++++++++++++++++++++++++++++++++++++++ ")
      (@Trans //T/R_/Collapse_/Action_/System "")))))))

