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
(define (@Move_Comment_Left_Test)
 (cond
  ((not (= (@Spec_Type (@Item)) //T_/Comment))
   (@Fail "Selected item is not a comment."))
  (#t
   (cond
    ((@Left?)
     (@Pass))
    ((not (@Up?))
     (@Fail "Can't move it any further left"))
    (#t
     (@Up)
     (while (and (not (= (@Gen_Type (@Item)) //T_/Statement)) (@Up?)) 
      (@Up))
     (cond
      ((= (@Gen_Type (@Item)) //T_/Statement)
       (@Pass))
      (#t
       (@Fail "Can't move it any further left"))))))))

(define (@Move_Comment_Left_Code //Data)
 (let ((/comment (@Item)))
  (cond
   ((@Left?)
    (@Cut)
    (@Left)
    (@Paste_Before (@Buffer)))
   (#t
    (@Clever_Delete)
    (cond
     ((@Up?)
      (@Up)))
    (while (and (not (= (@Gen_Type (@Item)) //T_/Statement)) (@Up?)) 
     (@Up))
    (cond
     ((= (@Gen_Type (@Item)) //T_/Statement)
      (@Paste_Before /comment)))))))

