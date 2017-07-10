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
(define (@Unfold_Proc_Call_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Proc_/Call))
   (@Fail "Not a Proc Call."))
  (#t
   (@Pass))))

(define (@Unfold_Proc_Call_Code //Data)
 (let ((/name (@V (@Get_n (@I) 1)))
       (/posn (@Posn))
       (/defn '()))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (while (and (not (= (@ST (@I)) //T_/Where)) (@Up?)) 
     (@Up))
    (cond
     ((not (= (@ST (@I)) //T_/Where))
      (set! /fl_flag1 1))
     (#t
      ; Find the body of the procedure 
      (@Down_To 2)
      (@Down)
      ; to first defn 
      (while (and (or (not (= (@ST (@I)) //T_/Proc)) (not (equal? (@V (@Get_n (@I) 1)) /name))) (@Right?)) 
       (@Right))
      (cond
       ((and (= (@ST (@I)) //T_/Proc) (equal? (@V (@Get_n (@I) 1)) /name))
        (set! /defn (@I))
        (@Goto /posn)
        (@Unfold_Proc_Call /defn)
        (cond
         ((= (@ST (@I)) //T_/Skip)
          (@Clever_Delete)))
        (set! /fl_flag1 1))
       (#t
        ; Proc is not declared in this WHERE clause, move up to next one 
        (@Up)
        (@Up)
        ; back to WHERE 
        (cond
         ((not (@Up?))
          (set! /fl_flag1 1))
         (#t
          (@Up)
          (set! /fl_flag1 0)))))))))))

#t
