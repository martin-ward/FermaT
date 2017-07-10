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
(define (@Else_If_To_Elsif_Test)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   (@Down_Last)))
 (cond
  ((and (= (@GT (@I)) //T_/Guarded) (not (@Right?)))
   (@Down_Last)
   (@Down)
   ; to first statement in ELSE clause 
   (let ((/n 0))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((= (@ST (@I)) //T_/Cond)
        (set! /n (+ /n 1))
        (cond
         ((> /n 1)
          (@Fail "The ELSE clause has more than one IF statement.")
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0))))
       ((= (@ST (@I)) //T_/Comment)
        (set! /fl_flag1 0))
       (#t
        (@Fail "The ELSE clause contains something other than an IF statement.")
        (set! /fl_flag1 1)))
      (cond
       ((= /fl_flag1 0)
        (cond
         ((@Right?)
          (@Right)
          (set! /fl_flag1 0))
         ((= /n 0)
          (@Fail "The ELSE clause doesn't have an IF statement.")
          (set! /fl_flag1 1))
         (#t
          (@Pass)
          (set! /fl_flag1 1)))))))))
  (#t
   (@Fail "The selected item is not an ELSE clause or IF statement."))))

(define (@Else_If_To_Elsif_Code //Data)
 (let ((/up 0)
       (/comments '())
       (//S '()))
  (cond
   ((= (@ST (@I)) //T_/Cond)
    (set! /up 1)
    (@Down_Last)))
  (@Down_Last)
  (@Down)
  ; to first statement in ELSE clause 
  (cond
   ((= (@ST (@I)) //T_/Comment)
    (set! /comments (cons (@I) /comments)))
   (#t
    (set! //S (@I))))
  (while (@Right?) 
   (begin
    (@Right)
    (cond
     ((= (@ST (@I)) //T_/Comment)
      (set! /comments (cons (@I) /comments)))
     (#t
      (set! //S (@I))))))
  (@Up)
  (@Up)
  ; back to ELSE clause 
  (@Splice_Over (@Cs //S))
  (@Down_Last)
  (@Down)
  (@Splice_Before (reverse /comments))
  (@Up)
  (@Up)
  (cond
   ((= /up 1)
    (@Up)))))

#t
