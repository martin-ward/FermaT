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
(define (@Combine_Wheres_Test)
 (let ((/orig_pos (@Posn))
       (/o_where "")
       (/double 0))
  (cond
   ((not (= (@Spec_Type (@Item)) //T_/Where))
    (@Fail "Not WHERE")))
  (cond
   ((and (not (@Failed?)) (@Up?))
    (@Up)
    (cond
     ((@Up?)
      (@Up)
      (cond
       ((= (@Spec_Type (@Item)) //T_/Where)
        (set! /double 1)
        (set! /o_where (@Posn))))))))
  (@Goto /orig_pos)
  (cond
   ((and (= /double 0) (not (@Failed?)) (@Down?))
    (@Down)
    (cond
     ((@Down?)
      (@Down)
      (cond
       ((= (@Spec_Type (@Item)) //T_/Where)
        (set! /double 1)
        (set! /o_where /orig_pos)))))))
  (cond
   ((= /double 1)
    (@Goto /o_where)
    (let ((/outer '())
          (/inner '()))
     (@Down)
     (@Right)
     (for /i 1 (@Size (@Item)) 1 
      (set! /outer (concat /outer (list (@V (@Get_n (@Get_n (@I) /i) 1))))))
     (@Goto /o_where)
     (@Down)
     (@Down)
     (@Down)
     (@Right)
     (for /i 1 (@Size (@Item)) 1 
      (cond
       ((member (@V (@Get_n (@Get_n (@I) /i) 1)) /outer)
        (@Fail "Name Clash")))))))
  (cond
   ((= /double 0)
    (@Fail "Not nested WHEREs")))
  (cond
   ((not (@Failed?))
    (@Pass)))))

(define (@Combine_Wheres_Code //Data)
 (let ((/o_where (@Posn))
       (//S1 "")
       (//S2 "")
       (/d1 "")
       (/d2 ""))
  (cond
   ((@Up?)
    (@Up)
    (cond
     ((@Up?)
      (@Up)
      (cond
       ((= (@Spec_Type (@Item)) //T_/Where)
        (set! /o_where (@Posn))))))))
  (@Goto /o_where)
  (set! /d1 (@Get_L (@Get_n (@Item) 2) 1 (@Size (@Get_n (@Item) 2))))
  (set! //S1 (@Get_L (@Get_n (@Item) 1) 2 (@Size (@Get_n (@Item) 1))))
  (@Down)
  (@Down)
  (set! //S2 (@Get_n (@Item) 1))
  (set! /d2 (@Get_n (@Item) 2))
  (@Goto /o_where)
  (@Paste_Over (@Make //T_/Where '() (list (concat //S2 //S1) (concat /d2 /d1))))))

