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
(define (@Elsif_To_Else_If_Test)
 (cond
  ((or (and (= (@ST (@I)) //T_/Cond) (> (@Size (@I)) 2)) (and (= (@GT (@I)) //T_/Guarded) (@Left?) (@Right?)))
   (@Pass))
  (#t
   (@Fail "The selected item is not, or does not contain, an `Elsif' clause."))))

(define (@Elsif_To_Else_If_Code //Data)
 (let ((//C 0)
       (//B '())
       (//S '()))
  ; C records whether the cond itself was originally selected 
  (cond
   ((= (@ST (@I)) //T_/Cond)
    (set! //C 1)
    (@Down)
    (@Right)))
  (set! //B (@Get_n (@I) 1))
  (set! //S (@Cs (@Get_n (@I) 2)))
  (@Cut_Rest)
  (@Paste_Over (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() //S))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))))
  (@Down_Last)
  (@Down)
  (@Down_Last)
  ; to the TRUE THEN SKIP guarded clause 
  (@Splice_Over (@Buffer))
  (@Up)
  (@Up)
  (@Up)
  ; back to Guarded 
  (cond
   ((= //C 1)
    (@Up)))))

#t
