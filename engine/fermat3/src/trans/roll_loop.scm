;;; Scheme translation of WSL code
(define /%const__roll_loop__1 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()) (@Make 141 '() (list (@Make 334 -1 '()) (@Make 17 '() (list (@Make 163 -2 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
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
(define (@Roll_Loop_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Cond))
   (@Fail "Selected item is not a Cond."))
  (#t
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__roll_loop__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S_save //S)
            (/__/B_save //B))
       (set! //S (vector-ref /__/Match_array 1))
       (set! //B (vector-ref /__/Match_array 0))
       (@Pass)
       (set! //S /__/S_save)
       (set! //B /__/B_save)))
     (#t
      (@Fail "The Cond is not in the right form ")))))))

(define (@Roll_Loop_Code //Data)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__roll_loop__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/S_save //S)
          (/__/B_save //B))
     (set! //S (vector-ref /__/Match_array 1))
     (set! //B (vector-ref /__/Match_array 0))
     (@Paste_Over (@Make 141 '() (list //B (@Make 17 '() //S))))
     (set! //S /__/S_save)
     (set! //B /__/B_save)))
   (#t
    (error "@Roll_Loop_Code: match failed!")))))

#t
