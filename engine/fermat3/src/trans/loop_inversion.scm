;;; Scheme translation of WSL code
(define /%const__loop_inversion__1 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
(define /%const__loop_inversion__2 (@Make 114 '() (list (@Make 7 '() (list (@Make 332 '() (list (@Make 205 1 '()))) (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list (@Make 107 -2 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
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
(define (@Loop_Inversion_Test)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (= (@GT (@I)) //T_/Statement))
   (@Fail "Selected item is not a Statement"))
  ((or (< (gen-length (@Posn)) 2) (not (= (@ST (@GParent)) //T_/Floop)))
   (@Fail "Selected statement is not at the top level of an Floop body"))
  ((= (@Posn_n) 1)
   (@Fail "Selected statement is already the first statement in the loop"))
  (#t
   (@Pass))))

(define (@Loop_Inversion_Code //Data)
 (let ((//S1 '())
       (//S2 '())
       (//S '())
       (/assert '())
       (/test '())
       (/done 0))
  (cond
   ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
    (@Up)))
  (cond
   ((not (@Right?))
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__loop_inversion__1 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__/B_save //B))
        (set! //B (vector-ref /__/Match_array 0))
        (set! /test //B)
        (set! //B /__/B_save)))))))
  (@Left)
  (@Cut_Rest)
  (set! //S1 (@Cs (@Parent)))
  (set! //S2 (@Buffer))
  (@Up)
  (@Up)
  ; to the Floop 
  (cond
   ((not (null? /test))
    (set! /assert (@Look_For_Assertion  /assert))
    (cond
     ((and (not (null? /assert)) (@Implies? /assert (@Not /test)))
      (@Paste_Over (@Make 133 '() (list (@Make 17 '() (append //S2 //S1)))))
      (set! /done 1)
      (cond
       ((>= (gen-length (@Posn)) 3)
        (let ((/posn (@Posn)))
         (@Up)
         (@Up)
         (@Up)
         (let ((/__/O/K 1))
          (vector-set! /__/Match_array 0 /assert)
          (set! /__/O/K (@New_Match  /%const__loop_inversion__2 (@I) /__/O/K))
          (cond
           ((= /__/O/K 1)
            (let ((/__/S_save //S))
             (set! //S (vector-ref /__/Match_array 1))
             (@Paste_Over (@Make 133 '() (list (@Make 17 '() //S))))
             (set! //S /__/S_save)))
           (#t
            (@Goto /posn)))))))))))
  (cond
   ((= /done 0)
    (set! //S (@Increment (@Make 133 '() (list (@Make 17 '() (append //S2 //S1)))) (@AS_Type) 1 0))
    (@Paste_Over (@Make 133 '() (list (@Make 17 '() (append //S1 //S)))))
    ; Check if the outer loop is really needed: 
    (cond
     ((@Trans? //T/R_/Remove_/Dummy_/Loop)
      (@Trans //T/R_/Remove_/Dummy_/Loop '())))))))

#t
