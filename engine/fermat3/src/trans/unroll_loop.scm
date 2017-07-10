;;; Scheme translation of WSL code
(define (/foreach-unroll_loop-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Exit)
   (cond
    ((equal? (@V (@I)) //Depth)
     (@Splice_Over (@Increment /loop //A/S //Depth 0)))
    (#t
     (@Splice_Over (@Increment (@I) //A/S (- 1) 1)))))
  ((= (@ST (@I)) //T_/Skip)
   ; depth must be zero! 
   (@Paste_Over /loop))
  ((@Gen_Improper? (@I) //A/S)
   #t)
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (@Make_Name "Z")))
   #t)
  (#t
   ; depth must be zero! 
   (@Paste_After /loop))))

(define /%const__unroll_loop__1 (@Make 141 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()))))))
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
(define (@Unroll_Loop_Test)
 (cond
  ((or (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/Floop))
   (@Pass))
  (#t
   (@Fail "Selected item is not a WHILE loop or Floop."))))

(define (@Unroll_Loop_Code //Data)
 (cond
  ((= (@ST (@I)) //T_/Floop)
   ; do S od == S[do S od+d/T|t=0][T-1/T|t>0] provided S is reducible 
   (let ((/loop-save /loop)
         (//A/S-save //A/S))
    (set! /loop (@I))
    (set! //A/S (@AS_Type))
    (cond
     ((not (@Gen_Reducible? (@Get_n (@I) 1) (@AS_Type)))
      (@Down)
      (@Trans //T/R_/Make_/Reducible "")
      (@Up)))
    (@Down)
    ; Add a SKIP at the end of the body, to avoid extra copies when 
    ; the last statement is a conditional 
    (@Down_Last)
    (cond
     ((and (= (@ST (@I)) //T_/Cond) (@Gen_Proper? (@I) (@AS_Type)))
      (@Paste_After (@Skip))))
    (@Up)
    (@Foreach_Terminal /foreach-unroll_loop-1 0 (@AS_Type) 1)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    ; Remove the extra surrounding loop 
    (@Up)
    ; back to Floop 
    (@Splice_Over (@Cs (@Get_n (@I) 1)))
    (set! /loop /loop-save)
    (set! //A/S //A/S-save)))
  (#t
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__unroll_loop__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S_save //S)
            (/__/B_save //B))
       (set! //S (vector-ref /__/Match_array 1))
       (set! //B (vector-ref /__/Match_array 0))
       (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() (append //S (list (@Make 141 '() (list //B (@Make 17 '() //S)))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
       (set! //S /__/S_save)
       (set! //B /__/B_save)))
     (#t
      (error "Not a WHILE loop!")))))))

#t
