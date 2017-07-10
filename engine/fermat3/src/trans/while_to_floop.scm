;;; Scheme translation of WSL code
(define /%const__while_to_floop__1 (@Make 141 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()))))))
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
(define (@While_To_Floop_Test)
 (cond
  ((not (= (@ST (@I)) //T_/While))
   (@Fail "Selected item is not a WHILE loop."))
  (#t
   (@Pass))))

(define (@While_To_Floop_Code //Data)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__while_to_floop__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/S_save //S)
          (/__/B_save //B))
     (set! //S (vector-ref /__/Match_array 1))
     (set! //B (vector-ref /__/Match_array 0))
     (set! //B (@Not //B))
     (@Paste_Over (@Make 133 '() (list (@Make 17 '() (append (list (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() (list (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '())))))))) //S)))))
     (set! //S /__/S_save)
     (set! //B /__/B_save)))
   (#t
    (error "Not a WHILE loop!")))))

#t
