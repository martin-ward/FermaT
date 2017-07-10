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
; Routines for handling names: variable, proc, function etc. 
; Instead of using Lisp atoms we use small integers: 
; these are just as easy to compare and take up no more room, 
; but can be efficiently stored as *sorted* lists 
; (efficient for union and intersection operations), 
; With the aid of an array (giving the string value for each name) 
; and a hash table (giving the index number for each string) 
; we can avoid all symbol->string calls. 
(set! //N_/Symbol_/Table (make-vector-eval 1000 ""))
(set! //N_/Symbol_/Table_/Length (make-vector-eval 1000 0))
(set! //N_/Symbol_/Table_/Next 1)
(set! //N_/Symbol_/Table_/Size 1000)
; This is a hash table really: 
(set! //N_/String_/To_/Symbol (make-vector-eval 1000 '()))
(define (@Make_Name /str)
 (let ((//R (gethash //N_/String_/To_/Symbol /str)))
  (cond
   ((null? //R)
    (set! //R //N_/Symbol_/Table_/Next)
    (vector-set! //N_/Symbol_/Table (- //N_/Symbol_/Table_/Next 1) /str)
    (vector-set! //N_/Symbol_/Table_/Length (- //N_/Symbol_/Table_/Next 1) (string-length /str))
    (puthash //N_/String_/To_/Symbol /str //R)
    (set! //N_/Symbol_/Table_/Next (+ //N_/Symbol_/Table_/Next 1))
    (cond
     ((> //N_/Symbol_/Table_/Next //N_/Symbol_/Table_/Size)
      (@Grow_Symbol_Table)))))
  //R))

; Return the string corresponding to the given name (if any): 
(define (@N_String /name)
 
 (if (null? /name) "" (vector-ref //N_/Symbol_/Table (- /name 1))))

; Return the length the given name (if any): 
(define (@N_Length /name)
 
 (if (null? /name) 0 (vector-ref //N_/Symbol_/Table_/Length (- /name 1))))

(define (@Grow_Symbol_Table)
 (let ((/new1 (make-vector-eval (* //N_/Symbol_/Table_/Size 2) ""))
       (/new2 (make-vector-eval (* //N_/Symbol_/Table_/Size 2) "")))
  (for /i 1 //N_/Symbol_/Table_/Size 1 
   (begin
    (wsl-set! /new1 (vector-ref //N_/Symbol_/Table (- /i 1)) /i)
    (wsl-set! /new2 (vector-ref //N_/Symbol_/Table_/Length (- /i 1)) /i)))
  (set! //N_/Symbol_/Table /new1)
  (set! //N_/Symbol_/Table_/Length /new2)
  (set! //N_/Symbol_/Table_/Size (* //N_/Symbol_/Table_/Size 2))))

; If the item has a name as first component, then return its value as a string, 
; otherwise return the empty string. 
; For a T_Name_Int_One we assume that the expression is a simple variable. 
(define (@Name_Value //I)
 (let ((//S/T '())
       (//R ""))
  (cond
   ((@Cs? //I)
    (set! //S/T (@ST (@Get_n //I 1)))
    (cond
     ((= //S/T //T_/Name)
      (set! //R (@N_String (@V (@Get_n //I 1)))))
     ((= //S/T //T_/Name_/Pat_/One)
      (set! //R (string-append "~?" (@N_String (@V (@Get_n //I 1))))))
     ((= //S/T //T_/Name_/Var_/One)
      (set! //R (string-append "~?=" (@N_String (@V (@Get_n //I 1))))))
     ((= //S/T //T_/Name_/Int_/One)
      (set! //R (string-append (string-append "~?(" (@Value_String (@Get_n (@Get_n //I 1) 1))) ")"))))))
  //R))

(define (@Name_Length //I)
 (let ((//S/T '())
       (//R 0))
  (cond
   ((@Cs? //I)
    (set! //S/T (@ST (@Get_n //I 1)))
    (cond
     ((= //S/T //T_/Name)
      (set! //R (@N_Length (@V (@Get_n //I 1)))))
     ((= //S/T //T_/Name_/Pat_/One)
      (set! //R (+ (@N_Length (@V (@Get_n //I 1))) 2)))
     ((= //S/T //T_/Name_/Var_/One)
      (set! //R (+ (@N_Length (@V (@Get_n //I 1))) 3)))
     ((= //S/T //T_/Name_/Int_/One)
      (set! //R (+ (@Value_Length (@Get_n (@Get_n //I 1) 1)) 4))))))
  //R))

(define (@Value_String //I)
 (let ((//R ""))
  (cond
   ((@Has_Value_Type? (@ST //I))
    (cond
     ((or (= (@ST //I) //T_/Number) (= (@ST //I) //T_/Exit))
      (set! //R (@String (@V //I))))
     ((or (= (@ST //I) //T_/String) (= (@ST //I) //T_/Comment))
      (set! //R (@V //I)))
     ((and (number? (@V //I)) (< (@V //I) 0))
      (set! //R (@String (- (@V //I)))))
     (#t
      (set! //R (@N_String (@V //I)))))))
  //R))

(define (@Value_Length //I)
 (let ((//R 0))
  (cond
   ((@Has_Value_Type? (@ST //I))
    (cond
     ((or (= (@ST //I) //T_/Number) (= (@ST //I) //T_/Exit))
      (set! //R (string-length (@String (@V //I)))))
     ((or (= (@ST //I) //T_/String) (= (@ST //I) //T_/Comment))
      (set! //R (string-length (@V //I))))
     ((and (number? (@V //I)) (< (@V //I) 0))
      (set! //R (string-length (@String (@V //I)))))
     (#t
      (set! //R (@N_Length (@V //I)))))))
  //R))

; ----------------------------------------------------------------------- 

