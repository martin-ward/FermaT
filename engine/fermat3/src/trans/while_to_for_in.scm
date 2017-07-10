;;; Scheme translation of WSL code
(define /%const__while_to_for_in__1 (@Make 141 '() (list (@Make 312 '() (list (@Make 321 '() (list (@Make 217 -1 '()))))) (@Make 17 '() (list (@Make 107 -2 '()) (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -1 '()) (@Make 241 '() (list (@Make 263 -1 '()))))))))))))
(define /%const__while_to_for_in__2 (@Make 141 '() (list (@Make 312 '() (list (@Make 321 '() (list (@Make 217 -1 '()))))) (@Make 17 '() (list (@Make 107 -2 '()) (@Make 110 '() (list (@Make 602 -3 '()) (@Make 6 '() (list (@Make 512 -1 '()) (@Make 241 '() (list (@Make 263 -1 '()))))))))))))
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
(define (@While_To_For_In_Test)
 (cond
  ((not (= (@ST (@I)) //T_/While))
   (@Fail "Current item is not a While loop"))
  (#t
   (@Edit)
   (@WF_Fix)
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__while_to_for_in__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S_save //S)
            (/__v_save /v))
       (set! //S (vector-ref /__/Match_array 1))
       (set! /v (vector-ref /__/Match_array 0))
       (cond
        ((@Elt_Clash? (@Elts_Assigned (@Make //T_/Statements '() //S)) (@Elements /v))
         (@Fail "The variable is altered in the loop body"))
        (#t
         (@Pass)))
       (set! //S /__/S_save)
       (set! /v /__v_save)))
     (#t
      (@Fail "While loop is not in a suitable form"))))
   (@Undo_Edit))))

(define (@While_To_For_In_Code //Data)
 (let ((/elt (@Make_Name "elt"))
       (/n 0)
       (/v1 '())
       (/e '()))
  (@WF_Fix)
  (while (member /elt (@Variables (@I))) 
   (begin
    (set! /n (+ /n 1))
    (set! /elt (@Make_Name (string-append "elt_" (@String /n))))))
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__while_to_for_in__1 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__/S_save //S)
           (/__v_save /v))
      (set! //S (vector-ref /__/Match_array 1))
      (set! /v (vector-ref /__/Match_array 0))
      (set! /v1 (@Make //T_/Var_/Lvalue /elt '()))
      (set! /e (@Make //T_/Variable /elt '()))
      (@Paste_Over (@Make 154 '() (list (@Expn_To_Var /v1) (@Var_To_Expn /v) (@Make 17 '() //S))))
      (set! /v (@Lvalue_To_Expn /v))
      (@Paste_Over (@Replace (@I) /e (@Make 240 '() (list (@Var_To_Expn /v)))))
      (set! //S /__/S_save)
      (set! /v /__v_save)))
    (#t
     (error "Bug in While_To_For_In!"))))))

(define (@WF_Fix)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__while_to_for_in__2 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__a_save /a)
          (/__/S_save //S)
          (/__v_save /v))
     (set! /a (vector-ref /__/Match_array 2))
     (set! //S (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((not (null? /a))
       (set! //S (concat //S (list (@Make //T_/Assignment '() /a))))
       (@Paste_Over (@Make 141 '() (list (@Make 312 '() (list (@Make 321 '() (list (@Var_To_Expn /v))))) (@Make 17 '() (append //S (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Make 241 '() (list (@Var_To_Expn /v)))))))))))))))
     (set! /a /__a_save)
     (set! //S /__/S_save)
     (set! /v /__v_save))))))

#t
