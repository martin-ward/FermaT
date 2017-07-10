;;; Scheme translation of WSL code
(define /%const__reverse_order__1 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 107 -3 '()))))))))
(define /%const__reverse_order__2 (@Make 227 '() (list (@Make 305 -1 '()) (@Make 217 -2 '()) (@Make 217 -3 '()))))
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
(define (@Reverse_Order_Test)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   (cond
    ((= (@Size (@I)) 2)
     (@Pass))
    (#t
     (@Fail "The `IF' statement does not have two branches."))))
  ((or (= (@ST (@I)) //T_/If) (= (@ST (@I)) //T_/Equal) (= (@ST (@I)) //T_/Not_/Equal) (= (@ST (@I)) //T_/Less) (= (@ST (@I)) //T_/Greater) (= (@ST (@I)) //T_/Less_/Eq) (= (@ST (@I)) //T_/Greater_/Eq))
   (@Pass))
  ((or (= (@ST (@I)) //T_/Assignment) (= (@ST (@I)) //T_/Plus) (= (@ST (@I)) //T_/Times) (= (@ST (@I)) //T_/Max) (= (@ST (@I)) //T_/Min) (= (@ST (@I)) //T_/And) (= (@ST (@I)) //T_/Or))
   (cond
    ((= (@Size (@I)) 2)
     (@Pass))
    (#t
     (@Fail "The item does not have two components."))))
  (#t
   (@Fail "The selected item is not of a suitable type."))))

(define (@Reverse_Order_Code //Data)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__reverse_order__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S2_save //S2)
            (/__/S1_save //S1)
            (/__/B_save //B))
       (set! //S2 (vector-ref /__/Match_array 2))
       (set! //S1 (vector-ref /__/Match_array 1))
       (set! //B (vector-ref /__/Match_array 0))
       (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list (@Not //B) (@Make 17 '() //S2))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() //S1))))))
       (set! //S2 /__/S2_save)
       (set! //S1 /__/S1_save)
       (set! //B /__/B_save))))))
  ((= (@ST (@I)) //T_/If)
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__reverse_order__2 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/E2_save //E2)
            (/__/E1_save //E1)
            (/__/B_save //B))
       (set! //E2 (vector-ref /__/Match_array 2))
       (set! //E1 (vector-ref /__/Match_array 1))
       (set! //B (vector-ref /__/Match_array 0))
       (@Paste_Over (@Make 227 '() (list (@Not //B) (@Var_To_Expn //E2) (@Var_To_Expn //E1))))
       (set! //E2 /__/E2_save)
       (set! //E1 /__/E1_save)
       (set! //B /__/B_save))))))
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   (@Cut)
   (@Paste_After (@Buffer))
   (@Up))
  (#t
   (@Paste_Over (@Swap_Expn (@I))))))

