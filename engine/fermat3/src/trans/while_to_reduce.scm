;;; Scheme translation of WSL code
(define (/foreach-while_to_reduce-1 //Depth //A/S_/Type)
 (cond
  ((@Equal? /e2 (@I))
   (@Paste_Over /e1))))

(define /%const__while_to_reduce__1 (@Make 141 '() (list (@Make 314 '() (list (@Make 217 -1 '()) (@Make 217 -2 '()))) (@Make 17 '() (list (@Make 107 -3 '()))))))
(define /%const__while_to_reduce__2 (@Make 141 '() (list (@Make 316 '() (list (@Make 217 -1 '()) (@Make 217 -2 '()))) (@Make 17 '() (list (@Make 107 -3 '()) (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -1 '()) (@Make 220 '() (list (@Make 263 -1 '()) (@Make 205 1 '()))))) (@Make 6 '() (list (@Make 506 -4 '()) (@Make 217 -5 '()))))))))))
(define /%const__while_to_reduce__3 (@Make 141 '() (list (@Make 316 '() (list (@Make 217 -1 '()) (@Make 217 -2 '()))) (@Make 17 '() (list (@Make 107 -3 '()) (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -4 '()) (@Make 217 -5 '()))) (@Make 6 '() (list (@Make 512 -1 '()) (@Make 220 '() (list (@Make 263 -1 '()) (@Make 205 1 '()))))))))))))
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
(define (@While_To_Reduce_Test)
 (cond
  ((not (= (@ST (@I)) //T_/While))
   (@Fail "Current item is not a While loop"))
  (#t
   (@Edit)
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__while_to_reduce__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S_save //S)
            (/__n_save /n)
            (/__i_save /i))
       (set! //S (vector-ref /__/Match_array 2))
       (set! /n (vector-ref /__/Match_array 1))
       (set! /i (vector-ref /__/Match_array 0))
       (@Paste_Over (@Make 141 '() (list (@Make 316 '() (list (@Var_To_Expn /i) (@Make 221 '() (list (@Var_To_Expn /n) (@Make 205 1 '()))))) (@Make 17 '() //S))))
       (set! //S /__/S_save)
       (set! /n /__n_save)
       (set! /i /__i_save)))))
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__while_to_reduce__2 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__e_save /e)
            (/__v_save /v)
            (/__/S_save //S)
            (/__n_save /n)
            (/__i_save /i))
       (set! /e (vector-ref /__/Match_array 4))
       (set! /v (vector-ref /__/Match_array 3))
       (set! //S (vector-ref /__/Match_array 2))
       (set! /n (vector-ref /__/Match_array 1))
       (set! /i (vector-ref /__/Match_array 0))
       (@Paste_Over (@Make 141 '() (list (@Make 316 '() (list (@Var_To_Expn /i) (@Var_To_Expn /n))) (@Make 17 '() (append //S (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /e))) (@Make 6 '() (list (@Expn_To_Var /i) (@Make 220 '() (list (@Var_To_Expn /i) (@Make 205 1 '())))))))))))))
       (set! /e /__e_save)
       (set! /v /__v_save)
       (set! //S /__/S_save)
       (set! /n /__n_save)
       (set! /i /__i_save)))))
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__while_to_reduce__3 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__e_save /e)
            (/__v_save /v)
            (/__/S_save //S)
            (/__n_save /n)
            (/__i_save /i))
       (set! /e (vector-ref /__/Match_array 4))
       (set! /v (vector-ref /__/Match_array 3))
       (set! //S (vector-ref /__/Match_array 2))
       (set! /n (vector-ref /__/Match_array 1))
       (set! /i (vector-ref /__/Match_array 0))
       (cond
        ((not (@Set_Subset? (my-reduce @Set_Union (my-map @Stat_Types //S)) (list //T_/Assert)))
         (@Fail "Loop body is not in a suitable form"))
        ((@Elt_Clash? (@Elements /i) (@Elements /v))
         (@Fail "Loop index is assigned more than once"))
        ((null? (@WR_Reduce_Pars /v /e /i /n))
         (@Fail "Cannot determine the parameters for the REDUCE"))
        (#t
         (@Pass)))
       (set! /e /__e_save)
       (set! /v /__v_save)
       (set! //S /__/S_save)
       (set! /n /__n_save)
       (set! /i /__i_save)))
     (#t
      (@Fail "While loop is not in a suitable form"))))
   (@Undo_Edit))))

(define (@While_To_Reduce_Code //Data)
 (let ((/pars '())
       (/e1-save /e1)
       (/e2-save /e2)
       (/e3 '())
       (/new '()))
  (set! /e1 '())
  (set! /e2 '())
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__while_to_reduce__1 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__/S_save //S)
           (/__n_save /n)
           (/__i_save /i))
      (set! //S (vector-ref /__/Match_array 2))
      (set! /n (vector-ref /__/Match_array 1))
      (set! /i (vector-ref /__/Match_array 0))
      (@Paste_Over (@Make 141 '() (list (@Make 316 '() (list (@Var_To_Expn /i) (@Make 221 '() (list (@Var_To_Expn /n) (@Make 205 1 '()))))) (@Make 17 '() //S))))
      (set! //S /__/S_save)
      (set! /n /__n_save)
      (set! /i /__i_save)))))
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__while_to_reduce__2 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__e_save /e)
           (/__v_save /v)
           (/__/S_save //S)
           (/__n_save /n)
           (/__i_save /i))
      (set! /e (vector-ref /__/Match_array 4))
      (set! /v (vector-ref /__/Match_array 3))
      (set! //S (vector-ref /__/Match_array 2))
      (set! /n (vector-ref /__/Match_array 1))
      (set! /i (vector-ref /__/Match_array 0))
      (@Paste_Over (@Make 141 '() (list (@Make 316 '() (list (@Var_To_Expn /i) (@Var_To_Expn /n))) (@Make 17 '() (append //S (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /e))) (@Make 6 '() (list (@Expn_To_Var /i) (@Make 220 '() (list (@Var_To_Expn /i) (@Make 205 1 '())))))))))))))
      (set! /e /__e_save)
      (set! /v /__v_save)
      (set! //S /__/S_save)
      (set! /n /__n_save)
      (set! /i /__i_save)))))
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__while_to_reduce__3 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__e_save /e)
           (/__v_save /v)
           (/__/S_save //S)
           (/__n_save /n)
           (/__i_save /i))
      (set! /e (vector-ref /__/Match_array 4))
      (set! /v (vector-ref /__/Match_array 3))
      (set! //S (vector-ref /__/Match_array 2))
      (set! /n (vector-ref /__/Match_array 1))
      (set! /i (vector-ref /__/Match_array 0))
      (set! /pars (@WR_Reduce_Pars /v /e /i /n))
      (cond
       ((= (gen-length /pars) 1)
        (set! /e3 (wsl-ref /pars 1)))
       (#t
        ; Replace e in the assertion by the whole sequence 
        (set! /e1 (wsl-ref /pars 2))
        (set! /e2 (wsl-ref /pars 3))
        (set! /new '())
        (@Edit)
        (for-in //S1 //S 
         (cond
          ((= (@ST //S1) //T_/Assert)
           (@New_Program //S1)
           (@Foreach_Expn /foreach-while_to_reduce-1 0 (@AS_Type) 0)
           (cond
            ((null? (@Program))
             (@New_Program (@Skips))))
           ; Collect the conditions in the assertions 
           (set! /new (cons (@Get_n (@I) 1) /new)))))
        (@Undo_Edit)
        (cond
         ((not (null? /new))
          (@Paste_Before (@Make //T_/Assert '() (list (@Simplify_Cond (@Make //T_/And '() /new)))))
          (@Right)))
        (set! /e3 (@Make //T_/Reduce '() (@Sub_Seg /pars 1 2)))
        (cond
         ((or (= (@ST /e) //T_/Funct_/Call) (= (@ST /e) //T_/X_/Funct_/Call) (= (@ST /e) //T_/M/W_/Funct_/Call))
          (set! /e3 (@Make (@ST /e) '() (list (@Get_n /e 1) (@Make //T_/Expressions '() (list (@Lvalue_To_Expn /v) /e3))))))
         (#t
          (set! /e3 (@Make (@ST /e) '() (list (@Lvalue_To_Expn /v) /e3)))))))
      (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /e3))))))
      #t
      (set! /e /__e_save)
      (set! /v /__v_save)
      (set! //S /__/S_save)
      (set! /n /__n_save)
      (set! /i /__i_save)))))
  (set! /e1 /e1-save)
  (set! /e2 /e2-save)))

; Try to work out the parameters for the REDUCE operation: 
(define (@WR_Reduce_Pars /vv /e-par /i-par /n-par)
 (let ((/n-save /n)
       (/i-save /i)
       (/e-save /e)
       (/v-save /v)
       (/name '())
       (//R '())
       (funct-result '()))
  (set! /n /n-par)
  (set! /i /i-par)
  (set! /e /e-par)
  (set! /v (@Lvalue_To_Expn /vv))
  (cond
   ((and (= (@Size /e) 2) (member (@ST /e) //Comm_/Ops) (@Equal? /v (@Get_n /e 2)))
    (set! /e (@Make (@ST /e) '() (list (@Get_n /e 2) (@Get_n /e 1))))))
  (cond
   (#f
    (display-list "v = ")
    (@Print_WSL /v "")
    (display-list "e = ")
    (@Print_WSL /e "")
    (display-list "i = ")
    (@Print_WSL /i "")))
  (cond
   ((and (or (= (@ST /e) //T_/Funct_/Call) (= (@ST /e) //T_/X_/Funct_/Call) (= (@ST /e) //T_/M/W_/Funct_/Call)) (= (@Size (@Get_n /e 2)) 2) (@Equal? /v (@Get_n (@Get_n /e 2) 1)))
    (set! //R (@WR_Reduce_Par2 (@Get_n /e 1) /v (@Get_n (@Get_n /e 2) 2) /i /n)))
   ((and (or (= (@ST /e) //T_/Plus) (= (@ST /e) //T_/Minus) (= (@ST /e) //T_/Times) (= (@ST /e) //T_/Divide) (= (@ST /e) //T_/Union) (= (@ST /e) //T_/Intersection) (= (@ST /e) //T_/Concat) (= (@ST /e) //T_/Max) (= (@ST /e) //T_/Min) (= (@ST /e) //T_/Exponent)) (= (@Size /e) 2) (@Equal? /v (@Get_n /e 1)))
    (cond
     ((= (@ST /e) //T_/Plus)
      (set! /name (@Name (@Make_Name "+"))))
     ((= (@ST /e) //T_/Minus)
      (set! /name (@Name (@Make_Name "-"))))
     ((= (@ST /e) //T_/Times)
      (set! /name (@Name (@Make_Name "*"))))
     ((= (@ST /e) //T_/Divide)
      (set! /name (@Name (@Make_Name "/"))))
     ((= (@ST /e) //T_/Union)
      (set! /name (@Name (@Make_Name "\\/"))))
     ((= (@ST /e) //T_/Intersection)
      (set! /name (@Name (@Make_Name "/\\"))))
     ((= (@ST /e) //T_/Concat)
      (set! /name (@Name (@Make_Name "++"))))
     ((= (@ST /e) //T_/Exponent)
      (set! /name (@Name (@Make_Name "**"))))
     (#t
      (set! /name (@Name (@Make_Name (@Type_Name (@ST /e)))))))
    (set! //R (@WR_Reduce_Par2 /name /v (@Get_n /e 2) /i /n))))
  (set! funct-result //R)
  (set! /n /n-save)
  (set! /i /i-save)
  (set! /e /e-save)
  (set! /v /v-save)
  funct-result))

; Compute the second parameter (the list of elements) for the REDUCE 
(define (@WR_Reduce_Par2 /par1 /v-par /e-par /i-par /n-par)
 (let ((/n-save /n)
       (/i-save /i)
       (/e-save /e)
       (/v-save /v)
       (//R '())
       (funct-result '()))
  (set! /n /n-par)
  (set! /i /i-par)
  (set! /e /e-par)
  (set! /v /v-par)
  (cond
   ((and (= (@ST /e) //T_/Aref) (= (@Size (@Get_n /e 2)) 1) (@Equal? (@Get_n (@Get_n /e 2) 1) /i))
    (set! //R (list /par1 (@Make //T_/Sub_/Seg '() (list (@Get_n /e 1) /i /n)) /e)))
   ((= (@ST /e) //T_/Number)
    (set! //R (list (@Simplify_Expn (@Make 222 '() (list (@Var_To_Expn /e) (@Make 220 '() (list (@Make 221 '() (list (@Var_To_Expn /n) (@Var_To_Expn /i))) (@Make 205 1 '()))))))))))
  (set! funct-result //R)
  (set! /n /n-save)
  (set! /i /i-save)
  (set! /e /e-save)
  (set! /v /v-save)
  funct-result))

#t
