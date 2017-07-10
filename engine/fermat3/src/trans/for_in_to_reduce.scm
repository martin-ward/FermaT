;;; Scheme translation of WSL code
(define /%const__for_in_to_reduce__1 (@Make 154 '() (list (@Make 506 -1 '()) (@Make 217 -2 '()) (@Make 17 '() (list (@Make 107 -3 '()) (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -4 '()) (@Make 217 -5 '()))))))))))
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
(define (@For_In_To_Reduce_Test)
 (cond
  ((not (= (@ST (@I)) //T_/For_/In))
   (@Fail "Current item is not a FOR x IN y loop"))
  (#t
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__for_in_to_reduce__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__e_save /e)
            (/__v_save /v)
            (/__/S_save //S)
            (/__/L_save //L)
            (/__i_save /i))
       (set! /e (vector-ref /__/Match_array 4))
       (set! /v (vector-ref /__/Match_array 3))
       (set! //S (vector-ref /__/Match_array 2))
       (set! //L (vector-ref /__/Match_array 1))
       (set! /i (vector-ref /__/Match_array 0))
       (cond
        ((not (@Set_Subset? (my-reduce @Set_Union (my-map @Stat_Types //S)) (list //T_/Assert)))
         (@Fail "Loop body is not in a suitable form"))
        ((null? (@FR_Reduce_Pars /v /e /i //L))
         (@Fail "Cannot determine the parameters for the REDUCE"))
        (#t
         (@Pass)))
       (set! /e /__e_save)
       (set! /v /__v_save)
       (set! //S /__/S_save)
       (set! //L /__/L_save)
       (set! /i /__i_save)))
     (#t
      (@Fail "FOR loop is not in a suitable form")))))))

(define (@For_In_To_Reduce_Code //Data)
 (let ((/pars '())
       (/e1 '())
       (/e2 '())
       (/e3 '())
       (/new '()))
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__for_in_to_reduce__1 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__e_save /e)
           (/__v_save /v)
           (/__/S_save //S)
           (/__/L_save //L)
           (/__i_save /i))
      (set! /e (vector-ref /__/Match_array 4))
      (set! /v (vector-ref /__/Match_array 3))
      (set! //S (vector-ref /__/Match_array 2))
      (set! //L (vector-ref /__/Match_array 1))
      (set! /i (vector-ref /__/Match_array 0))
      (set! /pars (@FR_Reduce_Pars /v /e /i //L))
      (cond
       ((= (gen-length /pars) 1)
        (set! /e3 (wsl-ref /pars 1)))
       (#t
        ; Replace i in the assertion by the whole sequence, ie L 
        (set! /e1 (wsl-ref /pars 2))
        (set! /new '())
        (for-in //S1 //S 
         (set! /new (cons (@Replace //S1 //L (@Lvalue_To_Expn /i)) /new)))
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
      (set! //L /__/L_save)
      (set! /i /__i_save)))))))

; Try to work out the parameters for the REDUCE operation: 
(define (@FR_Reduce_Pars /vv /e-par /i-par //L-par)
 (let ((//L-save //L)
       (/i-save /i)
       (/e-save /e)
       (/v-save /v)
       (/name '())
       (//R '())
       (funct-result '()))
  (set! //L //L-par)
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
    (set! //R (@FR_Reduce_Par2 (@Get_n /e 1) /v (@Get_n (@Get_n /e 2) 2) /i //L)))
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
    (set! //R (@FR_Reduce_Par2 /name /v (@Get_n /e 2) /i //L))))
  (set! funct-result //R)
  (set! //L //L-save)
  (set! /i /i-save)
  (set! /e /e-save)
  (set! /v /v-save)
  funct-result))

; Compute the second parameter (the list of elements) for the REDUCE 
(define (@FR_Reduce_Par2 /par1 /v-par /e-par /i-par //L-par)
 (let ((//L-save //L)
       (/i-save /i)
       (/e-save /e)
       (/v-save /v)
       (//R '())
       (funct-result '()))
  (set! //L //L-par)
  (set! /i /i-par)
  (set! /e /e-par)
  (set! /v /v-par)
  (cond
   (#f
    (display-list "v = ")
    (@Print_WSL /v "")
    (display-list "e = ")
    (@Print_WSL /e "")
    (display-list "i = ")
    (@Print_WSL /i "")
    (display-list "L = ")
    (@Print_WSL //L "")))
  (cond
   ((@LR_Equal? /e /i)
    (set! //R (list /par1 //L)))
   ((= (@ST /e) //T_/Number)
    (set! //R (list (@Simplify_Expn (@Make 222 '() (list (@Var_To_Expn /e) (@Make 244 '() (list (@Var_To_Expn //L))))))))))
  (set! funct-result //R)
  (set! //L //L-save)
  (set! /i /i-save)
  (set! /e /e-save)
  (set! /v /v-save)
  funct-result))

#t
