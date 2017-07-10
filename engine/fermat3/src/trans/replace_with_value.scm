;;; Scheme translation of WSL code
(define (/foreach-replace_with_value-1 //Depth //A/S_/Type)
 (cond
  ((equal? (@V (@I)) (car //V))
   (@Paste_Over //R2))))

(define (/foreach-replace_with_value-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Struct)
   (cond
    ((equal? (@Struct_Elts (@I)) //V)
     (@Paste_Over //R2))))))

(define /%const__replace_with_value__1 (@Make 109 '() (list (@Make 313 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 217 -2 '()))))))
(define /%const__replace_with_value__2 (@Make 109 '() (list (@Make 313 '() (list (@Make 217 -1 '()) (@Make 261 '() (list (@Make 205 2 '()))))))))
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
(define (@Replace_With_Value_Test)
 (cond
  ((and (not (= (@ST (@I)) //T_/Variable)) (not (= (@ST (@I)) //T_/Struct)) (not (and (= (@ST (@I)) //T_/Aref) (= (@Size (@Get_n (@I) 2)) 1) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Number))))
   (@Fail "The selected item is not a variable in an expression."))
  (#t
   (let ((/val (@Find_Value (@Struct_Elts (@I)))))
    (cond
     ((null? /val)
      (@Fail "Cannot easily determine the value."))
     ((> (@Posn_n) 1)
      (@Pass))
     ((and (or (= (@ST (@Parent)) //T_/Aref) (= (@ST (@Parent)) //T_/Sub_/Seg) (= (@ST (@Parent)) //T_/Rel_/Seg)) (not (= (@ST /val) //T_/Variable)))
      (@Fail "Replacement would lead to bad syntax."))
     ((and (or (= (@ST (@Parent)) //T_/Aref_/Lvalue) (= (@ST (@Parent)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Parent)) //T_/Rel_/Seg_/Lvalue)) (not (= (@ST /val) //T_/Var_/Lvalue)))
      (@Fail "Replacement would lead to bad syntax."))
     (#t
      (@Pass)))))))

(define (@Replace_With_Value_Code //Data)
 (let ((/val (@Find_Value (@Struct_Elts (@I)))))
  (cond
   ((null? /val)
    (display-list "ERROR in Replace_With_Value!!!"))
   (#t
    (@Paste_Over /val)))))

; Find the current value of the given variable name. 
; Return an item, or < > if the value cannot be found 
(define (@Find_Value //V-par)
 (let ((//V-save //V)
       (//E (@Elt_To_Expn //V-par))
       (//Orig_/Posn (@Posn))
       (/move 1)
       (//Result-save //Result)
       (/clobbered '())
       (/found 0)
       (//Calls (@Make_Set (list //T_/M/W_/Proc_/Call //T_/X_/Proc_/Call //T_/Proc_/Call)))
       (/registers (@Make_Set (my-map @Make_Name (list "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15"))))
       (funct-result '()))
  (set! //V //V-par)
  (set! //Result '())
  ; Repeated move left and up until we get a value for the variable. 
  ; If the variable is defined in terms of clobbered variables, then give up. 
  (while (and (or (and (not (= (@ST (@Parent)) //T_/M/W_/Funct)) (not (= (@ST (@Parent)) //T_/M/W_/B/Funct))) (not (= (@Posn_n) 5))) (not (= //T_/Statement (@GT (@I)))) (@Up?)) 
   (@Up))
  (cond
   ((and (or (= (@ST (@Parent)) //T_/M/W_/Funct) (= (@ST (@Parent)) //T_/M/W_/B/Funct)) (not (= //T_/Statement (@GT (@I)))) (= (@Posn_n) 5))
    (@Left)
    (@Down_Last)
    (set! /move 0)))
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    (cond
     ((and (= /move 1) (or (= (@ST (@I)) //T_/Floop) (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/A_/S)))
      ; If there are no calls and V is not assigned in the loop or system, 
      ; then we can safely get its value from outside: 
      (cond
       ((or (not (null? (intersection-n (@Stat_Types (@I)) //Calls))) (@Elt_Clash? (@Elts_Assigned (@I)) //V))
        (set! //Result '())
        (set! /fl_flag2 1))
       (#t
        (set! /fl_flag2 0))))
     (#t
      (set! /fl_flag2 0)))
    (cond
     ((= /fl_flag2 0)
      (cond
       ((not (= (@GT (@I)) //T_/Statement))
        (set! //Result '())
        (set! /fl_flag2 1))
       ((or (= /move 0) (@Left?))
        (cond
         ((= /move 0)
          (set! /move 1))
         (#t
          (@Left)))
        (while (and (= (@ST (@I)) //T_/Var) (not (@Elt_Clash? (@Elts_Assigned (@Get_n (@I) 1)) //V)) (@Elt_Clash? (@Elts_Assigned (@I)) //V)) 
         (begin
          ; V is not a local var and is assigned in the VAR, so look inside: 
          (set! /clobbered (union-n /clobbered (@Elts_Assigned (@Get_n (@I) 1))))
          (@Down_Last)
          (@Down_Last)))
        (cond
         ((= (@ST (@I)) //T_/Assert)
          (let ((/__/O/K 1))
           (vector-set! /__/Match_array 0 //E)
           (set! /__/O/K (@New_Match  /%const__replace_with_value__1 (@I) /__/O/K))
           (cond
            ((= /__/O/K 1)
             (let ((/__/X_save //X))
              (set! //X (vector-ref /__/Match_array 1))
              (set! //Result //X)
              (set! /found 1)
              (set! //X /__/X_save)))
            (#t
             (let ((/__/O/K 1))
              (vector-set! /__/Match_array 1 //E)
              (set! /__/O/K (@New_Match  /%const__replace_with_value__2 (@I) /__/O/K))
              (cond
               ((= /__/O/K 1)
                (let ((/__/X_save //X))
                 (set! //X (vector-ref /__/Match_array 0))
                 (set! //Result //X)
                 (set! /found 1)
                 (set! //X /__/X_save)))
               (#t
                (set! /found 0)))))))
          (cond
           ((= /found 1)
            (set! /fl_flag2 1))
           (#t
            (set! /fl_flag2 0))))
         ((and (= (@ST (@I)) //T_/Assignment) (member //V (@Elts_Assigned (@I))))
          (set! /clobbered (union-n /clobbered (@Set_Difference (@Elts_Assigned (@I)) (list //V))))
          (@Down)
          ; to first assign 
          (set! /fl_flag1 0)
          (while (= /fl_flag1 0) 
           (cond
            ((equal? //V (@Struct_Elts (@Get_n (@I) 1)))
             (set! //Result (@Get_n (@I) 2))
             (set! /fl_flag1 1))
            ((and (= (@ST (@Get_n (@I) 1)) //T_/Aref_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@Size (@Get_n (@Get_n (@I) 1) 2)) 1) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1)) //T_/Number) (equal? //V (list (@V (@Get_n (@Get_n (@I) 1) 1)) (- (@V (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1))))))
             ; TODO: Fix this hack! (Check for foo[n] := exp when we are looking for foo[n]) 
             (set! //Result (@Get_n (@I) 2))
             (set! /fl_flag1 1))
            ((not (@Right?))
             (set! /fl_flag1 1))
            (#t
             (@Right)
             (set! /fl_flag1 0))))
          (@Up)
          (cond
           ((not (null? //Result))
            ; Check if V is defined in terms of itself 
            (cond
             ((member (car //V) (@Used //Result))
              (set! //Result (@FV_Self_Assign  //V /clobbered //Result))))
            (set! /fl_flag2 1))
           (#t
            (set! /fl_flag2 0))))
         ((and (= (@ST (@I)) //T_/Assignment) (@Equal? (@Get_n (@Get_n (@I) 1) 2) //E) (not (and (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@Get_n (@I) 1) 1)) /registers))))
          ; The variable we are interested in was saved in a non-reg 
          (set! //Result (@Lvalue_To_Expn (@Get_n (@Get_n (@I) 1) 1)))
          (set! /found 1)
          (set! /fl_flag2 0))
         ((or (@Elt_Clash? (@Elts_Assigned (@I)) //V) (not (null? (intersection-n (@Stat_Types (@I)) //Calls))))
          (set! //Result '())
          (set! /fl_flag2 1))
         (#t
          (set! /fl_flag2 0)))
        (cond
         ((= /fl_flag2 0)
          (set! /clobbered (union-n /clobbered (@Elts_Assigned (@I))))
          ; Hack: record an assignment to a[a[...]] or a[rx...] 
          (cond
           ((and (= (@ST (@I)) //T_/Assignment) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Rel_/Seg_/Lvalue)) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) /a_name) (or (member /a_name (@Used (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2))) (not (null? (intersection-n /registers (@Used (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)))))))
            (set! /clobbered (union-n /clobbered (list (list /a_name /a_name))))))
          ; Hack: record an assignment to a[a[...]] or a[rx...] 
          (cond
           ((and (= (@ST (@I)) //T_/Assignment) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Mem_/Seg_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Mem_/Rel_/Lvalue)) (or (or (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Mem_/Seg) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Mem_/Rel) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Mem)) (not (null? (intersection-n /registers (@Used (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)))))))
            (set! /clobbered (union-n /clobbered (list (list /a_name /a_name))))))
          ; Hack: record an assignment to a[v_xxx...] as if it assigned to v_xxx 
          (cond
           ((and (= (@ST (@I)) //T_/Assignment) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Rel_/Seg_/Lvalue)) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) /a_name) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) //T_/Variable) (or (@Starts_With? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) "v_") (@Starts_With? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) "V_")))
            (set! /clobbered (union-n /clobbered (list (list (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2))))))))
          (cond
           ((and (member //T_/A_/Proc_/Call (@Stat_Types (@I))) (not (null? //A_/Proc_/Call_/Filter)))
            (cond
             ((not-member (car //V) //A_/Proc_/Call_/Filter)
              (set! //Result '())
              (set! /fl_flag2 1))
             (#t
              (set! /fl_flag2 0))))
           (#t
            (set! /fl_flag2 0))))))
       ((not (@Up?))
        (set! //Result '())
        (set! /fl_flag2 1))
       (#t
        (@Up)
        (cond
         ((not (@Up?))
          (set! //Result '())
          (set! /fl_flag2 1))
         (#t
          (@Up)
          ; to item containing the statement sequence 
          (cond
           ((and (= (@ST (@I)) //T_/Var) (member //V (@Elt_Lvars (@Get_n (@I) 1))))
            ; V is a local variable 
            (@Down)
            (@Down)
            ; to the first assign 
            (while (not (equal? (@Struct_Elts (@Get_n (@I) 1)) //V)) 
             (@Right))
            (set! //Result (@Get_n (@I) 2))
            ; Can't do a recursive search if this V is defined 
            ; in terms of itself. 
            (cond
             ((@Elt_Clash? (@Elts_Used //Result) //V)
              (set! //Result '())))
            (set! /fl_flag2 1))
           ((and (or (= (@ST (@I)) //T_/M/W_/Funct) (= (@ST (@I)) //T_/M/W_/B/Funct)) (member //V (@Elt_Lvars (@Get_n (@I) 3))))
            ; V is a local variable 
            (@Down_To 3)
            (@Down)
            ; to the first assign 
            (while (not (equal? (@Struct_Elts (@Get_n (@I) 1)) //V)) 
             (@Right))
            (set! //Result (@Get_n (@I) 2))
            ; Can't do a recursive search if this V is defined 
            ; in terms of itself. 
            (cond
             ((@Elt_Clash? (@Elts_Used //Result) //V)
              (set! //Result '())))
            (set! /fl_flag2 1))
           ((= (@ST (@I)) //T_/Var)
            ; V is global to the var clause, keep scanning left 
            (set! /fl_flag2 0))
           ((and (= (@GT (@I)) //T_/Guarded) (@Up?))
            (@Up)
            (set! /fl_flag2 0))
           ((and (or (= (@ST (@I)) //T_/Floop) (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/A_/S)) (@Up?))
            ; If V is not assigned in the loop or system, 
            ; and there are no proc calls in the loop (which could modify V) 
            ; then get its value from outside. 
            (cond
             ((not (null? (intersection-n (@Stat_Types (@I)) //Calls)))
              (set! //Result '())
              (set! /fl_flag2 1))
             ((@Elt_Clash? (@Elts_Assigned (@I)) //V)
              (set! //Result '())
              (set! /fl_flag2 1))
             (#t
              (set! /clobbered (union-n /clobbered (@Elts_Assigned (@I))))
              (set! /fl_flag2 0))))
           ((and (= (@ST (@I)) //T_/Action) (> (gen-length (@Posn)) 2))
            (@Up)
            (@Up)
            ; to the action system 
            (set! /fl_flag2 0))
           ((and (= (@ST (@I)) //T_/Where) (not-member //V (@Assigned (@I))))
            ; keep scanning left 
            (set! /fl_flag2 0))
           (#t
            (set! //Result '())
            (set! /fl_flag2 1)))))))))))
  (@Goto //Orig_/Posn)
  ; If a variable in the expression has been clobbered, then give up 
  (cond
   (#f
    (display-list " V = " (my-map @N_String //V) " ")
    (display-list-flush "Result = ")
    (cond
     ((null? //Result)
      (display-list '()))
     (#t
      (@PP_Item //Result 80 "")
      (display-list-flush "Used      = ")
      (@Print_Elts (@Elts_Used //Result))
      (display-list-flush "clobbered = ")
      (@Print_Elts /clobbered)))))
  ; Take <a_name> from clobbered since this is too broad? 
  (cond
   ((and (member (list /a_name /a_name) /clobbered) (not (null? //Result)))
    (cond
     ((and (or (= (@ST //Result) //T_/Sub_/Seg) (= (@ST //Result) //T_/Rel_/Seg)) (= (@ST (@Get_n //Result 1)) //T_/Variable) (equal? (@V (@Get_n //Result 1)) /a_name) (or (member /a_name (@Used (@Get_n //Result 2))) (not (null? (intersection-n /registers (@Used (@Get_n //Result 2)))))))
      (set! //Result '()))
     (#t
      (set! /clobbered (@Set_Difference /clobbered (list (list /a_name /a_name))))))))
  (cond
   ((and (not (null? //Result)) (@Elt_Clash_List? (@Elts_Used //Result) (@Set_Difference /clobbered (list (list /a_name)))))
    (set! //Result '())))
  (set! funct-result //Result)
  (set! //V //V-save)
  (set! //Result //Result-save)
  funct-result))

; The variable name in V appears somewhere in the result. 
; If a proper prefix of V, or an extension of V is used, then fail. 
; If V itself is used, try a recursive call to @Find_Value to get rid of V in Result 
; Otherwise, we are OK (only the other components of the structure are used) 
; NB: also check for foo := a[foo].bar 
(define (@FV_Self_Assign //V-par /clobbered //Result-par)
 (let ((//Result-save //Result)
       (//V-save //V)
       (funct-result '()))
  (set! //Result //Result-par)
  (set! //V //V-par)
  (let ((/used (@Elts_Used //Result))
        (/recurse 0))
   (while (and (not (null? /used)) (not (null? //Result))) 
    (begin
     (cond
      ((equal? (car /used) //V)
       (set! /recurse 1))
      ((@Prefix? (car /used) //V)
       (set! //Result '()))
      ((@Prefix? //V (car /used))
       (set! //Result '()))
      ((member (car //V) (car /used))
       (set! //Result '())))
     (set! /used (cdr /used))))
   (cond
    ((and (not (null? //Result)) (= /recurse 1))
     (cond
      ((@Elt_Clash_List? /clobbered /used)
       (set! //Result '()))
      (#t
       (let ((//R2-save //R2))
        (set! //R2 (@Find_Value //V))
        ; Replace V by R2 in Result 
        (cond
         ((null? //R2)
          (set! //Result '()))
         (#t
          (@Edit)
          (@New_Program //Result)
          (cond
           ((= (gen-length //V) 1)
            (@Foreach_Variable /foreach-replace_with_value-1 0 (@AS_Type) 0)
            (cond
             ((null? (@Program))
              (@New_Program (@Skips)))))
           (#t
            (@Foreach_Expn /foreach-replace_with_value-2 0 (@AS_Type) 0)
            (cond
             ((null? (@Program))
              (@New_Program (@Skips))))))
          (set! //Result (@Program))
          (@Undo_Edit)))
        (set! //R2 //R2-save)))))))
  (set! funct-result //Result)
  (set! //Result //Result-save)
  (set! //V //V-save)
  funct-result))

#t
