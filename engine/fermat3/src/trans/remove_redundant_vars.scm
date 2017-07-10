;;; Scheme translation of WSL code
(define (/foreach-remove_redundant_vars-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) (@V /x)))
   (@Paste_Over /value))))

(define (/foreach-remove_redundant_vars-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) (@V /x)))
   (@Paste_Over /value))))

(define (/foreach-remove_redundant_vars-3 //Depth //A/S_/Type)
 (cond
  ((not (null? (gethash /new (@V (@I)))))
   (@Paste_Over (@Make (@ST (@I)) (gethash /new (@V (@I))) '())))))

(define (/foreach-remove_redundant_vars-4 //Depth //A/S_/Type)
 (cond
  ((member (@V (@I)) /redundant)
   ; It is OK to replace a global Lvalue with a variable 
   ; -- the loop will fix it for us. 
   (@Paste_Over /dummy))))

(define (/foreach-remove_redundant_vars-5 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assert)
   (cond
    ((member /dummy (@Used (@I)))
     (@Delete))))
  ((and (= (@ST (@I)) //T_/Push) (member /dummy (@Assigned (@Get_n (@I) 1))))
   (@Delete))
  ((and (= (@ST (@I)) //T_/Pop) (member /dummy (@Assigned (@Get_n (@I) 2))))
   (@Delete))
  ((and (= (@ST (@I)) //T_/Pop) (member /dummy (@Assigned (@Get_n (@I) 1))))
   ; We need to pop the stack, but don't care where the value goes 
   ; so in this case we leave the dummy behind 
   ; (Could do Stack := TAIL(Stack) but this would upset Push_Pop!) 
  )
  ((and (= (@ST (@I)) //T_/Assignment) (member /dummy (@Variables (@I))))
   ; Delete the assigns which use dummy 
   (cond
    ((= (@Size (@I)) 1)
     (@Delete))
    (#t
     (@Down)
     ; to first assign 
     (while (or (member /dummy (@Variables (@I))) (@Right?)) 
      (cond
       ((member /dummy (@Variables (@I)))
        (@Clever_Delete))
       (#t
        (@Right)))))))))

(define (/foreach-remove_redundant_vars-6 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (cond
    ((and (= (@Size (@I)) 1) (@Equal? (@Get_n (@Get_n (@I) 1) 1) /v) (@Equal? (@Get_n (@Get_n (@I) 1) 2) /e))
     ; This assignment is OK 
    )
    ((member (@V /v) (@Assigned (@I)))
     (set! /keep 1))))
  ((or (= (@ST (@I)) //T_/Proc_/Call) (= (@ST (@I)) //T_/A_/Proc_/Call) (= (@ST (@I)) //T_/M/W_/Proc_/Call) (= (@ST (@I)) //T_/Push) (= (@ST (@I)) //T_/Pop))
   (cond
    ((member (@V /v) (@Assigned (@I)))
     (set! /keep 1))))))

(define (/foreach-remove_redundant_vars-7 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) /v) (@Up?))
   (cond
    ((= (@ST (@Parent)) //T_/Struct)
     (set! /keep 1))
    ((and (= (@Posn_n) 1) (or (= (@ST (@Parent)) //T_/Aref) (= (@ST (@Parent)) //T_/Sub_/Seg) (= (@ST (@Parent)) //T_/Rel_/Seg)))
     (set! /keep 1))))))

(define (/foreach-remove_redundant_vars-8 //Depth //A/S_/Type)
 (cond
  ((equal? (@V (@I)) (@V /v))
   (@Paste_Over /dummy))))

(define (/foreach-remove_redundant_vars-9 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) (@V /dummy)))
   (@Paste_Over /e))))

(define (/foreach-remove_redundant_vars-10 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (vector-set! /__/Match_array 0 /e)
  (set! /__/O/K (@New_Match  /%const__remove_redundant_vars__4 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (@Delete))
   (#t
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__remove_redundant_vars__5 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__e_save /e))
        (set! /e (vector-ref /__/Match_array 0))
        (@Delete)
        (set! /e /__e_save)))))))))

(define (/foreach-remove_redundant_vars-11 //Depth //A/S_/Type)
 (@Down)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (begin
   (cond
    ((and (= (@ST (@I)) //T_/Cond) (@Right?) (= (gen-length (@Assigned (@I))) 1) (= (gen-length (intersection-n (@Assigned (@I)) //V/L)) 1))
     (let ((/v-save /v))
      (set! /v (car (intersection-n (@Assigned (@I)) //V/L)))
      (@Right)
      (cond
       ((and (= (@ST (@I)) //T_/Cond) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Equal) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Not_/Equal)) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Variable) (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) /v) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) //T_/Number))
        (@Trans //T/R_/Merge_/Left "")
        (@Trans //T/R_/Constant_/Propagation "")
        (set! /found 1))
       (#t
        (@Left)))
      (set! /v /v-save))))
   (cond
    ((not (@Right?))
     (set! /fl_flag1 1))
    (#t
     (@Right)
     (set! /fl_flag1 0))))))

(define (/foreach-remove_redundant_vars-12 //Depth //A/S_/Type)
 (@Down)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (begin
   (cond
    ((and (= (@ST (@I)) //T_/Cond) (@Right?) (= (gen-length (@Assigned (@I))) 1) (= (gen-length (intersection-n (@Assigned (@I)) //V/L)) 1))
     (let ((/v-save /v))
      (set! /v (car (intersection-n (@Assigned (@I)) //V/L)))
      (@Right)
      (cond
       ((and (= (@ST (@I)) //T_/Cond) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Equal) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Not_/Equal)) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Variable) (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) /v) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) //T_/Number))
        (@Trans //T/R_/Merge_/Left "")
        (@Trans //T/R_/Constant_/Propagation "")
        (set! /found 1))
       (#t
        (@Left)))
      (set! /v /v-save))))
   (cond
    ((not (@Right?))
     (set! /fl_flag1 1))
    (#t
     (@Right)
     (set! /fl_flag1 0))))))

(define /%const__remove_redundant_vars__1 (@Make 17 '() (list (@Make 107 -1 '()) (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -2 '()) (@Make 261 '() (list (@Make 205 3 '()))))))) (@Make 107 -4 '()))))
(define /%const__remove_redundant_vars__2 (@Make 17 '() (list (@Make 107 -1 '()) (@Make 110 '() (list (@Make 6 '() (list (@Make 510 '() (list (@Make 205 2 '()))) (@Make 217 -3 '()))))) (@Make 107 -4 '()))))
(define /%const__remove_redundant_vars__3 (@Make 17 '() (list (@Make 140 '() (list (@Make 17 '() (list (@Make 107 -1 '()) (@Make 110 '() (list (@Make 6 '() (list (@Make 510 '() (list (@Make 205 2 '()))) (@Make 217 -3 '()))))) (@Make 107 -4 '()))) (@Make 14 '() (list (@Make 406 -5 '()))))))))
(define /%const__remove_redundant_vars__4 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "__DUMMY__") '()) (@Make 261 '() (list (@Make 205 1 '()))))))))
(define /%const__remove_redundant_vars__5 (@Make 137 '() (list (@Make 501 (@Make_Name "__DUMMY__") '()) (@Make 217 -1 '()))))
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
; @PPC_A_Proc_Call_os? returns TRUE if the given item contains an A_Proc_Call 
; with an os argument, and A_Proc_Call_Filter is not empty. 
; This is because the A_Proc_Call might use the local variable. 
; Hack: we disable this test for a top-level VAR (eg df in x86 code) 
(define (@Remove_Redundant_Vars_Test)
 (cond
  ((and (null? (@Posn)) (= (@ST (@I)) //T_/Statements))
   (@Down)))
 (cond
  ((= (@ST (@I)) //T_/Var)
   (cond
    ((and (>= (gen-length (@Posn)) 2) (= (@ST (@GParent)) //T_/Where) (= (@Size (@Parent)) 1))
     (cond
      ((or (@PPC_A_Proc_Call_os? (@GParent)) (member //T_/X_/Proc_/Call (@Stat_Types (@GParent))))
       (@Fail "Local variables might be used in a called procedure"))
      ((not (null? (@Proc_Calls (@GParent))))
       (@Fail "Local variables might be used in a local procedure"))
      (#t
       (@RRV_sub_Test (@Lvars (@Get_n (@I) 1))))))
    ((or (and (not (equal? (@Posn) (list 1))) (@PPC_A_Proc_Call_os? (@I))) (member //T_/X_/Proc_/Call (@Stat_Types (@I))))
     (@Fail "Local variables might be used in a called procedure"))
    ((and (not (null? (@Proc_Calls (@I)))) (not (null? (intersection-n (@RRV_Proc_Vars) (@Assigned (@Get_n (@I) 1))))))
     (@Fail "Local variables might be used in a local procedure"))
    (#t
     (@RRV_sub_Test (@Lvars (@Get_n (@I) 1))))))
  ((or (= (@ST (@I)) //T_/M/W_/Funct) (= (@ST (@I)) //T_/M/W_/B/Funct))
   (@RRV_Funct_Test (@Lvars (@Get_n (@I) 3))))
  (#t
   (@Fail "The selected item is not a VAR construct or function declaration."))))

(define (@RRV_Funct_Test //V/L-par)
 (let ((//V/L-save //V/L))
  (set! //V/L //V/L-par)
  (cond
   ((not (null? (@Set_Difference (@Set_Difference //V/L (@Used (@Get_n (@I) 4))) (@Used (@Get_n (@I) 5)))))
    (@Pass))
   ((not (null? (intersection-n //V/L (@Assd_To_Self (@Get_n (@I) 4)))))
    (@Pass))
   (#t
    ; There are references to all the local vars. See if some can be removed. 
    ; When a `stubborn' reference has been found, remove the var from VL 
    (@Edit)
    (@Down_To 4)
    ; to the body 
    ; Remove vars with `stubborn' references from VL, 
    ; temporarily updating the program 
    (set! //V/L (@RRV_Remove_Refs  //V/L))
    (@Right)
    ; to the expn or cond 
    (set! //V/L (@RRV_Remove_Refs  //V/L))
    (@Undo_Edit)
    ; Remaining variables are redundant 
    (cond
     ((not (null? //V/L))
      (@Pass))
     (#t
      (@Fail "All variables have stubborn references.")))))
  (set! //V/L //V/L-save)))

(define (@RRV_sub_Test //V/L-par)
 (let ((//V/L-save //V/L))
  (set! //V/L //V/L-par)
  (cond
   ((not (null? (@Set_Difference //V/L (@Used (@Get_n (@I) 2)))))
    (@Pass))
   ((not (null? (intersection-n //V/L (@Assd_To_Self (@Get_n (@I) 2)))))
    (@Pass))
   ((and (equal? (@Posn) (list 1)) (not (null? (@RRV_Top_Level_Redundant))))
    (@Pass))
   (#t
    ; There are references to all the local vars. See if some can be removed. 
    ; When a `stubborn' reference has been found, remove the var from VL 
    (@Edit)
    (@RRV_Rename)
    (set! //V/L (@Lvars (@Get_n (@I) 1)))
    (@Down_To 2)
    ; to the body 
    (@RRV_Maybe_Expand //V/L)
    ; Remove vars with `stubborn' references from VL, 
    ; temporarily updating the program 
    (let ((/orig_/V/L //V/L))
     (set! //V/L (@RRV_Remove_Refs  //V/L))
     (cond
      ((not (equal? //V/L /orig_/V/L))
       (@Undo_Edit)
       (@Edit)
       (@RRV_Rename)
       (@Down_To 2)
       (@RRV_Maybe_Expand //V/L)
       (set! //V/L (@RRV_Remove_Refs  //V/L)))))
    ; Remaining variables are redundant 
    (cond
     ((not (null? //V/L))
      (@Pass))
     (#t
      (@Left)
      ; to assigns 
      (let ((/assigns (@Cs (@I)))
            (/new-save /new))
       (set! /new '())
       (@Right)
       (set! /new (@RRV_Simple /assigns))
       (cond
        ((< (gen-length /new) (gen-length /assigns))
         (@Pass)))
       (set! /new /new-save))))
    (@Undo_Edit)
    (cond
     ((not (@Passed?))
      (@Fail "All variables have stubborn references.")))))
  (set! //V/L //V/L-save)))

; Look for a simple case where a local variable is copied to a global. 
; For each x:=e initialisation, try to match the body with: `S1; y:=x; S2' where: 
; y is not used/assigned in S1, and x is not assigned in S2 
; and no vars in e are assigned in S1 
; Convert to: y := e; S1[y/x]; S2[y/x] 
; Return the remaining assigns and update the body (which is current item). 
(define (@RRV_Simple /assigns)
 (let ((/new-save /new)
       (/x-save /x)
       (/e-save /e)
       (/value-save /value)
       (/defs '())
       (/assigned '())
       (/keep-save /keep)
       (funct-result '()))
  (set! /new '())
  (set! /x '())
  (set! /e '())
  (set! /value '())
  (set! /keep 1)
  (for-in /assign /assigns 
   (begin
    (set! /x (@Get_n /assign 1))
    (set! /keep 1)
    (cond
     ((= (@ST /x) //T_/Var_/Lvalue)
      (set! /e (@Get_n /assign 2))
      (let ((/__/O/K 1))
       (vector-set! /__/Match_array 2 /x)
       (set! /__/O/K (@New_Match  /%const__remove_redundant_vars__1 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__/S2_save //S2)
               (/__y_save /y)
               (/__/S1_save //S1))
          (set! //S2 (vector-ref /__/Match_array 3))
          (set! /y (vector-ref /__/Match_array 1))
          (set! //S1 (vector-ref /__/Match_array 0))
          (cond
           ((and (= (@ST /y) //T_/Var_/Lvalue) (not-member (@V /y) (my-reduce @Set_Union (my-map @Variables //S1))) (not-member (@V /x) (my-reduce @Set_Union (my-map @Assigned //S2))) (null? (intersection-n (@Used /e) (my-reduce @Set_Union (my-map @Assigned //S1)))))
            (set! /keep 0)
            (@Paste_Over (@Make //T_/Statements '() (concat (cons (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /y) (@Var_To_Expn /e))))) //S1) //S2)))
            (@Down)
            (while (@Right?) 
             (begin
              (@Right)
              (@Rename (@V /x) (@V /y))))))
          (set! //S2 /__/S2_save)
          (set! /y /__y_save)
          (set! //S1 /__/S1_save)))))
      (let ((/__/O/K 1))
       (vector-set! /__/Match_array 1 /x)
       (set! /__/O/K (@New_Match  /%const__remove_redundant_vars__2 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__/S2_save //S2)
               (/__val_save /val)
               (/__/S1_save //S1))
          (set! //S2 (vector-ref /__/Match_array 3))
          (set! /val (vector-ref /__/Match_array 2))
          (set! //S1 (vector-ref /__/Match_array 0))
          (set! /assigned (my-reduce @Set_Union (my-map @Assigned //S2)))
          (cond
           ((and (not-member (@V /x) (my-reduce @Set_Union (my-map @Variables //S1))) (not-member (@V /x) /assigned) (null? (intersection-n (@Variables /val) /assigned)))
            ; Work around a bug: 
            (set! /value /val)
            (@Paste_Over (@Make 17 '() (append //S1 //S2)))
            (@Foreach_Expn /foreach-remove_redundant_vars-1 0 (@AS_Type) 0)
            (cond
             ((null? (@Program))
              (@New_Program (@Skips))))
            (set! /keep 0)))
          (set! //S2 /__/S2_save)
          (set! /val /__val_save)
          (set! //S1 /__/S1_save)))))
      (let ((/__/O/K 1))
       (vector-set! /__/Match_array 1 /x)
       (set! /__/O/K (@New_Match  /%const__remove_redundant_vars__3 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__defs_save /defs)
               (/__/S2_save //S2)
               (/__val_save /val)
               (/__/S1_save //S1))
          (set! /defs (vector-ref /__/Match_array 4))
          (set! //S2 (vector-ref /__/Match_array 3))
          (set! /val (vector-ref /__/Match_array 2))
          (set! //S1 (vector-ref /__/Match_array 0))
          (set! /assigned (union-n (my-reduce @Set_Union (my-map @Assigned //S2)) (my-reduce @Set_Union (my-map @Assigned /defs))))
          (cond
           ((and (not-member (@V /x) (my-reduce @Set_Union (my-map @Variables //S1))) (not-member (@V /x) /assigned) (null? (intersection-n (@Variables /val) /assigned)))
            ; Work around a bug: 
            (set! /value /val)
            (@Paste_Over (@Make 17 '() (list (@Make 140 '() (list (@Make 17 '() (append //S1 //S2)) (@Make 14 '() /defs))))))
            (@Foreach_Expn /foreach-remove_redundant_vars-2 0 (@AS_Type) 0)
            (cond
             ((null? (@Program))
              (@New_Program (@Skips))))
            (set! /keep 0)))
          (set! /defs /__defs_save)
          (set! //S2 /__/S2_save)
          (set! /val /__val_save)
          (set! //S1 /__/S1_save)))))))
    (cond
     ((= /keep 1)
      (set! /new (cons /assign /new))))))
  (set! funct-result (reverse /new))
  (set! /new /new-save)
  (set! /x /x-save)
  (set! /e /e-save)
  (set! /value /value-save)
  (set! /keep /keep-save)
  funct-result))

; Update VL by deleting any variables with stubborn references. 
; Ignore assertions, and temporarily remove x from VL when in an assign to x. 
; Only called when VL is not empty and some vars in VL are used 
; NOTE: we have to actually MOVE to each variable in order to test 
; TR_Replace_With_Value. 
; Replace the references by their (found) values 
(define (@RRV_Remove_Refs //V/L-par)
 (let ((//V/L-save //V/L)
       (funct-result '()))
  (set! //V/L //V/L-par)
  (let ((//S/T (@ST (@I))))
   (cond
    ((= //S/T //T_/Struct)
     (let ((/name (car (@Struct_Elts (@I)))))
      (cond
       ((member /name //V/L)
        (set! //V/L (@Set_Difference //V/L (list /name))))))))
   (cond
    ((= //S/T //T_/Variable)
     (cond
      ((member (@V (@I)) //V/L)
       (let ((/val-save /val))
        (set! /val (@Find_Value (list (@V (@I)))))
        (cond
         ((null? /val)
          (set! //V/L (@Set_Difference //V/L (list (@V (@I))))))
         ((and (= (@ST (@Parent)) //T_/Aref) (= (@Posn_n) 1) (not (= (@ST /val) //T_/Variable)) (not (= (@ST /val) //T_/Struct)))
          ; Can't replace first component of an aref by 
          ; a complex expression: 
          (set! //V/L (@Set_Difference //V/L (list (@V (@I))))))
         (#t
          (@Paste_Over /val)))
        (set! /val /val-save)))))
    ((= //S/T //T_/Assert)
     #t)
    ((= //S/T //T_/Pop)
     (set! //V/L (@Set_Difference //V/L (@Variables (@I)))))
    ((= //S/T //T_/Push)
     (set! //V/L (@Set_Difference //V/L (@Variables (@Get_n (@I) 1))))
     (cond
      ((not (null? //V/L))
       (@Down_To 2)
       ; to the pushed expression 
       (set! //V/L (@RRV_Remove_Refs  //V/L))
       (@Up))))
    ((and (= //S/T //T_/Assign) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue))
     (let ((/x-save /x))
      (set! /x (@V (@Get_n (@I) 1)))
      (@Down_To 2)
      ; to the expression 
      (cond
       ((member /x //V/L)
        ; It may not matter if we can't remove these refs 
        ; but we still need to try: 
        (set! //V/L (@RRV_Remove_Refs  //V/L))
        (set! //V/L (union-n (list /x) //V/L)))
       (#t
        (set! //V/L (@RRV_Remove_Refs  //V/L))))
      (@Up)
      (set! /x /x-save)))
    ((and (= //S/T //T_/For) (member (@V (@Get_n (@I) 1)) //V/L))
     (set! //V/L (@Set_Difference //V/L (list (@V (@Get_n (@I) 1)))))
     (@Down_To 5)
     ; to body 
     (cond
      ((not (null? (intersection-n //V/L (@Used (@I)))))
       (set! //V/L (@RRV_Remove_Refs  //V/L))))
     (@Up)
     ; Restore the variable 
     (set! //V/L (union-n (list (@V (@Get_n (@I) 1))) //V/L)))
    ((= //S/T //T_/Var)
     ; NB: in VAR <x:=x>: ... ENDVAR the reference to x needs to be removed 
     ; since the `x' on the left is the LOCAL x, not the GLOBAL one! 
     (@Down)
     (@Down)
     ; to first assign 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (@Down_To 2)
       ; to expression 
       (cond
        ((not (null? (intersection-n //V/L (@Used (@I)))))
         (set! //V/L (@RRV_Remove_Refs  //V/L))
         (cond
          ((null? //V/L)
           (@Up)
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0))))
        (#t
         (set! /fl_flag1 0)))
       (cond
        ((= /fl_flag1 0)
         (@Up)
         ; back to assign 
         (cond
          ((not (@Right?))
           (set! /fl_flag1 1))
          (#t
           (@Right)
           (set! /fl_flag1 0)))))))
     (@Up)
     (@Up)
     ; back to var 
     ; Now the shadowed globals must be temporarily removed from VL 
     (let ((/shadowed (intersection-n //V/L (@Lvars (@Get_n (@I) 1)))))
      (@Down_To 2)
      ; to body 
      (cond
       ((null? /shadowed)
        (cond
         ((not (null? (intersection-n //V/L (@Used (@I)))))
          (set! //V/L (@RRV_Remove_Refs  //V/L)))))
       (#t
        (set! //V/L (@Set_Difference //V/L /shadowed))
        (cond
         ((not (null? (intersection-n //V/L (@Used (@I)))))
          (set! //V/L (@RRV_Remove_Refs  //V/L))))
        (set! //V/L (union-n //V/L /shadowed))))
      (@Up)))
    ((or (= //S/T //T_/Proc) (= //S/T //T_/Funct) (= //S/T //T_/B/Funct))
     ; The formal parameters are local to the proc body 
     (let ((/shadowed (intersection-n //V/L (union-n (@Assigned (@Get_n (@I) 2)) (@Assigned (@Get_n (@I) 3))))))
      (set! //V/L (@Set_Difference //V/L /shadowed))
      (@Down_Last)
      ; to body 
      (cond
       ((not (null? (intersection-n //V/L (@Used (@I)))))
        (set! //V/L (@RRV_Remove_Refs  //V/L))))
      (@Up)
      ; Restore the shadowed variables: 
      (set! //V/L (union-n //V/L /shadowed))))
    ((or (= //S/T //T_/A_/Proc_/Call) (= //S/T //T_/M/W_/Proc_/Call))
     ; Any VAR parameters cannot be removed, 
     ; BUT the VAR parameter may be an array ref containing references 
     ; which *can* be removed! 
     (set! //V/L (@Set_Difference //V/L (@Assigned (@I))))
     (cond
      ((not (null? //V/L))
       (@Down_To 2)
       ; to value pars 
       (set! //V/L (@RRV_Remove_Refs  //V/L))
       (@Right)
       ; to var pars 
       (set! //V/L (@RRV_Remove_Refs  //V/L))
       (@Up))))
    ((not (@Cs? (@I)))
     #t
     ; Base cases have already been dealt with 
    )
    (#t
     (@Down)
     ; Call @RRV_Remove_Refs recursively on each component. 
     ; Stop if VL becomes empty 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (cond
        ((not (null? (intersection-n //V/L (@Used (@I)))))
         (set! //V/L (@RRV_Remove_Refs  //V/L))
         (cond
          ((null? //V/L)
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0))))
        (#t
         (set! /fl_flag1 0)))
       (cond
        ((= /fl_flag1 0)
         (cond
          ((not (@Right?))
           (set! /fl_flag1 1))
          (#t
           (@Right)
           (set! /fl_flag1 0)))))))
     (@Up))))
  (set! funct-result //V/L)
  (set! //V/L //V/L-save)
  funct-result))

(define (@Remove_Redundant_Vars_Code //Data)
 (cond
  ((and (null? (@Posn)) (= (@ST (@I)) //T_/Statements))
   (@Down)))
 (cond
  ((and (>= (gen-length (@Posn)) 2) (= (@ST (@GParent)) //T_/Where) (= (@Size (@Parent)) 1))
   (let ((/defns (@Get_n (@I) 1)))
    (@Splice_Over (@Cs (@Get_n (@I) 2)))
    (@Up)
    (@Up)
    ; to WHERE clause 
    (@Paste_Over (@Make //T_/Var '() (list /defns (@Make //T_/Statements '() (list (@I)))))))))
 (cond
  ((= (@ST (@I)) //T_/Var)
   (let ((/n (@Size (@Get_n (@I) 1)))
         (/redundant-save /redundant))
    (set! /redundant (@RRV_Top_Level_Redundant))
    (cond
     ((not (null? /redundant))
      (@RRV_Top_Level_Remove /redundant)
      ; See if other variables can be removed: 
      (cond
       ((and (< (gen-length /redundant) /n) (@Trans? //T/R_/Remove_/Redundant_/Vars))
        (@Trans //T/R_/Remove_/Redundant_/Vars ""))))
     (#t
      (@RRV_Code_Var)))
    (set! /redundant /redundant-save)))
  (#t
   (@RRV_Code_Funct))))

(define (@RRV_Code_Var)
 ; If the local var is of the form v := f(v) then rename it within the body 
 ; (so that we don't replace a reference to v by v itself) 
 (@RRV_Rename)
 (@Edit)
 ; Find the list of vars with simple references and then remove these references 
 ; The redundant variables are the Assd_To_Self plus unreferenced variables 
 ; in the resulting body. 
 (let ((/vars (@Lvars (@Get_n (@I) 1)))
       (/orig_/V/L '()))
  (@Down_To 2)
  ; to the body 
  ; Calculate which vars have references and are not Assd_To_Self: 
  (let ((//V/L-save //V/L)
        (/redundant-save /redundant))
   (set! //V/L (@Set_Difference (intersection-n /vars (@Used (@I))) (@Assd_To_Self (@I))))
   (set! /redundant '())
   ; Remove vars with stubborn references from VL: 
   (cond
    ((not (null? //V/L))
     (set! /orig_/V/L //V/L)
     (@RRV_Maybe_Expand //V/L)
     (set! //V/L (@RRV_Remove_Refs  //V/L))
     (cond
      ((equal? //V/L /orig_/V/L)
       ; All the vars were redundant and refs were removed 
      )
      (#t
       ; Re-do with the smaller VL 
       (@Undo_Edit)
       (@Edit)
       (@Down_To 2)
       (@RRV_Maybe_Expand //V/L)
       (set! //V/L (@RRV_Remove_Refs  //V/L))))))
   ; The redundant variables are VL plus those not used plus the Assd_To_Self: 
   (set! /redundant (union-n //V/L (@Set_Difference /vars (@Used (@I))) (intersection-n /vars (@Assd_To_Self (@I)))))
   (display-list "Redundant variables are: " (my-map @N_String /redundant))
   ; Simple references have been removed 
   ; Calculate the new assigns list (may be empty) 
   (@Goto (list 1))
   ; to the Assigns 
   (let ((/assigns (@RRV_Filter_Assigns (@Cs (@I)) /redundant))
         (/new-save /new))
    (set! /new '())
    ; Replace the VAR structure with its body, and update the body to remove 
    ; references to redundant variables (these will be in Assertions 
    ; or self-assignments) 
    ; We have to do this carefully to avoid problems where a nested VAR structure 
    ; shadows a redundant variable. 
    (@Right)
    ; to the body 
    (@New_Program (@I))
    (@RRV_Delete_Refs /redundant)
    (cond
     ((not (null? /assigns))
      (set! /new (@RRV_Simple /assigns))
      (display-list "Redundant variables are: " (my-map @N_String (@Set_Difference (my-reduce @Set_Union (my-map @Assigned /assigns)) (my-reduce @Set_Union (my-map @Assigned /new)))))
      (set! /assigns /new)))
    ; Create a new VAR structure around the body if there are any assigns left 
    (cond
     ((not (null? /assigns))
      (@New_Program (@Make //T_/Var '() (list (@Make //T_/Assigns '() /assigns) (@Program))))))
    (set! /new /new-save))
   (set! //V/L //V/L-save)
   (set! /redundant /redundant-save)))
 (@End_Edit))

(define (@RRV_Rename)
 (let ((/renames '())
       (/new-save /new))
  (set! /new (hash-table))
  (@Down)
  (@Down)
  ; to first assign 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (cond
     ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@I) 1)) (@Used (@Get_n (@I) 2))))
      (puthash /new (@V (@Get_n (@I) 1)) (@Make_Name (string-append "__tmp__" (@N_String (@V (@Get_n (@I) 1))))))
      (@Down)
      (@Paste_Over (@Make //T_/Var_/Lvalue (gethash /new (@V (@I))) '()))
      (@Up)))
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@Up)
  (@Right)
  ; to body 
  (@Foreach_Global_Var /foreach-remove_redundant_vars-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Up)
  ; back to start 
  
  (set! /new /new-save)))

(define (@RRV_Code_Funct)
 (let ((/vars (@Lvars (@Get_n (@I) 3)))
       (/orig_/V/L '()))
  (@Edit)
  ; Calculate which vars have references and are not Assd_To_Self: 
  (let ((//V/L-save //V/L)
        (/redundant-save /redundant)
        (/result '()))
   (set! //V/L (@Set_Difference (intersection-n /vars (union-n (@Used (@Get_n (@I) 4)) (@Used (@Get_n (@I) 5)))) (@Set_Difference (@Assd_To_Self (@Get_n (@I) 4)) (@Used (@Get_n (@I) 5)))))
   (set! /redundant '())
   (@Down_To 4)
   ; to the body 
   ; Remove vars with stubborn references from VL: 
   (cond
    ((not (null? //V/L))
     (set! /orig_/V/L //V/L)
     (set! //V/L (@RRV_Remove_Refs  //V/L))
     (@Goto (list 5))
     (set! //V/L (@RRV_Remove_Refs  //V/L))
     (cond
      ((equal? //V/L /orig_/V/L)
       ; All the vars were redundant and refs were removed 
      )
      (#t
       ; Re-do with the smaller VL 
       (@Undo_Edit)
       (@Edit)
       (@Down_To 4)
       (set! //V/L (@RRV_Remove_Refs  //V/L))
       (@Goto (list 5))
       (set! //V/L (@RRV_Remove_Refs  //V/L))))))
   (@Goto (list 4))
   ; The redundant variables are VL plus those not used plus the Assd_To_Self: 
   (set! /redundant (union-n //V/L (@Set_Difference /vars (@Used (@I))) (intersection-n /vars (@Assd_To_Self (@I)))))
   ; Any vars still in the result cond/expn are not redundant: 
   (@Right)
   (set! /redundant (@Set_Difference /redundant (@Used (@I))))
   (display-list "Redundant variables are: " (my-map @N_String /redundant))
   ; Simple references have been removed 
   ; Calculate the new assigns list (may be empty) 
   (@Goto (list 3))
   ; to the Assigns 
   (let ((/assigns (@RRV_Filter_Assigns (@Cs (@I)) /redundant)))
    ; Replace the VAR structure with its body, and update the body to remove 
    ; references to redundant variables (these will be in Assertions 
    ; or self-assignments) 
    ; We have to do this carefully to avoid problems where a nested VAR structure 
    ; shadows a redundant variable. 
    (@Right)
    ; to the body 
    (@Edit)
    (@RRV_Delete_Refs /redundant)
    (@End_Edit)
    (@Left)
    (@Paste_Over (@Make //T_/Assigns '() /assigns)))
   (set! //V/L //V/L-save)
   (set! /redundant /redundant-save))
  (@End_Edit)))

; Return a new list of assigns, with assigns to redundant variables filtered out: 
(define (@RRV_Filter_Assigns /assigns /redundant-par)
 (let ((/redundant-save /redundant)
       (/new-save /new)
       (funct-result '()))
  (set! /redundant /redundant-par)
  (set! /new '())
  (while (not (null? /assigns)) 
   (begin
    (cond
     ((not-member (@V (@Get_n (car /assigns) 1)) /redundant)
      (set! /new (cons (car /assigns) /new))))
    (set! /assigns (cdr /assigns))))
  (set! funct-result (reverse /new))
  (set! /redundant /redundant-save)
  (set! /new /new-save)
  funct-result))

; Remove references to the redundant variables (should not touch a local variable 
; in a nested VAR which shadows a redundant variable). 
(define (@RRV_Delete_Refs /redundant-par)
 (let ((/redundant-save /redundant))
  (set! /redundant /redundant-par)
  ; First, rename the redundant GLOBAL variables to a dummy name, 
  ; then delete all assertions and assigns which use the dummy name. 
  (let ((/dummy-save /dummy))
   (set! /dummy (@Make //T_/Variable (@Make_Name "__DUMMY__") '()))
   (@Foreach_Global_Var /foreach-remove_redundant_vars-4 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /dummy /dummy-save))
  (let ((/dummy-save /dummy))
   (set! /dummy (@Make_Name "__DUMMY__"))
   (@Foreach_Statement /foreach-remove_redundant_vars-5 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /dummy /dummy-save))
  (@Trans //T/R_/Simplify "")
  (set! /redundant /redundant-save)))

; Look for a simple case, which applies to a top level VAR clause: 
; the local variable is assigned a constant value 
; and the only assignments to it in the body are that value 
; Return the list of redundant vars due to this 
(define (@RRV_Top_Level_Redundant)
 (let ((/redundant-save /redundant)
       (/v-save /v)
       (/e-save /e)
       (/keep-save /keep)
       (funct-result '()))
  (set! /redundant '())
  (set! /v '())
  (set! /e '())
  (set! /keep 1)
  #t
  (for-in /assign (@Cs (@Get_n (@I) 1)) 
   (begin
    (set! /v (@Get_n /assign 1))
    (set! /e (@Get_n /assign 2))
    (set! /keep 1)
    (cond
     ((and (= (@ST /v) //T_/Var_/Lvalue) (or (= (@ST /e) //T_/Number) (= (@ST /e) //T_/String)))
      (set! /keep 0)
      (@Down_To 2)
      (@Foreach_Statement /foreach-remove_redundant_vars-6 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      ; Check that replacing the variable by a value will not give bad syntax 
      (@Ateach_Expn /foreach-remove_redundant_vars-7 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (@Up)))
    (cond
     ((= /keep 0)
      (set! /redundant (cons (@V /v) /redundant))))))
  (set! funct-result /redundant)
  (set! /redundant /redundant-save)
  (set! /v /v-save)
  (set! /e /e-save)
  (set! /keep /keep-save)
  funct-result))

(define (@RRV_Top_Level_Remove /redundant-par)
 (let ((/redundant-save /redundant))
  (set! /redundant /redundant-par)
  (let ((/v-save /v)
        (/e-save /e)
        (/new-save /new)
        (/dummy-save /dummy))
   (set! /v '())
   (set! /e '())
   (set! /new '())
   (set! /dummy (@Make //T_/Variable (@Make_Name "__DUMMY__") '()))
   (display-list "Top_Level_Remove: " (my-map @N_String /redundant))
   (for-in /assign (@Cs (@Get_n (@I) 1)) 
    (begin
     (set! /v (@Get_n /assign 1))
     (set! /e (@Get_n /assign 2))
     (cond
      ((and (= (@ST /v) //T_/Var_/Lvalue) (member (@V /v) /redundant))
       (@Down_To 2)
       ; It is OK to replace a global Lvalue with a variable 
       ; -- the loop will fix it for us. 
       (@Foreach_Global_Var /foreach-remove_redundant_vars-8 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))
       (@Foreach_Expn /foreach-remove_redundant_vars-9 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))
       (@Foreach_Statement /foreach-remove_redundant_vars-10 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))
       (@Trans //T/R_/Simplify "")
       (@Up))
      (#t
       (set! /new (cons /assign /new))))))
   (cond
    ((null? /new)
     (@Splice_Over (@Cs (@Get_n (@I) 2))))
    (#t
     (@Down)
     ; to assigns 
     (@Paste_Over (@Make //T_/Assigns '() (reverse /new)))
     (@Up)))
   (set! /v /v-save)
   (set! /e /e-save)
   (set! /new /new-save)
   (set! /dummy /dummy-save))
  (set! /redundant /redundant-save)))

(define (@RRV_Maybe_Expand //V/L-par)
 (let ((//V/L-save //V/L))
  (set! //V/L //V/L-par)
  (let ((/found-save /found))
   (set! /found 0)
   (set! /found 0)
   (@Foreach_Stats /foreach-remove_redundant_vars-11 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (while (not (= /found 0)) 
    (begin
     (set! /found 0)
     (@Foreach_Stats /foreach-remove_redundant_vars-12 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))))
   (set! /found /found-save))
  (set! //V/L //V/L-save)))

; List the variables used in local procedures: 
(define (@RRV_Proc_Vars)
 (let ((//R '())
       (/posn (@Posn)))
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
   (@Up))
  (cond
   ((= (@ST (@I)) //T_/Where)
    (set! //R (@Used (@Get_n (@I) 2)))))
  (@Goto /posn)
  //R))

#t
