;;; Scheme translation of WSL code
(define (/foreach-move_to_right-1 //Depth //A/S_/Type)
 (cond
  ((@LR_Equal? (@I) (@Get_n (@Get_n //S1 1) 1))
   (@Paste_Over (@Get_n (@Get_n //S1 1) 2)))))

(define (/foreach-move_to_right-2 //Depth //A/S_/Type)
 (for-in /assign (@Cs //S1) 
  (cond
   ((@LR_Equal? (@Get_n /assign 1) (@I))
    (@Paste_Over (@Get_n /assign 2))))))

(define (/foreach-move_to_right-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Variable)
   (set! /v (@V (@I)))
   (for-in /assign /inverts 
    (cond
     ((equal? (car /assign) /v)
      (@Paste_Over (wsl-ref /assign 2))))))))

(define (/foreach-move_to_right-4 //Depth //A/S_/Type)
 (cond
  ((@LR_Equal? /x (@I))
   (@Paste_Over /f))))

(define (/foreach-move_to_right-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Struct) (@Equal? (@Get_n (@I) 2) /e))
   (set! /bad 1))
  ((and (or (= (@ST (@I)) //T_/Aref) (= (@ST (@I)) //T_/Sub_/Seg) (= (@ST (@I)) //T_/Rel_/Seg)) (@Equal? (@Get_n (@I) 1) /e))
   (set! /bad 1))))

(define (/foreach-move_to_right-6 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Variable)
   (set! /notstacks (union-n /notstacks (list (@V (@I))))))))

(define (/foreach-move_to_right-7 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Var_/Lvalue)
   (cond
    ((and (= (@ST (@Parent)) //T_/Push) (= (@Posn_n) 1))
     (set! /stacks (union-n /stacks (list (@V (@I))))))
    ((and (= (@ST (@Parent)) //T_/Pop) (= (@Posn_n) 2))
     (set! /stacks (union-n /stacks (list (@V (@I))))))
    (#t
     (set! /notstacks (union-n /notstacks (list (@V (@I))))))))))

(define (/foreach-move_to_right-8 //Depth //A/S_/Type)
 (set! /types (my-map @ST (@Cs (@I))))
 (cond
  ((or (member //T_/Push /types) (member //T_/Pop /types))
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (= (@ST (@I)) //T_/Push) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@I) 1)) /stacks))
       (puthash /total (@V (@Get_n (@I) 1)) (+ (gethash /total (@V (@Get_n (@I) 1))) 1)))
      ((and (= (@ST (@I)) //T_/Pop) (= (@ST (@Get_n (@I) 2)) //T_/Var_/Lvalue) (member (@V (@Get_n (@I) 2)) /stacks))
       (cond
        ((= (gethash /total (@V (@Get_n (@I) 2))) 0)
         (set! /notstacks (union-n /notstacks (list (@V (@Get_n (@I) 2))))))
        (#t
         (puthash /total (@V (@Get_n (@I) 2)) (- (gethash /total (@V (@Get_n (@I) 2))) 1))))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))
   (for-in /stack /stacks 
    (cond
     ((not (= (gethash /total /stack) 0))
      (set! /notstacks (union-n /notstacks (list /stack)))))))))

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
(define (@Move_To_Right_Test)
 (let ((//Bad_/Types (@Make_Set (list //T_/M/W_/Proc_/Call //T_/X_/Proc_/Call //T_/Proc_/Call //T_/Call)))
       (//S1-save //S1)
       (//S2 '()))
  (set! //S1 '())
  (cond
   ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
    (@Up)))
  (cond
   ((not (@Right?))
    (@Fail "There is no statement to the right of this one."))
   ((or (= (@GT (@I)) //T_/Definition) (= (@GT (@I)) //T_/Assign) (= (@GT (@I)) //T_/Action) (= (@GT (@I)) //T_/Guarded))
    (@Pass))
   ((and (or (= (@GT (@I)) //T_/Expression) (= (@GT (@I)) //T_/Condition)) (member (@ST (@Parent)) //Comm_/Ops))
    ;An expression which forms part of a commutative operation.
    (@Pass))
   ((= (@GT (@I)) //T_/Statement)
    (set! //S1 (@I))
    (@Right)
    (set! //S2 (@I))
    (cond
     ((or (= (@ST //S1) //T_/Skip) (= (@ST //S1) //T_/Comment))
      (@Pass))
     ((and (= (@ST //S1) //T_/Push) (= (@ST //S2) //T_/Proc_/Call))
      (@MR_Push_Call_Test))
     ((not (null? (intersection-n (@Stat_Types //S1) //Bad_/Types)))
      (@Fail "Cannot move statements which have unknown side effects"))
     ((and (or (= (@ST //S2) //T_/Proc_/Call) (= (@ST //S2) //T_/M/W_/Proc_/Call)) (= (@ST //S1) //T_/Assignment) (null? (intersection-n (@Assigned //S1) (@Used //S1))) (= (@Size //S1) 1) (@Set_Subset? (@Assigned //S1) (@Used //S2)) (null? (intersection-n (@Assigned //S1) (@Assigned //S2))))
      (@Pass))
     ((= (@ST //S2) //T_/Proc_/Call)
      (let ((/posn (@Posn))
            (/used (@Used //S1))
            (/assigned (@Assigned //S1)))
       (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
        (@Up))
       (cond
        ((= (@ST (@I)) //T_/Where)
         (set! /used (@Used (@I)))
         (set! /assigned (@Assigned (@I)))))
       (@Goto /posn)
       (set! /used (union-n /used (@Used //S2)))
       (set! /assigned (union-n /assigned (@Assigned //S2)))
       (cond
        ((and (null? (intersection-n (@Used //S1) /assigned)) (null? (intersection-n /used (@Assigned //S1))))
         (@Pass))
        (#t
         (@Fail "Procedure might use variables in the statement")))))
     ((or (= (@ST //S2) //T_/Proc_/Call) (= (@ST //S2) //T_/M/W_/Proc_/Call))
      (@Fail "Cannot move past statements with unknown side effects"))
     ((not (null? (intersection-n (@Stat_Types //S2) //Bad_/Types)))
      (@Fail "Cannot move past statements which have unknown side effects"))
     ((not (equal? (@Gen_TVs //S1 (@AS_Type)) (list 0)))
      (@Fail "The statement may not go on to execute the following statement."))
     ((not (equal? (@Gen_TVs //S2 (@AS_Type)) (list 0)))
      (@Fail "The second statement may cause a change in control flow."))
     ((and (not (@Elt_Clash_List? (@Elements //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Assigned //S1) (@Elements //S2))))
      ; No statement clobbers the other's variables, 
      ; so a simple swap is OK: 
      (@Pass))
     ((and (= (@ST //S1) //T_/Assignment) (not (@Elt_Clash_List? (@Elts_Assigned //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Used //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Used //S2) (@Elts_Assigned //S1))))
      (cond
       ((@MR_Bad_Subexp? (@Lvalue_To_Expn (@Get_n (@Get_n //S1 1) 1)) //S2)
        (@Fail "Assigned value has component(s) accessed"))
       (#t
        (@Pass))))
     ((and (@Elt_Clash_List? (@Elts_Used //S1) (@Elts_Assigned //S1)) (@Elt_Clash_List? (@Elts_Used //S2) (@Elts_Assigned //S2)) (@Elt_Clash_List? (@Elts_Used //S1) (@Elts_Assigned //S2)) (@Elt_Clash_List? (@Elts_Used //S2) (@Elts_Assigned //S1)))
      (@Fail "Each statment uses variables assigned in the other"))
     ((and (= (@ST //S1) //T_/Assignment) (@Simple_Var_Assigns? //S1) (not (@Elt_Clash_List? (@Elts_Assigned //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Used //S1) (@Elts_Assigned //S2))))
      (cond
       ((@MR_Bad_Subexp? (@Lvalue_To_Expn (@Get_n (@Get_n //S1 1) 1)) //S2)
        (@Fail "Assigned value has component(s) accessed"))
       (#t
        (@Pass))))
     ((and (= (@ST //S2) //T_/Assignment) (@Simple_Var_Assigns? //S2) (not (@Elt_Clash_List? (@Elts_Assigned //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Used //S2) (@Elts_Assigned //S1))) (@Invertible? (@Used //S1) (@Cs //S2)))
      (cond
       ((@MR_Bad_Subexp? (@Lvalue_To_Expn (@Get_n (@Get_n //S2 1) 1)) //S1)
        (@Fail "Assigned value has component(s) accessed"))
       (#t
        (@Pass))))
     ((and (= (@ST //S1) //T_/Assignment) (= (@Size //S1) 1) (= (@ST //S2) //T_/Assignment) (= (@Size //S2) 1) (= (@ST (@Get_n (@Get_n //S1 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n //S2 1) 1)) //T_/Var_/Lvalue) (not (equal? (@V (@Get_n (@Get_n //S1 1) 1)) (@V (@Get_n (@Get_n //S2 1) 1)))) (not (null? (@Invert (@Lvalue_To_Expn (@Get_n (@Get_n //S2 1) 1)) (@V (@Get_n (@Get_n //S1 1) 1)) (@Get_n (@Get_n //S2 1) 2)))))
      (cond
       ((@MR_Bad_Subexp? (@Lvalue_To_Expn (@Get_n (@Get_n //S1 1) 1)) //S2)
        (@Fail "Assigned value has component(s) accessed"))
       (#t
        (@Pass))))
     (#t
      (@Fail "Cannot swap two statement that change each other's variables."))))
   (#t
    (@Fail "It is not possible to move an item of the selected type.")))
  (set! //S1 //S1-save)))

(define (@Move_To_Right_Code //Data)
 (let ((//S1-save //S1)
       (//S2 '())
       (//B1 '())
       (//B2 '())
       (//B3 '()))
  (set! //S1 '())
  (cond
   ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
    (@Up)))
  (cond
   ((and (= (@GT (@I)) //T_/Guarded) (= (@ST (@Parent)) //T_/Cond))
    (set! //B1 (@Get_n (@I) 1))
    ; first condition (in the guarded to be moved) 
    (@Cut)
    (@Paste_After (@Buffer))
    (@Down)
    (set! //B2 (@I))
    ; original value of other condition 
    (set! //B3 (@And (@I) (@Not //B1)))
    ; New value for other condition 
    (@Paste_Over //B3)
    (@Up)
    (@Right)
    (cond
     ((or (= (@ST //B2) //T_/True) (@Implies? (@Not //B3) //B1))
      (@Down)
      (@Paste_Over (@Make //T_/True '() '()))
      (@Up))))
   ((or (= (@GT (@I)) //T_/Definition) (= (@GT (@I)) //T_/Assign) (= (@GT (@I)) //T_/Action) (= (@GT (@I)) //T_/Guarded) (= (@GT (@I)) //T_/Expression) (= (@GT (@I)) //T_/Condition))
    (@Cut)
    (@Paste_After (@Buffer))
    (@Right))
   ((= (@GT (@I)) //T_/Statement)
    (set! //S1 (@I))
    (@Cut)
    (set! //S2 (@I))
    (cond
     ((or (= (@ST //S1) //T_/Skip) (= (@ST //S1) //T_/Comment))
      (@Paste_After (@Buffer))
      (@Right))
     ((and (= (@ST //S1) //T_/Assignment) (or (= (@ST //S2) //T_/Proc_/Call) (= (@ST //S2) //T_/M/W_/Proc_/Call)))
      (@Down_To 2)
      ; to expressions 
      (@Foreach_Expn /foreach-move_to_right-1 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (@Up)
      (@Paste_After (@Buffer))
      (@Right))
     ((and (not (@Elt_Clash_List? (@Elements //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Assigned //S1) (@Elements //S2))))
      ; No statement clobbers the other's variables, 
      ; so a simple swap is OK: 
      (@Paste_After (@Buffer))
      (@Right))
     ((and (= (@ST //S1) //T_/Assignment) (not (@Elt_Clash_List? (@Elts_Assigned //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Used //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Used //S2) (@Elts_Assigned //S1))))
      (@Move_Assignment_Right //S1 //S2))
     ((and (= (@ST //S1) //T_/Assignment) (@Simple_Var_Assigns? //S1) (not (@Elt_Clash_List? (@Elts_Assigned //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Used //S1) (@Elts_Assigned //S2))))
      (@Move_Assignment_Right //S1 //S2))
     ((and (= (@ST //S2) //T_/Assignment) (@Simple_Var_Assigns? //S2) (not (@Elt_Clash_List? (@Elts_Assigned //S1) (@Elts_Assigned //S2))) (not (@Elt_Clash_List? (@Elts_Used //S2) (@Elts_Assigned //S1))) (@Invertible? (@Used //S1) (@Cs //S2)))
      (@Move_Assignment_Left //S1 //S2))
     ((and (= (@ST //S1) //T_/Assignment) (= (@Size //S1) 1) (= (@ST //S2) //T_/Assignment) (= (@Size //S2) 1) (= (@ST (@Get_n (@Get_n //S1 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n //S2 1) 1)) //T_/Var_/Lvalue) (not (equal? (@V (@Get_n (@Get_n //S1 1) 1)) (@V (@Get_n (@Get_n //S2 1) 1)))) (not (null? (@Invert (@Lvalue_To_Expn (@Get_n (@Get_n //S2 1) 1)) (@V (@Get_n (@Get_n //S1 1) 1)) (@Get_n (@Get_n //S2 1) 2)))))
      (@Swap_Assigns //S1 //S2))
     (#t
      (error "@Move_To_Right_Code: should never get here!"))))
   (#t
    (error "@Move_To_Right_Code: should never get here!")))
  (set! //S1 //S1-save)))

; S1 is the (deleted) assignment, currently S2 is selected: 
(define (@Move_Assignment_Right //S1-par //S2)
 (let ((//S1-save //S1))
  (set! //S1 //S1-par)
  (let ((/assign-save /assign))
   (set! /assign '())
   ; Replace the assigned vars by the values they will be assigned: 
   (@Foreach_Global_Var /foreach-move_to_right-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /assign /assign-save))
  (@Paste_Over (@Simplify (@I) (@Budget)))
  (@Paste_After //S1)
  (@Right)
  (set! //S1 //S1-save)))

; S1 is the (deleted) statement, S2 is the assignment 
; and is currently selected: 
(define (@Move_Assignment_Left //S1-par //S2)
 (let ((//S1-save //S1))
  (set! //S1 //S1-par)
  (let ((/assign-save /assign)
        (/inverts-save /inverts)
        (/vars (@Used //S1))
        (/v-save /v))
   (set! /assign '())
   (set! /inverts '())
   (set! /v '())
   (@Paste_After //S1)
   (@Right)
   (for-in /assign (@Cs //S2) 
    (cond
     ((not (null? (intersection-n (@Assigned /assign) /vars)))
      (set! /inverts (cons (list (@V (@Get_n /assign 1)) (@Invert (@Lvalue_To_Expn (@Get_n /assign 1)) (@V (@Get_n /assign 1)) (@Get_n /assign 2))) /inverts)))))
   (@Foreach_Global_Var /foreach-move_to_right-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Paste_Over (@Simplify (@I) (@Budget)))
   (set! /assign /assign-save)
   (set! /inverts /inverts-save)
   (set! /v /v-save))
  (set! //S1 //S1-save)))

; Swap two single assignments which use each other's variables: 
; x := f(x, y); y := g(x)  -->  y := g(f(x, y)); x := g^{-1}_x(y) 
; S1 is the (deleted) assignment, currently S2 is selected: 
(define (@Swap_Assigns //S1-par //S2)
 (let ((//S1-save //S1))
  (set! //S1 //S1-par)
  (let ((/x-save /x)
        (/f-save /f)
        (/y (@Get_n (@Get_n //S2 1) 1))
        (/g (@Get_n (@Get_n //S2 1) 2))
        (/new '()))
   (set! /x (@Get_n (@Get_n //S1 1) 1))
   (set! /f (@Get_n (@Get_n //S1 1) 2))
   ; First apply the deleted assignment S1 to S2 (currently selected): 
   (@Foreach_Global_Var /foreach-move_to_right-4 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Paste_Over (@Simplify (@I) (@Budget)))
   ; Now construct the inverse of g to recover and assign the x value: 
   ; from the current y value: 
   (set! /new (list /x (@Invert (@Make //T_/Variable (@V /y) '()) (@V /x) /g)))
   (@Paste_After (@Make //T_/Assignment '() (list (@Make //T_/Assign '() /new))))
   (@Paste_Over (@Simplify (@I) (@Budget)))
   (@Right)
   (set! /x /x-save)
   (set! /f /f-save))
  (set! //S1 //S1-save)))

; Check if e appears in a struct, array ref, rel seg or sub seg in S 
(define (@MR_Bad_Subexp? /e-par //S)
 (let ((/e-save /e)
       (/bad-save /bad)
       (funct-result '()))
  (set! /e /e-par)
  (set! /bad 0)
  (@Edit)
  (@New_Program //S)
  (@Foreach_Expn /foreach-move_to_right-5 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Undo_Edit)
  (set! funct-result (= /bad 1))
  (set! /e /e-save)
  (set! /bad /bad-save)
  funct-result))

; Check that assignments are all to simple variables 
(define (@Simple_Var_Assigns? //S)
 (let ((//O/K 1))
  (for-in /assign (@Cs //S) 
   (cond
    ((not (= (@ST (@Get_n /assign 1)) //T_/Var_/Lvalue))
     (set! //O/K 0))))
  (= //O/K 1)))

; Special test case: moving a PUSH past a local procedure call 
; Find all variables used/assigned in the procedure (including called procedures) 
; Take out stacks which only appear as PUSH/POP pairs. 
(define (@MR_Push_Call_Test)
 (let ((//O/K 1)
       (/posn (@Posn))
       (/used '())
       (/assigned '())
       (/stacks-save /stacks)
       (/notstacks-save /notstacks)
       (/types-save /types)
       (/total-save /total))
  (set! /stacks '())
  (set! /notstacks '())
  (set! /types '())
  (set! /total (hash-table))
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
   (@Up))
  (cond
   ((not (= (@ST (@I)) //T_/Where))
    (@Fail "Next statement has unknown side-effects"))
   (#t
    (set! /used (@Used (@I)))
    (set! /assigned (@Assigned (@I)))
    ; Stacks only appear in PUSH/POP statements: never in an expression. 
    (set! /notstacks (@Set_Difference /assigned /used))
    (@Foreach_Expn /foreach-move_to_right-6 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Ateach_Lvalue /foreach-move_to_right-7 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! /stacks (@Set_Difference /stacks /notstacks))
    ; Check that stack pushes and pops are balanced: 
    (for-in /stack /stacks 
     (puthash /total /stack 0))
    (@Foreach_Stats /foreach-move_to_right-8 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! /stacks (@Set_Difference /stacks /notstacks))
    (@Goto /posn)
    (set! /used (@Set_Difference /used /stacks))
    (set! /assigned (@Set_Difference /assigned /stacks))
    (@Left)
    ; to the PUSH 
    (cond
     (#f
      (display-list "Balanced stacks = " (my-map @N_String /stacks))
      (display-list "Call used     = " (my-map @N_String /used))
      (display-list "all assigned = " (my-map @N_String /assigned))
      (display-list "PUSH used     = " (my-map @N_String (@Used (@I))))
      (display-list "PUSH assigned = " (my-map @N_String (@Assigned (@I))))
      (@PP_Item (@I) 80 "")))
    ; Now we know what variables are potentially modified in the call 
    (cond
     ((and (null? (intersection-n (@Used (@I)) /assigned)) (null? (intersection-n (@Assigned (@I)) /used)))
      (@Pass))
     (#t
      (@Fail "The procedure call could affect the PUSH")))))
  (set! /stacks /stacks-save)
  (set! /notstacks /notstacks-save)
  (set! /types /types-save)
  (set! /total /total-save)))

;-----------------------------------------------------------------------

