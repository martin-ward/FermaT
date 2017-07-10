;;; Scheme translation of WSL code
(define (/foreach-simplify_item-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (or (> (@Size (@I)) 1) (not (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue)) (not (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) /v))))
   (@Fail "Bad statement in the FUNCT definition"))))

(define (/foreach-simplify_item-2 //Depth //A/S_/Type)
 (cond
  ((> (@Size (@I)) 1)
   (@Fail "Bad statements in the FUNCT definition"))))

(define (/foreach-simplify_item-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (cond
    ((equal? (@Value (@I)) (@Make_Name "Z"))
     (@Paste_Over (@Make //T_/Exit (+ //Depth 2) '())))
    (#t
     (@Paste_Over (@Make //T_/Exit (+ //Depth 1) '())))))))

(define (/foreach-simplify_item-4 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) (@ST /v))
   (@Paste_Over /expn))))

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
(define (@Simplify_Item_Test)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   ;An `If' statement can be simplified if any of the conditions
   ;(except the last one) are either TRUE or FALSE.
   (@Down)
   (while (and (@Right?) (not (member (@ST (@Get_n (@I) 1)) (list //T_/True //T_/False)))) 
    (@Right))
   (cond
    ((not (@Right?))
     (@Fail "The `If' statement should have a TRUE or FALSE condition"))
    ((member (@ST (@Get_n (@I) 1)) (list //T_/True //T_/False))
     (@Pass))))
  ((= (@ST (@I)) //T_/D_/If)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((= (@ST (@Get_n (@I) 1)) //T_/False)
      (@Pass)
      (set! /fl_flag1 1))
     ((@Right?)
      (@Right)
      (set! /fl_flag1 0))
     (#t
      (@Fail "The `D_If' statement should have a FALSE condition")
      (set! /fl_flag1 1)))))
  ((= (@ST (@I)) //T_/Floop)
   ;A `Do-Od' loop can be simplified if it has no terminal statements 
   ; or it contains two copies of the same statements.
   ; Old version: IFMATCH Statement DO ~*S; ~*=S OD 
   (cond
    ((null? (@TVs))
     (@Pass))
    ((@Trans? //T/R_/Remove_/Dummy_/Loop)
     (@Pass))
    ((even? (@Size (@Get_n (@I) 1)))
     (let ((//S (@Cs (@Get_n (@I) 1)))
           (/mid (quotient (@Size (@Get_n (@I) 1)) 2)))
      (cond
       ((@Seq_Equal? (@Sub_Seg //S 1 /mid) (@Final_Seg //S (+ /mid 1)))
        (@Pass))
       (#t
        (@Fail "The `Do-Od' loop is as simple as possible.")))))
    (#t
     (@Fail "The `Do-Od' loop is as simple as possible."))))
  ((= (@ST (@I)) //T_/A_/S)
   (cond
    ((= (@Size (@Get_n (@I) 2)) 1)
     (@Pass))
    (#t
     (@Fail "The action system must have just a single action."))))
  ((and (= (@ST (@I)) //T_/Var) (null? (intersection-n (@Assigned (@Get_n (@I) 1)) (@Variables (@Get_n (@I) 2)))))
   (@Pass))
  ((= (@ST (@I)) //T_/Var)
   (let ((/vars '()))
    (for-in /assign (@Cs (@Get_n (@I) 1)) 
     (cond
      ((= (@ST (@Get_n /assign 1)) //T_/Var_/Lvalue)
       (set! /vars (cons (@V (@Get_n /assign 1)) /vars)))))
    (@Down_To 2)
    (@Down)
    (cond
     ((and (= (@ST (@I)) //T_/Assignment) (= (@Size (@I)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@Get_n (@I) 1) 1)) /vars) (not-member (@V (@Get_n (@Get_n (@I) 1) 1)) (@Used (@Get_n (@I) 1))) (null? (intersection-n (@Used (@Get_n (@Get_n (@I) 1) 2)) (@Assigned (@Get_n (@GParent) 1)))))
      (@Pass))
     (#t
      (@Fail "This VAR cannot be simplified")))))
  ((= (@ST (@I)) //T_/Where)
   (let ((//O/K 1)
         (//Names (my-map @V1 (@Cs (@Get_n (@I) 2)))))
    (while (and (= //O/K 1) (not (null? //Names))) 
     (begin
      (cond
       ((or (@Proc_Called? (car //Names) (@Get_n (@I) 1)) (@Funct_Called? (car //Names) (@Get_n (@I) 1)))
        (set! //O/K 0)))
      (set! //Names (cdr //Names))))
    (cond
     ((= //O/K 1)
      (@Pass))
     (#t
      (@Fail "One of the procedures of functions is called.")))))
  ((and (= (@ST (@I)) //T_/While) (= (@ST (@Get_n (@I) 1)) //T_/True))
   (@Pass))
  ((and (= (@ST (@I)) //T_/While) (= (@ST (last-1 (@Cs (@Get_n (@I) 2)))) //T_/Assert) (@Implies? (@Get_n (last-1 (@Cs (@Get_n (@I) 2))) 1) (@Get_n (@I) 1)))
   (@Pass))
  ((and (= (@ST (@I)) //T_/Assign) (@Up?) (= (@ST (@Parent)) //T_/Assignment) (@LR_Equal? (@Get_n (@I) 1) (@Get_n (@I) 2)))
   (@Pass))
  ((= (@ST (@I)) //T_/Assignment)
   (for-in /assign (@Cs (@I)) 
    (cond
     ((@LR_Equal? (@Get_n /assign 1) (@Get_n /assign 2))
      (@Pass))))
   (cond
    ((not (@Passed?))
     (@Fail "Cannot simplify this assignment"))))
  ((and (= (@ST (@I)) //T_/Funct) (= (@Size (@Get_n (@I) 3)) 1) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 3) 1) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@Get_n (@Get_n (@I) 3) 1) 1)) (@Used (@Get_n (@I) 5))) (@Set_Subset? (@Stat_Types (@Get_n (@I) 4)) (@Make_Set (list //T_/Cond //T_/Skip //T_/Assignment))))
   ; Check that all assignments are to the assigned variable 
   (let ((/v-save /v))
    (set! /v (@V (@Get_n (@Get_n (@Get_n (@I) 3) 1) 1)))
    (@Down_To 4)
    (@Foreach_Statement /foreach-simplify_item-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Foreach_Stats /foreach-simplify_item-2 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (cond
     ((not (@Failed?))
      (@Pass)))
    (set! /v /v-save)))
  ((and (= (@GT (@I)) //T_/Guarded) (= (@Size (@Get_n (@I) 2)) 1) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Cond))
   ;We can simplify using `Align_Nested_Statements' if 
   ;there is a single `IF' statement in the guard.     
   (@Pass))
  ((or (= (@GT (@I)) //T_/Expression) (= (@GT (@I)) //T_/Condition))
   (@Pass))
  (#t
   (@Fail "The selected item is not of an appropriate type."))))

(define (@Simplify_Item_Code //Data)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((= (@ST (@Get_n (@I) 1)) //T_/False)
      (@Delete)
      (set! /fl_flag1 0))
     ((@Right?)
      (cond
       ((= (@ST (@Get_n (@I) 1)) //T_/True)
        (@Delete_Rest)
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0))))
     (#t
      (set! /fl_flag1 1))))
   (@Up)
   (@Fix_Cond))
  ((= (@ST (@I)) //T_/D_/If)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((null? (@I))
      (set! /fl_flag1 1))
     ((= (@ST (@Get_n (@I) 1)) //T_/False)
      (@Delete)
      (set! /fl_flag1 0))
     ((@Right?)
      (@Right)
      (set! /fl_flag1 0))
     (#t
      (set! /fl_flag1 1))))
   (@Up)
   (cond
    ((= (@Size (@I)) 0)
     (@Paste_Over (@Make //T_/Abort '() '())))
    ((= (@Size (@I)) 1)
     (cond
      ((= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/True)
       (@Splice_Over (@Cs (@Get_n (@Get_n (@I) 1) 2))))
      (#t
       (@Splice_Over (cons (@Make //T_/Assert '() (list (@Get_n (@Get_n (@I) 1) 1))) (@Cs (@Get_n (@Get_n (@I) 1) 2)))))))))
  ((= (@ST (@I)) //T_/Floop)
   (cond
    ((null? (@TVs))
     (@Paste_Over (@Make //T_/Abort '() '())))
    ((@Trans? //T/R_/Remove_/Dummy_/Loop)
     (@Trans //T/R_/Remove_/Dummy_/Loop ""))
    (#t
     (let ((//S (@Cs (@Get_n (@I) 1)))
           (/mid (quotient (@Size (@Get_n (@I) 1)) 2)))
      (set! //S (@Sub_Seg //S 1 /mid))
      (@Paste_Over (@Make 133 '() (list (@Make 17 '() //S))))))))
  ((= (@ST (@I)) //T_/A_/S)
   (cond
    ((not (@Regular? (@Get_n (@Get_n (@I) 2) 1)))
     (@Down_To 2)
     (@Down)
     (@Down_Last)
     (@Down_Last)
     (@Paste_After (@Make //T_/Call (@Make_Name "Z") '()))
     (@Up)
     (@Up)
     (@Up)
     (@Up)))
   (@Foreach_Non_Action_Statement /foreach-simplify_item-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Put_AS_In_Double_Loop))
  ((and (= (@ST (@I)) //T_/Var) (null? (intersection-n (@Assigned (@Get_n (@I) 1)) (@Variables (@Get_n (@I) 2)))))
   (@Splice_Over (@Cs (@Get_n (@I) 2))))
  ((= (@ST (@I)) //T_/Var)
   ; Look for an initial assignment to a variable 
   (let ((/vars '())
         (/var (@Get_n (@Get_n (@Get_n (@Get_n (@I) 2) 1) 1) 1))
         (/val (@Get_n (@Get_n (@Get_n (@Get_n (@I) 2) 1) 1) 2)))
    (@Down)
    (@Down)
    ; to first assign 
    (while (and (or (not (= //T_/Var_/Lvalue (@ST (@Get_n (@I) 1)))) (not (equal? (@V (@Get_n (@I) 1)) (@V /var)))) (@Right?)) 
     (@Right))
    (cond
     ((and (= //T_/Var_/Lvalue (@ST (@Get_n (@I) 1))) (equal? (@V (@Get_n (@I) 1)) (@V /var)))
      (@Paste_Over (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /val))))))
    (@Up)
    (@Right)
    (@Down)
    ; to first statement 
    (@Clever_Delete)))
  ((= (@ST (@I)) //T_/Where)
   (@Splice_Over (@Cs (@Get_n (@I) 1))))
  ((and (= (@ST (@I)) //T_/While) (= (@ST (@Get_n (@I) 1)) //T_/True))
   (@Paste_Over (@Make //T_/Abort '() '())))
  ((and (= (@ST (@I)) //T_/While) (= (@ST (last-1 (@Cs (@Get_n (@I) 2)))) //T_/Assert) (@Equal? (@Get_n (@I) 1) (last-1 (@Cs (@Get_n (@I) 2)))))
   (let ((//B (@Get_n (@I) 1)))
    (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() (list (@Make 108 '() '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))
  ((and (= (@ST (@I)) //T_/While) (= (@ST (last-1 (@Cs (@Get_n (@I) 2)))) //T_/Assert) (@Implies? (@Get_n (last-1 (@Cs (@Get_n (@I) 2))) 1) (@Get_n (@I) 1)))
   (let ((//B (@Get_n (@I) 1)))
    (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() (list (@Make 108 '() '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))
  ((and (= (@ST (@I)) //T_/Assign) (@LR_Equal? (@Get_n (@I) 1) (@Get_n (@I) 2)))
   (@Clever_Delete))
  ((= (@ST (@I)) //T_/Assignment)
   (let ((/new '()))
    (for-in /assign (@Cs (@I)) 
     (cond
      ((not (@LR_Equal? (@Get_n /assign 1) (@Get_n /assign 2)))
       (set! /new (cons /assign /new)))))
    (cond
     ((null? /new)
      (@Clever_Delete))
     (#t
      (@Paste_Over (@Make //T_/Assignment '() (reverse /new)))))))
  ((and (= (@ST (@I)) //T_/Funct) (= (@Size (@Get_n (@I) 3)) 1) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 3) 1) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@Get_n (@Get_n (@I) 3) 1) 1)) (@Used (@Get_n (@I) 5))) (@Set_Subset? (@Stat_Types (@Get_n (@I) 4)) (@Make_Set (list //T_/Cond //T_/Skip //T_/Assignment))))
   ; All assignments are to the assigned variable 
   ; Delete the assigns and convert T_Cond statements to T_If 
   (let ((//S (@Get_n (@I) 4))
         (/expn-save /expn)
         (/v-save /v))
    (set! /expn '())
    (set! /v (@Lvalue_To_Expn (@Get_n (@Get_n (@Get_n (@I) 3) 1) 1)))
    (@Down_To 3)
    (@Paste_Over (@Make //T_/Assigns '() '()))
    (@Right)
    (@Paste_Over (@Skips))
    (@Right)
    ; To the expression 
    (set! /expn (@SI_Stats_To_Expn //S /v))
    (@Foreach_Global_Var /foreach-simplify_item-4 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    #t
    (set! /expn /expn-save)
    (set! /v /v-save)))
  ((= (@GT (@I)) //T_/Guarded)
   (while (@Trans? //T/R_/Move_/To_/Right) 
    (@Trans //T/R_/Move_/To_/Right ""))
   (@Trans //T/R_/Align_/Nested_/Statements "")
   (@Up)
   (@Trans //T/R_/Delete_/All_/Skips "")
   (@Down_Last)
   (@Left))
  ((= (@GT (@I)) //T_/Expression)
   (@Paste_Over (@Simplify_Expn (@I))))
  (#t
   (@Paste_Over (@Simplify_Cond (@I))))))

; Put the action system body in a double loop and then simplify the loops. 
; Special case: If the first statement is a D_IF on entry_point, 
; then put each entry point body in a double loop. 
(define (@Put_AS_In_Double_Loop)
 (let ((//S (@I)))
  (@Edit)
  (@New_Program (@Get_n (@Get_n (@Get_n //S 2) 1) 2))
  (cond
   ((and (= (@ST (@Get_n (@I) 1)) //T_/D_/If) (member (@Make_Name "entry_point") (@Variables (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1))))
    (@Down)
    (cond
     ((@Trans? //T/R_/Fully_/Absorb_/Right)
      (@Trans //T/R_/Fully_/Absorb_/Right "")))
    (@Down)
    ; to first guarded 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (@Down_To 2)
      (set! //S (@Cs (@I)))
      (@Paste_Over (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() //S))))))))))
      (@Down)
      (cond
       ((@Trans? //T/R_/Double_/To_/Single_/Loop)
        (@Trans //T/R_/Double_/To_/Single_/Loop "")))
      (cond
       ((@Trans? //T/R_/Remove_/Dummy_/Loop)
        (@Trans //T/R_/Remove_/Dummy_/Loop "")))
      (@Up)
      (@Up)
      ; back to guarded 
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0))))))
   (#t
    (set! //S (@Cs (@I)))
    (@Paste_Over (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() //S))))))))))
    (@Down)
    (cond
     ((@Trans? //T/R_/Double_/To_/Single_/Loop)
      (@Trans //T/R_/Double_/To_/Single_/Loop "")))
    (cond
     ((@Trans? //T/R_/Remove_/Dummy_/Loop)
      (@Trans //T/R_/Remove_/Dummy_/Loop "")))))
  (@Goto '())
  (@Trans //T/R_/Delete_/All_/Skips "")
  (set! //S (@Cs (@Program)))
  (@Undo_Edit)
  (@Splice_Over //S)))

(define (@SI_Stats_To_Expn //S /v-par)
 (let ((/v-save /v)
       (//I /v-par)
       (funct-result '()))
  (set! /v /v-par)
  (cond
   ((= (@GT //S) //T_/Statements)
    (set! //I (@SI_Stats_To_Expn (@Get_n //S 1) /v)))
   ((and (= (@ST //S) //T_/Cond) (= (@Size //S) 2))
    (set! //I (@Make //T_/If '() (list (@Get_n (@Get_n //S 1) 1) (@SI_Stats_To_Expn (@Get_n (@Get_n //S 1) 2) /v) (@SI_Stats_To_Expn (@Get_n (@Get_n //S 2) 2) /v)))))
   ((= (@ST //S) //T_/Cond)
    (set! //I (@Make //T_/If '() (list (@Get_n (@Get_n //S 1) 1) (@SI_Stats_To_Expn (@Get_n (@Get_n //S 1) 2) /v) (@SI_Stats_To_Expn (@Make //T_/Cond '() (cdr (@Cs //S))) /v)))))
   ((= (@ST //S) //T_/Assignment)
    (set! //I (@Get_n (@Get_n //S 1) 2)))
   ((= (@ST //S) //T_/Skip)
    (set! //I /v))
   (#t
    (@Print_WSL //I "")
    (error "@SI_Stats_To_Expn: Unexpected statement!")))
  (set! funct-result //I)
  (set! /v /v-save)
  funct-result))

#t
