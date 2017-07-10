;;; Scheme translation of WSL code
(define (/foreach-semantic_slice-1 //Depth //A/S_/Type)
 (cond
  ((not (= (@ST (@I)) //T_/Var_/Lvalue))
   (@Fail "All assignments must be to simple variables."))))

(define (/foreach-semantic_slice-2 //Depth //A/S_/Type)
 (cond
  ((@Trans? //T/R_/Floop_/To_/While)
   (@Trans //T/R_/Floop_/To_/While ""))))

(define (/foreach-semantic_slice-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/While)
   (@Make_And_Use_Assertion))))

(define (/foreach-semantic_slice-4 //Depth //A/S_/Type)
 (cond
  ((@Trans? //T/R_/Refine_/Spec)
   (@Trans //T/R_/Refine_/Spec ""))))

(define (/foreach-semantic_slice-5 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/For_/In)
   (cond
    ((@Trans? //T/R_/For_/In_/To_/Reduce)
     (@Trans //T/R_/For_/In_/To_/Reduce ""))))
  ((= (@ST (@I)) //T_/While)
   (@Make_And_Use_Assertion)
   (cond
    ((@Trans? //T/R_/While_/To_/Reduce)
     (@Trans //T/R_/While_/To_/Reduce ""))
    ((@Trans? //T/R_/While_/To_/For_/In)
     (@Trans //T/R_/While_/To_/For_/In ""))
    ((> /speculative_unroll 0)
     (set! /speculative_unroll (- /speculative_unroll 1))
     (@Speculative_Unrolling //X /speculative_unroll))))))

(define (/foreach-semantic_slice-6 //Depth //A/S_/Type)
 (cond
  ((@Trans? //T/R_/Refine_/Spec)
   (@Trans //T/R_/Refine_/Spec ""))))

(define (/foreach-semantic_slice-7 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   ; to first assign 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (set! /v (@Get_n (@I) 1))
     (set! /e (@Get_n (@I) 2))
     (cond
      ((and (= (@ST /v) //T_/Var_/Lvalue) (member (@V /v) //X) (null? (gethash /bad_var (@V /v))) (or (= (@ST /e) //T_/Number) (= (@ST /e) //T_/String) (and (= (@ST /e) //T_/Variable) (member (@V /e) /constants))))
       (cond
        ((null? (gethash /var_val (@V /v)))
         (puthash /var_val (@V /v) /e))
        ((not (@Equal? (gethash /var_val (@V /v)) /e))
         (puthash /bad_var (@V /v) 1))))
      (#t
       (for-in /var (@Assigned (@I)) 
        (puthash /bad_var /var 1))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))
  (#t
   (cond
    ((or (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/Cond) (= (@ST (@I)) //T_/D_/If) (= (@ST (@I)) //T_/D_/Do) (= (@ST (@I)) //T_/Floop))
     ; Check the components 
    )
    (#t
     (for-in /var (@Assigned (@I)) 
      (puthash /bad_var /var 1)))))))

(define (/foreach-semantic_slice-8 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assert) (@Trans? //T/R_/Use_/Assertion))
   (set! /posn (@Posn))
   (@Trans //T/R_/Use_/Assertion "")
   (@Goto /posn))))

(define /%const__semantic_slice__1 (@Make 141 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()))))))
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
(define (@Semantic_Slice_Test)
 (cond
  ((and (not (= (@GT (@I)) //T_/Statements)) (not (= (@GT (@I)) //T_/Statement)))
   (@Fail "Can only slice statements."))
  ((not (@Is_Proper?))
   (@Fail "Current item is not a proper sequence."))
  ((@Set_Subset? (@Stat_Types (@I)) (union-n //W/P_/Types_/Set (@Make_Set (list //T_/While //T_/A_/Proc_/Call //T_/Floop //T_/Exit //T_/For_/In))))
   (@Foreach_Lvalue /foreach-semantic_slice-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (cond
    ((not (@Failed?))
     (@Pass))))
  (#t
   (@Fail "The current item contains a statement which cannot be sliced."))))

(define (@Semantic_Slice_Code //Data)
 (let ((//X-save //X)
       (//R '())
       (/orig '()))
  (set! //X '())
  ; Keep the original order of X here for the message 
  (display-list "Semantic Slice, initial variables are: " //Data)
  (set! //X (@Make_Set (my-map @Make_Name (@Split //Data))))
  (@Foreach_Statement /foreach-semantic_slice-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //R (@Semantic_Slice_Sub //X 1))
  (display-list "Semantic Slice,   final variables are: " (@Join " " (my-map @N_String (wsl-ref //R 2))))
  (set! //X //X-save)))

(define (@Semantic_Slice_Sub //X-par /speculative_unroll-par)
 (let ((/speculative_unroll-save /speculative_unroll)
       (//X-save //X)
       (//R '())
       (/orig '())
       (/save //A_/Proc_/Call_/Filter)
       (funct-result '()))
  (set! /speculative_unroll /speculative_unroll-par)
  (set! //X //X-par)
  (@Foreach_Statement /foreach-semantic_slice-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (set! /orig (@Program))
    (set! //R (@SSlice (@I) //X))
    (@Paste_Over (wsl-ref //R 1))
    (display-list "-------- Abstract version:")
    (@Checkpoint "")
    (@Foreach_Statement /foreach-semantic_slice-4 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    ; Attempt to reduce any WHILE loops: 
    (display-list "-------- Refined version:")
    (@Checkpoint "")
    (@Foreach_Statement /foreach-semantic_slice-5 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    ; Refine any spec statements in the result: 
    (@Foreach_Statement /foreach-semantic_slice-6 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Trans //T/R_/Delete_/All_/Skips "")
    (display-list "-------- after While_To_Reduce and Refine_Spec")
    (@Checkpoint "")
    (set! //A_/Proc_/Call_/Filter (@Variables (@Program)))
    (cond
     ((@Trans? //T/R_/Constant_/Propagation)
      (@Trans //T/R_/Constant_/Propagation "")))
    (@Paste_Over (@Simplify (@I) 50))
    (display-list "-------- after C_P")
    (@Checkpoint "")
    ; If the result is smaller, or is the same size but with fewer specifications 
    ; or the same size and number of specifications, but with fewer expressions 
    ; then carry on processing 
    (cond
     ((and (or (not (equal? (@Spec_Type_Count //T_/Spec (@Program)) (@Spec_Type_Count //T_/Spec /orig))) (>= (@Gen_Type_Count //T_/Expression (@Program)) (@Gen_Type_Count //T_/Expression /orig)) (not (equal? (@Total_Size (@Program)) (@Total_Size /orig)))) (or (>= (@Spec_Type_Count //T_/Spec (@Program)) (@Spec_Type_Count //T_/Spec /orig)) (not (equal? (@Total_Size (@Program)) (@Total_Size /orig)))) (>= (@Total_Size (@Program)) (@Total_Size /orig)))
      (set! /fl_flag1 1))
     ((< (@Total_Size (@Program)) (@Total_Size /orig))
      (set! /fl_flag1 0))
     ((and (equal? (@Total_Size (@Program)) (@Total_Size /orig)) (< (@Spec_Type_Count //T_/Spec (@Program)) (@Spec_Type_Count //T_/Spec /orig)))
      (set! /fl_flag1 0))
     (#t
      (set! /fl_flag1 0)))))
  (@Paste_Over /orig)
  (set! //A_/Proc_/Call_/Filter /save)
  (set! funct-result //R)
  (set! /speculative_unroll /speculative_unroll-save)
  (set! //X //X-save)
  funct-result))

; Slices the given item with the given slicing criterion 
; (set of final variables required). 
; Returns the sliced program plus new slicing criterion 
; (the set of input variables required) 
(define (@SSlice //I //X-par)
 (let ((//X-save //X)
       (//R '())
       (/new '())
       (/new/X '())
       (funct-result '()))
  (set! //X //X-par)
  ; Trim trailing statements which do not assign to variables in X: 
  (cond
   ((= (@ST //I) //T_/Statements)
    (let ((/comps (reverse (@Cs //I)))
          (/keep '()))
     (while (and (not (null? /comps)) (null? (intersection-n //X (@Assigned (car /comps))))) 
      (begin
       (cond
        ((= (@ST (car /comps)) //T_/Assert)
         (set! /keep (cons (car /comps) /keep))))
       (set! /comps (cdr /comps))))
     (set! /comps (concat (reverse /keep) /comps))
     (cond
      ((null? /comps)
       (set! //I (@Skips)))
      (#t
       (set! //I (@Make //T_/Statements '() (reverse /comps))))))))
  ; Convert to a spec statement, if possible, then slice: 
  (cond
   ((and (not-member //T_/While (@Stat_Types //I)) (not-member //T_/For_/In (@Stat_Types //I)))
    (cond
     ((or (= (@ST //I) //T_/Spec) (= (@ST //I) //T_/Assert) (= (@ST //I) //T_/Skip) (= (@ST //I) //T_/Abort))
      ; Nothing to do here 
     )
     ((and (= (@ST //I) //T_/Statements) (= (@Size //I) 1) (or (= (@ST (@Get_n //I 1)) //T_/Spec) (= (@ST (@Get_n //I 1)) //T_/Assert) (= (@ST (@Get_n //I 1)) //T_/Skip) (= (@ST (@Get_n //I 1)) //T_/Abort)))
      ; Nothing to do here 
     )
     (#t
      (@Edit)
      (@New_Program //I)
      (cond
       ((@Trans? //T/R_/Prog_/To_/Spec)
        (@Trans //T/R_/Prog_/To_/Spec "")))
      (set! //I (@Program))
      (@Undo_Edit)))))
  (cond
   ((= (@ST //I) //T_/Statements)
    (for-in //S (reverse (@Cs //I)) 
     (begin
      (set! //R (@SSlice //S //X))
      (cond
       ((= (@GT (wsl-ref //R 1)) //T_/Statement)
        (set! /new (cons (wsl-ref //R 1) /new)))
       ((= (@GT (wsl-ref //R 1)) //T_/Statements)
        (set! /new (concat (@Cs (wsl-ref //R 1)) /new)))
       (#t
        (@Print_WSL (wsl-ref //R 1) "")
        (error "Unexpected type in R")))
      (set! //X (wsl-ref //R 2))))
    (set! //R (list (@Make //T_/Statements '() /new) //X)))
   ((or (= (@ST //I) //T_/Assert) (= (@ST //I) //T_/Comment))
    (set! //R (list //I //X)))
   ((and (= (@GT //I) //T_/Statement) (null? (intersection-n //X (@Assigned //I))))
    (set! //R (list (@Skip) //X)))
   ((= (@ST //I) //T_/Assignment)
    (let ((/new '())
          (/new/I '()))
     (for-in /assign (@Cs //I) 
      (cond
       ((not (null? (intersection-n (@Assigned /assign) //X)))
        (set! /new (cons /assign /new)))))
     (cond
      ((null? /new)
       (set! //R (list (@Skip) //X)))
      (#t
       (set! /new/I (@Make //T_/Assignment '() (reverse /new)))
       (set! //R (list /new/I (union-n (@Set_Difference //X (@Assigned /new/I)) (@Used /new/I))))))))
   ((or (= (@ST //I) //T_/Cond) (= (@ST //I) //T_/D_/If))
    (for-in /guard (@Cs //I) 
     (begin
      (set! //R (@SSlice (@Get_n /guard 2) //X))
      (set! /new (cons (@Make //T_/Guarded '() (list (@Get_n /guard 1) (wsl-ref //R 1))) /new))
      (set! /new/X (union-n /new/X (@Variables (@Get_n /guard 1)) (wsl-ref //R 2)))))
    (set! //R (list (@Make (@ST //I) '() (reverse /new)) /new/X)))
   ((= (@ST //I) //T_/For_/In)
    ; Keep processing the body and adding the original vars 
    ; plus the vars in the expression, until the result converges 
    (let ((/v-save /v)
          (//E (@Variables (@Get_n //I 2)))
          (//S-save //S)
          (/new/X //X))
     (set! /v (@V (@Get_n //I 1)))
     (set! //S (@Get_n //I 3))
     (set! //R (@SSlice //S /new/X))
     (wsl-set! //R (union-n (wsl-ref //R 2) /new/X //E) 2)
     (cond
      ((member /v //X)
       (wsl-set! //R (union-n (wsl-ref //R 2) (list /v)) 2))
      (#t
       (wsl-set! //R (@Set_Difference (wsl-ref //R 2) (list /v)) 2)))
     (while (not (equal? /new/X (wsl-ref //R 2))) 
      (begin
       (set! /new/X (wsl-ref //R 2))
       (set! //R (@SSlice //S /new/X))
       (wsl-set! //R (union-n (wsl-ref //R 2) /new/X //E) 2)
       (cond
        ((member /v //X)
         (wsl-set! //R (union-n (wsl-ref //R 2) (list /v)) 2))
        (#t
         (wsl-set! //R (@Set_Difference (wsl-ref //R 2) (list /v)) 2)))))
     (set! /v /v-save)
     (set! //S //S-save))
    (set! //R (list (@Make //T_/For_/In '() (list (@Get_n //I 1) (@Get_n //I 2) (wsl-ref //R 1))) (wsl-ref //R 2))))
   ((= (@ST //I) //T_/While)
    ; Keep processing the body and adding the original vars 
    ; plus the vars in the condition, until the result converges 
    (@Edit)
    (@New_Program (@Make //T_/Statements '() (list //I)))
    (@Down)
    (cond
     ((@Trans? //T/R_/While_/To_/Reduce)
      (@Trans //T/R_/While_/To_/Reduce ""))
     ((> /speculative_unroll 0)
      (@Speculative_Unrolling //X (- /speculative_unroll 1))))
    (set! /new (@Program))
    (@Undo_Edit)
    (cond
     ((and #f (> /speculative_unroll 0))
      (display-list "======================= orig WHILE, X = " (my-map @N_String //X))
      (@PP_Item //I 80 "")
      (display-list "                    unrolled WHILE:")
      (@PP_Item /new 80 "")))
    (cond
     ((not (@Equal? (@Make //T_/Statements '() (list //I)) /new))
      ; Speculative unrolling must have worked 
      (set! //R (@SSlice /new //X)))
     (#t
      ; Original WHILE loop processing 
      (let ((//B-save //B)
            (//S-save //S)
            (/new/X //X))
       (set! //B (@Variables (@Get_n //I 1)))
       (set! //S (@Get_n //I 2))
       (set! //R (@SSlice //S /new/X))
       (wsl-set! //R (union-n (wsl-ref //R 2) /new/X //B) 2)
       (while (not (equal? /new/X (wsl-ref //R 2))) 
        (begin
         (set! /new/X (wsl-ref //R 2))
         (set! //R (@SSlice //S /new/X))
         (wsl-set! //R (union-n (wsl-ref //R 2) /new/X //B) 2)))
       (set! //B //B-save)
       (set! //S //S-save))
      (set! //R (list (@Make //T_/While '() (list (@Get_n //I 1) (wsl-ref //R 1))) (wsl-ref //R 2))))))
   ((= (@ST //I) //T_/Var)
    (let ((/v-save /v)
          (/e-save /e)
          (//S-save //S)
          (/new/X '()))
     (set! /v (@Get_n (@Get_n (@Get_n //I 1) 1) 1))
     (set! /e (@Get_n (@Get_n (@Get_n //I 1) 1) 2))
     (set! //S (@Get_n //I 2))
     (set! //R (@SSlice (@Get_n //I 2) (@Set_Difference //X (list (@V /v)))))
     (set! //S (wsl-ref //R 1))
     (set! /new/X (union-n (@Set_Difference (wsl-ref //R 2) (list (@V /v))) (intersection-n (list (@V /v)) //X)))
     (cond
      ((member (@V /v) (wsl-ref //R 2))
       (set! //R (list (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /e))))) (@Make 17 '() (list //S)))) (union-n /new/X (@Used /e)))))
      (#t
       (set! //R (list (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Make 207 (@Make_Name "BOTTOM") '()))))) (@Make 17 '() (list //S)))) /new/X))))
     (set! /v /v-save)
     (set! /e /e-save)
     (set! //S //S-save)))
   ((= (@ST //I) //T_/Spec)
    (let ((/new (intersection-n (@Assigned //I) //X)))
     (cond
      ((equal? /new (@Assigned //I))
       ; All variables are needed 
       (set! //R (list //I (union-n (@Set_Difference //X (@Assigned //I)) (@Used //I)))))
      (#t
       (let ((/removed (@Set_Difference (@Assigned //I) /new))
             (/vars '())
             (/cond (@Get_n //I 2))
             (/newvars '())
             (/new/I '()))
        ; Rename the removed variables and enclose in EXISTS 
        (for-in /var /removed 
         (begin
          (set! /vars (cons (@Make //T_/Var_/Lvalue (@Make_Name (string-append (@N_String /var) "_1")) '()) /vars))
          (set! /cond (@Rename_Primed /var (@V (car /vars)) /cond))))
        (for-in /var /new 
         (set! /newvars (cons (@Make //T_/Var_/Lvalue /var '()) /newvars)))
        (set! /cond (@Make //T_/Exists '() (list (@Make //T_/Lvalues '() /vars) /cond)))
        (set! /cond (@Simplify /cond 50))
        (set! /new/I (@Make //T_/Spec '() (list (@Make //T_/Lvalues '() /newvars) /cond)))
        (set! //R (list /new/I (union-n (@Set_Difference //X (@Assigned /new/I)) (@Used /new/I)))))))))
   ((= (@ST //I) //T_/A_/Proc_/Call)
    (set! //R (list //I (union-n //X (@Variables //I)))))
   ((= (@ST //I) //T_/Assert)
    (set! //R (list (@Skip) //X)))
   ((= (@ST //I) //T_/Skip)
    (set! //R (list //I //X)))
   ((= (@ST //I) //T_/Abort)
    (set! //R (list //I '())))
   (#t
    (error "Unexpected type: " (@Type_Name (@ST //I)))))
  (set! funct-result //R)
  (set! //X //X-save)
  funct-result))

(define (@Rename_Primed /old /new //I)
 (let ((//R //I))
  (cond
   ((= (@ST //I) //T_/Primed_/Var)
    (cond
     ((equal? (@V //I) /old)
      (set! //R (@Make //T_/Variable /new '())))))
   ((@Cs? //I)
    (let ((/comps '()))
     (for-in /comp (@Cs //I) 
      (set! /comps (cons (@Rename_Primed /old /new /comp) /comps)))
     (set! //R (@Make (@ST //I) '() (reverse /comps))))))
  //R))

; If any variable in X is assigned only from a given constant in the loop 
; then try unrolling the loop. 
; NB: This assumes that the loop is the only statement in the program 
; eg we are called in the body of a FOREACH 
(define (@Speculative_Unrolling //X-par /speculative_unroll-par)
 (let ((/speculative_unroll-save /speculative_unroll)
       (//X-save //X))
  (set! /speculative_unroll /speculative_unroll-par)
  (set! //X //X-par)
  (let ((/constants-save /constants)
        (/bad_var-save /bad_var)
        (/var_val-save /var_val)
        (/orig (@Program))
        (//R '())
        (/good '())
        (//Q-save //Q)
        (/posn-save /posn))
   (set! /constants (@Set_Difference (@Used (@I)) (@Assigned (@I))))
   (set! /bad_var (hash-table))
   (set! /var_val (hash-table))
   (set! //Q '())
   (set! /posn '())
   ; First see if there are any good vars in X: 
   (@Foreach_Statement /foreach-semantic_slice-7 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; IF so, then see if unrolling will help: 
   (set! /good (@Set_Difference (@Make_Set (@Hash_Keys /var_val)) (@Make_Set (@Hash_Keys /bad_var))))
   (cond
    ((not (null? /good))
     (set! //Q (@Speculative_Entire_Unrolling  /good /speculative_unroll //Q))
     ; If entire loop unrolling succeeded, then Q contains a condition 
     ; which must be false on the first iteration of the next loop 
     ; (If B is true, then Q must be false for the first loop to terminate) 
     (cond
      ((not (null? //Q))
       (@Right)))
     (@Trans //T/R_/Unroll_/Loop "")
     (@Down)
     (@Down_To 2)
     (@Down)
     ; to first statement in unrolled body 
     (cond
      ((not (null? //Q))
       (@UA_Process (@Not //Q))))
     (cond
      ((and (= (@ST (@I)) //T_/Cond) (@Right?))
       ; Expand the condition and simplify both arms 
       (@Trans //T/R_/Fully_/Absorb_/Right "")
       (cond
        ((@Trans? //T/R_/Insert_/Assertion)
         (@Trans //T/R_/Insert_/Assertion "")
         (@Ateach_Statement /foreach-semantic_slice-8 0 (@AS_Type) 0)
         (cond
          ((null? (@Program))
           (@New_Program (@Skips))))))))
     (@Goto '())
     (set! //R (@Semantic_Slice_Sub //X /speculative_unroll))
     (cond
      (#f
       (display-list "======================")
       (@PP_Item /orig 80 "")
       (display-list "Stats: " (@Stat_Count (@Program)) " " (@Stat_Count /orig))
       (display-list "Expns: " (@Gen_Type_Count //T_/Expression (@Program)) " " (@Gen_Type_Count //T_/Expression /orig))
       (display-list "Conds: " (@Gen_Type_Count //T_/Condition (@Program)) " " (@Gen_Type_Count //T_/Condition /orig))
       (@Checkpoint "")
       (display-list "======================")
       #t))
     (cond
      ((or (< (@Stat_Count (@Program)) (@Stat_Count /orig)) (and (equal? (@Stat_Count (@Program)) (@Stat_Count /orig)) (<= (@Gen_Type_Count //T_/Expression (@Program)) (@Gen_Type_Count //T_/Expression /orig))))
       ; keep new version 
      )
      (#t
       (@Paste_Over /orig)))))
   #t
   (set! /constants /constants-save)
   (set! /bad_var /bad_var-save)
   (set! /var_val /var_val-save)
   (set! //Q //Q-save)
   (set! /posn /posn-save))
  (set! /speculative_unroll /speculative_unroll-save)
  (set! //X //X-save)))

(define (@Speculative_Entire_Unrolling //X-par /speculative_unroll-par //Q-par)
 (let ((//Q-save //Q)
       (/speculative_unroll-save /speculative_unroll)
       (//X-save //X)
       (funct-result '()))
  (set! //Q //Q-par)
  (set! /speculative_unroll /speculative_unroll-par)
  (set! //X //X-par)
  (let ((/count (hash-table))
        (/posn-save /posn)
        (/name '()))
   (set! /posn (@Posn))
   (for-in /var //X 
    (puthash /count /var 0))
   (@Down_To 2)
   (cond
    ((and (= (@ST (@Get_n (@I) 1)) //T_/Cond) (null? (intersection-n //X (@Assigned (@Make //T_/Statements '() (cdr (@Cs (@I))))))))
     ; Count the number of guards in which X vars are assigned 
     ; If there is only one, then this is the condition we want 
     ; Speculatively unroll on loop cond AND NOT(Q) 
     (@Down)
     (@Down)
     ; to first guarded 
     (for-in /var (intersection-n //X (@Assigned (@I))) 
      (puthash /count /var (+ (gethash /count /var) 1)))
     (while (@Right?) 
      (begin
       (@Right)
       (for-in /var (intersection-n //X (@Assigned (@I))) 
        (puthash /count /var (+ (gethash /count /var) 1)))))
     (for-in /var //X 
      (cond
       ((= (gethash /count /var) 1)
        (set! /name /var))))
     (cond
      ((not (null? /name))
       ; Found a suitable variable 
       ; Compute the condition for this variable 
       (set! //Q (@Make //T_/False '() '()))
       (@To 1)
       ; back to first guarded 
       (set! /fl_flag1 0)
       (while (= /fl_flag1 0) 
        (cond
         ((member /name (intersection-n //X (@Assigned (@I))))
          (set! //Q (@Or //Q (@Not (@Get_n (@I) 1))))
          (set! /fl_flag1 1))
         (#t
          (set! //Q (@Or //Q (@Get_n (@I) 1)))
          (cond
           ((not (@Right?))
            (set! /fl_flag1 1))
           (#t
            (@Right)
            (set! /fl_flag1 0))))))
       (@Goto /posn)
       (set! //Q (@Entire_Loop_Unroll  //Q))))))
   (@Goto /posn)
   (set! /posn /posn-save))
  (set! funct-result //Q)
  (set! //Q //Q-save)
  (set! /speculative_unroll /speculative_unroll-save)
  (set! //X //X-save)
  funct-result))

(define (@Entire_Loop_Unroll //Q-par)
 (let ((//Q-save //Q)
       (funct-result '()))
  (set! //Q //Q-par)
  (let ((//B1 '())
        (//S1 '()))
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__semantic_slice__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S_save //S)
            (/__/B_save //B))
       (set! //S (vector-ref /__/Match_array 1))
       (set! //B (vector-ref /__/Match_array 0))
       ; Use Q to simplify the body of the first loop 
       (@Edit)
       (@New_Program (@Make //T_/Statements '() //S))
       (@Down)
       (@UA_Process //Q)
       (set! //S1 (@Cs (@Program)))
       (@Undo_Edit)
       (set! //B1 (@And //Q //B))
       ; Don't unroll if S1 cannot make Q true: 
       (cond
        ((null? (intersection-n (@Used //Q) (@Assigned (@Make //T_/Statements '() //S1))))
         (set! //Q '()))
        (#t
         (@Paste_Before (@Make 141 '() (list //B1 (@Make 17 '() //S1))))))
       (set! //S /__/S_save)
       (set! //B /__/B_save)))
     (#t
      (@Print_WSL (@I) "")
      (error "@Entire_Loop_Unroll: WHILE loop not found!!!")))))
  (set! funct-result //Q)
  (set! //Q //Q-save)
  funct-result))

; Insert an assertion into the loop body and use it to simplify the loop: 
(define (@Make_And_Use_Assertion)
 (@Trans //T/R_/Insert_/Assertion "")
 (cond
  ((@Trans? //T/R_/Use_/Assertion)
   (let ((/posn-save /posn))
    (set! /posn (@Posn))
    (@Trans //T/R_/Use_/Assertion "")
    (@Goto /posn)
    (cond
     ((= (@ST (@I)) //T_/Assert)
      (@Clever_Delete)))
    (set! /posn /posn-save))))
 (@Up)
 (@Up))

#t
