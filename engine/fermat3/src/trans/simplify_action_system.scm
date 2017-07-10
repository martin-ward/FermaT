;;; Scheme translation of WSL code
(define (/foreach-simplify_action_system-1 //Depth //A/S_/Type)
 (@Down)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (cond
   ((and (@Gen_Improper? (@I) //A/S_/Type) (@Right?))
    (@Delete_Rest)
    (display-list-flush "x")
    (set! /fl_flag1 1))
   ((@Right?)
    (@Right)
    (set! /fl_flag1 0))
   (#t
    (set! /fl_flag1 1)))))

(define (/foreach-simplify_action_system-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Cond) (member //T_/Call (@Stat_Types (@I))))
   (@Trans //T/R_/Simplify_/Item ""))))

(define (/foreach-simplify_action_system-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (< (@V (@I)) 0) (<= (- (@V (@I))) //N))
   (set! /n (- (@V (@I))))
   (cond
    ((= (wsl-ref //Replace /n) 0)
     (display-list "")
     (display-list "Found a call to a non-existant action: " (@N_String (wsl-ref //Names /n)) "(" /n ")")
     (display-list "In the body of action: " (@N_String (wsl-ref //Names //J)) "(" //J ")")
     (@Paste_Over (@Make //T_/Comment (string-append " Bad call: " (@N_String (wsl-ref //Names /n))) '())))
    ((equal? (wsl-ref //Replace /n) (- 1))
     (display-list-flush ".")
     (set! //I /n)
     (@Edit)
     (@New_Program (wsl-ref //Bodies //I))
     (wsl-set! //Replace 0 //I)
     (wsl-set! //Bodies '() //I)
     (@SAS_Unfold_Once_Called //I)
     (set! /body (@Program))
     (@Undo_Edit)
     (@Splice_Over (@Cs /body))
     ; The actions which were called by I are now called by J 
     ; So increase the call frequency of each sucessor of I in J by one. 
     ; Note that we don't care what I used to call, or who used to call I 
     (wsl-set! //Succs (@Set_Difference (union-n (wsl-ref //Succs //J) (wsl-ref //Succs //I)) (list //I)) //J)
     (@SAS_New_Call_Freq //I //J 0)
     (while (not (null? (wsl-ref //Succs //I))) 
      (begin
       (set! //H (car (wsl-ref //Succs //I)))
       (wsl-set! //Succs (cdr (wsl-ref //Succs //I)) //I)
       (@SAS_Update_Call_Freq //H //J (@SAS_Call_Freq //H //I))
       (cond
        ((< //H (+ //N 1))
         (wsl-set! //Preds (@Set_Difference (union-n (wsl-ref //Preds //H) (list //J)) (list //I)) //H)
         #t)))))))))

(define (/foreach-simplify_action_system-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (equal? (- (@V (@I))) //I))
   (set! /calls (+ /calls 1))
   (@Splice_Over (@Cs (wsl-ref //Bodies //I))))))

(define (/foreach-simplify_action_system-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (equal? (- (@V (@I))) //I))
   (@Paste_Over (@Make //T_/Exit (+ //Depth 1) '())))))

(define (/foreach-simplify_action_system-6 //Depth //A/S_/Type)
 (let ((//S/T (my-map @ST (@Cs (@I)))))
  ;Delete code which follows calls, either directly, or by absorbing
  ;statement sequences containing calls into IF statements which
  ;contain calls and then deleting.
  (cond
   ((and (member //T_/Call //S/T) (member //T_/Cond //S/T))
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      ((@Gen_Improper? (@I) //A/S_/Type)
       (@Delete_Rest)
       (set! /fl_flag1 1))
      (#t
       (cond
        ((and (= (@ST (@I)) //T_/Cond) (member //T_/Call (@Stat_Types (@I))))
         (@Merge_Calls_In_Cond)))
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0))))))))))

(define (/foreach-simplify_action_system-7 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Cond) (member //T_/Call (@Stat_Types (@I))))
   (cond
    ((and (@Trans? //T/R_/Separate_/Right) (or (not (= (@ST (last-1 (@Cs (@Get_n (@Get_n (@I) 1) 2)))) //T_/Call)) (not-member (- (@V (last-1 (@Cs (@Get_n (@Get_n (@I) 1) 2))))) //Leave_/Alone)))
     (@Trans //T/R_/Separate_/Right "")))
   (cond
    ((@Trans? //T/R_/Delete_/All_/Assertions)
     (@Trans //T/R_/Delete_/All_/Assertions "")))
   (cond
    ((@Trans? //T/R_/Else_/If_/To_/Elsif)
     (@Trans //T/R_/Else_/If_/To_/Elsif "")))
   (cond
    ((and (equal? //A/S_/Type "Reg") (@Gen_Improper? (@I) //A/S_/Type) (<= (@Size (@I)) //Max_/Cond_/Size))
     (@SAS_Take_Out_Dup_Calls))))))

(define (/foreach-simplify_action_system-8 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (- (@V (@I))) /name) (@Gen_Reachable? (@Program) (@Posn) //A/S_/Type))
   (set! //Valid (concat //Valid (list (@Posn)))))))

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
(define (@Simplify_Action_System_Test)
 (cond
  ((= (@ST (@I)) //T_/A_/S)
   (@Pass))
  (#t
   (@Fail "Not an action system"))))

(define (@Simplify_Action_System_Code //Data)
 (@Edit)
 (let ((//Old_/Size 0)
       (//Size (@Size (@Get_n (@I) 2)))
       (//Leave_/Alone_/Names (@Parse_String //Data))
       (//A/S_/Type-save //A/S_/Type))
  (set! //A/S_/Type (@System_Type (@I)))
  (display-list //Size " actions, type = " //A/S_/Type)
  (display-list-flush "Deleting unreachable code: ")
  (@Foreach_Stats /foreach-simplify_action_system-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (display-list "")
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    (let ((//N-save //N))
     (set! //N (@Size (@Get_n (@I) 2)))
     (let ((//A/S_/Name (@V (@Get_n (@I) 1)))
           (//A/S_/Type-save //A/S_/Type)
           (//Starting_/Action-save //Starting_/Action)
           (//N_/Actions 0)
           (/n-save /n)
           (//Bodies-save //Bodies)
           (//Replace-save //Replace)
           (//Changed-save //Changed)
           (//Names-save //Names)
           (//Succs-save //Succs)
           (//Preds-save //Preds)
           (//Name2/Num (hash-table))
           (//Num_/Calls-save //Num_/Calls)
           (//Leave_/Alone-save //Leave_/Alone)
           (/name-save /name)
           (/i 0)
           (//Actions '())
           (/dispatch (@Make_Name "dispatch"))
           (/dn-save /dn))
      (set! //A/S_/Type '())
      (set! //Starting_/Action 0)
      (set! /n 0)
      (set! //Bodies (make-vector-eval //N '()))
      (set! //Replace (make-vector-eval (+ //N 1) 0))
      (set! //Changed (make-vector-eval (+ //N 1) 1))
      (set! //Names (make-vector-eval (+ //N 1) '()))
      (set! //Succs (make-vector-eval (+ //N 1) '()))
      (set! //Preds (make-vector-eval (+ //N 1) '()))
      (set! //Num_/Calls (hash-table))
      (set! //Leave_/Alone '())
      (set! /name '())
      (set! /dn (- 1))
      (set! //A/S_/Type (@System_Type (@I)))
      (display-list "Action system type is " //A/S_/Type)
      (display-list "Leave_Alone_Names = " (@Join ", " (my-map @N_String //Leave_/Alone_/Names)))
      ; Calculate Bodies, Names, Name2Num 
      ; Hash table Name2Num maps action names (keys) to action numbers   
      (let ((/-result- (@FD_Init  //N //Bodies //Names //Name2/Num)))
       (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
       (set! //Names (car /-result-)) (set! /-result- (cdr /-result-))
       (set! //Name2/Num (car /-result-)) (set! /-result- (cdr /-result-)))
      (set! //Leave_/Alone '())
      (for-in /name //Leave_/Alone_/Names 
       (begin
        (cond
         ((equal? /name /dispatch)
          (set! /dn (gethash //Name2/Num /name))))
        (set! //Leave_/Alone (cons (gethash //Name2/Num /name) //Leave_/Alone))))
      (set! //Leave_/Alone (@Make_Set //Leave_/Alone))
      ; Find the starting action: 
      (set! //Starting_/Action (gethash //Name2/Num //A/S_/Name))
      (for /i 1 (+ //N 1) 1 
       (wsl-set! //Replace /i /i))
      ;Simplify all IF statements which contain calls
      (display-list-flush "Simplifying conditional statements: ")
      (for /i 1 //N 1 
       (cond
        ((and (not (equal? /i /dn)) (> (wsl-ref //Replace /i) 0) (member //T_/Cond (@Stat_Types (wsl-ref //Bodies /i))))
         (@New_Program (wsl-ref //Bodies /i))
         (@Foreach_Non_Action_Statement /foreach-simplify_action_system-2 0 (@AS_Type) 0)
         (cond
          ((null? (@Program))
           (@New_Program (@Skips))))
         (wsl-set! //Bodies (@Program) /i))))
      (display-list "")
      (let ((/-result- (@SAS_Succs_And_Preds  //N //Bodies //Starting_/Action //Succs //Preds //Num_/Calls)))
       (set! //Succs (car /-result-)) (set! /-result- (cdr /-result-))
       (set! //Preds (car /-result-)) (set! /-result- (cdr /-result-))
       (set! //Num_/Calls (car /-result-)) (set! /-result- (cdr /-result-)))
      (@Substitute_Once_Called //Leave_/Alone /dn)
      (@Remove_Elementary //Leave_/Alone /dn)
      ; Loop until no more actions can be removed. 
      ; How many actions are left ?
      (set! //N_/Actions 0)
      (for //I 1 //N 1 
       (cond
        ((> (wsl-ref //Replace //I) 0)
         (set! //N_/Actions (+ //N_/Actions 1)))))
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        ; Recalculate action system type 
        (set! //A/S_/Type (@SAS_AS_Type))
        (display-list "")
        (display-list "Action system type is " //A/S_/Type)
        (display-list //N_/Actions " actions")
        ; Simplify action bodies to merge calls and remove recursion. 
        (display-list "")
        (display-list-flush "Simplifying action bodies: ")
        (for //I 1 //N 1 
         (cond
          ((> (wsl-ref //Replace //I) 0)
           (cond
            ((and (> (@SAS_Call_Freq //I //I) 0) (not (equal? //I /dn)))
             (@Remove_Recursion //I (wsl-ref //Names //I) //A/S_/Type)
             (wsl-set! //Changed 1 //I)))
           (cond
            ((and (not (equal? //I /dn)) (= (wsl-ref //Changed //I) 1))
             (@Merge_Calls //I)
             (wsl-set! //Changed 0 //I))))))
        (display-list "")
        (cond
         ((= //N_/Actions 1)
          (set! /fl_flag1 1))
         (#t
          (@Substitute_Once_Called //Leave_/Alone /dn)
          ; If any actions have now become unreachable 
          ; then mark them and remove their bodies. 
          (for //I 1 //N 1 
           (cond
            ((and (not (equal? //I //Starting_/Action)) (> (wsl-ref //Replace //I) 0) (or (null? (wsl-ref //Preds //I)) (and (= (gen-length (wsl-ref //Preds //I)) 1) (equal? (car (wsl-ref //Preds //I)) //I))))
             (wsl-set! //Replace 0 //I)
             (wsl-set! //Bodies '() //I))))
          ; If no actions were removed in this pass then exit.     
          (set! /n 0)
          (for //I 1 //N 1 
           (cond
            ((> (wsl-ref //Replace //I) 0)
             (set! /n (+ /n 1)))))
          (cond
           ((equal? /n //N_/Actions)
            (set! /fl_flag1 1))
           (#t
            (set! //N_/Actions /n)
            (set! /fl_flag1 0)))))))
      (@FD_Rebuild_AS //N //Bodies //Names //A/S_/Name '())
      (cond
       ((@Syntax_OK? (@Program))
        (display-list "Syntax is OK."))
       (#t
        (display-list "Syntax NOT OK!")))
      (set! //A/S_/Type //A/S_/Type-save)
      (set! //Starting_/Action //Starting_/Action-save)
      (set! /n /n-save)
      (set! //Bodies //Bodies-save)
      (set! //Replace //Replace-save)
      (set! //Changed //Changed-save)
      (set! //Names //Names-save)
      (set! //Succs //Succs-save)
      (set! //Preds //Preds-save)
      (set! //Num_/Calls //Num_/Calls-save)
      (set! //Leave_/Alone //Leave_/Alone-save)
      (set! /name /name-save)
      (set! /dn /dn-save))
     (set! //N //N-save))
    (set! //Old_/Size //Size)
    (set! //Size (@Size (@Get_n (@I) 2)))
    (cond
     ((or (equal? //Old_/Size //Size) (<= //Size 1))
      (set! /fl_flag2 1))
     (#t
      (display-list "")
      (display-list "REITERATING...")
      (set! /fl_flag2 0)))))
  (set! //A/S_/Type //A/S_/Type-save))
 (@End_Edit)
 (cond
  ((and (equal? (@System_Type (@I)) "Rec") (> (@Size (@Get_n (@I) 2)) 1))
   (let ((/calls-save /calls)
         (/name-save /name))
    (set! /calls '())
    (set! /name (@V (@Get_n (@I) 1)))
    ; Convert suitable actions to procs 
    (@Down_To 2)
    (@Down)
    ; to first action 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((equal? (@V (@Get_n (@I) 1)) /name)
        ; starting action 
       )
       ((< (@Stat_Count (@I)) 2)
        ; too small 
       )
       (#t
        (set! /calls (@Calls (@I)))
        (cond
         ((or (= (gen-length /calls) 0) (and (= (gen-length /calls) 1) (equal? (@V (@Get_n (@I) 1)) (wsl-ref (wsl-ref /calls 1) 1))))
          (@Trans //T/R_/Make_/Proc "")))))
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (set! /calls /calls-save)
    (set! /name /name-save))))
 (display-list "Done"))

(define (@Parse_String //S)
 (let ((//L '())
       (//I-save //I)
       (funct-result '()))
  (set! //I '())
  (cond
   ((not (equal? //S ""))
    (set! //I (my-index " " //S 0))
    (while (>= //I 0) 
     (begin
      (set! //L (cons (@Make_Name (substr //S 0 //I)) //L))
      (set! //S (substr //S (+ //I 1)))
      (set! //I (my-index " " //S 0))))
    (set! //L (cons (@Make_Name //S) //L))))
  (set! funct-result //L)
  (set! //I //I-save)
  funct-result))

(define (@Substitute_Once_Called //Leave_/Alone-par /dn-par)
 (let ((/dn-save /dn)
       (//Leave_/Alone-save //Leave_/Alone))
  (set! /dn /dn-par)
  (set! //Leave_/Alone //Leave_/Alone-par)
  (display-list-flush "Eliminating actions which are only called once: ")
  ; First find and mark the `once called' actions, then unfold them. 
  ; Before unfolding an action body, recursively unfold all the once called actions. 
  (let ((//J-save //J)
        (//I-save //I)
        (//H-save //H))
   (set! //J 0)
   (set! //I 0)
   (set! //H 0)
   (for //I 1 //N 1 
    (cond
     ((and (not (equal? //I //Starting_/Action)) (> (wsl-ref //Replace //I) 0) (= (gen-length (wsl-ref //Preds //I)) 0))
      (wsl-set! //Replace 0 //I)
      (wsl-set! //Bodies '() //I))
     ((and (not (equal? //I //Starting_/Action)) (not-member //I //Leave_/Alone) (> (wsl-ref //Replace //I) 0) (= (gen-length (wsl-ref //Preds //I)) 1) (not (equal? (car (wsl-ref //Preds //I)) //I)) (= (@SAS_Call_Freq //I (car (wsl-ref //Preds //I))) 1) (not (equal? (car (wsl-ref //Preds //I)) /dn)))
      (wsl-set! //Replace (- 1) //I))))
   (for //J 1 //N 1 
    (cond
     ((> (wsl-ref //Replace //J) 0)
      (@New_Program (wsl-ref //Bodies //J))
      (@SAS_Unfold_Once_Called //J)
      (wsl-set! //Bodies (@Program) //J)
      ; If action is now recursive then remove recursion
      (cond
       ((and (> (@SAS_Call_Freq //J //J) 0) (not (equal? //J /dn)))
        (display-list "")
        (@Remove_Recursion //J (wsl-ref //Names //J) //A/S_/Type))))))
   (set! //J //J-save)
   (set! //I //I-save)
   (set! //H //H-save))
  (display-list "")
  (set! /dn /dn-save)
  (set! //Leave_/Alone //Leave_/Alone-save)))

; Check all the calls in the program, unfold calls to once called actions: 
(define (@SAS_Unfold_Once_Called //J-par)
 (let ((//J-save //J))
  (set! //J //J-par)
  (let ((/body-save /body)
        (//I-save //I)
        (/n-save /n))
   (set! /body '())
   (set! //I 0)
   (set! /n 0)
   (@Foreach_Non_Action_Statement /foreach-simplify_action_system-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /body /body-save)
   (set! //I //I-save)
   (set! /n /n-save))
  (set! //J //J-save)))

(define (@Remove_Elementary //Leave_/Alone-par /dn-par)
 (let ((/dn-save /dn)
       (//Leave_/Alone-save //Leave_/Alone))
  (set! /dn /dn-par)
  (set! //Leave_/Alone //Leave_/Alone-par)
  (display-list "Finding elementary actions")
  (for //J 1 //N 1 
   (cond
    ((and (not (equal? //J //Starting_/Action)) (not (member //J //Leave_/Alone)) (> (wsl-ref //Replace //J) 0) (= (gen-length (wsl-ref //Succs //J)) 1) (not (equal? (car (wsl-ref //Succs //J)) //J)) (= (@SAS_Call_Freq (car (wsl-ref //Succs //J)) //J) 1) (<= (@Total_Size (wsl-ref //Bodies //J)) 10) (not-member /dn (wsl-ref //Preds //J)) (@Set_Subset? (@Stat_Types (wsl-ref //Bodies //J)) (@Make_Set (list //T_/Call //T_/Comment //T_/Skip))))
     (wsl-set! //Replace (car (wsl-ref //Succs //J)) //J))))
  (display-list-flush "Removing elementary actions: ")
  (for //I 1 //N 1 
   (cond
    ((and (> (wsl-ref //Replace //I) 0) (not (equal? (wsl-ref //Replace //I) //I)))
     ;Action #I is elementary
     (display-list-flush (@N_String (wsl-ref //Names //I)) "=" (wsl-ref //Replace //I) " ")
     (@SAS_Replace_Elem_Action_Calls //I //Leave_/Alone /dn))))
  (display-list "")
  (set! /dn /dn-save)
  (set! //Leave_/Alone //Leave_/Alone-save)))

(define (@SAS_Replace_Elem_Action_Calls //I-par //Leave_/Alone-par /dn-par)
 (let ((/dn-save /dn)
       (//Leave_/Alone-save //Leave_/Alone)
       (//I-save //I))
  (set! /dn /dn-par)
  (set! //Leave_/Alone //Leave_/Alone-par)
  (set! //I //I-par)
  (let ((//S (car (wsl-ref //Succs //I)))
        (//P (wsl-ref //Preds //I))
        (//H-save //H))
   (set! //H '())
   (wsl-set! //Replace 0 //I)
   ;Signifies that the action has been / is being dealt with
   ;If successor is elementary then deal with it first
   (cond
    ((and (not (null? (wsl-ref //Succs //I))) (> (wsl-ref //Replace //S) 0) (not (equal? (wsl-ref //Replace //S) //S)))
     (@SAS_Replace_Elem_Action_Calls //S //Leave_/Alone /dn)))
   ; Update the predecessors list of the (single) action called by this action. 
   ; In the action's predecessors, expand calls to this action and 
   ; update successors list accordingly. 
   (cond
    ((not (null? (wsl-ref //Succs //I)))
     (wsl-set! //Preds (@Set_Difference (union-n (wsl-ref //Preds //S) //P) (list //I)) //S)))
   (while (not (null? //P)) 
    (begin
     (set! //H (car //P))
     (set! //P (cdr //P))
     (wsl-set! //Succs (@Set_Difference (union-n (wsl-ref //Succs //H) (wsl-ref //Succs //I)) (list //I)) //H)
     ; H now calls HEAD(Succs[I]) more often: 
     (let ((/calls-save /calls))
      (set! /calls 0)
      (set! /calls (@SAS_Expand_Calls_In_Body  //I //H //Leave_/Alone /dn /calls))
      (cond
       ((not (null? (wsl-ref //Succs //I)))
        (@SAS_Update_Call_Freq (car (wsl-ref //Succs //I)) //H /calls)))
      (set! /calls /calls-save))
     (wsl-set! //Changed 1 //H)))
   (wsl-set! //Bodies '() //I)
   (set! //H //H-save))
  (set! /dn /dn-save)
  (set! //Leave_/Alone //Leave_/Alone-save)
  (set! //I //I-save)))

(define (@SAS_Expand_Calls_In_Body //I-par //H-par //Leave_/Alone-par /dn-par /calls-par)
 (let ((/calls-save /calls)
       (/dn-save /dn)
       (//Leave_/Alone-save //Leave_/Alone)
       (//H-save //H)
       (//I-save //I)
       (funct-result '()))
  (set! /calls /calls-par)
  (set! /dn /dn-par)
  (set! //Leave_/Alone //Leave_/Alone-par)
  (set! //H //H-par)
  (set! //I //I-par)
  ; Unfold calls to action number I in the body of action number H 
  (cond
   ((not (null? (wsl-ref //Bodies //H)))
    (@New_Program (wsl-ref //Bodies //H))
    (@Foreach_Non_Action_Statement /foreach-simplify_action_system-4 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (wsl-set! //Bodies (@Program) //H)
    ;If the action has become recursive as a result,  
    ; remove the recursion immediately
    (cond
     ((and (not (equal? //H /dn)) (member //H (wsl-ref //Succs //H)))
      (display-list "")
      (@Remove_Recursion //H (wsl-ref //Names //H) //A/S_/Type)
      (display-list "")))))
  (set! funct-result /calls)
  (set! /calls /calls-save)
  (set! /dn /dn-save)
  (set! //Leave_/Alone //Leave_/Alone-save)
  (set! //H //H-save)
  (set! //I //I-save)
  funct-result))

; Remove the recursion in Bodies[I]: 
(define (@Remove_Recursion //I-par /name-par //A/S_/Type-par)
 (let ((//A/S_/Type-save //A/S_/Type)
       (/name-save /name)
       (//I-save //I))
  (set! //A/S_/Type //A/S_/Type-par)
  (set! /name /name-par)
  (set! //I //I-par)
  (let ((//Calls '())
        (//O/K 0)
        (//Message ""))
   (@New_Program (wsl-ref //Bodies //I))
   (cond
    ((or (equal? //A/S_/Type "Reg") (@SAS_Calls_Terminal? /name //A/S_/Type))
     ; Turn recursive body into loop 
     (display-list "Removing recursion in action " (@N_String (wsl-ref //Names //I)) "(" //I "), dn = " /dn)
     (@Paste_Over (@Increment (@I) //A/S_/Type 2 0))
     (@Foreach_Non_Action_Statement /foreach-simplify_action_system-5 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (let ((//S (@Cs (@I))))
      (@Paste_Over (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() //S)))))))))))
     ; Update the successor and predecessor lists for the action 
     (wsl-set! //Succs (@Set_Difference (wsl-ref //Succs //I) (list //I)) //I)
     (wsl-set! //Preds (@Set_Difference (wsl-ref //Preds //I) (list //I)) //I)
     (@SAS_New_Call_Freq //I //I 0)
     ;  Simplify the double loop if possible                             
     ;Down to outer DO loop
     (@Down)
     (cond
      ((@Trans? //T/R_/Delete_/All_/Skips)
       (@Trans //T/R_/Delete_/All_/Skips "")))
     (@Trans //T/R_/Delete_/Unreachable_/Code "")
     (let ((/-result- (@SR_Floop_Test  //A/S_/Type //O/K //Message)))
      (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-))
      (set! //Message (car /-result-)) (set! /-result- (cdr /-result-)))
     (cond
      ((= //O/K 1)
       (@SR_Floop //A/S_/Type)))
     (cond
      ((@Trans? //T/R_/Double_/To_/Single_/Loop)
       (@Trans //T/R_/Double_/To_/Single_/Loop "")))
     (cond
      ((@Gen_Dummy? (@I) //A/S_/Type)
       (@Splice_Over (@Cs (@Increment (@Get_n (@I) 1) //A/S_/Type (- 1) 0)))))
     (wsl-set! //Bodies (@Program) //I))))
  (set! //A/S_/Type //A/S_/Type-save)
  (set! /name /name-save)
  (set! //I //I-save)))

(define (@SAS_AS_Type)
 (let ((//J-save //J)
       (//Type "Reg")
       (//Rec 1)
       (funct-result '()))
  (set! //J 1)
  (for //J 1 //N 1 
   (cond
    ((and (> (wsl-ref //Replace //J) 0) (not (null? (wsl-ref //Bodies //J))))
     (cond
      ((null? (@Gen_TVs (wsl-ref //Bodies //J) "Reg"))
       (display-list "Action " (@N_String (wsl-ref //Names //J)) " is a non-terminating loop.")
       (display-list "Converting to a CALL Z.")
       (wsl-set! //Bodies (@Make //T_/Statements '() (list (@Make //T_/Call (- (+ //N 1)) '()))) //J)))
     (cond
      ((not (@Regular? (wsl-ref //Bodies //J)))
       (display-list "Action " (@N_String (wsl-ref //Names //J)) " is non regular")
       (set! //Type "Hyb")))
     (cond
      ((member (+ //N 1) (wsl-ref //Succs //J))
       (set! //Rec 0))))))
  (cond
   ((= //Rec 1)
    (set! //Type "Rec")))
  (set! funct-result //Type)
  (set! //J //J-save)
  funct-result))

(define (@Merge_Calls //I-par)
 (let ((//I-save //I))
  (set! //I //I-par)
  (let ((//Calls (@Calls (wsl-ref //Bodies //I)))
        (//P '())
        (//New_/Succs '())
        (//S/S '()))
   (cond
    ((or (> (gen-length //Calls) 1) (and (= (gen-length //Calls) 1) (> (wsl-ref (wsl-ref //Calls 1) 2) 1)))
     (display-list-flush ".")
     (@New_Program (@Make //T_/Action '() (list (@Name (wsl-ref //Names //I)) (wsl-ref //Bodies //I))))
     (cond
      ((equal? //A/S_/Type "Reg")
       (@Foreach_Stats /foreach-simplify_action_system-6 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))))
     ;For each IF statement, see if it is possible to take out duplicated
     ;calls from the end of two or more branches, thus merging them.
     (@Foreach_Non_Action_Statement /foreach-simplify_action_system-7 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (cond
      ((not (@Equal? (wsl-ref //Bodies //I) (@Get_n (@Program) 2)))
       (wsl-set! //Bodies (@Get_n (@Program) 2) //I)
       ; If the action now calls different actions than before, 
       ; then update its successors list, and the predecessors list 
       ; of those actions which are no longer called. 
       (set! //New_/Succs '())
       (for-in /call (@Calls (@Program)) 
        (begin
         (set! //New_/Succs (cons (- (wsl-ref /call 1)) //New_/Succs))
         (@SAS_New_Call_Freq (- (wsl-ref /call 1)) //I (wsl-ref /call 2))))
       (set! //New_/Succs (@Make_Set //New_/Succs))
       (set! //S/S (@Set_Difference (wsl-ref //Succs //I) //New_/Succs))
       ; SS is the list of actions which used to be called, but no longer are. 
       (wsl-set! //Succs //New_/Succs //I)
       (while (not (null? //S/S)) 
        (begin
         (wsl-set! //Preds (@Set_Difference (wsl-ref //Preds (car //S/S)) (list //I)) (car //S/S))
         (set! //S/S (cdr //S/S)))))))))
  (set! //I //I-save)))

; Check for two or more calls to the same action at the ends of the arms 
; of the IF statement which is currently selected. 
(define (@SAS_Take_Out_Dup_Calls)
 (let ((/n-save /n))
  (set! /n 0)
  (for-in /el (@Calls (@I)) 
   (cond
    ((and (not-member (- (wsl-ref /el 1)) //Leave_/Alone) (> (wsl-ref /el 2) /n))
     (set! /n (wsl-ref /el 2)))))
  (cond
   ((> /n 1)
    ; Something is called more than once 
    ; Check for two or more calls at the ends of the arms: 
    (let ((/freq (hash-table))
          (//Target '()))
     (set! /n 1)
     (@Down)
     ; to first guarded 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (@Down_To 2)
       (@Down_Last)
       (cond
        ((and (= (@ST (@I)) //T_/Call) (not-member (- (@V (@I))) //Leave_/Alone))
         (cond
          ((null? (gethash /freq (@V (@I))))
           (puthash /freq (@V (@I)) 1))
          (#t
           (puthash /freq (@V (@I)) (+ (gethash /freq (@V (@I))) 1))))
         (cond
          ((> (gethash /freq (@V (@I))) /n)
           (set! //Target (@V (@I)))
           (set! /n (gethash /freq (@V (@I))))))))
       (@Up)
       (@Up)
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))
     (@Up)
     ; to IF statement 
     (cond
      ((not (null? //Target))
       ;  
       (@Down)
       ; to first guarded
       (set! /fl_flag1 0)
       (while (= /fl_flag1 0) 
        (begin
         (@Down_To 2)
         (@Down_Last)
         (cond
          ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) //Target))
           (@Paste_Over (@Make //T_/Skip '() '()))))
         (@Up)
         (@Up)
         ; up to guarded
         (cond
          ((not (@Right?))
           (set! /fl_flag1 1))
          (#t
           (@Right)
           (set! /fl_flag1 0)))))
       (@Up)
       ; to IF statement
       (@Splice_After (list (@Make //T_/Call //Target '()))))))))
  (set! /n /n-save)))

; A Cond is selected which contains calls. Try to merge calls by absorption 
(define (@Merge_Calls_In_Cond)
 (let ((//P (@Posn)))
  (@Right)
  ;Look for a call following this IF statement
  (while (and (not (@Cs? (@I))) (not (= (@ST (@I)) //T_/Call)) (@Right?)) 
   (@Right))
  (cond
   ((not (= (@ST (@I)) //T_/Call))
    (@Goto //P))
   (#t
    ;Absorb the statements up to this call
    (@Goto //P)
    (while (not (= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Call)) 
     (@Trans //T/R_/Absorb_/Right ""))
    ;Now absorb the call itself
    (@Trans //T/R_/Absorb_/Right "")
    ;Now delete after each call in the IF statement
    (@Down)
    ; to guarded
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (@Down_Last)
      ; down to statement sequence
      (cond
       ((member //T_/Call (my-map @ST (@Cs (@I))))
        (@Down)
        (while (and (not (= (@ST (@I)) //T_/Call)) (@Right?)) 
         (@Right))
        (cond
         ((= (@ST (@I)) //T_/Call)
          (@Delete_Rest)))
        (@Up)))
      (@Up)
      ; to guarded
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)))))

; Find the next statement with the given type and value. 
; Useful for processing action calls and proc calls. 
(define (@Find_Statement /type /value)
 (set! /fl_flag2 0)
 (while (= /fl_flag2 0) 
  (begin
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((not (or (= (@Gen_Type (@I)) //T_/Expression) (= (@Gen_Type (@I)) //T_/Expressions) (= (@Gen_Type (@I)) //T_/Condition)))
      (set! /fl_flag1 1))
     ((@Right?)
      (@Right)
      (set! /fl_flag1 0))
     ((@Up?)
      (@Up)
      (while (and (@Up?) (not (@Right?))) 
       (@Up))
      (cond
       ((@Right?)
        (@Right)
        (set! /fl_flag1 0))
       (#t
        (set! /fl_flag1 2))))
     (#t
      (set! /fl_flag1 2))))
   (cond
    ((= /fl_flag1 2)
     (set! /fl_flag2 1))
    ((and (= (@ST (@I)) /type) (equal? (@V (@I)) /value))
     (set! /fl_flag2 1))
    ((and (= (@ST (@I)) /type) (> (@Size (@I)) 1) (= (@ST (@Get_n (@I) 1)) //T_/Name) (equal? (@V (@Get_n (@I) 1)) /value))
     (set! /fl_flag2 1))
    ((and (@Down?) (member /type (@Stat_Types (@I))))
     (@Down)
     (set! /fl_flag2 0))
    ((@Right?)
     (@Right)
     (set! /fl_flag2 0))
    ((@Up?)
     (@Up)
     (while (and (@Up?) (not (@Right?))) 
      (@Up))
     (cond
      ((not (@Right?))
       (set! /fl_flag2 1))
      (#t
       (@Right)
       (set! /fl_flag2 0))))
    (#t
     (set! /fl_flag2 1))))))

; Calculate Succs and Preds arrays and Num_Calls hash table. 
; NB: the calls in Bodies have numbers instead of names, 
;     Succs and Preds are arrays of < > and Num_Calls is empty 
(define (@SAS_Succs_And_Preds //N-par //Bodies-par //Starting_/Action-par //Succs-par //Preds-par //Num_/Calls-par)
 (let ((//Num_/Calls-save //Num_/Calls)
       (//Preds-save //Preds)
       (//Succs-save //Succs)
       (//Starting_/Action-save //Starting_/Action)
       (//Bodies-save //Bodies)
       (//N-save //N)
       (funct-result '()))
  (set! //Num_/Calls //Num_/Calls-par)
  (set! //Preds //Preds-par)
  (set! //Succs //Succs-par)
  (set! //Starting_/Action //Starting_/Action-par)
  (set! //Bodies //Bodies-par)
  (set! //N //N-par)
  (display-list-flush "Calculating Succs and Preds... ")
  (let ((/calls-save /calls)
        (//J-save //J)
        (/n-save /n))
   (set! /calls '())
   (set! //J 0)
   (set! /n 0)
   (for //I 1 //N 1 
    (cond
     ((not (null? (wsl-ref //Bodies //I)))
      (set! /calls (@Calls (wsl-ref //Bodies //I)))
      ; Returns a list of <call, number> pairs 
      (while (not (null? /calls)) 
       (begin
        (set! //J (- (wsl-ref (wsl-ref /calls 1) 1)))
        (set! /n (wsl-ref (wsl-ref /calls 1) 2))
        (set! /calls (cdr /calls))
        (cond
         ((not (number? //J))
          (display-list "Check me: " //J)
          (set! //J (+ //N 1))))
        (@SAS_Update_Call_Freq //J //I /n)
        (wsl-set! //Succs (union-n (list //J) (wsl-ref //Succs //I)) //I)
        (wsl-set! //Preds (union-n (wsl-ref //Preds //J) (list //I)) //J))))))
   (set! /calls /calls-save)
   (set! //J //J-save)
   (set! /n /n-save))
  (display-list "Done.")
  (set! funct-result (list //Succs //Preds //Num_/Calls))
  (set! //Num_/Calls //Num_/Calls-save)
  (set! //Preds //Preds-save)
  (set! //Succs //Succs-save)
  (set! //Starting_/Action //Starting_/Action-save)
  (set! //Bodies //Bodies-save)
  (set! //N //N-save)
  funct-result))

(define (@SAS_Calls_Terminal? /name-par //A/S_/Type-par)
 (let ((//A/S_/Type-save //A/S_/Type)
       (/name-save /name)
       (//Valid-save //Valid)
       (//R 0)
       (funct-result '()))
  (set! //A/S_/Type //A/S_/Type-par)
  (set! /name /name-par)
  (set! //Valid (@SAS_Reachable_Calls /name-par //A/S_/Type-par))
  (while (and (not (null? //Valid)) (= //R 1)) 
   (begin
    (cond
     ((not (@Is_Terminal_Posn? (car //Valid)))
      (set! //R 0)))
    (set! //Valid (cdr //Valid))))
  (set! funct-result (= //R 1))
  (set! //A/S_/Type //A/S_/Type-save)
  (set! /name /name-save)
  (set! //Valid //Valid-save)
  funct-result))

(define (@SAS_Reachable_Calls /name-par //A/S_/Type-par)
 (let ((//A/S_/Type-save //A/S_/Type)
       (/name-save /name)
       (//Calls '())
       (//Valid-save //Valid)
       (funct-result '()))
  (set! //A/S_/Type //A/S_/Type-par)
  (set! /name /name-par)
  (set! //Valid '())
  (@Edit)
  (@Ateach_Statement /foreach-simplify_action_system-8 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Undo_Edit)
  (set! funct-result //Valid)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! /name /name-save)
  (set! //Valid //Valid-save)
  funct-result))

; Return the call frequency from the Num_Calls hash table, 
; ie the number of times J is called in the body of I 
(define (@SAS_Call_Freq //J //I)
 (let ((//R (gethash //Num_/Calls (list //J //I))))
  (cond
   ((null? //R)
    (set! //R 0)))
  //R))

; Update the call frequency in the Num_Calls hash table, 
; ie the number of times J is called in the body of I, 
; by the given amount 
(define (@SAS_Update_Call_Freq //J //I /increment)
 (let ((/old (gethash //Num_/Calls (list //J //I))))
  (cond
   ((null? /old)
    (puthash //Num_/Calls (list //J //I) /increment))
   (#t
    (puthash //Num_/Calls (list //J //I) (+ /old /increment))))))

; Record a new call frequency -- number of calls of J in I 
(define (@SAS_New_Call_Freq //J //I /new)
 (puthash //Num_/Calls (list //J //I) /new)
 #t)

#t
