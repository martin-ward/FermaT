;;; Scheme translation of WSL code
(define (/foreach-merge_calls_in_system-1 //Depth //A/S_/Type)
 (set! /rest (cdr (@Cs (@I))))
 (@Down)
 (set! /fl_flag2 0)
 (while (= /fl_flag2 0) 
  (begin
   ; Find the next IF statement 
   (while (and (or (not (= //T_/Cond (@ST (@I)))) (<= //Max_/Cond_/Size (@Size (@I))) (not (@Right?))) (@Right?)) 
    (begin
     (set! /rest (cdr /rest))
     (@Right)))
   (cond
    ((and (or (not (= //T_/Cond (@ST (@I)))) (<= //Max_/Cond_/Size (@Size (@I))) (not (@Right?))) (not (@Right?)))
     (set! /fl_flag2 1))
    (#t
     (set! /fl_flag2 0)))
   (cond
    ((= /fl_flag2 0)
     ; Check that remaining statements in the sequence have multi-calls 
     ; and that a CALL in the IF also appears in the rest of the sequence 
     (cond
      ((and (not (null? (@Set_Difference (@Multi_Calls (@Make //T_/Statements '() (cons (@I) /rest)) 2) (list //Z)))) (not (null? (@Set_Difference (intersection-n (@Multi_Calls (@I) 1) (@Multi_Calls (@Make //T_/Statements '() /rest) 1)) (list //Z)))))
       ; If only one arm is not improper then fully expand the cond. 
       ; If all arms are improper, then delete what follows. 
       ; If the next statement is a CALL, then expand anyway. 
       (set! /n 0)
       (@Down)
       ; to first arm 
       (@Down_To 2)
       ; to statements 
       (cond
        ((not (@Gen_Improper? (@I) "Reg"))
         (set! /n (+ /n 1))))
       (@Up)
       (while (@Right?) 
        (begin
         (@Right)
         (@Down_To 2)
         ; to statements 
         (cond
          ((not (@Gen_Improper? (@I) "Reg"))
           (set! /n (+ /n 1))))
         (@Up)))
       (@Up)
       ; back to cond 
       (@Right)
       (cond
        ((= (@ST (@I)) //T_/Call)
         (set! /n 0)))
       (@Left)
       (cond
        ((and (<= /n 1) (>= (gen-length (@Posn)) 80))
         (display-list-flush " B1(" (gen-length (@Posn)) ") ")
         (set! /fl_flag2 0))
        ((<= /n 1)
         (display-list-flush "e")
         (set! /expansions (+ /expansions 1))
         ; Don't call Fully_Expand_Forwards here, since we 
         ; don't have access to the AS type at this point 
         ; -- hence code is duplicated! 
         (let ((/len (+ (gen-length (@Posn)) (@Max_Pos_L (@I)))))
          (set! /fl_flag1 0)
          (while (= /fl_flag1 0) 
           (cond
            ((not (@Right?))
             (set! /fl_flag1 1))
            (#t
             (@Right)
             (cond
              ((>= (+ /len (@Max_Pos_L (@I))) 80)
               (display-list-flush " B2(" (+ /len (@Max_Pos_L (@I))) ") ")
               (@Left)
               (set! /fl_flag1 1))
              (#t
               (@Left)
               (@Gen_Expand_Forward "Reg")
               (set! /len (+ (gen-length (@Posn)) (@Max_Pos_L (@I))))
               (set! /fl_flag1 0)))))))
         (set! /fl_flag2 1))
        (#t
         (set! /fl_flag2 0))))
      (#t
       (set! /fl_flag2 0)))
     (cond
      ((= /fl_flag2 0)
       (cond
        ((not (@Right?))
         (set! /fl_flag2 1))
        (#t
         (set! /rest (cdr /rest))
         (@Right)
         (set! /fl_flag2 0)))))))))
 (@Up))

(define (/foreach-merge_calls_in_system-2 //Depth //A/S_/Type)
 (set! /rest (cdr (@Cs (@I))))
 (@Down)
 (set! /fl_flag2 0)
 (while (= /fl_flag2 0) 
  (begin
   ; Find the next IF statement 
   (while (and (or (not (= //T_/Cond (@ST (@I)))) (<= //Max_/Cond_/Size (@Size (@I))) (not (@Right?))) (@Right?)) 
    (begin
     (set! /rest (cdr /rest))
     (@Right)))
   (cond
    ((and (or (not (= //T_/Cond (@ST (@I)))) (<= //Max_/Cond_/Size (@Size (@I))) (not (@Right?))) (not (@Right?)))
     (set! /fl_flag2 1))
    (#t
     (set! /fl_flag2 0)))
   (cond
    ((= /fl_flag2 0)
     ; Check that remaining statements in the sequence have multi-calls 
     ; and that a CALL in the IF also appears in the rest of the sequence 
     (cond
      ((and (not (null? (@Set_Difference (@Multi_Calls (@Make //T_/Statements '() (cons (@I) /rest)) 2) (list //Z)))) (not (null? (@Set_Difference (intersection-n (@Multi_Calls (@I) 1) (@Multi_Calls (@Make //T_/Statements '() /rest) 1)) (list //Z)))))
       ; If only one arm is not improper then fully expand the cond. 
       ; If all arms are improper, then delete what follows. 
       ; If the next statement is a CALL, then expand anyway. 
       (set! /n 0)
       (@Down)
       ; to first arm 
       (@Down_To 2)
       ; to statements 
       (cond
        ((not (@Gen_Improper? (@I) "Reg"))
         (set! /n (+ /n 1))))
       (@Up)
       (while (@Right?) 
        (begin
         (@Right)
         (@Down_To 2)
         ; to statements 
         (cond
          ((not (@Gen_Improper? (@I) "Reg"))
           (set! /n (+ /n 1))))
         (@Up)))
       (@Up)
       ; back to cond 
       (@Right)
       (cond
        ((= (@ST (@I)) //T_/Call)
         (set! /n 0)))
       (@Left)
       (cond
        ((and (<= /n 1) (>= (gen-length (@Posn)) 80))
         (display-list-flush " B1(" (gen-length (@Posn)) ") ")
         (set! /fl_flag2 0))
        ((<= /n 1)
         (display-list-flush "e")
         (set! /expansions (+ /expansions 1))
         ; Don't call Fully_Expand_Forwards here, since we 
         ; don't have access to the AS type at this point 
         ; -- hence code is duplicated! 
         (let ((/len (+ (gen-length (@Posn)) (@Max_Pos_L (@I)))))
          (set! /fl_flag1 0)
          (while (= /fl_flag1 0) 
           (cond
            ((not (@Right?))
             (set! /fl_flag1 1))
            (#t
             (@Right)
             (cond
              ((>= (+ /len (@Max_Pos_L (@I))) 80)
               (display-list-flush " B2(" (+ /len (@Max_Pos_L (@I))) ") ")
               (@Left)
               (set! /fl_flag1 1))
              (#t
               (@Left)
               (@Gen_Expand_Forward "Reg")
               (set! /len (+ (gen-length (@Posn)) (@Max_Pos_L (@I))))
               (set! /fl_flag1 0)))))))
         (set! /fl_flag2 1))
        (#t
         (set! /fl_flag2 0))))
      (#t
       (set! /fl_flag2 0)))
     (cond
      ((= /fl_flag2 0)
       (cond
        ((not (@Right?))
         (set! /fl_flag2 1))
        (#t
         (set! /rest (cdr /rest))
         (@Right)
         (set! /fl_flag2 0)))))))))
 (@Up))

(define (/foreach-merge_calls_in_system-3 //Depth //A/S_/Type)
 (@Down)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (cond
   ((and (@Gen_Improper? (@I) "Reg") (@Right?))
    (@Delete_Rest)
    (display-list-flush "x")
    (set! /fl_flag1 1))
   ((@Right?)
    (@Right)
    (set! /fl_flag1 0))
   (#t
    (set! /fl_flag1 1)))))

(define (/foreach-merge_calls_in_system-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Cond) (< (@Size (@I)) //Max_/Cond_/Size))
   (cond
    ((@Trans? //T/R_/Simplify_/Item)
     (@Trans //T/R_/Simplify_/Item "")
     (display-list-flush "s"))))))

(define (/foreach-merge_calls_in_system-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Cond) (< (@Size (@I)) //Max_/Cond_/Size))
   ; See if there is a take_out call at the end of an arm 
   ; which can be safely taken out 
   (set! /call '())
   (@Down)
   ; to first guarded 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (@Down_Last)
     (@Down_Last)
     ; to last st. 
     (cond
      ((= (@ST (@I)) //T_/Call)
       (cond
        ((and (null? /call) (member (@V (@I)) /take_out))
         (set! /call (@I))
         (set! /fl_flag1 0))
        (#t
         (set! /fl_flag1 0))))
      ((not (@Gen_Improper? (@I) "Reg"))
       (set! /call '())
       (@Up)
       (@Up)
       (set! /fl_flag1 1))
      (#t
       (set! /fl_flag1 0)))
     (cond
      ((= /fl_flag1 0)
       (@Up)
       (@Up)
       ; back to guarded 
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))))
   (@Up)
   (cond
    ((not (null? /call))
     (display-list-flush (@N_String (@V /call)) " ")
     (set! /n (+ /n 1))
     (@Down)
     ; to first guarded 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (@Down_Last)
       (@Down_Last)
       ; to last st. 
       (cond
        ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (@V /call)))
         (@Paste_Over /skip_st)))
       (@Up)
       (@Up)
       ; back to guarded 
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))
     (@Up)
     (@Paste_After /call))))))

(define (/foreach-merge_calls_in_system-6 //Depth //A/S_/Type)
 (@Down)
 (while (@Right?) 
  (cond
   ((= (@ST (@I)) //T_/Cond)
    (@Right)
    (cond
     ((= (@ST (@I)) //T_/Call)
      (@Left)
      (@Trans //T/R_/Absorb_/Right ""))))
   (#t
    (@Right)))))

(define (/foreach-merge_calls_in_system-7 //Depth //A/S_/Type)
 (cond
  ((and (= /push 1) (= (@ST (@I)) //T_/Push) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@I) 1)) /call_stack) (@FD_Is_Code? (@Get_n (@I) 2) //Code_/Hash))
   (set! /call (@FD_Find_Call 4 //Code_/Hash))
   (cond
    ((and (not (null? /call)) (not-member /call /ignore))
     (puthash //Return_/Regs /call (union-n (gethash //Return_/Regs /call) (list (@V (@Get_n (@I) 1)))))
     (set! //Entry_/Actions (union-n //Entry_/Actions (list /call))))))
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   ; to first assign 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (@FD_Is_Code? (@Get_n (@I) 2) //Code_/Hash) (not (@Starts_With? (@V (@Get_n (@I) 1)) "HANDLE_")))
       (set! /call (@FD_Find_Call 4 //Code_/Hash))
       (cond
        ((and (not (null? /call)) (not-member /call /ignore))
         (puthash //Return_/Regs /call (union-n (gethash //Return_/Regs /call) (list (@V (@Get_n (@I) 1)))))
         (set! //Entry_/Actions (union-n //Entry_/Actions (list /call)))))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))
   (@Up))))

(define /%const__merge_calls_in_system__1 (@Make 145 '() '()))
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
(define (@Merge_Calls_In_System_Test)
 (cond
  ((not (= (@ST (@I)) //T_/A_/S))
   (@Fail "Selected item is not an action system"))
  (#t
   (cond
    ((not (equal? (@System_Type (@I)) "Reg"))
     (@Fail "Action System is not regular"))
    ((< (@Size (@Get_n (@I) 2)) 2)
     (@Fail "Only one action in the system"))
    (#t
     (@Pass))))))

; Use expansion to reduce the number of calls in an action system 
(define (@Merge_Calls_In_System_Code //Data)
 (let ((/n-save /n)
       (//Z-save //Z)
       (//P '())
       (/expansions-save /expansions)
       (/loops 0)
       (/skip_st-save /skip_st)
       (/actions (@Cs (@Get_n (@I) 2)))
       (/action '())
       (/start (@V (@Get_n (@I) 1)))
       (/new '())
       (/calls '())
       (/take_out-save /take_out)
       (/ignore-save /ignore)
       (/dispatch (@Make_Name "dispatch"))
       (/rest-save /rest)
       (/call-save /call)
       (/size (@Total_Size (@I)))
       (/orig (@I)))
  (set! /n 0)
  (set! //Z (@Make_Name "Z"))
  (set! /expansions 0)
  (set! /skip_st /%const__merge_calls_in_system__1)
  (set! /take_out '())
  (set! /ignore '())
  (set! /rest '())
  (set! /call '())
  ; Treat each action in turn 
  ; (to avoid continually rebuilding the action list after each Take_Out_Right) 
  (@Edit)
  (display-list-flush "Using Expansion to reduce calls... ")
  (for-in /action /actions 
   (begin
    (@New_Program (@Get_n /action 2))
    (set! /loops 0)
    (cond
     ((not (equal? (@V (@Get_n /action 1)) /dispatch))
      (set! /expansions 0)
      ; Note we use ATEACH here because we want 
      ; to do the expansions from the top down: 
      (@Ateach_Stats /foreach-merge_calls_in_system-1 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (set! /loops (+ /loops 1))
      (while (and (not (= /expansions 0)) (<= /loops 10)) 
       (begin
        (set! /expansions 0)
        ; Note we use ATEACH here because we want 
        ; to do the expansions from the top down: 
        (@Ateach_Stats /foreach-merge_calls_in_system-2 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))
        (set! /loops (+ /loops 1))))))
    (set! /new (cons (@Make //T_/Action '() (list (@Get_n /action 1) (@Program))) /new))))
  (display-list "")
  ; New list of actions: 
  (set! /actions (reverse /new))
  ; Simplify the results: 
  (display-list-flush "Deleting unreachable code... ")
  (set! /new '())
  (for-in /action /actions 
   (begin
    (@New_Program (@Get_n /action 2))
    (cond
     ((not (equal? (@V (@Get_n /action 1)) /dispatch))
      (@Foreach_Stats /foreach-merge_calls_in_system-3 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (set! //P (@Posn))
      (@Foreach_Statement /foreach-merge_calls_in_system-4 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))))
    (set! /new (cons (@Make //T_/Action '() (list (@Get_n /action 1) (@Program))) /new))))
  ; New list of actions: 
  (set! /actions (reverse /new))
  (display-list "")
  ; Attempt to take out right any non-Z action calls 
  (display-list-flush "Taking out calls... ")
  ; Don't take out Z or dispatch or any entry actions: 
  (set! /ignore (@Make_Set (cons //Z (cons /dispatch (@MC_Entry_Actions (@Make //T_/A_/S '() (list (@Name /start) (@Make //T_/Actions '() /actions))) 1)))))
  (display-list "Entry actions (not taken out) = " (my-map @N_String (@Final_Seg /ignore 3)))
  (set! /loops 0)
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    (set! /n 0)
    (set! /new '())
    (for-in /action /actions 
     (begin
      (@New_Program (@Get_n /action 2))
      (cond
       ((not (equal? (@V (@Get_n /action 1)) /dispatch))
        ; Try to take out any action called more than once 
        (set! /take_out (@Set_Difference (@Multi_Calls (@Program) 2) /ignore))
        (@Foreach_Statement /foreach-merge_calls_in_system-5 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))))
      (set! /new (cons (@Make //T_/Action '() (list (@Get_n /action 1) (@Program))) /new))))
    (set! /actions (reverse /new))
    (display-list "")
    (set! /loops (+ /loops 1))
    (cond
     ((> /loops 100)
      (display-list "ERROR: too many loops in Merge_Calls!!!")
      (set! /fl_flag2 1))
     ((= /n 0)
      (set! /fl_flag2 1))
     (#t
      (set! /fl_flag2 0)))))
  (@Undo_Edit)
  (@Paste_Over (@Make //T_/A_/S '() (list (@Name /start) (@Make //T_/Actions '() /actions))))
  (cond
   ((@Trans? //T/R_/Delete_/All_/Skips)
    (@Trans //T/R_/Delete_/All_/Skips "")))
  (cond
   ((and (> (@Total_Size (@I)) 20000) (> (* 2 (@Total_Size (@I))) (* 3 /size)))
    (display-list "Size has increased too much!")
    (@Paste_Over /orig)))
  (set! /n /n-save)
  (set! //Z //Z-save)
  (set! /expansions /expansions-save)
  (set! /skip_st /skip_st-save)
  (set! /take_out /take_out-save)
  (set! /ignore /ignore-save)
  (set! /rest /rest-save)
  (set! /call /call-save)))

; Return list of actions which are called N or more times in the given item 
(define (@Multi_Calls //I //N)
 (let ((//R '())
       (/calls (@Calls //I)))
  (while (not (null? /calls)) 
   (begin
    (cond
     ((>= (wsl-ref (wsl-ref /calls 1) 2) //N)
      (set! //R (cons (wsl-ref (wsl-ref /calls 1) 1) //R))))
    (set! /calls (cdr /calls))))
  (@Make_Set //R)))

(define (@MC_Entry_Actions //I /push-par)
 (let ((/push-save /push)
       (/ignore-save /ignore)
       (//Codes '())
       (/code '())
       (//Code_/Hash-save //Code_/Hash)
       (//Return_/Regs-save //Return_/Regs)
       (//Entry_/Actions-save //Entry_/Actions)
       (/call-save /call)
       (/call_stack-save /call_stack)
       (funct-result '()))
  (set! /push /push-par)
  (set! /ignore (list (@Make_Name "dispatch") (@Make_Name "Z") (@V (@Get_n //I 1))))
  (set! //Code_/Hash (hash-table))
  (set! //Return_/Regs (hash-table))
  (set! //Entry_/Actions '())
  (set! /call '())
  (set! /call_stack (@Make_Name "call_stack"))
  (@Edit)
  (@New_Program //I)
  ; Absorb CALLs into IF statements before looking for subroutine calls: 
  (@Foreach_Stats /foreach-merge_calls_in_system-6 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //Codes (@Find_Dispatch_Codes (@Make_Name "dispatch")))
  (cond
   ((not (null? //Codes))
    (for-in /code //Codes 
     (cond
      ((> /code 4)
       (puthash //Code_/Hash /code 1))))
    (@Ateach_Statement /foreach-merge_calls_in_system-7 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (@Undo_Edit)
  ; A valid entry action must have exactly one Return_Regs entry: 
  (for-in /call //Entry_/Actions 
   (cond
    ((not (= (gen-length (gethash //Return_/Regs /call)) 1))
     (set! //Entry_/Actions (@Set_Difference //Entry_/Actions (list /call))))))
  ; An action with the name A_xxx is probably a fallthrough after L Rx,=A(...) 
  (for-in /call //Entry_/Actions 
   (cond
    ((@Starts_With? /call "A_")
     (set! //Entry_/Actions (@Set_Difference //Entry_/Actions (list /call))))))
  (set! funct-result //Entry_/Actions)
  (set! /push /push-save)
  (set! /ignore /ignore-save)
  (set! //Code_/Hash //Code_/Hash-save)
  (set! //Return_/Regs //Return_/Regs-save)
  (set! //Entry_/Actions //Entry_/Actions-save)
  (set! /call /call-save)
  (set! /call_stack /call_stack-save)
  funct-result))

#t
