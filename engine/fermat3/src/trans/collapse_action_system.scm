;;; Scheme translation of WSL code
(define (/foreach-collapse_action_system-1 //Depth //A/S_/Type)
 (cond
  ((and (null? (@Calls (@I))) (<= (gen-length (@Gen_TVs (@I) /as)) 1))
   (set! /body (list (@I)))
   (set! /span 1)
   (set! /stats (@Stat_Count (@I)))
   (set! /posn (@Posn))
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (cond
       ((not (null? (@Calls (@I))))
        (set! /fl_flag1 1))
       ((> (gen-length (@Gen_TVs (@I) /as)) 1)
        (set! /fl_flag1 1))
       (#t
        (set! /tvs (@Gen_TVs (@I) /as))
        (set! /span (+ /span 1))
        (set! /body (cons (@I) /body))
        (set! /stats (+ /stats (@Stat_Count (@I))))
        (set! /fl_flag1 0))))))
   (cond
    ((> /stats 5)
     ; Record this procedure in the list: 
     (cond
      ((null? /tvs)
       (set! /tv 0))
      (#t
       (set! /tv (car /tvs))))
     (set! /procs (cons (list /posn /span /tv (reverse /body)) /procs)))))))

(define (/foreach-collapse_action_system-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (- (+ //N 1))))
   (@Paste_Over /%const__collapse_action_system__1))
  ((= (@ST (@I)) //T_/Call)
   (set! /next (@Make //T_/Number (- (@V (@I))) '()))
   (@Paste_Over (@Make //T_/Call /sub '()))
   (@Paste_Before (@Make //T_/Comment (string-append "First action is " (@N_String (wsl-ref //Names (@V /next)))) '()))
   (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "action") '()) (@Var_To_Expn /next)))))))))

(define (/foreach-collapse_action_system-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (- (+ //N 1))))
   ; CALL Z 
   (@Paste_Over (@Make //T_/Exit (+ 1 //Depth) '()))
   (@Paste_Before (@Make //T_/Comment "Terminate action system" '()))
   (@Paste_Before /%const__collapse_action_system__2))
  ((= (@ST (@I)) //T_/Call)
   (set! /next (@Make //T_/Number (- (@V (@I))) '()))
   (@Paste_Over (@Make //T_/Exit (+ 1 //Depth) '()))
   (@Paste_Before (@Make //T_/Comment (string-append "Next action is " (@N_String (wsl-ref //Names (@V /next)))) '()))
   (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "action") '()) (@Var_To_Expn /next)))))))))

(define (/foreach-collapse_action_system-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (- (+ //N 1))))
   (@Paste_Over /%const__collapse_action_system__1))
  ((= (@ST (@I)) //T_/Call)
   (set! /next (@Make //T_/Number (- (@V (@I))) '()))
   (@Paste_Over (@Make //T_/Call /sub '()))
   (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "action") '()) (@Var_To_Expn /next)))))))))

(define (/foreach-collapse_action_system-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (- (+ //N 1))))
   (@Paste_Over /%const__collapse_action_system__1))
  ((= (@ST (@I)) //T_/Call)
   (set! /next (@Make //T_/Number (- (@V (@I))) '()))
   (@Paste_Over (@Make //T_/Exit (+ 1 //Depth) '()))
   (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "action") '()) (@Var_To_Expn /next)))))))))

(define (/foreach-collapse_action_system-6 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (- (+ //N 1))))
   (@Paste_Over (@Make //T_/Exit (+ 2 //Depth) '())))
  ((= (@ST (@I)) //T_/Call)
   (set! /next (@Make //T_/Number (- (@V (@I))) '()))
   (@Paste_Over (@Make //T_/Exit (+ 1 //Depth) '()))
   (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "action") '()) (@Var_To_Expn /next)))))))))

(define /%const__collapse_action_system__1 (@Make 112 (@Make_Name "Z") '()))
(define /%const__collapse_action_system__2 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "action") '()) (@Make 205 0 '()))))))
(define /%const__collapse_action_system__3 (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "action") '()) (@Make 205 0 '()))))))))))
(define /%const__collapse_action_system__4 (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 112 (@Make_Name "Z") '()))))))
(define /%const__collapse_action_system__5 (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 117 2 '()))))))
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
(define (@Collapse_Action_System_Test)
 (cond
  ((not (= (@ST (@I)) //T_/A_/S))
   (@Fail "Not an action system"))
  ((not (equal? (@System_Type (@I)) "Reg"))
   (@Fail "Not a regular action system"))
  (#t
   (@Pass))))

; The old version leads to exponential growth, so use this instead. 
; Convert to a double loop with an IF statement:
;    
;    action := 1;
;    DO DO IF action = 1 THEN A1
;          ELSIF action = 2 THEN A2
;          ...
;            ELSE EXIT(2) FI OD OD
;    
;    CALL Z becomes EXIT(2+depth)
;    CALL An becomes action := n; EXIT(1+depth)
;
;Note: if the first action (A1) starts with a D_IF and is not called
;from any other action, then we need to do this:
;
;ACTIONS MAIN_1:
;  MAIN_1 == A1 END
;  SUB_1 == DO DO IF action = 2 THEN A2
;                 ELSIF action = 3 THEN A3
;                 ...
;                 ELSE CALL Z FI OD OD END
;ENDACTIONS
;
;where in A1, CALL An becomes: action := n; CALL SUB_1
;  and in Ai, CALL An becomes: action := n; EXIT(1+depth)
;CALL Z is unchanged in both.
;
;Subsequent transformations should be able to handle this action system.
;
;Converting the arms of IF action = ... FI into separate procs ACTION_n
;could be done by @WSL_To_COBOL(?)
;
;
(define (@Collapse_Action_System_Code //Data)
 (let ((/size (@Total_Size (@I)))
       (/posn-save /posn)
       (/orig (@Program)))
  (set! /posn (@Posn))
  (@Collapse_Action_System_Simple //Data)
  (cond
   ((> (@Total_Size (@I)) (* 3 /size))
    (@New_Program /orig)
    (@Goto /posn)
    (display-list "Result of Collapse_Action_System_Simple is too big!")
    (display-list "Converting to a double loop")
    (cond
     ((and (> (@Size (@Get_n (@I) 2)) 2) (= (@ST (@Get_n (@Get_n (@Get_n (@Get_n (@I) 2) 1) 2) 1)) //T_/D_/If) (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 2) 1) 1)) (@V (@Get_n (@I) 1))) (not (@Called? (@V (@Get_n (@I) 1)) (@Get_n (@I) 2))))
      (@Collapse_Action_System_New1 //Data))
     (#t
      (@Collapse_Action_System_New2 //Data)))
    (@Goto /posn)
    (@Trans //T/R_/Delete_/All_/Skips "")))
  (set! /posn /posn-save)))

(define (@Collapse_Action_System_Simple //Data)
 (let ((/as-save /as)
       (//Entry_/Point (@V (@Get_n (@I) 1)))
       (/z_name (@Make_Name "Z"))
       (/total (@Total_Size (@I)))
       (/change 0))
  (set! /as "")
  (@Down_Last)
  (set! /as (@AS_Type))
  (@Up)
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    (cond
     ((@Trans? //T/R_/Simplify_/Action_/System)
      (display-list "Simplifying action system")
      (@Trans //T/R_/Simplify_/Action_/System "")))
    (display-list "")
    (display-list (@Size (@Get_n (@I) 2)) " actions remaining")
    (cond
     ((= (@Size (@Get_n (@I) 2)) 1)
      (set! /fl_flag2 1))
     (#t
      ; Make procs of all actions which only call Z 
      (@Down_Last)
      (@Down_Last)
      (display-list "CAS: Finding actions which only call Z...")
      (set! /change 0)
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (cond
         ((equal? (@V (@Get_n (@I) 1)) //Entry_/Point)
          (cond
           ((not (@Left?))
            (set! /fl_flag1 1))
           (#t
            (@Left)
            (set! /fl_flag1 0))))
         (#t
          (set! /fl_flag1 0)))
        (cond
         ((= /fl_flag1 0)
          (set! /calls (@Calls (@I)))
          (cond
           ((and (= (gen-length /calls) 1) (equal? (wsl-ref (wsl-ref /calls 1) 1) /z_name) (@Trans? //T/R_/Make_/Proc))
            (@Trans //T/R_/Make_/Proc "")
            (set! /change (@ATP_SAD  /change))
            (@To_Last)
            (set! /fl_flag1 0))
           ((not (@Left?))
            (set! /fl_flag1 1))
           (#t
            (@Left)
            (set! /fl_flag1 0)))))))
      (while (not (= /change 0)) 
       (begin
        (set! /change 0)
        (set! /fl_flag1 0)
        (while (= /fl_flag1 0) 
         (begin
          (cond
           ((equal? (@V (@Get_n (@I) 1)) //Entry_/Point)
            (cond
             ((not (@Left?))
              (set! /fl_flag1 1))
             (#t
              (@Left)
              (set! /fl_flag1 0))))
           (#t
            (set! /fl_flag1 0)))
          (cond
           ((= /fl_flag1 0)
            (set! /calls (@Calls (@I)))
            (cond
             ((and (= (gen-length /calls) 1) (equal? (wsl-ref (wsl-ref /calls 1) 1) /z_name) (@Trans? //T/R_/Make_/Proc))
              (@Trans //T/R_/Make_/Proc "")
              (set! /change (@ATP_SAD  /change))
              (@To_Last)
              (set! /fl_flag1 0))
             ((not (@Left?))
              (set! /fl_flag1 1))
             (#t
              (@Left)
              (set! /fl_flag1 0)))))))))
      (@Up)
      (@Up)
      (cond
       ((= (@Size (@Get_n (@I) 2)) 1)
        (set! /fl_flag2 1))
       (#t
        (@CAS_Unfold_Least_Called_Action /as //Entry_/Point /z_name /total)
        (cond
         ((> (@Total_Size (@I)) (* 3 /total))
          (set! /fl_flag2 1))
         (#t
          (set! /fl_flag2 0)))))))))
  ;End of outer loop. We should have only one action in the action system
  (while (not (= (@ST (@I)) //T_/A_/S)) 
   (@Up))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((not (@Left?))
     (set! /fl_flag1 1))
    (#t
     (@Left)
     (cond
      ((not (= (@ST (@I)) //T_/Skip))
       (@Right)
       (set! /fl_flag1 1))
      (#t
       (@Delete)
       (set! /fl_flag1 0))))))
  (cond
   ((@Trans? //T/R_/Simplify_/Item)
    (@Trans //T/R_/Simplify_/Item "")
    (@Trans //T/R_/Delete_/All_/Skips ""))
   (#t
    (display-list "Unable to remove action system completely.")
    (display-list "Some more restructuring will be necessary.")))
  (set! /as /as-save)))

(define (@CAS_Unfold_Least_Called_Action /as-par //Entry_/Point /z_name /total)
 (let ((/as-save /as))
  (set! /as /as-par)
  (let ((/min_n 0)
        (/min_size 0)
        (/min_name 0)
        (/size (hash-table))
        (/posn-save /posn))
   (set! /posn '())
   (display-list "Searching for least-called action")
   (for-in /action (@Cs (@Get_n (@I) 2)) 
    (puthash /size (@V (@Get_n /action 1)) (@Stat_Count (@Get_n /action 2))))
   (set! /min_n 32000)
   (for-in /call (@Calls (@I)) 
    (cond
     ((and (not (equal? (wsl-ref /call 1) /z_name)) (not (equal? (wsl-ref /call 1) //Entry_/Point)))
      (cond
       ((or (< (wsl-ref /call 2) /min_n) (and (equal? (wsl-ref /call 2) /min_n) (< (gethash /size (wsl-ref /call 1)) /min_size)))
        (set! /min_n (wsl-ref /call 2))
        (set! /min_size (gethash /size (wsl-ref /call 1)))
        (set! /min_name (wsl-ref /call 1)))))))
   (display-list "Action " (@N_String /min_name) " is called " /min_n " times")
   ;Down to first action
   (@Down_Last)
   (@Down)
   ; Move to the action we aim to remove 
   (while (not (equal? (@V (@Get_n (@I) 1)) /min_name)) 
    (@Right))
   ; Try to shrink the action by creating procedures from blocks of code 
   ; (we use ATEACH in order to try the larger blocks first) 
   ; TODO: Make this a separate transformation? 
   (let ((/body-save /body)
         (/span-save /span)
         (/stats-save /stats)
         (/procs-save /procs)
         (/tvs-save /tvs)
         (/tv-save /tv))
    (set! /body '())
    (set! /span 0)
    (set! /stats 0)
    (set! /procs '())
    (set! /tvs '())
    (set! /tv 0)
    (@Ateach_Statement /foreach-collapse_action_system-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (cond
     ((not (null? /procs))
      (display-list "Creating " (gen-length /procs) " proc(s).")))
    ; If any procs were created, ensure that there is a WHERE clause 
    ; and create the procs 
    (cond
     ((not (null? /procs))
      (let ((/posn-save /posn)
            (/where '())
            (/n 0)
            (/p '())
            (/new '())
            (/calls-save /calls)
            (/tv-save /tv)
            (/span-save /span)
            (/body-save /body)
            (/name '())
            (/call_n 1)
            (/defns '())
            (/expns (@Make //T_/Expressions '() '()))
            (/lvalues (@Make //T_/Lvalues '() '())))
       (set! /posn (@Posn))
       (set! /calls (@MP_Proc_Calls))
       (set! /tv 0)
       (set! /span 0)
       (set! /body '())
       ; Ensure that there is a surrounding WHERE clause 
       (set! /n (@MP_Ensure_Where  /n))
       (set! /where (@Posn))
       (cond
        ((>= /n 0)
         ; A WHERE had to be inserted, so fix the positions: 
         (set! /posn (concat (concat (@Sub_Seg /posn 1 /n) (list 1 1)) (@Final_Seg /posn (+ /n 1))))
         (for-in /proc /procs 
          (begin
           (set! /p (wsl-ref /proc 1))
           (set! /p (concat (concat (@Sub_Seg /p 1 /n) (list 1 1)) (@Final_Seg /p (+ /n 1))))
           (set! /new (cons (list /p (wsl-ref /proc 2) (wsl-ref /proc 3) (wsl-ref /proc 4)) /new))))
         (set! /procs /new)))
       (while (not (null? /procs)) 
        (begin
         (@Goto (wsl-ref (wsl-ref /procs 1) 1))
         (set! /span (wsl-ref (wsl-ref /procs 1) 2))
         (set! /tv (wsl-ref (wsl-ref /procs 1) 3))
         (set! /body (@Make //T_/Statements '() (wsl-ref (wsl-ref /procs 1) 4)))
         (cond
          ((> /tv 0)
           (set! /body (@Increment /body /as (- /tv) 0))))
         (set! /name (@Make_Name (string-append "p_" (@String /call_n))))
         (while (member /name /calls) 
          (begin
           (set! /call_n (+ /call_n 1))
           (set! /name (@Make_Name (string-append "p_" (@String /call_n))))))
         (set! /calls (union-n (list /name) /calls))
         (set! /defns (cons (@Make //T_/Proc '() (list (@Name /name) /lvalues /lvalues /body)) /defns))
         (@Paste_Over (@Make //T_/Proc_/Call '() (list (@Name /name) /expns /lvalues)))
         (cond
          ((and (> /tv 0) (= /span 1))
           ; Put the EXIT into an IF to preserve posn 
           (@Paste_Cond (list (@I) (@Make //T_/Exit /tv '()))))
          ((> /tv 0)
           (@Right)
           (set! /span (- /span 1))
           (@Paste_Over (@Make //T_/Exit /tv '()))))
         (while (> /span 1) 
          (begin
           (set! /span (- /span 1))
           (@Right)
           (@Paste_Over (@Skip))))
         (set! /procs (cdr /procs))))
       (@Goto /where)
       (@Down_Last)
       (@Down_Last)
       (@Splice_After (reverse /defns))
       (@Goto /posn)
       (set! /posn /posn-save)
       (set! /calls /calls-save)
       (set! /tv /tv-save)
       (set! /span /span-save)
       (set! /body /body-save))))
    (set! /body /body-save)
    (set! /span /span-save)
    (set! /stats /stats-save)
    (set! /procs /procs-save)
    (set! /tvs /tvs-save)
    (set! /tv /tv-save))
   ; Try to remove the action 
   (@Trans //T/R_/Delete_/All_/Skips "")
   (cond
    ((@Trans? //T/R_/Substitute_/And_/Delete)
     (display-list "Removing action " (@N_String /min_name))
     (@Trans //T/R_/Substitute_/And_/Delete ""))
    (#t
     (error (string-append (string-append "Unable to remove action " (@N_String /min_name)) "!!!"))))
   (while (not (= (@ST (@I)) //T_/A_/S)) 
    (@Up))
   (set! /posn /posn-save))
  (set! /as /as-save)))

; The starting action is the first action in the system, it starts with a D_IF 
; and is not called from within the system. This is typical for assembler modules 
; with multiple entry points. 
(define (@Collapse_Action_System_New1 //Data)
 (let ((//N-save //N))
  (set! //N (@Size (@Get_n (@I) 2)))
  (let ((//A/S_/Name (@V (@Get_n (@I) 1)))
        (//Starting_/Action 0)
        (//N_/Actions 0)
        (/n 0)
        (//Bodies (make-vector-eval //N '()))
        (//Names-save //Names)
        (//Name2/Num (hash-table))
        (/guards '())
        (/i 0)
        (/next-save /next)
        (/body-save /body)
        (//A1 '())
        (/main (@Make_Name "MAIN_1"))
        (/sub-save /sub)
        (/name '())
        (/procs-save /procs)
        (/expns (@Make //T_/Expressions '() '()))
        (/lvals (@Make //T_/Lvalues '() '()))
        (/posn-save /posn))
   (set! //Names (make-vector-eval (+ //N 1) '()))
   (set! /next '())
   (set! /body '())
   (set! /sub (@Make_Name "SUB_1"))
   (set! /procs '())
   (set! /posn '())
   (display-list "Extracting D_IF statement to be outside the loop ")
   (@Edit)
   ; Calculate Bodies, Names, Name2Num 
   ; Hash table Name2Num maps action names (keys) to action numbers   
   (let ((/-result- (@FD_Init  //N //Bodies //Names //Name2/Num)))
    (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Names (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Name2/Num (car /-result-)) (set! /-result- (cdr /-result-)))
   ; Find the starting action: 
   (set! //Starting_/Action (gethash //Name2/Num //A/S_/Name))
   (cond
    ((not (= //Starting_/Action 1))
     (error "Starting_Action should be action 1!!!")))
   (@New_Program (wsl-ref //Bodies 1))
   (@Foreach_Statement /foreach-collapse_action_system-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! //A1 (@Cs (@Program)))
   (for /i 2 //N 1 
    (begin
     (@New_Program (wsl-ref //Bodies /i))
     (@Foreach_Statement /foreach-collapse_action_system-3 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! /body (@Cs (@Program)))
     (set! /body (list (@Make //T_/Comment (string-append "This action is " (@N_String (wsl-ref //Names /i))) '()) (@Make 133 '() (list (@Make 17 '() /body)))))
     (set! /name (@Make //T_/Name (@Make_Name (string-append (@N_String (wsl-ref //Names /i)) "_ACTION")) '()))
     (set! /procs (cons (@Make //T_/Proc '() (list /name /lvals /lvals (@Make //T_/Statements '() /body))) /procs))
     (set! /body (list (@Make //T_/Proc_/Call '() (list /name /expns /lvals))))
     (set! /next (@Make //T_/Number /i '()))
     (set! /guards (cons (@Make 7 '() (list (@Make 313 '() (list (@Make 207 (@Make_Name "action") '()) (@Var_To_Expn /next))) (@Make 17 '() /body))) /guards))))
   (set! /guards (cons /%const__collapse_action_system__3 /guards))
   (set! /body (@Make //T_/Cond '() (reverse /guards)))
   (@Undo_Edit)
   (@Paste_Over (@Make 111 '() (list (@Make 9 (@Make_Name "MAIN_1") '()) (@Make 15 '() (list (@Make 8 '() (list (@Make 9 (@Make_Name "MAIN_1") '()) (@Make 17 '() //A1))) (@Make 8 '() (list (@Make 9 (@Make_Name "SUB_1") '()) (@Make 17 '() (list (@Make 141 '() (list (@Make 318 '() (list (@Make 207 (@Make_Name "action") '()) (@Make 205 0 '()))) (@Make 17 '() (list /body)))) (@Make 112 (@Make_Name "Z") '()))))))))))
   (set! /posn (@Posn))
   (set! /n (@MP_Ensure_Where  /n))
   (cond
    ((>= /n 0)
     (set! /posn (concat (concat (@Sub_Seg /posn 1 /n) (list 1 1)) (@Final_Seg /posn (+ /n 1))))))
   (cond
    ((not (= (@ST (@I)) //T_/Where))
     (error "@MP_Ensure_Where didn't work!!!")))
   (@Down_To 2)
   (@Down)
   ; to first defn 
   (@Splice_Before (reverse /procs))
   (@Goto /posn)
   ; Back to the action system 
   (cond
    ((@Trans? //T/R_/Collapse_/Action_/System)
     (@Trans //T/R_/Collapse_/Action_/System "")
     (@Trans //T/R_/Simplify_/Item "")
     (@Trans //T/R_/Delete_/All_/Skips "")))
   (set! //Names //Names-save)
   (set! /next /next-save)
   (set! /body /body-save)
   (set! /sub /sub-save)
   (set! /procs /procs-save)
   (set! /posn /posn-save))
  (set! //N //N-save)))

(define (@Collapse_Action_System_New1_Orig //Data)
 (let ((//N-save //N))
  (set! //N (@Size (@Get_n (@I) 2)))
  (let ((//A/S_/Name (@V (@Get_n (@I) 1)))
        (//Starting_/Action 0)
        (//N_/Actions 0)
        (/n 0)
        (//Bodies (make-vector-eval //N '()))
        (//Names-save //Names)
        (//Name2/Num (hash-table))
        (/guards '())
        (/i 0)
        (/next-save /next)
        (/body-save /body)
        (//A1 '())
        (/main (@Make_Name "MAIN_1"))
        (/sub-save /sub))
   (set! //Names (make-vector-eval (+ //N 1) '()))
   (set! /next '())
   (set! /body '())
   (set! /sub (@Make_Name "SUB_1"))
   (display-list "Extracting D_IF statement to be outside the loop ")
   (@Edit)
   ; Calculate Bodies, Names, Name2Num 
   ; Hash table Name2Num maps action names (keys) to action numbers   
   (let ((/-result- (@FD_Init  //N //Bodies //Names //Name2/Num)))
    (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Names (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Name2/Num (car /-result-)) (set! /-result- (cdr /-result-)))
   ; Find the starting action: 
   (set! //Starting_/Action (gethash //Name2/Num //A/S_/Name))
   (cond
    ((not (= //Starting_/Action 1))
     (error "Starting_Action should be action 1!!!")))
   (@New_Program (wsl-ref //Bodies 1))
   (@Foreach_Statement /foreach-collapse_action_system-4 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! //A1 (@Cs (@Program)))
   (for /i 2 //N 1 
    (begin
     (@New_Program (wsl-ref //Bodies /i))
     (@Foreach_Statement /foreach-collapse_action_system-5 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! /body (@Cs (@Program)))
     (set! /next (@Make //T_/Number /i '()))
     (set! /guards (cons (@Make 7 '() (list (@Make 313 '() (list (@Make 207 (@Make_Name "action") '()) (@Var_To_Expn /next))) (@Make 17 '() /body))) /guards))))
   (set! /guards (cons /%const__collapse_action_system__4 /guards))
   (set! /body (@Make //T_/Cond '() (reverse /guards)))
   (@Undo_Edit)
   (@Paste_Over (@Make 111 '() (list (@Make 9 (@Make_Name "MAIN_1") '()) (@Make 15 '() (list (@Make 8 '() (list (@Make 9 (@Make_Name "MAIN_1") '()) (@Make 17 '() //A1))) (@Make 8 '() (list (@Make 9 (@Make_Name "SUB_1") '()) (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list /body)))))))))))))))))
   (cond
    ((@Trans? //T/R_/Simplify_/Item)
     (@Trans //T/R_/Simplify_/Item "")
     (@Trans //T/R_/Delete_/All_/Skips "")))
   (set! //Names //Names-save)
   (set! /next /next-save)
   (set! /body /body-save)
   (set! /sub /sub-save))
  (set! //N //N-save)))

(define (@Collapse_Action_System_New2 //Data)
 (let ((//N-save //N))
  (set! //N (@Size (@Get_n (@I) 2)))
  (let ((//A/S_/Name (@V (@Get_n (@I) 1)))
        (//Starting_/Action 0)
        (//N_/Actions 0)
        (/n 0)
        (//Bodies (make-vector-eval //N '()))
        (//Names-save //Names)
        (//Name2/Num (hash-table))
        (/guards '())
        (/i 0)
        (/next-save /next)
        (/body-save /body))
   (set! //Names (make-vector-eval (+ //N 1) '()))
   (set! /next '())
   (set! /body '())
   (@Edit)
   ; Calculate Bodies, Names, Name2Num 
   ; Hash table Name2Num maps action names (keys) to action numbers   
   (let ((/-result- (@FD_Init  //N //Bodies //Names //Name2/Num)))
    (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Names (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Name2/Num (car /-result-)) (set! /-result- (cdr /-result-)))
   ; Find the starting action: 
   (set! //Starting_/Action (gethash //Name2/Num //A/S_/Name))
   (for /i 1 //N 1 
    (begin
     (@New_Program (wsl-ref //Bodies /i))
     (@Foreach_Statement /foreach-collapse_action_system-6 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! /body (@Cs (@Program)))
     (set! /next (@Make //T_/Number /i '()))
     (set! /guards (cons (@Make 7 '() (list (@Make 313 '() (list (@Make 207 (@Make_Name "action") '()) (@Var_To_Expn /next))) (@Make 17 '() /body))) /guards))))
   (set! /guards (cons /%const__collapse_action_system__5 /guards))
   (set! /body (@Make //T_/Cond '() (reverse /guards)))
   (set! /next (@Make //T_/Number //Starting_/Action '()))
   (@Undo_Edit)
   (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "action") '()) (@Var_To_Expn /next))))))
   (@Paste_After (@Make 133 '() (list (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list /body)))))))))
   (@Right)
   (cond
    ((@Trans? //T/R_/Simplify_/Item)
     (@Trans //T/R_/Simplify_/Item "")
     (@Trans //T/R_/Delete_/All_/Skips "")))
   (set! //Names //Names-save)
   (set! /next /next-save)
   (set! /body /body-save))
  (set! //N //N-save)))

; ------------------------------------------------------------------------ 

