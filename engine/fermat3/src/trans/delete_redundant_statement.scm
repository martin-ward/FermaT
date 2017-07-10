;;; Scheme translation of WSL code
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
(define (@Delete_Redundant_Statement_Test)
 (cond
  ((not (or (= (@GT (@I)) //T_/Statement) (and (= (@GT (@I)) //T_/Assign) (= (@ST (@Parent)) //T_/Assignment))))
   (@Fail "The selected item is neither a Statement nor an Assign."))
  (#t
   (cond
    ((not (null? (intersection-n (@Stat_Types (@I)) (@Make_Set (list //T_/Comment //T_/Assert //T_/Print //T_/Prinflush //T_/Abort //T_/Error)))))
     (@Fail "This transformation will not delete comments, assertions, print, ABORT or ERROR statements"))
    ((not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set)))
     (@Fail "The Statement contains a call"))
    ((not (@Is_Proper?))
     (@Fail "The Statement has a non-zero terminal value"))
    ((null? (@Assigned (@I)))
     (@Pass))
    ((and (= (@Spec_Type (@I)) //T_/Assignment) (= (@Size (@I)) 1) (@LR_Equal? (@Get_n (@Get_n (@I) 1) 1) (@Get_n (@Get_n (@I) 1) 2)))
     ; An assignment x:=x is redundant 
     (@Pass))
    ((and (= (@Spec_Type (@I)) //T_/Assign) (@LR_Equal? (@Get_n (@I) 1) (@Get_n (@I) 2)))
     ; An assignment x:=x is redundant 
     (@Pass))
    ((not (@Up?))
     (@Fail "There are no statements after this one to clobber the assigned variables."))
    ((and (= (@ST (@I)) //T_/Assignment) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Rel_/Seg_/Lvalue)))
     (@Fail "Statement may assign outside defined size of variable"))
    ((not (@DRS_Scan_OK? (@Elts_Assigned (@I))))
     (@Fail "Some assigned variable may be used."))
    (#t
     (@Pass))))))

(define (@Delete_Redundant_Statement_Code //Data)
 (@Clever_Delete))

; V is the set of elements assigned in the Statement. 
; An element is a list of the form: <name, field1, field2, ...> 
; The @DRS_Scan function checks that all the given variables are assigned before 
; they are accessed -- ie the values of the variables are redundant. 
; It searches forwards from the current position. 
; @DRS_Scan is only called at a statement within a statement sequence 
; (ie @Up? is true) and with V non-empty. 
; Done_List is a list of <P, V> pairs showing which positions (with their var sets) 
; have been processed or are currently being processed. 
; Agenda is the list of postponed <P, V> pairs which still need to be dealt with. 
; Agenda contains no duplicates, and does not overlap with Done_List. 
; Before a statement is processed, its posn is added to Done_List 
; (if it is a join posn) 
; If A_Proc_Call_Filter is non-empty then when a !P call is reached, 
; assume that anything not in it may be referenced. 
; The final value of r15 is preserved so that it can be used in !P RETURN 
; Hack: Set this flag if Globals_To_Pars has been executed: 
(set! //D/R/S_/Globals_/To_/Pars_/Done 0)
(define (@DRS_Scan_OK? //V-par)
 (let ((//V-save //V)
       (//O/K-save //O/K)
       (/v '())
       (//Done_/List-save //Done_/List)
       (//Agenda-save //Agenda)
       (//Call_/Depth-save //Call_/Depth)
       (//Max_/Call_/Depth-save //Max_/Call_/Depth)
       (//Posn_/List '())
       (//P '())
       (/type 0)
       (//End_/O/K 1)
       (/accs-save /accs)
       (/sysmacs-save /sysmacs)
       (/regs-save /regs)
       (/cc_name-save /cc_name)
       (/loops 0)
       (funct-result '()))
  (set! //V //V-par)
  (set! //O/K 1)
  (set! //Done_/List '())
  (set! //Agenda '())
  (set! //Call_/Depth 0)
  (set! //Max_/Call_/Depth 5)
  (set! /accs (@Make_Set (my-map @Make_Name (list "a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9" "a10" "a11" "a12" "a13" "a14" "a15" "x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "x10" "x11" "x12" "x13" "x14" "x15" "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "cc"))))
  (set! /sysmacs (@Make_Set (my-map @Make_Name (list "OPEN" "CLOSE" "AOPEN" "ACLOSE" "GET" "AGET" "READ" "ARD" "PUT" "APUT" "WRITE" "OSCOM_GET" "OSCOM_PUT" "GET_DTF" "PUT_DTF" "PUT_DTF_UPD" "OPEN_DTF" "CLOSE_DTF" "OSCOM_GET" "OSCOM_PUT" "GETV" "GET_FIXED" "GET_VARIABLE" "PUT_FIXED" "PUT_VARIABLE" "PUT_CONSOLE" "GET_CONSOLE" "pack" "unpk" "zap" "ap" "sp" "mp" "dp" "fill" "ed" "chain_reg" "init_NOP_flag"))))
  (set! /regs (@Make_Name "regs"))
  (set! /cc_name (@Make_Name "cc"))
  (for-in /v //V 
   (cond
    ((not (and (= (gen-length /v) 1) (member (car /v) /accs)))
     (set! //End_/O/K 0))))
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    ; If we are about to leave a VAR clause or FOR loop, update V 
    ; to remove the local variable(s) 
    ; TODO: check for reaching the end of a procedure body 
    (cond
     ((and (not (@Right?)) (>= (gen-length (@Posn)) 2) (@Is_Proper?))
      (set! //P (@Posn))
      (set! /type (@Spec_Type (@Get (@Program) (@Sub_Seg //P 1 (- (gen-length //P) 2)))))
      (cond
       ((or (= /type //T_/Var) (= /type //T_/For))
        (@Up)
        (@Up)
        (cond
         ((= /type //T_/Var)
          (set! //V (@Elt_Subtract //V (@Elt_Subtract (@Elt_Lvars (@Get_n (@I) 1)) (@Elts_Used (@Get_n (@I) 1))))))
         (#t
          (set! //V (@Elt_Remove //V (list (@Value (@Get_n (@I) 1)))))))))))
    ; If V is empty, then pop a posn and new V from the agenda 
    ; Otherwise, move to the next statement (updating Agenda if necessary) 
    (cond
     ((null? //V)
      (cond
       ((null? //Agenda)
        (set! //O/K 1)
        (set! /fl_flag2 1))
       (#t
        (@Goto (wsl-ref (wsl-ref //Agenda 1) 1))
        (set! //V (wsl-ref (wsl-ref //Agenda 1) 2))
        (set! //Agenda (cdr //Agenda))
        (set! /fl_flag2 0))))
     ((and (@Right?) (@Is_Proper?))
      ; If we can move right, and the current statement is a proper sequence 
      ; then simply process the next statement and continue: 
      (@Right)
      (set! /fl_flag2 0))
     (#t
      ; Find the list of `next' statements: 
      (set! //Posn_/List (@Next_Stats))
      ; Check if the end of a definition or the program has been reached: 
      (cond
       ((member (- 2) //Posn_/List)
        (set! //O/K 0)
        (set! /fl_flag2 1))
       ((member (- 1) //Posn_/List)
        (cond
         ((or (= //End_/O/K 0) (>= //Call_/Depth //Max_/Call_/Depth))
          (set! //O/K 0)
          (set! /fl_flag2 1))
         (#t
          (set! //Posn_/List (@Set_Difference (@Make_Set //Posn_/List) (list (- 1))))
          (set! /fl_flag2 0))))
       (#t
        (set! /fl_flag2 0)))
      (cond
       ((= /fl_flag2 0)
        (cond
         ((null? //Posn_/List)
          (cond
           ((null? //Agenda)
            ; All paths have been fully traced 
            (set! //O/K 1)
            (set! /fl_flag2 1))
           (#t
            ; Get the next posn and var list from the agenda: 
            (@Goto (wsl-ref (wsl-ref //Agenda 1) 1))
            (set! //V (wsl-ref (wsl-ref //Agenda 1) 2))
            (set! //Agenda (cdr //Agenda))
            (set! /fl_flag2 0))))
         (#t
          ; Go to first posn in Posn_List 
          (@Goto (car //Posn_/List))
          (set! //Posn_/List (cdr //Posn_/List))
          ; Check for re-iterating WHILE loop: 
          (cond
           ((and (> (gen-length (@Posn)) 1) (= (@Posn_n) 1))
            (cond
             ((= (@ST (@GParent)) //T_/While)
              (cond
               ((@Elt_Clash_List? (@Elts_Used (@Get_n (@GParent) 1)) //V)
                (set! //V '())
                (set! //O/K 0)
                (set! /fl_flag2 1))
               (#t
                (set! /fl_flag2 0))))
             (#t
              (set! /fl_flag2 0))))
           (#t
            (set! /fl_flag2 0)))
          (cond
           ((= /fl_flag2 0)
            ; Add any remaining positions to the agenda 
            (while (not (null? //Posn_/List)) 
             (begin
              (set! //Agenda (@DRS_Put //Agenda (car //Posn_/List) //V))
              (set! //Posn_/List (cdr //Posn_/List))))
            (set! /fl_flag2 0)))))))))
    (cond
     ((= /fl_flag2 0)
      ; Process the statement: if this clears V then keep processing the agenda 
      ; until a statement doesn't clear V (and a further scan is needed) 
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (let ((/-result- (@DRS_Process  //V //O/K //Agenda)))
         (set! //V (car /-result-)) (set! /-result- (cdr /-result-))
         (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-))
         (set! //Agenda (car /-result-)) (set! /-result- (cdr /-result-)))
        (cond
         ((not (null? //V))
          (set! /fl_flag1 1))
         ((= //O/K 0)
          ; Check if a variable was used: 
          (set! /fl_flag1 2))
         ((null? //Agenda)
          (set! //O/K 1)
          (set! /fl_flag1 2))
         (#t
          (set! /loops (+ /loops 1))
          (cond
           ((> /loops 100)
            (display-list-flush "-")
            (set! //O/K 0)
            (set! /fl_flag1 2))
           (#t
            (@Goto (wsl-ref (wsl-ref //Agenda 1) 1))
            (set! //V (wsl-ref (wsl-ref //Agenda 1) 2))
            (set! //Agenda (cdr //Agenda))
            (set! /fl_flag1 0)))))))
      (cond
       ((= /fl_flag1 2)
        (set! /fl_flag2 1))
       (#t
        (set! /fl_flag2 0)))))))
  (set! funct-result (= //O/K 1))
  (set! //V //V-save)
  (set! //O/K //O/K-save)
  (set! //Done_/List //Done_/List-save)
  (set! //Agenda //Agenda-save)
  (set! //Call_/Depth //Call_/Depth-save)
  (set! //Max_/Call_/Depth //Max_/Call_/Depth-save)
  (set! /accs /accs-save)
  (set! /sysmacs /sysmacs-save)
  (set! /regs /regs-save)
  (set! /cc_name /cc_name-save)
  funct-result))

; Process the currently-selected statement, updating V as appropriate. 
; May need to look inside the statement, and add to the Agenda. 
; Set V := < > and OK := 0 if a variable in V is used before assigned. 
; Remove a variable from V if it is redefined (and set OK := 1) 
; Set V := < > and OK := 1 if the current position and var list are 
; in the Done_List (this is to avoid infinite loops) 
; Notice the difference between processing a CALL and an EXIT: 
; A CALL is `processed' by doing nothing 
; (the next statement is the start of the action body which is a Join Point) 
; An EXIT is `processed' by moving up to the appropriate loop 
; (ready for further forward scanning). 
; After a CALL, @Next_Stats will give the next statement to process, 
; (ie the start of the appropriate action body), while after an EXIT 
; we have finished processing the enclosing loop and (after moving up to it) 
; are ready to move right. 
(define (@DRS_Process //V-par //O/K-par //Agenda-par)
 (let ((//Agenda-save //Agenda)
       (//O/K-save //O/K)
       (//V-save //V)
       (funct-result '()))
  (set! //Agenda //Agenda-par)
  (set! //O/K //O/K-par)
  (set! //V //V-par)
  (let ((//S/T (@Spec_Type (@I)))
        (/calls_ok 0))
   ; Check the Done_List if the current posn is a Join Point 
   (cond
    ((@Join_Point?)
     ; Check if this posn (and vars) has been done, if not, add it to Done_List: 
     (cond
      ((null? (@Set_Difference //V (@DRS_Get //Done_/List (@Posn))))
       (set! //V '())
       (set! //O/K 1))
      (#t
       (set! //Done_/List (@DRS_Put //Done_/List (@Posn) //V))))))
   (cond
    ((not (null? //V))
     ; Check for !P with regs or A_Proc_Call_Filter: 
     (cond
      ((@Set_Subset? (@Make_Set (my-map HEAD (@A_Proc_Calls (@I)))) /sysmacs)
       (set! /calls_ok 1)))
     (cond
      ((and (= /calls_ok 0) (member //T_/A_/Proc_/Call (@Stat_Types (@I))) (member /regs (@Assigned (@I))) (not (null? (intersection-n (@Make_Set (my-map HEAD //V)) (@Set_Difference /accs (list /cc_name))))))
       (set! //V '())
       (set! //O/K 0))
      ((and (= /calls_ok 0) (member //T_/A_/Proc_/Call (@Stat_Types (@I))) (not (null? //A_/Proc_/Call_/Filter)) (member /os_name (@Assigned (@I))) (not (null? (@Set_Difference //V //A_/Proc_/Call_/Filter))))
       (set! //V '())
       (set! //O/K 0))
      ((and (= /calls_ok 0) (member //T_/A_/Proc_/Call (@Stat_Types (@I))) (null? //A_/Proc_/Call_/Filter) (member /os_name (@Assigned (@I))))
       (set! //V '())
       (set! //O/K 0))
      ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (member (@V (@Get_n (@I) 1)) /sysmacs))
       (cond
        ((@Elt_Clash_List? (@Set_Difference (@Elts_UBA (@I)) (@Elts_Assd_To_Self (@I))) //V)
         ; Found a reference, so fail: 
         (set! //V '())
         (set! //O/K 0))
        (#t
         (set! //V (@Set_Difference //V (@Elts_Redefined (@I)))))))
      ((and (@Is_Proper?) (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set)))
       ; If there are no calls and the item is proper, then use @Elts_UBA: 
       (cond
        ((@Elt_Clash_List? (@Set_Difference (@Elts_UBA (@I)) (@Elts_Assd_To_Self (@I))) //V)
         ; Found a reference, so fail: 
         (set! //V '())
         (set! //O/K 0))
        (#t
         (set! //V (@Set_Difference //V (@Elts_Redefined (@I))))
         ; If we take the address of an element in and save it 
         ; then any reference to that address may be a reference to 
         ; the element in V. So add the assigned elements to V: 
         (cond
          ((and (= (@ST (@I)) //T_/Assignment) (@Elt_Clash_List? (@Elts_Addr (@I)) //V))
           (set! //V (union-n //V (@Elts_Assigned (@I))))))
         (cond
          ((null? //V)
           (set! //O/K 1))))))
      ((= //S/T //T_/Exit)
       ; Move up to the right loop 
       (let ((/n (@Value (@I))))
        (while (and (> /n 0) (@Up?)) 
         (begin
          (@Up)
          (cond
           ((= (@Spec_Type (@I)) //T_/Floop)
            (set! /n (- /n 1))))))
        (cond
         ((> /n 0)
          ; the exit leaves the whole program, so fail: 
          (set! //V '())
          (set! //O/K 0)))))
      ((or (= //S/T //T_/Call) (= //S/T //T_/Skip) (= //S/T //T_/Comment))
       ; Do nothing 
      )
      ((and (= //S/T //T_/Proc_/Call) (= //D/R/S_/Globals_/To_/Pars_/Done 1))
       ; Assume we have converted globals to pars: so this is OK! 
       ; Note: we have to assume that *all* elements are assigned 
       ; due to the way Globals_To_Pars is implemented! 
       ; Hence, we cannot assume any element is killed. 
       (cond
        ((member (list /os_name) (@Elements (@I)))
         (set! //V '())
         (set! //O/K 0))
        ((@Elt_Clash_List? (@Elements (@I)) //V)
         (set! //V '())
         (set! //O/K 0))))
      ((member //S/T //Call_/Types_/Set)
       (set! //V '())
       (set! //O/K 0))
      ((and (not-member //T_/Proc_/Call (@Stat_Types (@I))) (@Elt_Clash_List? (@Elts_UBA (@I)) //V))
       ; If the statement uses any variable before it is assigned 
       ; then we can fail. Otherwise, we have to check each component 
       ; (to trace the calls and exits) 
       (set! //V '())
       (set! //O/K 0))
      (#t
       (let ((/-result- (@DRS_Process_Comps  //S/T //V //O/K //Agenda)))
        (set! //V (car /-result-)) (set! /-result- (cdr /-result-))
        (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-))
        (set! //Agenda (car /-result-)) (set! /-result- (cdr /-result-))))))))
  (set! funct-result (list //V //O/K //Agenda))
  (set! //Agenda //Agenda-save)
  (set! //O/K //O/K-save)
  (set! //V //V-save)
  funct-result))

; The current item doesn't use any of the variables itself, but does contain 
; some calls or exits. So we need to trace each of its components. 
; We only need to consider specific types which have statements as components. 
(define (@DRS_Process_Comps //S/T //V-par //O/K-par //Agenda-par)
 (let ((//Agenda-save //Agenda)
       (//O/K-save //O/K)
       (//V-save //V)
       (funct-result '()))
  (set! //Agenda //Agenda-par)
  (set! //O/K //O/K-par)
  (set! //V //V-par)
  (cond
   ((or (= //S/T //T_/Cond) (= //S/T //T_/D_/If) (= //S/T //T_/D_/Do))
    ; Add all the clause bodies to the agenda in L to R order 
    (@Down_Last)
    ; to last clause 
    (set! //Agenda (cons (list (concat (@Posn) (list 2 1)) //V) //Agenda))
    (while (and (@Left?) (not (@Elt_Clash_List? (@Elements (@Get_n (@I) 1)) //V))) 
     (begin
      (@Left)
      (set! //Agenda (cons (list (concat (@Posn) (list 2 1)) //V) //Agenda))))
    (cond
     ((@Elt_Clash_List? (@Elements (@Get_n (@I) 1)) //V)
      (set! //V '())
      (set! //O/K 0)))
    (@Up)
    ; back to the statement 
    ; For a D_Do we also need to scan forwards, 
    ; otherwise we only need to process the agenda 
    (cond
     ((and (not (= //S/T //T_/D_/Do)) (= //O/K 1))
      (set! //V '()))))
   ((= //S/T //T_/A_/S)
    ; Add the first statement of the starting action to the agenda 
    ; and stop scanning from here (set V := < >; OK := 1) 
    (let ((/start (@Value (@Get_n (@I) 1))))
     (@Down_Last)
     (@Down)
     ; to first action 
     (while (and (not (equal? (@Value (@Get_n (@I) 1)) /start)) (@Right?)) 
      (@Right))
     (set! //Agenda (cons (list (concat (@Posn) (list 2 1)) //V) //Agenda))
     (set! //V '())
     (set! //O/K 1)
     ; Ready to process the starting action 
    ))
   ((= //S/T //T_/For)
    ; Add the body (with the local var removed from V) to the Agenda 
    ; (unless all globals are shadowed) and continue scanning 
    (cond
     ((not (equal? //V (list (@Value (@Get_n (@I) 1)))))
      (set! //Agenda (cons (list (concat (@Posn) (list 5 1)) (@Set_Difference //V (list (@V (@Get_n (@I) 1))))) //Agenda)))))
   ((= //S/T //T_/Var)
    ; Add the body (with the local vars removed from V) to the Agenda 
    ; (unless all globals are shadowed) and continue scanning 
    (cond
     ((not (null? (@Set_Difference //V (@Lvars (@Get_n (@I) 1)))))
      (set! //Agenda (cons (list (concat (@Posn) (list 2 1)) (@Set_Difference //V (@Elt_Lvars (@Get_n (@I) 1)))) //Agenda)))))
   ((= //S/T //T_/Floop)
    ; Add the loop body to the agenda for processing 
    (set! //Agenda (cons (list (concat (@Posn) (list 1 1)) //V) //Agenda))
    (set! //V '())
    (set! //O/K 1))
   ((= //S/T //T_/Where)
    ; Add the where body to the agenda 
    (set! //Agenda (cons (list (concat (@Posn) (list 1 1)) //V) //Agenda))
    (set! //V '())
    (set! //O/K 1))
   ((= //S/T //T_/While)
    ; Add the loop body to the agenda and continue scanning 
    (set! //Agenda (cons (list (concat (@Posn) (list 2 1)) //V) //Agenda)))
   (#t
    (set! //V '())
    (set! //O/K 0)))
  (set! funct-result (list //V //O/K //Agenda))
  (set! //Agenda //Agenda-save)
  (set! //O/K //O/K-save)
  (set! //V //V-save)
  funct-result))

; Return the V where <P, V> is in L (if not found, then return < >) 
(define (@DRS_Get //L //P)
 (let ((//V '()))
  (while (not (null? //L)) 
   (cond
    ((equal? (wsl-ref (wsl-ref //L 1) 1) //P)
     (set! //V (wsl-ref (wsl-ref //L 1) 2))
     (set! //L '()))
    (#t
     (set! //L (cdr //L)))))
  //V))

; Return a new L which contains <P, V> (overwriting any existing smaller V component) 
(define (@DRS_Put //L //P //V-par)
 (let ((//V-save //V)
       (//R '())
       (funct-result '()))
  (set! //V //V-par)
  (while (not (null? //L)) 
   (cond
    ((equal? (wsl-ref (wsl-ref //L 1) 1) //P)
     (cond
      ((@Set_Subset? //V (wsl-ref (wsl-ref //L 1) 2))
       (set! //R (concat (reverse //L) //R)))
      (#t
       (set! //R (concat (concat (reverse (cdr //L)) (list (list //P //V))) //R))))
     (set! //L '())
     (set! //V '()))
    (#t
     (set! //R (cons (car //L) //R))
     (set! //L (cdr //L)))))
  ; If V has been dealt with (by overwriting), then it will be empty. 
  ; Otherwise, add a new element to the list: 
  (cond
   ((not (null? //V))
    (set! //R (cons (list //P //V) //R))))
  (set! funct-result //R)
  (set! //V //V-save)
  funct-result))

; Returns true if the currently selected statement may have two or more previous statements 
; ie it is the statement after a Cond, D_If, D_Do or Floop, 
; OR it is the first statement in an action body, a Floop, a While body, 
; a For body or a D_Do body 
(set! //Join_/Types1 (@Make_Set (list //T_/Cond //T_/D_/If //T_/D_/Do //T_/Floop)))
(set! //Join_/Types2 (@Make_Set (list //T_/Floop //T_/While //T_/For //T_/D_/Do)))
(define (@Join_Point?)
 (let ((//J/P 0))
  (cond
   ((not (@Up?))
    (set! //J/P 0))
   (#t
    (cond
     ((@Left?)
      (@Left)
      ; to the previous statement 
      (cond
       ((not (null? (intersection-n (@Stat_Types (@I)) //Join_/Types1)))
        (set! //J/P 1))
       (#t
        (set! //J/P 0)))
      (@Right)
      ; back to the orig posn 
     )
     (#t
      ; We are at the beginning of a sequence, so check the enclosing statement 
      (let ((/orig_posn (@Posn)))
       (@Up)
       ; to the sequence 
       (while (and (@Up?) (not (= (@GT (@I)) //T_/Statement)) (not (= (@GT (@I)) //T_/Action))) 
        (@Up))
       (cond
        ((= (@GT (@I)) //T_/Action)
         (set! //J/P 1))
        ((member (@Spec_Type (@I)) //Join_/Types2)
         (set! //J/P 1))
        (#t
         (set! //J/P 0)))
       (@Goto /orig_posn))))))
  (= //J/P 1)))

; Return the list of possible next statements from the current statement. 
; The current statement is not an EXIT (they are dealt with in @DRS_Process) 
; The next statement for a CALL is the body of the destination action, 
; otherwise it is the next statement in the normal execution sequence. 
; If we could reach the end of the program, then return -1 as a posn. 
; If we have to give up (ie the vars could be used), then return -2 as a posn. 
; This function is allowed to clobber the current position. 
(define (@Next_Stats)
 (let ((//P/L '()))
  (cond
   ((= (@Spec_Type (@I)) //T_/Call)
    (cond
     ((>= //Call_/Depth //Max_/Call_/Depth)
      (set! //P/L (list (- 1))))
     (#t
      (set! //Call_/Depth (+ //Call_/Depth 1))
      ; For a CALL, the next statement is the first in the action body. 
      ; So find the right action: 
      (let ((/name (@Value (@I))))
       ; move up to the action system 
       (while (and (@Up?) (not (= (@Spec_Type (@I)) //T_/A_/S))) 
        (@Up))
       (cond
        ((not (= (@Spec_Type (@I)) //T_/A_/S))
         (set! //P/L (list (- 1))))
        (#t
         ; For CALL Z, execution continues after the action system, 
         ; otherwise, we need to look for the action: 
         (cond
          ((equal? /name (@Make_Name "Z"))
           (set! //P/L (@Next_Stats)))
          ((equal? /name (@Make_Name "dispatch"))
           (set! //P/L (list (- 2))))
          (#t
           (@Down_Last)
           (@Down)
           ; to first action 
           (while (and (@Right?) (not (equal? (@Value (@Get_n (@I) 1)) /name))) 
            (@Right))
           (cond
            ((not (equal? (@V (@Get_n (@I) 1)) /name))
             (set! //P/L (list (- 1))))
            (#t
             ; Return posn of first statement in action body: 
             (set! //P/L (list (concat (@Posn) (list 2 1))))))))))))))
   ((@Right?)
    (@Right)
    (set! //P/L (list (@Posn))))
   ((not (@Up?))
    ; Have reached the top of the program: 
    (set! //P/L (list (- 1))))
   (#t
    ; End of sequence reached, move up to the enclosing statement or action 
    (@Up)
    (while (and (and (not (= (@GT (@I)) //T_/Statement)) (not (= (@GT (@I)) //T_/Action)) (not (= (@GT (@I)) //T_/Definition))) (@Up?)) 
     (@Up))
    (cond
     ((= (@GT (@I)) //T_/Definition)
      (set! //P/L (list (- 2))))
     ((= (@GT (@I)) //T_/Action)
      ; The next statements after an action are the statements after each call 
      ; For now we will ignore these (for a regular action system, we will 
      ; never reach the end of an action body anyway). TODO: improve this. 
      (set! //P/L (list (- 2))))
     ((or (not (= (@GT (@I)) //T_/Statement)) (not (@Up?)))
      ; Must have reached the top of the program: 
      (set! //P/L (list (- 1))))
     (#t
      ; We are now sitting on the enclosing statement. 
      ; Check for loops (the next statement could restart the loop) 
      ; and add the list of next statements after THIS statement. 
      (cond
       ((= (@Spec_Type (@I)) //T_/Floop)
        ; Next statement after the end of a Floop is the body 
        ; The next statement CANNOT be the one after the loop 
        (set! //P/L (list (concat (@Posn) (list 1 1)))))
       (#t
        (cond
         ((= (@Spec_Type (@I)) //T_/While)
          (set! //P/L (list (concat (@Posn) (list 2 1)))))
         ((= (@Spec_Type (@I)) //T_/For)
          (set! //P/L (list (concat (@Posn) (list 5 1)))))
         ((= (@Spec_Type (@I)) //T_/D_/Do)
          ; Any arm of the loop could be executed next 
          (@Down)
          ; to first arm 
          (set! //P/L (list (concat (@Posn) (list 2 1))))
          (while (@Right?) 
           (begin
            (@Right)
            (set! //P/L (list (concat (@Posn) (list 2 1))))))
          (@Up)
          ; back to loop 
         ))
        ; Execution could continue after this statement 
        (set! //P/L (concat //P/L (@Next_Stats)))))))))
  //P/L))

