;;; Scheme translation of WSL code
(define (/foreach-blocks-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Down_Last)
   (@Down)
   ; to first defn 
   (cond
    ((= (@ST (@I)) //T_/Proc)
     (puthash //Proc_/Data (@V (@Get_n (@I) 1)) (list (@I) (@Posn)))))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((= (@ST (@I)) //T_/Proc)
       (puthash //Proc_/Data (@V (@Get_n (@I) 1)) (list (@I) (@Posn))))))))))

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
; Construct the Basic Block arrays for the given program. 
; A basic block is a sequence of consecutive statements in which 
; the flow of control enters at the beginning and leaves at the end 
; without halt or possibility of branching except at the end. 
; For simplicity, we ensure that there is no more than one 
; reference to any variable in the block. 
; (This should help us to track each reference back to the right 
; line in the assembler file) 
; We don't need to include the actual WSL code in the block 
; provided we give the position of the first statement, 
; plus the number of statements in the sequence which are in the block. 
; NB: This assumes that the procs contain no global variable references 
; Use TR_Globals_To_Pars on any WHERE clauses for correct results. 
; The output is a list of basic blocks in the form:
;
;Node: N posn:(...) len: L --> (succs list...)
;[node type]
;0: <control vars...>
;  v1 := phi(v11, v12, ...)
;  ...
;  w1 := <w11, w12, ...>
;
;Blocks are separated by blank lines.
;The file ends with the line:
;Entry node is: N
;
; For each block, the information we need for data flow analysis is: 
; (1) List of modified variables; 
; (2) For each modified variable, the list of vars its value depends on 
;     (for a KILL, this list will not include the var itself) 
; (3) For a block with >1 exits: list of control-dependent variables 
;     (ie variables whose initial value can affect which branch is taken) 
; The format of the Node: line is determined by @BB_Write_Node_Line  
; The format of the summary information is determined by 
; @BB_Write_Item and @BB_Write_List 
; These may generate extra nodes for funct calls. 
; Work backwards through the program, since we need to pass the target 
; node to branch to when the current statement terminates. 
; Node 0 is the end node (not generated) and we skip node 1 
; so that it can be used for the start node: 
(define (@Basic_Blocks //I-par //Filename)
 (let ((//I-save //I))
  (set! //I //I-par)
  (let ((//Action_/Call_/To_/Node-save //Action_/Call_/To_/Node)
        (//Loop_/Exits-save //Loop_/Exits)
        (//Proc_/Data-save //Proc_/Data)
        (//Next_/Node-save //Next_/Node)
        (/node 0)
        (/exit_flag-save /exit_flag))
   (set! //Action_/Call_/To_/Node '())
   (set! //Loop_/Exits '())
   (set! //Proc_/Data (hash-table))
   (set! //Next_/Node 2)
   (set! /exit_flag (@Make_Name "exit_flag"))
   ; Action_Call_To_Node is a stack of hash tables mapping action calls 
   ; to the block number of the start of the action. 
   ; The Loop_Exits stack records the exit node of each enclosing Floop. 
   ; Proc_Data lists the definition and position for each proc 
   ; (after renaming if necessary). 
   ; This is used to construct proc header, proc return and proc call nodes. 
   ; Next_Node is the number of the next free node. 
   (@Write_To //Filename)
   (@Edit)
   (@New_Program //I)
   (cond
    ((member //T_/Where (@Stat_Types (@I)))
     ; Rename the procs and proc calls which clash, if necessary: 
     (let ((//Defn_/Count (@Count_Proc_Defns (@I)))
           (//Orig_/Name (hash-table))
           (/doit 0))
      (for-in /name (@Hash_Keys //Defn_/Count) 
       (cond
        ((> (gethash //Defn_/Count /name) 1)
         (set! /doit 1))))
      (cond
       ((= /doit 1)
        (set! //Orig_/Name (@Rename_Procs  //Defn_/Count //Orig_/Name)))))
     ; If there are any proc definitions, then generate the WHERE Header 
     ; and put all the (now non-clashing) definitions 
     ; plus the body at the top level. 
     (@Foreach_Statement /foreach-blocks-1 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (let ((/heads '())
           (/defn '())
           (/posn '()))
      ; Process the body (ignoring all definitions): 
      (set! /heads (list (@BB_Statements (@Program) '() 0)))
      (for-in /name (@Hash_Keys //Proc_/Data) 
       (begin
        (set! /defn (wsl-ref (gethash //Proc_/Data /name) 1))
        (set! /posn (wsl-ref (gethash //Proc_/Data /name) 2))
        (set! /heads (cons (@BB_Block (list /defn) /posn 1 0) /heads))))
      ; Generate the PROC Header node and link to heads: 
      (set! /node //Next_/Node)
      (set! //Next_/Node (+ //Next_/Node 1))
      (@BB_Write_Node_Line /node '() 1 /heads)
      (@WL "WHERE Header")
      (@WL "")))
    (#t
     (set! /node (@BB_Statements (@Program) '() 0))))
   (@WL (string-append "Entry node is: " (@String /node)))
   (@Undo_Edit)
   (@End_Write)
   (set! //Action_/Call_/To_/Node //Action_/Call_/To_/Node-save)
   (set! //Loop_/Exits //Loop_/Exits-save)
   (set! //Proc_/Data //Proc_/Data-save)
   (set! //Next_/Node //Next_/Node-save)
   (set! /exit_flag /exit_flag-save))
  (set! //I //I-save)))

; List of statement types which cause a change in the control flow and 
; which will be the last statement in the current block 
(set! //B/B_/End_/Block_/Types (list //T_/Cond //T_/D_/If //T_/Exit //T_/Call //T_/Abort //T_/Proc_/Call))
; List of statements which start a new basic block when they are encountered: 
(set! //B/B_/New_/Block_/Types (list //T_/Floop //T_/While //T_/For //T_/D_/Do //T_/A_/S //T_/Var //T_/Where //T_/Proc_/Call))
(define (@BB_Statements //I /posn /dest)
 
 (@BB_List (reverse (@Cs //I)) (@Size //I) /posn /dest))

; L is a reversed list of statements, posn ++ <posn_n> is 
; the position of HEAD(L) in the original program 
(define (@BB_List //L /posn_n /posn /dest)
 (let ((/block '())
       (/len 1)
       (/vars '())
       (/addr 0))
  ; See how much of L can be added to the block: 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (set! /block (cons (car //L) /block))
    (set! //L (cdr //L))
    (cond
     ((or (null? //L) (member (@ST (car /block)) //B/B_/New_/Block_/Types))
      (set! /fl_flag1 1))
     ((or (member (@ST (car //L)) //B/B_/End_/Block_/Types) (member (@ST (car //L)) //B/B_/New_/Block_/Types))
      (set! /fl_flag1 1))
     (#t
      ; Check that there is no more than one reference to any variable: 
      (set! /vars (union-n /vars (@Variables (car /block))))
      (cond
       ((not (null? (intersection-n /vars (@Variables (car //L)))))
        (set! /fl_flag1 1))
       ((and (= (@ST (car /block)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (car /block) 1) 2)) //T_/Variable) (@Starts_With? (@N_String (@V (@Get_n (@Get_n (car /block) 1) 2))) "NOTUSED_"))
        ; If the (just added) statement assigned from a NOTUSED_ variable 
        ; then the original assembler was a BAL call which might have been 
        ; folded into the sequence. So, start a new node 
        (set! /fl_flag1 1))
       (#t
        ; Add next statement to the block: 
        (set! /posn_n (- /posn_n 1))
        (set! /len (+ /len 1))
        (set! /fl_flag1 0)))))))
  ; Process this block 
  (set! /dest (@BB_Block /block (concat /posn (list /posn_n)) /len /dest))
  ; Process the rest if the list: 
  (cond
   ((not (null? //L))
    (set! /dest (@BB_List //L (- /posn_n 1) /posn /dest))))
  /dest))

; Return a singleton list containing the first FermaT comment found 
; or the empty list if no FermaT comment was found: 
(define (@BB_Fermat_Comment //L)
 (let ((//R '()))
  (while (not (null? //L)) 
   (cond
    ((and (= (@ST (car //L)) //T_/Comment) (@Starts_With? (@V (car //L)) "<FermaT>"))
     (set! //R (list (car //L)))
     (set! //L '()))
    (#t
     (set! //L (cdr //L)))))
  //R))

; block is either a singleton of type BB_New_Block_Types 
; or a list of simple statements, possibly ending in a BB_End_Block_Types 
(define (@BB_Block /block /posn /len /dest)
 (let ((//S/T (@ST (last-1 /block)))
       (/node /dest))
  (cond
   ((or (= //S/T //T_/Cond) (= //S/T //T_/D_/If))
    (set! /node (@BB_Cond /block /posn /len /dest)))
   ((= //S/T //T_/Floop)
    (set! /node (@BB_Floop /block /posn /len /dest)))
   ((= //S/T //T_/Exit)
    (set! /node (@BB_Exit /block /posn /len /dest)))
   ((= //S/T //T_/While)
    (set! /node (@BB_While /block /posn /len /dest)))
   ((= //S/T //T_/D_/Do)
    (set! /node (@BB_D_Do /block /posn /len /dest)))
   ((= //S/T //T_/A_/S)
    (set! /node (@BB_A_S /block /posn /len /dest)))
   ((= //S/T //T_/Call)
    (set! /node (@BB_Call /block /posn /len /dest)))
   ((= //S/T //T_/Var)
    (set! /node (@BB_Var /block /posn /len /dest)))
   ((= //S/T //T_/For)
    (set! /node (@BB_For /block /posn /len /dest)))
   ((= //S/T //T_/Where)
    (set! /node (@BB_Where /block /posn /len /dest)))
   ((= //S/T //T_/Abort)
    (set! /node (@BB_Abort /block /posn /len /dest)))
   ((and (= //S/T //T_/Skip) (= /len 1))
    ; Don't create a node 
   )
   ((= //S/T //T_/Proc_/Call)
    (set! /node (@BB_Proc_Call /block /posn /len /dest)))
   ((= //S/T //T_/Proc)
    (set! /node (@BB_Proc /block /posn /len /dest)))
   ((= //S/T //T_/Funct)
    (error "@BB_Funct not yet implemented"))
   ((= //S/T //T_/B/Funct)
    (error "@BB_BFunct not yet implemented"))
   (#t
    (set! /node (@BB_Node (- 1) /block /posn /len (list /dest)))))
  /node))

(define (@BB_Cond /block /posn /len /dest)
 (let ((/targets '())
       (/last (last-1 /block))
       (/last_posn /posn)
       (/guarded '())
       (/n 1))
  (cond
   ((> /len 1)
    (set! /last_posn (concat (butlast-1 /posn) (list (- (+ (last-1 /posn) /len) 1))))))
  ; Process the arms of the cond to get the list of targets 
  ; for the node which does the tests: 
  (for-in /guarded (@Cs /last) 
   (begin
    (set! /targets (cons (@BB_Statements (@Get_n /guarded 2) (concat /last_posn (list /n 2)) /dest) /targets))
    (set! /n (+ /n 1))))
  (@BB_Node (- 1) /block /posn /len (reverse /targets))))

; Create a node containing the given data, return the node number. 
; If the given node number is negative, then allocate a new node. 
(define (@BB_Node /node /block /posn /len /targets)
 (cond
  ((< /node 0)
   (set! /node //Next_/Node)
   (set! //Next_/Node (+ //Next_/Node 1))))
 (cond
  ((and (not (null? /block)) (member /exit_flag (@Assigned (last-1 /block))))
   (let ((/last (last-1 /block)))
    (cond
     ((and (= (@ST /last) //T_/Assignment) (= (@ST (@Get_n (@Get_n /last 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n /last 1) 1)) /exit_flag) (= (@ST (@Get_n (@Get_n /last 1) 2)) //T_/Number) (= (@V (@Get_n (@Get_n /last 1) 2)) 1))
      ; This block sets exit flag, so set the target to the end node 
      (set! /targets (list 0)))))))
 (@BB_Write_Node_Line /node /posn /len /targets)
 (@BB_Write_Block /node /block))

(define (@BB_Exit /block /posn /len /dest)
 (let ((/node (if (> (@V (last-1 /block)) (gen-length //Loop_/Exits)) 0 (wsl-ref //Loop_/Exits (@V (last-1 /block))))))
  (cond
   ((> (gen-length /block) 1)
    ; Create a node for the rest of the list which 
    ; then passes control to `node' 
    (set! /node (@BB_Node (- 1) /block /posn /len (list /node)))))
  /node))

(define (@BB_Call /block /posn /len /dest)
 (let ((/node 0)
       (/name-save /name)
       (funct-result '()))
  (set! /name (@V (last-1 /block)))
  (set! /node (@BB_Get_Node /name //Action_/Call_/To_/Node))
  (cond
   ((null? /node)
    (set! /node 0))
   ((> (gen-length /block) 1)
    (set! /node (@BB_Node (- 1) /block /posn /len (list /node)))))
  (set! funct-result /node)
  (set! /name /name-save)
  funct-result))

; Look up a name in a stack of name -> node tables and return the node: 
(define (@BB_Get_Node /name //L)
 (let ((/node '()))
  (while (and (null? /node) (not (null? //L))) 
   (begin
    (set! /node (gethash (car //L) /name))
    (set! //L (cdr //L))))
  /node))

; Throw away this block, but return dest to avoid unreachable nodes: 
(define (@BB_Abort /block /posn /len /dest)
 
 /dest)

; For the following @BB_XXX calls, block must contain a single statement. 
(define (@BB_Floop /block /posn /len /dest)
 (let ((/head //Next_/Node)
       (/body 0))
  (set! //Next_/Node (+ //Next_/Node 1))
  ; An EXIT(1) within the loop will branch to dest 
  (set! //Loop_/Exits (cons /dest //Loop_/Exits))
  ; head will be the loop header node, and is also 
  ; the exit node for the body. We will create the head node 
  ; once we have a body to link it to: 
  (set! /body (@BB_Statements (@Get_n (car /block) 1) (concat /posn (list 1)) /head))
  (set! //Loop_/Exits (cdr //Loop_/Exits))
  (@BB_Node /head /block /posn /len (list /body))))

(define (@BB_While /block /posn /len /dest)
 (let ((/head //Next_/Node)
       (/body 0))
  (set! //Next_/Node (+ //Next_/Node 1))
  ; head will be the loop header node, and is also 
  ; the exit node for the body. We will create the head node 
  ; once we have a body to link it to: 
  (set! /body (@BB_Statements (@Get_n (car /block) 2) (concat /posn (list 2)) /head))
  (@BB_Node /head (list (car /block)) /posn /len (list /body /dest))))

(define (@BB_D_Do /block /posn /len /dest)
 (let ((/head //Next_/Node)
       (/body 0)
       (/targets '())
       (/last (last-1 /block))
       (/last_posn /posn)
       (/guarded '())
       (/n 1))
  (set! //Next_/Node (+ //Next_/Node 1))
  ; head will be the loop header node, and is also 
  ; the exit node for the various body nodes. 
  ; We will create the head node once we have a body to link it to. 
  ; Process the arms of the D_DO to get the list of targets 
  ; for the node which does the tests: 
  (for-in /guarded (@Cs /last) 
   (begin
    (set! /targets (cons (@BB_Statements (@Get_n /guarded 2) (concat /last_posn (list /n 2)) /head) /targets))
    (set! /n (+ /n 1))))
  (@BB_Node /head /block /posn /len (reverse (cons /dest /targets)))))

; To process a (regular) A_S, set up nodes for each action, 
; with `dest' as the node for CALL Z. 
; Then process each action body in turn to get the starting node for each action. 
; Create the action node as a simple branch to the start node. 
; NOTE: we must create a header node for each action: consider FOO == CALL FOO. 
; Return the node for the starting action. 
(define (@BB_A_S /block /posn /len /dest)
 (let ((/node 0)
       (/start (@V (@Get_n (car /block) 1)))
       (/tab (hash-table))
       (/n 1)
       (/head 0))
  ; Set up the entry nodes: 
  (for-in /action (@Cs (@Get_n (car /block) 2)) 
   (begin
    (set! /node //Next_/Node)
    (set! //Next_/Node (+ //Next_/Node 1))
    (puthash /tab (@V (@Get_n /action 1)) /node)))
  ; CALL Z branches to the dest: 
  (puthash /tab (@Make_Name "Z") /dest)
  (set! //Action_/Call_/To_/Node (cons /tab //Action_/Call_/To_/Node))
  ; Process the action bodies: 
  (for-in /action (@Cs (@Get_n (car /block) 2)) 
   (begin
    ; For a regular action the dest node here should not be used: 
    (set! /node (@BB_Statements (@Get_n /action 2) (concat /posn (list 2 /n 2)) /dest))
    (set! /head (gethash /tab (@V (@Get_n /action 1))))
    (set! /head (@BB_Node /head (@BB_Name "ACTION" (@Get_n /action 1)) (concat /posn (list 2 /n)) 1 (list /node)))
    (set! /n (+ /n 1))))
  (set! //Action_/Call_/To_/Node (cdr //Action_/Call_/To_/Node))
  ; Finally, return the start node header as the entry point: 
  
  (gethash /tab /start)))

; @BB_Where only needs to process the body: the defns have already been done: 
(define (@BB_Where /block /posn /len /dest)
 
 (@BB_Statements (@Get_n (car /block) 1) (concat /posn (list 1)) /dest))

; Save	stacks the values of local variables 
; VAR Init	initalises the new values of local variables 
; body goes here... 
; Restore	restores the old values of local variables from the stacks 
(define (@BB_Var /block /posn /len /dest)
 (let ((/node //Next_/Node)
       (/assigns (@Cs (@Get_n (car /block) 1)))
       (/stack '())
       (/saves '())
       (/restores '())
       (/body 0))
  (set! //Next_/Node (+ //Next_/Node 1))
  (@BB_Write_Node_Line /node /posn /len (list /dest))
  (@WL "Restore")
  (for-in /assign /assigns 
   (begin
    (set! /stack (@Make //T_/Variable (@Make_Name (string-append (@N_String (@V (@Get_n /assign 1))) "__save__")) '()))
    (set! /restores (cons (@Make //T_/Assign '() (list (@Get_n /assign 1) /stack)) /restores))))
  (set! /node (@BB_Write_Item /node (@Make //T_/Assignment '() (reverse /restores))))
  (set! /body (@BB_Statements (@Get_n (car /block) 2) (concat /posn (list 2)) /node))
  (set! /node //Next_/Node)
  (set! //Next_/Node (+ //Next_/Node 1))
  (@BB_Write_Node_Line /node (concat /posn (list 1)) /len (list /body))
  (@WL "VAR Init")
  (set! /body (@BB_Write_Item /node (@Make //T_/Assignment '() /assigns)))
  (set! /node //Next_/Node)
  (set! //Next_/Node (+ //Next_/Node 1))
  (@BB_Write_Node_Line /node (concat /posn (list 1)) /len (list /body))
  (@WL "Save")
  (for-in /assign /assigns 
   (begin
    (set! /stack (@Make //T_/Var_/Lvalue (@Make_Name (string-append (@N_String (@V (@Get_n /assign 1))) "__save__")) '()))
    (set! /saves (cons (@Make //T_/Assign '() (list /stack (@Lvalue_To_Expn (@Get_n /assign 1)))) /saves))))
  (@BB_Write_Item /node (@Make //T_/Assignment '() (reverse /saves)))))

; Save	stacks the value of the control variable 
; FOR Init	initalises the control variable 
; FOR Header	tests the control variable and exits the loop if necessary 
; body goes here... 
; FOR Footer	increments the control variable and branches to the header 
; Restore	restores the control variable 
(define (@BB_For /block /posn /len /dest)
 (let ((/exit //Next_/Node)
       (/var-save /var)
       (/start (@Get_n (car /block) 2))
       (/end (@Get_n (car /block) 3))
       (/inc (@Get_n (car /block) 4))
       (/stack '())
       (/init 0)
       (/head 0)
       (/body 0)
       (/footer 0)
       (/node 0)
       (funct-result '()))
  (set! /var (@Make //T_/Var_/Lvalue (@V (@Get_n (car /block) 1)) '()))
  (set! //Next_/Node (+ //Next_/Node 1))
  (@BB_Write_Node_Line /exit /posn /len (list /dest))
  (@WL "Restore")
  (set! /stack (@Make //T_/Variable (@Make_Name (string-append (@N_String (@V /var)) "__save__")) '()))
  (set! /exit (@BB_Write_Item /exit (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /stack)))))))
  (set! /head //Next_/Node)
  (set! //Next_/Node (+ //Next_/Node 1))
  ; We will create the header node when we have a body to link it to 
  (set! /footer //Next_/Node)
  (set! //Next_/Node (+ //Next_/Node 1))
  (@BB_Write_Node_Line /footer /posn /len (list /head))
  (@WL "FOR Footer")
  (set! /footer (@BB_Write_Item /footer (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Make 220 '() (list (@Var_To_Expn /var) (@Var_To_Expn /inc)))))))))
  (set! /body (@BB_Statements (@Get_n (car /block) 5) (concat /posn (list 5)) /footer))
  (@BB_Write_Node_Line /head /posn /len (list /body /exit))
  (@WL "FOR Header")
  (set! /head (@BB_Write_Item /head (@Make 316 '() (list (@Var_To_Expn /var) (@Var_To_Expn /end)))))
  (set! /init //Next_/Node)
  (set! //Next_/Node (+ //Next_/Node 1))
  (@BB_Write_Node_Line /init /posn /len (list /head))
  (@WL "FOR Init")
  (set! /init (@BB_Write_Item /init (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /start)))))))
  (set! /node //Next_/Node)
  (set! //Next_/Node (+ //Next_/Node 1))
  (@BB_Write_Node_Line /node /posn /len (list /init))
  (@WL "Save")
  (set! funct-result (@BB_Write_Item /node (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /stack) (@Var_To_Expn /var)))))))
  (set! /var /var-save)
  funct-result))

; Proc, Funct and BFunct definitions. 
; Do we need to record the parameters in the header node?? 
; (The PROC CALL 1 nodes will include a list of formal parameters) 
; We need a return node which references the var parameters 
; so that phi functions will be inserted for these variables: 
(define (@BB_Proc /block /posn /len /dest)
 (let ((/body 0)
       (/return (@BB_Node (- 1) (list (@Get_n (car /block) 3) (car /block)) (concat /posn (list 3)) 1 (list /dest))))
  (set! /body (@BB_Statements (@Get_n (car /block) 4) (concat /posn (list 4)) /return))
  (@BB_Node (- 1) /block /posn /len (list /body))))

; Return a singleton item list consisting of the name plus a prefix: 
(define (@BB_Name /prefix //I)
 
 (list (@Make //T_/Name (@Make_Name (concat (string-append /prefix " ") (@N_String (@V //I)))) '())))

; The Node: line for this block has already been written. 
; block is either a singleton of type BB_New_Block_Types 
; (in which case we generate a special `header' node) 
; or a list of simple statements, possibly ending in a BB_End_Block_Types 
; If the block ends in a T_Cond or a T_D_If then the various arms 
; of the conditional are the sucessor blocks: ie we only generate 
; data for the conditions. 
(define (@BB_Write_Block /node /block)
 (let ((//S/T1 (@ST (car /block)))
       (//S/T2 (@ST (last-1 /block))))
  (cond
   ((= //S/T1 //T_/Floop)
    (@WL "FLOOP Header")
    (@WL ""))
   ((= //S/T1 //T_/While)
    (@WL "WHILE Header")
    (set! /node (@BB_Write_Item /node (@Get_n (car /block) 1))))
   ((= //S/T1 //T_/Where)
    (@WL "WHERE Header")
    (@WL ""))
   ((= //S/T1 //T_/Name)
    ; The name has been prefixed with the type, eg ACTION 
    (@WL (@N_String (@V (car /block))))
    (@WL ""))
   ((= //S/T1 //T_/Lvalues)
    ; The block for a return node is the VAR parameters plus the whole proc: 
    (@WL (string-append "PROC Return " (@N_String (@V (@Get_n (wsl-ref /block 2) 1)))))
    (set! /node (@BB_Write_Item /node (car /block))))
   ((= //S/T1 //T_/Proc_/Call)
    ; Used for proc calls where the body cannot be found: 
    (@WL (string-append "PROC CALL " (@N_String (@V (@Get_n (car /block) 1)))))
    (set! /node (@BB_Write_Item /node (car /block))))
   ((= //S/T1 //T_/Proc)
    ; Include an assignment to each formal parameter, 
    ; these are used to create accurate proc summaries 
    ; via dataflow analysis through the proc body: 
    (@WL (string-append "PROC Header " (@N_String (@V (@Get_n (car /block) 1)))))
    (set! /node (@BB_Write_Item /node (@BB_Make_Assign (concat (@Cs (@Get_n (car /block) 2)) (@Cs (@Get_n (car /block) 3)))))))
   ((or (= //S/T2 //T_/Cond) (= //S/T2 //T_/D_/If) (= //S/T1 //T_/D_/Do))
    (@WL "IF")
    (let ((//L '()))
     (for-in /guard (@Cs (last-1 /block)) 
      (set! //L (cons (@Get_n /guard 1) //L)))
     (set! /node (@BB_Write_List /node (concat (butlast-1 /block) (reverse //L))))))
   ((and (= (@GT (car /block)) //T_/Statement) (= (@GT (last-1 /block)) //T_/Statement))
    (set! /node (@BB_Write_List /node /block)))
   (#t
    (display-list "Type1 = " (@Type_Name //S/T1) " Type2 = " (@Type_Name //S/T2))
    (error "Unknown types in this block!")))
  /node))

(define (@BB_Write_List /node //L)
 (for-in //I //L 
  (@BB_Write_Comments //I))
 (@Write_Summ2 (@Summ2_List //L))
 (@WL "")
 /node)

(define (@BB_Write_Item /node //I-par)
 (let ((//I-save //I)
       (funct-result '()))
  (set! //I //I-par)
  (@BB_Write_Comments //I)
  (@Write_Summ2 (@Summ2 //I))
  (@WL "")
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

; Print out the <FermaT> comments in the given item: 
(define (@BB_Write_Comments //I-par)
 (let ((//I-save //I))
  (set! //I //I-par)
  (cond
   ((and (= (@ST //I) //T_/Comment) (@Starts_With? (@V //I) "<FermaT>"))
    (@WL (substr (@V //I) 9))))
  (set! //I //I-save)))

; Create an assignment which assigns to the given lvalues: 
(define (@BB_Make_Assign //L)
 (let ((/assigns '())
       (/val (@Make //T_/Number 0 '()))
       (/done (hash-table)))
  (for-in /var //L 
   (cond
    ((null? (gethash /done /var))
     (puthash /done /var 1)
     (set! /assigns (cons (@Make //T_/Assign '() (list /var /val)) /assigns)))))
  (@Make //T_/Assignment '() (reverse /assigns))))

(define (@BB_Write_Node_Line /node /posn /len /targets)
 (@WL (concat (string-append (concat (string-append (concat (string-append (string-append "Node: " (@String /node)) " posn:") (@Struct_To_String /posn)) " len: ") (@String /len)) " --> ") (@Struct_To_String /targets))))

; For a proc call we need to generate three nodes: 
; (1) Copy actual pars to (renamed) formal pars 
; (2) The summary of the body with all variables renamed 
; (3) Copy (renamed) formal VAR pars to actual VAR pars. 
; As usual, the nodes are generated in reverse order. 
(define (@BB_Proc_Call /block /posn /len /dest)
 (let ((/name-save /name)
       (/vals (@Cs (@Get_n (car /block) 2)))
       (/vars (@Cs (@Get_n (car /block) 3)))
       (/node //Next_/Node)
       (/data '())
       (/formal_vals '())
       (/formal_vars '())
       (/result_vars '())
       (/prefix (string-append (@N_String (@V (@Get_n (car /block) 1))) "__"))
       (/formal_pars '())
       (/actual_pars '())
       (funct-result '()))
  (set! /name (@V (@Get_n (car /block) 1)))
  (set! /data (gethash //Proc_/Data /name))
  (cond
   ((null? /data)
    (set! /node (@BB_Node (- 1) /block /posn /len (list /dest))))
   (#t
    (set! /formal_vals (my-map @Struct_Elts (@Cs (@Get_n (wsl-ref /data 1) 2))))
    (set! /formal_vars (my-map @Struct_Elts (@Cs (@Get_n (wsl-ref /data 1) 3))))
    (set! //Next_/Node (+ //Next_/Node 1))
    (@BB_Write_Node_Line /node /posn /len (list /dest))
    (@WL (string-append "PROC CALL 3 " (@N_String /name)))
    ; Calculate the assignment statement which assigns the new values 
    ; to the actual VAR pars and print its summary: 
    (set! /result_vars (my-map @Elt_To_Expn (@S2_Prefix_List /formal_vars /prefix)))
    (set! /dest (@BB_Write_Assign /node /vars /result_vars))
    (set! /node //Next_/Node)
    (set! //Next_/Node (+ //Next_/Node 1))
    (@BB_Write_Node_Line /node /posn /len (list /dest))
    (@WL (string-append "PROC CALL 2 " (@N_String /name)))
    ; This is a placeholder for the summary of the proc body 
    ; (with the variables prefixed by the proc name): 
    (set! /dest (@BB_Write_Item /node (@Skip)))
    (set! /node //Next_/Node)
    (set! //Next_/Node (+ //Next_/Node 1))
    (@BB_Write_Node_Line /node /posn /len (list /dest))
    (@WL (string-append "PROC CALL 1 " (@N_String /name)))
    ; Calculate the assignment statement which assigns all the actual pars 
    ; to the formal pars and print its summary: 
    (set! /formal_pars (my-map @Elt_To_Lvalue (@S2_Prefix_List (concat /formal_vals /formal_vars) /prefix)))
    (set! /actual_pars (concat /vals (my-map @Lvalue_To_Expn /vars)))
    (set! /node (@BB_Write_Assign /node /formal_pars /actual_pars))))
  (set! funct-result /node)
  (set! /name /name-save)
  funct-result))

; Make an assignment and print its summary: 
(define (@BB_Write_Assign /node /vars /vals)
 (let ((/assigns '())
       (/pair '())
       (/done (hash-table)))
  (while (and (not (null? /vars)) (not (null? /vals))) 
   (begin
    (set! /pair (list (car /vars) (car /vals)))
    (cond
     ((null? (gethash /done /pair))
      (set! /assigns (cons (@Make //T_/Assign '() /pair) /assigns))
      (puthash /done /pair 1)))
    (set! /vars (cdr /vars))
    (set! /vals (cdr /vals))))
  (@BB_Write_Item /node (@Make //T_/Assignment '() /assigns))))

; ----------------------------------------------------------------------- 

