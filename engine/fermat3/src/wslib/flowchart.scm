;;; Scheme translation of WSL code
(define (/foreach-flowchart-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/S)
   (@Trans //T/R_/Simplify_/Action_/System "dispatch"))))

(define (/foreach-flowchart-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Comment)
   (set! //R (cons (@V (@I)) //R)))))

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
; Write a VCG-format flowchart for the given program to the given file. 
; We use backedge for loops (FOR, DO...OD and WHILE) and for action calls 
; to higher actions in the system. 
; Conditional statements use labelled bentnearedge type edges. 
; Loop_Exits is a stack which records the node for the end of each enclosing loop. 
(define (@Flowchart //I-par //Filename)
 (let ((//I-save //I))
  (set! //I //I-par)
  (let ((//Name2/Num-save //Name2/Num)
        (//Z (@Make_Name "Z"))
        (/entry_point-save /entry_point)
        (/dispatch-save /dispatch)
        (//N/O/T_/U/S/E/D (@Make_Name "NOT_USED"))
        (/dispatch_n-save /dispatch_n)
        (/dispatch_node-save /dispatch_node)
        (//Next_/Node-save //Next_/Node)
        (//End_/Node-save //End_/Node)
        (//Return_/Node-save //Return_/Node)
        (/entry 0)
        (//Subgraph-save //Subgraph)
        (//Subgraph_/N-save //Subgraph_/N)
        (//Loop_/Exits-save //Loop_/Exits)
        (//Current_/Action-save //Current_/Action)
        (//Options (hash-table))
        (/skipped_vars-save /skipped_vars)
        (//Dispatch_/Nodes-save //Dispatch_/Nodes)
        (//Runtime_/Flow-save //Runtime_/Flow)
        (//Thin_/Back_/Edges-save //Thin_/Back_/Edges)
        (//Max_/Box_/Lines1-save //Max_/Box_/Lines1)
        (//Max_/Box_/Lines2-save //Max_/Box_/Lines2)
        (//Max_/Box_/Chars-save //Max_/Box_/Chars)
        (//Max_/Rhomb_/Chars-save //Max_/Rhomb_/Chars))
   (set! //Name2/Num (hash-table))
   (set! /entry_point (@Make_Name "entry_point"))
   (set! /dispatch (@Make_Name "dispatch"))
   (set! /dispatch_n (- 1))
   (set! /dispatch_node 0)
   (set! //Next_/Node 2)
   (set! //End_/Node 2)
   (set! //Return_/Node (hash-table))
   (set! //Subgraph 0)
   (set! //Subgraph_/N 0)
   (set! //Loop_/Exits '())
   (set! //Current_/Action 0)
   (set! /skipped_vars (hash-table))
   (set! //Dispatch_/Nodes 1)
   (set! //Runtime_/Flow 0)
   (set! //Thin_/Back_/Edges 1)
   (set! //Max_/Box_/Lines1 8)
   (set! //Max_/Box_/Lines2 2)
   (set! //Max_/Box_/Chars 25)
   (set! //Max_/Rhomb_/Chars 20)
   (@Write_To //Filename)
   (set! //Options (@Read_Options_File //Options_/File))
   (cond
    ((not (null? (gethash //Options "Dispatch_Nodes")))
     (set! //Dispatch_/Nodes (gethash //Options "Dispatch_Nodes"))))
   (cond
    ((not (null? (gethash //Options "Runtime_Flow")))
     (set! //Runtime_/Flow (gethash //Options "Runtime_Flow"))))
   (cond
    ((not (null? (gethash //Options "Thin_Back_Edges")))
     (set! //Thin_/Back_/Edges (gethash //Options "Thin_Back_Edges"))))
   (cond
    ((not (null? (gethash //Options "Max_Box_Lines1")))
     (set! //Max_/Box_/Lines1 (gethash //Options "Max_Box_Lines1"))))
   (cond
    ((not (null? (gethash //Options "Max_Box_Lines2")))
     (set! //Max_/Box_/Lines2 (gethash //Options "Max_Box_Lines2"))))
   (cond
    ((not (null? (gethash //Options "Max_Box_Chars")))
     (set! //Max_/Box_/Chars (gethash //Options "Max_Box_Chars"))))
   (cond
    ((not (null? (gethash //Options "Max_Rhomb_Chars")))
     (set! //Max_/Rhomb_/Chars (gethash //Options "Max_Rhomb_Chars"))))
   ; Simplify any action systems: 
   (@Edit)
   (@New_Program //I)
   (cond
    (#f
     (@Foreach_Statement /foreach-flowchart-1 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (@Trans //T/R_/Constant_/Propagation 256)
     (@Trans //T/R_/Delete_/All_/Redundant "")
     (@Trans //T/R_/Simplify "")
     #t))
   ; Check which variables to skip: 
   (puthash /skipped_vars //N/O/T_/U/S/E/D 1)
   (let ((/v '()))
    (for-in /v (@Variables (@Program)) 
     (cond
      ((@Starts_With? /v "NOTUSED_")
       (puthash /skipped_vars /v 1)))))
   (display-list "Writing flowchart...")
   (@FC_Initialise)
   ; The trick is to work _backwards_ through the program, from the end node 
   ; to the start node. Each fragment of the flowchart may have several 
   ; outgoing edges (eg exits from a loop body) but a single incoming edge. 
   ; So, to process an item we pass it the node to which it is to link 
   ; its outcoming edges and it returns the node which is its entry point. 
   ; (loop exits and action body entry nodes are stored in global variables) 
   ; A SKIP statement just returns the exit node as its entry: ie it adds 
   ; no nodes or edges to the flowchart. 
   ; We pass a negated node number to indicate to the item that the node 
   ; it is linking to is _above_ it: ie any linking edges must be backedge types. 
   (@FC_Start_Subgraph "start" 0)
   (@FC_Node "ellipse" 0 "start" "" "")
   ; Generate a flowchart for @Program and link it to node 1: 
   ; (Each link to node 1 creates a new end node) 
   (set! /entry (@FC_Generic (@Program) 1))
   ; Write the start node and link it in: 
   (@FC_Edge "edge" "" 0 (abs /entry))
   (cond
    ((= //Subgraph 1)
     (@FC_End_Subgraph)))
   (@FC_Finalise)
   (display-list "Done.")
   (@End_Write)
   (@Undo_Edit)
   (set! //Name2/Num //Name2/Num-save)
   (set! /entry_point /entry_point-save)
   (set! /dispatch /dispatch-save)
   (set! /dispatch_n /dispatch_n-save)
   (set! /dispatch_node /dispatch_node-save)
   (set! //Next_/Node //Next_/Node-save)
   (set! //End_/Node //End_/Node-save)
   (set! //Return_/Node //Return_/Node-save)
   (set! //Subgraph //Subgraph-save)
   (set! //Subgraph_/N //Subgraph_/N-save)
   (set! //Loop_/Exits //Loop_/Exits-save)
   (set! //Current_/Action //Current_/Action-save)
   (set! /skipped_vars /skipped_vars-save)
   (set! //Dispatch_/Nodes //Dispatch_/Nodes-save)
   (set! //Runtime_/Flow //Runtime_/Flow-save)
   (set! //Thin_/Back_/Edges //Thin_/Back_/Edges-save)
   (set! //Max_/Box_/Lines1 //Max_/Box_/Lines1-save)
   (set! //Max_/Box_/Lines2 //Max_/Box_/Lines2-save)
   (set! //Max_/Box_/Chars //Max_/Box_/Chars-save)
   (set! //Max_/Rhomb_/Chars //Max_/Rhomb_/Chars-save))
  (set! //I //I-save)))

(define (@FC_Generic //I-par /to)
 (let ((//I-save //I)
       (//G/T (@GT //I-par))
       (/node /to)
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((= //G/T //T_/Statement)
    (set! /node (@FC_Statement //I /to)))
   ((= //G/T //T_/Assigns)
    (set! /node (@FC_Block //I /to)))
   ((= //G/T //T_/Expression)
    (set! /node (@FC_Block //I /to)))
   ((= //G/T //T_/Condition)
    (set! /node (@FC_Block //I /to)))
   ((= //G/T //T_/Definition)
    (set! /node (@FC_Definition //I /to)))
   ((= //G/T //T_/Lvalue)
    (set! /node (@FC_Block //I /to)))
   ((= //G/T //T_/Guarded)
    (set! /node (@FC_Guarded //I /to)))
   ((= //G/T //T_/Action)
    (set! /node (@FC_Action //I /to)))
   ((= //G/T //T_/Statements)
    (set! /node (@FC_Statements //I /to)))
   ((= //G/T //T_/Expressions)
    (set! /node (@FC_Block //I /to)))
   ((= //G/T //T_/Lvalues)
    (set! /node (@FC_Block //I /to)))
   ((= //G/T //T_/Definitions)
    (set! /node (@FC_Definitions //I /to)))
   ((= //G/T //T_/Actions)
    (set! /node (@FC_Actions //I /to)))
   ((= //G/T //T_/Guardeds)
    (set! /node (@FC_Guardeds //I /to)))
   (#t
    (display-list (string-append (concat (string-append (string-append "UNRECOGNISED TYPE " (@Type_Name (@GT //I))) "(") (@String (@GT //I))) ")"))))
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

; Fill in the flowchart for the given statement and join it to node to. 
(define (@FC_Statement //I-par /to)
 (let ((//I-save //I)
       (//S/T (@ST //I-par))
       (/node /to)
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((= //S/T //T_/A_/Proc_/Call)
    (set! /node (@FC_Block //I /to)))
   ((= //S/T //T_/X_/Proc_/Call)
    (set! /node (@FC_Block //I /to)))
   ((= //S/T //T_/M/W_/Proc)
    (set! /node (@FC_Block //I /to)))
   ((= //S/T //T_/Assignment)
    (set! /node (@FC_Block //I /to)))
   ((= //S/T //T_/A_/S)
    (set! /node (@FC_A_S //I /to)))
   ((= //S/T //T_/Call)
    (set! /node (@FC_Call //I /to)))
   ((= //S/T //T_/Comment)
    (set! /node (@FC_Block //I /to)))
   ((= //S/T //T_/Cond)
    (set! /node (@FC_Cond //I /to)))
   ((= //S/T //T_/Exit)
    (set! /node (@FC_Exit //I /to)))
   ((= //S/T //T_/For)
    (set! /node (@FC_For //I /to)))
   ((= //S/T //T_/Floop)
    (set! /node (@FC_Floop //I /to)))
   ((= //S/T //T_/Proc_/Call)
    (set! /node (@FC_Block //I /to)))
   ((= //S/T //T_/Skip)
    (set! /node (@FC_Block //I /to)))
   ((= //S/T //T_/Abort)
    (set! /node (@FC_Abort //I /to)))
   ((= //S/T //T_/Var)
    (set! /node (@FC_Var //I /to)))
   ((= //S/T //T_/Where)
    (set! /node (@FC_Where //I /to)))
   ((= //S/T //T_/While)
    (set! /node (@FC_While //I /to)))
   ((= //S/T //T_/D_/If)
    (set! /node (@FC_D_If //I /to)))
   ((= //S/T //T_/Print)
    (set! /node (@FC_Block //I /to)))
   (#t
    (display-list (string-append (concat (string-append (string-append "UNRECOGNISED STATEMENT: " (@Type_Name (@ST //I))) "(") (@String (@ST //I))) ") */"))))
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

(define (@FC_Statements //I /to)
 
 (@FC_Statement_List (reverse (@Cs //I)) /to))

; Process a sequence of statements: 
; First try to construct a block of statements with no CALLs or IFs, 
; starting at the end of the sequence. 
; If this can't be done, then process the last statement and try again. 
; NB: list contains the sequence of statements in reverse order. 
(define (@FC_Statement_List /list /to)
 (let ((/block '())
       (/node /to)
       (/stat '()))
  (while (and (not (null? /list)) (@FC_Block_OK? (car /list))) 
   (begin
    (set! /stat (car /list))
    (cond
     ((and (= (@ST /stat) //T_/Assignment) (= (@Size /stat) 1) (= (@ST (@Get_n (@Get_n /stat 1) 2)) //T_/Variable) (not (null? (gethash /skipped_vars (@V (@Get_n (@Get_n /stat 1) 2))))))
      #t)
     (#t
      (set! /block (cons /stat /block))))
    (set! /list (cdr /list))))
  (cond
   ((not (null? /block))
    ; Create a node for the block, use one line per statement 
    (set! /node (@FC_Multi_Block /block /to))))
  ; If there are any statements left, then process the first one: 
  (cond
   ((not (null? /list))
    (set! /node (@FC_Statement (car /list) /node))
    ; Process the rest of the list, if any: 
    (set! /node (@FC_Statement_List (cdr /list) /node))))
  /node))

; A simple basic block (expression, condition etc.) 
; This does nothing if the label would be empty (eg an empty expression list) 
(define (@FC_Block //I-par /to)
 (let ((//I-save //I)
       (/code (@FC_Code //I-par))
       (/comments (@FC_Comments //I-par))
       (/label "")
       (/node /to)
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((not (equal? /code ""))
    (set! /label /code))
   (#t
    (set! /label /comments)))
  (cond
   ((> (slength /label) //Max_/Box_/Chars)
    (set! /label (string-append (substr /label 0 (- //Max_/Box_/Chars 1)) "|"))))
  (cond
   ((not (equal? /label ""))
    (set! /node //Next_/Node)
    (set! //Next_/Node (+ //Next_/Node 1))
    (@FC_Node "box" /node /label /code /comments)
    (@FC_Edge "edge" "" /node /to)))
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

; A multi-line basic block, eg to summarise a sequence of statements: 
(define (@FC_Multi_Block /list /to)
 (let ((/code "")
       (/comments "")
       (/label "")
       (/node /to))
  ; Truncate the list, if necessary: 
  (cond
   ((> (gen-length /list) (+ //Max_/Box_/Lines1 //Max_/Box_/Lines2))
    (set! /list (concat (concat (@Sub_Seg /list 1 //Max_/Box_/Lines1) (list (@Make //T_/Comment "..." '()))) (@Final_Seg /list (+ (- (gen-length /list) //Max_/Box_/Lines2) 1))))))
  (for-in //I /list 
   (cond
    ((= (@ST //I) //T_/Skip)
     ; Ignore SKIP statements 
    )
    ((= (@ST //I) //T_/Comment)
     (set! /comments (@FC_Append /comments (@V //I) 0))
     (set! /label (@FC_Append /label (@V //I) //Max_/Box_/Chars)))
    (#t
     (set! /code (@FC_Append /code (@FC_Code //I) 0))
     (set! /comments (@FC_Append /comments (@FC_Comments //I) 0))
     (set! /label (@FC_Append /label (@FC_Code //I) //Max_/Box_/Chars)))))
  ; If the label is still empty, then don't generate a node: 
  (cond
   ((not (equal? /label ""))
    (set! /node //Next_/Node)
    (set! //Next_/Node (+ //Next_/Node 1))
    (@FC_Node "box" /node /label /code /comments)
    (@FC_Edge "edge" "" /node /to)))
  /node))

; Append a string to the given string with a newline if necessary. 
; Trim the string before appending if max > 0 
(define (@FC_Append /str /new /max)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R "")
  (cond
   ((> /max 0)
    (cond
     ((> (slength /new) /max)
      (set! /new (string-append (substr /new 0 (- /max 1)) "|"))))))
  (cond
   ((and (not (equal? /str "")) (not (equal? /new "")))
    (set! //R (concat (concat /str //Newline) /new)))
   (#t
    (set! //R (concat /str /new))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

; Write a node to the file: 
; shape is either box, rhomb, ellipse or triangle: 
(define (@FC_Node /shape /title /label /code /comments)
 (set! /title (@FC_Protect /title))
 (set! /label (@FC_Protect /label))
 (set! /code (@FC_Protect /code))
 (set! /comments (@FC_Protect /comments))
 (@WL (concat (string-append (concat (string-append (string-append "node: { shape: " /shape) " title: ") /title) " label: ") /label))
 (@WL (string-append (concat (string-append (string-append "        info1: " /code) " info2: ") /comments) " }")))

; Write an edge to the file: 
; type is either edge, nearedge, bentnearedge or backedge. 
; If the target node is negative, then convert an edge to a backedge 
; (unless it is a bentnearedge) and make it thinner if Thin_Back_Edges = 1 
(define (@FC_Edge /type /label /source /target)
 (cond
  ((and (= //Runtime_/Flow 0) (or (equal? /source /dispatch_n) (equal? /target /dispatch_n)))
   ; Ignore branches to/from dispatch unless Runtime_Flow is set 
  )
  ((and (equal? /source /dispatch_n) (= (abs /target) 1))
   ; Ignore an edge from dispatch to the end 
  )
  (#t
   (@FC_Edge2 /type /label /source /target))))

(define (@FC_Edge2 /type /label /source /target)
 (let ((/class "")
       (/style "")
       (/thick ""))
  (cond
   ((< /target 0)
    (cond
     ((equal? /type "edge")
      (set! /type "backedge")))
    (set! /target (abs /target))
    (cond
     ((= //Thin_/Back_/Edges 1)
      (set! /thick " thickness: 1")))))
  (cond
   ((equal? /source /dispatch_n)
    (cond
     ((equal? /type "edge")
      (set! /type "backedge")))
    (set! /class " class: 2")
    (set! /thick " thickness: 1")
    (cond
     ((= //Dispatch_/Nodes 1)
      (set! /dispatch_node (+ /dispatch_node 1))
      (set! /source (string-append "d" (@String /dispatch_node)))
      (@FC_Node "triangle" /source "?" "" "")
      (set! /type "nearedge")
      (set! /label ""))))
   ((equal? /target /dispatch_n)
    (set! /class " class: 3")
    (set! /thick " thickness: 1")
    (cond
     ((= //Dispatch_/Nodes 1)
      (set! /dispatch_node (+ /dispatch_node 1))
      (set! /target (string-append "d" (@String /dispatch_node)))
      (@FC_Node "triangle" /target "?" "" "")
      (set! /type "nearedge")
      (set! /label ""))))
   ((= /target 1)
    (set! /target //Next_/Node)
    (set! //Next_/Node (+ //Next_/Node 1))
    (@FC_Node "ellipse" /target "end" "" ""))
   ((not (null? (gethash //Return_/Node /target)))
    (set! /target //Next_/Node)
    (set! //Next_/Node (+ //Next_/Node 1))
    (@FC_Node "ellipse" /target "return" "" "")))
  (cond
   ((not (equal? /label ""))
    (set! /label (string-append " label: " (@FC_Protect /label)))))
  (@WS (concat (string-append /type ": { sourcename: ") (@FC_Protect /source)))
  (@WS (string-append " targetname: " (@FC_Protect /target)))
  (@WS (concat (concat (concat /label /class) /style) /thick))
  (@WL " }")))

; Protect a string with backslashes and add quotes 
(define (@FC_Protect /str)
 
 (concat (concat //Quote (@WS_Replace //Quote (concat //Backslash //Quote) (@WS_Replace //Backslash (concat //Backslash //Backslash) (@String /str)))) //Quote))

; Return the WSL code for the given item as a string: 
(define (@FC_Code //I-par)
 (let ((//I-save //I)
       (funct-result '()))
  (set! //I //I-par)
  (@New_Program //I)
  (@Trans //T/R_/Delete_/All_/Comments "")
  (set! funct-result (@PP_1 (@Program) 0))
  (set! //I //I-save)
  funct-result))

(define (@FC_Comments //I-par)
 (let ((//I-save //I)
       (//R-save //R)
       (funct-result '()))
  (set! //I //I-par)
  (set! //R '())
  (@New_Program //I)
  (@Foreach_Statement /foreach-flowchart-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result (@Join //Newline (reverse //R)))
  (set! //I //I-save)
  (set! //R //R-save)
  funct-result))

; Check if a statement is suitable for adding to a basic block 
; ie no IF statements, exits, action calls, loops etc.: 
; A proc call must go in a separate block 
(define (@FC_Block_OK? //I)
 
 (null? (intersection-n (@Stat_Types //I) (@Make_Set (list //T_/Cond //T_/D_/If //T_/While //T_/For //T_/Floop //T_/D_/Do //T_/Call //T_/A_/S //T_/Exit //T_/Var //T_/Where //T_/Proc_/Call)))))

; An action system starts by calling the first action. 
; We set up a table giving node numbers for each action, 
; including Z which is mapped to `to' 
; To decide if a call is a backedge we need to know the action name or number 
; for the current action: this is stored in the global variable Current_Action. 
; We process all the action bodies in turn and return the node for the start action. 
(define (@FC_A_S //I-par /to)
 (let ((//I-save //I)
       (//N (@Size (@Get_n //I-par 2)))
       (/node /to)
       (funct-result '()))
  (set! //I //I-par)
  (let ((//A/S_/Type (@System_Type //I))
        (//Bodies (make-vector-eval //N '()))
        (//Names (make-vector-eval (+ //N 1) '()))
        (//Nodes-save //Nodes)
        (//Name2/Num-save //Name2/Num)
        (/dispatch_n-save /dispatch_n))
   (set! //Nodes (make-vector-eval (+ //N 1) '()))
   (set! //Name2/Num (hash-table))
   (set! /dispatch_n (- 1))
   ; Calculate Bodies, Names, Name2Num 
   ; Hash table Name2Num maps action names (keys) to action numbers 
   (@New_Program //I)
   (let ((/-result- (@FD_Init  //N //Bodies //Names //Name2/Num)))
    (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Names (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Name2/Num (car /-result-)) (set! /-result- (cdr /-result-)))
   ; Create a node for each action: 
   (for /i 1 //N 1 
    (begin
     (wsl-set! //Nodes //Next_/Node /i)
     (set! //Next_/Node (+ //Next_/Node 1))
     (cond
      ((and (equal? (wsl-ref //Names /i) /dispatch) (= //Dispatch_/Nodes 1))
       #t)
      (#t
       (@FC_Node "box" (wsl-ref //Nodes /i) (@N_String (wsl-ref //Names /i)) "" "")))))
   ; CALL Z passes control to the next node after the action system: 
   (wsl-set! //Nodes /to (+ //N 1))
   (cond
    ((not (null? (gethash //Name2/Num /dispatch)))
     (set! /dispatch_n (wsl-ref //Nodes (gethash //Name2/Num /dispatch))))
    (#t
     (set! /dispatch_n (- 1))))
   ; Flowchart each action (we know they are all connected, 
   ; since we simplified all action systems): 
   (for /i 1 //N 1 
    (begin
     (set! //Current_/Action /i)
     ; Special case for dispatch action: 
     (cond
      ((equal? (wsl-ref //Names /i) /dispatch)
       (@FC_Dispatch (wsl-ref //Nodes /i) (wsl-ref //Bodies /i)))
      (#t
       (set! /node (@FC_Statements (wsl-ref //Bodies /i) /to))
       ; Link the entry node for the action to its body: 
       (@FC_Edge "edge" "" (wsl-ref //Nodes /i) /node)))))
   ; Return the starting action node: 
   (set! /node (wsl-ref //Nodes (gethash //Name2/Num (@V (@Get_n //I 1)))))
   (set! //Nodes //Nodes-save)
   (set! //Name2/Num //Name2/Num-save)
   (set! /dispatch_n /dispatch_n-save))
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

; The dispatch action is a single node with a lot of backedges coming out. 
; (@FC_Edge ensures that edges from dispatch and not to end are backedges) 
(define (@FC_Dispatch /node /body)
 (for-in /call (@Calls /body) 
  (@FC_Edge "edge" "" /node (wsl-ref //Nodes (- (wsl-ref /call 1))))))

; To process a call, we simply return the called node for our predecessor 
; to link to. We negate it if the call is a backedge: 
(define (@FC_Call //I-par /to)
 (let ((//I-save //I)
       (/node (wsl-ref //Nodes (- (@V //I-par))))
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((and (not (null? (gethash //Name2/Num (@V //I)))) (<= (gethash //Name2/Num (@V //I)) //Current_/Action))
    (set! /node (- /node))))
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

(define (@FC_Cond //I /to)
 
 (@FC_Guarded_List (@Cs //I) /to))

; Process a list of guardeds: 
(define (@FC_Guarded_List /list /to)
 (let ((/node /to))
  (cond
   ((null? /list)
    (set! /node /to))
   ((null? (cdr /list))
    ; process the ELSE clause: 
    (set! /node (@FC_Statements (@Get_n (car /list) 2) /to)))
   (#t
    ; Generate a rhombus and link in the two arms: 
    (let ((/true_node (@FC_Statements (@Get_n (car /list) 2) /to))
          (/false_node (@FC_Guarded_List (cdr /list) /to)))
     (set! /node //Next_/Node)
     (set! //Next_/Node (+ //Next_/Node 1))
     (@FC_Test (@Get_n (car /list) 1) /node /true_node /false_node 0))))
  /node))

; Make a rhombus node containing the test with links to the given nodes, 
; type = 0 for an IF, 1 for a WHILE, 2 for a FOR loop: 
(define (@FC_Test /cond /node /true_node /false_node /type)
 (let ((/code (@FC_Code /cond))
       (/label ""))
  ; Chop off the DO SKIP OD which comes from a FOR node: 
  (cond
   ((@Ends_With? /code " DO SKIP OD")
    (set! /code (substr /code 0 (- (slength /code) 11)))))
  (cond
   ((> (slength /code) //Max_/Rhomb_/Chars)
    (set! /label (string-append (substr /code 0 (- //Max_/Rhomb_/Chars 1)) "|")))
   (#t
    (set! /label /code)))
  (@FC_Node "rhomb" /node /label /code "")
  (cond
   ((= /type 0)
    (@FC_Edge "bentnearedge" "Y" /node /true_node)
    (@FC_Edge "bentnearedge" "N" /node /false_node))
   ((= /type 1)
    (@FC_Edge "edge" "Y" /node /true_node)
    (@FC_Edge "bentnearedge" "N" /node /false_node))
   (#t
    (@FC_Edge "edge" "" /node /true_node)
    (@FC_Edge "bentnearedge" "done" /node /false_node)))))

(define (@FC_Floop //I-par /to)
 (let ((//I-save //I)
       (/node //Next_/Node)
       (/top 0)
       (funct-result '()))
  (set! //I //I-par)
  (set! //Next_/Node (+ //Next_/Node 1))
  ; Create a node for the top of the loop, this is the target node 
  ; for the loop body: 
  (@FC_Node "box" /node (string-append "FLOOP " (@String (+ (gen-length //Loop_/Exits) 1))) "" "")
  (set! //Loop_/Exits (cons /to //Loop_/Exits))
  (set! /top (@FC_Statements (@Get_n //I 1) (- /node)))
  (set! //Loop_/Exits (cdr //Loop_/Exits))
  ; Link the loop's node to the top of the body and return the node: 
  (@FC_Edge "edge" "" /node /top)
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

; An EXIT returns the appropriate loop exit node: 
(define (@FC_Exit //I-par /to)
 (let ((//I-save //I)
       (/exit 0)
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((null? //Loop_/Exits)
    (set! /exit 0))
   ((> (@V //I) (gen-length //Loop_/Exits))
    (set! /exit (last-1 //Loop_/Exits)))
   (#t
    (set! /exit (wsl-ref //Loop_/Exits (@V //I)))))
  (set! funct-result /exit)
  (set! //I //I-save)
  funct-result))

; Check for entry points. 
; We use the given node for the first entry point and return its entry. 
; We create separate entry and exit nodes for the other entry points: 
(define (@FC_D_If //I-par /to)
 (let ((//I-save //I)
       (/node /to)
       (/guards (@Cs //I-par))
       (/start 0)
       (/end 0)
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((and (not (null? /guards)) (@FC_Entry_Point? (@Get_n (@Get_n //I 1) 1)))
    (set! /node (@FC_Statements (@Get_n (car /guards) 2) /to))
    (for-in /guard (cdr /guards) 
     (cond
      ((@FC_Entry_Point? (@Get_n /guard 1))
       (set! /start //Next_/Node)
       (set! //Next_/Node (+ //Next_/Node 1))
       (@FC_Force_To_End /start)
       (set! /end //Next_/Node)
       (set! //Next_/Node (+ //Next_/Node 1))
       (@FC_Start_Subgraph (@V (@Get_n (@Get_n /guard 1) 2)) 1)
       (@FC_Node "ellipse" /start (@V (@Get_n (@Get_n /guard 1) 2)) "" "")
       (puthash //Return_/Node /end 1)
       (set! /end (@FC_Statements (@Get_n /guard 2) /end))
       (@FC_Edge "edge" "" /start /end)))))
   (#t
    (set! /node (@FC_Cond //I /to))))
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

; Tests for Entry_Point = string: 
(define (@FC_Entry_Point? /cond)
 
 (and (= (@ST /cond) //T_/Equal) (= (@ST (@Get_n /cond 1)) //T_/Variable) (equal? (@V (@Get_n /cond 1)) /entry_point)))

(define (@FC_Guardeds //I /to)
 
 (@FC_Cond //I /to))

; Note that abort doesn't link to the target node: 
(define (@FC_Abort //I-par /to)
 (let ((//I-save //I)
       (/node //Next_/Node)
       (funct-result '()))
  (set! //I //I-par)
  (set! //Next_/Node (+ //Next_/Node 1))
  (@FC_Node "ellipse" /node "ABORT" "ABORT" "ABORT causes an error")
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

(define (@FC_While //I-par /to)
 (let ((//I-save //I)
       (/test //Next_/Node)
       (/top 0)
       (funct-result '()))
  (set! //I //I-par)
  (set! //Next_/Node (+ //Next_/Node 1))
  ; Process the loop body: it exits by returning to the test node: 
  (set! /top (@FC_Statements (@Get_n //I 2) (- /test)))
  ; Now create the test node and link it to the body 
  (@FC_Test (@Get_n //I 1) /test /top /to 1)
  (set! funct-result /test)
  (set! //I //I-save)
  funct-result))

(define (@FC_For //I-par /to)
 (let ((//I-save //I)
       (/test //Next_/Node)
       (/top 0)
       (/for (@Make //T_/For (@V (@Get_n //I-par 1)) (list (@Get_n //I-par 2) (@Get_n //I-par 3) (@Get_n //I-par 4) (@Skips))))
       (funct-result '()))
  (set! //I //I-par)
  (set! //Next_/Node (+ //Next_/Node 1))
  ; Process the loop body: it exits by returning to the test node: 
  (set! /top (@FC_Statements (@Get_n //I 5) (- /test)))
  ; Now create the test node and link it to the body 
  (@FC_Test /for /test /top /to 2)
  (set! funct-result /test)
  (set! //I //I-save)
  funct-result))

(define (@FC_Var //I-par /to)
 (let ((//I-save //I)
       (/node 0)
       (funct-result '()))
  (set! //I //I-par)
  (set! /node (@FC_Statements (@Get_n //I 2) /to))
  (cond
   ((not-member /dispatch (@Assigned (@Get_n //I 1)))
    (set! /node (@FC_Block (@Get_n //I 1) /node))))
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

(define (@FC_Where //I-par /to)
 (let ((//I-save //I)
       (/node /to)
       (funct-result '()))
  (set! //I //I-par)
  (set! /node (@FC_Statements (@Get_n //I 1) /node))
  (set! /node (@FC_Definitions (@Get_n //I 2) /node))
  (set! funct-result /node)
  (set! //I //I-save)
  funct-result))

; Each definition is a free-standing flowchart on its own, so ignore the given nodes: 
(define (@FC_Definition //I-par /to)
 (let ((//I-save //I)
       (/start //Next_/Node)
       (/end (+ //Next_/Node 1))
       (/comments "")
       (funct-result '()))
  (set! //I //I-par)
  (@FC_Force_To_End /start)
  (set! //Next_/Node (+ //Next_/Node 2))
  (cond
   ((= (@ST //I) //T_/Proc)
    (set! /comments "Procedure definition"))
   (#t
    (set! /comments "Function definition")))
  (@FC_Start_Subgraph (@V (@Get_n //I 1)) 1)
  (@FC_Node "ellipse" /start (@N_String (@V (@Get_n //I 1))) "" /comments)
  (puthash //Return_/Node /end 1)
  (for-in /comp (reverse (cdr (@Cs //I))) 
   (set! /end (@FC_Generic /comp /end)))
  (@FC_Edge "edge" "" /start /end)
  (set! funct-result /to)
  (set! //I //I-save)
  funct-result))

(define (@FC_Definitions //I-par /to)
 (let ((//I-save //I)
       (/dummy 0)
       (funct-result '()))
  (set! //I //I-par)
  (for-in /comp (@Cs //I) 
   (set! /dummy (@FC_Definition /comp /to)))
  (set! funct-result /to)
  (set! //I //I-save)
  funct-result))

(define (@FC_Guarded //I /to)
 (display-list "ERROR: bare Guarded item encountered!")
 /to)

(define (@FC_Action //I /to)
 (display-list "ERROR: bare Action encountered!")
 /to)

(define (@FC_Actions //I /to)
 (display-list "ERROR: bare Actions encountered!")
 /to)

; Create invisible, priority 0 edges which force this node to appear 
; after all the previous nodes: 
(define (@FC_Force_To_End_ /node)
 (let ((/target (string-append " targetname: " (@FC_Protect (abs /node)))))
  (set! /target (string-append /target " linestyle: invisible priority: 0 }"))
  (for /i //End_/Node (- /node 1) 1 
   (cond
    ((null? (gethash //Return_/Node /i))
     (@WL (concat (string-append "edge: { sourcename: " (@FC_Protect /i)) /target)))))
  (set! //End_/Node /node)))

(define (@FC_Force_To_End /node)
 #t
 #t)

(define (@FC_Initialise)
 (@WL (concat (string-append (string-append "graph: { title: " //Quote) "Flowchart") //Quote))
 (@WL "

  width: 1142
  height: 930
  x: 0 
  y: 0

  manhatten_edges: yes
  smanhatten_edges: no
  layoutalgorithm: minbackward
  // layoutalgorithm: dfs
  port_sharing: yes
  edge.thickness: 4
  foldnode.textcolor: red

  color: white
  xmax: 1200
  ymax: 1024
  xbase: 5
  ybase: 5
  xspace: 20
  xlspace: 12
  yspace: 50
  xraster: 1
  xlraster: 1
  yraster: 1
  shrink:  1
  stretch: 1
  layout_downfactor: 100
  layout_upfactor: 1
  layout_nearfactor: 0
  layout_splinefactor: 70
  spreadlevel: 1
  treefactor: 0.500000
  bmax: 50
  cmin: 0
  cmax: 10
  pmin: 0
  pmax: 20
  rmin: 0
  rmax: 30
  smax: 50
  node_alignment: center
  orientation: top_to_bottom
  late_edge_labels: no
  display_edge_labels: yes
  dirty_edge_labels: yes
  finetuning: yes
  nearedges: yes
  splines: no
  ignoresingles: no
  straight_phase: yes
  priority_phase: yes
  crossingphase2: yes
  crossingoptimization: yes
  crossingweight: barymedian
  arrow_mode: free
  colorentry 7 :  85 85 85
  colorentry 8 :  0 0 128
  colorentry 9 :  128 0 0
  colorentry 10 :  0 128 0
  colorentry 11 :  128 128 0
  colorentry 12 :  128 0 128
  colorentry 13 :  0 128 128
  colorentry 14 :  255 215 0
  colorentry 15 :  170 170 170
  colorentry 16 :  128 128 255
  colorentry 17 :  255 128 128
  colorentry 18 :  128 255 128
  colorentry 19 :  255 255 128
  colorentry 20 :  255 128 255
  colorentry 21 :  128 255 255
  colorentry 22 :  238 130 238
  colorentry 23 :  64 224 208
  colorentry 24 :  127 255 212
  colorentry 25 :  240 230 140
  colorentry 26 :  160 32 240
  colorentry 27 :  154 205 50
  colorentry 28 :  255 192 203
  colorentry 29 :  255 165 0
  colorentry 30 :  218 112 214

")
 (let ((/classes (list "normal" "from dispatch" "to dispatch")))
  (for /i 1 3 1 
   (@WL (concat (string-append (string-append "  classname " (@String /i)) " : ") (@FC_Protect (wsl-ref /classes /i))))))
 (@WL "")
 (let ((/names (list "WSL" "comments" "assembler")))
  (for /i 1 3 1 
   (@WL (concat (string-append (string-append "  infoname " (@String /i)) " : ") (@FC_Protect (wsl-ref /names /i))))))
 (@WL ""))

(define (@FC_Finalise)
 (@WL "")
 (@WL "}"))

(define (@FC_Start_Subgraph_ /title /fold)
 ; End a current subgraph if necessary: 
 (cond
  ((= //Subgraph 1)
   (@FC_End_Subgraph)))
 (set! //Subgraph 1)
 (set! //Subgraph_/N (+ //Subgraph_/N 1))
 (@WL "")
 (@WL (concat (string-append (string-append "graph: { title: " (@FC_Protect /title)) " folding: ") (@String /fold)))
 (@WL (string-append "  horizontal_order: " (@String //Subgraph_/N)))
 (@WL ""))

(define (@FC_End_Subgraph_)
 (@WL "}")
 (@WL ""))

(define (@FC_Start_Subgraph /title /fold)
 (@WL ""))

(define (@FC_End_Subgraph)
 (@WL ""))

; Read the options file into a hash table and return it 
(define (@Read_Options_File /file)
 (let ((//Options (hash-table))
       (/port (@Open_Input_File /file))
       (/line '())
       (/var "")
       (/n 0)
       (/m 0)
       (/space (string-append " =" //Tab))
       (/alphanum "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_")
       (/num "0123456789"))
  (display-list "Reading options file: " /file)
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (set! /line (@Read_Line /port))
    (cond
     ((@EOF? /line)
      (set! /fl_flag1 1))
     ((and (not (equal? /line "")) (not (equal? (substr /line 0 1) "#")))
      ; parse line as var = value 
      (set! /n 0)
      (while (and (not (@Char_In_Str? /line /n /space)) (@Char_In_Str? /line /n /alphanum) (not (= /n (string-length /line)))) 
       (set! /n (+ /n 1)))
      (cond
       ((and (not (@Char_In_Str? /line /n /alphanum)) (not (@Char_In_Str? /line /n /space)))
        (display-list "ERROR 1 in options file, n = " /n)
        (display-list /line)
        (error "@Read_Options_File:")
        (set! /fl_flag1 1))
       (#t
        (set! /var (substr /line 0 /n))
        (while (@Char_In_Str? /line /n /space) 
         (set! /n (+ /n 1)))
        (cond
         ((>= /n (string-length /line))
          ; Skip this blank line 
          (set! /fl_flag1 0))
         ((equal? (substr /line /n 1) //Quote)
          ; Check for string or number: 
          (set! /n (+ /n 1))
          (set! /m /n)
          (while (equal? (substr /line (- (string-length /line) 1) 1) //Backslash) 
           (begin
            (set! /line (substr /line 0 (- (string-length /line) 1)))
            (set! /line (concat /line (@Read_Line /port)))))
          (while (and (< /m (- (string-length /line) 1)) (not (equal? (substr /line /m 1) //Quote))) 
           (set! /m (+ /m 1)))
          (cond
           ((not (equal? (substr /line /m 1) //Quote))
            (display-list "ERROR 3 in options file:")
            (display-list /line)
            (error "@Read_Options_File:")
            (set! /fl_flag1 1))
           (#t
            (cond
             ((null? (gethash //Options /var))
              (puthash //Options /var (substr /line /n (- /m /n))))
             (#t
              (puthash //Options /var (concat (string-append (gethash //Options /var) " ") (substr /line /n (- /m /n))))))
            (cond
             (#f
              (display-list "Options.(" /var ") = " //Quote (gethash //Options /var) //Quote)
              (set! /fl_flag1 0))
             (#t
              (set! /fl_flag1 0))))))
         ((@Char_In_Str? /line /n /num)
          (set! /m /n)
          (while (@Char_In_Str? /line /m /num) 
           (set! /m (+ /m 1)))
          (puthash //Options /var (@String_To_Num (substr /line /n (- /m /n))))
          (cond
           (#f
            (display-list "Options.(" /var ") = " (gethash //Options /var))
            (set! /fl_flag1 0))
           (#t
            (set! /fl_flag1 0))))
         (#t
          (display-list "ERROR 4 in options file:")
          (display-list /line)
          (error "@Read_Options_File:")
          (set! /fl_flag1 1))))))
     (#t
      (set! /fl_flag1 0)))))
  //Options))

; ----------------------------------------------------------------------- 

