;;; Scheme translation of WSL code
(define (/foreach-metrics-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Down_To 2)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((= (@ST (@I)) //T_/Proc)
       (set! //Name (@V (@Get_n (@I) 1)))
       (cond
        ((> (@Stat_Count (@I)) /bignum)
         (set! /bignum (@Stat_Count (@I)))
         (set! /bigname //Name)))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))))

(define (/foreach-metrics-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/S)
   (set! /size (@Size (@Get_n (@I) 2)))
   (set! /total (+ /total /size))
   (set! //R (+ //R (* (@Effective_Size (@I)) /size))))
  ((= (@ST (@I)) //T_/Where)
   (set! /size (@Size (@Get_n (@I) 2)))
   (set! /total (+ /total /size))
   (set! //R (+ //R (* (@Effective_Size (@I)) /size))))))

(define (/foreach-metrics-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   ; to first assign 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (set! /total (+ /total 1))
     (cond
      ((@Set_Subset? (@Used (@Get_n (@I) 2)) //Constants)
       (set! /c1 (+ /c1 1))))
     (cond
      ((null? (intersection-n (@Used (@Get_n (@I) 2)) //Assigned))
       (set! /c2 (+ /c2 1))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))))

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
; ----------------------------------------------------------------------- 
; Metric calculation functions:                                           
;                                                                         
;      @Stat_Types - returns the types of statements used.                
;                                                                         
;      @Total_Size - returns the total number of nodes.                   
;                                                                         
;      @Stat_Count - returns the number of statements.                    
;                                                                         
;  @Gen_Type_Count - Returns the number of times that a certain generic   
;                    type appears.                                        
;                                                                         
; @Spec_Type_Count - returns the number of times that a certain specific  
;                    type appears.                                        
;                                                                         
;          @McCabe - returns the McCabe cycolmatic complexity measure.    
;                                                                         
;       @Essential - returns the McCabe essential complexity measure      
;                                                                         
;     @CFDF_Metric - returns the control-flow / data-flow metric.         
;                                                                         
;       @BL_Metric - returns the branch-loop metric.                      
;                                                                         
;   @Struct_Metric - returns the structural complexity metric.            
;                     -- a weighted sum over all nodes                    
;                                                                         
; ----------------------------------------------------------------------- 
(set! //Qry_/Statements (@Make_Name "Qry_Statements"))
(set! //Qry_/Total_/Size (@Make_Name "Qry_Total_Size"))
(set! //Qry_/Stat_/Count (@Make_Name "Qry_Stat_Count"))
(set! //Qry_/Stat_/Count_/N/C (@Make_Name "Qry_Stat_Count_NC"))
(set! //Qry_/Mc/Cabe_/Metric (@Make_Name "Qry_McCabe_Metric"))
(set! //Qry_/C/F/D/F_/Metric (@Make_Name "Qry_CFDF_Metric"))
(set! //Qry_/B/L_/Metric (@Make_Name "Qry_BL_Metric"))
(set! //Qry_/Struct_/Metric (@Make_Name "Qry_Struct_Metric"))
(set! //Qry_/Spec_/Types (@Make_Name "Qry_Spec_Types"))
(set! //B/U/G/F/I/X "                                                             ")
(define (@Stat_Types //I)
 (let ((//Result '()))
  (cond
   ((and (@Has_Statements_Type? (@GT //I)) (@Cs? //I))
    (let ((//Prev (@Dtable_Get //I //Qry_/Statements))
          (//Comps (@Cs //I)))
     (cond
      ((null? //Prev)
       (cond
        ((= (@GT //I) //T_/Statement)
         (set! //Result (list (@ST //I)))))
       (set! //Result (union-n //Result (my-reduce @Set_Union (my-map @Stat_Types //Comps))))
       (@Dtable_Put //I //Qry_/Statements //Result))
      (#t
       (set! //Result (@Dtable_Value_Part //Prev))))))
   (#t
    (set! //Result (if (= (@GT //I) //T_/Statement) (list (@ST //I)) '()))))
  //Result))

; Return the list of specific types found in the item: 
(define (@Spec_Types //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R '())
  (cond
   ((@Cs? //I)
    (let ((//Prev (@Dtable_Get //I //Qry_/Spec_/Types)))
     (cond
      ((null? //Prev)
       (set! //R (union-n (list (@ST //I)) (my-reduce @Set_Union (my-map @Spec_Types (@Cs //I)))))
       (@Dtable_Put //I //Qry_/Spec_/Types //R))
      (#t
       (set! //R (@Dtable_Value_Part //Prev))))))
   (#t
    (set! //R (list (@ST //I)))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@Total_Size //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R 1)
  (cond
   ((@Cs? //I)
    (let ((//Prev (@Dtable_Get //I //Qry_/Total_/Size))
          (//Comps (@Cs //I)))
     (cond
      ((null? //Prev)
       (set! //R (+ //R (my-reduce + (my-map @Total_Size //Comps))))
       (@Dtable_Put //I //Qry_/Total_/Size //R))
      (#t
       (set! //R (@Dtable_Value_Part //Prev)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@Stat_Count //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R (if (= (@GT //I) //T_/Statement) 1 0))
  (cond
   ((and (@Has_Statements_Type? (@GT //I)) (@Cs? //I))
    (let ((//Prev (@Dtable_Get //I //Qry_/Stat_/Count))
          (//Comps (@Cs //I)))
     (cond
      ((null? //Prev)
       (set! //R (+ //R (my-reduce + (my-map @Stat_Count //Comps))))
       (@Dtable_Put //I //Qry_/Stat_/Count //R))
      (#t
       (set! //R (@Dtable_Value_Part //Prev)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@Stat_Count_NC //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R (if (and (= (@GT //I) //T_/Statement) (not (= (@ST //I) //T_/Comment))) 1 0))
  (cond
   ((and (@Has_Statements_Type? (@GT //I)) (@Cs? //I))
    (let ((//Prev (@Dtable_Get //I //Qry_/Stat_/Count_/N/C))
          (//Comps (@Cs //I)))
     (cond
      ((null? //Prev)
       (set! //R (+ //R (my-reduce + (my-map @Stat_Count_NC //Comps))))
       (@Dtable_Put //I //Qry_/Stat_/Count_/N/C //R))
      (#t
       (set! //R (@Dtable_Value_Part //Prev)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@Gen_Type_Count //Type //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R (if (= (@GT //I) //Type) 1 0))
  (cond
   ((@Cs? //I)
    (let ((//Comps (@Cs //I)))
     (while (not (null? //Comps)) 
      (begin
       (set! //R (+ //R (@Gen_Type_Count //Type (car //Comps))))
       (set! //Comps (cdr //Comps)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@Spec_Type_Count //Type //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R (if (= (@ST //I) //Type) 1 0))
  (cond
   ((@Cs? //I)
    (let ((//Comps (@Cs //I)))
     (while (not (null? //Comps)) 
      (begin
       (set! //R (+ //R (@Spec_Type_Count //Type (car //Comps))))
       (set! //Comps (cdr //Comps)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

; McCabe Cyclomatic Complexity 
; Measures the number of `basic paths' through the program: 
; this is the number of different paths which taken in combination 
; will generate all possible paths. 
; v(G) = edges - nodes + connected_components 
; In a strongly connected graph, v(G) is the maximum number 
; of linearly independent circuits. 
; To make the components of a cfg strongly connected we need to add 
; an extra edge from the exit node of each conmponent to the entry node. 
; This adds an extra edge per connected component. So, for a cfg we have: 
; v(G) = edges - nodes + 2 * connected_components 
; For a simple graph (one component) this is the number of predicates plus 1 
; It is also equal to the number of separate regions in the flowgraph 
; For a complex graph, add the complexity of each component to get the total. 
; See mccabe.pdf 
; Each separate PROC, FUNCT or BFUNCT body is a separate component 
; as is the main program. 
(define (@McCabe //I)
 
 (+ (@McCabe_Sub //I) 1))

(define (@McCabe_Sub //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R 0)
  (cond
   ((= (@ST //I) //T_/For)
    (set! //R 1))
   ((or (= (@ST //I) //T_/Proc) (= (@ST //I) //T_/Funct) (= (@ST //I) //T_/B/Funct) (= (@ST //I) //T_/M/W_/Proc) (= (@ST //I) //T_/M/W_/Funct) (= (@ST //I) //T_/M/W_/B/Funct))
    (set! //R 1))
   ((= (@ST //I) //T_/D_/If)
    (set! //R (- 1))))
  (cond
   ((= (@GT //I) //T_/Condition)
    (cond
     ((or (= (@ST //I) //T_/True) (= (@ST //I) //T_/False))
      (set! //R 0))
     (#t
      (set! //R 1))))
   ((= (@GT //I) //T_/Expression)
    (set! //R 0))
   ((or (= (@ST //I) //T_/Assert) (= (@ST //I) //T_/Spec))
    (set! //R 0))
   ((@Cs? //I)
    (let ((//Prev (@Dtable_Get //I //Qry_/Mc/Cabe_/Metric))
          (//Comps (@Cs //I)))
     (cond
      ((null? //Prev)
       (set! //R (+ //R (my-reduce + (my-map @McCabe_Sub //Comps))))
       (@Dtable_Put //I //Qry_/Mc/Cabe_/Metric //R))
      (#t
       (set! //R (@Dtable_Value_Part //Prev)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@Essential //I)
 (let ((/block_file "tmp_ess.bb")
       (/ess_file "tmp_ess.ess")
       (/result "tmp_ess.txt")
       (/prefix (@String (@Random //Omega)))
       (/port '())
       (/line "")
       (/p 0)
       (//R-save //R)
       (funct-result '()))
  (set! //R 0)
  ; Add a random prefix to filenames to avoid race condition clashes: 
  (set! /block_file (concat (string-append /prefix "_") /block_file))
  (set! /ess_file (concat (string-append /prefix "_") /ess_file))
  (set! /result (concat (string-append /prefix "_") /result))
  (@Basic_Blocks //I /block_file)
  (perlscript "bbtoess" (concat (string-append /block_file " ") /ess_file))
  (perlscript "bbmccabe" (concat (string-append /ess_file " ") /result))
  (set! /port (@Open_Input_File /result))
  (let ((/-result- (@Read_Line_Proc  /line /port)))
   (set! /line (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /port (car /-result-)) (set! /-result- (cdr /-result-)))
  (@Close_Input_Port /port)
  (@Delete_File /block_file)
  (@Delete_File /ess_file)
  (@Delete_File /result)
  (cond
   ((< (string-length /line) 22)
    (error (string-append "@Essential: failed: " /line))))
  (set! /p (- (string-length /line) 1))
  (while (@Digit? (substr /line /p 1)) 
   (set! /p (- /p 1)))
  (set! //R (@String_To_Num (substr /line (+ /p 1))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@CFDF_Metric //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R 0)
  (cond
   ((or (or (= (@ST //I) //T_/Variable) (= (@ST //I) //T_/Aref)) (= (@GT //I) //T_/Lvalue))
    (set! //R 1))
   ((or (= (@ST //I) //T_/A_/Proc_/Call) (= (@ST //I) //T_/M/W_/Proc_/Call) (= (@ST //I) //T_/X_/Proc_/Call) (= (@ST //I) //T_/Call) (= (@ST //I) //T_/Proc_/Call))
    (set! //R 1))
   ((@Cs? //I)
    (let ((//Prev (@Dtable_Get //I //Qry_/C/F/D/F_/Metric))
          (//Comps (@Cs //I)))
     (cond
      ((null? //Prev)
       (set! //R (my-reduce + (my-map @CFDF_Metric //Comps)))
       (@Dtable_Put //I //Qry_/C/F/D/F_/Metric //R))
      (#t
       (set! //R (@Dtable_Value_Part //Prev)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@BL_Metric //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R 0)
  (cond
   ((or (= (@ST //I) //T_/Floop) (= (@ST //I) //T_/While) (= (@ST //I) //T_/For) (= (@ST //I) //T_/D_/Do))
    (set! //R 1))
   ((or (= (@ST //I) //T_/A_/Proc_/Call) (= (@ST //I) //T_/M/W_/Proc_/Call) (= (@ST //I) //T_/X_/Proc_/Call) (= (@ST //I) //T_/Call) (= (@ST //I) //T_/Proc_/Call))
    (set! //R 1))
   ((@Cs? //I)
    (let ((//Prev (@Dtable_Get //I //Qry_/B/L_/Metric))
          (//Comps (@Cs //I)))
     (cond
      ((null? //Prev)
       (set! //R (my-reduce + (my-map @BL_Metric //Comps)))
       (@Dtable_Put //I //Qry_/B/L_/Metric //R))
      (#t
       (set! //R (@Dtable_Value_Part //Prev)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@Struct_Metric //I)
 (let ((//R-save //R)
       (//Prev (@Dtable_Get //I //Qry_/Struct_/Metric))
       (funct-result '()))
  (set! //R 0)
  (cond
   ((null? //Prev)
    (cond
     ((or (= (@ST //I) //T_/Plus) (= (@ST //I) //T_/Times) (= (@ST //I) //T_/Variable) (= (@ST //I) //T_/Var_/Lvalue))
      (set! //R 1))
     ((or (= (@ST //I) //T_/Minus) (= (@ST //I) //T_/Divide) (= (@ST //I) //T_/Exponent) (= (@ST //I) //T_/Div) (= (@ST //I) //T_/Mod) (= (@ST //I) //T_/Abs) (= (@ST //I) //T_/Sgn) (= (@ST //I) //T_/Frac) (= (@ST //I) //T_/Int) (= (@ST //I) //T_/And) (= (@ST //I) //T_/Or))
      (set! //R 2))
     ((= (@GT //I) //T_/Expression)
      (set! //R 4))
     ((= (@GT //I) //T_/Assign)
      (set! //R 4))
     ((and (= (@ST //I) //T_/Cond) (= (@Size (@Get_n (@Get_n //I (@Size //I)) 2)) 1) (= (@ST (@Get_n (@Get_n (@Get_n //I (@Size //I)) 2) 1)) //T_/Skip))
      (set! //R (* 5 (- (@Size //I) 1))))
     ((or (= (@ST //I) //T_/Cond) (= (@ST //I) //T_/D_/If) (= (@ST //I) //T_/D_/Do))
      (set! //R (* 5 (@Size //I))))
     ((= (@ST //I) //T_/Call)
      (set! //R 20))
     ((= (@ST //I) //T_/A_/Proc_/Call)
      (set! //R 17))
     ((= (@ST //I) //T_/Proc_/Call)
      (set! //R 17))
     ((= (@ST //I) //T_/Exit)
      (set! //R (* (+ 8 (@V //I)) (@V //I))))
     ((= (@ST //I) //T_/Skip)
      (set! //R 1))
     ((= (@ST //I) //T_/Var)
      (set! //R (+ 8 (@Size (@Get_n //I 2)))))
     ((and (= (@GT //I) //T_/Statements) (= (@ST (@Get_n //I (@Size //I))) //T_/Exit))
      (set! //R (@Size //I)))
     ((= (@GT //I) //T_/Statement)
      (set! //R 4)))
    (cond
     ((@Cs? //I)
      (set! //R (+ //R (my-reduce + (my-map @Struct_Metric (@Components //I)))))))
    (@Dtable_Put //I //Qry_/Struct_/Metric //R))
   (#t
    (set! //R (@Dtable_Value_Part //Prev))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@If_Nest //I)
 
 (if (not (and (@Has_Statements_Type? (@GT //I)) (@Cs? //I))) 0 (if (= (@ST //I) //T_/Cond) (+ 1 (my-reduce MAX (my-map @If_Nest (@Cs //I)))) (+ 0 (my-reduce MAX (my-map @If_Nest (@Cs //I)))))))

(define (@Big_Proc //Data)
 (let ((/bignum-save /bignum)
       (/bigname-save /bigname)
       (funct-result '()))
  (set! /bignum 0)
  (set! /bigname "None")
  (while (@Up?) 
   (@Up))
  (@Foreach_Statement /foreach-metrics-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result (list /bignum /bigname))
  (set! /bignum /bignum-save)
  (set! /bigname /bigname-save)
  funct-result))

; Calculate the effective size of a WHERE clause or action system 
; Depth-first search of the call graph, ignoring recursion. 
; Let M be the set of modules (actions or procs+functions). Then: 
; ES = ESM(root, M  {root}) / #M 
; where ESM(x, M) (the effective size in modules) is defined: 
; ESM(x, M) = 1 + SUM_{y in M is called by x} ESM(y, M  {y}) 
;               + #{y is not in M and is called by x} 
; (note that for a recursive call, we don't look any deeper) 
; The hash table call_graph contains the list of <call, freq> pairs 
; for each node, while the table effective_size records the ESM values 
; for each node as they are calculated (to avoid recalculating). 
(define (@Effective_Size //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R 0)
  (cond
   ((= (@ST //I) //T_/A_/S)
    (let ((/call_graph-save /call_graph)
          (/effective_size-save /effective_size)
          (//M-save //M)
          (//Msize 0)
          (/root (@V (@Get_n //I 1)))
          (/actions (@Cs (@Get_n //I 2))))
     (set! /call_graph (hash-table))
     (set! /effective_size (hash-table))
     (set! //M (hash-table))
     ; Build the action call graph: 
     (while (not (null? /actions)) 
      (begin
       (puthash //M (@V (@Get_n (car /actions) 1)) 1)
       (set! //Msize (+ //Msize 1))
       (puthash /call_graph (@V (@Get_n (car /actions) 1)) (@Calls (car /actions)))
       (set! /actions (cdr /actions))))
     (cond
      ((= //Msize 0)
       (set! //R 0))
      (#t
       (puthash //M /root 0)
       (set! //R (/ (@ESM /root) //Msize))))
     (set! /call_graph /call_graph-save)
     (set! /effective_size /effective_size-save)
     (set! //M //M-save)))
   ((= (@ST //I) //T_/Where)
    (let ((/call_graph-save /call_graph)
          (/effective_size-save /effective_size)
          (//M-save //M)
          (//Msize 1)
          (/root "main")
          (/defns (@Cs (@Get_n //I 2))))
     (set! /call_graph (hash-table))
     (set! /effective_size (hash-table))
     (set! //M (hash-table))
     (puthash //M "main" 1)
     (puthash /call_graph "main" (concat (@Proc_Calls (@Get_n //I 1)) (@Funct_Calls (@Get_n //I 1))))
     ; Build the proc/funct call graph: 
     (while (not (null? /defns)) 
      (begin
       (puthash //M (@V (@Get_n (car /defns) 1)) 1)
       (set! //Msize (+ //Msize 1))
       (puthash /call_graph (@V (@Get_n (car /defns) 1)) (concat (@Proc_Calls (@Get_n (car /defns) 4)) (@Funct_Calls (@Get_n (car /defns) 4))))
       (set! /defns (cdr /defns))))
     (puthash //M /root 0)
     (set! //R (/ (@ESM /root) //Msize))
     (set! /call_graph /call_graph-save)
     (set! /effective_size /effective_size-save)
     (set! //M //M-save)))
   (#t
    (cond
     ((null? (intersection-n (list //T_/A_/S //T_/Where) (@Stat_Types //I)))
      (set! //R 1))
     (#t
      ; Count the total number of modules and the total ESM 
      ; for all action systems and WHERE clauses 
      (set! //R 0)
      (let ((/total-save /total)
            (/size-save /size))
       (set! /total 0)
       (set! /size 0)
       (@Edit)
       (@New_Program //I)
       (@Foreach_Statement /foreach-metrics-2 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))
       (@Undo_Edit)
       (cond
        ((> /total 0)
         (set! //R (/ //R /total))))
       (set! /total /total-save)
       (set! /size /size-save))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@ESM /x)
 (let ((//R (gethash /effective_size /x))
       (/y '()))
  (cond
   ((null? //R)
    (let ((/pairs (gethash /call_graph /x))
          (/y '()))
     (set! //R 1)
     (while (not (null? /pairs)) 
      (begin
       (set! /y (car (car /pairs)))
       (cond
        ((and (not (null? (gethash //M /y))) (= (gethash //M /y) 1))
         (puthash //M /y 0)
         (set! //R (+ //R (@ESM /y)))
         (puthash //M /y 1))
        (#t
         (set! //R (+ //R 1))))
       (set! /pairs (cdr /pairs))))
     (puthash /effective_size /x //R))))
  //R))

; Write an action or procedure call graph to the given file 
; Format is: 
; source: target1 n1 target2 n2 ... 
(define (@Call_Graph //I //Filename)
 (let ((/port '())
       (/main_list (my-map @Make_Name (list "MAIN" "Main" "main")))
       (/main '())
       (/calls '())
       (/n 1)
       (/action '())
       (/defn '()))
  (cond
   ((equal? //Filename "")
    (set! /port //Standard_/Output_/Port))
   (#t
    (set! /port (@Open_Output_File //Filename))))
  (cond
   ((= (@ST //I) //T_/A_/S)
    (for-in /action (@Cs (@Get_n //I 2)) 
     (@Write_CG_Line (@V (@Get_n /action 1)) (@Calls /action) /port)))
   ((= (@ST (@I)) //T_/Where)
    (set! /calls (union-n (my-map HEAD (@Proc_Calls (@Get_n //I 1))) (my-map HEAD (@Funct_Calls (@Get_n //I 1)))))
    (set! /main_list (@Set_Difference /main_list /calls))
    (cond
     ((null? /main_list)
      (set! /main (@Make_Name (string-append "main_" (@String /n))))
      (while (member /main /calls) 
       (begin
        (set! /n (+ /n 1))
        (set! /main (@Make_Name (string-append "main_" (@String /n)))))))
     (#t
      (set! /main (car /main_list))))
    (@Write_CG_Line /main (concat (@Proc_Calls (@Get_n //I 1)) (@Funct_Calls (@Get_n //I 1))) /port)
    (for-in /defn (@Cs (@Get_n //I 2)) 
     (@Write_CG_Line (@V (@Get_n /defn 1)) (concat (@Proc_Calls /defn) (@Funct_Calls /defn)) /port))))
  (@Close_Output_Port /port)))

; Write a line to a call graph file: 
(define (@Write_CG_Line /name /calls /port)
 (let ((/call '()))
  (@Write (@N_String /name) /port)
  (@Write ": " /port)
  (for-in /call /calls 
   (begin
    (@Write (@N_String (wsl-ref /call 1)) /port)
    (@Write " " /port)
    (@Write (@String (wsl-ref /call 2)) /port)
    (@Write " " /port)))
  (@Write_Line "" /port)))

; Count the total number of assignment statements 
; and the number of assignments where the value assigned is a constant 
(define (@Assignment_Stats //I)
 (let ((/total-save /total)
       (/c1-save /c1)
       (/c2-save /c2)
       (//Constants-save //Constants)
       (//Assigned-save //Assigned)
       (funct-result '()))
  (set! /total 0)
  (set! /c1 0)
  (set! /c2 0)
  (set! //Constants (@Set_Difference (@Used //I) (@Assigned //I)))
  (set! //Assigned (@Assigned //I))
  (@Edit)
  (@New_Program //I)
  (@Foreach_Statement /foreach-metrics-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Undo_Edit)
  (set! funct-result (list /c1 /c2 /total))
  (set! /total /total-save)
  (set! /c1 /c1-save)
  (set! /c2 /c2-save)
  (set! //Constants //Constants-save)
  (set! //Assigned //Assigned-save)
  funct-result))

; ----------------------------------------------------------------------- 

