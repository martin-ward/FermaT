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
; Complete rewrite of adt-procs.wsl 
; @Foreach_Statement(body, Depth, AS, simple) 
; @Foreach_Non_Action_Statement 
; @Foreach_Stats 
; @Foreach_Terminal 
; @Foreach_Terminal_Stats 
; @Foreach_Cond 
; @Foreach_Expn 
; @Foreach_Lvalue 
; @Foreach_Variable 
; @Foreach_Global_Var 
; Plus the @Ateach_ equivalent functions. 
; Record which generic types can have statements as components: 
(set! //Has_/Statements (make-vector-eval 1999 0))
(for-in /type (list //T_/Statement //T_/Definition //T_/Guarded //T_/Action //T_/Statements //T_/Definitions //T_/Actions //T_/Guarded) 
 (vector-set! //Has_/Statements (- /type 1) 1))
(define (@Has_Statements_Type? /type)
 
 (= (vector-ref //Has_/Statements (- /type 1)) 1))

(define (@Foreach_Statement /body //Depth //A/S /simple)
 (@Foreach_Gen_S /body //Depth //A/S /simple //T_/Statement 0 0))

(define (@Foreach_Non_Action_Statement /body //Depth //A/S /simple)
 (@Foreach_Gen_S /body //Depth //A/S /simple //T_/Statement 0 1))

(define (@Foreach_Stats /body //Depth //A/S /simple)
 (@Foreach_Gen_S /body //Depth //A/S /simple //T_/Statements 0 0))

(define (@Foreach_Terminal /body //Depth //A/S /simple)
 (@Foreach_Gen_S /body //Depth //A/S /simple //T_/Statement 1 0))

(define (@Foreach_Terminal_Stats /body //Depth //A/S /simple)
 (@Foreach_Gen_S /body //Depth //A/S /simple //T_/Statements 1 0))

(define (@Foreach_Gen_S /body //Depth //A/S /simple /req_/G/T /term //N_/A/S)
 (let ((/orig (@I))
       (/new '()))
  (@Edit)
  (set! /new (@Foreach_S (@I) 0 /body //Depth //A/S /simple /req_/G/T /term //N_/A/S))
  (@Undo_Edit)
  (cond
   ((null? /new)
    (@Clever_Delete))
   ((and (= (@GT /orig) //T_/Statement) (= (@GT /new) //T_/Statements))
    (cond
     ((> (@Size /new) 1)
      (cond
       ((@Up?)
        (@Splice_Over (@Cs /new)))
       (#t
        ; Convert program to a statement sequence 
        (@Paste_Over /new))))
     ((not (eq? /orig (@Get_n /new 1)))
      (@Paste_Over (@Get_n /new 1)))))
   ((not (eq? /orig /new))
    (@Paste_Over /new)))))

; A generic foreach for statements and statement sequences: 
; First process the components, rebuild I if any component has changed, 
; then call the body, if all the conditions are met. 
; If I is a statement, then may return a statement sequence. 
; Return the empty list if the item is deleted 
; Note: @Foreach is called inside an @Edit, so is allowed to clobber @Program  
; req_tv is the `required terminal value', the minimum terminal value 
; which will cause termination of the whole program. It increases with Depth 
; and is zero for the last statement in a terminal statement sequence 
; at depth zero, but one for all other statements. 
(define (@Foreach_S //I /req_tv /body //Depth //A/S /simple /req_/G/T /term //N_/A/S)
 (let ((//R //I)
       (//S/T (@ST //I))
       (//G/T (@GT //I)))
  (cond
   ((@Has_Statements_Type? //G/T)
    ; Process any components: 
    (let ((//Comps-save //Comps)
          (/old '())
          (/new/L '())
          (/new '())
          (/changed 0)
          (/sub_/A/S //A/S))
     (set! //Comps (@Components //I))
     (cond
      ((= //S/T //T_/Floop)
       (set! //Depth (+ //Depth 1))
       (set! /req_tv (+ /req_tv 1))))
     (cond
      ((= //S/T //T_/A_/S)
       (set! /sub_/A/S (@System_Type //I))))
     ; If simple = 1, don't process components of simple statements: 
     (cond
      ((and (= /simple 1) (@Simple? //I))
       (set! //Comps '()))
      ((or (@Ifmatch_Type? //S/T) (@Fill_Type? //S/T))
       ; Skip first component of IFMATCH or FILL: 
       (set! /new/L (list (car //Comps)))
       (set! //Comps (cdr //Comps))))
     (while (not (null? //Comps)) 
      (begin
       (set! /old (car //Comps))
       (set! //Comps (cdr //Comps))
       (cond
        ((and (= //N_/A/S 1) (= (@ST /old) //T_/A_/S))
         (set! /new/L (cons /old /new/L)))
        ((and (= /term 1) (not (@May_Term? /old /req_tv /sub_/A/S (if (null? //Comps) 1 0))))
         (set! /new/L (cons /old /new/L)))
        (#t
         (cond
          ((and (= /req_tv 0) (= //G/T //T_/Statements) (not (null? //Comps)))
           ; Need an EXIT for termination of this statement: 
           (set! /new (@Foreach_S /old 1 /body //Depth /sub_/A/S /simple /req_/G/T /term //N_/A/S)))
          (#t
           (set! /new (@Foreach_S /old /req_tv /body //Depth /sub_/A/S /simple /req_/G/T /term //N_/A/S))))
         ; Check if the component has changed: 
         (cond
          ((null? /new)
           (set! /changed 1))
          ((and (= //G/T //T_/Statements) (= (@GT /new) //T_/Statements))
           ; We passed a statement, but got a statement sequence: 
           (cond
            ((or (> (@Size /new) 1) (not (eq? /old (@Get_n /new 1))))
             (set! /changed 1)
             (set! /new/L (concat (reverse (@Cs /new)) /new/L)))
            (#t
             (set! /new/L (cons /old /new/L)))))
          ((not (eq? /old /new))
           (set! /changed 1)
           (set! /new/L (cons /new /new/L)))
          (#t
           (set! /new/L (cons /old /new/L))))))))
     (cond
      ((= //S/T //T_/Floop)
       (set! //Depth (- //Depth 1))
       (set! /req_tv (+ /req_tv 1))))
     ; Rebuild the item if any components have changed. 
     ; (Since there are components, there is no value to worry about): 
     ; Note that @Fix_Item may have to convert a statement to a stat seq: 
     (cond
      ((= /changed 1)
       (set! //I (@Fix_Item (@Make //S/T '() (reverse /new/L))))))
     ; Process this item if all the conditions are met: 
     (cond
      ((null? //I)
       (set! //R '()))
      ((or (not (= (@GT //I) /req_/G/T)) (and (= /simple 1) (not (@Simple? //I))))
       (set! //R //I))
      ((and (= /term 1) (not (@Will_Term? //I /req_tv //A/S)))
       (set! //R //I))
      (#t
       (cond
        ((= (@GT //I) //T_/Statement)
         (@New_Program (@Make //T_/Statements '() (list //I)))
         (@Down))
        (#t
         (@New_Program //I)))
       (apply /body (list //Depth //A/S))
       (set! //R (@Fix_Item (@Program)))))
     (set! //Comps //Comps-save))))
  //R))

(define (@Foreach_Cond /body //Depth //A/S /simple)
 (let ((/orig (@I))
       (/new '()))
  (@Edit)
  (set! /new (@Foreach_Gen (@I) /body //Depth //A/S //T_/Condition))
  (@Undo_Edit)
  (cond
   ((not (eq? /orig /new))
    (@Paste_Over /new)))))

(define (@Foreach_Expn /body //Depth //A/S /simple)
 (let ((/orig (@I))
       (/new '()))
  (@Edit)
  (set! /new (@Foreach_Gen (@I) /body //Depth //A/S //T_/Expression))
  (@Undo_Edit)
  (cond
   ((not (eq? /orig /new))
    (@Paste_Over /new)))))

(define (@Foreach_Lvalue /body //Depth //A/S /simple)
 (let ((/orig (@I))
       (/new '()))
  (@Edit)
  (set! /new (@Foreach_Gen (@I) /body //Depth //A/S //T_/Lvalue))
  (@Undo_Edit)
  (cond
   ((not (eq? /orig /new))
    (@Paste_Over /new)))))

; A generic foreach for all other types apart from variables. 
(define (@Foreach_Gen //I /body //Depth //A/S /req_/G/T)
 (let ((//R //I)
       (//S/T (@ST //I))
       (//G/T (@GT //I)))
  (cond
   ((and (= /req_/G/T //T_/Condition) (= //G/T //T_/Expression))
    ; An Expression cannot have a condition as a component 
   )
   (#t
    (let ((//Comps-save //Comps)
          (/old '())
          (/new/L '())
          (/new '())
          (/changed 0))
     (set! //Comps (@Components //I))
     (cond
      ((or (@Ifmatch_Type? //S/T) (@Fill_Type? //S/T))
       (set! /new/L (list (car //Comps)))
       (set! //Comps (cdr //Comps))))
     (while (not (null? //Comps)) 
      (begin
       (set! /old (car //Comps))
       (set! //Comps (cdr //Comps))
       (set! /new (@Foreach_Gen /old /body //Depth //A/S /req_/G/T))
       (cond
        ((null? /new)
         (set! /changed 1))
        ((not (eq? /old /new))
         (set! /changed 1)
         (set! /new/L (cons /new /new/L)))
        (#t
         (set! /new/L (cons /old /new/L))))))
     ; Rebuild the item if any components have changed. 
     (cond
      ((= /changed 1)
       (set! //I (@Make //S/T '() (reverse /new/L)))))
     ; Process this item if all the conditions are met: 
     (cond
      ((not (= (@GT //I) /req_/G/T))
       (set! //R //I))
      (#t
       (@New_Program //I)
       (apply /body (list //Depth //A/S))
       (set! //R (@Program))))
     (set! //Comps //Comps-save))))
  //R))

(define (@Foreach_Variable /body //Depth //A/S /simple)
 (let ((/orig (@I))
       (/new '()))
  (@Edit)
  (set! /new (@Foreach_Vars (@I) /body //Depth //A/S 0 0 '()))
  (@Undo_Edit)
  (cond
   ((not (eq? /orig /new))
    (@Paste_Over /new)))))

(define (@Foreach_Global_Var /body //Depth //A/S /simple)
 (let ((/orig (@I))
       (/new '()))
  (@Edit)
  (set! /new (@Foreach_Vars (@I) /body //Depth //A/S 0 1 '()))
  (@Undo_Edit)
  (cond
   ((not (eq? /orig /new))
    (@Paste_Over /new)))))

; A generic foreach for variables: 
(define (@Foreach_Vars //I /body //Depth //A/S /parent /global /vars)
 (let ((//R //I)
       (//S/T (@ST //I))
       (//G/T (@GT //I)))
  (let ((//Comps-save //Comps)
        (/old '())
        (/new/L '())
        (/new '())
        (/changed 0))
   (set! //Comps (@Components //I))
   (cond
    ((or (@Ifmatch_Type? //S/T) (@Fill_Type? //S/T))
     (set! /new/L (list (car //Comps)))
     (set! //Comps (cdr //Comps))))
   (while (not (null? //Comps)) 
    (begin
     (set! /old (car //Comps))
     (set! //Comps (cdr //Comps))
     (cond
      ((= /global 1)
       (cond
        ((and (or (= //S/T //T_/For) (= //S/T //T_/For_/In)) (null? //Comps))
         (set! /vars (union-n /vars (list (@V (@Get_n //I 1))))))
        ((or (= //S/T //T_/Proc) (= //S/T //T_/Funct) (= //S/T //T_/B/Funct))
         (set! /vars (union-n /vars (@Assigned (@Get_n //I 2)) (@Assigned (@Get_n //I 3)))))
        ((and (= //S/T //T_/Var) (null? //Comps))
         (set! /vars (union-n /vars (@Assigned (@Get_n //I 1))))))))
     ; In the assignments of a VAR, only process the expressions, if global = 1 
     ; Don't process the first component of a FOR if global = 1 
     ; Don't process formal parameters in proc/funct definitions. 
     (cond
      ((and (= /global 1) (= /parent //T_/Var) (= //G/T //T_/Assigns))
       (set! /new (@Foreach_Vars (@Get_n /old 2) /body //Depth //A/S //S/T /global /vars))
       (cond
        ((eq? (@Get_n /old 2) /new)
         (set! /new /old))
        (#t
         (set! /new (@Make //T_/Assign '() (list (@Get_n /old 1) /new))))))
      ((and (= /global 1) (or (= //S/T //T_/For) (= //S/T //T_/For_/In)) (= (@GT /old) //T_/Lvalue))
       (set! /new /old))
      (#t
       (set! /new (@Foreach_Vars /old /body //Depth //A/S //S/T /global /vars))))
     (cond
      ((null? /new)
       (set! /changed 1))
      ((not (eq? /old /new))
       (set! /changed 1)
       (set! /new/L (cons /new /new/L)))
      (#t
       (set! /new/L (cons /old /new/L))))))
   ; Rebuild the item if any components have changed. 
   (cond
    ((= /changed 1)
     (set! //I (@Make //S/T '() (reverse /new/L)))))
   ; Process this item if all the conditions are met: 
   (cond
    ((and (not (= //S/T //T_/Variable)) (not (= //S/T //T_/Var_/Lvalue)))
     (set! //R //I))
    ((and (= /global 1) (member (@V //I) /vars))
     (set! //R //I))
    (#t
     (@New_Program //I)
     (apply /body (list //Depth //A/S))
     (set! //R (@Program))
     ; Fix Lvalues which were replaced by variables: 
     (cond
      ((or (and (= (@ST //I) //T_/Variable) (= (@ST //R) //T_/Var_/Lvalue)) (and (= (@ST //I) //T_/Var_/Lvalue) (= (@ST //R) //T_/Variable)))
       (set! //R (@Make (@ST //I) (@V //R) '()))))))
   (set! //Comps //Comps-save))
  //R))

; Check the syntax of the (edited) item and repair or delete it as necessary: 
(define (@Fix_Item //I)
 (let ((//R //I)
       (//S/T (if (null? //I) '() (@ST //I))))
  (cond
   ((null? //I)
    #t)
   ((= //S/T //T_/Statements)
    ; Fix each component (if any): 
    (let ((/new '())
          (/new/L '())
          (/comp '())
          (/changed 0))
     (for-in /comp (@Cs //I) 
      (begin
       (set! /new (@Fix_Item /comp))
       (cond
        ((null? /new)
         (set! /changed 1))
        (#t
         (cond
          ((not (eq? /new /comp))
           (set! /changed 1)))
         (cond
          ((= (@GT /new) //T_/Statements)
           (set! /new/L (concat (reverse (@Cs /new)) /new/L)))
          (#t
           (set! /new/L (cons /new /new/L))))))))
     (cond
      ((null? /new/L)
       (set! //R '()))
      ((= /changed 1)
       (set! //R (@Make //T_/Statements '() (reverse /new/L)))))))
   ((= //S/T //T_/Cond)
    (set! //R (@Fix_Cond_Item //I)))
   ((or (= //S/T //T_/D_/If) (= //S/T //T_/D_/Do))
    (set! //R (@Fix_Dijkstra_Item //I)))
   ((= //S/T //T_/Guarded)
    ; Fix these in the enclosing statement 
   )
   ((or (and (not (@List_Type? //S/T)) (not (= (@Size //I) (gen-length (@Syntax //S/T))))) (and (@List_Type? //S/T) (not (@Cs? //I))))
    (cond
     ((= //S/T //T_/Where)
      (cond
       ((not (@Cs? //I))
        (set! //R '()))
       ((= (@GT (@Get_n //I 1)) //T_/Statements)
        (set! //R (@Get_n //I 1)))
       (#t
        (set! //R '()))))
     ((= //S/T //T_/While)
      (set! //R (@Make //T_/Assert '() (list (@Not (@Get_n //I 1))))))
     ((or (= //S/T //T_/D_/If) (= //S/T //T_/Floop))
      (set! //R (@Make //T_/Abort '() '())))
     ((= //S/T //T_/Join)
      (set! //R (@Get_n //I 1)))
     ((= (gen-length (@Syntax //S/T)) (+ (@Size //I) 1))
      (cond
       ((or (= //S/T //T_/For) (= //S/T //T_/For_/In) (= //S/T //T_/Var))
        (set! //R (@Skip)))
       ((= (last-1 (@Syntax //S/T)) //T_/Statements)
        ; eg IFMATCH or MW_PROC: 
        (set! //R (@Make //S/T '() (concat (@Cs //I) (list (@Skips))))))
       ((or (= //S/T //T_/M/W_/Funct) (= //S/T //T_/M/W_/B/Funct) (= //S/T //T_/Funct) (= //S/T //T_/B/Funct))
        (set! //R (@Make //S/T '() (concat (butlast-1 (@Cs //I)) (list (@Skips) (last-1 (@Cs //I)))))))
       (#t
        (set! //R '()))))
     (#t
      (set! //R '())))))
  //R))

(set! /adt_/Dummy_/Guarded (@Make //T_/Guarded '() (list (@Make //T_/True '() '()) (@Make //T_/Statements '() (list (@Make //T_/Skip '() '()))))))
(define (@Fix_Cond_Item //I)
 (let ((//R //I))
  (cond
   ((not (@Cs? //I))
    (set! //R '()))
   (#t
    ; Look for empty or short guards: 
    (let ((/guard '())
          (/found 0)
          (/last '()))
     (for-in /guard (@Cs //I) 
      (begin
       (set! /last /guard)
       (cond
        ((not (@Cs? /guard))
         (set! /found 1))
        ((@Any_Pattern_Type? (@ST /guard))
         #t)
        ((= (@Size /guard) 1)
         (set! /found 1))
        ((not (@Cs? (@Get_n /guard 2)))
         (set! /found 1)))))
     (cond
      ((= /found 1)
       (set! //R (@Fix_Cond_Guards (@Cs //I))))
      (#t
       (cond
        ((and (not (@Any_Pattern_Type? (@ST /last))) (not (= (@ST (@Get_n /last 1)) //T_/True)))
         (set! //R (@Make //T_/Cond '() (concat (@Cs //I) (list /adt_/Dummy_/Guarded)))))
        ((and (= (@Size //I) 1) (not (@Any_Pattern_Type? (@ST (@Get_n //I 1)))))
         ; The ELSIF clause is the only one left: 
         (set! //R (@Get_n (@Get_n //I 1) 2))
         (cond
          ((and (= (@Size //R) 1) (= (@ST (@Get_n //R 1)) //T_/Skip))
           (set! //R '()))))))))))
  //R))

; Some of these guards need to be deleted, 
; so later guards may need their conditions fixing: 
(define (@Fix_Cond_Guards /guards)
 (let ((/new/L '())
       (//B '())
       (/guard '())
       (//R '())
       (/size (gen-length /guards)))
  (for-in /guard /guards 
   (cond
    ((not (@Cs? /guard))
     #t)
    ((@Any_Pattern_Type? (@ST /guard))
     (set! /new/L (cons /guard /new/L)))
    ((or (= (@Size /guard) 1) (not (@Cs? (@Get_n /guard 2))))
     ; Delete this guard and start/continue computing B: 
     (cond
      ((>= /size 100)
       (set! /newl (cons (@Make //T_/Guarded '() (list (@Get_n /guard 1) (@Skips))) /new/L)))
      ((null? //B)
       (set! //B (@Not (@Get_n /guard 1))))
      (#t
       (set! //B (@And (@Not (@Get_n /guard 1)) //B)))))
    ((not (null? //B))
     ; Fix this guard and continue computing B: 
     (set! /new/L (cons (@Make //T_/Guarded '() (list (@And //B (@Get_n /guard 1)) (@Get_n /guard 2))) /new/L)))
    (#t
     (set! /new/L (cons /guard /new/L)))))
  (cond
   ((null? /new/L)
    (set! //R '()))
   ((and (= (gen-length /new/L) 1) (not (@Any_Pattern_Type? (@ST (car /new/L)))) (= (@ST (@Get_n (car /new/L) 1)) //T_/True))
    ; The ELSIF clause is the only one left: 
    (set! //R (@Get_n (car /new/L) 2)))
   ((and (not (@Any_Pattern_Type? (@ST (car /new/L)))) (not (= (@ST (@Get_n (car /new/L) 1)) //T_/True)))
    (set! //R (@Make //T_/Cond '() (reverse (cons /adt_/Dummy_/Guarded /new/L)))))
   (#t
    (set! //R (@Make //T_/Cond '() (reverse /new/L)))))
  //R))

(define (@Fix_Dijkstra_Item //I)
 (let ((//R //I))
  (cond
   ((not (@Cs? //I))
    (set! //R '()))
   (#t
    ; Look for empty or short guards: 
    (let ((/guard '())
          (/found 0)
          (/new '()))
     (for-in /guard (@Cs //I) 
      (cond
       ((not (@Cs? /guard))
        (set! /found 1))
       ((@Any_Pattern_Type? (@ST /guard))
        #t)
       ((or (= (@Size /guard) 1) (not (@Cs? (@Get_n /guard 2))))
        (set! /found 1))))
     (cond
      ((= /found 1)
       (for-in /guard (@Cs //I) 
        (cond
         ((not (@Cs? /guard))
          #t)
         ((@Any_Pattern_Type? (@ST /guard))
          (set! /new (cons /guard /new)))
         ((or (= (@Size /guard) 1) (not (@Cs? (@Get_n /guard 2))))
          (set! /new (cons (@Make //T_/Guarded '() (list (@Get_n /guard 1) (@Skips))) /new)))
         (#t
          (set! /new (cons /guard /new)))))
       (cond
        ((null? /new)
         (set! //R '()))
        (#t
         (set! //R (@Make (@ST //I) '() (reverse /new))))))))))
  //R))

; Return TRUE if there is a possibility that the given item may 
; terminate the program, given the required tv. 
; last = 1 if the item is the last component of its parent. 
(define (@May_Term? //I /req_tv //A/S /last)
 (let ((/tvs (@Gen_TVs //I //A/S))
       (//O/K 0))
  (cond
   ((and (= /req_tv 0) (= (@GT //I) //T_/Statement) (= /last 0))
    (set! /req_tv 1)))
  (while (and (not (null? /tvs)) (= //O/K 0)) 
   (cond
    ((>= (car /tvs) /req_tv)
     (set! //O/K 1))
    (#t
     (set! /tvs (cdr /tvs)))))
  (= //O/K 1)))

(define (@Will_Term? //I /req_tv //A/S)
 (let ((/tvs (@Gen_TVs //I //A/S))
       (//O/K 1))
  (cond
   ((and (= /req_tv 0) (= (@GT //I) //T_/Statement) (not (null? //Comps)))
    (set! /req_tv 1)))
  (while (and (not (null? /tvs)) (= //O/K 1)) 
   (cond
    ((not (>= (car /tvs) /req_tv))
     (set! //O/K 0))
    (#t
     (set! /tvs (cdr /tvs)))))
  (= //O/K 1)))

(set! /adt_/Skip_/Statement (@Make //T_/Skip '() '()))
(set! /adt_/Skip_/Statements (@Make //T_/Statements '() (list /adt_/Skip_/Statement)))
(define (@Skip)
 
 /adt_/Skip_/Statement)

(define (@Skips)
 
 /adt_/Skip_/Statements)

(define (@Skip? //Item)
 
 (or (= (@ST //Item) //T_/Skip) (and (= (@ST //Item) //T_/Exit) (= (@V //Item) 0))))

; ATEACH functions: 
; @Foreach_Statement(body, Depth, AS, simple) 
; @Foreach_Non_Action_Statement 
; @Foreach_Stats 
; @Foreach_Terminal 
; @Foreach_Terminal_Stats 
; @Foreach_Cond 
; @Foreach_Expn 
; @Foreach_Lvalue 
; @Foreach_Variable 
; @Foreach_Global_Var 
(define (@Ateach_Statement /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_S /body 0 //Depth //A/S /simple (gen-length //Orig_/P) //T_/Statement 0 0)
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Non_Action_Statement /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_S /body 0 //Depth //A/S /simple (gen-length //Orig_/P) //T_/Statement 0 1)
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Stats /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_S /body 0 //Depth //A/S /simple (gen-length //Orig_/P) //T_/Statements 0 0)
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Terminal /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_S /body 0 //Depth //A/S /simple (gen-length //Orig_/P) //T_/Statement 1 0)
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Terminal_Stats /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_S /body 0 //Depth //A/S /simple (gen-length //Orig_/P) //T_/Statements 1 0)
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

; A generic ATEACH for statements and statement sequences: 
; FIXME: Replace LENGTH(adt_Path_Items) by a new ADT function @Posn_Len 
; NOTE: It is the user's responsibility to take care when editing or moving 
; inside an ATEACH loop: infinite loops are possible and expensive to guard against. 
; However, the user may need to @Clever_Delete, in which case he or she 
; cannot avoid moving up. So we check for moving up past the original top. 
(define (@Ateach_S /body /req_tv //Depth //A/S /simple //Top /req_/G/T /term //N_/A/S)
 ; First check if we need to execute the body on @I: 
 (cond
  ((not (= (@GT (@I)) /req_/G/T))
   #t)
  ((and (= /simple 1) (not (@Simple? (@I))))
   #t)
  ((and (= /term 1) (not (@Will_Term? (@I) /req_tv //A/S)))
   #t)
  (#t
   (apply /body (list //Depth //A/S))))
 ; Next check if we need to look at the components of @I: 
 (cond
  ((or (@Passed?) (@Failed?) (null? (@I)))
   #t)
  ((< (gen-length /adt_/Path_/Items) //Top)
   ; We have moved out of the top 
  )
  ((or (not (@Has_Statements_Type? (@GT (@I)))) (not (@Cs? (@I))))
   #t)
  (#t
   (cond
    ((= (@ST (@I)) //T_/Floop)
     (set! //Depth (+ //Depth 1))
     (set! /req_tv (+ /req_tv 1))))
   (cond
    ((= (@ST (@I)) //T_/A_/S)
     (set! //A/S (@System_Type (@I)))))
   (cond
    ((@Ifmatch_Type? (@ST (@I)))
     (@Down_To 2))
    ((@Fill_Type? (@ST (@I)))
     #t)
    (#t
     (@Down)))
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (= //N_/A/S 1) (= (@ST (@I)) //T_/A_/S))
       (set! /fl_flag1 0))
      ((and (= /term 1) (not (@May_Term? (@I) /req_tv //A/S (if (@Right?) 0 1))))
       (set! /fl_flag1 0))
      (#t
       (cond
        ((and (= /req_tv 0) (= (@GT (@I)) //T_/Statement) (@Right?))
         (@Ateach_S /body 1 //Depth //A/S /simple //Top /req_/G/T /term //N_/A/S))
        (#t
         (@Ateach_S /body /req_tv //Depth //A/S /simple //Top /req_/G/T /term //N_/A/S)))
       (cond
        ((<= (gen-length /adt_/Path_/Items) //Top)
         ; We have moved up 
         (set! /fl_flag1 1))
        (#t
         (set! /fl_flag1 0)))))
     (cond
      ((= /fl_flag1 0)
       (cond
        ((not (@Right?))
         (@Up)
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0))))))))))

(define (@Ateach_Expn /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_Gen /body //Depth //A/S (gen-length //Orig_/P) //T_/Expression)
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Cond /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_Gen /body //Depth //A/S (gen-length //Orig_/P) //T_/Condition)
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Lvalue /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_Gen /body //Depth //A/S (gen-length //Orig_/P) //T_/Lvalue)
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Gen /body //Depth //A/S //Top /req_/G/T)
 (cond
  ((not (= (@GT (@I)) /req_/G/T))
   #t)
  (#t
   (apply /body (list //Depth //A/S))))
 (cond
  ((or (@Passed?) (@Failed?) (null? (@I)))
   #t)
  ((and (= /req_/G/T //T_/Condition) (= (@GT (@I)) //T_/Expression))
   #t)
  ((< (gen-length /adt_/Path_/Items) //Top)
   ; We have moved out of the top 
  )
  ((not (@Cs? (@I)))
   #t)
  (#t
   (cond
    ((= (@ST (@I)) //T_/Floop)
     (set! //Depth (+ //Depth 1))))
   (cond
    ((= (@ST (@I)) //T_/A_/S)
     (set! //A/S (@System_Type (@I)))))
   (cond
    ((@Ifmatch_Type? (@ST (@I)))
     (@Down_To 2))
    ((@Fill_Type? (@ST (@I)))
     #t)
    (#t
     (@Down)))
   (@Ateach_Gen /body //Depth //A/S //Top /req_/G/T)
   (while (and (< //Top (gen-length /adt_/Path_/Items)) (@Right?)) 
    (begin
     (@Right)
     (@Ateach_Gen /body //Depth //A/S //Top /req_/G/T)))
   (cond
    ((<= (gen-length /adt_/Path_/Items) //Top)
     ; We have moved up 
    )
    ((not (@Right?))
     (@Up))))))

(define (@Ateach_Variable /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_Var /body //Depth //A/S (gen-length //Orig_/P) 0 0 '())
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Global_Var /body //Depth //A/S /simple)
 (let ((//Orig_/P (@Posn)))
  (@Reset_Pass_Status)
  (@Ateach_Var /body //Depth //A/S (gen-length //Orig_/P) 0 1 '())
  (cond
   ((@Valid_Posn? (@Program) //Orig_/P)
    (@Goto //Orig_/P)))))

(define (@Ateach_Var /body //Depth //A/S //Top /parent /global /vars)
 (cond
  ((and (not (= (@ST (@I)) //T_/Variable)) (= (@ST (@I)) //T_/Var_/Lvalue))
   #t)
  ((and (= /global 1) (member (@V (@I)) /vars))
   #t)
  (#t
   (apply /body (list //Depth //A/S))))
 (cond
  ((or (@Passed?) (@Failed?) (null? (@I)))
   #t)
  ((< (gen-length /adt_/Path_/Items) //Top)
   ; We have moved out of the top 
  )
  ((not (@Cs? (@I)))
   #t)
  (#t
   (let ((//S/T (@ST (@I)))
         (//G/T (@GT (@I))))
    (cond
     ((= //S/T //T_/Floop)
      (set! //Depth (+ //Depth 1))))
    (cond
     ((= //S/T //T_/A_/S)
      (set! //A/S (@System_Type (@I)))))
    (cond
     ((@Ifmatch_Type? (@ST (@I)))
      (@Down_To 2))
     ((@Fill_Type? (@ST (@I)))
      #t)
     (#t
      (@Down)))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((= /global 1)
        (cond
         ((and (= //S/T //T_/For) (not (@Right?)))
          (set! /vars (union-n /vars (list (@V (@Get_n (@I) 1))))))
         ((and (= //S/T //T_/Var) (not (@Right?)))
          (set! /vars (union-n /vars (@Assigned (@Get_n (@I) 1))))))))
      ; In the assignments of a VAR, only process the expressions, if global = 1 
      ; Don't process the first component of a FOR if global = 1 
      (cond
       ((and (= /global 1) (= /parent //T_/Var) (= //G/T //T_/Assigns))
        (@Down_To 2)
        (@Ateach_Var /body //Depth //A/S //Top //S/T /global /vars)
        (cond
         ((> (gen-length /adt_/Path_/Items) //Top)
          (@Up))))
       ((or (not (= //T_/Lvalue (@GT (@I)))) (not (= //S/T //T_/For)) (not (= /global 1)))
        (@Ateach_Var /body //Depth //A/S //Top //S/T /global /vars)))
      (cond
       ((<= (gen-length /adt_/Path_/Items) //Top)
        ; We have moved up 
        (set! /fl_flag1 1))
       ((not (@Right?))
        (@Up)
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))))))

; ---------------------------------------------------------------------------  

