;;; Scheme translation of WSL code
(define (/foreach-summ2-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/X_/Funct_/Call)
   (@Down)
   ; to name 
   (set! /n (+ /n 1))
   (@Paste_Over (@Name (@Make_Name (string-append "funct" (@String /n))))))))

(define (/foreach-summ2-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/X_/B/Funct_/Call)
   (@Down)
   ; to name 
   (set! /n (+ /n 1))
   (@Paste_Over (@Name (@Make_Name (string-append (string-append "test" (@String /n)) "?")))))))

(define (/foreach-summ2-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (set! //Proc_/Summaries (cons (hash-table) //Proc_/Summaries))
   (@Summ2_Where_Defns (@Cs (@Get_n (@I) 2)))
   (for-in /body (@Cs (@Get_n (@I) 2)) 
    (begin
     (set! /summ (@S2_Get_Proc_Summary (@V (@Get_n /body 1)) //Proc_/Summaries))
     (cond
      ((and (= (@ST /body) //T_/Proc) (not (null? /summ)))
       (display-list (@N_String (@V (@Get_n /body 1))))
       (@Print_Summ2 (@Proc_To_Summ2 /summ))))))
   (set! //Proc_/Summaries (cdr //Proc_/Summaries)))))

(define /%const__summ2__1 (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "exit_flag") '()) (@Make 205 1 '()))))))))
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
; A different version of @Summarise which is used for data flow analysis 
; (in particular, for program slicing and Static Single Assignment construction). 
; It is more complex in that we need to know which vars contribute to the modified 
; values of each modified variable, but simpler in that we don't need to keep 
; track of copied variables, constant assignments and incremented variable, 
; nor do we need to treat exit_flag as a special case. 
; NB: An item may access variables but not modify any: 
; eg the condition in a while loop or cond. 
; In this case the accessed variables are control dependencies. 
; @Summ2(I) returns: 
; <<d1, <c1, c2, ...>, <v1, e11, e12,...>, <v2, e21, e22, ...>, ...>, 
;  <d2, ...>, 
;  ...> 
; dn are the depth levels (cf Constant_Propagation lists) 
; cn are accessed variables which are control dependencies 
; vn are the updated variables 
; enm are the variables whose values are used to update vn 
; A control dependency is a variable which is accessed in order to change 
; the control flow (eg vars in the condition of an IF or WHILE). 
; It does not directly affect another variable, but may do so indirectly. 
; Consider: IF x = 1 THEN y := 1 ELSE y := 2 FI 
; @Summ2 is <0, <x>, <y>> 
; Without the <x> we would assume that y is constant, but y actually 
; depends on the initial value of x. 
; Proc summaries are in the form: 
;   <body, val_pars, var_pars, cd_vars, <v1, e11, ..>, ...> 
; (Currently we assume that Functs and BFuncts are `pure': ie they depend only 
; on their parameters and don't reference any global variables) 
; The inital hash table allows for proc calls with no body. 
(set! //Proc_/Summaries (list (hash-table)))
; Used to check for recursion: 
(set! //Proc_/Stack (hash-table))
; Use this to rename local variables and formal pars to avoid name clashes: 
(set! //S2_/Par_/Count (- 1))
(set! /ind "")
(define (@Summ2 //I-par)
 (let ((//I-save //I)
       (//R '())
       (//S/T (@ST //I-par))
       (//G/T (@GT //I-par))
       (/ind-save /ind)
       (funct-result '()))
  (set! //I //I-par)
  (set! /ind (string-append /ind "  "))
  (cond
   ((= //G/T //T_/Statements)
    (set! //R (@S2_Sequence (@Cs //I))))
   ((= //G/T //T_/Expression)
    (set! //R (@S2_Default //I)))
   ((= //G/T //T_/Condition)
    (set! //R (@S2_Default //I)))
   ((= //S/T //T_/Cond)
    (set! //R (@S2_Cond (@Cs //I))))
   ((= //S/T //T_/D_/If)
    (set! //R (@S2_Cond (@Cs //I))))
   ((= //S/T //T_/Assignment)
    (set! //R (@S2_Assigns (@Cs //I))))
   ((= //S/T //T_/Assigns)
    (set! //R (@S2_Assigns (@Cs //I))))
   ((= //S/T //T_/Floop)
    (set! //R (@S2_Floop //I)))
   ((= //S/T //T_/While)
    (set! //R (@S2_While //I)))
   ((= //S/T //T_/Var)
    (set! //R (@S2_Var //I)))
   ((= //S/T //T_/For)
    (set! //R (@S2_For //I)))
   ((= //S/T //T_/Proc_/Call)
    (set! //R (@S2_Proc_Call //I)))
   ((= //S/T //T_/Where)
    (set! //R (@S2_Where //I)))
   ((= //S/T //T_/Lvalues)
    (set! //R (@S2_Lvalues //I)))
   (#t
    (set! //R (@S2_Default //I))))
  (set! funct-result //R)
  (set! //I //I-save)
  (set! /ind /ind-save)
  funct-result))

; Construct a summary of a sequential list of items (of arbitrary types): 
(define (@Summ2_List //L)
 
 (@S2_Sequence //L))

(define (@S2_Sequence /comps)
 (let ((//R (list (list 0 '())))
       (/comp '()))
  (while (not (null? /comps)) 
   (begin
    (set! /comp (car /comps))
    (set! /comps (cdr /comps))
    (cond
     ((= (@ST /comp) //T_/Exit)
      (set! //R (@S2_Increment //R (@V /comp)))
      (set! /comps '()))
     (#t
      (set! //R (@S2_Seq //R (@Summ2 /comp)))))))
  //R))

; If there is already an n level element in L then we must 
; merge the 0 level element with it: 
(define (@S2_Increment //L /n)
 
 (if (or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0)) //L (@S2_Merge (list (cons /n (cdr (wsl-ref //L 1)))) (cdr //L))))

; Apply the level 0 results (if any) sequentially to the rest of the list 
; and then decrement the list. 
(define (@S2_Floop //I-par)
 (let ((//I-save //I)
       (//R '())
       (//L (@S2_Loop (@S2_Sequence (@Cs (@Get_n //I-par 1)))))
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((null? //L)
    (set! //R '()))
   ((> (wsl-ref (wsl-ref //L 1) 1) 0)
    (set! //R (@S2_Decrement //L 1)))
   (#t
    (set! //R (@S2_Decrement (@S2_Seq_Sub (cdr (wsl-ref //L 1)) (cdr //L)) 1))))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

; A while loop may not execute at all: so all modified variables also 
; depend on their initial values (ie no variable is unconditionally clobbered). 
(define (@S2_While //I-par)
 (let ((//I-save //I)
       (/body-save /body)
       (/cond_vars (@Elements (@Get_n //I-par 1)))
       (funct-result '()))
  (set! //I //I-par)
  (set! /body (@S2_Maybe (@S2_Loop (@Summ2 (@Get_n //I-par 2)))))
  (cond
   ((and (not (null? /body)) (= (wsl-ref (wsl-ref /body 1) 1) 0))
    (set! /body (cons (cons 0 (cons (union-n /cond_vars (wsl-ref (wsl-ref /body 1) 2)) (@Final_Seg (wsl-ref /body 1) 3))) (cdr /body)))))
  (set! funct-result /body)
  (set! //I //I-save)
  (set! /body /body-save)
  funct-result))

; Indicate that a summary refers to a statement that may not be executed 
; by making each modified variable also depend on its initial value: 
(define (@S2_Maybe //L)
 
 (if (and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0)) (cons (cons 0 (cons (wsl-ref (wsl-ref //L 1) 2) (@S2_Maybe_Sub (@Final_Seg (wsl-ref //L 1) 3)))) (cdr //L)) //L))

(define (@S2_Maybe_Sub /assigns)
 
 (if (null? /assigns) '() (if (member (wsl-ref (wsl-ref /assigns 1) 1) (@Final_Seg (wsl-ref /assigns 1) 2)) (cons (car /assigns) (@S2_Maybe_Sub (cdr /assigns))) (cons (union-n (list (wsl-ref (wsl-ref /assigns 1) 1) (wsl-ref (wsl-ref /assigns 1) 1)) (@Final_Seg (wsl-ref /assigns 1) 2)) (@S2_Maybe_Sub (cdr /assigns))))))

; Process a summary of a statement which is iterated at least once. 
; If a := f(b) and b := g(c) then in the result, a also depends on c. 
; In effect, this is a graph marking problem: find all the reachable vars. 
; If v is clobbered in the statement (ie v doesn't depend on its initial value) 
; then it is also clobbered by the loop. 
(define (@S2_Loop //L)
 (let ((//R '()))
  (cond
   ((null? //L)
    (set! //R //L))
   ((> (wsl-ref (wsl-ref //L 1) 1) 0)
    (set! //R //L))
   (#t
    (let ((/old-save /old)
          (/new-save /new)
          (/done-save /done)
          (/newl '()))
     (set! /old (@S2_List_To_Hash (@Final_Seg (wsl-ref //L 1) 3)))
     (set! /new (hash-table))
     (set! /done (hash-table))
     (for-in /pair (@Final_Seg (wsl-ref //L 1) 3) 
      (set! /newl (cons (cons (wsl-ref /pair 1) (@S2_Reachable (wsl-ref /pair 1))) /newl)))
     (set! //R (cons (cons (wsl-ref (wsl-ref //L 1) 1) (cons (wsl-ref (wsl-ref //L 1) 2) /newl)) (cdr //L)))
     (set! /old /old-save)
     (set! /new /new-save)
     (set! /done /done-save))))
  //R))

(define (@S2_Reachable /x)
 (let ((//R '()))
  (cond
   ((not (null? (gethash /done /x)))
    (set! //R (gethash /new /x))
    (cond
     ((number? //R)
      (set! //R '()))))
   (#t
    (set! //R (gethash /old /x))
    (cond
     ((number? //R)
      (set! //R '())))
    (puthash /done /x 1)
    (for-in /y //R 
     (set! //R (union-n //R (@S2_Reachable /y))))
    (puthash /new /x //R)))
  //R))

; Convert a list of the form <<key, val1, val2, ...>, ...> to a hash table. 
; NB: If the list of values is empty, then we need to store something 
; other than < > in the hash table to show that the key is present. 
(define (@S2_List_To_Hash //L)
 (let ((//R (hash-table))
       (/pair '()))
  (for-in /pair //L 
   (cond
    ((null? (@Final_Seg /pair 2))
     (puthash //R (wsl-ref /pair 1) 0))
    (#t
     (puthash //R (wsl-ref /pair 1) (@Final_Seg /pair 2)))))
  //R))

(define (@S2_Decrement //L /n)
 
 (if (null? //L) '() (if (< (wsl-ref (wsl-ref //L 1) 1) /n) (@S2_Decrement (cdr //L) /n) (cons (cons (- (wsl-ref (wsl-ref //L 1) 1) /n) (cdr (wsl-ref //L 1))) (@S2_Decrement (cdr //L) /n)))))

; Sequential merging of two summaries: 
; If the first statement is improper (ie L1[1][1] > 0), 
; then the second statement is unreachable and has no effect. 
; Otherwise, sequentially merge L1[1] with ALL of L2 
; and then parallel merge the rest of L1 with the new L2 tail: 
(define (@S2_Seq //L1 //L2)
 
 (if (or (null? //L1) (null? //L2)) '() (if (> (wsl-ref (wsl-ref //L1 1) 1) 0) //L1 (cons (cons (wsl-ref (wsl-ref //L2 1) 1) (@S2_Seq_Vars (cdr (wsl-ref //L1 1)) (cdr (wsl-ref //L2 1)))) (@S2_Merge (cdr //L1) (@S2_Seq_Sub (cdr (wsl-ref //L1 1)) (cdr //L2)))))))

; Sequentially merge the vars with everything in the list: 
(define (@S2_Seq_Sub /vars //L)
 
 (if (null? //L) '() (cons (cons (wsl-ref (wsl-ref //L 1) 1) (@S2_Seq_Vars /vars (cdr (wsl-ref //L 1)))) (@S2_Seq_Sub /vars (cdr //L)))))

; <controls, <v1, e11, e12...>, ...> 
; Apply the first assigns to the second set of control vars. 
; Take the second list of assigns and apply the first assigns to each RHS. 
; Add any new assigns from the first list. 
(define (@S2_Seq_Vars //L1-par //L2)
 (let ((//L1-save //L1)
       (//R '())
       (/assigns1 (@Final_Seg //L1-par 2))
       (/done-save /done)
       (funct-result '()))
  (set! //L1 //L1-par)
  (set! /done (hash-table))
  (for-in /pair (@Final_Seg //L2 2) 
   (begin
    (set! //R (cons (cons (wsl-ref /pair 1) (@S2_Apply (@Final_Seg /pair 2) /assigns1)) //R))
    (puthash /done (wsl-ref /pair 1) 1)))
  (for-in /pair (@Final_Seg //L1 2) 
   (cond
    ((null? (gethash /done (wsl-ref /pair 1)))
     (set! //R (cons /pair //R)))))
  (set! funct-result (cons (union-n (wsl-ref //L1 1) (@S2_Apply (wsl-ref //L2 1) /assigns1)) (reverse //R)))
  (set! //L1 //L1-save)
  (set! /done /done-save)
  funct-result))

; Apply a list of assigns to each of a list of variables. 
; If an assign overwrites *part* of a variable, keep the variable 
; plus the values from the assign. 
; If an assign overwrites the struct containing the variable, 
; replace the variable by the values. 
(define (@S2_Apply //L /assigns)
 (let ((//R '())
       (/val '())
       (/overwrite 0))
  (for-in /var //L 
   (begin
    (set! /overwrite 0)
    (for-in /pair /assigns 
     (cond
      ((@Prefix? (wsl-ref /pair 1) /var)
       ; This assign overwrites all of the var 
       (set! //R (union-n //R (@Final_Seg /pair 2)))
       (set! /overwrite 1))
      ((@Prefix? /var (wsl-ref /pair 1))
       ; This assign overwrites part of the var 
       (set! //R (union-n //R (@Final_Seg /pair 2))))))
    (cond
     ((= /overwrite 0)
      (set! //R (union-n //R (list /var)))))))
  //R))

; Merging two summaries in parallel (eg arms of an IF): 
(define (@S2_Merge //L1 //L2)
 
 (if (null? //L1) //L2 (if (null? //L2) //L1 (if (< (wsl-ref (wsl-ref //L1 1) 1) (wsl-ref (wsl-ref //L2 1) 1)) (cons (wsl-ref //L1 1) (@S2_Merge (cdr //L1) //L2)) (if (> (wsl-ref (wsl-ref //L1 1) 1) (wsl-ref (wsl-ref //L2 1) 1)) (cons (wsl-ref //L2 1) (@S2_Merge //L1 (cdr //L2))) (cons (cons (wsl-ref (wsl-ref //L1 1) 1) (@S2_Merge_Vars (cdr (wsl-ref //L1 1)) (cdr (wsl-ref //L2 1)))) (@S2_Merge (cdr //L1) (cdr //L2))))))))

; <e, <v, e1, e2...>, ...> 
; If a variable is assigned in one arm, but not in the other, 
; then its final value depends on its initial value 
; (since the final value may be the unchanged initial value): 
(define (@S2_Merge_Vars //L1-par //L2)
 (let ((//L1-save //L1)
       (//R '())
       (/val '())
       (/assign1 (@S2_List_To_Hash (@Final_Seg //L1-par 2)))
       (/assign2 (@S2_List_To_Hash (@Final_Seg //L2 2)))
       (/done-save /done)
       (funct-result '()))
  (set! //L1 //L1-par)
  (set! /done (hash-table))
  (for-in /pair (@Final_Seg //L1 2) 
   (begin
    (puthash /done (wsl-ref /pair 1) 1)
    (set! /val (gethash /assign2 (wsl-ref /pair 1)))
    (cond
     ((null? /val)
      ; pair[1] is assigned in L1 but not in L2, so its final value also 
      ; depends on its initial value: 
      (set! //R (cons (cons (wsl-ref /pair 1) (union-n (@Final_Seg /pair 2) (list (wsl-ref /pair 1)))) //R)))
     ((number? /val)
      (set! //R (cons /pair //R)))
     (#t
      (set! //R (cons (cons (wsl-ref /pair 1) (union-n (@Final_Seg /pair 2) /val)) //R))))))
  (for-in /pair (@Final_Seg //L2 2) 
   (cond
    ((null? (gethash /done (wsl-ref /pair 1)))
     (set! /val (gethash /assign1 (wsl-ref /pair 1)))
     (cond
      ((null? /val)
       (set! //R (cons (cons (wsl-ref /pair 1) (union-n (@Final_Seg /pair 2) (list (wsl-ref /pair 1)))) //R)))
      ((number? /val)
       (set! //R (cons /pair //R)))
      (#t
       (set! //R (cons (cons (wsl-ref /pair 1) (union-n (@Final_Seg /pair 2) /val)) //R)))))))
  (set! funct-result (cons (union-n (wsl-ref //L1 1) (wsl-ref //L2 1)) (reverse //R)))
  (set! //L1 //L1-save)
  (set! /done /done-save)
  funct-result))

; The comps are all of type T_Guarded 
; The conditions are evaluated until one is true, then the corresponding 
; code is executed. So the nth arm includes the result of evaluating 
; the first n conditions: 
(define (@S2_Cond /comps)
 (let ((//R '())
       (/comp '())
       (//B '()))
  (while (not (null? /comps)) 
   (begin
    (set! /comp (car /comps))
    (set! /comps (cdr /comps))
    ; Get the results of the condition and apply to rest of sequence: 
    (cond
     ((null? //B)
      (set! //B (@S2_Cond_Test (@Get_n /comp 1))))
     (#t
      (set! //B (@S2_Seq //B (@S2_Cond_Test (@Get_n /comp 1))))))
    (set! //R (@S2_Merge //R (@S2_Seq //B (@Summ2 (@Get_n /comp 2)))))))
  //R))

(define (@S2_Cond_Test //I)
 
 (list (list 0 (@Elts_Used //I))))

; There are no control variables in an assignment statement: 
(define (@S2_Assigns /comps)
 (let ((//R '())
       (/used '()))
  (for-in //I /comps 
   (begin
    (set! /used (@Elts_Used //I))
    (for-in /var (@Elts_Assigned (@Get_n //I 1)) 
     (set! //R (cons (cons /var /used) //R)))))
  (list (cons 0 (cons '() //R)))))

; The default is to assume that every assigned element depends on 
; every used element. If there are no assigned elements in a statement, 
; then take os as the single assigned element, 
; otherwise (eg for a condition) assume all used elements are control vars. 
(define (@S2_Default //I-par)
 (let ((//I-save //I)
       (//R '())
       (/vals (@Elts_Used //I-par))
       (/vars (@Elts_Assigned //I-par))
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((and (null? /vars) (not (null? /vals)) (= (@GT //I) //T_/Statement))
    (set! /vars (list (list (@Make_Name "os"))))
    (set! /vals (concat /vars /vals))))
  (cond
   ((null? /vars)
    (set! //R (list (list 0 /vals))))
   (#t
    (for-in /var /vars 
     (set! //R (cons (cons /var /vals) //R)))
    (set! //R (list (cons 0 (cons '() //R))))))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

; This is used by @Basic_Blocks to generate the return node for a procedure. 
; Generate a reference to and update of each returned variable 
; x := <x>, y := <y> etc. 
(define (@S2_Lvalues //I-par)
 (let ((//I-save //I)
       (//R '())
       (funct-result '()))
  (set! //I //I-par)
  (for-in /var (@Elts_Assigned //I) 
   (set! //R (cons (list /var /var) //R)))
  (set! funct-result (list (cons 0 (cons '() //R))))
  (set! //I //I-save)
  funct-result))

; Remove local variables from a summary: 
(define (@S2_Remove //L /names)
 (let ((//R '())
       (//L1-save //L1)
       (/controls '())
       (/assigns '())
       (funct-result '()))
  (set! //L1 '())
  (cond
   ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
    (set! //R //L))
   (#t
    (set! /controls (wsl-ref (wsl-ref //L 1) 2))
    (for-in /pair (@Final_Seg (wsl-ref //L 1) 3) 
     (cond
      ((member (wsl-ref /pair 1) /names)
       ; Assignment to a local var, 
       ; convert the referenced vars to control vars. 
       (set! /controls (union-n /controls (@Elt_Subtract (@Final_Seg /pair 2) /names))))
      (#t
       (set! /assigns (cons (cons (wsl-ref /pair 1) (@Elt_Subtract (@Final_Seg /pair 2) /names)) /assigns)))))
    (set! //R (cons (cons 0 (cons (@Elt_Subtract /controls /names) /assigns)) (cdr //L)))))
  (set! funct-result //R)
  (set! //L1 //L1-save)
  funct-result))

; Add accessed variables to a summary: 
(define (@S2_Add //L /vars)
 (let ((//R //L))
  (cond
   ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
    #t)
   (#t
    (wsl-set! //R (union-n (wsl-ref (wsl-ref //R 1) 2) /vars) 1 2)))
  //R))

; Rename a variable in a summary: 
(define (@S2_Rename //L /old-par /new-par)
 (let ((/new-save /new)
       (/old-save /old)
       (//R '())
       (funct-result '()))
  (set! /new /new-par)
  (set! /old /old-par)
  (cond
   ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
    (set! //R //L))
   (#t
    (for-in /pair (@Final_Seg (wsl-ref //L 1) 2) 
     (set! //R (cons (@S2_Rename_sub /pair /old /new) //R)))
    (set! //R (cons (cons 0 (reverse //R)) (cdr //L)))))
  (set! funct-result //R)
  (set! /new /new-save)
  (set! /old /old-save)
  funct-result))

; Rename a variable in a list of variables: 
(define (@S2_Rename_sub //L /old-par /new-par)
 (let ((/new-save /new)
       (/old-save /old)
       (//R '())
       (funct-result '()))
  (set! /new /new-par)
  (set! /old /old-par)
  (for-in /elt //L 
   (cond
    ((equal? /elt /old)
     (set! //R (cons /new //R)))
    (#t
     (set! //R (cons /elt //R)))))
  (set! funct-result (reverse //R))
  (set! /new /new-save)
  (set! /old /old-save)
  funct-result))

; Add a prefix to all variables in a summary: 
; Drop an initial `a' and convert structured variables to simple variables. 
(define (@S2_Prefix //L /str)
 (let ((//R '()))
  (cond
   ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
    (set! //R //L))
   (#t
    (for-in /pair (@Final_Seg (wsl-ref //L 1) 2) 
     (set! //R (cons (@S2_Prefix_List /pair /str) //R)))
    (set! //R (cons (cons 0 (reverse //R)) (cdr //L)))))
  //R))

; Add a prefix to all variables in a list: 
; (a.FOO.BAR becomes prefix__foo.bar) 
(define (@S2_Prefix_List //L /str)
 (let ((//R '()))
  (for-in /elt //L 
   (cond
    ((> (wsl-ref /elt 1) 0)
     (cond
      ((and (equal? (wsl-ref /elt 1) /a_name) (> (gen-length /elt) 1))
       (set! /elt (cdr /elt))))
     (cond
      ((< (last-1 /elt) 0)
       (set! //R (cons (list (@S2_Join /str (butlast-1 /elt)) (last-1 /elt)) //R)))
      (#t
       (set! //R (cons (list (@S2_Join /str /elt)) //R)))))
    (#t
     (set! //R (cons /elt //R)))))
  (reverse //R)))

; Join the elements with _dot_ and add the prefix to make a new name: 
; (NB: The result has to be a simple variable since field names 
; have to be unique: ie each child has a unique parent field) 
(define (@S2_Join /str /elt)
 
 (@Make_Name (concat /str (@Join "_dot_" (my-map @N_String /elt)))))

; Rename the local vars (to avoid clashes), 
; Add the initial assignments to the body and remove the local vars: 
(define (@S2_Var //I-par)
 (let ((//I-save //I)
       (//R (@S2_Sequence (@Cs (@Get_n //I-par 2))))
       (/init '())
       (/var-save /var)
       (/vars '())
       (funct-result '()))
  (set! //I //I-par)
  (set! /var '())
  (for-in /assign (@Cs (@Get_n //I 1)) 
   (begin
    (set! /var (@CP_Var_Name (@Get_n /assign 1)))
    (cond
     ((not (null? /var))
      (set! //S2_/Par_/Count (- //S2_/Par_/Count 1))
      (set! /vars (cons (list //S2_/Par_/Count) /vars))
      (set! //R (@S2_Rename //R /var (list //S2_/Par_/Count)))
      (set! /init (cons (cons (list //S2_/Par_/Count) (@Elts_Used (@Get_n /assign 2))) /init))))))
  (set! /init (list (cons 0 (cons '() /init))))
  (set! //R (@S2_Remove (@S2_Seq /init //R) (@Make_Set /vars)))
  (set! funct-result //R)
  (set! //I //I-save)
  (set! /var /var-save)
  funct-result))

(define (@S2_For //I)
 
 (@S2_Add (@S2_Remove (@S2_Loop (@S2_Sequence (@Cs (@Get_n //I 5)))) (list (list (@V (@Get_n //I 1))))) (union-n (@Elts_Used (@Get_n //I 2)) (@Elts_Used (@Get_n //I 3)) (@Elts_Used (@Get_n //I 4)))))

; Look for a summary of the proc body and update according to the parameters: 
; A proc summary is: <body, val_pars, var_pars, control_vars, <v1, e11, ..>, ...> 
; This avoids an error when the proc call is not in the WHERE clause: 
(set! /proc_done (hash-table))
(define (@S2_Proc_Call //I-par)
 (let ((//I-save //I)
       (//R '())
       (/summ-save /summ)
       (/actual_vals '())
       (/formal_vals '())
       (/formal_vars '())
       (/actual '())
       (/init '())
       (/prefix (string-append (@N_String (@V (@Get_n //I-par 1))) "__"))
       (funct-result '()))
  (set! //I //I-par)
  (set! /summ (@S2_Get_Proc_Summary (@V (@Get_n //I-par 1)) //Proc_/Summaries))
  (cond
   ((null? /summ)
    ; Either we haven't processed the definition, or its a recursive call: 
    (cond
     ((not (null? (gethash /proc_done (@V (@Get_n //I 1)))))
      ; Recursive call: 
      (set! //R (list (list 0 '()))))
     (#t
      (set! //R (@S2_Default //I)))))
   (#t
    (set! //R (@Proc_To_Summ2 /summ))
    (cond
     ((or (not (= (gen-length (wsl-ref /summ 2)) (@Size (@Get_n //I 2)))) (not (= (gen-length (wsl-ref /summ 3)) (@Size (@Get_n //I 3)))))
      (display-list "Formal/Actual parameter mismatch in proc call:")
      (display-list "summ[2] = " (wsl-ref /summ 2))
      (display-list "summ[3] = " (wsl-ref /summ 3))
      (@Print_WSL //I ""))
     ((and (null? (wsl-ref /summ 2)) (null? (wsl-ref /summ 3)))
      ; parameterless proc call 
     )
     (#t
      (set! //R (@S2_Prefix //R /prefix))
      (set! /formal_vals (@S2_Prefix_List (wsl-ref /summ 2) /prefix))
      (set! /formal_vars (@S2_Prefix_List (wsl-ref /summ 3) /prefix))
      ; create init as formal_vals := actual_vals 
      (for-in /actual (@Cs (@Get_n //I 2)) 
       (begin
        (set! /init (cons (cons (car /formal_vals) (@Elts_Used /actual)) /init))
        (set! /formal_vals (cdr /formal_vals))))
      (set! /init (list (cons 0 (cons '() /init))))
      (for-in /actual (@Cs (@Get_n //I 3)) 
       (begin
        (set! //R (@S2_Rename //R (car /formal_vars) (@CP_Var_Name /actual)))
        (set! /formal_vars (cdr /formal_vars))))
      (set! //R (@S2_Seq /init //R))
      (set! //R (@S2_Remove //R (@Make_Set (@S2_Prefix_List (wsl-ref /summ 2) /prefix))))))))
  (set! funct-result //R)
  (set! //I //I-save)
  (set! /summ /summ-save)
  funct-result))

; Return the summary of the proc from the list, given its name: 
; bodies is a list of tables: 
; name -> <body, val_pars, var_pars, cd_vars, <v1, e1, ...>, ...> 
; NB: formal parameters in the summary are replaced by negative integers 
; which indicate their position in the parameter list. 
(define (@S2_Get_Proc_Summary /name /bodies)
 (let ((/tab '())
       (//R '()))
  (cond
   ((not (null? /bodies))
    (set! /tab (car /bodies))
    (set! //R (gethash /tab /name))
    (cond
     ((null? //R)
      (set! //R (@S2_Get_Proc_Summary /name (cdr /bodies)))))))
  //R))

; Process the proc/funct bodies (depth first order) and temporarily prepend 
; a table to Proc_Summaries while we process the body of the where. 
(define (@S2_Where //I-par)
 (let ((//I-save //I)
       (//R '())
       (funct-result '()))
  (set! //I //I-par)
  (set! //Proc_/Summaries (cons (hash-table) //Proc_/Summaries))
  (@Summ2_Where_Defns (@Cs (@Get_n //I 2)))
  (set! //R (@Summ2 (@Get_n //I 1)))
  (set! //Proc_/Summaries (cdr //Proc_/Summaries))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

; Add summaries of all the procs to the first table in Proc_Summaries: 
; (This is also used in Constant_Propagation and Basic_Blocks) 
(define (@Summ2_Where_Defns //L)
 (let ((/body-save /body)
       (/proc_done-save /proc_done)
       (/bodies (hash-table)))
  (set! /body '())
  (set! /proc_done (hash-table))
  (for-in /body //L 
   (puthash /bodies (@V (@Get_n /body 1)) /body))
  (for-in /body //L 
   (cond
    ((null? (gethash /proc_done (@V (@Get_n /body 1))))
     ; Summarise the body, first summarising any (non-recursivaly) called procs: 
     (set! /done (@S2_Summarise_Body  /bodies /body /done)))))
  (set! /body /body-save)
  (set! /proc_done /proc_done-save)))

; NB could have a call to a higher WHERE clause: 
(define (@S2_Summarise_Body /bodies /body-par /done-par)
 (let ((/done-save /done)
       (/body-save /body)
       (funct-result '()))
  (set! /done /done-par)
  (set! /body /body-par)
  (let ((/calls '())
        (/pair-save /pair)
        (/name '()))
   (set! /pair '())
   (puthash /proc_done (@V (@Get_n /body 1)) 1)
   (cond
    ((= (@ST /body) //T_/Proc)
     (set! /calls (@Proc_Calls (@Get_n /body 4)))
     (for-in /pair /calls 
      (begin
       (set! /name (car /pair))
       (cond
        ((and (null? (gethash /proc_done /name)) (not (null? (gethash /bodies /name))))
         ; Find the body and process it first: 
         (set! /done (@S2_Summarise_Body  /bodies (gethash /bodies /name) /done))))))))
   ; All called procs have been processed: 
   (@S2_Summarise_Body_Sub /body)
   (set! /pair /pair-save))
  (set! funct-result /done)
  (set! /done /done-save)
  (set! /body /body-save)
  funct-result))

; The old version renames parameters in the val, var and summary 
; to negative integers using S2_Par_Count. 
; This is to avoid name clashes. 
; Check for a var par which is also a val par: 
; (val pars are copied to var pars for the SSA transformation) 
(define (@S2_Summarise_Body_Sub_Orig /body-par)
 (let ((/body-save /body))
  (set! /body /body-par)
  (let ((/summ-save /summ)
        (/vals '())
        (/vars '())
        (/par '())
        (/par_tab (hash-table))
        (/tab (hash-table))
        (/n-save /n))
   (set! /summ '())
   (set! /n 0)
   (set! /summ (@Summ2 (@Get_n /body 4)))
   (cond
    ((and (not (null? /summ)) (= (wsl-ref (wsl-ref /summ 1) 1) 0))
     (for-in /par (@Cs (@Get_n /body 2)) 
      (begin
       (set! //S2_/Par_/Count (- //S2_/Par_/Count 1))
       (set! /summ (@S2_Rename /summ (@CP_Var_Name /par) (list //S2_/Par_/Count)))
       (set! /vals (cons (list //S2_/Par_/Count) /vals))
       (puthash /par_tab (@CP_Var_Name /par) //S2_/Par_/Count)))
     (for-in /par (@Cs (@Get_n /body 3)) 
      (begin
       (cond
        ((null? (gethash /par_tab (@CP_Var_Name /par)))
         (set! //S2_/Par_/Count (- //S2_/Par_/Count 1))
         (set! /n //S2_/Par_/Count))
        (#t
         (set! /n (gethash /par_tab (@CP_Var_Name /par)))))
       (set! /summ (@S2_Rename /summ (@CP_Var_Name /par) (list /n)))
       (set! /vars (cons (list /n) /vars))))
     (set! /tab (car //Proc_/Summaries))
     (puthash /tab (@V (@Get_n /body 1)) (cons /body (cons (reverse /vals) (cons (reverse /vars) (cdr (wsl-ref /summ 1))))))
     (wsl-set! //Proc_/Summaries /tab 1)))
   (set! /summ /summ-save)
   (set! /n /n-save))
  (set! /body /body-save)))

; For the basic blocks list we need to know what the original name 
; of each parameter is.  We do the renaming (to avoid name clashes) 
; when we process the proc call. 
(define (@S2_Summarise_Body_Sub /body-par)
 (let ((/body-save /body))
  (set! /body /body-par)
  (let ((/summ-save /summ)
        (/vals '())
        (/vars '())
        (/tab (hash-table)))
   (set! /summ '())
   (set! /summ (@Summ2 (@Get_n /body 4)))
   (cond
    ((and (not (null? /summ)) (= (wsl-ref (wsl-ref /summ 1) 1) 0))
     (set! /vals (my-map @CP_Var_Name (@Cs (@Get_n /body 2))))
     (set! /vars (my-map @CP_Var_Name (@Cs (@Get_n /body 3))))
     (set! /tab (car //Proc_/Summaries))
     (puthash /tab (@V (@Get_n /body 1)) (cons /body (cons /vals (cons /vars (@Final_Seg (wsl-ref /summ 1) 2)))))
     (wsl-set! //Proc_/Summaries /tab 1)))
   (set! /summ /summ-save))
  (set! /body /body-save)))

; Convert a summary to an equivalent WSL schema: 
(define (@Summ2_To_WSL //L)
 (let ((//R '())
       (/body-save /body)
       (funct-result '()))
  (set! /body '())
  (cond
   ((null? //L)
    (set! //R /%const__summ2__1))
   ((= (gen-length //L) 1)
    (set! //R (@Summ2_To_WSL_Sub (wsl-ref (cdr (wsl-ref //L 1)) 1) (@Final_Seg (cdr (wsl-ref //L 1)) 2)))
    (cond
     ((> (wsl-ref (wsl-ref //L 1) 1) 0)
      (set! //R (@Make //T_/Statements '() (concat (@Cs //R) (list (@Make //T_/Exit (wsl-ref (wsl-ref //L 1) 1) '()))))))
     ((= (@Size //R) 0)
      (set! //R (@Skips)))))
   (#t
    ; Multiple exit values to be accounted for 
    (let ((/expns '())
          (/body-save /body))
     (set! /body '())
     (while (not (null? //L)) 
      (begin
       (set! /expns (my-map @Name_To_WSL (wsl-ref (wsl-ref //L 1) 2)))
       (set! /body (@Summ2_To_WSL_Sub '() (@Final_Seg (cdr (wsl-ref //L 1)) 2)))
       (cond
        ((> (wsl-ref (wsl-ref //L 1) 1) 0)
         (set! /body (@Make //T_/Statements '() (concat (@Cs /body) (list (@Make //T_/Exit (wsl-ref (wsl-ref //L 1) 1) '()))))))
        ((= (@Size /body) 0)
         (set! /body (@Skips))))
       (set! //L (cdr //L))
       (cond
        ((null? //L)
         (set! /cond (@Make //T_/True '() '())))
        (#t
         (set! /cond (@Make 301 '() (list (@Make 9 (@Make_Name "test?") '()) (@Make 10 '() (@Var_To_Expn_List /expns)))))))
       (set! //R (cons (@Make //T_/Guarded '() (list /cond /body)) //R))))
     (set! /body /body-save))
    (set! //R (@Make //T_/Statements '() (list (@Make //T_/Cond '() (reverse //R)))))))
  (@Edit)
  (@New_Program //R)
  ; Rename funct and test so that they are all different 
  (let ((/n-save /n))
   (set! /n 0)
   (@Foreach_Expn /foreach-summ2-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /n 0)
   (@Foreach_Cond /foreach-summ2-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /n /n-save))
  (set! //R (@Program))
  (@Undo_Edit)
  (set! funct-result //R)
  (set! /body /body-save)
  funct-result))

; assigns are <<v1, e11, ...>, ...> 
(define (@Summ2_To_WSL_Sub /control /assigns)
 (let ((/vars '())
       (//R '()))
  (for-in /pair /assigns 
   (begin
    (set! /vars (my-map @Elt_To_Expn (@Final_Seg /pair 2)))
    (set! //R (cons (@Make //T_/Assign '() (list (@Elt_To_Lvalue (wsl-ref /pair 1)) (@Make 201 '() (list (@Make 9 (@Make_Name "funct") '()) (@Make 10 '() (@Var_To_Expn_List /vars)))))) //R))))
  (cond
   ((null? //R)
    (set! //R (@Skip)))
   (#t
    (set! //R (@Make //T_/Assignment '() (reverse //R)))))
  (cond
   ((not (null? /control))
    (set! /vars (my-map @Elt_To_Expn /control))
    (set! //R (@Make 114 '() (list (@Make 7 '() (list (@Make 301 '() (list (@Make 9 (@Make_Name "B?") '()) (@Make 10 '() (@Var_To_Expn_List /vars)))) (@Make 17 '() (list //R)))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))
  (@Make //T_/Statements '() (list //R))))

(define (@Print_Summ2 //L)
 (for-in //L1 //L 
  (begin
   (display-list-flush (wsl-ref //L1 1) ": <" (@Join ", " (my-map @Elt_To_String (wsl-ref //L1 2))) ">")
   (for-in /pair (@Final_Seg //L1 3) 
    (begin
     (display-list "")
     (display-list-flush "   " (@Elt_To_String (wsl-ref /pair 1)) " := <")
     (display-list-flush (@Join ", " (my-map @Elt_To_String (@Final_Seg /pair 2))) ">")))
   (display-list ""))))

(define (@Write_Summ2 //L)
 (for-in //L1 //L 
  (begin
   (@WS (string-append (concat (string-append (@String (wsl-ref //L1 1)) ": <") (@Join ", " (my-map @Elt_To_String (wsl-ref //L1 2)))) ">"))
   (for-in /pair (@Final_Seg //L1 3) 
    (begin
     (@WL "")
     (@WS (string-append (string-append "   " (@Elt_To_String (wsl-ref /pair 1))) " := <"))
     (@WS (string-append (@Join ", " (my-map @Elt_To_String (@Final_Seg /pair 2))) ">"))))
   (@WL ""))))

; Use this to list the summaries of all procedures in all WHERE clauses: 
(define (@Print_Proc_Summ2)
 (let ((/summ-save /summ))
  (set! /summ '())
  (@Foreach_Statement /foreach-summ2-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /summ /summ-save)))

; Convert a proc summary (eg from the Proc_Summaries tables) 
; to an ordinary summary: 
(define (@Proc_To_Summ2 /summ)
 
 (list (cons 0 (@Final_Seg /summ 4))))

#t
