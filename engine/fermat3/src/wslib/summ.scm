;;; Scheme translation of WSL code
(define /%const__summ__1 (@Make 101 '() (list (@Make 9 (@Make_Name "dummy") '()) (@Make 10 '() (list (@Make 207 (@Make_Name "r0") '()) (@Make 207 (@Make_Name "r1") '()) (@Make 207 (@Make_Name "r2") '()) (@Make 207 (@Make_Name "r3") '()) (@Make 207 (@Make_Name "r4") '()) (@Make 207 (@Make_Name "r5") '()) (@Make 207 (@Make_Name "r6") '()) (@Make 207 (@Make_Name "r7") '()) (@Make 207 (@Make_Name "r8") '()) (@Make 207 (@Make_Name "r9") '()) (@Make 207 (@Make_Name "r10") '()) (@Make 207 (@Make_Name "r1") '()) (@Make 207 (@Make_Name "r12") '()) (@Make 207 (@Make_Name "r13") '()) (@Make 207 (@Make_Name "r14") '()) (@Make 207 (@Make_Name "r15") '()))) (@Make 12 '() (list (@Make 501 (@Make_Name "os") '()))))))
(define /%const__summ__2 (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "exit_flag") '()) (@Make 205 1 '()))))))))
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
; Summarise a given statement, returns: 
; <<d1, <e1, e2,...>, <v1, v2, ...>, <i1, i2, ...>, <x1, y1>, <x2, y2>, ....>, 
;  <d2, ...>, ...> 
; dn are the depth levels (cf Constant_Propagation lists) 
; en are referenced variables, vn are updated variables, 
; in are the incremented variables: in := in + {4|8|12} 
; xn := yn are assignments of constants or (non-modified) variables to variables 
; Note that a variable may be conditionally incremented, or incremented 
; any number of times. 
; Constants are numbers or strings (which include hex constants) 
; A variable is a list of symbols, possibly ending in a negative number: 
; (a b c -23) represents @[b].c[23] (cf Constant_Propagation) 
; This information can be represented as a statement sequence: 
; !P update(e1, e2, ... VAR v1, v2, ...); <x1 := y1, x2 := y2, ...> 
; (Note: either or both statements may be missing if the lists are empty) 
(set! /exit_flag (@Make_Name "exit_flag"))
(set! /a_name (@Make_Name "a"))
; Proc summaries are in the form: <body, val_pars, var_pars, e, v, assn, ...> 
; Convert a proc summary summ to a summary via: <<0> ++ summ[4..]> 
; (Currently we assume that Functs and BFuncts are `pure': ie they depend only 
; on their parameters and don't reference any global variables) 
; Use this to rename local variables and formal pars to avoid name clashes: 
(set! //S_/Par_/Count (- 1))
(define (@Summarise //I-par)
 (let ((//I-save //I)
       (//R '())
       (//S/T (@ST //I-par))
       (//G/T (@GT //I-par))
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((= //G/T //T_/Statements)
    (set! //R (@S_Sequence (@Cs //I))))
   ((= //G/T //T_/Expression)
    (set! //R (@S_Default //I)))
   ((= //G/T //T_/Condition)
    (set! //R (@S_Default //I)))
   ((= //S/T //T_/Cond)
    (set! //R (@S_Cond (@Cs //I))))
   ((= //S/T //T_/D_/If)
    (set! //R (@S_Cond (@Cs //I))))
   ((= //S/T //T_/Assignment)
    (set! //R (@S_Assigns (@Cs //I))))
   ((= //S/T //T_/Assert)
    (set! //R (@S_Assert (@Get_n //I 1))))
   ((= //S/T //T_/Floop)
    (set! //R (@S_Floop //I)))
   ((= //S/T //T_/While)
    (set! //R (@S_While //I)))
   ((= //S/T //T_/Var)
    (set! //R (@S_Var //I)))
   ((= //S/T //T_/For)
    (set! //R (@S_For //I)))
   ((= //S/T //T_/Proc_/Call)
    (set! //R (@S_Proc_Call //I)))
   ((= //S/T //T_/Where)
    (set! //R (@S_Where //I)))
   (#t
    (set! //R (@S_Default //I))))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

(define (@S_Sequence /comps)
 (let ((//R (list (list 0 '() '() '())))
       (/comp '()))
  (while (not (null? /comps)) 
   (begin
    (set! /comp (car /comps))
    (set! /comps (cdr /comps))
    (cond
     ((= (@ST /comp) //T_/Exit)
      (set! //R (@S_Increment //R (@V /comp)))
      (set! /comps '()))
     (#t
      (set! //R (@S_Seq //R (@Summarise /comp)))))))
  //R))

(define (@S_Increment //L /n)
 
 (if (or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0)) //L (@S_Merge (list (cons /n (cdr (wsl-ref //L 1)))) (cdr //L))))

; Apply the level 0 results (if any) sequentially to the rest of the list 
; and then decrement the list: 
(define (@S_Floop //I-par)
 (let ((//I-save //I)
       (//R '())
       (//L '())
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((@Gen_Improper? (@Get_n //I 1) "Hyb")
    (set! //L (@S_Sequence (@Cs (@Get_n //I 1)))))
   (#t
    (set! //L (@S_Loop (@S_Sequence (@Cs (@Get_n //I 1)))))))
  (cond
   ((null? //L)
    (set! //R '()))
   ((> (wsl-ref (wsl-ref //L 1) 1) 0)
    (set! //R (@S_Decrement //L 1)))
   (#t
    (set! //R (@S_Decrement (@S_Seq_Sub (cdr (wsl-ref //L 1)) (cdr //L)) 1))))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

; A while loop may not execute at all: so any assignments may not happen! 
; We know that the condition is false on exit from the loop. 
; There can be no EXITs from a WHILE loop, hence only a 0 level in the result. 
(define (@S_While //I-par)
 (let ((//I-save //I)
       (//L (@S_Sequence (@Cs (@Get_n //I-par 2))))
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((not (null? //L))
    (set! //L (list (cons 0 (cons (wsl-ref (wsl-ref //L 1) 2) (cons (wsl-ref (wsl-ref //L 1) 3) (cons (wsl-ref (wsl-ref //L 1) 4) (@S_Deny_Condition (@Get_n //I 1))))))))))
  (set! funct-result //L)
  (set! //I //I-save)
  funct-result))

; Process a summary of a statement which may be iterated: 
; WAS: if a variable is assigned from another variable which is itself assigned 
; then delete the assignment, in other words, keep only idempotent assigns. 
; But the assignment may not happen at all, if the loop exits first, 
; so to be safe we should delete all assignments. 
(define (@S_Loop_Old //L)
 (let ((/clobber '())
       (/var '())
       (/pair '())
       (//R '())
       (/vars '()))
  (cond
   ((null? //L)
    (set! //R //L))
   (#t
    (set! /vars (wsl-ref (wsl-ref //L 1) 3))
    (set! /clobber (@Set_Difference (wsl-ref (wsl-ref //L 1) 3) (wsl-ref (wsl-ref //L 1) 4)))
    (for-in /pair (@Final_Seg (wsl-ref //L 1) 5) 
     (cond
      ((or (not (sequence? (wsl-ref /pair 2))) (not (@Elt_Clash? /clobber (wsl-ref /pair 2))))
       (set! //R (cons /pair //R)))
      (#t
       ; add pair[1] to the set of modified variables: 
       (set! /vars (union-n (list (wsl-ref /pair 1)) /vars)))))
    (set! //R (cons (cons (wsl-ref (wsl-ref //L 1) 1) (cons (wsl-ref (wsl-ref //L 1) 2) (cons /vars (cons (wsl-ref (wsl-ref //L 1) 4) //R)))) (@S_Loop (cdr //L))))))
  //R))

(define (@S_Loop //L)
 (let ((/clobber '())
       (/var '())
       (/pair '())
       (//R '())
       (/vars '()))
  (cond
   ((null? //L)
    (set! //R //L))
   (#t
    (set! /vars (wsl-ref (wsl-ref //L 1) 3))
    (set! /clobber (@Set_Difference (wsl-ref (wsl-ref //L 1) 3) (wsl-ref (wsl-ref //L 1) 4)))
    (for-in /pair (@Final_Seg (wsl-ref //L 1) 5) 
     (cond
      ((and (> (wsl-ref (wsl-ref //L 1) 1) 0) (or (not (sequence? (wsl-ref /pair 2))) (not (@Elt_Clash? /clobber (wsl-ref /pair 2)))))
       (set! //R (cons /pair //R)))
      (#t
       ; add pair[1] to the set of modified variables: 
       (set! /vars (union-n (list (wsl-ref /pair 1)) /vars)))))
    (set! //R (cons (cons (wsl-ref (wsl-ref //L 1) 1) (cons (wsl-ref (wsl-ref //L 1) 2) (cons /vars (cons (wsl-ref (wsl-ref //L 1) 4) //R)))) (@S_Loop (cdr //L))))))
  //R))

(define (@S_Decrement //L /n)
 
 (if (null? //L) '() (if (< (wsl-ref (wsl-ref //L 1) 1) /n) (@S_Decrement (cdr //L) /n) (cons (cons (- (wsl-ref (wsl-ref //L 1) 1) /n) (cdr (wsl-ref //L 1))) (@S_Decrement (cdr //L) /n)))))

; Sequential merging of two summaries: 
; If the first statement is improper (ie L1[1][1] > 0), 
; then the second statement is unreachable and has no effect. 
; Otherwise, sequentially merge L1[1] with ALL of L2 
; and then parallel merge the rest of L1 with L2: 
(define (@S_Seq //L1 //L2)
 
 (if (or (null? //L1) (null? //L2)) '() (if (> (wsl-ref (wsl-ref //L1 1) 1) 0) //L1 (@S_Merge (list (cons (wsl-ref (wsl-ref //L2 1) 1) (@S_Seq_Vars (cdr (wsl-ref //L1 1)) (cdr (wsl-ref //L2 1))))) (@S_Merge (cdr //L1) (@S_Seq_Sub (cdr (wsl-ref //L1 1)) (cdr //L2)))))))

; Sequentially merge the vars with everything in the list: 
(define (@S_Seq_Sub /vars //L)
 
 (if (null? //L) '() (cons (cons (wsl-ref (wsl-ref //L 1) 1) (@S_Seq_Vars /vars (cdr (wsl-ref //L 1)))) (@S_Seq_Sub /vars (cdr //L)))))

; <e, v, i, <x1, y1>, ...> 
; Assignments in L2 take precidence over modified vars in L1 
; (ie when the first statement assigns an unknown value 
; but the second statement assigns a known value) 
; If the second statement clobbers a variable, 
; then ignore any assign in the first statement. 
; Also, if the second statement assigns to a var on RHS of an assign 
; in the first statement, then ignore this assign: 
(define (@S_Seq_Vars //L1 //L2)
 (let ((//R '())
       (/pair '())
       (/done-save /done)
       (/var '())
       (/assigned1 '())
       (/clobber1 '())
       (/clobber2 '())
       (/tab1 (hash-table))
       (/y1 '())
       (/restored '())
       (/e-save /e)
       (/v-save /v)
       (/inc '())
       (funct-result '()))
  (set! /done '())
  (set! /e '())
  (set! /v '())
  (cond
   ((or (null? //L1) (null? //L2))
    ; One of the statements gives no data 
   )
   (#t
    (set! /e (union-n (wsl-ref //L1 1) (wsl-ref //L2 1)))
    (set! /v (union-n (wsl-ref //L1 2) (wsl-ref //L2 2)))
    (set! /tab1 (@List_To_Hash (@Final_Seg //L1 4)))
    (set! /assigned1 (@Make_Set (my-map HEAD (@Final_Seg //L1 4))))
    (set! /clobber1 (@Elt_Subtract (wsl-ref //L1 2) (union-n /assigned1 (wsl-ref //L1 3))))
    (set! /clobber2 (@Elt_Subtract (wsl-ref //L2 2) (union-n (@Make_Set (my-map HEAD (@Final_Seg //L2 4))) (wsl-ref //L2 3))))
    ; Don't let @[rx] := e clobber @[FOO].BAR 
    (set! /clobber1 (@Set_Difference /clobber1 (list (list /a_name))))
    (set! /clobber2 (@Set_Difference /clobber2 (list (list /a_name))))
    (set! /inc (@Set_Difference (wsl-ref //L1 3) /clobber2))
    (for-in /pair (@Final_Seg //L2 4) 
     (begin
      (cond
       ((@Elt_Clash? /assigned1 (wsl-ref /pair 1))
        (set! /done (cons (wsl-ref /pair 1) /done))))
      (set! /y1 (gethash /tab1 (wsl-ref /pair 2)))
      (cond
       ((and (sequence? (wsl-ref /pair 2)) (@Elt_Clash? /clobber1 (wsl-ref /pair 2)))
        ; The first statement clobbers the RHS 
        (set! /e (union-n /e (list (wsl-ref /pair 2))))
        (set! /v (union-n /v (list (wsl-ref /pair 1)))))
       ((null? /y1)
        (set! //R (cons /pair //R)))
       ((equal? (wsl-ref /pair 1) /y1)
        ; The second assignment restores the variable: 
        (set! /restored (cons /y1 /restored)))
       (#t
        (set! //R (cons (list (wsl-ref /pair 1) /y1) //R))))
      ; Check for overwritten incs: 
      (cond
       ((member (wsl-ref /pair 1) /inc)
        (set! /inc (@Set_Difference /inc (list (wsl-ref /pair 1))))))
      ; Check for copied incs: 
      (cond
       ((and (sequence? (wsl-ref /pair 2)) (member (wsl-ref /pair 2) (wsl-ref //L1 3)))
        (set! /inc (union-n /inc (list (wsl-ref /pair 1))))))))
    (set! /done (@Make_Set /done))
    (for-in /pair (@Final_Seg //L1 4) 
     (cond
      ((and (not (@Elt_Clash? /done (wsl-ref /pair 1))) (not (@Elt_Clash? /clobber2 (wsl-ref /pair 1))))
       (set! //R (cons /pair //R)))
      ((member (wsl-ref /pair 1) /done)
       ; This assign is overwritten by an assign in second statement 
      )
      ((@Any_Prefix_In? (wsl-ref /pair 1) /done)
       ; This assign is overwritten by an assignment to the struct 
      )
      (#t
       ; Convert the assignment to a clobber: 
       (set! /v (union-n /v (list (wsl-ref /pair 1)))))))
    (set! //R (cons /e (cons (@Elt_Remove_Fields (@Set_Difference /v (@Make_Set /restored))) (cons (union-n /inc (@Elt_Subtract (wsl-ref //L2 3) /clobber1)) //R))))))
  (set! funct-result //R)
  (set! /done /done-save)
  (set! /e /e-save)
  (set! /v /v-save)
  funct-result))

; Merging two summaries in parallel (eg arms of an IF): 
(define (@S_Merge //L1 //L2)
 
 (if (null? //L1) //L2 (if (null? //L2) //L1 (if (< (wsl-ref (wsl-ref //L1 1) 1) (wsl-ref (wsl-ref //L2 1) 1)) (cons (wsl-ref //L1 1) (@S_Merge (cdr //L1) //L2)) (if (> (wsl-ref (wsl-ref //L1 1) 1) (wsl-ref (wsl-ref //L2 1) 1)) (cons (wsl-ref //L2 1) (@S_Merge //L1 (cdr //L2))) (cons (cons (wsl-ref (wsl-ref //L1 1) 1) (@S_Merge_Vars (cdr (wsl-ref //L1 1)) (cdr (wsl-ref //L2 1)))) (@S_Merge (cdr //L1) (cdr //L2))))))))

; <e, v, i, <x1, y1>, ...> 
; If one arm has exit_flag := 1 then take the results from the other arm 
; (unless it also has exit_flag := 1). 
; If one arm has exit_flag := 0 in its assigns and the other doesn't, 
; then we want to keep these assigns, but only rely on them when exit_flag 
; has been tested against zero. So move the other arms assigns to the v,e lists 
; and use these assigns as the result. 
; Fix to avoid this problem:
;  IF cc = 0
;    THEN exit_flag_1 := 0
;    ELSE exit_flag := 0; exit_flag_1 := 1 FI END
;ignores the first arm of the IF and therefore sets exit_flag_1 = 1 
(set! //S_/Tail_/Recursive_/Call (@Make_Name "S_Tail_Recursive_Call"))
(define (@S_Merge_Vars //L1 //L2)
 (let ((//R '())
       (/pair '())
       (/done-save /done)
       (/tab1 (hash-table))
       (/tab2 (hash-table))
       (/v1 '())
       (/v2 '())
       (/y2 '())
       (/e-save /e)
       (/v-save /v)
       (/i '())
       (funct-result '()))
  (set! /done (hash-table))
  (set! /e '())
  (set! /v '())
  (cond
   ((or (null? //L1) (null? //L2))
    ; One of the statements gives no data 
   )
   (#t
    (set! /tab1 (@List_To_Hash (@Final_Seg //L1 4)))
    (set! /tab2 (@List_To_Hash (@Final_Seg //L2 4)))
    (set! /v1 (gethash /tab1 (list /exit_flag)))
    (set! /v2 (gethash /tab2 (list /exit_flag)))
    (cond
     ((and (number? /v1) (= /v1 1) (not (and (number? /v2) (= /v2 1))))
      ; Ignore the results from L1: 
      (set! //R //L2))
     ((and (number? /v2) (= /v2 1) (not (and (number? /v1) (= /v1 1))))
      (set! //R //L1))
     (#t
      (set! /e (union-n (wsl-ref //L1 1) (wsl-ref //L2 1)))
      (set! /v (union-n (wsl-ref //L1 2) (wsl-ref //L2 2)))
      (set! /tab1 (@List_To_Hash (@Final_Seg //L1 4)))
      (set! /tab2 (@List_To_Hash (@Final_Seg //L2 4)))
      (set! /v1 (gethash /tab1 (list /exit_flag)))
      (set! /v2 (gethash /tab2 (list /exit_flag)))
      (cond
       ((and (number? /v1) (= /v1 0) (number? /v2) (not (= /v2 0)))
        ; Move L2's assigns to v,e and keep all of L1's assigns: 
        (let ((/-result- (@S_Move_Assign_Vars  (@Final_Seg //L2 4) /v /e)))
         (set! /v (car /-result-)) (set! /-result- (cdr /-result-))
         (set! /e (car /-result-)) (set! /-result- (cdr /-result-)))
        (set! //R (@Final_Seg //L1 4)))
       ((and (number? /v2) (= /v2 0) (number? /v1) (not (= /v1 0)))
        (let ((/-result- (@S_Move_Assign_Vars  (@Final_Seg //L1 4) /v /e)))
         (set! /v (car /-result-)) (set! /-result- (cdr /-result-))
         (set! /e (car /-result-)) (set! /-result- (cdr /-result-)))
        (set! //R (@Final_Seg //L2 4)))
       (#t
        ; Pick out only the assignments which are in both lists: 
        (for-in /pair (@Final_Seg //L1 4) 
         (cond
          ((equal? (gethash /tab2 (wsl-ref /pair 1)) (wsl-ref /pair 2))
           (set! //R (cons /pair //R))
           (puthash /done (wsl-ref /pair 1) 1))
          (#t
           ; Move this assign to the v,e lists: 
           (set! /v (union-n (list (wsl-ref /pair 1)) /v))
           (cond
            ((sequence? (wsl-ref /pair 2))
             (set! /e (union-n (list (wsl-ref /pair 2)) /e)))))))
        ; Extra assigns in the second list are also moved 
        (for-in /pair (@Final_Seg //L2 4) 
         (cond
          ((null? (gethash /done (wsl-ref /pair 1)))
           (set! /v (union-n (list (wsl-ref /pair 1)) /v))
           (cond
            ((sequence? (wsl-ref /pair 2))
             (set! /e (union-n (list (wsl-ref /pair 2)) /e)))))))))
      ; Keep non-clobbered incs from either arm: 
      (set! /i (@Set_Difference (union-n (wsl-ref //L1 3) (wsl-ref //L2 3)) (@Elt_Subtract /v (@Make_Set (my-map HEAD //R)))))
      (set! //R (cons /e (cons /v (cons /i //R))))))))
  (set! funct-result //R)
  (set! /done /done-save)
  (set! /e /e-save)
  (set! /v /v-save)
  funct-result))

(define (@S_Move_Assign_Vars /assigns /v /e)
 (let ((/pair '()))
  (for-in /pair /assigns 
   (begin
    (set! /v (union-n (list (wsl-ref /pair 1)) /v))
    (cond
     ((sequence? (wsl-ref /pair 2))
      (set! /e (union-n (list (wsl-ref /pair 2)) /e)))))))
 (list /v /e))

; Convert a list to a hash table: 
(define (@List_To_Hash //L)
 (let ((//R (hash-table))
       (/pair '()))
  (for-in /pair //L 
   (puthash //R (wsl-ref /pair 1) (wsl-ref /pair 2)))
  //R))

; The comps are all of type T_Guarded 
(define (@S_Cond /comps)
 (let ((//R '())
       (/comp '())
       (/deny (@Make //T_/True '() '())))
  ; Large conds are very inefficient: 
  (cond
   ((or (> (gen-length /comps) 20) (> (@Total_Size (@Get_n (car /comps) 1)) 200))
    (set! //R (@S_Default (@Make //T_/Cond '() /comps))))
   (#t
    (while (not (null? /comps)) 
     (begin
      (set! /comp (car /comps))
      (set! /comps (cdr /comps))
      ; Get the results of the condition and apply to rest of sequence: 
      (set! //R (@S_Merge //R (@S_Seq (@S_Cond_Test (@And /deny (@Get_n /comp 1))) (@Summarise (@Get_n /comp 2)))))
      (cond
       ((not (null? /comps))
        (set! /deny (@And /deny (@Not (@Get_n /comp 1))))))))))
  //R))

(define (@S_Assert //I-par)
 (let ((//I-save //I)
       (//R '())
       (funct-result '()))
  (set! //I //I-par)
  (set! //R (@S_Cond_Test //I))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

; Check for (x = y) AND ... etc. 
(define (@S_Cond_Test //I)
 
 (list (cons 0 (cons (@Elts_Used //I) (cons '() (cons '() (@S_Assert_Condition //I)))))))

; Return any assignments asserted by the condition: 
(define (@S_Assert_Condition //I)
 
 (if (= (@ST //I) //T_/Equal) (@S_Check_Pair (@Get_n //I 1) (@Get_n //I 2)) (if (= (@ST //I) //T_/And) (my-reduce @Set_Union (my-map @S_Assert_Condition (@Cs //I))) (if (= (@ST //I) //T_/Not) (@S_Deny_Condition (@Get_n //I 1)) '()))))

; Return any assignments denied by the condition: 
(define (@S_Deny_Condition //I)
 
 (if (= (@ST //I) //T_/Not_/Equal) (@S_Check_Pair (@Get_n //I 1) (@Get_n //I 2)) (if (= (@ST //I) //T_/Or) (my-reduce @Set_Union (my-map @S_Deny_Condition (@Cs //I))) (if (= (@ST //I) //T_/Not) (@S_Assert_Condition (@Get_n //I 1)) '()))))

; Check if the pair of expressions form a suitable assignment, 
; if so then return it: 
(define (@S_Check_Pair /e1 /e2)
 
 (if (not (@CP_Variable? /e1)) (if (and (@CP_Variable? /e2) (or (= (@ST /e1) //T_/Number) (= (@ST /e1) //T_/String))) (list (list (@CP_Var_Name /e2) (@V /e1))) '()) (if (@CP_Variable? /e2) (list (list (@CP_Var_Name /e1) (@CP_Var_Name /e2))) (if (or (= (@ST /e2) //T_/Number) (= (@ST /e2) //T_/String)) (list (list (@CP_Var_Name /e1) (@V /e2))) '()))))

; Check for assigning exit_flag a value > 1 (indicates a tail-recursive call). 
; Also check for x := x assigns (skip) and x := x + {4|8|12} (incremented variable): 
(define (@S_Assigns /comps)
 (let ((//R '())
       (/e-save /e)
       (/v-save /v)
       (/i '())
       (/assign '())
       (funct-result '()))
  (set! /e '())
  (set! /v '())
  (for-in //I /comps 
   (cond
    ((@LR_Equal? (@Get_n //I 1) (@Get_n //I 2))
     #t)
    ((and (= (@ST (@Get_n //I 2)) //T_/Variable) (@Starts_With? (@V (@Get_n //I 2)) "NOTUSED_"))
     #t)
    ((and (@CP_Variable? (@Get_n //I 2)) (@CP_Variable? (@Get_n //I 1)))
     (set! //R (cons (list (@CP_Var_Name (@Get_n //I 1)) (@CP_Var_Name (@Get_n //I 2))) //R)))
    (#t
     (set! /e (union-n (@Elts_Used //I) /e))
     (set! /v (union-n (@Elts_Assigned //I) /v))
     (cond
      ((and (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n //I 1)) /exit_flag) (= (@ST (@Get_n //I 2)) //T_/Number) (> (@V (@Get_n //I 2)) 1))
       (set! /v (union-n /v (list (list //S_/Tail_/Recursive_/Call))))))
     (cond
      ((and (@CP_Variable? (@Get_n //I 1)) (= (@ST (@Get_n //I 2)) //T_/Plus) (= (@ST (@Get_n (@Get_n //I 2) 2)) //T_/Number) (or (= (@V (@Get_n (@Get_n //I 2) 2)) 12) (= (@V (@Get_n (@Get_n //I 2) 2)) 8) (= (@V (@Get_n (@Get_n //I 2) 2)) 4)) (@CP_Variable? (@Get_n (@Get_n //I 2) 1)) (equal? (@CP_Var_Name (@Get_n //I 1)) (@CP_Var_Name (@Get_n (@Get_n //I 2) 1))))
       (set! /i (union-n (list (@CP_Var_Name (@Get_n //I 1))) /i)))
      ((and (@CP_Variable? (@Get_n //I 1)) (or (= (@ST (@Get_n //I 2)) //T_/Number) (= (@ST (@Get_n //I 2)) //T_/String)))
       (set! //R (cons (list (@CP_Var_Name (@Get_n //I 1)) (@V (@Get_n //I 2))) //R)))))))
  (set! funct-result (list (cons 0 (cons /e (cons /v (cons /i //R))))))
  (set! /e /e-save)
  (set! /v /v-save)
  funct-result))

; The default is to assume that there are no assigns available 
; (eg a !P or an expression or condition) 
(define (@S_Default //I-par)
 (let ((//I-save //I)
       (//L (list (@Elts_Used //I-par) (@Elts_Assigned //I-par) '()))
       (//R '())
       (funct-result '()))
  (set! //I //I-par)
  (for-in /v (reverse (@Gen_TVs //I "Hyb")) 
   (set! //R (cons (cons /v //L) //R)))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

; Remove local variables from a summary: 
(define (@S_Remove //L /names)
 (let ((//R '())
       (/pair '())
       (//L1 '())
       (/keep_vals '())
       (/keep_vars '()))
  (cond
   ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
    (set! //R //L))
   (#t
    (set! //L1 (cdr (wsl-ref //L 1)))
    (for-in /pair (@Final_Seg //L1 4) 
     (cond
      ((and (member (wsl-ref /pair 1) /names) (or (member (wsl-ref /pair 2) /names) (not (sequence? (wsl-ref /pair 2)))))
       ; an entirely local assign 
      )
      ((member (wsl-ref /pair 1) /names)
       ; Convert to a generic reference: 
       (set! /keep_vals (cons (wsl-ref /pair 2) /keep_vals)))
      ((member (wsl-ref /pair 2) /names)
       ; Convert to a generic assignment: 
       (set! /keep_vars (cons (wsl-ref /pair 1) /keep_vars)))
      (#t
       ; An entirely global assignment: 
       (set! //R (cons /pair //R)))))
    (set! //R (cons (cons 0 (cons (union-n (@Set_Difference (wsl-ref //L1 1) /names) (@Make_Set /keep_vals)) (cons (union-n (@Set_Difference (wsl-ref //L1 2) /names) (@Make_Set /keep_vars)) (cons (@Set_Difference (wsl-ref //L1 3) /names) //R)))) (cdr //L)))))
  //R))

; Add accessed variables to a summary: 
(define (@S_Add //L /vars)
 (let ((//R '())
       (/pair '())
       (//L1 '()))
  (cond
   ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
    (set! //R //L))
   (#t
    (set! //L1 (cdr (wsl-ref //L 1)))
    (set! //R (cons (cons 0 (cons (union-n (wsl-ref //L1 1) /vars) (@Final_Seg //L1 2))) (cdr //L)))))
  //R))

; Rename a variable in a summary: 
(define (@S_Rename //L /old /new)
 (let ((//R '())
       (//L1 '()))
  (cond
   ((or (null? /old) (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
    (set! //R //L))
   (#t
    (set! //L1 (cdr (wsl-ref //L 1)))
    (set! //R (cons (cons 0 (cons (@Make_Set (@S_Rename_sub (wsl-ref //L1 1) /old /new)) (cons (@Make_Set (@S_Rename_sub (wsl-ref //L1 2) /old /new)) (cons (@Make_Set (@S_Rename_sub (wsl-ref //L1 3) /old /new)) (@S_Rename_sub (@Final_Seg //L1 4) /old /new))))) (cdr //L)))))
  //R))

; Rename a variable in a structure: 
(define (@S_Rename_sub //L /old /new)
 (let ((//R '())
       (/elt-save /elt)
       (funct-result '()))
  (set! /elt '())
  (cond
   ((equal? //L /old)
    (set! //R /new))
   ((sequence? //L)
    (for-in /elt //L 
     (set! //R (cons (@S_Rename_sub /elt /old /new) //R)))
    (set! //R (reverse //R)))
   (#t
    (set! //R //L)))
  (set! funct-result //R)
  (set! /elt /elt-save)
  funct-result))

; Rename the local vars (to avoid clashes), 
; Add the initial assignments to the body and remove the local vars: 
(define (@S_Var //I-par)
 (let ((//I-save //I)
       (//R (@S_Sequence (@Cs (@Get_n //I-par 2))))
       (/init '())
       (/assign '())
       (/var '())
       (/vals '())
       (/vars '())
       (funct-result '()))
  (set! //I //I-par)
  (for-in /assign (@Cs (@Get_n //I 1)) 
   (begin
    (set! /var (@CP_Var_Name (@Get_n /assign 1)))
    (cond
     ((not (null? /var))
      (set! //S_/Par_/Count (- //S_/Par_/Count 1))
      (set! //R (@S_Rename //R /var (list //S_/Par_/Count)))
      (set! /vars (cons (list //S_/Par_/Count) /vars))
      (cond
       ((or (= (@ST (@Get_n /assign 2)) //T_/Number) (= (@ST (@Get_n /assign 2)) //T_/String))
        (set! /init (cons (list (list //S_/Par_/Count) (@V (@Get_n /assign 2))) /init)))
       ((@CP_Variable? (@Get_n /assign 2))
        (set! /init (cons (list (list //S_/Par_/Count) (@CP_Var_Name (@Get_n /assign 2))) /init))
        (set! /vals (cons (@CP_Var_Name (@Get_n /assign 2)) /vals))))))))
  (set! /vals (@Make_Set /vals))
  (set! /vars (@Make_Set /vars))
  (set! /init (list (cons 0 (cons /vals (cons /vars (cons '() /init))))))
  (set! //R (@S_Remove (@S_Seq /init //R) /vars))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

(define (@S_For //I)
 
 (@S_Add (@S_Remove (@S_Loop (@S_Sequence (@Cs (@Get_n //I 5)))) (list (list (@V (@Get_n //I 1))))) (union-n (@Elts_Used (@Get_n //I 2)) (@Elts_Used (@Get_n //I 3)) (@Elts_Used (@Get_n //I 4)))))

; Look for a summary of the proc body and update according to the parameters: 
; A proc summary is: <body, val_pars, var_pars, e, v, i, assigns...> 
(define (@S_Proc_Call //I-par)
 (let ((//I-save //I)
       (//R '())
       (/summ (@S_Get_Proc_Summary (@V (@Get_n //I-par 1)) //Proc_/Summaries))
       (/actual_vals '())
       (/formal_vals '())
       (/formal_vars '())
       (/actual '())
       (/init '())
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((null? /summ)
    ; Either we haven't processed the definition, or this is a recursive call: 
    (set! //R (@S_Default //I)))
   (#t
    (set! //R (list (cons 0 (@Final_Seg /summ 4))))
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
      (set! /formal_vals (wsl-ref /summ 2))
      (set! /formal_vars (wsl-ref /summ 3))
      ; create init as formal_vals := actual_vals 
      (for-in /actual (@Cs (@Get_n //I 2)) 
       (begin
        (cond
         ((or (= (@ST /actual) //T_/Number) (= (@ST /actual) //T_/String))
          (set! /init (cons (list (car /formal_vals) (@V /actual)) /init)))
         ((@CP_Variable? /actual)
          (set! /actual_vals (cons (@CP_Var_Name /actual) /actual_vals))
          (set! /init (cons (list (car /formal_vals) (car /actual_vals)) /init))))
        (set! /formal_vals (cdr /formal_vals))))
      (set! /init (list (cons 0 (cons (@Make_Set /actual_vals) (cons (@Make_Set /formal_vals) (cons '() /init))))))
      (for-in /actual (@Cs (@Get_n //I 3)) 
       (begin
        (set! //R (@S_Rename //R (car /formal_vars) (@CP_Var_Name /actual)))
        (set! /formal_vars (cdr /formal_vars))))
      (set! //R (@S_Seq /init //R))
      (set! //R (@S_Remove //R (wsl-ref /summ 2)))))))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

; Return the summary of the proc from the list, given its name: 
; bodies is a list of tables: name -> <body, val_pars, var_pars, e, v, i, assigns...> 
(define (@S_Get_Proc_Summary /name /bodies)
 (let ((/tab '())
       (//R '()))
  (cond
   ((not (null? /bodies))
    (set! /tab (car /bodies))
    (set! //R (gethash /tab /name))
    (cond
     ((null? //R)
      (set! //R (@S_Get_Proc_Summary /name (cdr /bodies)))))))
  //R))

; Process the proc/funct bodies (depth first order) and temporarily prepend 
; a table to Proc_Summaries while we process the body of the where. 
(define (@S_Where //I-par)
 (let ((//I-save //I)
       (//R '())
       (funct-result '()))
  (set! //I //I-par)
  (set! //Proc_/Summaries (cons (hash-table) //Proc_/Summaries))
  (@Summarise_Where_Defns (@Cs (@Get_n //I 2)))
  (set! //R (@Summarise (@Get_n //I 1)))
  (set! //Proc_/Summaries (cdr //Proc_/Summaries))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

; Add summaries of all the procs to the first table in Proc_Summaries: 
; (This is also used in Constant_Propagation) 
(define (@Summarise_Where_Defns //L)
 (let ((/body '())
       (/done-save /done)
       (/bodies (hash-table)))
  (set! /done (hash-table))
  (for-in /body //L 
   (puthash /bodies (@V (@Get_n /body 1)) /body))
  (for-in /body //L 
   (cond
    ((null? (gethash /done (@V (@Get_n /body 1))))
     ; Summarise the body, first summarising any (non-recursivaly) called procs: 
     (set! /done (@S_Summarise_Body  /bodies /body /done)))))
  (set! /done /done-save)))

; NB could have a call to a higher WHERE clause: 
(define (@S_Summarise_Body /bodies /body /done-par)
 (let ((/done-save /done)
       (funct-result '()))
  (set! /done /done-par)
  (let ((/calls '())
        (/pair '())
        (/name '()))
   (puthash /done (@V (@Get_n /body 1)) 1)
   (cond
    ((= (@ST /body) //T_/Proc)
     (set! /calls (@Proc_Calls (@Get_n /body 4)))
     (for-in /pair /calls 
      (begin
       (set! /name (car /pair))
       (cond
        ((and (null? (gethash /done /name)) (not (null? (gethash /bodies /name))))
         ; Find the body and process it first: 
         (set! /done (@S_Summarise_Body  /bodies (gethash /bodies /name) /done))))))))
   ; All called procs have been processed: 
   (@S_Summarise_Body_Sub /body))
  (set! funct-result /done)
  (set! /done /done-save)
  funct-result))

(define (@S_Summarise_Body_Sub /body)
 (let ((/summ '())
       (/vals '())
       (/vars '())
       (/par '()))
  (set! /summ (@Summarise (@Get_n /body 4)))
  (cond
   ((and (not (null? /summ)) (= (wsl-ref (wsl-ref /summ 1) 1) 0))
    (for-in /par (@Cs (@Get_n /body 2)) 
     (begin
      (set! //S_/Par_/Count (- //S_/Par_/Count 1))
      (set! /summ (@S_Rename /summ (@CP_Var_Name /par) (list //S_/Par_/Count)))
      (set! /vals (cons (list //S_/Par_/Count) /vals))))
    (for-in /par (@Cs (@Get_n /body 3)) 
     (begin
      (set! //S_/Par_/Count (- //S_/Par_/Count 1))
      (set! /summ (@S_Rename /summ (@CP_Var_Name /par) (list //S_/Par_/Count)))
      (set! /vars (cons (list //S_/Par_/Count) /vars))))
    (set! //Proc_/Summaries (@S_Put_Proc_Summary  (@V (@Get_n /body 1)) (cons /body (cons (@Make_Set /vals) (cons (@Make_Set /vars) (cdr (wsl-ref /summ 1))))) //Proc_/Summaries))))))

(define (@S_Put_Proc_Summary /name /value //Proc_/Summaries)
 (let ((/tab (if (null? //Proc_/Summaries) (hash-table) (car //Proc_/Summaries))))
  (puthash /tab /name /value)
  (cond
   ((null? //Proc_/Summaries)
    (set! //Proc_/Summaries (list /tab)))
   (#t
    (wsl-set! //Proc_/Summaries /tab 1))))
 //Proc_/Summaries)

; Convert a summary to equivalent WSL code 
; For any summary of length <=1, @Summarise(@Summ_To_WSL(summ)) 
; should be the same as summ (modulo ordering of lists). 
(define (@Summ_To_WSL //L)
 (let ((//R '())
       (/body '())
       (/dummy_ref /%const__summ__1))
  (cond
   ((null? //L)
    (set! //R /%const__summ__2))
   ((= (gen-length //L) 1)
    (set! //R (@Summ_To_WSL_Sub (cdr (wsl-ref //L 1))))
    (cond
     ((> (wsl-ref (wsl-ref //L 1) 1) 0)
      (set! //R (@Make //T_/Statements '() (concat (@Cs //R) (list (@Make //T_/Exit (wsl-ref (wsl-ref //L 1) 1) '()))))))
     ((= (@Size //R) 0)
      (set! //R (@Skips)))))
   (#t
    ; Multiple exit values to be accounted for 
    (let ((/expns (@Make //T_/Expressions '() (my-map @Name_To_WSL (wsl-ref (wsl-ref //L 1) 2))))
          (/count 0)
          (/body '()))
     (while (not (null? //L)) 
      (begin
       (set! /count (+ /count 1))
       (set! /body (@Summ_To_WSL_Sub (cdr (wsl-ref //L 1))))
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
         (set! /cond (@Make //T_/X_/B/Funct_/Call '() (list (@Make //T_/Name (@Make_Name (string-append "test" (@String /count))) '()) /expns)))))
       (set! //R (cons (@Make //T_/Guarded '() (list /cond /body)) //R)))))
    (set! //R (@Make //T_/Statements '() (list (@Make //T_/Cond '() (reverse //R)))))))
  (@Edit)
  (@New_Program //R)
  (@Trans //T/R_/Remove_/All_/Redundant_/Vars "")
  (@Down_Last)
  (@Paste_After /dummy_ref)
  (@Up)
  (@Trans //T/R_/Delete_/All_/Redundant "")
  (@Down_Last)
  (cond
   ((@Equal? (@I) /dummy_ref)
    (@Clever_Delete)))
  (set! //R (@Program))
  (@Undo_Edit)
  //R))

; Construct a VAR clause to save the initial values of clobbered variables 
; Generate a !P to update non-assigned variables 
; Add assignments to the assigned variables. 
(define (@Summ_To_WSL_Sub //L)
 (let ((/e-save /e)
       (/v-save /v)
       (/inc (wsl-ref //L 3))
       (/assigns (@Final_Seg //L 4))
       (//R '())
       (/inits '())
       (/updates '())
       (/pair '())
       (/e1 '())
       (/e2 '())
       (/v1 '())
       (/v2 '())
       (/var '())
       (funct-result '()))
  (set! /e (wsl-ref //L 1))
  (set! /v (@Set_Difference (wsl-ref //L 2) (wsl-ref //L 3)))
  (for-in /pair /assigns 
   (begin
    (set! /v (@Elt_Remove /v (list (wsl-ref /pair 1))))
    (cond
     ((sequence? (wsl-ref /pair 2))
      ; save pair[2] in a local variable 
      (set! /v1 (@Make_Name (string-append "save__" (@Join "_" (my-map @N_String (wsl-ref /pair 2))))))
      (set! /e1 (@Name_To_WSL (wsl-ref /pair 2)))
      (set! /e2 (@Make //T_/Variable /v1 '()))
      (set! /inits (cons (@Make //T_/Assign '() (list (@Make //T_/Var_/Lvalue /v1 '()) /e1)) /inits)))
     (#t
      (set! /e2 (@Name_To_WSL (wsl-ref /pair 2)))))
    (set! /v2 (@Expn_To_Lvalue (@Name_To_WSL (wsl-ref /pair 1))))
    (set! /updates (cons (@Make //T_/Assign '() (list /v2 /e2)) /updates))))
  (cond
   ((not (null? /v))
    (set! //R (list (@Make //T_/A_/Proc_/Call '() (list (@Make //T_/Name (@Make_Name "update") '()) (@Make //T_/Expressions '() (my-map @Name_To_WSL /e)) (@Make //T_/Lvalues '() (my-map @Expn_To_Lvalue (my-map @Name_To_WSL /v)))))))))
  (while (not (null? /updates)) 
   (begin
    (set! //R (cons (@Make //T_/Assignment '() (list (car /updates))) //R))
    (set! /updates (cdr /updates))))
  ; Add code for the increments. 
  ; We have to use a loop since we don't know exacly how many increments 
  ; (if any) will be executed on the variable: 
  (while (not (null? /inc)) 
   (begin
    (set! /var (@Name_To_WSL (car /inc)))
    (set! /inc (cdr /inc))
    (set! //R (cons (@Make 141 '() (list (@Make 301 '() (list (@Make 9 (@Make_Name "incr") '()) (@Make 10 '() (list (@Var_To_Expn /var))))) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Make 220 '() (list (@Var_To_Expn /var) (@Make 205 4 '()))))))))))) //R))))
  (set! //R (@Make //T_/Statements '() (reverse //R)))
  ; Make a VAR clause if required: 
  (cond
   ((not (null? /inits))
    (set! //R (@Make //T_/Var '() (list (@Make //T_/Assigns '() /inits) //R)))
    (set! //R (@Make //T_/Statements '() (list //R)))))
  (set! funct-result //R)
  (set! /e /e-save)
  (set! /v /v-save)
  funct-result))

(define (@Name_To_WSL /name)
 (let ((//R '()))
  (cond
   ((string? /name)
    (set! //R (@Make //T_/String /name '())))
   ((number? /name)
    (set! //R (@Make //T_/Number /name '())))
   ((= (gen-length /name) 1)
    (set! //R (@Make //T_/Variable (car /name) '())))
   ((and (number? (last-1 /name)) (< (last-1 /name) 0))
    (set! //R (@Make //T_/Aref '() (list (@Name_To_WSL (butlast-1 /name)) (@Make //T_/Expressions '() (list (@Make //T_/Number (- (last-1 /name)) '())))))))
   (#t
    (cond
     ((equal? (car /name) /a_name)
      (set! //R (@Make //T_/Aref '() (list (@Make //T_/Variable /a_name '()) (@Make //T_/Expressions '() (list (@Make //T_/Variable (wsl-ref /name 2) '()))))))
      (set! /name (cdr /name)))
     (#t
      (set! //R (@Make //T_/Variable (car /name) '()))))
    (set! /name (cdr /name))
    (while (not (null? /name)) 
     (begin
      (set! //R (@Make //T_/Struct '() (list (@Make //T_/Name (car /name) '()) //R)))
      (set! /name (cdr /name))))))
  //R))

; Print the summary: 
(define (@Print_Summ //L)
 (cond
  ((null? //L)
   (display-list "  exit_flag := 1;"))
  (#t
   (while (not (null? //L)) 
    (begin
     (display-list-flush (wsl-ref (wsl-ref //L 1) 1) ": < ")
     (for-in /elt (wsl-ref (wsl-ref //L 1) 2) 
      (begin
       (@Print_Name /elt)
       (display-list-flush " ")))
     (display-list-flush ">< ")
     (for-in /elt (wsl-ref (wsl-ref //L 1) 3) 
      (begin
       (@Print_Name /elt)
       (display-list-flush " ")))
     (display-list-flush ">< ")
     (for-in /elt (wsl-ref (wsl-ref //L 1) 4) 
      (begin
       (@Print_Name /elt)
       (display-list-flush " ")))
     (display-list ">")
     (cond
      ((> (wsl-ref (wsl-ref //L 1) 1) 0)
       (display-list-flush "  EXIT(" (wsl-ref (wsl-ref //L 1) 1) ");")))
     (@Print_Assigns_Sub (wsl-ref //L 1))
     (set! //L (cdr //L))
     (display-list ""))))))

(define (@Print_Assigns_Sub //L)
 (let ((/pair '())
       (/val '()))
  (for-in /pair (@Final_Seg //L 5) 
   (begin
    (display-list-flush "  ")
    (@Print_Name (wsl-ref /pair 1))
    (display-list-flush " := ")
    (set! /val (wsl-ref /pair 2))
    (cond
     ((string? /val)
      (display-list-flush (concat (concat //Quote /val) //Quote)))
     ((number? /val)
      (display-list-flush /val))
     (#t
      (@Print_Name /val)))))))

(define (@Print_Name /name)
 (cond
  ((and (> (gen-length /name) 1) (equal? (car /name) /a_name))
   (display-list-flush "@[" (@N_String (wsl-ref /name 2)) "]")
   (set! /name (cdr /name)))
  ((and (number? (car /name)) (< (car /name) 0))
   (display-list-flush "[" (- (car /name)) "]"))
  (#t
   (display-list-flush (@N_String (car /name)))))
 (set! /name (cdr /name))
 (cond
  ((not (null? /name))
   (display-list-flush ".")
   (@Print_Name /name))))

#t
