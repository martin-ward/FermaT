;;; Scheme translation of WSL code
(define (/foreach-unfold_proc_calls-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/S)
   (for-in /action (@Cs (@Get_n (@I) 2)) 
    (cond
     ((equal? (@V (@Get_n /action 1)) /dispatch)
      (for-in /pair (@Proc_Calls (@Get_n /action 2)) 
       (puthash /dispatch_call (wsl-ref /pair 1) 1))))))))

(define (/foreach-unfold_proc_calls-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (not (null? (gethash /unfold (@V (@Get_n (@I) 1))))))
   (@Unfold_Proc_Call (gethash /bodies (@V (@Get_n (@I) 1)))))))

(define (/foreach-unfold_proc_calls-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (not (null? (gethash /unfold (@V (@Get_n (@I) 1))))))
   (cond
    ((null? (gethash /done (@V (@Get_n (@I) 1))))
     (puthash /done (@V (@Get_n (@I) 1)) 1)
     (@UPC_Unfold (@V (@Get_n (@I) 1)))))
   (@Unfold_Proc_Call (gethash /bodies (@V (@Get_n (@I) 1)))))))

(define (/foreach-unfold_proc_calls-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Comment) (equal? (@V (@I)) " <ENTRY POINT> "))
   (set! //O/K 1))))

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
(define (@Unfold_Proc_Calls_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Where))
   (@Fail "Not a WHERE clause."))
  (#t
   (let ((/calls (@Qry_Call_Join (@Proc_Calls (@Get_n (@I) 1)) (@Proc_Calls (@Get_n (@I) 2))))
         (/local (hash-table))
         (/defn '())
         (/pair-save /pair)
         (/locals 0))
    (set! /pair '())
    (for-in /defn (@Cs (@Get_n (@I) 2)) 
     (cond
      ((= (@ST /defn) //T_/Proc)
       (set! /locals (+ /locals 1))
       (puthash /local (@V (@Get_n /defn 1)) 1)
       (cond
        ((and (< (@Stat_Count_NC (@Get_n /defn 4)) 4) (< (@Stat_Count (@Get_n /defn 4)) 12))
         (@Pass))))))
    (cond
     ((@Passed?)
      ; There is a small proc to unfold 
     )
     ((= /locals 0)
      (@Fail "No local procedures in the WHERE clause."))
     (#t
      (for-in /pair /calls 
       (cond
        ((@Ends_With? (wsl-ref /pair 1) "_ACTION")
         ; Don't unfold name_ACTION procs 
        )
        ((and (not (null? (gethash /local (wsl-ref /pair 1)))) (<= (wsl-ref /pair 2) 1))
         (@Pass))))
      (cond
       ((not (@Passed?))
        (@Fail "Everything is called more than once.")))))
    (set! /pair /pair-save)))))

(define (@Unfold_Proc_Calls_Code //Data)
 (let ((/calls (@Qry_Call_Join (@Proc_Calls (@Get_n (@I) 1)) (@Proc_Calls (@Get_n (@I) 2))))
       (/local (hash-table))
       (/defn '())
       (/pair-save /pair)
       (/new_defns '())
       (/body '())
       (/unfold-save /unfold)
       (/bodies-save /bodies)
       (/done-save /done)
       (/dispatch-save /dispatch)
       (/dispatch_call-save /dispatch_call))
  (set! /pair '())
  (set! /unfold (hash-table))
  (set! /bodies (hash-table))
  (set! /done (hash-table))
  (set! /dispatch (@Make_Name "dispatch"))
  (set! /dispatch_call (hash-table))
  (@Edit)
  ; (1) Calculate the list of calls to unfold 
  ; (2) Unfold the calls 
  ; (3) Delete the definitions of unfolded procs 
  ; Note that this will delete procs which have one recursive call 
  ; and no external calls. 
  ; A proc body can be unfolded either if there is only a single call 
  ; to the proc or if the body is `small', ie if there are 
  ; less than 3 non-comment statements and no calls to unfolded procs in the body. 
  ; Consider the case where A contains only a call to B and B is huge. 
  ; If there are many calls to A but no other calls to B, 
  ; then both A and B will be unfolded: resulting in a huge duplication! 
  ; Don't unfold anything which is called in dispatch (in case it is dead code 
  ; which we want to keep even when dispatch is eliminated) 
  (@Foreach_Statement /foreach-unfold_proc_calls-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (for-in /defn (@Cs (@Get_n (@I) 2)) 
   (cond
    ((= (@ST /defn) //T_/Proc)
     (puthash /local (@V (@Get_n /defn 1)) 1)
     (puthash /bodies (@V (@Get_n /defn 1)) /defn))
    (#t
     (set! /new_defns (cons /defn /new_defns)))))
  (for-in /pair /calls 
   (cond
    ((not (null? (gethash /local (wsl-ref /pair 1))))
     (set! /body (@Get_n (gethash /bodies (wsl-ref /pair 1)) 4))
     (cond
      ((and (= (wsl-ref /pair 2) 0) (@Is_Entry_Point? /body))
       ; Don't delete an uncalled proc with an <ENTRY POINT> comment 
      )
      ((@Ends_With? (wsl-ref /pair 1) "_ACTION")
       ; Don't unfold name_ACTION procs 
      )
      ((not (null? (gethash /dispatch_call (wsl-ref /pair 1))))
       ; Don't delete something called in dispatch 
      )
      ((or (and (< (@Stat_Count_NC /body) 4) (< (@Stat_Count /body) 12)) (<= (wsl-ref /pair 2) 1))
       (puthash /unfold (wsl-ref /pair 1) 1))))))
  ; Don't unfold a proc whose body contains calls to procs which will be unfolded 
  (for-in /defn (reverse (@Cs (@Get_n (@I) 2))) 
   (begin
    (set! /name (@V (@Get_n /defn 1)))
    (cond
     ((not (null? (gethash /unfold /name)))
      (for-in /pair (@Proc_Calls (@Get_n (gethash /bodies /name) 4)) 
       (cond
        ((not (null? (gethash /unfold (wsl-ref /pair 1))))
         (puthash /unfold /name '()))))))))
  ; First we need to do all the unfolding in proc bodies 
  ; (before unfolding call, unfold everything in ITS body). 
  (for-in /defn (@Cs (@Get_n (@I) 2)) 
   (cond
    ((and (= (@ST /defn) //T_/Proc) (null? (gethash /done (@V (@Get_n /defn 1)))))
     (puthash /done (@V (@Get_n /defn 1)) 1)
     (@UPC_Unfold (@V (@Get_n /defn 1))))))
  ; Then process the main program. 
  (@Down)
  (@Foreach_Statement /foreach-unfold_proc_calls-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Up)
  ; Compute the new defns list: 
  (for-in /defn (@Cs (@Get_n (@I) 2)) 
   (cond
    ((not (= (@ST /defn) //T_/Proc))
     (set! /new_defns (cons /defn /new_defns)))
    ((null? (gethash /unfold (@V (@Get_n /defn 1))))
     (set! /new_defns (cons (gethash /bodies (@V (@Get_n /defn 1))) /new_defns)))))
  ; Build a new WHERE clause (if there are any defns left): 
  (display-list "new_defns = " (gen-length /new_defns))
  (cond
   ((null? /new_defns)
    (@Paste_Over (@Get_n (@I) 1)))
   (#t
    (@Paste_Over (@Make //T_/Where '() (list (@Get_n (@I) 1) (@Make //T_/Definitions '() (reverse /new_defns)))))))
  (@End_Edit)
  (set! /pair /pair-save)
  (set! /unfold /unfold-save)
  (set! /bodies /bodies-save)
  (set! /done /done-save)
  (set! /dispatch /dispatch-save)
  (set! /dispatch_call /dispatch_call-save)))

; Unfold all the required proc calls in the given body. 
; Before unfolding each call, unfold all the calls in ITS body. 
(define (@UPC_Unfold /name-par)
 (let ((/name-save /name))
  (set! /name /name-par)
  (display-list "@UPC_Unfold: " (@N_String /name))
  (@Edit)
  (@New_Program (gethash /bodies /name))
  (@Foreach_Statement /foreach-unfold_proc_calls-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (puthash /bodies /name (@Program))
  (@Undo_Edit)
  (set! /name /name-save)))

; Unfold the (selected) proc call, given the definition: 
(define (@Unfold_Proc_Call /defn)
 (display-list "Unfolding Proc_Call " (@N_String (@V (@Get_n /defn 1))) " at " (@Posn))
 (cond
  ((and (= (@Size (@Get_n (@I) 3)) 0) (= (@Size (@Get_n (@I) 2)) 0))
   (@Splice_Over (@Cs (@Get_n /defn 4))))
  ((or (not (= (@Size (@Get_n (@I) 3)) (@Size (@Get_n /defn 3)))) (not (= (@Size (@Get_n (@I) 2)) (@Size (@Get_n /defn 2)))))
   (display-list "ERROR: Proc call does not match body:")
   (@Print_WSL (@I) ""))
  (#t
   (let ((/actual_vals (@Cs (@Get_n (@I) 2)))
         (/actual_vars (@Cs (@Get_n (@I) 3)))
         (/formal_vals (@Cs (@Get_n /defn 2)))
         (/formal_vars (@Cs (@Get_n /defn 3)))
         (/tmp '())
         (/tmp_v '())
         (/tmp_e '())
         (/var '())
         (/n 1)
         (/used (@Used /defn))
         (/empty (@Make //T_/Number 0 '()))
         (/a1 '())
         (/a2 '())
         (/a3 '())
         (/a4 '()))
    (set! /a2 (@Make_Assigns (concat /formal_vals /formal_vars) (concat /actual_vals /actual_vars)))
    ; We need to create a list of `new' temporary variable names 
    ; with the same length as formal_vars: 
    (for-in /var /formal_vars 
     (begin
      (set! /tmp (@Make_Name (string-append (@N_String (@V /var)) "_tmp")))
      (cond
       ((member /tmp /used)
        (set! /tmp (@Make_Name (concat (string-append (@N_String (@V /var)) "_tmp_") (@String /n))))
        (while (member /tmp /used) 
         (begin
          (set! /n (+ /n 1))
          (set! /tmp (@Make_Name (concat (string-append (@N_String (@V /var)) "_tmp_") (@String /n))))))))
      (set! /tmp_v (@Make //T_/Var_/Lvalue /tmp '()))
      (set! /tmp_e (@Make //T_/Variable /tmp '()))
      (set! /a1 (cons (@Make //T_/Assign '() (list /tmp_v /empty)) /a1))
      (set! /a3 (cons (@Make //T_/Assign '() (list /tmp_v (@Lvalue_To_Expn /var))) /a3))
      (set! /a4 (cons (@Make //T_/Assign '() (list (car /actual_vars) /tmp_e)) /a4))
      (set! /actual_vars (cdr /actual_vars))))
    (set! /a1 (@Make //T_/Assigns '() (reverse /a1)))
    (set! /a3 (@Make //T_/Assignment '() (reverse /a3)))
    (set! /a4 (@Make //T_/Assignment '() (reverse /a4)))
    (@Paste_Over (@Make //T_/Var '() (list /a1 (@Make //T_/Statements '() (list (@Make //T_/Var '() (list /a2 (@Make //T_/Statements '() (concat (@Cs (@Get_n /defn 4)) (list /a3))))) /a4)))))
    ; Simplify the resulting VAR clauses 
    (@Down_To 2)
    ; to outer VARs statements 
    (@Down)
    ; to inner VAR 
    (cond
     ((@Trans? //T/R_/Remove_/Redundant_/Vars)
      (@Trans //T/R_/Remove_/Redundant_/Vars "")))
    (@Up)
    (@Up)
    (cond
     ((@Trans? //T/R_/Remove_/Redundant_/Vars)
      (@Trans //T/R_/Remove_/Redundant_/Vars "")))))))

(define (@Make_Assigns /vars /expns)
 (let ((/assigns '()))
  (while (not (null? /vars)) 
   (begin
    (set! /assigns (cons (@Make //T_/Assign '() (list (@Expn_To_Lvalue (wsl-ref /vars 1)) (@Lvalue_To_Expn (wsl-ref /expns 1)))) /assigns))
    (set! /vars (cdr /vars))
    (set! /expns (cdr /expns))))
  (@Make //T_/Assigns '() (reverse /assigns))))

; Search for an <ENTRY POINT> comment: 
(define (@Is_Entry_Point? //I)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K 0)
  (@Edit)
  (@New_Program //I)
  (@Foreach_Statement /foreach-unfold_proc_calls-4 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Undo_Edit)
  (set! funct-result (= //O/K 1))
  (set! //O/K //O/K-save)
  funct-result))

;
;
;Unfolding a proc with parameters:
;
;Call: name(e1, e2 VAR var1, var2)
;
;Defn: name(v1, v2 VAR v3, v4)
;
;-->
;
;VAR < tmp1 := < >, tmp2 := < > >:
;  VAR < v1 := e1, v2 := e2, v3 := var1, v4 := var2 >:
;    body;
;    tmp1 := v3; tmp2 := v4 ENDVAR;
;  var1 := tmp1; var2 := tmp2 ENDVAR
;
;the `tmps' are needed because var1 might be the same as v3 etc.
;
;Apply Remove_Redundant_Vars after the transformation.
;
;
#t
