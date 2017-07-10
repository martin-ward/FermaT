;;; Scheme translation of WSL code
(define (/foreach-stack_to_return-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) /name) (= (@Size (@Get_n (@I) 3)) 0))
   (set! /var (@Make_Name (concat (string-append (@N_String /par) "_") /n)))
   (set! /locals (cons /var /locals))
   (set! /n (+ /n 1))
   ; Add the return parameter and push it onto the stack 
   (@Down_To 3)
   (@Paste_Over (@Make //T_/Lvalues '() (list (@Make //T_/Var_/Lvalue /var '()))))
   (@Up)
   (@Paste_After (@Make //T_/Push '() (list /stack (@Make //T_/Variable /var '()))))
   (set! /done 0)
   (@STR_Make_Local /var))))

(define (/foreach-stack_to_return-2 //Depth //A/S_/Type)
 (cond
  ((and (@Left?) (= (@ST (@I)) //T_/Var))
   (let ((/used (@Used (@Get_n (@I) 2))))
    (@Left)
    (cond
     ((and (= (@ST (@I)) //T_/Proc_/Call) (not (null? (intersection-n (@Assigned (@Get_n (@I) 3)) /used))) (@Trans? //T/R_/Merge_/Right))
      (@Trans //T/R_/Merge_/Right "")
      (set! /done 0))
     ((and (= (@ST (@I)) //T_/Push) (not (null? (intersection-n (@Assigned (@I)) /used))) (@Trans? //T/R_/Merge_/Right))
      (@Trans //T/R_/Merge_/Right "")
      (set! /done 0))
     (#t
      (@Right)))))))

(define (/foreach-stack_to_return-3 //Depth //A/S_/Type)
 (cond
  ((and (@Left?) (= (@ST (@I)) //T_/Pop) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue))
   (let ((/stack-save /stack))
    (set! /stack (@V (@Get_n (@I) 2)))
    (@Left)
    (cond
     ((and (member //T_/Push (@Stat_Types (@I))) (member /stack (@Assigned (@I))) (@Trans? //T/R_/Absorb_/Right))
      (@Trans //T/R_/Absorb_/Right "")
      (set! /done 0))
     (#t
      (@Right)))
    (set! /stack /stack-save)))))

(define (/foreach-stack_to_return-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Push) (@Trans? //T/R_/Move_/To_/Right))
   (@Trans //T/R_/Move_/To_/Right "")
   (set! /done 0)))
 (cond
  ((and (= (@ST (@I)) //T_/Push) (@Trans? //T/R_/Push_/Pop))
   (@Trans //T/R_/Push_/Pop "")
   (set! /done 0))))

(define (/foreach-stack_to_return-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) /name) (= (@Size (@Get_n (@I) 3)) 0))
   (set! /var (@Make_Name (concat (string-append (@N_String /par) "_") /n)))
   (set! /locals (cons /var /locals))
   (set! /n (+ /n 1))
   ; Add the return parameter and push it onto the stack 
   (@Down_To 3)
   (@Paste_Over (@Make //T_/Lvalues '() (list (@Make //T_/Var_/Lvalue /var '()))))
   (@Up)
   (@Paste_After (@Make //T_/Push '() (list /stack (@Make //T_/Variable /var '()))))
   (set! /done 0)
   (@STR_Make_Local /var))))

(define (/foreach-stack_to_return-6 //Depth //A/S_/Type)
 (cond
  ((and (@Left?) (= (@ST (@I)) //T_/Var))
   (let ((/used (@Used (@Get_n (@I) 2))))
    (@Left)
    (cond
     ((and (= (@ST (@I)) //T_/Proc_/Call) (not (null? (intersection-n (@Assigned (@Get_n (@I) 3)) /used))) (@Trans? //T/R_/Merge_/Right))
      (@Trans //T/R_/Merge_/Right "")
      (set! /done 0))
     ((and (= (@ST (@I)) //T_/Push) (not (null? (intersection-n (@Assigned (@I)) /used))) (@Trans? //T/R_/Merge_/Right))
      (@Trans //T/R_/Merge_/Right "")
      (set! /done 0))
     (#t
      (@Right)))))))

(define (/foreach-stack_to_return-7 //Depth //A/S_/Type)
 (cond
  ((and (@Left?) (= (@ST (@I)) //T_/Pop) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue))
   (let ((/stack-save /stack))
    (set! /stack (@V (@Get_n (@I) 2)))
    (@Left)
    (cond
     ((and (member //T_/Push (@Stat_Types (@I))) (member /stack (@Assigned (@I))) (@Trans? //T/R_/Absorb_/Right))
      (@Trans //T/R_/Absorb_/Right "")
      (set! /done 0))
     (#t
      (@Right)))
    (set! /stack /stack-save)))))

(define (/foreach-stack_to_return-8 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Push) (@Trans? //T/R_/Move_/To_/Right))
   (@Trans //T/R_/Move_/To_/Right "")
   (set! /done 0)))
 (cond
  ((and (= (@ST (@I)) //T_/Push) (@Trans? //T/R_/Push_/Pop))
   (@Trans //T/R_/Push_/Pop "")
   (set! /done 0))))

;
;==========================================================================
;FermaT Transformation System
;Copyright (C) 2015 Software Migrations Limited.
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
; Add a POP at the end of the procedure body to pop the value 
; from the stack into a VAR parameter. 
; Modify each call to PUSH the returned value back onto the stack. 
; Hopefully we can then merge the pushes and pops. 
; 
;    dotrans z-1.wsl Stack_To_Return posn=1,2,1,2,2
;
(define (@Stack_To_Return_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Proc))
   (@Fail "Selected item is not a procedure definition"))
  ((> (@Size (@Get_n (@I) 3)) 0)
   (@Fail "Procedure already has VAR parameters"))
  ((null? (@SRT_Find_Stack (@Get_n (@I) 4)))
   (@Fail "No suitable stack PUSH in the procedure body"))
  (#t
   (@Pass))))

(define (@Stack_To_Return_Code //Data)
 (let ((/posn (@Posn))
       (/name-save /name)
       (//O/K 1)
       (/stack-save /stack)
       (/str (@String /data))
       (/n-save /n)
       (/var-save /var)
       (/all (@Variables (@Program)))
       (/locals-save /locals)
       (/assigns '())
       (/done-save /done))
  (set! /name (@V (@Get_n (@I) 1)))
  (set! /stack (@SRT_Find_Stack (@Get_n (@I) 4)))
  (set! /n 1)
  (set! /var '())
  (set! /locals '())
  (set! /done 0)
  (cond
   ((equal? /str "")
    (set! /str (string-append (@N_String /name) "_Return"))))
  (set! /var (@Make_Name /str))
  (while (member /var /all) 
   (begin
    (set! /var (@Make_Name (concat (string-append /str "_") /n)))
    (set! /n (+ /n 1))))
  (set! /n 1)
  (set! /par /var)
  (set! /locals (list /par))
  ; Add the return parameter to the definition: 
  (@Down_To 3)
  ; to var pars 
  (@Paste_Over (@Make //T_/Lvalues '() (list (@Make //T_/Var_/Lvalue /par '()))))
  (@Up)
  ; Pop the stacked value into the parameter just before returning: 
  (@Down_To 4)
  (@Down_Last)
  ; last statement in body 
  (@Paste_After (@Make //T_/Pop '() (list (@Make //T_/Var_/Lvalue /par '()) /stack)))
  (@Up)
  (@Up)
  ; Find all the calls and fix them: 
  (@Up)
  (@Up)
  ; to WHERE clause 
  (@Edit)
  ; The outer loop is needed because ATEACH terminates early 
  ; when a VAR structure is inserted above the current position: 
  (set! /done 1)
  (@Ateach_Statement /foreach-stack_to_return-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Absorb a proc call into a VAR which references the parameter 
  (@Ateach_Statement /foreach-stack_to_return-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Absorb a POP into a preceding structure which contains a PUSH: 
  (@Ateach_Statement /foreach-stack_to_return-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Ateach_Statement /foreach-stack_to_return-4 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (while (not (= /done 1)) 
   (begin
    (set! /done 1)
    (@Ateach_Statement /foreach-stack_to_return-5 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    ; Absorb a proc call into a VAR which references the parameter 
    (@Ateach_Statement /foreach-stack_to_return-6 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    ; Absorb a POP into a preceding structure which contains a PUSH: 
    (@Ateach_Statement /foreach-stack_to_return-7 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Ateach_Statement /foreach-stack_to_return-8 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (cond
   (#f
    (display-list "locals are: " (my-map @N_String (@Sort_List /locals)))))
  (@End_Edit)
  (@Goto '())
  (@Trans //T/R_/Remove_/All_/Redundant_/Vars "")
  (cond
   ((@Valid_Posn? (@Program) /posn)
    (@Goto /posn)))
  (set! /name /name-save)
  (set! /stack /stack-save)
  (set! /n /n-save)
  (set! /var /var-save)
  (set! /locals /locals-save)
  (set! /done /done-save)))

; Look for a PUSH at the end of the given item 
(define (@SRT_Find_Stack //I)
 (let ((/stack-save /stack)
       (//O/K 1)
       (funct-result '()))
  (set! /stack '())
  (cond
   ((= (@GT //I) //T_/Statements)
    (set! /stack (@SRT_Find_Stack (@Get_n //I (@Size //I)))))
   ((not (= (@GT //I) //T_/Statement))
    (error (string-append "Unexpected type in @SRT_Find_Stack " (@Type_Name (@ST //I)))))
   ((= (@ST //I) //T_/Var)
    (set! /stack (@SRT_Find_Stack (@Get_n //I 2))))
   ((= (@ST //I) //T_/Push)
    (set! /stack (@Get_n //I 1)))
   ((or (= (@ST //I) //T_/Cond) (= (@ST //I) //T_/D_/If))
    (for-in //I (@Cs //I) 
     (cond
      ((null? /stack)
       (set! /stack (@SRT_Find_Stack (@Get_n //I 2))))
      ((not (@Equal? /stack (@SRT_Find_Stack (@Get_n //I 2))))
       (set! //O/K 0))))
    (cond
     ((= //O/K 0)
      (set! /stack '())))))
  (set! funct-result /stack)
  (set! /stack /stack-save)
  funct-result))

(define (@STR_Make_Local /var-par)
 (let ((/var-save /var))
  (set! /var /var-par)
  (let ((/rel_posn '())
        (/assign (@Make //T_/Assign '() (list (@Make //T_/Var_/Lvalue /var '()) //Mth_0))))
   (while (and (@Up?) (not (= (@ST (@I)) //T_/Var)) (not (= (@ST (@Parent)) //T_/Proc)) (not (= (@ST (@Parent)) //T_/Where))) 
    (begin
     (set! /rel_posn (cons (@Posn_n) /rel_posn))
     (@Up)))
   (cond
    ((not (= (@ST (@I)) //T_/Var))
     ; Add an enclosing VAR clause 
     (cond
      ((= (@GT (@I)) //T_/Statements)
       (@Paste_Over (@Make //T_/Statements '() (list (@Make //T_/Var '() (list (@Make //T_/Assigns '() '()) (@I))))))
       (@Down)
       (set! /rel_posn (cons 2 /rel_posn)))
      ((= (@GT (@I)) //T_/Statement)
       (@Paste_Over (@Make //T_/Var '() (list (@Make //T_/Assigns '() '()) (@Make //T_/Statements '() (list (@I))))))
       (set! /rel_posn (cons 2 (cons 1 /rel_posn))))
      (#t
       (error "Stack_To_Return expecting a statement or statements!")))))
   ; Insert the assignment into this VAR 
   (@Down)
   (@Paste_Over (@Make //T_/Assigns '() (concat (@Cs (@I)) (list /assign))))
   (@Up)
   ; Go back to original position 
   (while (not (null? /rel_posn)) 
    (begin
     (@Down_To (car /rel_posn))
     (set! /rel_posn (cdr /rel_posn)))))
  (set! /var /var-save)))

#t
