;;; Scheme translation of WSL code
(define (/foreach-push_pop-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (member /os_name (@Assigned (@Get_n (@I) 3))))
   (set! //Found 1))))

(define /%const__push_pop__1 (@Make 135 '() (list (@Make 501 (@Make_Name "__DUMMY__") '()) (@Make 506 -1 '()))))
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
; Replace PUSH(stack, e); ... POP(v, stack)  by 
; VAR < tmp_N := e >: ... v := tmp_N ENDVAR 
; NB: no need to pop into __DUMMY__ 
; POP statement could be inside a VAR: put this VAR inside our VAR 
; The VAR clauses mess up Fix_Assembler (when it makes local procs 
; it moves up to the top, or to an enclosing VAR!) 
; Note: !P proc calls with os in the VAR part can clobber anything. 
(define (@Push_Pop_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Push))
   (@Fail "Current item is not a PUSH statement"))
  ((not (@Right?))
   (@Fail "No statements to the right of current item "))
  ((null? (@PP_Find_Pop (@Get_n (@I) 1) (@Variables (@Get_n (@I) 1)) (@Variables (@Get_n (@I) 2))))
   (@Fail "Cannot find a suitable POP"))
  (#t
   (@Pass))))

(define (@PP_Find_Pop /stack /vars /used)
 (let ((/pop '())
       (/all (@Variables (@I))))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((not (@Right?))
     (set! /fl_flag1 1))
    (#t
     (@Right)
     (while (and (= (@ST (@I)) //T_/Var) (null? (intersection-n (@Assigned (@Get_n (@I) 1)) /all))) 
      (begin
       (@Down_To 2)
       (@Down)))
     (cond
      ((and (= (@ST (@I)) //T_/Pop) (@Equal? (@Get_n (@I) 2) /stack))
       (set! /pop (@Posn))
       (set! /fl_flag1 1))
      ((not (null? (intersection-n (@Variables (@I)) /vars)))
       (set! /fl_flag1 1))
      ((not (null? (intersection-n (@Assigned (@I)) /used)))
       (set! /fl_flag1 1))
      ((not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set)))
       (set! /fl_flag1 1))
      ((not (@Is_Proper?))
       (set! /fl_flag1 1))
      (#t
       (set! /fl_flag1 0))))))
  /pop))

(define (@Push_Pop_Code //Data)
 (let ((/posn (@Posn))
       (/p1 (@Posn_n))
       (/p2 0)
       (/stack (@Get_n (@I) 1))
       (/vars (@Variables (@Get_n (@I) 1)))
       (/pop '())
       (/v '())
       (/e (@Get_n (@I) 2))
       (/tmp '())
       (/parent (@Parent))
       (/body '())
       (//L '())
       (/needed 0))
  (set! /pop (@PP_Find_Pop /stack /vars (@Variables (@Get_n (@I) 2))))
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__push_pop__1 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__v_save /v))
      (set! /v (vector-ref /__/Match_array 0))
      (@Clever_Delete)
      (@Goto /posn)
      (@Clever_Delete)
      (set! /v /__v_save)))
    (#t
     (set! /v (@Get_n (@I) 1))
     (while (> (gen-length (@Posn)) (gen-length /posn)) 
      (@Up))
     (set! /p2 (@Posn_n))
     (set! /body (@Get_L (@Parent) (+ /p1 1) /p2))
     (@Goto /pop)
     (cond
      ((null? (intersection-n (@Variables /e) (@Assigned (@Make //T_/Statements '() /body))))
       (set! /needed 0)
       (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /e)))))))
      (#t
       (set! /needed 1)
       (set! /tmp (@PP_New_Var (@Make_Name "tmp") /parent))
       (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /tmp))))))))
     (while (> (gen-length (@Posn)) (gen-length /posn)) 
      (@Up))
     (set! /p2 (@Posn_n))
     (set! /body (@Get_L (@Parent) (+ /p1 1) /p2))
     (@Up)
     (set! //L (@Cs (@I)))
     (@Paste_Over (@Make //T_/Statements '() (concat (@Sub_Seg //L 1 /p1) (@Sub_Seg //L (+ /p2 1) (gen-length //Comps)))))
     (@Down_To /p1)
     (cond
      ((= /needed 1)
       (@Paste_Over (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /tmp) (@Var_To_Expn /e))))) (@Make 17 '() /body))))
       (cond
        ((@Trans? //T/R_/Remove_/Redundant_/Vars)
         (@Trans //T/R_/Remove_/Redundant_/Vars ""))))
      (#t
       (@Splice_Over /body))))))))

; Find an unused tmp name: 
(define (@PP_New_Var /tmp //I)
 (let ((//R /tmp)
       (/n 1)
       (/vars (@Variables //I)))
  (cond
   ((member //R /vars)
    (while (member (@Make_Name (concat (@N_String /tmp) (@String /n))) /vars) 
     (set! /n (+ /n 1)))
    (set! //R (@Make_Name (concat (@N_String /tmp) (@String /n))))))
  (@Make //T_/Variable //R '())))

(define (@Push_Pop_Test_old)
 (cond
  ((not (= (@ST (@I)) //T_/Push))
   (@Fail "Current item is not a PUSH statement"))
  ((not (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue))
   (@Fail "Stack is not a simple variable"))
  ((not (= (@ST (@Get_n (@I) 2)) //T_/Variable))
   (@Fail "PUSH statement does not push a simple variable"))
  (#t
   (@Pass))))

(define (@Push_Pop_Code_old //Data)
 (let ((/vars '()))
  (cond
   ((and (= (@ST (@I)) //T_/Push) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@I) 2)) //T_/Variable) (@Right?))
    ; Move the PUSH forwards, if possible 
    (set! /vars (@Used (@I)))
    (@Cut)
    (while (and (@Right?) (null? (intersection-n /vars (@Assigned (@I)))) (@PPC_A_Proc_Call_os? (@I)) (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set)) (@Is_Proper?)) 
     (@Right))
    (@Paste_Before (@Buffer))
    (let ((/p1 (@Posn_n))
          (/p2 0)
          (/stack (@V (@Get_n (@I) 1)))
          (/v1 (@Get_n (@I) 2))
          (/v2 '()))
     ; Look for the nearest corresponding POP with 
     ; with no intervening PUSH: 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (cond
       ((not (@Is_Proper?))
        (set! /fl_flag1 1))
       ((member //T_/Call (@Stat_Types (@I)))
        (set! /fl_flag1 1))
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (cond
         ((and (= (@ST (@I)) //T_/Push) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@I) 1)) /stack))
          (set! /fl_flag1 1))
         ((and (= (@ST (@I)) //T_/Push) (= (@ST (@Get_n (@I) 2)) //T_/Variable) (equal? (@V (@Get_n (@I) 2)) (@V /v1)))
          (set! /fl_flag1 1))
         ((and (= (@ST (@I)) //T_/Pop) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@I) 2)) /stack) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue))
          (set! /p2 (@Posn_n))
          (set! /v2 (@Get_n (@I) 1))
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0))))))
     (@To /p1)
     (cond
      ((= /p2 (+ /p1 1))
       ; PUSH is immediately followed by POP 
       ; Convert to a simple assignment if the value pushed 
       ; is not popped into the same variable: 
       (let ((/e (@Get_n (@I) 2))
             (/v '()))
        (@Delete)
        (cond
         ((@LR_Equal? (@Get_n (@I) 2) /e)
          (@Clever_Delete))
         (#t
          (set! /v (@Get_n (@I) 2))
          (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /e))))))))))
      ((> /p2 0)
       (@Push_Pop_Doit /p1 /p2 /v1 /v2))))))))

(define (@Push_Pop_Doit /p1 /p2 /v1 /v2)
 (let ((//S (@Get_L (@Parent) (+ /p1 1) (- /p2 1)))
       (/tmp (@Make_Name "__tmp")))
  (cond
   ((not (@PPC_A_Proc_Call_os? (@Make //T_/Statements '() //S)))
    (display-list "Adding VAR for PUSH/POP at " /p1)
    (cond
     ((or (equal? (@V /v1) (@V /v2)) (equal? (@V /v2) (@Make_Name "__DUMMY__")))
      (@Paste_Over (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /v1) (@Var_To_Expn /v1))))) (@Make 17 '() //S)))))
     ((null? (intersection-n (@Stat_Types (@Make //T_/Statements '() //S)) //Call_/Types_/Set))
      (set! /tmp (@PP_New_Var /tmp (@Make //T_/Statements '() //S)))
      (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (append (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /tmp) (@Var_To_Expn /v1)))))) //S (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v2) (@Var_To_Expn /tmp)))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '())))))))))
     (#t
      (set! /tmp (@PP_New_Var /tmp (@Program)))
      (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (append (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /tmp) (@Var_To_Expn /v1)))))) //S (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v2) (@Var_To_Expn /tmp)))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))
    (cond
     ((> /p2 /p1)
      (@Right)
      (for /i (+ /p1 1) /p2 1 
       (@Clever_Delete))))
    (@To /p1))))
 (cond
  ((@Trans? //T/R_/Remove_/Redundant_/Vars)
   (@Trans //T/R_/Remove_/Redundant_/Vars ""))))

; Check if item contains an A_Proc_Call with os argument 
; (and A_Proc_Call_Filter is not empty): 
(set! //A_/Proc_/Call_/Filter '())
(define (@PPC_A_Proc_Call_os? //I)
 (let ((//Found-save //Found)
       (funct-result '()))
  (set! //Found 0)
  (cond
   ((not (null? //A_/Proc_/Call_/Filter))
    (cond
     ((member //T_/A_/Proc_/Call (@Stat_Types //I))
      (@Edit)
      (@New_Program //I)
      (@Foreach_Statement /foreach-push_pop-1 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (@Undo_Edit)))))
  (set! funct-result (= //Found 1))
  (set! //Found //Found-save)
  funct-result))

#t
