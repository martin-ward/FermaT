;;; Scheme translation of WSL code
(define (/foreach-wsl2scheme-1 //Depth //A/S_/Type)
 (cond
  ((@Ifmatch_Type? (@ST (@I)))
   (@Trans //T/R_/Ifmatch_/Processing ""))))

(define (/foreach-wsl2scheme-2 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__wsl2scheme__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e_save /e))
     (set! /e (vector-ref /__/Match_array 0))
     (@Paste_Over (@Make 216 '() (list (@Make 9 (@Make_Name "concat") '()) (@Var_To_Expn /e))))
     (set! /e /__e_save))))))

(define (/foreach-wsl2scheme-3 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__wsl2scheme__2 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__stack_save /stack)
          (/__v_save /v))
     (set! /stack (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (@Splice_Over (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Make 240 '() (list (@Var_To_Expn /stack))))))) (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /stack) (@Make 241 '() (list (@Var_To_Expn /stack)))))))))
     (set! /stack /__stack_save)
     (set! /v /__v_save))))))

(define (/foreach-wsl2scheme-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Minus) (> (@Size (@I)) 2))
   (@Paste_Over (@Make //T_/Minus '() (list (@Get_n (@I) 1) (@Make //T_/Plus '() (cdr (@Cs (@I))))))))))

(define (/foreach-wsl2scheme-5 //Depth //A/S_/Type)
 (cond
  ((or (and (@Fill_Type? (@ST (@I))) (@WS_Constant_Fill? (@I))) (and #f (= (@ST (@I)) //T_/M/W_/Funct_/Call) (equal? (@V (@Get_n (@I) 1)) /make_name) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/String)))
   (set! /v '())
   (for-in /init /inits 
    (cond
     ((@Equal? (@Get_n /init 2) (@I))
      (set! /v (@Get_n /init 1)))))
   (cond
    ((null? /v)
     (set! /n (+ /n 1))
     (set! /v (@Make //T_/Var_/Lvalue (@Make_Name (concat /name (@String /n))) '()))
     (set! /inits (cons (@Make //T_/Assign '() (list /v (@I))) /inits))))
   (@Paste_Over (@Lvalue_To_Expn /v)))))

(define (/foreach-wsl2scheme-6 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__wsl2scheme__3 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e_save /e)
          (/__v_save /v))
     (set! /e (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /v) //T_/Variable) (member (@V /v) /arrays))
       (set! /e (@Simplify_Expn (@Make 221 '() (list (@Var_To_Expn /e) (@Make 205 1 '())))))
       (@Paste_Over (@Make //T_/X_/Funct_/Call '() (list /ref (@Make //T_/Expressions '() (list /v /e)))))))
     (set! /e /__e_save)
     (set! /v /__v_save))))))

(define (/foreach-wsl2scheme-7 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__wsl2scheme__4 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e1_save /e1)
          (/__e_save /e)
          (/__v_save /v))
     (set! /e1 (vector-ref /__/Match_array 2))
     (set! /e (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /v) //T_/Var_/Lvalue) (member (@V /v) /arrays))
       (set! /e (@Simplify_Expn (@Make 221 '() (list (@Var_To_Expn /e) (@Make 205 1 '())))))
       (set! /v (@Lvalue_To_Expn /v))
       (@Paste_Over (@Make //T_/X_/Proc_/Call '() (list /set (@Make //T_/Expressions '() (list /v /e /e1)))))))
     (set! /e1 /__e1_save)
     (set! /e /__e_save)
     (set! /v /__v_save))))))

(define (/foreach-wsl2scheme-8 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Concat) (= (@ST (@Get_n (@I) 1)) //T_/Sequence) (> (@Size (@I)) 1))
   (set! /args '())
   (set! /rest (@Cs (@I)))
   (while (and (not (null? /rest)) (= (@ST (car /rest)) //T_/Sequence)) 
    (begin
     (set! /args (concat (reverse (@Cs (@Get_n (car /rest) 1))) /args))
     (set! /rest (cdr /rest))))
   (cond
    ((= (gen-length /rest) 0)
     (@Paste_Over (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() (reverse /args))))))
    (#t
     (cond
      ((= (gen-length /rest) 1)
       (set! /rest (car /rest)))
      (#t
       (set! /rest (@Make //T_/Concat '() /rest))))
     ; cons args to front of rest: 
     (for-in /arg /args 
      (set! /rest (@Make_Funct /cons (list /arg /rest))))
     (@Paste_Over /rest))))
  ((and (= (@ST (@I)) //T_/Concat) (> (@Size (@I)) 1) (or (= (@ST (@Get_n (@I) 1)) //T_/String) (= (@ST (@Get_n (@I) 2)) //T_/String)))
   (@Paste_Over (@Make_Funct /string_append (@Cs (@I)))))))

(define (/foreach-wsl2scheme-9 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Concat)
   (cond
    ((= (@Size (@I)) 0)
     (@Paste_Over (@Make //T_/String "" '())))
    ((= (@Size (@I)) 1)
     (@Paste_Over (@Get_n (@I) 1)))
    (#t
     (while (and (= (@ST (@I)) //T_/Concat) (> (@Size (@I)) 2)) 
      (begin
       ; combine first two arguments: 
       (set! /arg1 (@Make //T_/Concat '() (list (@Get_n (@I) 1) (@Get_n (@I) 2))))
       (@Down)
       (@Delete)
       (@Paste_Over /arg1)
       (@Up))))))))

(define (/foreach-wsl2scheme-10 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/M/W_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) /yy_print))
   (@Delete))))

(define (/foreach-wsl2scheme-11 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Floop) (@Trans? //T/R_/Floop_/To_/While))
   ; Check for a single EXIT at the top level: 
   (set! /floop 1)
   (@Down)
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__wsl2scheme__5 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S2_save //S2)
            (/__/S_save //S)
            (/__/B_save //B)
            (/__/S1_save //S1))
       (set! //S2 (vector-ref /__/Match_array 3))
       (set! //S (vector-ref /__/Match_array 2))
       (set! //B (vector-ref /__/Match_array 1))
       (set! //S1 (vector-ref /__/Match_array 0))
       (@Up)
       (cond
        ((@Gen_Proper? (@Make //T_/Statements '() (concat (concat //S1 //S) //S2)) //A/S)
         (set! //B (@Not //B))
         (@Splice_Over (@Cs (@Make 17 '() (append //S1 (list (@Make 141 '() (list //B (@Make 17 '() (append //S2 //S1))))) //S)))))
        (#t
         (@Trans //T/R_/Floop_/To_/While "")))
       (set! //S2 /__/S2_save)
       (set! //S /__/S_save)
       (set! //B /__/B_save)
       (set! //S1 /__/S1_save)))
     (#t
      (@Up)
      (@Trans //T/R_/Floop_/To_/While "")))))))

(define (/foreach-wsl2scheme-12 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/In) (= (@ST (@Get_n (@I) 2)) //T_/Sequence))
   (set! /comps '())
   (for-in /elt (@Cs (@Get_n (@Get_n (@I) 2) 1)) 
    (set! /comps (cons (@Make //T_/Equal '() (list (@Get_n (@I) 1) /elt)) /comps)))
   (@Paste_Over (@Make //T_/Or '() (reverse /comps))))
  ((and (= (@ST (@I)) //T_/Not_/In) (= (@ST (@Get_n (@I) 2)) //T_/Sequence))
   (set! /comps '())
   (for-in /elt (@Cs (@Get_n (@Get_n (@I) 2) 1)) 
    (set! /comps (cons (@Make //T_/Not_/Equal '() (list (@Get_n (@I) 1) /elt)) /comps)))
   (@Paste_Over (@Make //T_/And '() (reverse /comps))))))

(define (/foreach-wsl2scheme-13 //Depth //A/S_/Type)
 (cond
  ((and (or (= (@ST (@I)) //T_/And) (= (@ST (@I)) //T_/Or)) (or (and (member //T_/Head (@Spec_Types (@I))) (member //T_/Empty (@Spec_Types (@I)))) (member //T_/Get_n (@Spec_Types (@I)))) (> (@Size (@I)) 1) (@Equal? (@I) (@Simplify_Cond (@I))))
   (@Paste_Over (@Make (@ST (@I)) '() (reverse (@Cs (@I)))))
   (cond
    ((and (not-member //T_/Head (@Spec_Types (@Get_n (@I) 2))) (member //T_/Head (@Spec_Types (@Get_n (@I) 1))))
     (@Paste_Over (@Make (@ST (@I)) '() (cons (@Get_n (@I) 2) (cons (@Get_n (@I) 1) (cdr (cdr (@Cs (@I)))))))))
    ((and (not-member //T_/Get_n (@Spec_Types (@Get_n (@I) 2))) (member //T_/Get_n (@Spec_Types (@Get_n (@I) 1))))
     (@Paste_Over (@Make (@ST (@I)) '() (cons (@Get_n (@I) 2) (cons (@Get_n (@I) 1) (cdr (cdr (@Cs (@I)))))))))))))

(define (/foreach-wsl2scheme-14 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Reduce)
   (@Down)
   (cond
    ((or (equal? (@V (@I)) /union1) (equal? (@V (@I)) /union2))
     (@Paste_Over (@Make //T_/Name /new_union '())))
    ((or (equal? (@V (@I)) /intersect1) (equal? (@V (@I)) /intersect2))
     (@Paste_Over (@Make //T_/Name /new_intersect '()))))
   (@Up))))

(define (/foreach-wsl2scheme-15 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/And) (= (@ST (@I)) //T_/Or))
   (@Down)
   (set! /index_vars (@Find_Index_Vars))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((not (null? (intersection-n (@Set_Difference (@Used (@I)) (@Find_Index_Vars)) /index_vars)))
       (@Trans //T/R_/Move_/To_/Left "")
       (@Right)))
     (set! /index_vars (@Find_Index_Vars)))))))

(define (/foreach-wsl2scheme-16 //Depth //A/S_/Type)
 (cond
  ((member (@ST (@I)) //Foreach_/Types)
   (set! //Globals (union-n //Globals (@Variables (@Get_n (@I) 1)) (@Match_Vars (@Get_n (@I) 1))))
   (set! /foreach (+ /foreach 1))
   (set! /name (@Make_Name (concat (string-append (string-append "foreach-" //Input) "-") (@String /foreach))))
   (set! /body_var (@Make //T_/Variable /name '()))
   (@WSSL (string-append (concat (string-append (concat (string-append (string-append "(define (" (@WS_Name /name)) " ") (@WS_Name (@Make_Name "Depth"))) " ") (@WS_Name (@Make_Name "AS_Type"))) ")"))
   (@WS_Items (@Cs (@Get_n (@I) 1)))
   (@WSSL ")")
   (@WSSL "")
   ; Replace the FOREACH by a suitable proc call: 
   (set! //S/T (@ST (@I)))
   (cond
    ((member //S/T //Only_/Simple_/Types)
     (@Paste_Over (@Make 102 '() (list (@Make 9 (@Make_Name "@Foreach") '()) (@Make 10 '() (list (@Var_To_Expn /body_var) (@Make 205 0 '()) (@Make 202 '() (list (@Make 9 (@Make_Name "@AS_Type") '()) (@Make 10 '() '()))) (@Make 205 1 '()))) (@Make 12 '() '())))))
    (#t
     (@Paste_Over (@Make 102 '() (list (@Make 9 (@Make_Name "@Foreach") '()) (@Make 10 '() (list (@Var_To_Expn /body_var) (@Make 205 0 '()) (@Make 202 '() (list (@Make 9 (@Make_Name "@AS_Type") '()) (@Make 10 '() '()))) (@Make 205 0 '()))) (@Make 12 '() '()))))))
   ; Get the actual proc name from WS_Funct: 
   (@Paste_Over (@Make //T_/M/W_/Proc_/Call '() (cons (@Name (@Make_Name (vector-ref //W/S_/Funct (- //S/T 1)))) (cdr (@Cs (@I))))))
   ; Check for deletion of the whole program: 
   (@Paste_After /%const__wsl2scheme__6))))

(define (/foreach-wsl2scheme-17 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Aref)
   (set! /vars (union-n /vars (@Used (@Get_n (@I) 2)))))
  ((or (= (@ST (@I)) //T_/Sub_/Seg) (= (@ST (@I)) //T_/Rel_/Seg))
   (set! /vars (union-n /vars (@Used (@Get_n (@I) 2)) (@Used (@Get_n (@I) 3)))))))

(define /%const__wsl2scheme__1 (@Make 216 '() (list (@Make 9 (@Make_Name "++") '()) (@Make 217 -1 '()))))
(define /%const__wsl2scheme__2 (@Make 135 '() (list (@Make 506 -1 '()) (@Make 506 -2 '()))))
(define /%const__wsl2scheme__3 (@Make 210 '() (list (@Make 217 -1 '()) (@Make 10 '() (list (@Make 217 -2 '()))))))
(define /%const__wsl2scheme__4 (@Make 110 '() (list (@Make 6 '() (list (@Make 502 '() (list (@Make 506 -1 '()) (@Make 10 '() (list (@Make 217 -2 '()))))) (@Make 217 -3 '()))))))
(define /%const__wsl2scheme__5 (@Make 17 '() (list (@Make 107 -1 '()) (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -2 '()) (@Make 17 '() (list (@Make 107 -3 '()) (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))) (@Make 107 -4 '()))))
(define /%const__wsl2scheme__6 (@Make 114 '() (list (@Make 7 '() (list (@Make 321 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@Program") '()) (@Make 10 '() '()))))) (@Make 17 '() (list (@Make 102 '() (list (@Make 9 (@Make_Name "@New_Program") '()) (@Make 10 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@Skips") '()) (@Make 10 '() '()))))) (@Make 12 '() '()))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
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
; WSL to Scheme Translator 
; Keep track of the indentation, start a newline after each statement. 
; NOTE: Scheme itself is NOT case-dependent while WSL is case-dependent 
; So we need to rename the WSL variable names to distinguish cases 
; (and avoid clashes with vars generated by the translator) 
(set! //W/S/L_/Option_/Quiet 0)
; Current nesting level (1 = top level): 
(set! //W/S_/Nesting 1)
; Indentation for each level of nesting: 
(set! //W/S_/Indents (make-vector-eval 1999 0))
; Current position of cursor (number of chars on line): 
(set! //W/S_/X_/Pos 0)
; A flag to record whether the indent for this line has been printed yet: 
(set! //W/S_/Indent_/Pending 0)
(set! //Spaces "                                                             ")
(set! /ws_macro_pars '())
; Write a string, keep a record of the position of each open bracket 
; and the level of bracket nesting. 
; Update the indents _before_ printing the indent for this line: 
(define (@WSS /str)
 (let ((/i 0)
       (/closes 0)
       (/quote 0)
       (/comment 0))
  ; If there are pending indents, adjust the indent according to the number 
  ; of initial )'s, then print the indent and continue: 
  (cond
   ((= //W/S_/Indent_/Pending 1)
    (while (and (< /i (string-length /str)) (equal? (substr /str /i 1) ")")) 
     (begin
      (set! /closes (+ /closes 1))
      (set! /i (+ /i 1))))
    ; Calculate the actual indent and print it: 
    (cond
     ((< (- //W/S_/Nesting /closes) 1)
      (error "@WSS" "Too many closing brackets" /str)))
    (set! //W/S_/X_/Pos (modulo (vector-ref //W/S_/Indents (- //W/S_/Nesting (+ 1 /closes))) 40))
    (@Write (substr //Spaces 0 //W/S_/X_/Pos) //Output_/Port)
    (set! //W/S_/Indent_/Pending 0)))
  (for /i 0 (- (string-length /str) 1) 1 
   (begin
    (cond
     ((equal? (substr /str /i 1) //Quote)
      (set! /quote (- 1 /quote)))
     ((equal? (substr /str /i 1) ";")
      (set! /comment 1)))
    (cond
     ((and (= /comment 0) (= /quote 0))
      (cond
       ((equal? (substr /str /i 1) "(")
        (set! //W/S_/Nesting (+ //W/S_/Nesting 1))
        (vector-set! //W/S_/Indents (- //W/S_/Nesting 1) (+ //W/S_/X_/Pos /i 1)))
       ((equal? (substr /str /i 1) ")")
        (cond
         ((<= //W/S_/Nesting 1)
          (error "@WSS" "Too many closing brackets" /str)))
        (set! //W/S_/Nesting (- //W/S_/Nesting 1))))))))
  (@Write /str //Output_/Port)
  (set! //W/S_/X_/Pos (+ //W/S_/X_/Pos (string-length /str)))))

; Start a newline and indent to the appropriate level: 
(define (@WSSL /str)
 (@WSS /str)
 (@Write_Line "" //Output_/Port)
 (set! //W/S_/Indent_/Pending 1))

; Initialisation section: 
(set! /fl_flag1 0)
(set! /fl_flag2 0)
(set! /fl_flag3 0)
(set! /fl_flag4 0)
(set! /fl_flag5 0)
(set! /fl_flag6 0)
(set! /fl_flag7 0)
(set! /fl_flag8 0)
(set! /fl_flag9 0)
(set! /fl_flag/X 0)
(let ((/type-save /type))
 (set! /type 0)
 ; Types which can be translated as (funct par1 par2 ...), the default type 
 (set! //Scheme_/Funct 1)
 ; Types which can be translated as funct++value 
 (set! //Scheme_/Literal 2)
 ; Proc or Funct call types, may have value and VAR parameters: 
 (set! //Scheme_/Call 3)
 ; Proc or Funct declaration: 
 (set! //Scheme_/Decl 4)
 ; Some kind of IFMATCH: 
 (set! //Scheme_/Ifmatch 5)
 ; Some kind of FILL: 
 (set! //Scheme_/Fill 6)
 ; Special cases 
 (set! //Scheme_/Special 99)
 (set! //W/S_/Type (make-vector-eval 1999 //Scheme_/Funct))
 (set! //W/S_/Funct (make-vector-eval 1999 ""))
 ; Data for each type: 
 (vector-set! //W/S_/Type (- //T_/Statements 1) //Scheme_/Special)
 (vector-set! //W/S_/Funct (- //T_/Expressions 1) "")
 (vector-set! //W/S_/Funct (- //T_/Lvalues 1) "")
 (vector-set! //W/S_/Funct (- //T_/Assigns 1) "")
 (vector-set! //W/S_/Funct (- //T_/Definitions 1) "")
 (vector-set! //W/S_/Funct (- //T_/Assign 1) "")
 (vector-set! //W/S_/Funct (- //T_/Guarded 1) "")
 (vector-set! //W/S_/Funct (- //T_/Abort 1) "abort")
 (vector-set! //W/S_/Type (- //T_/Skip 1) //Scheme_/Literal)
 (vector-set! //W/S_/Type (- //T_/True 1) //Scheme_/Literal)
 (vector-set! //W/S_/Type (- //T_/False 1) //Scheme_/Literal)
 (vector-set! //W/S_/Funct (- //T_/Skip 1) "#t")
 (vector-set! //W/S_/Funct (- //T_/True 1) "#t")
 (vector-set! //W/S_/Funct (- //T_/False 1) "#f")
 ; TODO: make-hash-table requires a size parameter for the array. 
 ; ??? Write our own hash code to double the size of a table 
 ; when it gets `too full' ??? 
 (vector-set! //W/S_/Funct (- //T_/Hash_/Table 1) "make-hash-table")
 (vector-set! //W/S_/Funct (- //T_/Slength 1) "string-length")
 (vector-set! //W/S_/Funct (- //T_/Substr 1) "substr")
 (vector-set! //W/S_/Funct (- //T_/Index 1) "my-index")
 (for-in /type (list //T_/Expn_/Place //T_/Var_/Place //T_/Cond_/Place //T_/Stat_/Place) 
  (vector-set! //W/S_/Type (- /type 1) //Scheme_/Literal))
 (vector-set! //W/S_/Funct (- //T_/Expn_/Place 1) "$Expn$")
 (vector-set! //W/S_/Funct (- //T_/Var_/Place 1) "$Var$")
 (vector-set! //W/S_/Funct (- //T_/Cond_/Place 1) "$Condition$")
 (vector-set! //W/S_/Funct (- //T_/Stat_/Place 1) "$Statement$")
 (for-in /type (list //T_/Stat_/Pat_/One //T_/Expn_/Pat_/One //T_/Cond_/Pat_/One //T_/Defn_/Pat_/One //T_/Lvalue_/Pat_/One //T_/Assign_/Pat_/One //T_/Guarded_/Pat_/One //T_/Action_/Pat_/One //T_/Name_/Pat_/One) 
  (begin
   (vector-set! //W/S_/Type (- /type 1) //Scheme_/Literal)
   (vector-set! //W/S_/Funct (- /type 1) "~?")))
 (for-in /type (list //T_/Stat_/Pat_/Many //T_/Expn_/Pat_/Many //T_/Cond_/Pat_/Many //T_/Defn_/Pat_/Many //T_/Lvalue_/Pat_/Many //T_/Assign_/Pat_/Many //T_/Guarded_/Pat_/Many //T_/Action_/Pat_/Many) 
  (begin
   (vector-set! //W/S_/Type (- /type 1) //Scheme_/Literal)
   (vector-set! //W/S_/Funct (- /type 1) "~+")))
 (for-in /type (list //T_/Stat_/Pat_/Any //T_/Expn_/Pat_/Any //T_/Cond_/Pat_/Any //T_/Defn_/Pat_/Any //T_/Lvalue_/Pat_/Any //T_/Assign_/Pat_/Any //T_/Guarded_/Pat_/Any //T_/Action_/Pat_/Any) 
  (begin
   (vector-set! //W/S_/Type (- /type 1) //Scheme_/Literal)
   (vector-set! //W/S_/Funct (- /type 1) "~*")))
 (for-in /type (list //T_/Proc_/Call //T_/A_/Proc_/Call //T_/M/W_/Proc_/Call //T_/X_/Proc_/Call //T_/X_/Funct_/Call //T_/M/W_/Funct_/Call //T_/Funct_/Call //T_/X_/B/Funct_/Call //T_/M/W_/B/Funct_/Call //T_/B/Funct_/Call) 
  (vector-set! //W/S_/Type (- /type 1) //Scheme_/Call))
 (vector-set! //W/S_/Funct (- //T_/Print 1) "display-list")
 (vector-set! //W/S_/Funct (- //T_/Prinflush 1) "display-list-flush")
 (vector-set! //W/S_/Funct (- //T_/Maphash 1) "maphash ")
 (vector-set! //W/S_/Funct (- //T_/Error 1) "error")
 (vector-set! //W/S_/Funct (- //T_/Assert 1) "assertion")
 (vector-set! //W/S_/Type (- //T_/Assignment 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/A_/S 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/Call 1) //Scheme_/Special)
 (vector-set! //W/S_/Funct (- //T_/Action 1) "")
 (vector-set! //W/S_/Type (- //T_/Comment 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/Cond 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/D_/If 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/D_/Do 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/Exit 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/For 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/For_/In 1) //Scheme_/Special)
 (vector-set! //W/S_/Funct (- //T_/Foreach_/Stat 1) "@Foreach_Statement")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/N/A/S 1) "@Foreach_Non_Action_Statement")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/Stats 1) "@Foreach_Stats")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/T/S 1) "@Foreach_Terminal")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/T/Ss 1) "@Foreach_Terminal_Stats")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/S/T/S 1) "@Foreach_Terminal")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/Cond 1) "@Foreach_Cond")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/Expn 1) "@Foreach_Expn")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/Lvalue 1) "@Foreach_Lvalue")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/Variable 1) "@Foreach_Variable")
 (vector-set! //W/S_/Funct (- //T_/Foreach_/Global_/Var 1) "@Foreach_Global_Var")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/Stat 1) "@Ateach_Statement")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/N/A/S 1) "@Ateach_Non_Action_Statement")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/Stats 1) "@Ateach_Stats")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/T/S 1) "@Ateach_Terminal")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/T/Ss 1) "@Ateach_Terminal_Stats")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/S/T/S 1) "@Ateach_Terminal")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/Cond 1) "@Ateach_Cond")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/Expn 1) "@Ateach_Expn")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/Lvalue 1) "@Ateach_Lvalue")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/Variable 1) "@Ateach_Variable")
 (vector-set! //W/S_/Funct (- //T_/Ateach_/Global_/Var 1) "@Ateach_Global_Var")
 ; Note: we need to keep track of the current depth for Floop and Exit: 
 (vector-set! //W/S_/Type (- //T_/Floop 1) //Scheme_/Special)
 (vector-set! //W/S_/Funct (- //T_/Join 1) "join")
 (vector-set! //W/S_/Funct (- //T_/Pop 1) "pop")
 (vector-set! //W/S_/Funct (- //T_/Puthash 1) "puthash")
 (vector-set! //W/S_/Funct (- //T_/Push 1) "push")
 (vector-set! //W/S_/Funct (- //T_/Spec 1) "spec-stat")
 ; Save the global values and overwrite with the assignments: 
 ; (This is needed to get dynamic binding to work) 
 (vector-set! //W/S_/Type (- //T_/Var 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/Where 1) //Scheme_/Special)
 (vector-set! //W/S_/Funct (- //T_/While 1) "while")
 (for-in /type (list //T_/Proc //T_/Funct //T_/B/Funct //T_/M/W_/Proc //T_/M/W_/Funct //T_/M/W_/B/Funct) 
  (vector-set! //W/S_/Type (- /type 1) //Scheme_/Decl))
 (for-in /type (list //T_/Ifmatch_/Stat //T_/Ifmatch_/Expn //T_/Ifmatch_/Cond //T_/Ifmatch_/Defn //T_/Ifmatch_/Lvalue //T_/Ifmatch_/Assign //T_/Ifmatch_/Guarded //T_/Ifmatch_/Action //T_/Ifmatch_/Stats //T_/Ifmatch_/Expns //T_/Ifmatch_/Lvalues //T_/Ifmatch_/Assigns //T_/Ifmatch_/Defns) 
  (vector-set! //W/S_/Type (- /type 1) //Scheme_/Ifmatch))
 ; Generic type: Expression 
 (vector-set! //W/S_/Funct (- //T_/Get_n 1) "@Get_n")
 (vector-set! //W/S_/Funct (- //T_/Get 1) "@Get")
 (vector-set! //W/S_/Funct (- //T_/Gethash 1) "gethash")
 (vector-set! //W/S_/Funct (- //T_/Hash_/Table 1) "hash-table")
 (for-in /type (list //T_/Number //T_/Variable) 
  (vector-set! //W/S_/Type (- /type 1) //Scheme_/Literal))
 (vector-set! //W/S_/Type (- //T_/String 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/Sequence 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/Aref 1) //Scheme_/Special)
 (vector-set! //W/S_/Funct (- //T_/Sub_/Seg 1) "@Sub_Seg")
 (vector-set! //W/S_/Funct (- //T_/Rel_/Seg 1) "@Rel_Seg")
 (vector-set! //W/S_/Funct (- //T_/Final_/Seg 1) "@Final_Seg")
 (vector-set! //W/S_/Funct (- //T_/Map 1) "my-map ")
 (vector-set! //W/S_/Funct (- //T_/Reduce 1) "my-reduce ")
 (vector-set! //W/S_/Funct (- //T_/Plus 1) "+")
 (vector-set! //W/S_/Funct (- //T_/Minus 1) "-")
 (vector-set! //W/S_/Funct (- //T_/Times 1) "*")
 (vector-set! //W/S_/Funct (- //T_/Divide 1) "/")
 (vector-set! //W/S_/Funct (- //T_/Exponent 1) "integer-expt")
 (vector-set! //W/S_/Funct (- //T_/Mod 1) "modulo")
 (vector-set! //W/S_/Funct (- //T_/Div 1) "quotient")
 (vector-set! //W/S_/Funct (- //T_/If 1) "if")
 (vector-set! //W/S_/Funct (- //T_/Abs 1) "abs")
 (vector-set! //W/S_/Funct (- //T_/Frac 1) "frac")
 (vector-set! //W/S_/Funct (- //T_/Int 1) "int")
 (vector-set! //W/S_/Funct (- //T_/Sgn 1) "sgn")
 (vector-set! //W/S_/Funct (- //T_/Max 1) "max")
 (vector-set! //W/S_/Funct (- //T_/Min 1) "min")
 ; NOTE: slib has an implementation of `balanced binary trees' 
 ; which includes efficient set operations. 
 (vector-set! //W/S_/Funct (- //T_/Intersection 1) "intersection-n")
 (vector-set! //W/S_/Funct (- //T_/Union 1) "union-n")
 (vector-set! //W/S_/Funct (- //T_/Set_/Diff 1) "@Set_Difference")
 (vector-set! //W/S_/Funct (- //T_/Powerset 1) "@Powerset")
 (vector-set! //W/S_/Funct (- //T_/Set 1) "set-construction")
 (vector-set! //W/S_/Funct (- //T_/Array 1) "make-vector-eval")
 ; was: make-vector-eval 
 (vector-set! //W/S_/Funct (- //T_/Head 1) "car")
 (vector-set! //W/S_/Funct (- //T_/Tail 1) "cdr")
 (vector-set! //W/S_/Funct (- //T_/Last 1) "last-1")
 (vector-set! //W/S_/Funct (- //T_/Butlast 1) "butlast-1")
 (vector-set! //W/S_/Funct (- //T_/Length 1) "gen-length")
 (vector-set! //W/S_/Funct (- //T_/Reverse 1) "reverse")
 (vector-set! //W/S_/Funct (- //T_/Concat 1) "concat")
 ; Dummy functions for unimplemented features: 
 (vector-set! //W/S_/Funct (- //T_/Address_/Of 1) "address-of")
 (vector-set! //W/S_/Funct (- //T_/Mem 1) "memory-access")
 (vector-set! //W/S_/Funct (- //T_/Mem_/Lvalue 1) "memory-access-lvalue")
 (vector-set! //W/S_/Funct (- //T_/Mem_/Seg 1) "memory-access-seg")
 (vector-set! //W/S_/Funct (- //T_/Mem_/Seg_/Lvalue 1) "memory-access-seg-lvalue")
 (vector-set! //W/S_/Funct (- //T_/Mem_/Rel 1) "memory-access-rel")
 (vector-set! //W/S_/Funct (- //T_/Mem_/Rel_/Lvalue 1) "memory-access-rel-lvalue")
 (vector-set! //W/S_/Funct (- //T_/Negate 1) "-")
 (vector-set! //W/S_/Funct (- //T_/Invert 1) "/")
 (vector-set! //W/S_/Type (- //T_/Struct 1) //Scheme_/Special)
 (for-in /type (list //T_/Fill_/Stat //T_/Fill_/Expn //T_/Fill_/Cond //T_/Fill_/Defn //T_/Fill_/Stats //T_/Fill_/Expns //T_/Fill_/Defns //T_/Fill_/Lvalue //T_/Fill_/Lvalues //T_/Fill_/Assign //T_/Fill_/Guarded //T_/Fill_/Action //T_/Fill_/Assigns //T_/Fill_/Defns) 
  (vector-set! //W/S_/Type (- /type 1) //Scheme_/Fill))
 ; Generic Type: Condition 
 (vector-set! //W/S_/Funct (- //T_/And 1) "and")
 (vector-set! //W/S_/Funct (- //T_/Or 1) "or")
 (vector-set! //W/S_/Funct (- //T_/Not 1) "not")
 ; Use eq? for atoms, = for numbers and equal? for anything else (or unknown) 
 (vector-set! //W/S_/Type (- //T_/Equal 1) //Scheme_/Special)
 (vector-set! //W/S_/Type (- //T_/Not_/Equal 1) //Scheme_/Special)
 (vector-set! //W/S_/Funct (- //T_/Less 1) "<")
 (vector-set! //W/S_/Funct (- //T_/Greater 1) ">")
 (vector-set! //W/S_/Funct (- //T_/Less_/Eq 1) "<=")
 (vector-set! //W/S_/Funct (- //T_/Greater_/Eq 1) ">=")
 (vector-set! //W/S_/Funct (- //T_/Even 1) "even?")
 (vector-set! //W/S_/Funct (- //T_/Odd 1) "odd?")
 (vector-set! //W/S_/Funct (- //T_/Empty 1) "null?")
 (vector-set! //W/S_/Funct (- //T_/Subset 1) "@Set_Subset?")
 (vector-set! //W/S_/Funct (- //T_/Member 1) "member")
 (vector-set! //W/S_/Funct (- //T_/In 1) "member")
 (vector-set! //W/S_/Funct (- //T_/Not_/In 1) "not-member")
 (vector-set! //W/S_/Funct (- //T_/Forall 1) "forall")
 (vector-set! //W/S_/Funct (- //T_/Exists 1) "exists")
 (vector-set! //W/S_/Funct (- //T_/Implies 1) "implies")
 (vector-set! //W/S_/Funct (- //T_/Sequenceq 1) "sequence?")
 (vector-set! //W/S_/Funct (- //T_/Numberq 1) "number?")
 (vector-set! //W/S_/Funct (- //T_/Stringq 1) "string?")
 ; Generic Type: Definition (see above) -- Scheme_Decl 
 ; Generic_Type: Lvalue 
 (vector-set! //W/S_/Type (- //T_/Var_/Lvalue 1) //Scheme_/Literal)
 (vector-set! //W/S_/Funct (- //T_/Var_/Lvalue 1) "")
 ; Arefs, Sub_Segs, Rel_Segs and Final_Segs should all have been 
 ; translated higher up in the WSL tree (in the translation of 
 ; the assignment or whatever), so these function should not be called: 
 (vector-set! //W/S_/Funct (- //T_/Aref_/Lvalue 1) "@Aref_Lvalue")
 (vector-set! //W/S_/Funct (- //T_/Sub_/Seg_/Lvalue 1) "@Sub_Seg_Lvalue")
 (vector-set! //W/S_/Funct (- //T_/Rel_/Seg_/Lvalue 1) "@Rel_Seg_Lvalue")
 (vector-set! //W/S_/Funct (- //T_/Final_/Seg_/Lvalue 1) "@Final_Seg_Lvalue")
 (vector-set! //W/S_/Type (- //T_/Struct_/Lvalue 1) //Scheme_/Special)
 ; End of WS_Init 
 
 (set! /type /type-save))
(define (@WSL_To_Scheme //I //Filename //Input-par)
 (let ((//Input-save //Input))
  (set! //Input //Input-par)
  (let ((//Globals-save //Globals)
        (/floop_depth-save /floop_depth)
        (/z_name-save /z_name)
        (/m_one-save /m_one)
        (/make_n-save /make_n)
        (/make_name-save /make_name)
        (/tmp_result-save /tmp_result)
        (/tmp_var-save /tmp_var)
        (/funct_result-save /funct_result)
        (//Call_/Types-save //Call_/Types)
        (//Macros-save //Macros))
   (set! //Globals '())
   (set! /floop_depth 0)
   (set! /z_name (@Make_Name "Z"))
   (set! /m_one (@Make //T_/Number (- 1) '()))
   (set! /make_n (@Make //T_/Name (@Make_Name "@Make") '()))
   (set! /make_name (@Make_Name "@Make_Name"))
   (set! /tmp_result (@Make_Name "-result-"))
   (set! /tmp_var "tmp-var")
   (set! /funct_result "funct-result")
   (set! //Call_/Types (list //T_/Proc_/Call //T_/Funct_/Call //T_/B/Funct_/Call //T_/M/W_/Proc_/Call //T_/M/W_/Funct_/Call //T_/M/W_/B/Funct_/Call //T_/X_/Proc_/Call //T_/X_/Funct_/Call //T_/X_/B/Funct_/Call))
   (set! //Macros (my-map @Make_Name (list "@Size" "@Get_n" "@Spec_Type" "@ST" "@Gen_Type" "@GT" "@Value" "@V" "@Components" "@Cs" "@Components?" "@Cs?" "@Program" "@Item" "@I" "@Parent" "@GParent" "@Posn" "@Posn_n" "@Data" "@Buffer" "@Make" "@Right?" "@Right" "@Left?" "@Left" "@Up?" "@Up" "@Down?" "@Down" "@Down_Last" "@To" "@To_Last" "@Down_To" "@Cut" "@New_Program" "@Dtable_Get" "@Dtable_Value_Part" "@Dtable_Put" "@String" "@Make_Name")))
   ; Macros are currently broken: 
   (set! //Macros '())
   ; Initial Simplification Transformations: 
   (@Edit)
   (@New_Program //I)
   ; TODO: Simplify `simple' IFMATCH constructs to a nested IF which 
   ; sets a local Match_Result variable and the pattern variables 
   (@Foreach_Statement /foreach-wsl2scheme-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Expn /foreach-wsl2scheme-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; pop macro does not handle complex variables eg arrays 
   ; So convert POP to the equivalent assignment statements: 
   (@Foreach_Statement /foreach-wsl2scheme-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; (- a b ...) is only optional in R5RS, scheme48 only has two argument minus: 
   (@Foreach_Expn /foreach-wsl2scheme-4 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; Replace constant FILL constructs by global variables. 
   ; This should be done _after_ Ifmatch_Processing! 
   ; Ditto for @Make_Name calls with a string parameter by global variables. 
   (let ((/inits-save /inits)
         (/name-save /name)
         (/n-save /n)
         (/v-save /v)
         (/make_name-save /make_name))
    (set! /inits '())
    (set! /name (string-append (string-append "%const__" //Input) "__"))
    (set! /n 0)
    (set! /v '())
    (set! /make_name (@Make_Name "@Make_Name"))
    (@Foreach_Expn /foreach-wsl2scheme-5 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (cond
     ((not (null? /inits))
      (@Down)
      (@Paste_Before (@Make //T_/Assignment '() (reverse /inits)))
      (@Up)))
    (set! /inits /inits-save)
    (set! /name /name-save)
    (set! /n /n-save)
    (set! /v /v-save)
    (set! /make_name /make_name-save))
   ;  Check for array references and assignments on known array variables 
   ; (and other global vars which are known to be arrays) 
   ; and convert to !XF vector-ref(name, index - 1) and 
   ; !XP vector-set! (name, index - 1, value) as appropriate 
   (let ((/arrays-save /arrays)
         (/ref-save /ref)
         (/set-save /set))
    (set! /arrays (my-map @Make_Name (list "Eval_Op" "Has_Statements" "Identity_Value" "Inverse_Op" "Mth_Ord" "N_String_To_Symbol" "N_Symbol_Table" "N_Symbol_Table_Length" "PP_Closing" "PP_Indent" "PP_Opening" "PP_Operator" "PP_Operator1" "PP_Operator2" "PP_Prec" "PP_Split" "PP_Sub_Indent" "PP_Type" "Power_Op" "Reverse_Op" "Spec_To_Gen_Type" "Syntax_Comps" "Syntax_E_To_V" "Syntax_Name" "Syntax_Type" "Syntax_V_To_E" "Syntax_Value" "TRs_Code" "TRs_Data_Gen_Type" "TRs_Help" "TRs_Keywords" "TRs_Name" "TRs_Proc_Name" "TRs_Prompt" "TRs_Test" "WS_Funct" "WS_Indents" "WS_Type" "Zero_Value" "__Match_array" "orig_Bodies" "orig_Entries")))
    (set! /ref (@Name (@Make_Name "vector-ref")))
    (set! /set (@Name (@Make_Name "vector-set!")))
    (@Foreach_Expn /foreach-wsl2scheme-6 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Foreach_Statement /foreach-wsl2scheme-7 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! /arrays /arrays-save)
    (set! /ref /ref-save)
    (set! /set /set-save))
   ; Replace concat by either cons or string-append: 
   (let ((/args-save /args)
         (/arg-save /arg)
         (/rest-save /rest)
         (/new '())
         (/cons-save /cons)
         (/string_append-save /string_append))
    (set! /args '())
    (set! /arg '())
    (set! /rest '())
    (set! /cons (@Make_Name "cons"))
    (set! /string_append (@Make_Name "string-append"))
    ; Flatten nested concats (from the inside out): 
    ; Check for prepending sequences: 
    (@Foreach_Expn /foreach-wsl2scheme-8 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! /args /args-save)
    (set! /arg /arg-save)
    (set! /rest /rest-save)
    (set! /cons /cons-save)
    (set! /string_append /string_append-save))
   ; Ensure remaining concats have two arguments each: 
   (let ((/arg1-save /arg1))
    (set! /arg1 '())
    (@Foreach_Expn /foreach-wsl2scheme-9 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! /arg1 /arg1-save))
   ; Delete calls to @yy_PRINT -- these are for debugging only 
   (let ((/yy_print-save /yy_print))
    (set! /yy_print (@Make_Name "@yy_PRINT"))
    (@Foreach_Statement /foreach-wsl2scheme-10 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! /yy_print /yy_print-save))
   ; Eliminate Floops where possible. 
   ; Note that we call TR_Floop_To_While without testing it: 
   ; This may result in copying code to eliminate the Floop. 
   (let ((//A/S-save //A/S)
         (/floop-save /floop)
         (/var-save /var))
    (set! //A/S (@AS_Type))
    (set! /floop 0)
    (set! /var '())
    (@Foreach_Statement /foreach-wsl2scheme-11 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! //A/S //A/S-save)
    (set! /floop /floop-save)
    (set! /var /var-save))
   ; Try to determine types of variables and simplify array refs etc. 
   ; Anything compared against a S_, TR_xxx, T_xxx, @ST, @GT, @Size 
   ; or any arithmetic expression is numeric. 
   ; A variable initialised by foo := ARRAY(...) is an array. 
   (let ((//Num_/Var_/Types-save //Num_/Var_/Types)
         (//Num_/Funct_/Types-save //Num_/Funct_/Types)
         (//Num_/Types-save //Num_/Types))
    (set! //Num_/Var_/Types (hash-table))
    (set! //Num_/Funct_/Types (hash-table))
    (set! //Num_/Types (hash-table))
    (cond
     ((or (equal? //Input "cond_parser") (equal? //Input "exp_parser") (equal? //Input "lexer") (equal? //Input "parser"))
      (puthash //Num_/Var_/Types (@Make_Name "token1") 1)))
    (puthash //Num_/Var_/Types (@Make_Name "ST") 1)
    (puthash //Num_/Var_/Types (@Make_Name "GT") 1)
    (for-in /var (@Variables (@I)) 
     (cond
      ((or (@Starts_With? /var "T_") (@Starts_With? /var "TR_"))
       (puthash //Num_/Var_/Types /var 1))))
    (for-in /name (my-map @Make_Name (list "@ST" "@GT" "@Size" "@Posn_n")) 
     (puthash //Num_/Funct_/Types /name 1))
    (for-in /type (list //T_/Plus //T_/Minus //T_/Times //T_/Divide //T_/Exponent //T_/Max //T_/Min //T_/Number //T_/Length //T_/Slength) 
     (puthash //Num_/Types /type 1))
    ; Replace foo IN <x1, ...> by foo = x1 OR ... 
    (let ((/comps-save /comps))
     (set! /comps '())
     (@Foreach_Cond /foreach-wsl2scheme-12 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! /comps /comps-save))
    ; Hack for tests of the form: NOT (EMPTY?(HEAD(x))) AND NOT EMPTY?(x) 
    ; Only fiddle with the condition if it may have been simplified by FermaT 
    (@Foreach_Cond /foreach-wsl2scheme-13 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    ; Fix REDUCE on / and / functions: 
    (let ((/union1-save /union1)
          (/union2-save /union2)
          (/new_union-save /new_union)
          (/intersect1-save /intersect1)
          (/intersect2-save /intersect2)
          (/new_intersect-save /new_intersect))
     (set! /union1 (@Make_Name (string-append //Backslash "/")))
     (set! /union2 (@Make_Name (string-append (concat //Backslash //Backslash) "/")))
     (set! /new_union (@Make_Name "@Set_Union"))
     (set! /intersect1 (@Make_Name (string-append "/" //Backslash)))
     (set! /intersect2 (@Make_Name (concat (string-append "/" //Backslash) //Backslash)))
     (set! /new_intersect (@Make_Name "@Set_Intersect"))
     (@Foreach_Expn /foreach-wsl2scheme-14 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! /union1 /union1-save)
     (set! /union2 /union2-save)
     (set! /new_union /new_union-save)
     (set! /intersect1 /intersect1-save)
     (set! /intersect2 /intersect2-save)
     (set! /new_intersect /new_intersect-save))
    ; Sort tests to put off memory access tests in case of segfaults: 
    (let ((/index_vars-save /index_vars))
     (set! /index_vars '())
     (@Foreach_Cond /foreach-wsl2scheme-15 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! /index_vars /index_vars-save))
    ; End of Simplification -- modified I is in @Program 
    ; Put macros into a separate file (so that they can be loaded first) 
    ; We assume that there are no FOREACH/ATEACH calls in a macro body! 
    (let ((//Macro_/Filename "")
          (/stat '())
          (/macro_decls '())
          (/new '())
          (//Output_/Port-save //Output_/Port))
     (set! //Output_/Port '())
     (cond
      ((equal? //Filename "")
       (set! //Macro_/Filename ""))
      ((equal? (substr //Filename (- (string-length //Filename) 4) 4) ".scm")
       (set! //Macro_/Filename (string-append (substr //Filename 0 (- (string-length //Filename) 4)) "-mac.scm")))
      (#t
       (set! //Macro_/Filename (string-append //Filename "-mac.scm"))))
     ; MW_PROC/FUNCT declarations are all on the top level: 
     (for-in /stat (@Cs (@Program)) 
      (cond
       ((and (equal? (vector-ref //W/S_/Type (- (@ST /stat) 1)) //Scheme_/Decl) (member (@V (@Get_n /stat 1)) //Macros))
        (set! /macro_decls (cons /stat /macro_decls))
        (set! /new (cons (@Make //T_/Comment (string-append " Macro declaration: " (@N_String (@V (@Get_n /stat 1)))) '()) /new)))
       (#t
        (set! /new (cons /stat /new)))))
     ; If there are macros, write a macro file, otherwise delete it: 
     (cond
      ((equal? //Macro_/Filename "")
       (set! //Output_/Port //Standard_/Output_/Port))
      (#t
       (set! //Output_/Port (@Open_Output_File //Macro_/Filename))))
     (cond
      ((not (null? /macro_decls))
       (cond
        ((= //W/S/L_/Option_/Quiet 0)
         (display-list (string-append "Writing: " //Macro_/Filename))))
       (for-in /stat (reverse /macro_decls) 
        (begin
         (@WS_Item /stat)
         (@WSSL "")))
       (@Paste_Over (@Make //T_/Statements '() (reverse /new)))))
     (cond
      ((not (equal? //Macro_/Filename ""))
       (@Close_Output_Port //Output_/Port)))
     (set! //Output_/Port //Output_/Port-save))
    ; Now write the main file: 
    (cond
     ((equal? //Filename "")
      (set! //Output_/Port //Standard_/Output_/Port))
     (#t
      (set! //Output_/Port (@Open_Output_File //Filename))
      (cond
       ((= //W/S/L_/Option_/Quiet 0)
        (display-list "Writing: " //Filename)))))
    (@WSSL ";;; Scheme translation of WSL code")
    ; Compute the list of global vars, ignoring parameters of MW_PROCs and functs 
    ; (unless they are used globally elsewhere) 
    (let ((/stat '()))
     (for-in /stat (@Cs (@Program)) 
      (cond
       ((= (@ST /stat) //T_/M/W_/Proc)
        (set! //Globals (union-n //Globals (@Set_Difference (@Variables /stat) (@Variables (@Get_n /stat 2))))))
       ((or (= (@ST /stat) //T_/M/W_/Funct) (= (@ST /stat) //T_/M/W_/B/Funct))
        (set! //Globals (union-n //Globals (@Set_Difference (@Set_Difference (@Variables /stat) (@Variables (@Get_n /stat 2))) (@Assigned (@Get_n /stat 3))))))
       (#t
        (set! //Globals (union-n //Globals (@Variables /stat)))))))
    ; Process FOREACH and ATEACH clauses to generate top-level declarations 
    ; for the bodies: 
    ; NB: Add global vars in these clauses to the Globals list: 
    ; NB: Patterns (for MATCH/FILL) are also global variables! 
    (let ((/foreach-save /foreach)
          (/name-save /name)
          (/body_var-save /body_var)
          (//S/T-save //S/T)
          (//Foreach_/Types-save //Foreach_/Types)
          (//Only_/Simple_/Types-save //Only_/Simple_/Types))
     (set! /foreach 0)
     (set! /name "")
     (set! /body_var '())
     (set! //S/T 0)
     (set! //Foreach_/Types (@Make_Set (list //T_/Foreach_/Stat //T_/Foreach_/Stats //T_/Foreach_/T/S //T_/Foreach_/T/Ss //T_/Foreach_/S/T/S //T_/Foreach_/Expn //T_/Foreach_/Cond //T_/Foreach_/N/A/S //T_/Foreach_/Variable //T_/Foreach_/Global_/Var //T_/Foreach_/Lvalue //T_/Ateach_/Stat //T_/Ateach_/Stats //T_/Ateach_/T/S //T_/Ateach_/T/Ss //T_/Ateach_/S/T/S //T_/Ateach_/Expn //T_/Ateach_/Cond //T_/Ateach_/N/A/S //T_/Ateach_/Variable //T_/Ateach_/Global_/Var //T_/Ateach_/Lvalue)))
     (set! //Only_/Simple_/Types (list //T_/Foreach_/S/T/S //T_/Ateach_/S/T/S))
     ; Note that this will process inner FOREACH/ATEACH clauses first: 
     (cond
      ((not (null? (intersection-n (@Stat_Types (@I)) //Foreach_/Types)))
       (@Foreach_Statement /foreach-wsl2scheme-16 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))))
     (set! /foreach /foreach-save)
     (set! /name /name-save)
     (set! /body_var /body_var-save)
     (set! //S/T //S/T-save)
     (set! //Foreach_/Types //Foreach_/Types-save)
     (set! //Only_/Simple_/Types //Only_/Simple_/Types-save))
    ; Put the modified program back into I 
    (set! //I (@Program))
    (@Undo_Edit)
    (cond
     ((= (@GT //I) //T_/Statements)
      (@WS_Items (@Cs //I)))
     (#t
      (@WS_Item //I)))
    (@WSSL "")
    (cond
     ((not (equal? //Filename ""))
      (@Close_Output_Port //Output_/Port)))
    (set! //Num_/Var_/Types //Num_/Var_/Types-save)
    (set! //Num_/Funct_/Types //Num_/Funct_/Types-save)
    (set! //Num_/Types //Num_/Types-save))
   (set! //Globals //Globals-save)
   (set! /floop_depth /floop_depth-save)
   (set! /z_name /z_name-save)
   (set! /m_one /m_one-save)
   (set! /make_n /make_n-save)
   (set! /make_name /make_name-save)
   (set! /tmp_result /tmp_result-save)
   (set! /tmp_var /tmp_var-save)
   (set! /funct_result /funct_result-save)
   (set! //Call_/Types //Call_/Types-save)
   (set! //Macros //Macros-save))
  (set! //Input //Input-save)))

; Check for a FOREACH/ATEACH component or a called proc/function 
; other than a macro or recursive call. 
; If any of these are found, then local variables which mask globals 
; must be copied into the global (and restored afterwards), 
; so that they are visible to the called proc/function. 
(define (@WS_Calls? /name-par //I)
 (let ((/name-save /name)
       (/calls 0)
       (/comps-save /comps)
       (funct-result '()))
  (set! /name /name-par)
  (set! /comps (@Components //I))
  (cond
   ((member (@ST //I) //Foreach_/Types)
    (set! /calls 1))
   ((or (= (@ST //I) //T_/Maphash) (= (@ST //I) //T_/Map) (= (@ST //I) //T_/Reduce))
    (set! /calls 1))
   ((and (member (@ST //I) //Call_/Types) (not (equal? (@V (@Get_n //I 1)) /name)) (not-member (@V (@Get_n //I 1)) //Macros))
    (set! /calls 1))
   (#t
    (while (and (= /calls 0) (not (null? /comps))) 
     (cond
      ((@WS_Calls? /name (car /comps))
       (set! /calls 1))
      (#t
       (set! /comps (cdr /comps)))))))
  (set! funct-result (= /calls 1))
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

; Convert an item to Scheme, using the WS_Type and WS_Funct tables: 
(define (@WS_Item //I)
 (let ((//S/T-save //S/T)
       (/type-save /type)
       (/funct ""))
  (set! //S/T (@ST //I))
  (set! /type "")
  (set! /type (vector-ref //W/S_/Type (- //S/T 1)))
  ; If the value is a variable/proc name, then convert it here: 
  ; (Don't convert strings, numbers, EXIT values, or external function names) 
  (cond
   ((or (= //S/T //T_/Variable) (= //S/T //T_/Var_/Lvalue))
    (set! /funct (concat (vector-ref //W/S_/Funct (- //S/T 1)) (@WS_Name (@V //I)))))
   ((or (= //S/T //T_/For) (= //S/T //T_/Struct) (= //S/T //T_/Struct_/Lvalue) (= //S/T //T_/Proc_/Call) (= //S/T //T_/Funct_/Call) (= //S/T //T_/Action))
    (set! /funct (concat (vector-ref //W/S_/Funct (- //S/T 1)) (@WS_Name (@V (@Get_n //I 1))))))
   ((= //S/T //T_/Call)
    (set! /funct (string-append (concat (vector-ref //W/S_/Funct (- //S/T 1)) (@WS_Name (@V //I))) "_a")))
   ((and (@Cs? //I) (= (@ST (@Get_n //I 1)) //T_/Name))
    (set! /funct (concat (vector-ref //W/S_/Funct (- //S/T 1)) (@N_String (@V (@Get_n //I 1))))))
   (#t
    (set! /funct (concat (vector-ref //W/S_/Funct (- //S/T 1)) (@String (@Value //I))))))
  (cond
   (#f
    (display-list "@WS_Item: ST = " (@Type_Name //S/T) " funct = " /funct " type = " /type)))
  (cond
   ((equal? /type //Scheme_/Literal)
    (@WSS /funct))
   ((equal? /type //Scheme_/Funct)
    (cond
     ((and (not (equal? /funct "")) (> (@Size //I) 0))
      (set! /funct (string-append /funct " "))))
    (@WSS (string-append "(" /funct))
    (cond
     ((and (= (@Size //I) 1) (= (@GT (@Get_n //I 1)) //T_/Expressions))
      (@WS_Items (@Cs (@Get_n //I 1))))
     ((and (= (@Size //I) 2) (= (@ST (@Get_n //I 1)) //T_/Name) (= (@GT (@Get_n //I 2)) //T_/Expressions))
      (@WS_Items (@Cs (@Get_n //I 2))))
     ((and (> (@Size //I) 1) (= (@ST (@Get_n //I 1)) //T_/Name))
      (@WS_Items (cdr (@Cs //I))))
     (#t
      (@WS_Items (@Cs //I))))
    (@WSS ")"))
   ((equal? /type //Scheme_/Call)
    (cond
     ((and (not (equal? /funct "")) (or (> (@Size (@Get_n //I 2)) 0) (and (> (@Size //I) 2) (> (@Size (@Get_n //I 3)) 0))))
      (set! /funct (string-append /funct " "))))
    (let ((/var-save /var)
          (/res '()))
     (set! /var '())
     ; Special case: @Make_Name(str) --> 'str 
     ; NB: SCM returns 'foo for 'FOO, so this doesn't work! 
     (cond
      ((and #f (equal? (@V (@Get_n //I 1)) /make_name) (= (@ST (@Get_n (@Get_n //I 2) 1)) //T_/String))
       (@WSS (string-append "'" (@V (@Get //I (list 1 1))))))
      ((or (= (@Size //I) 2) (and (= (@Size //I) 3) (= (@Size (@Get_n //I 3)) 0)))
       ; Simple proc or funct call: 
       (@WSS (string-append "(" /funct))
       (@WS_Items (@Cs (@Get_n //I 2)))
       (@WSS ")"))
      ((and (= (@Size //I) 3) (= (@Size (@Get_n //I 3)) 1))
       ; A single VAR parameter -- result is returned by the funct 
       (@WS_Assign (@Get_n (@Get_n //I 3) 1) (@Make_Funct (@Make_Name /funct) (concat (@Cs (@Get_n //I 2)) (@Cs (@Get_n //I 3))))))
      ((and (= (@Size //I) 3) (> (@Size (@Get_n //I 3)) 1))
       ; Multiple results are returned: return a list which is 
       ; split up and assigned to the parameters 
       (@WSS (string-append (string-append "(let ((" (@WS_Name /tmp_result)) " "))
       (@WS_Item (@Make_Funct (@Make_Name /funct) (concat (@Cs (@Get_n //I 2)) (@Cs (@Get_n //I 3)))))
       (@WSS "))")
       (set! /res (@Make //T_/Variable /tmp_result '()))
       (for-in /var (@Cs (@Get_n //I 3)) 
        (begin
         (@WSSL "")
         (@WS_Assign /var (@Make //T_/Head '() (list /res)))
         (@WSS " ")
         (@WS_Assign /res (@Make //T_/Tail '() (list /res)))))
       (@WSS ")"))
      (#t
       (display-list "Unknown format of Scheme_Call: " (@Value //I))
       (@Print_WSL //I "")))
     (set! /var /var-save)))
   ((equal? /type //Scheme_/Decl)
    ; Proc or Funct declaration 
    (@WS_Decl //I))
   ((equal? /type //Scheme_/Ifmatch)
    ; Some kind of IFMATCH1 
    (@WS_Ifmatch //I))
   ((equal? /type //Scheme_/Fill)
    ; Some kind of FILL 
    (@WS_Fill //I))
   ((equal? /type //Scheme_/Special)
    (@WS_Special //I))
   (#t
    (error (string-append "Unknown type: " (@String /type)))))
  ; End of @WS_Item 
  
  (set! //S/T //S/T-save)
  (set! /type /type-save)))

; Translate an IFMATCH1 construct <pattern, yes-sts, no-sts> 
(define (@WS_Ifmatch //I)
 (let ((/vars-save /vars)
       (/vals '())
       (/n-save /n)
       (/pairs '()))
  (set! /vars (@Match_Vars (@Get_n //I 1)))
  (set! /n 0)
  (error "IFMATCH should be handeled by Ifmatch_Processing!")
  (error "Old version of Ifmatch is deprecated")
  (for-in /var /vars 
   (set! /pairs (cons (list /var (string-append (concat (concat (string-append "(@Make_Name " //Quote) (@N_String /var)) //Quote) ") ")) /pairs)))
  ; Set up a table with the current values of the listed vars: 
  (@WSS "(let* ((tab (list ")
  (set! /n (gen-length /vars))
  (for-in /pair /pairs 
   (begin
    (@WSS (string-append (concat (string-append "(cons " (wsl-ref /pair 2)) (@WS_Name (wsl-ref /pair 1))) ")"))
    (set! /n (- /n 1))
    (cond
     ((> /n 0)
      (@WSSL "")))))
  (@WSSL "))")
  (@WSS "(res (@Match ")
  ; Write the raw internal format of the item: 
  (@WSS "`")
  (@Write_Raw_Item (@Get_n //I 1) //Output_/Port)
  (@WSSL " (@I) tab)))")
  (@WSSL "(if (not (null? res))")
  ; Create local vars for the THEN part: 
  (@WSS "(let (")
  (for-in /pair /pairs 
   (begin
    (@WSS (string-append "(" (@WS_Name (wsl-ref /pair 1))))
    (@WSSL (string-append (string-append " (cdr (assq " (wsl-ref /pair 2)) " (cdr res))))"))))
  (@WSSL ")")
  (@WS_Item (@Get_n //I 2))
  (@WSSL ")")
  ; close the let 
  (@WS_Item (@Get_n //I 3))
  (@WSSL "))")
  ; close the if and let* 
  
  (set! /vars /vars-save)
  (set! /n /n-save)))

(define (@Match_Vars /pat)
 
 (if (and (@All_Pattern_Type? (@ST /pat)) (@Has_Value_Type? (@ST /pat))) (list (@V /pat)) (my-reduce @Set_Union (my-map @Match_Vars (@Components /pat)))))

; Translate a FILL construct: build a suitable @Make structure and translate it 
(define (@WS_Fill //I)
 (let ((/v_to_e-save /v_to_e)
       (/v_to_e_l-save /v_to_e_l)
       (/e_to_v-save /e_to_v)
       (/e_to_v_l-save /e_to_v_l))
  (set! /v_to_e (@Make_Name "@Var_To_Expn"))
  (set! /v_to_e_l (@Make_Name "@Var_To_Expn_List"))
  (set! /e_to_v (@Make_Name "@Expn_To_Var"))
  (set! /e_to_v_l (@Make_Name "@Expn_To_Var_List"))
  (@WS_Item (@Pattern_To_Make2 (@Get_n //I 1)))
  (set! /v_to_e /v_to_e-save)
  (set! /v_to_e_l /v_to_e_l-save)
  (set! /e_to_v /e_to_v-save)
  (set! /e_to_v_l /e_to_v_l-save)))

; Convert an item to a tree of @Make calls which will construct that item 
; but with patterns replaced by the appropriate variables: 
(define (@Pattern_To_Make //I)
 (let ((/comp '())
       (/new '())
       (/tr_new '())
       (/append (@Make_Name "append")))
  ; Build a list of translated components which form the components to an append 
  ; which, in turn, to forms the list of components for a @Make: 
  (for-in /comp (@Components //I) 
   (cond
    ((or (@Any_Pattern_Type? (@ST /comp)) (@Many_Pattern_Type? (@ST /comp)))
     (cond
      ((= (@GT /comp) //T_/Expression)
       (set! /new (cons (@Make_MWF /v_to_e_l (@Make //T_/Variable (@V /comp) '())) /new)))
      ((= (@GT /comp) //T_/Lvalue)
       (set! /new (cons (@Make_MWF /e_to_v_l (@Make //T_/Variable (@V /comp) '())) /new)))
      (#t
       (set! /new (cons (@Make //T_/Variable (@V /comp) '()) /new)))))
    (#t
     (set! /comp (@Pattern_To_Make /comp))
     ; Prepend the comp to the first T_Expressions in new, if there is one: 
     (cond
      ((or (null? /new) (not (= (@ST (wsl-ref /new 1)) //T_/Expressions)))
       (set! /new (cons (@Make //T_/Expressions '() (list /comp)) /new)))
      (#t
       (wsl-set! /new (@Make //T_/Expressions '() (cons /comp (@Cs (wsl-ref /new 1)))) 1))))))
  ; new is a list of variable/MW_Funct_Call/expressions items, convert it to an item 
  (for-in /comp /new 
   (cond
    ((or (= (@ST /comp) //T_/Variable) (= (@ST /comp) //T_/M/W_/Funct_/Call))
     (set! /tr_new (cons /comp /tr_new)))
    (#t
     (set! /tr_new (cons (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() (reverse (@Cs /comp))))) /tr_new)))))
  (cond
   ((null? /tr_new)
    (set! /tr_new (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() '())))))
   ((= (gen-length /tr_new) 1)
    (set! /tr_new (car /tr_new)))
   (#t
    (set! /tr_new (@Make_Funct /append /tr_new))))
  (if (@One_Pattern_Type? (@ST //I)) (if (= (@GT //I) //T_/Expression) (@Make_MWF /v_to_e (@Make //T_/Variable (@V //I) '())) (if (= (@GT //I) //T_/Lvalue) (@Make_MWF /e_to_v (@Make //T_/Variable (@V //I) '())) (@Make //T_/Variable (@V //I) '()))) (@Make_Make (@ST //I) (@Value //I) /tr_new))))

; If the item is a pattern with a negative integer as the value or expression 
; then let it pass through without being filled in 
; (ie generate a @Make call which will return this pattern): 
(define (@Pattern_To_Make2 //I)
 (let ((/comp '())
       (/new '())
       (/tr_new '())
       (/append (@Make_Name "append")))
  ; Build a list of translated components which form the components to an append 
  ; which, in turn, to forms the list of components for a @Make: 
  (for-in /comp (@Components //I) 
   (cond
    ((and (or (@Any_Pattern_Type? (@ST /comp)) (@Many_Pattern_Type? (@ST /comp))) (> (@V /comp) 0))
     (cond
      ((= (@GT /comp) //T_/Expression)
       (set! /new (cons (@Make_MWF /v_to_e_l (@Make //T_/Variable (@V /comp) '())) /new)))
      ((= (@GT /comp) //T_/Lvalue)
       (set! /new (cons (@Make_MWF /e_to_v_l (@Make //T_/Variable (@V /comp) '())) /new)))
      (#t
       (set! /new (cons (@Make //T_/Variable (@V /comp) '()) /new)))))
    ((and (@Any_Int_Type? (@ST /comp)) (not (= (@ST (@Get_n /comp 1)) //T_/Number)))
     (cond
      ((= (@GT /comp) //T_/Expression)
       (set! /new (cons (@Make_MWF /v_to_e_l (@Get_n /comp 1)) /new)))
      ((= (@GT /comp) //T_/Lvalue)
       (set! /new (cons (@Make_MWF /e_to_v_l (@Get_n /comp 1)) /new)))
      (#t
       ; This item needs to be interpolated in the final list: 
       (set! /new (cons /comp /new)))))
    ((and (@Any_Var_Type? (@ST /comp)) (> (@V /comp) 0))
     (error "Backreference variable interpolation ~*=var not valid in FILL!"))
    ((and (@One_Var_Type? (@ST /comp)) (> (@V /comp) 0))
     (error "Backreference variable interpolation ~?=var not valid in FILL!"))
    (#t
     (set! /comp (@Pattern_To_Make2 /comp))
     ; Prepend the comp to the first T_Expressions in new, if there is one: 
     (cond
      ((or (null? /new) (not (= (@ST (wsl-ref /new 1)) //T_/Expressions)))
       (set! /new (cons (@Make //T_/Expressions '() (list /comp)) /new)))
      (#t
       (wsl-set! /new (@Make //T_/Expressions '() (cons /comp (@Cs (wsl-ref /new 1)))) 1))))))
  ; new is a list of variable/MW_Funct_Call/expressions items, convert it to an item 
  (for-in /comp /new 
   (cond
    ((or (= (@ST /comp) //T_/Variable) (= (@ST /comp) //T_/M/W_/Funct_/Call))
     (set! /tr_new (cons /comp /tr_new)))
    ((and (@Any_Int_Type? (@ST /comp)) (not (= (@ST (@Get_n /comp 1)) //T_/Number)))
     (set! /tr_new (cons (@Get_n /comp 1) /tr_new)))
    (#t
     (set! /tr_new (cons (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() (reverse (@Cs /comp))))) /tr_new)))))
  (cond
   ((null? /tr_new)
    (set! /tr_new (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() '())))))
   ((= (gen-length /tr_new) 1)
    (set! /tr_new (car /tr_new)))
   (#t
    (set! /tr_new (@Make_Funct /append /tr_new))))
  (if (and (@One_Pattern_Type? (@ST //I)) (> (@V //I) 0)) (if (= (@GT //I) //T_/Expression) (@Make_MWF /v_to_e (@Make //T_/Variable (@V //I) '())) (if (= (@GT //I) //T_/Lvalue) (@Make_MWF /e_to_v (@Make //T_/Variable (@V //I) '())) (@Make //T_/Variable (@V //I) '()))) (if (and (@One_Int_Type? (@ST //I)) (not (= (@ST (@Get_n //I 1)) //T_/Number))) (if (= (@GT //I) //T_/Expression) (@Make_MWF /v_to_e (@Get_n //I 1)) (if (= (@GT //I) //T_/Lvalue) (@Make_MWF /e_to_v (@Get_n //I 1)) (@Get_n //I 1))) (@Make_Make (@ST //I) (@Value //I) /tr_new)))))

; Make a @Make function call with the given parameters 
; (type is a number, value is a value, comps is an item): 
(define (@Make_Make /type /value /comps)
 
 (@Make //T_/M/W_/Funct_/Call '() (list /make_n (@Make //T_/Expressions '() (list (@Make //T_/Number /type '()) (@WS_Value_To_Make /type /value) /comps)))))

(define (@WS_Value_To_Make /type-par /value)
 (let ((/type-save /type)
       (//R '())
       (funct-result '()))
  (set! /type /type-par)
  (cond
   ((null? /value)
    (set! //R (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() '())))))
   ((or (= /type //T_/Number) (= /type //T_/Exit))
    (set! //R (@Make //T_/Number /value '())))
   ((or (= /type //T_/String) (= /type //T_/Comment))
    (set! //R (@Make //T_/String /value '())))
   ((@All_Pattern_Type? /type)
    ; Value is either an identifier or a (negative) number: 
    (cond
     ((> /value 0)
      (set! //R (@Make_MWF /make_name (@Make //T_/String (@N_String /value) '()))))
     (#t
      (set! //R (@Make //T_/Number /value '())))))
   (#t
    ; Assume it is a name 
    (set! //R (@Make_MWF /make_name (@Make //T_/String (@N_String /value) '())))))
  (set! funct-result //R)
  (set! /type /type-save)
  funct-result))

; Translate a PROC or FUNCT declaration 
; If there is one VAR parameter, then return it. 
; If there is more than one, then return them all in a list. 
; Otherwise, return #t 
(define (@WS_Decl //I)
 (let ((//S/T-save //S/T))
  (set! //S/T (@ST //I))
  ; Check for MW types first 
  (cond
   ((= //S/T //T_/M/W_/Proc)
    ; values, vars, body 
    (let ((/result '()))
     ; If there are var parameters, then either return the (single) par 
     ; or return the list of pars as the result: 
     (cond
      ((= (@Size (@Get_n //I 3)) 1)
       (set! /result (@Get_n (@Get_n //I 3) 1)))
      ((> (@Size (@Get_n //I 3)) 1)
       (set! /result (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() (my-map @Lvalue_To_Expn (@Cs (@Get_n //I 3)))))))))
     (@WS_Funct_Decl (@V (@Get_n //I 1)) (concat (@Cs (@Get_n //I 2)) (@Cs (@Get_n //I 3))) '() (@Get_n //I 4) /result)))
   ((or (= //S/T //T_/M/W_/Funct) (= //S/T //T_/M/W_/B/Funct))
    ; pars, assigns, body, result 
    (@WS_Funct_Decl (@V (@Get_n //I 1)) (@Cs (@Get_n //I 2)) (@Cs (@Get_n //I 3)) (@Get_n //I 4) (@Get_n //I 5)))
   ((= //S/T //T_/Proc)
    ; values, vars, body 
    (@WSS (string-append (string-append "(" (@WS_Name (@V (@Get_n //I 1)))) " (lambda ("))
    (@WS_Items (concat (@Cs (@Get_n //I 2)) (@Cs (@Get_n //I 3))))
    (@WSSL ")")
    (cond
     ((or (not (= (@Size (@Get_n //I 4)) 1)) (not (= (@ST (@Get_n (@Get_n //I 4) 1)) //T_/Skip)))
      (@WS_Items (@Cs (@Get_n //I 4)))
      (@WSSL "")))
    (cond
     ((= (@Size (@Get_n //I 3)) 0)
      (@WSSL "#t))"))
     ((= (@Size (@Get_n //I 3)) 1)
      (@WS_Item (@Get_n (@Get_n //I 3) 1))
      (@WSSL "))"))
     (#t
      (@WSS "(list ")
      (@WS_Items (@Cs (@Get_n //I 3)))
      (@WSSL ")))"))))
   ((or (= //S/T //T_/Funct) (= //S/T //T_/B/Funct))
    ; pars, assigns, body, result 
    (@WSS (string-append (string-append "(" (@WS_Name (@V (@Get_n //I 1)))) " (lambda ("))
    (@WS_Items (@Cs (@Get_n //I 2)))
    (@WSSL ")")
    (@WSS "(let ")
    ; List of assigns: 
    (@WS_Item (@Get_n //I 3))
    (@WSSL "")
    ; Body: 
    (cond
     ((= (@Size //I) 5)
      (@WS_Item (@Get_n //I 4))))
    ; Result: 
    (@WS_Item (@Get_n //I (@Size //I)))
    (@WSSL ")))"))
   (#t
    (display-list "Unknown Declaration type: " //S/T)))
  (set! //S/T //S/T-save)))

; Generate either a macro or function definition 
; depending on whether the name is in the Macros list. 
; big = 1 if the body to be generated will have more than one item 
; (in which case a defmacro will need a (begin ...) enclosing the body) 
(define (@WS_Funct_Decl /name-par /pars /assigns /body /result)
 (let ((/name-save /name))
  (set! /name /name-par)
  (let ((/globals '())
        (/par '())
        (/new_pars '())
        (/big 0)
        (/renames '()))
   ; Check for calls: 
   (cond
    ((and (= (@Size /body) 1) (= (@ST (@Get_n /body 1)) //T_/Skip))
     ; This is a pure function 
     (set! /new_pars /pars))
    ((or (@WS_Calls? /name /body) (and (not (null? /result)) (@WS_Calls? /name /result)))
     ; Check for globals in the pars, add assigns to the block if necessary 
     ; and rename the parameter to `name-par' 
     (for-in /par /pars 
      (cond
       ((member (@V /par) //Globals)
        (set! /assigns (cons (@Make //T_/Assign '() (list /par (@Make //T_/Variable (@WS_Par /par) '()))) /assigns))
        (set! /renames (cons (@V /par) /renames))
        (set! /new_pars (cons (@Make //T_/Var_/Lvalue (@WS_Par /par) '()) /new_pars)))
       (#t
        (set! /new_pars (cons /par /new_pars)))))
     (set! /new_pars (reverse /new_pars)))
    (#t
     (set! /new_pars /pars)))
   ; Write the header: 
   (cond
    ((member /name //Macros)
     (@WSS (string-append (string-append "(defmacro " (@WS_Name /name)) " (")))
    (#t
     (@WSS (string-append "(define (" (@WS_Name /name)))
     (cond
      ((not (null? /new_pars))
       (@WSS " ")))))
   (@WS_Items /new_pars)
   (@WSSL ")")
   (cond
    ((or (not (null? /result)) (> (gen-length /assigns) 0) (> (@Size /body) 1) (not (= (@ST (@Get_n /body 1)) //T_/Skip)))
     (set! /big 1)))
   (cond
    ((member /name //Macros)
     (set! /ws_macro_pars (my-map @WS_Name (my-map @V /new_pars)))
     (cond
      ((= /big 1)
       (@WSSL "`(begin"))
      (#t
       (@WSS "`")))))
   ; Write the var block, or just write the body and result: 
   (cond
    ((null? /assigns)
     ; Check for a body: 
     (cond
      ((or (> (@Size /body) 1) (not (= (@ST (@Get_n /body 1)) //T_/Skip)))
       (@WS_Items (@Cs /body))))
     ; Check for a result: 
     (cond
      ((not (null? /result))
       (@WSSL "")
       (@WS_Item /result))))
    (#t
     ; Within the assigns, use name-par for the pars which shadow globals: 
     (@Edit)
     (let ((/new_assigns '())
           (/assign '())
           (/var-save /var))
      (set! /var '())
      (for-in /assign /assigns 
       (begin
        (@New_Program /assign)
        (@Down_To 2)
        (for-in /var (intersection-n (@Make_Set /renames) (@Variables (@I))) 
         (@Rename /var (@Make_Name (string-append (@N_String /var) "-par"))))
        (set! /new_assigns (cons (@Program) /new_assigns))))
      (@WS_Var_Block /name (reverse /new_assigns) /body /result)
      (set! /var /var-save))))
   ; Write the footer: 
   (cond
    ((member /name //Macros)
     (set! /ws_macro_pars '())))
   (cond
    ((and (member /name //Macros) (= /big 1))
     (@WSSL "))"))
    (#t
     (@WSSL ")"))))
  (set! /name /name-save)))

; Rename a variable by adding `-par' and return the new name 
(define (@WS_Par //I)
 
 (@Make_Name (string-append (@N_String (@V //I)) "-par")))

; Generate a block (for Funct/Bfunct/Var) with local vars 
; ensuring dynamic binding if necessary: 
(define (@WS_Var_Block /name-par /assigns /body /result)
 (let ((/name-save /name))
  (set! /name /name-par)
  (let ((/globals '())
        (/assign '())
        (/n-save /n))
   (set! /n 0)
   ; Check for calls: 
   (cond
    ((and (not (@WS_Calls? /name /body)) (or (null? /result) (not (@WS_Calls? /name /result))))
     ; No need to check for globals 
    )
    (#t
     (for-in /assign /assigns 
      (cond
       ((member (@V (@Get_n /assign 1)) //Globals)
        (set! /globals (cons (@V (@Get_n /assign 1)) /globals)))))))
   (cond
    ((null? /globals)
     ; We can use a simple let: 
     (@WSS "(let (")
     (set! /n (gen-length /assigns))
     (for-in /assign /assigns 
      (begin
       (@WSS "(")
       (@WS_Item (@Get_n /assign 1))
       (@WSS " ")
       (@WS_Item (@Get_n /assign 2))
       (@WSS ")")
       (set! /n (- /n 1))
       (cond
        ((> /n 0)
         (@WSSL "")))))
     (@WSS ")")
     ; end of the let vars 
     ; Check for a body: 
     (cond
      ((or (not (= (@Size /body) 1)) (not (= (@ST (@Get_n /body 1)) //T_/Skip)))
       (@WSSL "")
       (@WS_Items (@Cs /body))))
     ; Check for a result: 
     (cond
      ((not (null? /result))
       (@WSSL "")
       (@WS_Item /result)))
     (@WSS ")"))
    (#t
     ; Save the values of global vars in local vars, 
     ; initialise global vars, execute the body, save the result, 
     ; restore the values of global vars and return the saved result: 
     (@WSS "(let (")
     (set! /n (gen-length /assigns))
     (for-in /assign /assigns 
      (begin
       (cond
        ((member (@V (@Get_n /assign 1)) /globals)
         (@WSS "(")
         (@WS_Item (@Get_n /assign 1))
         (@WSS "-save ")
         (@WS_Item (@Get_n /assign 1))
         (@WSS ")"))
        (#t
         (@WSS "(")
         (@WS_Item (@Get_n /assign 1))
         (@WSS " ")
         (@WS_Item (@Get_n /assign 2))
         (@WSS ")")))
       (set! /n (- /n 1))
       (cond
        ((> /n 0)
         (@WSSL "")))))
     ; Check if we need a local var for the result 
     (cond
      ((and (not (null? /result)) (not (null? /globals)))
       (@WSSL "")
       (@WSS (string-append (string-append "(" /funct_result) " '())"))))
     (@WSS ")")
     ; end of the let vars 
     ; Assign the local values to the global vars: 
     (for-in /assign /assigns 
      (cond
       ((member (@V (@Get_n /assign 1)) /globals)
        (@WSSL "")
        (@WS_Assign (@Get_n /assign 1) (@Get_n /assign 2)))))
     ; Funct/BFunct/Var body: 
     (cond
      ((or (> (@Size /body) 1) (not (= (@ST (@Get_n /body 1)) //T_/Skip)))
       (@WSSL "")
       (@WS_Items (@Cs /body))))
     ; save result in funct_result if necessary: 
     (cond
      ((and (not (null? /result)) (not (null? /globals)))
       (@WSSL "")
       (@WSS (string-append (string-append "(set! " /funct_result) " "))
       (@WS_Item /result)
       (@WSS ")")))
     ; Restore the global values: 
     (for-in /assign /assigns 
      (cond
       ((member (@V (@Get_n /assign 1)) /globals)
        (@WSSL "")
        (@WSS "(set! ")
        (@WS_Item (@Get_n /assign 1))
        (@WSS " ")
        (@WS_Item (@Get_n /assign 1))
        (@WSS "-save)"))))
     ; Return the result: 
     (cond
      ((not (null? /result))
       (@WSSL "")
       (cond
        ((not (null? /globals))
         (@WSS /funct_result))
        (#t
         (@WS_Item /result)))))
     (@WSS ")")))
   (set! /n /n-save))
  (set! /name /name-save)))

; replace chars `from' by `to' in string, and return the result: 
(define (@WS_Replace /from /to /str)
 (let ((//R "")
       (/n 0)
       (/from_len (string-length /from)))
  (cond
   ((null? /str)
    (set! /str "")))
  (set! /n (my-index /from /str))
  (while (>= /n 0) 
   (begin
    (set! //R (concat (concat //R (substr /str 0 /n)) /to))
    (set! /str (substr /str (+ /n /from_len)))
    (set! /n (my-index /from /str))))
  (set! //R (concat //R /str))
  //R))

(define (@WS_Special //I)
 (let ((//S/T-save //S/T))
  (set! //S/T (@ST //I))
  (cond
   ((= //S/T //T_/Statements)
    (cond
     ((= (@Size //I) 1)
      (@WS_Item (@Get_n //I 1)))
     (#t
      (@WSSL "(begin")
      (@WS_Items (@Cs //I))
      (@WSS ")"))))
   ((= //S/T //T_/String)
    ; Ensure that the quotes are printed and backslash any backslashes: 
    (@WSS (concat (concat //Quote (@Fix_Quotes (@V //I))) //Quote)))
   ((= //S/T //T_/Sequence)
    (cond
     ((= (@Size (@Get_n //I 1)) 0)
      (@WSS "'()"))
     (#t
      (@WSS "(list ")
      (@WS_Items (@Cs (@Get_n //I 1)))
      (@WSS ")"))))
   ((= //S/T //T_/Comment)
    ; A comment must always be followed by a newline 
    ; Insert ; after each newline in the comment 
    (@WSSL (string-append ";" (@WS_Replace //Newline (string-append //Newline ";") (@V //I)))))
   ((= //S/T //T_/Assignment)
    ; Check for a parallel assignment: 
    (cond
     ((= (@Size //I) 1)
      (cond
       ((= (@ST (@Get_n //I 1)) //T_/Assign)
        (@WS_Assign (@Get //I (list 1 1)) (@Get //I (list 1 2))))
       (#t
        (@WS_Item (@Get_n //I 1)))))
     ((null? (intersection-n (@Assigned //I) (@Used //I)))
      ; Can do the assignments in sequence 
      (let ((/assign '())
            (/n-save /n))
       (set! /n (@Size //I))
       (for-in /assign (@Cs //I) 
        (begin
         (cond
          ((= (@ST /assign) //T_/Assign)
           (@WS_Assign (@Get_n /assign 1) (@Get_n /assign 2)))
          (#t
           (@WS_Item /assign)))
         (set! /n (- /n 1))
         (cond
          ((> /n 0)
           (@WSSL "")))))
       (set! /n /n-save)))
     (#t
      ; assign the values to temp vars 
      (let ((/n-save /n)
            (/assign '()))
       (set! /n 1)
       (@WSS "(let (")
       (set! /n (@Size //I))
       (for-in /assign (@Cs //I) 
        (begin
         (cond
          ((= (@ST /assign) //T_/Assign)
           (@WSS (string-append (string-append "(" (@WS_Name (@Make_Name (concat /tmp_var (@String /n))))) " "))
           (@WS_Item (@Get_n /assign 2))
           (@WSS ")"))
          (#t
           (@WS_Item /assign)))
         (set! /n (- /n 1))
         (cond
          ((> /n 0)
           (@WSSL "")))))
       (@WSSL ")")
       ; copy from temp vars to real vars 
       (set! /n (@Size //I))
       (for-in /assign (@Cs //I) 
        (begin
         (cond
          ((= (@ST /assign) //T_/Assign)
           (@WS_Assign (@Get_n /assign 1) (@Make //T_/Variable (@Make_Name (concat /tmp_var (@String /n))) '())))
          (#t
           (@WS_Item /assign)))
         (set! /n (- /n 1))
         (cond
          ((> /n 0)
           (@WSSL "")))))
       (@WSS ")")
       (set! /n /n-save)))))
   ((= //S/T //T_/A_/S)
    (let ((/action '()))
     ; A CALL Z terminates this action system: 
     (@WSSL "(call-with-current-continuation (lambda (//Z) (letrec (")
     (for-in /action (@Cs (@Get_n //I 2)) 
      (cond
       ((= (@ST /action) //T_/Action)
        (@WSSL (string-append (string-append (string-append "(" (@WS_Name (@V (@Get_n /action 1)))) "_a") " (lambda ()"))
        (@WS_Item (@Get_n /action 2))
        (@WSSL "))"))
       (#t
        (@WS_Item /action))))
     (@WSSL ")")
     ; Kick off with a call to the starting action: 
     (@WSS (string-append (string-append (string-append "(" (@WS_Name (@V (@Get_n //I 1)))) "_a") "))))"))))
   ((= //S/T //T_/Call)
    ; CALL Z requires a parameter: 
    (cond
     ((equal? (@V //I) /z_name)
      (@WSS (string-append (string-append "(" (@WS_Name (@V //I))) " #t)")))
     (#t
      (@WSS (string-append (string-append (string-append "(" (@WS_Name (@V //I))) "_a") ")")))))
   ((= //S/T //T_/D_/Do)
    (let ((/guard '())
          (/n-save /n))
     (set! /n 0)
     (@WSSL "(let D_Do_Loop ()")
     (@WSSL "(cond")
     (set! /n (@Size //I))
     (for-in /guard (@Cs //I) 
      (begin
       (@WSS "(")
       (@WS_Item (@Get_n /guard 1))
       (@WSSL "")
       (@WS_Items (@Cs (@Get_n /guard 2)))
       (@WSS " (D_Do_Loop))")
       (set! /n (- /n 1))
       (cond
        ((> /n 0)
         (@WSSL "")))))
     (@WSS ")")
     (set! /n /n-save)))
   ((or (= //S/T //T_/Cond) (= //S/T //T_/D_/If))
    (let ((/guard '())
          (/comps-save /comps)
          (/n-save /n))
     (set! /comps '())
     (set! /n 0)
     (@WSSL "(cond")
     (set! /comps (@Cs //I))
     ; Ignore a redundant clause in a Cond 
     (cond
      ((and (= //S/T //T_/Cond) (= (@ST (@Get_n (last-1 /comps) 1)) //T_/True) (= (@Size (@Get_n (last-1 /comps) 2)) 1) (= (@ST (@Get (last-1 /comps) (list 2 1))) //T_/Skip))
       (set! /comps (butlast-1 /comps))))
     (set! /n (gen-length /comps))
     (for-in /guard /comps 
      (begin
       (cond
        ((= (@ST /guard) //T_/Guarded)
         (@WSS "(")
         (@WS_Item (@Get_n /guard 1))
         (@WSSL "")
         (@WS_Items (@Cs (@Get_n /guard 2)))
         (@WSS ")"))
        (#t
         (@WS_Item /guard)))
       (set! /n (- /n 1))
       (cond
        ((> /n 0)
         (@WSSL "")))))
     (@WSS ")")
     (set! /comps /comps-save)
     (set! /n /n-save)))
   ((= //S/T //T_/Exit)
    (let ((/tv (- /floop_depth (@V //I))))
     (cond
      ((< /tv 0)
       (set! /tv 0)))
     (@WSS (string-append (string-append "(exit_" (@String /tv)) " #t)"))))
   ((= //S/T //T_/Floop)
    (@WSSL (string-append "(floop exit_" (@String /floop_depth)))
    (set! /floop_depth (+ /floop_depth 1))
    (@WS_Item (@Get_n //I 1))
    (set! /floop_depth (- /floop_depth 1))
    (@WSS ")"))
   ((= //S/T //T_/Var)
    ; Save global values in local vars, then set! new values. 
    ; (We have to do it this way for dynamic binding to work properly). 
    ; But if there are no calls, we don't have to worry: 
    (@WS_Var_Block '() (@Cs (@Get_n //I 1)) (@Get_n //I 2) '()))
   ((= //S/T //T_/Where)
    (@WSS "(letrec ")
    (@WS_Item (@Get_n //I 2))
    (@WSSL "")
    (@WS_Item (@Get_n //I 1))
    (@WSSL ")"))
   ((= //S/T //T_/Aref)
    (@WSS "(wsl-ref ")
    (@WS_Item (@Get_n //I 1))
    (@WSS " ")
    (@WS_Items (@Cs (@Get_n //I 2)))
    (@WSS ")"))
   ((or (= //S/T //T_/Struct) (= //S/T //T_/Struct_/Lvalue))
    (@WS_Item (@Get_n //I 2))
    (@WSS ".")
    (@WSS (@WS_Name (@V (@Get_n //I 1)))))
   ((= //S/T //T_/Equal)
    (cond
     ((or (@WC_Numeric_Exp? (@Get_n //I 2)) (@WC_Numeric_Exp? (@Get_n //I 1)))
      (@WSS "(= "))
     (#t
      (@WSS "(equal? ")))
    (@WS_Item (@Get_n //I 1))
    (@WSS " ")
    (@WS_Item (@Get_n //I 2))
    (@WSS ")"))
   ((= //S/T //T_/Not_/Equal)
    (@WSS "(not ")
    (@WS_Item (@Make //T_/Equal '() (@Cs //I)))
    (@WSS ")"))
   ((= //S/T //T_/For)
    (@WSS (string-append (string-append "(for " (@WS_Name (@V (@Get_n //I 1)))) " "))
    (@WS_Items (cdr (@Cs //I)))
    (@WSS ")"))
   ((= //S/T //T_/For_/In)
    (@WSS (string-append (string-append "(for-in " (@WS_Name (@V (@Get_n //I 1)))) " "))
    (@WS_Items (cdr (@Cs //I)))
    (@WSS ")"))
   (#t
    (display-list "Unknown Scheme_Special type: " //S/T)))
  (set! //S/T //S/T-save)))

; Write a list of items (space or newline separated) 
; Statements are newline separated, but note that a comment already 
; has the newline added. 
(define (@WS_Items /list)
 (while (not (null? /list)) 
  (begin
   (cond
    ((and (= (@GT (car /list)) //T_/Statements) (= //W/S_/Indent_/Pending 0))
     (@WSSL "")))
   (@WS_Item (car /list))
   (cond
    ((not (null? (cdr /list)))
     (cond
      ((= (@GT (car /list)) //T_/Statement)
       (cond
        ((not (= (@ST (car /list)) //T_/Comment))
         (@WSSL ""))))
      (#t
       (@WSS " ")))))
   (set! /list (cdr /list)))))

; Make an external funct call 
(define (@Make_Funct /name /comps)
 
 (@Make //T_/X_/Funct_/Call '() (list (@Name /name) (@Make //T_/Expressions '() (@Var_To_Expn_List /comps)))))

; Make an external bfunct call 
(define (@Make_BFunct /name /comps)
 
 (@Make //T_/X_/B/Funct_/Call '() (list (@Name /name) (@Make //T_/Expressions '() (@Var_To_Expn_List /comps)))))

; Make an MW_FUNCT call with one argument: 
(define (@Make_MWF /name /arg)
 
 (@Make //T_/M/W_/Funct_/Call '() (list (@Name /name) (@Make //T_/Expressions '() (list (@Var_To_Expn /arg))))))

; Generate an assignment (check for array references on LHS) 
(define (@WS_Assign /var-par /val)
 (let ((/var-save /var))
  (set! /var /var-par)
  (cond
   ((and (or (= (@ST /var) //T_/Variable) (= (@ST /var) //T_/Var_/Lvalue)) (@Starts_With? (@V /var) "%const__"))
    ; const definition 
    (@WSS "(define ")
    (@WS_Item /var)
    (@WSS " ")
    (@WS_Item /val)
    (@WSS ")"))
   ((or (= (@ST /var) //T_/Variable) (= (@ST /var) //T_/Var_/Lvalue) (= (@ST /var) //T_/Struct) (= (@ST /var) //T_/Struct_/Lvalue) (= (@ST /var) //T_/Lvalue_/Pat_/One) (= (@ST /var) //T_/Lvalue_/Pat_/Many) (= (@ST /var) //T_/Lvalue_/Pat_/Any) (= (@ST /var) //T_/Mem_/Lvalue) (= (@ST /var) //T_/Mem_/Seg_/Lvalue) (= (@ST /var) //T_/Mem_/Rel_/Lvalue))
    ; simple assignment 
    (@WSS "(set! ")
    (@WS_Item /var)
    (@WSS " ")
    (@WS_Item /val)
    (@WSS ")"))
   ((= (@ST /var) //T_/Aref_/Lvalue)
    ; Collect the indexes from nested Arefs: 
    (let ((/indexes '()))
     (while (= (@ST /var) //T_/Aref_/Lvalue) 
      (begin
       (set! /indexes (concat (@Cs (@Get_n /var 2)) /indexes))
       (set! /var (@Get_n /var 1))))
     (@WSS "(wsl-set! ")
     (@WS_Item /var)
     (@WSS " ")
     (@WS_Item /val)
     (@WSS " ")
     (@WS_Items /indexes)
     (@WSS ")")))
   ((= (@ST /var) //T_/Sub_/Seg_/Lvalue)
    (@WS_Seg (@Get_n /var 1) (@Get_n /var 2) (@Get_n /var 3) /val))
   ((= (@ST /var) //T_/Rel_/Seg_/Lvalue)
    (@WS_Seg (@Get_n /var 1) (@Get_n /var 2) (@Make //T_/Plus '() (list (@Get_n /var 2) (@Get_n /var 3) /m_one)) /val))
   ((= (@ST /var) //T_/Final_/Seg_/Lvalue)
    (@WS_Seg (@Get_n /var 1) (@Get_n /var 2) (@Make //T_/Length '() (list (@Lvalue_To_Expn (@Get_n /var 1)))) /val))
   (#t
    (display-list "Unknown type in LHS of assign: " (@ST /var))))
  (set! /var /var-save)))

; Generate code to replace a segment of an array or list: 
(define (@WS_Seg /var-par /from /to /val)
 (let ((/var-save /var))
  (set! /var /var-par)
  (@WS_Assign /var (@Make_Funct (@Make_Name "@Update_Segment") (list /var /from /to /val)))
  (set! /var /var-save)))

; Convert a WSL name to a string suitable for a Scheme atom. 
; Ensure that the result will distinguish upper and lower case. 
; Add a prefix to all names (to avoid clashes with Scheme symbols) 
; Replace backslash by _bs_ (to avoid problems after C compilation) 
; Don't fix names that start with @ 
(define (@WS_Name /name-par)
 (let ((/name-save /name)
       (//R "")
       (/i 0)
       (/caps "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
       (/prefix "/")
       (funct-result '()))
  (set! /name /name-par)
  (cond
   ((< /name 0)
    (set! //R (@String (- /name))))
   (#t
    (set! /name (@N_String /name))
    (cond
     ((equal? (substr /name 0 1) "@")
      (set! //R /name))
     (#t
      (set! //R /prefix)
      (for /i 0 (- (string-length /name) 1) 1 
       (begin
        (cond
         ((>= (my-index (substr /name /i 1) /caps) 0)
          (set! //R (concat //R /prefix))))
        (cond
         ((equal? (substr /name /i 1) //Backslash)
          (set! //R (string-append //R "_bs_")))
         (#t
          (set! //R (concat //R (substr /name /i 1)))))))))))
  (set! funct-result (if (member //R /ws_macro_pars) (string-append "," //R) //R))
  (set! /name /name-save)
  funct-result))

(define (@WC_Numeric_Exp? //I)
 (let ((//R 0))
  (cond
   ((= (@ST //I) //T_/Variable)
    (cond
     ((not (null? (gethash //Num_/Var_/Types (@V //I))))
      (set! //R 1))))
   ((= (@ST //I) //T_/M/W_/Funct_/Call)
    (cond
     ((not (null? (gethash //Num_/Funct_/Types (@V (@Get_n //I 1)))))
      (set! //R 1))))
   ((not (null? (gethash //Num_/Types (@ST //I))))
    (set! //R 1)))
  (= //R 1)))

(define (@WS_Constant_Fill? //I)
 (let ((//R 1)
       (//S/T-save //S/T)
       (/comps-save /comps)
       (funct-result '()))
  (set! //S/T (@ST //I))
  (set! /comps '())
  (cond
   ((or (@One_Pattern_Type? //S/T) (@Many_Pattern_Type? //S/T) (@Any_Pattern_Type? //S/T) (@One_Var_Type? //S/T) (@Any_Var_Type? //S/T))
    (cond
     ((> (@V //I) 0)
      (set! //R 0))))
   ((or (@One_Int_Type? //S/T) (@Any_Int_Type? //S/T))
    (cond
     ((not (= (@ST (@Get_n //I 1)) //T_/Number))
      (set! //R 0))))
   ((@Cs? //I)
    (set! /comps (@Cs //I))
    (while (and (= //R 1) (not (null? /comps))) 
     (cond
      ((not (@WS_Constant_Fill? (car /comps)))
       (set! //R 0))
      (#t
       (set! /comps (cdr /comps)))))))
  (set! funct-result (= //R 1))
  (set! //S/T //S/T-save)
  (set! /comps /comps-save)
  funct-result))

; Return the set of variables used as indices in arrays: 
(define (@Find_Index_Vars)
 (let ((/vars-save /vars)
       (funct-result '()))
  (set! /vars '())
  (@Foreach_Expn /foreach-wsl2scheme-17 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result /vars)
  (set! /vars /vars-save)
  funct-result))

#t
