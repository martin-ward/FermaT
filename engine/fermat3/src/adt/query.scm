;;; Scheme translation of WSL code
(define (/foreach-query-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Variable)
   (set! /vars (union-n (list (@V (@I))) /vars)))))

(define (/foreach-query-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Var_/Lvalue)
   (set! /vars (union-n (list (@V (@I))) /vars)))))

(define /%const__query__1 (@Make 112 (@Make_Name "Z") '()))
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
; This file contains the `query' functions; ie those which return         
; information about part of the WSL tree structure.  Many of these        
; functions store their results in tables linked to the trees, but        
; not all of them do this.                                                
; ----------------------------------------------------------------------- 
; ----------------------------------------------------------------------- 
; The first set of functions in this file return information about the    
; use of variables in a section of code.                                  
; ----------------------------------------------------------------------- 
(set! //Qry_/Vars (@Make_Name "Qry_Vars"))
(set! //Qry_/Clobbered (@Make_Name "Qry_Clobbered"))
(set! //Qry_/Redefined (@Make_Name "Qry_Redefined"))
(set! //Qry_/U/B/A (@Make_Name "Qry_UBA"))
(set! //Qry_/Elts_/U/B/A (@Make_Name "Qty_Elts_UBA"))
(set! //Qry_/Elts (@Make_Name "Qry_Elts"))
(set! //Qry_/Elts_/Redefined (@Make_Name "Qry_Elts_Redefined"))
(set! //Qry_/Calls (@Make_Name "Qry_Calls"))
(set! //Qry_/Proc_/Calls (@Make_Name "Qry_Proc_Calls"))
(set! //Qry_/A_/Proc_/Calls (@Make_Name "Qry_A_Proc_Calls"))
(set! //Qry_/Funct_/Calls (@Make_Name "Qry_Funct_Calls"))
(set! //Qry_/X_/Funct_/Calls (@Make_Name "Qry_X_Funct_Calls"))
(set! //Qry_/Max_/Dep (@Make_Name "Qry_Max_Dep"))
(set! //Qry_/Rec_/T/Vs (@Make_Name "Qry_Rec_TVs"))
(set! //Qry_/Reg_/T/Vs (@Make_Name "Qry_Reg_TVs"))
(set! //Qry_/Hyb_/T/Vs (@Make_Name "Qry_Hyb_TVs"))
(set! //Qry_/Max_/Pos_/L (@Make_Name "Qry_Max_Pos_L"))
(set! //Assume_/A_/S_/Regular 0)
(set! /os_name (@Make_Name "os"))
(define (@Variables //I)
 (let ((//Result (@Qry_Vars //I)))
  (union-n (wsl-ref //Result 1) (wsl-ref //Result 2))))

(define (@Assigned //I)
 (let ((//Result (@Qry_Vars //I)))
  (wsl-ref //Result 1)))

(define (@Used //I)
 (let ((//Result (@Qry_Vars //I)))
  (wsl-ref //Result 2)))

(define (@Assd_Only //I)
 (let ((//Result (@Qry_Vars //I)))
  (@Set_Difference (wsl-ref //Result 1) (wsl-ref //Result 2))))

(define (@Used_Only //I)
 (let ((//Result (@Qry_Vars //I)))
  (@Set_Difference (wsl-ref //Result 2) (wsl-ref //Result 1))))

(define (@Assd_To_Self //I)
 (let ((//Result (@Qry_Vars //I)))
  (wsl-ref //Result 3)))

; ----------------------------------------------------------------------- 
; The following function returns a set of variables used etc.             
; The format is as follows...                                             
;                                                                         
; <Assigned, Used, Assigned_To_Self>                                      
;                                                                         
; where...                                                                
;                                                                         
;         Assigned - are the variables referred to as l-values.           
;                                                                         
;             Used - are the variables which are used in expressions.     
;                                                                         
; Assigned_To_Self - are the variables which are only used in assign-     
;                    ments to themselves (eg `X' in `X := X + Q').        
;                                                                         
; ----------------------------------------------------------------------- 
(define (@Qry_Vars //I)
 (let ((//S/T (@ST //I))
       (//G/T (@GT //I))
       (//Result '()))
  (cond
   ((not (@Cs? //I))
    ;The item is a leaf node, so we know the results for that, trivially.
    (set! //Result (list (if (= //S/T //T_/Var_/Lvalue) (list (@V //I)) '()) (if (= //S/T //T_/Variable) (list (@V //I)) '()) (if (or (= //S/T //T_/Var_/Lvalue) (= //S/T //T_/Variable)) (list (@V //I)) '()))))
   ((or (= //G/T //T_/Expression) (= //G/T //T_/Condition) (= //G/T //T_/Expressions))
    (set! //Result (list '() (@Qry_Exp_Vars //I) '())))
   (#t
    ;The item is not a leaf node, so we look in a table for the result. 
    (let ((//Previous (@Dtable_Get //I //Qry_/Vars))
          (//Assd '())
          (//Used '())
          (//Self '())
          (//Comps (@Cs //I))
          (//Temp '()))
     ; Check for a stored result 
     (cond
      ((not (null? //Previous))
       (set! //Result (@Dtable_Value_Part //Previous)))
      (#t
       ;The result is not stored in a table, so it must be calculated. 
       ;We look at each component in turn, calculate the variables for 
       ;that and combine these results.                                
       ; An array Lvalue uses as well as assigns to its first component 
       ; Also, VAR parameters are used as well as assigned.             
       (while (not (null? //Comps)) 
        (begin
         (cond
          ((or (= (@GT (car //Comps)) //T_/Expression) (= (@GT (car //Comps)) //T_/Condition))
           (set! //Used (union-n //Used (@Qry_Exp_Vars (car //Comps)))))
          (#t
           (set! //Temp (@Qry_Vars (car //Comps)))
           (cond
            ((= //S/T //T_/Assign)
             (set! //Self (union-n //Self (wsl-ref //Temp 3))))
            ((or (= //G/T //T_/Statement) (= //G/T //T_/Assignment) (= //G/T //T_/Variable) (= //G/T //T_/Statements) (= //G/T //T_/Guarded) (= //G/T //T_/Action))
             (set! //Self (union-n (@Set_Difference (wsl-ref //Temp 3) //Used) (@Set_Difference //Self (wsl-ref //Temp 2)))))
            (#t
             (set! //Self '())))
           (set! //Assd (union-n //Assd (wsl-ref //Temp 1)))
           (set! //Used (union-n //Used (wsl-ref //Temp 2)))))
         (set! //Comps (cdr //Comps))))
       ;If the current item is a local variable structure or a `FOR' loop 
       ;then there are local variables which we don't want to return as   
       ;part of the result; so these must be taken out.                   
       (cond
        ((= //S/T //T_/Var)
         (let ((//Local_/Vars (@Make_Set (my-map @Qry_Get_Lvar (@Cs (@Get_n //I 1))))))
          (set! //Assd (@Set_Difference //Assd //Local_/Vars))
          (set! //Used (@Set_Difference //Used //Local_/Vars))
          (set! //Self (@Set_Difference //Self //Local_/Vars))))
        ((= //S/T //T_/For)
         (let ((//Loop_/Var (list (@V (@Get_n //I 1)))))
          (set! //Assd (@Set_Difference //Assd //Loop_/Var))
          (set! //Used (@Set_Difference //Used //Loop_/Var))
          (set! //Self (@Set_Difference //Self //Loop_/Var))))
        ((or (= //S/T //T_/Proc) (= //S/T //T_/Funct) (= //S/T //T_/B/Funct))
         (let ((//Local_/Vars (union-n (@Assigned (@Get_n //I 2)) (@Assigned (@Get_n //I 3)))))
          (set! //Assd (@Set_Difference //Assd //Local_/Vars))
          (set! //Used (@Set_Difference //Used //Local_/Vars))
          (set! //Self (@Set_Difference //Self //Local_/Vars))))
        ((or (= //S/T //T_/Print) (= //S/T //T_/Prinflush) (= //S/T //T_/Error))
         (set! //Assd (union-n //Assd (list /os_name)))
         (set! //Used (union-n //Used (list /os_name))))
        ((= //S/T //T_/Pop)
         (set! //Used (union-n //Used (@Assigned (@Get_n //I 2))))
         (set! //Self '()))
        ((or (= //S/T //T_/Mem_/Lvalue) (= //S/T //T_/Mem_/Seg_/Lvalue) (= //S/T //T_/Mem_/Rel_/Lvalue))
         (set! //Assd (union-n //Assd (list /a_name)))
         (set! //Used (union-n //Used (list /a_name))))
        ((or (= //S/T //T_/Struct_/Lvalue) (= //S/T //T_/Aref_/Lvalue) (= //S/T //T_/Sub_/Seg_/Lvalue) (= //S/T //T_/Rel_/Seg_/Lvalue) (= //S/T //T_/Final_/Seg_/Lvalue) (= //S/T //T_/Proc_/Call) (= //S/T //T_/A_/Proc_/Call) (= //S/T //T_/M/W_/Proc_/Call) (= //S/T //T_/Push))
         (set! //Used (union-n //Used //Assd))))
       (set! //Result (list //Assd //Used //Self))
       ;Store the result in the table associated with this item.
       (@Dtable_Put //I //Qry_/Vars //Result))))))
  //Result))

; Temporary functions for pointers 
; (1) Replace all tests by function calls 
; (2) Add new syntax to WSL 
; (3) Extend functions to handle both versions 
; (4) Modify translators and functions to generate new syntax 
; (5) Modify functions to complain about old syntax 
; (6) Test thoroughly 
; (7) Replace functions by direct references to new syntax. 
(set! /a_name (@Make_Name "a"))
(set! /a_name_var (@Make //T_/Variable /a_name '()))
(set! /a_name_lvar (@Make //T_/Var_/Lvalue /a_name '()))
(set! /address_of (@Make_Name "address_of"))
(set! /address_of_name (@Make //T_/Name /address_of '()))
(define (@Is_Mem? //I)
 
 (or (and (or (= (@ST //I) //T_/Aref) (= (@ST //I) //T_/Aref_/Lvalue)) (or (= (@ST (@Get_n //I 1)) //T_/Variable) (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue)) (equal? (@V (@Get_n //I 1)) /a_name)) (= (@ST //I) //T_/Mem) (= (@ST //I) //T_/Mem_/Lvalue)))

(define (@Is_DSECT? //I)
 
 (or (and (or (= (@ST //I) //T_/Aref) (= (@ST //I) //T_/Aref_/Lvalue)) (or (= (@ST (@Get_n //I 1)) //T_/Variable) (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue)) (equal? (@V (@Get_n //I 1)) /a_name) (= (@ST (@Get_n (@Get_n //I 2) 1)) //T_/Variable)) (and (or (= (@ST //I) //T_/Mem) (= (@ST //I) //T_/Mem_/Lvalue)) (= (@ST (@Get_n //I 1)) //T_/Variable))))

(define (@Is_Mem_Rel? //I)
 
 (or (and (or (= (@ST //I) //T_/Rel_/Seg) (= (@ST //I) //T_/Rel_/Seg_/Lvalue)) (or (= (@ST (@Get_n //I 1)) //T_/Variable) (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue)) (equal? (@V (@Get_n //I 1)) /a_name)) (= (@ST //I) //T_/Mem_/Rel) (= (@ST //I) //T_/Mem_/Rel_/Lvalue)))

(define (@Get_Mem //I)
 
 (if (or (= (@ST //I) //T_/Mem) (= (@ST //I) //T_/Mem_/Lvalue)) (@Get_n //I 1) (@Get_n (@Get_n //I 2) 1)))

(define (@Get_Mem_Rel //I)
 
 (if (or (= (@ST //I) //T_/Mem_/Rel) (= (@ST //I) //T_/Mem_/Rel_/Lvalue)) (@Get_n //I 1) (@Get_n //I 2)))

(define (@Get_Mem_Rel_N //I)
 
 (if (or (= (@ST //I) //T_/Mem_/Rel) (= (@ST //I) //T_/Mem_/Rel_/Lvalue)) (@Get_n //I 2) (@Get_n //I 3)))

(define (@Is_Addr? //I)
 
 (or (and (= (@ST //I) //T_/X_/Funct_/Call) (equal? (@V (@Get_n //I 1)) /address_of)) (= (@ST //I) //T_/Address_/Of)))

(define (@Get_Addr //I)
 
 (if (= (@ST //I) //T_/Address_/Of) (@Get_n //I 1) (@Get_n (@Get_n //I 2) 1)))

; Use 0 for old and 1 for new 
(set! //Mem_/Version 0)
(define (@Make_Addr //I)
 
 (if (= //Mem_/Version 0) (@Make //T_/X_/Funct_/Call '() (list /address_of_name (@Make //T_/Expressions '() (list //I)))) (@Make //T_/Address_/Of '() (list //I))))

(define (@Make_Mem //I)
 
 (if (= //Mem_/Version 0) (@Make //T_/Aref '() (list /a_name_var (@Make //T_/Expressions '() (list //I)))) (@Make //T_/Mem '() (list //I))))

(define (@Make_Mem_L //I)
 
 (if (= //Mem_/Version 0) (@Make //T_/Aref_/Lvalue '() (list /a_name_lvar (@Make //T_/Expressions '() (list //I)))) (@Make //T_/Mem_/Lvalue '() (list //I))))

(define (@Make_Mem_Seg //I /n)
 
 (if (= //Mem_/Version 0) (@Make //T_/Rel_/Seg '() (list /a_name_var //I /n)) (@Make //T_/Mem_/Rel '() (list //I /n))))

(define (@Make_Mem_Seg_L //I)
 
 (if (= //Mem_/Version 0) (@Make //T_/Rel_/Seg_/Lvalue '() (list /a_name_lvar //I /n)) (@Make //T_/Mem_/Rel_/Lvalue '() (list //I /n))))

; ----------------------------------------------------------------------- 
; The following function calculates the variables used in an expression   
; or condition.  The reasons for having a separate function to do this    
; are:                                                                    
;                                                                         
;   1.  We know that there will be no assigned variables.                 
;   2.  There is little point in storing the results in tables (or in     
;       looking in tables for the results).                               
;                                                                         
; ----------------------------------------------------------------------- 
(define (@Qry_Exp_Vars //I)
 
 (if (@Cs? //I) (if (@Is_Addr? //I) (@Qry_Exp_Vars_A (@Get_Addr //I)) (if (or (= (@ST //I) //T_/Forall) (= (@ST //I) //T_/Exists)) (@Set_Difference (@Qry_Exp_Vars (@Get_n //I 2)) (@Variables (@Get_n //I 1))) (if (or (= (@ST //I) //T_/Mem) (= (@ST //I) //T_/Mem_/Seg) (= (@ST //I) //T_/Mem_/Rel)) (union-n (list /a_name) (my-reduce @Set_Union (my-map @Qry_Exp_Vars (@Cs //I)))) (my-reduce @Set_Union (my-map @Qry_Exp_Vars (@Cs //I)))))) (if (= (@ST //I) //T_/Variable) (list (@V //I)) '())))

; !XF address_of(foo) does not use value of foo, 
; but !XF address_of(foo[bar]) *does* depend on the value of bar! 
(define (@Qry_Exp_Vars_A //I)
 
 (if (member (@ST //I) (list //T_/Aref //T_/Sub_/Seg //T_/Rel_/Seg //T_/Final_/Seg)) (union-n (@Qry_Exp_Vars_A (@Get_n //I 1)) (my-reduce @Set_Union (my-map @Qry_Exp_Vars (cdr (@Cs //I))))) (if (@Cs? //I) (my-reduce @Set_Union (my-map @Qry_Exp_Vars_A (@Cs //I))) '())))

; Return ALL variables, including !XF address_of(...) variables: 
(define (@All_Variables //I)
 
 (if (@Cs? //I) (if (or (= (@ST //I) //T_/Mem) (= (@ST //I) //T_/Mem_/Seg) (= (@ST //I) //T_/Mem_/Rel)) (union-n (list /a_name) (my-reduce @Set_Union (my-map @Qry_Exp_Vars (@Cs //I)))) (my-reduce @Set_Union (my-map @All_Variables (@Cs //I)))) (if (or (= (@ST //I) //T_/Variable) (= (@ST //I) //T_/Var_/Lvalue)) (list (@V //I)) '())))

; Compute the set of primed vars in the given expression or condition: 
(define (@Primed_Vars //I)
 
 (if (@Cs? //I) (my-reduce @Set_Union (my-map @Primed_Vars (@Cs //I))) (if (= (@ST //I) //T_/Primed_/Var) (list (@V //I)) '())))

; Return the list of variable names assigned from an Assigns or Assignment item 
; (Note: @Elt_Lvars returns the list of element lists, <name, field, ...>) 
(define (@Lvars //I)
 
 (@Make_Set (my-map @Qry_Get_Lvar (@Cs //I))))

; Given an Assign, return the name of the variable assigned 
(define (@Qry_Get_Lvar //I)
 
 (@Lvalue_Name (@Get_n //I 1)))

; Return the name of the variable assigned in the given Lvalue 
(define (@Lvalue_Name //I)
 
 (if (= (@ST //I) //T_/Var_/Lvalue) (@V //I) (if (= (@ST //I) //T_/Struct_/Lvalue) (@Lvalue_Name (@Get_n //I 2)) (if (or (= (@ST //I) //T_/Lvalue_/Pat_/One) (= (@ST //I) //T_/Lvalue_/Pat_/Many) (= (@ST //I) //T_/Lvalue_/Pat_/Any)) (@V //I) (if (or (= (@ST //I) //T_/Mem_/Lvalue) (= (@ST //I) //T_/Mem_/Seg_/Lvalue) (= (@ST //I) //T_/Mem_/Rel_/Lvalue)) /a_name (@Lvalue_Name (@Get_n //I 1)))))))

; Return the list of elements assigned, given an Assigns or Assignment item 
(define (@Elt_Lvars //I)
 
 (@Make_Set (my-map @Qry_Get_Lvar_Elts (@Cs //I))))

(define (@Qry_Get_Lvar_Elts //I)
 
 (@Struct_Elts (@Get_n //I 1)))

; Given a Variable or Struct or _any_ Lvalue, return <name, field, field, ...> 
(define (@Struct_Elts_Old //I)
 
 (if (or (= (@ST //I) //T_/Variable) (= (@ST //I) //T_/Var_/Lvalue)) (list (@V //I)) (if (or (= (@ST //I) //T_/Struct) (= (@ST //I) //T_/Struct_/Lvalue)) (concat (@Struct_Elts (@Get_n //I 2)) (list (@V (@Get_n //I 1)))) (if (or (= (@ST //I) //T_/Lvalue_/Pat_/One) (= (@ST //I) //T_/Lvalue_/Pat_/Many) (= (@ST //I) //T_/Lvalue_/Pat_/Any)) (list (@V //I)) (@Struct_Elts (@Get_n //I 2))))))

; Special case for DSECTs: @[FOO].BAR --> <a, FOO, BAR> not <a, BAR> 
(define (@Struct_Elts //I)
 
 (if (or (= (@ST //I) //T_/Variable) (= (@ST //I) //T_/Var_/Lvalue) (= (@ST //I) //T_/Lvalue_/Pat_/One) (= (@ST //I) //T_/Lvalue_/Pat_/Many) (= (@ST //I) //T_/Lvalue_/Pat_/Any)) (list (@V //I)) (if (or (= (@ST //I) //T_/Struct) (= (@ST //I) //T_/Struct_/Lvalue)) (concat (@Struct_Elts (@Get_n //I 2)) (list (@V (@Get_n //I 1)))) (if (and (or (and (= (@ST //I) //T_/Aref) (= (@ST (@Get_n //I 1)) //T_/Variable)) (and (= (@ST //I) //T_/Aref_/Lvalue) (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue))) (equal? (@V (@Get_n //I 1)) /a_name)) (cons /a_name (@Struct_Elts (@Get_n (@Get_n //I 2) 1))) (if (or (= (@ST //I) //T_/Mem) (= (@ST //I) //T_/Mem_/Lvalue)) (cons /a_name (@Struct_Elts (@Get_n //I 1))) (if (@Cs? //I) (@Struct_Elts (@Get_n //I 1)) '()))))))

; ----------------------------------------------------------------------- 
; @Clobbered takes a program item and returns a set of all the variables  
; which will always be changed by execution of that item.                 
;                                                                         
; @Redefined takes a program item and returns a set of all the variables  
; which will always be redefined, and not in terms of themselves, by that 
; item.                                                                   
; ----------------------------------------------------------------------- 
(define (@Clobbered //I)
 (let ((//S/T (@ST //I))
       (//G/T (@GT //I))
       (//Clob '()))
  (cond
   ((@Cs? //I)
    (let ((//Previous (@Dtable_Get //I //Qry_/Clobbered)))
     (cond
      ((null? //Previous)
       (cond
        ((= //S/T //T_/Assign)
         (set! //Clob (list (@Lvalue_Name (@Get_n //I 1)))))
        ((= //S/T //T_/For)
         (set! //Clob (@Set_Difference (@Clobbered (@Get_n //I 4)) (list (@V (@Get_n //I 1))))))
        ((= //S/T //T_/Var)
         (set! //Clob (@Set_Difference (@Clobbered (@Get_n //I 2)) (@Lvars (@Get_n //I 1)))))
        ((= //S/T //T_/While)
         (set! //Clob '()))
        ((= //S/T //T_/Guarded)
         (set! //Clob (@Clobbered (@Get_n //I 2))))
        ((or (= //G/T //T_/Statements) (= //S/T //T_/Assignment))
         (set! //Clob (my-reduce @Set_Union (my-map @Clobbered (@Cs //I)))))
        ((or (= //G/T //T_/Expression) (= //G/T //T_/Expression))
         (set! //Clob '()))
        (#t
         (set! //Clob (my-reduce @Set_Intersect (my-map @Clobbered (@Cs //I))))))
       (@Dtable_Put //I //Qry_/Clobbered //Clob))
      (#t
       (set! //Clob (@Dtable_Value_Part //Previous)))))))
  //Clob))

(define (@Redefined //I)
 (let ((//S/T (@ST //I))
       (//G/T (@GT //I))
       (//Redf '()))
  (cond
   ((@Cs? //I)
    (let ((//Previous (@Dtable_Get //I //Qry_/Redefined)))
     (cond
      ((null? //Previous)
       (cond
        ((= //S/T //T_/Assign)
         (cond
          ((and (not-member (@Lvalue_Name (@Get_n //I 1)) (@Used (@Get_n //I 2))) (not-member (@Lvalue_Name (@Get_n //I 1)) (@Used (@Get_n //I 1))))
           (set! //Redf (list (@Lvalue_Name (@Get_n //I 1)))))))
        ((= //S/T //T_/For)
         (set! //Redf (@Set_Difference (@Redefined (@Get_n //I 4)) (list (@V (@Get_n //I 1))))))
        ((= //S/T //T_/Var)
         (set! //Redf (@Set_Difference (@Redefined (@Get_n //I 2)) (@Lvars (@Get_n //I 1)))))
        ((= //S/T //T_/While)
         (set! //Redf '()))
        ((= //S/T //T_/Guarded)
         (set! //Redf (@Redefined (@Get_n //I 2))))
        ((= //S/T //T_/Pop)
         (set! //Redef (@Variables (@Get_n //I 1))))
        ((or (= //G/T //T_/Statements) (= //S/T //T_/Assignment))
         (set! //Redf (my-reduce @Set_Union (my-map @Redefined (@Cs //I)))))
        ((or (= //G/T //T_/Expression) (= //G/T //T_/Condition))
         (set! //Redf '()))
        (#t
         (set! //Redf (my-reduce @Set_Intersect (my-map @Redefined (@Cs //I))))))
       (@Dtable_Put //I //Qry_/Redefined //Redf))
      (#t
       (set! //Redf (@Dtable_Value_Part //Previous)))))))
  //Redf))

; This query function returns the list of variables whose initial values 
; (on entry to the selected item) are used, before new values are assigned 
; to the variables. For example, in `x:=y; y:=1; z:=x' variables x and y are both 
; used and both assigned, but variable y is used before assigned, while x is assigned 
; before it is used. 
; Note that the function does not (indeed, cannot) take account of procedure 
; or function calls or action calls which may also use the values of variables. 
; See @Full_UBA for a version which takes into account procedures. 
(set! /pop_regs (@Make_Name "pop_regs"))
(define (@UBA //I)
 (let ((//S/T (@ST //I))
       (//G/T (@GT //I))
       (//Result '()))
  (cond
   ((or (= //G/T //T_/Expression) (= //G/T //T_/Condition) (= //G/T //T_/Expressions))
    ; All vars are used before assigned: 
    (set! //Result (@Qry_Exp_Vars //I)))
   ((not (@Cs? //I))
    ;The item is a leaf node, but is not a variable: 
    (set! //Result '()))
   (#t
    ;The item is not a leaf node, so we look in a table for the result. 
    (let ((//Previous (@Dtable_Get //I //Qry_/U/B/A))
          (//Comps (@Cs //I))
          (//Assigned '()))
     (cond
      ((not (null? //Previous))
       ;We can get the result from the table attached to the item.
       (set! //Result (@Dtable_Value_Part //Previous)))
      (#t
       ;The result is not in the table, so it must be calculated. 
       ;We look at each component in turn and combine the results. 
       (cond
        ((= //G/T //T_/Statements)
         (while (not (null? //Comps)) 
          (begin
           (set! //Result (union-n //Result (@Set_Difference (@UBA (car //Comps)) //Assigned)))
           ; A self-assignment does not count as clobbering the variable: 
           (set! //Assigned (union-n //Assigned (@Redefined (car //Comps))))
           (cond
            ((@Gen_Improper? (car //Comps) "Hyb")
             (set! //Comps '()))
            (#t
             (set! //Comps (cdr //Comps)))))))
        ((= //S/T //T_/Pop)
         (set! //Result (union-n //Result (@Variables (@Get_n //I 2)))))
        ((and (= //S/T //T_/A_/Proc_/Call) (equal? (@V (@Get_n //I 1)) /pop_regs))
         (set! //Result (union-n //Result (@Variables (last-1 (@Cs (@Get_n //I 3)))))))
        ((or (= //S/T //T_/Struct_/Lvalue) (= //S/T //T_/Proc_/Call) (= //S/T //T_/A_/Proc_/Call) (= //S/T //T_/Aref_/Lvalue) (= //S/T //T_/Sub_/Seg_/Lvalue) (= //S/T //T_/Rel_/Seg_/Lvalue) (= //S/T //T_/Final_/Seg_/Lvalue) (= //S/T //T_/Push))
         (set! //Result (union-n //Result (@Variables //I))))
        (#t
         (while (not (null? //Comps)) 
          (begin
           (set! //Result (union-n //Result (@UBA (car //Comps))))
           (set! //Comps (cdr //Comps))))
         ;If the current item is a VAR structure or a FOR loop 
         ;then there are local variables which we don't want to return 
         ;as part of the result; so these must be taken out. 
         (cond
          ((= //S/T //T_/Var)
           (set! //Result (@Set_Difference //Result (@Set_Difference (@Lvars (@Get_n //I 1)) (@Used (@Get_n //I 1))))))
          ((= //S/T //T_/For)
           (set! //Result (@Set_Difference //Result (list (@V (@Get_n //I 1)))))))))
       ;Store the result in the table associated with this item.
       (@Dtable_Put //I //Qry_/U/B/A //Result))))))
  //Result))

; Return the list of element lists <name, field, field, ...> which are accessed 
; before being overwritten in the given item. 
; Note that the function does not (indeed, cannot) take account of procedure 
; or function calls or action calls which may also use the values of variables. 
(define (@Elts_UBA //I)
 (let ((//S/T (@ST //I))
       (//G/T (@GT //I))
       (//Result '()))
  (cond
   ((or (= //G/T //T_/Expression) (= //G/T //T_/Condition) (= //G/T //T_/Expressions))
    (set! //Result (@Qry_Exp_Elts //I)))
   ((not (@Cs? //I))
    ;The item is a leaf node, but is not a variable or struct: 
    (set! //Result '()))
   (#t
    ;The item is not a leaf node, so we look in a table for the result. 
    (let ((//Previous (@Dtable_Get //I //Qry_/Elts_/U/B/A))
          (//Comps (@Cs //I))
          (//Assigned '()))
     (cond
      ((not (null? //Previous))
       ;We can get the result from the table attached to the item.
       (set! //Result (@Dtable_Value_Part //Previous)))
      (#t
       ;The result is not in the table, so it must be calculated. 
       ;We look at each component in turn and combine the results. 
       (cond
        ((= //G/T //T_/Statements)
         (while (not (null? //Comps)) 
          (begin
           (set! //Result (union-n //Result (@Set_Difference (@Elts_UBA (car //Comps)) //Assigned)))
           ; A self-assignment does not count as clobbering the variable: 
           (set! //Assigned (union-n //Assigned (@Elts_Redefined (car //Comps))))
           (cond
            ((@Gen_Improper? (car //Comps) "Hyb")
             (set! //Comps '()))
            (#t
             (set! //Comps (cdr //Comps)))))))
        ((= //S/T //T_/Pop)
         (set! //Result (union-n //Result (@Elements (@Get_n //I 2)))))
        ((and (= //S/T //T_/A_/Proc_/Call) (equal? (@V (@Get_n //I 1)) /pop_regs))
         (set! //Result (union-n //Result (@Elements (@Get_n //I 1)))))
        ((or (= //S/T //T_/Struct_/Lvalue) (= //S/T //T_/Proc_/Call) (= //S/T //T_/A_/Proc_/Call) (= //S/T //T_/Aref_/Lvalue) (= //S/T //T_/Sub_/Seg_/Lvalue) (= //S/T //T_/Rel_/Seg_/Lvalue) (= //S/T //T_/Final_/Seg_/Lvalue) (= //S/T //T_/Push))
         (set! //Result (union-n //Result (@Elements //I))))
        (#t
         (while (not (null? //Comps)) 
          (begin
           (set! //Result (union-n //Result (@Elts_UBA (car //Comps))))
           (set! //Comps (cdr //Comps))))
         ;If the current item is a VAR structure or a FOR loop 
         ;then there are local variables which we don't want to return 
         ;as part of the result; so these must be taken out. 
         (cond
          ((= //S/T //T_/Var)
           (set! //Result (@Set_Difference //Result (@Set_Difference (@Elt_Lvars (@Get_n //I 1)) (@Elts_Used (@Get_n //I 1))))))
          ((= //S/T //T_/For)
           (set! //Result (@Set_Difference //Result (list (list (@V (@Get_n //I 1))))))))))
       ;Store the result in the table associated with this item.
       (@Dtable_Put //I //Qry_/Elts_/U/B/A //Result))))))
  //Result))

; Rename a variable to a new name throughout the current item 
(define (@Rename /old-par /new)
 (let ((/old-save /old))
  (set! /old /old-par)
  (cond
   ((and (= (@ST (@I)) //T_/Var_/Lvalue) (equal? (@V (@I)) /old))
    (@Paste_Over (@Make //T_/Var_/Lvalue /new '())))
   ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) /old))
    (@Paste_Over (@Make //T_/Variable /new '()))))
  (cond
   ((and (@Components? (@I)) (member /old (@All_Variables (@I))))
    (@Down)
    (@Rename /old /new)
    (while (@Right?) 
     (begin
      (@Right)
      (@Rename /old /new)))
    (@Up)))
  (set! /old /old-save)))

(define (@Elements //I)
 (let ((//Result (@Qry_Elts //I)))
  (union-n (wsl-ref //Result 1) (wsl-ref //Result 2))))

(define (@Elts_Assigned //I)
 (let ((//Result (@Qry_Elts //I)))
  (wsl-ref //Result 1)))

(define (@Elts_Used //I)
 (let ((//Result (@Qry_Elts //I)))
  (wsl-ref //Result 2)))

(define (@Elts_Assd_To_Self //I)
 (let ((//Result (@Qry_Elts //I)))
  (wsl-ref //Result 3)))

; Return a list of three lists: the list of structure elements assigned to, 
; the list of structure elements used, and the list of structure elements 
; only used in assigments to themselves, in the given item. 
; An element of a structure is recorded in the form: <name, field, field, ...> 
; An array element with numeric index is recorded: <name, n> 
; A sub seg or rel seg with <= Max_Subfield known elements 
; is recorded as a list of elements. 
; Default value for Max_Subfield: 
; (This is small because an assignment of a struct from itself expands into 
; Max_Subfield + 1 assignments each with Max_Subfield + 1 references). 
(set! //Max_/Subfield 4)
(define (@Qry_Elts //I)
 (let ((//S/T (@ST //I))
       (//G/T (@GT //I))
       (//Result '()))
  (cond
   ((not (@Cs? //I))
    ;The item is a leaf node, so we know the results for that, trivially.
    (set! //Result (list (if (= //S/T //T_/Var_/Lvalue) (list (list (@V //I))) '()) (if (= //S/T //T_/Variable) (list (list (@V //I))) '()) (if (or (= //S/T //T_/Var_/Lvalue) (= //S/T //T_/Variable)) (list (list (@V //I))) '()))))
   ((and (= //S/T //T_/Struct) (@Qry_Simple_Struct? (@Get_n //I 2)))
    (set! //Result (list '() (list (@Struct_Elts //I)) '())))
   ((and (= //S/T //T_/Struct_/Lvalue) (@Qry_Simple_LStruct? (@Get_n //I 2)))
    (set! //Result (list (list (@Struct_Elts //I)) '() '())))
   ((or (= //S/T //T_/Aref) (= //S/T //T_/Aref_/Lvalue) (= //S/T //T_/Sub_/Seg) (= //S/T //T_/Sub_/Seg_/Lvalue) (= //S/T //T_/Rel_/Seg) (= //S/T //T_/Rel_/Seg_/Lvalue))
    (set! //Result (@Qry_Aref_Elts //I)))
   ((or (= //G/T //T_/Expression) (= //G/T //T_/Condition) (= //G/T //T_/Expressions))
    (set! //Result (list '() (@Qry_Exp_Elts //I) '())))
   (#t
    ;The item is not a leaf node, so we look in a table for the result. 
    (let ((//Previous (@Dtable_Get //I //Qry_/Elts))
          (//Assd '())
          (//Used '())
          (//Self '())
          (//Temp '())
          (//Comps (@Cs //I)))
     ; Check for a stored result 
     (cond
      ((not (null? //Previous))
       (set! //Result (@Dtable_Value_Part //Previous)))
      (#t
       ; The result is not stored in a table, so it must be calculated. 
       ; We look at each component in turn, calculate the variables for 
       ; that and combine these results.                                
       ; An array Lvalue uses as well as assigns to its first component 
       ; Also, VAR parameters are used as well as assigned.             
       (while (not (null? //Comps)) 
        (begin
         (cond
          ((or (= (@GT (car //Comps)) //T_/Expression) (= (@GT (car //Comps)) //T_/Condition))
           (set! //Used (union-n //Used (@Qry_Exp_Elts (car //Comps)))))
          (#t
           (set! //Temp (@Qry_Elts (car //Comps)))
           (cond
            ((= //S/T //T_/Assign)
             (set! //Self (union-n //Self (wsl-ref //Temp 3))))
            ((or (= //G/T //T_/Statement) (= //G/T //T_/Assignment) (= //G/T //T_/Variable) (= //G/T //T_/Statements) (= //G/T //T_/Guarded) (= //G/T //T_/Action))
             (set! //Self (union-n (@Set_Difference (wsl-ref //Temp 3) //Used) (@Set_Difference //Self (wsl-ref //Temp 2)))))
            (#t
             (set! //Self '())))
           (set! //Assd (union-n //Assd (wsl-ref //Temp 1)))
           (set! //Used (union-n //Used (wsl-ref //Temp 2)))))
         (set! //Comps (cdr //Comps))))
       ;If the current item is a local variable structure or a `FOR' loop 
       ;then there are local variables which we don't want to return as   
       ;part of the result; so these must be taken out.                   
       (cond
        ((= //S/T //T_/Var)
         (let ((//Local_/Vars (@Elt_Lvars (@Get_n //I 1))))
          (set! //Assd (@Set_Difference //Assd //Local_/Vars))
          (set! //Used (@Set_Difference //Used //Local_/Vars))
          (set! //Self (@Set_Difference //Self //Local_/Vars))))
        ((= //S/T //T_/For)
         (let ((//Loop_/Var (list (@V (@Get_n //I 1)))))
          (set! //Assd (@Set_Difference //Assd //Loop_/Var))
          (set! //Used (@Set_Difference //Used //Loop_/Var))
          (set! //Self (@Set_Difference //Self //Loop_/Var))))
        ((or (= //S/T //T_/Proc) (= //S/T //T_/Funct) (= //S/T //T_/B/Funct))
         (let ((//Local_/Vars (union-n (@Elts_Assigned (@Get_n //I 2)) (@Elts_Assigned (@Get_n //I 3)))))
          (set! //Assd (@Set_Difference //Assd //Local_/Vars))
          (set! //Used (@Set_Difference //Used //Local_/Vars))
          (set! //Self (@Set_Difference //Self //Local_/Vars))))
        ((or (= //S/T //T_/Print) (= //S/T //T_/Prinflush) (= //S/T //T_/Error))
         (set! //Assd (union-n //Assd (list (list /os_name))))
         (set! //Used (union-n //Used (list (list /os_name)))))
        ((= //S/T //T_/Pop)
         (set! //Used (union-n //Used (@Elts_Assigned (@Get_n //I 2))))
         (set! //Self '()))
        ((or (= //S/T //T_/Struct_/Lvalue) (= //S/T //T_/Aref_/Lvalue) (= //S/T //T_/Sub_/Seg_/Lvalue) (= //S/T //T_/Rel_/Seg_/Lvalue) (= //S/T //T_/Final_/Seg_/Lvalue) (= //S/T //T_/Proc_/Call) (= //S/T //T_/A_/Proc_/Call) (= //S/T //T_/Push) (= //S/T //T_/Mem_/Lvalue) (= //S/T //T_/Mem_/Seg_/Lvalue) (= //S/T //T_/Mem_/Rel_/Lvalue))
         (set! //Used (union-n //Used //Assd))))
       (set! //Result (list //Assd //Used //Self))
       ;Store the result in the table associated with this item.
       (@Dtable_Put //I //Qry_/Elts //Result))))))
  //Result))

; Check that the struct is a simple foo.bar.baz, not (eg) foo[bar].baz: 
; NB: @[foo].bar is a simple struct: 
(define (@Qry_Simple_Struct? //I)
 
 (or (= (@ST //I) //T_/Variable) (and (= (@ST //I) //T_/Aref) (= (@ST (@Get_n //I 1)) //T_/Variable) (equal? (@V (@Get_n //I 1)) /a_name)) (= (@ST //I) //T_/Mem) (and (= (@ST //I) //T_/Struct) (@Qry_Simple_Struct? (@Get_n //I 2)))))

(define (@Qry_Simple_LStruct? //I)
 
 (or (= (@ST //I) //T_/Var_/Lvalue) (and (= (@ST //I) //T_/Aref_/Lvalue) (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n //I 1)) /a_name)) (= (@ST //I) //T_/Mem_/Lvalue) (and (= (@ST //I) //T_/Struct_/Lvalue) (@Qry_Simple_LStruct? (@Get_n //I 2)))))

; Query elements for array references: eg A[x] := 0 uses and assigns to A, 
; while A[2] := 0 assigns to <A, 2> and doesn't use A. 
; A[2..3] := 0 assigns to <A, 2> and <A, 3>, 
; while A[2..6] := 0 uses and assigns to A (the list of individual elements is too big!) 
; For an Lvalue, R1 lists the assigned elements, while R2 lists the used only elements. 
; For an expression, nothing is assigned and R1 / R2 are the used elements. 
; In a[!XF address_of(FOO)...] variable FOO is actually used 
; even though it appears within an address_of call. 
(define (@Qry_Aref_Elts //I)
 (let ((//S/T (@ST //I))
       (//R1 (list (@Struct_Elts (@Get_n //I 1))))
       (//R2 '()))
  (cond
   ((and (null? (car //R1)) (not (null? (@Elements (@Get_n //I 1)))))
    (set! //R1 (@Elements (@Get_n //I 1))))
   ((and (or (= //S/T //T_/Aref) (= //S/T //T_/Aref_/Lvalue)) (= (@Size (@Get_n //I 2)) 1) (= (@ST (@Get_n (@Get_n //I 2) 1)) //T_/Number))
    (set! //R1 (@Qry_Add_Index1 //R1 (list (@V (@Get_n (@Get_n //I 2) 1))))))
   ((and (or (= //S/T //T_/Sub_/Seg) (= //S/T //T_/Sub_/Seg_/Lvalue)) (= (@ST (@Get_n //I 2)) //T_/Number) (= (@ST (@Get_n //I 3)) //T_/Number) (< (- (@V (@Get_n //I 3)) (@V (@Get_n //I 2))) //Max_/Subfield))
    (set! //R1 (@Qry_Add_Index1 //R1 (@Num_List (@V (@Get_n //I 2)) (@V (@Get_n //I 3))))))
   ((and (or (= //S/T //T_/Rel_/Seg) (= //S/T //T_/Rel_/Seg_/Lvalue)) (= (@ST (@Get_n //I 2)) //T_/Number) (= (@ST (@Get_n //I 3)) //T_/Number) (<= (@V (@Get_n //I 3)) //Max_/Subfield))
    (set! //R1 (@Qry_Add_Index1 //R1 (@Num_List (@V (@Get_n //I 2)) (- (+ (@V (@Get_n //I 2)) (@V (@Get_n //I 3))) 1)))))
   (#t
    (cond
     ((and (or (= (@ST (@Get_n //I 1)) //T_/Variable) (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue)) (equal? (@V (@Get_n //I 1)) /a_name))
      (set! //R1 (union-n //R1 (@Elts_Addr //I)))))
    (set! //R2 (union-n //R1 (my-reduce @Set_Union (my-map @Qry_Exp_Elts (cdr (@Components //I))))))))
  (cond
   ((or (= (@ST (@Get_n //I 1)) //T_/Aref) (= (@ST (@Get_n //I 1)) //T_/Aref_/Lvalue) (= (@ST (@Get_n //I 1)) //T_/Sub_/Seg) (= (@ST (@Get_n //I 1)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n //I 1)) //T_/Rel_/Seg) (= (@ST (@Get_n //I 1)) //T_/Rel_/Seg_/Lvalue))
    (let ((/sub (@Qry_Aref_Elts (@Get_n //I 1))))
     (set! //R1 (union-n //R1 (wsl-ref /sub 1)))
     (set! //R2 (union-n //R2 (wsl-ref /sub 2))))))
  (if (= (@GT //I) //T_/Lvalue) (list //R1 //R2 '()) (list '() (union-n //R1 //R2) '()))))

; Add each element of the list of index numbers to each component of each result 
; to create a new result, eg: << >, <<x>>, < >> plus <1, 2> becomes 
; << >, <<x, -1>, <x, -2>>, < >> 
(define (@Qry_Add_Index //R /nums)
 (let ((/new_/R '())
       (/new '())
       (/elt-save /elt)
       (/num '())
       (funct-result '()))
  (set! /elt '())
  ; Ensure that there is at least one element: 
  (cond
   ((null? /nums)
    (set! /nums (list 0))))
  (for-in /old //R 
   (begin
    (set! /new '())
    (for-in /elt /old 
     (for-in /num /nums 
      (cond
       ((equal? /elt (list /a_name))
        (set! /new (cons /elt /new)))
       (#t
        (set! /new (cons (concat /elt (list (- /num))) /new))))))
    (set! /new_/R (cons /new /new_/R))))
  (set! funct-result (@Make_Set /new_/R))
  (set! /elt /elt-save)
  funct-result))

(define (@Qry_Add_Index1 /old-par /nums)
 (let ((/old-save /old)
       (/new '())
       (/elt-save /elt)
       (/num '())
       (funct-result '()))
  (set! /old /old-par)
  (set! /elt '())
  ; Ensure that there is at least one element: 
  (cond
   ((null? /nums)
    (set! /nums (list 0))))
  (for-in /elt /old 
   (for-in /num /nums 
    (cond
     ((equal? /elt (list /a_name))
      (set! /new (cons /elt /new)))
     (#t
      (set! /new (cons (concat /elt (list (- /num))) /new))))))
  (set! funct-result (@Make_Set /new))
  (set! /old /old-save)
  (set! /elt /elt-save)
  funct-result))

; Return the list of numbers: <n, n+1, n+2, ..., m> 
(define (@Num_List /n-par /m)
 (let ((/n-save /n)
       (//R '())
       (funct-result '()))
  (set! /n /n-par)
  (while (<= /n /m) 
   (begin
    (set! //R (cons /n //R))
    (set! /n (+ /n 1))))
  (set! funct-result (reverse //R))
  (set! /n /n-save)
  funct-result))

; Return the list of element structures appearing in the expression or condition 
(define (@Qry_Exp_Elts //I)
 (let ((//Result '())
       (//S/T (@ST //I)))
  (cond
   ((@Is_Addr? //I)
    (set! //Result (@Qry_Exp_Elts_A (@Get_Addr //I))))
   ((and (= //S/T //T_/Struct) (@Qry_Simple_Struct? (@Get_n //I 2)))
    (set! //Result (list (@Struct_Elts //I))))
   ((and (= //S/T //T_/Struct_/Lvalue) (@Qry_Simple_LStruct? (@Get_n //I 2)))
    (set! //Result (list (@Struct_Elts //I))))
   ((and (or (= //S/T //T_/Aref) (= //S/T //T_/Aref_/Lvalue)) (= (@Size (@Get_n //I 2)) 1) (= (@ST (@Get_n (@Get_n //I 2) 1)) //T_/Number))
    (set! //Result (@Qry_Add_Index1 (@Qry_Exp_Elts (@Get_n //I 1)) (list (@V (@Get_n (@Get_n //I 2) 1))))))
   ((and (or (= //S/T //T_/Sub_/Seg) (= //S/T //T_/Sub_/Seg_/Lvalue)) (= (@ST (@Get_n //I 2)) //T_/Number) (= (@ST (@Get_n //I 3)) //T_/Number) (< (- (@V (@Get_n //I 3)) (@V (@Get_n //I 2))) //Max_/Subfield))
    (set! //Result (@Qry_Add_Index1 (@Qry_Exp_Elts (@Get_n //I 1)) (@Num_List (@V (@Get_n //I 2)) (@V (@Get_n //I 3))))))
   ((and (or (= //S/T //T_/Rel_/Seg) (= //S/T //T_/Rel_/Seg_/Lvalue)) (= (@ST (@Get_n //I 2)) //T_/Number) (= (@ST (@Get_n //I 3)) //T_/Number) (<= (@V (@Get_n //I 3)) //Max_/Subfield))
    (set! //Result (@Qry_Add_Index1 (@Qry_Exp_Elts (@Get_n //I 1)) (@Num_List (@V (@Get_n //I 2)) (- (+ (@V (@Get_n //I 3)) (@V (@Get_n //I 2))) 1)))))
   ((or (= //S/T //T_/Rel_/Seg) (= //S/T //T_/Sub_/Seg) (= //S/T //T_/Aref))
    (set! //Result (union-n (my-reduce @Set_Union (my-map @Qry_Exp_Elts (@Components //I))) (@Elts_Addr //I))))
   ((@Cs? //I)
    (set! //Result (my-reduce @Set_Union (my-map @Qry_Exp_Elts (@Components //I)))))
   ((or (= //S/T //T_/Variable) (= //S/T //T_/Var_/Lvalue))
    (set! //Result (list (list (@V //I))))))
  //Result))

; !XF address_of(foo) does not use value of foo, 
; but !XF address_of(foo[bar]) *does* depend on the value of bar! 
(define (@Qry_Exp_Elts_A //I)
 
 (if (member (@ST //I) (list //T_/Aref //T_/Sub_/Seg //T_/Rel_/Seg //T_/Final_/Seg)) (union-n (@Qry_Exp_Elts_A (@Get_n //I 1)) (my-reduce @Set_Union (my-map @Qry_Exp_Elts (cdr (@Cs //I))))) (if (@Cs? //I) (my-reduce @Set_Union (my-map @Qry_Exp_Elts_A (@Cs //I))) '())))

(define (@Elts_Redefined //I)
 (let ((//S/T (@ST //I))
       (//G/T (@GT //I))
       (//Redef-save //Redef)
       (//L '())
       (//Cs '())
       (funct-result '()))
  (set! //Redef '())
  (cond
   ((@Cs? //I)
    (let ((//Previous (@Dtable_Get //I //Qry_/Elts_/Redefined)))
     (cond
      ((null? //Previous)
       (cond
        ((= //S/T //T_/Assign)
         (let ((//A1 (@Elts_Assigned (@Get_n //I 1))))
          (cond
           ((and (not (@Elt_Clash_List? (@Elts_Used (@Get_n //I 2)) //A1)) (not (@Elt_Clash_List? (@Elts_Used (@Get_n //I 1)) //A1)))
            (set! //Redef //A1)))))
        ((= //S/T //T_/For)
         (set! //Redef (@Elt_Remove (@Elts_Redefined (@Get_n //I 5)) (list (@V (@Get_n //I 1))))))
        ((= //S/T //T_/Var)
         (set! //Redef (@Elt_Subtract (@Elts_Redefined (@Get_n //I 2)) (@Elt_Lvars (@Get_n //I 1)))))
        ((= //S/T //T_/While)
         (set! //Redef '()))
        ((= //S/T //T_/Floop)
         ; Anything redefined before an EXIT is redefined in the floop 
         (set! //L '())
         (set! //Cs (@Cs (@Get_n //I 1)))
         (while (not (null? //Cs)) 
          (cond
           ((@Gen_Proper? (car //Cs) "Hyb")
            (set! //L (cons (car //Cs) //L))
            (set! //Cs (cdr //Cs)))
           (#t
            (set! //Cs '()))))
         (cond
          ((null? //L)
           (set! //Redef '()))
          (#t
           (set! //Redef (my-reduce @Set_Union (my-map @Elts_Redefined (reverse //L)))))))
        ((= //S/T //T_/Guarded)
         (set! //Redef (@Elts_Redefined (@Get_n //I 2))))
        ((= //S/T //T_/Pop)
         (set! //Redef (@Elements (@Get_n //I 1))))
        ((or (= //G/T //T_/Statements) (= //S/T //T_/Assignment))
         (set! //Redef (my-reduce @Set_Union (my-map @Elts_Redefined (@Cs //I)))))
        ((or (= //G/T //T_/Expression) (= //G/T //T_/Condition))
         (set! //Redef '()))
        (#t
         (set! //Redef (my-reduce @Set_Intersect (my-map @Elts_Redefined (@Cs //I))))))
       (@Dtable_Put //I //Qry_/Elts_/Redefined //Redef))
      (#t
       (set! //Redef (@Dtable_Value_Part //Previous)))))))
  (set! funct-result //Redef)
  (set! //Redef //Redef-save)
  funct-result))

; Utility functions for processing lists of elements. 
; NOTE: Removing foo also removes all its field references (foo.bar etc.) 
; NOTE: If foo.bar is in the list, and foo is referenced, 
; then this must be treated as a reference to foo.bar also. 
; Remove an element (and all its components) from a list: 
; (Removing foo also removes foo.bar). 
; This uses the fact that V is sorted and a prefix of a list is ordered 
; before the list itself: 
(define (@Elt_Remove //V /elt)
 
 (if (null? //V) '() (if (@Prefix? /elt (car //V)) (@Elt_Remove (cdr //V) /elt) (if (@Gen_Less? /elt (car //V)) //V (cons (car //V) (@Elt_Remove (cdr //V) /elt))))))

; Subtract one element list from another, using the fact that they are sorted: 
(define (@Elt_Subtract //V //W)
 
 (if (null? //V) '() (if (null? //W) //V (if (@Prefix? (car //W) (car //V)) (@Elt_Subtract (cdr //V) //W) (if (@Gen_Less? (car //W) (car //V)) (@Elt_Subtract //V (cdr //W)) (cons (car //V) (@Elt_Subtract (cdr //V) //W)))))))

; Given a sorted element list, remove fields where the struct is also present. 
; This uses the fact that a prefix of a list is ordered before the list itself. 
(define (@Elt_Remove_Fields //V)
 
 (if (null? //V) //V (if (null? (cdr //V)) //V (if (@Prefix? (car //V) (car (cdr //V))) (@Elt_Remove_Fields (cons (car //V) (cdr (cdr //V)))) (cons (car //V) (@Elt_Remove_Fields (cdr //V)))))))

; Convert an element list to an item (ie the opposite of @Struct_Elts) 
(define (@Elt_To_Expn //V)
 (let ((//R (@Make //T_/Variable (car //V) '())))
  (cond
   ((and (equal? (car //V) /a_name) (not (null? (cdr //V))))
    ; Return @[FOO] 
    (set! //V (cdr //V))
    (set! //R (@Make_Mem (@Make //T_/Variable (car //V) '())))))
  (set! //V (cdr //V))
  (while (and (not (null? //V)) (> (car //V) 0)) 
   (begin
    (set! //R (@Make //T_/Struct '() (list (@Name (car //V)) //R)))
    (set! //V (cdr //V))))
  //R))

; Convert an element list to an Lvalue 
(define (@Elt_To_Lvalue //V)
 (let ((//R (@Make //T_/Var_/Lvalue (car //V) '())))
  (cond
   ((and (equal? (car //V) /a_name) (not (null? (cdr //V))))
    ; Return a[FOO] 
    (set! //V (cdr //V))
    (set! //R (@Make_Mem_L (@Make //T_/Variable (car //V) '())))))
  (set! //V (cdr //V))
  (while (and (not (null? //V)) (> (car //V) 0)) 
   (begin
    (set! //R (@Make //T_/Struct_/Lvalue '() (list (@Name (car //V)) //R)))
    (set! //V (cdr //V))))
  //R))

(define (@Elt_To_String /elt-par)
 (let ((/elt-save /elt)
       (/item '())
       (//R '())
       (funct-result '()))
  (set! /elt /elt-par)
  (cond
   ((sequence? /elt)
    ; Don't print the a. for a DSECT reference: 
    (cond
     ((and (> (gen-length /elt) 1) (equal? (car /elt) /a_name) (> (wsl-ref /elt 2) 0))
      (set! /elt (cdr /elt))))
    (set! //R (@Name_Or_Num_String (car /elt)))
    (for-in /item (cdr /elt) 
     (cond
      ((<= /item 0)
       (set! //R (string-append (concat (string-append //R "[") (@String (- /item))) "]")))
      (#t
       (set! //R (concat (string-append //R ".") (@Name_Or_Num_String /item)))))))
   (#t
    (set! //R (@Name_Or_Num_String /elt))))
  (set! funct-result //R)
  (set! /elt /elt-save)
  funct-result))

(define (@Name_Or_Num_String /n)
 
 (if (and (number? /n) (<= /n 0)) (@String (- /n)) (@N_String /n)))

; Element set handling routines: 
(define (@Elt_Clash? //V /e)
 (let ((//R 0))
  (while (not (null? //V)) 
   (cond
    ((and #f (equal? (car //V) (list /a_name)))
     (set! //V (cdr //V)))
    ((or (@Either_Prefix? (car //V) /e) (@DSECT_Clash? (car //V) /e))
     (set! //R 1)
     (set! //V '()))
    (#t
     (set! //V (cdr //V)))))
  (= //R 1)))

; This is equivalent to @Prefix?(A, B) OR @Prefix(B, A): 
(define (@Either_Prefix? //A //B)
 
 (or (null? //A) (null? //B) (and (not (null? //A)) (not (null? //B)) (equal? (car //A) (car //B)) (@Either_Prefix? (cdr //A) (cdr //B)))))

; Check if one element is a DSECT reference and the other is the DSECT pointer: 
(define (@DSECT_Clash? //A //B)
 
 (or (and (= (gen-length //A) 1) (> (gen-length //B) 1) (equal? (wsl-ref //B 1) /a_name) (equal? (wsl-ref //A 1) (wsl-ref //B 2))) (and (= (gen-length //B) 1) (> (gen-length //A) 1) (equal? (wsl-ref //A 1) /a_name) (equal? (wsl-ref //B 1) (wsl-ref //A 2)))))

; Check if any element in the second list is in the first list, 
; or is a prefix or extension of an element in the first list: 
; Also check for DSECT clashes. 
; Strip any a's from the front of elements before checking, 
; and make sure the result is sorted. 
; Special case: If V contains a DSECT pointer, then references to the DSECT elements 
; in W are *not* clashes 
(define (@Elt_Clash_List? //V //W)
 (let ((/found 0))
  (cond
   ((and (= (gen-length //V) 1) (= (gen-length (car //V)) 1))
    (set! //W (@Elt_Strip_DSECT (car (car //V)) //W))))
  (set! //V (@Elt_Strip_a //V))
  (set! //W (@Elt_Strip_a //W))
  (while (and (not (null? //V)) (not (null? //W))) 
   (cond
    ((@Either_Prefix? (car //W) (car //V))
     (set! /found 1)
     (set! //V '()))
    ((@Gen_Less? (car //W) (car //V))
     (set! //W (cdr //W)))
    (#t
     (set! //V (cdr //V)))))
  (= /found 1)))

(define (@Elt_Strip_a //V)
 (let ((//R '()))
  (for-in /elt //V 
   (cond
    ((and (not (null? /elt)) (equal? (wsl-ref /elt 1) /a_name) (> (gen-length /elt) 1))
     (set! //R (cons (cdr /elt) //R)))
    (#t
     (set! //R (cons /elt //R)))))
  (@Make_Set //R)))

(define (@Elt_Strip_DSECT /v //W)
 (let ((//R '()))
  (for-in /elt //W 
   (cond
    ((and (> (gen-length /elt) 2) (equal? (wsl-ref /elt 1) /a_name) (equal? (wsl-ref /elt 2) /v))
     #t)
    (#t
     (set! //R (cons /elt //R)))))
  (reverse //R)))

; ----------------------------------------------------------------------- 
; The following function returns information about the actions calls.     
; The information takes the form of a list of pairs in which each pair    
; contains the name of an action followed by a number indicating how      
; often that action is called.  The function does not look inside nested  
; action systems.                                                         
; ----------------------------------------------------------------------- 
(define (@Calls //I)
 (let ((//Result '()))
  (cond
   ((@Cs? //I)
    (let ((//Previous (@Dtable_Get //I //Qry_/Calls))
          (/comp-save /comp))
     (set! /comp '())
     (cond
      ((null? //Previous)
       ;If the result has not been stored in a table already, we   
       ;have to calculate it by looking at each component in turn. 
       (for-in /comp (@Cs //I) 
        (cond
         ((not (or (= (@ST /comp) //T_/A_/S) (= (@GT /comp) //T_/Expression) (= (@GT /comp) //T_/Condition)))
          ;If the current item is a whole action system or in an  
          ;expression or condition, then we needn't look for more 
          ;`CALL's inside it.                                     
          (set! //Result (@Qry_Call_Join //Result (@Calls /comp))))))
       (@Dtable_Put //I //Qry_/Calls //Result))
      (#t
       (set! //Result (@Dtable_Value_Part //Previous))))
     (set! /comp /comp-save)))
   ((= (@ST //I) //T_/Call)
    (set! //Result (list (list (@V //I) 1)))))
  //Result))

; ----------------------------------------------------------------------- 
; The following two functions return more information about action calls. 
; ----------------------------------------------------------------------- 
(define (@Called? //Name //I)
 
 (not (null? (@Assoc //Name (@Calls //I)))))

(define (@Call_Freq //Name //I)
 (let ((//Result (@Assoc //Name (@Calls //I))))
  (cond
   ((null? //Result)
    (set! //Result 0))
   (#t
    (set! //Result (car (cdr //Result)))))
  //Result))

; ----------------------------------------------------------------------- 
; @Proc_Calls returns a list of pairs: <name, no-of-calls>                
; ----------------------------------------------------------------------- 
; MW 13/03/95: BUGFIX -- ignore local calls 
(define (@Proc_Calls //I)
 (let ((//Result '()))
  (cond
   ((@Cs? //I)
    (let ((//Previous (@Dtable_Get //I //Qry_/Proc_/Calls))
          (//Temp (@Cs //I)))
     (cond
      ((not (null? //Previous))
       (set! //Result (@Dtable_Value_Part //Previous)))
      ((= (@ST //I) //T_/Proc_/Call)
       (set! //Result (list (list (@V (@Get_n //I 1)) 1))))
      (#t
       ; If the result has not been stored in a table already, we   
       ; have to calculate it by looking at each component in turn, 
       ; then remove any local procs (if we are on a WHERE clause). 
       (while (not (null? //Temp)) 
        (begin
         (cond
          ((and (not (= (@GT (car //Temp)) //T_/Expression)) (not (= (@GT (car //Temp)) //T_/Condition)) (not (= (@GT (car //Temp)) //T_/Expressions)))
           ;If the current item is an expression(s) or condition(s), 
           ;then we needn't look for more calls inside it. 
           (set! //Result (@Qry_Call_Join //Result (@Proc_Calls (car //Temp))))))
         (set! //Temp (cdr //Temp))))
       (cond
        ((= (@ST //I) //T_/Where)
         (set! //Temp (@Cs (@Get_n //I 2)))
         (while (not (null? //Temp)) 
          (begin
           (cond
            ((= (@ST (car //Temp)) //T_/Proc)
             (set! //Result (@Qry_Remove_A_Call //Result (@V (@Get_n (car //Temp) 1))))))
           (set! //Temp (cdr //Temp))))))
       (@Dtable_Put //I //Qry_/Proc_/Calls //Result))))))
  //Result))

(define (@A_Proc_Calls //I)
 (let ((//Result '()))
  (cond
   ((@Cs? //I)
    (let ((//Previous (@Dtable_Get //I //Qry_/A_/Proc_/Calls))
          (//Temp (@Cs //I)))
     (cond
      ((not (null? //Previous))
       (set! //Result (@Dtable_Value_Part //Previous)))
      ((= (@ST //I) //T_/A_/Proc_/Call)
       (set! //Result (list (list (@V (@Get_n //I 1)) 1))))
      (#t
       ; If the result has not been stored in a table already, we   
       ; have to calculate it by looking at each component in turn, 
       ; then remove any local procs (if we are on a WHERE clause). 
       (while (not (null? //Temp)) 
        (begin
         (cond
          ((and (not (= (@GT (car //Temp)) //T_/Expression)) (not (= (@GT (car //Temp)) //T_/Condition)) (not (= (@GT (car //Temp)) //T_/Expressions)))
           ;If the current item is an expression(s) or condition(s), 
           ;then we needn't look for more calls inside it. 
           (set! //Result (@Qry_Call_Join //Result (@A_Proc_Calls (car //Temp))))))
         (set! //Temp (cdr //Temp))))
       (cond
        ((= (@ST //I) //T_/Where)
         (set! //Temp (@Cs (@Get_n //I 2)))
         (while (not (null? //Temp)) 
          (begin
           (cond
            ((= (@ST (car //Temp)) //T_/Proc)
             (set! //Result (@Qry_Remove_A_Call //Result (@V (@Get_n (car //Temp) 1))))))
           (set! //Temp (cdr //Temp))))))
       (@Dtable_Put //I //Qry_/A_/Proc_/Calls //Result))))))
  //Result))

; ----------------------------------------------------------------------- 
; The following functions return more information about procedure calls.  
; ----------------------------------------------------------------------- 
(define (@Proc_Called? //Name //I)
 
 (not (null? (@Assoc //Name (@Proc_Calls //I)))))

(define (@Proc_Call_Freq //Name //I)
 (let ((//Result (@Assoc //Name (@Proc_Calls //I))))
  (cond
   ((null? //Result)
    (set! //Result 0))
   (#t
    (set! //Result (car (cdr //Result)))))
  //Result))

; ----------------------------------------------------------------------- 
; Return the number of times a procedure is called in the body of the given 
; WHERE clause, plus the bodies of all other procedures in the WHERE 
; ----------------------------------------------------------------------- 
(define (@Non_Recursive_Calls //Name //I)
 (let ((//Result (@Proc_Call_Freq //Name (@Get_n //I 1)))
       (/bodies (@Cs (@Get_n //I 2))))
  (while (not (null? /bodies)) 
   (begin
    (cond
     ((and (= (@ST (car /bodies)) //T_/Proc) (not (equal? //Name (@V (@Get_n (car /bodies) 1)))))
      (set! //Result (+ //Result (@Proc_Call_Freq //Name (@Get_n (car /bodies) 4))))))
    (set! /bodies (cdr /bodies))))
  //Result))

; ----------------------------------------------------------------------- 
; @Funct_Calls returns a list of pairs: <name, no-of-calls>               
; ----------------------------------------------------------------------- 
(define (@Funct_Calls //I)
 (let ((//Result '()))
  (cond
   ((@Cs? //I)
    (let ((//Previous (@Dtable_Get //I //Qry_/Funct_/Calls))
          (//Temp (@Cs //I)))
     (cond
      ((not (null? //Previous))
       (set! //Result (@Dtable_Value_Part //Previous)))
      ((or (= (@ST //I) //T_/Funct_/Call) (= (@ST //I) //T_/B/Funct_/Call))
       (set! //Result (list (list (@V (@Get_n //I 1)) 1)))
       (@Dtable_Put //I //Qry_/Funct_/Calls //Result))
      (#t
       ;If the result has not been stored in a table already, we   
       ;have to calculate it by looking at each component in turn. 
       (while (not (null? //Temp)) 
        (begin
         (set! //Result (@Qry_Call_Join //Result (@Funct_Calls (car //Temp))))
         (set! //Temp (cdr //Temp))))
       (@Dtable_Put //I //Qry_/Funct_/Calls //Result))))))
  //Result))

(define (@Funct_Called? //Name //I)
 
 (not (null? (@Assoc //Name (@Funct_Calls //I)))))

(define (@Funct_Call_Freq //Name //I)
 (let ((//Result (@Assoc //Name (@Funct_Calls //I))))
  (cond
   ((null? //Result)
    (set! //Result 0))
   (#t
    (set! //Result (car (cdr //Result)))))
  //Result))

(define (@X_Funct_Calls //I)
 (let ((//Result '()))
  (cond
   ((@Cs? //I)
    (let ((//Previous (@Dtable_Get //I //Qry_/X_/Funct_/Calls))
          (//Temp (@Cs //I)))
     (cond
      ((not (null? //Previous))
       (set! //Result (@Dtable_Value_Part //Previous)))
      ((or (= (@ST //I) //T_/X_/Funct_/Call) (= (@ST //I) //T_/X_/B/Funct_/Call))
       (set! //Result (list (list (@V (@Get_n //I 1)) 1)))
       (@Dtable_Put //I //Qry_/X_/Funct_/Calls //Result))
      (#t
       ;If the result has not been stored in a table already, we   
       ;have to calculate it by looking at each component in turn. 
       (while (not (null? //Temp)) 
        (begin
         (set! //Result (@Qry_Call_Join //Result (@X_Funct_Calls (car //Temp))))
         (set! //Temp (cdr //Temp))))
       (@Dtable_Put //I //Qry_/X_/Funct_/Calls //Result))))))
  //Result))

(define (@X_Funct_Called? //Name //I)
 
 (not (null? (@Assoc //Name (@X_Funct_Calls //I)))))

(define (@X_Funct_Call_Freq //Name //I)
 (let ((//Result (@Assoc //Name (@X_Funct_Calls //I))))
  (cond
   ((null? //Result)
    (set! //Result 0))
   (#t
    (set! //Result (car (cdr //Result)))))
  //Result))

(set! /dec_conds (my-map @Make_Name (list "dec_eq" "dec_not_eq" "dec_greater" "dec_greater_eq" "dec_less" "dec_less_eq")))
(define (@Unsafe_Test? //I)
 (let ((/found 0))
  (cond
   ((and (= (@ST //I) //T_/X_/B/Funct_/Call) (member (@V (@Get_n //I 1)) /dec_conds))
    (set! /found 1))
   ((or (= (@ST //I) //T_/And) (= (@ST //I) //T_/Or) (= (@ST //I) //T_/Not))
    ; TODO: Maybe define as a query function 
    (for-in /comp (@Cs //I) 
     (cond
      ((@Unsafe_Test? /comp)
       (set! /found 1))))))
  (= /found 1)))

; ----------------------------------------------------------------------- 
; Return the number of times a procedure is called in the body of the given 
; WHERE clause, plus the bodies of all other procedures in the WHERE 
; ----------------------------------------------------------------------- 
(define (@Non_Recursive_Funct_Calls //Name //I)
 (let ((//Result (@Funct_Call_Freq //Name (@Get_n //I 1)))
       (/bodies (@Cs (@Get_n //I 2))))
  (while (not (null? /bodies)) 
   (begin
    (cond
     ((not (equal? //Name (@V (@Get_n (car /bodies) 1))))
      (set! //Result (+ //Result (@Funct_Call_Freq //Name (car /bodies))))))
    (set! /bodies (cdr /bodies))))
  //Result))

; ----------------------------------------------------------------------- 
; The following functions are used to join two sets of `calls'-style      
; information.  For example,                                              
;                                                                         
;    @Qry_Call_Join (<<A, 1>, <B, 2>>,  <<B, 1>, <C, 1>>)                 
;                                                                         
; would return the list                                                   
;                                                                         
;    <<A, 1>, <B, 3>, <C, 1>>.                                            
; ----------------------------------------------------------------------- 
; It should be more efficient to merge sorted lists: 
(define (@Qry_Call_Join //L1 //L2)
 
 (if (null? //L1) //L2 (if (null? //L2) //L1 (if (equal? (car (car //L1)) (car (car //L2))) (cons (list (car (car //L1)) (+ (car (cdr (car //L1))) (car (cdr (car //L2))))) (@Qry_Call_Join (cdr //L1) (cdr //L2))) (if (< (car (car //L1)) (car (car //L2))) (cons (car //L1) (@Qry_Call_Join (cdr //L1) //L2)) (cons (car //L2) (@Qry_Call_Join //L1 (cdr //L2))))))))

(define (@Qry_Remove_A_Call //L /name)
 
 (if (null? //L) '() (if (equal? /name (car (car //L))) (cdr //L) (cons (car //L) (@Qry_Remove_A_Call (cdr //L) /name)))))

; ----------------------------------------------------------------------- 
; The following function returns `TRUE' if and only if the given item is  
; a simple statement; ie a statement that cannot be terminated by an      
; `EXIT'.                                                                 
; ----------------------------------------------------------------------- 
(define (@Simple? //I)
 
 (and (= (@GT //I) //T_/Statement) (and (not (= (@ST //I) //T_/Cond)) (not (= (@ST //I) //T_/D_/If)) (not (= (@ST //I) //T_/Floop)) (not (= (@ST //I) //T_/Join)))))

; ----------------------------------------------------------------------- 
;The function `@Terminal_n?' returns true if and only if the nth          
;component of I is a terminal position of I.                              
; ----------------------------------------------------------------------- 
(define (@Terminal_n? //I /n)
 
 (and (<= 1 /n) (<= /n (@Size //I)) (or (or (= (@ST //I) //T_/Cond) (= (@ST //I) //T_/D_/If) (= (@ST //I) //T_/Join)) (and (not (= (@ST //I) //T_/Floop)) (= /n (@Size //I))))))

; ----------------------------------------------------------------------- 
; The following function returns `TRUE' if and only if the component at   
; the *relative* position p is in a terminal position.                    
;                                                                         
; A general component of an item is in a terminal position if every step  
; on the path to that component is terminal.                              
; ----------------------------------------------------------------------- 
(define (@Terminal_Posn? //I /p)
 
 (or (null? /p) (and (@Terminal_n? //I (car /p)) (@Terminal_Posn? (@Get_n //I (car /p)) (cdr /p)))))

; ----------------------------------------------------------------------- 
; The function `@Outermost_Floop_Terminal?' returns `TRUE' if and only if 
; the outermost `FLOOP' containing the given position is in a terminal    
; position.                                                               
; ----------------------------------------------------------------------- 
(define (@Outermost_Floop_Terminal? //I /p)
 (let ((//O/K 0)
       (//Temp_/I //I)
       (//Temp_/P '())
       (//Old_/P /p)
       (//Term_/Posn 1))
  (while (and (= //O/K 0) (not (null? //Old_/P))) 
   (cond
    ((= (@ST //Temp_/I) //T_/Floop)
     (set! //O/K //Term_/Posn))
    (#t
     (cond
      ((and (= //Term_/Posn 1) (not (@Terminal_n? //Temp_/I (car //Old_/P))))
       (set! //Term_/Posn (- 1))))
     (set! //Temp_/I (@Get_n //Temp_/I (car //Old_/P)))
     (set! //Temp_/P (concat //Temp_/P (list (car //Old_/P))))
     (set! //Old_/P (cdr //Old_/P)))))
  (= //O/K 1)))

; ----------------------------------------------------------------------- 
; The following function returns the depth within the item of that item   
; with the given *relative* position.                                     
; ----------------------------------------------------------------------- 
(define (@Depth //I /p)
 (let ((//Result 0)
       (//Temp_/I //I)
       (//Old_/P /p))
  (while (not (null? //Old_/P)) 
   (begin
    (cond
     ((= (@ST //Temp_/I) //T_/Floop)
      (set! //Result (+ //Result 1))))
    (set! //Temp_/I (@Get_n //Temp_/I (car //Old_/P)))
    (set! //Old_/P (cdr //Old_/P))))
  //Result))

; ----------------------------------------------------------------------- 
; The following function returns the maximum position depth of            
; any of its component simple statements. (NOT the loop nesting depth)    
; Any simple statement has Max_Depth = 1                                  
; (This is used to ignore cases where S1 is a subcomponent of S2)         
; ----------------------------------------------------------------------- 
(define (@Max_Depth //I)
 (let ((//Result 0)
       (//Previous '()))
  (cond
   ((or (not (@Cs? //I)) (= (@GT //I) //T_/Expression) (= (@GT //I) //T_/Expressions) (= (@GT //I) //T_/Condition))
    #t)
   ((@Simple? //I)
    (set! //Result 1))
   (#t
    (set! //Previous (@Dtable_Get //I //Qry_/Max_/Dep))
    (cond
     ((not (null? //Previous))
      (set! //Result (@Dtable_Value_Part //Previous)))
     (#t
      (set! //Result (+ 1 (my-reduce MAX (my-map @Max_Depth (@Cs //I)))))
      (@Dtable_Put //I //Qry_/Max_/Dep //Result)))))
  //Result))

; ----------------------------------------------------------------------- 
; The next three functions return the terminal values of all reachable    
; components of the given item.  (For statements sequences, the terminal  
; values of each statement are only included provided all the previous    
; statements allow zero as a terminal value.)                             
;                                                                         
; For more information on these functions, see Martin's document:         
; `GREET: Requirements and High Level Design'.                            
; ----------------------------------------------------------------------- 
(define (@Rec_TVs //I)
 (let ((//Temp '())
       (//G (@GT //I))
       (//S (@ST //I))
       (//Result (@Dtable_Get //I //Qry_/Rec_/T/Vs)))
  (cond
   ((null? //Result)
    (cond
     ((or (= //G //T_/Condition) (= //G //T_/Expression) (= //G //T_/Expressions))
      (set! //Result '()))
     ((= //S //T_/Abort)
      (set! //Result (list 0)))
     ((= //S //T_/Exit)
      (set! //Result (list (@V //I))))
     ((@Simple? //I)
      (set! //Result (list 0)))
     ((= //S //T_/Statements)
      (set! //Result (my-reduce @TV_Conditional_Union (my-map @Rec_TVs (@Components //I)))))
     ((= //S //T_/Floop)
      (set! //Temp (my-reduce @TV_Conditional_Union (my-map @Rec_TVs (@Components //I))))
      (while (not (null? //Temp)) 
       (begin
        (cond
         ((> (car //Temp) 0)
          (set! //Result (concat //Result (list (- (car //Temp) 1))))))
        (set! //Temp (cdr //Temp)))))
     (#t
      (set! //Result (my-reduce @Set_Union (my-map @Rec_TVs (@Components //I))))))
    (@Dtable_Put //I //Qry_/Rec_/T/Vs //Result))
   (#t
    (set! //Result (@Dtable_Value_Part //Result))))
  //Result))

; Omega is the largest simple integer: 
(set! //Omega 536870911)
(define (@Reg_TVs //I)
 (let ((//Temp '())
       (//G (@GT //I))
       (//S (@ST //I))
       (//Result (@Dtable_Get //I //Qry_/Reg_/T/Vs)))
  (cond
   ((null? //Result)
    (cond
     ((or (= //G //T_/Condition) (= //G //T_/Expression) (= //G //T_/Expressions))
      (set! //Result '()))
     ((= //S //T_/Abort)
      (set! //Result (list 0)))
     ((= //S //T_/Exit)
      (set! //Result (list (@V //I))))
     ((= //S //T_/Call)
      (set! //Result (list //Omega)))
     ((@Simple? //I)
      (set! //Result (list 0)))
     ((= //S //T_/Statements)
      (set! //Result (my-reduce @TV_Conditional_Union (my-map @Reg_TVs (@Components //I)))))
     ((= //S //T_/Floop)
      (set! //Temp (my-reduce @TV_Conditional_Union (my-map @Reg_TVs (@Components //I))))
      (while (not (null? //Temp)) 
       (begin
        (cond
         ((equal? (car //Temp) //Omega)
          (set! //Result (concat //Result (list (car //Temp)))))
         ((> (car //Temp) 0)
          (set! //Result (concat //Result (list (- (car //Temp) 1))))))
        (set! //Temp (cdr //Temp)))))
     (#t
      (set! //Result (my-reduce @Set_Union (my-map @Reg_TVs (@Components //I))))))
    (@Dtable_Put //I //Qry_/Reg_/T/Vs //Result))
   (#t
    (set! //Result (@Dtable_Value_Part //Result))))
  //Result))

(define (@Hyb_TVs //I)
 (let ((//Temp '())
       (//G (@GT //I))
       (//S (@ST //I))
       (//Result (@Dtable_Get //I //Qry_/Hyb_/T/Vs)))
  (cond
   ((null? //Result)
    (cond
     ((or (= //G //T_/Condition) (= //G //T_/Expression) (= //G //T_/Expressions))
      (set! //Result '()))
     ((= //S //T_/Abort)
      (set! //Result (list 0)))
     ((= //S //T_/Exit)
      (set! //Result (list (@V //I))))
     ((= //S //T_/Call)
      (cond
       ((equal? (@V //I) (@Make_Name "Z"))
        (set! //Result (list //Omega)))
       (#t
        (set! //Result (list 0 //Omega)))))
     ((@Simple? //I)
      (set! //Result (list 0)))
     ((= //S //T_/Statements)
      (set! //Result (my-reduce @TV_Conditional_Union (my-map @Hyb_TVs (@Components //I)))))
     ((= //S //T_/Floop)
      (set! //Temp (my-reduce @TV_Conditional_Union (my-map @Hyb_TVs (@Components //I))))
      (while (not (null? //Temp)) 
       (begin
        (cond
         ((equal? (car //Temp) //Omega)
          (set! //Result (concat //Result (list (car //Temp)))))
         ((> (car //Temp) 0)
          (set! //Result (concat //Result (list (- (car //Temp) 1))))))
        (set! //Temp (cdr //Temp)))))
     (#t
      (set! //Result (my-reduce @Set_Union (my-map @Hyb_TVs (@Components //I))))))
    (@Dtable_Put //I //Qry_/Hyb_/T/Vs //Result))
   (#t
    (set! //Result (@Dtable_Value_Part //Result))))
  //Result))

(define (@Gen_TVs //I //A/S)
 
 (if (equal? //A/S "Rec") (@Rec_TVs //I) (if (equal? //A/S "Reg") (@Reg_TVs //I) (@Hyb_TVs //I))))

(define (@TV_Conditional_Union //A //B)
 
 (if (member 0 //A) (union-n (@Set_Difference //A (list 0)) //B) //A))

; ----------------------------------------------------------------------- 
; A component is reachable if all the preceding statements in the         
; execution sequence have 0 as a possible terminal value.                 
; ----------------------------------------------------------------------- 
(define (@Gen_Reachable? //I /p //A/S)
 (let ((//O/K 1)
       (//Temp '())
       (//C 1))
  (cond
   ((not (or (null? /p) (@Simple? //I)))
    (cond
     ((= (@ST //I) //T_/Statements)
      (set! //Temp (@Cs //I))
      (while (and (= //O/K 1) (not (null? //Temp)) (< //C (car /p))) 
       (begin
        (cond
         ((not-member 0 (@Gen_TVs (car //Temp) //A/S))
          (set! //O/K 0)))
        (set! //Temp (cdr //Temp))
        (set! //C (+ //C 1))))))
    (cond
     ((and (= //O/K 1) (not (@Gen_Reachable? (@Get_n //I (car /p)) (cdr /p) //A/S)))
      (set! //O/K 0)))))
  (= //O/K 1)))

; ----------------------------------------------------------------------- 
; A position within a statement is terminal if it is reachable and...     
;                                                                         
;    1. One of the terminal values of that statement is greater than the  
;       depth OR                                                          
;                                                                         
;    2. The component is in a `Floop', the outermost `Floop' is in a      
;       terminal position, and one of the terminal values of the          
;       statement is equal to the depth OR                                
;                                                                         
;    3. The component is in a terminal position.                          
;                                                                         
; ----------------------------------------------------------------------- 
(define (@Gen_Terminal? //I /p //A/S)
 (let ((//O/K 0)
       (/d 0)
       (//T/Vs '())
       (/n-save /n)
       (//O/F/T 0)
       (//Reach 1)
       (//Comps '())
       (//Temp_/I //I)
       (//Temp_/P '())
       (//Old_/P /p)
       (//Term_/Posn 1)
       (funct-result '()))
  (set! /n 0)
  (while (not (null? //Old_/P)) 
   (begin
    (cond
     ((= //Reach 1)
      (cond
       ((@Simple? //Temp_/I)
        (set! //Reach 2))
       ((= (@ST //Temp_/I) //T_/Statements)
        (set! //Comps (@Cs //Temp_/I))
        (set! /n 1)
        (while (and (= //Reach 1) (not (null? //Comps)) (< /n (car //Old_/P))) 
         (begin
          (cond
           ((not-member 0 (@Gen_TVs (car //Comps) //A/S))
            (set! //Reach 0)))
          (set! //Comps (cdr //Comps))
          (set! /n (+ /n 1))))))))
    (cond
     ((= (@ST //Temp_/I) //T_/Floop)
      (set! /d (+ /d 1))
      (cond
       ((= //O/F/T 0)
        (set! //O/F/T //Term_/Posn)))))
    (cond
     ((and (= //Term_/Posn 1) (not (@Terminal_n? //Temp_/I (car //Old_/P))))
      (set! //Term_/Posn (- 1))))
    (set! //Temp_/I (@Get_n //Temp_/I (car //Old_/P)))
    (set! //Temp_/P (concat //Temp_/P (list (car //Old_/P))))
    (set! //Old_/P (cdr //Old_/P))))
  (cond
   ((not (= //Reach 0))
    (set! //T/Vs (@Gen_TVs (@Get //I /p) //A/S))
    (while (and (= //O/K 0) (not (null? //T/Vs))) 
     (begin
      (set! /n (car //T/Vs))
      (cond
       ((or (equal? /n //Omega) (> /n /d) (and (equal? /n /d) (= //O/F/T 1)) (and (= /d 0) (= //Term_/Posn 1)))
        (set! //O/K 1)))
      (set! //T/Vs (cdr //T/Vs))))))
  (set! funct-result (= //O/K 1))
  (set! /n /n-save)
  funct-result))

; ----------------------------------------------------------------------- 
; A proper statement is a statement which cannot cause termination of an  
; enclosing loop by way of an `EXIT' statement.                           
; ----------------------------------------------------------------------- 
(set! //Proper_/T/Vs_/Set (@Make_Set (list 0 //Omega)))
(define (@Gen_Proper? //I //A/S)
 
 (@Set_Subset? (@Gen_TVs //I //A/S) //Proper_/T/Vs_/Set))

; ----------------------------------------------------------------------- 
; An improper statement is one which only terminates via an `EXIT' or     
; `CALL' which would cause termination of one or more enclosing loops.    
; ----------------------------------------------------------------------- 
(define (@Gen_Improper? //I //A/S)
 (let ((//O/K 1)
       (//Temp (@Gen_TVs //I //A/S)))
  (while (and (= //O/K 1) (not (null? //Temp))) 
   (begin
    (cond
     ((and (not (equal? (car //Temp) //Omega)) (<= (car //Temp) 0))
      (set! //O/K 0)))
    (set! //Temp (cdr //Temp))))
  (= //O/K 1)))

; ----------------------------------------------------------------------- 
; A statement is reducible if replacing any terminal statement `EXIT (k)' 
; with terminal value 1 by `EXIT (k-1)' results in a terminal statement   
; with terminal value 0.                                                  
; ----------------------------------------------------------------------- 
(define (@Gen_Reducible? //I //A/S)
 
 (@Gen_Do_Rdc? //I //I '() 0 //A/S))

(define (@Gen_Do_Rdc? //Top //I /p /d //A/S)
 (let ((//O/K 1)
       (//T/C (@Components //I))
       (//T/N 1)
       (/d2 /d))
  (cond
   ((and (not (= (@GT //I) //T_/Expression)) (not (= (@GT //I) //T_/Condition)))
    (cond
     ((and (@Gen_Terminal? //Top /p //A/S) (= (@ST //I) //T_/Exit) (= (@V //I) (+ /d 1)))
      (cond
       ((not (or (@Terminal_Posn? //Top /p) (and (> (@V //I) /d) (@Outermost_Floop_Terminal? //Top /p))))
        (set! //O/K 0)))))
    (cond
     ((not (@Simple? //I))
      (while (and (= //O/K 1) (not (null? //T/C))) 
       (begin
        (cond
         ((= (@ST //I) //T_/Floop)
          (set! /d2 (+ /d 1))))
        (cond
         ((not (@Gen_Do_Rdc? //Top (car //T/C) (concat /p (list //T/N)) /d2 //A/S))
          (set! //O/K 0)))
        (set! //T/C (cdr //T/C))
        (set! //T/N (+ //T/N 1))))))))
  (= //O/K 1)))

; ----------------------------------------------------------------------- 
; The following function returns `TRUE' if and only if the item is a      
; dummy loop.                                                             
; ----------------------------------------------------------------------- 
(define (@Gen_Dummy? //I //A/S)
 
 (and (= (@ST //I) //T_/Floop) (@Gen_Reducible? (@Get_n //I 1) //A/S) (@Gen_Improper? (@Get_n //I 1) //A/S)))

; ----------------------------------------------------------------------- 
; The following function returns `TRUE' if and only if the item is        
; regular.                                                                
; ----------------------------------------------------------------------- 
(define (@Regular? //I)
 
 (@Set_Subset? (@Reg_TVs //I) (list //Omega)))

; ----------------------------------------------------------------------- 
; The following function returns `TRUE' if and only if the item is        
; a regular action system.                                                
; ----------------------------------------------------------------------- 
(define (@Regular_System? //I)
 (let ((//O/K 0)
       (//Temp '()))
  (cond
   ((not (= (@ST //I) //T_/A_/S))
    (set! //O/K 0))
   ((= //Assume_/A_/S_/Regular 1)
    (set! //O/K 1))
   (#t
    (set! //O/K 1)
    (set! //Temp (@Cs (@Get_n //I 2)))
    (while (and (= //O/K 1) (not (null? //Temp))) 
     (begin
      (cond
       ((not (@Regular? (car //Temp)))
        (set! //O/K 0)))
      (set! //Temp (cdr //Temp))))))
  (= //O/K 1)))

; ----------------------------------------------------------------------- 
; The following function returns `TRUE' if and only if the item is        
; recursive.                                                              
; ----------------------------------------------------------------------- 
(define (@Recursive? //I)
 (let ((//O/K 1)
       (//Temp '()))
  (cond
   ((@Simple? //I)
    (cond
     ((@Equal? //I /%const__query__1)
      (set! //O/K 0))))
   ((not (@Has_Comps? //I))
    #t)
   (#t
    (set! //Temp (@Cs //I))
    (while (and (= //O/K 1) (not (null? //Temp))) 
     (begin
      (cond
       ((not (@Recursive? (car //Temp)))
        (set! //O/K 0)))
      (set! //Temp (cdr //Temp))))))
  (= //O/K 1)))

; ----------------------------------------------------------------------- 
; The following function returns `TRUE' if and only if the item is        
; a recursive action system.                                              
; ----------------------------------------------------------------------- 
(define (@Recursive_System? //I)
 (let ((//O/K (if (= (@ST //I) //T_/A_/S) 1 0))
       (//Temp (@Cs (@Get_n //I 2))))
  (while (and (= //O/K 1) (not (null? //Temp))) 
   (begin
    (cond
     ((not (@Recursive? (car //Temp)))
      (set! //O/K 0)))
    (set! //Temp (cdr //Temp))))
  (= //O/K 1)))

; Return the type of the given action system: 
(define (@System_Type //I)
 
 (if (= //Assume_/A_/S_/Regular 1) "Reg" (@System_Type_Gen //I)))

(define (@System_Type_Gen //I)
 (let ((//Type "Reg")
       (//Rec 1)
       (//Z (@Make_Name "Z"))
       (//Actions (@Cs (@Get_n //I 2))))
  (while (not (null? //Actions)) 
   (begin
    (cond
     ((not (@Regular? (car //Actions)))
      (set! //Type "Hyb")))
    (cond
     ((> (@Call_Freq //Z (car //Actions)) 0)
      (set! //Rec 0)))
    (set! //Actions (cdr //Actions))))
  (cond
   ((= //Rec 1)
    (set! //Type "Rec")))
  //Type))

; ----------------------------------------------------------------------- 
; Note that an item of type `Actions' is not a statement.                 
; These functions consider items of type `Actions', and this has two      
; consequences:                                                           
;                                                                         
;  (a)  An `Action System' statement is only `in' a regular system if it  
;       itself if inside a regular system.                                
;                                                                         
;  (b)  The function `@AS_Type' returns the type of the enclosing action  
;       system, and not of the selected system (if one is selected).      
;                                                                         
; ----------------------------------------------------------------------- 
(define (@In_Reg_System?)
 
 (or (= //Assume_/A_/S_/Regular 1) (@In_Reg_System_Gen?)))

(define (@In_Reg_System_Gen?)
 (let ((//P (@Posn)))
  (while (and (not (= (@ST (@Get (@Program) //P)) //T_/Actions)) (not (null? //P))) 
   (set! //P (butlast-1 //P)))
  (and (= (@ST (@Get (@Program) //P)) //T_/Actions) (@Regular? (@Get (@Program) //P)))))

(define (@In_Rec_System?)
 
 (and (= //Assume_/A_/S_/Regular 0) (@In_Rec_System_Gen?)))

(define (@In_Rec_System_Gen?)
 (let ((//P (@Posn)))
  (while (and (not (= (@ST (@Get (@Program) //P)) //T_/Actions)) (not (null? //P))) 
   (set! //P (butlast-1 //P)))
  (and (= (@ST (@Get (@Program) //P)) //T_/Actions) (@Recursive? (@Get (@Program) //P)))))

(define (@In_Hyb_System?)
 
 (and (= //Assume_/A_/S_/Regular 0) (@In_Hyb_System_Gen?)))

(define (@In_Hyb_System_Gen?)
 (let ((//P (@Posn)))
  (while (and (not (= (@ST (@Get (@Program) //P)) //T_/Actions)) (not (null? //P))) 
   (set! //P (butlast-1 //P)))
  (and (not (and (= (@ST (@Get (@Program) //P)) //T_/Actions) (@Regular? (@Get (@Program) //P)))) (not (or (not (= (@ST (@Get (@Program) //P)) //T_/Actions)) (@Recursive? (@Get (@Program) //P)))))))

(define (@AS_Type)
 
 (if (= //Assume_/A_/S_/Regular 1) "Reg" (@AS_Type_Gen)))

(define (@AS_Type_Gen)
 (let ((//P (@Posn))
       (//Result '()))
  (while (and (not (= (@ST (@Get (@Program) //P)) //T_/Actions)) (not (null? //P))) 
   (set! //P (butlast-1 //P)))
  (cond
   ((= (@ST (@Get (@Program) //P)) //T_/Actions)
    (cond
     ((@Regular? (@Get (@Program) //P))
      (set! //Result "Reg"))
     ((@Recursive? (@Get (@Program) //P))
      (set! //Result "Rec"))
     (#t
      (set! //Result "Hyb"))))
   ((not (null? //A/S_/Type))
    (set! //Result //A/S_/Type))
   (#t
    (set! //Result "Hyb")))
  //Result))

; ----------------------------------------------------------------------- 
; The following functions are special cases of the ones earlier in this   
; file.  Each returns the corresponding result for the *current* item in  
; the *current* type of action system.                                    
; ----------------------------------------------------------------------- 
(define (@TVs)
 
 (@Gen_TVs (@I) (@AS_Type)))

(define (@Is_Reachable? /p)
 
 (@Gen_Reachable? (@I) /p (@AS_Type)))

(define (@Is_Terminal? /p)
 
 (@Gen_Terminal? (@I) /p (@AS_Type)))

(define (@Is_Terminal_Posn? /p)
 
 (@Terminal_Posn? (@I) /p))

(define (@Is_Proper?)
 
 (@Gen_Proper? (@I) (@AS_Type)))

(define (@Is_Improper?)
 
 (@Gen_Improper? (@I) (@AS_Type)))

(define (@Is_Reducible?)
 
 (@Gen_Reducible? (@I) (@AS_Type)))

(define (@Is_Dummy?)
 
 (@Gen_Dummy? (@I) (@AS_Type)))

(define (@All_Vars //I)
 (let ((/vars-save /vars)
       (funct-result '()))
  (set! /vars '())
  (@Edit)
  (@New_Program //I)
  (@Ateach_Expn /foreach-query-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Ateach_Lvalue /foreach-query-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Undo_Edit)
  (set! funct-result /vars)
  (set! /vars /vars-save)
  funct-result))

; Return the maximum length of sub-positions of items in the component: 
(define (@Max_Pos_L //I)
 (let ((//R 1)
       (//Previous '())
       (//Temp '()))
  (cond
   ((@Cs? //I)
    (set! //Previous (@Dtable_Get //I //Qry_/Max_/Pos_/L))
    (cond
     ((not (null? //Previous))
      (set! //R (@Dtable_Value_Part //Previous)))
     (#t
      (set! //R (+ 1 (my-reduce MAX (my-map @Max_Pos_L (@Components //I)))))
      (@Dtable_Put //I //Qry_/Max_/Pos_/L //R)))))
  //R))

; Compute the list of variables whose initial values 
; (on entry to the selected item) are used, before new values are assigned. 
; This version does *not* store the results in the dbase table because 
; it takes into account the bodies of known procedures in the procs table. 
; The proc_vars table stores the full UBA and Redefined values for the proc body. 
; TODO: This version assumes that the procs have no parameters! 
(define (@Full_UBA //I /procs /proc_vars)
 (let ((//R '()))
  (cond
   ((not-member //T_/Proc_/Call (@Stat_Types //I))
    ; Can use the query function on this component: 
    (set! //R (@UBA //I)))
   (#t
    ;We look at each component in turn and combine the results. 
    (let ((//G/T (@GT //I))
          (//S/T (@ST //I))
          (/redef-save /redef)
          (/comps (@Cs //I))
          (/comp-save /comp))
     (set! /redef '())
     (set! /comp '())
     (cond
      ((= //G/T //T_/Statements)
       (while (not (null? /comps)) 
        (begin
         (set! /comp (car /comps))
         (set! /comps (cdr /comps))
         (set! //R (union-n //R (@Set_Difference (@Full_UBA /comp /procs /proc_vars) /redef)))
         (set! /redef (union-n /redef (@Full_Redefined /comp /procs /proc_vars)))
         (cond
          ((@Gen_Improper? /comp "Hyb")
           (set! /comps '()))))))
      ((= //S/T //T_/Proc_/Call)
       (let ((/name (@V (@Get_n //I 1))))
        (cond
         ((null? (gethash /procs /name))
          (set! //R (@UBA //I)))
         ((not (null? (gethash /proc_vars /name)))
          ; Use the cached value 
          (set! //R (car (cdr (gethash /proc_vars /name)))))
         (#t
          ; Store dummy values in case of recursion, 
          ; and compute the value for the proc body: 
          (puthash /proc_vars /name (list (list '() '() '()) '() '()))
          (set! /vars (@Full_Vars (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
          (set! //R (@Full_UBA (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
          (set! /redef (@Full_Redefined (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
          (puthash /proc_vars /name (list /vars //R /redef))))))
      ((or (= //S/T //T_/A_/Proc_/Call) (= //S/T //T_/Push))
       (set! //R (union-n //R (@Variables //I))))
      ((= //S/T //T_/Where)
       ; Assumes proc names in nested WHEREs are unique: 
       (for-in /defn (@Cs (@Get_n //I 2)) 
        (cond
         ((= (@ST /defn) //T_/Proc)
          (puthash /procs (@V (@Get_n /defn 1)) /defn))))
       (set! //R (@Full_UBA (@Get_n //I 1) /procs /proc_vars)))
      (#t
       (for-in /comp /comps 
        (set! //R (union-n //R (@Full_UBA /comp /procs /proc_vars))))
       ;If the current item is a VAR structure or a FOR loop 
       ;then there are local variables which we don't want to return 
       ;as part of the result; so these must be taken out. 
       (cond
        ((= //S/T //T_/Var)
         (set! //R (@Set_Difference //R (@Set_Difference (@Lvars (@Get_n //I 1)) (@Used (@Get_n //I 1))))))
        ((= //S/T //T_/For)
         (set! //R (@Set_Difference //R (list (@V (@Get_n //I 1)))))))))
     (set! /redef /redef-save)
     (set! /comp /comp-save))))
  //R))

(define (@Full_Redefined //I /procs /proc_vars)
 (let ((//R '())
       (//G/T (@GT //I))
       (//S/T (@ST //I)))
  (cond
   ((not-member //T_/Proc_/Call (@Stat_Types //I))
    ; Can use the query function on this component: 
    (set! //R (@Redefined //I)))
   ((= //S/T //T_/Proc_/Call)
    (let ((/name (@V (@Get_n //I 1)))
          (/vars-save /vars)
          (//U/B/A-save //U/B/A))
     (set! /vars '())
     (set! //U/B/A '())
     (cond
      ((null? (gethash /procs /name))
       (set! //R (@Redefined //I)))
      ((not (null? (gethash /proc_vars /name)))
       ; Use the cached value 
       (set! //R (car (cdr (cdr (gethash /proc_vars /name))))))
      (#t
       ; Store dummy values in case of recursion, 
       ; and compute the value for the proc body: 
       (puthash /proc_vars /name (list (list '() '() '()) '() '()))
       (set! /vars (@Full_Vars (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
       (set! //U/B/A (@Full_UBA (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
       (set! //R (@Full_Redefined (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
       (puthash /proc_vars /name (list /vars //U/B/A //R))))
     (set! /vars /vars-save)
     (set! //U/B/A //U/B/A-save)))
   ((= //S/T //T_/For)
    (set! //R (@Set_Difference (@Full_Redefined (@Get_n //I 4) /procs /proc_vars) (list (@V (@Get_n //I 1))))))
   ((= //S/T //T_/Var)
    (set! //R (@Set_Difference (@Full_Redefined (@Get_n //I 2) /procs /proc_vars) (@Lvars (@Get_n //I 1)))))
   ((= //S/T //T_/While)
    (set! //R '()))
   ((= //S/T //T_/Guarded)
    (set! //R (@Full_Redefined (@Get_n //I 2) /procs /proc_vars)))
   ((= //G/T //T_/Statements)
    (for-in /comp (@Cs //I) 
     (set! //R (union-n //R (@Full_Redefined /comp /procs /proc_vars)))))
   (#t
    (set! //R (@Full_Redefined (@Get_n //I 1) /procs /proc_vars))
    (for-in /comp (cdr (@Cs //I)) 
     (set! //R (intersection-n //R (@Full_Redefined /comp /procs /proc_vars))))))
  //R))

; Returns <assigned, used, assigned_to_self> variables, cf @Qry_Vars: 
(define (@Full_Vars //I /procs /proc_vars)
 (let ((//R '()))
  (cond
   ((not-member //T_/Proc_/Call (@Stat_Types //I))
    ; Can use the query function on this component: 
    (set! //R (@Qry_Vars //I)))
   ((= (@ST //I) //T_/Proc_/Call)
    (let ((/name (@V (@Get_n //I 1))))
     (cond
      ((null? (gethash /procs /name))
       (set! //R (@Qry_Vars //I)))
      ((not (null? (gethash /proc_vars /name)))
       (set! //R (car (gethash /proc_vars /name))))
      (#t
       ; Store dummy values in case of recursion, 
       ; and compute the value for the proc body: 
       (puthash /proc_vars /name (list (list '() '() '()) '() '()))
       (set! //R (@Full_Vars (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
       (set! /redef (@Full_Redefined (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
       (set! //U/B/A (@Full_UBA (@Get_n (gethash /procs /name) 4) /procs /proc_vars))
       (puthash /proc_vars /name (list //R //U/B/A /redef))))))
   (#t
    ; We look at each component in turn and combine these results. 
    ; NB: VAR parameters are used as well as assigned.             
    (let ((//S/T (@ST //I))
          (//G/T (@GT //I))
          (//Comps (@Cs //I))
          (//Assd '())
          (//Used '())
          (//Self '())
          (//Temp '()))
     (while (not (null? //Comps)) 
      (begin
       (cond
        ((or (= (@GT (car //Comps)) //T_/Expression) (= (@GT (car //Comps)) //T_/Condition))
         (set! //Used (union-n //Used (@Qry_Exp_Vars (car //Comps)))))
        (#t
         (set! //Temp (@Full_Vars (car //Comps) /procs /proc_vars))
         (cond
          ((= //S/T //T_/Assign)
           (set! //Self (union-n //Self (wsl-ref //Temp 3))))
          ((or (= //G/T //T_/Statement) (= //G/T //T_/Assignment) (= //G/T //T_/Variable) (= //G/T //T_/Statements) (= //G/T //T_/Guarded) (= //G/T //T_/Action))
           (set! //Self (union-n (@Set_Difference (wsl-ref //Temp 3) //Used) (@Set_Difference //Self (wsl-ref //Temp 2)))))
          (#t
           (set! //Self '())))
         (set! //Assd (union-n //Assd (wsl-ref //Temp 1)))
         (set! //Used (union-n //Used (wsl-ref //Temp 2)))))
       (set! //Comps (cdr //Comps))))
     ; If the current item is a local variable structure or a `FOR' loop 
     ; then there are local variables which we don't want to return as   
     ; part of the result; so these must be taken out.                   
     (cond
      ((= //S/T //T_/Var)
       (let ((//Local_/Vars (@Make_Set (my-map @Qry_Get_Lvar (@Cs (@Get_n //I 1))))))
        (set! //Assd (@Set_Difference //Assd //Local_/Vars))
        (set! //Used (@Set_Difference //Used //Local_/Vars))
        (set! //Self (@Set_Difference //Self //Local_/Vars))))
      ((= //S/T //T_/For)
       (let ((//Loop_/Var (list (@V (@Get_n //I 1)))))
        (set! //Assd (@Set_Difference //Assd //Loop_/Var))
        (set! //Used (@Set_Difference //Used //Loop_/Var))
        (set! //Self (@Set_Difference //Self //Loop_/Var))))
      ((or (= //S/T //T_/Proc) (= //S/T //T_/Funct) (= //S/T //T_/B/Funct))
       (let ((//Local_/Vars (union-n (@Assigned (@Get_n //I 2)) (@Assigned (@Get_n //I 3)))))
        (set! //Assd (@Set_Difference //Assd //Local_/Vars))
        (set! //Used (@Set_Difference //Used //Local_/Vars))
        (set! //Self (@Set_Difference //Self //Local_/Vars))))
      ((or (= //S/T //T_/Print) (= //S/T //T_/Prinflush) (= //S/T //T_/Error))
       (set! //Assd (union-n //Assd (list /os_name)))
       (set! //Used (union-n //Used (list /os_name))))
      ((= //S/T //T_/Pop)
       (set! //Used (union-n //Used (@Assigned (@Get_n //I 2))))
       (set! //Self '()))
      ((or (= //S/T //T_/Struct_/Lvalue) (= //S/T //T_/Aref_/Lvalue) (= //S/T //T_/Sub_/Seg_/Lvalue) (= //S/T //T_/Rel_/Seg_/Lvalue) (= //S/T //T_/Final_/Seg_/Lvalue) (= //S/T //T_/Proc_/Call) (= //S/T //T_/A_/Proc_/Call) (= //S/T //T_/Push))
       (set! //Used (union-n //Used //Assd))))
     (set! //R (list //Assd //Used //Self)))))
  //R))

(define (@Full_Variables //I /procs /proc_vars)
 (let ((//R (@Full_Vars //I /procs /proc_vars)))
  (union-n (wsl-ref //R 1) (wsl-ref //R 2))))

(define (@Full_Assigned //I /procs /proc_vars)
 (let ((//R (@Full_Vars //I /procs /proc_vars)))
  (wsl-ref //R 1)))

(define (@Full_Used //I /procs /proc_vars)
 (let ((//R (@Full_Vars //I /procs /proc_vars)))
  (wsl-ref //R 2)))

(define (@Full_Assd_Only //I /procs /proc_vars)
 (let ((//R (@Full_Vars //I /procs /proc_vars)))
  (@Set_Difference (wsl-ref //R 1) (wsl-ref //R 2))))

(define (@Full_Used_Only //I /procs /proc_vars)
 (let ((//R (@Full_Vars //I /procs /proc_vars)))
  (@Set_Difference (wsl-ref //R 2) (wsl-ref //R 1))))

(define (@Full_Assd_To_Self //I /procs /proc_vars)
 (let ((//R (@Full_Vars //I /procs /proc_vars)))
  (wsl-ref //R 3)))

(define (@Print_Elts /elts)
 (display-list-flush "<")
 (while (not (null? /elts)) 
  (begin
   (cond
    ((< (last-1 (car /elts)) 0)
     (display-list-flush (@Join "." (my-map @N_String (butlast-1 (car /elts)))))
     (display-list-flush "." (- (last-1 (car /elts)))))
    (#t
     (display-list-flush (@Join "." (my-map @N_String (car /elts))))))
   (set! /elts (cdr /elts))
   (cond
    ((not (null? /elts))
     (display-list-flush ", ")))))
 (display-list ">"))

; Find all elements whose address is taken: 
(define (@Elts_Addr //I)
 
 (if (@Is_Addr? //I) (@Elements (@Get_Addr //I)) (if (@Cs? //I) (my-reduce @Set_Union (my-map @Elts_Addr (@Cs //I))) '())))

; ----------------------------------------------------------------------- 

