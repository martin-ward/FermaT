;;; Scheme translation of WSL code
(define (/foreach-absorb_left-1 //Depth //A/S_/Type)
 (for-in /assign /assigns 
  (cond
   ((@LR_Equal? (@I) (@Get_n /assign 1))
    (@Paste_Over (@Get_n /assign 2))))))

(define (/foreach-absorb_left-2 //Depth //A/S_/Type)
 (cond
  ((@Cs? (@I))
   (@Paste_Over (@Simplify_Expn (@I))))))

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
(define (@Absorb_Left_Test)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (= (@GT (@I)) //T_/Statement))
   (@Fail "The selected item is not a Statement."))
  ((not (@Left?))
   (@Fail "There is no statement to the left of this one."))
  ((= (@ST (@I)) //T_/Assignment)
   (@Absorb_Left_Test_Assignment))
  ((= (@ST (@I)) //T_/For)
   (@Absorb_Left_Test_For))
  ((= (@ST (@I)) //T_/Var)
   (@Absorb_Left_Test_Var))
  ((= (@ST (@I)) //T_/Where)
   (@Absorb_Left_Test_Where))
  ((= (@ST (@I)) //T_/While)
   (@Absorb_Left_Test_While))
  ((or (= (@ST (@I)) //T_/Cond) (= (@ST (@I)) //T_/D_/If))
   (@Absorb_Left_Test_Cond))
  ((= (@ST (@I)) //T_/Floop)
   (@Absorb_Left_Test_Floop))
  (#t
   (@Fail "The selected statement must be of a suitable type."))))

;-----------------------------------------------------------------------
(define (@Absorb_Left_Test_Assignment)
 (let ((//C1 '())
       (//C2 '())
       (//Assns '())
       (//New '())
       (//U '())
       (//V '()))
  (@Left)
  (cond
   ((not (= (@ST (@I)) //T_/Assignment))
    (@Fail "Can only absorb another assignment into an assignment"))
   (#t
    ;Basically, we 'do' the transformation, then see if it 
    ; produced something invalid
    ; First, replace any variables in the second statement 
    ; with the values assigned to them by the first 
    (set! //C1 (@Components (@I)))
    (set! //U (@Elts_Assigned (@I)))
    (@Right)
    (set! //V (@Elts_Assigned (@I)))
    (cond
     ((@Elt_Clash_List? //U (@Set_Difference //V //U))
      (@Fail "Second assignment updates part of first"))
     (#t
      (@Edit)
      (@Absorb_Left_Fix_Vars //C1)
      ; Now, merge in any assignments from the first statement 
      ; to variables which are not overwritten by the second 
      (set! //V (@Elts_Assigned (@I)))
      (for-in //I //C1 
       (cond
        ((not (@Set_Subset? (@Elts_Assigned (@Get_n //I 1)) //V))
         (cond
          ((@Elt_Clash_List? (@Elts_Assigned (@Get_n //I 1)) //V)
           (@Fail "The two assignments clash")))
         (set! //New (cons //I //New)))))
      (@Down_Last)
      (cond
       ((not (null? //New))
        (@Splice_After //New)))
      (@Up)
      ; Now, see if this resulted in variables being 
      ; effectively set and used simultaneously 
      (set! //C2 (@Components (@I)))
      ; The individual Assign's 
      (set! //Assns (@Elts_Assigned (@I)))
      ; The assigned variables 
      (for-in //I //C2 
       (begin
        (set! //U (@Elts_Used //I))
        (set! //V (@Elts_Assigned //I))
        (cond
         ((not (null? (intersection-n //Assns (@Set_Difference //U //V))))
          (@Fail "Variable would be simultaneously used and assigned")))))
      (cond
       ((not (@Failed?))
        (@Pass)))
      (@Undo_Edit)))))))

(define (@Absorb_Left_Test_For)
 (@Left)
 (cond
  ((not (@Is_Proper?))
   (@Fail "Cannot absorb a non-proper statement into a simple statement"))
  ((or (not (null? (intersection-n (@Assigned (@I)) (@Used (@I))))) (not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set))))
   (@Fail "A statement cannot be absorbed into a `For' if it could assign to variables it uses."))
  (#t
   ;The thing being assigned is OK, but is it compatible with the loop?
   ;Before we go on, we need to note what variables are used in it or its parameters.
   (let ((//V (@Variables (@I))))
    (@Right)
    (cond
     ((= (@ST (@I)) //T_/For)
      (let ((//L (@V (@Get_n (@I) 1)))
            (//E1 (@Get_n (@I) 2))
            (//E2 (@Get_n (@I) 3))
            (//E3 (@Get_n (@I) 4))
            (//S (@Cs (@Get_n (@I) 5))))
       ;Check that the loop doesn't use the same variables as the other statement...
       ;...and check that the loop will be executed at least once.
       (cond
        ((or (not (null? (intersection-n //V (@Variables (@I))))) (member //L //V))
         (@Fail "The loop used the same variables as the other statement."))
        ((not (or (and (@True? (@Make 315 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 317 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1))))) (and (@True? (@Make 314 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 316 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1)))))))
         (@Fail "The loop won't definitely execute."))
        (#t
         (@Pass))))))))))

(define (@Absorb_Left_Test_Var)
 ;Check that the statement to be absorbed doesn't use any of the local variables.
 (let ((//Local (@Assigned (@Get_n (@I) 1)))
       (//V (@Variables (@Get_n (@I) 1))))
  (@Left)
  (cond
   ((not (@Is_Proper?))
    (@Fail "Cannot absorb a non-proper statement into a simple statement"))
   ((= (@ST (@I)) //T_/Proc_/Call)
    (let ((/vars //Local)
          (/posn (@Posn)))
     (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
      (@Up))
     (cond
      ((= (@ST (@I)) //T_/Where)
       (set! /vars (@Variables (@I)))))
     (@Goto /posn)
     (cond
      ((not (null? (intersection-n //Local /vars)))
       (@Fail "The proc call being absorbed could use some of the local variables."))
      (#t
       (@Pass)))))
   ((or (not (null? (intersection-n //Local (@Variables (@I))))) (not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set))))
    (@Fail "The statement being absorbed could use some of the local variables."))
   ((and (not (= (@ST (@I)) //T_/Assignment)) (not (null? (intersection-n //V (@Assigned (@I))))))
    (@Fail "Some of the assigning variables are changed by the statement being absorbed."))
   (#t
    (@Pass)))))

(define (@Absorb_Left_Test_Where)
 ;Check that the statement to be absorbed doesn't use any of the definitions.
 (let ((//D (@Make_Set (my-map @V1 (@Cs (@Get_n (@I) 2))))))
  (@Left)
  (cond
   ((not (null? (intersection-n //D (@Make_Set (concat (my-map HEAD (@Proc_Calls (@I))) (my-map HEAD (@Funct_Calls (@I))))))))
    (@Fail "The statement being absorbed uses some of the definitions."))
   (#t
    (@Pass)))))

(define (@Absorb_Left_Test_While)
 (@Left)
 (cond
  ((not (@Is_Proper?))
   (@Fail "Cannot absorb a non-proper statement into a simple statement"))
  ((or (not (null? (intersection-n (@Assigned (@I)) (@Used (@I))))) (not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set))))
   (@Fail "A statement cannot be absorbed into a `While' if it could assign to variables it uses."))
  (#t
   ;The thing being assigned is OK, but is it compatible with the loop?
   ;Before we go on, we need to note what variables are used in it.
   (let ((//V (@Variables (@I)))
         (//A (@Assigned (@I)))
         (//B '()))
    (@Right)
    (set! //B (@Get_n (@I) 1))
    ;Check that the loop doesn't use the same variables as the other statement.
    ;Now check that the loop will be executed at least once.
    ;This is done by looking for an assertion before the loop.
    (cond
     ((not (null? (intersection-n //V (@Variables (@I)))))
      (@Fail "The loop used the same variables as the other statement."))
     ((or (<= (@Posn_n) 2) (not (= (@ST (@Get_n (@Parent) (- (@Posn_n) 2))) //T_/Assert)))
      (@Fail "There is no assertion before the loop."))
     (#t
      (@Left)
      (@Left)
      ;The assertion my imply that the loop executes at least once.
      (cond
       ((not (@Implies? (@Get_n (@I) 1) //B))
        (@Fail "The assertion does not imply that the loop executes."))
       ((not (null? (intersection-n //A (@Variables (@I)))))
        (@Fail "The assertion could be changed by the statement being absorbed."))
       (#t
        (@Pass)))))))))

(define (@Absorb_Left_Test_Cond)
 (@Left)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Pass))
  ((not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set)))
   (@Fail "The statement may affect the conditions."))
  (#t
   (let ((//A (@Assigned (@I))))
    (@Right)
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((not (null? (intersection-n (@Variables (@Get_n (@I) 1)) //A)))
       (@Fail "The variables in the statement affect the conditions.")
       (set! /fl_flag1 1))
      ((@Right?)
       (@Right)
       (set! /fl_flag1 0))
      (#t
       (@Pass)
       (set! /fl_flag1 1))))))))

(define (@Absorb_Left_Test_Floop)
 (let ((//I-save //I))
  (set! //I (@Increment (@Get_n (@Parent) (- (@Posn_n) 1)) (@AS_Type) 1 1))
  (cond
   ((and (= (gen-length //I) 1) (@Equal? (last-1 (@Cs (@Get_n (@I) 1))) (car //I)))
    (@Pass))
   (#t
    (@Fail "The statement to the left doesn't match the end of the body.")))
  (set! //I //I-save)))

;-----------------------------------------------------------------------
(define (@Absorb_Left_Code //Data)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (let ((//C '()))
  (cond
   ((= (@ST (@I)) //T_/Assignment)
    (@Absorb_Left_Code_Assignment))
   ((member (@ST (@I)) (list //T_/For //T_/While))
    (@Left)
    (@Cut)
    (@Down_Last)
    (@Down)
    (@Paste_Before (@Buffer))
    (@Up)
    (@Up))
   ((= (@ST (@I)) //T_/Where)
    (@Left)
    (@Cut)
    (@Down)
    (@Down)
    (@Paste_Before (@Buffer))
    (@Up)
    (@Up))
   ((= (@ST (@I)) //T_/Var)
    (@Left)
    (cond
     ((= (@ST (@I)) //T_/Assignment)
      (set! //C (@Cs (@I)))))
    (@Cut)
    (@Down_Last)
    (@Down)
    (@Paste_Before (@Buffer))
    (@Up)
    (@Left)
    (cond
     ((not (null? //C))
      (@Absorb_Left_Fix_Vars //C)))
    (@Up))
   ((= (@ST (@I)) //T_/Floop)
    (@Left)
    (@Delete)
    (@Down)
    (@Down)
    ; Paste before cutting in case there is only one statement! 
    (@Paste_Before (last-1 (@Cs (@Parent))))
    (@To_Last)
    (@Delete)
    (@Up)
    (@Up))
   (#t
    ;We have a `Cond' or `D_If' statement which is preceded by an assignment.
    (@Left)
    (cond
     ((= (@ST (@I)) //T_/Assignment)
      (set! //C (@Cs (@I)))))
    (@Cut)
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (@Down_Last)
      (@Down)
      ; Check for two assignments to the same variable 
      ; and attempt to merge them if possible. 
      (cond
       ((and (= (@ST (@Buffer)) //T_/Assignment) (= (@ST (@I)) //T_/Assignment) (= (@Size (@Buffer)) 1) (= (@Size (@I)) 1) (@Equal? (@Get_n (@Get_n (@Buffer) 1) 1) (@Get_n (@Get_n (@I) 1) 1)))
        (@Paste_Before (@Buffer))
        (cond
         ((@Trans? //T/R_/Absorb_/Right)
          (@Trans //T/R_/Absorb_/Right ""))))
       (#t
        (@Paste_Before (@Buffer))))
      (@Up)
      (@Left)
      (cond
       ((not (null? //C))
        (@Absorb_Left_Fix_Vars //C)))
      (@Up)
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)))))

;-----------------------------------------------------------------------
(define (@Absorb_Left_Fix_Vars /assigns-par)
 (let ((/assigns-save /assigns))
  (set! /assigns /assigns-par)
  (@Foreach_Expn /foreach-absorb_left-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Expn /foreach-absorb_left-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /assigns /assigns-save)))

(define (@Absorb_Left_Code_Assignment)
 (let ((//C1 '())
       (//V '())
       (//New '()))
  (@Left)
  (set! //C1 (@Cs (@I)))
  (@Right)
  (@Absorb_Left_Fix_Vars //C1)
  (set! //V (@Assigned (@I)))
  (for-in //I //C1 
   (cond
    ((not-member (@Value (@Get_n //I 1)) //V)
     (set! //New (cons //I //New)))))
  (@Down_Last)
  (cond
   ((not (null? //New))
    (@Splice_After //New)))
  (@Up)
  (@Left)
  (@Delete)))

#t
