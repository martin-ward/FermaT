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
(define (@Take_Out_Right_Test)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (= (@GT (@I)) //T_/Statement))
   (@Fail "The selected item is not a Statement."))
  ((@Right?)
   (@Fail "There is a statement to the right of this one."))
  ((not (@Up?))
   (@Fail "There is no structure out of which to take this item."))
  ((not (null? (intersection-n (@Stat_Types (@I)) //Ext_/Call_/Types_/Set)))
   (@Fail "There are undetermined procedure or function calls in the selected item."))
  (#t
   (let ((//S (@I))
         (//Orig_/A/S (@AS_Type)))
    (@Save_State)
    (@Delete)
    (@Up)
    (cond
     ((not (@Up?))
      (@Fail "There is no structure out of which to take this item."))
     (#t
      (@Up)
      (cond
       ((= (@ST (@I)) //T_/For)
        (@Take_Out_Right_Test_For //S))
       ((= (@ST (@I)) //T_/Var)
        (@Take_Out_Right_Test_Var //S))
       ((= (@ST (@I)) //T_/Where)
        (@Take_Out_Right_Test_Where //S))
       ((= (@ST (@I)) //T_/While)
        (@Take_Out_Right_Test_While //S))
       ((and (= (@GT (@I)) //T_/Guarded) (not (= (@ST (@Parent)) //T_/D_/Do)))
        (@Take_Out_Right_Test_Guarded //S //Orig_/A/S))
       (#t
        (@Fail "The enclosing item is not of a suitable type.")))))
    (@Undo)))))

(define (@Take_Out_Right_Test_For //S)
 (cond
  ((not (null? (intersection-n (@Assigned //S) (@Used //S))))
   (@Fail "A statement cannot be taken out of a `For' if it could assign to variables it uses."))
  (#t
   ;The thing being assigned is OK, but can it be taken from the loop?
   (let ((//L (@V (@Get_n (@I) 1)))
         (//E1 (@Get_n (@I) 2))
         (//E2 (@Get_n (@I) 3))
         (//E3 (@Get_n (@I) 4))
         (//Body (@Get_n (@I) 5))
         (//Assd (@Assigned //S)))
    ;Check that the loop doesn't use the same variables as the other statement...
    ;...and check that the loop will be executed at least once.
    (cond
     ((member //L (@Variables //S))
      (@Fail "The statement uses the loop index."))
     ((not (null? (intersection-n //Assd (@Used //Body))))
      (@Fail "The statement changes variables which are used in the loop body"))
     ((not (or (and (@True? (@Make 315 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 317 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1))))) (and (@True? (@Make 314 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 316 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1)))))))
      (@Fail "Cannot prove that the loop will be executed."))
     (#t
      (@Pass)))))))

(define (@Take_Out_Right_Test_Var //S)
 ;Check that the statement to be taken out doesn't use any of the local variables.
 (let ((//Local (@Assigned (@Get_n (@I) 1))))
  (cond
   ((not (null? (intersection-n //Local (@Variables //S))))
    (@Fail "The selected statement being uses some of the local variables."))
   ((not (null? (intersection-n (@Stat_Types //S) (@Make_Set (list //T_/M/W_/Proc_/Call //T_/Proc_/Call)))))
    (@Fail "Calls in the statement may use the local variables"))
   (#t
    (@Pass)))))

(define (@Take_Out_Right_Test_Where //S)
 ;Check that the statement to be taken out doesn't use any of the definitions.
 (let ((//D (@Make_Set (my-map @V1 (@Cs (@Get_n (@I) 2))))))
  (cond
   ((not (null? (intersection-n //D (@Make_Set (concat (my-map HEAD (@Proc_Calls //S)) (my-map HEAD (@Funct_Calls //S)))))))
    (@Fail "The statement being taken out uses some of the definitions."))
   (#t
    (@Pass)))))

(define (@Take_Out_Right_Test_While //S)
 (cond
  ((not (null? (intersection-n (@Assigned //S) (@Used //S))))
   (@Fail "A statement cannot be taken out into a `While' if it could assign to variables it uses."))
  (#t
   ;The thing being assigned is OK, but can it be taken from the loop?
   (let ((//B (@Get_n (@I) 1)))
    ;Check that the loop doesn't use the same variables as the other statement.
    ;Now check that the loop will be executed at least once.
    ;This is done by looking for an assertion before the loop.
    (cond
     ((not (null? (intersection-n (@Assigned //S) (@Variables (@I)))))
      (@Fail "The loop used some variables assigned in the other statement."))
     ((or (not (@Left?)) (not (= (@ST (@Get_n (@Parent) (- (@Posn_n) 1))) //T_/Assert)))
      (@Fail "There is no assertion before the loop."))
     (#t
      (@Left)
      ;The assertion my imply that the loop executes at least once.
      (cond
       ((not (@Implies? (@Get_n (@I) 1) //B))
        (@Fail "The assertion does not imply that the loop executes."))
       ((not (null? (intersection-n (@Assigned //S) (@Variables (@I)))))
        (@Fail "The assertion could be changed by the statement being taken out."))
       (#t
        (@Pass)))))))))

(define (@Take_Out_Right_Test_Guarded //S //A/S_/Type)
 ;This function is called with a guarded selected.
 (let ((//P (@Posn_n))
       (//A (@Assigned //S)))
  (while (and (not (@Failed?)) (@Left?)) 
   (begin
    (@Left)
    (cond
     ((member 0 (@Gen_TVs (@Get_n (@I) 2) //A/S_/Type))
      (@Down_Last)
      (@Down_Last)
      (cond
       ((not (or (@Equal? (@I) //S) (and (= (@ST (@I)) //T_/Exit) (> (@V (@I)) 1))))
        (@Fail "The selected statement does not end each guard.")))
      (@Up)
      (@Up)))))
  (@To //P)
  (while (and (not (@Failed?)) (@Right?)) 
   (begin
    (@Right)
    (cond
     ((member 0 (@Gen_TVs (@Get_n (@I) 2) //A/S_/Type))
      (@Down_Last)
      (@Down_Last)
      (cond
       ((not (or (@Equal? (@I) //S) (and (= (@ST (@I)) //T_/Exit) (> (@V (@I)) 1))))
        (@Fail "The selected statement does not end each guard.")))
      (@Up)
      (@Up)))))
  (cond
   ((not (@Failed?))
    (@Pass)))))

;-------------------------------------------------------------------------
(define (@Take_Out_Right_Code //Data)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (let ((//S (@I))
       (//P1 '())
       (//P2 '())
       (//P3 '()))
  (set! //P1 (@Posn_n))
  (@Up)
  (set! //P2 (@Posn_n))
  (@Up)
  (cond
   ((= (@GT (@I)) //T_/Guarded)
    (@Up)
    (@Edit_Parent)
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (@Down_Last)
      (@Down_Last)
      (cond
       ((@Equal? (@I) //S)
        (@Delete)))
      (@Up)
      (@Up)
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)
    (@Paste_After //S)
    (cond
     ((= (@ST (@I)) //T_/Cond)
      (@Fix_Cond))
     (#t
      (@Fix_Dijkstra)))
    (@End_Edit))
   (#t
    (@Edit_Parent)
    (@Paste_After //S)
    (@Down_To //P2)
    (@Down_To //P1)
    (@Clever_Delete)
    (@End_Edit)))))

#t
