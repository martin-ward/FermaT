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
(define (@Take_Out_Left_Test)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (= (@GT (@I)) //T_/Statement))
   (@Fail "The selected item is not a Statement."))
  ((@Left?)
   (@Fail "There is a statement to the left of this one."))
  ((< (gen-length (@Posn)) 2)
   (@Fail "There is no structure out of which to take this item."))
  ((not (null? (intersection-n (@Stat_Types (@I)) //Ext_/Call_/Types_/Set)))
   (@Fail "There are undetermined procedure or function calls in the selected item."))
  ((and (not (= (@ST (@GParent)) //T_/For)) (not (= (@ST (@GParent)) //T_/Var)) (not (= (@ST (@GParent)) //T_/Where)) (not (= (@ST (@GParent)) //T_/While)) (not (= (@ST (@GParent)) //T_/Guarded)) (not (= (@ST (@GParent)) //T_/Floop)))
   (@Fail "The enclosing structure is not of a suitable type."))
  (#t
   (let ((//S (@I)))
    (@Save_State)
    (@Delete)
    (@Up)
    (@Up)
    ; to enclosing structure 
    (cond
     ((= (@ST (@I)) //T_/For)
      (@Take_Out_Left_Test_For //S))
     ((= (@ST (@I)) //T_/Var)
      (@Take_Out_Left_Test_Var //S))
     ((= (@ST (@I)) //T_/Where)
      (@Take_Out_Left_Test_Where //S))
     ((= (@ST (@I)) //T_/While)
      (@Take_Out_Left_Test_While //S))
     ((and (= (@GT (@I)) //T_/Guarded) (@Up?) (not (= (@ST (@Parent)) //T_/D_/Do)))
      (@Take_Out_Left_Test_Guarded //S))
     ((= (@ST (@I)) //T_/Floop)
      (@Take_Out_Left_Test_Floop //S))
     (#t
      (@Fail "The enclosing item is not of a suitable type.")))
    (@Undo)))))

(define (@Take_Out_Left_Test_For //S)
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
     ((not (null? (intersection-n (union-n (union-n (@Used //E1) (@Used //E2)) (@Used //E3)) //Assd)))
      (@Fail "The limits or step would be changed by the selected statement"))
     ((not (null? (intersection-n (@Used //S) (@Assigned //Body))))
      (@Fail "The statement uses variables which are changed by the loop body."))
     ((not (null? (intersection-n (intersection-n //Assd (@Assigned //Body)) (@Used //Body))))
      (@Fail "Statement assigns to variables which are used in the loop body"))
     ((not (or (and (@True? (@Make 315 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 317 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1))))) (and (@True? (@Make 314 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 316 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1)))))))
      (@Fail "Cannot prove that the loop will be executed."))
     (#t
      (@Pass)))))))

(define (@Take_Out_Left_Test_Var //S)
 ;Check that the statement to be taken out doesn't use any of the local variables.
 (let ((//Local (@Assigned (@Get_n (@I) 1)))
       (//U (@Variables (@Get_n (@I) 1))))
  (cond
   ((not (null? (intersection-n //Local (@Variables //S))))
    (@Fail "The selected statement being uses some of the local variables."))
   ((not (null? (intersection-n //U (@Assigned //S))))
    (@Fail "Some of the assigning variables are changed by the selected statement."))
   (#t
    (@Pass)))))

(define (@Take_Out_Left_Test_Where //S)
 ;Check that the statement to be taken out doesn't use any of the definitions.
 (let ((//D (@Make_Set (my-map @V1 (@Cs (@Get_n (@I) 2))))))
  (cond
   ((not (null? (intersection-n //D (@Make_Set (concat (my-map HEAD (@Proc_Calls //S)) (my-map HEAD (@Funct_Calls //S)))))))
    (@Fail "The statement being taken out uses some of the definitions."))
   (#t
    (@Pass)))))

(define (@Take_Out_Left_Test_While //S)
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
     ((not (null? (intersection-n (@Variables //S) (@Assigned (@I)))))
      (@Fail "The loop assigns to some variables used in the other statement."))
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

(define (@Take_Out_Left_Test_Guarded //S)
 ;This function is called with a guarded selected.
 (let ((//P (@Posn_n))
       (//A (@Assigned //S)))
  (cond
   ((not (null? (intersection-n (@Variables (@Get_n (@I) 1)) //A)))
    (@Fail "The selected statement uses the same variables as one of the conditions.")))
  (while (and (not (@Failed?)) (@Left?)) 
   (begin
    (@Left)
    (cond
     ((not (null? (intersection-n (@Variables (@Get_n (@I) 1)) //A)))
      (@Fail "The selected statement uses the same variables as one of the conditions.")))
    (cond
     ((not (@Equal? (@Get_n (@Get_n (@I) 2) 1) //S))
      (@Fail "The selected statement does not begin each guard.")))))
  (@To //P)
  (while (and (not (@Failed?)) (@Right?)) 
   (begin
    (@Right)
    (cond
     ((not (null? (intersection-n (@Variables (@Get_n (@I) 1)) //A)))
      (@Fail "The selected statement uses the same variables as one of the conditions.")))
    (cond
     ((not (@Equal? (@Get_n (@Get_n (@I) 2) 1) //S))
      (@Fail "The selected statement does not begin each guard.")))))
  (cond
   ((not (@Failed?))
    (@Pass)))))

(define (@Take_Out_Left_Test_Floop //S)
 (cond
  ((@Gen_Proper? //S (@AS_Type))
   (@Pass))
  (#t
   (@Fail "Statement is not a proper sequence"))))

(define (@Take_Out_Left_Code //Data)
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
    (@Down_Last)
    (@Down)
    (@Delete)
    (@Up)
    (@Up)
    (while (@Right?) 
     (begin
      (@Right)
      (@Down_Last)
      (@Down)
      (@Delete)
      (@Up)
      (@Up)))
    (@Up)
    (@Paste_Before //S)
    (@Right)
    (@Fix_Cond)
    (@End_Edit))
   (#t
    (@Edit_Parent)
    (@Paste_Before //S)
    (@Right)
    (cond
     ((= (@ST (@I)) //T_/Floop)
      ; Add a copy of S to the end of the loop body 
      (@Down)
      (@Down_Last)
      (@Paste_After //S)
      (@Up)
      (@Up)))
    (@Down_To //P2)
    (@Down_To //P1)
    (@Clever_Delete)
    (@End_Edit)))))

