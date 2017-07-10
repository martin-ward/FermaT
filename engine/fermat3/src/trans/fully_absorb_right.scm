;;; Scheme translation of WSL code
(define (/foreach-fully_absorb_right-1 //Depth //A/S_/Type)
 (cond
  ((or (= //Depth 0) (and (= (@Spec_Type (@Item)) //T_/Exit) (equal? (@Value (@Item)) //Depth)))
   (cond
    ((and (= (@Spec_Type (@Item)) //T_/Exit) (> //Depth 0))
     (@Splice_Over (@Cs (@Increment (@Make //T_/Statements '() (@Buffer)) //A/S_/Type //Depth 0))))
    ((= (@Spec_Type (@Item)) //T_/Skip)
     (@Splice_Over (@Buffer)))
    ((or (and (= (@Spec_Type (@Item)) //T_/Exit) (= //Depth 0)) (@Gen_Improper? (@Item) //A/S))
     #t)
    ((and (= (@Spec_Type (@Item)) //T_/Call) (equal? (@Value (@Item)) (@Make_Name "Z")))
     #t)
    (#t
     (@Splice_After (@Buffer)))))))

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
(define (@Fully_Absorb_Right_Test)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (= (@GT (@I)) //T_/Statement))
   (@Fail "The selected item is not a Statement."))
  ((not (@Right?))
   (@Fail "There is no statement to the right of this one."))
  ((= (@ST (@I)) //T_/Assignment)
   (@Right)
   (cond
    ((and (= (@ST (@I)) //T_/Assignment) (@Trans? //T/R_/Absorb_/Left))
     (@Pass))
    (#t
     (@Fail "An assignment can only 'absorb' another assignment")))
   (@Left))
  ((= (@ST (@I)) //T_/For)
   (let ((//L (@Cs (@Parent)))
         (//I '()))
    (@Right)
    (set! //I (@Make //T_/Statements '() (@Sub_Seg //L (@Posn_n) (@Size (@Parent)))))
    (cond
     ((or (not (null? (intersection-n (@Assigned //I) (@Used //I)))) (not (null? (intersection-n (@Stat_Types //I) //Call_/Types_/Set))))
      (@Fail "A statement cannot be absorbed into a `For' if it could assign to variables it uses."))
     (#t
      ;The thing being assigned is OK, but is it compatible with the loop?
      ;Before we go on, we need to note what variables are used in it.
      (let ((//V (@Variables //I)))
       (@Left)
       (let ((//L (@V (@Get_n (@I) 1)))
             (//E1 (@Get_n (@I) 2))
             (//E2 (@Get_n (@I) 3))
             (//E3 (@Get_n (@I) 4))
             (//S (@Cs (@Get_n (@I) 5))))
        ;Check that the loop doesn't use the same variables as the other statement...
        ;...and check that the loop will be executed at least once.
        (cond
         ((or (not (null? (intersection-n //V (@Variables (@Get_n (@I) 5))))) (member //L //V))
          (@Fail "The loop used the same variables as the other statement."))
         ((not (or (and (@True? (@Make 315 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 317 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1))))) (and (@True? (@Make 314 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 316 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1)))))))
          (@Fail "The loop won't definitely execute."))
         (#t
          (@Pass)))))))))
  ((= (@ST (@I)) //T_/Var)
   ;Check that the statement to be absorbed doesn't use any of the local variables.
   (let ((//L (@Cs (@Parent)))
         (//I '())
         (//V (@Assigned (@Get_n (@I) 1))))
    (@Right)
    (set! //I (@Make //T_/Statements '() (@Sub_Seg //L (@Posn_n) (@Size (@Parent)))))
    (cond
     ((not (@Gen_Proper? //I (@AS_Type)))
      (@Fail "Cannot absorb a non-proper statement into a simple statement"))
     ((or (not (null? (intersection-n //V (@Variables //I)))) (not (null? (intersection-n (@Stat_Types //I) //Call_/Types_/Set))))
      (@Fail "The statement being absorbed could use some of the local variables."))
     (#t
      (@Pass)))))
  ((= (@ST (@I)) //T_/Where)
   ;Check that the statement to be absorbed doesn't use any of the definitions.
   (let ((//L (@Cs (@Parent)))
         (//I '())
         (//D (@Make_Set (my-map @V1 (@Cs (@Get_n (@I) 2))))))
    (@Right)
    (set! //I (@Make //T_/Statements '() (@Sub_Seg //L (@Posn_n) (@Size (@Parent)))))
    (cond
     ((not (null? (intersection-n //D (@Make_Set (concat (my-map HEAD (@Proc_Calls //I)) (my-map HEAD (@Funct_Calls //I)))))))
      (@Fail "The statement being absorbed uses some of the definitions."))
     (#t
      (@Pass)))))
  ((= (@ST (@I)) //T_/While)
   (let ((//L (@Cs (@Parent)))
         (//I '())
         (//V (@Assigned (@Get_n (@I) 1))))
    (@Right)
    (set! //I (@Make //T_/Statements '() (@Sub_Seg //L (@Posn_n) (@Size (@Parent)))))
    (cond
     ((not (@Gen_Proper? //I (@AS_Type)))
      (@Fail "Cannot absorb a non-proper statement into a simple statement"))
     ((or (not (null? (intersection-n (@Assigned //I) (@Used //I)))) (not (null? (intersection-n (@Stat_Types //I) //Call_/Types_/Set))))
      (@Fail "A statement cannot be absorbed into a `While' if it could assign to variables it uses."))
     (#t
      ;The thing being assigned is OK, but is it compatible with the loop?
      ;Before we go on, we need to note what variables are used in it.
      (let ((//V (@Variables //I))
            (//B '()))
       (@Left)
       (set! //B (@Get_n (@I) 1))
       ;Check that the loop doesn't use the same variables as the other statement.
       ;Now check that the loop will be executed at least once.
       ;This is done by looking for an assertion before the loop.
       (cond
        ((not (null? (intersection-n //V (@Variables (@I)))))
         (@Fail "The loop used the same variables as the other statement."))
        ((or (not (@Left?)) (not (= (@ST (@Get_n (@Parent) (- (@Posn_n) 1))) //T_/Assert)))
         (@Fail "There is no assertion before the loop."))
        (#t
         (@Left)
         ;The assertion my imply that the loop executes at least once.
         (cond
          ((not (@Implies? (@Get_n (@I) 1) //B))
           (@Fail "The assertion does not imply that the loop executes."))
          (#t
           (@Pass))))))))))
  ((and (@Simple? (@I)) (not (= (@ST (@I)) //T_/Exit)))
   (@Fail "The selected statement must be non-simple, or an `EXIT' statement."))
  (#t
   (@Pass))))

(define (@Fully_Absorb_Right_Code //Data)
 (cond
  ((and (= (@Gen_Type (@Item)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (let ((//A/S-save //A/S))
  (set! //A/S (@AS_Type))
  (@Cut_Rest)
  (cond
   ((or (= (@ST (@I)) //T_/For) (= (@ST (@I)) //T_/Var) (= (@ST (@I)) //T_/While))
    (@Down_Last)
    (@Down_Last)
    (@Splice_After (@Buffer))
    (@Up)
    (@Up))
   ((= (@Spec_Type (@Item)) //T_/Where)
    (@Down)
    (@Down_Last)
    (@Splice_After (@Buffer))
    (@Up)
    (@Up))
   (#t
    (@Foreach_Terminal /foreach-fully_absorb_right-1 0 (@AS_Type) 1)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    ; If we have been merging into an `IF' statement, then we may be able to 
    ; simplify the result by aligning any nested `IF' statements. 
    ; BUT: this messes up Remove_Dummy_Loop and Double_To_Single_Loop 
    ; where we use multiple calls of Fully_Absorb_Right to make the loop 
    ; body reducible. Align_Nested_Statements can cause a later Absorb 
    ; to create multiple copies instead of just one! 
    (cond
     ((and #f (= (@Spec_Type (@Item)) //T_/Cond))
      (@Down_Last)
      (cond
       ((@Trans? //T/R_/Align_/Nested_/Statements)
        (@Trans //T/R_/Align_/Nested_/Statements "")))
      (@Up)))))
  (set! //A/S //A/S-save)))

