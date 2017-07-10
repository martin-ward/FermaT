;;; Scheme translation of WSL code
(define (/foreach-absorb_right-1 //Depth //A/S_/Type)
 (cond
  ((or (= //Depth 0) (and (= (@ST (@I)) //T_/Exit) (equal? (@V (@I)) //Depth)))
   (cond
    ((and (= (@ST (@I)) //T_/Exit) (> //Depth 0))
     (@Splice_Over (@Increment (@Buffer) //A/S //Depth 0)))
    ((= (@ST (@I)) //T_/Skip)
     (@Paste_Over (@Buffer)))
    ((or (and (= (@ST (@I)) //T_/Exit) (= //Depth 0)) (@Gen_Improper? (@I) //A/S))
     #t)
    ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (@Make_Name "Z")))
     #t)
    (#t
     (@Paste_After (@Buffer)))))))

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
(define (@Absorb_Right_Test)
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
   (@Right)
   (cond
    ((or (not (null? (intersection-n (@Assigned (@I)) (@Used (@I))))) (not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set))))
     (@Fail "A statement cannot be absorbed into a `For' if it could assign to variables it uses."))
    (#t
     ;The thing being assigned is OK, but is it compatible with the loop?
     ;Before we go on, we need to note what variables are used in it.
     (let ((//V (@Variables (@I))))
      (@Left)
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
          ((or (not (null? (intersection-n //V (@Variables (@Get_n (@I) 5))))) (member //L //V))
           (@Fail "The loop used the same variables as the other statement."))
          ((not (or (and (@True? (@Make 315 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 317 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1))))) (and (@True? (@Make 314 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 316 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1)))))))
           (@Fail "The loop won't definitely execute."))
          (#t
           (@Pass))))))))))
  ((= (@ST (@I)) //T_/Var)
   ;Check that the statement to be absorbed doesn't use any of the local variables.
   (let ((//V (@Assigned (@Get_n (@I) 1))))
    (@Right)
    (cond
     ((not (@Is_Proper?))
      (@Fail "Cannot absorb a non-proper statement into a simple statement"))
     ((or (not (null? (intersection-n //V (@Variables (@I))))) (not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set))))
      (@Fail "The statement being absorbed could use some of the local variables."))
     (#t
      (@Pass)))))
  ((= (@ST (@I)) //T_/Where)
   ;Check that the statement to be absorbed doesn't use any of the definitions.
   (let ((//D (@Make_Set (my-map @V1 (@Cs (@Get_n (@I) 2))))))
    (@Right)
    (cond
     ((not (null? (intersection-n //D (@Make_Set (concat (my-map HEAD (@Proc_Calls (@I))) (my-map HEAD (@Funct_Calls (@I))))))))
      (@Fail "The statement being absorbed uses some of the definitions."))
     (#t
      (@Pass)))))
  ((= (@ST (@I)) //T_/While)
   (@Right)
   (cond
    ((not (@Is_Proper?))
     (@Fail "Cannot absorb a non-proper statement into a simple statement"))
    ((or (not (null? (intersection-n (@Assigned (@I)) (@Used (@I))))) (not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set))))
     (@Fail "A statement cannot be absorbed into a `While' if it could assign to variables it uses."))
    (#t
     ;The thing being assigned is OK, but is it compatible with the loop?
     ;Before we go on, we need to note what variables are used in it.
     (let ((//V (@Variables (@I)))
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
          (@Pass)))))))))
  ((and (@Simple? (@I)) (not (= (@ST (@I)) //T_/Exit)))
   (@Fail "The selected statement must be non-simple, or an `EXIT' statement."))
  (#t
   (@Pass))))

(define (@Absorb_Right_Code //Data)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Right)
   (@Trans //T/R_/Absorb_/Left ""))
  (#t
   (@Absorb_Right_Code2 //Data))))

(define (@Absorb_Right_Code2 //Data)
 (let ((//A/S-save //A/S)
       (//O/K 0))
  (set! //A/S (@AS_Type))
  (@Right)
  (@Cut)
  (@Left)
  (cond
   ((and (= (@ST (@I)) //T_/Var) (= (@ST (@Buffer)) //T_/Var) (equal? (@Assigned (@Get_n (@I) 1)) (@Assigned (@Get_n (@Buffer) 1))))
    (@Down_Last)
    (@Down_Last)
    (@Paste_After (@Make //T_/Assignment '() (@Cs (@Get_n (@Buffer) 1))))
    (@Right)
    (@Splice_After (@Cs (@Get_n (@Buffer) 2)))
    (@Up)
    (@Up))
   ((or (= (@ST (@I)) //T_/For) (= (@ST (@I)) //T_/Var) (= (@ST (@I)) //T_/While))
    (@Down_Last)
    (@Down_Last)
    (@Paste_After (@Buffer))
    (@Up)
    (@Up))
   ((= (@ST (@I)) //T_/Where)
    (@Down)
    (@Down_Last)
    (@Paste_After (@Buffer))
    (@Up)
    (@Up))
   (#t
    (@Foreach_Terminal /foreach-absorb_right-1 0 (@AS_Type) 1)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    ;If we have been merging into an `IF' statement, then we may be able to
    ;simplify the result by aligning any nested `IF' statements.
    (cond
     ((and (= (@ST (@I)) //T_/Cond) (<= (@Size (@I)) //Max_/Cond_/Size))
      (@Down)
      ; to first guarded 
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (set! //O/K 1)
        ; check for comments + IF 
        ; with either nothing after the IF, or <=1 non-improper clause 
        ; in the IF 
        (@Down_Last)
        (@Down)
        ; to first statement 
        (while (and (@Right?) (= (@ST (@I)) //T_/Comment)) 
         (@Right))
        (cond
         ((not (= (@ST (@I)) //T_/Cond))
          (set! //O/K 0))
         ((not (@Right?))
          (set! //O/K 1))
         (#t
          (set! //O/K 2)
          ; one non-improper clause is allowable 
          (@Down)
          ; to first guarded in the inner IF 
          (cond
           ((not (@Gen_Improper? (@I) //A/S))
            (set! //O/K (- //O/K 1))))
          (while (and (> //O/K 0) (@Right?)) 
           (begin
            (@Right)
            (cond
             ((not (@Gen_Improper? (@I) //A/S))
              (set! //O/K (- //O/K 1))))))
          (@Up)
          ; back to inner IF 
         ))
        (@Up)
        (@Up)
        ; back to guarded 
        (cond
         ((> //O/K 0)
          (cond
           ((@Trans? //T/R_/Align_/Nested_/Statements)
            (@Trans //T/R_/Align_/Nested_/Statements ""))
           (#t
            ; Could be too big for Align_Nested 
           ))))
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0)))))
      (@Up)
      ; Back to the cond 
     ))))
  (set! //A/S //A/S-save)))

