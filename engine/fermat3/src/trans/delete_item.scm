;;; Scheme translation of WSL code
(define /%const__delete_item__1 (@Make 205 1 '()))
(define /%const__delete_item__2 (@Make 205 0 '()))
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
(define (@Delete_Item_Test)
 (let ((//S/T (@ST (@I))))
  (cond
   ((= //S/T //T_/Action)
    (let ((//N (@V (@Get_n (@I) 1))))
     (cond
      ((equal? (@V (@Get_n (@GParent) 1)) //N)
       (@Fail "It is invalid to delete the starting action."))
      (#t
       (cond
        ((@Called? //N (@Parent))
         (@Fail "The action is called somewhere."))
        (#t
         (@Pass)))))))
   ((and (= //S/T //T_/Proc) (= (@Non_Recursive_Calls (@V (@Get_n (@I) 1)) (@GParent)) 0))
    (@Pass))
   ((and (or (= //S/T //T_/Funct) (= //S/T //T_/B/Funct)) (= (@Non_Recursive_Funct_Calls (@V (@Get_n (@I) 1)) (@GParent)) 0))
    (@Pass))
   ((= //S/T //T_/Assert)
    (@Pass))
   ((= //S/T //T_/Skip)
    (@Pass))
   ((= //S/T //T_/Comment)
    (@Pass))
   ((and (= //S/T //T_/Assignment) (= (@Size (@I)) 1) (@LR_Equal? (@Get_n (@Get_n (@I) 1) 1) (@Get_n (@Get_n (@I) 1) 2)))
    (@Pass))
   ((and (= //S/T //T_/While) (@False? (@Get_n (@I) 1)))
    (@Pass))
   ((= //S/T //T_/Guarded)
    (let ((//B (@Simplify_Cond (@Get_n (@I) 1)))
          (//A (@Make //T_/False '() '()))
          (//Cnd '())
          (//P '()))
     (cond
      ((= (@ST //B) //T_/False)
       (@Pass))
      (#t
       (set! //P (@Posn))
       (cond
        ((= (@ST (@Parent)) //T_/Cond)
         (while (@Left?) 
          (begin
           (@Left)
           (set! //Cnd (@Simplify_Cond (@Get_n (@I) 1)))
           (set! //A (@Or //A //Cnd))
           (cond
            ((or (@Implies? //B //Cnd) (= (@ST //A) //T_/True))
             (@Pass)))))
         (@Goto //P)))
       (cond
        ((not (@Passed?))
         (@Down_Last)
         (@Down)
         (set! /fl_flag1 0)
         (while (= /fl_flag1 0) 
          (cond
           ((@Failed?)
            (set! /fl_flag1 1))
           (#t
            (cond
             ((not (@Trans? //T/R_/Delete_/Item))
              (@Fail "Component cannot be deleted")))
            (cond
             ((not (@Right?))
              (set! /fl_flag1 1))
             (#t
              (@Right)
              (set! /fl_flag1 0))))))
         (@Goto //P)
         (cond
          ((not (@Failed?))
           (@Pass)))))))))
   ((= (@GT (@I)) //T_/Expression)
    (let ((//I_/One /%const__delete_item__1)
          (//I_/Zero /%const__delete_item__2))
     (cond
      ((or (and (@Equal? (@I) //I_/One) (or (= (@ST (@Parent)) //T_/Times) (and (@Left?) (= (@ST (@Parent)) //T_/Divide)))) (and (@Equal? (@I) //I_/Zero) (or (= (@ST (@Parent)) //T_/Plus) (and (= (@ST (@Parent)) //T_/Minus) (@Left?)))))
       (@Pass)))))
   ((= (@GT (@I)) //T_/Condition)
    (cond
     ((or (and (@False? (@I)) (= (@ST (@Parent)) //T_/Or)) (and (@True? (@I)) (= (@ST (@Parent)) //T_/And)))
      (@Pass))))
   (#t
    (cond
     ((= (@ST (@I)) //T_/For)
      (let ((//E1 (@Get_n (@I) 2))
            (//E2 (@Get_n (@I) 3))
            (//E3 (@Get_n (@I) 4)))
       ;Check whether the loop will be executed.
       (cond
        ((or (and (@True? (@Make 315 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 314 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1))))) (and (@True? (@Make 314 '() (list (@Var_To_Expn //E3) (@Make 205 0 '())))) (@True? (@Make 315 '() (list (@Var_To_Expn //E2) (@Var_To_Expn //E1))))))
         (@Pass))))))))
  (cond
   ((and (not (@Passed?)) (@Up?) (or (= (@GT (@I)) //T_/Statement) (= (@GT (@I)) //T_/Statements) (= (@ST (@I)) //T_/Guarded) (= (@ST (@I)) //T_/Assign)))
    (let ((//P (@Posn_n)))
     (@Up)
     (cond
      ((not (@Is_Reachable? (list //P)))
       (@Pass))))))
  (cond
   ((not (@Passed?))
    (@Fail "The item is not redundant")))))

(define (@Delete_Item_Code //Data)
 (cond
  ((= (@ST (@I)) //T_/Action)
   (@Delete))
  ((and (= (@ST (@I)) //T_/Guarded) (= (@ST (@Parent)) //T_/Cond))
   (let ((//B (@Get_n (@I) 1))
         (//P (@Posn))
         (//A (@Make //T_/False '() '()))
         (//Cnd '())
         (//O/K 0))
    (cond
     ((@False? //B)
      (@Clever_Delete))
     (#t
      (while (@Left?) 
       (begin
        (@Left)
        (set! //Cnd (@Simplify_Cond (@Get_n (@I) 1)))
        (set! //A (@Or //A //Cnd))
        (cond
         ((or (@Implies? //B //Cnd) (= (@ST //A) //T_/True))
          (set! //O/K 1)))))
      (@Goto //P)
      (cond
       ((= //O/K 1)
        (@Clever_Delete))
       (#t
        (@Down_Last)
        (@Delete)
        (@Up)
        (@Up)
        (@Fix_Cond)
        (@Goto //P)))))))
  ((= (@GT (@I)) //T_/Expression)
   (cond
    ((> (@Size (@Parent)) 2)
     (@Delete))
    (#t
     (cond
      ((@Left?)
       (@Left))
      (#t
       (@Right)))
     (let ((//Exp (@I)))
      (@Up)
      (@Paste_Over //Exp)))))
  ((= (@GT (@I)) //T_/Condition)
   (cond
    ((> (@Size (@Parent)) 2)
     (@Delete))
    (#t
     (cond
      ((@Left?)
       (@Left))
      (#t
       (@Right)))
     (let ((//Con (@I)))
      (@Up)
      (@Paste_Over //Con)))))
  (#t
   (@Clever_Delete))))

