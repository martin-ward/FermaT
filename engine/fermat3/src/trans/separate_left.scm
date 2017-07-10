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
(define (@Separate_Left_Test)
 (let ((//S '())
       (/types '())
       (//F 0)
       (/assigned '())
       (//V '())
       (//Cond_/Vars '())
       (//S/T (@ST (@I)))
       (/bad_types (@Make_Set (list //T_/X_/Proc_/Call //T_/Proc_/Call //T_/M/W_/Proc_/Call)))
       (/funct_types (@Make_Set (list //T_/Funct_/Call //T_/X_/Funct_/Call //T_/M/W_/Funct_/Call))))
  (cond
   ((or (= //S/T //T_/Cond) (= //S/T //T_/D_/If))
    (@Down_Last)
    (@Down_Last)
    (@Down)
    ; to first statement in first guard 
    (set! //S (@I))
    (set! /types (@Stat_Types (@I)))
    (cond
     ((or (= (@ST //S) //T_/Skip) (not (null? (intersection-n /bad_types /types))))
      (@Fail "Won't take out skips, can't take out proc calls"))
     (#t
      (set! /assigned (@Assigned //S))
      (@Up)
      (@Up)
      ; back to first guard 
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (cond
        ((not (@Left?))
         (set! /fl_flag1 1))
        (#t
         (@Left)
         (@Down_Last)
         (@Down)
         ; to first statement of guard 
         (cond
          ((not (@Equal? (@I) //S))
           (@Fail "Nothing to take out")
           (set! /fl_flag1 1))
          (#t
           (@Up)
           (@To 1)
           (cond
            ((not (null? (intersection-n (@Spec_Types (@I)) /funct_types)))
             (@Fail "Function calls in condition"))
            ((not (null? (intersection-n (@Used (@I)) /assigned)))
             (cond
              ((and (= (@ST //S) //T_/Assignment) (@Invertible? (intersection-n (@Used (@I)) /assigned) (@Cs //S)))
               ; OK 
              )
              (#t
               (@Fail "Variable occurs in condition")))))
           (@Up)
           (cond
            ((@Failed?)
             (set! /fl_flag1 1))
            (#t
             (set! /fl_flag1 0))))))))))
    (cond
     ((not (@Failed?))
      (@Pass))))
   ((= //S/T //T_/Where)
    (@Down)
    (@Down)
    (let ((/calls (@Make_Set (concat (my-map HEAD (@Proc_Calls (@I))) (my-map HEAD (@Funct_Calls (@I)))))))
     (@Up)
     (@Right)
     (cond
      ((null? (intersection-n /calls (@Make_Set (my-map @V1 (@Cs (@I))))))
       (@Pass))
      (#t
       (@Fail "The statement uses procs or funct defined in the WHERE")))))
   ((= //S/T //T_/Var)
    (let ((/local (@Assigned (@Get_n (@I) 1)))
          (/used (@Used (@Get_n (@I) 1))))
     (@Down_Last)
     (@Down)
     (cond
      ((not (null? (intersection-n /local (@Variables (@I)))))
       (@Fail "The statement uses the local variables"))
      ((not (null? (intersection-n /used (@Assigned (@I)))))
       (@Fail "The statement assigns to variables needed for initalisation"))
      (#t
       (@Pass)))))
   ((or (= //S/T //T_/While) (= //S/T //T_/For))
    (@Down_Last)
    (@Down)
    (cond
     ((@Trans? //T/R_/Take_/Out_/Left)
      (@Pass))
     (#t
      (@Fail "Cannot take anything out"))))
   ((and (= //S/T //T_/Assignment) (> (@Size (@I)) 1))
    (let ((/vars '())
          (/first (@Get_n (@I) 1))
          (/rest (cdr (@Cs (@I))))
          (/assign '()))
     (for-in /assign /rest 
      (set! /vars (union-n (@Elts_Used /assign) /vars)))
     (cond
      ((@Elt_Clash_List? /vars (@Elts_Assigned /first))
       (@Fail "The variable assigned in the first assign is used elsewhere."))
      (#t
       (@Pass)))))
   (#t
    (@Fail "Cannot separate from this type of statement")))))

(define (@Separate_Left_Code //Data)
 (let ((/posn (@Posn))
       (//S/T (@ST (@I)))
       (//S '())
       (/assigned '())
       (/i 0)
       (/new '()))
  (cond
   ((or (= //S/T //T_/Cond) (= //S/T //T_/D_/If))
    (@Splice_Before (list (@Get (@I) (list 1 2 1))))
    (set! //S (@I))
    (@Right)
    (@Edit)
    ; Update conditions if necessary 
    (cond
     ((= (@ST //S) //T_/Assignment)
      (set! /assigned (@Assigned //S))
      (@Down)
      ; To first guard 
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (@Down)
        ; To cond 
        (cond
         ((not (null? (intersection-n (@Used (@I)) /assigned)))
          ; Replace each assigned variable with its inverse 
          (@Invert_All /assigned (@Cs //S))))
        (@Up)
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0)))))
      (@Up)))
    (for /i 1 (@Size (@I)) 1 
     (begin
      (@Goto (list /i 2 1))
      (@Delete)))
    (@End_Edit)
    (cond
     ((= //S/T //T_/Cond)
      (@Fix_Cond))
     (#t
      (@Fix_Dijkstra))))
   ((= //S/T //T_/Where)
    (@Splice_Before (list (@Get (@I) (list 1 1))))
    (@Right)
    (@Down)
    (@Down)
    (@Delete)
    (@Fixup)
    (@Goto /posn)
    (cond
     ((@Right?)
      (@Right))))
   ((or (= //S/T //T_/While) (= //S/T //T_/For) (= //S/T //T_/Var))
    (@Down_Last)
    (@Down)
    (set! //S (@I))
    (@Goto /posn)
    (@Splice_Before (list //S))
    (@Right)
    (@Down_Last)
    (@Down)
    (@Delete)
    (@Fixup)
    (@Goto /posn)
    (cond
     ((@Right?)
      (@Right))))
   ((= //S/T //T_/Assignment)
    (@Down)
    (@Cut)
    (@Up)
    (@Paste_Before (@Make //T_/Assignment '() (list (@Buffer))))
    (cond
     ((@Right?)
      (@Right))))))
 (cond
  ((@Trans? //T/R_/Separate_/Left)
   (@Trans //T/R_/Separate_/Left '()))))

