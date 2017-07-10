;;; Scheme translation of WSL code
(define (/foreach-substitute_and_delete-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) //N))
   (@Splice_Over //S))))

(define (/foreach-substitute_and_delete-2 //Depth //A/S_/Type)
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
(define (@Substitute_And_Delete_Test)
 (let ((//N-save //N))
  (set! //N '())
  (cond
   ((= (@ST (@I)) //T_/Action)
    (set! //N (@V (@Get_n (@I) 1)))
    (cond
     ((equal? (@V (@Get_n (@GParent) 1)) //N)
      (@Fail "It is invalid to delete the starting action."))
     (#t
      (cond
       ((@Called? //N (@I))
        (@Fail "The action is recursive."))
       (#t
        (@Pass))))))
   ((= (@ST (@I)) //T_/Proc)
    (set! //N (@V (@Get_n (@I) 1)))
    (cond
     ((@Proc_Called? //N (@I))
      (@Fail "Procedure is recursive"))
     (#t
      (@Pass))))
   ((= (@ST (@I)) //T_/Funct)
    (set! //N (@V (@Get_n (@I) 1)))
    (cond
     ((@Funct_Called? //N (@I))
      (@Fail "Function is recursive"))
     ((or (> (@Size (@Get_n (@I) 4)) 1) (not (= (@ST (@Get_n (@Get_n (@I) 4) 1)) //T_/Skip)))
      (@Fail "Function definition includes statements"))
     (#t
      (@Pass))))
   (#t
    (@Fail "Not an action, function or proc definition")))
  (@Goto //Orig_/Pos)
  (set! //N //N-save)))

(define (@Substitute_And_Delete_Code //Data)
 (let ((//Orig_/Pos-save //Orig_/Pos)
       (//N-save //N)
       (//S-save //S))
  (set! //Orig_/Pos (@Posn_n))
  (set! //N (@V (@Get_n (@I) 1)))
  (set! //S '())
  (cond
   ((= (@ST (@I)) //T_/Action)
    (set! //S (@Cs (@Get_n (@I) 2)))
    (@Up)
    (@Foreach_Statement /foreach-substitute_and_delete-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Down_To //Orig_/Pos)
    (@Clever_Delete))
   ((= (@ST (@I)) //T_/Proc)
    (@Up)
    (@Up)
    (@Edit)
    (@Expand_Proc_Calls //N)
    (@End_Edit)
    (@Down_Last)
    (@Down_To //Orig_/Pos)
    (@Clever_Delete))
   ((= (@ST (@I)) //T_/Funct)
    (@Up)
    (@Up)
    (@Edit)
    (@Expand_Funct_Calls //N)
    (@End_Edit)
    (@Foreach_Expn /foreach-substitute_and_delete-2 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Down_Last)
    (@Down_To //Orig_/Pos)
    (@Clever_Delete)))
  (set! //Orig_/Pos //Orig_/Pos-save)
  (set! //N //N-save)
  (set! //S //S-save)))

(define (@Expand_Proc_Calls //N-par)
 (let ((//N-save //N))
  (set! //N //N-par)
  (cond
   ((and (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) //N))
    (@Trans //T/R_/Expand_/Call ""))
   ((and (@Cs? (@I)) (@Has_Statements_Type? (@GT (@I))))
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (let ((//O/K 1))
       (cond
        ((= (@ST (@I)) //T_/Where)
         (@Down_Last)
         (@Down)
         (while (and (not (equal? (@V (@Get_n (@I) 1)) //N)) (@Right?)) 
          (@Right))
         (cond
          ((equal? (@V (@Get_n (@I) 1)) //N)
           (set! //O/K 0)))
         (@Up)
         (@Up)))
       (cond
        ((= //O/K 1)
         (@Expand_Proc_Calls //N))))
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)))
  (set! //N //N-save)))

(define (@Expand_Funct_Calls //N-par)
 (let ((//N-save //N))
  (set! //N //N-par)
  (cond
   ((and (= (@ST (@I)) //T_/Funct_/Call) (equal? (@V (@Get_n (@I) 1)) //N))
    (@Trans //T/R_/Expand_/Call "")))
  (cond
   ((@Cs? (@I))
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (let ((//O/K 1))
       (cond
        ((= (@ST (@I)) //T_/Where)
         (@Down_Last)
         (@Down)
         (while (and (not (equal? (@V (@Get_n (@I) 1)) //N)) (@Right?)) 
          (@Right))
         (cond
          ((equal? (@V (@Get_n (@I) 1)) //N)
           (set! //O/K 0)))
         (@Up)
         (@Up)))
       (cond
        ((= //O/K 1)
         (@Expand_Funct_Calls //N))))
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)))
  (set! //N //N-save)))

#t
