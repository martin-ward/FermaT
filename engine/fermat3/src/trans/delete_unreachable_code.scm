;;; Scheme translation of WSL code
(define (/foreach-delete_unreachable_code-1 //Depth //A/S_/Type)
 (cond
  ((and (@Gen_Improper? (@Item) (@AS_Type)) (@Right?))
   (@Pass)))
 (cond
  ((and (not (@Passed?)) (member (@Spec_Type (@Item)) (list //T_/Cond //T_/D_/If //T_/D_/Do)) (<= (@Size (@Item)) 20))
   (let ((//D (@Make //T_/False '() '()))
         (//Con '())
         (//B '())
         (//Type (@Spec_Type (@Item))))
    (@Down)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((not (@Cs? (@I)))
       (set! /fl_flag1 1))
      (#t
       (set! //Con (@Simplify_Cond (@Get_n (@Item) 1)))
       (cond
        ((= (@Spec_Type //Con) //T_/False)
         (@Pass))
        ((= //Type //T_/Cond)
         (cond
          ((@Implies? //Con //D)
           (cond
            ((@Right?)
             (@Pass))
            (#t
             (@Down_Last)
             (let ((/__/O/K 1))
              (set! /__/O/K (@New_Match  /%const__delete_unreachable_code__1 (@I) /__/O/K))
              (cond
               ((not (= /__/O/K 1))
                (@Pass))))))
           (@Up))
          (#t
           (set! //D (@Or //D //Con))))))
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0))))))
    (@Up)))))

(define (/foreach-delete_unreachable_code-2 //Depth //A/S_/Type)
 (@Down)
 ;IF MEMBER?(@Spec_Type(@Item),<T_Cond,T_D_If,T_D_Do>)
;	     THEN @Delete_Unreachable_Cases() FI
 (while (and (or (not (@Gen_Improper? (@Item) //A/S/T/Y/P/E)) (not (@Right?))) (@Right?)) 
  (begin
   (@Right)
   ;IF MEMBER?(@Spec_Type(@Item),<T_Cond,T_D_If,T_D_Do>)
;	     THEN @Delete_Unreachable_Cases() FI
  ))
 (cond
  ((and (@Gen_Improper? (@Item) //A/S/T/Y/P/E) (@Right?))
   (@Delete_Rest)
   (display-list-flush "x"))))

(define /%const__delete_unreachable_code__1 (@Make 17 '() (list (@Make 145 '() '()))))
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
(define (@Delete_Unreachable_Code_Test)
 (let ((//Orig_/Pos (@Posn))
       (//A/S/Type '()))
  (cond
   ((and (not (@Passed?)) (< (@Stat_Count (@Item)) 100))
    (cond
     ((= (@Spec_Type (@Item)) //T_/A_/S)
      (@Down_Last)))
    (set! //A/S/Type (@AS_Type))
    (@Ateach_Statement /foreach-delete_unreachable_code-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (cond
   ((not (@Passed?))
    (@Fail "Nothing to delete")))))

(define (@Delete_Unreachable_Code_Code //Data)
 (let ((//Orig_/Pos (@Posn)))
  (cond
   ((= (@Spec_Type (@Item)) //T_/A_/S)
    (let ((//A/S_/Size (@Size (@Get_n (@I) 2)))
          (//Unreachable '())
          (//Actions '()))
     (@Down_Last)
     (@Down_Last)
     (for //I //A/S_/Size 1 (- 1) 
      (begin
       (cond
        ((not (member //I //Unreachable))
         (set! //Actions (cons (@Item) //Actions)))
        (#t
         (display-list-flush "x")))
       (@Left)))
     (@Up)
     (@Paste_Over (@Make //T_/Actions '() //Actions)))))
  (cond
   ((= (@Spec_Type (@Item)) //T_/A_/S)
    (@Down_Last)))
  (let ((//A/S/T/Y/P/E-save //A/S/T/Y/P/E))
   (set! //A/S/T/Y/P/E (@AS_Type))
   (@Edit)
   (@Foreach_Stats /foreach-delete_unreachable_code-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (cond
    ((member (@Spec_Type (@Item)) (list //T_/Cond //T_/D_/If //T_/D_/Do))
     (@Delete_Unreachable_Cases)))
   (@End_Edit)
   (@Goto //Orig_/Pos)
   (set! //A/S/T/Y/P/E //A/S/T/Y/P/E-save))))

(define (@Delete_Unreachable_Cases)
 (let ((//D (@Make //T_/False '() '()))
       (//Con '())
       (//P '())
       (//Pos (@Posn))
       (//Type (@Spec_Type (@Item))))
  (@Down)
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (set! //Con (@Simplify_Cond (@Get_n (@Item) 1)))
    (cond
     ((= (@Spec_Type //Con) //T_/False)
      (set! //P (@Posn))
      (@Clever_Delete)
      (display-list-flush "x")
      (@Goto //P)
      (set! /fl_flag1 0))
     ((= //Type //T_/Cond)
      (cond
       ((@Implies? //Con //D)
        (@Delete)
        (display-list-flush "x")
        (@Fixup)
        (set! /fl_flag1 0))
       (#t
        (set! //D (@Or //D //Con))
        (cond
         ((and (= (@Spec_Type //D) //T_/True) (@Right?))
          (@Delete_Rest)
          (display-list-flush "x")
          (@Fixup)
          (set! /fl_flag1 1))
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0))))))
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))
    (cond
     ((= /fl_flag1 0)
      (cond
       ((not (@Valid_Posn? (@Program) (@Posn)))
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0)))))))
  (@Goto //Pos)))

#t
