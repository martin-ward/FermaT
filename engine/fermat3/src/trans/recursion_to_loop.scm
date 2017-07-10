;;; Scheme translation of WSL code
(define (/foreach-recursion_to_loop-1 //Depth //A/S_/Type)
 (cond
  ((or (and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) //N)) (and (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) //N)))
   (set! //Calls (concat //Calls (list (@Posn)))))))

(define (/foreach-recursion_to_loop-2 //Depth //A/S_/Type)
 (cond
  ((or (and (= /type //T_/Action) (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) //N)) (and (= /type //T_/Proc) (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) //N)))
   (cond
    ((= (@ST /orig) //T_/Proc)
     (cond
      ((@Terminal_Posn? /orig (@Posn))
       (@Paste_Over (@Make //T_/Exit (+ //Depth 1) '())))))
    (#t
     (@Paste_Over (@Make //T_/Exit (+ //Depth 1) '())))))))

(define (/foreach-recursion_to_loop-3 //Depth //A/S_/Type)
 (@Down)
 (while (and (not (= //T_/Call (@ST (@I)))) (@Right?)) 
  (@Right))
 (cond
  ((= //T_/Call (@ST (@I)))
   (@Cut_Rest)
   (@Paste_Over (@Make //T_/Exit //Depth '())))))

(define (/foreach-recursion_to_loop-4 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (@Paste_After (@Make //T_/Exit //Depth '())))))

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
(define (@Recursion_To_Loop_Test)
 (cond
  ((= (@ST (@Item)) //T_/Action)
   (let ((//N-save //N))
    (set! //N (@V (@Get_n (@Item) 1)))
    (cond
     ((not (@Called? //N (@Item)))
      (@Fail "Action is not recursive"))
     ((@In_Reg_System?)
      (@Pass))
     ((and (@Tail_Recursive? //N) (= (gen-length (@Calls (@I))) 1))
      (@Pass))
     ((and (@Tail_Recursive? //N) (= (gen-length (@Calls (@I))) 2) (@Regular? (@I)) (@All_Tail_Recursive? //N (@Calls (@I))))
      (@Pass))
     (#t
      (@Fail "Cannot remove recursion")))
    (set! //N //N-save)))
  ((and (= (@ST (@Item)) //T_/Proc) (or (> (@Size (@Get_n (@I) 3)) 0) (> (@Size (@Get_n (@I) 2)) 0)))
   (@Fail "Procedure has parameters -- not yet implemented"))
  ((= (@ST (@Item)) //T_/Proc)
   (let ((//N-save //N))
    (set! //N (@V (@Get_n (@Item) 1)))
    (cond
     ((not-member //N (my-map HEAD (@Proc_Calls (@I))))
      (@Fail "Procedure is not recursive"))
     ((not (@Any_Tail_Recursive? //N))
      (@Fail "Procedure has no tail-recursive calls"))
     ((@Any_Call_In_Var? //N)
      (@Fail "A call is inside a VAR"))
     (#t
      (@Pass)))
    (set! //N //N-save)))
  (#t
   (@Fail "Selected item is not an action or proc"))))

(define (@Any_Call_In_Var? //N-par)
 (let ((//N-save //N)
       (//Valid (@Reachable_Calls //N-par))
       (/found 0)
       (funct-result '()))
  (set! //N //N-par)
  (while (and (not (null? //Valid)) (= /found 0)) 
   (begin
    (cond
     ((@Is_In_Var? (car //Valid))
      (set! /found 1)))
    (set! //Valid (cdr //Valid))))
  (set! funct-result (= /found 1))
  (set! //N //N-save)
  funct-result))

(define (@Is_In_Var? /posn)
 (let ((/orig-save /orig)
       (/bad 0)
       (/found 0)
       (funct-result '()))
  (set! /orig (@Posn))
  (while (and (= /found 0) (not (null? /posn))) 
   (begin
    (@Down_To (car /posn))
    (set! /posn (cdr /posn))
    (cond
     ((= (@ST (@I)) //T_/Var)
      (set! /found 1)))))
  (@Goto /orig)
  (set! funct-result (= /found 1))
  (set! /orig /orig-save)
  funct-result))

(define (@Any_Tail_Recursive? //N-par)
 (let ((//N-save //N)
       (//Valid (@Reachable_Calls //N-par))
       (//O/K 0)
       (funct-result '()))
  (set! //N //N-par)
  (while (and (not (null? //Valid)) (= //O/K 0)) 
   (begin
    (cond
     ((@Is_Terminal_Posn? (car //Valid))
      (set! //O/K 1)))
    (set! //Valid (cdr //Valid))))
  (set! funct-result (= //O/K 1))
  (set! //N //N-save)
  funct-result))

(define (@All_Tail_Recursive? //N-par //L)
 (let ((//N-save //N)
       (//O/K 1)
       (funct-result '()))
  (set! //N //N-par)
  (while (and (= //O/K 1) (not (null? //L))) 
   (cond
    ((and (not (equal? (wsl-ref (wsl-ref //L 1) 1) //N)) (= (wsl-ref (wsl-ref //L 1) 2) 1))
     ; A single extra call in a regular action is OK 
     ; because we can take the whole statement sequence 
     ; out of the loop 
     (set! //L (cdr //L)))
    ((not (@Tail_Recursive? (wsl-ref (wsl-ref //L 1) 1)))
     (set! //O/K 0))
    (#t
     (set! //L (cdr //L)))))
  (set! funct-result (= //O/K 1))
  (set! //N //N-save)
  funct-result))

(define (@Tail_Recursive? //N-par)
 (let ((//N-save //N)
       (//Valid (@Reachable_Calls //N-par))
       (//O/K 1)
       (funct-result '()))
  (set! //N //N-par)
  (@Down_Last)
  ; to action body 
  (while (and (not (null? //Valid)) (= //O/K 1)) 
   (begin
    (cond
     ((not (@Is_Terminal_Posn? (cdr (car //Valid))))
      (set! //O/K 0)))
    (set! //Valid (cdr //Valid))))
  (@Up)
  (set! funct-result (= //O/K 1))
  (set! //N //N-save)
  funct-result))

(define (@Reachable_Calls //N-par)
 (let ((//N-save //N)
       (//Calls-save //Calls)
       (//Valid '())
       (//H '())
       (//A/S/Type (@AS_Type))
       (funct-result '()))
  (set! //N //N-par)
  (set! //Calls '())
  (@Edit)
  (@Ateach_Statement /foreach-recursion_to_loop-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (while (not (null? //Calls)) 
   (begin
    (set! //H (car //Calls))
    (cond
     ((@Gen_Reachable? (@I) //H //A/S/Type)
      (set! //Valid (concat //Valid (list //H)))))
    (set! //Calls (cdr //Calls))))
  (@Undo_Edit)
  (set! funct-result //Valid)
  (set! //N //N-save)
  (set! //Calls //Calls-save)
  funct-result))

(define (@Recursion_To_Loop_Code //Data)
 (let ((//N-save //N)
       (//A/S/Type (@AS_Type))
       (//Calls-save //Calls)
       (/type-save /type)
       (/orig-save /orig))
  (set! //N (@V (@Get_n (@I) 1)))
  (set! //Calls '())
  (set! /type (@ST (@I)))
  (set! /orig (@I))
  (@Trans //T/R_/Delete_/Unreachable_/Code "")
  (@Edit)
  (@Down_Last)
  (@Paste_Over (@Increment (@I) //A/S/Type 2 0))
  (@Ateach_Statement /foreach-recursion_to_loop-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (let ((//S (@Cs (@I))))
   (cond
    ((or (= /type //T_/Proc) (equal? //A/S/Type "Reg"))
     (@Paste_Over (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() //S)))))))))))
    (#t
     (@Paste_Over (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (append //S (list (@Make 117 2 '()))))))))))))))))
  (@Down)
  ; to outer loop 
  (set! //Calls (@Calls (@I)))
  (cond
   ((and (= (gen-length //Calls) 1) (equal? //A/S/Type "Rec"))
    (@Foreach_Stats /foreach-recursion_to_loop-3 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Paste_After (@Make //T_/Call (car (car //Calls)) '()))
    (@Right)
    (@Splice_After (@Increment_List (@Buffer) //A/S/Type (- 2) 0)))
   (#t
    (@Foreach_Statement /foreach-recursion_to_loop-4 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (@End_Edit)
  (@Trans //T/R_/Delete_/All_/Skips "")
  (@Down_Last)
  (@Down)
  (@Trans //T/R_/Delete_/Unreachable_/Code "")
  (cond
   ((@Trans? //T/R_/Double_/To_/Single_/Loop)
    (@Trans //T/R_/Double_/To_/Single_/Loop "")))
  (cond
   ((@Trans? //T/R_/Separate_/Right)
    (@Trans //T/R_/Separate_/Right "")))
  (cond
   ((@Trans? //T/R_/Remove_/Dummy_/Loop)
    (@Trans //T/R_/Remove_/Dummy_/Loop "")))
  (@Up)
  (@Up)
  ; Back to outer loop 
  (cond
   ((@Trans? //T/R_/Separate_/Right)
    (@Trans //T/R_/Separate_/Right "")))
  (@Trans //T/R_/Delete_/All_/Skips "")
  (set! //N //N-save)
  (set! //Calls //Calls-save)
  (set! /type /type-save)
  (set! /orig /orig-save)))

