;;; Scheme translation of WSL code
(define (/foreach-simple_slice-1 //Depth //A/S_/Type)
 (cond
  ((not (= (@ST (@I)) //T_/Var_/Lvalue))
   (@Fail "All assignments must be to simple variables."))))

(define (/foreach-simple_slice-2 //Depth //A/S_/Type)
 (cond
  ((or (and (= (@ST (@I)) //T_/Assignment) (> (@Size (@I)) 1)) (and (= (@ST (@I)) //T_/Var) (> (@Size (@Get_n (@I) 1)) 1)))
   (@Fail "Statement contains a parallel assignment."))))

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
(define (@Simple_Slice_Test)
 (cond
  ((and (not (= (@GT (@I)) //T_/Statements)) (not (= (@GT (@I)) //T_/Statement)))
   (@Fail "Can only slice statements."))
  ((@Set_Subset? (@Stat_Types (@I)) (@Make_Set (list //T_/Cond //T_/D_/If //T_/While //T_/Assignment //T_/Var //T_/Assert //T_/Skip //T_/Abort //T_/Comment //T_/Spec)))
   (@Foreach_Lvalue /foreach-simple_slice-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Statement /foreach-simple_slice-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (cond
    ((not (@Failed?))
     (@Pass))))
  (#t
   (@Fail "The current item contains a statement which cannot be sliced."))))

(define (@Simple_Slice_Code //Data)
 (let ((/x-save /x)
       (//I-save //I)
       (/bottom-save /bottom))
  (set! /x (my-map @Make_Name (@Split //Data)))
  (set! //I (@I))
  (set! /bottom (@Make //T_/Variable (@Make_Name "BOTTOM") '()))
  (display-list "Simple Slice, initial variables are: " (@Join " " (my-map @N_String /x)))
  (@Slice)
  (@Paste_Over //I)
  (display-list "Simple Slice,   final variables are: " (@Join " " (my-map @N_String /x)))
  (set! /x /x-save)
  (set! //I //I-save)
  (set! /bottom /bottom-save)))

; Implements: <I, x> := <I', x'>.(I' slice{x'}{x} I) 
(define (@Slice)
 (cond
  ((= (@GT //I) //T_/Statements)
   (let ((//L '()))
    (for-in //I (reverse (@Cs //I)) 
     (begin
      (@Slice)
      (set! //L (cons //I //L))))
    (set! //I (@Make //T_/Statements '() //L))))
  ((= (@GT //I) //T_/Guarded)
   (let ((//B (@Get_n //I 1)))
    (set! //I (@Get_n //I 2))
    (@Slice)
    (set! /x (union-n /x (@Used //B)))
    (set! //I (@Make //T_/Guarded '() (list //B //I)))))
  ((= (@ST //I) //T_/Abort)
   (set! /x '()))
  ((and (= (@GT //I) //T_/Statement) (null? (intersection-n /x (@Assigned //I))))
   (set! //I (@Skip)))
  ((or (= (@ST //I) //T_/Assignment) (= (@ST //I) //T_/Spec))
   (set! /x (union-n (@Set_Difference /x (@Assigned //I)) (@Used //I))))
  ((= (@ST //I) //T_/Var)
   (let ((/assign (@Get_n (@Get_n //I 1) 1)))
    (let ((/v (@V (@Get_n /assign 1)))
          (/e (@Used (@Get_n /assign 2)))
          (/x0 /x))
     (set! //I (@Get_n //I 2))
     (@Slice)
     (cond
      ((not-member /v /x)
       (set! /assign (@Make //T_/Assign '() (list (@Get_n /assign 1) /bottom)))))
     (set! /x (union-n (@Set_Difference /x (list /v)) (intersection-n (list /v) /x0) /e))
     (set! //I (@Make //T_/Var '() (list (@Make //T_/Assigns '() (list /assign)) //I))))))
  ((or (= (@ST //I) //T_/Cond) (= (@ST //I) //T_/D_/If))
   (let ((/x1 '())
         (/x0 /x)
         (//G '())
         (//S/T (@ST //I)))
    (for-in /guard (@Cs //I) 
     (begin
      (set! //I /guard)
      (set! /x /x0)
      (@Slice)
      (set! //G (cons //I //G))
      (set! /x1 (union-n /x1 /x))))
    (set! /x /x1)
    (set! //I (@Make //S/T '() (reverse //G)))))
  ((= (@ST //I) //T_/While)
   ; Keep processing the body and adding the original vars 
   ; plus the vars in the condition, until the result converges 
   (let ((//B (@Get_n //I 1))
         (//I0 (@Get_n //I 2))
         (/x1 (union-n /x (@Used (@Get_n //I 1)))))
    (set! //I //I0)
    (set! /x /x1)
    (@Slice)
    (while (not (@Set_Subset? /x /x1)) 
     (begin
      (set! /x1 (union-n /x1 /x))
      (set! //I //I0)
      (set! /x /x1)
      (@Slice)))
    (set! //I (@Make //T_/While '() (list //B //I)))
    (set! /x /x1)))
  (#t
   (error "Unexpected type: " (@Type_Name (@ST //I))))))

; Slices the given item with the given slicing criterion 
; (set of final variables required). 
; Returns the sliced program plus new slicing criterion 
; (the set of input variables required) 
(define (@Slice_orig //I-par //X)
 (let ((//I-save //I)
       (//R '())
       (/new '())
       (/new/X '())
       (funct-result '()))
  (set! //I //I-par)
  (cond
   ((= (@ST //I) //T_/Statements)
    (for-in //S (reverse (@Cs //I)) 
     (begin
      (set! //R (@Slice_orig //S //X))
      (set! /new (cons (wsl-ref //R 1) /new))
      (set! //X (wsl-ref //R 2))))
    (set! //R (list (@Make //T_/Statements '() /new) //X)))
   ((= (@ST //I) //T_/Abort)
    (set! //R (list //I '())))
   ((and (= (@GT //I) //T_/Statement) (null? (intersection-n //X (@Assigned //I))))
    (set! //R (list (@Skip) //X)))
   ((= (@ST //I) //T_/Assignment)
    (set! //R (list //I (union-n (@Set_Difference //X (@Assigned //I)) (@Used //I)))))
   ((or (= (@ST //I) //T_/Cond) (= (@ST //I) //T_/D_/If))
    (for-in /guard (@Cs //I) 
     (begin
      (set! //R (@Slice_orig (@Get_n /guard 2) //X))
      (set! /new (cons (@Make //T_/Guarded '() (list (@Get_n /guard 1) (wsl-ref //R 1))) /new))
      (set! /new/X (union-n /new/X (@Variables (@Get_n /guard 1)) (wsl-ref //R 2)))))
    (set! //R (list (@Make (@ST //I) '() (reverse /new)) /new/X)))
   ((= (@ST //I) //T_/While)
    ; Keep processing the body and adding the original vars 
    ; plus the vars in the condition, until the result converges 
    (let ((//B (@Variables (@Get_n //I 1)))
          (//S-save //S)
          (/new/X //X))
     (set! //S (@Get_n //I 2))
     (set! //R (@Slice_orig //S /new/X))
     (wsl-set! //R (union-n (wsl-ref //R 2) /new/X //B) 2)
     (while (not (equal? /new/X (wsl-ref //R 2))) 
      (begin
       (set! /new/X (wsl-ref //R 2))
       (set! //R (@Slice_orig //S /new/X))
       (wsl-set! //R (union-n (wsl-ref //R 2) /new/X //B) 2)))
     (set! //S //S-save))
    (set! //R (list (@Make //T_/While '() (list (@Get_n //I 1) (wsl-ref //R 1))) (wsl-ref //R 2))))
   ((= (@ST //I) //T_/Var)
    (let ((/v (@Get_n (@Get_n (@Get_n //I 1) 1) 1))
          (/e (@Get_n (@Get_n (@Get_n //I 1) 1) 2))
          (//S-save //S)
          (/new/X '()))
     (set! //S (@Get_n //I 2))
     (set! //R (@Slice_orig (@Get_n //I 2) (@Set_Difference //X (list (@V /v)))))
     (set! //S (wsl-ref //R 1))
     (set! /new/X (union-n (@Set_Difference (wsl-ref //R 2) (list (@V /v))) (intersection-n (list (@V /v)) //X)))
     (cond
      ((member (@V /v) (wsl-ref //R 2))
       (set! //R (list (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /e))))) (@Make 17 '() (list //S)))) (union-n /new/X (@Used /e)))))
      (#t
       (set! //R (list (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Make 207 (@Make_Name "BOTTOM") '()))))) (@Make 17 '() (list //S)))) /new/X))))
     (set! //S //S-save)))
   (#t
    (error "Unexpected type: " (@Type_Name (@ST //I)))))
  (set! funct-result //R)
  (set! //I //I-save)
  funct-result))

#t
