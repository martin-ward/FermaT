;;; Scheme translation of WSL code
(define (/foreach-actions_to_where-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (@Paste_Over (@Make //T_/Proc_/Call '() (list (@Name (@V (@I))) /empty/E /empty/L))))))

(define (/foreach-actions_to_where-2 //Depth //A/S_/Type)
 (cond
  ((and (= //Depth 0) (@Right?) (member //T_/Call (@Stat_Types (@I))))
   ; May need to protect the rest of the sequence 
   (cond
    ((<= (@Zero_TV_Count (@I) "Rec") 1)
     (@Trans //T/R_/Fully_/Absorb_/Right ""))
    (#t
     (@Right)
     (cond
      ((and (= (@ST (@I)) //T_/Cond) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Equal) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Variable) (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) /exit_flag))
       ; Already protected 
      )
      (#t
       (@Left)
       (@Cut_Rest)
       (@Paste_After //S)
       (@Right)
       (@Down)
       (@Down_To 2)
       ; to statement sequence 
       (@Down)
       (@Splice_Over (@Buffer))
       (@Up)
       (@Up))))))))

(define (/foreach-actions_to_where-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (cond
    ((equal? (@V (@I)) //Z)
     (@Paste_Over /assign)))
   (cond
    ((> //Depth 0)
     (@Paste_After (@Make //T_/Exit //Depth '())))))))

(define (/foreach-actions_to_where-4 //Depth //A/S_/Type)
 (cond
  ((member //Depth (@Gen_TVs (@I) //A/S/Type))
   (set! //N (+ //N 1)))))

(define /%const__actions_to_where__1 (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Make 207 (@Make_Name "exit_flag") '()) (@Make 205 0 '()))) (@Make 17 '() (list (@Make 145 '() '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
(define /%const__actions_to_where__2 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "exit_flag") '()) (@Make 205 1 '()))))))
(define /%const__actions_to_where__3 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "exit_flag") '()) (@Make 205 0 '()))))))
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
; Convert an action system to a WHERE clause. 
; Use the variable exit_flag to indicate whether the Z action has been called. 
(define (@Actions_To_Where_Test)
 (cond
  ((not (= (@Spec_Type (@Item)) //T_/A_/S))
   (@Fail "Not an action system."))
  (#t
   (@Pass))))

(define (@Actions_To_Where_Code //Data)
 (let ((//Z-save //Z)
       (/start (@V (@Get_n (@I) 1)))
       (/empty/E-save /empty/E)
       (/empty/L-save /empty/L)
       (/new '())
       (/action '())
       (/body '()))
  (set! //Z (@Make_Name "Z"))
  (set! /empty/E (@Make //T_/Expressions '() '()))
  (set! /empty/L (@Make //T_/Lvalues '() '()))
  (cond
   ((not (@Recursive_System? (@I)))
    (@AW_Make_Recursive)))
  ; Convert action calls to proc calls: 
  (@Foreach_Non_Action_Statement /foreach-actions_to_where-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Convert actions to procs: 
  (for-in /action (@Cs (@Get_n (@I) 2)) 
   (set! /new (cons (@Make //T_/Proc '() (list (@Name (@V (@Get_n /action 1))) /empty/L /empty/L (@Get_n /action 2))) /new)))
  (set! /body (@Make //T_/Statements '() (list (@Make //T_/Proc_/Call '() (list (@Name (@V (@Get_n (@I) 1))) /empty/E /empty/L)))))
  (@Paste_Over (@Make //T_/Where '() (list /body (@Make //T_/Definitions '() (reverse /new)))))
  #t
  (set! //Z //Z-save)
  (set! /empty/E /empty/E-save)
  (set! /empty/L /empty/L-save)))

; Convert the action system to a recursive action system 
; (ie eliminate CALL Z) by using exit_flag to indicate when Z was called 
; and interting protective IF statements where necessary. 
(define (@AW_Make_Recursive)
 (let ((/exit_flag-save /exit_flag)
       (//Z-save //Z)
       (//S-save //S)
       (/assign-save /assign))
  (set! /exit_flag (@Make_Name "exit_flag"))
  (set! //Z (@Make_Name "Z"))
  (set! //S /%const__actions_to_where__1)
  (set! /assign /%const__actions_to_where__2)
  (@Down_To 2)
  ; to list of actions 
  (@Ateach_Non_Action_Statement /foreach-actions_to_where-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Replace all the CALL Zs and ensure that any calls will 
  ; terminate any enclosing Floops: 
  (@Foreach_Non_Action_Statement /foreach-actions_to_where-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Up)
  ; Back to A_S 
  (@Paste_Before /%const__actions_to_where__3)
  (@Right)
  (set! /exit_flag /exit_flag-save)
  (set! //Z //Z-save)
  (set! //S //S-save)
  (set! /assign /assign-save)))

(define (@Zero_TV_Count //I //A/S/Type-par)
 (let ((//A/S/Type-save //A/S/Type)
       (//N-save //N)
       (funct-result '()))
  (set! //A/S/Type //A/S/Type-par)
  (set! //N 0)
  (@Edit)
  (@New_Program //I)
  (@Ateach_Terminal /foreach-actions_to_where-4 0 (@AS_Type) 1)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Undo_Edit)
  (set! funct-result //N)
  (set! //A/S/Type //A/S/Type-save)
  (set! //N //N-save)
  funct-result))

#t
