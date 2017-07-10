;;; Scheme translation of WSL code
(define (/foreach-wp-1 //Depth //A/S_/Type)
 (cond
  ((@LR_Equal? (@Get_n /assign 1) (@I))
   (@Paste_Over /tmp))))

(define (/foreach-wp-2 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /tmp)
   (@Paste_Over (@Get_n /assign 2)))))

(define (/foreach-wp-3 //Depth //A/S_/Type)
 (cond
  ((not (null? (gethash /new (@V (@I)))))
   (@Paste_Over (@Make (@ST (@I)) (gethash /new (@V (@I))) '())))))

(define (/foreach-wp-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) /var))
   (@Paste_Over /v))))

(define (/foreach-wp-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Primed_/Var) (equal? (@V (@I)) /var))
   (@Paste_Over /v))))

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
; Compute the Weakest Precondition of a program and a postcondition. 
(define (@WP //S //R)
 (let ((//W/P '()))
  (cond
   ((= (@GT //S) //T_/Statements)
    (set! //W/P //R)
    (for-in //S1 (reverse (@Cs //S)) 
     (set! //W/P (@WP //S1 //W/P))))
   ((= (@ST //S) //T_/Assert)
    (set! //W/P (@And (@Get_n //S 1) //R)))
   ((or (= (@ST //S) //T_/Comment) (= (@ST //S) //T_/Skip) (= (@ST //S) //T_/Print) (= (@ST //S) //T_/Prinflush))
    (set! //W/P //R))
   ((= (@ST //S) //T_/Abort)
    (set! //W/P (@Make //T_/False '() '())))
   ((= (@ST //S) //T_/Assignment)
    ; Simultaneously replace each variable by its value 
    ; Replace the vars by temps, then replace the temps by values 
    ; Consider the assignment <x := y, y := x> 
    (let ((/tmp-save /tmp)
          (/n 1))
     (set! /tmp '())
     (@Edit)
     (@New_Program //R)
     (for-in /assign (@Cs //S) 
      (begin
       (set! /tmp (@Make //T_/Variable (@Make_Name (string-append "*tmp*" (@String /n))) '()))
       (set! /n (+ /n 1))
       (@Foreach_Expn /foreach-wp-1 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))))
     (set! /n 1)
     (for-in /assign (@Cs //S) 
      (begin
       (set! /tmp (@Make //T_/Variable (@Make_Name (string-append "*tmp*" (@String /n))) '()))
       (set! /n (+ /n 1))
       (@Foreach_Expn /foreach-wp-2 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))))
     (set! //W/P (@Program))
     (@Undo_Edit)
     (set! /tmp /tmp-save)))
   ((= (@ST //S) //T_/Cond)
    (let ((//B (@Make //T_/False '() '())))
     (set! //W/P (@Make //T_/True '() '()))
     (for-in /guard (@Cs //S) 
      (begin
       (set! //W/P (@And //W/P (@Or //B (@Or (@Not (@Get_n /guard 1)) (@WP (@Get_n /guard 2) //R)))))
       (set! //B (@Or //B (@Get_n /guard 1)))))))
   ((= (@ST //S) //T_/D_/If)
    (set! //W/P (@Make //T_/True '() '()))
    (let ((//B (@Make //T_/False '() '())))
     (for-in /guard (@Cs //S) 
      (begin
       (set! //W/P (@And //W/P (@Or (@Not (@Get_n /guard 1)) (@WP (@Get_n /guard 2) //R))))
       (set! //B (@Or //B (@Get_n /guard 1)))))
     (set! //W/P (@And //B //W/P))))
   ((= (@ST //S) //T_/Var)
    ; Rename the local vars in the body with new temp vars 
    ; Then process the initial assignment and the body as a sequence 
    (let ((/new-save /new)
          (/assigns '())
          (/n 0)
          (/used (@Used (@Get_n //S 2)))
          (/name '()))
     (set! /new (hash-table))
     (for-in /assign (@Cs (@Get_n //S 1)) 
      (begin
       (set! /n 0)
       (set! /name (@Make_Name (string-append "tmp__" (@N_String (@V (@Get_n /assign 1))))))
       (while (member /var /used) 
        (begin
         (set! /n (+ /n 1))
         (set! /name (@Make_Name (concat (string-append "tmp__" (@N_String (@V (@Get_n /assign 1)))) (@String /n))))))
       (puthash /new (@V (@Get_n /assign 1)) /name)
       (set! /assigns (cons (@Make //T_/Assign '() (list (@Make //T_/Var_/Lvalue /name '()) (@Get_n /assign 2))) /assigns))))
     (@Edit)
     (@New_Program (@Get_n //S 2))
     (@Foreach_Global_Var /foreach-wp-3 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! //S (@Make //T_/Statements '() (cons (@Make //T_/Assignment '() (reverse /assigns)) (@Cs (@Program)))))
     (@Undo_Edit)
     (set! //W/P (@WP //S //R))
     (set! /new /new-save)))
   ((= (@ST //S) //T_/Spec)
    ; WP(x := x'.Q, R) is Ex'.Q AND Ax'(Q => R[x'/x]) 
    ; We use x__p for the (local) variables x' and assume that 
    ; these variables are not already in use. 
    (let ((//Q '())
          (/vars '())
          (/v-save /v))
     (set! /v '())
     (@Edit)
     (@New_Program //R)
     (for-in /var (@Assigned (@Get_n //S 1)) 
      (begin
       (set! /v (@Make //T_/Variable (@Make_Name (string-append (@N_String /var) "__p")) '()))
       (set! /vars (cons /v /vars))
       (@Foreach_Global_Var /foreach-wp-4 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))))
     (set! //R (@Program))
     (set! /vars (@Make //T_/Lvalues '() (reverse /vars)))
     (@New_Program (@Get_n //S 2))
     (for-in /var (@Assigned (@Get_n //S 1)) 
      (begin
       (set! /v (@Make //T_/Variable (@Make_Name (string-append (@N_String /var) "__p")) '()))
       (@Foreach_Expn /foreach-wp-5 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))))
     (set! //Q (@Program))
     (@Undo_Edit)
     (set! //W/P (@And (@Make //T_/Exists '() (list /vars //Q)) (@Make //T_/Forall '() (list /vars (@Implies //Q //R)))))
     (set! /v /v-save)))
   ((= (@ST //S) //T_/A_/Proc_/Call)
    (let ((/name '())
          (/count 1)
          (/assigns '())
          (/e '())
          (/vars '()))
     (for-in /v (@Variables //S) 
      (set! /vars (cons (@Make //T_/Variable /v '()) /vars)))
     (set! /vars (@Make //T_/Expressions '() /vars))
     (for-in /v (@Cs (@Get_n //S 3)) 
      (begin
       (set! /name (@Make_Name (concat (string-append (@N_String (@V (@Get_n //S 1))) "__") (@String /count))))
       (set! /count (+ /count 1))
       (set! /e (@Make //T_/X_/Funct_/Call '() (list (@Name /name) /vars)))
       (set! /assigns (cons (@Make //T_/Assign '() (list /v /e)) /assigns))))
     (set! //W/P (@WP (@Make //T_/Assignment '() /assigns) //R))))
   (#t
    (error (string-append "@WP is not defined for type " (@Type_Name (@ST //S))))))
  //W/P))

; Statement types on which @WP can be calculated: 
(set! //W/P_/Types_/Set (@Make_Set (list //T_/Cond //T_/D_/If //T_/Assignment //T_/Var //T_/Assert //T_/Skip //T_/Abort //T_/Spec //T_/Comment //T_/Print //T_/Prinflush //T_/A_/Proc_/Call)))
#t
