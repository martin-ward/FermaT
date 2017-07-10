;;; Scheme translation of WSL code
(define (/foreach-restore_local_vars-1 //Depth //A/S_/Type)
 (@Down)
 (cond
  ((and (= (@Size (@I)) 1) (= //T_/Assignment (@ST (@I))) (= //T_/Var_/Lvalue (@ST (@Get_n (@Get_n (@I) 1) 1))))
   (set! /pair (@RLV_Parse_Var (@N_String (@V (@Get_n (@Get_n (@I) 1) 1))) /prefix))
   (cond
    ((not (null? /pair))
     (@RLV_Process /prefix /pair)))))
 (while (@Right?) 
  (begin
   (@Right)
   (cond
    ((and (= (@Size (@I)) 1) (= //T_/Assignment (@ST (@I))) (= //T_/Var_/Lvalue (@ST (@Get_n (@Get_n (@I) 1) 1))))
     (set! /pair (@RLV_Parse_Var (@N_String (@V (@Get_n (@Get_n (@I) 1) 1))) /prefix))
     (cond
      ((not (null? /pair))
       (@RLV_Process /prefix /pair)))))))
 (@Up))

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
(define (@Restore_Local_Vars_Test)
 (@Pass))

(define (@Restore_Local_Vars_Code //Data)
 (let ((/prefix-save /prefix))
  (set! /prefix (@String //Data))
  (cond
   ((equal? /prefix "")
    (set! /prefix "var")))
  (set! /prefix (string-append /prefix "_"))
  ; We need to process from the top down: 
  (@Ateach_Stats /foreach-restore_local_vars-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /prefix /prefix-save)))

; Check that the var is of the form prefix ++ N ++ __ ++ x where n > 0 
; and x is the original local variable name. 
; If so, then return <N, x>, otherwise return < > 
(define (@RLV_Parse_Var /var /prefix-par)
 (let ((/prefix-save /prefix)
       (/p1 (string-length /prefix-par))
       (/p2 0)
       (//R '())
       (funct-result '()))
  (set! /prefix /prefix-par)
  (cond
   ((and (@Starts_With? /var /prefix) (>= (string-length /var) (+ /p1 4)))
    (set! /p2 /p1)
    (while (@Digit? (substr /var /p2 1)) 
     (set! /p2 (+ /p2 1)))
    (cond
     ((and (> /p2 /p1) (> (string-length /var) (+ /p2 2)) (equal? (substr /var /p2 2) "__"))
      ; var is in the right form 
      (set! //R (list (substr /var /p1 (- /p2 /p1)) (substr /var (+ /p2 2))))))))
  (set! funct-result //R)
  (set! /prefix /prefix-save)
  funct-result))

; Construct one local VAR structure 
(define (@RLV_Process /prefix-par /pair-par)
 (let ((/pair-save /pair)
       (/prefix-save /prefix))
  (set! /pair /pair-par)
  (set! /prefix /prefix-par)
  (let ((/assigns '())
        (//N (wsl-ref /pair 1))
        (/rename '())
        (/vars '())
        (/body '())
        (/p1 (@Posn_n))
        (/p2 0)
        (/p3 0))
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (set! /assigns (cons (@RLV_Assign (@Get_n (@I) 1) (wsl-ref /pair 2)) /assigns))
     (set! /vars (union-n (list (@V (@Get_n (@Get_n (@I) 1) 1))) /vars))
     (set! /rename (cons (list (@V (@Get_n (@Get_n (@I) 1) 1)) (@Make_Name (wsl-ref /pair 2))) /rename))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (cond
        ((not (and (= (@ST (@I)) //T_/Assignment) (= (@Size (@I)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (not-member (@V (@Get_n (@Get_n (@I) 1) 1)) /vars)))
         (set! /fl_flag1 1))
        (#t
         (set! /pair (@RLV_Parse_Var (@N_String (@V (@Get_n (@Get_n (@I) 1) 1))) /prefix))
         (cond
          ((or (null? /pair) (not (equal? (wsl-ref /pair 1) //N)))
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0)))))))))
   ; Determine the extent of the body (if any) 
   (set! /p2 (- (@Posn_n) 1))
   (@To (@Size (@Parent)))
   (while (and (>= (@Posn_n) /p2) (null? (intersection-n (@Variables (@I)) /vars))) 
    (@Left))
   (cond
    ((and (>= /p2 /p1) (> (@Posn_n) /p2))
     (set! /p3 (@Posn_n))
     ; assigns are p1..p2, body is p2+1..p3 
     (@To /p1)
     (@Edit_Parent)
     ; If the body is improper, then we can't create a VAR clause 
     (set! /body (@Make //T_/Statements '() (@Get_L (@Parent) (+ /p2 1) /p3)))
     (cond
      ((@Gen_Proper? /body (@AS_Type))
       ; Delete all but one statement of p1..p3 
       (for /i 1 (- /p3 /p1) 1 
        (@Delete))
       (@Paste_Over (@Make //T_/Var '() (list (@Make //T_/Assigns '() (reverse /assigns)) /body)))
       (@Down_To 2)
       (for-in /pair /rename 
        (@Rename (wsl-ref /pair 1) (wsl-ref /pair 2))))
      (#t
       (@Up)
       (for-in /pair /rename 
        (@Rename (wsl-ref /pair 1) (wsl-ref /pair 2)))))
     (@End_Edit))
    (#t
     (@To /p1))))
  (set! /pair /pair-save)
  (set! /prefix /prefix-save)))

; Construct an assign from the given assign plus new name for the variable: 
(define (@RLV_Assign /assign /var)
 
 (@Make //T_/Assign '() (list (@Make //T_/Var_/Lvalue (@Make_Name /var) '()) (@Get_n /assign 2))))

#t
