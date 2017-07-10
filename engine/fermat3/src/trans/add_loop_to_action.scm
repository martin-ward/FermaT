;;; Scheme translation of WSL code
(define (/foreach-add_loop_to_action-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (- /n)))
   (@Paste_Over (@Make //T_/Exit (+ //Depth 1) '())))))

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
; Look for an action which is only called by one other action. 
; Insert a loop in the calling action and replace the calls by EXITs 
; plus a single call at the end. We can assume that Simplify_Action_System 
; has already been tried 
(define (@Add_Loop_To_Action_Test)
 (cond
  ((not (= (@ST (@I)) //T_/A_/S))
   (@Fail "Not an action system"))
  ((not (equal? (@System_Type (@I)) "Reg"))
   (@Fail "Action system is not regular"))
  (#t
   (@Pass))))

(define (@Add_Loop_To_Action_Code //Data)
 (@Edit)
 (let ((//Old_/Size 0)
       (//Size (@Size (@Get_n (@I) 2)))
       (//Leave_/Alone_/Names (@Parse_String //Data))
       (//A/S_/Type (@System_Type (@I)))
       (//N (@Size (@Get_n (@I) 2))))
  (let ((//A/S_/Name (@V (@Get_n (@I) 1)))
        (//A/S_/Type '())
        (//Starting_/Action 0)
        (/dispatch 0)
        (//Bodies (make-vector-eval //N '()))
        (//Names (make-vector-eval (+ //N 1) '()))
        (//Succs (make-vector-eval (+ //N 1) '()))
        (//Preds (make-vector-eval (+ //N 1) '()))
        (//Name2/Num (hash-table))
        (//Num_/Calls (hash-table))
        (//Leave_/Alone '())
        (/n-save /n)
        (/m 0)
        (//S '()))
   (set! /n 0)
   (set! //A/S_/Type (@System_Type (@I)))
   (display-list "Looking for an action to add a loop to")
   (display-list "Leave_Alone_Names = " (@Join ", " (my-map @N_String //Leave_/Alone_/Names)))
   ; Calculate Bodies, Names, Name2Num 
   ; Hash table Name2Num maps action names (keys) to action numbers   
   (let ((/-result- (@FD_Init  //N //Bodies //Names //Name2/Num)))
    (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Names (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Name2/Num (car /-result-)) (set! /-result- (cdr /-result-)))
   (set! //Leave_/Alone '())
   (for-in /name //Leave_/Alone_/Names 
    (set! //Leave_/Alone (cons (gethash //Name2/Num /name) //Leave_/Alone)))
   ; Find the starting action: 
   (set! //Starting_/Action (gethash //Name2/Num //A/S_/Name))
   (set! /dispatch (gethash //Name2/Num (@Make_Name "dispatch")))
   (let ((/-result- (@SAS_Succs_And_Preds  //N //Bodies //Starting_/Action //Succs //Preds //Num_/Calls)))
    (set! //Succs (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Preds (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //Num_/Calls (car /-result-)) (set! /-result- (cdr /-result-)))
   ; Look for an action which is only called by one other action: 
   (set! /n 1)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((and (not (equal? /n //Starting_/Action)) (= (gen-length (wsl-ref //Preds /n)) 1) (not (equal? (car (wsl-ref //Preds /n)) /n)) (not (equal? (car (wsl-ref //Preds /n)) /dispatch)) (not (equal? /n /dispatch)) (not (and (member //T_/D_/If (@Stat_Types (wsl-ref //Bodies (car (wsl-ref //Preds /n))))) (member (@Make_Name "entry_point") (@Variables (wsl-ref //Bodies (car (wsl-ref //Preds /n))))))))
      (set! /fl_flag1 1))
     (#t
      (set! /n (+ /n 1))
      (cond
       ((> /n //N)
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0))))))
   (cond
    ((<= /n //N)
     (set! /m (car (wsl-ref //Preds /n)))
     (display-list "Unfolding " (@N_String (wsl-ref //Names /n)) " into " (@N_String (wsl-ref //Names /m)))
     (@New_Program (wsl-ref //Bodies /m))
     (@Foreach_Statement /foreach-add_loop_to_action-1 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! //S (@Cs (@Program)))
     (@Paste_Over (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() //S))))))
     (@Down)
     ; to the Floop 
     (@Splice_After (@Cs (wsl-ref //Bodies /n)))
     (wsl-set! //Bodies (@Program) /m)
     (wsl-set! //Bodies '() /n)))
   (@FD_Rebuild_AS //N //Bodies //Names //A/S_/Name '())
   (cond
    ((@Syntax_OK? (@Program))
     (display-list "Syntax is OK."))
    (#t
     (display-list "Syntax NOT OK!")))
   (set! /n /n-save)))
 (@End_Edit)
 (display-list "Done"))

#t
