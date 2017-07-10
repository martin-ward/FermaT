;;; Scheme translation of WSL code
(define (/foreach-actions_to_procs-1 //Depth //A/S_/Type)
 (@Down)
 (while (and (not (@Is_Improper?)) (@Right?)) 
  (@Right))
 (cond
  ((@Is_Improper?)
   (cond
    ((@Right?)
     (@Delete_Rest)))))
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
(define (@Actions_To_Procs_Test)
 (cond
  ((not (= (@Spec_Type (@I)) //T_/A_/S))
   (@Fail "Not an action system."))
  (#t
   (let ((/start (@V (@Get_n (@I) 1))))
    (@Down_Last)
    (@Down)
    ; to first action 
    (let ((/as (@AS_Type)))
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (cond
        ((not (equal? (@V (@Get_n (@I) 1)) /start))
         (cond
          ((@ATP_PROCable? /as (@Calls (@I)) (@V (@Get_n (@I) 1)))
           (@Pass)
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0))))
        (#t
         (set! /fl_flag1 0)))
       (cond
        ((= /fl_flag1 0)
         (cond
          ((not (@Right?))
           (@Fail "No suitable actions.")
           (set! /fl_flag1 1))
          (#t
           (@Right)
           (set! /fl_flag1 0))))))))))))

(define (@Actions_To_Procs_Code //Data)
 ; First, convert actions which only call Z 
 (let ((//Z (@Make_Name "Z"))
       (/start (@V (@Get_n (@I) 1)))
       (/calls '())
       (/dispatch (@Make_Name "dispatch"))
       (//Leave_/Alone (@Make_Set (@Parse_String //Data)))
       (/dispatch_calls (@ATP_Dispatch_Calls (@I)))
       (/as (@AS_Type))
       (/change-save /change)
       (/found 0)
       (/orig '()))
  (set! /change 0)
  (display-list "Actions_To_Procs Leave_Alone = " //Data)
  (cond
   ((@Regular_System? (@I))
    (@Ateach_Stats /foreach-actions_to_procs-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (set! /as (@AS_Type))
  (set! /found 0)
  (set! /orig (@I))
  (@Down_Last)
  (@Down)
  ; First deal with the actions which only call Z 
  ; NB: removing some actions may make others suitable, hence the outer loop 
  (display-list "Finding actions which only call Z...")
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    (set! /change 0)
    (@To 1)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((or (equal? (@V (@Get_n (@I) 1)) /start) (equal? (@V (@Get_n (@I) 1)) /dispatch) (member (@V (@Get_n (@I) 1)) /dispatch_calls))
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0))))
      (#t
       (set! /calls (@Calls (@I)))
       (cond
        ((and (= (gen-length /calls) 1) (equal? (wsl-ref (wsl-ref /calls 1) 1) //Z) (<= (@Stat_Count (@I)) 2))
         (set! /change (@ATP_SAD  /change))
         (set! /fl_flag1 0))
        ((and (= (gen-length /calls) 1) (equal? (wsl-ref (wsl-ref /calls 1) 1) //Z) (@Trans? //T/R_/Make_/Proc))
         (@Trans //T/R_/Make_/Proc "")
         (set! /change (@ATP_SAD  /change))
         (set! /fl_flag1 0))
        ((and (= (gen-length /calls) 1) (equal? (wsl-ref (wsl-ref /calls 1) 1) (@V (@Get_n (@I) 1))) (@Trans? //T/R_/Make_/Proc))
         (@Trans //T/R_/Make_/Proc "")
         (set! /change (@ATP_SAD  /change))
         (set! /fl_flag1 0))
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0))))))
    (cond
     ((= /change 0)
      (set! /fl_flag2 1))
     (#t
      (set! /found 1)
      (set! /fl_flag2 0)))))
  ; Next, actions which call one other action, also in a loop: 
  ; Only do this if there were no actions which only call Z 
  (cond
   ((= /found 0)
    (display-list "Finding actions which call one other action...")
    (set! /fl_flag2 0)
    (while (= /fl_flag2 0) 
     (begin
      (set! /change 0)
      (@To 1)
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (set! /calls (@Calls (@I)))
        (cond
         ((and (not (equal? (@V (@Get_n (@I) 1)) /start)) (not-member (@V (@Get_n (@I) 1)) //Leave_/Alone) (= (gen-length /calls) 1) (null? (intersection-n (@Make_Set (my-map HEAD /calls)) //Leave_/Alone)) (@ATP_PROCable? /as /calls (@V (@Get_n (@I) 1))) (@Trans? //T/R_/Make_/Proc))
          (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " only calls " (@N_String (wsl-ref (wsl-ref /calls 1) 1)))
          (cond
           ((and (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Comment) (= (@ST (@Get_n (@Get_n (@I) 2) 2)) //T_/Call))
            ; Don't make a proc from a single comment 
           )
           (#t
            (@Trans //T/R_/Make_/Proc "")))
          (set! /change (@ATP_SAD  /change))))
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0)))))
      (cond
       ((= /change 0)
        (set! /fl_flag2 1))
       (#t
        (set! /found 1)
        (set! /fl_flag2 0)))))))
  (cond
   ((= /found 0)
    (set! /change 0)
    (@To 1)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (set! /calls (@Calls (@I)))
      (cond
       ((and (not (equal? (@V (@Get_n (@I) 1)) /start)) (not-member (@V (@Get_n (@I) 1)) //Leave_/Alone) (null? (intersection-n (@Make_Set (my-map HEAD /calls)) //Leave_/Alone)) (@ATP_PROCable? /as /calls (@V (@Get_n (@I) 1))) (@Trans? //T/R_/Make_/Proc))
        (cond
         ((= (gen-length /calls) 0)
          (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " has no calls."))
         ((= (gen-length /calls) 2)
          (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " only calls " (@N_String (wsl-ref (wsl-ref /calls 1) 1)) " and " (@N_String (wsl-ref (wsl-ref /calls 2) 1))))
         (#t
          (display-list "Error in ATP_PROCable!!!")))
        (@Trans //T/R_/Make_/Proc "")
        (set! /change (@ATP_SAD  /change))))
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (while (not (= /change 0)) 
     (begin
      (set! /change 0)
      (@To 1)
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (set! /calls (@Calls (@I)))
        (cond
         ((and (not (equal? (@V (@Get_n (@I) 1)) /start)) (not-member (@V (@Get_n (@I) 1)) //Leave_/Alone) (null? (intersection-n (@Make_Set (my-map HEAD /calls)) //Leave_/Alone)) (@ATP_PROCable? /as /calls (@V (@Get_n (@I) 1))) (@Trans? //T/R_/Make_/Proc))
          (cond
           ((= (gen-length /calls) 0)
            (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " has no calls."))
           ((= (gen-length /calls) 2)
            (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " only calls " (@N_String (wsl-ref (wsl-ref /calls 1) 1)) " and " (@N_String (wsl-ref (wsl-ref /calls 2) 1))))
           (#t
            (display-list "Error in ATP_PROCable!!!")))
          (@Trans //T/R_/Make_/Proc "")
          (set! /change (@ATP_SAD  /change))))
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0)))))))))
  (while (and (@Up?) (not (= (@ST (@I)) //T_/A_/S))) 
   (@Up))
  (while (not (@Equal? /orig (@I))) 
   (begin
    (set! /as (@AS_Type))
    (set! /found 0)
    (set! /orig (@I))
    (@Down_Last)
    (@Down)
    ; First deal with the actions which only call Z 
    ; NB: removing some actions may make others suitable, hence the outer loop 
    (display-list "Finding actions which only call Z...")
    (set! /fl_flag2 0)
    (while (= /fl_flag2 0) 
     (begin
      (set! /change 0)
      (@To 1)
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (cond
        ((or (equal? (@V (@Get_n (@I) 1)) /start) (equal? (@V (@Get_n (@I) 1)) /dispatch) (member (@V (@Get_n (@I) 1)) /dispatch_calls))
         (cond
          ((not (@Right?))
           (set! /fl_flag1 1))
          (#t
           (@Right)
           (set! /fl_flag1 0))))
        (#t
         (set! /calls (@Calls (@I)))
         (cond
          ((and (= (gen-length /calls) 1) (equal? (wsl-ref (wsl-ref /calls 1) 1) //Z) (<= (@Stat_Count (@I)) 2))
           (set! /change (@ATP_SAD  /change))
           (set! /fl_flag1 0))
          ((and (= (gen-length /calls) 1) (equal? (wsl-ref (wsl-ref /calls 1) 1) //Z) (@Trans? //T/R_/Make_/Proc))
           (@Trans //T/R_/Make_/Proc "")
           (set! /change (@ATP_SAD  /change))
           (set! /fl_flag1 0))
          ((and (= (gen-length /calls) 1) (equal? (wsl-ref (wsl-ref /calls 1) 1) (@V (@Get_n (@I) 1))) (@Trans? //T/R_/Make_/Proc))
           (@Trans //T/R_/Make_/Proc "")
           (set! /change (@ATP_SAD  /change))
           (set! /fl_flag1 0))
          ((not (@Right?))
           (set! /fl_flag1 1))
          (#t
           (@Right)
           (set! /fl_flag1 0))))))
      (cond
       ((= /change 0)
        (set! /fl_flag2 1))
       (#t
        (set! /found 1)
        (set! /fl_flag2 0)))))
    ; Next, actions which call one other action, also in a loop: 
    ; Only do this if there were no actions which only call Z 
    (cond
     ((= /found 0)
      (display-list "Finding actions which call one other action...")
      (set! /fl_flag2 0)
      (while (= /fl_flag2 0) 
       (begin
        (set! /change 0)
        (@To 1)
        (set! /fl_flag1 0)
        (while (= /fl_flag1 0) 
         (begin
          (set! /calls (@Calls (@I)))
          (cond
           ((and (not (equal? (@V (@Get_n (@I) 1)) /start)) (not-member (@V (@Get_n (@I) 1)) //Leave_/Alone) (= (gen-length /calls) 1) (null? (intersection-n (@Make_Set (my-map HEAD /calls)) //Leave_/Alone)) (@ATP_PROCable? /as /calls (@V (@Get_n (@I) 1))) (@Trans? //T/R_/Make_/Proc))
            (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " only calls " (@N_String (wsl-ref (wsl-ref /calls 1) 1)))
            (cond
             ((and (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Comment) (= (@ST (@Get_n (@Get_n (@I) 2) 2)) //T_/Call))
              ; Don't make a proc from a single comment 
             )
             (#t
              (@Trans //T/R_/Make_/Proc "")))
            (set! /change (@ATP_SAD  /change))))
          (cond
           ((not (@Right?))
            (set! /fl_flag1 1))
           (#t
            (@Right)
            (set! /fl_flag1 0)))))
        (cond
         ((= /change 0)
          (set! /fl_flag2 1))
         (#t
          (set! /found 1)
          (set! /fl_flag2 0)))))))
    (cond
     ((= /found 0)
      (set! /change 0)
      (@To 1)
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (set! /calls (@Calls (@I)))
        (cond
         ((and (not (equal? (@V (@Get_n (@I) 1)) /start)) (not-member (@V (@Get_n (@I) 1)) //Leave_/Alone) (null? (intersection-n (@Make_Set (my-map HEAD /calls)) //Leave_/Alone)) (@ATP_PROCable? /as /calls (@V (@Get_n (@I) 1))) (@Trans? //T/R_/Make_/Proc))
          (cond
           ((= (gen-length /calls) 0)
            (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " has no calls."))
           ((= (gen-length /calls) 2)
            (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " only calls " (@N_String (wsl-ref (wsl-ref /calls 1) 1)) " and " (@N_String (wsl-ref (wsl-ref /calls 2) 1))))
           (#t
            (display-list "Error in ATP_PROCable!!!")))
          (@Trans //T/R_/Make_/Proc "")
          (set! /change (@ATP_SAD  /change))))
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0)))))
      (while (not (= /change 0)) 
       (begin
        (set! /change 0)
        (@To 1)
        (set! /fl_flag1 0)
        (while (= /fl_flag1 0) 
         (begin
          (set! /calls (@Calls (@I)))
          (cond
           ((and (not (equal? (@V (@Get_n (@I) 1)) /start)) (not-member (@V (@Get_n (@I) 1)) //Leave_/Alone) (null? (intersection-n (@Make_Set (my-map HEAD /calls)) //Leave_/Alone)) (@ATP_PROCable? /as /calls (@V (@Get_n (@I) 1))) (@Trans? //T/R_/Make_/Proc))
            (cond
             ((= (gen-length /calls) 0)
              (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " has no calls."))
             ((= (gen-length /calls) 2)
              (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " only calls " (@N_String (wsl-ref (wsl-ref /calls 1) 1)) " and " (@N_String (wsl-ref (wsl-ref /calls 2) 1))))
             (#t
              (display-list "Error in ATP_PROCable!!!")))
            (@Trans //T/R_/Make_/Proc "")
            (set! /change (@ATP_SAD  /change))))
          (cond
           ((not (@Right?))
            (set! /fl_flag1 1))
           (#t
            (@Right)
            (set! /fl_flag1 0)))))))))
    (while (and (@Up?) (not (= (@ST (@I)) //T_/A_/S))) 
     (@Up))))
  (display-list " ")
  (set! /change /change-save)))

; Substitute and delete current action, if possible, and set change := 1 
(define (@ATP_SAD /change-par)
 (let ((/change-save /change)
       (funct-result '()))
  (set! /change /change-par)
  (while (and (not (= (@Spec_Type (@I)) //T_/Action)) (@Up?)) 
   (@Up))
  (cond
   ((not (= (@Spec_Type (@I)) //T_/Action))
    (error "ERROR in Actions_To_Proc -- can't get back to the action!"))
   ((@Trans? //T/R_/Substitute_/And_/Delete)
    (@Trans //T/R_/Substitute_/And_/Delete "")
    (set! /change 1))
   (#t
    (@PP_Item (@I) 80 "")
    (display-list "Cannot Substitute_And_Delete: " (@N_String (@V (@Get_n (@I) 1))))
    (error "ERROR in Actions_To_Proc -- ")
    (@Right)))
  (set! funct-result /change)
  (set! /change /change-save)
  funct-result))

; An action is PROCable if: 
; (1) It calls no other actions, OR 
; (2) It calls Z and one other action, and the system is regular, OR 
; (3) It calls one other action, and the system is regular, OR 
; (4) It calls one other action, the action is regular, and 
;       all the calls to the other action are in terminal positions, OR 
; (5) It only calls itself and the system is recursive. 
(define (@ATP_PROCable? /as /calls /name)
 (let ((//O/K 0)
       (//Z (@Make_Name "Z")))
  (cond
   ((= (gen-length /calls) 0)
    (set! //O/K 1))
   ((and (= (gen-length /calls) 2) (equal? /as "Reg"))
    (cond
     ((or (and (equal? (wsl-ref (wsl-ref /calls 1) 1) //Z) (not (equal? (wsl-ref (wsl-ref /calls 2) 1) /name))) (and (equal? (wsl-ref (wsl-ref /calls 2) 1) //Z) (not (equal? (wsl-ref (wsl-ref /calls 1) 1) /name))))
      (set! //O/K 1))
     (#t
      (set! //O/K 0))))
   ((> (gen-length /calls) 1)
    (set! //O/K 0))
   ((and (equal? /as "Rec") (= (gen-length /calls) 1) (equal? (@V (@Get_n (@I) 1)) (wsl-ref (wsl-ref /calls 1) 1)))
    (set! //O/K 1))
   ((equal? (wsl-ref (wsl-ref /calls 1) 1) /name)
    (set! //O/K 0))
   ((equal? /as "Reg")
    (set! //O/K 1))
   ((and (@Regular? (@I)) (@SAS_Calls_Terminal? (wsl-ref (wsl-ref /calls 1) 1) /as))
    (set! //O/K 1))
   ((and (@Regular? (@I)) (@SAS_Calls_Terminal? (wsl-ref (wsl-ref /calls 1) 1) /as))
    (set! //O/K 1))
   (#t
    (set! //O/K 0)))
  (= //O/K 1)))

; Return the list of actions called from the dispatch action: 
(define (@ATP_Dispatch_Calls //I)
 (let ((//R '())
       (/dispatch (@Make_Name "dispatch")))
  (@Edit)
  (@New_Program //I)
  (@Down_Last)
  (@Down_Last)
  ; to last action 
  (while (and (not (= /dispatch (@ST (@I)))) (@Right?)) 
   (@Right))
  (cond
   ((= /dispatch (@ST (@I)))
    (set! //R (my-map HEAD (@Calls (@I))))))
  (@Undo_Edit)
  //R))

#t
