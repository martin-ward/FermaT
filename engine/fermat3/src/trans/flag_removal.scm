;;; Scheme translation of WSL code
(define (/foreach-flag_removal-1 //Depth //A/S_/Type)
 (cond
  ((@Trans? //T/R_/Separate_/Right)
   (display-list "Separating right...")
   (@Trans //T/R_/Separate_/Right ""))))

(define (/foreach-flag_removal-2 //Depth //A/S_/Type)
 (@Down)
 (set! /assigned 0)
 (cond
  ((member /flag (@Assigned (@I)))
   (set! /assigned (@Posn_n)))
  ((and (member /flag (@UBA (@I))) (> /assigned 0))
   (let ((/-result- (@FR_Process1  /flag /assigned (@Posn_n) /change /unroll)))
    (set! /change (car /-result-)) (set! /-result- (cdr /-result-))
    (set! /unroll (car /-result-)) (set! /-result- (cdr /-result-)))))
 (while (@Right?) 
  (begin
   (@Right)
   (cond
    ((member /flag (@Assigned (@I)))
     (set! /assigned (@Posn_n)))
    ((and (member /flag (@UBA (@I))) (> /assigned 0))
     (let ((/-result- (@FR_Process1  /flag /assigned (@Posn_n) /change /unroll)))
      (set! /change (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /unroll (car /-result-)) (set! /-result- (cdr /-result-)))))))
 (@Up))

(define (/foreach-flag_removal-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/While)
   (set! /old (@I))
   (@Trans //T/R_/While_/To_/Floop ""))
  (#t
   (set! /old '())))
 (while (and (= (@ST (@I)) //T_/Floop) (@Trans? //T/R_/Absorb_/Left)) 
  (begin
   (set! /old '())
   (set! /change 1)
   (@Trans //T/R_/Absorb_/Left "")))
 (cond
  ((and (= (@ST (@I)) //T_/Floop) (not (null? /old)))
   (@Paste_Over /old))))

(define (/foreach-flag_removal-4 //Depth //A/S_/Type)
 (@Down)
 (set! /assigned 0)
 (cond
  ((member /flag (@Assigned (@I)))
   (set! /assigned (@Posn_n)))
  ((and (member /flag (@UBA (@I))) (> /assigned 0))
   (let ((/-result- (@FR_Process1  /flag /assigned (@Posn_n) /change /unroll)))
    (set! /change (car /-result-)) (set! /-result- (cdr /-result-))
    (set! /unroll (car /-result-)) (set! /-result- (cdr /-result-)))))
 (while (@Right?) 
  (begin
   (@Right)
   (cond
    ((member /flag (@Assigned (@I)))
     (set! /assigned (@Posn_n)))
    ((and (member /flag (@UBA (@I))) (> /assigned 0))
     (let ((/-result- (@FR_Process1  /flag /assigned (@Posn_n) /change /unroll)))
      (set! /change (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /unroll (car /-result-)) (set! /-result- (cdr /-result-)))))))
 (@Up))

(define (/foreach-flag_removal-5 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/While)
   (set! /old (@I))
   (@Trans //T/R_/While_/To_/Floop ""))
  (#t
   (set! /old '())))
 (while (and (= (@ST (@I)) //T_/Floop) (@Trans? //T/R_/Absorb_/Left)) 
  (begin
   (set! /old '())
   (set! /change 1)
   (@Trans //T/R_/Absorb_/Left "")))
 (cond
  ((and (= (@ST (@I)) //T_/Floop) (not (null? /old)))
   (@Paste_Over /old))))

(define (/foreach-flag_removal-6 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) (@V /var)))
   (cond
    ((not (= (@Posn_n) 1))
     (set! //O/K 0))
    (#t
     (@Up)
     (cond
      ((and (not (= (@ST (@I)) //T_/Equal)) (not (= (@ST (@I)) //T_/Not_/Equal)))
       (set! //O/K 0))
      ((and (not (= (@ST (@Get_n (@I) 2)) //T_/Number)) (not (= (@ST (@Get_n (@I) 2)) //T_/String)))
       (set! //O/K 0))
      (#t
       (set! /values (union-n (list (@V (@Get_n (@I) 2))) /values))))
     (@Down))))))

(define (/foreach-flag_removal-7 //Depth //A/S_/Type)
 (@Down)
 (set! /tested 0)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (begin
   (cond
    ((member /flag (@UBA (@I)))
     (set! /tested (@Posn_n))))
   (cond
    ((and (member /flag (@Assigned (@I))) (> /tested 0) (= /unroll 0))
     (set! /posn (@Posn))
     (while (and (@Up?) (and (not (= (@ST (@I)) //T_/While)) (not (= (@ST (@I)) //T_/Floop)))) 
      (@Up))
     (cond
      ((@Trans? //T/R_/Unroll_/Loop)
       (set! /unroll 1)
       (set! /change 1)
       (display-list "  ======== unrolling, flag = " (@N_String /flag))
       (@PP_Item (@I) 80 "")
       (@Trans //T/R_/Unroll_/Loop "")
       (set! /fl_flag1 1))
      (#t
       (@Goto /posn)
       (set! /fl_flag1 0))))
    (#t
     (set! /fl_flag1 0)))
   (cond
    ((= /fl_flag1 0)
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))))
 (@Up))

(define (/foreach-flag_removal-8 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) /flag))
   (set! /val (@Find_Value (list /flag)))
   (cond
    ((not (null? /val))
     (display-list-flush (@N_String /flag) " at " (@Posn) " has value = ")
     (@PP_Item /val 80 "")
     (@Paste_Over /val)
     (set! /change 1))))))

(define (/foreach-flag_removal-9 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Floop) (@Trans? //T/R_/Floop_/To_/While))
   (set! /done 1)
   (@Trans //T/R_/Floop_/To_/While "")))
 (cond
  ((and (= (@ST (@I)) //T_/While) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Cond) (member /flag (@Assigned (@Get_n (@Get_n (@I) 2) 1))) (= /unroll 0))
   (@Down_To 2)
   (@Down)
   (@Down)
   ; to first guarded 
   ; B is the condition we are building, 
   ; B1 is the condition under which this branch of the cond is reached 
   (set! //B (@Make //T_/False '() '()))
   (set! //B1 (@Make //T_/True '() '()))
   (set! //G '())
   (set! //G1 '())
   (set! //S '())
   (cond
    ((not-member /flag (@Assigned (@Get_n (@I) 2)))
     (set! //B (@Or //B (@And //B1 (@Get_n (@I) 1))))
     (set! //G (cons (@I) //G)))
    (#t
     (set! //G1 (cons (@I) //G1))))
   (set! //B1 (@And //B1 (@Not (@Get_n (@I) 1))))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((not-member /flag (@Assigned (@Get_n (@I) 2)))
       (set! //B (@Or //B (@And //B1 (@Get_n (@I) 1))))
       (set! //G (cons (@I) //G)))
      (#t
       (set! //G1 (cons (@I) //G1))))
     (set! //B1 (@And //B1 (@Not (@Get_n (@I) 1))))))
   (@Up)
   ; back to guarded 
   (while (@Right?) 
    (begin
     (@Right)
     (set! //S (cons (@I) //S))))
   (@Up)
   (@Up)
   ; back to WHILE 
   (set! //B (@And (@Get_n (@I) 1) //B))
   (set! //B1 (@Get_n (@I) 1))
   (cond
    ((> (gen-length //G) 1)
     (set! //G (list (@Make //T_/Cond '() (reverse //G)))))
    ((= (gen-length //G) 1)
     (set! //G (@Cs (@Get_n (car //G) 2)))))
   (cond
    ((> (gen-length //G1) 1)
     (set! //G1 (list (@Make //T_/Cond '() (reverse //G1)))))
    ((= (gen-length //G1) 1)
     (set! //G1 (@Cs (@Get_n (car //G1) 2)))))
   (set! //S (reverse //S))
   (set! //S1 (@I))
   (@Splice_Over (list (@Make 141 '() (list //B (@Make 17 '() (append //G //S)))) (@Make 114 '() (list (@Make 7 '() (list //B1 (@Make 17 '() (append //G1 //S (list //S1))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '())))))))))
   (set! /unroll 1)
   (set! /change 1)
   (@To_Last))))

(define (/foreach-flag_removal-10 //Depth //A/S_/Type)
 (cond
  ((and (not (= (@GT (@Parent)) //T_/Condition)) (member (@V /var) (@Variables (@I))))
   ; top level condition 
   (set! /v1 (@Make (if (number? (wsl-ref /values 1)) //T_/Number //T_/String) (wsl-ref /values 1) '()))
   (set! /v2 (@Make (if (number? (wsl-ref /values 2)) //T_/Number //T_/String) (wsl-ref /values 2) '()))
   (set! //B1 (@FR_Replace (@I) /var /v1))
   (set! //B2 (@FR_Replace (@I) /var /v2))
   (@Paste_Over (@Make 311 '() (list (@Make 310 '() (list (@Make 313 '() (list (@Var_To_Expn /var) (@Var_To_Expn /v1))) //B1)) (@Make 310 '() (list (@Make 313 '() (list (@Var_To_Expn /var) (@Var_To_Expn /v2))) //B2))))))))

(define (/foreach-flag_removal-11 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (vector-set! /__/Match_array 0 /var)
  (set! /__/O/K (@New_Match  /%const__flag_removal__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e_save /e))
     (set! /e (vector-ref /__/Match_array 1))
     (cond
      ((or (= (@ST /e) //T_/Number) (= (@ST /e) //T_/String))
       (set! /values (union-n (list (@V /e)) /values)))
      (#t
       (set! //O/K 0)))
     (set! /e /__e_save)))
   (#t
    (cond
     ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (member (@V /var) (@Assigned (@I))))
      (set! //O/K 0)))))))

(define (/foreach-flag_removal-12 //Depth //A/S_/Type)
 (cond
  ((@LR_Equal? (@I) /var)
   (@Paste_Over /value))))

(define /%const__flag_removal__1 (@Make 110 '() (list (@Make 6 '() (list (@Make 510 '() (list (@Make 205 1 '()))) (@Make 217 -2 '()))))))
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
(define (@Flag_Removal_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Var))
   (@Fail "Current item is not a VAR clause"))
  ((null? (@FR_Check_For_Flags))
   (@Fail "None of the variables in this VAR is a flag"))
  (#t
   (@Pass))))

(define (@Flag_Removal_Code //Data)
 (let ((/flags '())
       (/change-save /change)
       (/unroll-save /unroll)
       (/assigned-save /assigned)
       (/orig '())
       (/posn-save /posn))
  (set! /change 0)
  (set! /unroll 0)
  (set! /assigned 0)
  (set! /posn (@Posn))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (@Goto /posn)
    (set! /flags (@FR_Check_For_Flags))
    (cond
     ((null? /flags)
      (set! /fl_flag1 1))
     (#t
      (set! /orig (@Program))
      (display-list "flags = " (my-map @N_String /flags))
      (@FR_Preprocess_Flags /flags)
      (@Foreach_Statement /foreach-flag_removal-1 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (for-in /flag /flags 
       (begin
        (set! /unroll 0)
        (set! /change 0)
        ; Look for a statement which sets the flag, followed by 
        ; a statement which tests the flag 
        (@Ateach_Stats /foreach-flag_removal-2 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))
        (@Trans //T/R_/Simplify "")
        (cond
         ((@Trans? //T/R_/Remove_/Redundant_/Vars)
          (@Trans //T/R_/Remove_/Redundant_/Vars "")))
        ; Look for an opportunity for entire loop unrolling 
        ; so that the first iteration of the remainder will set the flag 
        (cond
         ((= /unroll 0)
          (let ((/-result- (@FR_Entire_Loop_Flag  /flag /change /unroll)))
           (set! /change (car /-result-)) (set! /-result- (cdr /-result-))
           (set! /unroll (car /-result-)) (set! /-result- (cdr /-result-)))
          (cond
           ((> /unroll 0)
            (set! /change (@FR_Find_Values  /flag /change))))))
        ; Look for a possible first time through flag: 
        (cond
         ((= /unroll 0)
          (let ((/-result- (@FR_Loop_Flag  /flag /change /unroll)))
           (set! /change (car /-result-)) (set! /-result- (cdr /-result-))
           (set! /unroll (car /-result-)) (set! /-result- (cdr /-result-)))
          (cond
           ((> /unroll 0)
            (set! /change (@FR_Find_Values  /flag /change))
            (@Goto '())
            (@Trans //T/R_/Constant_/Propagation "")))))
        ; Check if a loop can be rolled up. 
        ; Simple loop rolling won't work if there was a `first time through' flag 
        ; so also try converting a WHILE to an FLOOP and absorbing left 
        (let ((/old-save /old))
         (set! /old '())
         (@Ateach_Statement /foreach-flag_removal-3 0 (@AS_Type) 0)
         (cond
          ((null? (@Program))
           (@New_Program (@Skips))))
         (set! /old /old-save))
        (while (not (= /change 0)) 
         (begin
          ; It should be safer to do a constant propagation *after* 
          ; rolling up loops: 
          (@Trans //T/R_/Simplify "")
          (@Trans //T/R_/Constant_/Propagation "")
          (set! /change 0)
          ; Look for a statement which sets the flag, followed by 
          ; a statement which tests the flag 
          (@Ateach_Stats /foreach-flag_removal-4 0 (@AS_Type) 0)
          (cond
           ((null? (@Program))
            (@New_Program (@Skips))))
          (@Trans //T/R_/Simplify "")
          (cond
           ((@Trans? //T/R_/Remove_/Redundant_/Vars)
            (@Trans //T/R_/Remove_/Redundant_/Vars "")))
          ; Look for an opportunity for entire loop unrolling 
          ; so that the first iteration of the remainder will set the flag 
          (cond
           ((= /unroll 0)
            (let ((/-result- (@FR_Entire_Loop_Flag  /flag /change /unroll)))
             (set! /change (car /-result-)) (set! /-result- (cdr /-result-))
             (set! /unroll (car /-result-)) (set! /-result- (cdr /-result-)))
            (cond
             ((> /unroll 0)
              (set! /change (@FR_Find_Values  /flag /change))))))
          ; Look for a possible first time through flag: 
          (cond
           ((= /unroll 0)
            (let ((/-result- (@FR_Loop_Flag  /flag /change /unroll)))
             (set! /change (car /-result-)) (set! /-result- (cdr /-result-))
             (set! /unroll (car /-result-)) (set! /-result- (cdr /-result-)))
            (cond
             ((> /unroll 0)
              (set! /change (@FR_Find_Values  /flag /change))
              (@Goto '())
              (@Trans //T/R_/Constant_/Propagation "")))))
          ; Check if a loop can be rolled up. 
          ; Simple loop rolling won't work if there was a `first time through' flag 
          ; so also try converting a WHILE to an FLOOP and absorbing left 
          (let ((/old-save /old))
           (set! /old '())
           (@Ateach_Statement /foreach-flag_removal-5 0 (@AS_Type) 0)
           (cond
            ((null? (@Program))
             (@New_Program (@Skips))))
           (set! /old /old-save))))))
      (cond
       ((@Equal? (@Program) /orig)
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0)))))))
  (set! /change /change-save)
  (set! /unroll /unroll-save)
  (set! /assigned /assigned-save)
  (set! /posn /posn-save)))

; flag is used before assigned in current statement 
; and is assigned in the statement as position n 
(define (@FR_Process1 /flag-par /n1 /n2 /change-par /unroll-par)
 (let ((/unroll-save /unroll)
       (/change-save /change)
       (/flag-save /flag)
       (funct-result '()))
  (set! /unroll /unroll-par)
  (set! /change /change-par)
  (set! /flag /flag-par)
  (let ((/val-save /val)
        (/orig (@Program))
        (/posn-save /posn))
   (set! /val '())
   (set! /posn (@Posn))
   (@To /n1)
   (cond
    ((and (or (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/Floop)) (= /unroll 0))
     (set! /unroll 1)
     (display-list "Unrolling loop...")
     (@Trans //T/R_/Unroll_/Loop "")))
   (cond
    ((= (@ST (@I)) //T_/Cond)
     (while (> /n2 /n1) 
      (begin
       (cond
        ((@Trans? //T/R_/Absorb_/Right)
         (display-list "Absorbing into Cond...")
         (@Trans //T/R_/Absorb_/Right "")))
       (set! /n2 (- /n2 1))))
     ; See if some references to flag can be removed. 
     ; This should remove references in the WHILE if the flag is assigned 
     ; a constant value in the body of the loop (eg first time through switch) 
     (set! /change (@FR_Find_Values  /flag /change)))
    (#t
     (@To /n2)))
   (cond
    ((= /change 0)
     (@New_Program /orig)
     (@Goto /posn)))
   #t
   (set! /val /val-save)
   (set! /posn /posn-save))
  (set! funct-result (list /change /unroll))
  (set! /unroll /unroll-save)
  (set! /change /change-save)
  (set! /flag /flag-save)
  funct-result))

(define (@FR_Check_For_Flags)
 (let ((/flags '())
       (/values-save /values)
       (//O/K-save //O/K)
       (/var-save /var)
       (funct-result '()))
  (set! /values '())
  (set! //O/K 1)
  (set! /var '())
  (cond
   ((= (@ST (@I)) //T_/Var)
    (for-in /assign (@Cs (@Get_n (@I) 1)) 
     (begin
      (set! //O/K 1)
      (set! /var (@Get_n /assign 1))
      (cond
       ((and (= (@ST /var) //T_/Var_/Lvalue) (or (= (@ST (@Get_n /assign 2)) //T_/Number) (= (@ST (@Get_n /assign 2)) //T_/String)))
        (set! /values (list (@V (@Get_n /assign 2))))
        (let ((/-result- (@FR_Get_Values  /var /values //O/K)))
         (set! /values (car /-result-)) (set! /-result- (cdr /-result-))
         (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-)))
        (cond
         ((not (= (gen-length /values) 2))
          (set! //O/K 0))))
       (#t
        (set! //O/K 0)))
      (cond
       ((= //O/K 1)
        (set! /flags (cons (@V /var) /flags))))))))
  (set! funct-result (reverse /flags))
  (set! /values /values-save)
  (set! //O/K //O/K-save)
  (set! /var /var-save)
  funct-result))

(define (@FR_Check_For_Flags_Orig)
 (let ((/flags '())
       (/values-save /values)
       (//O/K-save //O/K)
       (/var-save /var)
       (funct-result '()))
  (set! /values '())
  (set! //O/K 1)
  (set! /var '())
  (for-in /assign (@Cs (@Get_n (@I) 1)) 
   (begin
    (set! //O/K 1)
    (set! /var (@Get_n /assign 1))
    (cond
     ((and (= (@ST /var) //T_/Var_/Lvalue) (or (= (@ST (@Get_n /assign 2)) //T_/Number) (= (@ST (@Get_n /assign 2)) //T_/String)))
      (set! /values (list (@V (@Get_n /assign 2))))
      (let ((/-result- (@FR_Get_Values  /var /values //O/K)))
       (set! /values (car /-result-)) (set! /-result- (cdr /-result-))
       (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-)))
      (cond
       ((not (= (gen-length /values) 2))
        (set! //O/K 0))
       (#t
        (@Down_To 2)
        ; to body 
        (@Edit)
        (@Ateach_Expn /foreach-flag_removal-6 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))
        (@Undo_Edit)
        (@Up))))
     (#t
      (set! //O/K 0)))
    (cond
     ((and (= //O/K 1) (= (gen-length /values) 2))
      (set! /flags (cons (@V /var) /flags))))))
  (set! funct-result (reverse /flags))
  (set! /values /values-save)
  (set! //O/K //O/K-save)
  (set! /var /var-save)
  funct-result))

; Look for a possible first time through flag. 
; This is a test of the flag in a loop, followed by code which sets the flag. 
(define (@FR_Loop_Flag /flag-par /change-par /unroll-par)
 (let ((/unroll-save /unroll)
       (/change-save /change)
       (/flag-save /flag)
       (funct-result '()))
  (set! /unroll /unroll-par)
  (set! /change /change-par)
  (set! /flag /flag-par)
  (let ((/tested-save /tested)
        (/posn-save /posn))
   (set! /tested 0)
   (set! /posn '())
   (display-list "-------------- Loop_Flag -------------")
   (@Ateach_Stats /foreach-flag_removal-7 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /tested /tested-save)
   (set! /posn /posn-save))
  (set! funct-result (list /change /unroll))
  (set! /unroll /unroll-save)
  (set! /change /change-save)
  (set! /flag /flag-save)
  funct-result))

(define (@FR_Find_Values /flag-par /change-par)
 (let ((/change-save /change)
       (/flag-save /flag)
       (funct-result '()))
  (set! /change /change-par)
  (set! /flag /flag-par)
  (let ((/val-save /val))
   (set! /val '())
   (@Ateach_Expn /foreach-flag_removal-8 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /val /val-save))
  (set! funct-result /change)
  (set! /change /change-save)
  (set! /flag /flag-save)
  funct-result))

; Look for a loop of the form: 
; WHILE B DO IF ... ELSIF Bx THEN flag := C ... FI; ... OD 
; Compute the condition under which to split the loop so that 
; the first loop does not assign flag and the first iteration of the second loop 
; (if any) will set the flag. 
(define (@FR_Entire_Loop_Flag /flag-par /change-par /unroll-par)
 (let ((/unroll-save /unroll)
       (/change-save /change)
       (/flag-save /flag)
       (funct-result '()))
  (set! /unroll /unroll-par)
  (set! /change /change-par)
  (set! /flag /flag-par)
  (let ((/posn-save /posn)
        (/orig (@Program))
        (/done-save /done)
        (//B-save //B)
        (//B1-save //B1)
        (//G-save //G)
        (//G1-save //G1)
        (//S-save //S)
        (//S1-save //S1))
   (set! /posn (@Posn))
   (set! /done 0)
   (set! //B '())
   (set! //B1 '())
   (set! //G '())
   (set! //G1 '())
   (set! //S '())
   (set! //S1 '())
   (@Ateach_Statement /foreach-flag_removal-9 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (cond
    ((and (= /unroll 0) (= /done 1))
     (@New_Program /orig)
     (@Goto /posn)))
   (set! /posn /posn-save)
   (set! /done /done-save)
   (set! //B //B-save)
   (set! //B1 //B1-save)
   (set! //G //G-save)
   (set! //G1 //G1-save)
   (set! //S //S-save)
   (set! //S1 //S1-save))
  (set! funct-result (list /change /unroll))
  (set! /unroll /unroll-save)
  (set! /change /change-save)
  (set! /flag /flag-save)
  funct-result))

; Preprocess tests so that only the two known values are tested for, 
; eg if the values are 0 and 1 then x > 0 becomes x = 1 and so on. 
(define (@FR_Preprocess_Flags /flags)
 (let ((/values-save /values)
       (//O/K-save //O/K)
       (/var-save /var)
       (//B1-save //B1)
       (//B2-save //B2)
       (/v1-save /v1)
       (/v2-save /v2))
  (set! /values '())
  (set! //O/K 1)
  (set! /var '())
  (set! //B1 '())
  (set! //B2 '())
  (set! /v1 '())
  (set! /v2 '())
  (for-in /assign (@Cs (@Get_n (@I) 1)) 
   (begin
    (set! //O/K 1)
    (set! /var (@Get_n /assign 1))
    (cond
     ((and (= (@ST /var) //T_/Var_/Lvalue) (or (= (@ST (@Get_n /assign 2)) //T_/Number) (= (@ST (@Get_n /assign 2)) //T_/String)))
      (set! /values (list (@V (@Get_n /assign 2))))
      (let ((/-result- (@FR_Get_Values  /var /values //O/K)))
       (set! /values (car /-result-)) (set! /-result- (cdr /-result-))
       (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-)))
      (cond
       ((not (= (gen-length /values) 2))
        (set! //O/K 0))))
     (#t
      (set! //O/K 0)))
    (cond
     ((= //O/K 1)
      (display-list "Preprocessing: " (@N_String (@V /var)) " with values: " /values)
      (@Ateach_Cond /foreach-flag_removal-10 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))))))
  (@Trans //T/R_/Simplify "")
  (@Trans //T/R_/Remove_/All_/Redundant_/Vars "")
  (set! /values /values-save)
  (set! //O/K //O/K-save)
  (set! /var /var-save)
  (set! //B1 //B1-save)
  (set! //B2 //B2-save)
  (set! /v1 /v1-save)
  (set! /v2 /v2-save)))

(define (@FR_Get_Values /var-par /values-par //O/K-par)
 (let ((//O/K-save //O/K)
       (/values-save /values)
       (/var-save /var)
       (funct-result '()))
  (set! //O/K //O/K-par)
  (set! /values /values-par)
  (set! /var /var-par)
  (@Foreach_Statement /foreach-flag_removal-11 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result (list /values //O/K))
  (set! //O/K //O/K-save)
  (set! /values /values-save)
  (set! /var /var-save)
  funct-result))

; Replace given variable with given value and simplify the condition 
(define (@FR_Replace //I /var-par /value-par)
 (let ((/value-save /value)
       (/var-save /var)
       (//R '())
       (funct-result '()))
  (set! /value /value-par)
  (set! /var /var-par)
  (@Edit)
  (@New_Program //I)
  (@Foreach_Expn /foreach-flag_removal-12 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //R (@Simplify_Cond (@I)))
  (@Undo_Edit)
  (set! funct-result //R)
  (set! /value /value-save)
  (set! /var /var-save)
  funct-result))

#t
