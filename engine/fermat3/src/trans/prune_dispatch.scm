;;; Scheme translation of WSL code
(define (/foreach-prune_dispatch-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (@Starts_With? (@V (@I)) "NOTUSED_"))
   (puthash /notused (@String_To_Num (substr (@N_String (@V (@I))) 8)) 1))))

(define (/foreach-prune_dispatch-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Var)
   (set! /vars (union-n /vars (@Elts_Assigned (@Get_n (@I) 1)))))))

(define (/foreach-prune_dispatch-3 //Depth //A/S_/Type)
 (@Down_Last)
 (cond
  ((and (@Regular? (@I)) (@Only_Calls_Z? (@I)) (@Gen_Proper? (@I) "Reg"))
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (@Delete)
     (cond
      ((not (@Left?))
       (set! /fl_flag1 1))
      (#t
       (@Left)
       (cond
        ((or (not (@Only_Calls_Z? (@I))) (not (@Gen_Proper? (@I) "Reg")))
         (set! /fl_flag1 1))
        (#t
         (set! /fl_flag1 0))))))))))

(define (/foreach-prune_dispatch-4 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (set! /occ (@PD_Check_Dest_Codes_Assigns  (@I) /codes /vars /notused /occ)))
  ((= (@ST (@I)) //T_/Var)
   (set! /occ (@PD_Check_Dest_Codes_Assigns  (@Get_n (@I) 1) /codes /vars /notused /occ)))
  ((and (= (@ST (@I)) //T_/Push) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@I) 1)) /call_stack) (= (@ST (@Get_n (@I) 2)) //T_/Number))
   (cond
    ((not (null? (gethash /codes (@V (@Get_n (@I) 2)))))
     (puthash /occ (@V (@Get_n (@I) 2)) 1))))))

(define (/foreach-prune_dispatch-5 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   ; to first assign 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (= (@ST (@Get_n (@I) 2)) //T_/Number) (member (@Struct_Elts (@Get_n (@I) 1)) /vars))
       (set! /n (@V (@Get_n (@I) 2)))
       (cond
        ((not (null? (gethash /codes /n)))
         (while (and (not (null? (gethash /codes /n))) (< /n //Omega)) 
          (begin
           (puthash /occ /n 1)
           (set! /n (+ /n 4)))))))
      ((and (= (@ST (@Get_n (@I) 2)) //T_/X_/Funct_/Call) (member (@V (@Get_n (@Get_n (@I) 2) 1)) /bit_ops) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 2) 2) 1)) //T_/Number) (member (@Struct_Elts (@Get_n (@I) 1)) /vars))
       (set! /n (@V (@Get_n (@Get_n (@Get_n (@I) 2) 2) 1)))
       (cond
        ((not (null? (gethash /codes /n)))
         (while (and (not (null? (gethash /codes /n))) (< /n //Omega)) 
          (begin
           (puthash /occ /n 1)
           (set! /n (+ /n 4))))))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))))

(define /%const__prune_dispatch__1 (@Make 313 '() (list (@Make 207 (@Make_Name "destination") '()) (@Make 217 -1 '()))))
(define /%const__prune_dispatch__2 (@Make 8 '() (list (@Make 9 (@Make_Name "dispatch") '()) (@Make 17 '() (list (@Make 145 '() '()))))))
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
; Check for dispatch codes which are tested in dispatch but never 
; assigned anywhere and remove them from dispatch 
; Note that an assignment like this: v := disp_code; FOO(); CALL Z 
; can be ignored since the disp_code assigned here can never reach 
; the dispatch action. 
; If there is an occurrence of NOTUSED_code, then don't treat code 
; as occuring simply because there is a reference to code-4 etc. 
(define (@Prune_Dispatch_Test)
 (let ((/dispatch (@Make_Name "dispatch"))
       (/destination-save /destination))
  (set! /destination (@Make_Name "destination"))
  (cond
   ((= (@ST (@I)) //T_/A_/S)
    (@Down_Last)
    (@Down)
    (while (and (@Right?) (not (equal? (@V (@I)) /dispatch))) 
     (@Right))))
  (cond
   ((= (@GT (@I)) //T_/Action)
    (cond
     ((equal? (@V (@Get_n (@I) 1)) (@Make_Name "dispatch"))
      (@Down_Last))
     (#t
      (@Fail "Only applies to the dispatch action.")))))
  (cond
   ((not (@Failed?))
    (cond
     ((= (@GT (@I)) //T_/Statements)
      (@Down)))
    (cond
     ((not (= (@ST (@I)) //T_/Cond))
      (@Fail "IF statement not found"))
     (#t
      (@Down)
      ; to first guarded 
      (set! /fl_flag2 0)
      (while (= /fl_flag2 0) 
       (cond
        ((not (@Right?))
         (set! /fl_flag2 1))
        (#t
         (@Down)
         (cond
          ((= (@ST (@I)) //T_/Or)
           (@Down)))
         (set! /fl_flag1 0)
         (while (= /fl_flag1 0) 
          (begin
           (let ((/__/O/K 1))
            (set! /__/O/K (@New_Match  /%const__prune_dispatch__1 (@I) /__/O/K))
            (cond
             ((= /__/O/K 1)
              (let ((/__n_save /n))
               (set! /n (vector-ref /__/Match_array 0))
               (cond
                ((and (not (= (@ST /n) //T_/Number)) (not (and (= (@ST /n) //T_/Negate) (= (@ST (@Get_n /n 1)) //T_/Number))))
                 (@Fail "destination is not compared to a number!")))
               (set! /n /__n_save)))
             (#t
              (@Fail "IF statement is not in right format."))))
           (cond
            ((@Failed?)
             (set! /fl_flag1 2))
            ((not (= (@ST (@Parent)) //T_/Or))
             (set! /fl_flag1 1))
            ((not (@Right?))
             (@Up)
             (set! /fl_flag1 1))
            (#t
             (@Right)
             (set! /fl_flag1 0)))))
         (cond
          ((= /fl_flag1 2)
           (set! /fl_flag2 1))
          (#t
           (@Up)
           (@Right)
           (set! /fl_flag2 0))))))
      (cond
       ((not (@Failed?))
        (@Pass)))))))
  (set! /destination /destination-save)))

; dest_codes is a table of dispatch codes, dest_occ records which codes 
; are referenced apart from the tests in the dispatch action 
(define (@Prune_Dispatch_Code //Data)
 (let ((/dest_codes (hash-table))
       (/dest_occ (hash-table))
       (/notused-save /notused)
       (/dispatch (@Make_Name "dispatch"))
       (/destination-save /destination)
       (/posn (@Posn))
       (/action '())
       (/dispatch_posn '())
       (/vars-save /vars)
       (/delete 0)
       (/bit_ops-save /bit_ops))
  (set! /notused (hash-table))
  (set! /destination (@Make_Name "destination"))
  (set! /vars '())
  (set! /bit_ops (list (@Make_Name "bit_and") (@Make_Name "bit_or")))
  (display-list "Finding accessed variables...")
  ; R14 may be referenced via call_via_ptr functions since it holds a return address: 
  (set! /vars (union-n (@Elts_Used (@Program)) (list (list (@Make_Name "r14")))))
  (@Goto '())
  (@Foreach_Expn /foreach-prune_dispatch-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Statement /foreach-prune_dispatch-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Goto /posn)
  (display-list "Pruning dispatch...")
  (cond
   ((= (@ST (@I)) //T_/A_/S)
    (@Down_Last)
    (@Down)
    (while (and (@Right?) (not (equal? (@V (@I)) /dispatch))) 
     (@Right))))
  (cond
   ((= (@GT (@I)) //T_/Action)
    (@Down_Last)))
  (cond
   ((= (@GT (@I)) //T_/Statements)
    (@Down)))
  (@Down)
  ; to first guard 
  (while (@Right?) 
   (begin
    (@Down)
    ; to condition 
    (cond
     ((= (@ST (@I)) //T_/Or)
      (@Down)))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__prune_dispatch__1 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__n_save /n))
          (set! /n (vector-ref /__/Match_array 0))
          (puthash /dest_codes (@V /n) 1)
          (set! /n /__n_save)))
        (#t
         (prit)
         (error "internal error in Prune_Dispatch"))))
      (cond
       ((not (= (@ST (@Parent)) //T_/Or))
        (set! /fl_flag1 1))
       ((not (@Right?))
        (@Up)
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)
    (@Right)))
  ; Temporarily delete the dispatch action, then search for references to dest codes: 
  (@Up)
  (@Up)
  (@Up)
  ; to the dispatch action 
  (cond
   ((not (= (@GT (@I)) //T_/Action))
    (error "ERROR" "Prune_Dispatch" (@ST (@I)))))
  (set! /action (@I))
  (set! /dispatch_posn (@Posn))
  (@Paste_Over /%const__prune_dispatch__2)
  (@Goto '())
  (set! /dest_occ (@PD_Check_Dest_Codes  /dest_codes /vars /notused /dest_occ))
  ; Restore dispatch, then search for references to dest codes in the statements 
  (@Goto /dispatch_posn)
  (@Paste_Over /action)
  (@Down_Last)
  (@Down)
  (@Down)
  ; to first guard 
  (@Down_To 2)
  ; to the statements 
  (set! /dest_occ (@PD_Check_Dest_Codes  /dest_codes /vars /notused /dest_occ))
  (@Up)
  (while (@Right?) 
   (begin
    (@Right)
    (@Down_To 2)
    ; to the statements 
    (set! /dest_occ (@PD_Check_Dest_Codes  /dest_codes /vars /notused /dest_occ))
    (@Up)))
  (@Up)
  ; to the IF 
  (cond
   ((@Right?)
    (@Right)
    ; Process the rest of the dispatch action 
    (set! /dest_occ (@PD_Check_Dest_Codes  /dest_codes /vars /notused /dest_occ))
    (while (@Right?) 
     (begin
      (@Right)
      (set! /dest_occ (@PD_Check_Dest_Codes  /dest_codes /vars /notused /dest_occ))))))
  (@Goto /dispatch_posn)
  ; Prune the dispatch arms whose codes aren't used (but keep code zero): 
  (puthash /dest_occ 0 1)
  (@Down_Last)
  (@Down)
  (@Down)
  ; to first guard 
  (while (@Right?) 
   (begin
    (@Down)
    ; to condition 
    (set! /delete 0)
    (cond
     ((= (@ST (@I)) //T_/Or)
      (@Down)))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((= (@ST (@I)) //T_/Equal)
        (cond
         ((and (= (@ST (@Get_n (@I) 1)) //T_/Variable) (equal? (@V (@Get_n (@I) 1)) /destination) (= (@ST (@Get_n (@I) 2)) //T_/Number) (null? (gethash /dest_occ (@V (@Get_n (@I) 2)))))
          (display-list-flush (@V (@Get_n (@I) 2)) " ")
          (@Paste_Over (@Make //T_/False '() '()))))))
      (cond
       ((not (= (@ST (@Parent)) //T_/Or))
        (set! /fl_flag1 1))
       ((not (@Right?))
        (@Up)
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Paste_Over (@Simplify_Cond (@I)))
    (cond
     ((= (@ST (@I)) //T_/False)
      (@Up)
      (@Delete))
     (#t
      (@Up)
      (@Right)))))
  (@Up)
  ; back to cond 
  (@Fixup)
  (display-list "")
  (@Goto /posn)
  (set! /notused /notused-save)
  (set! /destination /destination-save)
  (set! /vars /vars-save)
  (set! /bit_ops /bit_ops-save)))

; Record in the occ table which codes are found in the current item 
; Ignore occurrences which are assignments to non-accessed variables 
; A statement sequence which is regular and only calls Z can be ignored 
; since any code references in this sequence cannot reach dispatch 
; codes is the table of codes, vars is the set of accessed variables 
; (if a dispatch code is assigned to a variable which is never accessed, 
; then that reference can be ignored). 
; occ is the table in which occurences of code references are recorded. 
(define (@PD_Check_Dest_Codes /codes-par /vars-par /notused-par /occ-par)
 (let ((/occ-save /occ)
       (/notused-save /notused)
       (/vars-save /vars)
       (/codes-save /codes)
       (funct-result '()))
  (set! /occ /occ-par)
  (set! /notused /notused-par)
  (set! /vars /vars-par)
  (set! /codes /codes-par)
  (let ((/n-save /n)
        (/call_stack-save /call_stack))
   (set! /n 0)
   (set! /call_stack (@Make_Name "call_stack"))
   ; Temporarily delete statements whose code references cannot reach dispatch: 
   (@Edit)
   (@Foreach_Stats /foreach-prune_dispatch-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Statement /foreach-prune_dispatch-4 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Undo_Edit)
   (set! /n /n-save)
   (set! /call_stack /call_stack-save))
  (set! funct-result /occ)
  (set! /occ /occ-save)
  (set! /notused /notused-save)
  (set! /vars /vars-save)
  (set! /codes /codes-save)
  funct-result))

(define (@PD_Check_Dest_Codes_Assigns //I /codes-par /vars-par /notused-par /occ-par)
 (let ((/occ-save /occ)
       (/notused-save /notused)
       (/vars-save /vars)
       (/codes-save /codes)
       (funct-result '()))
  (set! /occ /occ-par)
  (set! /notused /notused-par)
  (set! /vars /vars-par)
  (set! /codes /codes-par)
  (for-in /assign (@Cs //I) 
   (cond
    ((and (= (@ST (@Get_n /assign 2)) //T_/Number) (not (null? (intersection-n /vars (@Elements (@Get_n /assign 1))))))
     (set! /n (@V (@Get_n /assign 2)))
     (cond
      ((not (null? (gethash /codes /n)))
       (puthash /occ /n 1)
       (while (and (not (null? (gethash /codes /n))) (null? (gethash /notused /n)) (< /n //Omega)) 
        (begin
         (puthash /occ /n 1)
         (set! /n (+ /n 4)))))))
    ((and (= (@ST (@Get_n /assign 2)) //T_/X_/Funct_/Call) (equal? (@V (@Get_n (@Get_n /assign 2) 1)) /inline_par) (= (@ST (@Get_n (@Get_n (@Get_n /assign 2) 2) 1)) //T_/Number))
     (set! /n (@V (@Get_n (@Get_n (@Get_n /assign 2) 2) 1)))
     (cond
      ((not (null? (gethash /codes /n)))
       (puthash /occ /n 1)
       (while (and (not (null? (gethash /codes /n))) (< /n //Omega)) 
        (begin
         (puthash /occ /n 1)
         (set! /n (+ /n 4)))))))
    ((and (= (@ST (@Get_n /assign 2)) //T_/X_/Funct_/Call) (member (@V (@Get_n (@Get_n /assign 2) 1)) /bit_ops) (= (@ST (@Get_n (@Get_n (@Get_n /assign 2) 2) 1)) //T_/Number) (not (null? (intersection-n /vars (@Elements (@Get_n /assign 1))))))
     (set! /n (@V (@Get_n (@Get_n (@Get_n /assign 2) 2) 1)))
     (cond
      ((not (null? (gethash /codes /n)))
       (puthash /occ /n 1)
       (while (and (not (null? (gethash /codes /n))) (< /n //Omega)) 
        (begin
         (puthash /occ /n 1)
         (set! /n (+ /n 4)))))))))
  (set! funct-result /occ)
  (set! /occ /occ-save)
  (set! /notused /notused-save)
  (set! /vars /vars-save)
  (set! /codes /codes-save)
  funct-result))

(define (@Only_Calls_Z? //I)
 (let ((/calls (@Calls //I)))
  (or (null? /calls) (and (= (gen-length /calls) 1) (equal? (car (car /calls)) (@Make_Name "Z"))))))

(define (@PD_Check_Dest_Codes_Orig /codes-par /vars-par /occ-par)
 (let ((/occ-save /occ)
       (/vars-save /vars)
       (/codes-save /codes)
       (funct-result '()))
  (set! /occ /occ-par)
  (set! /vars /vars-par)
  (set! /codes /codes-par)
  (let ((/n-save /n))
   (set! /n 0)
   (@Foreach_Statement /foreach-prune_dispatch-5 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /n /n-save))
  (set! funct-result /occ)
  (set! /occ /occ-save)
  (set! /vars /vars-save)
  (set! /codes /codes-save)
  funct-result))

#t
