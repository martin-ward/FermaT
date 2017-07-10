;;; Scheme translation of WSL code
(define (/foreach-separate_exit_code-1 //Depth //A/S_/Type)
 (cond
  ((and (= //Depth 1) (equal? (@Gen_TVs (@I) //A/S_/Type) (list 1)) (not (equal? (@Stat_Types (@I)) (list //T_/Exit))))
   (cond
    ((and (not (@Failed?)) (@Gen_Reducible? (@I) //A/S_/Type))
     (@Pass))
    (#t
     (@Fail "Some of the exit code is not reducible"))))))

(define (/foreach-separate_exit_code-2 //Depth //A/S_/Type)
 (cond
  ((and (= //Depth 1) (= (gen-length (@Gen_TVs (@I) //A/S_/Type)) 1) (> (car (@Gen_TVs (@I) //A/S_/Type)) 0))
   (set! /n (@Make //T_/Number (+ (@V /n) 1) '()))
   (set! /body (@I))
   (set! /count (+ /count 1))
   (@Paste_Over /%const__separate_exit_code__1))))

(define (/foreach-separate_exit_code-3 //Depth //A/S_/Type)
 (cond
  ((and (= //Depth 1) (= (gen-length (@Gen_TVs (@I) //A/S_/Type)) 1) (> (car (@Gen_TVs (@I) //A/S_/Type)) 0))
   (cond
    ((@Set_Subset? (@Stat_Types (@I)) /skips)
     ; No actual code to take out 
    )
    (#t
     (set! /body (@Cs (@I)))
     (set! /n (@Make //T_/Number (+ (@V /n) 1) '()))
     (set! /guards (cons (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /flag) (@Var_To_Expn /n))) (@Make 17 '() /body))) /guards))
     (@Paste_Over (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /flag) (@Var_To_Expn /n))))) (@Make 117 1 '())))))))
  ((and (= //Depth 1) (> (gen-length (@Gen_TVs (@I) //A/S_/Type)) 1) (= (@ST (@Get_n (@I) (@Size (@I)))) //T_/Exit) (= (@V (@Get_n (@I) (@Size (@I)))) 1))
   (let ((/-result- (@SEC_Split  (@Cs (@I)) //S1 //S2)))
    (set! //S1 (car /-result-)) (set! /-result- (cdr /-result-))
    (set! //S2 (car /-result-)) (set! /-result- (cdr /-result-)))
   (cond
    ((@Set_Subset? (@Stat_Types //S2) /skips)
     ; No actual code to take out 
    )
    (#t
     (set! /body (@Cs //S2))
     (set! /n (@Make //T_/Number (+ (@V /n) 1) '()))
     (set! /guards (cons (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /flag) (@Var_To_Expn /n))) (@Make 17 '() /body))) /guards))
     (@Paste_Over (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /flag) (@Var_To_Expn /n))))) (@Make 117 1 '()))))
     (@Down)
     (@Splice_Before (@Cs //S1))
     (@Up))))))

(define /%const__separate_exit_code__1 (@Make 17 '() (list (@Make 117 1 '()))))
(define /%const__separate_exit_code__2 (@Make 133 '() (list (@Make 17 '() (list (@Make 107 -1 '()) (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -2 '()) (@Make 17 '() (list (@Make 107 -3 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 107 -4 '()) (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Make 217 -5 '()) (@Make 217 -6 '()))) (@Make 17 '() (list (@Make 107 -7 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))))))))))
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
(define (@Separate_Exit_Code_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Floop))
   (@Fail "Selected item is not an Floop"))
  ((@Trans? //T/R_/Separate_/Right)
   (@Pass))
  ((@SEC_Simple?)
   (@Pass))
  (#t
   (@Foreach_Stats /foreach-separate_exit_code-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (cond
    ((and (not (@Passed?)) (not (@Failed?)))
     (@Fail "No suitable statements to take out"))))))

(define (@Separate_Exit_Code_Code //Data)
 (cond
  ((@Trans? //T/R_/Separate_/Right)
   (@Trans //T/R_/Separate_/Right ""))
  ((@SEC_Simple?)
   (let ((//O/K-save //O/K))
    (set! //O/K 0)
    (set! //O/K (@SEC_Simple  1 //O/K))
    (set! //O/K //O/K-save)))
  (#t
   (@Separate_Exit_Code_Sub //Data))))

(define (@Separate_Exit_Code_Sub //Data)
 (let ((/flag-save /flag)
       (/count-save /count)
       (/skips-save /skips)
       (//Flags (my-map @Make_Name (list "fl_flag1" "fl_flag2" "fl_flag3" "fl_flag4" "fl_flag5" "fl_flag6" "fl_flag7" "fl_flag8" "fl_flag9" "fl_flagX")))
       (/max 10)
       (/guards-save /guards)
       (/n-save /n)
       (/body-save /body)
       (//S1-save //S1)
       (//S2-save //S2))
  (set! /flag '())
  (set! /count 0)
  (set! /skips (@Make_Set (list //T_/Skip //T_/Comment //T_/Exit)))
  (set! /guards '())
  (set! /n (@Make //T_/Number 0 '()))
  (set! /body '())
  (set! //S1 '())
  (set! //S2 '())
  ; Choose a suitable flag variable: 
  (cond
   ((not (equal? (@String //Data) ""))
    (set! /name (@Make_Name (@String //Data))))
   (#t
    (while (null? /name) 
     (begin
      (cond
       ((member (car //Flags) (@Variables (@I)))
        (set! //Flags (cdr //Flags)))
       (#t
        (set! /name (car //Flags))))
      (cond
       ((null? //Flags)
        (set! /max (+ /max 1))
        (set! /name (@Make_Name (string-append "fl_flag" (@String /max))))))))))
  (set! /flag (@Make //T_/Variable /name '()))
  ; Work top-down so that we can take out bigger chunks of code 
  ; First a trial run to see if the flag is needed: 
  (@Edit)
  (@Ateach_Stats /foreach-separate_exit_code-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (cond
   ((= /count 0)
    (error "@Separate_Exit_Code_Sub: no exit code found!"))
   ((= /count 1)
    (@End_Edit)
    (@Splice_After (@Cs (@Increment /body (@AS_Type) (- 1) 1))))
   (#t
    (@Undo_Edit)
    (set! /n (@Make //T_/Number 0 '()))
    (set! /guards (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /flag) (@Make 205 0 '()))) (@Make 17 '() (list (@Make 145 '() '())))))))
    (@Ateach_Stats /foreach-separate_exit_code-3 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /flag) (@Make 205 0 '()))))))
    (@Right)
    (@Paste_After (@Make //T_/Cond '() (reverse /guards)))
    (@Right)
    (@Down_Last)
    (@Down)
    ; to last condition 
    (@Paste_Over (@Make //T_/True '() '()))
    (@Up)
    (@Up)
    ; Back to cond 
    ; Decrement the exit code: 
    (@Splice_Over (@Increment (@I) (@AS_Type) (- 1) 1))
    (@Left)))
  (set! /flag /flag-save)
  (set! /count /count-save)
  (set! /skips /skips-save)
  (set! /guards /guards-save)
  (set! /n /n-save)
  (set! /body /body-save)
  (set! //S1 //S1-save)
  (set! //S2 //S2-save)))

(define (@SEC_Simple?)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K 0)
  (set! //O/K (@SEC_Simple  0 //O/K))
  (set! funct-result (= //O/K 1))
  (set! //O/K //O/K-save)
  funct-result))

(define (@SEC_Simple /doit //O/K-par)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K //O/K-par)
  (let ((//S '())
        (/e2 '()))
   (set! //O/K 0)
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__separate_exit_code__2 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S4_save //S4)
            (/__e1_save /e1)
            (/__v_save /v)
            (/__/S3_save //S3)
            (/__/S2_save //S2)
            (/__/B1_save //B1)
            (/__/S1_save //S1))
       (set! //S4 (vector-ref /__/Match_array 6))
       (set! /e1 (vector-ref /__/Match_array 5))
       (set! /v (vector-ref /__/Match_array 4))
       (set! //S3 (vector-ref /__/Match_array 3))
       (set! //S2 (vector-ref /__/Match_array 2))
       (set! //B1 (vector-ref /__/Match_array 1))
       (set! //S1 (vector-ref /__/Match_array 0))
       (set! //S2 (@Make //T_/Statements '() //S2))
       (set! //S4 (@Make //T_/Statements '() //S4))
       (cond
        ((and (@Gen_Proper? (@Make //T_/Statements '() (concat //S1 //S3)) (@AS_Type)) (equal? (@Gen_TVs //S2 (@AS_Type)) (list 1)) (equal? (@Gen_TVs //S4 (@AS_Type)) (list 1)) (@Gen_Reducible? //S2 (@AS_Type)) (@Gen_Reducible? //S4 (@AS_Type)) (or (= (@ST /v) //T_/Variable) (= (@ST /v) //T_/Struct)) (@Set_Subset? (@Stat_Types (@Make //T_/Statements '() //S1)) (list //T_/Comment)) (= (@ST /e1) //T_/Number))
         ; Check that v is initially different from the exit test: 
         (@Paste_Before (@Skip))
         (set! /e2 (@Find_Value (@Struct_Elts /v)))
         (@Delete)
         (cond
          ((and (not (null? /e2)) (= (@ST /e2) //T_/Number) (not (equal? (@V /e2) (@V /e1))))
           (set! //O/K 1)
           (cond
            ((= /doit 1)
             (set! //S2 (@Cs //S2))
             (set! //S4 (@Cs //S4))
             (set! //S (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /v) (@Var_To_Expn /e1))) (@Make 17 '() //S4))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() //S2))))))
             (@Splice_Over (cons (@Make 133 '() (list (@Make 17 '() (append //S1 (list (@Make 114 '() (list (@Make 7 '() (list //B1 (@Make 17 '() (list (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (append //S3 (list (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /v) (@Var_To_Expn /e1))) (@Make 17 '() (list (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))))))))))) (@Increment //S (@AS_Type) (- 1) 1)))
             ; done 
            ))))))
       (set! //S4 /__/S4_save)
       (set! /e1 /__e1_save)
       (set! /v /__v_save)
       (set! //S3 /__/S3_save)
       (set! //S2 /__/S2_save)
       (set! //B1 /__/B1_save)
       (set! //S1 /__/S1_save))))))
  (set! funct-result //O/K)
  (set! //O/K //O/K-save)
  funct-result))

; Split a list of statements into two statement sequences 
; List ends in EXIT(1) and has other EXITs before. 
; Second statement sequence includes all preceding proper sequences. 
(define (@SEC_Split //L //S1-par //S2-par)
 (let ((//S2-save //S2)
       (//S1-save //S1)
       (funct-result '()))
  (set! //S2 //S2-par)
  (set! //S1 //S1-par)
  (let ((//L1 (cdr (reverse //L)))
        (//L2 (list (last-1 //L))))
   (while (@Gen_Proper? (car //L1) (@AS_Type)) 
    (begin
     (set! //L2 (cons (car //L1) //L2))
     (set! //L1 (cdr //L1))))
   (set! //S1 (@Make //T_/Statements '() (reverse //L1)))
   (set! //S2 (@Make //T_/Statements '() //L2)))
  (set! funct-result (list //S1 //S2))
  (set! //S2 //S2-save)
  (set! //S1 //S1-save)
  funct-result))

#t
