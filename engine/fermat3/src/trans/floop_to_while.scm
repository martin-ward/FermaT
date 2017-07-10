;;; Scheme translation of WSL code
(define (/foreach-floop_to_while-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Exit)
   (set! /count (+ /count (@V (@I)))))))

(define (/foreach-floop_to_while-2 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__floop_to_while__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/S2_save //S2)
          (/__/S1_save //S1)
          (/__/B_save //B))
     (set! //S2 (vector-ref /__/Match_array 2))
     (set! //S1 (vector-ref /__/Match_array 1))
     (set! //B (vector-ref /__/Match_array 0))
     (cond
      ((not (= (@ST (car //S2)) //T_/Skip))
       (@Splice_Over (@Cs (@Make 17 '() (append (list (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() (append //S1 (list (@Make 117 1 '())))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '())))))))) //S2))))))
     (set! //S2 /__/S2_save)
     (set! //S1 /__/S1_save)
     (set! //B /__/B_save))))))

(define (/foreach-floop_to_while-3 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__floop_to_while__2 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/S2_save //S2)
          (/__/S1_save //S1)
          (/__/B_save //B))
     (set! //S2 (vector-ref /__/Match_array 2))
     (set! //S1 (vector-ref /__/Match_array 1))
     (set! //B (vector-ref /__/Match_array 0))
     (cond
      ((not (= (@ST (car //S1)) //T_/Skip))
       (@Splice_Over (@Cs (@Make 17 '() (append (list (@Make 114 '() (list (@Make 7 '() (list (@Make 312 '() (list //B)) (@Make 17 '() (append //S2 (list (@Make 117 1 '())))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '())))))))) //S1))))))
     (set! //S2 /__/S2_save)
     (set! //S1 /__/S1_save)
     (set! //B /__/B_save))))))

(define (/foreach-floop_to_while-4 //Depth //A/S_/Type)
 (cond
  ((or (not (@Left?)) (= //O/K 0))
   (set! //O/K 0))
  (#t
   (@Left)
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__floop_to_while__6 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__e_save /e)
            (/__v_save /v))
       (set! /e (vector-ref /__/Match_array 1))
       (set! /v (vector-ref /__/Match_array 0))
       (cond
        ((or (not (= (@ST /e) //T_/Number)) (= (@V /e) 0))
         (set! //O/K 0))
        ((and (not (null? /var)) (not (@Equal? /v /var)))
         (set! //O/K 0))
        (#t
         (set! /var /v)))
       (set! /e /__e_save)
       (set! /v /__v_save)))
     (#t
      (set! //O/K 0))))
   (@Right))))

(define (/foreach-floop_to_while-5 //Depth //A/S_/Type)
 (cond
  ((not (= (@ST (@I)) //T_/Exit))
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /var)
    (set! /__/O/K (@New_Match  /%const__floop_to_while__7 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      #t)
     (#t
      (set! /e (@Find_Value (@Struct_Elts /var)))
      (cond
       ((or (null? /e) (not (= (@ST /e) //T_/Number)) (not (= (@V /e) 0)))
        (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Make 205 0 '()))))))))))))))

(define (/foreach-floop_to_while-6 //Depth //A/S_/Type)
 (cond
  ((member //Depth (@Gen_TVs (@I) //A/S/Type))
   (set! //N (+ //N 1)))))

(define (/foreach-floop_to_while-7 //Depth //A/S_/Type)
 (cond
  ((= //Depth 1)
   ; NB: Process the statements from left to right so as to avoid 
   ; absorbing in (and missing) some statements which need processing. 
   (@Down_Last)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((and (@Right?) (not (@Is_Proper?)))
      (set! //N 0)
      (@Ateach_Terminal /foreach-floop_to_while-6 0 (@AS_Type) 1)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (cond
       ((> //N 1)
        (set! /fl_flag1 1))
       (#t
        ; Can use absorption here 
        (display-list-flush "a")
        (set! /done 0)
        (@Trans //T/R_/Fully_/Absorb_/Right "")
        (set! /fl_flag1 1))))
     ((not (@Left?))
      (set! /fl_flag1 1))
     (#t
      (@Left)
      (set! /fl_flag1 0)))))))

(define (/foreach-floop_to_while-8 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Cond) (@Trans? //T/R_/Else_/If_/To_/Elsif))
   (@Trans //T/R_/Else_/If_/To_/Elsif ""))))

(define (/foreach-floop_to_while-9 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /assign2)
   (@Paste_Over (@Skip)))
  ((= (@ST (@I)) //T_/Exit)
   (cond
    ((equal? (@V (@I)) //Depth)
     (@Paste_Before /assign0))))
  ((= (@ST (@I)) //T_/Skip)
   (@Paste_Over /assign0))
  (#t
   (@Paste_After /assign0))))

(define (/foreach-floop_to_while-10 //Depth //A/S_/Type)
 (@Down)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (cond
   ((and (@Right?) (not (@Is_Proper?)))
    (@Right)
    ; Protect this sequence, unless it is already protected 
    (cond
     ((and (= (@ST (@I)) //T_/Cond) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Equal) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) //T_/Variable) (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1)) /name))
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0))))
     (#t
      (@Left)
      (display-list-flush "i")
      (set! /done 1)
      ; Insert assignments ~?name := 0 to terminal posns 
      ; which don't have assigns (to ensure name is set just before 
      ; it is tested -- called procs may have clobbered it!) 
      (@Foreach_Terminal /foreach-floop_to_while-9 0 (@AS_Type) 1)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      ; Protect the rest of the sequence with an IF: 
      (@Cut_Rest)
      (@Paste_After //S)
      (@Right)
      (@Down)
      (@Down_To 2)
      ; to statement sequence 
      (@Down)
      (@Splice_Over (@Buffer))
      (set! /fl_flag1 0))))
   ((not (@Right?))
    (set! /fl_flag1 1))
   (#t
    (@Right)
    (set! /fl_flag1 0)))))

(define (/foreach-floop_to_while-11 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /assign2)
   (@Paste_Over (@Skip)))
  ((= (@ST (@I)) //T_/Skip)
   (@Paste_Over /assign0))
  ((not (= (@ST (@I)) //T_/Exit))
   (@Paste_After /assign0))
  (#t
   (set! /val (@V (@I)))
   (set! /tv (- /val (+ //Depth 1)))
   ; tv is number of additional loops to terminate 
   ; So if tv < 0 then the main loop will continue: 
   (cond
    ((< /tv 0)
     (@Paste_Over /assign0))
    ((= /tv 0)
     (@Paste_Over /assign1))
    (#t
     (@Paste_Over /assign1)
     (@Down)
     (@Down_To 2)
     ; to the 1 
     (@Paste_Over (@Make //T_/Number (+ /tv 1) '()))
     (@Up)
     (@Up)))
   (cond
    ((> /tv /max)
     (set! /max /tv)))
   (cond
    ((> //Depth 0)
     (@Paste_After (@Make //T_/Exit //Depth '())))))))

(define (/foreach-floop_to_while-12 //Depth //A/S_/Type)
 (cond
  ((and (= //Depth 0) (@Right?) (not (@Is_Proper?)))
   (set! //R 0))))

(define /%const__floop_to_while__1 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()) (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 107 -3 '()))))))))
(define /%const__floop_to_while__2 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 107 -3 '()) (@Make 117 1 '()))))))))
(define /%const__floop_to_while__3 (@Make 133 '() (list (@Make 17 '() (list (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()) (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))) (@Make 107 -3 '()))))))
(define /%const__floop_to_while__4 (@Make 133 '() (list (@Make 17 '() (list (@Make 107 -1 '()) (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -2 '()) (@Make 17 '() (list (@Make 107 -3 '()) (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))) (@Make 107 -4 '()))))))
(define /%const__floop_to_while__5 (@Make 114 '() (list (@Make 7 '() (list (@Make 332 '() (list (@Make 205 1 '()))) (@Make 17 '() (list (@Make 141 '() (list (@Make 332 '() (list (@Make 205 2 '()))) (@Make 17 '() (list (@Make 161 '() (list (@Make 205 3 '()))))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
(define /%const__floop_to_while__6 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 217 -2 '()))))))
(define /%const__floop_to_while__7 (@Make 110 '() (list (@Make 6 '() (list (@Make 510 '() (list (@Make 205 1 '()))) (@Make 205 0 '()))))))
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
(define (@Floop_To_While_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Floop))
   (@Fail "Selected item is not a DO...OD loop."))
  ((member //T_/Call (@Stat_Types (@I)))
   (@Fail "Loop contains action calls"))
  (#t
   (@Pass))))

; If flag is empty, then choose an fl_flag. 
; If flag is - then don't translate the loop if a flag is needed. 
; Otherwise, use the provided flag. 
(define (@Floop_To_While_Code //Data)
 (let ((/assert-save /assert)
       (/flag ""))
  (set! /assert '())
  (set! /flag (@String //Data))
  (set! /assert (@Look_For_Assertion  /assert))
  (@Floop_To_While_Sub /flag /assert)
  (set! /assert /assert-save)))

(define (@Floop_To_While_Sub /flag /assert-par)
 (let ((/assert-save /assert))
  (set! /assert /assert-par)
  (let ((/count-save /count)
        (/posn '()))
   (set! /count 0)
   ; Simplify the case with a single conditional EXIT(1) at the top level: 
   (@Foreach_Statement /foreach-floop_to_while-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (cond
    ((= /count 1)
     (while (@Trans? //T/R_/Separate_/Right) 
      (@Trans //T/R_/Separate_/Right ""))
     (set! /posn (@Posn))
     (@Down)
     (@Down)
     ; to first statement in body 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (cond
        ((and (= (@ST (@I)) //T_/Cond) (not (@Gen_Proper? (@I) (@AS_Type))))
         (@Down)
         (while (and (@Right?) (not (= //T_/Exit (@ST (last-1 (@Cs (@Get_n (@I) 2))))))) 
          (@Right))
         (cond
          ((= //T_/Exit (@ST (last-1 (@Cs (@Get_n (@I) 2)))))
           (while (@Trans? //T/R_/Move_/To_/Left) 
            (@Trans //T/R_/Move_/To_/Left ""))))
         (@Up)))
       (cond
        ((and (= (@ST (@I)) //T_/Cond) (= (@Size (@Get_n (@Get_n (@I) 1) 2)) 1) (= (@ST (last-1 (@Cs (@Get_n (@Get_n (@I) 1) 2)))) //T_/Exit))
         (let ((//B-save //B)
               (//S-save //S))
          (set! //B (@Get_n (@Get_n (@I) 1) 1))
          (set! //S (@Cs (@Get_n (@Get_n (@I) 1) 2)))
          (@Paste_Before (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() //S))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
          (set! //B //B-save)
          (set! //S //S-save))
         (@Right)
         (@Down)
         (@Clever_Delete)
         (set! /fl_flag1 1))
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))
     (@Goto /posn)
     (cond
      ((not (= (@ST (@I)) //T_/Floop))
       (error "Bug in Floop_To_While")))))
   (@Foreach_Statement /foreach-floop_to_while-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Statement /foreach-floop_to_while-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; First check for simple cases: 
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__floop_to_while__3 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S2_save //S2)
            (/__/S1_save //S1)
            (/__/B_save //B))
       (set! //S2 (vector-ref /__/Match_array 2))
       (set! //S1 (vector-ref /__/Match_array 1))
       (set! //B (vector-ref /__/Match_array 0))
       (cond
        ((and (not (null? //S2)) (@Gen_Proper? (@Make //T_/Statements '() (concat //S1 //S2)) (@AS_Type)))
         (@Paste_Over (@Make 141 '() (list (@Not //B) (@Make 17 '() //S2))))
         (@Splice_After //S1))
        (#t
         (@Floop_To_While_Main1 /flag)))
       (set! //S2 /__/S2_save)
       (set! //S1 /__/S1_save)
       (set! //B /__/B_save)))
     (#t
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__floop_to_while__4 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__/S2_save //S2)
               (/__/S1_save //S1)
               (/__/B_save //B)
               (/__/S_save //S))
          (set! //S2 (vector-ref /__/Match_array 3))
          (set! //S1 (vector-ref /__/Match_array 2))
          (set! //B (vector-ref /__/Match_array 1))
          (set! //S (vector-ref /__/Match_array 0))
          (cond
           ((and (not (null? /assert)) (null? //S2) (@Implies? /assert (@Not //B)) (@Gen_Proper? (@Make //T_/Statements '() (concat (concat //S //S1) //S2)) (@AS_Type)))
            (set! //B (@Not //B))
            (@Paste_Over (@Make 141 '() (list //B (@Make 17 '() //S))))
            (@Splice_After //S1)
            (cond
             ((and (null? //S1) (>= (gen-length (@Posn)) 3))
              (let ((/posn (@Posn)))
               (@Up)
               (@Up)
               (@Up)
               (let ((/__/O/K 1))
                (vector-set! /__/Match_array 2 //S)
                (vector-set! /__/Match_array 1 //B)
                (vector-set! /__/Match_array 0 //B)
                (set! /__/O/K (@New_Match  /%const__floop_to_while__5 (@I) /__/O/K))
                (cond
                 ((= /__/O/K 1)
                  (@Paste_Over (@Make 141 '() (list //B (@Make 17 '() //S)))))
                 (#t
                  (@Goto /posn))))))))
           ((and (<= (@Stat_Count_NC (@Make //T_/Statements '() //S)) 4) (@Gen_Proper? (@Make //T_/Statements '() (concat (concat //S //S1) //S2)) (@AS_Type)))
            (set! //B (@Not //B))
            (let ((//S3 //S))
             (while (and (not (null? //S3)) (= (@ST (car //S3)) //T_/Comment)) 
              (set! //S3 (cdr //S3)))
             (cond
              ((and (null? //S2) (null? //S3))
               (set! //S3 (list (@Skip)))))
             (@Paste_Over (@Make 141 '() (list //B (@Make 17 '() (append //S2 //S3)))))
             (@Splice_After //S1)
             (@Splice_Before //S)
             (while (not (null? //S)) 
              (begin
               (@Right)
               (set! //S (cdr //S))))))
           (#t
            (@Floop_To_While_Main1 /flag)))
          (set! //S2 /__/S2_save)
          (set! //S1 /__/S1_save)
          (set! //B /__/B_save)
          (set! //S /__/S_save)))
        (#t
         (@Floop_To_While_Main1 /flag)))))))
   (set! /count /count-save))
  (set! /assert /assert-save)))

; Check if there is an existing flag which can be used for the loop 
; (eg due to a prior use of Separate_Exit_Code) 
; Loop must be a proper statement, body must be reducible, 
; and every exit from the loop should set the flag to a non-zero value 
; and the flag must be initially zero. 
(define (@Floop_To_While_Main1 /flag)
 (cond
  ((not (@Gen_Proper? (@I) (@AS_Type)))
   (@Floop_To_While_Main2 /flag))
  ((not (@Gen_Can_Reduce? (@Get_n (@I) 1) (@AS_Type)))
   (@Floop_To_While_Main2 /flag))
  (#t
   (cond
    ((not (@Gen_Reducible? (@Get_n (@I) 1) (@AS_Type)))
     (@Down)
     (cond
      ((@Trans? //T/R_/Make_/Reducible)
       (@Trans //T/R_/Make_/Reducible ""))
      (#t
       (error "@Floop_To_While_Main1: TR_Make_Reducible failed!")))
     (@Up)))
   ; After making the loop reducible, it may become a dummy loop 
   (cond
    ((@Trans? //T/R_/Remove_/Dummy_/Loop)
     (display-list "Loop turns out to be a dummy loop")
     (@Trans //T/R_/Remove_/Dummy_/Loop ""))
    (#t
     (@Floop_To_While_Main1a /flag))))))

(define (@Floop_To_While_Main1a /flag)
 (let ((/var-save /var)
       (//O/K-save //O/K)
       (/init '())
       (/body '())
       (/e-save /e))
  (set! /var '())
  (set! //O/K 1)
  (set! /e '())
  (@Ateach_Terminal /foreach-floop_to_while-4 0 (@AS_Type) 1)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (cond
   ((null? /var)
    (set! //O/K 0)))
  (cond
   ((= //O/K 1)
    ; Found a suitable var: check its initial value 
    (@Paste_Before (@Skip))
    (set! /init (@Find_Value (@Struct_Elts /var)))
    (@Delete)
    (cond
     ((or (null? /init) (not (= (@ST /init) //T_/Number)) (not (= (@V /init) 0)))
      (set! //O/K 0)))))
  (cond
   ((= //O/K 1)
    ; Ensure flag is cleared at the end of each iteration 
    (@Down)
    (@Ateach_Terminal /foreach-floop_to_while-5 0 (@AS_Type) 1)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Up)
    (set! /body (@Cs (@Increment (@Get_n (@I) 1) (@AS_Type) (- 1) 1)))
    (@Paste_Over (@Make 141 '() (list (@Make 313 '() (list (@Var_To_Expn /var) (@Make 205 0 '()))) (@Make 17 '() /body))))
    (@Trans //T/R_/Delete_/All_/Skips ""))
   (#t
    (@Floop_To_While_Main2 /flag)))
  (set! /var /var-save)
  (set! //O/K //O/K-save)
  (set! /e /e-save)))

(define (@Floop_To_While_Main2 /flag)
 (let ((//Flags (my-map @Make_Name (list "fl_flag1" "fl_flag2" "fl_flag3" "fl_flag4" "fl_flag5" "fl_flag6" "fl_flag7" "fl_flag8" "fl_flag9" "fl_flagX")))
       (/name-save /name)
       (/max-save /max)
       (/done-save /done)
       (//N-save //N))
  (set! /name '())
  (set! /max 10)
  (set! /done 0)
  (set! //N 0)
  ; Choose a suitable flag variable: 
  (cond
   ((equal? /flag "")
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
        (set! /name (@Make_Name (string-append "fl_flag" (@String /max)))))))))
   ((equal? /flag "-")
    (set! /name '()))
   (#t
    (set! /name (@Make_Name /flag))))
  (cond
   ((@Trans? //T/R_/Delete_/Unreachable_/Code)
    (@Trans //T/R_/Delete_/Unreachable_/Code "")))
  (display-list-flush "Using absorption to move EXITs...")
  (while (= /done 0) 
   (begin
    (set! /done 1)
    (@Foreach_Stats /foreach-floop_to_while-7 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (display-list "")
  (@Foreach_Statement /foreach-floop_to_while-8 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Check for small loops that can be converted with a little copying 
  ; instead of generating a flag 
  (cond
   ((and (<= (@Stat_Count (@I)) 10) (<= (@Total_Size (@I)) 100))
    (@Floop_To_While_Invert)))
  (cond
   ((and (= (@ST (@I)) //T_/Floop) (not (equal? /flag "-")))
    ; Inversion didn't work or was not tested for 
    ; Check if we need to insert an IF for some of the sequences: 
    (cond
     ((not (@Exits_Terminal? (@Get_n (@I) 1) (@AS_Type)))
      (@Floop_To_While_Complex /name /flag)))
    (@Floop_To_While_Convert /name /flag)))
  (set! /name /name-save)
  (set! /max /max-save)
  (set! /done /done-save)
  (set! //N //N-save)))

(define (@Floop_To_While_Complex /name-par /flag)
 (let ((/name-save /name))
  (set! /name /name-par)
  (let ((/done-save /done)
        (//S-save //S)
        (/assign0-save /assign0)
        (/assign2-save /assign2))
   (set! /done 0)
   (set! //S '())
   (set! /assign0 '())
   (set! /assign2 '())
   (set! /name (@Make //T_/Variable /name '()))
   (set! /assign0 (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /name) (@Make 205 0 '()))))))
   (set! /assign2 (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /name) (@Var_To_Expn /name))))))
   ; The ~?name := ~?name assignment is a placeholder to tell 
   ; Floop_To_While_Convert to put a SKIP rather than ~?name := 0 
   ; If the flag was given, then we can assume it is unique 
   ; and called procs cannot clobber it! 
   (cond
    ((not (equal? /flag ""))
     (set! /assign0 /assign2)))
   (set! /assign2 (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /name) (@Var_To_Expn /name))))))
   (set! //S (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /name) (@Make 205 0 '()))) (@Make 17 '() (list (@Make 145 '() '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /name) (@Var_To_Expn /name))))))))))))
   (display-list-flush "Inserting IF statements to move EXITs...")
   (while (= /done 0) 
    (begin
     (set! /done 1)
     (@Foreach_Stats /foreach-floop_to_while-10 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))))
   (display-list "")
   (set! /done /done-save)
   (set! //S //S-save)
   (set! /assign0 /assign0-save)
   (set! /assign2 /assign2-save))
  (set! /name /name-save)))

(define (@Floop_To_While_Convert /name-par /flag)
 (let ((/name-save /name))
  (set! /name /name-par)
  (let ((/assign0-save /assign0)
        (/assign1-save /assign1)
        (/body '())
        (/tv-save /tv)
        (/val-save /val)
        (/max-save /max))
   (set! /assign0 '())
   (set! /assign1 '())
   (set! /tv 0)
   (set! /val 0)
   (set! /max 0)
   (set! /name (@Make //T_/Var_/Lvalue /name '()))
   (set! /assign0 (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /name) (@Make 205 0 '()))))))
   (set! /assign1 (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /name) (@Make 205 1 '()))))))
   (set! /assign2 (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /name) (@Var_To_Expn /name))))))
   ; If the flag was given, then we can assume it is unique 
   ; and called procs cannot clobber it! 
   (cond
    ((not (equal? /flag ""))
     (set! /assign0 (@Skip))))
   (@Down)
   ; to body 
   (@Foreach_Terminal /foreach-floop_to_while-11 0 (@AS_Type) 1)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Up)
   (@Trans //T/R_/Delete_/All_/Skips "")
   (set! /body (@Cs (@Get_n (@I) 1)))
   (@Paste_Over (@Make 141 '() (list (@Make 313 '() (list (@Var_To_Expn /name) (@Make 205 0 '()))) (@Make 17 '() /body))))
   (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /name) (@Make 205 0 '()))))))
   (@Right)
   (cond
    ((> /max 0)
     ; Ensure that any enclosing loops are also terminated as needed: 
     (set! /name (@Make //T_/Variable (@V /name) '()))
     (@Paste_After (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /name) (@Make 205 2 '()))) (@Make 17 '() (list (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
     (@Right)
     (@Down)
     ; to the guarded 
     (set! /n 3)
     (while (<= /n (+ /max 1)) 
      (begin
       (@Paste_After (@Make //T_/Guarded '() (list (@Make //T_/Equal '() (list /name (@Make //T_/Number /n '()))) (@Make //T_/Statements '() (list (@Make //T_/Exit (- /n 1) '()))))))
       (@Right)
       (set! /n (+ /n 1))))
     (@Up)
     (@Left)
     ; back to the loop 
    ))
   (set! /assign0 /assign0-save)
   (set! /assign1 /assign1-save)
   (set! /tv /tv-save)
   (set! /val /val-save)
   (set! /max /max-save))
  (set! /name /name-save)))

(define (@Exits_Terminal? //I //A/S/Type-par)
 (let ((//A/S/Type-save //A/S/Type)
       (//R-save //R)
       (funct-result '()))
  (set! //A/S/Type //A/S/Type-par)
  (set! //R 1)
  (@Edit)
  (@New_Program //I)
  (@Ateach_Statement /foreach-floop_to_while-12 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Undo_Edit)
  (set! funct-result (= //R 1))
  (set! //A/S/Type //A/S/Type-save)
  (set! //R //R-save)
  funct-result))

; Fix this (small) floop by using loop inversion. 
; S1 = statements to invert and copy outside loop 
; S2 = rest of loop body (ELSE clause) 
; G1 = guardeds for the Cond statement forming the body of the WHILE 
; G2 = guardeds for the Cond statement which goes after the WHILE 
; B  = the condition under which the loop terminates 
(define (@Floop_To_While_Invert)
 (let ((//O/K-save //O/K)
       (//S1-save //S1)
       (//S2-save //S2)
       (//G1 '())
       (//G2 '())
       (//B-save //B)
       (//A/S (@AS_Type)))
  (set! //O/K 1)
  (set! //S1 '())
  (set! //S2 '())
  (set! //B (@Make //T_/False '() '()))
  (@Edit)
  ; All but the last statement in the body should be proper 
  (@Down)
  (@Down)
  ; to first statement in body 
  (while (and (or (@Gen_Proper? (@I) //A/S) (not (@Right?))) (@Right?)) 
   (begin
    (set! //S1 (cons (@I) //S1))
    (@Right)))
  (cond
   ((and (not (@Gen_Proper? (@I) //A/S)) (@Right?))
    (set! //O/K 0)))
  (cond
   ((and (= //O/K 1) (= (@ST (@I)) //T_/Cond))
    (set! //S1 (reverse //S1))
    ; Could use TR_Move_Right to move non-terminating guardeds forwards here 
    (@Down)
    ; to first guarded 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((not (@Gen_Proper? (@I) //A/S))
        (cond
         ((or (not (@Gen_Improper? (@I) //A/S)) (not (null? //G1)))
          (set! //O/K 0)
          (set! /fl_flag1 1))
         (#t
          ; If this branch is taken, then the loop terminates 
          (set! //B (@Or //B (@Get_n (@I) 1)))
          (set! //G2 (cons (@Increment (@I) //A/S (- 1) 1) //G2))
          (set! /fl_flag1 0))))
       (#t
        ; This branch is within the loop body 
        ; No subsequent branches should terminate the loop 
        (set! //G1 (cons (@I) //G1))
        (set! /fl_flag1 0)))
      (cond
       ((= /fl_flag1 0)
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0))))))))
   (#t
    (set! //O/K 0)))
  (cond
   ((= //O/K 1)
    (set! //G1 (@Make //T_/Cond '() (reverse //G1)))
    (set! //G2 (@Make //T_/Cond '() (reverse //G2)))
    (set! //B (@Not //B))
    (@New_Program (@Make 17 '() (append //S1 (list (@Make 141 '() (list //B (@Make 17 '() (append (list //G1) //S1)))) //G2))))
    (@Trans //T/R_/Simplify "")
    (set! //S1 (@Cs (@Program)))
    (@Undo_Edit)
    (@Splice_Over //S1))
   (#t
    (@Undo_Edit)))
  #t
  (set! //O/K //O/K-save)
  (set! //S1 //S1-save)
  (set! //S2 //S2-save)
  (set! //B //B-save)))

(define (@Look_For_Assertion /assert-par)
 (let ((/assert-save /assert)
       (funct-result '()))
  (set! /assert /assert-par)
  ; Check for a surrounding Cond which may be useful 
  (cond
   ((and (> (gen-length (@Posn)) 2) (= (@ST (@GGParent)) //T_/Cond) (= (@Posn_n) 1))
    (@Up)
    (@Up)
    ; to the guarded 
    (let ((/posn_n (@Posn_n)))
     (set! /assert (@Get_n (@I) 1))
     (while (@Left?) 
      (begin
       (@Left)
       (set! /assert (@And /assert (@Not (@Get_n (@I) 1))))))
     (@To /posn_n)
     (@Down_To 2)
     (@Down)))
   ((@Left?)
    (@Left)
    (cond
     ((= (@ST (@I)) //T_/Assert)
      (set! /assert (@Get_n (@I) 1))))
    (@Right)))
  (set! funct-result /assert)
  (set! /assert /assert-save)
  funct-result))

#t
