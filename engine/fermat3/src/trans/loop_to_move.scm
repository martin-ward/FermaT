;;; Scheme translation of WSL code
(define (/foreach-loop_to_move-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Comment)
   (@Delete))))

(define /%const__loop_to_move__1 (@Make 318 '() (list (@Make 217 -1 '()) (@Make 205 0 '()))))
(define /%const__loop_to_move__2 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 210 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 10 '() (list (@Make 217 -2 '()))))))))))
(define /%const__loop_to_move__3 (@Make 110 '() (list (@Make 6 '() (list (@Make 502 '() (list (@Make 501 (@Make_Name "a") '()) (@Make 10 '() (list (@Make 217 -1 '()))))) (@Make 217 -2 '()))))))
(define /%const__loop_to_move__4 (@Make 110 '() (list (@Make 6 '() (list (@Make 502 '() (list (@Make 501 (@Make_Name "a") '()) (@Make 10 '() (list (@Make 217 -1 '()))))) (@Make 210 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 10 '() (list (@Make 217 -2 '()))))))))))
(define /%const__loop_to_move__5 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 220 '() (list (@Make 263 -1 '()) (@Make 205 1 '()))))))))
(define /%const__loop_to_move__6 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 221 '() (list (@Make 263 -1 '()) (@Make 205 1 '()))))))))
(define /%const__loop_to_move__7 (@Make 110 '() (list (@Make 6 '() (list (@Make 504 '() (list (@Make 501 (@Make_Name "a") '()) (@Make 217 -1 '()) (@Make 205 256 '()))) (@Make 212 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 217 -2 '()) (@Make 205 256 '()))))))))
(define /%const__loop_to_move__8 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 220 '() (list (@Make 263 -1 '()) (@Make 205 256 '()))))))))
(define /%const__loop_to_move__9 (@Make 110 '() (list (@Make 6 '() (list (@Make 510 '() (list (@Make 205 1 '()))) (@Make 221 '() (list (@Make 261 '() (list (@Make 205 2 '()))) (@Make 205 256 '()))))))))
(define /%const__loop_to_move__10 (@Make 110 '() (list (@Make 6 '() (list (@Make 510 '() (list (@Make 205 1 '()))) (@Make 220 '() (list (@Make 261 '() (list (@Make 205 2 '()))) (@Make 205 256 '()))))))))
(define /%const__loop_to_move__11 (@Make 114 '() (list (@Make 7 '() (list (@Make 318 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 0 '()))) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 504 '() (list (@Make 501 (@Make_Name "a") '()) (@Make 261 '() (list (@Make 205 2 '()))) (@Make 261 '() (list (@Make 205 3 '()))))) (@Make 212 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 261 '() (list (@Make 205 4 '()))) (@Make 261 '() (list (@Make 205 5 '()))))))))) (@Make 107 -6 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
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
; Attempt to convert a loop to a data move operation 
(set! //L/T/M_/Types (@Make_Set (list //T_/Comment //T_/Assignment //T_/Cond //T_/Exit //T_/Skip)))
(define (@Loop_To_Move_Test)
 (let ((/assert '()))
  (cond
   ((and (not (= (@ST (@I)) //T_/Floop)) (not (= (@ST (@I)) //T_/While)))
    (@Fail "Selected item is not a DO...OD or WHILE loop."))
   ((and (= (@ST (@I)) //T_/Floop) (not (@Set_Subset? (@Stat_Types (@Get_n (@I) 1)) //L/T/M_/Types)))
    (@Fail "DO...OD loop body contains unsuitable types"))
   ((and (= (@ST (@I)) //T_/While) (not (@Set_Subset? (@Stat_Types (@Get_n (@I) 2)) //L/T/M_/Types)))
    (@Fail "WHILE loop body contains unsuitable types"))
   ((= (@ST (@I)) //T_/Floop)
    (set! /assert (@Look_For_Assertion  /assert))
    (@Edit_Parent)
    (@Floop_To_While_Sub "-" /assert)
    (cond
     ((= (@ST (@I)) //T_/While)
      (@Loop_To_Move_Sub)
      (cond
       ((and (= (@ST (@I)) //T_/While) (= (@ST (@Get_n (@I) 1)) //T_/Greater_/Eq) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (= (@V (@Get_n (@Get_n (@I) 1) 2)) 0))
        (@Loop_To_Move_Sub2 (@Get_n (@Get_n (@I) 1) 1))))
      (cond
       ((= (@ST (@I)) //T_/While)
        (@Fail "Could not process the WHILE loop"))
       (#t
        (@Pass))))
     (#t
      (@Fail "Could not convert the DO...OD loop")))
    (@Undo_Edit))
   ((= (@ST (@I)) //T_/While)
    (@Edit_Parent)
    (@Loop_To_Move_Sub)
    (cond
     ((= (@ST (@I)) //T_/While)
      (@Fail "Could not process the WHILE loop"))
     (#t
      (@Pass)))
    (@Undo_Edit))
   (#t
    (@Fail "Internal error!!!")))))

; Sample loop:
;
;WHILE r5 <> 0 DO
;  r9[4] := a[r1];
;  a[r6] := a[r1];
;  r6 := r6 + 1;
;  r1 := r1 + 1;
;  r5 := r5 - 1 OD
;
;Another sample:
;
;WHILE r5 <> 0 DO
;  r9[4] := a[r1];
;  a[r6] := r9[4];
;  r6 := r6 + 1;
;  r1 := r1 + 1;
;  r5 := r5 - 1 OD
;
;And another:
;
;WHILE r1 <> 0 DO
;  a[r14] := a[r15];
;  r14 := r14 - 1;
;  r15 := r15 - 1;
;  r1 := r1 - 1 OD
;
;
(define (@Loop_To_Move_Code //Data)
 (let ((/assert '()))
  (cond
   ((= (@ST (@I)) //T_/Floop)
    (set! /assert (@Look_For_Assertion  /assert))
    (@Edit_Parent)
    (@Floop_To_While_Sub "-" /assert)
    (cond
     ((= (@ST (@I)) //T_/While)
      (@Loop_To_Move_Sub)
      (cond
       ((and (= (@ST (@I)) //T_/While) (= (@ST (@Get_n (@I) 1)) //T_/Greater_/Eq) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (= (@V (@Get_n (@Get_n (@I) 1) 2)) 0))
        (@Loop_To_Move_Sub2 (@Get_n (@Get_n (@I) 1) 1))))
      (cond
       ((= (@ST (@I)) //T_/While)
        (error "@Loop_To_Move_Code: Could not process the WHILE loop"))
       (#t
        (@Pass))))
     (#t
      (@Fail "Could not convert the DO...OD loop")))
    (@End_Edit))
   ((= (@ST (@I)) //T_/While)
    (@Edit_Parent)
    (@Loop_To_Move_Sub)
    (cond
     ((= (@ST (@I)) //T_/While)
      (error "@Loop_To_Move_Code: Could not process the WHILE loop"))
     (#t
      (@Pass)))
    (@End_Edit))
   (#t
    (error "@Loop_To_Move_Code: Internal error!!!")))))

; p1 is dest pointer, p2 is source pointer, var is extra variable 
; i1, i2, i3 indicate if p1 and p2 are incremented and count is decremented 
(define (@Loop_To_Move_Sub)
 (let ((/count '())
       (/p1 '())
       (/p2 '())
       (/var '())
       (/i1 0)
       (/i2 0)
       (/i3 0)
       (/d1 0)
       (/d2 0)
       (//O/K 1)
       (/match-save /match)
       (/comments-save /comments)
       (/p 0))
  (set! /match 0)
  (set! /comments '())
  (@Down)
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__loop_to_move__1 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__v_save /v))
      (set! /v (vector-ref /__/Match_array 0))
      (set! /count /v)
      (set! /v /__v_save)))
    (#t
     (set! //O/K 0))))
  (@Up)
  (cond
   ((= //O/K 1)
    (@Down_To 2)
    (@Down)
    ; to start of body 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (set! /match 0)
      (cond
       ((= (@ST (@I)) //T_/Comment)
        (set! /match 1)
        (set! /comments (cons (@I) /comments)))
       ((= (@ST (@I)) //T_/Skip)
        (set! /match 1)))
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__loop_to_move__2 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__e2_save /e2)
               (/__v_save /v))
          (set! /e2 (vector-ref /__/Match_array 1))
          (set! /v (vector-ref /__/Match_array 0))
          (cond
           ((or (= (@ST /v) //T_/Var_/Lvalue) (= (@ST /v) //T_/Struct_/Lvalue) (and (= (@ST /v) //T_/Aref_/Lvalue) (= (@ST (@Get_n (@Get_n /v 2) 1)) //T_/Number)))
            (set! /match 1)
            (set! /var /v)
            (cond
             ((null? /p2)
              (set! /p2 /e2))
             ((not (@Equal? /p2 /e2))
              (set! //O/K 0)))))
          (set! /e2 /__e2_save)
          (set! /v /__v_save)))))
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__loop_to_move__3 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__v_save /v)
               (/__e1_save /e1))
          (set! /v (vector-ref /__/Match_array 1))
          (set! /e1 (vector-ref /__/Match_array 0))
          (cond
           ((and (not (null? /var)) (@LR_Equal? /var /v))
            (set! /match 1)
            (cond
             ((null? /p1)
              (set! /p1 /e1))
             ((not (@Equal? /p1 /e1))
              (set! //O/K 0)))))
          (set! /v /__v_save)
          (set! /e1 /__e1_save)))))
      (cond
       ((= /match 0)
        (let ((/__/O/K 1))
         (set! /__/O/K (@New_Match  /%const__loop_to_move__4 (@I) /__/O/K))
         (cond
          ((= /__/O/K 1)
           (let ((/__e2_save /e2)
                 (/__e1_save /e1))
            (set! /e2 (vector-ref /__/Match_array 1))
            (set! /e1 (vector-ref /__/Match_array 0))
            (set! /match 1)
            (cond
             ((null? /p1)
              (set! /p1 /e1))
             ((not (@Equal? /p1 /e1))
              (set! //O/K 0)))
            (cond
             ((null? /p2)
              (set! /p2 /e2))
             ((not (@Equal? /p2 /e2))
              (set! //O/K 0)))
            (set! /e2 /__e2_save)
            (set! /e1 /__e1_save)))))))
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__loop_to_move__5 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__e1_save /e1))
          (set! /e1 (vector-ref /__/Match_array 0))
          (set! /match 1)
          (cond
           ((and (not (null? /p1)) (@LR_Equal? /e1 /p1))
            (set! /i1 1))
           ((and (not (null? /p2)) (@LR_Equal? /e1 /p2))
            (set! /i2 1))
           (#t
            (set! //O/K 0)))
          (set! /e1 /__e1_save)))))
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__loop_to_move__6 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__e1_save /e1))
          (set! /e1 (vector-ref /__/Match_array 0))
          (set! /match 1)
          (cond
           ((@LR_Equal? /e1 /count)
            (set! /i3 1))
           ((and (not (null? /p1)) (@LR_Equal? /e1 /p1))
            (set! /d1 1))
           ((and (not (null? /p2)) (@LR_Equal? /e1 /p2))
            (set! /d2 1))
           (#t
            (set! //O/K 0)))
          (set! /e1 /__e1_save)))))
      (cond
       ((and #f (and (not (= (@ST (@I)) //T_/Skip)) (not (= (@ST (@I)) //T_/Comment))))
        (display-list "")
        (@PP_Item (@I) 80 "")
        (display-list "match = " /match)
        (display-list "p1 = " /p1)
        (display-list "p2 = " /p2)
        (display-list "var = " /var)
        (display-list "i1 = " /i1 " i2 = " /i2 " i3 = " /i3)
        (display-list "d1 = " /d1 " d2 = " /d2)))
      (cond
       ((= /match 0)
        (set! //O/K 0)))
      (cond
       ((= //O/K 0)
        (set! /fl_flag1 1))
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /fl_flag1 0)))))
    (@Up)
    (@Up)
    (cond
     ((or (and (or (= /i1 0) (= /i2 0)) (or (= /d1 0) (= /d2 0))) (= /i3 0))
      (set! //O/K 0)))
    (cond
     ((or (null? /p1) (null? /p2))
      (set! //O/K 0)))
    (cond
     ((and (= /i1 1) (= /i2 1) (= /i3 1))
      (set! /p (@Posn_n))
      (@Splice_Over (@Cs (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 504 '() (list (@Make 501 (@Make_Name "a") '()) (@Var_To_Expn /p1) (@Var_To_Expn /count))) (@Make 212 '() (list (@Make 207 (@Make_Name "a") '()) (@Var_To_Expn /p2) (@Var_To_Expn /count))))))) (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /p1) (@Make 220 '() (list (@Var_To_Expn /p1) (@Var_To_Expn /count))))))) (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /p2) (@Make 220 '() (list (@Var_To_Expn /p2) (@Var_To_Expn /count)))))))))))
      (@Right)
      (@Right)
      (cond
       ((not (null? /var))
        (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Make 210 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 10 '() (list (@Make 221 '() (list (@Var_To_Expn /p2) (@Make 205 1 '()))))))))))))
        (@Right)))
      (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /count) (@Make 205 0 '()))))))
      (@To /p)
      (@Splice_Before (reverse /comments))
      (@To (+ /p (gen-length /comments))))
     ((and (= /d1 1) (= /d2 1) (= /i3 1))
      (set! /p (@Posn_n))
      (@Splice_Over (@Cs (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /p1) (@Make 221 '() (list (@Var_To_Expn /p1) (@Var_To_Expn /count))))))) (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /p2) (@Make 221 '() (list (@Var_To_Expn /p2) (@Var_To_Expn /count))))))) (@Make 110 '() (list (@Make 6 '() (list (@Make 504 '() (list (@Make 501 (@Make_Name "a") '()) (@Make 220 '() (list (@Var_To_Expn /p1) (@Make 205 1 '()))) (@Var_To_Expn /count))) (@Make 212 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 220 '() (list (@Var_To_Expn /p2) (@Make 205 1 '()))) (@Var_To_Expn /count)))))))))))
      (@Right)
      (@Right)
      (cond
       ((not (null? /var))
        (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Make 210 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 10 '() (list (@Make 221 '() (list (@Var_To_Expn /p2) (@Make 205 1 '()))))))))))))
        (@Right)))
      (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /count) (@Make 205 0 '()))))))
      (@To /p)
      (@Splice_Before (reverse /comments))
      (@To (+ /p (gen-length /comments)))))))
  (set! /match /match-save)
  (set! /comments /comments-save)))

; Look for a loop of this form:
;
;WHILE count >= 0 DO
;  a[dest, 256] := a[src, 256];
;  dest := dest + 256;
;  src := src + 256;
;  count := count - 256 OD;
;count := count + 256;
;IF count <> 0
;  THEN a[dest, count] := a[src, count] FI
;
;
(define (@Loop_To_Move_Sub2 /count)
 (let ((/dest '())
       (/src '())
       (/i1 0)
       (/i2 0)
       (/i3 0)
       (//O/K 1)
       (/p (@Posn_n))
       (/p1 0)
       (/p2 0))
  (@Down_To 2)
  (@Down)
  ; to start of body 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (set! /match 0)
    (cond
     ((= (@ST (@I)) //T_/Comment)
      (set! /match 1)
      (set! /comments (cons (@I) /comments)))
     ((= (@ST (@I)) //T_/Skip)
      (set! /match 1)))
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__loop_to_move__7 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__e2_save /e2)
             (/__e1_save /e1))
        (set! /e2 (vector-ref /__/Match_array 1))
        (set! /e1 (vector-ref /__/Match_array 0))
        (set! /match 1)
        (cond
         ((null? /dest)
          (set! /dest /e1))
         ((not (@Equal? /dest /e1))
          (set! //O/K 0)))
        (cond
         ((null? /src)
          (set! /src /e2))
         ((not (@Equal? /src /e2))
          (set! //O/K 0)))
        (set! /e2 /__e2_save)
        (set! /e1 /__e1_save)))))
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__loop_to_move__8 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__v_save /v))
        (set! /v (vector-ref /__/Match_array 0))
        (set! /match 1)
        (cond
         ((or (null? /dest) (null? /src))
          (set! //O/K 0))
         ((@LR_Equal? /v /dest)
          (set! /i1 (+ /i1 1)))
         ((@LR_Equal? /v /src)
          (set! /i2 (+ /i2 1)))
         (#t
          (set! //O/K 0)))
        (set! /v /__v_save)))))
    (let ((/__/O/K 1))
     (vector-set! /__/Match_array 1 /count)
     (vector-set! /__/Match_array 0 /count)
     (set! /__/O/K (@New_Match  /%const__loop_to_move__9 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (set! /match 1)
       (set! /i3 (+ /i3 1)))))
    (cond
     ((and #f (and (not (= (@ST (@I)) //T_/Skip)) (not (= (@ST (@I)) //T_/Comment))))
      (display-list "")
      (@PP_Item (@I) 80 "")
      (display-list "match = " /match)
      (display-list "dest = " /dest)
      (display-list "src  = " /src)
      (display-list "i1 = " /i1 " i2 = " /i2 " i3 = " /i3)))
    (cond
     ((= /match 0)
      (set! //O/K 0)))
    (cond
     ((= //O/K 0)
      (set! /fl_flag1 1))
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@Up)
  (@Up)
  (cond
   ((and (= //O/K 1) (= /i1 1) (= /i2 1) (= /i3 1))
    ; Search for surrounding count := count - 256 ... count := count + 256 
    (cond
     ((@Left?)
      (@Left)))
    (while (and (= (@ST (@I)) //T_/Comment) (@Left?)) 
     (@Left))
    (let ((/__/O/K 1))
     (vector-set! /__/Match_array 1 /count)
     (vector-set! /__/Match_array 0 /count)
     (set! /__/O/K (@New_Match  /%const__loop_to_move__9 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (set! /p1 (@Posn_n)))
      (#t
       (set! //O/K 0))))
    (@To /p)
    (cond
     ((@Right?)
      (@Right)))
    (while (and (= (@ST (@I)) //T_/Comment) (@Right?)) 
     (@Right))
    (let ((/__/O/K 1))
     (vector-set! /__/Match_array 1 /count)
     (vector-set! /__/Match_array 0 /count)
     (set! /__/O/K (@New_Match  /%const__loop_to_move__10 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (set! /p2 (@Posn_n)))
      (#t
       (set! //O/K 0))))
    ; Look for IF count <> 0 THEN a[dest, count] := a[source, count] FI 
    (cond
     ((@Right?)
      (@Right)))
    (while (and (= (@ST (@I)) //T_/Comment) (@Right?)) 
     (@Right))
    (cond
     (#f
      (display-list "")
      (display-list "p1 = " /p1 " p = " /p " p2 = " /p2 " OK = " //O/K)
      (@PP_Item (@I) 80 "")))
    (cond
     ((and (= //O/K 1) (= (@ST (@I)) //T_/Cond) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Not_/Equal))
      (@Foreach_Statement /foreach-loop_to_move-1 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (let ((/__/O/K 1))
       (vector-set! /__/Match_array 4 /count)
       (vector-set! /__/Match_array 3 /src)
       (vector-set! /__/Match_array 2 /count)
       (vector-set! /__/Match_array 1 /dest)
       (vector-set! /__/Match_array 0 /count)
       (set! /__/O/K (@New_Match  /%const__loop_to_move__11 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__/S_save //S))
          (set! //S (vector-ref /__/Match_array 5))
          (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Make 504 '() (list (@Make 501 (@Make_Name "a") '()) (@Var_To_Expn /dest) (@Var_To_Expn /count))) (@Make 212 '() (list (@Make 207 (@Make_Name "a") '()) (@Var_To_Expn /src) (@Var_To_Expn /count))))))))
          (@Splice_After //S)
          (@To /p1)
          (@Paste_Over (@Skip))
          (@To /p2)
          (@Paste_Over (@Skip))
          (@To /p)
          (@Paste_Over (@Skip))
          (set! //S /__/S_save)))
        (#t
         (set! //O/K 0))))))
    (@To /p)))))

#t
