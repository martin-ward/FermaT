;;; Scheme translation of WSL code
(define /%const__simplify_if__1 (@Make 114 '() (list (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 107 -1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
(define /%const__simplify_if__2 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 133 '() (list (@Make 17 '() (list (@Make 107 -2 '()) (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -3 '()) (@Make 17 '() (list (@Make 117 1 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
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
; Any Cond statement with more than this number of guards will 
; not be processed: 
(set! //Max_/Cond_/Size 15)
(define (@Simplify_If_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Cond))
   (@Fail "Not an IF statement"))
  ((@SI_Special? (@I))
   (@Pass))
  ((> (@Size (@I)) //Max_/Cond_/Size)
   (@Fail "IF statement is too large"))
  ((and (= (@ST (@Get_n (@Get_n (@I) (- (@Size (@I)) 1)) 1)) //T_/Not_/Equal) (= (@ST (@Get_n (@Get_n (@I) (@Size (@I))) 1)) //T_/True))
   (@Pass))
  ((@Simplify_If_Loop?)
   (@Pass))
  (#t
   ;Test for redundant or false cases
   (let ((//D (@Make //T_/False '() '()))
         (//B '()))
    (@Down)
    ; to first guarded 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((or (> (@Total_Size //D) 100) (> (@Total_Size (@Get_n (@I) 1)) 100))
       (set! /fl_flag1 1))
      (#t
       (set! //B (@Simplify_Cond (@Get_n (@I) 1)))
       (cond
        ((@Simplify_If_Using? //D //B (@Cs (@Get_n (@I) 2)))
         (@Pass)))
       (cond
        ((or (@Passed?) (not (@Right?)))
         (set! /fl_flag1 1))
        (#t
         (cond
          ((or (= (@ST //B) //T_/False) (= (@ST //B) //T_/True))
           (@Pass))
          ((@Implies? //B //D)
           (@Pass))
          ((= (@ST (@Or //D //B)) //T_/True)
           (@Pass))
          ((equal? (@Stat_Types (@Get_n (@I) 2)) (list //T_/Comment))
           (@Pass))
          (#t
           (set! //D (@Or //D //B))))
         (@Right)
         (cond
          ((and (< (@Total_Size (@Get_n (@I) 1)) 100) (< (@Total_Size //D) 100) (< (@Total_Size (@Simplify_Using (@Get_n (@I) 1) (@Not //D) 20)) (@Total_Size (@Get_n (@I) 1))))
           (@Pass)
           (set! /fl_flag1 0))
          (#t
           (set! /fl_flag1 0))))))))
    (@Up)
    (cond
     ((not (@Passed?))
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__simplify_if__1 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__/S_save //S))
          (set! //S (vector-ref /__/Match_array 0))
          (@Pass)
          (set! //S /__/S_save)))))))
    (cond
     ((and (not (@Passed?)) (@Trans? //T/R_/Separate_/Right))
      (@Pass)))
    (cond
     ((and (not (@Passed?)) (@Trans? //T/R_/Separate_/Left))
      (@Pass)))
    (cond
     ((not (@Passed?))
      (@Fail "No simplification possible")))))))

(define (@Simplify_If_Code //Data)
 (cond
  ((@SI_Special? (@I))
   (@SI_Special))
  ((@Simplify_If_Loop?)
   (let ((//O/K 0))
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__simplify_if__2 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__/B2_save //B2)
             (/__/S_save //S)
             (/__/B1_save //B1))
        (set! //B2 (vector-ref /__/Match_array 2))
        (set! //S (vector-ref /__/Match_array 1))
        (set! //B1 (vector-ref /__/Match_array 0))
        (@Paste_Over (@Make 141 '() (list //B1 (@Make 17 '() //S))))
        (set! //B2 /__/B2_save)
        (set! //S /__/S_save)
        (set! //B1 /__/B1_save)))
      (#t
       (error "@Simplify_If_Code: match failed!"))))))
  (#t
   (let ((/n (@Size (@I)))
         (//D (@Make //T_/False '() '()))
         (//B '())
         (//P '())
         (//Q '())
         (/comments '()))
    (@Edit)
    (cond
     ((and (= (@ST (@Get_n (@Get_n (@I) (- /n 1)) 1)) //T_/Not_/Equal) (= (@ST (@Get_n (@Get_n (@I) /n) 1)) //T_/True))
      (@Down_To (- /n 1))
      (@Trans //T/R_/Move_/To_/Right "")
      (@Up)))
    (@Down)
    ; Take out comments in branches that only contain comments 
    (cond
     ((equal? (@Stat_Types (@Get_n (@I) 2)) (list //T_/Comment))
      (set! /comments (concat /comments (@Cs (@Get_n (@I) 2))))
      (@Down_To 2)
      (@Paste_Over (@Skips))
      (@Up)))
    (while (@Right?) 
     (begin
      (@Right)
      (cond
       ((equal? (@Stat_Types (@Get_n (@I) 2)) (list //T_/Comment))
        (set! /comments (concat /comments (@Cs (@Get_n (@I) 2))))
        (@Down_To 2)
        (@Paste_Over (@Skips))
        (@Up)))))
    (cond
     ((null? /comments)
      (@To 1))
     (#t
      (@Up)
      (@End_Edit)
      (@Splice_Before /comments)
      (while (not (null? /comments)) 
       (begin
        (@Right)
        (set! /comments (cdr /comments))))
      (@Edit)
      (@Down)))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (set! //B (@Simplify_Cond (@Get_n (@I) 1)))
      (cond
       ((@Simplify_If_Using? //D //B (@Cs (@Get_n (@I) 2)))
        (@Down_To 2)
        (@Simplify_If_Using //D //B)
        ; Simplify_If_Using could delete the whole sequence 
        (cond
         ((@Up?)
          (@Up)))
        (set! /fl_flag1 1))
       ((not (@Right?))
        (set! /fl_flag1 1))
       ((or (= (@ST //B) //T_/False) (@Implies? //B //D))
        (@Delete)
        (set! /fl_flag1 0))
       ((or (= (@ST //B) //T_/True) (= (@ST (@Or //D //B)) //T_/True))
        (@Down)
        (@Paste_Over (@Make //T_/True '() '()))
        (@Up)
        (@Delete_Rest)
        (set! /fl_flag1 0))
       (#t
        (set! //D (@Or //D //B))
        (@Right)
        (set! //Q (@Simplify_Using (@Get_n (@I) 1) (@Not //D) 20))
        (cond
         ((< (@Total_Size //Q) (@Total_Size (@Get_n (@I) 1)))
          (@Down)
          (@Paste_Over //Q)
          (@Up)
          (set! /fl_flag1 0))
         (#t
          (set! /fl_flag1 0)))))))
    (@End_Edit)
    (@Fix_Cond)
    (cond
     ((= (@ST (@I)) //T_/Cond)
      (cond
       ((and (= (@Size (@I)) 2) (@True? (@Get_n (@Get_n (@I) 1) 1)))
        (@Splice_Over (@Components (@Get_n (@Get_n (@I) 1) 2))))
       (#t
        (cond
         ((@Trans? //T/R_/Separate_/Right)
          (@Trans //T/R_/Separate_/Right "")))
        (cond
         ((@Trans? //T/R_/Separate_/Left)
          (@Trans //T/R_/Separate_/Left "")))
        (@Trans //T/R_/Delete_/All_/Skips "")))))))))

(define (@Simplify_If_Loop?)
 (let ((//O/K 0))
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__simplify_if__2 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__/B2_save //B2)
           (/__/S_save //S)
           (/__/B1_save //B1))
      (set! //B2 (vector-ref /__/Match_array 2))
      (set! //S (vector-ref /__/Match_array 1))
      (set! //B1 (vector-ref /__/Match_array 0))
      (cond
       ((and (@Gen_Proper? (@Make //T_/Statements '() //S) (@AS_Type)) (@Equal? //B1 (@Not //B2)))
        (set! //O/K 1)))
      (set! //B2 /__/B2_save)
      (set! //S /__/S_save)
      (set! //B1 /__/B1_save)))))
  (= //O/K 1)))

; Can we simplify the tests in a nested IF 
; using the fact that D is false and B is true? 
(define (@Simplify_If_Using? //D //B //Cs)
 (let ((//O/K 0))
  (while (and (not (null? //Cs)) (= (@ST (car //Cs)) //T_/Comment)) 
   (set! //Cs (cdr //Cs)))
  (cond
   ((and (not (null? //Cs)) (= (@ST (car //Cs)) //T_/Cond))
    (set! //D (@Or //D (@Not //B)))
    (set! //Cs (@Cs (car //Cs)))
    (while (not (null? //Cs)) 
     (begin
      (set! //B (@Get_n (car //Cs) 1))
      (cond
       ((@Simplify_If_Using? //D //B (@Cs (@Get_n (car //Cs) 2)))
        (set! //O/K 1)
        (set! //Cs '()))
       ((null? (cdr //Cs))
        (set! //Cs '()))
       ((@Implies? //B //D)
        (set! //O/K 1)
        (set! //Cs '()))
       ((or (= (@ST //B) //T_/True) (= (@ST (@Or //D //B)) //T_/True))
        (set! //O/K 1)
        (set! //Cs '()))
       (#t
        (set! //D (@Or //D //B))
        (set! //Cs (cdr //Cs))))))))
  (= //O/K 1)))

; Simplify the statement sequence item using the fact that D is false and B is true 
(define (@Simplify_If_Using //D //B)
 (@Down)
 (while (and (@Right?) (= (@ST (@I)) //T_/Comment)) 
  (@Right))
 (cond
  ((= (@ST (@I)) //T_/Cond)
   (set! //D (@Or //D (@Not //B)))
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (set! //B (@Simplify_Cond (@Get_n (@I) 1)))
     (cond
      ((@Simplify_If_Using? //D //B (@Cs (@Get_n (@I) 2)))
       (@Down_To 2)
       (@Simplify_If_Using //D //B)
       (@Up)
       (set! /fl_flag1 1))
      ((not (@Right?))
       (set! /fl_flag1 1))
      ((@Implies? //B //D)
       (@Delete)
       (set! /fl_flag1 0))
      ((or (= (@ST //B) //T_/True) (= (@ST (@Or //D //B)) //T_/True))
       (@Down)
       (@Paste_Over (@Make //T_/True '() '()))
       (@Up)
       (@Delete_Rest)
       (set! /fl_flag1 0))
      (#t
       (set! //D (@Or //D //B))
       (@Right)
       (set! /fl_flag1 0)))))
   (@Up)
   (@Fix_Cond)))
 (@Up))

; Special case 
(define (@SI_Special? //I)
 (let ((//O/K 1)
       (/v '())
       (//S1 '())
       (//S2 '())
       (//L '()))
  (cond
   ((< (@Size //I) 3)
    (set! //O/K 0))
   ((not (@SI_Special_Guard? (@Get_n //I 1) /v //S1 //S2))
    (set! //O/K 0))
   (#t
    (set! /v (@Get_n (@Get_n (@Get_n //I 1) 1) 1))
    (set! //S1 (@Get_n (@Get_n (@Get_n (@Get_n (@Get_n //I 1) 2) 1) 1) 2))
    ; THEN clause of inner IF 
    (set! //S2 (@Get_n (@Get_n (@Get_n (@Get_n (@Get_n //I 1) 2) 1) 2) 2))
    ; ELSE clause of inner IF 
    (set! //L (cdr (@Cs //I)))
    (while (and (= //O/K 1) (not (null? //L))) 
     (begin
      (cond
       ((null? (cdr //L))
        ; This is the ELSE clause 
        (cond
         ((not (@Equal? (@Get_n (car //L) 2) //S2))
          (set! //O/K 0))))
       ((not (@SI_Special_Guard? (car //L) /v //S1 //S2))
        (set! //O/K 0)))
      (set! //L (cdr //L))))))
  (= //O/K 1)))

(define (@SI_Special_Guard? //G /v //S1 //S2)
 (let ((//O/K 1))
  (cond
   ((not (= (@ST (@Get_n //G 1)) //T_/Equal))
    (set! //O/K 0))
   ((and (not (= (@ST (@Get_n (@Get_n //G 1) 2)) //T_/String)) (not (= (@ST (@Get_n (@Get_n //G 1) 2)) //T_/Number)))
    (set! //O/K 0))
   ((and (not (null? /v)) (not (@Equal? /v (@Get_n (@Get_n //G 1) 1))))
    (set! //O/K 0))
   ((not (= (@ST (@Get_n (@Get_n //G 2) 1)) //T_/Cond))
    (set! //O/K 0))
   ((not (= (@Size (@Get_n //G 2)) 1))
    (set! //O/K 0))
   ((and (not (null? //S1)) (not (@Equal? //S1 (@Get_n (@Get_n (@Get_n (@Get_n //G 2) 1) 1) 2))))
    (set! //O/K 0))
   ((and (not (null? //S2)) (not (@Equal? //S2 (@Get_n (@Get_n (@Get_n (@Get_n //G 2) 1) 2) 2))))
    (set! //O/K 0)))
  (= //O/K 1)))

(define (@SI_Special)
 (let ((//B/L (list (@Make //T_/And '() (list (@Get_n (@Get_n (@I) 1) 1) (@Get_n (@Get_n (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1) 1) 1)))))
       (//B '())
       (//S1 (@Cs (@Get_n (@Get_n (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1) 1) 2)))
       (//S2 (@Cs (@Get_n (@Get_n (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1) 2) 2)))
       (//L (@Cs (@I))))
  (while (not (null? (cdr //L))) 
   (begin
    (set! //B/L (cons (@Make //T_/And '() (list (@Get_n (car //L) 1) (@Get_n (@Get_n (@Get_n (@Get_n (car //L) 2) 1) 1) 1))) //B/L))
    (set! //L (cdr //L))))
  (set! //B (@Make //T_/Or '() (reverse //B/L)))
  (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() //S1))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() //S2))))))))

#t
