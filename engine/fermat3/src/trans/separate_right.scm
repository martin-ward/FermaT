;;; Scheme translation of WSL code
(define (/foreach-separate_right-1 //Depth //A/S_/Type)
 (cond
  ((equal? (@Gen_TVs (@I) //A/S/Type) (list (+ //Depth //T)))
   (set! //N (+ //N 1))
   (cond
    ((@Left?)
     (@Left)
     (cond
      ((not (@Gen_Proper? (@I) //A/S/Type))
       (set! //O/K 0))
      (#t
       (set! //S (@I))))
     (@Right))
    (#t
     (set! //O/K 0)))
   (cond
    ((> //N 1)
     (cond
      ((not (@Equal? //S //S1))
       (set! //O/K 0))))
    ((null? //S)
     (set! //O/K 0))
    (#t
     (set! //S1 //S))))))

(define (/foreach-separate_right-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (set! //S (@I))
   (@Paste_Over (@Make //T_/Exit //Depth '())))))

(define (/foreach-separate_right-3 //Depth //A/S_/Type)
 (cond
  ((equal? (@Gen_TVs (@I) //A/S/Type) (list (+ //Depth //T)))
   (set! //P/P (cons (list (@Posn) //Depth) //P/P))
   ; ensure that the EXIT is at the end of the sequence: 
   (cond
    ((@Right?)
     (@Delete_Rest)))
   ; Put proper statements into the tail 
   (set! /tail '())
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((not (@Left?))
      (set! /fl_flag1 1))
     (#t
      (@Left)
      (cond
       ((not (@Gen_Proper? (@I) //A/S/Type))
        (set! /fl_flag1 1))
       (#t
        (set! /tail (cons (@I) /tail))
        (set! /fl_flag1 0))))))
   (while (@Right?) 
    (@Right))
   (cond
    ((null? //Take_/Out)
     (set! //Take_/Out /tail))
    (#t
     (set! //Take_/Out (@LC_Suffix //Take_/Out /tail)))))))

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
(define (@Separate_Right_Test)
 (let ((//S-save //S)
       (//F 0)
       (//Count 1)
       (//All_/The_/Same 1)
       (//Type (@ST (@I)))
       (//O/K-save //O/K)
       (//Message-save //Message))
  (set! //S '())
  (set! //O/K 0)
  (set! //Message "")
  (cond
   ((and (or (= //Type //T_/Cond) (= //Type //T_/D_/If)) (<= (@Size (@I)) //Max_/Cond_/Size))
    (@Down_Last)
    (@Down_Last)
    (@Down_Last)
    ; to last statement of last guarded 
    (set! //S (@I))
    (cond
     ((@Is_Improper?)
      (set! //F 1)))
    (@Up)
    (@Up)
    ; back to guarded 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((not (@Left?))
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0)))
      (cond
       ((= /fl_flag1 0)
        (@Left)
        (@Down_Last)
        (@Down_Last)
        ; to last statement of guarded 
        (cond
         ((not (@Equal? (@I) //S))
          (set! //All_/The_/Same 0)
          (cond
           ((not (@Is_Improper?))
            (cond
             ((not (= //F 1))
              (@Fail "Nothing to take out")
              (@Up)
              (@Up)
              (set! /fl_flag1 1))
             (#t
              (set! //F 0)
              (set! //S (@I))
              (set! /fl_flag1 0))))
           (#t
            (set! /fl_flag1 0))))
         (#t
          (set! /fl_flag1 0)))
        (cond
         ((= /fl_flag1 0)
          (@Up)
          (@Up)
          (set! /fl_flag1 0)))))))
    (@Up)
    (cond
     ((and (= //F 1) (= //All_/The_/Same 0))
      (@Fail "Not all the same")))
    (cond
     ((and (not (@Failed?)) (not (= (@ST //S) //T_/Skip)))
      (@Pass))
     (#t
      (@Fail "Nothing to take out")))
    (cond
     ((@Failed?)
      (cond
       ((and (= (@Size (@I)) 2) (@Equal? (@Last_Non_Comment (@Cs (@Get_n (@Get_n (@I) 1) 2))) (@Last_Non_Comment (@Cs (@Get_n (@Get_n (@I) 2) 2)))))
        (cond
         ((not (null? (@Last_Non_Comment (@Cs (@Get_n (@Get_n (@I) 1) 2)))))
          (@Pass))))))))
   ((= //Type //T_/Where)
    (@Down)
    (@Down_Last)
    ; to last statement of body 
    (let ((//Calls (@Make_Set (concat (my-map HEAD (@Proc_Calls (@I))) (my-map HEAD (@Funct_Calls (@I)))))))
     (@Up)
     (@Up)
     (@Down_Last)
     ; to definitions 
     (cond
      ((null? (intersection-n //Calls (@Make_Set (my-map @V1 (@Cs (@I))))))
       (@Pass))
      (#t
       (@Fail "The statement uses procs or functs defined in the WHERE")))))
   ((= //Type //T_/Var)
    (let ((//V (@Assigned (@Get_n (@I) 1))))
     (@Down_Last)
     (@Down_Last)
     ; to last statement of body 
     (cond
      ((not (null? (intersection-n //V (@Variables (@I)))))
       (@Fail "The statement uses the local variables"))
      ((not (null? (intersection-n (@Stat_Types (@I)) (@Make_Set (list //T_/M/W_/Proc_/Call //T_/Proc_/Call)))))
       (@Fail "Calls in the statement may use the local variables"))
      (#t
       (@Pass)))))
   ((or (= //Type //T_/While) (= //Type //T_/For))
    (@Down_Last)
    (@Down_Last)
    (cond
     ((@Trans? //T/R_/Take_/Out_/Right)
      (@Pass))
     (#t
      (@Fail "Cannot take anything out"))))
   ((= //Type //T_/Floop)
    (let ((/-result- (@SR_Floop_Test  (@AS_Type) //O/K //Message)))
     (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-))
     (set! //Message (car /-result-)) (set! /-result- (cdr /-result-)))
    (cond
     ((= //O/K 1)
      (@Pass))
     (#t
      (@Fail //Message))))
   ((and (= //Type //T_/Assignment) (> (@Size (@I)) 1))
    (let ((/-result- (@SR_Assignment_Test  //O/K //Message)))
     (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-))
     (set! //Message (car /-result-)) (set! /-result- (cdr /-result-)))
    (cond
     ((= //O/K 1)
      (@Pass))
     (#t
      (@Fail //Message))))
   (#t
    (@Fail "Can't separate from this kind of statement")))
  (set! //S //S-save)
  (set! //O/K //O/K-save)
  (set! //Message //Message-save)))

(define (@SR_Floop_Test //A/S/Type-par //O/K-par //Message-par)
 (let ((//Message-save //Message)
       (//O/K-save //O/K)
       (//A/S/Type-save //A/S/Type)
       (funct-result '()))
  (set! //Message //Message-par)
  (set! //O/K //O/K-par)
  (set! //A/S/Type //A/S/Type-par)
  (let ((//N-save //N)
        (//T-save //T)
        (//S-save //S)
        (//S1-save //S1))
   (set! //N 0)
   (set! //T 0)
   (set! //S '())
   (set! //S1 '())
   (set! //O/K 0)
   (set! //Message "Nothing to take out")
   (cond
    ((equal? (@Gen_TVs (@I) //A/S/Type) (list //Omega))
     (cond
      ((and (equal? //A/S/Type "Reg") (= (gen-length (@Calls (@I))) 1))
       (set! //O/K 1))
      (#t
       (set! //Message "Cannot take out of this loop"))))
    ((equal? (@Gen_TVs (@I) //A/S/Type) '())
     (set! //Message "Cannot take anything out of this loop"))
    (#t
     (set! //O/K 1)
     (while (not-member //T (@Gen_TVs (@I) //A/S/Type)) 
      (set! //T (+ //T 1)))
     (@Ateach_Terminal /foreach-separate_right-1 0 (@AS_Type) 1)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (cond
      ((and (= //O/K 1) (not (= //N 0)))
       (set! //O/K 1))
      (#t
       (set! //O/K 0)))))
   (set! //N //N-save)
   (set! //T //T-save)
   (set! //S //S-save)
   (set! //S1 //S1-save))
  (set! funct-result (list //O/K //Message))
  (set! //Message //Message-save)
  (set! //O/K //O/K-save)
  (set! //A/S/Type //A/S/Type-save)
  funct-result))

(define (@SR_Assignment_Test //O/K-par //Message-par)
 (let ((//Message-save //Message)
       (//O/K-save //O/K)
       (funct-result '()))
  (set! //Message //Message-par)
  (set! //O/K //O/K-par)
  (let ((/vars '())
        (/last (last-1 (@Cs (@I))))
        (/first (butlast-1 (@Cs (@I))))
        (/assign '()))
   (for-in /assign /first 
    (set! /vars (union-n (@Elts_Assigned /assign) /vars)))
   (cond
    ((@Elt_Clash_List? /vars (@Elts_Used /last))
     (set! //O/K 0)
     (set! //Message "A variable used in the last assign is assigned to in the others"))
    (#t
     (set! //O/K 1))))
  (set! funct-result (list //O/K //Message))
  (set! //Message //Message-save)
  (set! //O/K //O/K-save)
  funct-result))

(define (@Separate_Right_Code //Data)
 (let ((//Orig_/Pos (@Posn))
       (//Type (@ST (@I))))
  (cond
   ((or (= //Type //T_/Cond) (= //Type //T_/D_/If))
    (let ((//S-save //S)
          (//F 1)
          (//P '())
          (//O/K-save //O/K))
     (set! //S '())
     (set! //O/K 1)
     ; F = 1 if all branches end with the same statement.
     ; F = 0 implies all branches end with the same statement or with
     ;       an improper statement
     (@Down)
     (set! //S (last-1 (@Cs (@Get_n (@I) 2))))
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (cond
         ((not (@Equal? //S (last-1 (@Cs (@Get_n (@I) 2)))))
          (set! //F 0)
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0))))))
     (@To 1)
     (set! //S '())
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (@Down_Last)
       (@Down_Last)
       (cond
        ((or (= //F 1) (not (@Is_Improper?)))
         (cond
          ((null? //S)
           (set! //S (@I)))
          ((not (@Equal? //S (@I)))
           (set! //O/K 0)))))
       (@Up)
       (@Up)
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))
     (@Up)
     ; back to cond 
     (cond
      ((= //O/K 1)
       (@Paste_After //S)
       (for //J 1 (@Size (@I)) 1 
        (begin
         (@Down_To //J)
         (@Down_Last)
         (@Down_Last)
         (cond
          ((@Equal? //S (@I))
           (@Delete)))
         (@Up)
         (@Up)
         (@Up))))
      ((and (= (@Size (@I)) 2) (@Equal? (@Last_Non_Comment (@Cs (@Get_n (@Get_n (@I) 1) 2))) (@Last_Non_Comment (@Cs (@Get_n (@Get_n (@I) 2) 2)))))
       (let ((/save '()))
        (@Down)
        (@Down_Last)
        (@Down_Last)
        (while (= (@ST (@I)) //T_/Comment) 
         (@Left))
        (set! //S (@I))
        (@Delete)
        (@Up)
        (@Up)
        (@Right)
        ; to second guarded 
        (@Down_Last)
        (@Down_Last)
        (while (= (@ST (@I)) //T_/Comment) 
         (@Left))
        (@Delete)
        (@Up)
        (@Up)
        (@Up)
        ; back to cond 
        (@Splice_After (cons //S /save))))
      (#t
       (error "BUG in @Separate_Right_Code!")))
     (cond
      ((= //Type //T_/Cond)
       (@Fix_Cond))
      (#t
       (@Fix_Dijkstra)))
     (cond
      ((@Right?)
       (@Right)
       (cond
        ((= (@ST (@I)) //T_/Skip)
         (@Delete)))))
     (set! //S //S-save)
     (set! //O/K //O/K-save))
    (@Goto //Orig_/Pos))
   ((= //Type //T_/Where)
    (@Down)
    (@Down_Last)
    (set! //S (@I))
    (@Up)
    (@Up)
    (@Paste_After //S)
    (@Goto (@Posn))
    (@Down)
    (@Down_Last)
    (@Delete)
    (@Fixup)
    (@Goto //Orig_/Pos))
   ((or (= //Type //T_/While) (= //Type //T_/For) (= //Type //T_/Var))
    (@Down_Last)
    (@Down_Last)
    (set! //S (@I))
    (@Up)
    (@Up)
    (@Paste_After //S)
    (@Goto (@Posn))
    (@Down_Last)
    (@Down_Last)
    (@Delete)
    (@Fixup)
    (@Goto //Orig_/Pos))
   ((= //Type //T_/Floop)
    (@SR_Floop (@AS_Type)))
   ((= //Type //T_/Assignment)
    (@Down_Last)
    (@Cut)
    (@Up)
    (@Paste_After (@Make //T_/Assignment '() (list (@Buffer))))))
  (cond
   ((@Trans? //T/R_/Separate_/Right)
    (@Trans //T/R_/Separate_/Right '())))))

(define (@SR_Floop //A/S/Type-par)
 (let ((//A/S/Type-save //A/S/Type))
  (set! //A/S/Type //A/S/Type-par)
  (let ((//S-save //S)
        (//T-save //T)
        (//P (@Posn))
        (//P/P-save //P/P)
        (//D 0)
        (//Take_/Out-save //Take_/Out)
        (//S1-save //S1)
        (/tail-save /tail)
        (/taken 0))
   (set! //S '())
   (set! //T 0)
   (set! //P/P '())
   (set! //Take_/Out '())
   (set! //S1 '())
   (set! /tail '())
   (cond
    ((equal? (@Gen_TVs (@I) //A/S/Type) (list //Omega))
     (@Foreach_Statement /foreach-separate_right-2 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (@Paste_After //S)
     (@Goto //P))
    (#t
     ; If all the exits have TV at least T, then take out 
     ; the statements with TV = T 
     (while (not-member //T (@Gen_TVs (@I) //A/S/Type)) 
      (set! //T (+ //T 1)))
     (@Edit)
     ;We want to get the _relative_ positions of the STS's
     (@Ateach_Terminal /foreach-separate_right-3 0 (@AS_Type) 1)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! //P/P (reverse //P/P))
     (@End_Edit)
     (set! /taken (gen-length //Take_/Out))
     (cond
      ((> //T 0)
       (set! //Take_/Out (concat //Take_/Out (list (@Make //T_/Exit //T '()))))))
     (@Splice_After //Take_/Out)
     (@Goto //P)
     (@Edit)
     (while (not (null? //P/P)) 
      (begin
       (@Goto (wsl-ref (wsl-ref //P/P 1) 1))
       (@Up)
       (set! //S1 (@Cs (@I)))
       ; Chop off the EXIT and Take_Out statements in @I 
       ; and replace them with a new EXIT statement 
       (@Paste_Over (@Make //T_/Statements '() (concat (@Sub_Seg //S1 1 (- (@Size (@I)) (+ /taken 1))) (list (@Make //T_/Exit (wsl-ref (wsl-ref //P/P 1) 2) '())))))
       (set! //P/P (cdr //P/P))))
     (@End_Edit)))
   (set! //S //S-save)
   (set! //T //T-save)
   (set! //P/P //P/P-save)
   (set! //Take_/Out //Take_/Out-save)
   (set! //S1 //S1-save)
   (set! /tail /tail-save))
  (set! //A/S/Type //A/S/Type-save)))

; A function which returns the largest common suffix of 
; the two given lists of items: 
(define (@LC_Suffix //L1 //L2)
 (let ((//R '()))
  (set! //L1 (reverse //L1))
  (set! //L2 (reverse //L2))
  (while (and (not (null? //L1)) (not (null? //L2)) (@Equal? (car //L1) (car //L2))) 
   (begin
    (set! //R (cons (car //L1) //R))
    (set! //L1 (cdr //L1))
    (set! //L2 (cdr //L2))))
  //R))

; Return the last non-comment statement in the given list 
(define (@Last_Non_Comment //L)
 (let ((//R '())
       (//L1 (reverse //L)))
  (while (and (not (null? //L1)) (= (@ST (car //L1)) //T_/Comment)) 
   (set! //L1 (cdr //L1)))
  (cond
   ((not (null? //L1))
    (set! //R (car //L1))))
  //R))

#t
