;;; Scheme translation of WSL code
(define (/foreach-join_all_cases-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Comment)
   (set! /strings (union-n (list (@V (@I))) /strings)))))

(define (/foreach-join_all_cases-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Comment)
   (set! /strings (union-n (list (@V (@I))) /strings)))))

(define /%const__join_all_cases__1 (@Make 309 '() '()))
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
(define (@Join_All_Cases_Test)
 ; Check for two guards with the same statement sequence within Max_Distance 
 ; of each other (the Max_Distance limit prevents the test from being O(n^2)) 
 (cond
  ((not (= (@ST (@I)) //T_/Cond))
   (@Fail "Not an IF statement"))
  ((= (@Size (@I)) 2)
   (cond
    ((@Equal_X_Comments? (@Get_n (@Get_n (@I) 1) 2) (@Get_n (@Get_n (@I) 2) 2))
     (@Pass))
    (#t
     (@Fail "The binary IF has different statements in the two arms"))))
  (#t
   (let ((//Max_/Distance 10)
         (/guard '())
         (/seen '())
         (//S '())
         (/tmp '()))
    (for-in /guard (@Cs (@I)) 
     (begin
      (set! //S (@Get_n /guard 2))
      (set! /tmp /seen)
      (while (and (not (null? /tmp)) (not (@Passed?))) 
       (cond
        ((@Equal? //S (car /tmp))
         (@Pass))
        (#t
         (set! /tmp (cdr /tmp)))))
      (cond
       ((not (@Passed?))
        ; Add S to seen and trim to Max_Distance elements 
        (set! /seen (cons //S /seen))
        (cond
         ((> (gen-length /seen) //Max_/Distance)
          (set! /seen (butlast-1 /seen))))))))
    (cond
     ((not (@Passed?))
      (@Fail "The IF statement does not have a repeated guard")))))))

; Compare equality excluding comments: 
; (This one isn't used since it allows too much restructuring 
; and therefore prevents some CASE statements from being recovered 
; from the WSL). 
(define (@Equal_X_Comments? //I1 //I2)
 (let ((//O/K 1))
  (cond
   ((not (= (@ST //I1) (@ST //I2)))
    (set! //O/K 0))
   ((@Has_Value_Type? (@ST //I1))
    (cond
     ((and (not (= (@ST //I1) //T_/Comment)) (not (equal? (@V //I1) (@V //I2))))
      (set! //O/K 0))))
   (#t
    (let ((//C1 (@Cs //I1))
          (//C2 (@Cs //I2)))
     (while (and (= //O/K 1) (not (null? //C1)) (not (null? //C2))) 
      (cond
       ((= (@ST (car //C1)) //T_/Comment)
        (set! //C1 (cdr //C1)))
       ((= (@ST (car //C2)) //T_/Comment)
        (set! //C2 (cdr //C2)))
       ((not (@Equal_X_Comments? (car //C1) (car //C2)))
        (set! //O/K 0))
       (#t
        (set! //C1 (cdr //C1))
        (set! //C2 (cdr //C2)))))
     (while (and (not (null? //C1)) (= (@ST (car //C1)) //T_/Comment)) 
      (set! //C1 (cdr //C1)))
     (while (and (not (null? //C2)) (= (@ST (car //C2)) //T_/Comment)) 
      (set! //C2 (cdr //C2)))
     (cond
      ((or (not (null? //C1)) (not (null? //C2)))
       (set! //O/K 0))))))
  (= //O/K 1)))

; Merge all copies of the first repeated statement encountered: 
(define (@Join_All_Cases_Code //Data)
 (let ((//Max_/Distance 10)
       (/guards '())
       (/seen '())
       (//S '())
       (/tmp '())
       (/found 0)
       (/posn 0)
       (/newcond '())
       (/fix_last 0)
       (/comments '())
       (/strings-save /strings))
  (set! /strings '())
  ; First find the repeated statement, the first occurrence is at posn 
  (@Edit)
  (set! /guards (@Cs (@I)))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((null? /guards)
     (set! /fl_flag1 1))
    (#t
     (set! //S (@Get_n (car /guards) 2))
     (set! /guards (cdr /guards))
     (set! /posn (+ /posn 1))
     (set! /tmp /seen)
     (while (and (not (null? /tmp)) (= /found 0)) 
      (cond
       ((@Equal_X_Comments? //S (car /tmp))
        (cond
         ((> (@Total_Size (car /tmp)) (@Total_Size //S))
          (set! //S (car /tmp))))
        (set! /posn (- /posn (+ (- (gen-length /seen) (gen-length /tmp)) 1)))
        (set! /found 1))
       (#t
        (set! /tmp (cdr /tmp)))))
     (cond
      ((= /found 1)
       (set! /fl_flag1 1))
      (#t
       ; Add S to seen and trim to Max_Distance elements 
       (set! /seen (cons //S /seen))
       (cond
        ((> (gen-length /seen) //Max_/Distance)
         (set! /seen (butlast-1 /seen))
         (set! /fl_flag1 0))
        (#t
         (set! /fl_flag1 0))))))))
  ; Find the last guard containing S 
  ; Note: if the very last guard is deleted, then change the previous guard's 
  ; condition to TRUE 
  (cond
   ((= /found 0)
    (display-list "Error in Join_All_Cases!!!")))
  (@Down_Last)
  ; Find the final occurence of the repeated statements 
  (while (and (@Left?) (not (@Equal_X_Comments? (@Get_n (@I) 2) //S))) 
   (@Left))
  (set! /newcond /%const__join_all_cases__1)
  (while (> (@Posn_n) /posn) 
   (begin
    ; Delete this guard after updating newcond 
    (set! /newcond (@Or (@Get_n (@I) 1) /newcond))
    ; If this is the last guard, and its cond is TRUE, then note that 
    ; the new last guard will need to have its condition changed to TRUE 
    (cond
     ((and (not (@Right?)) (= (@ST (@Get_n (@I) 1)) //T_/True))
      (set! /fix_last 1)))
    (for-in //I (@Cs (@Get_n (@I) 2)) 
     (cond
      ((= (@ST //I) //T_/Comment)
       (set! /comments (cons //I /comments)))))
    (@Delete)
    (@Left)
    ; Move to the next occurrence of the repeated guard, updating newcond: 
    (while (and (@Left?) (not (@Equal_X_Comments? (@Get_n (@I) 2) //S))) 
     (begin
      (set! /newcond (@And (@Not (@Get_n (@I) 1)) /newcond))
      (@Left)))))
  ; Keep the first guard (at posn), but update the condition: 
  (@Down)
  (@Paste_Over (@Or (@I) /newcond))
  (@Up)
  (cond
   ((= /fix_last 1)
    (@To_Last)
    (@Down)
    (@Paste_Over (@Make //T_/True '() '()))
    (@Up)))
  (@Up)
  ; back to the IF statement 
  ; See which comments remain, append any from comments list which would disappear 
  (@Down)
  (@Foreach_Statement /foreach-join_all_cases-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (while (@Right?) 
   (begin
    (@Right)
    (@Foreach_Statement /foreach-join_all_cases-2 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (@Up)
  (@End_Edit)
  (@Fix_Cond)
  (cond
   ((= (@ST (@I)) //T_/Statements)
    (@Down)))
  (for-in //I /comments 
   (cond
    ((not-member (@V //I) /strings)
     (@Paste_After //I))))
  (set! /strings /strings-save)))

#t
