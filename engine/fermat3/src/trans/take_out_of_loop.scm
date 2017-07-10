;;; Scheme translation of WSL code
(define (/foreach-take_out_of_loop-1 //Depth //A/S_/Type)
 (cond
  ((not (null? (gethash /posns (@Posn))))
   (@Delete_Rest)
   (@Paste_Over (@Make //T_/Exit //Depth '())))))

(define (/foreach-take_out_of_loop-2 //Depth //A/S_/Type)
 ; If this exit is outside our range, then ignore it. 
 (cond
  ((and (= (@ST (@I)) //T_/Exit) (> (- (@V (@I)) //Depth) (- //Max_/T/V /loops)))
   #t)
  (#t
   ; Compute the required increment for this depth: 
   (cond
    ((equal? //Min_/T/V //Omega)
     ; Incrementing S has no effect: 
     (set! //S1 (car //Incs)))
    (#t
     (set! /inc (+ //Depth (- //Min_/T/V /loops)))
     (cond
      ((<= (gen-length //Incs) (+ /inc 1))
       (set! //Incs (@TOL_Add_Incs //Incs //S //A/S_/Type /inc))))
     (set! //S1 (wsl-ref //Incs (+ /inc 1)))))
   ; Check if the current statement matches, move up 
   ; until a match is reached or we reach the top: 
   (set! //L (reverse (@Cs //S1)))
   (cond
    ((@Equal? (car //L) (@I))
     ; Possible match at this level: 
     (let ((/-result- (@TOL_Check_List  //L //A/S_/Type //Depth //O/K /posns)))
      (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /posns (car /-result-)) (set! /-result- (cdr /-result-)))
     (@To_Last))
    (#t
     (set! //O/K 0)))
   (cond
    ((= //O/K 0)
     ; We need to move up and check again 
     (set! //O/K 1)
     (set! /posn (@Posn))
     ; Adjust Depth as we move up and restore it at the end 
     (let ((//Depth-save //Depth))
      (set! //Depth //Depth)
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (@Up)
        (@Up_To_Statement)
        (cond
         ((not (@Up?))
          (set! //O/K 0)
          (set! /fl_flag1 1))
         (#t
          (cond
           ((= (@ST (@I)) //T_/Floop)
            ; We had to move out of a loop, so adjust Depth and L 
            (set! //Depth (- //Depth 1))
            (cond
             ((not (equal? //Min_/T/V //Omega))
              (set! /inc (- /inc 1))
              (set! //L (reverse (@Cs (wsl-ref //Incs (+ /inc 1)))))))))
          (cond
           ((@Equal? (car //L) (@I))
            ; Possible match at this level: 
            (let ((/-result- (@TOL_Check_List  //L //A/S_/Type //Depth //O/K /posns)))
             (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-))
             (set! /posns (car /-result-)) (set! /-result- (cdr /-result-)))
            (cond
             ((= //O/K 1)
              (set! /fl_flag1 1))
             (#t
              (set! /fl_flag1 0))))
           (#t
            (set! /fl_flag1 0)))))))
      (set! //Depth //Depth-save))
     (@Goto /posn)))
   ; If we started on a CALL in a regular action system, 
   ; then don't worry if we couldn't fix it: 
   (cond
    ((and (= //O/K 0) (equal? (@Gen_TVs (@I) //A/S_/Type) (list //Omega)))
     (set! //O/K 1)))
   (cond
    ((= //O/K 0)
     (@Fail "Not all terminal statements match the selected statement"))))))

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
(define (@Take_Out_Of_Loop_Test)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((not (@Up?))
   (@Fail "It is not possible to take the whole program out of anything."))
  ((not (= (@GT (@I)) //T_/Statement))
   (@Fail "The selected item is not a statement."))
  (#t
   (@Take_Out_Of_Loop_Test2))))

(define (@Take_Out_Of_Loop_Test2)
 (let ((//S-save //S)
       (//A/S_/Type-save //A/S_/Type)
       (/loops-save /loops)
       (/posns-save /posns))
  (set! //S '())
  (set! //A/S_/Type (@AS_Type))
  (set! /loops 0)
  (set! /posns (hash-table))
  ; If the selected item is a statement which occurs in a statement sequence 
  ; then we should take all the statements up to the end of that sequence.   
  (set! //S (@Cs (@Parent)))
  (set! //S (@Make //T_/Statements '() (@Final_Seg //S (@Posn_n))))
  (cond
   ((member 0 (@Gen_TVs //S //A/S_/Type))
    (@Fail "The statement will not always lead to termination of the loop"))
   (#t
    (let ((/-result- (@TOL_Count_Loops  //S //A/S_/Type /loops /posns)))
     (set! /loops (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /posns (car /-result-)) (set! /-result- (cdr /-result-)))
    (cond
     ((= /loops 0)
      (@Fail "The statement is not in a suitable loop."))
     (#t
      (@Pass)))))
  (set! //S //S-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! /loops /loops-save)
  (set! /posns /posns-save)))

; ------------------------------------------------------------------------- 
(define (@Take_Out_Of_Loop_Code //Data)
 (cond
  ((and (= (@GT (@I)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (let ((//S-save //S)
       (//A/S_/Type-save //A/S_/Type)
       (/loops-save /loops)
       (/posns-save /posns)
       (//O/K-save //O/K)
       (/inc-save /inc))
  (set! //S '())
  (set! //A/S_/Type (@AS_Type))
  (set! /loops 0)
  (set! /posns (hash-table))
  (set! //O/K 0)
  (set! /inc 0)
  ; If the selected item is a statement which occurs in a statement sequence 
  ; then we should take all the statements up to the end of that sequence.   
  (set! //S (@Cs (@Parent)))
  (set! //S (@Make //T_/Statements '() (@Final_Seg //S (@Posn_n))))
  ; Move up to the loop which works: 
  (let ((/-result- (@TOL_Count_Loops  //S //A/S_/Type /loops /posns)))
   (set! /loops (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /posns (car /-result-)) (set! /-result- (cdr /-result-)))
  ; Carry out the edits from the posns table: 
  (@Edit)
  (@Trans //T/R_/Delete_/All_/Skips "")
  (@Ateach_Statement /foreach-take_out_of_loop-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@End_Edit)
  ; Append an appropriately decremented statement sequence to the loop: 
  (set! //S (@Cs (@Increment //S //A/S_/Type (- /loops) 0)))
  (cond
   ((= (@ST (last-1 //S)) //T_/Skip)
    (set! //S (butlast-1 //S))))
  (@Splice_After //S)
  (set! //S //S-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! /loops /loops-save)
  (set! /posns /posns-save)
  (set! //O/K //O/K-save)
  (set! /inc /inc-save)))

; The statement S == IF x = 0 THEN EXIT(2) ELSE EXIT(3) FI 
; has a Min_TV of 2 and therefore could be taken out of either one or two loops. 
; This procedure checks the enclosing loops to see how many it can be taken out of. 
; To check a loop we check each simple terminal statement: calculate the increment 
; of S that we are looking for and move up until we either get a match or 
; reach the loop body (in which case we have failed for this loop). 
; If everything matches OK, then return how many loops we moved up through 
; and leave this loop selected. 
(define (@TOL_Count_Loops //S-par //A/S_/Type-par /loops-par /posns-par)
 (let ((/posns-save /posns)
       (/loops-save /loops)
       (//A/S_/Type-save //A/S_/Type)
       (//S-save //S)
       (funct-result '()))
  (set! /posns /posns-par)
  (set! /loops /loops-par)
  (set! //A/S_/Type //A/S_/Type-par)
  (set! //S //S-par)
  (let ((//Min_/T/V-save //Min_/T/V)
        (//Max_/T/V-save //Max_/T/V)
        (//O/K-save //O/K))
   (set! //Min_/T/V (@TOL_Min_TV (@Gen_TVs //S //A/S_/Type)))
   (set! //Max_/T/V (@TOL_Max_TV (@Gen_TVs //S //A/S_/Type)))
   (set! //O/K 0)
   ; First we normalise S by decrementing by Min_TV (if necessary): 
   (set! //S (@TOL_Normalise //S //A/S_/Type //Min_/T/V))
   (set! /fl_flag2 0)
   (while (= /fl_flag2 0) 
    (begin
     ; Move up to the next enclosing loop: 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (cond
       ((or (not (@Up?)) (@Simple? (@Parent)))
        ; We have failed to find a loop: 
        (set! /loops 0)
        (set! /fl_flag1 2))
       (#t
        (@Up)
        (cond
         ((= (@ST (@I)) //T_/Floop)
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0))))))
     (cond
      ((= /fl_flag1 2)
       (set! /fl_flag2 1))
      (#t
       (set! /loops (+ /loops 1))
       (cond
        ((and (not (equal? //Min_/T/V //Omega)) (> /loops //Min_/T/V))
         (set! /loops 0)
         (set! /fl_flag2 1))
        (#t
         ; Check this loop for validity: 
         (set! /posns (hash-table))
         (let ((/-result- (@TOL_Check_Loop  //S //A/S_/Type //Min_/T/V //Max_/T/V /loops 0 //O/K /posns)))
          (set! //O/K (car /-result-)) (set! /-result- (cdr /-result-))
          (set! /posns (car /-result-)) (set! /-result- (cdr /-result-)))
         (cond
          ((= //O/K 1)
           (set! /fl_flag2 1))
          (#t
           (set! /fl_flag2 0)))))))))
   (set! //Min_/T/V //Min_/T/V-save)
   (set! //Max_/T/V //Max_/T/V-save)
   (set! //O/K //O/K-save))
  (set! funct-result (list /loops /posns))
  (set! /posns /posns-save)
  (set! /loops /loops-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! //S //S-save)
  funct-result))

; Check all the terminal statements in a loop to see if they 
; occur in a suitable increment of the statement sequence S. 
(define (@TOL_Check_Loop //S-par //A/S_/Type-par //Min_/T/V-par //Max_/T/V-par /loops-par //Update //O/K-par /posns-par)
 (let ((/posns-save /posns)
       (//O/K-save //O/K)
       (/loops-save /loops)
       (//Max_/T/V-save //Max_/T/V)
       (//Min_/T/V-save //Min_/T/V)
       (//A/S_/Type-save //A/S_/Type)
       (//S-save //S)
       (funct-result '()))
  (set! /posns /posns-par)
  (set! //O/K //O/K-par)
  (set! /loops /loops-par)
  (set! //Max_/T/V //Max_/T/V-par)
  (set! //Min_/T/V //Min_/T/V-par)
  (set! //A/S_/Type //A/S_/Type-par)
  (set! //S //S-par)
  (let ((/inc-save /inc)
        (//Incs-save //Incs)
        (//S1-save //S1)
        (/posn-save /posn)
        (//L-save //L))
   (set! /inc 0)
   (set! //Incs (list //S))
   (set! //S1 '())
   (set! /posn '())
   (set! //L '())
   (@Reset_Pass_Status)
   (set! //O/K 1)
   (@Edit)
   (@Trans //T/R_/Delete_/All_/Skips "")
   (@Ateach_Terminal /foreach-take_out_of_loop-2 0 (@AS_Type) 1)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; Have finished processing this loop 
   (cond
    ((= //Update 1)
     (@End_Edit))
    (#t
     (@Undo_Edit)))
   (set! /inc /inc-save)
   (set! //Incs //Incs-save)
   (set! //S1 //S1-save)
   (set! /posn /posn-save)
   (set! //L //L-save))
  (set! funct-result (list //O/K /posns))
  (set! /posns /posns-save)
  (set! //O/K //O/K-save)
  (set! /loops /loops-save)
  (set! //Max_/T/V //Max_/T/V-save)
  (set! //Min_/T/V //Min_/T/V-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! //S //S-save)
  funct-result))

; Check the list of statements to the left of the current statement 
; against the list L, if there is a match then add the position to posns. 
(define (@TOL_Check_List //L-par //A/S_/Type-par //Depth-par //O/K-par /posns-par)
 (let ((/posns-save /posns)
       (//O/K-save //O/K)
       (//Depth-save //Depth)
       (//A/S_/Type-save //A/S_/Type)
       (//L-save //L)
       (funct-result '()))
  (set! /posns /posns-par)
  (set! //O/K //O/K-par)
  (set! //Depth //Depth-par)
  (set! //A/S_/Type //A/S_/Type-par)
  (set! //L //L-par)
  (cond
   ((< (@Posn_n) (gen-length //L))
    (set! //O/K 0))
   (#t
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (set! //L (cdr //L))
      (cond
       ((null? //L)
        (set! //O/K 1)
        (set! /fl_flag1 1))
       (#t
        (@Left)
        (cond
         ((not (@Equal? (car //L) (@I)))
          (set! //O/K 0)
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0)))))))
    (cond
     ((= //O/K 1)
      (puthash /posns (@Posn) 1)))))
  (set! funct-result (list //O/K /posns))
  (set! /posns /posns-save)
  (set! //O/K //O/K-save)
  (set! //Depth //Depth-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! //L //L-save)
  funct-result))

; This will return Omega if all the list is just <Omega> 
(define (@TOL_Min_TV /list)
 (let ((//Min //Omega))
  (for-in //T/V /list 
   (cond
    ((or (equal? //Min //Omega) (and (not (equal? //T/V //Omega)) (< //T/V //Min)))
     (set! //Min //T/V))))
  //Min))

(define (@TOL_Max_TV /list)
 (let ((//Max //Omega))
  (for-in //T/V /list 
   (cond
    ((or (equal? //Max //Omega) (and (not (equal? //T/V //Omega)) (< //T/V //Max)))
     (set! //Max //T/V))))
  //Max))

; Add one or more increments to the list and return a new list: 
(define (@TOL_Add_Incs //Incs-par //S-par //A/S_/Type-par /inc-par)
 (let ((/inc-save /inc)
       (//A/S_/Type-save //A/S_/Type)
       (//S-save //S)
       (//Incs-save //Incs)
       (/new (reverse //Incs-par))
       (/last_inc (- (gen-length //Incs-par) 1))
       (funct-result '()))
  (set! /inc /inc-par)
  (set! //A/S_/Type //A/S_/Type-par)
  (set! //S //S-par)
  (set! //Incs //Incs-par)
  (while (< /last_inc /inc) 
   (begin
    (set! /last_inc (+ /last_inc 1))
    (set! /new (cons (@Increment //S //A/S_/Type /last_inc 0) /new))))
  (set! funct-result (reverse /new))
  (set! /inc /inc-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! //S //S-save)
  (set! //Incs //Incs-save)
  funct-result))

; Normalise a statement by decrementing it by the amount given 
; and chopping any trailing SKIP: 
(define (@TOL_Normalise //S-par //A/S_/Type-par /inc-par)
 (let ((/inc-save /inc)
       (//A/S_/Type-save //A/S_/Type)
       (//S-save //S)
       (funct-result '()))
  (set! /inc /inc-par)
  (set! //A/S_/Type //A/S_/Type-par)
  (set! //S //S-par)
  (cond
   ((and (not (equal? /inc //Omega)) (> /inc 0))
    (set! //S (@Increment //S //A/S_/Type (- /inc) 0))))
  (cond
   ((and (> (@Size //S) 1) (= (@ST (@Get_n //S (@Size //S))) //T_/Skip))
    (set! //S (@Make //T_/Statements '() (butlast-1 (@Cs //S))))))
  (set! funct-result //S)
  (set! /inc /inc-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! //S //S-save)
  funct-result))

#t
