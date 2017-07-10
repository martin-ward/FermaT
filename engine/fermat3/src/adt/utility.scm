;;; Scheme translation of WSL code
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
; ----------------------------------------------------------------------- 
; First some definitions of Lisp-like functions                           
; ----------------------------------------------------------------------- 
; Check if the first list of names is a prefix of the second 
(define (@Prefix? //A //B)
 
 (or (null? //A) (and (not (null? //B)) (eq? (car //A) (car //B)) (@Prefix? (cdr //A) (cdr //B)))))

; Check if any prefix of the element is in the given list 
(define (@Any_Prefix_In? /elt //L)
 
 (and (not (null? //L)) (or (@Prefix? (car //L) /elt) (@Any_Prefix_In? /elt (cdr //L)))))

; ----------------------------------------------------------------------- 
; Now some functions which allow regression testing.                      
; ----------------------------------------------------------------------- 
(define (@Test_Trans //Text //Before //Position //T/R_/Num //Data //After)
 (@New_Program //Before)
 (@Clear_State)
 (@Goto //Position)
 (cond
  ((@Trans? //T/R_/Num)
   (@Trans //T/R_/Num //Data)
   (cond
    ((equal? //After "Fail")
     (display-list "----------------------------------------------")
     (display-list //Text " was NOT supposed to be valid !!!")
     (display-list " ")
     (display-list "Initial program...")
     (@PP_Item //Before 80 "")
     (display-list " ")
     (display-list "Final program...")
     (@PP_Item (@Program) 80 "")
     (display-list " ")
     (display-list "Point of transformation application..." //Position)
     (display-list "----------------------------------------------")
     (display-list " "))
    ((@Equal? (@Program) //After)
     (display-list //Text " was OK."))
    (#t
     (display-list "----------------------------------------------")
     (display-list //Text " FAILED !!!")
     (display-list " ")
     (display-list "Initial program...")
     (@PP_Item //Before 80 "")
     (display-list " ")
     (display-list "Point of transformation application... " //Position)
     (display-list "Transformation number...               " //T/R_/Num)
     (display-list "User-supplied data...                  " //Data)
     (display-list " ")
     (display-list "Final program should have been...")
     (@PP_Item //After 80 "")
     (display-list " ")
     (display-list "Final program actually was...")
     (@PP_Item (@Program) 80 "")
     (display-list "----------------------------------------------")
     (display-list " "))))
  (#t
   (cond
    ((equal? //After "Fail")
     (display-list //Text " was OK."))
    (#t
     (display-list "----------------------------------------------")
     (display-list //Text " WAS supposed to be valid, but the test failed !!!")
     (display-list "Message: " (@Fail_Message))
     (display-list " ")
     (display-list "Initial program...")
     (@PP_Item (@Program) 80 "")
     (display-list " ")
     (display-list "Final program should have been...")
     (@PP_Item //After 80 "")
     (display-list " ")
     (display-list "Point of transformation application..." //Position)
     (display-list "----------------------------------------------")
     (display-list " "))))))

; ----------------------------------------------------------------------- 
; The following functions are used to generate random programs and to     
; apply random transformations to them at random positions.               
; ----------------------------------------------------------------------- 
(define (@RP_Trans)
 (let ((//D (@Random 6))
       (//Temp 0))
  (@New_Program (@RP_Item //T_/Statements 5 0))
  (@Clear_State)
  (for //I 1 5 1 
   (begin
    (@Goto '())
    (while (and (> //D 1) (@Down?)) 
     (begin
      (set! //Temp (@Random (@Size (@Item))))
      (@Down_To //Temp)
      (set! //D (- //D 1))))
    (set! //Temp (@What_Trans '()))
    (set! //Temp (wsl-ref //Temp (@Random (gen-length //Temp))))
    (display-list //I)
    (display-list "      Position :  " (@Posn))
    (display-list "Transformation :  " (vector-ref //T/Rs_/Name (- //Temp 1)))
    (display-list "   Size Before :  " (@Total_Size (@Program)))
    (@Trans //Temp "")
    (display-list "    Size After :  " (@Total_Size (@Program)))
    (cond
     ((not (@Syntax_OK? (@Program)))
      (display-list "")
      (display-list "Syntax Failed !!")
      (display-list "")
      (@Undo)
      (display-list (@Program))))))
  (display-list "")))

(define (@Test_Maths)
 (cond
  ((= (@Random 2) 1)
   (let ((//Exp (@RP_Expn 4)))
    (@Print_WSL //Exp "")
    (set! //Exp (@Simplify_Expn //Exp))
    (@Print_WSL //Exp "")))
  (#t
   (let ((//Exp (@RP_Cond 4)))
    (@Print_WSL //Exp "")
    (@PP_Item //Exp 80 "")
    (set! //Exp (@Simplify_Cond //Exp))
    (@Print_WSL //Exp "")))))

(define (@RP_Item //Type //Dep //In_/A_/S)
 (let ((//Result '())
       (//S 0)
       (//Seq '())
       (//Syn '())
       (//Val '()))
  (cond
   ((= //Type //T_/Statement)
    (set! //Result (@RP_Stat //Dep //In_/A_/S)))
   ((= //Type //T_/Lvalue)
    (set! //Result (@RP_Lval //Dep)))
   ((= //Type //T_/Expression)
    (set! //Result (@RP_Expn 3)))
   ((= //Type //T_/Condition)
    (set! //Result (@RP_Cond 3)))
   (#t
    (set! //Syn (@Syntax //Type))
    (cond
     ((@List_Type? //Type)
      (set! //S (+ (@Random 5) 1))
      (for //I 1 //S 1 
       (set! //Seq (cons (@RP_Item (car //Syn) (- //Dep 1) //In_/A_/S) //Seq))))
     (#t
      (while (not (null? //Syn)) 
       (begin
        (set! //Seq (concat //Seq (list (@RP_Item (car //Syn) (- //Dep 1) //In_/A_/S))))
        (set! //Syn (cdr //Syn))))))
    (set! //Result (@Make //Type '() //Seq))))
  //Result))

(define (@RP_Stat //Dep //In_/A_/S)
 (let ((//Result '())
       (//R (@Random (if (> //Dep 0) 16 8)))
       (//S 0)
       (//Seq '())
       (//Temp '()))
  (cond
   ((= //R 1)
    (set! //Result (@Make //T_/Skip '() '())))
   ((= //R 2)
    (set! //Result (@Make //T_/Exit (@Random 4) '())))
   ((= //R 3)
    (cond
     ((= //In_/A_/S 1)
      (set! //Result (@Make //T_/Call '() (list (@Make //T_/Name (@RP_Name //Dep) '())))))
     (#t
      (set! //Result (@Make //T_/Skip '() '())))))
   ((<= //R 8)
    (set! //Temp (list //T_/Abort //T_/Assert //T_/Assignment //T_/Pop //T_/Push))
    (set! //Result (@RP_Item (wsl-ref //Temp (- //R 3)) (- //Dep 1) //In_/A_/S)))
   ((<= //R 15)
    (set! //Temp (list //T_/Cond //T_/D_/If //T_/D_/Do //T_/Floop //T_/Var //T_/While //T_/Cond))
    (set! //Result (@RP_Item (wsl-ref //Temp (- //R 8)) (- //Dep 1) //In_/A_/S)))
   ((= //R 16)
    (set! //Temp (list "A1" "B2" "C3" "D4" "E5" "F6"))
    (set! //S (@Random 6))
    (for //I 1 //S 1 
     (begin
      (set! //Seq (concat //Seq (list (@Make //T_/Action '() (list (@Make //T_/Name (@Make_Name (car //Temp)) '()) (@RP_Item //T_/Statements (- //Dep 1) 1))))))
      (set! //Temp (cdr //Temp))))
    (set! //Result (@Make //T_/A_/S '() (list (@Make //T_/Name (@Make_Name "A1") '()) (@Make //T_/Actions '() //Seq))))))
  //Result))

(define (@RP_Name //Dep)
 (let ((//Result '())
       (//R (@Random 5))
       (//Temp (list "A1" "B2" "C3" "D4" "E5" "F6")))
  (set! //Result (@Make_Name (wsl-ref //Temp //R)))
  //Result))

(define (@RP_Lval //Dep)
 (let ((//Result '())
       (//R (@Random 5))
       (//Temp (list "A" "B" "C" "X" "Y")))
  (set! //Result (@Make //T_/Var_/Lvalue (@Make_Name (wsl-ref //Temp //R)) '()))
  //Result))

(define (@RP_Expn //Dep)
 (let ((//Result '())
       (//R (@Random (if (> //Dep 0) 27 17)))
       (//S 0)
       (//Seq '())
       (//Temp '()))
  (cond
   ((<= //R 7)
    (set! //Temp (list "A" "B" "C" "I" "J" "X" "Y"))
    (set! //Result (@Make //T_/Variable (@Make_Name (wsl-ref //Temp //R)) '())))
   ((<= //R 17)
    (set! //Temp (list 0 1 2 3 4 5 10 20 50 100))
    (set! //Result (@Make //T_/Number (wsl-ref //Temp (- //R 7)) '())))
   ((<= //R 23)
    (set! //S (+ (@Random 3) 2))
    (for //I 1 //S 1 
     (set! //Seq (cons (@RP_Expn (- //Dep 1)) //Seq)))
    (set! //Temp (list //T_/Plus //T_/Minus //T_/Times //T_/Divide //T_/Max //T_/Min))
    (set! //Result (@Make (wsl-ref //Temp (- //R 17)) '() //Seq)))
   ((<= //R 27)
    (set! //Temp (list //T_/Abs //T_/Sgn //T_/Int //T_/Frac))
    (set! //Result (@Make (wsl-ref //Temp (- //R 23)) '() (list (@RP_Expn (- //Dep 1)))))))
  //Result))

(define (@RP_Cond //Dep)
 (let ((//Result '())
       (//R (@Random (if (> //Dep 0) 11 6)))
       (//S 0)
       (//Seq '())
       (//Temp '()))
  (cond
   ((<= //R 6)
    (set! //Temp (list //T_/Equal //T_/Not_/Equal //T_/Less //T_/Greater //T_/Less_/Eq //T_/Greater_/Eq))
    (set! //Result (@Make (wsl-ref //Temp //R) '() (list (@RP_Expn (- //Dep 1)) (@RP_Expn (- //Dep 1))))))
   ((= //R 7)
    (set! //Result (@Make //T_/Not '() (list (@RP_Cond (- //Dep 1))))))
   ((<= //R 9)
    (set! //S (+ (@Random 3) 2))
    (for //I 1 //S 1 
     (set! //Seq (cons (@RP_Cond (- //Dep 1)) //Seq)))
    (set! //Temp (list //T_/And //T_/Or))
    (set! //Result (@Make (wsl-ref //Temp (- //R 7)) '() //Seq)))
   ((= //R 10)
    (set! //Result (@Make //T_/True '() '())))
   ((= //R 11)
    (set! //Result (@Make //T_/False '() '()))))
  //Result))

; Move to the first statement with the given specific type, eg T_A_S or T_Where. 
; This is used by the dotrans script option type=T_xxx 
(define (@Find_Type /type-par)
 (let ((/type-save /type))
  (set! /type /type-par)
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((equal? (@Spec_Type (@Item)) /type)
     (set! /fl_flag1 1))
    ((@Down?)
     (@Down)
     (set! /fl_flag1 0))
    ((@Right?)
     (@Right)
     (set! /fl_flag1 0))
    ((@Up?)
     (@Up)
     (while (and (@Up?) (not (@Right?))) 
      (@Up))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))
    (#t
     (set! /fl_flag1 1))))
  (set! /type /type-save)))

; Convert a string to a decimal number 
(define (@String_To_Num /s)
 (let ((/r 0)
       (/l 0)
       (/d 0))
  (cond
   ((string? /s)
    (set! /l (string-length /s))
    (cond
     ((> /l 0)
      (cond
       ((equal? (substr /s 0 1) "-")
        (set! /r (- (@String_To_Num (substr /s 1)))))
       (#t
        (set! /r (@String_To_Num (substr /s 0 (- /l 1))))
        (set! /r (* /r 10))
        (set! /d (substr /s (- /l 1) 1))
        (cond
         ((equal? /d "1")
          (set! /r (+ /r 1)))
         ((equal? /d "2")
          (set! /r (+ /r 2)))
         ((equal? /d "3")
          (set! /r (+ /r 3)))
         ((equal? /d "4")
          (set! /r (+ /r 4)))
         ((equal? /d "5")
          (set! /r (+ /r 5)))
         ((equal? /d "6")
          (set! /r (+ /r 6)))
         ((equal? /d "7")
          (set! /r (+ /r 7)))
         ((equal? /d "8")
          (set! /r (+ /r 8)))
         ((equal? /d "9")
          (set! /r (+ /r 9))))))))))
  /r))

(define (@Hex_To_Num /s)
 (let ((/r 0)
       (/l (string-length /s)))
  (cond
   ((> /l 0)
    (cond
     ((equal? (substr /s 0 1) "-")
      (set! /r (- (@Hex_To_Num (substr /s 1)))))
     (#t
      (set! /r (@Hex_To_Num (substr /s 0 (- /l 1))))
      (set! /r (* /r 16))
      (let ((/d (substr /s (- /l 1) 1)))
       (cond
        ((equal? /d "1")
         (set! /r (+ /r 1)))
        ((equal? /d "2")
         (set! /r (+ /r 2)))
        ((equal? /d "3")
         (set! /r (+ /r 3)))
        ((equal? /d "4")
         (set! /r (+ /r 4)))
        ((equal? /d "5")
         (set! /r (+ /r 5)))
        ((equal? /d "6")
         (set! /r (+ /r 6)))
        ((equal? /d "7")
         (set! /r (+ /r 7)))
        ((equal? /d "8")
         (set! /r (+ /r 8)))
        ((equal? /d "9")
         (set! /r (+ /r 9)))
        ((equal? /d "A")
         (set! /r (+ /r 10)))
        ((equal? /d "B")
         (set! /r (+ /r 11)))
        ((equal? /d "C")
         (set! /r (+ /r 12)))
        ((equal? /d "D")
         (set! /r (+ /r 13)))
        ((equal? /d "E")
         (set! /r (+ /r 14)))
        ((equal? /d "F")
         (set! /r (+ /r 15)))))))))
  /r))

; This version is needed for hex numbers which are too large for Scheme ints 
(define (@Hex_To_Num_String /s)
 (let ((/r "")
       (/l (string-length /s)))
  (cond
   ((= /l 0)
    (set! /r "0"))
   ((equal? (substr /s 0 1) "-")
    (set! /r (string-append "-" (@Hex_To_Num_String (substr /s 1)))))
   ((< /l 14)
    (set! /r (@String (@Hex_To_Num /s))))
   (#t
    ; xxxxx TODO xxxxx 
    (set! /r (@String (@Hex_To_Num /s)))))
  /r))

(define (@Num_To_Hex /n-par)
 (let ((/n-save /n)
       (/r "")
       (/d 0)
       (/digits "0123456789ABCDEF")
       (funct-result '()))
  (set! /n /n-par)
  ; Pick out the last digit and prepend it to the result: 
  (set! /d (modulo /n 16))
  (set! /r (concat (substr /digits /d 1) /r))
  (set! /n (quotient /n 16))
  (while (not (= /n 0)) 
   (begin
    ; Pick out the last digit and prepend it to the result: 
    (set! /d (modulo /n 16))
    (set! /r (concat (substr /digits /d 1) /r))
    (set! /n (quotient /n 16))))
  ; Ensure an even number of hex digits: 
  (cond
   ((= (modulo (string-length /r) 2) 1)
    (set! /r (string-append "0" /r))))
  (set! funct-result /r)
  (set! /n /n-save)
  funct-result))

(define (@Bin_To_Hex /bin)
 (let ((/hex "")
       (/n-save /n)
       (/i-save /i)
       (/digit 0)
       (/d "")
       (funct-result '()))
  (set! /n 0)
  (set! /i 0)
  (while (> (modulo (string-length /bin) 4) 0) 
   (set! /bin (string-append "0" /bin)))
  (while (< /n (string-length /bin)) 
   (begin
    ; Pick out the next digit and append to result: 
    (set! /digit 0)
    (for /i 0 3 1 
     (begin
      (set! /digit (* /digit 2))
      (cond
       ((equal? (substr /bin (+ /n /i) 1) "1")
        (set! /digit (+ /digit 1))))))
    (set! /d (@Num_To_Hex /digit))
    (set! /hex (concat /hex (substr /d (- (string-length /d) 1) 1)))
    (set! /n (+ /n 4))))
  (set! funct-result /hex)
  (set! /n /n-save)
  (set! /i /i-save)
  funct-result))

(define (@Lvalue_To_Expn //I)
 (let ((//S/T (@ST //I))
       (/comps (@Components //I)))
  (cond
   ((= (@GT //I) //T_/Lvalue)
    (cond
     ((= //S/T //T_/Var_/Lvalue)
      (set! //S/T //T_/Variable))
     ((= //S/T //T_/Aref_/Lvalue)
      (set! //S/T //T_/Aref))
     ((= //S/T //T_/Sub_/Seg_/Lvalue)
      (set! //S/T //T_/Sub_/Seg))
     ((= //S/T //T_/Rel_/Seg_/Lvalue)
      (set! //S/T //T_/Rel_/Seg))
     ((= //S/T //T_/Final_/Seg_/Lvalue)
      (set! //S/T //T_/Final_/Seg))
     ((= //S/T //T_/Struct_/Lvalue)
      (set! //S/T //T_/Struct))
     ((= //S/T //T_/Mem_/Lvalue)
      (set! //S/T //T_/Mem))
     ((= //S/T //T_/Mem_/Seg_/Lvalue)
      (set! //S/T //T_/Mem_/Seg))
     ((= //S/T //T_/Mem_/Rel_/Lvalue)
      (set! //S/T //T_/Mem_/Rel))
     (#t
      (@Print_WSL //I "")))
    (cond
     ((= //S/T //T_/Struct)
      (wsl-set! /comps (@Lvalue_To_Expn (wsl-ref /comps 2)) 2))
     ((or (= //S/T //T_/Mem) (= //S/T //T_/Mem_/Seg) (= //S/T //T_/Mem_/Rel))
      #t)
     ((not (null? /comps))
      (wsl-set! /comps (@Lvalue_To_Expn (wsl-ref /comps 1)) 1)))
    (set! //I (@Make //S/T (@Value //I) /comps))))
  //I))

(define (@Expn_To_Lvalue //I)
 (let ((//S/T (@ST //I))
       (/comps (@Components //I)))
  (cond
   ((= (@GT //I) //T_/Expression)
    (cond
     ((= //S/T //T_/Variable)
      (set! //S/T //T_/Var_/Lvalue))
     ((= //S/T //T_/Aref)
      (set! //S/T //T_/Aref_/Lvalue))
     ((= //S/T //T_/Sub_/Seg)
      (set! //S/T //T_/Sub_/Seg_/Lvalue))
     ((= //S/T //T_/Rel_/Seg)
      (set! //S/T //T_/Rel_/Seg_/Lvalue))
     ((= //S/T //T_/Final_/Seg)
      (set! //S/T //T_/Final_/Seg_/Lvalue))
     ((= //S/T //T_/Struct)
      (set! //S/T //T_/Struct_/Lvalue))
     ((= //S/T //T_/Mem)
      (set! //S/T //T_/Mem_/Lvalue))
     ((= //S/T //T_/Mem_/Seg)
      (set! //S/T //T_/Mem_/Seg_/Lvalue))
     ((= //S/T //T_/Mem_/Rel)
      (set! //S/T //T_/Mem_/Rel_/Lvalue)))
    (cond
     ((= //S/T //T_/Struct_/Lvalue)
      (wsl-set! /comps (@Expn_To_Lvalue (wsl-ref /comps 2)) 2))
     ((or (= //S/T //T_/Mem_/Lvalue) (= //S/T //T_/Mem_/Seg_/Lvalue) (= //S/T //T_/Mem_/Rel_/Lvalue))
      #t)
     ((not (null? /comps))
      (wsl-set! /comps (@Expn_To_Lvalue (wsl-ref /comps 1)) 1)))
    (set! //I (@Make //S/T (@Value //I) /comps))))
  //I))

; Return TRUE if the given word appears in the string: 
(define (@Word_In_String? /word /str)
 
 (>= (my-index (string-append (string-append " " /word) " ") (string-append (string-append " " /str) " ") 0) 0))

; Convert a list of strings to a single string 
(define (@Join_Removing_Dups /glue /l)
 
 (if (null? /l) "" (if (= (gen-length /l) 1) (wsl-ref /l 1) (if (member (car /l) (cdr /l)) (@Join_Removing_Dups /glue (cdr /l)) (concat (concat (wsl-ref /l 1) /glue) (@Join_Removing_Dups /glue (cdr /l)))))))

(define (@Join /glue /l)
 
 (if (null? /l) "" (if (= (gen-length /l) 1) (wsl-ref /l 1) (concat (concat (wsl-ref /l 1) /glue) (@Join /glue (cdr /l))))))

; Convert a structure of nested lists to a string (roughly as PRINT does) 
(define (@Struct_To_String //L)
 
 (if (sequence? //L) (string-append (string-append "(" (@Join " " (my-map @Struct_To_String //L))) ")") (@String //L)))

; Split a string into a list of (non-empty) words: 
(define (@Split /str-par)
 (let ((/str-save /str)
       (//R '())
       (/p 0)
       (/q (my-index " " /str-par 0))
       (funct-result '()))
  (set! /str /str-par)
  (while (>= /q 0) 
   (begin
    (cond
     ((> /q /p)
      (set! //R (cons (substr /str /p (- /q /p)) //R))))
    (set! /p (+ /q 1))
    (set! /q (my-index " " /str (+ /q 1)))))
  (cond
   ((< /p (string-length /str))
    (set! //R (cons (substr /str /p) //R))))
  (set! funct-result (reverse //R))
  (set! /str /str-save)
  funct-result))

; Check if the string ends with the given extension: 
(define (@Ends_With? /str-par /extn)
 (let ((/str-save /str)
       (//R 0)
       (/len (string-length /extn))
       (funct-result '()))
  (set! /str /str-par)
  (cond
   ((number? /str)
    (set! /str (@N_String /str))))
  (cond
   ((and (>= (string-length /str) /len) (equal? (substr /str (- (string-length /str) /len) /len) /extn))
    (set! //R 1)))
  (set! funct-result (= //R 1))
  (set! /str /str-save)
  funct-result))

; Check if the string starts with the given extension: 
(define (@Starts_With? /str-par /extn)
 (let ((/str-save /str)
       (//R 0)
       (/len (string-length /extn))
       (funct-result '()))
  (set! /str /str-par)
  (cond
   ((number? /str)
    (set! /str (@N_String /str))))
  (cond
   ((and (>= (string-length /str) /len) (equal? (substr /str 0 /len) /extn))
    (set! //R 1)))
  (set! funct-result (= //R 1))
  (set! /str /str-save)
  funct-result))

; Check if name is of the form FOO_xxx_nnn (given string plus 3 digits): 
(define (@Starts_With_Plus_Num? /name-par /extn)
 (let ((/name-save /name)
       (//R 0)
       (/len (string-length /extn))
       (funct-result '()))
  (set! /name /name-par)
  (cond
   ((and (number? /name) (> /name 0))
    (set! /name (@N_String /name))
    (cond
     ((and (>= (string-length /name) (+ /len 3)) (equal? (substr /name 0 /len) /extn) (@Digits? (substr /name (- (string-length /name) 3) 3)))
      (set! //R 1)))))
  (set! funct-result (= //R 1))
  (set! /name /name-save)
  funct-result))

; Check if given name ends in __ plus one or more digits: 
(define (@Ends_With_Underscore_Digits? /name-par)
 (let ((/name-save /name)
       (//R 0)
       (/p 0)
       (funct-result '()))
  (set! /name /name-par)
  (cond
   ((and (number? /name) (> /name 0))
    (set! /name (@N_String /name))
    (set! /p (- (string-length /name) 1))
    (while (and (> /p 0) (@Digit? (substr /name /p 1))) 
     (set! /p (- /p 1)))
    (cond
     ((and (> /p 1) (< /p (- (string-length /name) 1)))
      (cond
       ((equal? (substr /name (- /p 1) 2) "__")
        (set! //R 1)))))))
  (set! funct-result (= //R 1))
  (set! /name /name-save)
  funct-result))

; Trim __nnn from the given string and return the number nnn 
(define (@Trim_Underscore_Number /str-par /n-par)
 (let ((/n-save /n)
       (/str-save /str)
       (funct-result '()))
  (set! /n /n-par)
  (set! /str /str-par)
  (let ((/p (- (string-length /str) 1)))
   (while (and (>= /p 0) (@Digit? (substr /str /p 1))) 
    (set! /p (- /p 1)))
   (set! /n (@String_To_Num (substr /str (+ /p 1))))
   (set! /str (substr /str 0 (+ /p 1)))
   (cond
    ((> /p 1)
     (cond
      ((equal? (substr /str (- /p 1) 2) "__")
       (set! /str (substr /str 0 (- /p 1))))))))
  (set! funct-result (list /str /n))
  (set! /n /n-save)
  (set! /str /str-save)
  funct-result))

; Sort a list of names or elements: 
(define (@Sort_List //L)
 (let ((//R '())
       (/len (gen-length //L)))
  (cond
   ((<= /len 1)
    (set! //R //L))
   ((= /len 2)
    (cond
     ((@Sort_Less? (wsl-ref //L 1) (wsl-ref //L 2))
      (set! //R //L))
     (#t
      (set! //R (list (wsl-ref //L 2) (wsl-ref //L 1))))))
   (#t
    (let ((/mid (quotient /len 2)))
     (set! //R (@Sort_Merge (@Sort_List (@Sub_Seg //L 1 /mid)) (@Sort_List (@Final_Seg //L (+ /mid 1))))))))
  //R))

; Merge two sorted lists: 
(define (@Sort_Merge //L1 //L2)
 (let ((//R '()))
  (cond
   ((null? //L1)
    (set! //R //L2))
   ((null? //L2)
    (set! //R //L1))
   ((@Sort_Less? (car //L1) (car //L2))
    (set! //R (cons (car //L1) (@Sort_Merge (cdr //L1) //L2))))
   (#t
    (set! //R (cons (car //L2) (@Sort_Merge //L1 (cdr //L2))))))
  //R))

; Compare two names or lists of names: 
(define (@Sort_Less? /a /b)
 (let ((//R 0))
  (cond
   ((sequence? /a)
    (cond
     ((not (sequence? /b))
      (set! //R 0))
     ((null? /a)
      (set! //R 1))
     ((null? /b)
      (set! //R 0))
     ((equal? (car /a) (car /b))
      (cond
       ((@Sort_Less? (cdr /a) (cdr /b))
        (set! //R 1))
       (#t
        (set! //R 0))))
     ((@Sort_Less? (car /a) (car /b))
      (set! //R 1))
     (#t
      (set! //R 0))))
   ((sequence? /b)
    (set! //R 1))
   ((@String_Less? (@Name_Or_Num_String /a) (@Name_Or_Num_String /b))
    (set! //R 1))
   (#t
    (set! //R 0)))
  (= //R 1)))

; Sort a list of numbers: 
(define (@Sort_Num //L)
 (let ((//R '())
       (/len (gen-length //L)))
  (cond
   ((<= /len 1)
    (set! //R //L))
   ((= /len 2)
    (cond
     ((< (wsl-ref //L 1) (wsl-ref //L 2))
      (set! //R //L))
     (#t
      (set! //R (list (wsl-ref //L 2) (wsl-ref //L 1))))))
   (#t
    (let ((/mid (quotient /len 2)))
     (set! //R (@Sort_Merge_Num (@Sort_Num (@Sub_Seg //L 1 /mid)) (@Sort_Num (@Final_Seg //L (+ /mid 1))))))))
  //R))

(define (@Sort_Merge_Num //L1 //L2)
 (let ((//R '()))
  (cond
   ((null? //L1)
    (set! //R //L2))
   ((null? //L2)
    (set! //R //L1))
   ((< (car //L1) (car //L2))
    (set! //R (cons (car //L1) (@Sort_Merge_Num (cdr //L1) //L2))))
   (#t
    (set! //R (cons (car //L2) (@Sort_Merge_Num //L1 (cdr //L2))))))
  //R))

(set! //Swap_/Case 1)
(define (@Swap_Case /obj)
 
 (if (= //Swap_/Case 1) (swapcase (@N_String /obj)) (@N_String /obj)))

(define (@Upper_Case /obj)
 
 (upcase (@N_String /obj)))

(define (@Lower_Case /obj)
 
 (lowcase (@N_String /obj)))

; Simple file writing: 
(set! //Output_/Stack '())
(set! //Output_/Port //Standard_/Output_/Port)
(define (@Write_To /file)
 (set! //Output_/Stack (cons (list //Output_/Port /file) //Output_/Stack))
 (set! //Output_/Port (if (equal? /file "") //Standard_/Output_/Port (@Open_Output_File /file))))

(define (@End_Write)
 (cond
  ((null? //Output_/Stack)
   (error "@End_Write called with no corresponding @Write_To"))
  (#t
   (cond
    ((not (equal? (wsl-ref (wsl-ref //Output_/Stack 1) 2) ""))
     (@Close_Output_Port //Output_/Port)))
   (set! //Output_/Port (wsl-ref (wsl-ref //Output_/Stack 1) 1))
   (set! //Output_/Stack (cdr //Output_/Stack)))))

(define (@WS /string)
 (@Write /string //Output_/Port))

(define (@WL /string)
 (@Write_Line /string //Output_/Port))

; Write a number (eg integer represented as floating point) 
(define (@WN /n-par)
 (let ((/n-save /n))
  (set! /n /n-par)
  (cond
   ((< /n 0)
    (@WS "-")
    (@WN (- /n)))
   ((> /n 10000)
    (@WN (quotient /n 10000))
    (set! /n (modulo /n 10000))
    (let ((/m 1000))
     (@WS (quotient /n /m))
     (while (not (= /m 1)) 
      (begin
       (set! /n (modulo /n /m))
       (set! /m (quotient /m 10))
       (@WS (quotient /n /m))))))
   (#t
    (let ((/digits '())
          (/m 0))
     (while (> /n 0) 
      (begin
       (set! /m /n)
       (set! /n (quotient /n 10))
       (set! /digits (cons (modulo /m 10) /digits))))
     (cond
      ((null? /digits)
       (set! /digits (list 0))))
     (while (not (null? /digits)) 
      (begin
       (@WS (car /digits))
       (set! /digits (cdr /digits)))))))
  (set! /n /n-save)))

; Is nth char of str in valid list? 
(define (@Char_In_Str? /str /n /valid)
 
 (and (> (string-length /str) /n) (>= (my-index (substr /str /n 1) /valid 0) 0)))

; Check if the given character (single char string) is a digit: 
(define (@Digit? /c)
 
 (>= (my-index /c "0123456789" 0) 0))

(define (@Digits? /s)
 
 (or (= (string-length /s) 0) (and (@Digit? (substr /s 0 1)) (@Digits? (substr /s 1)))))

; Check if the given character (single char string) is a hex digit: 
(define (@Hex? /c)
 
 (>= (my-index /c "0123456789ABCDEFabcdef" 0) 0))

(define (@All_Hex? /s)
 
 (or (= (string-length /s) 0) (and (@Hex? (substr /s 0 1)) (@All_Hex? (substr /s 1)))))

; Strip the given character from the string: 
(define (@Strip_Char /c /str)
 (let ((//R "")
       (/p 0))
  (set! /p (my-index /c /str 0))
  (while (>= /p 0) 
   (begin
    (set! //R (concat //R (substr /str 0 /p)))
    (set! /str (substr /str (+ /p 1)))
    (set! /p (my-index /c /str 0))))
  (concat //R /str)))

; Go from the action call to the corresponding body 
(define (@Goto_Body)
 (let ((/posn (@Posn))
       (/name-save /name))
  (set! /name (@V (@I)))
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Actions))) 
   (@Up))
  (cond
   ((= (@ST (@I)) //T_/Actions)
    (@Down)
    ; to first action 
    (while (and (@Right?) (not (equal? /name (@V (@Get_n (@I) 1))))) 
     (@Right))))
  (cond
   ((or (not (= (@ST (@I)) //T_/Action)) (not (equal? (@V (@Get_n (@I) 1)) /name)))
    (@Goto /posn)))
  (set! /name /name-save)))

; Compute the transitive closure of a hash of lists and return a new hash 
; Floyd Warshall algorithm from: 
; http://www.foad.org/~abigail/Perl/Algorithms/Graphs/ 
(define (@Transitive_Closure /succs-par)
 (let ((/succs-save /succs)
       (//R (hash-table))
       (/elts (@Hash_Keys /succs-par))
       (/graph (hash-table))
       (funct-result '()))
  (set! /succs /succs-par)
  (cond
   (#f
    (display-list "Transitive_Closure input: ")
    (for-in /v (@Hash_Keys /succs) 
     (display-list /v " --> " (gethash /succs /v)))))
  (for-in /i /elts 
   (for-in /j (gethash /succs /i) 
    (puthash /graph (list /i /j) 1)))
  (for-in /k /elts 
   (for-in /i /elts 
    (for-in /j /elts 
     (cond
      ((and (not (null? (gethash /graph (list /k /j)))) (not (null? (gethash /graph (list /i /k)))))
       (puthash /graph (list /i /j) 1))))))
  (for-in /i /elts 
   (for-in /j /elts 
    (cond
     ((not (null? (gethash /graph (list /i /j))))
      (puthash //R /i (cons /j (gethash //R /i)))))))
  (cond
   (#f
    (display-list "Transitive_Closure output: ")
    (for-in /v (@Hash_Keys /succs) 
     (display-list /v " --> " (gethash //R /v)))))
  (set! funct-result //R)
  (set! /succs /succs-save)
  funct-result))

; Simple algorithm which iterates to convergance 
(define (@Transitive_Closure_Simple /succs-par)
 (let ((/succs-save /succs)
       (//R (hash-table))
       (/elts (@Hash_Keys /succs-par))
       (/change 0)
       (funct-result '()))
  (set! /succs /succs-par)
  (cond
   (#t
    (display-list "Transitive_Closure input: ")
    (for-in /v (@Hash_Keys /succs) 
     (display-list /v " --> " (gethash /succs /v)))))
  (for-in /elt /elts 
   (puthash //R /elt (@Make_Set (cons /elt (gethash /succs /elt)))))
  (set! /change 0)
  (for-in /i /elts 
   (for-in /j (gethash //R /i) 
    (cond
     ((not (@Set_Subset? (gethash //R /j) (gethash //R /i)))
      (set! /change 1)
      (puthash //R /i (union-n (gethash //R /i) (gethash //R /j)))))))
  (while (not (= /change 0)) 
   (begin
    (set! /change 0)
    (for-in /i /elts 
     (for-in /j (gethash //R /i) 
      (cond
       ((not (@Set_Subset? (gethash //R /j) (gethash //R /i)))
        (set! /change 1)
        (puthash //R /i (union-n (gethash //R /i) (gethash //R /j)))))))))
  (cond
   (#t
    (display-list "Transitive_Closure output: ")
    (for-in /v (@Hash_Keys /succs) 
     (display-list /v " --> " (gethash //R /v)))))
  (set! funct-result //R)
  (set! /succs /succs-save)
  funct-result))

; Compute the transitive closure of a hash of lists and return a new hash 
; The algorithm is from `An Efficient Transitive Closure Algorithm 
; for Cyclic Digraphs' by Esko Nuutila, Information Processing Letters, 
; 52:207--213, 1994. http://www.cs.hut.fi/~enu/tc.html 
(define (@Transitive_Closure_New /succs-par)
 (let ((/succs-save /succs)
       (//R (hash-table))
       (//C-save //C)
       (//S/C-save //S/C)
       (/root-save /root)
       (/dfs-save /dfs)
       (/dfs_num-save /dfs_num)
       (/nstack-save /nstack)
       (/cstack-save /cstack)
       (/hsaved-save /hsaved)
       (//Succ-save //Succ)
       (//Elts-save //Elts)
       (funct-result '()))
  (set! /succs /succs-par)
  (set! //C '())
  (set! //S/C (hash-table))
  (set! /root (hash-table))
  (set! /dfs (hash-table))
  (set! /dfs_num 1)
  (set! /nstack '())
  (set! /cstack '())
  (set! /hsaved (hash-table))
  (set! //Succ (hash-table))
  (set! //Elts (hash-table))
  (cond
   (#f
    (display-list "Transitive_Closure input: ")
    (for-in /v (@Hash_Keys /succs) 
     (display-list (@N_String /v) " --> " (my-map @N_String (gethash /succs /v))))))
  (cond
   (#t
    (display-list "Transitive_Closure input: ")
    (for-in /v (@Hash_Keys /succs) 
     (display-list /v " --> " (gethash /succs /v)))))
  (for-in /v (@Hash_Keys /succs) 
   (cond
    ((null? (gethash /dfs /v))
     (@COMP_TC /v))))
  (cond
   (#f
    (display-list "Strong Components:")
    (for-in /v (@Hash_Keys //Elts) 
     (display-list (@N_String /v) " --> " (my-map @N_String (gethash //Elts /v))))))
  (cond
   (#t
    (display-list "Strong Components:")
    (for-in /v (@Hash_Keys //Elts) 
     (display-list /v " --> " (gethash //Elts /v)))))
  ; Compute R for each strong component root: 
  (for-in /v (@Hash_Keys //Elts) 
   (begin
    (for-in /w (gethash //Succ /v) 
     (begin
      (display-list "R.(" /v ") := " (gethash //Elts /w) " ++ " (gethash //R /v))
      (puthash //R /v (concat (gethash //Elts /w) (gethash //R /v)))))
    (puthash //R /v (@Make_Set (gethash //R /v)))))
  ; Copy R from the strong component root to each element of the component 
  (for-in /v (@Hash_Keys /succs) 
   (puthash //R /v (gethash //R (gethash //S/C /v))))
  (cond
   (#f
    (display-list "Transitive_Closure output: ")
    (for-in /v (@Hash_Keys /succs) 
     (display-list (@N_String /v) " --> " (my-map @N_String (gethash //R /v))))))
  (cond
   (#t
    (display-list "Transitive_Closure output: ")
    (for-in /v (@Hash_Keys /succs) 
     (display-list /v " --> " (gethash //R /v)))))
  (set! funct-result //R)
  (set! /succs /succs-save)
  (set! //C //C-save)
  (set! //S/C //S/C-save)
  (set! /root /root-save)
  (set! /dfs /dfs-save)
  (set! /dfs_num /dfs_num-save)
  (set! /nstack /nstack-save)
  (set! /cstack /cstack-save)
  (set! /hsaved /hsaved-save)
  (set! //Succ //Succ-save)
  (set! //Elts //Elts-save)
  funct-result))

(define (@COMP_TC /v-par)
 (let ((/v-save /v))
  (set! /v /v-par)
  (let ((/forward_edge 0)
        (//X '()))
   (puthash /dfs /v /dfs_num)
   (set! /dfs_num (+ /dfs_num 1))
   (puthash /root /v /v)
   (puthash //S/C /v '())
   (set! /nstack (cons /v /nstack))
   (puthash /hsaved /v (gen-length /cstack))
   (for-in /w (gethash /succs /v) 
    (begin
     ; Is v -> w a forward edge? (to a marked node with higher dfs) 
     (cond
      ((and (not (null? (gethash /dfs /w))) (> (gethash /dfs /w) (gethash /dfs /v)))
       (set! /forward_edge 1)))
     (display-list "forward_edge " /v " -> " /w " = " /forward_edge)
     (cond
      ((null? (gethash /dfs /w))
       (@COMP_TC /w)))
     (cond
      ((null? (gethash //S/C /w))
       (cond
        ((< (gethash /dfs (gethash /root /w)) (gethash /dfs (gethash /root /v)))
         (puthash /root /v (gethash /root /w)))))
      ((= /forward_edge 0)
       (set! /cstack (cons (gethash //S/C /w) /cstack))))))
   (cond
    ((equal? (gethash /root /v) /v)
     ; Create a new component 
     (set! //C /v)
     ; If there is only one element in this component 
     ; and this element doesn't have a self-loop edge 
     ; then C is not a sucessor of C 
     (cond
      ((and (equal? (car /nstack) /v) (not-member /v (gethash /succs /v)))
       (puthash //Succ //C '()))
      (#t
       (puthash //Succ //C (list //C))))
     ; Could process these components in topological order 
     ; to minimise the number of set operations 
     (while (not (= (gen-length /cstack) (gethash /hsaved /v))) 
      (begin
       (set! //X (car /cstack))
       (set! /cstack (cdr /cstack))
       (cond
        ((not-member //X (gethash //Succ //C))
         (puthash //Succ //C (union-n (gethash //Succ //C) (list //X) (gethash //Succ //X)))))))
     (set! /w (car /nstack))
     (set! /nstack (cdr /nstack))
     ; Insert w into component C 
     (display-list "Inserting " /w " into component " //C)
     (puthash //Elts //C (cons /w (gethash //Elts //C)))
     (puthash //S/C /w //C)
     (while (not (equal? /v /w)) 
      (begin
       (set! /w (car /nstack))
       (set! /nstack (cdr /nstack))
       ; Insert w into component C 
       (display-list "Inserting " /w " into component " //C)
       (puthash //Elts //C (cons /w (gethash //Elts //C)))
       (puthash //S/C /w //C))))))
  (set! /v /v-save)))

; Compute a breadth first search of the succs array, return the list of nodes. 
(define (@Breadth_First_Search /start /succs-par)
 (let ((/succs-save /succs)
       (//R '())
       (/fringe (list /start))
       (/done (hash-table))
       (/node '())
       (funct-result '()))
  (set! /succs /succs-par)
  (while (not (null? /fringe)) 
   (begin
    (set! /node (car /fringe))
    (set! /fringe (cdr /fringe))
    (set! //R (cons /node //R))
    (for-in /succ (@Sort_Num (wsl-ref /succs /node)) 
     (cond
      ((null? (gethash /done /succ))
       (puthash /done /succ 1)
       (set! /fringe (concat /fringe (list /succ))))))))
  (set! funct-result (reverse //R))
  (set! /succs /succs-save)
  funct-result))

; Replace copies of one item by a different one (for small items only): 
(define (@Replace //I /new /old)
 (let ((//R //I))
  (cond
   ((@Equal? //I /old)
    (set! //R /new))
   ((@Cs? //I)
    (let ((//L '()))
     (for-in /comp (@Cs //I) 
      (set! //L (cons (@Replace /comp /new /old) //L)))
     (set! //R (@Make (@ST //I) '() (reverse //L))))))
  //R))

(define (@Inc_Hash /key /tab)
 (cond
  ((null? (gethash /tab /key))
   (puthash /tab /key 1))
  (#t
   (puthash /tab /key (+ (gethash /tab /key) 1))))
 /tab)

; Compute a topological sort of a graph given a hash table of predecessors. 
; The second argument is a hash table giving a priority order 
; (it maps names to numbers) to use as a secondary ordering relation. 
; mins is the list of minimal elements in todo 
; numpreds.(n) is the number of predecessors (in todo) for n. 
(define (@Topological_Sort /nodes /succs-par /preds /order)
 (let ((/succs-save /succs)
       (/todo (@Make_Set /nodes))
       (/mins '())
       (/numpreds (hash-table))
       (/min '())
       (//R '())
       (funct-result '()))
  (set! /succs /succs-par)
  ; Ensure all nodes are in order 
  (for-in /name /nodes 
   (cond
    ((null? (gethash /order /name))
     (puthash /order /name 0))))
  (for-in /name /todo 
   (begin
    (puthash /numpreds /name (gen-length (gethash /preds /name)))
    (cond
     ((= (gethash /numpreds /name) 0)
      (set! /mins (union-n (list /name) /mins))))))
  (while (not (null? /todo)) 
   (begin
    (cond
     ((not (null? /mins))
      ; Pick the minimal element with the smallest order number 
      (set! /min (car /mins))
      (for-in /elt (cdr /mins) 
       (cond
        ((< (gethash /order /elt) (gethash /order /min))
         (set! /min /elt))))
      (set! /mins (@Set_Difference /mins (list /min))))
     (#t
      ; There must be a recursive call somewhere. 
      (display-list "Recursive call found!!!")
      ; Pick an element with the smallest number of predecessors 
      (set! /min (car /todo))
      (for-in /elt (cdr /todo) 
       (cond
        ((or (< (gethash /numpreds /elt) (gethash /numpreds /min)) (and (equal? (gethash /numpreds /elt) (gethash /numpreds /min)) (< (gethash /order /elt) (gethash /order /min))))
         (set! /min /elt))))
      ; Treat min as if it were a real minimal element: 
      (puthash /numpreds /min 0)))
    ; Build up the result in reverse order: 
    (set! //R (cons /min //R))
    (set! /todo (@Set_Difference /todo (list /min)))
    ; Update numpreds: decrement numpreds for mins successors: 
    ; Note: could have spurious entries in succs table for proc calls with no defn 
    (for-in /elt (gethash /succs /min) 
     (cond
      ((not (null? (gethash /numpreds /elt)))
       (puthash /numpreds /elt (- (gethash /numpreds /elt) 1))
       (cond
        ((= (gethash /numpreds /elt) 0)
         (set! /mins (union-n (list /elt) /mins)))))))))
  (set! funct-result (reverse //R))
  (set! /succs /succs-save)
  funct-result))

(define (@Sort_Test /type-par //N-par /k-par)
 (let ((/k-save /k)
       (//N-save //N)
       (/type-save /type))
  (set! /k /k-par)
  (set! //N //N-par)
  (set! /type /type-par)
  (let ((//A-save //A)
        (/t 0)
        (/mod (integer-expt 2 27)))
   (set! //A (make-vector-eval //N 0))
   (display-list "Initialising... " (@Runtime))
   (let ((/x (@Random 100000))
         (/m 1664525)
         (/c -59837601))
    (for /i 1 //N 1 
     (begin
      (wsl-set! //A /x /i)
      (set! /x (modulo (+ (* /m /x) /c) /mod)))))
   (for /i 1 //N 1 
    (wsl-set! //A (wsl-ref //A /i) /i))
   (display-list "Starting... " (@Runtime))
   (set! /t (@Runtime))
   (cond
    ((equal? /type "Quick")
     (set! //A (@Quick_sort  //N /k //A)))
    ((equal? /type "DPQ")
     (set! //A (@DPQ_sort  //N /k //A)))
    ((equal? /type "Shell")
     (set! //A (@Shell_sort  //N /k //A)))
    ((equal? /type "Insertion")
     (set! //A (@Insertion_sort  //N /k //A)))
    ((equal? /type "Bubble")
     (set! //A (@Bubble_sort  //N /k //A)))
    (#t
     (display-list "Unknown sort type: " /type)))
   (display-list "Checking... " (@Runtime))
   (for /i 2 //N 1 
    (cond
     ((> (wsl-ref //A (- /i 1)) (wsl-ref //A /i))
      (error "Shouldn't happen!"))))
   (cond
    ((<= //N 1000)
     (display-list "Result = " //A)))
   (display-list "Finished! " (@Runtime))
   (display-list "sort time = " (- (@Runtime) /t))
   #t
   (set! //A //A-save))
  (set! /k /k-save)
  (set! //N //N-save)
  (set! /type /type-save)))

(define (@DPQ_Pivots /i /j)
 (let ((/sixth 0)
       (/e1 0)
       (/e2 0)
       (/e3 0)
       (/e4 0)
       (/e5 0))
  (set! /sixth (quotient (+ (- /j /i) 1) 6))
  (set! /e1 (+ /i /sixth))
  (set! /e3 (quotient (+ /i /j) 2))
  (set! /e2 (- /e3 /sixth))
  (set! /e4 (+ /e3 /sixth))
  (set! /e5 (- /j /sixth))
  (list /e1 /e2 /e3 /e4 /e5)))

(define (@Sort_Test_List /type_list /data //N-par /k-par)
 (let ((/k-save /k)
       (//N-save //N))
  (set! /k /k-par)
  (set! //N //N-par)
  (let ((//A-save //A)
        (//B (make-vector-eval //N 0))
        (/start 0)
        (/end 0)
        (/mod (integer-expt 2 27)))
   (set! //A (make-vector-eval //N 0))
   (let ((/pivots (@DPQ_Pivots 1 //N))
         (/min 0)
         (/e1 0)
         (/e2 0)
         (/e3 0))
    (display-list "pivots = " /pivots)
    (display-list "Initialising... " (@Runtime))
    (let ((/x (@Random 100000))
          (/m 1664525)
          (/c -59837601))
     (for /i 1 //N 1 
      (begin
       (wsl-set! //A /x /i)
       (set! /x (modulo (+ (* /m /x) /c) /mod)))))
    (for /i 1 //N 1 
     (begin
      (cond
       ((equal? /data "Random")
        #t)
       ((equal? /data "Ordered")
        (wsl-set! //A /i /i))
       ((equal? /data "Random01")
        (wsl-set! //A (modulo (quotient (wsl-ref //A /i) 10) 2) /i))
       ((equal? /data "Random16")
        (wsl-set! //A (modulo (quotient (wsl-ref //A /i) 10) 16) /i))
       ((equal? /data "Same")
        (wsl-set! //A 1 /i))
       ((equal? /data "VVVVV")
        ; Set A[i] to the distance to the nearest pivot: 
        (set! /min //N)
        (set! /e1 0)
        (for-in /pivot /pivots 
         (begin
          (set! /e1 (+ /e1 1))
          (cond
           ((< (abs (- /i /pivot)) /min)
            (set! /min (abs (- /i /pivot)))
            (set! /e2 /e1)))))
        (wsl-set! //A (+ (* 10 /min) (* 0 /e2)) /i)
        (wsl-set! //A //N /i))
       (#t
        (error (string-append "Unknown data value: " /data))))
      (wsl-set! //B (wsl-ref //A /i) /i)))
    (set! /e1 0)
    (set! /e2 0)
    (set! /e3 0)
    (set! /i 1)
    (while (< /i (quotient //N 6)) 
     (begin
      (for-in /j (@DPQ_Pivots /i //N) 
       (begin
        (while (and (< (+ /j /e2 /e3) //N) (<= (wsl-ref //A (+ /j /e2 /e3)) /e1)) 
         (set! /e2 (+ /e2 1)))
        (set! /j (+ /j /e2 /e3))
        (cond
         ((<= //N 10000)
          (display-list "inserting A[" /j "] = " (wsl-ref //A /j) " --> " /e1)))
        (wsl-set! //A /e1 /j)
        (wsl-set! //B /e1 /j)))
      (set! /i (+ /i 5))
      (set! /e1 (+ /e1 1))
      (set! /e3 (+ /e3 1))))
    (display-list "--------------------------------------------------------")
    (for-in /type /type_list 
     (begin
      (for /i 1 //N 1 
       (wsl-set! //A (wsl-ref //B /i) /i))
      (display-list "================ " /type " sort N = " //N " k = " /k " data = " /data)
      (display-list "Starting... " (@Runtime))
      (set! /start (@Runtime))
      (cond
       ((equal? /type "Quick")
        (set! //A (@Quick_sort  //N /k //A)))
       ((equal? /type "Quick2")
        (set! //A (@Quick_sort2  //N /k //A)))
       ((equal? /type "Quick3")
        (set! //A (@Quick_sort3  //N /k //A)))
       ((equal? /type "Quick4")
        (set! //A (@Quick_sort4  //N /k //A)))
       ((equal? /type "Quick5")
        (set! //A (@Quick_sort5  //N /k //A)))
       ((equal? /type "DPQ")
        (cond
         ((< /k 6)
          (set! /k 6)))
        (set! //A (@DPQ_sort  //N /k //A)))
       ((equal? /type "DPQ2")
        (cond
         ((< /k 6)
          (set! /k 6)))
        (set! //A (@DPQ2_sort  //N /k //A)))
       ((equal? /type "Shell")
        (set! //A (@Shell_sort  //N /k //A)))
       ((equal? /type "Insertion")
        (set! //A (@Insertion_sort  //N /k //A)))
       ((equal? /type "Bubble")
        (set! //A (@Bubble_sort  //N /k //A)))
       (#t
        (display-list "Unknown sort type: " /type)))
      (set! /end (@Runtime))
      (display-list "Checking... " (@Runtime))
      (for /i 2 //N 1 
       (cond
        ((> (wsl-ref //A (- /i 1)) (wsl-ref //A /i))
         (display-list //A)
         (error "Shouldn't happen!"))))
      (cond
       ((<= //N 1000)
        (display-list "Result = " //A)))
      (display-list "Finished! " (@Runtime))
      (display-list "sort time = " (- /end /start))))
    (display-list "--------------------------------------------------------"))
   (set! //A //A-save))
  (set! /k /k-save)
  (set! //N //N-save)))

(define (@Quick_sort //N-par /k-par //A-par)
 (let ((//A-save //A)
       (/k-save /k)
       (//N-save //N)
       (funct-result '()))
  (set! //A //A-par)
  (set! /k /k-par)
  (set! //N //N-par)
  (let ((//A/S '())
        (/a 1)
        (/b //N)
        (/i-save /i)
        (/j-save /j)
        (/v-save /v))
   (set! /i 0)
   (set! /j 0)
   (set! /v 0)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((<= //N 1000)
       (display-list "a = " /a " b = " /b " AS = " //A/S)))
     (cond
      ((> (+ (- /b /a) 1) /k)
       (set! /j (quotient (+ /a /b) 2))
       (cond
        ((or (and (< (wsl-ref //A /a) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /b))) (and (< (wsl-ref //A /b) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /a))))
         (let ((/tmp-var2 (wsl-ref //A /a))
               (/tmp-var1 (wsl-ref //A /j)))
          (wsl-set! //A /tmp-var2 /j)
          (wsl-set! //A /tmp-var1 /a)))
        ((or (and (< (wsl-ref //A /a) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /j))) (and (< (wsl-ref //A /j) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /a))))
         (let ((/tmp-var2 (wsl-ref //A /a))
               (/tmp-var1 (wsl-ref //A /b)))
          (wsl-set! //A /tmp-var2 /b)
          (wsl-set! //A /tmp-var1 /a))))
       (set! /i (- /a 1))
       (set! /j (+ /b 1))
       (set! /v (wsl-ref //A /a))
       (set! /i (+ /i 1))
       (while (< (wsl-ref //A /i) /v) 
        (set! /i (+ /i 1)))
       (set! /j (- /j 1))
       (while (< /v (wsl-ref //A /j)) 
        (set! /j (- /j 1)))
       (while (< /i /j) 
        (begin
         (let ((/tmp-var2 (wsl-ref //A /j))
               (/tmp-var1 (wsl-ref //A /i)))
          (wsl-set! //A /tmp-var2 /i)
          (wsl-set! //A /tmp-var1 /j))
         (set! /i (+ /i 1))
         (while (< (wsl-ref //A /i) /v) 
          (set! /i (+ /i 1)))
         (set! /j (- /j 1))
         (while (< /v (wsl-ref //A /j)) 
          (set! /j (- /j 1)))))
       (cond
        ((> (- /j /a) (- /b (+ /j 1)))
         (push //A/S /a)
         (push //A/S /j)
         (set! /a (+ /j 1))
         (set! /fl_flag1 0))
        (#t
         (push //A/S (+ /j 1))
         (push //A/S /b)
         (set! /b /j)
         (set! /fl_flag1 0))))
      ((equal? //A/S '())
       (set! /fl_flag1 1))
      (#t
       (set! /b (car //A/S))
       (set! //A/S (cdr //A/S))
       (set! /a (car //A/S))
       (set! //A/S (cdr //A/S))
       (set! /fl_flag1 0)))))
   (cond
    ((> /k 1)
     (display-list "Final insertion sort... " (@Runtime))
     (set! //A (@Insertion_sort  //N /k //A))))
   (set! /i /i-save)
   (set! /j /j-save)
   (set! /v /v-save))
  (set! funct-result //A)
  (set! //A //A-save)
  (set! /k /k-save)
  (set! //N //N-save)
  funct-result))

(define (@Quick_sort2 //N-par /k-par //A-par)
 (let ((//A-save //A)
       (/k-save /k)
       (//N-save //N)
       (funct-result '()))
  (set! //A //A-par)
  (set! /k /k-par)
  (set! //N //N-par)
  (set! //A (@Quick_sort_rec2  /k 1 //N //A))
  ; NB: k may be dynamically changed, so always do the final insertion sort: 
  (cond
   ((> /k 1)
    (display-list "Final insertion sort... " (@Runtime))
    (set! //A (@Insertion_sort  //N /k //A))))
  (set! funct-result //A)
  (set! //A //A-save)
  (set! /k /k-save)
  (set! //N //N-save)
  funct-result))

; Recursive Quicksort: 
(define (@Quick_sort_rec2 /k /a /b //A)
 (let ((/i-save /i)
       (/j-save /j)
       (/v-save /v))
  (set! /i 0)
  (set! /j 0)
  (set! /v 0)
  (cond
   ((> (+ (- /b /a) 1) /k)
    ; Median of three partitioning: 
    (set! /j (quotient (+ /a /b) 2))
    (cond
     ((or (and (< (wsl-ref //A /a) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /b))) (and (< (wsl-ref //A /b) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /a))))
      (let ((/tmp-var2 (wsl-ref //A /a))
            (/tmp-var1 (wsl-ref //A /j)))
       (wsl-set! //A /tmp-var2 /j)
       (wsl-set! //A /tmp-var1 /a)))
     ((or (and (< (wsl-ref //A /a) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /j))) (and (< (wsl-ref //A /j) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /a))))
      (let ((/tmp-var2 (wsl-ref //A /a))
            (/tmp-var1 (wsl-ref //A /b)))
       (wsl-set! //A /tmp-var2 /b)
       (wsl-set! //A /tmp-var1 /a))))
    (set! /i (- /a 1))
    (set! /j (+ /b 1))
    (set! /v (wsl-ref //A /a))
    (set! /i (+ /i 1))
    (while (< (wsl-ref //A /i) /v) 
     (set! /i (+ /i 1)))
    (set! /j (- /j 1))
    (while (< /v (wsl-ref //A /j)) 
     (set! /j (- /j 1)))
    (while (< /i /j) 
     (begin
      (let ((/tmp-var2 (wsl-ref //A /j))
            (/tmp-var1 (wsl-ref //A /i)))
       (wsl-set! //A /tmp-var2 /i)
       (wsl-set! //A /tmp-var1 /j))
      (set! /i (+ /i 1))
      (while (< (wsl-ref //A /i) /v) 
       (set! /i (+ /i 1)))
      (set! /j (- /j 1))
      (while (< /v (wsl-ref //A /j)) 
       (set! /j (- /j 1)))))
    (let ((/tmp-var2 (wsl-ref //A /j))
          (/tmp-var1 (wsl-ref //A /a)))
     (wsl-set! //A /tmp-var2 /a)
     (wsl-set! //A /tmp-var1 /j))
    (cond
     ((> (- /j /a) (- /b (+ /j 1)))
      (set! //A (@Quick_sort_rec2  /k (+ /j 1) /b //A))
      (set! //A (@Quick_sort_rec2  /k /a /j //A)))
     (#t
      (set! //A (@Quick_sort_rec2  /k /a /j //A))
      (set! //A (@Quick_sort_rec2  /k (+ /j 1) /b //A))))))
  (set! /i /i-save)
  (set! /j /j-save)
  (set! /v /v-save))
 //A)

(define (@Quick_sort3 //N-par /k-par //A-par)
 (let ((//A-save //A)
       (/k-save /k)
       (//N-save //N)
       (funct-result '()))
  (set! //A //A-par)
  (set! /k /k-par)
  (set! //N //N-par)
  (set! //A (@Quick_sort_rec3  /k 1 //N //A))
  ; NB: k may be dynamically changed, so always do the final insertion sort: 
  (cond
   ((> /k 1)
    (display-list "Final insertion sort... " (@Runtime))
    (set! //A (@Insertion_sort  //N /k //A))))
  (set! funct-result //A)
  (set! //A //A-save)
  (set! /k /k-save)
  (set! //N //N-save)
  funct-result))

; Recursive Quicksort: 
(define (@Quick_sort_rec3 /k /a /b //A)
 (let ((/i-save /i)
       (/j-save /j)
       (/v-save /v)
       (/swap 0))
  (set! /i 0)
  (set! /j 0)
  (set! /v 0)
  (cond
   ((> (+ (- /b /a) 1) /k)
    ; Median of three partitioning: 
    (set! /j (quotient (+ /a /b) 2))
    (cond
     ((or (and (< (wsl-ref //A /a) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /b))) (and (< (wsl-ref //A /b) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /a))))
      (let ((/tmp-var2 (wsl-ref //A /a))
            (/tmp-var1 (wsl-ref //A /j)))
       (wsl-set! //A /tmp-var2 /j)
       (wsl-set! //A /tmp-var1 /a)))
     ((or (and (< (wsl-ref //A /a) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /j))) (and (< (wsl-ref //A /j) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /a))))
      (let ((/tmp-var2 (wsl-ref //A /a))
            (/tmp-var1 (wsl-ref //A /b)))
       (wsl-set! //A /tmp-var2 /b)
       (wsl-set! //A /tmp-var1 /a))))
    (set! /i (- /a 1))
    (set! /j (+ /b 1))
    (set! /v (wsl-ref //A /a))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (set! /i (+ /i 1))
      (while (< (wsl-ref //A /i) /v) 
       (set! /i (+ /i 1)))
      (set! /j (- /j 1))
      (while (< /v (wsl-ref //A /j)) 
       (set! /j (- /j 1)))
      (cond
       ((>= /i /j)
        (set! /fl_flag1 1))
       ((not (equal? (wsl-ref //A /i) (wsl-ref //A /j)))
        (set! /swap (+ /swap 1))
        (let ((/tmp-var2 (wsl-ref //A /j))
              (/tmp-var1 (wsl-ref //A /i)))
         (wsl-set! //A /tmp-var2 /i)
         (wsl-set! //A /tmp-var1 /j))
        (cond
         ((> /swap 1)
          (set! /i (+ /i 1))
          (while (< (wsl-ref //A /i) /v) 
           (set! /i (+ /i 1)))
          (set! /j (- /j 1))
          (while (< /v (wsl-ref //A /j)) 
           (set! /j (- /j 1)))
          (while (< /i /j) 
           (begin
            (cond
             ((not (equal? (wsl-ref //A /i) (wsl-ref //A /j)))
              (set! /swap (+ /swap 1))))
            (let ((/tmp-var2 (wsl-ref //A /j))
                  (/tmp-var1 (wsl-ref //A /i)))
             (wsl-set! //A /tmp-var2 /i)
             (wsl-set! //A /tmp-var1 /j))
            (set! /i (+ /i 1))
            (while (< (wsl-ref //A /i) /v) 
             (set! /i (+ /i 1)))
            (set! /j (- /j 1))
            (while (< /v (wsl-ref //A /j)) 
             (set! /j (- /j 1)))))
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0))))
       (#t
        (set! /fl_flag1 0)))))
    (let ((/tmp-var2 (wsl-ref //A /j))
          (/tmp-var1 (wsl-ref //A /a)))
     (wsl-set! //A /tmp-var2 /a)
     (wsl-set! //A /tmp-var1 /j))
    (cond
     ((<= /swap 1)
      (set! /k (* 10 /k))))
    (cond
     ((> (- /j /a) (- /b (+ /j 1)))
      (set! //A (@Quick_sort_rec3  /k (+ /j 1) /b //A))
      (set! //A (@Quick_sort_rec3  /k /a /j //A)))
     (#t
      (set! //A (@Quick_sort_rec3  /k /a /j //A))
      (set! //A (@Quick_sort_rec3  /k (+ /j 1) /b //A))))))
  (set! /i /i-save)
  (set! /j /j-save)
  (set! /v /v-save))
 //A)

(define (@Quick_sort4 //N-par /k-par //A-par)
 (let ((//A-save //A)
       (/k-save /k)
       (//N-save //N)
       (funct-result '()))
  (set! //A //A-par)
  (set! /k /k-par)
  (set! //N //N-par)
  (set! //A (@Quick_sort_rec4  /k 1 //N //A))
  ; NB: k may be dynamically changed, so always do the final insertion sort: 
  (display-list "Final insertion sort... " (@Runtime))
  (set! //A (@Insertion_sort  //N /k //A))
  (set! funct-result //A)
  (set! //A //A-save)
  (set! /k /k-save)
  (set! //N //N-save)
  funct-result))

; Recursive Quicksort: 
(define (@Quick_sort_rec4 /k /a /b //A)
 (let ((/i-save /i)
       (/j-save /j)
       (/v-save /v)
       (/swap 0)
       (/same 0))
  (set! /i 0)
  (set! /j 0)
  (set! /v 0)
  (cond
   ((> (+ (- /b /a) 1) /k)
    ; Median of three partitioning: 
    (set! /j (quotient (+ /a /b) 2))
    (cond
     ((or (and (< (wsl-ref //A /a) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /b))) (and (< (wsl-ref //A /b) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /a))))
      (let ((/tmp-var2 (wsl-ref //A /a))
            (/tmp-var1 (wsl-ref //A /j)))
       (wsl-set! //A /tmp-var2 /j)
       (wsl-set! //A /tmp-var1 /a)))
     ((or (and (< (wsl-ref //A /a) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /j))) (and (< (wsl-ref //A /j) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /a))))
      (let ((/tmp-var2 (wsl-ref //A /a))
            (/tmp-var1 (wsl-ref //A /b)))
       (wsl-set! //A /tmp-var2 /b)
       (wsl-set! //A /tmp-var1 /a))))
    (set! /i (- /a 1))
    (set! /j (+ /b 1))
    (set! /v (wsl-ref //A /a))
    (set! /i (+ /i 1))
    (while (< (wsl-ref //A /i) /v) 
     (set! /i (+ /i 1)))
    (set! /j (- /j 1))
    (while (< /v (wsl-ref //A /j)) 
     (set! /j (- /j 1)))
    (while (< /i /j) 
     (begin
      (cond
       ((equal? (wsl-ref //A /i) (wsl-ref //A /j))
        (set! /same (+ /same 1)))
       (#t
        (set! /swap (+ /swap 1))
        (let ((/tmp-var2 (wsl-ref //A /j))
              (/tmp-var1 (wsl-ref //A /i)))
         (wsl-set! //A /tmp-var2 /i)
         (wsl-set! //A /tmp-var1 /j))))
      (set! /i (+ /i 1))
      (while (< (wsl-ref //A /i) /v) 
       (set! /i (+ /i 1)))
      (set! /j (- /j 1))
      (while (< /v (wsl-ref //A /j)) 
       (set! /j (- /j 1)))))
    (let ((/tmp-var2 (wsl-ref //A /j))
          (/tmp-var1 (wsl-ref //A /a)))
     (wsl-set! //A /tmp-var2 /a)
     (wsl-set! //A /tmp-var1 /j))
    (cond
     ((<= /swap 1)
      (set! /k (* 10 /k))))
    (cond
     ((equal? /same (quotient (+ (- /b /a) 1) 2))
      ; All elements are the same: so the array is sorted 
     )
     ((> (- /j /a) (- /b (+ /j 1)))
      (set! //A (@Quick_sort_rec4  /k (+ /j 1) /b //A))
      (set! //A (@Quick_sort_rec4  /k /a /j //A)))
     (#t
      (set! //A (@Quick_sort_rec4  /k /a /j //A))
      (set! //A (@Quick_sort_rec4  /k (+ /j 1) /b //A))))))
  (set! /i /i-save)
  (set! /j /j-save)
  (set! /v /v-save))
 //A)

(define (@Quick_sort5 //N-par /k-par //A-par)
 (let ((//A-save //A)
       (/k-save /k)
       (//N-save //N)
       (funct-result '()))
  (set! //A //A-par)
  (set! /k /k-par)
  (set! //N //N-par)
  (set! //A (@Quick_sort_rec5  /k 1 //N //A))
  (set! funct-result //A)
  (set! //A //A-save)
  (set! /k /k-save)
  (set! //N //N-save)
  funct-result))

; Recursive Quicksort: 
(define (@Quick_sort_rec5 /k /a /b //A)
 (let ((/i-save /i)
       (/j-save /j)
       (/v-save /v)
       (/swap 0)
       (/same 1))
  (set! /i 0)
  (set! /j 0)
  (set! /v 0)
  (cond
   ((<= (+ (- /b /a) 1) /k)
    ; Insertion sort 
    (for /i (+ /a 1) /b 1 
     (begin
      (set! /v (wsl-ref //A /i))
      (set! /j /i)
      (while (and (> /j /a) (> (wsl-ref //A (- /j 1)) /v)) 
       (begin
        (wsl-set! //A (wsl-ref //A (- /j 1)) /j)
        (set! /j (- /j 1))))
      (wsl-set! //A /v /j))))
   (#t
    ; Median of three partitioning: 
    (set! /j (quotient (+ /a /b) 2))
    (cond
     ((or (and (< (wsl-ref //A /a) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /b))) (and (< (wsl-ref //A /b) (wsl-ref //A /j)) (< (wsl-ref //A /j) (wsl-ref //A /a))))
      (let ((/tmp-var2 (wsl-ref //A /a))
            (/tmp-var1 (wsl-ref //A /j)))
       (wsl-set! //A /tmp-var2 /j)
       (wsl-set! //A /tmp-var1 /a)))
     ((or (and (< (wsl-ref //A /a) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /j))) (and (< (wsl-ref //A /j) (wsl-ref //A /b)) (< (wsl-ref //A /b) (wsl-ref //A /a))))
      (let ((/tmp-var2 (wsl-ref //A /a))
            (/tmp-var1 (wsl-ref //A /b)))
       (wsl-set! //A /tmp-var2 /b)
       (wsl-set! //A /tmp-var1 /a))))
    (set! /i (- /a 1))
    (set! /j (+ /b 1))
    (set! /v (wsl-ref //A /a))
    (set! /fl_flag2 0)
    (while (= /fl_flag2 0) 
     (begin
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (set! /i (+ /i 1))
        (cond
         ((>= (wsl-ref //A /i) /v)
          (set! /fl_flag1 1))
         (#t
          (set! /same 0)
          (set! /i (+ /i 1))
          (while (< (wsl-ref //A /i) /v) 
           (set! /i (+ /i 1)))
          (cond
           ((>= (wsl-ref //A /i) /v)
            (set! /fl_flag1 1))
           (#t
            (set! /fl_flag1 0)))))))
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (set! /j (- /j 1))
        (cond
         ((<= (wsl-ref //A /j) /v)
          (set! /fl_flag1 1))
         (#t
          (set! /same 0)
          (set! /j (- /j 1))
          (while (< /v (wsl-ref //A /j)) 
           (set! /j (- /j 1)))
          (cond
           ((<= (wsl-ref //A /j) /v)
            (set! /fl_flag1 1))
           (#t
            (set! /fl_flag1 0)))))))
      (cond
       ((>= /i /j)
        (set! /fl_flag2 1))
       (#t
        (cond
         ((not (equal? (wsl-ref //A /i) (wsl-ref //A /j)))
          (set! /swap (+ /swap 1))
          (set! /same 0)
          (let ((/tmp-var2 (wsl-ref //A /j))
                (/tmp-var1 (wsl-ref //A /i)))
           (wsl-set! //A /tmp-var2 /i)
           (wsl-set! //A /tmp-var1 /j))))
        (cond
         ((> /swap 1)
          ; No point in checking further 
          (set! /i (+ /i 1))
          (while (< (wsl-ref //A /i) /v) 
           (set! /i (+ /i 1)))
          (set! /j (- /j 1))
          (while (< /v (wsl-ref //A /j)) 
           (set! /j (- /j 1)))
          (while (< /i /j) 
           (begin
            (let ((/tmp-var2 (wsl-ref //A /j))
                  (/tmp-var1 (wsl-ref //A /i)))
             (wsl-set! //A /tmp-var2 /i)
             (wsl-set! //A /tmp-var1 /j))
            (set! /i (+ /i 1))
            (while (< (wsl-ref //A /i) /v) 
             (set! /i (+ /i 1)))
            (set! /j (- /j 1))
            (while (< /v (wsl-ref //A /j)) 
             (set! /j (- /j 1)))))
          (set! /fl_flag2 1))
         (#t
          (set! /fl_flag2 0)))))))
    (let ((/tmp-var2 (wsl-ref //A /j))
          (/tmp-var1 (wsl-ref //A /a)))
     (wsl-set! //A /tmp-var2 /a)
     (wsl-set! //A /tmp-var1 /j))
    (cond
     ((<= /swap 1)
      (set! /k (* 10 /k))))
    (cond
     ((= /same 1)
      ; All elements are the same: so the array is sorted 
     )
     ((> (- /j /a) (- /b (+ /j 1)))
      (set! //A (@Quick_sort_rec5  /k (+ /j 1) /b //A))
      (set! //A (@Quick_sort_rec5  /k /a /j //A)))
     (#t
      (set! //A (@Quick_sort_rec5  /k /a /j //A))
      (set! //A (@Quick_sort_rec5  /k (+ /j 1) /b //A))))))
  (set! /i /i-save)
  (set! /j /j-save)
  (set! /v /v-save))
 //A)

(define (@Shell_sort //N /k //A)
 (let ((/gap 1)
       (/i 0)
       (/j 0)
       (/v 0)
       (/index 0))
  (while (<= /gap //N) 
   (set! /gap (+ (* 3 /gap) 1)))
  (set! /gap (quotient (- /gap 1) 3))
  (set! /i (+ /gap 1))
  (while (<= /i //N) 
   (begin
    (set! /v (wsl-ref //A /i))
    (set! /j /i)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (set! /index (- /j /gap))
      (cond
       ((not (> (wsl-ref //A /index) /v))
        (set! /fl_flag1 1))
       (#t
        (wsl-set! //A (wsl-ref //A /index) /j)
        (set! /j (- /j /gap))
        (cond
         ((<= /j /gap)
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0)))))))
    (wsl-set! //A /v /j)
    (set! /i (+ /i 1))))
  (while (not (= /gap 1)) 
   (begin
    (set! /gap (quotient (- /gap 1) 3))
    (set! /i (+ /gap 1))
    (while (<= /i //N) 
     (begin
      (set! /v (wsl-ref //A /i))
      (set! /j /i)
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (set! /index (- /j /gap))
        (cond
         ((not (> (wsl-ref //A /index) /v))
          (set! /fl_flag1 1))
         (#t
          (wsl-set! //A (wsl-ref //A /index) /j)
          (set! /j (- /j /gap))
          (cond
           ((<= /j /gap)
            (set! /fl_flag1 1))
           (#t
            (set! /fl_flag1 0)))))))
      (wsl-set! //A /v /j)
      (set! /i (+ /i 1)))))))
 //A)

(define (@Insertion_sort //N /k //A)
 (for /i 2 //N 1 
  (begin
   (set! /v (wsl-ref //A /i))
   (set! /j /i)
   (while (and (> /j 1) (> (wsl-ref //A (- /j 1)) /v)) 
    (begin
     (wsl-set! //A (wsl-ref //A (- /j 1)) /j)
     (set! /j (- /j 1))))
   (wsl-set! //A /v /j)))
 //A)

(define (@Bubble_sort1 //N /k //A)
 (for /i //N 1 (- 1) 
  (for /j 2 /i 1 
   (cond
    ((> (wsl-ref //A (- /j 1)) (wsl-ref //A /j))
     (let ((/tmp-var2 (wsl-ref //A /j))
           (/tmp-var1 (wsl-ref //A (- /j 1))))
      (wsl-set! //A /tmp-var2 (- /j 1))
      (wsl-set! //A /tmp-var1 /j))))))
 //A)

(define (@Bubble_sort //N /k //A)
 (let ((/swap 0))
  (set! /swap 0)
  (for /j 2 //N 1 
   (cond
    ((> (wsl-ref //A (- /j 1)) (wsl-ref //A /j))
     (let ((/tmp-var2 (wsl-ref //A /j))
           (/tmp-var1 (wsl-ref //A (- /j 1))))
      (wsl-set! //A /tmp-var2 (- /j 1))
      (wsl-set! //A /tmp-var1 /j))
     (set! /swap 1))))
  (while (not (= /swap 0)) 
   (begin
    (set! /swap 0)
    (for /j 2 //N 1 
     (cond
      ((> (wsl-ref //A (- /j 1)) (wsl-ref //A /j))
       (let ((/tmp-var2 (wsl-ref //A /j))
             (/tmp-var1 (wsl-ref //A (- /j 1))))
        (wsl-set! //A /tmp-var2 (- /j 1))
        (wsl-set! //A /tmp-var1 /j))
       (set! /swap 1)))))))
 //A)

; Dual-Pivot Quicksort 
(define (@DPQ_sort //N-par /k-par //A-par)
 (let ((//A-save //A)
       (/k-save /k)
       (//N-save //N)
       (funct-result '()))
  (set! //A //A-par)
  (set! /k /k-par)
  (set! //N //N-par)
  (cond
   ((<= //N 1000)
    (display-list "Start  = " //A)))
  (set! //A (@DPQ_sort_iter  /k 1 //N //A))
  (cond
   ((<= //N 1000)
    (display-list "Result = " //A)))
  (cond
   ((> /k 1)
    (display-list "Final insertion sort... " (@Runtime))
    (set! //A (@Insertion_sort  //N /k //A))))
  (set! funct-result //A)
  (set! //A //A-save)
  (set! /k /k-save)
  (set! //N //N-save)
  funct-result))

(define (@DPQ2_sort //N-par /k-par //A-par)
 (let ((//A-save //A)
       (/k-save /k)
       (//N-save //N)
       (funct-result '()))
  (set! //A //A-par)
  (set! /k /k-par)
  (set! //N //N-par)
  (cond
   ((<= //N 1000)
    (display-list "Start  = " //A)))
  (set! //A (@DPQ_sort_test  /k 1 //N //A))
  (cond
   ((<= //N 1000)
    (display-list "Result = " //A)))
  (cond
   ((> /k 1)
    (display-list "Final insertion sort... " (@Runtime))
    (set! //A (@Insertion_sort  //N /k //A))))
  (set! funct-result //A)
  (set! //A //A-save)
  (set! /k /k-save)
  (set! //N //N-save)
  funct-result))

(define (@DPQ_sort_test /k-par /i-par /j-par //A-par)
 (let ((//A-save //A)
       (/j-save /j)
       (/i-save /i)
       (/k-save /k)
       (funct-result '()))
  (set! //A //A-par)
  (set! /j /j-par)
  (set! /i /i-par)
  (set! /k /k-par)
  (let ((//P1 0)
        (//P2 0)
        (//L 0)
        (//K 0)
        (//G 0)
        (/e2 0)
        (/e4 0)
        (//A2 0)
        (//A4 0)
        (/e1 0)
        (/e3 0)
        (/e5 0)
        (//A1 0)
        (//A3 0)
        (//A5 0)
        (/sixth 0)
        (/swap 0))
   (cond
    ((> (+ (- /j /i) 1) /k)
     ; Pick two pivot elements. 
     ; NB these five indices must be distinct! 
     (set! /sixth (quotient (+ (- /j /i) 1) 6))
     (set! /e1 (+ /i /sixth))
     (set! /e3 (quotient (+ /i /j) 2))
     (set! /e2 (- /e3 /sixth))
     (set! /e4 (+ /e3 /sixth))
     (set! /e5 (- /j /sixth))
     (set! //A1 (wsl-ref //A /e1))
     (set! //A2 (wsl-ref //A /e2))
     (set! //A3 (wsl-ref //A /e3))
     (set! //A4 (wsl-ref //A /e4))
     (set! //A5 (wsl-ref //A /e5))
     (cond
      ((> //A1 //A2)
       (let ((/tmp-var2 //A2)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A2 /tmp-var1))))
     (cond
      ((> //A4 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A4))
        (set! //A4 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (cond
      ((> //A1 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A2 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A1 //A4)
       (let ((/tmp-var2 //A4)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A4 /tmp-var1))))
     (cond
      ((> //A3 //A4)
       (let ((/tmp-var2 //A4)
             (/tmp-var1 //A3))
        (set! //A3 /tmp-var2)
        (set! //A4 /tmp-var1))))
     (cond
      ((> //A2 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (cond
      ((> //A2 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A4 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A4))
        (set! //A4 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (wsl-set! //A //A1 /e1)
     (wsl-set! //A //A3 /e3)
     (wsl-set! //A //A5 /e5)
     ; Swap the pivot values to the ends: 
     (set! //P1 //A2)
     (wsl-set! //A (wsl-ref //A /i) /e2)
     (set! //P2 //A4)
     (wsl-set! //A (wsl-ref //A /j) /e4)
     (set! //L (+ /i 1))
     (set! //G (- /j 1))
     ; Partitioning algorithm (my version): 
     (set! //K //L)
     (while (<= //K //G) 
      (begin
       (set! //A/K (wsl-ref //A //K))
       (cond
        ((< //A/K //P1)
         (wsl-set! //A (wsl-ref //A //L) //K)
         (wsl-set! //A //A/K //L)
         (set! //L (+ //L 1))
         (set! //K (+ //K 1)))
        ((> //A/K //P2)
         (wsl-set! //A (wsl-ref //A //G) //K)
         (wsl-set! //A //A/K //G)
         (set! //G (- //G 1)))
        (#t
         (set! //K (+ //K 1))))))
     (wsl-set! //A //P1 /i)
     (wsl-set! //A //P2 /j)
     ; Swap the pivot values back to the right places: 
     (wsl-set! //A (wsl-ref //A (- //L 1)) /i)
     (wsl-set! //A //P1 (- //L 1))
     (wsl-set! //A (wsl-ref //A (+ //G 1)) /j)
     (wsl-set! //A //P2 (+ //G 1))
     ; sort the sub-arrays 
     (cond
      ((<= //N 1000)
       (display-list "pivots = A[" /e2 "] = " //P1 " A[" /e4 "] = " //P2)
       (display-list "partition = " /i " -- " (- //L 2) " -- " (+ //G 2) " -- " /j)
       (let ((/i-save /i)
             (/part '()))
        (set! /i (+ //G 2))
        (set! /sixth (quotient (+ (- /j /i) 1) 6))
        (set! /e1 (+ /i /sixth))
        (set! /e3 (quotient (+ /i /j) 2))
        (set! /e2 (- /e3 /sixth))
        (set! /e4 (+ /e3 /sixth))
        (set! /e5 (- /j /sixth))
        (set! /part (list (list /i /e1) (list /e1 /e2) (list /e2 /e3) (list /e3 /e4) (list /e4 /e5) (list /e5 /j)))
        (for-in /pair /part 
         (@Print_Ar "== " //A (wsl-ref /pair 1) (wsl-ref /pair 2)))
        (set! /i /i-save))))
     (set! //A (@DPQ_sort_test  /k (+ //G 2) /j //A))
     (cond
      ((not (equal? //P1 //P2))
       (set! //A (@DPQ_sort_test  /k //L //G //A))))
     (set! //A (@DPQ_sort_test  /k /i (- //L 2) //A))
     #t)))
  (set! funct-result //A)
  (set! //A //A-save)
  (set! /j /j-save)
  (set! /i /i-save)
  (set! /k /k-save)
  funct-result))

(define (@DPQ_sort_iter /k /i /j //A)
 (let ((//A/S '())
       (//P1 0)
       (//P2 0)
       (//L 0)
       (//K 0)
       (//G 0)
       (/e2 0)
       (/e4 0)
       (//A2 0)
       (//A4 0)
       (/e1 0)
       (/e3 0)
       (/e5 0)
       (//A1 0)
       (//A3 0)
       (//A5 0)
       (/sixth 0))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((> (+ (- /j /i) 1) /k)
     ; Pick two pivot elements 
     (set! /sixth (quotient (+ (- /j /i) 1) 6))
     (set! /e1 (+ /i /sixth))
     (set! /e3 (quotient (+ /i /j) 2))
     (set! /e2 (- /e3 /sixth))
     (set! /e4 (+ /e3 /sixth))
     (set! /e5 (- /j /sixth))
     (set! //A1 (wsl-ref //A /e1))
     (set! //A2 (wsl-ref //A /e2))
     (set! //A3 (wsl-ref //A /e3))
     (set! //A4 (wsl-ref //A /e4))
     (set! //A5 (wsl-ref //A /e5))
     (cond
      ((> //A1 //A2)
       (let ((/tmp-var2 //A2)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A2 /tmp-var1))))
     (cond
      ((> //A4 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A4))
        (set! //A4 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (cond
      ((> //A1 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A2 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A1 //A4)
       (let ((/tmp-var2 //A4)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A4 /tmp-var1))))
     (cond
      ((> //A3 //A4)
       (let ((/tmp-var2 //A4)
             (/tmp-var1 //A3))
        (set! //A3 /tmp-var2)
        (set! //A4 /tmp-var1))))
     (cond
      ((> //A2 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (cond
      ((> //A2 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A4 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A4))
        (set! //A4 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (wsl-set! //A //A1 /e1)
     (wsl-set! //A //A3 /e3)
     (wsl-set! //A //A5 /e5)
     ; Swap the pivot values to the ends: 
     (set! //P1 //A2)
     (wsl-set! //A (wsl-ref //A /i) /e2)
     (set! //P2 //A4)
     (wsl-set! //A (wsl-ref //A /j) /e4)
     (set! //L (+ /i 1))
     (set! //G (- /j 1))
     ; Partitioning algorithm (my version): 
     (set! //K //L)
     (while (<= //K //G) 
      (begin
       (set! //A/K (wsl-ref //A //K))
       (cond
        ((< //A/K //P1)
         (wsl-set! //A (wsl-ref //A //L) //K)
         (wsl-set! //A //A/K //L)
         (set! //L (+ //L 1))
         (set! //K (+ //K 1)))
        ((> //A/K //P2)
         (wsl-set! //A (wsl-ref //A //G) //K)
         (wsl-set! //A //A/K //G)
         (set! //G (- //G 1)))
        (#t
         (set! //K (+ //K 1))))))
     ; Swap the pivot values back to the right places: 
     (wsl-set! //A (wsl-ref //A (- //L 1)) /i)
     (wsl-set! //A //P1 (- //L 1))
     (wsl-set! //A (wsl-ref //A (+ //G 1)) /j)
     (wsl-set! //A //P2 (+ //G 1))
     ; sort the sub-arrays 
     (push //A/S /i)
     (push //A/S (- //L 2))
     (cond
      ((not (equal? //P1 //P2))
       (push //A/S //L)
       (push //A/S //G)))
     (set! /i (+ //G 2))
     (set! /fl_flag1 0))
    ((equal? //A/S '())
     (set! /fl_flag1 1))
    (#t
     (set! /j (car //A/S))
     (set! //A/S (cdr //A/S))
     (set! /i (car //A/S))
     (set! //A/S (cdr //A/S))
     (set! /fl_flag1 0)))))
 //A)

(define (@DPQ_sort_opt /k /i /j //A)
 (let ((//A/S '())
       (//P1 0)
       (//P2 0)
       (//L 0)
       (//K 0)
       (//G 0)
       (/e1 0)
       (/e2 0)
       (/e3 0)
       (/e4 0)
       (/e5 0)
       (//A1 0)
       (//A2 0)
       (//A3 0)
       (//A4 0)
       (//A5 0)
       (//A/K 0)
       (/sixth 0))
  (set! /fl_flag3 0)
  (while (= /fl_flag3 0) 
   (cond
    ((> (+ (- /j /i) 1) /k)
     ; Pick two pivot elements 
     (set! /sixth (quotient (+ (- /j /i) 1) 6))
     (set! /e1 (+ /i /sixth))
     (set! /e3 (quotient (+ /i /j) 2))
     (set! /e2 (- /e3 /sixth))
     (set! /e4 (+ /e3 /sixth))
     (set! /e5 (- /j /sixth))
     (set! //A1 (wsl-ref //A /e1))
     (set! //A2 (wsl-ref //A /e2))
     (set! //A3 (wsl-ref //A /e3))
     (set! //A4 (wsl-ref //A /e4))
     (set! //A5 (wsl-ref //A /e5))
     (cond
      ((> //A1 //A2)
       (let ((/tmp-var2 //A2)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A2 /tmp-var1))))
     (cond
      ((> //A4 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A4))
        (set! //A4 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (cond
      ((> //A1 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A2 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A1 //A4)
       (let ((/tmp-var2 //A4)
             (/tmp-var1 //A1))
        (set! //A1 /tmp-var2)
        (set! //A4 /tmp-var1))))
     (cond
      ((> //A3 //A4)
       (let ((/tmp-var2 //A4)
             (/tmp-var1 //A3))
        (set! //A3 /tmp-var2)
        (set! //A4 /tmp-var1))))
     (cond
      ((> //A2 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (cond
      ((> //A2 //A3)
       (let ((/tmp-var2 //A3)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A3 /tmp-var1))))
     (cond
      ((> //A4 //A5)
       (let ((/tmp-var2 //A5)
             (/tmp-var1 //A4))
        (set! //A4 /tmp-var2)
        (set! //A5 /tmp-var1))))
     (wsl-set! //A //A1 /e1)
     (wsl-set! //A //A3 /e3)
     (wsl-set! //A //A5 /e5)
     ; Swap the pivot values to the ends: 
     (set! //P1 //A2)
     (wsl-set! //A (wsl-ref //A /i) /e2)
     (set! //P2 //A4)
     (wsl-set! //A (wsl-ref //A /j) /e4)
     (set! //L (+ /i 1))
     (set! //G (- /j 1))
     ; Partitioning algorithm (optimised version): 
     (set! //K //L)
     (set! /fl_flag2 0)
     (while (= /fl_flag2 0) 
      (begin
       (cond
        ((> //K //G)
         (set! /fl_flag2 1))
        (#t
         (set! /fl_flag2 0)))
       (cond
        ((= /fl_flag2 0)
         (set! //A/K (wsl-ref //A //K))
         (cond
          ((< //A/K //P1)
           ; Move A[K] to left part 
           (cond
            ((not (equal? //K //L))
             (wsl-set! //A (wsl-ref //A //L) //K)
             (wsl-set! //A //A/K //L)))
           (set! //L (+ //L 1))
           (set! /fl_flag2 0))
          ((> //A/K //P2)
           (set! /fl_flag1 0)
           (while (= /fl_flag1 0) 
            (cond
             ((<= (wsl-ref //A //G) //P2)
              (set! /fl_flag1 1))
             (#t
              (set! //G (- //G 1))
              (cond
               ((= (+ //G 1) //K)
                (set! /fl_flag1 2))
               (#t
                (set! /fl_flag1 0))))))
           (cond
            ((= /fl_flag1 2)
             (set! /fl_flag2 1))
            ((< (wsl-ref //A //G) //P1)
             (wsl-set! //A (wsl-ref //A //L) //K)
             (wsl-set! //A (wsl-ref //A //G) //L)
             (set! //L (+ //L 1))
             (wsl-set! //A //A/K //G)
             (set! //G (- //G 1))
             (set! /fl_flag2 0))
            (#t
             (wsl-set! //A (wsl-ref //A //G) //K)
             (wsl-set! //A //A/K //G)
             (set! //G (- //G 1))
             (set! /fl_flag2 0))))
          (#t
           (set! /fl_flag2 0)))
         (cond
          ((= /fl_flag2 0)
           (set! //K (+ //K 1))
           (set! /fl_flag2 0)))))))
     ; Swap the pivot values back to the right places: 
     (wsl-set! //A (wsl-ref //A (- //L 1)) /i)
     (wsl-set! //A //P1 (- //L 1))
     (wsl-set! //A (wsl-ref //A (+ //G 1)) /j)
     (wsl-set! //A //P2 (+ //G 1))
     ; sort the sub-arrays 
     (push //A/S /i)
     (push //A/S (- //L 2))
     (cond
      ((not (equal? //P1 //P2))
       (push //A/S //L)
       (push //A/S //G)))
     (set! /i (+ //G 2))
     (set! /fl_flag3 0))
    ((equal? //A/S '())
     (set! /fl_flag3 1))
    (#t
     (set! /j (car //A/S))
     (set! //A/S (cdr //A/S))
     (set! /i (car //A/S))
     (set! //A/S (cdr //A/S))
     (set! /fl_flag3 0)))))
 //A)

(define (@DPQ_sort_rec /k /i /j //A)
 (while (> (+ (- /j /i) 1) /k) 
  (let ((//P1 0)
        (//P2 0)
        (//L 0)
        (//K 0)
        (//G 0)
        (/sixth (quotient (+ (- /j /i) 1) 6))
        (/e3 (quotient (+ /i /j) 2)))
   ; Pick two pivot elements 
   (let ((/e2 /i)
         (/e4 /j))
    (let ((//A2 (wsl-ref //A /e2))
          (//A4 (wsl-ref //A /e4)))
     (cond
      ((> //A2 //A4)
       (let ((/tmp-var2 //A4)
             (/tmp-var1 //A2))
        (set! //A2 /tmp-var2)
        (set! //A4 /tmp-var1))))
     ; Swap the pivot values to the ends: 
     (set! //P1 //A2)
     (wsl-set! //A (wsl-ref //A /i) /e2)
     (set! //P2 //A4)
     (wsl-set! //A (wsl-ref //A /j) /e4)
     (set! //L (+ /i 1))
     (set! //G (- /j 1))
     ; Partitioning algorithm (my version): 
     (set! //K //L)
     (while (<= //K //G) 
      (cond
       ((< (wsl-ref //A //K) //P1)
        (let ((/tmp-var2 (wsl-ref //A //K))
              (/tmp-var1 (wsl-ref //A //L)))
         (wsl-set! //A /tmp-var2 //L)
         (wsl-set! //A /tmp-var1 //K))
        (set! //L (+ //L 1))
        (set! //K (+ //K 1)))
       ((> (wsl-ref //A //K) //P2)
        (let ((/tmp-var2 (wsl-ref //A //G))
              (/tmp-var1 (wsl-ref //A //K)))
         (wsl-set! //A /tmp-var2 //K)
         (wsl-set! //A /tmp-var1 //G))
        (set! //G (- //G 1)))
       (#t
        (set! //K (+ //K 1)))))
     ; Swap the pivot values back to the right places: 
     (wsl-set! //A (wsl-ref //A (- //L 1)) /i)
     (wsl-set! //A //P1 (- //L 1))
     (wsl-set! //A (wsl-ref //A (+ //G 1)) /j)
     (wsl-set! //A //P2 (+ //G 1))
     ; Recursively sort the sub-arrays, excluding the pivot values: 
     (set! //A (@DPQ_sort_rec  /k /i (- //L 2) //A))
     (cond
      ((not (equal? //P1 //P2))
       (set! //A (@DPQ_sort_rec  /k //L //G //A))))
     (set! /i (+ //G 2))))))
 //A)

(define (@Print_Ar /str //A /i /j)
 (display-list-flush /str)
 (for /k /i /j 1 
  (display-list-flush (wsl-ref //A /k) " "))
 (display-list ""))

(define (@DPQ_sort_rec_dumb_pivot /k /i /j //A)
 (cond
  ((> (+ (- /j /i) 1) /k)
   (let ((//P1 0)
         (//P2 0)
         (//L 0)
         (//K 0)
         (//G 0)
         (/sixth (quotient (+ (- /j /i) 1) 6))
         (/e3 (quotient (+ /i /j) 2)))
    ; Pick two pivot elements 
    (let ((/e2 /i)
          (/e4 /j))
     (let ((//A2 (wsl-ref //A /e2))
           (//A4 (wsl-ref //A /e4)))
      (cond
       ((> //A2 //A4)
        (let ((/tmp-var2 //A4)
              (/tmp-var1 //A2))
         (set! //A2 /tmp-var2)
         (set! //A4 /tmp-var1))))
      ; Swap the pivot values to the ends: 
      (set! //P1 //A2)
      (wsl-set! //A (wsl-ref //A /i) /e2)
      (set! //P2 //A4)
      (wsl-set! //A (wsl-ref //A /j) /e4)
      (set! //L (+ /i 1))
      (set! //G (- /j 1))
      ; Partitioning algorithm (my version): 
      (set! //K //L)
      (while (<= //K //G) 
       (cond
        ((< (wsl-ref //A //K) //P1)
         (let ((/tmp-var2 (wsl-ref //A //K))
               (/tmp-var1 (wsl-ref //A //L)))
          (wsl-set! //A /tmp-var2 //L)
          (wsl-set! //A /tmp-var1 //K))
         (set! //L (+ //L 1))
         (set! //K (+ //K 1)))
        ((> (wsl-ref //A //K) //P2)
         (let ((/tmp-var2 (wsl-ref //A //G))
               (/tmp-var1 (wsl-ref //A //K)))
          (wsl-set! //A /tmp-var2 //K)
          (wsl-set! //A /tmp-var1 //G))
         (set! //G (- //G 1)))
        (#t
         (set! //K (+ //K 1)))))
      ; Swap the pivot values back to the right places: 
      (wsl-set! //A (wsl-ref //A (- //L 1)) /i)
      (wsl-set! //A //P1 (- //L 1))
      (wsl-set! //A (wsl-ref //A (+ //G 1)) /j)
      (wsl-set! //A //P2 (+ //G 1))
      ; Recursively sort the sub-arrays, excluding the pivot values: 
      (set! //A (@DPQ_sort_rec_dumb_pivot  /k /i (- //L 2) //A))
      (cond
       ((not (equal? //P1 //P2))
        (set! //A (@DPQ_sort_rec_dumb_pivot  /k //L //G //A))))
      (set! //A (@DPQ_sort_rec_dumb_pivot  /k (+ //G 2) /j //A)))))))
 //A)

(define (@DPQ_sort_rec_mine /k /i /j //A)
 (cond
  ((> (+ (- /j /i) 1) /k)
   (let ((//P1 0)
         (//P2 0)
         (//L 0)
         (//K 0)
         (//G 0)
         (/sixth (quotient (+ (- /j /i) 1) 6))
         (/e3 (quotient (+ /i /j) 2)))
    ; Pick two pivot elements 
    (let ((/e1 (+ /i /sixth))
          (/e2 (- /e3 /sixth))
          (/e4 (+ /e3 /sixth))
          (/e5 (- /j /sixth)))
     (let ((//A1 (wsl-ref //A /e1))
           (//A2 (wsl-ref //A /e2))
           (//A3 (wsl-ref //A /e3))
           (//A4 (wsl-ref //A /e4))
           (//A5 (wsl-ref //A /e5)))
      (cond
       ((> //A1 //A2)
        (let ((/tmp-var2 //A2)
              (/tmp-var1 //A1))
         (set! //A1 /tmp-var2)
         (set! //A2 /tmp-var1))))
      (cond
       ((> //A4 //A5)
        (let ((/tmp-var2 //A5)
              (/tmp-var1 //A4))
         (set! //A4 /tmp-var2)
         (set! //A5 /tmp-var1))))
      (cond
       ((> //A1 //A3)
        (let ((/tmp-var2 //A3)
              (/tmp-var1 //A1))
         (set! //A1 /tmp-var2)
         (set! //A3 /tmp-var1))))
      (cond
       ((> //A2 //A3)
        (let ((/tmp-var2 //A3)
              (/tmp-var1 //A2))
         (set! //A2 /tmp-var2)
         (set! //A3 /tmp-var1))))
      (cond
       ((> //A1 //A4)
        (let ((/tmp-var2 //A4)
              (/tmp-var1 //A1))
         (set! //A1 /tmp-var2)
         (set! //A4 /tmp-var1))))
      (cond
       ((> //A3 //A4)
        (let ((/tmp-var2 //A4)
              (/tmp-var1 //A3))
         (set! //A3 /tmp-var2)
         (set! //A4 /tmp-var1))))
      (cond
       ((> //A2 //A5)
        (let ((/tmp-var2 //A5)
              (/tmp-var1 //A2))
         (set! //A2 /tmp-var2)
         (set! //A5 /tmp-var1))))
      (cond
       ((> //A2 //A3)
        (let ((/tmp-var2 //A3)
              (/tmp-var1 //A2))
         (set! //A2 /tmp-var2)
         (set! //A3 /tmp-var1))))
      (cond
       ((> //A4 //A5)
        (let ((/tmp-var2 //A5)
              (/tmp-var1 //A4))
         (set! //A4 /tmp-var2)
         (set! //A5 /tmp-var1))))
      (wsl-set! //A //A1 /e1)
      (wsl-set! //A //A3 /e3)
      (wsl-set! //A //A5 /e5)
      ; Swap the pivot values to the ends: 
      (set! //P1 //A2)
      (wsl-set! //A (wsl-ref //A /i) /e2)
      (set! //P2 //A4)
      (wsl-set! //A (wsl-ref //A /j) /e4)
      (set! //L (+ /i 1))
      (set! //G (- /j 1))
      ; Partitioning algorithm (my version): 
      (set! //K //L)
      (while (<= //K //G) 
       (cond
        ((< (wsl-ref //A //K) //P1)
         (let ((/tmp-var2 (wsl-ref //A //K))
               (/tmp-var1 (wsl-ref //A //L)))
          (wsl-set! //A /tmp-var2 //L)
          (wsl-set! //A /tmp-var1 //K))
         (set! //L (+ //L 1))
         (set! //K (+ //K 1)))
        ((> (wsl-ref //A //K) //P2)
         (let ((/tmp-var2 (wsl-ref //A //G))
               (/tmp-var1 (wsl-ref //A //K)))
          (wsl-set! //A /tmp-var2 //K)
          (wsl-set! //A /tmp-var1 //G))
         (set! //G (- //G 1)))
        (#t
         (set! //K (+ //K 1)))))
      ; Swap the pivot values back to the right places: 
      (wsl-set! //A (wsl-ref //A (- //L 1)) /i)
      (wsl-set! //A //P1 (- //L 1))
      (wsl-set! //A (wsl-ref //A (+ //G 1)) /j)
      (wsl-set! //A //P2 (+ //G 1))
      ; Recursively sort the sub-arrays, excluding the pivot values: 
      (set! //A (@DPQ_sort_rec_mine  /k /i (- //L 2) //A))
      (cond
       ((not (equal? //P1 //P2))
        (set! //A (@DPQ_sort_rec_mine  /k //L //G //A))))
      (set! //A (@DPQ_sort_rec_mine  /k (+ //G 2) /j //A)))))))
 //A)

(define (@Powerset /set)
 (let ((//R '())
       (/x '())
       (/s1 '())
       (/y '()))
  (cond
   ((null? /set)
    (set! //R (list '())))
   (#t
    (set! /x (car /set))
    (set! /s1 (@Powerset (cdr /set)))
    (for-in /y /s1 
     (set! //R (cons (cons /x /y) //R)))
    (set! //R (concat /s1 //R))))
  //R))

(define (@fib /n)
 
 (if (<= /n 1) 1 (+ (@fib (- /n 1)) (@fib (- /n 2)))))

; ----------------------------------------------------------------------- 

