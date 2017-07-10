;;; Scheme translation of WSL code
(define (/foreach-/S/S/A-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Down_Last)
   (@Down)
   ; to first defn 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((= (@ST (@I)) //T_/Proc)
       (@Down_To 4)
       (@Down_Last)
       (cond
        ((not (= (@ST (@I)) //T_/Skip))
         (@Paste_After (@Skip))))
       (@Up)
       (@Up)))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))))

(define (/foreach-/S/S/A-2 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/Variable) (= (@ST (@I)) //T_/Struct) (= (@ST (@I)) //T_/Aref))
   (set! /old (@Struct_Elts (@I)))
   (cond
    ((or (null? /only) (member /old /only))
     (set! /new (gethash /rename_exp /old))
     (cond
      ((not (null? /new))
       (@Paste_Over /new))))))))

(define (/foreach-/S/S/A-3 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/Var_/Lvalue) (= (@ST (@I)) //T_/Struct_/Lvalue) (= (@ST (@I)) //T_/Aref_/Lvalue))
   (set! /old (@Struct_Elts (@I)))
   (cond
    ((or (null? /only) (member /old /only))
     (set! /new (gethash /rename_lvalue /old))
     (cond
      ((not (null? /new))
       (cond
        ((= (@GT /new) //T_/Lvalue)
         (@Paste_Over /new))
        (#t
         (@Paste_Over (@Expn_To_Lvalue /new)))))))))))

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
; Convert a WSL program to Static Single Assignment form 
; given a file of information generated from bbtossa 
; NB: Statements of the form !P foo(VAR x) cause problems converted since 
; the *input* x name is different from the *output* x name. 
; Our solution is to ensure that x also appears in the value pars list: 
; !P foo(x VAR x). Then the renaming gives, eg !P foo(x__0 VAR x__1) 
; The file is a list of basic blocks in the form:
;
;Node: N posn:(...) len: L --> (succs list...)
;[node type]
;[NNNNNNNN var var:var ...]
;[...]
;0: <control vars...>
;  v1 := phi(v11, v12, ...)
;  ...
;  w1 := <w11, w12, ...>
;
;Blocks are separated by blank lines.
;The file ends with the line:
;
;Entry node is: N
;
; Note: we need to rename all the variables in the statements 
; in the block (the vars in the conditions for an IF block) 
; and also insert extra statements for the phi functions. 
; To insert the phi functions without disturbing position numbers, 
; we put them in the dbase table first and then insert from the tables 
; in a top-down fashion. 
; The first job is to parse the basic blocks file produced by bbtossa. 
; @Parse_Basic_Blocks returns a list of the form <entry, node, ...> 
; where a node is: <node, posn, len, succs, type, control, phi, assigns> 
; phi and assigns are lists of lists of strings. 
; NB We have to convert the strings to names before renaming etc. 
(set! //Alpha_/Num "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.[]")
(define (@Parse_Basic_Blocks /file)
 (let ((//R '())
       (/port (@Open_Input_File /file))
       (/entry 0)
       (/tmp-save /tmp)
       (/word-save /word)
       (/line-save /line)
       (/node 0)
       (/posn '())
       (/len 0)
       (/succs '())
       (/type "")
       (/control '())
       (/phi '())
       (/assigns '())
       (/list-save /list)
       (/p-save /p)
       (/n-save /n)
       (/v "")
       (/e "")
       (funct-result '()))
  (set! /tmp '())
  (set! /word '())
  (set! /line "")
  (set! /list '())
  (set! /p 0)
  (set! /n 0)
  (display-list "Reading basic blocks file: " /file)
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    ; Skip to the next node (if any) 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (set! /line (@Read_Line /port))
      (cond
       ((@EOF? /line)
        (set! /fl_flag1 2))
       (#t
        (cond
         ((@Starts_With? /line "Entry node is: ")
          (set! /entry (@String_To_Num (substr /line 15)))))
        (cond
         ((@Starts_With? /line "Node:")
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0)))))))
    (cond
     ((= /fl_flag1 2)
      (set! /fl_flag2 1))
     (#t
      ; Parse the line Node: n posn:(...) len: l --> (...) 
      (set! /p 5)
      (let ((/-result- (@Parse_Num  /line /p /node)))
       (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
       (set! /node (car /-result-)) (set! /-result- (cdr /-result-)))
      (set! /p (@Skip_Spaces  /line /p))
      (cond
       ((not (@Str_Match? /line /p "posn:("))
        (display-list "ERROR: no posn keyword:")
        (display-list /line)
        (set! /fl_flag2 1))
       (#t
        (set! /p (+ /p 6))
        ; Read a space separated list of numbers into posn: 
        (set! /posn '())
        (let ((/-result- (@Parse_Nums  /line /p /posn)))
         (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
         (set! /posn (car /-result-)) (set! /-result- (cdr /-result-)))
        (cond
         ((not (@Str_Match? /line /p ") len: "))
          (display-list "ERROR: no len keyword in bb file:")
          (display-list /line)
          (set! /fl_flag2 1))
         (#t
          (set! /p (+ /p 7))
          (let ((/-result- (@Parse_Num  /line /p /len)))
           (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
           (set! /len (car /-result-)) (set! /-result- (cdr /-result-)))
          (cond
           ((not (@Str_Match? /line /p " --> ("))
            (display-list "ERROR: no --> in bb file:")
            (display-list /line)
            (set! /fl_flag2 1))
           (#t
            ; We don't need the list of successors, but parse it for debugging: 
            (set! /p (+ /p 6))
            (set! /succs '())
            (let ((/-result- (@Parse_Nums  /line /p /succs)))
             (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
             (set! /succs (car /-result-)) (set! /-result- (cdr /-result-)))
            (set! /line (@Read_Line /port))
            (cond
             ((@EOF? /line)
              (display-list "ERROR: EOF reading type or control in bb file.")
              (set! /fl_flag2 1))
             (#t
              (set! /type "")
              (cond
               ((and (> (string-length /line) 0) (not (@Digit? (substr /line 0 1))))
                (set! /type /line)
                (set! /line (@Read_Line /port))))
              (cond
               ((@EOF? /line)
                (display-list "ERROR: EOF reading control or links in bb file.")
                (set! /fl_flag2 1))
               (#t
                (set! /links '())
                (while (and (not (@EOF? /line)) (> (string-length /line) 8) (@Digits? (substr /line 0 8))) 
                 (begin
                  (set! /links (cons /line /links))
                  (set! /line (@Read_Line /port))))
                (set! /links (reverse /links))
                (cond
                 ((@EOF? /line)
                  (display-list "ERROR: EOF reading control in bb file.")
                  (set! /fl_flag2 1))
                 (#t
                  (set! /p 0)
                  (cond
                   ((or (@EOF? /line) (and (not (@Str_Match? /line /p "0: <")) (not (@Str_Match? /line /p "1: <"))))
                    (display-list "ERROR1: badly formatted control line in bb file:")
                    (cond
                     ((not (@EOF? /line))
                      (display-list /line)))
                    (set! /fl_flag2 1))
                   (#t
                    (set! /p 4)
                    (set! /control '())
                    (let ((/-result- (@Parse_Words  /line /p /control)))
                     (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
                     (set! /control (car /-result-)) (set! /-result- (cdr /-result-)))
                    ; Read phi functions and assignments until a blank line is reached 
                    (set! /phi '())
                    (set! /assigns '())
                    (set! /fl_flag1 0)
                    (while (= /fl_flag1 0) 
                     (begin
                      (set! /line (@Read_Line /port))
                      (cond
                       ((@EOF? /line)
                        (set! /fl_flag1 1))
                       ((equal? /line "")
                        (set! /fl_flag1 1))
                       ((not (@Starts_With? /line "   "))
                        (display-list "ERROR2: badly formatted assign/phi line in bb file:")
                        (display-list /line)
                        (set! /fl_flag1 1))
                       (#t
                        (set! /p 3)
                        (let ((/-result- (@Parse_Word  /line /p /v)))
                         (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
                         (set! /v (car /-result-)) (set! /-result- (cdr /-result-)))
                        (set! /list (list (@Split_On /v ".")))
                        (cond
                         ((@Str_Match? /line /p " := phi(")
                          ; Read the phi function 
                          (set! /p (+ /p 8))
                          (let ((/-result- (@Parse_Words  /line /p /list)))
                           (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
                           (set! /list (car /-result-)) (set! /-result- (cdr /-result-)))
                          (set! /phi (cons /list /phi))
                          (set! /fl_flag1 0))
                         ((@Str_Match? /line /p " := <")
                          ; Read the assignment 
                          (set! /p (+ /p 5))
                          (let ((/-result- (@Parse_Words  /line /p /list)))
                           (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
                           (set! /list (car /-result-)) (set! /-result- (cdr /-result-)))
                          (set! /assigns (cons /list /assigns))
                          (set! /fl_flag1 0))
                         (#t
                          (display-list "ERROR3: badly formatted assign/phi line in bb file:")
                          (display-list (substr /line /p))
                          (display-list /line)
                          (set! /fl_flag1 1)))))))
                    (set! /phi (reverse /phi))
                    (set! /assigns (reverse /assigns))
                    (set! //R (cons (list /node /posn /len /succs /type /links /control /phi /assigns) //R))
                    (cond
                     ((@EOF? /line)
                      (set! /fl_flag2 1))
                     (#t
                      (set! /fl_flag2 0)))))))))))))))))))))
  (@Close_Input_Port /port)
  (set! funct-result (cons /entry (reverse //R)))
  (set! /tmp /tmp-save)
  (set! /word /word-save)
  (set! /line /line-save)
  (set! /list /list-save)
  (set! /p /p-save)
  (set! /n /n-save)
  funct-result))

; Read the word starting at position p1 in str 
(define (@Parse_Word /str /p1-par /word-par)
 (let ((/word-save /word)
       (/p1-save /p1)
       (funct-result '()))
  (set! /word /word-par)
  (set! /p1 /p1-par)
  (let ((/p2 0))
   (set! /p1 (@Skip_Spaces  /str /p1))
   (set! /p2 /p1)
   (while (@Char_In_Str? /str /p2 //Alpha_/Num) 
    (set! /p2 (+ /p2 1)))
   (set! /word (substr /str /p1 (- /p2 /p1)))
   (set! /p1 /p2))
  (set! funct-result (list /p1 /word))
  (set! /word /word-save)
  (set! /p1 /p1-save)
  funct-result))

; Read the number starting at position p1 in str: 
(define (@Parse_Num /str /p1-par /n-par)
 (let ((/n-save /n)
       (/p1-save /p1)
       (funct-result '()))
  (set! /n /n-par)
  (set! /p1 /p1-par)
  (let ((/p2 0))
   (set! /p1 (@Skip_Spaces  /str /p1))
   (set! /p2 /p1)
   (while (@Char_In_Str? /str /p2 "0123456789") 
    (set! /p2 (+ /p2 1)))
   (cond
    ((equal? /p2 /p1)
     (error (string-append "Number not found where expected: " /str))))
   (set! /n (@String_To_Num (substr /str /p1 (- /p2 /p1))))
   (set! /p1 /p2))
  (set! funct-result (list /p1 /n))
  (set! /n /n-save)
  (set! /p1 /p1-save)
  funct-result))

; Read a space separated list of numbers and append to list 
(define (@Parse_Nums /str /p-par /list-par)
 (let ((/list-save /list)
       (/p-save /p)
       (funct-result '()))
  (set! /list /list-par)
  (set! /p /p-par)
  (let ((/n-save /n)
        (//L '()))
   (set! /n 0)
   (while (and (> (string-length /str) /p) (not (@Char_In_Str? /str /p ")>"))) 
    (begin
     (let ((/-result- (@Parse_Num  /str /p /n)))
      (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /n (car /-result-)) (set! /-result- (cdr /-result-)))
     (set! //L (cons /n //L))
     (set! /p (@Skip_Spaces  /str /p))))
   (set! /list (concat /list (reverse //L)))
   (set! /n /n-save))
  (set! funct-result (list /p /list))
  (set! /list /list-save)
  (set! /p /p-save)
  funct-result))

(define (@Skip_Spaces /str /p)
 (let ((/l (string-length /str)))
  (while (and (< /p /l) (or (equal? (substr /str /p 1) " ") (equal? (substr /str /p 1) ","))) 
   (set! /p (+ /p 1))))
 /p)

; Check that the string at posn p matches the given string: 
(define (@Str_Match? /line /p /str)
 
 (and (>= (string-length /line) (+ /p (string-length /str))) (equal? (substr /line /p (string-length /str)) /str)))

; Split a string into a list of words with the given separator 
(define (@Split_On /str /sep)
 (let ((//R '())
       (/p-save /p)
       (/q (my-index /sep /str 0))
       (funct-result '()))
  (set! /p 0)
  (while (>= /q 0) 
   (begin
    (set! //R (cons (substr /str /p (- /q /p)) //R))
    (set! /p (+ /q 1))
    (set! /q (my-index /sep /str (+ /q 1)))))
  (cond
   ((< /p (string-length /str))
    (set! //R (cons (substr /str /p) //R))))
  (set! funct-result (reverse //R))
  (set! /p /p-save)
  funct-result))

; Parse a space/comma separated list of words and append to given list 
(define (@Parse_Words /line-par /p-par /list-par)
 (let ((/list-save /list)
       (/p-save /p)
       (/line-save /line)
       (funct-result '()))
  (set! /list /list-par)
  (set! /p /p-par)
  (set! /line /line-par)
  (let ((/word-save /word)
        (//L '()))
   (set! /word "")
   (while (and (> (string-length /line) /p) (not (@Char_In_Str? /line /p ")>"))) 
    (begin
     (let ((/-result- (@Parse_Word  /line /p /word)))
      (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /word (car /-result-)) (set! /-result- (cdr /-result-)))
     (cond
      ((equal? /word "")
       (display-list "ERROR: badly formatted word list in bb file:")
       (display-list /line)
       (set! /p (string-length /line)))
      (#t
       (set! //L (cons (@Split_On /word ".") //L))
       (set! /p (@Skip_Spaces  /line /p))))))
   (set! /list (concat /list (reverse //L)))
   (set! /word /word-save))
  (set! funct-result (list /p /list))
  (set! /list /list-save)
  (set! /p /p-save)
  (set! /line /line-save)
  funct-result))

; Print a list of elements, comma separated: 
(define (@Print_Elt_List /list-par)
 (let ((/list-save /list))
  (set! /list /list-par)
  (while (not (null? /list)) 
   (begin
    (@Print_Elt (car /list))
    (set! /list (cdr /list))
    (cond
     ((not (null? /list))
      (display-list-flush ", ")))))
  (set! /list /list-save)))

; Print a single element, dot separated: 
(define (@Print_Elt /elt)
 (while (not (null? /elt)) 
  (begin
   (display-list-flush (@String (car /elt)))
   (set! /elt (cdr /elt))
   (cond
    ((not (null? /elt))
     (display-list-flush "."))))))

(define (@Print_Basic_Blocks /blocks)
 (let ((/entry (car /blocks))
       (/block '())
       (/node 0)
       (/posn '())
       (/len 0)
       (/succs '())
       (/type "")
       (/links-save /links)
       (/control '())
       (/phi '())
       (/assigns '()))
  (set! /links '())
  (set! /blocks (cdr /blocks))
  (for-in /block /blocks 
   (begin
    (set! /node (wsl-ref /block 1))
    (set! /posn (wsl-ref /block 2))
    (set! /len (wsl-ref /block 3))
    (set! /succs (wsl-ref /block 4))
    (set! /type (wsl-ref /block 5))
    (set! /links (wsl-ref /block 6))
    (set! /control (wsl-ref /block 7))
    (set! /phi (wsl-ref /block 8))
    (set! /assigns (wsl-ref /block 9))
    (display-list-flush "Node: " /node " posn:" /posn " len: " /len " --> (")
    (set! /tmp /succs)
    (while (not (null? /tmp)) 
     (begin
      (display-list-flush (car /tmp))
      (set! /tmp (cdr /tmp))
      (cond
       ((not (null? /tmp))
        (display-list-flush " ")))))
    (display-list ")")
    (cond
     ((not (equal? /type ""))
      (display-list /type)))
    (for-in /line /links 
     (display-list /line))
    (display-list-flush "0: <")
    (@Print_Elt_List /control)
    (display-list ">")
    (for-in /list /phi 
     (begin
      (display-list-flush "   ")
      (@Print_Elt (car /list))
      (display-list-flush " := phi(")
      (@Print_Elt_List (cdr /list))
      (display-list ")")))
    (for-in /list /assigns 
     (begin
      (display-list-flush "   ")
      (@Print_Elt (car /list))
      (display-list-flush " := <")
      (@Print_Elt_List (cdr /list))
      (display-list ">")))
    (display-list "")))
  (display-list "Entry node is: " /entry)
  (set! /links /links-save)))

; Convert the given item to SSA form using the given blocks 
; At each statement which starts a block we have a table of renames 
; for lvalue variables (actually, elements) and another table 
; for expression variables. 
; If elts is non-empty, then only rename the given elements 
; (in this case, the result is only valid SSA form for these elements). 
(define (@WSL_To_SSA //I /blocks /only-par)
 (let ((/only-save /only)
       (//R '())
       (/entry (car /blocks))
       (/block '())
       (/a_name (@Make_Name "a"))
       (/rename_exp-save /rename_exp)
       (/rename_lvalue-save /rename_lvalue)
       (/assign-save /assign)
       (/elt '())
       (/len 0)
       (/elts '())
       (/v '())
       (/phi_tab (hash-table))
       (/count-save /count)
       (funct-result '()))
  (set! /only /only-par)
  (set! /rename_exp (hash-table))
  (set! /rename_lvalue (hash-table))
  (set! /assign '())
  (set! /count 0)
  (@Edit)
  (@New_Program //I)
  (set! /blocks (cdr /blocks))
  (for-in /block /blocks 
   (cond
    ((equal? (wsl-ref /block 5) "FLOOP Header")
     ; Floop header nodes don't have associated code to rename 
    )
    (#t
     (set! /rename_exp (hash-table))
     (set! /rename_lvalue (hash-table))
     ; Process control vars 
     (for-in /elt (wsl-ref /block 7) 
      (puthash /rename_exp (@SSA_Orig_Elt /elt) (@SSA_Convert_Elt /elt //T_/Expression)))
     ; Process phi functions and other assignments: 
     (for-in /assign (wsl-ref /block 9) 
      (begin
       (set! /elt (car /assign))
       (puthash /rename_lvalue (@SSA_Orig_Elt /elt) (@SSA_Convert_Elt /elt //T_/Lvalue))
       (for-in /elt (cdr /assign) 
        (puthash /rename_exp (@SSA_Orig_Elt /elt) (@SSA_Convert_Elt /elt //T_/Expression)))))
     (cond
      ((not (@Valid_Posn? (@Program) (wsl-ref /block 2)))
       (display-list (wsl-ref /block 2))
       (@Checkpoint "SSA_ERROR.wsl")
       (error "@WSL_To_SSA: not a valid position!")))
     (@Goto (wsl-ref /block 2))
     (set! /elts '())
     (set! /len (wsl-ref /block 3))
     (cond
      ((and (= /len 1) (equal? (wsl-ref /block 5) "IF"))
       ; Process the conditions only: 
       (@Down)
       ; to first Guarded 
       (@Down)
       ; to condition 
       (@SSA_Rename /rename_exp /rename_lvalue /only)
       (@Up)
       (while (@Right?) 
        (begin
         (@Right)
         (@Down)
         ; to condition 
         (@SSA_Rename /rename_exp /rename_lvalue /only)
         (@Up)))
       (@Up))
      ((and (= /len 1) (equal? (wsl-ref /block 5) "WHILE Header"))
       ; Process the condition only: 
       (@Down)
       ; to the condition 
       (@SSA_Rename /rename_exp /rename_lvalue /only)
       (@Up))
      ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "ACTION "))
       ; No renaming needed in the action header node 
      )
      ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC Return "))
       ; A return node has var := var assignments 
       ; Use the expressions to rename the var parameters: 
       (@SSA_Rename /rename_exp /rename_exp /only))
      ((@Starts_With? (wsl-ref /block 5) "PROC Header")
       ; Process parameters only 
       (@Down_To 2)
       (@SSA_Rename /rename_exp /rename_lvalue /only)
       (@Right)
       (@SSA_Rename /rename_exp /rename_lvalue /only)
       (@Up))
      ((equal? (wsl-ref /block 5) "WHERE Header")
       ; Nothing to do here 
      )
      (#t
       (set! /elts (@Elts_Assigned (@I)))
       (@SSA_Rename /rename_exp /rename_lvalue /only)))
     (while (not (= /len 1)) 
      (begin
       ; If a variable has been assigned in this statement 
       ; then subsequent expressions use the name from this assignment: 
       (for-in /v /elts 
        (cond
         ((not (null? (gethash /rename_lvalue /v)))
          (puthash /rename_exp /v (@Lvalue_To_Expn (gethash /rename_lvalue /v))))))
       (@Right)
       (set! /len (- /len 1))
       (cond
        ((and (= /len 1) (equal? (wsl-ref /block 5) "IF"))
         ; Process the conditions only: 
         (@Down)
         ; to first Guarded 
         (@Down)
         ; to condition 
         (@SSA_Rename /rename_exp /rename_lvalue /only)
         (@Up)
         (while (@Right?) 
          (begin
           (@Right)
           (@Down)
           ; to condition 
           (@SSA_Rename /rename_exp /rename_lvalue /only)
           (@Up)))
         (@Up))
        ((and (= /len 1) (equal? (wsl-ref /block 5) "WHILE Header"))
         ; Process the condition only: 
         (@Down)
         ; to the condition 
         (@SSA_Rename /rename_exp /rename_lvalue /only)
         (@Up))
        ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "ACTION "))
         ; No renaming needed in the action header node 
        )
        ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC Return "))
         ; A return node has var := var assignments 
         ; Use the expressions to rename the var parameters: 
         (@SSA_Rename /rename_exp /rename_exp /only))
        ((@Starts_With? (wsl-ref /block 5) "PROC Header")
         ; Process parameters only 
         (@Down_To 2)
         (@SSA_Rename /rename_exp /rename_lvalue /only)
         (@Right)
         (@SSA_Rename /rename_exp /rename_lvalue /only)
         (@Up))
        ((equal? (wsl-ref /block 5) "WHERE Header")
         ; Nothing to do here 
        )
        (#t
         (set! /elts (@Elts_Assigned (@I)))
         (@SSA_Rename /rename_exp /rename_lvalue /only))))))))
  ; Insert SKIP at the end of each proc body as a placeholder 
  ; for phi functions (this doesn't affect any posns) 
  (@Goto '())
  (@Foreach_Statement /foreach-/S/S/A-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Now insert phi functions in two passes. 
  ; The first pass puts the functions in a hash table indexed on 
  ; the position of the actual statement before which the function will appear. 
  ; The second pass inserts the phi functions via a top-down recursive 
  ; procedure which keeps track of the *original* position of the statements. 
  (for-in /block /blocks 
   (begin
    (@Goto (wsl-ref /block 2))
    (cond
     ((not (null? (wsl-ref /block 8)))
      (set! /count (+ /count (gen-length (wsl-ref /block 8))))
      (cond
       ((@Starts_With? (wsl-ref /block 5) "ACTION ")
        (@Down_Last)
        (@Down)))
      (cond
       ((@Starts_With? (wsl-ref /block 5) "PROC Return ")
        ; Insert phi functions at the *end* of the proc body 
        (@To 4)
        (@Down_Last)
        (cond
         ((not (= (@ST (@I)) //T_/Skip))
          (error "No SKIP at end of proc!!!")))))
      (cond
       ((not (= (@GT (@I)) //T_/Statement))
        (@Print_WSL (@I) "")
        (display-list "posn = " (@Posn))
        (error "Trying to insert phi functions on a non-statement!!!")))
      (puthash /phi_tab (reverse (@Posn)) (concat (gethash /phi_tab (reverse (@Posn))) (wsl-ref /block 8)))))))
  (@Goto '())
  (set! /count (@SSA_Phi_Insert  '() /phi_tab /only /count))
  (cond
   ((not (= /count 0))
    (error (string-append (@String /count) " phi function(s) inserted but not processed!"))))
  (@Trans //T/R_/Delete_/All_/Skips "")
  (set! //R (@Program))
  (@Undo_Edit)
  (set! funct-result //R)
  (set! /only /only-save)
  (set! /rename_exp /rename_exp-save)
  (set! /rename_lvalue /rename_lvalue-save)
  (set! /assign /assign-save)
  (set! /count /count-save)
  funct-result))

; Insert phi functions in a bottom-up fashion, keeping track of 
; the original (reversed) position in rposn: 
(define (@SSA_Phi_Insert /rposn /phi_tab /only-par /count-par)
 (let ((/count-save /count)
       (/only-save /only)
       (funct-result '()))
  (set! /count /count-par)
  (set! /only /only-par)
  (cond
   ((not (@Has_Statements_Type? (@GT (@I))))
    #t)
   ((@Cs? (@I))
    (let ((/phi '())
          (/n-save /n)
          (/up 0))
     (set! /n 1)
     (@Down)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       ; Convert in bottom-up order: 
       (set! /count (@SSA_Phi_Insert  (cons /n /rposn) /phi_tab /only /count))
       (set! /phi (gethash /phi_tab (cons /n /rposn)))
       (cond
        ((not (null? /phi))
         (cond
          ((= (@ST (@I)) //T_/Floop)
           (@Down)
           (@Down)
           (set! /up 2))
          ((= (@ST (@I)) //T_/While)
           ; Convert the WHILE to a FLOOP 
           ; and insert the phi functions at the top of the loop body. 
           ; (this is because they have to come BEFORE the test) 
           (@Trans //T/R_/While_/To_/Floop "")
           (@Down)
           (@Down)
           (set! /up 2))
          ((= (@ST (@I)) //T_/D_/Do)
           ; Convert the D_DO to a FLOOP plus D_IF 
           ; and insert at the top of the loop body. 
           (@Trans //T/R_/D_/Do_/To_/Floop "")
           (@Down)
           (@Down)
           (set! /up 2))
          (#t
           (set! /up 0)))
         (for-in /assign (reverse /phi) 
          (begin
           (set! /count (- /count 1))
           (cond
            ((or (null? /only) (member (@SSA_Orig_Elt (car /assign)) /only))
             (@Paste_Before (@SSA_Make_Phi /assign)))
            (#t
             (@Paste_Before (@Skip))))))
         (for /i 1 (gen-length /phi) 1 
          (@Right))
         (while (> /up 0) 
          (begin
           (set! /up (- /up 1))
           (@Up)))))
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (set! /n (+ /n 1))
         (@Right)
         (set! /fl_flag1 0)))))
     (@Up)
     (set! /n /n-save))))
  (set! funct-result /count)
  (set! /count /count-save)
  (set! /only /only-save)
  funct-result))

; Convert an SSA element (list of strings) 
; to either an expression or Lvalue: 
(define (@SSA_Convert_Elt /elt /type)
 
 (if (= /type //T_/Expression) (@Elt_To_Expn (my-map @SSA_Make_Name /elt)) (@Elt_To_Lvalue (my-map @SSA_Make_Name /elt))))

; Convert strings to names and negate numbers. 
; This converts an SSA element to a normal element 
(define (@SSA_Make_Name /x)
 
 (if (number? /x) (- /x) (@Make_Name /x)))

; Deduce the original name for the given SSA element 
; by chopping off the __num from the last element. 
; Also chop off [n] if present: 
(define (@SSA_Orig_Elt /elt)
 (let ((/str (last-1 /elt))
       (/p-save /p)
       (funct-result '()))
  (set! /p (- 1))
  ; Find the last occurrence of __ in the string 
  (while (>= (my-index "__" /str (+ /p 1)) 0) 
   (set! /p (my-index "__" /str (+ /p 1))))
  (cond
   ((< /p 0)
    (error (string-append "@SSA_Orig_Elt: no __ found in " /str))))
  ; Check for [ in the string 
  (cond
   ((>= (my-index "[" /str 0) 0)
    (set! /p (my-index "[" /str 0))))
  (set! funct-result (my-map @SSA_Make_Name (concat (butlast-1 /elt) (list (substr /str 0 /p)))))
  (set! /p /p-save)
  funct-result))

; Deduce the original name for the given variable or Lvalue 
; by chopping off the __num from the last element: 
(define (@SSA_Orig_Var //I)
 (let ((//R '()))
  (cond
   ((= (@ST //I) //T_/Struct)
    (set! //R (@Make //T_/Struct '() (list (@SSA_Orig_Name (@Get_n //I 1)) (@Get_n //I 2)))))
   ((= (@ST //I) //T_/Struct_/Lvalue)
    (set! //R (@Make //T_/Struct_/Lvalue '() (list (@SSA_Orig_Name (@Get_n //I 1)) (@Get_n //I 2)))))
   ((or (= (@ST //I) //T_/Variable) (= (@ST //I) //T_/Var_/Lvalue))
    (set! //R (@SSA_Orig_Name //I)))
   ((or (and (= (@ST //I) //T_/Aref) (= (@ST (@Get_n //I 1)) //T_/Variable)) (and (= (@ST //I) //T_/Aref_/Lvalue) (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue)))
    (set! //R (@Make (@ST //I) '() (list (@Get_n //I 1) (@Make //T_/Expressions '() (cons (@SSA_Orig_Var (@Get_n (@Get_n //I 2) 1)) (cdr (@Cs (@Get_n //I 2)))))))))
   (#t
    (@Print_WSL //I "")
    (error "@SSA_Orig_Var: unexpected type!")))
  //R))

; Convert the value of the given item to the original name. 
; (type can be T_Name, T_Variable or T_Var_Lvalue  
(define (@SSA_Orig_Name //I)
 (let ((/str (@N_String (@V //I)))
       (/p-save /p)
       (funct-result '()))
  (set! /p (- 1))
  ; Find the last occurrence of __ in the string 
  (while (>= (my-index "__" /str (+ /p 1)) 0) 
   (set! /p (my-index "__" /str (+ /p 1))))
  (set! funct-result (@Make (@ST //I) (@Make_Name (substr /str 0 /p)) '()))
  (set! /p /p-save)
  funct-result))

; Make a phi function assignment 
(define (@SSA_Make_Phi /assign-par)
 (let ((/assign-save /assign)
       (/v (@SSA_Convert_Elt (car /assign-par) //T_/Lvalue))
       (/e '())
       (/elt '())
       (funct-result '()))
  (set! /assign /assign-par)
  (for-in /elt (cdr /assign) 
   (set! /e (cons (@SSA_Convert_Elt /elt //T_/Expression) /e)))
  (set! /e (reverse /e))
  (set! funct-result (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Make 214 '() (list (@Make 9 (@Make_Name "phi") '()) (@Make 10 '() (@Var_To_Expn_List /e)))))))))
  (set! /assign /assign-save)
  funct-result))

; Do the renaming on the currently selected item 
; using the given hash tables: 
(define (@SSA_Rename /rename_exp-par /rename_lvalue-par /only-par)
 (let ((/only-save /only)
       (/rename_lvalue-save /rename_lvalue)
       (/rename_exp-save /rename_exp))
  (set! /only /only-par)
  (set! /rename_lvalue /rename_lvalue-par)
  (set! /rename_exp /rename_exp-par)
  (cond
   ((= (@ST (@I)) //T_/A_/Proc_/Call)
    (@SSA_Fix_A_Proc_Call)))
  (let ((/old-save /old)
        (/new-save /new))
   (set! /old '())
   (set! /new '())
   (@Foreach_Expn /foreach-/S/S/A-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Lvalue /foreach-/S/S/A-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /old /old-save)
   (set! /new /new-save))
  (set! /only /only-save)
  (set! /rename_lvalue /rename_lvalue-save)
  (set! /rename_exp /rename_exp-save)))

(define (@SSA_Fix_A_Proc_Call)
 (let ((/val (@Elements (@Get_n (@I) 2)))
       (/var (@Elements (@Get_n (@I) 3)))
       (/extra '()))
  (set! /extra (@Mth_Sort (my-map @Elt_To_Expn (@Set_Difference /var /val))))
  (cond
   ((not (null? /extra))
    (@Down_To 2)
    (@Paste_Over (@Make //T_/Expressions '() (concat (@Cs (@I)) /extra)))
    (@Up)))))

; ----------------------------------------------------------------------- 

