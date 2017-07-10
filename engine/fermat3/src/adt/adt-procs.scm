;;; Scheme translation of WSL code
(define /%const__adt-procs__1 (@Make 108 '() '()))
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
; Exported functions are: 
; @Equal?  @Seq_Equal?  @LR_Equal?  @LR_Seq_Equal? 
; @Increment 
; @Syntax_OK? 
; @Clever_Delete, @Fixup, @Fix_Cond, @Fix_Dijkstra 
; Test two WSL items for equality: 
(define (@Equal? //I1 //I2)
 (let ((//O/K 1))
  (cond
   ((eq? //I1 //I2)
    (set! //O/K 1))
   ((or (null? //I1) (null? //I2))
    (set! //O/K 0))
   ((@Equal_Items? //I1 //I2)
    (set! //O/K 1))
   (#t
    (set! //O/K 0)))
  (= //O/K 1)))

(define (@Equal_Items? //I1 //I2)
 (let ((//O/K 1))
  (cond
   ((not (= (@ST //I1) (@ST //I2)))
    (set! //O/K 0))
   ((@Has_Value_Type? (@ST //I1))
    (cond
     ((not (equal? (@V //I1) (@V //I2)))
      (set! //O/K 0))))
   ((not (= (@Size //I1) (@Size //I2)))
    (set! //O/K 0))
   (#t
    (let ((//C1 (@Cs //I1))
          (//C2 (@Cs //I2)))
     (while (and (= //O/K 1) (not (null? //C1))) 
      (cond
       ((not (@Equal_Items? (car //C1) (car //C2)))
        (set! //O/K 0))
       (#t
        (set! //C1 (cdr //C1))
        (set! //C2 (cdr //C2))))))))
  (= //O/K 1)))

; Test two sequences of WSL items for equality: 
(define (@Seq_Equal? //L1 //L2)
 (let ((//O/K 1))
  (cond
   ((not (= (gen-length //L1) (gen-length //L2)))
    (set! //O/K 0))
   (#t
    (while (and (= //O/K 1) (not (null? //L1))) 
     (cond
      ((not (@Equal_Items? (car //L1) (car //L2)))
       (set! //O/K 0))
      (#t
       (set! //L1 (cdr //L1))
       (set! //L2 (cdr //L2)))))))
  (= //O/K 1)))

; Test two WSL items for equality, where an Lvalue is treated as equal 
; to the corresponding expression: 
(define (@LR_Equal? //I1 //I2)
 (let ((//O/K 1))
  (cond
   ((and (not (= (@ST //I1) (@ST //I2))) (not (and (= (@GT //I1) //T_/Lvalue) (= (@Var_To_Expn_Type (@ST //I1)) (@ST //I2)))) (not (and (= (@GT //I2) //T_/Lvalue) (= (@Var_To_Expn_Type (@ST //I2)) (@ST //I1)))))
    (set! //O/K 0))
   ((@Has_Value_Type? (@ST //I1))
    (cond
     ((not (equal? (@V //I1) (@V //I2)))
      (set! //O/K 0))))
   ((not (= (@Size //I1) (@Size //I2)))
    (set! //O/K 0))
   (#t
    (let ((//C1 (@Cs //I1))
          (//C2 (@Cs //I2)))
     (while (and (= //O/K 1) (not (null? //C1))) 
      (cond
       ((not (@LR_Equal? (car //C1) (car //C2)))
        (set! //O/K 0))
       (#t
        (set! //C1 (cdr //C1))
        (set! //C2 (cdr //C2))))))))
  (= //O/K 1)))

(define (@LR_Seq_Equal? //L1 //L2)
 (let ((//O/K 1))
  (cond
   ((not (= (gen-length //L1) (gen-length //L2)))
    (set! //O/K 0))
   (#t
    (while (and (= //O/K 1) (not (null? //L1))) 
     (cond
      ((not (@LR_Equal? (car //L1) (car //L2)))
       (set! //O/K 0))
      (#t
       (set! //L1 (cdr //L1))
       (set! //L2 (cdr //L2)))))))
  (= //O/K 1)))

; ---------------------------------------------------------------------------  
; The following function increments a piece of WSL code.                       
; The four parameters are:                                                     
;                                                                              
;   * The WSL code to be incremented,                                          
;   * The action system type in which the code is contained (or `Rec'),        
;   * The amount by which the code is to be incremented (a negative value      
;     would be used when *removing* code from a loop),                         
;   * The minimum terminal values within the WSL code that need to be          
;     incremented.                                                             
;                                                                              
; If the item was a Statement, then the result is a sequence of statements     
; Otherwise, the result is an incremented item of the same type.               
;                                                                              
; ---------------------------------------------------------------------------  
(define (@Increment //I //A-par /inc /tv)
 (let ((//A-save //A)
       (//R '())
       (funct-result '()))
  (set! //A //A-par)
  (cond
   ((= (@ST //I) //T_/Exit)
    (cond
     ((>= (@V //I) /tv)
      (cond
       ((<= (+ (@V //I) /inc) 0)
        (set! //R (list (@Make //T_/Skip '() '()))))
       (#t
        (set! //R (list (@Make //T_/Exit (+ (@V //I) /inc) '()))))))
     (#t
      (set! //R (list //I)))))
   ((= (@ST //I) //T_/Skip)
    (cond
     ((and (> /inc 0) (= /tv 0))
      (set! //R (list (@Make //T_/Exit /inc '()))))
     (#t
      (set! //R (list //I)))))
   ((and (= (@ST //I) //T_/Call) (equal? //A "Reg"))
    (set! //R (list //I)))
   ((@Simple? //I)
    (cond
     ((= /tv 0)
      (cond
       ((<= /inc 0)
        (set! //R (list //I (@Make //T_/Skip '() '()))))
       (#t
        (set! //R (list //I (@Make //T_/Exit /inc '()))))))
     (#t
      (set! //R (list //I)))))
   ((= (@GT //I) //T_/Statements)
    (set! //R (@Make //T_/Statements '() (@Increment_List (@Cs //I) //A /inc /tv))))
   ((= (@ST //I) //T_/Floop)
    (set! //R (list (@Make //T_/Floop '() (list (@Increment (@Get_n //I 1) //A /inc (+ /tv 1)))))))
   ((@Has_Comps_Type? (@ST //I))
    (let ((/comp '()))
     (for-in /comp (@Cs //I) 
      (set! //R (cons (@Increment /comp //A /inc /tv) //R)))
     (cond
      ((= (@GT //I) //T_/Statement)
       (set! //R (list (@Make (@ST //I) '() (reverse //R)))))
      (#t
       (set! //R (@Make (@ST //I) '() (reverse //R)))))))
   (#t
    (set! //R //I)))
  (set! funct-result //R)
  (set! //A //A-save)
  funct-result))

(define (@Increment_List //L //A-par /inc /tv)
 (let ((//A-save //A)
       (//Result '())
       (//Term_/V /tv)
       (funct-result '()))
  (set! //A //A-par)
  (cond
   ((and (= /tv 0) (> (gen-length //L) 1))
    (set! //Term_/V 1)))
  (cond
   ((null? //L)
    #t)
   ((@Gen_Improper? (car //L) //A)
    (set! //Result (concat (@Increment (car //L) //A /inc //Term_/V) (cdr //L))))
   (#t
    (set! //Result (concat (@Increment (car //L) //A /inc //Term_/V) (@Increment_List (cdr //L) //A /inc /tv)))))
  (set! funct-result //Result)
  (set! //A //A-save)
  funct-result))

; ---------------------------------------------------------------------------  
; The following function compares two statements to see if the first is equal  
; to the second, or if an increment of the first is equal to the second.  The  
; procedure @I_Eq updates the variable i, according to whether the argument S  
; needs to be incremented before it matches I, where i takes the following     
; values:                                                                      
;                                                                              
;   i >= 0   The statements are equal so far, with the increment set to i,     
;   i = -1   No increment has been fixed and no inequality detected,           
;   i = -2   An inequality has been detected.                                  
; ---------------------------------------------------------------------------  
(define (@Incremented_Equal? //S //I)
 (let ((/i-save /i)
       (funct-result '()))
  (set! /i (- 1))
  (set! /i (@I_Eq  //S //I (@AS_Type) /i))
  (set! funct-result (>= /i 0))
  (set! /i /i-save)
  funct-result))

(define (@I_Eq //S //I //A-par /i-par)
 (let ((/i-save /i)
       (//A-save //A)
       (funct-result '()))
  (set! /i /i-par)
  (set! //A //A-par)
  ;A sequence of statements may be incremented by appending an  
  ;`EXIT', or by having a final `SKIP' replaced by an `EXIT'.  
  (cond
   ((and (= (@ST //S) //T_/Statements) (= (@ST //I) //T_/Statements))
    (@I_Eq_Stats (@Cs //S) (@Cs //I)))
   ((or (not (= (@ST //S) (@ST //I))) (not (= (@Size //S) (@Size //I))))
    (set! /i (- 2)))
   (#t
    (cond
     ((and (@Simple? //S) (@Simple? //I))
      (cond
       ((= (@ST //S) //T_/Exit)
        (cond
         ((and (>= /i 0) (= (+ (@V //S) /i) (@V //I)))
          #t)
         ((and (equal? /i (- 1)) (>= (@V //I) (@V //S)))
          (set! /i (- (@V //I) (@V //S))))
         (#t
          (set! /i (- 2)))))
       ((or (> /i 0) (not (@Equal_Items? //S //I)))
        (set! /i (- 2)))))
     (#t
      (let ((//S (@Cs //S))
            (//I (@Cs //I)))
       (while (and (not (equal? /i (- 2))) (not (null? //S))) 
        (begin
         (set! /i (@I_Eq  (car //S) (car //I) //A /i))
         (set! //S (cdr //S))
         (set! //I (cdr //I)))))))))
  (set! funct-result /i)
  (set! /i /i-save)
  (set! //A //A-save)
  funct-result))

(define (@I_Eq_Stats //S //I)
 (cond
  ((null? //S)
   (cond
    ((null? //I)
     (cond
      ((or (= /i 0) (equal? /i (- 1)))
       (set! /i 0))
      (#t
       (set! /i (- 2)))))
    ((and (>= /i 0) (= (gen-length //I) 1) (= (@ST (car //I)) //T_/Exit) (equal? (@V (car //I)) /i))
     #t)
    ((and (equal? /i (- 1)) (= (gen-length //I) 1) (= (@ST (car //I)) //T_/Exit))
     (set! /i (@V (car //I))))
    (#t
     (set! /i (- 2)))))
  ((null? //I)
   (set! /i (- 2)))
  (#t
   (set! /i (@I_Eq  (car //S) (car //I) //A /i))
   (cond
    ((not (@Gen_Improper? (car //S) //A))
     (@I_Eq_Stats (cdr //S) (cdr //I)))))))

; ---------------------------------------------------------------------------  
; The following function tests the syntax of a piece of WSL code.              
; ---------------------------------------------------------------------------  
(define (@Syntax_OK? //Item)
 
 (@S_OK? //Item '()))

(define (@S_OK? //Item //Posn)
 (let ((//O/K 1)
       (//C '())
       (//S/T (@ST //Item))
       (//S '())
       (//L 0)
       (/i-save /i)
       (funct-result '()))
  (set! /i 1)
  (cond
   ((@Has_Value_Type? //S/T)
    #t)
   (#t
    (set! //C (@Cs //Item))
    (set! //S (@Syntax //S/T))
    (cond
     ((@List_Type? //S/T)
      (set! //L 1))
     ((not (= (gen-length //S) (gen-length //C)))
      (display-list "Bad length at " (reverse //Posn) " Type " //S/T)
      (display-list "Length = " //C " should be: " //S)
      (set! //O/K 0)))
    (while (and (= //O/K 1) (not (null? //C))) 
     (begin
      (cond
       ((not (= (@GT (car //C)) (car //S)))
        (@Print_WSL //Item "")
        (display-list "Bad type at " (reverse (cons /i //Posn)))
        (display-list "Gen type is: " (@Type_Name (@GT (car //C))) "(" (@GT (car //C)) ")  Should be: " (@Type_Name (car //S)) "(" (car //S) ")")
        (set! //O/K 0))
       ((not (@S_OK? (car //C) (cons /i //Posn)))
        (set! //O/K 0)))
      (cond
       ((= //L 0)
        (set! //S (cdr //S))))
      (set! //C (cdr //C))
      (set! /i (+ /i 1))))))
  (set! funct-result (= //O/K 1))
  (set! /i /i-save)
  funct-result))

; ---------------------------------------------------------------------------  
; The following function deletes something `cleverly'.  I.e. it removes any    
; anomalies after deletion by fixing upwards to restore the syntax.            
; ---------------------------------------------------------------------------  
(define (@Clever_Delete)
 (@Delete)
 (@Fixup))

; ---------------------------------------------------------------------------  
; The following functions are for performing the removal of syntactic          
; anomalies.  In particular, `@Fixup' looks for:                               
;                                                                              
;   * Statements (in particular `COND's) with the wrong number of components,  
;   * Empty statement sequences,                                               
;   * Procedure definitions with empty definitions,                            
;   * `Guardeds' which are the only component of a `COND' statement,           
;   * Actions with empty definitions.                                          
;                                                                              
; ---------------------------------------------------------------------------  
(define (@Fixup)
 (cond
  ((@Up?)
   (let ((//G (@GT (@Parent)))
         (//S (@ST (@Parent))))
    (cond
     ((= //G //T_/Statement)
      (cond
       ((= //S //T_/Cond)
        (@Fixup_Cond))
       ((or (and (not (@List_Type? //S)) (not (= (@Size (@Parent)) (gen-length (@Syntax //S))))) (not (@Cs? (@Parent))))
        (@Up)
        (@Fixup_Statement //S))))
     ((= //G //T_/Statements)
      (cond
       ((not (@Cs? (@Parent)))
        (@Up)
        (cond
         ((@Up?)
          (@Clever_Delete))
         (#t
          (@Paste_Over (@Skips)))))))
     ((= //G //T_/Definition)
      (cond
       ((and (= //S //T_/Proc) (< (@Size (@Parent)) 4))
        ;Add a body to the procedure:
        (@To_Last)
        (@Paste_After (@Skips))
        (@Right))
       ((and (or (= //S //T_/Funct) (= //S //T_/B/Funct)) (< (@Size (@Parent)) 5))
        ;Add a body to the function:
        (@To_Last)
        (@Paste_Before (@Skips))
        (@Right))))
     ((= //G //T_/Definitions)
      (cond
       ((not (@Cs? (@Parent)))
        ;Have just deleted the last definition in a `Where'.
        (@Up)
        (@Up)
        (@Splice_Over (@Cs (@Get_n (@I) 1))))))
     ((= //G //T_/Guarded)
      (cond
       ((< (@Size (@Parent)) 2)
        (@Up)
        (cond
         ((= (@ST (@Parent)) //T_/Cond)
          ;AND the negation of this condition with the other conditions.
          (let ((//P (@Posn))
                (//C (@Not (@Get_n (@I) 1))))
           (while (@Right?) 
            (begin
             (@Right)
             (@Down)
             (@Paste_Over (@And //C (@I)))
             (@Up)))
           (@Goto //P)
           (@Clever_Delete)))
         (#t
          ;Cannot delete a guard in a `D_If' or `D_Do'.
          (@Down)
          (@Paste_After (@Skips))
          (@Up))))))
     ((= //G //T_/Action)
      (cond
       ((and (= (@Size (@Parent)) 1) (= (@ST (@Get_n (@Parent) 1)) //T_/Name))
        ;An action cannot just be deleted since it may be called...
        ;but checking whether it is called is too `expensive'.
        (@To 1)
        (@Paste_After (@Skips)))))
     ((member //G (list //T_/Expression //T_/Expressions //T_/Condition //T_/Lvalue //T_/Lvalues //T_/Assign //T_/Assigns))
      #t)))
   ; Move left/up to a valid position, if necessary: 
   (while (and (@Up?) (> (@Posn_n) (@Size (@Parent))) (@Left?)) 
    (@Left))
   (while (and (@Up?) (> (@Posn_n) (@Size (@Parent))) (@Up?)) 
    (@Up)))))

(define (@Fixup_Statement //S/T)
 (cond
  ((= //S/T //T_/While)
   (@Paste_Over (@Make //T_/Assert '() (list (@Not (@Get_n (@I) 1))))))
  ((or (= //S/T //T_/D_/If) (= //S/T //T_/Floop))
   (@Paste_Over /%const__adt-procs__1))
  ((= //S/T //T_/Join)
   (@Splice_Over (@Cs (@Get_n (@I) 1))))
  ((or (@Ifmatch_Type? //S/T) (= //S/T //T_/M/W_/Proc))
   (@Down_Last)
   (@Paste_After (@Skips)))
  ((or (= //S/T //T_/M/W_/Funct) (= //S/T //T_/M/W_/B/Funct))
   (@Down_Last)
   (@Paste_Before (@Skips)))
  (#t
   (@Clever_Delete))))

(define (@Fixup_Cond)
 (cond
  ((not (@Cs? (@Parent)))
   (@Up)
   (@Clever_Delete))
  (#t
   (let ((/p (@Posn_n)))
    (@Up)
    (@Fix_Cond)
    (cond
     ((>= (@Size (@I)) /p)
      (@Down_To /p))
     ((@Down?)
      (@Down)))))))

(define (@Fix_Cond)
 (cond
  ((not (@Cs? (@I)))
   (@Clever_Delete))
  (#t
   (let ((/fixed (@Fix_Cond_Item (@I))))
    (cond
     ((null? /fixed)
      (@Clever_Delete))
     ((= (@ST /fixed) //T_/Statements)
      (@Splice_Over (@Cs /fixed)))
     ((not (eq? /fixed (@I)))
      (@Paste_Over /fixed)))))))

(define (@Fix_Dijkstra)
 ;The currently selected item is a `D_IF' or `D_DO' which may need fixing.
 (cond
  ((not (@Cs? (@I)))
   (cond
    ((= (@ST (@I)) //T_/D_/If)
     (@Delete))
    (#t
     (@Paste_Over (@Make //T_/Abort '() '())))))
  (#t
   ;Look at each of the guards to see whether any of them are empty.
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((= (@Size (@I)) 1)
       (@Down)
       (@Paste_After (@Skips))
       (@Up))
      ((not (@Cs? (@Get_n (@I) 2)))
       (@Down_Last)
       (@Paste_Over (@Skips))
       (@Up)))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))
   (@Up))))

; ---------------------------------------------------------------------------  

