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
; Variables and functions for storing transformations. 
(set! //T/Rs_/Name (make-vector-eval 200 '()))
(set! //T/Rs_/Proc_/Name (make-vector-eval 200 '()))
(set! //T/Rs_/Test (make-vector-eval 200 '()))
(set! //T/Rs_/Code (make-vector-eval 200 '()))
(set! //T/Rs_/Keywords (make-vector-eval 200 '()))
(set! //T/Rs_/Help (make-vector-eval 200 '()))
(set! //T/Rs_/Prompt (make-vector-eval 200 '()))
(set! //T/Rs_/Data_/Gen_/Type (make-vector-eval 200 '()))
(set! /adt_/T/R_/Next_/Number 0)
(define (@New_TR_Number)
 (set! /adt_/T/R_/Next_/Number (+ /adt_/T/R_/Next_/Number 1))
 /adt_/T/R_/Next_/Number)

; Variables and functions for controlling the transformation process. 
(set! /adt_/Trans_/Tests 1)
(set! /adt_/Passed (- 1))
(set! /adt_/Fail_/Message "")
(set! /adt_/T/R_/Start_/P '())
(set! /adt_/T/R_/Start_/Span '())
(define (@Use_Trans_Tests)
 (set! /adt_/Trans_/Tests 1))

(define (@No_Trans_Tests)
 (set! /adt_/Trans_/Tests 0))

(define (@Trans_Tests_Status?)
 
 (= /adt_/Trans_/Tests 1))

(define (@Reset_Pass_Status)
 (cond
  ((= /adt_/In_/A_/Trans 1)
   (set! /adt_/Fail_/Message "")))
 (set! /adt_/Passed (- 1)))

(define (@Pass)
 (cond
  ((= /adt_/In_/A_/Trans 1)
   (set! /adt_/Fail_/Message "")))
 (set! /adt_/Passed 1))

(define (@Fail //Reason)
 (cond
  ((= /adt_/In_/A_/Trans 1)
   (set! /adt_/Fail_/Message //Reason)))
 (set! /adt_/Passed 0))

(define (@Notice //Message)
 (cond
  ((= /adt_/In_/A_/Trans 1)
   (set! /adt_/Fail_/Message //Message))))

(define (@Fail_Message)
 
 /adt_/Fail_/Message)

(define (@Passed?)
 
 (= /adt_/Passed 1))

(define (@Failed?)
 
 (= /adt_/Passed 0))

(define (@Trans? /trans)
 (let ((//R 0)
       (//Name (if (number? /trans) (vector-ref //T/Rs_/Proc_/Name (- /trans 1)) '())))
  (cond
   ((null? //Name)
    (display-list "No such transformation number:" /trans))
   (#t
    (set! /adt_/T/R_/Start_/P (cons (@Posn) /adt_/T/R_/Start_/P))
    (set! /adt_/In_/A_/Trans (+ /adt_/In_/A_/Trans 1))
    (set! /adt_/Fail_/Message "")
    (set! /adt_/Passed (- 1))
    (apply (vector-ref //T/Rs_/Test (- /trans 1)) '())
    (set! //R /adt_/Passed)
    (set! /adt_/Passed (- 1))
    (@Goto (car /adt_/T/R_/Start_/P))
    (set! /adt_/T/R_/Start_/P (cdr /adt_/T/R_/Start_/P))
    (set! /adt_/In_/A_/Trans (- /adt_/In_/A_/Trans 1))
    (cond
     ((and (not (= //R 1)) (not (= //R 0)))
      (display-list "Neither `Passed' nor `Failed' in transformation: " //Name)))))
  (= //R 1)))

(define (@What_Trans //Words)
 (let ((/i /adt_/T/R_/Next_/Number)
       (//List '()))
  (while (not (= /i 1)) 
   (begin
    (set! /i (- /i 1))
    (cond
     ((and (@Set_Subset? //Words (@Make_Set (vector-ref //T/Rs_/Keywords (- /i 1)))) (not-member "Hidden" (vector-ref //T/Rs_/Keywords (- /i 1))) (or (= /adt_/Trans_/Tests 0) (@Trans? /i)))
      (set! //List (cons /i //List))))))
  //List))

(set! //T/R_/No_/Change (@Make_Name "TR_No_Change"))
(set! /adt_/Trans_/Count 0)
(define (@Trans /trans //Data)
 (let ((//Name (if (number? /trans) (vector-ref //T/Rs_/Proc_/Name (- /trans 1)) '()))
       (//Program_/Type (@ST (@Program))))
  (cond
   ((null? //Name)
    (display-list "No such transformation number: " /trans))
   ((or (> /adt_/In_/A_/Trans 0) (= /adt_/Trans_/Tests 1) (@Trans? /trans))
    (cond
     ((= /adt_/In_/A_/Trans 0)
      (@Save_State)
      (@Dtable_Put (@Program) //T/R_/No_/Change 1)))
    ; Indicate that we are now in a transformation 
    (set! /adt_/T/R_/Start_/P (cons (@Posn) /adt_/T/R_/Start_/P))
    (set! /adt_/In_/A_/Trans (+ /adt_/In_/A_/Trans 1))
    (set! /adt_/Fail_/Message "")
    (set! /adt_/Passed (- 1))
    (set! /adt_/Trans_/Count (+ /adt_/Trans_/Count 1))
    (apply (vector-ref //T/Rs_/Code (- /trans 1)) (list //Data))
    (set! /adt_/Passed (- 1))
    (set! /adt_/T/R_/Start_/P (cdr /adt_/T/R_/Start_/P))
    (set! /adt_/In_/A_/Trans (- /adt_/In_/A_/Trans 1))
    ; We have now finished a transformation; set a message 
    ; if nothing has happened and there is no message set. 
    (cond
     ((and (= /adt_/In_/A_/Trans 0) (equal? /adt_/Fail_/Message "") (not (null? (@Dtable_Get (@Program) //T/R_/No_/Change))))
      (set! /adt_/Fail_/Message (string-append (vector-ref //T/Rs_/Name (- /trans 1)) " did not change the program."))))
    (@Fixup)
    (cond
     ((and (null? (@Program)) (= /adt_/In_/A_/Trans 0) (= //Program_/Type //T_/Statements))
      (@New_Program (@Skips))))))))

#t
