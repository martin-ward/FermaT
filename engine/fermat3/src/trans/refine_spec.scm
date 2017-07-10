;;; Scheme translation of WSL code
(define (/foreach-refine_spec-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   (@Down)
   (cond
    ((@Trans? //T/R_/Align_/Nested_/Statements)
     (@Trans //T/R_/Align_/Nested_/Statements "")))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((@Trans? //T/R_/Align_/Nested_/Statements)
       (@Trans //T/R_/Align_/Nested_/Statements "")))))
   (@Up))))

(define (/foreach-refine_spec-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Primed_/Var) (not (null? (gethash /value (@V (@I))))))
   (@Paste_Over (gethash /value (@V (@I)))))))

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
; Refine a specification statement. 
(define (@Refine_Spec_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Spec))
   (@Fail "Not a specification statement"))
  (#t
   (@Pass))))

(define (@Refine_Spec_Code //Data)
 ; Things to look for: 
 ; Assertion: SPEC <x>: Q AND R ENDSPEC where x' does not appear in Q 
 ; Simple IF statement: SPEC <x>: (Q AND P1) OR (NOT Q AND P2) ENDSPEC 
 ;   becomes: IF Q THEN SPEC <x>: P1 ENDSPEC ELSE SPEC <x>: P2 ENDSPEC 
 ;   provided x' does not occur in Q 
 ; Simple assignment: SPEC <x>: x' = e ENDSPEC where x' does not appear in e 
 ;   Also check for parallel simple assignments 
 (let ((//Budget (@String_To_Num //Data))
       (/new '()))
  ; Need at least 11 budget for @Simplify_Using: 
  (cond
   ((<= //Budget 0)
    (set! //Budget 30)))
  ; Apply Align_Nested_Statements to any cond statements generated: 
  (@Edit)
  (@New_Program (@Make //T_/Statements '() (@Refine_Spec (@Cs (@Get_n (@I) 1)) (@Get_n (@I) 2) //Budget)))
  (@Foreach_Statement /foreach-refine_spec-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Trans //T/R_/Delete_/All_/Skips "")
  (set! /new (@Cs (@Program)))
  (@Undo_Edit)
  (cond
   ((and (= (@GT (@I)) //T_/Statement) (null? (@Posn)))
    (@Paste_Over (@Make //T_/Statements '() /new)))
   (#t
    (@Splice_Over /new)))))

; Takes the list of assigned vars and the condition for a spec 
; and returns the refined result as a sequence of statements 
(define (@Refine_Spec /v /cond //Budget)
 (let ((//R '())
       (/vars (@Variables (@Make //T_/Lvalues '() /v)))
       (/assert '())
       (/new '()))
  ; First, factor out any assertion: 
  (cond
   ((= (@ST /cond) //T_/And)
    (for-in /comp (@Cs /cond) 
     (cond
      ((null? (intersection-n /vars (@Primed_Vars /comp)))
       (set! /assert (cons /comp /assert)))
      (#t
       (set! /new (cons /comp /new)))))
    (cond
     ((null? /assert)
      (set! //R (@Refine_Spec_Sub1 /v (@Simplify /cond //Budget) //Budget)))
     (#t
      (set! /assert (@Simplify (@Make //T_/And '() /assert) //Budget))
      (set! //R (cons (@Make //T_/Assert '() (list /assert)) (@Refine_Spec_Sub1 /v (@Simplify (@Make //T_/And '() /new) //Budget) //Budget))))))
   (#t
    (set! //R (@Refine_Spec_Sub1 /v (@Simplify /cond //Budget) //Budget))))
  (cond
   ((null? //R)
    (set! //R (list (@Skip)))))
  //R))

; Check for refinement to an IF statement: 
; Look for Q OR (P AND B) where B doesn't contain x' 
; Check that the resulting spec statements are strictly smaller 
; Pick the candidate B which gives the smallest result 
(define (@Refine_Spec_Sub1 /v /cond //Budget)
 (let ((//R '())
       (//P '())
       (//Q '())
       (//B '())
       (/size 0)
       (/min_/P '())
       (/min_/Q '())
       (/min_/B '())
       (/min_size 0)
       (//S1 '())
       (//S2 '())
       (/vars (@Variables (@Make //T_/Lvalues '() /v))))
  (cond
   ((= (@ST /cond) //T_/Or)
    (for-in /comp (@Cs /cond) 
     (cond
      ((= (@ST /comp) //T_/And)
       (for-in /sub (@Cs /comp) 
        (cond
         ((null? (intersection-n /vars (@Primed_Vars /sub)))
          (set! //B (cons /sub //B))))))))
    (for-in /comp (@Mth_Sort //B) 
     (begin
      (set! //P (@Simplify_Using /cond /comp //Budget))
      (set! //Q (@Simplify_Using /cond (@Not /comp) //Budget))
      (set! /size (+ (@Total_Size //P) (@Total_Size //Q)))
      (cond
       ((or (= /min_size 0) (< (+ (@Total_Size //P) (@Total_Size //Q)) /min_size))
        (set! /min_/B /comp)
        (set! /min_/P //P)
        (set! /min_/Q //Q))
       ((and (equal? /size /min_size) (= (@ST //B) //T_/Equal) (not (= (@ST /min_/B) //T_/Equal)))
        (set! /min_/B /comp)
        (set! /min_/P //P)
        (set! /min_/Q //Q)))))
    (cond
     ((not (null? /min_/B))
      (cond
       ((and (< (@Total_Size /min_/P) (@Total_Size /cond)) (< (@Total_Size /min_/Q) (@Total_Size /cond)))
        (cond
         ((= (@ST /min_/B) //T_/Not_/Equal)
          (set! /min_/B (@Not /min_/B))
          (let ((/tmp-var2 /min_/Q)
                (/tmp-var1 /min_/P))
           (set! /min_/P /tmp-var2)
           (set! /min_/Q /tmp-var1))))
        (set! //S1 (@Refine_Spec /v (@Simplify /min_/P //Budget) //Budget))
        (set! //S2 (@Refine_Spec /v (@Simplify /min_/Q //Budget) //Budget))
        (set! //R (list (@Make 114 '() (list (@Make 7 '() (list /min_/B (@Make 17 '() //S1))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() //S2))))))))
       (#t
        (set! //R (@Refine_Spec_Sub2 /v /cond //Budget)))))
     (#t
      (set! //R (@Refine_Spec_Sub2 /v /cond //Budget)))))
   (#t
    (set! //R (@Refine_Spec_Sub2 /v /cond //Budget))))
  //R))

; Check for extracting a simple assignment statement. 
; After factoring out an assignment to x, remove the x var and replace x' 
; by the value we just assigned to x (since this is the only possible value) 
; Eg: SPEC <x>: x' = A AND (x' = B AND p > q OR r > s) ENDSPEC 
; becomes: 
; {A = B AND p > q OR r > s}; x := A 
; ALSO TODO: update Mth_Simplify_Using to simplify A using B OR C 
; to check if (A using B) OR (A using C) is simpler than A 
; (Do this if Budget > 10 and pass Budget DIV 2 to each half) 
; See z1.wsl 
; Eg: tax' = small AND income > 30000 OR income > 36075) AND tax' = big 
; Using: income <= 3000 OR income > 36075 
; Gives: tax' = big 
(define (@Refine_Spec_Sub2 /v /cond //Budget)
 (let ((//R '())
       (/vars (@Variables (@Make //T_/Lvalues '() /v)))
       (/value-save /value)
       (/orig /cond)
       (/done '())
       (/new '())
       (funct-result '()))
  (set! /value (hash-table))
  (@Edit)
  (@New_Program /cond)
  (cond
   ((= (@ST (@I)) //T_/And)
    (@Down)))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (cond
     ((and (= (@ST (@I)) //T_/Equal) (= (@ST (@Get_n (@I) 1)) //T_/Primed_/Var) (null? (gethash /value (@V (@Get_n (@I) 1)))) (member (@V (@Get_n (@I) 1)) /vars) (null? (intersection-n (@Primed_Vars (@Get_n (@I) 2)) /vars)))
      (set! /new (cons (@Make //T_/Assign '() (list (@Make //T_/Var_/Lvalue (@V (@Get_n (@I) 1)) '()) (@Get_n (@I) 2))) /new))
      (set! /done (union-n (list (@V (@Get_n (@I) 1))) /done))
      (puthash /value (@V (@Get_n (@I) 1)) (@Get_n (@I) 2))
      (@Paste_Over (@Make //T_/True '() '()))))
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@Goto '())
  (cond
   ((not (null? /new))
    ; If there is any of the condition left, 
    ; then replace any primed assigned variables by their new values 
    ; and check that there are no remaining assigned vars 
    ; (i.e. the result is an assertion or spec 
    ; which can preceed the assignment) 
    (@Paste_Over (@Simplify (@I) //Budget))
    (@Foreach_Expn /foreach-refine_spec-2 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (cond
     ((not (null? (intersection-n (@Primed_Vars (@I)) /done)))
      ; Don't extract this assignment 
      (set! /new '())))))
  (cond
   ((not (null? /new))
    (set! /cond (@Simplify (@Program) //Budget))))
  (@Undo_Edit)
  ; Delete redundant assignments: 
  (let ((/tmp /new))
   (set! /new '())
   (for-in /assign /tmp 
    (cond
     ((not (@LR_Equal? (@Get_n /assign 1) (@Get_n /assign 2)))
      (set! /new (cons /assign /new))))))
  (cond
   ((not (null? /new))
    (set! //R (list (@Make //T_/Assignment '() /new)))))
  (cond
   ((= (@ST /cond) //T_/True)
    #t)
   ((not (null? (intersection-n /vars (@Primed_Vars /cond))))
    (cond
     ((or (not (null? /new)) (< (@Total_Size /cond) (@Total_Size /orig)))
      ; Refine the new (smaller) spec 
      (set! //R (concat (@Refine_Spec (@RS_Filter /v /cond) /cond //Budget) //R)))
     (#t
      (set! //R (concat (@Refine_Spec_Sub3 (@RS_Filter /v /cond) /cond //Budget) //R)))))
   (#t
    (set! //R (cons (@Make //T_/Assert '() (list /cond)) //R))))
  (set! funct-result //R)
  (set! /value /value-save)
  funct-result))

; Check if the spec can be decomposed to a sequence of two specs: 
(define (@Refine_Spec_Sub3 /v /cond //Budget)
 (let ((//R '())
       (/assigned '())
       (/used '())
       (/tmp '())
       (//O/K 1)
       (/v1 '()))
  (cond
   ((= (@ST /cond) //T_/And)
    (set! /assigned (@Primed_Vars (@Get_n /cond 1)))
    (set! /used (@Variables (@Get_n /cond 1)))
    (set! /tmp (cdr (@Cs /cond)))
    (while (and (= //O/K 1) (not (null? /tmp))) 
     (cond
      ((not (null? (intersection-n /assigned (@Variables (car /tmp)))))
       (set! //O/K 0))
      ((not (null? (intersection-n /used (@Primed_Vars (car /tmp)))))
       (set! //O/K 0))
      (#t
       (set! /tmp (cdr /tmp)))))
    (cond
     ((= //O/K 1)
      (set! //R (@Refine_Spec (@RS_Filter /v (@Get_n /cond 1)) (@Get_n /cond 1) //Budget))
      (set! /cond (@Simplify (@Make //T_/And '() (cdr (@Cs /cond))) //Budget))
      (set! //R (concat //R (@Refine_Spec (@RS_Filter /v /cond) /cond //Budget))))
     (#t
      (set! //R (@Refine_Spec_Sub4 /v /cond //Budget)))))
   (#t
    (set! //R (@Refine_Spec_Sub4 /v /cond //Budget))))
  //R))

; For future expansion... 
(define (@Refine_Spec_Sub4 /v /cond //Budget)
 (let ((//R '()))
  (set! //R (list (@Make 138 '() (list (@Make 12 '() (@Expn_To_Var_List /v)) /cond))))
  //R))

; Filter the given var list to return only the variables which appear primed 
; in the given condition 
(define (@RS_Filter /v /cond)
 (let ((/new '())
       (/vars (@Primed_Vars /cond)))
  (for-in /var /v 
   (cond
    ((member (@V /var) /vars)
     (set! /new (cons /var /new)))))
  (reverse /new)))

#t
