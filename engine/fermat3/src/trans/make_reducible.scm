;;; Scheme translation of WSL code
(define (/foreach-make_reducible-1 //Depth //A/S_/Type)
 (cond
  ((= //Depth 0)
   (@Down_Last)
   (cond
    ((and (member 1 (@Gen_TVs (@I) /as)) (@Right?))
     (cond
      ((= //T_/Cond (@ST (@I)))
       (@GCR_Cond_Fix)))
     (@Trans //T/R_/Fully_/Absorb_/Right "")))
   (while (@Left?) 
    (begin
     (@Left)
     (cond
      ((and (member 1 (@Gen_TVs (@I) /as)) (@Right?))
       (cond
        ((= //T_/Cond (@ST (@I)))
         (@GCR_Cond_Fix)))
       (@Trans //T/R_/Fully_/Absorb_/Right "")))))
   (@Up))))

(define (/foreach-make_reducible-2 //Depth //A/S_/Type)
 (cond
  ((member //Depth (@Gen_TVs (@I) /as))
   (set! /n (+ /n 1)))))

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
(define (@Make_Reducible_Test)
 (cond
  ((and (not (= (@GT (@I)) //T_/Statement)) (not (= (@GT (@I)) //T_/Statements)))
   (@Fail "Item is not a statement or statement sequence"))
  ((@Gen_Reducible? (@I) (@AS_Type))
   (@Fail "Selected item is already reducible"))
  (#t
   (@Pass))))

(define (@Make_Reducible_Code //Data)
 (let ((/as-save /as)
       (/calls-save /calls)
       (/call_n-save /call_n))
  (set! /as (@AS_Type))
  (set! /calls (@MP_Proc_Calls))
  (set! /call_n 1)
  (let ((/-result- (@MR_Process  /as /calls /call_n)))
   (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-)))
  ; First MR_Process might make second one succeed 
  (cond
   ((not (@Gen_Reducible? (@I) (@AS_Type)))
    (let ((/-result- (@MR_Process  /as /calls /call_n)))
     (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-)))))
  (cond
   ((@Gen_Reducible? (@I) (@AS_Type))
    ; @MR_Process succeeded 
   )
   (#t
    ; Use absorption to make the item reducible 
    (display-list "Using absorption as last resort in Make_Reducible:")
    (@Ateach_Stats /foreach-make_reducible-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (set! /as /as-save)
  (set! /calls /calls-save)
  (set! /call_n /call_n-save)))

(define (@MR_Process /as-par /calls-par /call_n-par)
 (let ((/call_n-save /call_n)
       (/calls-save /calls)
       (/as-save /as)
       (funct-result '()))
  (set! /call_n /call_n-par)
  (set! /calls /calls-par)
  (set! /as /as-par)
  (cond
   ((and (@Cs? (@I)) (not (@Gen_Proper? (@I) /as)))
    (cond
     ((= (@ST (@I)) //T_/Statements)
      (let ((/-result- (@MR_Statements  /as /calls /call_n)))
       (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
       (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-))))
     (#t
      (@Down)
      (let ((/-result- (@MR_Process  /as /calls /call_n)))
       (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
       (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-)))
      (while (@Right?) 
       (begin
        (@Right)
        (let ((/-result- (@MR_Process  /as /calls /call_n)))
         (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
         (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-)))))
      (@Up)))))
  (set! funct-result (list /calls /call_n))
  (set! /call_n /call_n-save)
  (set! /calls /calls-save)
  (set! /as /as-save)
  funct-result))

(define (@MR_Statements /as-par /calls-par /call_n-par)
 (let ((/call_n-save /call_n)
       (/calls-save /calls)
       (/as-save /as)
       (funct-result '()))
  (set! /call_n /call_n-par)
  (set! /calls /calls-par)
  (set! /as /as-par)
  (let ((/tvs '())
        (/n-save /n)
        (/trans 0))
   (set! /n 0)
   (@Down)
   ; to first statement 
   ; process this statement's components: 
   (cond
    ((not (= (@ST (@I)) //T_/Floop))
     (let ((/-result- (@MR_Process  /as /calls /call_n)))
      (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-)))))
   (while (@Right?) 
    (begin
     (set! /tvs (@Gen_TVs (@I) /as))
     (cond
      ((member 1 /tvs)
       ; See if a simple absorb will be OK, 
       ; ie there are <=1 terminal sts with tv zero 
       ; First check for a COND which we can fully expand (<=1 arm with tv=0) 
       ; If not, then check the number of STS's with tv=0 and absorb if <=1 
       (set! /n 0)
       (cond
        ((= (@ST (@I)) //T_/Cond)
         (set! /trans //T/R_/Fully_/Expand_/Forward)
         (@Down)
         (cond
          ((member 0 (@Gen_TVs (@I) /as))
           (set! /n (+ /n 1))))
         (while (@Right?) 
          (begin
           (@Right)
           (cond
            ((member 0 (@Gen_TVs (@I) /as))
             (set! /n (+ /n 1))))))
         (@Up))
        (#t
         (set! /trans //T/R_/Fully_/Absorb_/Right)
         (set! //Depth 0)
         (@Ateach_Terminal /foreach-make_reducible-2 0 (@AS_Type) 1)
         (cond
          ((null? (@Program))
           (@New_Program (@Skips))))))
       (cond
        (#t
         (display-list "P = " (@Posn) " has n = " /n)))
       (cond
        ((<= /n 1)
         (cond
          ((@Trans? /trans)
           (@Trans /trans ""))
          (#t
           (@Right))))
        (#t
         ; If the next statement is `small' and is the last in  
         ; the sequence then absorb and copy it. 
         ; Otherwise, turn the rest of the sequence 
         ; into a procedure (if possible) and absorb the proc call. 
         (@Right)
         (cond
          ((and (not (@Right?)) (<= (@Stat_Count (@I)) 1) (<= (@Total_Size (@I)) 20))
           (@Left)
           (cond
            ((@Trans? //T/R_/Absorb_/Right)
             (@Trans //T/R_/Absorb_/Right ""))
            (#t
             (@Right))))
          (#t
           (let ((/-result- (@MR_Make_And_Absorb_Proc  /as /calls /call_n)))
            (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
            (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-))))))))
      (#t
       (@Right)))
     ; process this statement's components: 
     (cond
      ((not (= (@ST (@I)) //T_/Floop))
       (let ((/-result- (@MR_Process  /as /calls /call_n)))
        (set! /calls (car /-result-)) (set! /-result- (cdr /-result-))
        (set! /call_n (car /-result-)) (set! /-result- (cdr /-result-)))))))
   (@Up)
   (set! /n /n-save))
  (set! funct-result (list /calls /call_n))
  (set! /call_n /call_n-save)
  (set! /calls /calls-save)
  (set! /as /as-save)
  funct-result))

; Make the sequence from @I onwards into a proc (if possible) 
; and absorb the proc call into the preceding statement. 
; NB: Don't put references to cc into a proc: we may not be able to remove them! 
(define (@MR_Make_And_Absorb_Proc /as-par /calls-par /call_n-par)
 (let ((/call_n-save /call_n)
       (/calls-save /calls)
       (/as-save /as)
       (funct-result '()))
  (set! /call_n /call_n-par)
  (set! /calls /calls-par)
  (set! /as /as-par)
  (let ((/posn_n (@Posn_n))
        (/span 0)
        (/cc_name (@Make_Name "cc"))
        (/tvs '())
        (/new_tvs '())
        (/name '()))
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((not (null? (@Calls (@I))))
      (set! /fl_flag1 1))
     (#t
      ; Test the current statement for inclusion in the proc 
      ; If it is suitable, copy the new_tvs into tvs and continue 
      (set! /new_tvs (@Gen_TVs (@I) /as))
      (cond
       ((not (@Gen_Proper? (@I) /as))
        ; If the item is reducible and has a single terminal value of 1, 
        ; then it can still be included in the procedure, provided it contains 
        ; two or more statements 
        (cond
         ((and (equal? /new_tvs (list 1)) (@Gen_Reducible? (@I) /as) (or (> /span 0) (>= (@Stat_Count (@I)) 2)))
          (set! /span (+ /span 1))
          (set! /tvs /new_tvs)))
        (set! /fl_flag1 1))
       ((member /cc_name (@Variables (@I)))
        (set! /fl_flag1 1))
       (#t
        (set! /span (+ /span 1))
        (set! /tvs /new_tvs)
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0))))))))
   (cond
    ((or (= /span 0) (@Right?))
     ; Either we can't make a proc or the proc doesn't cover the whole sequence 
    )
    (#t
     (@To /posn_n)
     ; If there is a single small item (eg a proc call), then don't make a proc: 
     (cond
      ((and (= /span 1) (<= (@Stat_Count (@I)) 1) (<= (@Total_Size (@I)) 20))
       #t)
      (#t
       ; Choose a suitable name for the proc 
       (set! /name (@Make_Name (string-append "p_" (@String /call_n))))
       (while (member /name /calls) 
        (begin
         (set! /call_n (+ /call_n 1))
         (set! /name (@Make_Name (string-append "p_" (@String /call_n))))))
       (set! /calls (union-n (list /name) /calls))
       (cond
        ((null? /tvs)
         (@Make_Proc /name /span 0))
        (#t
         (@Make_Proc /name /span (car /tvs))))))
     (cond
      ((not (@Left?))
       (error "Can't move left in @MR_Make_And_Absorb_Proc!!!")))
     (@Left)
     (cond
      ((@Trans? //T/R_/Absorb_/Right)
       (display-list "Absorbing right...")
       (@Trans //T/R_/Absorb_/Right ""))
      (#t
       (error "Cannot Absorb_Right at: " (@Posn)))))))
  (set! funct-result (list /calls /call_n))
  (set! /call_n /call_n-save)
  (set! /calls /calls-save)
  (set! /as /as-save)
  funct-result))

#t
