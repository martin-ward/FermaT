;;; Scheme translation of WSL code
; Fast versions of the basic ADT functions. 
; The structure of a WSL item is either: 
; CONS(dtable, CONS(spec_type, value)) OR: 
; CONS(dtable, CONS(spec_type, component_list)) 
; These definitions assume that we ONLY call @V() on an item which is known 
; to have a value: 
(define (@V //I)
 
 (cdr (cdr //I)))

(define (@Cs //I)
 
 (cdr (cdr //I)))

; These definitions are more relaxed and allow the functions to be applied 
; to any type of item: 
(define (@Value //I)
 
 (if (@Has_Comps_Type? (car (cdr //I))) '() (cdr (cdr //I))))

(define (@Components //I)
 
 (if (@Has_Comps_Type? (car (cdr //I))) (cdr (cdr //I)) '()))

(define (@Components? //I)
 
 (not (null? (@Components //I))))

(define (@Cs? //I)
 
 (not (null? (@Components //I))))

(define (@Spec_Type //I)
 
 (car (cdr //I)))

(define (@ST //I)
 
 (car (cdr //I)))

(set! //Spec_/To_/Gen_/Type (make-vector-eval 1999 0))
(for /i 1 1999 1 
 (vector-set! //Spec_/To_/Gen_/Type (- /i 1) (if (< /i 100) /i (quotient /i 100))))
(define (@Gen_Type //I)
 
 (vector-ref //Spec_/To_/Gen_/Type (- (car (cdr //I)) 1)))

(define (@GT //I)
 
 (vector-ref //Spec_/To_/Gen_/Type (- (car (cdr //I)) 1)))

(define (@Size //I)
 
 (if (@Has_Comps_Type? (car (cdr //I))) (- (gen-length //I) 2) 0))

; @Get_n is now a macro which checks for small integer constant n 
(define (@Get_L //I /n /m)
 
 (firstn (+ (- /m /n) 1) (nthcdr (+ /n 1) //I)))

(define (@Get //I /posn)
 
 (if (null? /posn) //I (@Get (@Get_n //I (car /posn)) (cdr /posn))))

(define (@Make //S/T /value /comps)
 
 (if (null? /value) (cons '() (cons //S/T /comps)) (cons '() (cons //S/T /value))))

; Routines for handling sets as sorted lists, using @Gen_Less? for comparisons. 
(define (@Set_Union /a /b)
 
 (if (null? /a) /b (if (null? /b) /a (if (equal? (car /a) (car /b)) (cons (car /a) (@Set_Union (cdr /a) (cdr /b))) (if (@Gen_Less? (car /a) (car /b)) (cons (car /a) (@Set_Union (cdr /a) /b)) (cons (car /b) (@Set_Union /a (cdr /b))))))))

(define (@Set_Intersect /a /b)
 
 (if (or (null? /a) (null? /b)) '() (if (equal? (car /a) (car /b)) (cons (car /a) (@Set_Intersect (cdr /a) (cdr /b))) (if (@Gen_Less? (car /a) (car /b)) (@Set_Intersect (cdr /a) /b) (@Set_Intersect /a (cdr /b))))))

(define (@Set_Difference /a /b)
 
 (if (null? /a) '() (if (null? /b) /a (if (equal? (car /a) (car /b)) (@Set_Difference (cdr /a) (cdr /b)) (if (@Gen_Less? (car /a) (car /b)) (cons (car /a) (@Set_Difference (cdr /a) /b)) (@Set_Difference /a (cdr /b)))))))

(define (@Set_Subset? /a /b)
 
 (or (null? /a) (and (not (null? /b)) (or (and (equal? (car /a) (car /b)) (@Set_Subset? (cdr /a) (cdr /b))) (and (@Gen_Less? (car /b) (car /a)) (@Set_Subset? /a (cdr /b)))))))

; Sort a list of elements (maybe with duplicates) and return a set: 
(define (@Make_Set //L)
 (let ((//R '())
       (/len (gen-length //L)))
  (cond
   ((<= /len 1)
    (set! //R //L))
   ((= /len 2)
    (cond
     ((equal? (car //L) (car (cdr //L)))
      (set! //R (list (car //L))))
     ((@Gen_Less? (car //L) (car (cdr //L)))
      (set! //R //L))
     (#t
      (set! //R (list (car (cdr //L)) (car //L))))))
   (#t
    (let ((/mid (quotient /len 2)))
     (set! //R (@Set_Union (@Make_Set (@Sub_Seg //L 1 /mid)) (@Make_Set (@Final_Seg //L (+ /mid 1))))))))
  //R))

; An efficient way to form the union or intersection of a list of sets 
; is to pick the two smallest sets in the list and process those, 
; returning the result to the list. Repeat until only one set remains. 
; I think that this can be done efficiently using a heap structure. 
; Such a function can be used to optimise REDUCE(/, ...) 
; and REDUCE(/, ...) functions. 
; ============================================================================== 

