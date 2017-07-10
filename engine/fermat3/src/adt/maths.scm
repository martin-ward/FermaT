;;; Scheme translation of WSL code
(define (/foreach-maths-1 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/And) (= (@ST (@I)) //T_/Or))
   (@Mth_Duplicate_Relations)))
 (cond
  ((= (@ST (@I)) //T_/And)
   (@Mth_Known_Value1 //Budget)
   (@Mth_Known_Value3 //Budget)))
 (cond
  ((= (@ST (@I)) //T_/Or)
   (@Mth_Known_Value2 //Budget)
   (@Mth_Known_Value4 //Budget)))
 (cond
  ((> //Budget 10)
   (@Mth_Expensive //Budget)))
 (cond
  ((or (= (@ST (@I)) //T_/Forall) (= (@ST (@I)) //T_/Exists))
   (@Mth_Quantifiers //Budget))))

(define (/foreach-maths-2 //Depth //A/S_/Type)
 (cond
  ((or (and (= (@ST (@I)) //T_/And) (> (@Size (@I)) 1) (= (@ST (@Get_n (@I) 1)) //T_/Or) (= (@ST (@Get_n (@I) 2)) //T_/Or)) (and (= (@ST (@I)) //T_/Or) (> (@Size (@I)) 1) (= (@ST (@Get_n (@I) 1)) //T_/And) (= (@ST (@Get_n (@I) 2)) //T_/And)))
   (@Mth_Common_Components //Budget))))

(define (/foreach-maths-3 //Depth //A/S_/Type)
 (cond
  ((and (@Cs? (@I)) (> (vector-ref //Eval_/Op (- (@ST (@I)) 1)) 0) (@Mth_Constants? (@Cs (@I)) (vector-ref //Eval_/Op (- (@ST (@I)) 1))))
   (set! //S/T (@ST (@I)))
   (set! /v (@Mth_Values (@Cs (@I))))
   ; Convert hex values to numbers 
   (let ((/vv '()))
    (for-in /n /v 
     (cond
      ((and (string? /n) (@Starts_With? /n "hex 0x") (< (string-length /n) 14))
       (set! /vv (cons (@Hex_To_Num (substr /n 6)) /vv)))
      (#t
       (set! /vv (cons /n /vv)))))
    (set! /v (reverse /vv)))
   (cond
    ((= //S/T //T_/Plus)
     (@Paste_Over (@Make //T_/Number (my-reduce + /v) '())))
    ((= //S/T //T_/Times)
     (@Paste_Over (@Make //T_/Number (my-reduce * /v) '())))
    ((= //S/T //T_/Exponent)
     (@Paste_Over (my-reduce @Mth_Eval_Exponent (@Cs (@I)))))
    ((= //S/T //T_/Max)
     (@Paste_Over (@Make //T_/Number (my-reduce MAX /v) '())))
    ((= //S/T //T_/Min)
     (@Paste_Over (@Make //T_/Number (my-reduce MIN /v) '())))
    ((= //S/T //T_/Intersection)
     (@Paste_Over (@Mth_Sequence (my-reduce @Set_Intersect /v))))
    ((= //S/T //T_/Union)
     (@Paste_Over (@Mth_Sequence (my-reduce @Set_Union /v))))
    ((= //S/T //T_/Negate)
     (@Paste_Over (@Make //T_/Number (- 0 (car /v)) '())))
    ((= //S/T //T_/Invert)
     ; Cannot simplify this since we only have integers! 
    )
    ((= //S/T //T_/Div)
     (cond
      ((not (= (wsl-ref /v 2) 0))
       (@Paste_Over (@Make //T_/Number (quotient (wsl-ref /v 1) (wsl-ref /v 2)) '())))))
    ((= //S/T //T_/Mod)
     (cond
      ((not (= (wsl-ref /v 2) 0))
       (@Paste_Over (@Make //T_/Number (modulo (wsl-ref /v 1) (wsl-ref /v 2)) '())))))
    ((= //S/T //T_/Abs)
     (@Paste_Over (@Make //T_/Number (abs (car /v)) '())))
    ((= //S/T //T_/Sgn)
     (@Paste_Over (@Make //T_/Number (sgn (car /v)) '())))
    ((= //S/T //T_/Int)
     (@Paste_Over (@Make //T_/Number (int (car /v)) '())))
    ((= //S/T //T_/Frac)
     (@Paste_Over (@Make //T_/Number (frac (car /v)) '())))
    ((= //S/T //T_/Length)
     (@Paste_Over (@Make //T_/Number (gen-length (car /v)) '())))
    ((= //S/T //T_/Slength)
     (@Paste_Over (@Make //T_/Number (string-length (car /v)) '())))
    ((= //S/T //T_/Reverse)
     (@Paste_Over (@Mth_Sequence (reverse (car /v)))))
    ((= //S/T //T_/Set_/Diff)
     (@Paste_Over (@Mth_Sequence (@Set_Difference (wsl-ref /v 1) (wsl-ref /v 2)))))
    ((= //S/T //T_/Concat)
     (@Paste_Over (@Mth_Sequence (my-reduce concat /v))))
    ((= //S/T //T_/Minus)
     (@Paste_Over (@Make //T_/Number (my-reduce - /v) '())))
    ((= //S/T //T_/Divide)
     (while (and (> (gen-length /v) 1) (not (= (wsl-ref /v 2) 0)) (= (modulo (wsl-ref /v 1) (wsl-ref /v 2)) 0)) 
      (set! /v (cons (/ (wsl-ref /v 1) (wsl-ref /v 2)) (@Final_Seg /v 3))))
     (cond
      ((= (gen-length /v) 1)
       (@Paste_Over (@Make //T_/Number (wsl-ref /v 1) '())))))
    ((= //S/T //T_/If)
     (cond
      ((= (@ST (@Get_n (@I) 1)) //T_/True)
       (@Paste_Over (@Get_n (@I) 2)))
      (#t
       (@Paste_Over (@Get_n (@I) 3)))))
    ((= //S/T //T_/Index)
     (cond
      ((= (@Size (@Get_n (@I) 1)) 2)
       (@Paste_Over (@Make //T_/Number (my-index (@V (@Get_n (@Get_n (@I) 1) 1)) (@V (@Get_n (@Get_n (@I) 1) 2))) '())))
      (#t
       (@Paste_Over (@Make //T_/Number (my-index (@V (@Get_n (@Get_n (@I) 1) 1)) (@V (@Get_n (@Get_n (@I) 1) 2)) (@V (@Get_n (@Get_n (@I) 1) 3))) '())))))
    ((= //S/T //T_/Substr)
     (cond
      ((>= (@V (@Get_n (@Get_n (@I) 1) 2)) (string-length (@V (@Get_n (@Get_n (@I) 1) 1))))
       #t)
      ((= (@Size (@Get_n (@I) 1)) 2)
       (@Paste_Over (@Make //T_/String (substr (@V (@Get_n (@Get_n (@I) 1) 1)) (@V (@Get_n (@Get_n (@I) 1) 2))) '())))
      (#t
       (@Paste_Over (@Make //T_/String (substr (@V (@Get_n (@Get_n (@I) 1) 1)) (@V (@Get_n (@Get_n (@I) 1) 2)) (@V (@Get_n (@Get_n (@I) 1) 3))) '())))))
    ((or (= //S/T //T_/Tail) (= //S/T //T_/Head))
     #t)
    (#t
     (display-list "ERROR!!! Unknown type in @Mth_Evaluate: " (@Type_Name //S/T) "(" //S/T ")"))))))

(define (/foreach-maths-4 //Depth //A/S_/Type)
 (cond
  ((and (@Cs? (@I)) (> (vector-ref //Eval_/Op (- (@ST (@I)) 1)) 0) (@Mth_Constants? (@Cs (@I)) (vector-ref //Eval_/Op (- (@ST (@I)) 1))))
   (set! //S/T (@ST (@I)))
   (set! /v (@Mth_Values (@Cs (@I))))
   ; Convert hex values to numbers 
   (let ((/vv '()))
    (for-in /n /v 
     (cond
      ((and (string? /n) (@Starts_With? /n "hex 0x") (< (string-length /n) 14))
       (set! /vv (cons (@Hex_To_Num (substr /n 6)) /vv)))
      (#t
       (set! /vv (cons /n /vv)))))
    (set! /v (reverse /vv)))
   (cond
    ((= //S/T //T_/And)
     (cond
      ((not (member //T_/False (@Mth_Types (@Cs (@I)))))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Or)
     (cond
      ((member //T_/True (@Mth_Types (@Cs (@I))))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Not)
     (cond
      ((= (@ST (@Get_n (@I) 1)) //T_/False)
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Even)
     (cond
      ((even? (car /v))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Odd)
     (cond
      ((odd? (car /v))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Subset)
     (cond
      ((@Set_Subset? (wsl-ref /v 1) (wsl-ref /v 2))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Member)
     (cond
      ((member (wsl-ref /v 1) (wsl-ref /v 2))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/In)
     (cond
      ((member (wsl-ref /v 1) (wsl-ref /v 2))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Not_/In)
     (cond
      ((not (member (wsl-ref /v 1) (wsl-ref /v 2)))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Equal)
     (cond
      ((equal? (wsl-ref /v 1) (wsl-ref /v 2))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Not_/Equal)
     (cond
      ((not (equal? (wsl-ref /v 1) (wsl-ref /v 2)))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Less)
     ; Can't use @String_Less? since assembler and COBOL strings 
     ; may be stored in EBCDIC! 
     (cond
      ((and #f (string? (wsl-ref /v 1)) (string? (wsl-ref /v 2)))
       (cond
        ((@String_Less? (wsl-ref /v 1) (wsl-ref /v 2))
         (@Paste_Over //Mth_/True))
        (#t
         (@Paste_Over //Mth_/False))))
      ((or (string? (wsl-ref /v 1)) (string? (wsl-ref /v 2)))
       #t)
      (#t
       (cond
        ((< (wsl-ref /v 1) (wsl-ref /v 2))
         (@Paste_Over //Mth_/True))
        (#t
         (@Paste_Over //Mth_/False))))))
    ((= //S/T //T_/Less_/Eq)
     (cond
      ((and #f (string? (wsl-ref /v 1)) (string? (wsl-ref /v 2)))
       (cond
        ((@String_Less_Eq? (wsl-ref /v 1) (wsl-ref /v 2))
         (@Paste_Over //Mth_/True))
        (#t
         (@Paste_Over //Mth_/False))))
      ((or (string? (wsl-ref /v 1)) (string? (wsl-ref /v 2)))
       #t)
      (#t
       (cond
        ((<= (wsl-ref /v 1) (wsl-ref /v 2))
         (@Paste_Over //Mth_/True))
        (#t
         (@Paste_Over //Mth_/False))))))
    ((= //S/T //T_/Greater)
     (cond
      ((and #f (string? (wsl-ref /v 1)) (string? (wsl-ref /v 2)))
       (cond
        ((@String_Less? (wsl-ref /v 2) (wsl-ref /v 1))
         (@Paste_Over //Mth_/True))
        (#t
         (@Paste_Over //Mth_/False))))
      ((or (string? (wsl-ref /v 1)) (string? (wsl-ref /v 2)))
       #t)
      (#t
       (cond
        ((> (wsl-ref /v 1) (wsl-ref /v 2))
         (@Paste_Over //Mth_/True))
        (#t
         (@Paste_Over //Mth_/False))))))
    ((= //S/T //T_/Greater_/Eq)
     (cond
      ((and #f (string? (wsl-ref /v 1)) (string? (wsl-ref /v 2)))
       (cond
        ((@String_Less_Eq? (wsl-ref /v 2) (wsl-ref /v 1))
         (@Paste_Over //Mth_/True))
        (#t
         (@Paste_Over //Mth_/False))))
      ((or (string? (wsl-ref /v 1)) (string? (wsl-ref /v 2)))
       #t)
      (#t
       (cond
        ((>= (wsl-ref /v 1) (wsl-ref /v 2))
         (@Paste_Over //Mth_/True))
        (#t
         (@Paste_Over //Mth_/False))))))
    ((= //S/T //T_/Sequenceq)
     (cond
      ((= (@ST (@Get_n (@I) 1)) //T_/Sequence)
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Numberq)
     (cond
      ((= (@ST (@Get_n (@I) 1)) //T_/Number)
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Stringq)
     (cond
      ((= (@ST (@Get_n (@I) 1)) //T_/String)
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    ((= //S/T //T_/Implies)
     (cond
      ((or (= (@ST (@Get_n (@I) 1)) //T_/False) (= (@ST (@Get_n (@I) 2)) //T_/True))
       (@Paste_Over //Mth_/True))
      (#t
       (@Paste_Over //Mth_/False))))
    (#t
     (display-list "ERROR!!! Unknown type in @Mth_Evaluate: " (@Type_Name //S/T) "(" //S/T ")"))))))

(define (/foreach-maths-5 //Depth //A/S_/Type)
 (cond
  ((@Cs? (@I))
   (set! //S/T (@ST (@I)))
   (cond
    ((= //S/T //T_/Minus)
     (@Paste_Over (@Make //T_/Plus '() (cons (@Get_n (@I) 1) (my-map @Mth_Negate (cdr (@Cs (@I)))))))
     (set! //S/T //T_/Plus))
    ((= //S/T //T_/Divide)
     (@Paste_Over (@Make //T_/Times '() (cons (@Get_n (@I) 1) (my-map @Mth_Invert (cdr (@Cs (@I)))))))
     (set! //S/T //T_/Times))
    ((and (= //S/T //T_/Concat) (= (@ST (@Get_n (@I) 1)) //T_/Concat))
     (@Paste_Over (@Make //T_/Concat '() (concat (@Cs (@Get_n (@I) 1)) (cdr (@Cs (@I))))))))
   ; The remaining associative operators are the commutative operators 
   (cond
    ((= (vector-ref //Reverse_/Op (- //S/T 1)) //S/T)
     (set! /new '())
     (for-in /x (@Cs (@I)) 
      (cond
       ((= (@ST /x) //S/T)
        (set! /new (concat (reverse (@Cs /x)) /new)))
       (#t
        (set! /new (cons /x /new)))))
     (@Paste_Over (@Make //S/T '() (reverse /new))))))))

(define (/foreach-maths-6 //Depth //A/S_/Type)
 (set! //S/T (@ST (@I)))
 (cond
  ((= (vector-ref //Reverse_/Op (- //S/T 1)) //S/T)
   (set! /new '())
   (for-in /x (@Cs (@I)) 
    (cond
     ((= (@ST /x) //S/T)
      (set! /new (concat (reverse (@Cs /x)) /new)))
     (#t
      (set! /new (cons /x /new)))))
   (@Paste_Over (@Make //S/T '() (reverse /new))))))

(define (/foreach-maths-7 //Depth //A/S_/Type)
 (cond
  ((= (vector-ref //Reverse_/Op (- (@ST (@I)) 1)) (@ST (@I)))
   (@Paste_Over (@Mth_Sort_Merge_Item (@I) (@ST (@I)) 0)))
  ((and (= (@ST (@I)) //T_/Concat) (= (@Size (@I)) 1))
   (@Paste_Over (@Get_n (@I) 1)))))

(define (/foreach-maths-8 //Depth //A/S_/Type)
 (set! /count 0)
 (cond
  ((or (= (@ST (@I)) //T_/And) (= (@ST (@I)) //T_/Or))
   (for-in /comp (@Cs (@I)) 
    (cond
     ((@Unsafe_Test? /comp)
      (set! /count (+ /count 1)))))))
 (cond
  ((= (vector-ref //Reverse_/Op (- (@ST (@I)) 1)) (@ST (@I)))
   (@Paste_Over (@Mth_Sort_Merge_Item (@I) (@ST (@I)) /count)))))

(define (/foreach-maths-9 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /rel)
   (set! /change 1)
   (@Paste_Over /same))
  ((= (@ST (@I)) (vector-ref //Reverse_/Op (- (@ST /rel) 1)))
   (cond
    ((and (@Equal? (@Get_n (@I) 2) (@Get_n /rel 1)) (@Equal? (@Get_n (@I) 1) (@Get_n /rel 2)))
     (set! /change 1)
     (@Paste_Over /same))))
  ((= (@ST (@I)) (vector-ref //Inverse_/Op (- (@ST /rel) 1)))
   (cond
    ((and (@Equal? (@Get_n (@I) 2) (@Get_n /rel 2)) (@Equal? (@Get_n (@I) 1) (@Get_n /rel 1)))
     (set! /change 1)
     (@Paste_Over /diff))))
  ((= (@ST (@I)) (vector-ref //Inverse_/Op (- (vector-ref //Reverse_/Op (- (@ST /rel) 1)) 1)))
   (cond
    ((and (@Equal? (@Get_n (@I) 2) (@Get_n /rel 1)) (@Equal? (@Get_n (@I) 1) (@Get_n /rel 2)))
     (set! /change 1)
     (@Paste_Over /diff))))))

(define (/foreach-maths-10 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) (car /vars))
   (@Paste_Over (car /vals)))))

(define (/foreach-maths-11 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) (car /vars))
   (@Paste_Over (car /vals)))))

(define (/foreach-maths-12 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /var)
   (@Paste_Over /val))))

(define (/foreach-maths-13 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /var)
   (@Paste_Over /val))))

(define (/foreach-maths-14 //Depth //A/S_/Type)
 (cond
  ((@Equal? //I (@I))
   (set! //R (+ //R 1)))))

(define (/foreach-maths-15 //Depth //A/S_/Type)
 ; Check for * with + components 
 (cond
  ((= (@ST (@I)) //T_/Times)
   ; Search for the group of + components: 
   (@Down)
   (while (and (@Right?) (< (vector-ref //Mth_/Ord (- (@ST (@I)) 1)) (vector-ref //Mth_/Ord (- //T_/Plus 1)))) 
    (@Right))
   (cond
    ((and (= (@ST (@I)) //T_/Plus) (@Right?))
     (while (and (@Right?) (= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Plus)) 
      (begin
       ; Multiply out current item with next item 
       (@Mth_Multiply //T_/Times //T_/Plus //Budget)))
     ; Check if there is now only one component: 
     (@Up)
     (cond
      ((= (@Size (@I)) 1)
       (@Paste_Over (@Get_n (@I) 1)))))))))

(define (/foreach-maths-16 //Depth //A/S_/Type)
 ; Check for AND with OR components 
 (cond
  ((= (@ST (@I)) //T_/And)
   ; Search for the group of OR components: 
   (@Down)
   (while (and (@Right?) (< (vector-ref //Mth_/Ord (- (@ST (@I)) 1)) (vector-ref //Mth_/Ord (- //T_/Or 1)))) 
    (@Right))
   (cond
    ((and (= (@ST (@I)) //T_/Or) (@Right?))
     (while (and (@Right?) (= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Or)) 
      (begin
       ; Multiply out current item with next item 
       (@Mth_Multiply //T_/And //T_/Or //Budget)))
     (cond
      ((and (@Right?) (< (@Size (@I)) (quotient //Budget 2)))
       (@Right)
       (@Paste_Over (@Make //T_/Or '() (list (@I))))
       (@Left)
       (@Mth_Multiply //T_/And //T_/Or //Budget)))
     ; Check if there is now only one component: 
     (@Up)
     (cond
      ((= (@Size (@I)) 1)
       (@Paste_Over (@Get_n (@I) 1)))))))))

(define (/foreach-maths-17 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Plus) (> (@Mth_Occs //T_/Times (my-map @ST (@Cs (@I)))) 1))
   (@Mth_Factor //T_/Plus //T_/Times))))

(define (/foreach-maths-18 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Or) (> (@Mth_Occs //T_/And (my-map @ST (@Cs (@I)))) 1))
   (@Mth_Factor //T_/Or //T_/And))))

(define (/foreach-maths-19 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Plus) (> (@Mth_Occs //T_/Times (my-map @ST (@Cs (@I)))) 1))
   (@Mth_Factor //T_/Plus //T_/Times))))

(define (/foreach-maths-20 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Or) (> (@Mth_Occs //T_/And (my-map @ST (@Cs (@I)))) 1))
   (@Mth_Factor //T_/Or //T_/And))))

(define (/foreach-maths-21 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Number) (< (@V (@I)) 0))
   (@Paste_Over (@Make //T_/Negate '() (list (@Make //T_/Number (- (@V (@I))) '())))))
  ((= (@ST (@I)) //T_/Times)
   (cond
    ((member //T_/Invert (my-map @ST (@Cs (@I))))
     (@Mth_Prettify_Inverses //T_/Times //T_/Invert //T_/Divide))))
  ((= (@ST (@I)) //T_/Plus)
   (cond
    ((member //T_/Negate (my-map @ST (@Cs (@I))))
     (@Mth_Prettify_Inverses //T_/Plus //T_/Negate //T_/Minus))))
  ((and (= (@ST (@I)) //T_/Concat) (> (@Size (@I)) 2))
   (@Paste_Over (@Mth_Bug_Fix_Concat (@I))))))

(define (/foreach-maths-22 //Depth //A/S_/Type)
 (cond
  ((and (= (@Size (@I)) 2) (> (vector-ref //Reverse_/Op (- (@ST (@I)) 1)) 0))
   (cond
    ((and (= (@ST (@Get_n (@I) 1)) //T_/X_/Funct_/Call) (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) (@Make_Name "string")))
     (@Paste_Over (@Make (vector-ref //Reverse_/Op (- (@ST (@I)) 1)) '() (list (@Get_n (@I) 2) (@Get_n (@I) 1)))))
    ((and (= (@ST (@Get_n (@I) 2)) //T_/Negate) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Number))
     #t)
    ((or (@Mth_Lt? (@Get_n (@I) 2) (@Get_n (@I) 1)) (and (or (= (@ST (@Get_n (@I) 2)) //T_/Variable) (= (@ST (@Get_n (@I) 2)) //T_/Primed_/Var)) (and (not (= (@ST (@Get_n (@I) 1)) //T_/Variable)) (not (= (@ST (@Get_n (@I) 1)) //T_/Primed_/Var)))))
     (@Paste_Over (@Make (vector-ref //Reverse_/Op (- (@ST (@I)) 1)) '() (list (@Get_n (@I) 2) (@Get_n (@I) 1)))))))))

(define (/foreach-maths-23 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Times)
   (cond
    ((= (@ST (@Get_n (@I) (@Size (@I)))) //T_/Number)
     (@Paste_Over (@Make //T_/Times '() (cons (@Get_n (@I) (@Size (@I))) (butlast-1 (@Cs (@I)))))))))))

(define (/foreach-maths-24 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/X_/Funct_/Call)
   (cond
    ((and (equal? (@V (@Get_n (@I) 1)) /bit_and) (@Mth_All_Constants? (@Cs (@Get_n (@I) 2))))
     (@Simplify_Bit /bit_and))
    ((and (equal? (@V (@Get_n (@I) 1)) /bit_or) (@Mth_All_Constants? (@Cs (@Get_n (@I) 2))))
     (@Simplify_Bit /bit_or))
    ((and (equal? (@V (@Get_n (@I) 1)) /bit_xor) (@Mth_All_Constants? (@Cs (@Get_n (@I) 2))))
     (@Simplify_Bit /bit_xor))
    ((and (equal? (@V (@Get_n (@I) 1)) /bit_or) (= (@Size (@Get_n (@I) 2)) 2) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Number) (= (@V (@Get_n (@Get_n (@I) 2) 1)) 0))
     (@Paste_Over (@Get_n (@Get_n (@I) 2) 2)))
    ((and (equal? (@V (@Get_n (@I) 1)) /bit_or) (= (@Size (@Get_n (@I) 2)) 2) (= (@ST (@Get_n (@Get_n (@I) 2) 2)) //T_/Number) (= (@V (@Get_n (@Get_n (@I) 2) 2)) 0))
     (@Paste_Over (@Get_n (@Get_n (@I) 2) 1)))))))

(define (/foreach-maths-25 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@Or /not_/B (@I))) //T_/True)
   (@Paste_Over //Mth_/True))
  ((= (@ST (@And //B (@I))) //T_/False)
   (@Paste_Over //Mth_/False))))

(define (/foreach-maths-26 //Depth //A/S_/Type)
 (cond
  ((not (null? (gethash /replace (@V (@I)))))
   (@Paste_Over (gethash /replace (@V (@I)))))))

(define (/foreach-maths-27 //Depth //A/S_/Type)
 (cond
  ((equal? (@V (@I)) /v)
   (set! /count (+ /count 1)))))

(define (/foreach-maths-28 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__maths__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__exp_save /exp)
          (/__var_save /var))
     (set! /exp (vector-ref /__/Match_array 1))
     (set! /var (vector-ref /__/Match_array 0))
     (@Paste_Over //Mth_/True)
     (set! /exp /__exp_save)
     (set! /var /__var_save)))))
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__maths__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__var_save /var)
          (/__exp_save /exp))
     (set! /var (vector-ref /__/Match_array 1))
     (set! /exp (vector-ref /__/Match_array 0))
     (@Paste_Over //Mth_/True)
     (set! /var /__var_save)
     (set! /exp /__exp_save))))))

(define (/foreach-maths-29 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Not) (or (= (@ST (@Get_n (@I) 1)) //T_/B/Funct_/Call) (= (@ST (@Get_n (@I) 1)) //T_/X_/B/Funct_/Call)))
   (set! /calls (cons (@Get_n (@I) 1) /calls)))))

(define (/foreach-maths-30 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /old)
   (@Paste_Over /new))))

(define /%const__maths__1 (@Make 313 '() (list (@Make 217 -1 '()) (@Make 217 -2 '()))))
; This file contains the functions which perform symbolic mathematics. 
; Initialise local data for the simplifier: 
; NB for efficiency, call this once at the first call to the simplifier. 
(set! //Mth_/True (@Make //T_/True '() '()))
(set! //Mth_/False (@Make //T_/False '() '()))
(set! //Mth_0 (@Make //T_/Number 0 '()))
(set! //Mth_1 (@Make //T_/Number 1 '()))
(set! //Mth_m1 (@Make //T_/Number (- 1) '()))
(set! //Mth_/Empty (@Make //T_/Sequence '() '()))
(set! //Qry_/Simple (@Make_Name "Qry_Simple"))
(define (@Mth_Init)
 ; Commutative operators 
 (set! //Comm_/Ops (list //T_/Plus //T_/Times //T_/Union //T_/Intersection //T_/Equal //T_/Not_/Equal //T_/And //T_/Or //T_/Max //T_/Min))
 ; Reversable operators relations 
 (set! //Reverse_/Op (make-vector-eval 1999 0))
 (for-in /op //Comm_/Ops 
  (vector-set! //Reverse_/Op (- /op 1) /op))
 (vector-set! //Reverse_/Op (- //T_/Less 1) //T_/Greater)
 (vector-set! //Reverse_/Op (- //T_/Greater 1) //T_/Less)
 (vector-set! //Reverse_/Op (- //T_/Less_/Eq 1) //T_/Greater_/Eq)
 (vector-set! //Reverse_/Op (- //T_/Greater_/Eq 1) //T_/Less_/Eq)
 ; Negated versions of the relations: 
 (set! //Inverse_/Op (make-vector-eval 1999 0))
 (vector-set! //Inverse_/Op (- //T_/Less 1) //T_/Greater_/Eq)
 (vector-set! //Inverse_/Op (- //T_/Greater 1) //T_/Less_/Eq)
 (vector-set! //Inverse_/Op (- //T_/Less_/Eq 1) //T_/Greater)
 (vector-set! //Inverse_/Op (- //T_/Greater_/Eq 1) //T_/Less)
 (vector-set! //Inverse_/Op (- //T_/Equal 1) //T_/Not_/Equal)
 (vector-set! //Inverse_/Op (- //T_/Not_/Equal 1) //T_/Equal)
 ; De Morgan's Laws: 
 (vector-set! //Inverse_/Op (- //T_/And 1) //T_/Or)
 (vector-set! //Inverse_/Op (- //T_/Or 1) //T_/And)
 ; Other inverses: 
 (vector-set! //Inverse_/Op (- //T_/Odd 1) //T_/Even)
 (vector-set! //Inverse_/Op (- //T_/Even 1) //T_/Odd)
 (vector-set! //Inverse_/Op (- //T_/In 1) //T_/Not_/In)
 (vector-set! //Inverse_/Op (- //T_/Not_/In 1) //T_/In)
 ; Gen eval ops (type 1) can have any constant arguments 
 ; Num eval ops (type 2) must have numeric arguments 
 (set! //Eval_/Ops_/Gen (list //T_/And //T_/Or //T_/Not //T_/Subset //T_/Member //T_/Equal //T_/Not_/Equal //T_/If //T_/Intersection //T_/Union //T_/Set_/Diff //T_/Length //T_/Reverse //T_/Concat //T_/Sequenceq //T_/Numberq //T_/Stringq //T_/Implies //T_/In //T_/Not_/In //T_/X_/Funct_/Call //T_/Slength //T_/Length //T_/Forall //T_/Exists //T_/Less //T_/Less_/Eq //T_/Greater //T_/Greater_/Eq //T_/Index //T_/Substr //T_/Head //T_/Tail //T_/M/W_/Funct_/Call))
 (set! //Eval_/Ops_/Num (list //T_/Even //T_/Odd //T_/Abs //T_/Sgn //T_/Int //T_/Frac //T_/Max //T_/Min //T_/Exponent //T_/Times //T_/Divide //T_/Plus //T_/Div //T_/Mod //T_/Negate //T_/Invert //T_/Minus))
 (set! //Eval_/Op (make-vector-eval 1999 0))
 (for-in /op //Eval_/Ops_/Gen 
  (vector-set! //Eval_/Op (- /op 1) 1))
 (for-in /op //Eval_/Ops_/Num 
  (vector-set! //Eval_/Op (- /op 1) 2))
 (set! //Idempotent_/Ops (list //T_/Union //T_/Intersection //T_/And //T_/Or //T_/Max //T_/Min))
 (set! //Power_/Op (make-vector-eval 1999 0))
 (for-in /op //Idempotent_/Ops 
  (vector-set! //Power_/Op (- /op 1) /op))
 (vector-set! //Power_/Op (- //T_/Plus 1) //T_/Times)
 (vector-set! //Power_/Op (- //T_/Times 1) //T_/Exponent)
 (set! //Identity_/Value (make-vector-eval 1999 '()))
 (vector-set! //Identity_/Value (- //T_/Plus 1) //Mth_0)
 (vector-set! //Identity_/Value (- //T_/Times 1) //Mth_1)
 (vector-set! //Identity_/Value (- //T_/Union 1) //Mth_/Empty)
 (vector-set! //Identity_/Value (- //T_/And 1) //Mth_/True)
 (vector-set! //Identity_/Value (- //T_/Or 1) //Mth_/False)
 (set! //Zero_/Value (make-vector-eval 1999 '()))
 (vector-set! //Zero_/Value (- //T_/Times 1) //Mth_0)
 (vector-set! //Zero_/Value (- //T_/Intersection 1) //Mth_/Empty)
 (vector-set! //Zero_/Value (- //T_/And 1) //Mth_/False)
 (vector-set! //Zero_/Value (- //T_/Or 1) //Mth_/True)
 ; Operator ordering (for sorting components of commutative operators): 
 (set! //Mth_/Ord (make-vector-eval 1999 1999))
 (vector-set! //Mth_/Ord (- //T_/And 1) 1)
 (vector-set! //Mth_/Ord (- //T_/Or 1) 2)
 (vector-set! //Mth_/Ord (- //T_/Forall 1) 4)
 (vector-set! //Mth_/Ord (- //T_/Exists 1) 5)
 (vector-set! //Mth_/Ord (- //T_/Even 1) 6)
 (vector-set! //Mth_/Ord (- //T_/Odd 1) 7)
 (vector-set! //Mth_/Ord (- //T_/Empty 1) 8)
 (vector-set! //Mth_/Ord (- //T_/Subset 1) 9)
 (vector-set! //Mth_/Ord (- //T_/Member 1) 10)
 (vector-set! //Mth_/Ord (- //T_/Equal 1) 11)
 (vector-set! //Mth_/Ord (- //T_/Not_/Equal 1) 11)
 (vector-set! //Mth_/Ord (- //T_/Less 1) 13)
 (vector-set! //Mth_/Ord (- //T_/Greater 1) 14)
 (vector-set! //Mth_/Ord (- //T_/Less_/Eq 1) 15)
 (vector-set! //Mth_/Ord (- //T_/Greater_/Eq 1) 16)
 (vector-set! //Mth_/Ord (- //T_/If 1) 19)
 (vector-set! //Mth_/Ord (- //T_/Abs 1) 20)
 (vector-set! //Mth_/Ord (- //T_/Sgn 1) 21)
 (vector-set! //Mth_/Ord (- //T_/Int 1) 22)
 (vector-set! //Mth_/Ord (- //T_/Frac 1) 23)
 (vector-set! //Mth_/Ord (- //T_/Max 1) 24)
 (vector-set! //Mth_/Ord (- //T_/Min 1) 25)
 (vector-set! //Mth_/Ord (- //T_/Intersection 1) 26)
 (vector-set! //Mth_/Ord (- //T_/Union 1) 27)
 (vector-set! //Mth_/Ord (- //T_/Set_/Diff 1) 28)
 (vector-set! //Mth_/Ord (- //T_/Powerset 1) 29)
 (vector-set! //Mth_/Ord (- //T_/Set 1) 30)
 (vector-set! //Mth_/Ord (- //T_/Array 1) 31)
 (vector-set! //Mth_/Ord (- //T_/Head 1) 32)
 (vector-set! //Mth_/Ord (- //T_/Tail 1) 33)
 (vector-set! //Mth_/Ord (- //T_/Butlast 1) 34)
 (vector-set! //Mth_/Ord (- //T_/Last 1) 35)
 (vector-set! //Mth_/Ord (- //T_/Length 1) 36)
 (vector-set! //Mth_/Ord (- //T_/Reverse 1) 37)
 (vector-set! //Mth_/Ord (- //T_/Exponent 1) 38)
 (vector-set! //Mth_/Ord (- //T_/Times 1) 39)
 (vector-set! //Mth_/Ord (- //T_/Plus 1) 40)
 (vector-set! //Mth_/Ord (- //T_/Concat 1) 41)
 (vector-set! //Mth_/Ord (- //T_/Div 1) 42)
 (vector-set! //Mth_/Ord (- //T_/Mod 1) 43)
 (vector-set! //Mth_/Ord (- //T_/Sequenceq 1) 44)
 (vector-set! //Mth_/Ord (- //T_/Numberq 1) 45)
 (vector-set! //Mth_/Ord (- //T_/Stringq 1) 46)
 (vector-set! //Mth_/Ord (- //T_/Map 1) 47)
 (vector-set! //Mth_/Ord (- //T_/Reduce 1) 48)
 (vector-set! //Mth_/Ord (- //T_/Forall 1) 49)
 (vector-set! //Mth_/Ord (- //T_/Exists 1) 50)
 (vector-set! //Mth_/Ord (- //T_/Implies 1) 51)
 (vector-set! //Mth_/Ord (- //T_/In 1) 52)
 (vector-set! //Mth_/Ord (- //T_/Not_/In 1) 53)
 (vector-set! //Mth_/Ord (- //T_/Address_/Of 1) 70)
 (vector-set! //Mth_/Ord (- //T_/X_/Funct_/Call 1) 71)
 (vector-set! //Mth_/Ord (- //T_/X_/B/Funct_/Call 1) 72)
 (vector-set! //Mth_/Ord (- //T_/M/W_/Funct_/Call 1) 73)
 (vector-set! //Mth_/Ord (- //T_/M/W_/B/Funct_/Call 1) 74)
 (vector-set! //Mth_/Ord (- //T_/Funct_/Call 1) 75)
 (vector-set! //Mth_/Ord (- //T_/B/Funct_/Call 1) 76)
 (vector-set! //Mth_/Ord (- //T_/Get_n 1) 77)
 (vector-set! //Mth_/Ord (- //T_/Get 1) 78)
 (vector-set! //Mth_/Ord (- //T_/Gethash 1) 79)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Stat 1) 80)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Expn 1) 81)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Cond 1) 82)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Defn 1) 83)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Lvalue 1) 84)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Assign 1) 85)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Guarded 1) 86)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Action 1) 87)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Stats 1) 88)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Expns 1) 89)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Lvalues 1) 91)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Assigns 1) 92)
 (vector-set! //Mth_/Ord (- //T_/Fill_/Defns 1) 93)
 (vector-set! //Mth_/Ord (- //T_/Negate 1) 100)
 (vector-set! //Mth_/Ord (- //T_/Invert 1) 101)
 ; Variables, structs, arrays: 
 (vector-set! //Mth_/Ord (- //T_/Primed_/Var 1) 200)
 (vector-set! //Mth_/Ord (- //T_/Variable 1) 201)
 (vector-set! //Mth_/Ord (- //T_/Struct 1) 202)
 (vector-set! //Mth_/Ord (- //T_/Aref 1) 203)
 (vector-set! //Mth_/Ord (- //T_/Sub_/Seg 1) 204)
 (vector-set! //Mth_/Ord (- //T_/Rel_/Seg 1) 205)
 (vector-set! //Mth_/Ord (- //T_/Final_/Seg 1) 206)
 (vector-set! //Mth_/Ord (- //T_/Expn_/Pat_/One 1) 207)
 (vector-set! //Mth_/Ord (- //T_/Expn_/Pat_/Many 1) 208)
 (vector-set! //Mth_/Ord (- //T_/Expn_/Pat_/Any 1) 209)
 (vector-set! //Mth_/Ord (- //T_/Mem 1) 210)
 (vector-set! //Mth_/Ord (- //T_/Mem_/Seg 1) 211)
 (vector-set! //Mth_/Ord (- //T_/Mem_/Rel 1) 212)
 ; Constants: 
 (vector-set! //Mth_/Ord (- //T_/Expn_/Place 1) 300)
 (vector-set! //Mth_/Ord (- //T_/Var_/Place 1) 301)
 (vector-set! //Mth_/Ord (- //T_/Cond_/Place 1) 302)
 (vector-set! //Mth_/Ord (- //T_/Number 1) 303)
 (vector-set! //Mth_/Ord (- //T_/String 1) 304)
 (vector-set! //Mth_/Ord (- //T_/Hash_/Table 1) 309)
 (vector-set! //Mth_/Ord (- //T_/True 1) 310)
 (vector-set! //Mth_/Ord (- //T_/False 1) 311)
 #t)

(@Mth_Init)
; ----------------------------------------------------------------------- 
; The interface functions which are called by transformations etc.        
; ----------------------------------------------------------------------- 
(set! //Mth_/Default_/Budget 10)
(define (@Set_Budget /n)
 (set! //Mth_/Default_/Budget /n))

(define (@Budget)
 
 //Mth_/Default_/Budget)

(define (@Simplify_Expn //Expn)
 
 (@Simplify //Expn //Mth_/Default_/Budget))

(define (@Simplify_Cond //C)
 
 (@Simplify //C //Mth_/Default_/Budget))

(define (@True? //C)
 
 (= (@ST (@Simplify //C //Mth_/Default_/Budget)) //T_/True))

(define (@False? //C)
 
 (= (@ST (@Simplify //C //Mth_/Default_/Budget)) //T_/False))

(define (@Implies? //C1 //C2)
 
 (= (@ST (@Simplify (@Make //T_/Or '() (list (@Make //T_/Not '() (list //C1)) //C2)) //Mth_/Default_/Budget)) //T_/True))

(define (@And //C1 //C2)
 
 (@Simplify (@Make //T_/And '() (list //C1 //C2)) //Mth_/Default_/Budget))

(define (@Or //C1 //C2)
 
 (@Simplify (@Make //T_/Or '() (list //C1 //C2)) //Mth_/Default_/Budget))

(define (@Not //C)
 
 (@Simplify (@Make //T_/Not '() (list //C)) //Mth_/Default_/Budget))

(define (@Implies //C1 //C2)
 
 (@Simplify (@Make //T_/Or '() (list (@Make //T_/Not '() (list //C1)) //C2)) //Mth_/Default_/Budget))

; Return an equivalent expression with the first two components reversed, 
; eg: (x < y) becomes (y > x), while (x + y + z) becomes (y + x + z) 
(define (@Swap_Expn /expn)
 (let ((/op-save /op)
       (/comps (@Cs /expn))
       (funct-result '()))
  (set! /op (vector-ref //Reverse_/Op (- (@ST /expn) 1)))
  (set! funct-result (if (= /op 0) /expn (@Make /op '() (cons (wsl-ref /comps 2) (cons (wsl-ref /comps 1) (@Final_Seg /comps 3))))))
  (set! /op /op-save)
  funct-result))

(define (@Mth_Swappable? //X)
 
 (> (vector-ref //Reverse_/Op (- (@ST //X) 1)) 0))

; The main simplifier. 
; The `Budget' gives the maximum depth of recursive calls to the simplifier. 
; The algorithm works in three stages: 
; REORGANISE: 
;   (1) Push NOT operators down to lowest level (de Morgan's laws) 
;       (1a) Delete pairs of NOTs and invert NOT (x relop y) 
;   (2) Reverse T_Greater and T_Greater_Eq 
;   (3) Flatten associative operators 
;   (4) Evaluate `constant' components 
;   (5) Sort components of commutative operators 
;       (5a) If a `power' op is available: merge groups 
;            of identical elements and re-sort 
;   (6) Merge groups of constant components and check for identity or zero 
;   (7) Multiply out * over + and AND over OR 
;       (7a) Re-flatten and re-sort if necessary 
; SIMPLIFY: 
;   (1) Apply all applicable patterns 
;   (2) Evaluate `constant' components 
;   (3) If some patterns applied then: 
;       (3a) Reduce budget. If still >0 then reorganise and simplify again 
; FACTORISE: 
;   (1) Take out common factors from + and OR compounds (re-ordering as necessary) 
;       (1a) Simplify the resulting factors (with a smaller budget) if they are compounds 
;   (2) Convert A*(1/B) to A/B and A+(-B) to A-B 
;   (3) Sort relations to put constants on the RHS, eg: 0<x becomes x>0 
;   (4) Sort * operations to put constants first, and + to put constants last 
;       eg: 3*x but x+3 
; Note that the first two stages are in a loop, while the third state 
; is a `finishing off' step which is applied once. 
; We can only simplify expressions or conditions if Eval_Op[@ST(I)] > 0 
(define (@Simplify //I-par //Budget-par)
 (let ((//Budget-save //Budget)
       (//I-save //I)
       (/new-save /new)
       (/level (@Dtable_Get //I-par //Qry_/Simple))
       (funct-result '()))
  (set! //Budget //Budget-par)
  (set! //I //I-par)
  (set! /new //I-par)
  (cond
   ((and (not (null? /level)) (>= (@Dtable_Value_Part /level) //Budget))
    #t)
   ((or (and (not (= (@GT //I) //T_/Condition)) (not (= (@GT //I) //T_/Expression))) (<= (vector-ref //Eval_/Op (- (@ST //I) 1)) 0))
    (cond
     ((@Cs? //I)
      ; Call @Simplify recursively on the components: 
      (let ((/comps '()))
       (for-in /comp (@Cs //I) 
        (set! /comps (cons (@Simplify /comp //Budget) /comps)))
       (set! /new (@Make (@ST //I) '() (reverse /comps)))
       (@Dtable_Put /new //Qry_/Simple //Budget)))))
   (#t
    (cond
     ((= (@ST //I) //T_/If)
      (let ((/comps '()))
       (for-in /comp (@Cs //I) 
        (set! /comps (cons (@Simplify /comp //Budget) /comps)))
       (set! //I (@Make (@ST //I) '() (reverse /comps))))))
    (let ((/old-save /old)
          (//Orig_/Budget //Budget)
          (//Orig_/Size (@Total_Size //I))
          (//Orig //I))
     (set! /old '())
     (@Edit)
     (@New_Program //I)
     (@Simplify_Bit_Ops)
     (cond
      ((> //Budget 5)
       (@Simplify_BFunct_Calls //Budget)))
     (set! /fl_flag2 0)
     (while (= /fl_flag2 0) 
      (cond
       ((= //Budget 0)
        (set! /fl_flag2 1))
       (#t
        ; REORGANISE 
        ; push down NOTs, delete pairs of NOTs and reverse >/>= 
        ; Also replace IMPLIES? by the equivalent: 
        (cond
         ((and (= (@GT (@I)) //T_/Condition) (@Cs? (@I)))
          (@Paste_Over (@Mth_De_Morgan (@I)))))
        (set! /fl_flag1 0)
        (while (= /fl_flag1 0) 
         (begin
          (@Mth_Flatten)
          (@Mth_Evaluate)
          (@Mth_Sort_Merge)
          (set! /old (@Program))
          (@Foreach_Cond /foreach-maths-1 0 (@AS_Type) 0)
          (cond
           ((null? (@Program))
            (@New_Program (@Skips))))
          ; We can run into exponential growth if we try to expand 
          ; when the expression/condition is already `large'. 
          ; The value 2 * Budget was determined experimentally 
          (cond
           ((< (@Total_Size (@I)) (* 2 //Budget))
            (@Mth_Expand //Budget)))
          (@Foreach_Cond /foreach-maths-2 0 (@AS_Type) 0)
          (cond
           ((null? (@Program))
            (@New_Program (@Skips))))
          (cond
           ((> (@Total_Size (@Program)) (* //Budget //Orig_/Size))
            (@Paste_Over //Orig)
            (set! /fl_flag1 2))
           ((@Equal? /old (@Program))
            (set! /fl_flag1 1))
           (#t
            (set! /fl_flag1 0)))))
        (cond
         ((= /fl_flag1 2)
          (set! /fl_flag2 1))
         (#t
          ; SIMPLIFY 
          (set! /old (@Program))
          (cond
           (#f
            (display-list "Patterns 1: " //Budget)
            (@PP_Item (@I) 80 "")
            (display-list "-----")))
          (cond
           ((>= (@Total_Size (@I)) 3)
            (@Mth_Patterns //Budget)))
          (cond
           (#f
            (display-list "Patterns 2:")
            (@PP_Item (@I) 80 "")
            (display-list "-----")))
          (@Mth_Evaluate)
          (cond
           ((@Equal? /old (@Program))
            (set! /fl_flag2 1))
           (#t
            (set! /fl_flag2 0))))))))
     ; FACTORISE 
     (set! //Budget //Orig_/Budget)
     (@Mth_Factorise //Budget)
     (@Mth_Flatten)
     ; Simplify A*(1/B) and A+(-B), sort relations, *, and + operations 
     (@Mth_Prettify)
     (cond
      ((not (@Equal? (@Program) //Orig))
       ; Hack 
       (@Mth_Flatten)
       (@Mth_Evaluate)
       (@Mth_Sort_Merge)
       (@Mth_Factorise //Budget)
       (@Mth_Flatten)
       (@Mth_Prettify)))
     (set! /new (@Program))
     (cond
      ((> //Budget 10)
       (set! /new (@Simplify /new (- //Budget 10)))
       (cond
        ((> (@Total_Size /new) (@Total_Size (@Program)))
         (set! /new (@Program))))))
     (@Undo_Edit)
     (@Dtable_Put /new //Qry_/Simple //Orig_/Budget)
     (set! /old /old-save))))
  (set! funct-result /new)
  (set! //Budget //Budget-save)
  (set! //I //I-save)
  (set! /new /new-save)
  funct-result))

; Evaluate operators all of whose arguments are constants: 
; Note: T_Length, T_Reverse and T_Concat do not require the components 
; of the argument to be constants, but the other set operations do. 
(define (@Mth_Evaluate)
 (let ((//S/T-save //S/T)
       (/v-save /v)
       (/new-save /new))
  (set! //S/T 0)
  (set! /v '())
  (set! /new '())
  (@Foreach_Expn /foreach-maths-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Cond /foreach-maths-4 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //S/T //S/T-save)
  (set! /v /v-save)
  (set! /new /new-save)))

(define (@Mth_Eval_Exponent //X //Y)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R '())
  (cond
   ((or (not (= (@ST //X) //T_/Number)) (not (= (@ST //Y) //T_/Number)) (<= (@V //X) 0) (not (number? (@V //X))) (not (number? (@V //Y))) (> (@V //Y) 27))
    (set! //R (@Make //T_/Exponent '() (list //X //Y))))
   (#t
    (set! //R (@Make //T_/Number (integer-expt (@V //X) (@V //Y)) '()))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

; Convert a list of values to a T_Sequence item: 
(define (@Mth_Sequence //L)
 (let ((/comps '())
       (/val-save /val)
       (//R-save //R)
       (funct-result '()))
  (set! /val '())
  (set! //R '())
  (cond
   ((string? //L)
    ; ++ on strings returns a string 
    (set! //R (@Make //T_/String //L '())))
   (#t
    (for-in /val //L 
     (cond
      ((number? /val)
       (set! /comps (cons (@Make //T_/Number /val '()) /comps)))
      ((string? /val)
       (set! /comps (cons (@Make //T_/String /val '()) /comps)))
      ((sequence? /val)
       (set! /comps (cons (@Mth_Sequence /val) /comps)))
      (#t
       (display-list "ERROR: Unknown value in Mth_Sequence: " /val))))
    (set! //R (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() (reverse /comps)))))))
  (set! funct-result //R)
  (set! /val /val-save)
  (set! //R //R-save)
  funct-result))

; Check that the list of items consists only of constants: 
(define (@Mth_Constants? //L /type)
 (let ((//O/K 0))
  (cond
   ((and (= /type 1) (@Mth_All_Constants? //L))
    (set! //O/K 1))
   ((and (= /type 2) (@Mth_All_Numbers? //L))
    (set! //O/K 1)))
  (= //O/K 1)))

(define (@Mth_All_Constants? //L)
 (let ((//O/K 1))
  (while (and (= //O/K 1) (not (null? //L))) 
   (cond
    ((not (@Mth_Constant? (car //L)))
     (set! //O/K 0))
    (#t
     (set! //L (cdr //L)))))
  (= //O/K 1)))

(define (@Mth_Constant? //I)
 
 (or (= (@ST //I) //T_/Number) (= (@ST //I) //T_/String) (= (@ST //I) //T_/True) (= (@ST //I) //T_/False) (and (= (@ST //I) //T_/Sequence) (@Mth_All_Constants? (@Cs (@Get_n //I 1)))) (and (= (@ST //I) //T_/Expressions) (@Mth_All_Constants? (@Cs //I)))))

(define (@Mth_Values //L)
 (let ((//R-save //R)
       (//I-save //I)
       (funct-result '()))
  (set! //R '())
  (set! //I '())
  (for-in //I //L 
   (cond
    ((= (@ST //I) //T_/Sequence)
     (set! //R (cons (@Mth_Values (@Cs (@Get_n //I 1))) //R)))
    (#t
     (set! //R (cons (@Value //I) //R)))))
  (set! funct-result (reverse //R))
  (set! //R //R-save)
  (set! //I //I-save)
  funct-result))

(define (@Mth_Types //L)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R '())
  (while (not (null? //L)) 
   (begin
    (set! //R (cons (@ST (car //L)) //R))
    (set! //L (cdr //L))))
  (set! funct-result (reverse //R))
  (set! //R //R-save)
  funct-result))

; Check for a list of all numbers: 
(define (@Mth_All_Numbers? //L)
 (let ((//O/K 1))
  (while (and (= //O/K 1) (not (null? //L))) 
   (cond
    ((and (= (@ST (car //L)) //T_/String) (@Starts_With? (@V (car //L)) "hex 0x") (or (< (string-length (@V (car //L))) 14) (@Starts_With? (@V (car //L)) "hex 0x00")))
     (set! //L (cdr //L)))
    ((= (@ST (car //L)) //T_/Number)
     (set! //L (cdr //L)))
    (#t
     (set! //O/K 0))))
  (= //O/K 1)))

; ----------------------------------------------------------------------- 
; Flatten assocative operators by removing nested parentheses; e.g.       
; ((a+b) + c) --> (a+b+c). This function also deals with `-' and `/'      
; operations by replacing them with `Negate' and `Invert' operations,     
; respectively.                                                           
; ----------------------------------------------------------------------- 
(define (@Mth_Flatten)
 (let ((//S/T-save //S/T)
       (/new-save /new)
       (/x-save /x))
  (set! //S/T '())
  (set! /new '())
  (set! /x '())
  (@Foreach_Expn /foreach-maths-5 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (cond
   ((= (@GT (@I)) //T_/Condition)
    (@Foreach_Cond /foreach-maths-6 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (set! //S/T //S/T-save)
  (set! /new /new-save)
  (set! /x /x-save)))

; Negate an item: converting (a + b) to (-a) + (-b) 
(define (@Mth_Negate //I)
 
 (if (= (@ST //I) //T_/Plus) (@Make //T_/Plus '() (my-map @Mth_Negate (@Cs //I))) (if (or (= (@ST //I) //T_/Times) (= (@ST //I) //T_/Divide)) (@Make (@ST //I) '() (cons (@Mth_Negate (@Get_n //I 1)) (cdr (@Cs //I)))) (@Make //T_/Negate '() (list //I)))))

; Invert an item: converting (a * b) to (1/a) * (1/b) 
(define (@Mth_Invert //I)
 
 (if (= (@ST //I) //T_/Times) (@Make //T_/Times '() (my-map @Mth_Invert (@Cs //I))) (@Make //T_/Invert '() (list //I))))

; Push down NOTs, delete pairs of NOTs and reverse >/>= 
(define (@Mth_De_Morgan //I-par)
 (let ((//I-save //I)
       (//R-save //R)
       (funct-result '()))
  (set! //I //I-par)
  (set! //R '())
  (cond
   ((= (@ST //I) //T_/Not)
    (cond
     ((or (= (@ST (@Get_n //I 1)) //T_/And) (= (@ST (@Get_n //I 1)) //T_/Or))
      (set! //R (@Make (vector-ref //Inverse_/Op (- (@ST (@Get_n //I 1)) 1)) '() (my-map @Mth_De_Morgan_Not (@Cs (@Get_n //I 1))))))
     ((> (vector-ref //Inverse_/Op (- (@ST (@Get_n //I 1)) 1)) 0)
      (cond
       ((or (= (vector-ref //Inverse_/Op (- (@ST (@Get_n //I 1)) 1)) //T_/Greater) (= (vector-ref //Inverse_/Op (- (@ST (@Get_n //I 1)) 1)) //T_/Greater_/Eq))
        (set! //R (@Make (vector-ref //Reverse_/Op (- (vector-ref //Inverse_/Op (- (@ST (@Get_n //I 1)) 1)) 1)) '() (list (@Get //I (list 1 2)) (@Get //I (list 1 1))))))
       (#t
        (set! //R (@Make (vector-ref //Inverse_/Op (- (@ST (@Get_n //I 1)) 1)) '() (@Cs (@Get_n //I 1)))))))
     ((= (@ST (@Get_n //I 1)) //T_/Implies)
      (set! //R (@Make //T_/And '() (list (@Mth_De_Morgan (@Get //I (list 1 1))) (@Mth_De_Morgan_Not (@Get //I (list 1 2)))))))
     ((= (@ST (@Get_n //I 1)) //T_/Not)
      (set! //R (@Mth_De_Morgan (@Get //I (list 1 1)))))
     (#t
      (set! //R //I))))
   ((or (= (@ST //I) //T_/Greater) (= (@ST //I) //T_/Greater_/Eq))
    (set! //R (@Make (vector-ref //Reverse_/Op (- (@ST //I) 1)) '() (list (@Get_n //I 2) (@Get_n //I 1)))))
   ((= (@ST //I) //T_/Implies)
    (set! //R (@Make //T_/Or '() (list (@Mth_De_Morgan_Not (@Get_n //I 1)) (@Mth_De_Morgan (@Get_n //I 2))))))
   ((or (= (@ST //I) //T_/And) (= (@ST //I) //T_/Or))
    (set! //R (@Make (@ST //I) '() (my-map @Mth_De_Morgan (@Cs //I)))))
   (#t
    (set! //R //I)))
  (set! funct-result //R)
  (set! //I //I-save)
  (set! //R //R-save)
  funct-result))

(define (@Mth_De_Morgan_Not //I-par)
 (let ((//I-save //I)
       (//R-save //R)
       (funct-result '()))
  (set! //I //I-par)
  (set! //R '())
  (cond
   ((= (@ST //I) //T_/Not)
    (set! //R (@Mth_De_Morgan (@Get_n //I 1))))
   ((or (= (@ST //I) //T_/And) (= (@ST //I) //T_/Or))
    (set! //R (@Make (vector-ref //Inverse_/Op (- (@ST //I) 1)) '() (my-map @Mth_De_Morgan_Not (@Cs //I)))))
   ((> (vector-ref //Inverse_/Op (- (@ST //I) 1)) 0)
    (cond
     ((or (= (vector-ref //Inverse_/Op (- (@ST //I) 1)) //T_/Greater) (= (vector-ref //Inverse_/Op (- (@ST //I) 1)) //T_/Greater_/Eq))
      (set! //R (@Make (vector-ref //Reverse_/Op (- (vector-ref //Inverse_/Op (- (@ST //I) 1)) 1)) '() (list (@Get_n //I 2) (@Get_n //I 1)))))
     (#t
      (set! //R (@Make (vector-ref //Inverse_/Op (- (@ST //I) 1)) '() (@Cs //I))))))
   ((= (@ST //I) //T_/Implies)
    (set! //R (@Make //T_/And '() (list (@Mth_De_Morgan (@Get_n //I 1)) (@Mth_De_Morgan_Not (@Get_n //I 2))))))
   (#t
    (set! //R (@Make //T_/Not '() (list //I)))))
  (set! funct-result //R)
  (set! //I //I-save)
  (set! //R //R-save)
  funct-result))

; Sort components of commutative operators and combine groups of 
; so that identical terms: for example  (a+a+b+a+b) = ((a*3) + (b*2)). 
; This function also deals with identity elements and annihilating elements, 
; for example a+0 = 0 and a*0 = 0 
; If a power op was inserted and there are two or more than arguments remaining, 
; then the arguments need to be re-sorted (to put the power op in the right place) 
(define (@Mth_Sort_Merge)
 (let ((/count-save /count))
  (set! /count 0)
  ; Check for two or more unsafe tests in an AND or OR :
  (@Foreach_Expn /foreach-maths-7 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Cond /foreach-maths-8 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /count /count-save)))

(define (@Mth_Sort_Merge_Item //I-par //S/T-par /count-par)
 (let ((/count-save /count)
       (//S/T-save //S/T)
       (//I-save //I)
       (//R-save //R)
       (/comps (@Cs //I-par))
       (/new-save /new)
       (/n-save /n)
       (/re_sort 0)
       (/nums '())
       (/power (vector-ref //Power_/Op (- //S/T-par 1)))
       (/id (vector-ref //Identity_/Value (- //S/T-par 1)))
       (/zero (vector-ref //Zero_/Value (- //S/T-par 1)))
       (funct-result '()))
  (set! /count /count-par)
  (set! //S/T //S/T-par)
  (set! //I //I-par)
  (set! //R '())
  (set! /new '())
  (set! /n 0)
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (set! /re_sort 0)
    (cond
     ((< /count 2)
      (set! /comps (@Mth_Sort /comps))))
    (set! /new '())
    (while (not (null? /comps)) 
     (cond
      ((and (not (null? /id)) (@Equal? (car /comps) /id))
       (set! /comps (cdr /comps)))
      ((and (not (null? /zero)) (@Equal? (car /comps) /zero))
       (set! /new (list /zero))
       (set! /comps '()))
      ((and (= //S/T //T_/Times) (= (@ST (car /comps)) //T_/Invert) (= (@ST (@Get_n (car /comps) 1)) //T_/Number))
       ; Combine numeric operands, note that (-n) has been evaluated 
       ; Any numbers should directly follow any inverted numbers 
       (let ((/num 1)
             (/denom 1))
        (while (and (not (null? /comps)) (= (@ST (car /comps)) //T_/Invert) (= (@ST (@Get_n (car /comps) 1)) //T_/Number)) 
         (begin
          (set! /denom (* /denom (@V (@Get_n (car /comps) 1))))
          (set! /comps (cdr /comps))))
        (while (and (not (null? /comps)) (= (@ST (car /comps)) //T_/Number)) 
         (begin
          (set! /num (* /num (@V (car /comps))))
          (set! /comps (cdr /comps))))
        (cond
         ((= /num 1)
          (set! /new (cons (@Make //T_/Invert '() (list (@Make //T_/Number /denom '()))) /new)))
         ((and (> (abs /num) (abs /denom)) (not (= /denom 0)) (= (modulo /num /denom) 0))
          (set! /new (cons (@Make //T_/Number (/ /num /denom) '()) /new)))
         (#t
          (set! /new (cons (@Make //T_/Invert '() (list (@Make //T_/Number /denom '()))) (cons (@Make //T_/Number /num '()) /new)))))))
      ((= (@ST (car /comps)) //T_/Number)
       ; Combine numeric operands, note that (-n) has been evaluated 
       (set! /nums '())
       (while (and (not (null? /comps)) (= (@ST (car /comps)) //T_/Number)) 
        (begin
         (set! /nums (cons (car /comps) /nums))
         (set! /comps (cdr /comps))))
       (cond
        ((= (gen-length /nums) 1)
         (set! /new (concat /nums /new)))
        (#t
         ; Evaluate the numeric component 
         (@Edit)
         (@New_Program (@Make //S/T '() /nums))
         (@Mth_Evaluate)
         (cond
          ((not (@Equal? (@Program) /id))
           (set! /new (cons (@Program) /new))))
         (@Undo_Edit))))
      ((> /power 0)
       ; Check for identical elements: 
       (set! /n 1)
       (while (and (not (null? (cdr /comps))) (@Equal? (car /comps) (car (cdr /comps)))) 
        (begin
         (set! /n (+ /n 1))
         (set! /comps (cdr /comps))))
       (cond
        ((or (= /n 1) (= /power //S/T))
         ; There is only one element, or the operator is idempotent 
         (set! /new (cons (car /comps) /new)))
        (#t
         (set! /new (cons (@Make /power '() (list (car /comps) (@Make //T_/Number /n '()))) /new))
         (set! /re_sort 1)))
       (set! /comps (cdr /comps)))
      (#t
       (set! /new (cons (car /comps) /new))
       (set! /comps (cdr /comps)))))
    (cond
     ((null? /new)
      ; Everything was an identity (so there must be an identity for this ST!) 
      (set! //R /id)
      (set! /fl_flag1 1))
     ((= (gen-length /new) 1)
      (set! //R (car /new))
      (set! /fl_flag1 1))
     ((= /re_sort 0)
      (set! //R (@Make //S/T '() (reverse /new)))
      (set! /fl_flag1 1))
     (#t
      (set! /comps (reverse /new))
      (set! /fl_flag1 0)))))
  (set! funct-result //R)
  (set! /count /count-save)
  (set! //S/T //S/T-save)
  (set! //I //I-save)
  (set! //R //R-save)
  (set! /new /new-save)
  (set! /n /n-save)
  funct-result))

(define (@Mth_Sort //L)
 
 (if (or (null? //L) (null? (cdr //L))) //L (if (= (gen-length //L) 2) (if (@Mth_Lt? (wsl-ref //L 2) (wsl-ref //L 1)) (list (wsl-ref //L 2) (wsl-ref //L 1)) //L) (@Mth_Merge (@Mth_Sort (@Sub_Seg //L 1 (quotient (gen-length //L) 2))) (@Mth_Sort (@Final_Seg //L (+ (quotient (gen-length //L) 2) 1)))))))

; Merge two sorted lists using @Mth_Lt? 
(define (@Mth_Merge //L1 //L2)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R '())
  (while (and (not (null? //L1)) (not (null? //L2))) 
   (cond
    ((@Mth_Lt? (car //L2) (car //L1))
     (set! //R (cons (car //L2) //R))
     (set! //L2 (cdr //L2)))
    (#t
     (set! //R (cons (car //L1) //R))
     (set! //L1 (cdr //L1)))))
  ; One of L1 or L2 is empty, so prepend the other list to R 
  (while (not (null? //L1)) 
   (begin
    (set! //R (cons (car //L1) //R))
    (set! //L1 (cdr //L1))))
  (while (not (null? //L2)) 
   (begin
    (set! //R (cons (car //L2) //R))
    (set! //L2 (cdr //L2))))
  (set! funct-result (reverse //R))
  (set! //R //R-save)
  funct-result))

; By sorting first on the total size we ensure that (-x) comes before x 
; no matter what the type of x is. We put larger components first: 
(define (@Mth_Lt? /x-par /y)
 (let ((/x-save /x)
       (//O/K 0)
       (funct-result '()))
  (set! /x /x-par)
  (cond
   ((> (@Total_Size /x) (@Total_Size /y))
    (set! //O/K 1))
   ((< (@Total_Size /x) (@Total_Size /y))
    (set! //O/K 0))
   ((< (vector-ref //Mth_/Ord (- (@ST /x) 1)) (vector-ref //Mth_/Ord (- (@ST /y) 1)))
    (set! //O/K 1))
   ((> (vector-ref //Mth_/Ord (- (@ST /x) 1)) (vector-ref //Mth_/Ord (- (@ST /y) 1)))
    (set! //O/K 0))
   ((< (@Size /x) (@Size /y))
    (set! //O/K 1))
   ((> (@Size /x) (@Size /y))
    (set! //O/K 0))
   ((@Has_Value_Type? (@ST /x))
    (cond
     ((or (= (@ST /x) //T_/Number) (= (@ST /x) //T_/Exit))
      (cond
       ((< (@V /x) (@V /y))
        (set! //O/K 1))))
     ((or (= (@ST /x) //T_/String) (= (@ST /x) //T_/Comment))
      (cond
       ((@String_Less? (@V /x) (@V /y))
        (set! //O/K 1))))
     ((@String_Less? (@N_String (@V /x)) (@N_String (@V /y)))
      (set! //O/K 1))))
   ((not (@Cs? /x))
    (set! //O/K 1))
   ((not (@Cs? /y))
    (set! //O/K 0))
   ((@Mth_Lt? (@Get_n /x 1) (@Get_n /y 1))
    (set! //O/K 1))
   ((@Equal? (@Get_n /x 1) (@Get_n /y 1))
    (let ((/c1 (cdr (@Cs /x)))
          (/c2 (cdr (@Cs /y))))
     (set! //O/K 0)
     (while (not (null? /c1)) 
      (cond
       ((@Mth_Lt? (car /c1) (car /c2))
        (set! //O/K 1)
        (set! /c1 '()))
       ((@Equal? (car /c1) (car /c2))
        (set! /c1 (cdr /c1))
        (set! /c2 (cdr /c2)))
       (#t
        (set! //O/K 0)
        (set! /c1 '()))))))
   (#t
    (set! //O/K 0)))
  (set! funct-result (= //O/K 1))
  (set! /x /x-save)
  funct-result))

; Check for an AND or OR with an invertable relation at the top level. 
; Check for other copies of the relation or inverse relation in other components 
; and replace by TRUE or FALSE as appropriate. 
; Eg: (x = y OR p = q) AND x <> y simplifies to (FALSE OR p = q) AND x <> y 
; since if x = y the whole thing is false anyway. 
(define (@Mth_Duplicate_Relations)
 (let ((/top (@ST (@I)))
       (/comp-save /comp)
       (/rels '())
       (/rel-save /rel)
       (/new-save /new)
       (/same-save /same)
       (/diff-save /diff)
       (/comps (@Cs (@I)))
       (/change-save /change))
  (set! /comp '())
  (set! /rel '())
  (set! /new '())
  (set! /same '())
  (set! /diff '())
  (set! /change 0)
  (cond
   ((= /top //T_/And)
    (set! /same //Mth_/True)
    (set! /diff //Mth_/False))
   (#t
    (set! /same //Mth_/False)
    (set! /diff //Mth_/True)))
  (for-in /comp /comps 
   (cond
    ((or (= (@ST /comp) //T_/Equal) (= (@ST /comp) //T_/Not_/Equal) (= (@ST /comp) //T_/Less) (= (@ST /comp) //T_/Less_/Eq))
     (set! /rels (cons /comp /rels)))))
  (cond
   ((not (null? /rels))
    (@Edit)
    (for-in /rel /rels 
     (begin
      (set! /new '())
      (for-in /comp /comps 
       (cond
        ((@Equal? /comp /rel)
         (set! /new (cons /comp /new)))
        ((= (@ST /comp) (vector-ref //Reverse_/Op (- (@ST /rel) 1)))
         (cond
          ((and (@Equal? (@Get_n /comp 2) (@Get_n /rel 1)) (@Equal? (@Get_n /comp 1) (@Get_n /rel 2)))
           (set! /change 1)
           (set! /new (cons /same /new)))
          (#t
           (set! /new (cons /comp /new)))))
        ((= (@ST /comp) (vector-ref //Inverse_/Op (- (@ST /rel) 1)))
         (cond
          ((and (@Equal? (@Get_n /comp 2) (@Get_n /rel 2)) (@Equal? (@Get_n /comp 1) (@Get_n /rel 1)))
           (set! /change 1)
           (set! /new (cons /diff /new)))
          (#t
           (set! /new (cons /comp /new)))))
        ((= (@ST /comp) (vector-ref //Inverse_/Op (- (vector-ref //Reverse_/Op (- (@ST /rel) 1)) 1)))
         (cond
          ((and (@Equal? (@Get_n /comp 2) (@Get_n /rel 1)) (@Equal? (@Get_n /comp 1) (@Get_n /rel 2)))
           (set! /change 1)
           (set! /new (cons /diff /new)))
          (#t
           (set! /new (cons /comp /new)))))
        ((and (or (= (@ST /comp) //T_/Equal) (= (@ST /comp) //T_/Not_/Equal) (= (@ST /comp) //T_/Less) (= (@ST /comp) //T_/Less_/Eq)) (@Equal? (@Get_n /comp 1) (@Get_n /rel 1)) (@Equal? (@Get_n /comp 2) (@Get_n /rel 2)))
         (set! /new (cons (@Mth_Same_Args /top /comp /rel 0) /new))
         (cond
          ((not (@Equal? (car /new) /comp))
           (set! /change 1))))
        ((and (or (= (@ST /comp) //T_/Equal) (= (@ST /comp) //T_/Not_/Equal) (= (@ST /comp) //T_/Less) (= (@ST /comp) //T_/Less_/Eq)) (@Equal? (@Get_n /comp 1) (@Get_n /rel 2)) (@Equal? (@Get_n /comp 2) (@Get_n /rel 1)))
         (set! /new (cons (@Mth_Same_Args /top /comp /rel 1) /new))
         (cond
          ((not (@Equal? (car /new) /comp))
           (set! /change 1))))
        ((or (= (@ST /comp) //T_/Or) (= (@ST /comp) //T_/And))
         (set! /change (@Mth_Duplicate_Relations_Sub  /comp /rel /change))
         (set! /new (cons (@Program) /new)))
        (#t
         (set! /new (cons /comp /new)))))
      (set! /comps (reverse /new))))
    (@Undo_Edit)
    (cond
     ((= /change 1)
      (@Paste_Over (@Make /top '() /comps))
      ; Get rid of any inserted TRUE/FALSE components: 
      (@Mth_Sort_Merge)))))
  (set! /comp /comp-save)
  (set! /rel /rel-save)
  (set! /new /new-save)
  (set! /same /same-save)
  (set! /diff /diff-save)
  (set! /change /change-save)))

(define (@Mth_Duplicate_Relations_Sub /comp-par /rel-par /change-par)
 (let ((/change-save /change)
       (/rel-save /rel)
       (/comp-save /comp)
       (funct-result '()))
  (set! /change /change-par)
  (set! /rel /rel-par)
  (set! /comp /comp-par)
  (@New_Program /comp)
  (@Foreach_Cond /foreach-maths-9 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result /change)
  (set! /change /change-save)
  (set! /rel /rel-save)
  (set! /comp /comp-save)
  funct-result))

; rel and comp have the same two arguments (rev = 1 if args are reversed) 
; Return the new comp (note: rel will still be there) 
(define (@Mth_Same_Args /top /comp-par /rel-par /rev)
 (let ((/rel-save /rel)
       (/comp-save /comp)
       (//R-save //R)
       (funct-result '()))
  (set! /rel /rel-par)
  (set! /comp /comp-par)
  (set! //R /comp-par)
  (cond
   ((= /top //T_/And)
    (cond
     ((= (@ST /rel) //T_/Equal)
      (cond
       ((or (= (@ST /comp) //T_/Not_/Equal) (= (@ST /comp) //T_/Less))
        (set! //R //Mth_/False))
       (#t
        (set! //R //Mth_/True))))
     ((= (@ST /rel) //T_/Not_/Equal)
      ; Do these cases the other way round 
     )
     ((and (= (@ST /rel) //T_/Less) (= /rev 0))
      (cond
       ((= (@ST /comp) //T_/Equal)
        (set! //R //Mth_/False))
       ((= (@ST /comp) //T_/Not_/Equal)
        (set! //R //Mth_/True))
       ((= (@ST /comp) //T_/Less_/Eq)
        (set! //R //Mth_/True))))
     ((= (@ST /rel) //T_/Less_/Eq)
      (cond
       ((= (@ST /comp) //T_/Not_/Equal)
        (set! //R (@Make //T_/Less '() (list (@Get_n /rel 1) (@Get_n /rel 2)))))
       ((and (= (@ST /comp) //T_/Less_/Eq) (= /rev 1))
        (set! //R (@Make //T_/Equal '() (list (@Get_n /rel 1) (@Get_n /rel 2)))))))
     ((and (= (@ST /rel) //T_/Less) (= /rev 1))
      (cond
       ((= (@ST /comp) //T_/Equal)
        (set! //R //Mth_/False))
       ((= (@ST /comp) //T_/Not_/Equal)
        (set! //R //Mth_/True))
       ((= (@ST /comp) //T_/Less)
        (set! //R //Mth_/False))
       ((= (@ST /comp) //T_/Less_/Eq)
        (set! //R //Mth_/False))))))
   (#t
    (cond
     ((= (@ST /comp) //T_/Equal)
      ; Do these the other way round 
     )
     ((= (@ST /rel) //T_/Not_/Equal)
      (cond
       ((= (@ST /comp) //T_/Equal)
        (set! //R //Mth_/True))
       ((= (@ST /comp) //T_/Less)
        (set! //R //Mth_/False))
       ((= (@ST /comp) //T_/Less_/Eq)
        (set! //R //Mth_/True))))
     ((and (= (@ST /rel) //T_/Less) (= /rev 0))
      (cond
       ((= (@ST /comp) //T_/Equal)
        (set! //R (@Make //T_/Less_/Eq '() (list (@Get_n /rev 1) (@Get_n /rev 2)))))))
     ((and (= (@ST /rel) //T_/Less_/Eq) (= /rev 0))
      (cond
       ((= (@ST /comp) //T_/Equal)
        (set! //R //Mth_/False))
       ((= (@ST /comp) //T_/Not_/Equal)
        (set! //R //Mth_/True))
       ((= (@ST /comp) //T_/Less)
        (set! //R //Mth_/False))))
     ((and (= (@ST /rel) //T_/Less_/Eq) (= /rev 1))
      (cond
       ((= (@ST /comp) //T_/Equal)
        (set! //R //Mth_/False))
       (#t
        (set! //R //Mth_/True)))))))
  (set! funct-result //R)
  (set! /rel /rel-save)
  (set! /comp /comp-save)
  (set! //R //R-save)
  funct-result))

; Check for an AND at the top level which gives a value to a variable 
; where the variable appears in the other components, eg: X = 1 AND (X > 2) 
; Replace the variable by its known value and see if the result is 
; TRUE or FALSE (ie we get 1 = 1 AND 1 > 2 which is FALSE) 
(define (@Mth_Known_Value1 //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((/comp-save /comp)
        (/vars-save /vars)
        (/vals-save /vals)
        (/done 0)
        (//S/T-save //S/T))
   (set! /comp '())
   (set! /vars '())
   (set! /vals '())
   (set! //S/T 0)
   (for-in /comp (@Cs (@I)) 
    (cond
     ((= (@ST /comp) //T_/Equal)
      (cond
       ((and (or (= (@ST (@Get_n /comp 2)) //T_/Number) (= (@ST (@Get_n /comp 2)) //T_/String)) (> (@Mth_Count_Occs (@Get_n /comp 1)) 1))
        (set! /vars (cons (@Get_n /comp 1) /vars))
        (set! /vals (cons (@Get_n /comp 2) /vals)))))))
   (while (and (not (null? /vars)) (= /done 0)) 
    (begin
     (@Edit)
     (@Foreach_Expn /foreach-maths-10 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! //S/T (@ST (@Simplify (@I) (quotient //Budget 2))))
     (cond
      ((= //S/T //T_/True)
       (@Paste_Over (@Make //T_/Equal '() (list (car /vars) (car /vals))))
       (set! /done 1)
       (@End_Edit))
      ((= //S/T //T_/False)
       (@Paste_Over (@Make //T_/False '() '()))
       (set! /done 1)
       (@End_Edit))
      (#t
       (@Undo_Edit)))
     (set! /vals (cdr /vals))
     (set! /vars (cdr /vars))))
   (set! /comp /comp-save)
   (set! /vars /vars-save)
   (set! /vals /vals-save)
   (set! //S/T //S/T-save))
  (set! //Budget //Budget-save)))

; Check for an OR at the top level which denies a value to a variable 
; where the variable appears in the other components, eg: X <> 1 OR (X > 2) 
; Replace the variable the value and see if the result is TRUE or FALSE 
; (ie we get 1 = 1 AND 1 > 2 which is FALSE) 
(define (@Mth_Known_Value2 //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((/comp-save /comp)
        (/vars-save /vars)
        (/vals-save /vals)
        (/done 0)
        (//S/T-save //S/T))
   (set! /comp '())
   (set! /vars '())
   (set! /vals '())
   (set! //S/T 0)
   (for-in /comp (@Cs (@I)) 
    (cond
     ((= (@ST /comp) //T_/Not_/Equal)
      (cond
       ((and (or (= (@ST (@Get_n /comp 2)) //T_/Number) (= (@ST (@Get_n /comp 2)) //T_/String)) (> (@Mth_Count_Occs (@Get_n /comp 1)) 1))
        (set! /vars (cons (@Get_n /comp 1) /vars))
        (set! /vals (cons (@Get_n /comp 2) /vals)))))))
   (while (and (not (null? /vars)) (= /done 0)) 
    (begin
     (@Edit)
     (@Foreach_Expn /foreach-maths-11 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! //S/T (@ST (@Simplify (@I) (quotient //Budget 2))))
     (cond
      ((= //S/T //T_/True)
       (@Paste_Over (@Make //T_/True '() '()))
       (set! /done 1)
       (@End_Edit))
      ((= //S/T //T_/False)
       (@Paste_Over (@Make //T_/Not_/Equal '() (list (car /vars) (car /vals))))
       (set! /done 1)
       (@End_Edit))
      (#t
       (@Undo_Edit)))
     (set! /vals (cdr /vals))
     (set! /vars (cdr /vars))))
   (set! /comp /comp-save)
   (set! /vars /vars-save)
   (set! /vals /vals-save)
   (set! //S/T //S/T-save))
  (set! //Budget //Budget-save)))

; Check for an AND at the top level of the form var <> value 
; where the variable appears in the other components. 
; Replace the variable by the denied value and see if any other conjunct 
; simplifies to FALSE: if so, then the original conjunct is redundant: 
(define (@Mth_Known_Value3 //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((/comp-save /comp)
        (/comps (@Cs (@I)))
        (/vars-save /vars)
        (/vals-save /vals)
        (/index '())
        (/n-save /n)
        (/i 0)
        (/done 0)
        (//S/T-save //S/T)
        (/var-save /var)
        (/val-save /val))
   (set! /comp '())
   (set! /vars '())
   (set! /vals '())
   (set! /n 0)
   (set! //S/T 0)
   (set! /var '())
   (set! /val '())
   (for-in /comp /comps 
    (begin
     (set! /n (+ /n 1))
     (cond
      ((= (@ST /comp) //T_/Not_/Equal)
       (cond
        ((and (or (= (@ST (@Get_n /comp 2)) //T_/Number) (= (@ST (@Get_n /comp 2)) //T_/String)) (> (@Mth_Count_Occs (@Get_n /comp 1)) 1))
         (set! /vars (cons (@Get_n /comp 1) /vars))
         (set! /vals (cons (@Get_n /comp 2) /vals))
         (set! /index (cons /n /index))))))))
   (@Edit)
   (while (and (not (null? /vars)) (= /done 0)) 
    (begin
     (set! /var (car /vars))
     (set! /val (car /vals))
     (set! /i (car /index))
     (set! /vars (cdr /vars))
     (set! /vals (cdr /vals))
     (set! /index (cdr /index))
     (set! /n 0)
     (for-in /comp /comps 
      (begin
       (set! /n (+ /n 1))
       (cond
        ((not (equal? /n /i))
         (@New_Program /comp)
         (cond
          ((> (@Mth_Count_Occs /var) 0)
           (@Foreach_Expn /foreach-maths-12 0 (@AS_Type) 0)
           (cond
            ((null? (@Program))
             (@New_Program (@Skips))))
           (set! //S/T (@ST (@Simplify (@I) (quotient //Budget 2))))
           (cond
            ((= //S/T //T_/False)
             (set! /done 1)))))))))))
   (@Undo_Edit)
   (cond
    ((= /done 1)
     ; Delete ith component of the AND 
     (@Down_To /i)
     (@Clever_Delete)
     (cond
      ((@Up?)
       (@Up)))))
   (set! /comp /comp-save)
   (set! /vars /vars-save)
   (set! /vals /vals-save)
   (set! /n /n-save)
   (set! //S/T //S/T-save)
   (set! /var /var-save)
   (set! /val /val-save))
  (set! //Budget //Budget-save)))

; Check for an OR at the top level of the form var = value 
; where the variable appears in the other components. 
; Replace the variable by its known value and see if any other conjunct 
; simplifies to TRUE: if so, then this conjunct is redundant: 
(define (@Mth_Known_Value4 //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((/comp-save /comp)
        (/comps (@Cs (@I)))
        (/vars-save /vars)
        (/vals-save /vals)
        (/index '())
        (/n-save /n)
        (/i 0)
        (/done 0)
        (//S/T-save //S/T)
        (/var-save /var)
        (/val-save /val))
   (set! /comp '())
   (set! /vars '())
   (set! /vals '())
   (set! /n 0)
   (set! //S/T 0)
   (set! /var '())
   (set! /val '())
   (for-in /comp /comps 
    (begin
     (set! /n (+ /n 1))
     (cond
      ((= (@ST /comp) //T_/Equal)
       (cond
        ((and (or (= (@ST (@Get_n /comp 2)) //T_/Number) (= (@ST (@Get_n /comp 2)) //T_/String)) (> (@Mth_Count_Occs (@Get_n /comp 1)) 1))
         (set! /vars (cons (@Get_n /comp 1) /vars))
         (set! /vals (cons (@Get_n /comp 2) /vals))
         (set! /index (cons /n /index))))))))
   (@Edit)
   (while (and (not (null? /vars)) (= /done 0)) 
    (begin
     (set! /var (car /vars))
     (set! /val (car /vals))
     (set! /i (car /index))
     (set! /vars (cdr /vars))
     (set! /vals (cdr /vals))
     (set! /index (cdr /index))
     (set! /n 0)
     (for-in /comp /comps 
      (begin
       (set! /n (+ /n 1))
       (cond
        ((not (equal? /n /i))
         (@New_Program /comp)
         (cond
          ((> (@Mth_Count_Occs /var) 0)
           (@Foreach_Expn /foreach-maths-13 0 (@AS_Type) 0)
           (cond
            ((null? (@Program))
             (@New_Program (@Skips))))
           (set! //S/T (@ST (@Simplify (@I) (quotient //Budget 2))))
           (cond
            ((= //S/T //T_/True)
             (set! /done 1)))))))))))
   (@Undo_Edit)
   (cond
    ((= /done 1)
     ; Delete ith component of the AND 
     (@Down_To /i)
     (@Clever_Delete)
     (cond
      ((@Up?)
       (@Up)))))
   (set! /comp /comp-save)
   (set! /vars /vars-save)
   (set! /vals /vals-save)
   (set! /n /n-save)
   (set! //S/T //S/T-save)
   (set! /var /var-save)
   (set! /val /val-save))
  (set! //Budget //Budget-save)))

; Check for an AND with OR components, or an OR with AND components, 
; where the elements of one component are a subset of another component. 
; If so, then delete the larger component. 
; Either main = T_And and sub = T_Or or main = T_Or and sub := T_And 
(define (@Mth_Common_Components //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((/main (@ST (@I)))
        (/sub (@ST (@Get_n (@I) 1)))
        (/comps '())
        (/delete '())
        (/comp1 '())
        (/comp2 '())
        (/elts '()))
   (@Edit)
   ; We must ensure that there are no duplicates in the component list 
   ; (otherwise we end up deleting both: since each is a subset of the other!) 
   ; We also want each component to have its components sorted 
   (@Mth_Sort_Merge)
   (cond
    ((or (not (= (@ST (@I)) /main)) (null? (@Cs (@I))))
     (@Undo_Edit))
    (#t
     (set! /comps (@Cs (@I)))
     (while (not (null? (cdr /comps))) 
      (begin
       (set! /comp1 (car /comps))
       (set! /comps (cdr /comps))
       (set! /elts (@Elements /comp1))
       (for-in /comp2 /comps 
        (cond
         ((= (@ST /comp2) /sub)
          (cond
           ((and (@Set_Subset? (@Elements /comp2) /elts) (@Mth_Subset? (@Cs /comp2) (@Cs /comp1)))
            (set! /delete (cons /comp1 /delete)))
           ((and (@Set_Subset? /elts (@Elements /comp2)) (@Mth_Subset? (@Cs /comp1) (@Cs /comp2)))
            (set! /delete (cons /comp2 /delete)))))))))
     (cond
      ((null? /delete)
       (@Undo_Edit))
      (#t
       (@End_Edit)
       ; Delete one occurrence of each element of the delete list 
       ; (There can only be one occurrence) 
       (for-in /comp1 /delete 
        (begin
         (@Down)
         (set! /fl_flag1 0)
         (while (= /fl_flag1 0) 
          (cond
           ((@Equal? (@I) /comp1)
            (@Delete)
            (set! /fl_flag1 1))
           ((@Right?)
            (@Right)
            (set! /fl_flag1 0))
           (#t
            (set! /fl_flag1 1))))
         (@Up)
         (cond
          ((not (@Cs? (@I)))
           (error "BUG in @Mth_Common_Components!!!"))))))))))
  (set! //Budget //Budget-save)))

; See if one (sorted) set of items is a subset of another 
(define (@Mth_Subset? //L1 //L2)
 
 (= (@Mth_Subset_Sub //L1 //L2) 1))

(define (@Mth_Subset_Sub //L1 //L2)
 
 (if (null? //L1) 1 (if (null? //L2) 0 (if (@Equal? (car //L1) (car //L2)) (@Mth_Subset_Sub (cdr //L1) (cdr //L2)) (if (@Mth_Lt? (car //L1) (car //L2)) 0 (@Mth_Subset_Sub //L1 (cdr //L2)))))))

; Count the occurrences of an item in the current item: 
(define (@Mth_Count_Occs //I-par)
 (let ((//I-save //I)
       (//R-save //R)
       (funct-result '()))
  (set! //I //I-par)
  (set! //R 0)
  (@Foreach_Expn /foreach-maths-14 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result //R)
  (set! //I //I-save)
  (set! //R //R-save)
  funct-result))

; Expand * over + and AND over OR 
; NOTE: after @Mth_Sort_Merge the +/OR components will have been 
; collected together. We process them in pairs (for simplicity) 
(define (@Mth_Expand //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (@Foreach_Expn /foreach-maths-15 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (cond
   ((= (@GT (@I)) //T_/Condition)
    (@Foreach_Cond /foreach-maths-16 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (set! //Budget //Budget-save)))

; Multiply out current item with next item (unless the result is too big) 
(define (@Mth_Multiply /mul /add //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((//L1 (@Cs (@I)))
        (//L2 '())
        (/new-save /new)
        (/x-save /x)
        (/y '()))
   (set! /new '())
   (set! /x '())
   (@Right)
   (cond
    ((< (* (gen-length //L1) (@Size (@I))) (quotient //Budget 2))
     (set! //L2 (@Cs (@I)))
     (for-in /x //L1 
      (for-in /y //L2 
       (cond
        ((= (@ST /x) /mul)
         (set! /new (cons (@Make /mul '() (concat (@Cs /x) (list /y))) /new)))
        (#t
         (set! /new (cons (@Make /mul '() (list /x /y)) /new))))))
     (@Left)
     (@Delete)
     (@Paste_Over (@Make /add '() (reverse /new)))))
   (set! /new /new-save)
   (set! /x /x-save))
  (set! //Budget //Budget-save)))

; Take common factors out of * components of + operations 
; and out of AND components of OR operations 
(define (@Mth_Factorise //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((/orig '()))
   (set! /orig (@I))
   (@Foreach_Expn /foreach-maths-17 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Cond /foreach-maths-18 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (while (not (@Equal? (@I) /orig)) 
    (begin
     (set! /orig (@I))
     (@Foreach_Expn /foreach-maths-19 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (@Foreach_Cond /foreach-maths-20 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips)))))))
  (set! //Budget //Budget-save)))

; Return number if times elt occurs in list: 
(define (@Mth_Occs /elt /list)
 (let ((//R 0))
  (while (not (null? /list)) 
   (begin
    (cond
     ((equal? /elt (car /list))
      (set! //R (+ //R 1))))
    (set! /list (cdr /list))))
  //R))

; Search for common factors in the Times components of the Plus operation. 
; The brute force method is O(n^2) in the number of factors. 
; The simple, elegant, solution is to use a hash table: BUT we can't do that 
; because of the dbase tables (need a hash table with @Equal? as the equality op) 
; So instead, we sort the list of factors (using @Mth_Sort) 
; and check the list for duplicates 
(define (@Mth_Factor //Plus //Times)
 (let ((/factors '())
       (/common '())
       (/occs 0)
       (/max 0)
       (/comp-save /comp))
  (set! /comp '())
  ; Get the list of factors, sort it, then find the most common 
  (for-in /comp (@Cs (@I)) 
   (cond
    ((= (@ST /comp) //Times)
     (set! /factors (concat (@Cs /comp) /factors)))))
  (set! /factors (@Mth_Sort /factors))
  (while (not (null? /factors)) 
   (begin
    (set! /occs 1)
    (while (and (not (null? (cdr /factors))) (@Equal? (car /factors) (car (cdr /factors)))) 
     (begin
      (set! /occs (+ /occs 1))
      (cond
       ((> /occs /max)
        (set! /common (car /factors))
        (set! /max /occs)))
      (set! /factors (cdr /factors))))
    (set! /factors (cdr /factors))))
  (cond
   ((>= /max 2)
    ; We have found a common factor, so take it out. 
    ; From this point on, efficiency isn't so important. 
    (let ((/new-save /new)
          (/sub '())
          (/newsub '())
          (/newcomp '())
          (/done 0))
     (set! /new '())
     (for-in /comp (@Cs (@I)) 
      (cond
       ((= (@ST /comp) //Times)
        (set! /newcomp '())
        (set! /done 0)
        (for-in /sub (@Cs /comp) 
         (cond
          ((and (= /done 0) (@Equal? /common /sub))
           (cond
            ((not (equal? (vector-ref //Power_/Op (- //Times 1)) //Times))
             ; Don't take out more than one 
             (set! /done 1))))
          (#t
           (set! /newcomp (cons /sub /newcomp)))))
        (cond
         ((= (gen-length /newcomp) 0)
          #t)
         ((= (gen-length /newcomp) 1)
          (set! /newsub (concat /newcomp /newsub)))
         ((< (gen-length /newcomp) (@Size /comp))
          (set! /newsub (cons (@Make //Times '() (reverse /newcomp)) /newsub)))
         (#t
          (set! /new (cons /comp /new)))))
       (#t
        (set! /new (cons /comp /new)))))
     ; newsub is the result after common has been taken out 
     ; new contains the elements which didn't contain common 
     (cond
      ((= (gen-length /newsub) 0)
       #t)
      ((= (gen-length /newsub) 1)
       (set! /newsub (@Make //Times '() (@Mth_Sort (list /common (wsl-ref /newsub 1))))))
      (#t
       (set! /newsub (@Make //Times '() (@Mth_Sort (list /common (@Make //Plus '() (@Mth_Sort /newsub))))))))
     (cond
      ((not (null? /newsub))
       (cond
        ((null? /new)
         (@Paste_Over /newsub))
        (#t
         (@Paste_Over (@Make //Plus '() (@Mth_Sort (cons /newsub /new))))))))
     (set! /new /new-save))))
  (set! /comp /comp-save)))

; Simplify A*(1/B) and A+(-B), sort relations, *, and + operations 
; Constants are placed last, so x+5 will be OK, but we need to re-sort x*5 to 5*x 
; NB: We ensure that negative numbers are implemented using T_Negate 
; so that x - 1 doesn't come out as x + -1 
; Although -n is larger than x (and therefore is normally ordered first) 
; we want to say x = -n and not -n = x 
; Ensure T_Concat has only two components to work around a bug in FME 
(define (@Mth_Prettify)
 (let ((/count-save /count))
  (set! /count 0)
  (@Foreach_Expn /foreach-maths-21 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (cond
   ((= (@GT (@I)) //T_/Condition)
    ; Check for two or more unsafe tests in an AND or OR :
    (cond
     ((or (= (@ST (@I)) //T_/And) (= (@ST (@I)) //T_/Or))
      (for-in /comp (@Cs (@I)) 
       (cond
        ((@Unsafe_Test? /comp)
         (set! /count (+ /count 1)))))))))
  (cond
   ((and (= (@GT (@I)) //T_/Condition) (< /count 2))
    (@Foreach_Cond /foreach-maths-22 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (@Foreach_Expn /foreach-maths-23 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /count /count-save)))

(define (@Mth_Bug_Fix_Concat //I)
 
 (if (and (= (@ST //I) //T_/Concat) (> (@Size //I) 2)) (@Make //T_/Concat '() (list (@Mth_Bug_Fix_Concat (@Make //T_/Concat '() (butlast-1 (@Cs //I)))) (last-1 (@Cs //I)))) //I))

(define (@Mth_Prettify_Inverses //Times //Invert //Divide)
 (let ((/top '())
       (/bottom '())
       (/comp-save /comp))
  (set! /comp '())
  (for-in /comp (@Cs (@I)) 
   (cond
    ((= (@ST /comp) //Invert)
     (set! /bottom (cons (@Get_n /comp 1) /bottom)))
    (#t
     (set! /top (cons /comp /top)))))
  (cond
   ((null? /top)
    (@Paste_Over (@Make //Invert '() (list (@C_Make //Times /bottom)))))
   (#t
    (@Paste_Over (@Make //Divide '() (list (@C_Make //Times /top) (@C_Make //Times /bottom))))))
  (set! /comp /comp-save)))

; A conditional @Make: If there is one item in the list, return it. 
; Otherwise, construct an item of the given type: 
(define (@C_Make /type //L)
 
 (if (= (gen-length //L) 1) (car //L) (@Make /type '() //L)))

(define (@Simplify_Bit_Ops)
 (let ((/bit_and-save /bit_and)
       (/bit_or-save /bit_or)
       (/bit_xor-save /bit_xor))
  (set! /bit_and (@Make_Name "bit_and"))
  (set! /bit_or (@Make_Name "bit_or"))
  (set! /bit_xor (@Make_Name "bit_xor"))
  (@Foreach_Expn /foreach-maths-24 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /bit_and /bit_and-save)
  (set! /bit_or /bit_or-save)
  (set! /bit_xor /bit_xor-save)))

; Check that all the components are numbers or hex numbers, 
; Convert all components to hex strings and process them 
; a character at a time using a look-up table with keys: 
; <type, x, y> which returns a character 
; Paste over the hex string result. 
; Note: we produce a hex string result rather than a number 
; because of the limited size of integers in (efficient) compiled Scheme. 
(define (@Simplify_Bit /type)
 (let ((/comps (@Cs (@Get_n (@I) 2)))
       (//O/K 1)
       (/comp-save /comp)
       (/pars '()))
  (set! /comp '())
  (for-in /comp /comps 
   (cond
    ((= (@ST /comp) //T_/Number)
     (set! /pars (cons (@Num_To_Hex (@V /comp)) /pars)))
    ((and (= (@ST /comp) //T_/String) (@Starts_With? (@V /comp) "hex 0x"))
     (set! /pars (cons (upcase (substr (@V /comp) 6)) /pars)))
    (#t
     (set! //O/K 0))))
  (cond
   ((= //O/K 1)
    (while (> (gen-length /pars) 1) 
     (set! /pars (cons (@Bit_Op /type (wsl-ref /pars 1) (wsl-ref /pars 2)) (@Final_Seg /pars 3))))
    (cond
     ((@Mth_Zeros? (wsl-ref /pars 1))
      (@Paste_Over (@Make //T_/Number 0 '())))
     (#t
      (@Paste_Over (@Make //T_/String (string-append "hex 0x" (wsl-ref /pars 1)) '()))))))
  (set! /comp /comp-save)))

(define (@Mth_Zeros? /s)
 
 (or (= (string-length /s) 0) (and (equal? (substr /s 0 1) "0") (@Mth_Zeros? (substr /s 1)))))

; Apply the bitwise operation to the two hex strings and return the result: 
(define (@Bit_Op /type /p1 /p2)
 (let ((//R-save //R)
       (/n-save /n)
       (/i 0)
       (funct-result '()))
  (set! //R "")
  (set! /n (max (string-length /p1) (string-length /p2)))
  ; Pad the strings to the same length 
  (for /i (+ (string-length /p1) 1) /n 1 
   (set! /p1 (string-append "0" /p1)))
  (for /i (+ (string-length /p2) 1) /n 1 
   (set! /p2 (string-append "0" /p2)))
  (for /i 0 (- /n 1) 1 
   (set! //R (concat //R (gethash //Bit_/Op (list /type (substr /p1 /i 1) (substr /p2 /i 1))))))
  (set! funct-result //R)
  (set! //R //R-save)
  (set! /n /n-save)
  funct-result))

; Set up the Bit_Op lookup table: 
(set! //Bit_/Op (hash-table))
(let ((/bit_and (@Make_Name "bit_and"))
      (/bit_or (@Make_Name "bit_or"))
      (/bit_xor (@Make_Name "bit_xor"))
      (/type '())
      (/p1 0)
      (/p2 0)
      (/p11 0)
      (/p22 0)
      (/r 0)
      (/bit 0)
      (/digits "0123456789ABCDEF"))
 (for-in /type (list /bit_and /bit_or /bit_xor) 
  (for /p1 0 15 1 
   (for /p2 0 15 1 
    (begin
     ; Compute r := p1 bit_XXX p2 and store in table: 
     (set! /bit 1)
     (set! /r 0)
     (set! /p11 /p1)
     (set! /p22 /p2)
     (while (or (> /p11 0) (> /p22 0)) 
      (begin
       (cond
        ((equal? /type /bit_and)
         (cond
          ((and (= (modulo /p11 2) 1) (= (modulo /p22 2) 1))
           (set! /r (+ /r /bit)))))
        ((equal? /type /bit_or)
         (cond
          ((or (= (modulo /p11 2) 1) (= (modulo /p22 2) 1))
           (set! /r (+ /r /bit)))))
        ((equal? /type /bit_xor)
         (cond
          ((or (and (= (modulo /p11 2) 0) (= (modulo /p22 2) 1)) (and (= (modulo /p11 2) 1) (= (modulo /p22 2) 0)))
           (set! /r (+ /r /bit))))))
       (set! /p11 (quotient /p11 2))
       (set! /p22 (quotient /p22 2))
       (set! /bit (* /bit 2))))
     (puthash //Bit_/Op (list /type (substr /digits /p1 1) (substr /digits /p2 1)) (substr /digits /r 1)))))))
; Computationally expensive simplifications 
(define (@Mth_Expensive //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((//Orig '())
        (//B1 '())
        (//B2 '())
        (/comp-save /comp)
        (/not_comp '())
        (//R-save //R))
   (set! /comp '())
   (set! //R '())
   (set! /fl_flag2 0)
   (while (= /fl_flag2 0) 
    (cond
     ((<= //Budget 10)
      (set! /fl_flag2 1))
     (#t
      (set! //Orig (@I))
      (cond
       ((and #f (or (= (@ST (@I)) //T_/And) (= (@ST (@I)) //T_/Or)))
        (display-list "@Mth_Expensive")
        (@PP_Item (@I) 80 "")
        (display-list "")))
      ; In the following, the remainder could be a large formula 
      ; and @Simplify_Using does a FOREACH over it. 
      ; So limit the calls to cases where the component is a simple 
      ; relation comparing something with a number. 
      (cond
       ((= (@ST (@I)) //T_/And)
        ; Use each component to simplify the others 
        ; Can assume any component is true if that simplifies the remainder 
        (set! //B1 '())
        (set! //B2 (cdr (@Cs (@I))))
        (set! /comp (@Get_n (@I) 1))
        (set! /fl_flag1 0)
        (while (= /fl_flag1 0) 
         (begin
          (cond
           ((or (and (or (= (@ST /comp) //T_/Equal) (= (@ST /comp) //T_/Not_/Equal) (= (@ST /comp) //T_/Less) (= (@ST /comp) //T_/Less_/Eq) (= (@ST /comp) //T_/Greater) (= (@ST /comp) //T_/Greater_/Eq)) (or (= (@ST (@Get_n /comp 1)) //T_/Number) (= (@ST (@Get_n /comp 2)) //T_/Number)) (or (or (= (@ST (@Get_n /comp 1)) //T_/Variable) (= (@ST (@Get_n /comp 1)) //T_/Struct)) (or (= (@ST (@Get_n /comp 2)) //T_/Variable) (= (@ST (@Get_n /comp 2)) //T_/Struct)))) (= (@ST /comp) //T_/B/Funct_/Call))
            (cond
             ((= (gen-length //B1) 1)
              (set! //R (@Mth_Simplify_Using (car //B1) /comp (- //Budget 1)))
              (cond
               ((< (@Total_Size //R) (@Total_Size (car //B1)))
                (@Paste_Over (@Make //T_/And '() (cons //R (cons /comp //B2))))
                (set! /fl_flag1 1))
               (#t
                (set! /fl_flag1 0))))
             ((> (gen-length //B1) 1)
              (set! //R (@Mth_Simplify_Using (@Make //T_/And '() //B1) /comp (- //Budget 1)))
              (cond
               ((< (@Total_Size //R) (@Total_Size (@Make //T_/And '() //B1)))
                (@Paste_Over (@Make //T_/And '() (cons //R (cons /comp //B2))))
                (set! /fl_flag1 1))
               (#t
                (set! /fl_flag1 0))))
             (#t
              (set! /fl_flag1 0)))
            (cond
             ((= /fl_flag1 0)
              (cond
               ((= (gen-length //B2) 1)
                (set! //R (@Mth_Simplify_Using (car //B2) /comp (- //Budget 1)))
                (cond
                 ((< (@Total_Size //R) (@Total_Size (car //B2)))
                  (@Paste_Over (@Make //T_/And '() (cons /comp (cons //R //B1))))
                  (set! /fl_flag1 1))
                 (#t
                  (set! /fl_flag1 0))))
               ((> (gen-length //B2) 1)
                (set! //R (@Mth_Simplify_Using (@Make //T_/And '() //B2) /comp (- //Budget 1)))
                (cond
                 ((< (@Total_Size //R) (@Total_Size (@Make //T_/And '() //B2)))
                  (@Paste_Over (@Make //T_/And '() (cons /comp (cons //R //B1))))
                  (set! /fl_flag1 1))
                 (#t
                  (set! /fl_flag1 0))))
               (#t
                (set! /fl_flag1 0))))))
           (#t
            (set! /fl_flag1 0)))
          (cond
           ((= /fl_flag1 0)
            (cond
             ((null? //B2)
              (set! /fl_flag1 1))
             (#t
              (set! //B1 (cons /comp //B1))
              (set! /comp (car //B2))
              (set! //B2 (cdr //B2))
              (set! /fl_flag1 0))))))))
       ((= (@ST (@I)) //T_/Or)
        ; Use each component to simplify the others 
        ; Can assume any component is false if that simplifies the remainder 
        (set! //B1 '())
        (set! //B2 (cdr (@Cs (@I))))
        (set! /comp (@Get_n (@I) 1))
        (set! /not_comp (@Not (@Get_n (@I) 1)))
        (set! /fl_flag1 0)
        (while (= /fl_flag1 0) 
         (begin
          (cond
           ((or (and (or (= (@ST /comp) //T_/Equal) (= (@ST /comp) //T_/Not_/Equal) (= (@ST /comp) //T_/Less) (= (@ST /comp) //T_/Less_/Eq) (= (@ST /comp) //T_/Greater) (= (@ST /comp) //T_/Greater_/Eq)) (or (= (@ST (@Get_n /comp 1)) //T_/Number) (= (@ST (@Get_n /comp 2)) //T_/Number)) (or (or (= (@ST (@Get_n /comp 1)) //T_/Variable) (= (@ST (@Get_n /comp 1)) //T_/Struct)) (or (= (@ST (@Get_n /comp 2)) //T_/Variable) (= (@ST (@Get_n /comp 2)) //T_/Struct)))) (= (@ST /comp) //T_/B/Funct_/Call))
            (cond
             ((= (gen-length //B1) 1)
              (set! //R (@Mth_Simplify_Using (car //B1) /not_comp (- //Budget 1)))
              (cond
               ((< (@Total_Size //R) (@Total_Size (car //B1)))
                (@Paste_Over (@Make //T_/Or '() (cons //R (cons /comp //B2))))
                (set! /fl_flag1 1))
               (#t
                (set! /fl_flag1 0))))
             ((> (gen-length //B1) 1)
              (set! //R (@Mth_Simplify_Using (@Make //T_/Or '() //B1) /not_comp (- //Budget 1)))
              (cond
               ((< (@Total_Size //R) (@Total_Size (@Make //T_/Or '() //B1)))
                (@Paste_Over (@Make //T_/Or '() (cons //R (cons /comp //B2))))
                (set! /fl_flag1 1))
               (#t
                (set! /fl_flag1 0))))
             (#t
              (set! /fl_flag1 0)))
            (cond
             ((= /fl_flag1 0)
              (cond
               ((= (gen-length //B2) 1)
                (set! //R (@Mth_Simplify_Using (car //B2) /not_comp (- //Budget 1)))
                (cond
                 ((< (@Total_Size //R) (@Total_Size (car //B2)))
                  (@Paste_Over (@Make //T_/Or '() (cons /comp (cons //R //B1))))
                  (set! /fl_flag1 1))
                 (#t
                  (set! /fl_flag1 0))))
               ((> (gen-length //B2) 1)
                (set! //R (@Mth_Simplify_Using (@Make //T_/Or '() //B2) /not_comp (- //Budget 1)))
                (cond
                 ((< (@Total_Size //R) (@Total_Size (@Make //T_/Or '() //B2)))
                  (@Paste_Over (@Make //T_/Or '() (cons /comp (cons //R //B1))))
                  (set! /fl_flag1 1))
                 (#t
                  (set! /fl_flag1 0))))
               (#t
                (set! /fl_flag1 0))))))
           (#t
            (set! /fl_flag1 0)))
          (cond
           ((= /fl_flag1 0)
            (cond
             ((null? //B2)
              (set! /fl_flag1 1))
             (#t
              (set! //B1 (cons /comp //B1))
              (set! /comp (car //B2))
              (set! /not_comp (@Not /comp))
              (set! //B2 (cdr //B2))
              (set! /fl_flag1 0)))))))))
      (cond
       ((@Equal? //Orig (@I))
        (set! /fl_flag2 1))
       (#t
        (set! //Budget (- //Budget 1))
        (set! /fl_flag2 0))))))
   (set! /comp /comp-save)
   (set! //R //R-save))
  (set! //Budget //Budget-save)))

; Simplify condition A given that condition B is true 
; For example: x = 0 AND y = 3 might simplify to FALSE when x > 0 is known. 
; We can assume that both A and B are already simplified 
(define (@Simplify_Using //A //B-par //Budget-par)
 (let ((//Budget-save //Budget)
       (//B-save //B)
       (//R-save //R)
       (funct-result '()))
  (set! //Budget //Budget-par)
  (set! //B //B-par)
  (set! //R '())
  (set! //R (@Mth_Simplify_Using //A //B //Budget))
  (cond
   ((< (@Total_Size //R) (@Total_Size //A))
    (set! //R (@Simplify //R (quotient //Budget 2))))
   (#t
    (set! //R //A)))
  (set! funct-result //R)
  (set! //Budget //Budget-save)
  (set! //B //B-save)
  (set! //R //R-save)
  funct-result))

; The internal version avoids calling @Simplify to avoid re-factorising 
(define (@Mth_Simplify_Using //A //B-par //Budget-par)
 (let ((//Budget-save //Budget)
       (//B-save //B)
       (//R-save //R)
       (/v-save /v)
       (/e '())
       (/not_/B-save /not_/B)
       (funct-result '()))
  (set! //Budget //Budget-par)
  (set! //B //B-par)
  (set! //R '())
  (set! /v '())
  (set! /not_/B '())
  (cond
   ((or (<= //Budget 10) (null? (intersection-n (@Used //A) (@Used //B))))
    ; Not enough budget, or no shared variables 
    (set! //R //A)))
  (cond
   (#f
    (display-list "Simplify: ")
    (@PP_Item //A 80 "")
    (display-list "Using: ")
    (@PP_Item //B 80 "")))
  (cond
   ((and (null? //R) (= (@ST //B) //T_/Equal) (not (null? (intersection-n (@Used //A) (@Used (@Get_n //B 1))))))
    ; Try replacing B^1 with B^2 in A 
    (set! //R (@Mth_Replace (@Get_n //B 1) (@Get_n //B 2) //A))
    (cond
     ((>= (@Total_Size //R) (@Total_Size //A))
      (set! //R '())))))
  (cond
   ((and (null? //R) (= (@ST //B) //T_/Equal) (not (null? (intersection-n (@Used //A) (@Used (@Get_n //B 2))))))
    ; Try replacing B^2 with B^1 in A 
    (set! //R (@Mth_Replace (@Get_n //B 2) (@Get_n //B 1) //A))
    (cond
     ((>= (@Total_Size //R) (@Total_Size //A))
      (set! //R '())))))
  ; If any component of A is implied by B, then replace it by TRUE 
  (cond
   ((null? //R)
    (set! /not_/B (@Not //B))
    (@Edit)
    (@New_Program //A)
    (@Foreach_Cond /foreach-maths-25 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (cond
     ((<= (@Total_Size (@Program)) (@Total_Size //A))
      (set! //R (@Program))))
    (@Undo_Edit)))
  (cond
   ((null? //R)
    (set! //R //A)))
  (cond
   (#f
    (display-list "Result: ")
    (@PP_Item //R 80 "")
    (display-list "")))
  (set! funct-result //R)
  (set! //Budget //Budget-save)
  (set! //B //B-save)
  (set! //R //R-save)
  (set! /v /v-save)
  (set! /not_/B /not_/B-save)
  funct-result))

(define (@Mth_Replace /e1 /e2 //B-par)
 (let ((//B-save //B)
       (//R-save //R)
       (funct-result '()))
  (set! //B //B-par)
  (set! //R '())
  (cond
   ((@Equal? //B /e1)
    (set! //R /e2))
   ((> (@Total_Size //B) (@Total_Size /e1))
    (let ((/new-save /new)
          (/e '()))
     (set! /new '())
     (for-in /comp (@Cs //B) 
      (set! /new (cons (@Mth_Replace /e1 /e2 /comp) /new)))
     (set! //R (@Make (@ST //B) '() (reverse /new)))
     (cond
      ((@Equal? //R //B)
       (set! //R //B))
      (#t
       (set! //R (@Simplify //R 10))))
     (set! /new /new-save)))
   (#t
    (set! //R //B)))
  (set! funct-result //R)
  (set! //B //B-save)
  (set! //R //R-save)
  funct-result))

; Simplify FORALL and EXISTS: 
(define (@Mth_Quantifiers //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  ; Ex.(A OR B) simplifies to Ex.A OR Ex.B 
  ; Ax.(A AND B) simplifies to Ax.A AND Ax.B 
  (let ((/vars-save /vars)
        (/new-save /new)
        (/rest '()))
   (set! /vars (@Variables (@Get_n (@I) 1)))
   (set! /new '())
   (cond
    ((null? (intersection-n /vars (@Used (@Get_n (@I) 2))))
     (@Paste_Over (@Get_n (@I) 2)))
    ((or (and (= (@ST (@I)) //T_/Exists) (= (@ST (@Get_n (@I) 2)) //T_/Or)) (and (= (@ST (@I)) //T_/Forall) (= (@ST (@Get_n (@I) 2)) //T_/And)))
     (for-in /comp (@Cs (@Get_n (@I) 2)) 
      (cond
       ((null? (intersection-n /vars (@Used /comp)))
        (set! /new (cons /comp /new)))
       (#t
        (set! /new (cons (@Make (@ST (@I)) '() (list (@Get_n (@I) 1) /comp)) /new)))))
     (@Paste_Over (@Make (@ST (@Get_n (@I) 2)) '() (reverse /new))))
    ((or (= (@ST (@Get_n (@I) 2)) //T_/Or) (= (@ST (@Get_n (@I) 2)) //T_/And))
     ; Factor out any components which don't use vars 
     (for-in /comp (@Cs (@Get_n (@I) 2)) 
      (cond
       ((not (null? (intersection-n /vars (@Used /comp))))
        (set! /new (cons /comp /new)))
       (#t
        (set! /rest (cons /comp /rest)))))
     (cond
      ((not (null? /rest))
       (cond
        ((> (gen-length /new) 1)
         (set! /new (@Make (@ST (@Get_n (@I) 2)) '() (reverse /new))))
        (#t
         (set! /new (car /new))))
       (set! /new (@Make (@ST (@I)) '() (list (@Get_n (@I) 1) /new)))
       (@Paste_Over (@Make (@ST (@Get_n (@I) 2)) '() (cons /new (reverse /rest)))))
      (#t
       (@Mth_Quantifiers_Sub //Budget))))
    (#t
     (@Mth_Quantifiers_Sub //Budget)))
   (set! /vars /vars-save)
   (set! /new /new-save))
  (set! //Budget //Budget-save)))

(define (@Mth_Quantifiers_Sub //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((/vars-save /vars)
        (/replace-save /replace)
        (/sub 0)
        (/rel-save /rel))
   (set! /vars (@Variables (@Get_n (@I) 1)))
   (set! /replace (hash-table))
   (set! /rel 0)
   (cond
    ((= (@ST (@I)) //T_/Forall)
     (set! /sub //T_/Or)
     (set! /rel //T_/Not_/Equal))
    (#t
     (set! /sub //T_/And)
     (set! /rel //T_/Equal)))
   (@Down_To 2)
   ; to the condition 
   ; Ax.(x <> e OR Q) simplifies to Q[e/x] 
   ; Ex.(x = e AND Q) simplifies to Q[e/x] 
   (cond
    ((= (@ST (@I)) /sub)
     (for-in /comp (@Cs (@I)) 
      (cond
       ((and (= (@ST /comp) /rel) (= (@ST (@Get_n /comp 1)) //T_/Variable) (member (@V (@Get_n /comp 1)) /vars))
        (puthash /replace (@V (@Get_n /comp 1)) (@Get_n /comp 2)))
       ((and (= (@ST /comp) /rel) (= (@ST (@Get_n /comp 2)) //T_/Variable) (member (@V (@Get_n /comp 2)) /vars))
        (puthash /replace (@V (@Get_n /comp 2)) (@Get_n /comp 1)))))
     (@Foreach_Global_Var /foreach-maths-26 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))))
   (@Up)
   ; Back to FORALL/EXISTS 
   ; Ex.( ... x = e ...) simplifies to Ex.( ... TRUE ...) 
   ; if this is the only occurrence of x 
   (cond
    ((= (@ST (@I)) //T_/Exists)
     (@Down_To 2)
     ; to the condition 
     (let ((/count-save /count))
      (set! /count 0)
      (for-in /v /vars 
       (begin
        (set! /count 0)
        (@Foreach_Global_Var /foreach-maths-27 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))
        (cond
         ((= /count 1)
          (let ((/var-save /var))
           (set! /var (@Make //T_/Variable /v '()))
           (@Foreach_Cond /foreach-maths-28 0 (@AS_Type) 0)
           (cond
            ((null? (@Program))
             (@New_Program (@Skips))))
           (set! /var /var-save))))))
      (set! /count /count-save))
     (@Up)))
   ; Check for unused variables: 
   (cond
    ((not (null? (@Set_Difference /vars (@Used (@Get_n (@I) 2)))))
     (set! /vars (intersection-n /vars (@Used (@Get_n (@I) 2))))
     (cond
      ((null? /vars)
       (@Paste_Over (@Get_n (@I) 2)))
      (#t
       (let ((/new-save /new))
        (set! /new '())
        (for-in /v /vars 
         (set! /new (cons (@Make //T_/Var_/Lvalue /v '()) /new)))
        (@Down)
        (@Paste_Over (@Make //T_/Lvalues '() (reverse /new)))
        (@Up)
        (set! /new /new-save))))))
   (set! /vars /vars-save)
   (set! /replace /replace-save)
   (set! /rel /rel-save))
  (set! //Budget //Budget-save)))

; If (NOT q?(x)) appears, try replacing q?(x) by both TRUE and FALSE 
; If the results are identical and smaller than the original, then replace it 
; Ditto for (NOT !XC q(x)) 
(define (@Simplify_BFunct_Calls //Budget-par)
 (let ((//Budget-save //Budget))
  (set! //Budget //Budget-par)
  (let ((/calls-save /calls))
   (set! /calls '())
   (@Foreach_Cond /foreach-maths-29 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (cond
    ((not (null? /calls))
     (let ((/new1 '())
           (/new2 '()))
      (for-in /call /calls 
       (begin
        (set! /new1 (@Replace_Condition (@I) /call //Mth_/True 5))
        (set! /new2 (@Replace_Condition (@I) /call //Mth_/False 5))
        (cond
         ((and (@Equal? /new1 /new2) (< (@Total_Size /new1) (@Total_Size (@I))))
          (@Paste_Over /new1))
         ((< (+ (+ (@Total_Size /new1) (@Total_Size /new2)) 10) (@Total_Size (@I)))
          (@Paste_Over (@Make 311 '() (list (@Make 310 '() (list /call /new1)) (@Make 310 '() (list (@Make 312 '() (list /call)) /new2))))))))))))
   (set! /calls /calls-save))
  (set! //Budget //Budget-save)))

(define (@Replace_Condition //I-par /old-par /new-par //Budget-par)
 (let ((//Budget-save //Budget)
       (/new-save /new)
       (/old-save /old)
       (//I-save //I)
       (//R-save //R)
       (funct-result '()))
  (set! //Budget //Budget-par)
  (set! /new /new-par)
  (set! /old /old-par)
  (set! //I //I-par)
  (set! //R '())
  (@Edit)
  (@New_Program //I)
  (@Foreach_Cond /foreach-maths-30 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //R (@Simplify (@Program) //Budget))
  (@Undo_Edit)
  (set! funct-result //R)
  (set! //Budget //Budget-save)
  (set! /new /new-save)
  (set! /old /old-save)
  (set! //I //I-save)
  (set! //R //R-save)
  funct-result))

#t
