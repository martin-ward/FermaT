;;; Scheme translation of WSL code
(define (/foreach-constant_propagation-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) /pop_regs))
   (@Delete))
  ((and (= (@ST (@I)) //T_/Assignment) (= (@Size (@I)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@Get_n (@I) 1) 1)) /registers) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Variable) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /reg_inits))
   (@Delete))))

(define (/foreach-constant_propagation-2 //Depth //A/S_/Type)
 (cond
  ((@Is_Addr? (@I))
   (set! /addr (concat (@Elts_Used (@Get_Addr (@I))) /addr)))))

(define (/foreach-constant_propagation-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Variable) (or (@Starts_With? (@V (@Get_n (@Get_n (@I) 1) 2)) "v_") (@Starts_With? (@V (@Get_n (@Get_n (@I) 1) 2)) "V_")))
   (set! //Vcons (union-n (@Elts_Assigned (@Get_n (@I) 1)) //Vcons))
   (puthash //Vcons_vals (@Elts_Assigned (@Get_n (@I) 1)) (union-n (gethash //Vcons_vals (@Elts_Assigned (@Get_n (@I) 1))) (list (@V (@Get_n (@Get_n (@I) 1) 2))))))))

(define (/foreach-constant_propagation-4 //Depth //A/S_/Type)
 (cond
  ((and (or (= (@ST (@I)) //T_/Variable) (= (@ST (@I)) //T_/Struct)) (= (gen-length (gethash //Vcons_vals (@Elements (@I)))) 1))
   (@Paste_Over (@Make //T_/Variable (car (gethash //Vcons_vals (@Elements (@I)))) '())))))

(define (/foreach-constant_propagation-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Struct) (@Is_Mem? (@Get_n (@I) 2)) (= (@ST (@Get_Mem (@Get_n (@I) 2))) //T_/Variable))
   (puthash //D/S/E/C/Ts (@V (@Get_Mem (@Get_n (@I) 2))) 1)))
 (cond
  ((and (= (@ST (@I)) //T_/Aref) (@Is_Mem? (@Get_n (@I) 1)) (= (@ST (@Get_Mem (@Get_n (@I) 1))) //T_/Variable))
   (puthash //D/S/E/C/Ts (@V (@Get_Mem (@Get_n (@I) 1))) 1))))

(define (/foreach-constant_propagation-6 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Struct_/Lvalue) (@Is_Mem? (@Get_n (@I) 2)) (= (@ST (@Get_Mem (@Get_n (@I) 2))) //T_/Variable))
   (puthash //D/S/E/C/Ts (@V (@Get_Mem (@Get_n (@I) 2))) 1))))

(define (/foreach-constant_propagation-7 //Depth //A/S_/Type)
 (set! /name (@CP_Var_Name (@I)))
 (cond
  ((and (= (@ST (@I)) //T_/Plus) (= (@Size (@I)) 2) (= (@ST (@Get_n (@I) 1)) //T_/Number) (= (@ST (@Get_n (@I) 2)) //T_/String) (equal? (@V (@Get_n (@I) 2)) "hex 0x80000000") (not (null? (gethash //Code_/Hash (@V (@Get_n (@I) 1))))))
   (@Paste_Over (@Get_n (@I) 1))))
 ; Check for reg__n if expn is a[reg + n, 4] 
 (cond
  ((and (@Is_Mem_Rel? (@I)) (= (@ST (@Get_Mem_Rel (@I))) //T_/Plus) (= (@ST (@Get_n (@Get_Mem_Rel (@I)) 1)) //T_/Variable) (= (@ST (@Get_n (@Get_Mem_Rel (@I)) 2)) //T_/Number) (= (@ST (@Get_Mem_Rel_N (@I))) //T_/Number) (or (member (@V (@Get_n (@Get_Mem_Rel (@I)) 1)) /reg_inits) (member (@V (@Get_n (@Get_Mem_Rel (@I)) 1)) /registers)))
   (let ((/name-save /name))
    (set! /name (@Make_Name (concat (string-append (@N_String (@V (@Get_n (@Get_Mem_Rel (@I)) 1))) "__") (@String (@V (@Get_n (@Get_Mem_Rel (@I)) 2))))))
    (cond
     ((not (null? (@CP_Get /l (list /name))))
      (display-list-flush "c")
      (@Paste_Over (@CP_Get /l (list /name)))
      (set! /change (+ /change 1))))
    (set! /name /name-save))))
 ; Don't propagate inside ADDRESS_OF or sizeof calls: 
 (cond
  ((and (> (gen-length (@Posn)) 1) (or (@Is_Addr? (@GParent)) (= (@ST (@Parent)) //T_/Address_/Of) (and (= (@ST (@GParent)) //T_/X_/Funct_/Call) (equal? (@V (@Get_n (@GParent) 1)) /sizeof))))
   #t)
  ((and (> (gen-length (@Posn)) 1) (= (@Posn_n) 2) (or (= (@ST (@Parent)) //T_/Rel_/Seg) (= (@ST (@Parent)) //T_/Rel_/Seg_/Lvalue)) (not (null? (@Used (@I)))))
   ; Don't replace a variable array index by a constant 
   (set! /p1 (@Get_n (@Parent) 1))
   (@Edit)
   (set! //L (@CP_Update  //L))
   (cond
    ((null? (@Used (@I)))
     ; Index was a variable, but is now a constant. 
     ; So we need to increment it by one. 
     ; Unless it is an absolute memory address. 
     (cond
      ((or (not (null? (@Funct_Calls (@I)))) (not (null? (@X_Funct_Calls (@I)))) (member /a_name (@Variables /p1)))
       (@End_Edit))
      (#t
       (@Paste_Over (@Make //T_/Plus '() (list (@I) (@Make //T_/Number 1 '()))))
       (@End_Edit)))
     ; We have processed this subtree, so move away: 
     (@Right))
    (#t
     (@End_Edit))))
  ((and (= (@ST (@I)) //T_/Sub_/Seg) (= (@ST (@Get_n (@I) 2)) //T_/Number) (= (@ST (@Get_n (@I) 3)) //T_/Number) (< (- (@V (@Get_n (@I) 3)) (@V (@Get_n (@I) 2))) 4))
   (set! /name (@CP_Var_Name (@Get_n (@I) 1)))
   (cond
    ((not (null? /name))
     (set! /i (@V (@Get_n (@I) 2)))
     (set! /n 0)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (set! /val (@CP_Get /l (concat /name (list (- /i)))))
       (cond
        ((null? /val)
         (set! /fl_flag1 1))
        ((not (= (@ST /val) //T_/Number))
         (set! /val '())
         (set! /fl_flag1 1))
        (#t
         (set! /n (+ (* /n 256) (@V /val)))
         (set! /i (+ /i 1))
         (cond
          ((> /i (@V (@Get_n (@I) 3)))
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0)))))))
     (cond
      ((not (null? /val))
       (display-list-flush "b" (+ (- (@V (@Get_n (@I) 3)) (@V (@Get_n (@I) 2))) 1))
       (@Paste_Over (@Make //T_/Number /n '()))
       (set! /change (+ /change 1)))))))
  ((and (= (@ST (@I)) //T_/Rel_/Seg) (= (@ST (@Get_n (@I) 2)) //T_/Number) (= (@ST (@Get_n (@I) 3)) //T_/Number) (<= (@V (@Get_n (@I) 3)) 4))
   (set! /name (@CP_Var_Name (@Get_n (@I) 1)))
   (cond
    ((not (null? /name))
     (set! /i (@V (@Get_n (@I) 2)))
     (set! /n 0)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (set! /val (@CP_Get /l (concat /name (list (- /i)))))
       (cond
        ((null? /val)
         (set! /fl_flag1 1))
        ((not (= (@ST /val) //T_/Number))
         (set! /val '())
         (set! /fl_flag1 1))
        (#t
         (set! /n (+ (* /n 256) (@V /val)))
         (set! /i (+ /i 1))
         (cond
          ((>= /i (+ (@V (@Get_n (@I) 2)) (@V (@Get_n (@I) 3))))
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0)))))))
     (cond
      ((not (null? /val))
       (display-list-flush "b" (@V (@Get_n (@I) 3)))
       (@Paste_Over (@Make //T_/Number /n '()))
       (set! /change (+ /change 1)))))))
  ((and (= (@ST (@I)) //T_/Variable) (@Ends_With? (@V (@I)) "_INDEX"))
   ; Don't update known index values! 
  )
  ((not (null? /name))
   (set! /val (@CP_Get /l /name))
   (cond
    ((not (null? /val))
     ; Don't update only part of a struct 
     ; or the first component of an array reference. 
     ; Also don't replace a[FOO] by a[n] 
     ; Also don't replace a register by another register 
     ; (it may mess up DSECT pointers) 
     ; Also don't replace a component by its parent/gparent 
     ; Also rX = rY causes problems when both are reg_inits 
     (cond
      ((and (= (@Posn_n) 2) (= (@ST (@Parent)) //T_/Struct))
       #t)
      ((and (= (@Posn_n) 1) (or (= (@ST (@Parent)) //T_/Aref) (= (@ST (@Parent)) //T_/Sub_/Seg) (= (@ST (@Parent)) //T_/Rel_/Seg) (= (@ST (@Parent)) //T_/Final_/Seg)))
       #t)
      ((and (> (gen-length (@Posn)) 1) (@Is_Mem? (@Parent)) (or (> (gen-length /name) 1) (not-member (car /name) /x86_regs)) (or (= (@ST (@GParent)) //T_/Struct) (= (@ST (@GParent)) //T_/Struct_/Lvalue)))
       #t)
      ((and (> (gen-length (@Posn)) 1) (@Is_Mem? (@GParent)) (or (> (gen-length /name) 1) (not-member (car /name) /x86_regs)) (or (= (@ST (@GGParent)) //T_/Struct) (= (@ST (@GGParent)) //T_/Struct_/Lvalue)))
       #t)
      ((and (= (@ST /val) //T_/Variable) (= (@ST (@I)) //T_/Variable) (not-member (@V (@I)) /registers) (not-member (@V /val) /vcons))
       #t)
      ((and (= (@ST /val) //T_/Variable) (= (@ST (@I)) //T_/Variable) (member (@V /val) /registers) (member (@V (@I)) /registers))
       #t)
      ((and (> (gen-length (@Posn)) 0) (not (null? (@Parent))) (@Equal? (@Parent) /val))
       #t)
      ((and (= (@ST /val) //T_/Minus) (= (@ST (@Get_n /val 1)) //T_/Plus))
       #t)
      ((and (= (@ST /val) //T_/Variable) (member (@V /val) /reg_inits))
       ; don't paste a reg init anywhere! 
      )
      ((and (= (@ST /val) //T_/Variable) (not (null? (gethash /rename (@V /val)))))
       ; don't paste a renamed variable 
      )
      ((and (equal? (car /name) /a_name) (member /a_name (@Used /val)))
       ; something wierd 
      )
      (#t
       (display-list-flush "x")
       (@Paste_Over /val)
       (cond
        ((> /change 200)
         (display-list "posn = " (@Posn))
         (display-list "name = " /name)
         (display-list-flush "val = ")
         (@Print_WSL /val "")
         (display-list "change = " /change)
         (error "@CP_Update:" "too many updates!")))
       (set! /change (+ /change 1)))))))))

(define (/foreach-constant_propagation-8 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (let ((/__/O/K 1))
      (vector-set! /__/Match_array 0 /n)
      (set! /__/O/K (@New_Match  /%const__constant_propagation__2 (@I) /__/O/K))
      (cond
       ((= /__/O/K 1)
        (let ((/__/S_save //S))
         (set! //S (vector-ref /__/Match_array 1))
         (set! /call (car //S))
         (set! //S /__/S_save)))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))))

(define (/foreach-constant_propagation-9 //Depth //A/S_/Type)
 (cond
  ((and #f (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (< (@V (@I)) 0))
   (wsl-set! //Entries (list 0 '()) (- (@V (@I)))))))

(define (/foreach-constant_propagation-10 //Depth //A/S_/Type)
 (cond
  ((and #f (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (< (@V (@I)) 0))
   (wsl-set! //Entries (list 0 '()) (- (@V (@I)))))))

(define (/foreach-constant_propagation-11 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Equal)
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /cc)
    (set! /__/O/K (@New_Match  /%const__constant_propagation__9 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (display-list-flush "c0")
      (@Paste_Over //B0))))
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /cc)
    (set! /__/O/K (@New_Match  /%const__constant_propagation__10 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (display-list-flush "c1")
      (@Paste_Over (@And (@Not //B0) //B1)))))
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /cc)
    (set! /__/O/K (@New_Match  /%const__constant_propagation__11 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (display-list-flush "c2")
      (@Paste_Over (@And (@And (@Not //B0) (@Not //B1)) //B2)))))
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /cc)
    (set! /__/O/K (@New_Match  /%const__constant_propagation__12 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (display-list-flush "c3")
      (@Paste_Over (@And (@And (@And (@Not //B0) (@Not //B1)) (@Not //B2)) //B3))))))
  ((= (@ST (@I)) //T_/Not_/Equal)
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /cc)
    (set! /__/O/K (@New_Match  /%const__constant_propagation__13 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (display-list-flush "C0")
      (@Paste_Over (@Not //B0)))))
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /cc)
    (set! /__/O/K (@New_Match  /%const__constant_propagation__14 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (display-list-flush "C1")
      (@Paste_Over (@Or //B0 (@Not //B1))))))
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /cc)
    (set! /__/O/K (@New_Match  /%const__constant_propagation__15 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (display-list-flush "C2")
      (@Paste_Over (@Or (@Or //B0 //B1) (@Not //B2))))))
   (let ((/__/O/K 1))
    (vector-set! /__/Match_array 0 /cc)
    (set! /__/O/K (@New_Match  /%const__constant_propagation__16 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (display-list-flush "C3")
      (@Paste_Over (@Or (@Or (@Or //B0 //B1) //B2) (@Not //B3)))))))))

(define (/foreach-constant_propagation-12 //Depth //A/S_/Type)
 (cond
  ((and (and (not (= (@ST (@I)) //T_/And)) (not (= (@ST (@I)) //T_/Or)) (not (= (@ST (@I)) //T_/Not)) (not (= (@ST (@I)) //T_/X_/B/Funct_/Call))) (member /cc_name (@Variables (@I))))
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__constant_propagation__17 (@I) /__/O/K))
    (cond
     ((not (= /__/O/K 1))
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__constant_propagation__18 (@I) /__/O/K))
       (cond
        ((not (= /__/O/K 1))
         (set! //O/K 0))))))))))

(define (/foreach-constant_propagation-13 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__17 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (@Paste_Over //B1))
   (#t
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__constant_propagation__18 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (@Paste_Over //B2))))))))

(define (/foreach-constant_propagation-14 //Depth //A/S_/Type)
 (cond
  ((and (and (not (= (@ST (@I)) //T_/And)) (not (= (@ST (@I)) //T_/Or)) (not (= (@ST (@I)) //T_/Not)) (not (= (@ST (@I)) //T_/X_/B/Funct_/Call))) (member /cc_name (@Variables (@I))))
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__constant_propagation__21 (@I) /__/O/K))
    (cond
     ((not (= /__/O/K 1))
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__constant_propagation__22 (@I) /__/O/K))
       (cond
        ((not (= /__/O/K 1))
         (set! //O/K 0))))))))))

(define (/foreach-constant_propagation-15 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__21 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (@Paste_Over //B1))
   (#t
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__constant_propagation__22 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (@Paste_Over //B2))))))))

(define (/foreach-constant_propagation-16 //Depth //A/S_/Type)
 (cond
  ((equal? (@Variables (@I)) (list /cc_name))
   (set! /newtests '())
   (set! /tests /orig_tests)
   (for-in /cc_val /orig_cc_values 
    (begin
     (set! //L (@CP_Put0 //L (list /cc_name) /cc_val))
     (@Edit)
     (let ((//Migration-save //Migration))
      (set! //Migration 1)
      (set! //L (@CP_Update  //L))
      (set! //Migration //Migration-save))
     (cond
      ((= (@ST (@I)) //T_/True)
       (set! /newtests (cons (car /tests) /newtests))))
     (@Undo_Edit)
     (set! /tests (cdr /tests))))
   (cond
    ((= (gen-length /newtests) 0)
     (@Paste_Over (@Make //T_/False '() '())))
    ((= (gen-length /newtests) 1)
     (@Paste_Over (car /newtests)))
    (#t
     (@Paste_Over (@Make //T_/Or '() /newtests)))))))

(define (/foreach-constant_propagation-17 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (equal? (- (@V (@I))) /dispatch))
   (@Paste_Over (@Make //T_/Call (- (+ //N 1)) '()))
   (@Paste_Before (@Make //T_/Comment "FIXME: recursion detected!!" '())))))

(define (/foreach-constant_propagation-18 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) /destination))
   (@Paste_Over /val))))

(define (/foreach-constant_propagation-19 //Depth //A/S_/Type)
 (cond
  ((and (= //O/K 1) (= (@ST (@I)) //T_/Assignment) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Struct_/Lvalue)) (@LR_Equal? (@Get_n (@Get_n (@I) 1) 1) /v))
   (cond
    ((and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /dispatch_codes))
     (set! /values (union-n (list (@Get_n (@Get_n (@I) 1) 2)) /values)))
    ((and (or (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Variable) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Struct)) (or (member (@CP_Var_Name (@Get_n (@Get_n (@I) 1) 2)) //Vcons) (member (@CP_Var_Name (@Get_n (@Get_n (@I) 1) 2)) //Constants)))
     (set! /values (union-n (list (@Get_n (@Get_n (@I) 1) 2)) /values)))
    (#t
     (set! //O/K 0))))))

(define (/foreach-constant_propagation-20 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Number) (equal? (@V (@I)) /val))
   (@Paste_Over /notused)
   (display-list-flush "N"))))

(define (/foreach-constant_propagation-21 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@Get_n (@I) (@Size (@I)))) //T_/Exit)
   (@Paste_Over (@Skips)))))

(define (/foreach-constant_propagation-22 //Depth //A/S_/Type)
 (cond
  ((and #f (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (< (@V (@I)) 0))
   (wsl-set! //Entries (list 0 '()) (- (@V (@I)))))))

(define (/foreach-constant_propagation-23 //Depth //A/S_/Type)
 (cond
  ((and #f (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (< (@V (@I)) 0))
   (wsl-set! //Entries (list 0 '()) (- (@V (@I)))))))

(define (/foreach-constant_propagation-24 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Exit) (@Left?))
   (set! /move 1)
   (@Left))
  (#t
   (set! /move 0)))
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (not (@Cs? (@Get_n (@I) 2))) (not (@Cs? (@Get_n (@I) 3))))
   (set! /calls (union-n (list (@V (@Get_n (@I) 1))) /calls))
   (@Paste_Over (@Skip)))
  (#t
   (set! //O/K 0)))
 (cond
  ((= /move 1)
   (@Right))))

(define (/foreach-constant_propagation-25 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (= (@Size (@I)) 1))
   (cond
    ((and (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (not (null? (gethash //D/S/E/C/Ts (@V (@Get_n (@Get_n (@I) 1) 1))))) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Variable) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /registers))
     (cond
      ((null? (gethash //D/S/E/C/T_reg (@V (@Get_n (@Get_n (@I) 1) 1))))
       (puthash //D/S/E/C/T_reg (@V (@Get_n (@Get_n (@I) 1) 1)) (@V (@Get_n (@Get_n (@I) 1) 2)))
       (set! //D/S/E/C/T_list (cons (@V (@Get_n (@Get_n (@I) 1) 1)) //D/S/E/C/T_list)))
      ((not (equal? (gethash //D/S/E/C/T_reg (@V (@Get_n (@Get_n (@I) 1) 1))) (@V (@Get_n (@Get_n (@I) 1) 2))))
       (puthash //D/S/E/C/T_bad (@V (@Get_n (@Get_n (@I) 1) 1)) 1))))))))

(define (/foreach-constant_propagation-26 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__33 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__n_save /n)
          (/__v_save /v))
     (set! /n (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /v) //T_/Variable) (= (@ST /n) //T_/Number) (or (member (@V /v) /registers) (member (@V /v) /reg_inits)))
       (set! /new (@Make_Name (concat (string-append (@N_String (@V /v)) "__") (@String (@V /n)))))
       (@Paste_Over (@Make //T_/Variable /new '()))))
     (set! /n /__n_save)
     (set! /v /__v_save))))))

(define (/foreach-constant_propagation-27 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__34 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__n_save /n)
          (/__v_save /v))
     (set! /n (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /v) //T_/Variable) (= (@ST /n) //T_/Number) (or (member (@V /v) /registers) (member (@V /v) /reg_inits)))
       (set! /new (@Make_Name (concat (string-append (@N_String (@V /v)) "__") (@String (@V /n)))))
       (@Paste_Over (@Make //T_/Var_/Lvalue /new '()))))
     (set! /n /__n_save)
     (set! /v /__v_save))))))

(define (/foreach-constant_propagation-28 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (not (null? (gethash //D/S/E/C/Ts (@V (@Get_n (@I) 1))))) (null? (gethash //D/S/E/C/T_bad (@V (@Get_n (@I) 1)))))
       (set! /name (@V (@Get_n (@I) 1)))
       (cond
        ((null? (gethash //D/S/E/C/T_init /name))
         (puthash //D/S/E/C/T_init /name (@Get_n (@I) 2))
         (puthash //D/S/E/C/T_init_count /name 1))
        ((@Equal? (gethash //D/S/E/C/T_init /name) (@Get_n (@I) 2))
         (puthash //D/S/E/C/T_init_count /name (+ (gethash //D/S/E/C/T_init_count /name) 1)))
        ((and (= (@ST (gethash //D/S/E/C/T_init /name)) //T_/Variable) (equal? (@Variables (@Get_n (@I) 2)) (list (@V (gethash //D/S/E/C/T_init /name)))))
         (puthash //D/S/E/C/T_init /name (@Get_n (@I) 2))
         (puthash //D/S/E/C/T_init_count /name 1))
        ((or (not (= //T_/Variable (@ST (@Get_n (@I) 2)))) (not (equal? (list (@V (@Get_n (@I) 2))) (@Variables (gethash //D/S/E/C/T_init /name)))))
         (puthash //D/S/E/C/T_bad /name 1)))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))))

(define (/foreach-constant_propagation-29 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   (cond
    ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@I) 1)) /registers) (not (and (= (@ST (@Get_n (@I) 2)) //T_/Variable) (member (@V (@Get_n (@I) 2)) /reg_inits))))
     (puthash /reg_init_count (@V (@Get_n (@I) 1)) (+ (gethash /reg_init_count (@V (@Get_n (@I) 1))) 1))))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@I) 1)) /registers) (not (and (= (@ST (@Get_n (@I) 2)) //T_/Variable) (member (@V (@Get_n (@I) 2)) /reg_inits))))
       (puthash /reg_init_count (@V (@Get_n (@I) 1)) (+ (gethash /reg_init_count (@V (@Get_n (@I) 1))) 1)))))))))

(define (/foreach-constant_propagation-30 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (not (equal? (@V (@Get_n (@I) 1)) /pop_regs)) (not (equal? (@V (@Get_n (@I) 1)) /chain_reg)))
   (for-in /var (@Assigned (@I)) 
    (cond
     ((member /var /registers)
      (puthash /reg_init_count /var (+ (gethash /reg_init_count /var) 1))))))))

(define (/foreach-constant_propagation-31 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (not (null? (gethash //D/S/E/C/T_reg (@V (@Get_n (@I) 1))))) (null? (gethash //D/S/E/C/T_bad (@V (@Get_n (@I) 1)))))
      (display-list-flush "X")
      (@Delete)
      (cond
       ((> (@Posn_n) (@Size (@Parent)))
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0))))
     ((@Right?)
      (@Right)
      (set! /fl_flag1 0))
     (#t
      (set! /fl_flag1 1))))
   (@Up)
   (cond
    ((= (@Size (@I)) 0)
     (@Delete))))))

(define (/foreach-constant_propagation-32 //Depth //A/S_/Type)
 (set! /dsects_done '())
 (@Down_Last)
 (set! /fl_flag2 0)
 (while (= /fl_flag2 0) 
  (begin
   (cond
    ((= (@ST (@I)) //T_/Assignment)
     (@Down)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (cond
       ((not (and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@I) 1)) /registers) (not (null? (gethash /reg_/D/S/E/C/Ts (@V (@Get_n (@I) 1))))) (not (and (= (@ST (@Get_n (@I) 2)) //T_/Variable) (member (@V (@Get_n (@I) 2)) /reg_inits))) (null? (gethash /done (gethash /reg_/D/S/E/C/Ts (@V (@Get_n (@I) 1)))))))
        (for-in /name (@Sort_List (intersection-n /dsects_done (@Set_Difference (@Used (@I)) (@Assigned (@I))))) 
         (begin
          (set! /var (@Make //T_/Var_/Lvalue /name '()))
          (set! /e (gethash //D/S/E/C/T_init (@V /var)))
          (@Up)
          (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /e))))))
          (@Right)
          (@Down)))
        (set! /fl_flag1 1))
       (#t
        (for-in /name (@Sort_List (gethash /reg_/D/S/E/C/Ts (@V (@Get_n (@I) 1)))) 
         (begin
          (set! /var (@Make //T_/Var_/Lvalue /name '()))
          (set! /e (gethash //D/S/E/C/T_init (@V /var)))
          (display-list-flush " " (@N_String /name) " ")
          (@Up)
          (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /e))))))
          (@Down)))
        ; Record that we have done this one: 
        (set! /dsects_done (union-n (gethash /reg_/D/S/E/C/Ts (@V (@Get_n (@I) 1))) /dsects_done))
        (puthash /done (gethash /reg_/D/S/E/C/Ts (@V (@Get_n (@I) 1))) 1)
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0))))))
     (@Up))
    ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (not (equal? (@V (@Get_n (@I) 1)) /pop_regs)))
     (for-in /v (@Sort_List (intersection-n (@Assigned (@I)) /registers)) 
      (cond
       ((and (not (null? (gethash /reg_/D/S/E/C/Ts /v))) (null? (gethash /done (gethash /reg_/D/S/E/C/Ts /v))))
        (for-in /name (gethash /reg_/D/S/E/C/Ts /v) 
         (begin
          (set! /var (@Make //T_/Var_/Lvalue /name '()))
          (set! /e (gethash //D/S/E/C/T_init (@V /var)))
          (display-list-flush " " (@N_String /name) " ")
          (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /e)))))))))))))
   (cond
    ((and (null? (intersection-n /pointers (@Assigned (@I)))) (not (null? (intersection-n /pointers (@Used (@I))))))
     (for-in /v (intersection-n /pointers (@Used (@I))) 
      (puthash /done /v '()))))
   (cond
    ((not (@Left?))
     (set! /fl_flag2 1))
    (#t
     (@Left)
     (set! /fl_flag2 0))))))

(define (/foreach-constant_propagation-33 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Comment) (equal? (@V (@I)) " <ENTRY POINT> "))
   ; Step past register initialisation code: 
   (while (and (@Right?) (= (@ST (@I)) //T_/Comment)) 
    (@Right))
   (while (and (@Right?) (not (null? (intersection-n (@Used (@I)) /all_inits)))) 
    (@Right))
   (cond
    ((@Left?)
     (@Left)))
   (set! /v (@Make //T_/Var_/Lvalue /var '()))
   (set! /e (gethash //D/S/E/C/T_init /var))
   (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /e)))))))))

(define (/foreach-constant_propagation-34 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__26 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e_save /e)
          (/__v_save /v))
     (set! /e (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /e) //T_/Number) (not (= (@V /e) 0)) (member (@V /e) /dispatch_codes))
       (set! /elt (@Struct_Elts /v))
       (cond
        ((and (not (null? /elt)) (not-member /elt //R) (not (equal? /elt (list /a_name))))
         (set! /done 0)
         (set! //R (union-n //R (list /elt))))))
      ((and (= (@ST /e) //T_/Negate) (= (@ST (@Get_n /e 1)) //T_/Number) (member (- (@V (@Get_n /e 1))) /dispatch_codes))
       (set! /elt (@Struct_Elts /v))
       (cond
        ((and (not (null? /elt)) (not-member /elt //R) (not (equal? /elt (list /a_name))))
         (set! /done 0)
         (set! //R (union-n //R (list /elt))))))
      ((and (= (@ST /e) //T_/Sub_/Seg) (= (@ST (@Get_n /e 2)) //T_/Number))
       ; Skip statements like rX := FOO[9..12] 
      )
      ((and (or (= (@ST /e) //T_/Variable) (= (@ST /e) //T_/Struct) (= (@ST /e) //T_/Rel_/Seg) (= (@ST /e) //T_/Sub_/Seg)) (or (member (@Struct_Elts /e) //R) (and (= (@ST /e) //T_/Variable) (@Starts_With? (@V /e) "NOTUSED_"))))
       (set! /elt (@Struct_Elts /v))
       (cond
        ((and (not (null? /elt)) (not-member /elt //R) (not (equal? /elt (list /a_name))))
         (set! /done 0)
         (set! //R (union-n //R (list /elt)))))))
     (set! /e /__e_save)
     (set! /v /__v_save)))))
 (cond
  ((= (@ST (@I)) //T_/Var)
   (for-in /var (@Assigned (@Get_n (@I) 1)) 
    (cond
     ((@Starts_With? /var "EODAD_")
      (set! //R (union-n //R (list (list /var))))))))))

(define (/foreach-constant_propagation-35 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__26 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e_save /e)
          (/__v_save /v))
     (set! /e (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /e) //T_/Number) (not (= (@V /e) 0)) (member (@V /e) /dispatch_codes))
       (set! /elt (@Struct_Elts /v))
       (cond
        ((and (not (null? /elt)) (not-member /elt //R) (not (equal? /elt (list /a_name))))
         (set! /done 0)
         (set! //R (union-n //R (list /elt))))))
      ((and (= (@ST /e) //T_/Negate) (= (@ST (@Get_n /e 1)) //T_/Number) (member (- (@V (@Get_n /e 1))) /dispatch_codes))
       (set! /elt (@Struct_Elts /v))
       (cond
        ((and (not (null? /elt)) (not-member /elt //R) (not (equal? /elt (list /a_name))))
         (set! /done 0)
         (set! //R (union-n //R (list /elt))))))
      ((and (= (@ST /e) //T_/Sub_/Seg) (= (@ST (@Get_n /e 2)) //T_/Number))
       ; Skip statements like rX := FOO[9..12] 
      )
      ((and (or (= (@ST /e) //T_/Variable) (= (@ST /e) //T_/Struct) (= (@ST /e) //T_/Rel_/Seg) (= (@ST /e) //T_/Sub_/Seg)) (or (member (@Struct_Elts /e) //R) (and (= (@ST /e) //T_/Variable) (@Starts_With? (@V /e) "NOTUSED_"))))
       (set! /elt (@Struct_Elts /v))
       (cond
        ((and (not (null? /elt)) (not-member /elt //R) (not (equal? /elt (list /a_name))))
         (set! /done 0)
         (set! //R (union-n //R (list /elt)))))))
     (set! /e /__e_save)
     (set! /v /__v_save)))))
 (cond
  ((= (@ST (@I)) //T_/Var)
   (for-in /var (@Assigned (@Get_n (@I) 1)) 
    (cond
     ((@Starts_With? /var "EODAD_")
      (set! //R (union-n //R (list (list /var))))))))))

(define (/foreach-constant_propagation-36 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (cond
    ((and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (not (null? (gethash //Code_/Hash (@V (@Get_n (@Get_n (@I) 1) 2))))) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Struct_/Lvalue)))
     (puthash /var_value (@Struct_Elts (@Get_n (@Get_n (@I) 1) 1)) (@V (@Get_n (@Get_n (@I) 1) 2))))))))

(define (/foreach-constant_propagation-37 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (cond
    ((or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Struct_/Lvalue))
     (set! /elt (@Struct_Elts (@Get_n (@Get_n (@I) 1) 1)))
     (cond
      ((and (not (null? (gethash /var_value /elt))) (or (not (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number)) (not (equal? (@V (@Get_n (@Get_n (@I) 1) 2)) (gethash /var_value /elt)))))
       (puthash /var_value /elt '()))))))
  ((= (@ST (@I)) //T_/A_/Proc_/Call)
   (for-in /elt (@Elts_Assigned (@I)) 
    (cond
     ((not (null? (gethash /var_value /elt)))
      (puthash /var_value /elt '())))))))

(define (/foreach-constant_propagation-38 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/Variable) (= (@ST (@I)) //T_/Struct))
   (set! /elt (@Struct_Elts (@I)))
   (cond
    ((not (null? (gethash /var_value /elt)))
     (@Paste_Over (@Make //T_/Number (gethash /var_value /elt) '()))
     (cond
      ((and (= (@ST (@Parent)) //T_/Assign) (= (@ST (@Get_n (@Parent) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Parent) 1)) /destination))
       (puthash /delete /elt (gethash /var_value /elt))
       (set! /codes (union-n (list (gethash /var_value /elt)) /codes)))))))))

(define (/foreach-constant_propagation-39 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (cond
    ((or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Struct_/Lvalue))
     (set! /elt (@Struct_Elts (@Get_n (@Get_n (@I) 1) 1)))
     (cond
      ((not (null? (gethash /delete /elt)))
       (@Delete))
      ((and (member (@V (@Get_n (@Get_n (@I) 1) 1)) /registers) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /codes))
       (@Delete))))))))

(define /%const__constant_propagation__1 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "r15") '()) (@Make 217 -1 '()))))))
(define /%const__constant_propagation__2 (@Make 7 '() (list (@Make 313 '() (list (@Make 207 (@Make_Name "destination") '()) (@Make 261 '() (list (@Make 205 1 '()))))) (@Make 17 '() (list (@Make 107 -2 '()))))))
(define /%const__constant_propagation__3 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -2 '()) (@Make 205 0 '()))))))))) (@Make 7 '() (list (@Make 305 -3 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 1 '()))))))))) (@Make 7 '() (list (@Make 305 -4 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 2 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
(define /%const__constant_propagation__4 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -2 '()) (@Make 205 0 '()))))))))) (@Make 7 '() (list (@Make 305 -3 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 1 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 2 '()))))))))))))
(define /%const__constant_propagation__5 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -2 '()) (@Make 205 0 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 1 '()))))))))))))
(define /%const__constant_propagation__6 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -2 '()) (@Make 205 1 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 0 '()))))))))))))
(define /%const__constant_propagation__7 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -2 '()) (@Make 205 0 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 2 '()))))))))))))
(define /%const__constant_propagation__8 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -2 '()) (@Make 205 0 '()))))))))) (@Make 7 '() (list (@Make 305 -3 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 3 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 205 1 '()))))))))))))
(define /%const__constant_propagation__9 (@Make 313 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 0 '()))))
(define /%const__constant_propagation__10 (@Make 313 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 1 '()))))
(define /%const__constant_propagation__11 (@Make 313 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 2 '()))))
(define /%const__constant_propagation__12 (@Make 313 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 3 '()))))
(define /%const__constant_propagation__13 (@Make 318 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 0 '()))))
(define /%const__constant_propagation__14 (@Make 318 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 1 '()))))
(define /%const__constant_propagation__15 (@Make 318 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 2 '()))))
(define /%const__constant_propagation__16 (@Make 318 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 3 '()))))
(define /%const__constant_propagation__17 (@Make 313 '() (list (@Make 207 (@Make_Name "cc") '()) (@Make 205 0 '()))))
(define /%const__constant_propagation__18 (@Make 318 '() (list (@Make 207 (@Make_Name "cc") '()) (@Make 205 0 '()))))
(define /%const__constant_propagation__19 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 0 '()))))))))) (@Make 7 '() (list (@Make 305 -2 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 1 '()))))))))) (@Make 7 '() (list (@Make 305 -3 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 2 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
(define /%const__constant_propagation__20 (@Make 114 '() (list (@Make 7 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 0 '()))))))))) (@Make 7 '() (list (@Make 305 -2 '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 1 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 2 '()))))))))))))
(define /%const__constant_propagation__21 (@Make 313 '() (list (@Make 207 (@Make_Name "cc") '()) (@Make 205 3 '()))))
(define /%const__constant_propagation__22 (@Make 318 '() (list (@Make 207 (@Make_Name "cc") '()) (@Make 205 3 '()))))
(define /%const__constant_propagation__23 (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Make 217 -1 '()) (@Make 205 0 '()))) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 0 '()))))))))) (@Make 7 '() (list (@Make 313 '() (list (@Make 263 -1 '()) (@Make 217 -2 '()))) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 3 '()))))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 205 1 '()))))))))))))
(define /%const__constant_propagation__24 (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "cc") '()) (@Make 217 -1 '()))))))))
(define /%const__constant_propagation__25 (@Make 17 '() (list (@Make 145 '() '()))))
(define /%const__constant_propagation__26 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 217 -2 '()))))))
(define /%const__constant_propagation__27 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Make 212 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 217 -1 '()) (@Make 205 4 '()))))))))
(define /%const__constant_propagation__28 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Make 217 -1 '()))))))
(define /%const__constant_propagation__29 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "r15") '()) (@Make 207 (@Make_Name "result_code") '()))))))
(define /%const__constant_propagation__30 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 261 '() (list (@Make 205 2 '()))))))))
(define /%const__constant_propagation__31 (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))
(define /%const__constant_propagation__32 (@Make 133 '() (list (@Make 17 '() (list (@Make 107 -1 '()) (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Make 217 -2 '()) (@Make 217 -3 '()))) (@Make 17 '() (list (@Make 107 -4 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))) (@Make 107 -5 '()) (@Make 110 '() (list (@Make 6 '() (list (@Make 512 -2 '()) (@Make 217 -6 '()))))) (@Make 107 -7 '()))))))
(define /%const__constant_propagation__33 (@Make 212 '() (list (@Make 207 (@Make_Name "a") '()) (@Make 220 '() (list (@Make 217 -1 '()) (@Make 217 -2 '()))) (@Make 205 4 '()))))
(define /%const__constant_propagation__34 (@Make 504 '() (list (@Make 501 (@Make_Name "a") '()) (@Make 220 '() (list (@Make 217 -1 '()) (@Make 217 -2 '()))) (@Make 205 4 '()))))
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
(set! /reg_inits (@Make_Set (my-map @Make_Name (list "__r0_init__" "__r1_init__" "__r2_init__" "__r3_init__" "__r4_init__" "__r5_init__" "__r6_init__" "__r7_init__" "__r8_init__" "__r9_init__" "__r10_init__" "__r11_init__" "__r12_init__" "__r13_init__" "__r14_init__"))))
(set! /date_time_functs (my-map @Make_Name (list "system_time_bin" "system_time_pkd" "system_YYYYDDD_pkd" "system_YYYYMMDD_pkd" "system_MMDDYYYY_pkd" "system_DDMMYYYY_pkd" "system_time_pkds" "system_YYYYDDD_pkds" "system_YYYYMMDD_pkds" "system_MMDDYYYY_pkds" "system_DDMMYYYY_pkds" "system_time_str" "system_YYYYDDD_str" "system_YYYYMMDD_str" "system_MMDDYYYY_str" "system_DDMMYYYY_str" "system_CCYYDDD_pkds" "system_CCYYDDD_str")))
(define (@Constant_Propagation_Test)
 (cond
  ((or (= (@ST (@I)) //T_/Condition) (= (@ST (@I)) //T_/Expression))
   (@Fail "Selection contains no statements"))
  (#t
   (@Pass))))

(define (@Constant_Propagation_Code //Data)
 (let ((//L-save //L)
       (//Entries-save //Entries)
       (//Call_/Path-save //Call_/Path)
       (//Constants-save //Constants)
       (//Vcons-save //Vcons)
       (/vcons-save /vcons)
       (//D/S/E/C/Ts-save //D/S/E/C/Ts)
       (//C/P_/State-save //C/P_/State)
       (//State_/Saves-save //State_/Saves)
       (//Unfold_/Dispatch-save //Unfold_/Dispatch)
       (/calls_processed-save /calls_processed)
       (/call_depth-save /call_depth)
       (/call_budget-save /call_budget)
       (/effort-save /effort)
       (/initial_call_budget-save /initial_call_budget)
       (/registers-save /registers)
       (/x86_regs-save /x86_regs)
       (/cc_name-save /cc_name)
       (/zf_name-save /zf_name)
       (/cf_name-save /cf_name)
       (/r1_name-save /r1_name)
       (/push_regs-save /push_regs)
       (/pop_regs-save /pop_regs)
       (/chain_reg-save /chain_reg)
       (/reg_stack-save /reg_stack)
       (/call_stack-save /call_stack)
       (/call_via_ptr-save /call_via_ptr)
       (/call_via_ptr_pars-save /call_via_ptr_pars)
       (/pack-save /pack)
       (//E/X/E/C_/C/I/C/S-save //E/X/E/C_/C/I/C/S)
       (/mvi (@Make_Name "mvi"))
       (/true-save /true)
       (/false-save /false)
       (/dispatch-save /dispatch)
       (//A/S_/Type-save //A/S_/Type)
       (/exit_flag-save /exit_flag)
       (/sizeof-save /sizeof)
       (/dispatch_codes-save /dispatch_codes)
       (/return_elts-save /return_elts)
       (//Code_/Hash-save //Code_/Hash)
       (/rename-save /rename)
       (/posn '())
       (/orig_program-save /orig_program)
       (//Notused_/Value-save //Notused_/Value)
       (//Migration-save //Migration))
  (set! //L (list (list 0 '())))
  (set! //Entries '())
  (set! //Call_/Path '())
  (set! //Constants '())
  (set! //Vcons '())
  (set! /vcons '())
  (set! //D/S/E/C/Ts (hash-table))
  (set! //C/P_/State '())
  (set! //State_/Saves 0)
  (set! //Unfold_/Dispatch 1)
  (set! /calls_processed 0)
  (set! /call_depth 0)
  (set! /call_budget 25600)
  (set! /effort 0)
  (set! /initial_call_budget 0)
  (set! /registers (@Make_Set (my-map @Make_Name (list "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15" "ax" "bx" "cx" "dx" "bp" "si" "di" "cs" "ds" "ss" "es"))))
  (set! /x86_regs (@Make_Set (my-map @Make_Name (list "ax" "bx" "cx" "dx"))))
  (set! /cc_name (@Make_Name "cc"))
  (set! /zf_name (@Make_Name "zf"))
  (set! /cf_name (@Make_Name "cf"))
  (set! /r1_name (@Make_Name "r1"))
  (set! /push_regs (@Make_Name "push_regs"))
  (set! /pop_regs (@Make_Name "pop_regs"))
  (set! /chain_reg (@Make_Name "chain_reg"))
  (set! /reg_stack (@Make_Name "reg_stack"))
  (set! /call_stack (@Make_Name "call_stack"))
  (set! /call_via_ptr (@Make_Name "call_via_ptr"))
  (set! /call_via_ptr_pars (@Make_Name "call_via_ptr_pars"))
  (set! /pack (@Make_Name "pack"))
  (set! //E/X/E/C_/C/I/C/S (@Make_Name "EXEC_CICS"))
  (set! /true (@Make //T_/True '() '()))
  (set! /false (@Make //T_/False '() '()))
  (set! /dispatch 0)
  (set! //A/S_/Type (@AS_Type))
  (set! /exit_flag (@Make_Name "exit_flag"))
  (set! /sizeof (@Make_Name "sizeof"))
  (set! /dispatch_codes '())
  (set! /return_elts '())
  (set! //Code_/Hash (hash-table))
  (set! /rename (hash-table))
  (set! /orig_program (@I))
  (set! //Notused_/Value (hash-table))
  (set! //Migration 1)
  ; If Data contains a space, then it is two numbers: effort plus budget. 
  ; Otherwise, Data is the call budget. 
  ; effort >= 2 -- if dispatch is to a variable which is only assigned 
  ;                code values or constant values, then convert to an IF 
  ; effort >= 3 -- also if dispatch is assigned to a non-constant variable 
  ;                then convert to a call_via_ptr if there is a return code 
  (let ((/list (my-map @String_To_Num (@Split (@String //Data)))))
   (cond
    ((= (gen-length /list) 3)
     (set! //Migration (wsl-ref /list 1))
     (set! /effort (wsl-ref /list 2))
     (set! /call_budget (wsl-ref /list 3)))
    ((= (gen-length /list) 2)
     (set! /effort (wsl-ref /list 1))
     (set! /call_budget (wsl-ref /list 2)))
    ((and (= (gen-length /list) 1) (> (wsl-ref /list 1) 0))
     (set! /call_budget (wsl-ref /list 1)))))
  (display-list "Constant_Propagation effort = " /effort " budget = " /call_budget)
  (set! /initial_call_budget /call_budget)
  (cond
   ((member //T_/A_/S (@Stat_Types (@I)))
    (set! /posn (@Posn))
    (@Find_Type //T_/A_/S)
    (set! /dispatch_codes (@CP_Find_Dispatch_Codes (@Make_Name "dispatch")))
    (set! /return_elts (@CP_Find_Return_Elts /dispatch_codes))
    (@Goto '())
    (for-in /code /dispatch_codes 
     (cond
      ((> /code 0)
       (puthash //Code_/Hash /code 1))))
    (set! /rename (@FD_Rename_DSECT_Code_Stores  //Code_/Hash /rename))
    (cond
     ((and #f (> /call_budget 200))
      (@CP_Once_Called_Codes //Code_/Hash)))
    (@Goto /posn)
    (set! /return_elts (union-n /return_elts (list (list (@Make_Name "r14")))))
    (set! /return_elts (union-n /return_elts (@CP_Find_Return_Elts /dispatch_codes))))
   (#t
    (set! /return_elts '())))
  ; If dispatch destination is a symbolic constant, check for a return address 
  ; in a register and if found, then generate a procedure call and dispatch 
  ; on the register 
  ; Ignore pop_regs calls and rx := __rx_init__ assignments when computing 
  ; Constants: If the address is taken of a variable, then it may not be constant! 
  (@Edit)
  (@Foreach_Statement /foreach-constant_propagation-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //Constants '())
  (for-in /v (union-n (@Set_Difference (@Used (@I)) (@Assigned (@I))) /reg_inits) 
   (set! //Constants (cons (list /v) //Constants)))
  (set! //Constants (union-n (@Make_Set //Constants) (@Set_Difference (@Elts_Used (@I)) (@Elts_Assigned (@I)))))
  (@Undo_Edit)
  (let ((/new-save /new)
        (/elt-save /elt))
   (set! /new '())
   (set! /elt '())
   (for-in /elt //Constants 
    (cond
     ((and (= (gen-length /elt) 1) (@Starts_With? (car /elt) "NOTUSED_"))
      #t)
     ((< (last-1 /elt) 0)
      ; Ignore byte references unless the whole field is also constant 
     )
     (#t
      (set! /new (cons /elt /new)))))
   (set! //Constants (@Make_Set /new))
   (set! /new /new-save)
   (set! /elt /elt-save))
  (let ((/addr-save /addr))
   (set! /addr '())
   (@Foreach_Expn /foreach-constant_propagation-2 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! //Constants (@Set_Difference //Constants (@Make_Set /addr)))
   (set! /addr /addr-save))
  ; Anything which is assigned from a vcon (v_XXX) can be treated as a constant 
  ; as far as dispatch processing is concerned: ie use call_via_ptr on it. 
  ; But don't include registers here! 
  (let ((//Vcons_vals-save //Vcons_vals))
   (set! //Vcons_vals (hash-table))
   (@Foreach_Statement /foreach-constant_propagation-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /reg /registers 
    (begin
     (set! //Vcons (@Set_Difference //Vcons (list (list /reg))))
     (puthash //Vcons_vals (list (list /reg)) '())))
   (puthash //Vcons_vals (list (list (@Make_Name "destination"))) '())
   (@Foreach_Expn /foreach-constant_propagation-4 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /var (@Variables (@I)) 
    (cond
     ((or (@Starts_With? /var "V_") (@Starts_With? /var "v_"))
      (set! /vcons (union-n (list /var) /vcons)))))
   (set! //Vcons_vals //Vcons_vals-save))
  (display-list "Call Budget = " /call_budget)
  (set! //D/S/E/C/Ts (@CP_Find_DSECTs  //D/S/E/C/Ts))
  (display-list "DSECTs = " (@Join " " (my-map @N_String (@Hash_Keys //D/S/E/C/Ts))))
  (cond
   ((null? (@Posn))
    (@CP_Constant_DSECT_Pointers //D/S/E/C/Ts)))
  (set! //L (@CP_Generic  //L))
  (cond
   ((not (null? (@Hash_Keys /rename)))
    (set! /posn (@Posn))
    (@Goto '())
    (@FD_Restore_DSECT_Code_Stores /rename)
    (@Goto /posn)))
  (display-list " ")
  (display-list "Simplifying the result")
  (@Trans //T/R_/Simplify "")
  (set! //L //L-save)
  (set! //Entries //Entries-save)
  (set! //Call_/Path //Call_/Path-save)
  (set! //Constants //Constants-save)
  (set! //Vcons //Vcons-save)
  (set! /vcons /vcons-save)
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-save)
  (set! //C/P_/State //C/P_/State-save)
  (set! //State_/Saves //State_/Saves-save)
  (set! //Unfold_/Dispatch //Unfold_/Dispatch-save)
  (set! /calls_processed /calls_processed-save)
  (set! /call_depth /call_depth-save)
  (set! /call_budget /call_budget-save)
  (set! /effort /effort-save)
  (set! /initial_call_budget /initial_call_budget-save)
  (set! /registers /registers-save)
  (set! /x86_regs /x86_regs-save)
  (set! /cc_name /cc_name-save)
  (set! /zf_name /zf_name-save)
  (set! /cf_name /cf_name-save)
  (set! /r1_name /r1_name-save)
  (set! /push_regs /push_regs-save)
  (set! /pop_regs /pop_regs-save)
  (set! /chain_reg /chain_reg-save)
  (set! /reg_stack /reg_stack-save)
  (set! /call_stack /call_stack-save)
  (set! /call_via_ptr /call_via_ptr-save)
  (set! /call_via_ptr_pars /call_via_ptr_pars-save)
  (set! /pack /pack-save)
  (set! //E/X/E/C_/C/I/C/S //E/X/E/C_/C/I/C/S-save)
  (set! /true /true-save)
  (set! /false /false-save)
  (set! /dispatch /dispatch-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! /exit_flag /exit_flag-save)
  (set! /sizeof /sizeof-save)
  (set! /dispatch_codes /dispatch_codes-save)
  (set! /return_elts /return_elts-save)
  (set! //Code_/Hash //Code_/Hash-save)
  (set! /rename /rename-save)
  (set! /orig_program /orig_program-save)
  (set! //Notused_/Value //Notused_/Value-save)
  (set! //Migration //Migration-save)))

(define (@CP_Find_DSECTs //D/S/E/C/Ts-par)
 (let ((//D/S/E/C/Ts-save //D/S/E/C/Ts)
       (funct-result '()))
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-par)
  (@Foreach_Expn /foreach-constant_propagation-5 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Lvalue /foreach-constant_propagation-6 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result //D/S/E/C/Ts)
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-save)
  funct-result))

(define (@CP_Save_State)
 (set! //State_/Saves (+ //State_/Saves 1))
 (set! //C/P_/State (cons /calls_processed //C/P_/State))
 (set! //C/P_/State (cons /call_budget //C/P_/State))
 (set! /call_budget (quotient /call_budget 2)))

(define (@CP_Restore_State)
 (set! /call_budget (car //C/P_/State))
 (set! //C/P_/State (cdr //C/P_/State))
 (set! /calls_processed (car //C/P_/State))
 (set! //C/P_/State (cdr //C/P_/State)))

; L is a list of the form: <<n1, Ln1>, <n2, Ln2>, ...>
; where Lni are the lists of variables with known values. n1, n2, etc 
; are terminal values (in increasing order), and Ln1, Ln2 etc. are the 
; lists of (variable, value) pairs when the item terminates with 
; the corresponding terminal value. 
; Variable values are propagated through the current item. 
; The item is also updated by replacing variables with values. 
; All values are recorded as WSL items. 
; Variables are recorded as either <name> for simple variables, 
; or <name, n1, n2, ...> where name[n1][n2]... is a known array element, 
; or <name, name1, name2, ...> where name.name1.name2... is a structure reference. 
; Eg if a[2][3] = 1, then <<a, 2, 3>, @Make(T_Number, 1, < >)> 
; appears in the list for level zero. 
; Similarly if foo.bar.baz = 3 then <<foo, bar, baz>, @Make(T_Number, 3, < >)> 
; appears in the list for level zero. 
; The variable `Entries' will be set to the array of value lists 
; (one per action) when we enter an action system. 
; Proc_Summaries will be updated with the summaries of local procedures 
; when we enter a WHERE structure. When a call is encountered, 
; we can update L by processing the summary 
; Call_Path contains the `stack' of proc calls being processed. This is used 
; to detect a recursive call and prevent our simple-minded algorithm from 
; looping indefinitely. 
; Constants lists the simple variables which are not assigned to and which 
; can therefore also be treated as constants. A dispatch to a constant may be 
; a procedure call: check for a return address in r14. 
; NB Local variables in a VAR structure, and parameters in a proc body 
; may `shadow' global constants. These will therefore have to be (temporarily) 
; removed from the Constants list. 
; Functions for processing variable names: 
; Return the full name for the given item, return < > if the item 
; is not a simple variable, or a struct, or an array with integer indices. 
; Note: we can assume that known variables have already been replaced 
; in the indices 
; For a simple variable, the full name is <name>. 
; For a struct, the full name is <name, field, field, ...>. 
; For a known array element, the full name is <name, -n1, -n2, ...>. 
; Special case: a[foo].bar is <a, foo, bar> 
(set! //Qry_/Var_/Name (@Make_Name "Qry_var_Name"))
(define (@CP_Var_Name //I)
 (let ((//R-save //R)
       (//S/T (@ST //I))
       (funct-result '()))
  (set! //R '())
  (cond
   ((or (= //S/T //T_/Variable) (= //S/T //T_/Var_/Lvalue))
    (set! //R (list (@V //I))))
   (#t
    (let ((//Previous (@Dtable_Get //I //Qry_/Var_/Name)))
     (cond
      ((null? //Previous)
       (cond
        ((or (= //S/T //T_/Struct) (= //S/T //T_/Struct_/Lvalue))
         (cond
          ((@Is_Mem? (@Get_n //I 2))
           (set! //R (concat (cons /a_name (@CP_Var_Name (@Get_Mem (@Get_n //I 2)))) (list (@V (@Get_n //I 1))))))
          (#t
           (set! //R (concat (@CP_Var_Name (@Get_n //I 2)) (list (@V (@Get_n //I 1))))))))
        ((or (= //S/T //T_/Aref) (= //S/T //T_/Aref_/Lvalue))
         (cond
          ((or (= (@ST (@Get_n //I 1)) //T_/Variable) (= (@ST (@Get_n //I 1)) //T_/Var_/Lvalue))
           (cond
            ((@CP_All_Numbers? (@Cs (@Get_n //I 2)))
             (set! //R (cons (@V (@Get_n //I 1)) (my-map @CP_Minus_V (@Cs (@Get_n //I 2))))))))))))
      (#t
       (set! //R (@Dtable_Value_Part //Previous)))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@CP_Minus_V //I)
 
 (- (@V //I)))

(define (@CP_All_Numbers? /l)
 
 (or (null? /l) (and (= (@ST (car /l)) //T_/Number) (@CP_All_Numbers? (cdr /l)))))

; Return TRUE if the given item is a simple variable, 
; or an array with known indices, or a struct: 
(define (@CP_Variable? //I)
 
 (not (null? (@CP_Var_Name //I))))

; Functions for processing value lists: 
(define (@CP_Get //L //N)
 
 (if (null? //L) '() (if (equal? (car (car //L)) //N) (car (cdr (car //L))) (@CP_Get (cdr //L) //N))))

(define (@CP_Put //L //N //V)
 (let ((/old (@CP_Get //L //N)))
  (if (or (equal? (car //N) /exit_flag) (and (not (null? /old)) (@Equal? //V /old))) //L (cons (list //N //V) (@CP_Remove //L //N)))))

; Add an entry to the 0th level list in the given list 
; An entry for exit_flag is never stored, 
; while an entry of exit_flag := 1 clobbers L (as ABORT does). 
; If CP_In_Preserves_Dest > 1 then any exit value > 0 clobbers L: 
(define (@CP_Put0 //L //N //V)
 
 (if (or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0)) (if (not (equal? (car //N) /exit_flag)) (cons (list 0 (list (list //N //V))) //L) //L) (if (equal? (car //N) /exit_flag) (if (or (= (@V //V) 1) (and (> //C/P_/In_/Preserves_/Dest 1) (> (@V //V) 1))) (cdr //L) //L) (cons (list 0 (@CP_Put (wsl-ref (wsl-ref //L 1) 2) //N //V)) (cdr //L)))))

; Remove a variable from the list (don't remove the whole of a) 
(define (@CP_Remove_Sub //L-par //N-par)
 (let ((//N-save //N)
       (//L-save //L)
       (//R-save //R)
       (funct-result '()))
  (set! //N //N-par)
  (set! //L //L-par)
  (set! //R '())
  (cond
   ((null? //L)
    #t)
   ((@Either_Prefix? //N (wsl-ref (wsl-ref //L 1) 1))
    (set! //R (@CP_Remove_Sub (cdr //L) //N)))
   ((and (= (@ST (wsl-ref (wsl-ref //L 1) 2)) //T_/Variable) (equal? (@V (wsl-ref (wsl-ref //L 1) 2)) (car //N)))
    (set! //R (@CP_Remove_Sub (cdr //L) //N)))
   ((and (= (@ST (wsl-ref (wsl-ref //L 1) 2)) //T_/Minus) (member (car //N) (@Variables (wsl-ref (wsl-ref //L 1) 2))))
    (set! //R (@CP_Remove_Sub (cdr //L) //N)))
   ((and (= (@ST (wsl-ref (wsl-ref //L 1) 2)) //T_/Struct) (not (equal? //N (list /a_name))))
    (cond
     ((@Either_Prefix? //N (@Struct_Elts (wsl-ref (wsl-ref //L 1) 2)))
      (set! //R (@CP_Remove_Sub (cdr //L) //N)))
     (#t
      (set! //R (cons (car //L) (@CP_Remove_Sub (cdr //L) //N))))))
   (#t
    (set! //R (cons (car //L) (@CP_Remove_Sub (cdr //L) //N)))))
  (set! funct-result //R)
  (set! //N //N-save)
  (set! //L //L-save)
  (set! //R //R-save)
  funct-result))

; If a DSECT pointer is clobbered, also clobber all the data 
; (but clobbering a data element <a, dsect, elt> will NOT clobber dsect): 
(define (@CP_Remove //L //N)
 
 (if (and (= (gen-length //N) 1) (not (null? (gethash //D/S/E/C/Ts (car //N))))) (@CP_Remove_Sub (@CP_Remove_Sub //L //N) (cons /a_name //N)) (@CP_Remove_Sub //L //N)))

; Function to merge two value lists: this is used whenever 
; two or more lines of control meet, for example at the end of a Cond. 
; Only consistent values in both tables are retained 
(define (@CP_Merge //L1 //L2)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R '())
  (cond
   ((or (null? //L1) (null? //L2))
    (set! //R '()))
   ((@Equal? (wsl-ref (wsl-ref //L1 1) 2) (@CP_Get //L2 (wsl-ref (wsl-ref //L1 1) 1)))
    (set! //R (cons (car //L1) (@CP_Merge (cdr //L1) //L2))))
   (#t
    (set! //R (@CP_Merge (cdr //L1) //L2))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

; Similar to @CP_Merge but all values are retained in the form <var, val1, val2, ...> 
; instead of deleting variables with inconsistent values. 
; Variables in only one list ARE deleted though. 
; Note that L1 is a multi-value list, while L2 is an ordinary list. 
(define (@CP_Merge_Keep //L1 //L2)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R '())
  (cond
   ((or (null? //L1) (null? //L2))
    (set! //R '()))
   (#t
    (let ((/v1values (cdr (car //L1)))
          (/v2 (@CP_Get //L2 (wsl-ref (wsl-ref //L1 1) 1))))
     (cond
      ((null? /v2)
       (set! //R (@CP_Merge_Keep (cdr //L1) //L2)))
      ((@CP_In? /v2 /v1values)
       (set! //R (cons (car //L1) (@CP_Merge_Keep (cdr //L1) //L2))))
      (#t
       (set! //R (cons (cons (wsl-ref (wsl-ref //L1 1) 1) (cons /v2 /v1values)) (@CP_Merge_Keep (cdr //L1) //L2))))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

(define (@CP_In? /v //L)
 
 (and (not (null? //L)) (or (@Equal? /v (car //L)) (@CP_In? /v (cdr //L)))))

; Function to merge two lists of the form <<n0, Ln0>, <n1, Ln1>, <n2, Ln2>, ...> 
; The lists at each terminal value are merged using @CP_Merge 
(define (@CP_MergeL //L1 //L2)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R '())
  (cond
   ((null? //L1)
    (set! //R //L2))
   ((null? //L2)
    (set! //R //L1))
   ((< (wsl-ref (wsl-ref //L1 1) 1) (wsl-ref (wsl-ref //L2 1) 1))
    (set! //R (cons (car //L1) (@CP_MergeL (cdr //L1) //L2))))
   ((> (wsl-ref (wsl-ref //L1 1) 1) (wsl-ref (wsl-ref //L2 1) 1))
    (set! //R (cons (car //L2) (@CP_MergeL //L1 (cdr //L2)))))
   (#t
    (set! //R (cons (list (wsl-ref (wsl-ref //L1 1) 1) (@CP_Merge (wsl-ref (wsl-ref //L1 1) 2) (wsl-ref (wsl-ref //L2 1) 2))) (@CP_MergeL (cdr //L1) (cdr //L2))))))
  (set! funct-result //R)
  (set! //R //R-save)
  funct-result))

; A constant is a number, string, name or negated number: 
; NB: We cannot treat ADDRESS_OF(...) as a constant, consider 
; ADDRESS_OF(a[FOO].BAR) when pointer FOO gets modified! 
; Allow (reg + ptr) - ADDRESS_OF(a[ptr].foo.bar) as a constant 
(define (@CP_Constant_? //I)
 (let ((//R-save //R)
       (funct-result '()))
  (set! //R 0)
  (cond
   ((@CP_Constant_? //I)
    (@Print_WSL //I "")
    (display-list "-- is constant")
    (set! //R 1))
   (#t
    (@Print_WSL //I "")
    (display-list "-- NOT constant")
    (set! //R 0)))
  (set! funct-result (= //R 1))
  (set! //R //R-save)
  funct-result))

(define (@CP_Constant? //I)
 
 (or (= (@ST //I) //T_/Number) (= (@ST //I) //T_/String) (and (= (@ST //I) //T_/Struct) (@CP_Constant? (@Get_n //I 2)) (member (@Struct_Elts //I) //Constants)) (and (= (@ST //I) //T_/Variable) (member (@Struct_Elts //I) //Constants)) (and (@Is_Mem? //I) (@CP_Constant? (@Get_Mem //I))) (and (= (@ST //I) //T_/Negate) (= (@ST (@Get_n //I 1)) //T_/Number)) (and (= (@ST //I) //T_/Minus) (= (@ST (@Get_n //I 1)) //T_/Variable) (member (list (@V (@Get_n //I 1))) //Constants) (= (@ST (@Get_n //I 2)) //T_/Number)) (and (= (@ST //I) //T_/Minus) (= (@ST (@Get_n //I 1)) //T_/Plus) (= (@ST (@Get_n (@Get_n //I 1) 1)) //T_/Variable) (= (@ST (@Get_n (@Get_n //I 1) 2)) //T_/Variable) (@Is_Addr? (@Get_n //I 2)) (= (@ST (@Get_Addr (@Get_n //I 2))) //T_/Struct) (equal? (car (@WC_Struct_Elts (@Get_Addr (@Get_n //I 2)))) (@V (@Get_n (@Get_n //I 1) 2)))) (and (@Is_Addr? //I) (or (= (@ST (@Get_Addr //I)) //T_/Struct) (= (@ST (@Get_Addr //I)) //T_/Variable)) (not-member /a_name (@Used (@Get_Addr //I)))) (and (= (@ST //I) //T_/X_/Funct_/Call) (member (@V (@Get_n //I 1)) /date_time_functs)) (and (= (@ST //I) //T_/Variable) (member (@V //I) /vcons)) (equal? (@Variables //I) (list /a_name)) (and (= (@ST //I) //T_/Array) (@CP_Constant? (@Get_n //I 1)) (@CP_Constant? (@Get_n //I 2))) (and (= (@ST //I) //T_/Sequence) (@CP_Constant? (@Get_n //I 1))) (and (= (@GT //I) //T_/Expressions) (not (@Cs? //I))) (and (= (@GT //I) //T_/Expressions) (@Cs? //I) (@CP_Constant? (@Get_n //I 1)) (@CP_Constant? (@Make //T_/Expressions '() (cdr (@Cs //I)))))))

(define (@CP_Generic //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((//G/T (@GT (@I))))
   (cond
    ((= //G/T //T_/Statement)
     (set! //L (@CP_Statement  //L)))
    ((= //G/T //T_/Expression)
     (set! //L (@CP_Update  //L)))
    ((= //G/T //T_/Condition)
     (set! //L (@CP_Update  //L)))
    ((= //G/T //T_/Definition)
     (set! //L (@CP_Definition  //L)))
    ((= //G/T //T_/Lvalue)
     (set! //L (@CP_Update  //L)))
    ((= //G/T //T_/Guarded)
     (set! //L (@CP_Guarded  //L)))
    ((= //G/T //T_/Action)
     (set! //L (@CP_Action  //L)))
    ((= //G/T //T_/Statements)
     (set! //L (@CP_Statements  //L)))
    ((= //G/T //T_/Expressions)
     (set! //L (@CP_Update  //L)))
    ((= //G/T //T_/Lvalues)
     (set! //L (@CP_Update  //L)))
    ((= //G/T //T_/Assigns)
     (set! //L (@CP_Assigns  //L)))
    ((= //G/T //T_/Definitions)
     (set! //L (@CP_Definitions  //L)))
    ((= //G/T //T_/Actions)
     (set! //L (@CP_Actions  //L)))
    (#t
     (display-list "UNKNOWN GENERIC TYPE: " (@Type_Name //G/T) "(" //G/T ") at " (@Posn)))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Conditions, expressions, Lvalues etc. are simply updated from the list 
; (which remains unchanged). Do nothing if the list is empty. 
; (An Lvalue might be an array reference a[x] where x has a known value) 
; Special case for small Sub_Seg and Rel_Seg where all the elements 
; in the segment have known values 
(define (@CP_Update //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/name-save /name)
        (/val-save /val)
        (/l-save /l)
        (/change-save /change)
        (/i-save /i)
        (/n-save /n)
        (/p1-save /p1))
   (set! /name '())
   (set! /val '())
   (set! /l '())
   (set! /change 0)
   (set! /i 0)
   (set! /n 0)
   (set! /p1 '())
   (cond
    ((= //Migration 0)
     ; Don't update anything if we are only calculating data flows 
    )
    ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0) (not (null? (wsl-ref (wsl-ref //L 1) 2))))
     (set! /l (wsl-ref (wsl-ref //L 1) 2))
     (@Ateach_Expn /foreach-constant_propagation-7 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (cond
      ((> /change 0)
       (@Trans //T/R_/Simplify "")))))
   (set! /name /name-save)
   (set! /val /val-save)
   (set! /l /l-save)
   (set! /change /change-save)
   (set! /i /i-save)
   (set! /n /n-save)
   (set! /p1 /p1-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Called when all the variables are clobbered 
(define (@CP_Clobber //L)
 (cond
  ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
   (wsl-set! //L '() 1 2)))
 //L)

; Called when we run out of budget. 
(define (@CP_Clobber_Broke //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (display-list-flush "B")
  (cond
   ((<= /initial_call_budget 200)
    (set! //L (@CP_Clobber  //L)))
   ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
    (wsl-set! //L (@CP_Clobber_Zeros (wsl-ref (wsl-ref //L 1) 2)) 1 2)
    ; If we are skipping EXIT statements, then all the merges: 
    (for-in /n (@Gen_TVs (@I) (@AS_Type)) 
     (cond
      ((> /n 0)
       (display-list-flush "<" /n ">")
       (set! //L (@CP_Exit_Sub (wsl-ref (wsl-ref //L 1) 2) /n //L)))))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Some form of external proc: all variables are clobbered EXCEPT 
; the vars passed as parameters. (If you passed the var as a parameter, 
; then the external proc probably doesn't have direct access to it, 
; and therefore probably doesn't modify it. So we use the convention 
; that variables passed as parameters to an external proc are guaranteed 
; to be preserved by the proc) 
; Delete all the pairs in l whose names are not in except. 
(define (@CP_All_But /l /except)
 
 (if (null? /l) '() (if (member (wsl-ref (wsl-ref (wsl-ref /l 1) 1) 1) /except) (cons (wsl-ref /l 1) (@CP_All_But (cdr /l) /except)) (@CP_All_But (cdr /l) /except))))

(define (@CP_Clobber_List /l-par /clobbered)
 (let ((/l-save /l)
       (/elt-save /elt)
       (funct-result '()))
  (set! /l /l-par)
  (set! /elt '())
  (for-in /elt /clobbered 
   (cond
    ((or (> (gen-length /elt) 1) (not (equal? (car /elt) /a_name)))
     (set! /l (@CP_Remove /l /elt)))))
  (set! funct-result /l)
  (set! /l /l-save)
  (set! /elt /elt-save)
  funct-result))

; Over a !P FOO(... VAR ... os) call, assume that anything which was initialised 
; to zero or spaces might be clobbered by the call 
; NB: This can also clobber return addresses which are saved as four bytes, eg: 
; FOO[1..4] := code 
; Solution: if foo.bar.1 = 0 and foo.bar.4 <> 0 or foo.bar.3 <> 0 
; then keep this zero 
(define (@CP_Clobber_Zeros /l-par)
 (let ((/l-save /l)
       (//R-save //R)
       (funct-result '()))
  (set! /l /l-par)
  (set! //R '())
  (for-in /pair /l 
   (cond
    ((or (equal? (wsl-ref /pair 1) (list /reg_stack)) (equal? (wsl-ref /pair 1) (list /call_stack)))
     ; Always keep reg_stack's value 
     (set! //R (cons /pair //R)))
    ((and (= (gen-length (wsl-ref /pair 1)) 1) (@Starts_With? (@N_String (wsl-ref (wsl-ref /pair 1) 1)) "F_"))
     ; Keep this flag value 
     (set! //R (cons /pair //R)))
    ((and (= (gen-length (wsl-ref /pair 1)) 1) (@Starts_With? (@N_String (wsl-ref (wsl-ref /pair 1) 1)) "HANDLE_"))
     ; Keep this handler 
     (set! //R (cons /pair //R)))
    ((and (= (gen-length (wsl-ref /pair 1)) 1) (@Starts_With? (@N_String (wsl-ref (wsl-ref /pair 1) 1)) "DPMS") (@Digits? (substr (@N_String (wsl-ref (wsl-ref /pair 1) 1)) 4)))
     ; Keep this handler 
     (set! //R (cons /pair //R)))
    ((and (= (@ST (wsl-ref /pair 2)) //T_/Variable) (@Starts_With? (@N_String (@V (wsl-ref /pair 2))) "V_"))
     ; Keep this vcon value 
     (set! //R (cons /pair //R)))
    ((and (= (@ST (wsl-ref /pair 2)) //T_/Variable) (@Starts_With? (@N_String (@V (wsl-ref /pair 2))) "v_"))
     ; Keep this vcon value 
     (set! //R (cons /pair //R)))
    ((and (= (@ST (wsl-ref /pair 2)) //T_/Variable) (equal? (@V (wsl-ref /pair 2)) (@Make_Name "___r1_init___")))
     ; Keep this r1 initial value 
     (set! //R (cons /pair //R)))
    ((and (= (@ST (wsl-ref /pair 2)) //T_/Variable) (equal? (@V (wsl-ref /pair 2)) (@Make_Name "__r14_init__")))
     ; Keep this r14 initial value 
     (set! //R (cons /pair //R)))
    ((and (= (@ST (wsl-ref /pair 2)) //T_/Variable) (member (@V (wsl-ref /pair 2)) /reg_inits))
     ; Keep this register initial value 
     (set! //R (cons /pair //R)))
    ((and (or (not (= (@ST (wsl-ref /pair 2)) //T_/Number)) (< (@V (wsl-ref /pair 2)) //C/P_/Special_/Value)) (not-member (wsl-ref /pair 1) /return_elts))
     ; Clobber this non-dispatch-code-containing variable 
    )
    ((= (@ST (wsl-ref /pair 2)) //T_/String)
     ; Clobber this string 
    )
    ((and (= (@ST (wsl-ref /pair 2)) //T_/Variable) (not-member (@V (wsl-ref /pair 2)) /reg_inits) (not-member (@V (wsl-ref /pair 2)) /registers) (not (@Starts_With? (@N_String (@V (wsl-ref /pair 2))) "V_")) (not (@Starts_With? (@N_String (@V (wsl-ref /pair 2))) "v_")))
     ; Clobber this value (don't clobber a value that is a VCON) 
    )
    ((and (= (@ST (wsl-ref /pair 2)) //T_/Number) (= (@V (wsl-ref /pair 2)) 0))
     ; Have found a zero 
     (cond
      ((and (> (gen-length (wsl-ref /pair 1)) 1) (< (last-1 (wsl-ref /pair 1)) 0))
       (let ((/e1-save /e1)
             (/e2-save /e2)
             (/e3 (@CP_Get /l (concat (butlast-1 (wsl-ref /pair 1)) (list (- (last-1 (wsl-ref /pair 1)) 1))))))
        (set! /e1 (@CP_Get /l (concat (butlast-1 (wsl-ref /pair 1)) (list (- (last-1 (wsl-ref /pair 1)) 3)))))
        (set! /e2 (@CP_Get /l (concat (butlast-1 (wsl-ref /pair 1)) (list (- (last-1 (wsl-ref /pair 1)) 2)))))
        (cond
         ((and (not (null? /e1)) (= (@ST /e1) //T_/Number) (not (= (@V /e1) 0)))
          (set! //R (cons /pair //R)))
         ((and (not (null? /e2)) (= (@ST /e2) //T_/Number) (not (= (@V /e2) 0)))
          (set! //R (cons /pair //R)))
         ((and (not (null? /e3)) (= (@ST /e3) //T_/Number) (not (= (@V /e3) 0)))
          (set! //R (cons /pair //R)))
         (#t
          ; Clobber this zero 
         ))
        (set! /e1 /e1-save)
        (set! /e2 /e2-save)))
      (#t
       ; Clobber this zero 
      )))
    (#t
     (set! //R (cons /pair //R)))))
  (set! funct-result (reverse //R))
  (set! /l /l-save)
  (set! //R //R-save)
  funct-result))

(define (@CP_X_Proc //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
    ; delete everything except the parameters (i.e. the directly used vars) 
    (wsl-set! //L (@CP_All_But (wsl-ref (wsl-ref //L 1) 2) (@Used (@I))) 1 2)))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; For a !P, update the value parameters and clobber the var parameters. 
; If A_Proc_Call_Filter is non-empty then clobber everything not in the filter list. 
; Check for the special cases push_regs and pop_regs. 
; push_regs will stack known register values on the value of reg_stack 
; pop_regs will check the top of value of reg_stack and restore stacked reg values. 
; Hack: don't propogate inside a !P pack 
(define (@CP_A_Proc //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
    (cond
     ((and (equal? (@V (@Get_n (@I) 1)) /push_regs) (member /reg_stack (@Variables (@Get_n (@I) 3))))
      (set! //L (@CP_Push_Regs  (@Cs (@Get_n (@I) 2)) (@CP_Get (wsl-ref (wsl-ref //L 1) 2) (list /reg_stack)) //L)))
     ((and (equal? (@V (@Get_n (@I) 1)) /pop_regs) (member /reg_stack (@Variables (@Get_n (@I) 3))))
      (set! //L (@CP_Pop_Regs  (@Cs (@Get_n (@I) 3)) (@CP_Get (wsl-ref (wsl-ref //L 1) 2) (list /reg_stack)) //L)))
     (#t
      ; Don't propagate inside VPP calls or pack or EXEC_CICS: 
      (cond
       ((or (and (not-member (@Make_Name "vpp") (@Assigned (@I))) (not (equal? (@V (@Get_n (@I) 1)) /pack)) (not (equal? (@V (@Get_n (@I) 1)) //E/X/E/C_/C/I/C/S)) (not-member (@Make_Name "regs") (@Assigned (@I)))) (equal? (@V (@Get_n (@I) 1)) /call_via_ptr) (equal? (@V (@Get_n (@I) 1)) /call_via_ptr_pars))
        (@Down_To 2)
        ; to the value parameters 
        (set! //L (@CP_Update  //L))
        (@Up)))
      (cond
       ((and (or (equal? (@V (@Get_n (@I) 1)) /call_via_ptr) (equal? (@V (@Get_n (@I) 1)) /call_via_ptr_pars)) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Variable) (equal? (@V (@Get_n (@Get_n (@I) 2) 1)) (@Make_Name "result_code")))
        ; Scan back for an assignment of a vcon to r15 
        (let ((/p (@Posn_n))
              (/v-save /v))
         (set! /v '())
         (set! /fl_flag1 0)
         (while (= /fl_flag1 0) 
          (begin
           (let ((/__/O/K 1))
            (set! /__/O/K (@New_Match  /%const__constant_propagation__1 (@I) /__/O/K))
            (cond
             ((= /__/O/K 1)
              (let ((/__e_save /e))
               (set! /e (vector-ref /__/Match_array 0))
               (cond
                ((and (= (@ST /e) //T_/Variable) (or (@Starts_With? (@V /e) "V_") (@Starts_With? (@V /e) "v_")))
                 (set! /v /e)))
               (set! /e /__e_save)))))
           (cond
            ((not (@Left?))
             (set! /fl_flag1 1))
            (#t
             (@Left)
             (cond
              ((and (not (= (@ST (@I)) //T_/Comment)) (not (= (@ST (@I)) //T_/Assignment)) (not (= (@ST (@I)) //T_/A_/Proc_/Call)))
               (set! /fl_flag1 1))
              (#t
               (set! /fl_flag1 0)))))))
         (@To /p)
         (cond
          ((not (null? /v))
           (@Down_To 2)
           (@Down)
           (@Paste_Over /v)
           (@Up)
           (@Up)))
         (set! /v /v-save))))
      (cond
       ((and (or (equal? (@V (@Get_n (@I) 1)) /call_via_ptr) (equal? (@V (@Get_n (@I) 1)) /call_via_ptr_pars)) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Variable) (or (@Starts_With? (@V (@Get_n (@Get_n (@I) 2) 1)) "v_") (@Starts_With? (@V (@Get_n (@Get_n (@I) 2) 1)) "V_")))
        (let ((/name-save /name))
         (set! /name (@V (@Get_n (@Get_n (@I) 2) 1)))
         ; Call to a register converted to a proc call: 
         ; we now know what the register value is: 
         (@Down)
         (@Paste_Over (@Name (@Make_Name (substr (@N_String /name) 2))))
         ; Delete the parameter with the function pointer: 
         (@Right)
         (@Down)
         (@Delete)
         (@Up)
         (@Up)
         (set! /name /name-save)))
       ((and (or (equal? (@V (@Get_n (@I) 1)) /call_via_ptr) (equal? (@V (@Get_n (@I) 1)) /call_via_ptr_pars)) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Negate) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 2) 1) 1)) //T_/Number) (> /dispatch 0) (not (null? (wsl-ref //Bodies /dispatch))))
        ; Find the proc from dispatch 
        (let ((/n-save /n)
              (/posn (@Posn))
              (/call-save /call))
         (set! /n (@Get_n (@Get_n (@I) 2) 1))
         (set! /call '())
         (@Edit)
         (@New_Program (wsl-ref //Bodies /dispatch))
         (@Foreach_Statement /foreach-constant_propagation-8 0 (@AS_Type) 0)
         (cond
          ((null? (@Program))
           (@New_Program (@Skips))))
         (@Undo_Edit)
         (cond
          ((not (null? /call))
           (@Paste_Over /call)))
         (set! /n /n-save)
         (set! /call /call-save))))
      (cond
       ((and (not (null? //A_/Proc_/Call_/Filter)) (member /os_name (@Assigned (@Get_n (@I) 3))))
        (wsl-set! //L (@CP_All_But (wsl-ref (wsl-ref //L 1) 2) //A_/Proc_/Call_/Filter) 1 2))
       ((member /os_name (@Assigned (@Get_n (@I) 3)))
        (wsl-set! //L (@CP_Clobber_Zeros (wsl-ref (wsl-ref //L 1) 2)) 1 2)))
      ; delete all var parameters from L[1][2] 
      (cond
       ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
        (wsl-set! //L (@CP_Clobber_List (wsl-ref (wsl-ref //L 1) 2) (@Elts_Assigned (@I))) 1 2)))
      (cond
       ((and (> /dispatch 0) (not (null? (wsl-ref //Bodies /dispatch))) (equal? (@V (@Get_n (@I) 1)) /call_via_ptr) (@CP_Reg_Init_Var? (@Get_n (@Get_n (@I) 2) 1)))
        (@Paste_Over (@Make //T_/Call (- (+ //N 1)) '()))))))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Check if the given item is a reg init or currently holds a reg init value: 
(define (@CP_Reg_Init_Var? //I)
 (let ((//R-save //R)
       (/val-save /val)
       (funct-result '()))
  (set! //R 0)
  (set! /val '())
  (cond
   ((and (= (@ST //I) //T_/Variable) (member (@V //I) /reg_inits))
    (set! //R 1))
   (#t
    (set! /val (@CP_Get (wsl-ref (wsl-ref //L 1) 2) (@CP_Var_Name //I)))
    (cond
     ((and (not (null? /val)) (= (@ST /val) //T_/Variable) (member (@V /val) /reg_inits))
      (set! //R 1)))))
  (set! funct-result (= //R 1))
  (set! //R //R-save)
  (set! /val /val-save)
  funct-result))

; Push known register values on top of reg_stack value in L 
(define (@CP_Push_Regs /regs /stack_val //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/vals '())
        (/val-save /val)
        (/name-save /name))
   (set! /val '())
   (set! /name '())
   (for-in /reg /regs 
    (begin
     (set! /val '())
     (set! /name (@CP_Var_Name /reg))
     (cond
      ((and (not (null? /name)) (not (null? (@CP_Get (wsl-ref (wsl-ref //L 1) 2) /name))))
       (set! /val (@CP_Get (wsl-ref (wsl-ref //L 1) 2) /name)))
      ((@CP_Constant? /reg)
       (set! /val /reg)))
     (cond
      ((null? /val)
       (set! /vals (cons (@CP_Make_Seq '()) /vals)))
      (#t
       (set! /vals (cons /val /vals))))))
   (set! /vals (@CP_Make_Seq (reverse /vals)))
   (cond
    ((or (null? /stack_val) (not (= (@ST /stack_val) //T_/Sequence)))
     (set! /stack_val (@CP_Make_Seq (list /vals))))
    (#t
     (set! /stack_val (@CP_Make_Seq (cons /vals (@Cs (@Get_n /stack_val 1)))))))
   (wsl-set! //L (@CP_Put (wsl-ref (wsl-ref //L 1) 2) (list /reg_stack) /stack_val) 1 2)
   (set! /val /val-save)
   (set! /name /name-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_Make_Seq /comps)
 
 (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() /comps))))

; Pop register values from stack_val into the registers: 
(define (@CP_Pop_Regs /regs /stack_val //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((and (not (null? /stack_val)) (= (@ST /stack_val) //T_/Sequence) (@Cs? (@Get_n /stack_val 1)))
    (let ((/vals (@Cs (@Get_n (@Get_n (@Get_n /stack_val 1) 1) 1)))
          (/val-save /val))
     (set! /val '())
     (set! /stack_val (@CP_Make_Seq (cdr (@Cs (@Get_n /stack_val 1)))))
     (set! //L (@CP_Put0 //L (list /reg_stack) /stack_val))
     (for-in /reg /regs 
      (cond
       ((member (@V /reg) /registers)
        (set! /val (wsl-ref /vals (+ (gethash //C/P_/Reg_/To_/Num (@V /reg)) 1)))
        (cond
         (#f
          (display-list-flush (@N_String (@V /reg)) " = ")
          (@PP_Item /val 80 "")))
        (cond
         ((and (= (@ST /val) //T_/Sequence) (not (@Cs? (@Get_n /val 1))))
          ; Unknown value was pushed and popped: 
          (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) (list (@V /reg))) 1 2))
         (#t
          ; Restore the known value of the register: 
          (wsl-set! //L (@CP_Put (wsl-ref (wsl-ref //L 1) 2) (list (@V /reg)) /val) 1 2))))))
     (set! /val /val-save)))
   ((and #f (null? /stack_val))
    ; Stack val is unknown: set r14 to zero in case we dispatch on it 
    (wsl-set! //L (@CP_Put (wsl-ref (wsl-ref //L 1) 2) (list (@Make_Name "r14")) (@Make //T_/Number 0 '())) 1 2)))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; PUSH(stack, expn); ...; POP(v, stack); 
; See also TR_Push_Pop 
(define (@CP_Push //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
    (let ((/v-save /v)
          (/val-save /val)
          (/stack (@CP_Var_Name (@Get_n (@I) 1)))
          (/stack_val '()))
     (set! /v (@CP_Var_Name (@Get_n (@I) 2)))
     (set! /val '())
     (set! //L (@CP_Update  //L))
     (cond
      ((@CP_Constant? (@Get_n (@I) 2))
       (set! /val (@Get_n (@I) 2)))
      ((not (null? /v))
       (set! /val (@CP_Get (wsl-ref (wsl-ref //L 1) 2) /v))))
     (cond
      ((null? /val)
       (set! /val (@CP_Make_Seq '()))))
     (cond
      ((not (null? /stack))
       (set! /stack_val (@CP_Get (wsl-ref (wsl-ref //L 1) 2) /stack))
       (cond
        ((or (null? /stack_val) (not (= (@ST /stack_val) //T_/Sequence)))
         (set! /stack_val (@CP_Make_Seq (list /val))))
        (#t
         (set! /stack_val (@CP_Make_Seq (cons /val (@Cs (@Get_n /stack_val 1)))))))
       (wsl-set! //L (@CP_Put (wsl-ref (wsl-ref //L 1) 2) /stack /stack_val) 1 2)))
     (set! /v /v-save)
     (set! /val /val-save))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_Pop //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
    (let ((/v-save /v)
          (/val-save /val)
          (/stack (@CP_Var_Name (@Get_n (@I) 2)))
          (/stack_val '()))
     (set! /v (@CP_Var_Name (@Get_n (@I) 1)))
     (set! /val '())
     (cond
      ((not (null? /stack))
       (set! /stack_val (@CP_Get (wsl-ref (wsl-ref //L 1) 2) /stack))
       (cond
        ((and (not (null? /stack_val)) (= (@ST /stack_val) //T_/Sequence) (@Cs? (@Get_n /stack_val 1)))
         (set! /val (@Get_n (@Get_n /stack_val 1) 1))
         (set! //L (@CP_Put0 //L /stack (@CP_Make_Seq (cdr (@Cs (@Get_n /stack_val 1))))))
         (cond
          ((and (= (@ST /val) //T_/Sequence) (not (@Cs? (@Get_n /val 1))))
           ; Unknown value was pushed and popped: 
           (cond
            ((not (null? /v))
             (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) /v) 1 2))))
          (#t
           ; Restore the known value: 
           (cond
            ((not (null? /v))
             (wsl-set! //L (@CP_Put (wsl-ref (wsl-ref //L 1) 2) /v /val) 1 2))))))
        ((not (null? /v))
         (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) /v) 1 2)))))
     (set! /v /v-save)
     (set! /val /val-save))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; A specification statement has components: <lvalues, condition> 
(define (@CP_Spec //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
    ; delete the assigned vars from L[1][2] 
    (wsl-set! //L (@CP_Clobber_List (wsl-ref (wsl-ref //L 1) 2) (@Elts_Assigned (@I))) 1 2)))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_Skip //L)
 
 //L)

(define (@CP_Statement //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((//S/T (@ST (@I))))
   (cond
    ((= //S/T //T_/A_/Proc_/Call)
     (set! //L (@CP_A_Proc  //L)))
    ((= //S/T //T_/M/W_/Proc_/Call)
     (set! //L (@CP_A_Proc  //L)))
    ((= //S/T //T_/X_/Proc_/Call)
     (set! //L (@CP_X_Proc  //L)))
    ((= //S/T //T_/Stat_/Place)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/Stat_/Pat_/One)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/Stat_/Pat_/Many)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/Stat_/Pat_/Any)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/Abort)
     (set! //L (@CP_Abort  //L)))
    ((= //S/T //T_/Assert)
     (set! //L (@CP_Assert  //L)))
    ((= //S/T //T_/Assignment)
     (set! //L (@CP_Assignment  //L)))
    ((= //S/T //T_/A_/S)
     (set! //L (@CP_A_S  //L)))
    ((= //S/T //T_/Call)
     (set! //L (@CP_Call  //L)))
    ((= //S/T //T_/Comment)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/Cond)
     (set! //L (@CP_Cond  //L)))
    ((= //S/T //T_/D_/If)
     (set! //L (@CP_Cond  //L)))
    ((= //S/T //T_/D_/Do)
     (set! //L (@CP_D_Do  //L)))
    ((= //S/T //T_/Exit)
     (set! //L (@CP_Exit  //L)))
    ((= //S/T //T_/For)
     (set! //L (@CP_For  //L)))
    ((= //S/T //T_/For_/In)
     (set! //L (@CP_For_In  //L)))
    ((= //S/T //T_/Foreach_/Stat)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Foreach_/Stats)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Foreach_/T/S)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Foreach_/T/Ss)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Foreach_/S/T/S)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Foreach_/Cond)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Foreach_/Expn)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Floop)
     (set! //L (@CP_Floop  //L)))
    ((= //S/T //T_/Join)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Pop)
     (set! //L (@CP_Pop  //L)))
    ((= //S/T //T_/Push)
     (set! //L (@CP_Push  //L)))
    ((= //S/T //T_/Proc_/Call)
     (set! //L (@CP_Proc_Call  //L)))
    ((= //S/T //T_/Spec)
     (set! //L (@CP_Spec  //L)))
    ((= //S/T //T_/Skip)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/Var)
     (set! //L (@CP_Var  //L)))
    ((= //S/T //T_/Where)
     (set! //L (@CP_Where  //L)))
    ((= //S/T //T_/While)
     (set! //L (@CP_While  //L)))
    ((= //S/T //T_/Ifmatch_/Stat)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/Ifmatch_/Expn)
     (set! //L (@CP_Clobber  //L)))
    ((= //S/T //T_/M/W_/Proc)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/M/W_/Funct)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/M/W_/B/Funct)
     (set! //L (@CP_Skip  //L)))
    ((= //S/T //T_/Print)
     (set! //L (@CP_Update  //L)))
    ((= //S/T //T_/Prinflush)
     (set! //L (@CP_Update  //L)))
    ((null? //S/T)
     (error "@CP_Statement:" "Bad statement" //S/T))
    (#t
     (display-list "UNKNOWN STATEMENT: " (@Type_Name //S/T) "(" //S/T ") at " (@Posn)))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_Action //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (@Down_To 2)
  ; to statements 
  (set! //L (@CP_Statements  //L))
  (@Up)
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_Actions //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (@Down)
  ; to first action 
  (set! //L (@CP_Action  //L))
  (while (@Right?) 
   (begin
    (@Right)
    (set! //L (@CP_Action  //L))))
  (@Up)
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_Definitions //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (@Down)
  ; to first defn 
  (set! //L (@CP_Definition  //L))
  (while (@Right?) 
   (begin
    (@Right)
    (set! //L (@CP_Definition  //L))))
  (@Up)
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_Definition //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((= (@ST (@I)) //T_/Proc)
    (@Down_To 3)
    ; to proc body 
    (let ((//L-save //L))
     (set! //L (list (list 0 '())))
     (set! //L (@CP_Statements  //L))
     (set! //L //L-save))
    (@Up)))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Absorption test: 
; Consider the code: IF B THEN x:=1 ELSE x:=0 FI; IF x=1 THEN ... FI 
; Here a variable (1) has a known value at the end of each arm of a COND, 
; (2) is tested in the next statement (a conditional) 
; In this case, it would be useful to expand over the next statement 
; before carrying out constant propagation. 
; PROBLEM: if the first IF statement has more arms than the second IF 
; (as often happens with Assembler translations) then the absorption 
; will result in duplicated code -- but while the assignments to x remain, 
; the arms of the IF are not identical and cannot be fully merged. 
; -- Partially_Join_Cases ought to work, but the pattern matcher is buggy. 
; Also, after doing the absorption, the assignments are likely to be redundant 
; and can be deleted, whence Join_All_Cases will become applicable. 
(define (@CP_Statements //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/loop_check 0))
   (cond
    ((not (= (@ST (@I)) //T_/Statements))
     (error "@CP_Statements:" "wrong type:" (@ST (@I))))
    ((and #f (< /call_budget 100))
     (display-list-flush "/")
     (set! //L (@CP_Clobber_Broke  //L))
     (@Foreach_Statement /foreach-constant_propagation-9 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips)))))
    ((and (> //State_/Saves /initial_call_budget) (> /initial_call_budget 200))
     (display-list-flush "#")
     (set! //L (@CP_Clobber_Broke  //L))
     (@Foreach_Statement /foreach-constant_propagation-10 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips)))))
    (#t
     (@Down)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (cond
        ((null? //L)
         (set! /fl_flag1 1))
        (#t
         (set! /fl_flag1 0)))
       (cond
        ((= /fl_flag1 0)
         (cond
          ((and (= (@ST (@I)) //T_/Cond) (@Right?))
           ; Check for common patterns of conditional cc assignments: 
           ; Moved to a separate proc to work round a bug in gsi! 
           (@CP_cc_Check)
           (cond
            ((= //Migration 1)
             (@CP_Maybe_Expand //L)))))
         (cond
          ((and (= (@ST (@I)) //T_/Cond) (@Left?) (= //Migration 1))
           ; Check for rX := code before the Cond 
           ; where each branch of the Cond is either a CALL 
           ; or proc call plus CALL: 
           (set! //L (@CP_Maybe_Absorb_Left  //L))))
         (set! //L (@CP_Statement  //L))
         (cond
          ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
           (cond
            ((not (<= /initial_call_budget 200))
             (set! /fl_flag1 1))
            (#t
             (set! //L (list (list 0 '())))
             (set! /fl_flag1 0))))
          (#t
           (set! /fl_flag1 0)))
         (cond
          ((= /fl_flag1 0)
           (cond
            ((@Is_Improper?)
             (cond
              ((@Right?)
               (@Delete_Rest)))
             (set! /fl_flag1 1))
            ((and (not (null? //Entries)) (@Regular? (@I)))
             (set! /fl_flag1 1))
            ((not (@Right?))
             (set! /fl_flag1 1))
            (#t
             (@Right)
             (set! /loop_check (+ /loop_check 1))
             (cond
              ((> /loop_check 30000)
               (error "!!!Loop checker failed!!!")
               (set! /fl_flag1 1))
              (#t
               (set! /fl_flag1 0)))))))))))
     (@Up))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_cc_Check)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__3 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/B3_save //B3)
          (/__/B2_save //B2)
          (/__cc_save /cc)
          (/__/B1_save //B1))
     (set! //B3 (vector-ref /__/Match_array 3))
     (set! //B2 (vector-ref /__/Match_array 2))
     (set! /cc (vector-ref /__/Match_array 1))
     (set! //B1 (vector-ref /__/Match_array 0))
     (@CP_cc_Set /cc //B1 //B2 //B3 /true)
     (set! //B3 /__/B3_save)
     (set! //B2 /__/B2_save)
     (set! /cc /__cc_save)
     (set! //B1 /__/B1_save)))))
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__4 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/B2_save //B2)
          (/__cc_save /cc)
          (/__/B1_save //B1))
     (set! //B2 (vector-ref /__/Match_array 2))
     (set! /cc (vector-ref /__/Match_array 1))
     (set! //B1 (vector-ref /__/Match_array 0))
     (@CP_cc_Set /cc //B1 //B2 /true /false)
     (set! //B2 /__/B2_save)
     (set! /cc /__cc_save)
     (set! //B1 /__/B1_save)))))
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__5 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__cc_save /cc)
          (/__/B1_save //B1))
     (set! /cc (vector-ref /__/Match_array 1))
     (set! //B1 (vector-ref /__/Match_array 0))
     (@CP_cc_Set /cc //B1 /true /false /false)
     (set! /cc /__cc_save)
     (set! //B1 /__/B1_save)))))
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__6 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__cc_save /cc)
          (/__/B1_save //B1))
     (set! /cc (vector-ref /__/Match_array 1))
     (set! //B1 (vector-ref /__/Match_array 0))
     (@CP_cc_Set /cc (@Not //B1) /true /false /false)
     (set! /cc /__cc_save)
     (set! //B1 /__/B1_save)))))
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__7 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__cc_save /cc)
          (/__/B1_save //B1))
     (set! /cc (vector-ref /__/Match_array 1))
     (set! //B1 (vector-ref /__/Match_array 0))
     (@CP_cc_Set /cc //B1 /false /true /false)
     (set! /cc /__cc_save)
     (set! //B1 /__/B1_save)))))
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__constant_propagation__8 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/B2_save //B2)
          (/__cc_save /cc)
          (/__/B1_save //B1))
     (set! //B2 (vector-ref /__/Match_array 2))
     (set! /cc (vector-ref /__/Match_array 1))
     (set! //B1 (vector-ref /__/Match_array 0))
     (@CP_cc_Set /cc //B1 (@Not //B2) /false /true)
     (set! //B2 /__/B2_save)
     (set! /cc /__cc_save)
     (set! //B1 /__/B1_save))))))

; We know the conditions under which cc is set to each value, 
; so scan forwards for tests of cc: 
(define (@CP_cc_Set /cc-par //B0-par //B1-par //B2-par //B3-par)
 (let ((//B3-save //B3)
       (//B2-save //B2)
       (//B1-save //B1)
       (//B0-save //B0)
       (/cc-save /cc))
  (set! //B3 //B3-par)
  (set! //B2 //B2-par)
  (set! //B1 //B1-par)
  (set! //B0 //B0-par)
  (set! /cc /cc-par)
  (cond
   ((= (@ST /cc) //T_/Var_/Lvalue)
    (let ((/call_types-save /call_types)
          (/vars-save /vars))
     (set! /call_types (@Make_Set (list //T_/Call //T_/Proc_/Call //T_/X_/Proc_/Call //T_/M/W_/Proc_/Call)))
     (set! /vars (union-n (union-n (union-n (union-n (list (list (@V /cc))) (@Elts_Used //B0)) (@Elts_Used //B1)) (@Elts_Used //B2)) (@Elts_Used //B3)))
     (@Right)
     (@CP_cc_Statements /cc //B0 //B1 //B2 //B3 /vars)
     (@Left)
     (set! /call_types /call_types-save)
     (set! /vars /vars-save))))
  (set! //B3 //B3-save)
  (set! //B2 //B2-save)
  (set! //B1 //B1-save)
  (set! //B0 //B0-save)
  (set! /cc /cc-save)))

; If A_Proc_Call_Filter is not empty, then these are the only vars 
; preserved by a T_A_Proc_Call 
(define (@CP_cc_Statements /cc-par //B0-par //B1-par //B2-par //B3-par /vars-par)
 (let ((/vars-save /vars)
       (//B3-save //B3)
       (//B2-save //B2)
       (//B1-save //B1)
       (//B0-save //B0)
       (/cc-save /cc))
  (set! /vars /vars-par)
  (set! //B3 //B3-par)
  (set! //B2 //B2-par)
  (set! //B1 //B1-par)
  (set! //B0 //B0-par)
  (set! /cc /cc-par)
  (let ((/posn_n (@Posn_n)))
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (or (= (@ST (@I)) //T_/Cond) (= (@ST (@I)) //T_/D_/If)) (member (@V /cc) (@Used (@I))))
       (@CP_cc_Cond /cc //B0 //B1 //B2 //B3 /vars)))
     (cond
      ((= (@ST (@I)) //T_/Comment)
       ; skip comments 
       (set! /fl_flag1 0))
      ((not (null? (intersection-n (@Stat_Types (@I)) /call_types)))
       ; Can't (easily) track any further 
       (set! /fl_flag1 1))
      ((and (member //T_/A_/Proc_/Call (@Stat_Types (@I))) (not (null? //A_/Proc_/Call_/Filter)) (not (@Set_Subset? (@Make_Set (my-map HEAD /vars)) //A_/Proc_/Call_/Filter)))
       (set! /fl_flag1 1))
      ((@Elt_Clash_List? /vars (@Elts_Assigned (@I)))
       (set! /fl_flag1 1))
      ((member (@V /cc) (@Assigned (@I)))
       (set! /fl_flag1 1))
      ((not (@Gen_Proper? (@I) //A/S_/Type))
       (set! /fl_flag1 1))
      (#t
       (set! /fl_flag1 0)))
     (cond
      ((= /fl_flag1 0)
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))))
   (@To /posn_n))
  (set! /vars /vars-save)
  (set! //B3 //B3-save)
  (set! //B2 //B2-save)
  (set! //B1 //B1-save)
  (set! //B0 //B0-save)
  (set! /cc /cc-save)))

(define (@CP_cc_Cond /cc-par //B0-par //B1-par //B2-par //B3-par /vars-par)
 (let ((/vars-save /vars)
       (//B3-save //B3)
       (//B2-save //B2)
       (//B1-save //B1)
       (//B0-save //B0)
       (/cc-save /cc))
  (set! /vars /vars-par)
  (set! //B3 //B3-par)
  (set! //B2 //B2-par)
  (set! //B1 //B1-par)
  (set! //B0 //B0-par)
  (set! /cc /cc-par)
  (@Down)
  ; to first guarded 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (@Down)
    ; to condition in guarded 
    (cond
     ((member (@V /cc) (@Used (@I)))
      (@Foreach_Cond /foreach-constant_propagation-11 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))))
    (@Right)
    ; to statements 
    (cond
     ((member (@V /cc) (@Used (@I)))
      (@Down)
      (@CP_cc_Statements /cc //B0 //B1 //B2 //B3 /vars)
      (@Up)))
    (@Up)
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@Up)
  (set! /vars /vars-save)
  (set! //B3 //B3-save)
  (set! //B2 //B2-save)
  (set! //B1 //B1-save)
  (set! //B0 //B0-save)
  (set! /cc /cc-save)))

; Check if it would be worthwhile to expand over the next statement 
; if so, then do so! (move this COND past any intervening comments). 
; Current statement is a COND and there is a statement after it 
(define (@CP_Maybe_Expand //L-par)
 (let ((//L-save //L))
  (set! //L //L-par)
  (let ((//P1 (@Posn))
        (//P2 '())
        (/cond1 (@I))
        (/used '())
        (//O/K-save //O/K)
        (/cc_assigned 0)
        (/comments '())
        (/var-save /var)
        (/clash 0)
        (/v1 '())
        (/v2 '()))
   (set! //O/K 0)
   (set! /var 0)
   ; If cc is assigned in the COND then allow expansion over a single assignment: 
   ; But *don't* expand over an assignment to cc! 
   (cond
    ((and (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1) 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1) 1) 1)) /cc_name))
     (set! /used (union-n (list (list /cc_name)) (@Elts_Used (@I))))
     (set! //O/K 1)
     (set! /cc_assigned 1)))
   ; Check for another COND, beyond any intervening comments and a simple assignment: 
   ; P1 is this COND, P2 will be next COND or CALL dispatch 
   (@Right)
   (set! /fl_flag2 0)
   (while (= /fl_flag2 0) 
    (cond
     ((not (@Right?))
      (set! /fl_flag2 1))
     ((and (= (@ST (@I)) //T_/Assignment) (or (= //O/K 1) (and (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) /destination)) (and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /dispatch_codes))))
      ; don't skip an assignment to a DSECT pointer since Absorb_Left 
      ; will then replace the pointer by its value in a[FOO].BAR 
      (set! //O/K 0)
      (for-in /var (@Elts_Assigned (@I)) 
       (cond
        ((and (@Elt_Clash? /used /var) (not (and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (not (null? (gethash //D/S/E/C/Ts (@V (@Get_n (@Get_n (@I) 1) 1))))))))
         (set! //O/K (- 1)))))
      (@Right)
      (set! /fl_flag2 0))
     ((= (@ST (@I)) //T_/Comment)
      (@Right)
      (set! /fl_flag2 0))
     ((and (= (@ST (@I)) //T_/Cond) (= /cc_assigned 1) (not-member /cc_name (@Used (@I))))
      ; Check if each arm is either improper or SKIP 
      (@Down)
      ; to first guarded 
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (cond
         ((@Gen_Improper? (@Get_n (@I) 2) "Reg")
          ; OK 
          (set! /fl_flag1 0))
         ((and (= (@Size (@Get_n (@I) 2)) 1) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Skip))
          ; OK 
          (set! /fl_flag1 0))
         (#t
          (set! /fl_flag1 2)))
        (cond
         ((= /fl_flag1 0)
          (cond
           ((not (@Right?))
            (set! /fl_flag1 1))
           (#t
            (@Right)
            (set! /fl_flag1 0)))))))
      (cond
       ((= /fl_flag1 2)
        (set! /fl_flag2 1))
       (#t
        ; If we get here, then it is OK to skip this Cond 
        ; We will process it later (to ensure that any CALLs are processed) 
        ; See LAST(P2) below! 
        (@Up)
        (@Right)
        (set! /fl_flag2 0))))
     (#t
      (set! /fl_flag2 1))))
   (cond
    ((and (= (@ST (@I)) //T_/Cond) (equal? //O/K (- 1)))
     ; We have something like: 
     ;         IF r1 = 0 THEN cc := 0 ... FI; r1 := r3; IF cc = 0 
     ; Expand the second cond backwards over the assignment 
     (while (and (not (= (@ST (@Get_n (@Parent) (- (@Posn_n) 1))) //T_/Assignment)) (@Trans? //T/R_/Absorb_/Left)) 
      (@Trans //T/R_/Absorb_/Left ""))
     (cond
      ((@Trans? //T/R_/Absorb_/Left)
       (@Trans //T/R_/Absorb_/Left "")))
     (set! //P2 (@Posn))))
   (cond
    ((= (@ST (@I)) //T_/Cond)
     (set! //P2 (@Posn))
     ; Check if the second COND only has tests cc = 0 and/or cc <> 0 
     ; If so, then we can replace the tests directly. 
     (cond
      ((= /cc_assigned 1)
       (set! //O/K (@CP_Simple_cc0  //P1 //P2 //O/K))
       (cond
        ((= //O/K 0)
         (set! //O/K (@CP_Simple_cc3  //P1 //P2 //O/K))))))
     ; If either Simple_cc fix worked, then move the first COND: 
     (cond
      ((= //O/K 1)
       (@Left)
       (while (not (= (@ST (@I)) //T_/Cond)) 
        (begin
         (cond
          ((= (@ST (@I)) //T_/Comment)
           (set! /comments (cons (@I) /comments))
           (@Paste_Over (@Skip))))
         (@Left)))
       (@Right)
       (while (not (= (@ST (@I)) //T_/Cond)) 
        (@Right))))
     ; Even if the above succeeded, there may be some tests in nested CONDs: 
     (set! //O/K 0)
     (set! /vars (@CP_Multi_Valued_Vars /cond1 //L))
     (cond
      ((and (null? /vars) (= (gen-length (@Elts_Assigned /cond1)) 1) (= (@Size (@Get_n (@Get_n /cond1 1) 2)) 1) (= (@ST (@Get_n (@Get_n (@Get_n /cond1 1) 2) 1)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@Get_n (@Get_n (@Get_n /cond1 1) 2) 1) 1) 2)) //T_/Number))
       (set! /vars (@Elts_Assigned /cond1))))
     ; See if any of these variables appear in the tests in the next statement. 
     (@Down)
     ; to first guard 
     (while (and (or (not (= //T_/Cond (@ST (@Get_n (@Get_n (@I) 2) 1)))) (null? (intersection-n (@Elts_Used (@Get_n (@Get_n (@Get_n (@I) 2) 1) 1)) /vars))) (null? (intersection-n (@Elts_Used (@Get_n (@I) 1)) /vars)) (@Right?)) 
      (@Right))
     (cond
      ((not (null? (intersection-n (@Elts_Used (@Get_n (@I) 1)) /vars)))
       ; Found one: do the expand 
       (set! //O/K 1))
      ((and (= //T_/Cond (@ST (@Get_n (@Get_n (@I) 2) 1))) (not (null? (intersection-n (@Elts_Used (@Get_n (@Get_n (@Get_n (@I) 2) 1) 1)) /vars))))
       (set! //O/K 1)))
     (@Goto //P1)
     ; back to orig position and insert any deleted comments 
     (cond
      ((= //O/K 0)
       (while (not (null? /comments)) 
        (begin
         (@Paste_Before (car /comments))
         (@Right)
         (set! /comments (cdr /comments)))))
      (#t
       (@Paste_Over (@Skip))
       ; Process any Cond we skipped earlier. 
       (while (< (@Posn_n) (last-1 //P2)) 
        (begin
         (set! //L (@CP_Statement  //L))
         (@Right)))
       (@Goto //P2)
       (while (not (null? /comments)) 
        (begin
         (@Paste_Before (car /comments))
         (@Right)
         (set! /comments (cdr /comments))))
       (@Paste_Before /cond1)
       ; First cond is selected now: either expand the first cond, 
       ; OR (if the first cond is a 3-way cond which sets cc) 
       ; update the conditions in the second cond and leave it selected. 
       (cond
        ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
         (@CP_Maybe_Expand_cc //L /vars))))))
    ((and (= (@ST (@I)) //T_/Call) (equal? (- (@V (@I))) /dispatch))
     (set! //P2 (@Posn))
     (@Goto //P1)
     ; back to orig position 
     (cond
      ((equal? (@Assigned (@I)) (list /cc_name))
       ; The Cond only sets up cc 
      )
      (#t
       (display-list " ")
       (display-list-flush "Expanding a Cond over CALL dispatch ")
       (let ((/n-save /n))
        (set! /n (- (last-1 //P2) (last-1 //P1)))
        (while (> /n 1) 
         (begin
          (cond
           ((= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Comment)
            (@Trans //T/R_/Move_/To_/Right ""))
           (#t
            (@Trans //T/R_/Absorb_/Right "")
            (display-list-flush "E")))
          (set! /n (- /n 1))))
        (set! /n /n-save))
       ; Now absorb the dispatch: 
       (@Trans //T/R_/Absorb_/Right "")
       (display-list "E")
       ; Back to the first position (so that the assignment gets processed) 
       (@Goto //P1))))
    ((and (= (@ST (@I)) //T_/Proc_/Call) (@Right?))
     (set! //P2 (@Posn))
     (@Right)
     (while (and (@Right?) (or (= (@ST (@I)) //T_/Comment) (= (@ST (@I)) //T_/Proc_/Call))) 
      (@Right))
     (cond
      ((or (and (= (@ST (@I)) //T_/Call) (equal? (- (@V (@I))) /dispatch)) (and (<= (@Total_Size (@I)) 12) (> (@Call_Freq (- /dispatch) (@I)) 0)))
       (set! /vars (@CP_Multi_Valued_Vars /cond1 //L))
       (@Goto //P1)
       ; back to orig position 
       ; We have to hope that one of the multi-valued variables 
       ; gets copied into destination in one of the called procs. 
       (cond
        ((not (null? /vars))
         (@Paste_Over (@Skip))
         (@Goto //P2)
         (@Paste_Before /cond1)
         ; First cond is selected now 
         ; Keep expanding (over proc calls and comments) 
         ; until the dispatch call is reached 
         (while (and (or (= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Comment) (= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Proc_/Call)) (@Trans? //T/R_/Expand_/Forward)) 
          (@Trans //T/R_/Expand_/Forward ""))
         (cond
          ((@Trans? //T/R_/Expand_/Forward)
           (@Trans //T/R_/Expand_/Forward "")))
         (display-list-flush "F"))))
      (#t
       (@Goto //P1))))
    (#t
     (@Goto //P1)
     ; don't expand 
    ))
   (set! //O/K //O/K-save)
   (set! /var /var-save))
  (set! //L //L-save)))

; Check for the tests cc = 0 and cc <> 0 in the second COND (selected) 
; If all the tests take this form, then replace them with a condition from 
; the first COND: 
(define (@CP_Simple_cc0 //P1 //P2 //O/K-par)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K //O/K-par)
  (set! //O/K 1)
  (@Down)
  ; to first guarded 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (@Down)
    (@Foreach_Cond /foreach-constant_propagation-12 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Up)
    (cond
     ((= //O/K 0)
      (set! /fl_flag1 1))
     ((@Right?)
      (@Right)
      (set! /fl_flag1 0))
     (#t
      (set! /fl_flag1 1)))))
  (@Up)
  (cond
   ((= //O/K 1)
    (@Goto //P1)
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__constant_propagation__19 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__/B3_save //B3)
             (/__/B2_save //B2)
             (/__/B1_save //B1))
        (set! //B3 (vector-ref /__/Match_array 2))
        (set! //B2 (vector-ref /__/Match_array 1))
        (set! //B1 (vector-ref /__/Match_array 0))
        (cond
         ((not-member /cc_name (@Variables //B1))
          (set! //B2 (@Not //B1))
          (@Goto //P2)
          (@CP_Replace_cc0 //B1 //B2))
         (#t
          (set! //O/K 0)))
        (set! //B3 /__/B3_save)
        (set! //B2 /__/B2_save)
        (set! //B1 /__/B1_save)))
      (#t
       (let ((/__/O/K 1))
        (set! /__/O/K (@New_Match  /%const__constant_propagation__20 (@I) /__/O/K))
        (cond
         ((= /__/O/K 1)
          (let ((/__/B2_save //B2)
                (/__/B1_save //B1))
           (set! //B2 (vector-ref /__/Match_array 1))
           (set! //B1 (vector-ref /__/Match_array 0))
           (cond
            ((not-member /cc_name (@Variables //B1))
             (set! //B2 (@Not //B1))
             (@Goto //P2)
             (@CP_Replace_cc0 //B1 //B2))
            (#t
             (set! //O/K 0)))
           (set! //B2 /__/B2_save)
           (set! //B1 /__/B1_save)))
         (#t
          (@Goto //P2)
          (set! //O/K 0)))))))))
  (set! funct-result //O/K)
  (set! //O/K //O/K-save)
  funct-result))

(define (@CP_Replace_cc0 //B1-par //B2-par)
 (let ((//B2-save //B2)
       (//B1-save //B1))
  (set! //B2 //B2-par)
  (set! //B1 //B1-par)
  (@Down)
  ; to first guarded 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (@Down)
    (@Foreach_Cond /foreach-constant_propagation-13 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Up)
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@Up)
  (set! //B2 //B2-save)
  (set! //B1 //B1-save)))

; Check for the tests cc = 3 and cc <> 3 in the second COND (selected) 
; If all the tests take this form, then replace them with a condition from 
; the first COND (provided the tests in the first COND are suitable): 
(define (@CP_Simple_cc3 //P1 //P2 //O/K-par)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K //O/K-par)
  (set! //O/K 1)
  (@Down)
  ; to first guarded 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (@Down)
    (@Foreach_Cond /foreach-constant_propagation-14 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Up)
    (cond
     ((= //O/K 0)
      (set! /fl_flag1 1))
     ((@Right?)
      (@Right)
      (set! /fl_flag1 0))
     (#t
      (set! /fl_flag1 1)))))
  (@Up)
  (cond
   ((= //O/K 1)
    (@Goto //P1)
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__constant_propagation__23 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__e2_save /e2)
             (/__e1_save /e1))
        (set! /e2 (vector-ref /__/Match_array 1))
        (set! /e1 (vector-ref /__/Match_array 0))
        (@Goto //P2)
        (cond
         ((or (and (= (@ST /e2) //T_/Number) (not (= (@V /e2) 0))) (and (= (@ST /e2) //T_/String) (not (equal? (@V /e2) "hex 0x00"))))
          (cond
           ((not-member /cc_name (@Variables /e1))
            (@CP_Replace_cc3 (@Make //T_/Equal '() (list /e1 /e2)) (@Make //T_/Not_/Equal '() (list /e1 /e2))))
           (#t
            (set! //O/K 0))))
         (#t
          (set! //O/K 0)))
        (set! /e2 /__e2_save)
        (set! /e1 /__e1_save)))
      (#t
       (@Goto //P2)
       (set! //O/K 0))))))
  (set! funct-result //O/K)
  (set! //O/K //O/K-save)
  funct-result))

(define (@CP_Replace_cc3 //B1-par //B2-par)
 (let ((//B2-save //B2)
       (//B1-save //B1))
  (set! //B2 //B2-par)
  (set! //B1 //B1-par)
  (@Down)
  ; to first guarded 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (@Down)
    (@Foreach_Cond /foreach-constant_propagation-15 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Up)
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@Up)
  (set! //B2 //B2-save)
  (set! //B1 //B1-save)))

; Check for the case where the first cond is a three-way assignment to cc 
; and the second cond tests cc, otherwise do the expand. 
; First cond is selected, vars is the list of multi-valued vars in the first cond 
; which are accessed in the second cond. 
(define (@CP_Maybe_Expand_cc //L-par /vars-par)
 (let ((/vars-save /vars)
       (//L-save //L))
  (set! /vars /vars-par)
  (set! //L //L-par)
  (let ((/cc_values '())
        (/ok 1)
        (/n-save /n)
        (/tests-save /tests)
        (/cc_val-save /cc_val)
        (/size 0)
        (/p 0))
   (set! /n '())
   (set! /tests '())
   (set! /cc_val '())
   (set! /cc_val (@CP_Get (wsl-ref (wsl-ref //L 1) 2) (list /cc_name)))
   ; Count the number of guards, ignoring improper ones 
   ; First cond sets cc to multiple values, and second cond tests it 
   ; so expanding the first cond should not lead to code bloat 
   (for-in /guard (@Cs (@I)) 
    (cond
     ((not (@Gen_Improper? /guard "Reg"))
      (set! /size (+ /size 1)))))
   (cond
    ((<= /size 2)
     (cond
      ((@Trans? //T/R_/Expand_/Forward)
       (@Trans //T/R_/Expand_/Forward "")
       (display-list-flush "2"))))
    ((not (member (list /cc_name) /vars))
     #t)
    (#t
     ; For a 3-way cond, check that each arm contains an assignment cc := n 
     ; or is a skip (the value that was assigned to cc is already there) 
     ; or is improper 
     (@Down)
     ; to first guard 
     (set! /test /true)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (set! /tests (cons (@And /test (@Get_n (@I) 1)) /tests))
       (set! /test (@And /test (@Not (@Get_n (@I) 1))))
       (@Down_To 2)
       ; to statements 
       (let ((/__/O/K 1))
        (set! /__/O/K (@New_Match  /%const__constant_propagation__24 (@I) /__/O/K))
        (cond
         ((= /__/O/K 1)
          (let ((/__n_save /n))
           (set! /n (vector-ref /__/Match_array 0))
           (cond
            ((= (@ST /n) //T_/Number)
             (set! /cc_values (cons /n /cc_values)))
            (#t
             (set! /ok 0)))
           (set! /n /__n_save)))
         (#t
          (let ((/__/O/K 1))
           (set! /__/O/K (@New_Match  /%const__constant_propagation__25 (@I) /__/O/K))
           (cond
            ((= /__/O/K 1)
             (cond
              ((not (null? /cc_val))
               (set! /cc_values (cons /cc_val /cc_values)))
              (#t
               (set! /ok 0))))
            ((not (@Is_Improper?))
             (set! /ok 0)))))))
       (@Up)
       (cond
        ((or (not (@Right?)) (not (= /ok 1)))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))
     (set! /tests (reverse /tests))
     (set! /cc_values (reverse /cc_values))
     (@Up)
     ; back to first cond 
     (cond
      ((= /ok 1)
       (@Right)
       (@CP_Fix_cc_Conds /cc_values /tests)))))
   (set! /n /n-save)
   (set! /tests /tests-save)
   (set! /cc_val /cc_val-save))
  (set! /vars /vars-save)
  (set! //L //L-save)))

(define (@CP_Fix_cc_Conds /orig_cc_values-par /orig_tests-par)
 (let ((/orig_tests-save /orig_tests)
       (/orig_cc_values-save /orig_cc_values))
  (set! /orig_tests /orig_tests-par)
  (set! /orig_cc_values /orig_cc_values-par)
  (let ((/tests-save /tests)
        (/newtests-save /newtests)
        (/p (@Posn_n))
        (/cc_val-save /cc_val)
        (/vars-save /vars))
   (set! /tests '())
   (set! /newtests '())
   (set! /cc_val '())
   (set! /vars (union-n (my-reduce @Set_Union (my-map @Variables /orig_tests)) (list /cc_name)))
   (set! /fl_flag2 0)
   (while (= /fl_flag2 0) 
    (begin
     ; Process each of the tests in the cond: 
     ; For each condition, find the sub-conditions which only reference cc 
     ; Construct a list of all the cc values for which the condition is true, 
     ; Replace the condition by an OR of the corresponding orig_tests: 
     (@Down)
     ; to first guarded 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (@Down)
       ; to first cond 
       (@Foreach_Cond /foreach-constant_propagation-16 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))
       (@Right)
       (@Down)
       ; to first statement 
       (cond
        ((and (= (@ST (@I)) //T_/Cond) (member /cc_name (@Variables (@I))))
         (@CP_Fix_cc_Conds /orig_cc_values /orig_tests)))
       (@Up)
       (@Up)
       ; Back to guarded 
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))
     (@Up)
     ; Move to next Cond (if any), unless current statement clobbers cc: 
     (cond
      ((or (not (null? (intersection-n /vars (@Assigned (@I))))) (member //T_/Proc_/Call (@Stat_Types (@I))))
       (set! /fl_flag2 1))
      ((not (@Right?))
       (set! /fl_flag2 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)
       (while (= /fl_flag1 0) 
        (cond
         ((and (= (@ST (@I)) //T_/Cond) (member /cc_name (@Variables (@I))))
          (set! /fl_flag1 1))
         ((not (null? (intersection-n /vars (@Assigned (@I)))))
          (set! /fl_flag1 2))
         ((member //T_/Proc_/Call (@Stat_Types (@I)))
          (set! /fl_flag1 2))
         ((@Right?)
          (@Right)
          (set! /fl_flag1 0))
         (#t
          (set! /fl_flag1 2))))
       (cond
        ((= /fl_flag1 2)
         (set! /fl_flag2 1))
        (#t
         (set! /fl_flag2 0)))))))
   ; Restore the position: 
   (while (> (@Posn_n) /p) 
    (@Left))
   (set! /tests /tests-save)
   (set! /newtests /newtests-save)
   (set! /cc_val /cc_val-save)
   (set! /vars /vars-save))
  (set! /orig_tests /orig_tests-save)
  (set! /orig_cc_values /orig_cc_values-save)))

; Return the set of variable names which are assigned two or more values 
; in the arms of the given Cond statement. 
; First collect the value lists for each branch into ll, 
; but combine inconsistent values into a list instead of deleting them. 
; Record the vars which end with 2 or more known values. 
(define (@CP_Multi_Valued_Vars /cond //L-par)
 (let ((//L-save //L)
       (/ll '())
       (/orig/L //L-par)
       (/vars-save /vars)
       (/ok 0)
       (//Unfold_/Dispatch-save //Unfold_/Dispatch)
       (funct-result '()))
  (set! //L //L-par)
  (set! /vars '())
  (set! //Unfold_/Dispatch 0)
  (@CP_Save_State)
  (@Edit)
  (@New_Program /cond)
  (@Down)
  ; to first guarded 
  (set! //L (@CP_Guarded  //L))
  (cond
   ((not (null? //L))
    (set! /ll (wsl-ref (wsl-ref //L 1) 2))))
  ; Merge the results of other branches with ll: 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((not (@Right?))
     (set! /fl_flag1 1))
    (#t
     (@Down)
     (@Edit)
     (set! /orig/L (@CP_Deny_Condition  /orig/L))
     (@Undo_Edit)
     (@Up)
     (set! //L /orig/L)
     (@Right)
     (set! //L (@CP_Guarded  //L))
     (cond
      ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
       (set! /ll (@CP_Merge_Keep /ll (wsl-ref (wsl-ref //L 1) 2)))))
     ; Ignore the rest of the guards after a TRUE guard in a Cond 
     (cond
      ((= (@ST (@Get_n (@I) 1)) //T_/True)
       (set! /fl_flag1 1))
      (#t
       (set! /fl_flag1 0))))))
  ; Extract the variables with two or more values from ll 
  ; If any value is variable, then skip this one 
  (while (not (null? /ll)) 
   (begin
    (cond
     ((and (> (gen-length (wsl-ref /ll 1)) 2) (not-member //T_/Variable (my-map @ST (cdr (wsl-ref /ll 1)))))
      (set! /vars (cons (wsl-ref (wsl-ref /ll 1) 1) /vars))))
    (set! /ll (cdr /ll))))
  (@Undo_Edit)
  (@CP_Restore_State)
  (set! funct-result (@Make_Set /vars))
  (set! //L //L-save)
  (set! /vars /vars-save)
  (set! //Unfold_/Dispatch //Unfold_/Dispatch-save)
  funct-result))

; Update the condition, then do the body (unless the condition becomes FALSE) 
(define (@CP_Guarded //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (@Down)
  ; to condition 
  (set! //L (@CP_Update  //L))
  ; If the condition is v=n or (v=n AND ...), then we know that v=n 
  (set! //L (@CP_Assert_Condition  //L))
  (cond
   ((= (@ST (@I)) //T_/False)
    (set! //L '())
    ; Don't let this arm affect results from other arms 
   )
   (#t
    (@Right)
    ; to statements 
    (set! //L (@CP_Statements  //L))))
  (@Up)
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; After an assertion, we know the condition is true 
(define (@CP_Assert //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (@Down)
  (set! //L (@CP_Update  //L))
  (set! //L (@CP_Assert_Condition  //L))
  (@Up)
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Update L with values provided by the condition 
(define (@CP_Assert_Condition //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((= (@ST (@I)) //T_/Equal)
    (cond
     ((and (@CP_Variable? (@Get_n (@I) 1)) (@CP_Constant? (@Get_n (@I) 2)))
      (set! //L (@CP_Put0_If_Safe //L (@CP_Var_Name (@Get_n (@I) 1)) (@Get_n (@I) 2))))
     ((and (@CP_Variable? (@Get_n (@I) 2)) (@CP_Constant? (@Get_n (@I) 1)))
      (set! //L (@CP_Put0_If_Safe //L (@CP_Var_Name (@Get_n (@I) 2)) (@Get_n (@I) 1))))))
   ((= (@ST (@I)) //T_/And)
    (@Down)
    ; to first conjunct 
    (set! //L (@CP_Assert_Condition  //L))
    (while (@Right?) 
     (begin
      (@Right)
      (set! //L (@CP_Assert_Condition  //L))))
    (@Up))
   ((= (@ST (@I)) //T_/Not)
    (@Down)
    (set! //L (@CP_Deny_Condition  //L))
    (@Up)))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; If an arm of a cond has v<>n then we know v=n afterwards, 
; also after WHILE v<>n DO ... OD we know v=n 
(define (@CP_Deny_Condition //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((= (@ST (@I)) //T_/Not_/Equal)
    (cond
     ((and (@CP_Variable? (@Get_n (@I) 1)) (@CP_Constant? (@Get_n (@I) 2)))
      (set! //L (@CP_Put0_If_Safe //L (@CP_Var_Name (@Get_n (@I) 1)) (@Get_n (@I) 2))))
     ((and (@CP_Variable? (@Get_n (@I) 2)) (@CP_Constant? (@Get_n (@I) 1)))
      (set! //L (@CP_Put0_If_Safe //L (@CP_Var_Name (@Get_n (@I) 2)) (@Get_n (@I) 1))))))
   ((= (@ST (@I)) //T_/Or)
    (@Down)
    ; to first conjunct 
    (set! //L (@CP_Deny_Condition  //L))
    (while (@Right?) 
     (begin
      (@Right)
      (set! //L (@CP_Deny_Condition  //L))))
    (@Up))
   ((= (@ST (@I)) //T_/Not)
    (@Down)
    (set! //L (@CP_Assert_Condition  //L))
    (@Up)))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; A version of @CP_Put0 which doesn't clobber fields of the value if they 
; already appear in the list. The problem is this: 
; foo.bar := 1234; 
; IF foo = 0 THEN ... ELSE ... FI; 
; PRINT(foo.bar) 
; Within the THEN part of the IF we must not clobber foo.bar 
; since it will not get restored! 
; This is OK since we can assume foo = 0 but we haven't assigned to it. 
; Note: if foo is already a known value not equal to 0 then this won't be applied! 
(define (@CP_Put0_If_Safe //L //N //V)
 
 (if (or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0)) (if (not (equal? (car //N) /exit_flag)) (cons (list 0 (list (list //N //V))) //L) //L) (if (equal? (car //N) /exit_flag) (if (or (= (@V //V) 1) (and (> //C/P_/In_/Preserves_/Dest 1) (> (@V //V) 1))) (cdr //L) //L) (cons (list 0 (@CP_Put_If_Safe (wsl-ref (wsl-ref //L 1) 2) //N //V)) (cdr //L)))))

(define (@CP_Put_If_Safe //L //N //V)
 (let ((/old (@CP_Get //L //N)))
  (if (or (equal? (car //N) /exit_flag) (and (not (null? /old)) (@Equal? //V /old))) //L (cons (list //N //V) //L))))

(define (@CP_Assignment //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (set! //L (@CP_Assigns  //L))
  ; Replace with a SKIP if there are no assigns left: 
  (cond
   ((not (@Cs? (@I)))
    (@Paste_Over (@Make //T_/Skip '() '()))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; For an assign: 
; (v is a `simple' variable, e is any expression, C is a constant) 
; v:=e first call @CP_Update on e, 
;   then remove v from the list and check for these cases: 
; v:=C adds v to the list with value C 
;   if we initially had v=C, then delete the assign 
; v1:=v2 where v2 is in the list with value C, adds/updates v1 to C 
;   if we initially had v1=C, then delete the assign 
;   (this case is dealt with by updating v1 and v2 before checking) 
; v[n..m]:=C where (n-m) < 4: split up C into separate bytes and store 
; as separate entries in L (up to 4 bytes) 
(define (@CP_Assigns //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  ; Check for an assignment to exit_flag or an increment of a variable 
  ; with the special value: 
  (cond
   ((or (null? //L) (not (= (wsl-ref (wsl-ref //L 1) 1) 0)))
    #t)
   ((and (= (@Size (@I)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) /exit_flag) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number))
    (cond
     ((or (= (@V (@Get_n (@Get_n (@I) 1) 2)) 1) (and (> //C/P_/In_/Preserves_/Dest 1) (> (@V (@Get_n (@Get_n (@I) 1) 2)) 1)))
      (set! //L (cdr //L)))))
   ((and (= (@Size (@I)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Plus))
    (cond
     ((and (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1)) //T_/Variable) (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) (@V (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1))) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 2) 2)) //T_/Number) (or (= (@V (@Get_n (@Get_n (@Get_n (@I) 1) 2) 2)) 12) (= (@V (@Get_n (@Get_n (@Get_n (@I) 1) 2) 2)) 8) (= (@V (@Get_n (@Get_n (@Get_n (@I) 1) 2) 2)) 4)))
      (let ((/val-save /val)
            (/inc (@V (@Get_n (@Get_n (@Get_n (@I) 1) 2) 2))))
       (set! /val (@CP_Get (wsl-ref (wsl-ref //L 1) 2) (list (@V (@Get_n (@Get_n (@I) 1) 1)))))
       (cond
        ((null? /val)
         (set! //L (@CP_Assigns_Sub  //L)))
        ((not (= (@ST /val) //T_/Number))
         (set! //L (@CP_Assigns_Sub  //L)))
        ((equal? (@V /val) //C/P_/Special_/Value)
         (display-list "<inc: " (@N_String (@V (@Get_n (@Get_n (@I) 1) 1))) ">")
         (set! //C/P_/Return_/Code_/Inc (union-n //C/P_/Return_/Code_/Inc (list /inc))))
        (#t
         (set! //L (@CP_Assigns_Sub  //L))))
       (set! /val /val-save)))
     (#t
      (set! //L (@CP_Assigns_Sub  //L)))))
   (#t
    (set! //L (@CP_Assigns_Sub  //L))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_Assigns_Sub //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/new/L-save /new/L)
        (/old/L-save /old/L)
        (/new-save /new)
        (/v-save /v)
        (/e-save /e)
        (/e2-save /e2)
        (/keep-save /keep)
        (/old_assign '())
        (/changed 0)
        (/v1 '())
        (/e1-save /e1))
   (set! /new/L (wsl-ref (wsl-ref //L 1) 2))
   (set! /old/L (wsl-ref (wsl-ref //L 1) 2))
   (set! /new '())
   (set! /v '())
   (set! /e '())
   (set! /e2 '())
   (set! /keep 1)
   (set! /e1 '())
   (@Edit)
   (@Down)
   ; to first assign 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (set! /keep 1)
     ; `keep' records whether we need to keep this assign 
     (set! /old_assign (@I))
     ; `changed' will record whether any assign has been changed or deleted 
     ; Check for rX[1] := hex 0xNN where rX is known to be zero already 
     (cond
      ((and (= (@ST (@Get_n (@I) 1)) //T_/Aref_/Lvalue) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1)) //T_/Number) (= (@V (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue))
       (cond
        ((or (and (= (@ST (@Get_n (@I) 2)) //T_/String) (or (equal? (@V (@Get_n (@I) 2)) " ") (@Starts_With? (@V (@Get_n (@I) 2)) "hex 0x"))) (= (@ST (@Get_n (@I) 2)) //T_/Number))
         (let ((/v-save /v)
               (/e-save /e)
               (/str ""))
          (set! /v (@Get_n (@Get_n (@I) 1) 1))
          (set! /e (@CP_Get /old/L (@CP_Var_Name (@Get_n (@Get_n (@I) 1) 1))))
          (cond
           ((and (not (null? /e)) (= (@ST /e) //T_/Number) (= (@V /e) 0))
            (@Down)
            ; to rX[1] lvalue 
            (@Paste_Over /v)
            (@Right)
            ; to expn 
            (cond
             ((= (@ST (@I)) //T_/Number)
              (set! /str (@Num_To_Hex (@V (@I))))
              (while (< (string-length /str) 2) 
               (set! /str (string-append "0" /str))))
             ((and (= (@ST (@I)) //T_/String) (equal? (@V (@I)) " "))
              (set! /str "hex 0x40"))
             (#t
              (set! /str (@V (@I)))))
            (@Paste_Over (@Make //T_/String (string-append /str "000000") '()))
            (@Up)))
          (set! /v /v-save)
          (set! /e /e-save))))))
     (@Down)
     ; to lvalue 
     ; Update both sides of the assign with current known values, 
     ; then check if a variable is being assigned a constant value. 
     ; (Consider the assignment v[a] := x where both a and x are known) 
     (set! //L (@CP_Update  //L))
     (cond
      ((@Is_Mem_Rel? (@I))
       (cond
        ((@Is_Addr? (@Get_Mem_Rel (@I)))
         (let ((/v1 (@Expn_To_Lvalue (@Get_Addr (@Get_Mem_Rel (@I)))))
               (/e1-save /e1))
          (set! /e1 (@Get_Mem_Rel_N (@I)))
          (cond
           ((or (= (@ST /v1) //T_/Sub_/Seg_/Lvalue) (= (@ST /v1) //T_/Rel_/Seg_/Lvalue))
            (set! /v1 (@Get_n /v1 1))))
          (cond
           ((= (@GT /v1) //T_/Lvalue)
            (@Paste_Over (@Make 503 '() (list (@Expn_To_Var /v1) (@Make 205 1 '()) (@Var_To_Expn /e1))))))
          (set! /e1 /e1-save)))))
      ((@Is_Mem? (@I))
       (cond
        ((@Is_Addr? (@Get_Mem (@I)))
         (let ((/v1 (@Expn_To_Lvalue (@Get_Addr (@Get_Mem (@I))))))
          (cond
           ((= (@ST /v1) //T_/Aref_/Lvalue)
            (@Paste_Over /v1))
           ((or (= (@ST /v1) //T_/Sub_/Seg_/Lvalue) (= (@ST /v1) //T_/Rel_/Seg_/Lvalue))
            (set! /v1 (@Get_n /v1 1))
            (@Paste_Over (@Make 502 '() (list (@Expn_To_Var /v1) (@Make 10 '() (list (@Make 205 1 '())))))))
           (#t
            (@Paste_Over (@Make 502 '() (list (@Expn_To_Var /v1) (@Make 10 '() (list (@Make 205 1 '())))))))))))))
     (cond
      ((and (@Is_Mem_Rel? (@I)) (not (= (@ST (@Get_Mem_Rel_N (@I))) //T_/Number)))
       ; This assignment will clobber random memory via a pointer
       (set! /new/L (@CP_Clobber_Zeros /new/L))))
     (set! /v (@CP_Var_Name (@I)))
     (cond
      ((and #f (= (@ST (@I)) //T_/Var_/Lvalue) (not (null? (gethash //D/S/E/C/Ts (@V (@I))))))
       ; Don't update the expression in a DSECT pointer assignment 
      )
      ((and (= (@ST (@I)) //T_/Var_/Lvalue) (equal? (@V (@I)) /r1_name))
       ; Hack: don't update assignment to restore r1 
      )
      (#t
       (@Right)
       (set! //L (@CP_Update  //L))
       (@Left)))
     (cond
      ((and (= (@ST (@I)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n (@I) 2)) //T_/Number) (= (@ST (@Get_n (@I) 3)) //T_/Number) (< (- (@V (@Get_n (@I) 3)) (@V (@Get_n (@I) 2))) 4))
       (set! /new/L (@CP_Sub_Seg_Lvalue  /new/L)))
      ((and (= (@ST (@I)) //T_/Rel_/Seg_/Lvalue) (= (@ST (@Get_n (@I) 2)) //T_/Number) (= (@ST (@Get_n (@I) 3)) //T_/Number) (<= (@V (@Get_n (@I) 3)) 4))
       (set! /new/L (@CP_Rel_Seg_Lvalue  /new/L)))
      ((not (null? /v))
       (@Right)
       ; to expression 
       (set! /e (@CP_Get /old/L /v))
       (cond
        ((or (null? /e) (not (@Equal? /e (@I))))
         (set! /new/L (@CP_Remove /new/L /v))))
       (cond
        ((and (= (@ST (@I)) //T_/Variable) (@Starts_With? (@V (@I)) "NOTUSED_"))
         (set! /e2 (gethash //Notused_/Value (@V (@I))))
         (cond
          ((null? /e2)
           (set! /e2 (@Make //T_/Number (@String_To_Num (substr (@N_String (@V (@I))) 8)) '()))
           (puthash //Notused_/Value (@V (@I)) /e2))))
        ((or (= (@ST (@I)) //T_/Variable) (= (@ST (@I)) //T_/Struct))
         (set! /e2 (@CP_Get /old/L (@CP_Var_Name (@I))))
         (cond
          ((null? /e2)
           (set! /e2 (@I)))))
        (#t
         (set! /e2 (@I))))
       (cond
        ((@CP_Constant? /e2)
         ; Don't record FOO.BAR as the value of FOO. 
         ; Check for incremented special value. 
         (cond
          ((@Prefix? /v (@CP_Var_Name /e2))
           (display-list-flush "^"))
          ((and (= (@ST /e2) //T_/Minus) (= (@ST (@Get_n /e2 1)) //T_/Plus))
           ; Hack for ptr := (reg + ptr) - ADDRESS_OF(a[ptr].foo.bar) 
           (set! /new/L (@CP_Put /new/L /v /e2)))
          ((member /v (@Elements /e2))
           ; Don't record a constant from v := v - n 
           ; (This shouldn't happen since v is not constant) 
          )
          ((and (= (@ST /e2) //T_/Number) (equal? (@V /e2) //C/P_/Special_/Value))
           (set! //C/P_/Return_/Code_/Normal 1)
           (set! /new/L (@CP_Put /new/L /v /e2)))
          ((and (= (@ST /e2) //T_/Number) (or (= (@V /e2) (+ //C/P_/Special_/Value 4)) (= (@V /e2) (+ //C/P_/Special_/Value 8)) (= (@V /e2) (+ //C/P_/Special_/Value 12))))
           (let ((/inc (- (@V /e2) //C/P_/Special_/Value)))
            (set! /new/L (@CP_Put /new/L /v (@Make //T_/Number //C/P_/Special_/Value '())))
            (display-list "(special value inc " /inc ")")
            (set! //C/P_/Return_/Code_/Inc (union-n //C/P_/Return_/Code_/Inc (list /inc)))))
          (#t
           (set! /new/L (@CP_Put /new/L /v /e2))))
         ; Don't delete an assign to cc just because you know the value: 
         (cond
          ((and (not (null? /e)) (and (not (equal? /v (list /cc_name))) (not (equal? /v (list /zf_name))) (not (equal? /v (list /cf_name))) (not (equal? /v (list /destination)))) (@Equal? /e /e2))
           ; delete this assign: the var already had this value 
           (display-list-flush "X")
           (set! /keep 0))))))
      ((= (@ST (@I)) //T_/Var_/Lvalue)
       ; For a complex assign, eg v[a]:=b, where a is unknown, 
       ; remove the assigned variable from L: 
       (set! /new/L (@CP_Remove /new/L (list (@V (@I))))))
      ((or (= (@ST (@I)) //T_/Aref_/Lvalue) (= (@ST (@I)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@I)) //T_/Rel_/Seg_/Lvalue) (= (@ST (@I)) //T_/Final_/Seg_/Lvalue))
       (set! /v (@CP_Var_Name (@Get_n (@I) 1)))
       (cond
        ((and (not (equal? /v (list /a_name))) (not (null? /v)))
         (set! /new/L (@CP_Remove /new/L /v)))))
      (#t
       (set! /v (@Elts_Assigned (@I)))
       (cond
        ((= (gen-length /v) 1)
         (set! /new/L (@CP_Remove /new/L (car /v)))))))
     (@Up)
     ; back to Assign 
     ; Check for and record simple variable equivalences: 
     (let ((/-result- (@CP_Check_Equiv  (@Get_n (@I) 1) (@Get_n (@I) 2) /old/L /new/L /keep)))
      (set! /old/L (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /new/L (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /keep (car /-result-)) (set! /-result- (cdr /-result-)))
     ; Check for saving a dispatch code in memory  a[e + n, 4] := c 
     (cond
      ((and (@Is_Mem_Rel? (@Get_n (@I) 1)) (= (@ST (@Get_Mem_Rel_N (@Get_n (@I) 1))) //T_/Number) (= (@V (@Get_Mem_Rel_N (@Get_n (@I) 1))) 4) (= (@ST (@Get_Mem_Rel (@Get_n (@I) 1))) //T_/Plus) (= (@ST (@Get_n (@Get_Mem_Rel (@Get_n (@I) 1)) 1)) //T_/Variable) (= (@ST (@Get_n (@Get_Mem_Rel (@Get_n (@I) 1)) 2)) //T_/Number) (or (@CP_Reg_Init_Var? (@Get_n (@Get_Mem_Rel (@Get_n (@I) 1)) 1)) (member (@V (@Get_n (@Get_Mem_Rel (@Get_n (@I) 1)) 1)) /registers)) (= (@ST (@Get_n (@I) 2)) //T_/Number) (member (@V (@Get_n (@I) 2)) /dispatch_codes) (> (@V (@Get_n (@I) 2)) 0))
       (display-list-flush "C")
       (let ((/name-save /name))
        (set! /name (@Make_Name (concat (string-append (@N_String (@V (@Get_n (@Get_Mem_Rel (@Get_n (@I) 1)) 1))) "__") (@String (@V (@Get_n (@Get_Mem_Rel (@Get_n (@I) 1)) 2))))))
        (set! /new/L (@CP_Put /new/L (list /name) (@Get_n (@I) 2)))
        (set! /name /name-save))))
     ; Don't replace a register by a reg_init (but do propagate the init) 
     (cond
      ((and (= (@ST (@Get_n /old_assign 2)) //T_/Variable) (= (@ST (@Get_n (@I) 2)) //T_/Variable) (member (@V (@Get_n (@I) 2)) /reg_inits) (member (@V (@Get_n /old_assign 2)) /registers))
       (@Paste_Over /old_assign)))
     ; If a reg_init is incremented, then keep it: 
     (cond
      ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@I) 2)) //T_/Plus) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Variable) (= (@ST (@Get_n (@Get_n (@I) 2) 2)) //T_/Number) (equal? (@V (@Get_n (@I) 1)) (@V (@Get_n (@Get_n (@I) 2) 1))))
       ; Incremented register 
       (let ((/val-save /val))
        (set! /val (@CP_Get /old/L (@CP_Var_Name (@Get_n (@I) 1))))
        (cond
         ((and (not (null? /val)) (= (@ST /val) //T_/Variable) (member (@V /val) /reg_inits))
          (set! /new/L /old/L)))
        (set! /val /val-save))))
     (cond
      ((or (= /keep 1) (member /destination (@Variables (@Get_n (@I) 1))))
       (set! /new (cons (@I) /new))
       (cond
        ((and (= /changed 0) (not (eq? /old_assign (@I))))
         (set! /changed 1))))
      (#t
       (set! /changed 1)))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))
   (set! //L (cons (list 0 /new/L) (cdr //L)))
   (@Up)
   ; to Assigns/Assignment 
   (@Undo_Edit)
   (cond
    ((= /changed 1)
     (@Paste_Over (@Make (@ST (@I)) '() (reverse /new)))))
   (set! /new/L /new/L-save)
   (set! /old/L /old/L-save)
   (set! /new /new-save)
   (set! /v /v-save)
   (set! /e /e-save)
   (set! /e2 /e2-save)
   (set! /keep /keep-save)
   (set! /e1 /e1-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Check for simple variable equivalences, if foo = bar already in oldL, 
; then set keep := 0, otherwise record the equivalence in newL. 
; The main use for this is in eliminating redundant reloads of dsect pointers 
; from the base register. 
(define (@CP_Check_Equiv /var-par /expn /old/L-par /new/L-par /keep-par)
 (let ((/keep-save /keep)
       (/new/L-save /new/L)
       (/old/L-save /old/L)
       (/var-save /var)
       (funct-result '()))
  (set! /keep /keep-par)
  (set! /new/L /new/L-par)
  (set! /old/L /old/L-par)
  (set! /var /var-par)
  (cond
   ((and (= (@ST /var) //T_/Var_/Lvalue) (= (@ST /expn) //T_/Variable))
    (let ((/val1 (@CP_Get /old/L (list (@V /var))))
          (/val2 (@CP_Get /old/L (list (@V /expn)))))
     (cond
      ((and (not (null? /val1)) (= (@ST /val1) //T_/Variable) (equal? (@V /val1) (@V /expn)))
       (set! /keep 0))
      ((and (not (null? /val2)) (= (@ST /val2) //T_/Variable) (equal? (@V /val2) (@V /var)))
       (set! /keep 0))
      ((@Starts_With? (@V /expn) "NOTUSED_")
       ; Propagate NOTUSED_nnn not nnn 
       (set! /new/L (@CP_Put /new/L (list (@V /var)) /expn)))
      ((not (null? (@CP_Get /new/L (list (@V /var)))))
       ; The assignment gave a known value to var 
      )
      (#t
       (set! /new/L (@CP_Put /new/L (list (@V /var)) /expn)))))))
  (set! funct-result (list /old/L /new/L /keep))
  (set! /keep /keep-save)
  (set! /new/L /new/L-save)
  (set! /old/L /old/L-save)
  (set! /var /var-save)
  funct-result))

; Assigning a number to a 4 or fewer byte segment 
(define (@CP_Sub_Seg_Lvalue /new/L-par)
 (let ((/new/L-save /new/L)
       (funct-result '()))
  (set! /new/L /new/L-par)
  (let ((/v-save /v)
        (/e-save /e)
        (/n-save /n)
        (/m (@V (@Get_n (@I) 3))))
   (set! /v (@CP_Var_Name (@Get_n (@I) 1)))
   (set! /e '())
   (set! /n (@V (@Get_n (@I) 2)))
   (cond
    ((not (null? /v))
     ; Remove the old values of the segments: 
     (for /i /n /m 1 
      (set! /new/L (@CP_Remove /new/L (concat /v (list (- /i))))))
     (@Right)
     ; to expression 
     (cond
      ((= (@ST (@I)) //T_/Number)
       (for /i /n /m 1 
        (begin
         (set! /e (@Make //T_/Number (@Byte (@V (@I)) (- /m /i)) '()))
         (set! /new/L (@CP_Put /new/L (concat /v (list (- /i))) /e))))))))
   (set! /v /v-save)
   (set! /e /e-save)
   (set! /n /n-save))
  (set! funct-result /new/L)
  (set! /new/L /new/L-save)
  funct-result))

(define (@CP_Rel_Seg_Lvalue /new/L-par)
 (let ((/new/L-save /new/L)
       (funct-result '()))
  (set! /new/L /new/L-par)
  (let ((/v-save /v)
        (/e-save /e)
        (/n-save /n)
        (/m (- (+ (@V (@Get_n (@I) 2)) (@V (@Get_n (@I) 3))) 1)))
   (set! /v (@CP_Var_Name (@Get_n (@I) 1)))
   (set! /e '())
   (set! /n (@V (@Get_n (@I) 2)))
   (cond
    ((not (null? /v))
     ; Remove the old values of the segments: 
     (for /i /n /m 1 
      (set! /new/L (@CP_Remove /new/L (concat /v (list (- /i))))))
     (@Right)
     ; to expression 
     (cond
      ((= (@ST (@I)) //T_/Number)
       (for /i /n /m 1 
        (begin
         (set! /e (@Make //T_/Number (@Byte (@V (@I)) (- /m /i)) '()))
         (set! /new/L (@CP_Put /new/L (concat /v (list (- /i))) /e))))))))
   (set! /v /v-save)
   (set! /e /e-save)
   (set! /n /n-save))
  (set! funct-result /new/L)
  (set! /new/L /new/L-save)
  funct-result))

; Return the nth most significant byte of an integer: 
(define (@Byte /i /n)
 
 (if (= /n 0) (modulo /i 256) (@Byte (quotient /i 256) (- /n 1))))

; For an Action System (which may have thousands of actions) we take this approach: 
; (1) Put the action bodies into an array 
; (2) Initialise `entry' tables for each action (including Z) 
;     These are initialised to empty, apart from the entry action, 
;     which is initialised with the values known on entry to the system. 
; (3) Iterate over all the actions, updating entry tables with new information, 
;     but NOT modifying any of the bodies with this `tentative' information 
; (4) Once this process converges, we can update the bodies using 
;     the entry tables for each action. Rebuild the action system. 
; (5) The entry table for Z is the result. 
; TODO: the iteration over actions should be carried out in dominator tree order 
; rather than a simple breadth-first order (to minimise the number of passes required.) 
; See the makepict stript for algorithms, the method is: 
;   (1) Calculate predecessor sets 
;   (2) Calculate a BFS ordering 
;   (3) Compute the dominator set for each node 
;   (4) Remove back edges 
;   (5) Do a topological sort (for an irreducible flowgraph there will 
;       be cycles: in this case pick an element with the minimum 
;       number of predecessors) 
; The entry table for each action is either < > (no information known) 
; or <0, l> where l is a list of variable-value pairs. 
; If we are in a proc call, then don't process a sub-action-system 
; (otherwise, the multiple passes can lead to exponential time factors) 
(define (@CP_A_S //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((//A/S_/Type-save //A/S_/Type))
   (set! //A/S_/Type (@System_Type (@I)))
   (cond
    ((not (@Regular_System? (@I)))
     (display-list "Action-System " (@N_String (@V (@Get_n (@I) 1))) " is not regular.")
     (@Down_To 2)
     (@Down)
     (@Down_To 2)
     (set! //L (list (list 0 '())))
     (set! //L (@CP_Statements  //L))
     (@Up)
     (while (@Right?) 
      (begin
       (@Right)
       (@Down_To 2)
       (set! //L (list (list 0 '())))
       (set! //L (@CP_Statements  //L))
       (@Up)))
     (@Up)
     (@Up)
     (set! //L (@CP_Clobber  //L)))
    ((not (null? //Call_/Path))
     (display-list "Sub-Action-System " (@N_String (@V (@Get_n (@I) 1))) " at " //Call_/Path)
     (@Down_To 2)
     (@Down)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (@Down_To 2)
       (set! //L (list (list 0 '())))
       (set! //L (@CP_Statements  //L))
       (@Up)
       (cond
        ((not (@Regular? (@I)))
         (display-list "Action " (@N_String (@V (@Get_n (@I) 1))) " is not regular!")))
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))
     (@Up)
     (@Up)
     (set! //L (@CP_Clobber  //L)))
    (#t
     (set! //L (@CP_A_S_Reg  //L))))
   (set! //A/S_/Type //A/S_/Type-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_A_S_Reg //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((//N-save //N))
   (set! //N (@Size (@Get_n (@I) 2)))
   (let ((//A/S_/Name (@V (@Get_n (@I) 1)))
         (//Start 0)
         (/dispatch-save /dispatch)
         (/destination-save /destination)
         (/orig_/Bodies (make-vector-eval //N '()))
         (//Bodies-save //Bodies)
         (//Names-save //Names)
         (//Entries-save //Entries)
         (//Succs (make-vector-eval (+ //N 1) '()))
         (//Preds-save //Preds)
         (/orig_/Entries (make-vector-eval (+ //N 1) '()))
         (//Name2/Num (hash-table))
         (/runs 1))
    (set! /dispatch 0)
    (set! /destination (@Make_Name "destination"))
    (set! //Bodies (make-vector-eval //N '()))
    (set! //Names (make-vector-eval (+ //N 1) '()))
    (set! //Entries (make-vector-eval (+ //N 1) '()))
    (set! //Preds (make-vector-eval (+ //N 1) '()))
    (@Edit)
    ; Calculate Bodies, Names, Name2Num 
    (let ((/-result- (@FD_Init  //N //Bodies //Names //Name2/Num)))
     (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
     (set! //Names (car /-result-)) (set! /-result- (cdr /-result-))
     (set! //Name2/Num (car /-result-)) (set! /-result- (cdr /-result-)))
    ; Find the starting action number: 
    (set! //Start (gethash //Name2/Num //A/S_/Name))
    (let ((/-result- (@FD_Succs_And_Preds  //N //Bodies //Start //Succs //Preds)))
     (set! //Succs (car /-result-)) (set! /-result- (cdr /-result-))
     (set! //Preds (car /-result-)) (set! /-result- (cdr /-result-)))
    ; Check for a dispatch action: 
    (set! /dispatch (gethash //Name2/Num (@Make_Name "dispatch")))
    (cond
     ((null? /dispatch)
      (set! /dispatch (- 1))))
    ; On entry to the system, only one entry list is known: 
    (wsl-set! //Entries (car //L) //Start)
    ; Do a breadth-first walk through the call graph, updating the entry lists. 
    ; Repeat until convergence (restore bodies after each iteration).  
    (for /i 1 //N 1 
     (vector-set! /orig_/Bodies (- /i 1) (wsl-ref //Bodies /i)))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (for /i 1 (+ //N 1) 1 
       (vector-set! /orig_/Entries (- /i 1) (wsl-ref //Entries /i)))
      (let ((//State_/Saves-save //State_/Saves))
       (set! //State_/Saves 0)
       (@CP_Save_State)
       (let ((/-result- (@CP_Update_Entries  //Start //N //Names //Bodies //Entries)))
        (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
        (set! //Entries (car /-result-)) (set! /-result- (cdr /-result-)))
       (@CP_Restore_State)
       (set! //State_/Saves //State_/Saves-save))
      (display-list "")
      (cond
       ((equal? /orig_/Entries //Entries)
        (set! /fl_flag1 1))
       (#t
        (set! /runs (+ /runs 1))
        (cond
         ((> /runs 20)
          (display-list "Too many scans required.")
          (set! /fl_flag1 1))
         (#t
          (display-list "")
          (display-list "Re-Scanning Action System, scan: " /runs)
          ; Restore bodies 
          (for /i 1 //N 1 
           (wsl-set! //Bodies (vector-ref /orig_/Bodies (- /i 1)) /i))
          (set! /fl_flag1 0)))))))
    ; Rebuild AS with new bodies: 
    (@FD_Rebuild_AS //N //Bodies //Names //A/S_/Name '())
    (@End_Edit)
    (cond
     ((null? (wsl-ref //Entries (+ //N 1)))
      ; Action system never reaches a CALL Z! 
      (set! //L (cdr //L)))
     (#t
      (wsl-set! //L (wsl-ref //Entries (+ //N 1)) 1)))
    (set! /dispatch /dispatch-save)
    (set! /destination /destination-save)
    (set! //Bodies //Bodies-save)
    (set! //Names //Names-save)
    (set! //Entries //Entries-save)
    (set! //Preds //Preds-save))
   (set! //N //N-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Breadth-first search for information, updating Entries 
; TODO: Use breadth-first search of dominator graph, not call graph 
(define (@CP_Update_Entries //Start //N-par //Names-par //Bodies-par //Entries-par)
 (let ((//Entries-save //Entries)
       (//Bodies-save //Bodies)
       (//Names-save //Names)
       (//N-save //N)
       (funct-result '()))
  (set! //Entries //Entries-par)
  (set! //Bodies //Bodies-par)
  (set! //Names //Names-par)
  (set! //N //N-par)
  (let ((/todo (list //Start))
        (/done-save /done)
        (/action-save /action)
        (//L-save //L)
        (/calls-save /calls)
        (/call-save /call))
   (set! /done (hash-table))
   (set! /action 0)
   (set! //L '())
   (set! /calls '())
   (set! /call '())
   (puthash /done (+ //N 1) 1)
   (puthash /done //Start 1)
   (while (not (null? /todo)) 
    (begin
     (set! /action (car /todo))
     (set! /todo (cdr /todo))
     ; Update the info for this actions successors: 
     (cond
      ((<= /initial_call_budget 200)
       (set! //L (list (list 0 '()))))
      ((not (null? (wsl-ref //Entries /action)))
       (set! //L (list (wsl-ref //Entries /action))))
      (#t
       (set! //L '())))
     (display-list-flush ".")
     (cond
      ((equal? /action /dispatch)
       (cond
        ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
         (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) (list /destination)) 1 2)))))
     (@New_Program (wsl-ref //Bodies /action))
     ; each CALL n will merge the current list with Entries[n] 
     (set! //L (@CP_Statements  //L))
     ; Calculate the successors from the edited body 
     ; and add new successors to the end of the todo list: 
     (set! /calls (@Calls (@Program)))
     (while (not (null? /calls)) 
      (begin
       (set! /call (- (car (car /calls))))
       (set! /calls (cdr /calls))
       (cond
        ((null? (gethash /done /call))
         (set! /todo (concat /todo (list /call)))
         (puthash /done /call 1)))))
     (wsl-set! //Bodies (@Program) /action)))
   (set! /done /done-save)
   (set! /action /action-save)
   (set! //L //L-save)
   (set! /calls /calls-save)
   (set! /call /call-save))
  (set! funct-result (list //Bodies //Entries))
  (set! //Entries //Entries-save)
  (set! //Bodies //Bodies-save)
  (set! //Names //Names-save)
  (set! //N //N-save)
  funct-result))

(define (@CP_Call //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((and (number? (@V (@I))) (< (@V (@I)) 0) (not (null? //Entries)) (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
    (let ((/n-save /n)
          (/new/L-save /new/L)
          (/dest_value 0)
          (/posn_n (@Posn_n))
          (/done-save /done)
          (/seen_code 0)
          (/tmp '()))
     (set! /n (- (@V (@I))))
     (set! /new/L (wsl-ref (wsl-ref //L 1) 2))
     (set! /done 0)
     ; If the call is to dispatch, and the value of destination 
     ; is known and an integer, then unfold and simplify the dispatch action 
     ; BUT don't unfold a dispatch call inside dispatch itself! 
     (cond
      ((and (equal? /n /dispatch) (not (equal? /action /dispatch)))
       (set! /dest_value (@CP_Get /new/L (list /destination)))
       ; If the value is a variable, check the variable's value 
       ; in case it is a reg init: 
       (cond
        ((and (not (null? /dest_value)) (= (@ST /dest_value) //T_/Variable) (not-member (@V /dest_value) /reg_inits))
         (set! /tmp (@CP_Get /new/L (list (@V /dest_value))))
         (cond
          ((not (null? /tmp))
           (set! /dest_value /tmp)))))
       (cond
        ((or (null? /dest_value) (= (@ST /dest_value) //T_/Variable))
         (set! /fl_flag1 0)
         (while (= /fl_flag1 0) 
          (cond
           ((not (@Left?))
            (set! /fl_flag1 1))
           (#t
            (@Left)
            (let ((/__/O/K 1))
             (set! /__/O/K (@New_Match  /%const__constant_propagation__26 (@I) /__/O/K))
             (cond
              ((= /__/O/K 1)
               (let ((/__e_save /e)
                     (/__v_save /v))
                (set! /e (vector-ref /__/Match_array 1))
                (set! /v (vector-ref /__/Match_array 0))
                (cond
                 ((and (= (@ST /v) //T_/Var_/Lvalue) (member (@V /v) /registers) (= (@ST /e) //T_/Number) (> (@V /e) 0) (member (@V /e) /dispatch_codes))
                  (set! /seen_code 1)))
                (set! /e /__e_save)
                (set! /v /__v_save)))))
            (cond
             ((and (not (= (@ST (@I)) //T_/Comment)) (not (= (@ST (@I)) //T_/Assignment)))
              (set! /fl_flag1 1))
             (#t
              (set! /fl_flag1 0))))))
         (@To /posn_n)))
       (cond
        ((null? /dest_value)
         (set! /fl_flag1 0)
         (while (= /fl_flag1 0) 
          (cond
           ((not (@Left?))
            (set! /fl_flag1 1))
           (#t
            (@Left)
            (cond
             ((and (not (= (@ST (@I)) //T_/Comment)) (not (= (@ST (@I)) //T_/Assignment)))
              (set! /fl_flag1 1))
             (#t
              (let ((/__/O/K 1))
               (set! /__/O/K (@New_Match  /%const__constant_propagation__27 (@I) /__/O/K))
               (cond
                ((= /__/O/K 1)
                 (let ((/__e_save /e))
                  (set! /e (vector-ref /__/Match_array 0))
                  (set! /e (@Get_n (@Get_n (@I) 1) 2))
                  (@To /posn_n)
                  (set! //L (@CP_Unfold_Dispatch  /e /action /seen_code //L))
                  (set! /done 1)
                  (set! /e /__e_save)))
                (#t
                 (let ((/__/O/K 1))
                  (set! /__/O/K (@New_Match  /%const__constant_propagation__28 (@I) /__/O/K))
                  (cond
                   ((= /__/O/K 1)
                    (let ((/__e_save /e))
                     (set! /e (vector-ref /__/Match_array 0))
                     (cond
                      ((and (= (@ST /e) //T_/Plus) (= (@ST (@Get_n /e 1)) //T_/Variable) (= (@ST (@Get_n /e 2)) //T_/Number) (member (@V (@Get_n /e 1)) /registers))
                       (let ((/e1-save /e1))
                        (set! /e1 (@CP_Get /new/L (list (@V (@Get_n /e 1)))))
                        (cond
                         ((and (not (null? /e1)) (= (@ST /e1) //T_/Variable) (member (@V /e1) /reg_inits))
                          ; Incremented module return 
                          (@To /posn_n)
                          (@Paste_Over (@Make //T_/Call (- (+ //N 1)) '()))
                          (set! /done 1)))
                        (set! /e1 /e1-save))))
                     (cond
                      ((= /done 1)
                       ; Done 
                      )
                      ((and (or (= (@ST /e) //T_/Variable) (= (@ST /e) //T_/Struct) (= (@ST /e) //T_/Sub_/Seg) (= (@ST /e) //T_/Rel_/Seg)) (or (not-member (@Struct_Elts /e) /return_elts) (member (@Struct_Elts /e) //Vcons) (= /seen_code 1)))
                       (@To /posn_n)
                       (set! //L (@CP_Unfold_Dispatch  /e /action /seen_code //L)))
                      ((and (>= /effort 2) (= //C/P_/In_/Preserves_/Dest 0))
                       (set! //L (@CP_Multiple_Destinations  /e //L)))
                      (#t
                       (display-list-flush "d1")
                       (set! //L (cdr //L))))
                     (set! /done 1)
                     (set! /e /__e_save))))))))
              (cond
               ((= /done 1)
                (set! /fl_flag1 1))
               (#t
                (set! /fl_flag1 0))))))))
         ; Check for reaching the start of an entry point 
         (cond
          ((and (= /done 0) (= (@Posn_n) 1) (= (@ST (@I)) //T_/Comment) (equal? (@V (@I)) " <ENTRY POINT> "))
           (@To /posn_n)
           (@Paste_Over (@Make //T_/Call (- (+ //N 1)) '()))
           (set! /done 1)))
         (@To /posn_n)
         (cond
          ((= /done 0)
           (display-list-flush "d1")
           (set! //L (cdr //L)))))
        (#t
         (set! //L (@CP_Unfold_Dispatch  /dest_value /action /seen_code //L)))))
      ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
       (set! //L (cdr //L))))
     (cond
      ((= (@ST (@I)) //T_/Call)
       (set! /n (- (@V (@I))))
       (cond
        ((null? (wsl-ref //Entries /n))
         (wsl-set! //Entries (list 0 /new/L) /n))
        (#t
         (wsl-set! //Entries (list 0 (@CP_Merge /new/L (wsl-ref (wsl-ref //Entries /n) 2))) /n)))))
     (set! /n /n-save)
     (set! /new/L /new/L-save)
     (set! /done /done-save)))
   (#t
    (set! //L (@CP_Clobber  //L))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Unfold this dispatch and add the value to the list of unfolded dispatches 
; unless it has already been unfolded (to prevent looping) 
; seen_code = 1 when destination is assigned from a variable and then 
; a register is assigned a dest code followed by the CALL dispatch. 
(define (@CP_Unfold_Dispatch /val-par /action-par /seen_code //L-par)
 (let ((//L-save //L)
       (/action-save /action)
       (/val-save /val)
       (funct-result '()))
  (set! //L //L-par)
  (set! /action /action-par)
  (set! /val /val-par)
  ; Convert a hex value to numeric, ignoring the top bits: 
  (cond
   ((and (= (@ST /val) //T_/String) (@Starts_With? (@V /val) "hex 0x"))
    (set! /val (substr (@V /val) 6))
    (cond
     ((> (string-length /val) 7)
      (set! /val (substr /val (- (string-length /val) 7)))))
    (set! /val (@Make //T_/Number (@Hex_To_Num /val) '())))
   ((and (= (@ST /val) //T_/Negate) (= (@ST (@Get_n /val 1)) //T_/Number))
    (set! /val (@Make //T_/Number (- (@V (@Get_n /val 1))) '()))))
  (cond
   ((= //Unfold_/Dispatch 0)
    ; dispatch unfolding is prohibited 
    (display-list-flush "d2")
    (set! //L (cdr //L)))
   ((or (= (@ST /val) //T_/Number) (and (= (@ST /val) //T_/Negate) (= (@ST (@Get_n /val 1)) //T_/Number)))
    ; Known numeric destination 
    (cond
     ((@CP_Cycle? (@Posn) 20 3)
      (display-list "Posn cycle detected: " (@Posn))
      (let ((/posn (@Posn)))
       ; Clear out any other dispatch calls in this action: 
       ; (desperate times...) 
       (@Goto '())
       (@Foreach_Statement /foreach-constant_propagation-17 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))
       (@Goto /posn)))
     (#t
      (display-list-flush "D")
      (cond
       ((not (= (@V /val) 0))
        (@CP_Clobber_Value (@V /val) (@Posn))))
      (display-list " ")
      (display-list "Unfolding a dispatch: destination=" (@V /val) " posn=" (@Posn))
      (@Splice_Over (@Cs (wsl-ref //Bodies /dispatch)))
      (set! //L (@CP_Put0 //L (list /destination) /val))
      ; Simplify the dispatch before recursively processing the result. 
      ; This helps @C_Clobber_Value to find more values. 
      (@Foreach_Expn /foreach-constant_propagation-18 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (cond
       (#t
        (@Trans //T/R_/Simplify "")))
      (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) (list /destination)) 1 2)
      ; Check for a cycle: 
      (cond
       ((and (@Left?) (@Right?) (= (@ST (@I)) //T_/Proc_/Call) (@Equal? (@I) (@Get_n (@Parent) (- (@Posn_n) 1))) (= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Call) (equal? (@V (@Get_n (@Parent) (+ (@Posn_n) 1))) (- /dispatch)))
        (display-list "dispatch unfolding cycle detected: " (@Posn))
        (@Right)
        (@Paste_Over (@Make //T_/Comment "FIXME: dispatch cycle detected!" '()))
        (@Paste_After (@Make //T_/Call (- (+ //N 1)) '()))))
      (set! //L (@CP_Statement  //L)))))
   ((or (not (null? (intersection-n /reg_inits (@Variables /val)))) (@CP_Reg_Init_Var? /val) (member (@Make_Name "NOT_USED") (@Variables /val)))
    ; A dispatch to the return address initially given in r11, r13 or r14 
    ; ie a return from the whole program! 
    ; Check for a return code in case this is a subroutine table call 
    (let ((/done-save /done))
     (set! /done 0)
     (let ((/-result- (@CP_Scan_For_Return_Code  /val /reg /done)))
      (set! /reg (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /done (car /-result-)) (set! /-result- (cdr /-result-)))
     (cond
      ((= /done 0)
       ; No return code: convert to CALL Z 
       (display-list "R")
       (display-list-flush "Unfolding a dispatch: destination = ")
       (@PP_Item /val 80 "")
       (display-list " posn=" (@Posn))
       (@Paste_Over (@Make //T_/Call (- (+ //N 1)) '()))))
     (set! /done /done-save)))
   ((@Is_Addr? /val)
    ; destination is an (external) address 
    (let ((/done-save /done))
     (set! /done 0)
     (let ((/-result- (@CP_Scan_For_Return_Code  /val /reg /done)))
      (set! /reg (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /done (car /-result-)) (set! /-result- (cdr /-result-)))
     ; If there is no return code assignment, assume that 
     ; the proc call *doesn't* return: 
     (cond
      ((= /done 0)
       (@Paste_Over (@Make //T_/Call (- (+ //N 1)) '()))
       (@CP_Make_Proc_Call /val (@Make //T_/Number 0 '()))
       (display-list-flush "d8")))
     (set! /done /done-save)))
   ((and (or (= (@ST /val) //T_/Variable) (= (@ST /val) //T_/Struct) (= (@ST /val) //T_/Sub_/Seg) (= (@ST /val) //T_/Rel_/Seg)) (not-member (@Struct_Elts /val) /return_elts) (or (>= /effort 2) (member (@Struct_Elts /val) //Constants) (member (@Struct_Elts /val) //Vcons)))
    ; destination is a symbolic constant -- scan for a return address 
    ; (or a return code is assigned after the assignment to dispatch) 
    (let ((/done-save /done))
     (set! /done 0)
     (let ((/-result- (@CP_Scan_For_Return_Code  /val /reg /done)))
      (set! /reg (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /done (car /-result-)) (set! /-result- (cdr /-result-)))
     ; If there is no return code assignment, assume that 
     ; the proc call *doesn't* return (eg a BR or BNZR): 
     (cond
      ((and (= /done 0) (not (and (= (@ST /val) //T_/Variable) (@Ends_With? (@V /val) "_RETURN"))))
       ; Replace CALL dispatch by CALL Z and add proc call 
       (@Paste_Over (@Make //T_/Call (- (+ //N 1)) '()))
       (@CP_Make_Proc_Call /val (@Make //T_/Number 0 '()))
       (display-list-flush "d7")))
     (set! /done /done-save)))
   ((or (= (@ST /val) //T_/Variable) (= (@ST /val) //T_/Struct) (= (@ST /val) //T_/Sub_/Seg) (= (@ST /val) //T_/Rel_/Seg))
    (let ((/done-save /done))
     (set! /done 0)
     ; val is a variable, but not a constant. 
     (let ((/-result- (@CP_Scan_For_Return_Code  /val /reg /done)))
      (set! /reg (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /done (car /-result-)) (set! /-result- (cdr /-result-)))
     (cond
      ((= /done 0)
       ; Check if all assignments to this variable are dispatch codes 
       ; or constants: if so, then we know the list of possible targets! 
       ; NB: Only do this on the later passes: effort >= 2 
       (cond
        ((and (>= /effort 2) (= //C/P_/In_/Preserves_/Dest 0))
         (set! //L (@CP_Multiple_Destinations  /val //L)))
        ((and (< /effort 2) (not (or (member (@Struct_Elts /val) //Constants) (member (@Struct_Elts /val) //Vcons))))
         ; Leave this for now 
        )
        ((or (member //T_/Times (@Spec_Types /val)) (not-member (@Struct_Elts /val) /return_elts))
         ; dispatch on a complex expression is a call. 
         ; Assume that any dispatch on a non-return register 
         ; is an external call and not a return: 
         (@CP_Make_Proc_Call /val (@Make //T_/Number 0 '()))
         (display-list-flush "d8")))))
     (set! /done /done-save)))
   (#t
    ; val is neither symbolic nor a number 
    (display-list-flush "d5=")
    (@Print_WSL /val "")
    (set! //L (cdr //L))))
  (set! funct-result //L)
  (set! //L //L-save)
  (set! /action /action-save)
  (set! /val /val-save)
  funct-result))

(define (@CP_Scan_For_Return_Code /val-par /reg-par /done-par)
 (let ((/done-save /done)
       (/reg-save /reg)
       (/val-save /val)
       (funct-result '()))
  (set! /done /done-par)
  (set! /reg /reg-par)
  (set! /val /val-par)
  (let ((/posn_n (@Posn_n)))
   ; Don't simply check for return codes in a register: 
   ; This might pick up the return code of a *previous* call 
   ; Instead, scan backwards for the first assignment to a register. 
   ; (This will pick up a normal BAL) 
   ; If there is a return code, then the dispatch is almost certainly 
   ; an external call: so generate a call_via_ptr 
   ; (unless initial_call_budget is small). 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((not (@Left?))
      (set! /fl_flag1 1))
     (#t
      (@Left)
      (cond
       ((and (not (= (@ST (@I)) //T_/Comment)) (not (= (@ST (@I)) //T_/Assignment)))
        (set! /fl_flag1 1))
       ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (not (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) (@Make_Name "destination"))) (not (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) (@Make_Name "exit_flag"))))
        (cond
         ((not-member (@V (@Get_n (@Get_n (@I) 1) 1)) /registers)
          (set! /fl_flag1 1))
         ((and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Variable) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /reg_inits) (= (@ST /val) //T_/Variable) (member (@V /val) /reg_inits))
          ; Return code is a return from the whole program 
          ; Returning with done = 0 will cause a CALL Z to be inserted 
          (set! /done 0)
          (set! /fl_flag1 1))
         ((or (and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /dispatch_codes) (> (@V (@Get_n (@Get_n (@I) 1) 2)) 0)) (and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Variable) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /reg_inits)))
          (set! /done 1)
          ; Note that a return code was found 
          (set! /reg (@V (@Get_n (@Get_n (@I) 1) 1)))
          (@To /posn_n)
          (cond
           ((<= /initial_call_budget 200)
            (set! /done 1)
            (set! //L (cdr //L))
            ; Don't unfold dispatch: val might not be accurate 
            (set! /fl_flag1 1))
           ((or (equal? (@Variables /val) (list /a_name)) (= (@ST /val) //T_/X_/Funct_/Call))
            ; Must be an external call 
            (@CP_Make_Proc_Call /val (@Make //T_/Variable /reg '()))
            (display-list-flush "d9a")
            (set! /fl_flag1 1))
           ((or (not-member (@Struct_Elts /val) /return_elts) (member (list (@V /val)) //Vcons))
            ; Must be an external call 
            (@CP_Make_Proc_Call /val (@Make //T_/Variable /reg '()))
            (display-list-flush "d9b")
            (set! /fl_flag1 1))
           ((and (not (equal? (@V /val) (@Make_Name "r15"))) (< /effort 2))
            ; See if we can find the register's value instead 
            (set! /fl_flag1 1))
           (#t
            (@CP_Make_Proc_Call /val (@Make //T_/Variable /reg '()))
            (display-list-flush "d9c")
            (set! /fl_flag1 1))))
         (#t
          (set! /fl_flag1 1))))
       (#t
        (set! /fl_flag1 0))))))
   ; Check if val is loaded from memory (i.e. call_via_ptr or jump table) 
   (cond
    ((and (= /done 0) (= (@ST /val) //T_/Variable) (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) (@V /val)) (member /a_name (@Used (@Get_n (@Get_n (@I) 1) 2))))
     (@To /posn_n)
     (@Splice_Over (list (@Make 101 '() (list (@Make 9 (@Make_Name "call_via_ptr") '()) (@Make 10 '() (list (@Var_To_Expn /val) (@Make 207 (@Make_Name "r1") '()))) (@Make 12 '() (list (@Make 501 (@Make_Name "regs") '()) (@Make 501 (@Make_Name "result_code") '()) (@Make 501 (@Make_Name "os") '()))))) (@Make //T_/Call (- (+ //N 1)) '())))
     (set! /done 1))
    (#t
     (@To /posn_n)))
   ; Check for dispatch at end of small entry point: 
   (cond
    ((and (= /done 0) (> (gen-length (@Posn)) 1) (= (@ST (@GParent)) //T_/Guarded) (= (@ST (@Get_n (@Parent) 1)) //T_/Comment) (equal? (@V (@Get_n (@Parent) 1)) " <ENTRY POINT> ") (@Set_Subset? (@Stat_Types (@Parent)) (@Make_Set (list //T_/Skip //T_/Comment //T_/Assignment //T_/Call))))
     (@Paste_Over (@Make //T_/Call (- (+ //N 1)) '())))))
  (set! funct-result (list /reg /done))
  (set! /done /done-save)
  (set! /reg /reg-save)
  (set! /val /val-save)
  funct-result))

(define (@CP_Multiple_Destinations /v-par //L-par)
 (let ((//L-save //L)
       (/v-save /v)
       (funct-result '()))
  (set! //L //L-par)
  (set! /v /v-par)
  (let ((/posn (@Posn))
        (//O/K-save //O/K)
        (/values-save /values))
   (set! //O/K 1)
   (set! /values '())
   (@Edit)
   (@New_Program /orig_program)
   (@Foreach_Statement /foreach-constant_propagation-19 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Undo_Edit)
   (cond
    ((and (= //O/K 1) (not (null? /values)))
     (let ((/guards '())
           (/call-save /call))
      (set! /call (@Make //T_/Call (- /dispatch) '()))
      (for-in /e /values 
       (set! /guards (cons (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /v) (@Var_To_Expn /e))) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Var_To_Expn /e))))) /call)))) /guards)))
      (set! /call (@Make //T_/Call (- (+ //N 1)) '()))
      (set! /guards (cons (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list /call)))) /guards))
      (@Paste_Over (@Make //T_/Cond '() (reverse /guards)))
      (set! //L (@CP_Statement  //L))
      (set! /call /call-save))))
   #t
   (set! //O/K //O/K-save)
   (set! /values /values-save))
  (set! funct-result //L)
  (set! //L //L-save)
  (set! /v /v-save)
  funct-result))

; Make a call to the given proc pointer assuming that the given register 
; contains the return code. If val is a simple variable, then this may 
; get converted to a simple proc call 
(define (@CP_Make_Proc_Call /val-par /reg-par)
 (let ((/reg-save /reg)
       (/val-save /val))
  (set! /reg /reg-par)
  (set! /val /val-par)
  (display-list " ")
  (display-list-flush "Making a call to procedure: ")
  (@PP_Item /val 80 "")
  (display-list-flush " returning via ")
  (@PP_Item /reg 80 "")
  ; Need to cope with destination := a[r14]; r14 := code; CALL dispatch 
  (cond
   ((@Left?)
    (@Left)
    (cond
     ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST /reg) //T_/Variable) (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) (@V /reg)) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (member (@V /reg) (@Variables /val)))
      (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) (list (@V /reg))) 1 2)
      (set! /reg (@Get_n (@Get_n (@I) 1) 2))
      (@Delete))
     (#t
      (@Right)))))
  (@Splice_Before (list (@Make 101 '() (list (@Make 9 (@Make_Name "call_via_ptr") '()) (@Make 10 '() (list (@Var_To_Expn /val) (@Make 207 (@Make_Name "r1") '()))) (@Make 12 '() (list (@Make 501 (@Make_Name "regs") '()) (@Make 501 (@Make_Name "result_code") '()) (@Make 501 (@Make_Name "os") '()))))) /%const__constant_propagation__29 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Var_To_Expn /reg)))))))
  (set! //L (@CP_Statement  //L))
  (set! /reg /reg-save)
  (set! /val /val-save)))

; Search backwards for references to the given value, and delete them. 
; (We assume that, since the value has been `used' (to unfold a dispatch call) 
; it won't be `used' again: if it IS used again to unfold a dispatch call, 
; then we have an infinite loop and therefore a probable translation error). 
(define (@CP_Clobber_Value /val-par /posn)
 (let ((/val-save /val))
  (set! /val /val-par)
  (let ((/steps 10)
        (/notused-save /notused))
   (set! /notused '())
   (cond
    ((< /val 0)
     (set! /notused (@Make //T_/Variable (@Make_Name (string-append "NOTUSED__" (@String (- /val)))) '())))
    (#t
     (set! /notused (@Make //T_/Variable (@Make_Name (string-append "NOTUSED_" (@String /val))) '()))))
   (display-list-flush "-" /val "-")
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (set! /steps (- /steps 1))
     (cond
      ((= /steps 0)
       (set! /fl_flag1 1))
      (#t
       (set! /fl_flag1 0)))
     (cond
      ((= /fl_flag1 0)
       (cond
        ((@Left?)
         (@Left)
         (set! /fl_flag1 0))
        ((@Up?)
         (@Up)
         (@Up_To_Statement)
         (set! /fl_flag1 0))
        (#t
         (set! /fl_flag1 1)))
       (cond
        ((= /fl_flag1 0)
         (cond
          ((and (= (@ST (@I)) //T_/Assignment) (not (and (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (@Starts_With? (@V (@Get_n (@Get_n (@I) 1) 1)) "HANDLE_CONDITION_"))))
           (@Foreach_Expn /foreach-constant_propagation-20 0 (@AS_Type) 0)
           (cond
            ((null? (@Program))
             (@New_Program (@Skips))))
           (set! /fl_flag1 0))
          ((and (not (= (@ST (@I)) //T_/Assignment)) (not (= (@ST (@I)) //T_/Skip)) (not (= (@ST (@I)) //T_/Comment)) (not (= (@ST (@I)) //T_/A_/Proc_/Call)))
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0)))))))))
   (cond
    ((and (null? (@Posn)) (not (null? //Names)))
     ; Have reached the start of and action (num is in action) 
     ; If there is only one caller, then check it also 
     (cond
      ((= (gen-length (wsl-ref //Preds /action)) 1)
       (@Edit)
       (@New_Program (wsl-ref //Bodies (car (wsl-ref //Preds /action))))
       (@Down_Last)
       (let ((/e-save /e)
             (/calls-save /calls))
        (set! /e (@Make //T_/Number '() /val))
        (set! /calls '())
        (set! /fl_flag1 0)
        (while (= /fl_flag1 0) 
         (begin
          (let ((/__/O/K 1))
           (vector-set! /__/Match_array 1 /e)
           (set! /__/O/K (@New_Match  /%const__constant_propagation__30 (@I) /__/O/K))
           (cond
            ((= /__/O/K 1)
             (let ((/__v_save /v))
              (set! /v (vector-ref /__/Match_array 0))
              (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /notused))))))
              (set! /v /__v_save)))))
          (cond
           ((member //T_/Proc_/Call (@Stat_Types (@I)))
            (set! /fl_flag1 1))
           (#t
            (set! /fl_flag1 0)))
          (cond
           ((= /fl_flag1 0)
            (set! /calls (@Calls (@I)))
            (cond
             ((not (null? /calls))
              (cond
               ((or (> (gen-length /calls) 1) (not (equal? (car (car /calls)) (- /action))))
                (set! /fl_flag1 1))
               (#t
                (set! /fl_flag1 0))))
             (#t
              (set! /fl_flag1 0)))
            (cond
             ((= /fl_flag1 0)
              (cond
               ((not (@Left?))
                (set! /fl_flag1 1))
               (#t
                (@Left)
                (set! /fl_flag1 0)))))))))
        (set! /e /e-save)
        (set! /calls /calls-save))
       (wsl-set! //Bodies (@Program) (car (wsl-ref //Preds /action)))
       (@Undo_Edit)))))
   (@Goto /posn)
   (set! /notused /notused-save))
  (set! /val /val-save)))

; For a Cond we calculate the lists for each arm and merge them. 
; Assert the condition within the arm's body, and deny it for the remaining arms. 
(define (@CP_Cond //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((//R-save //R)
        (/orig/L //L))
   (set! //R '())
   (@Down)
   ; to first guarded 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (set! //L (@CP_Guarded  //L))
     (set! //R (@CP_MergeL //R //L))
     ; If L=<> this will not change R 
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      ((= (@ST (@Get_n (@I) 1)) //T_/True)
       ; Ignore the rest of the guards after a TRUE guard in a Cond 
       (cond
        ((@Right?)
         (@Delete_Rest)))
       (cond
        ((or (> (@Size (@Get_n (@I) 2)) 1) (not (= (@ST (@Get (@I) (list 2 1))) //T_/Skip)))
         (@Paste_After /%const__constant_propagation__31)))
       (set! /fl_flag1 1))
      (#t
       ; The rest of the guards can deny this guard's condition 
       (@Down)
       (set! /orig/L (@CP_Deny_Condition  /orig/L))
       (@Up)
       (@Right)
       (set! //L /orig/L)
       (set! /fl_flag1 0)))))
   (@Up)
   (set! //L //R)
   (set! //R //R-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; For a D_If we simply calculate the lists for each arm and merge them. 
(define (@CP_D_If //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((//R-save //R)
        (/orig/L //L))
   (set! //R '())
   (@Down)
   ; to first guarded 
   (set! //L (@CP_Guarded  //L))
   (set! //R (@CP_MergeL //R //L))
   ; If L=<> this will not change R 
   (while (@Right?) 
    (begin
     (@Right)
     (set! //L /orig/L)
     (set! //L (@CP_Guarded  //L))
     (set! //R (@CP_MergeL //R //L))
     ; If L=<> this will not change R 
    ))
   (@Up)
   (set! //L //R)
   (set! //R //R-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; A D_Do is equivalent to WHILE ... DO D_IF ... FI OD 
; TODO We will deal with this later 
(define (@CP_D_Do //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (set! //L (@CP_Clobber  //L))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; For an EXIT(n) we merge the 0th level list with the nth level list 
; and delete the 0th level list 
(define (@CP_Exit //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0) (> (@V (@I)) 0))
    (set! //L (@CP_Exit_Sub (wsl-ref (wsl-ref //L 1) 2) (@V (@I)) (cdr //L)))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Merge l with the nth level list in L, returning the new list 
(define (@CP_Exit_Sub /l-par /n-par //L-par)
 (let ((//L-save //L)
       (/n-save /n)
       (/l-save /l)
       (//R-save //R)
       (funct-result '()))
  (set! //L //L-par)
  (set! /n /n-par)
  (set! /l /l-par)
  (set! //R '())
  (cond
   ((null? //L)
    (set! //R (list (list /n /l))))
   ((< (wsl-ref (wsl-ref //L 1) 1) /n)
    (set! //R (cons (car //L) (@CP_Exit_Sub /l /n (cdr //L)))))
   ((> (wsl-ref (wsl-ref //L 1) 1) /n)
    (set! //R (cons (list /n /l) //L)))
   (#t
    (set! //R (cons (list /n (@CP_Merge /l (wsl-ref (wsl-ref //L 1) 2))) (cdr //L)))))
  (set! funct-result //R)
  (set! //L //L-save)
  (set! /n /n-save)
  (set! /l /l-save)
  (set! //R //R-save)
  funct-result))

; For a loop we need to determine: (a) which known values are preserved 
; and (b) which known values are exported. Consider this loop: 
; a := 1; b := 2; 
; DO a := a+1; 
;    b := 2; 
;    IF a > n THEN c := 4; EXIT FI OD 
; Here, the value b=2 is preserved over the loop, but a=1 is not. 
; MERGE the list from the END of the loop with the list on entry to the loop, 
; to form the new entry list. If this differs from the original entry list, 
; (and the loop body is NOT improper) then restore the loop body and 
; repeat the propagation with the new input list until the result converges. 
; The output list is the decremented list from the end of the loop. 
; In the example, the output list will be: b=2, c=4 
(define (@CP_Floop //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (set! //L (@CP_First_Time_Flag  //L))
  (@Down)
  ; to loop body 
  (set! //L (@CP_Increment_L //L))
  ; If the body is improper, then it can only be executed once 
  (cond
   ((@Gen_Improper? (@I) "Reg")
    (set! //L (@CP_Statements  //L)))
   (#t
    (set! //L (@CP_Gen_Loop  1 //L))))
  (@Up)
  ; Back to loop 
  ; If we ran out of budget, then don't decrement: 
  (cond
   ((not (null? //L))
    (set! //L (@CP_Decrement_L //L))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Check for a `first time through' flag test where the flag value is known 
; on entering the loop, and is set to a different value in the loop body. 
; Eg: {m=1}; DO IF m=1 THEN ... FI; ... m:=0; ... OD 
(define (@CP_First_Time_Flag //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/n3 '()))
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__constant_propagation__32 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S4_save //S4)
            (/__n2_save /n2)
            (/__/S3_save //S3)
            (/__/S2_save //S2)
            (/__n1_save /n1)
            (/__v_save /v)
            (/__/S1_save //S1))
       (set! //S4 (vector-ref /__/Match_array 6))
       (set! /n2 (vector-ref /__/Match_array 5))
       (set! //S3 (vector-ref /__/Match_array 4))
       (set! //S2 (vector-ref /__/Match_array 3))
       (set! /n1 (vector-ref /__/Match_array 2))
       (set! /v (vector-ref /__/Match_array 1))
       (set! //S1 (vector-ref /__/Match_array 0))
       (set! //S5 (@CP_Trim_Exit_Code //S4))
       (cond
        ((and (or (= (@ST /v) //T_/Variable) (= (@ST /v) //T_/Struct)) (= (@ST /n1) //T_/Number) (= (@ST /n2) //T_/Number) (@Gen_Proper? (@Make //T_/Statements '() (concat //S1 //S2)) (@AS_Type)) (not-member (@Struct_Elts /v) (@Elements (@Make //T_/Statements '() (concat (concat (concat //S1 //S2) //S3) //S5)))))
         (set! /n3 (@CP_Get (wsl-ref (wsl-ref //L 1) 2) (@Struct_Elts /v)))
         (cond
          ((and (not (null? /n3)) (= (@ST /n3) //T_/Number) (equal? (@V /n3) (@V /n1)))
           (@Splice_Over (@Cs (@Make 17 '() (append //S1 (list (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /v) (@Var_To_Expn /n1))) (@Make 17 '() //S2))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))) (@Make 133 '() (list (@Make 17 '() (append //S3 (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /n2)))))) //S4 //S1 (list (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /v) (@Var_To_Expn /n1))) (@Make 17 '() //S2))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '())))))))))))))))))
           (while (not (= (@ST (@I)) //T_/Floop)) 
            (begin
             (set! //L (@CP_Statement  //L))
             (@Right)))
           ; Ready for the main CP_Floop 
          ))))
       (set! //S4 /__/S4_save)
       (set! /n2 /__n2_save)
       (set! //S3 /__/S3_save)
       (set! //S2 /__/S2_save)
       (set! /n1 /__n1_save)
       (set! /v /__v_save)
       (set! //S1 /__/S1_save))))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Trim code before an EXIT statement in given list of statements: 
(define (@CP_Trim_Exit_Code //L-par)
 (let ((//L-save //L)
       (//R-save //R)
       (funct-result '()))
  (set! //L //L-par)
  (set! //R '())
  (cond
   ((not (null? //L))
    (@Edit)
    (@New_Program (@Make //T_/Statements '() //L))
    (@Foreach_Stats /foreach-constant_propagation-21 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! //R (@Cs (@Program)))
    (@Undo_Edit)))
  (set! funct-result //R)
  (set! //L //L-save)
  (set! //R //R-save)
  funct-result))

; Generic loop processing, iterate the propagation until it converges 
; This proc is called with the loop body selected. 
; min is the minimum number of iterations to process before merging starts. 
(define (@CP_Gen_Loop /min //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/runs 0)
        (/entry/L '())
        (/end/L '())
        (/init/L '())
        (/orig '())
        (/saved/L2 '())
        (/posn (@Posn)))
   (cond
    ((not (null? //L))
     (@Edit)
     ; The higher levels of L are computed from a merged L[1] 
     ; plus the *original* values of L[2..] 
     ; At the end of the loop, we are only interested in L[1][2] 
     ; (and only then if L[1][1] = 0). 
     (set! /entry/L (wsl-ref (wsl-ref //L 1) 2))
     (set! /saved/L2 (@Final_Seg //L 2))
     (set! /init/L (wsl-ref (wsl-ref //L 1) 2))
     (set! /orig (@I))
     ; may need to restore the item and re-run the propagation 
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (@CP_Save_State)
       (set! //L (@CP_Statements  //L))
       (@CP_Restore_State)
       (cond
        ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
         (set! /fl_flag1 1))
        (#t
         ; Check the new entry list against the previous one: 
         (set! /end/L (@CP_Merge /entry/L (wsl-ref (wsl-ref //L 1) 2)))
         (cond
          ((equal? /end/L /entry/L)
           (set! /fl_flag1 1))
          (#t
           ; Some info has been lost/gained by the loop body, 
           ; so restore the body and repeat the propagation 
           (set! /runs (+ /runs 1))
           (cond
            ((> /runs 20)
             (display-list "Too many loop scans demanded!!!")
             (@Paste_Over /orig)
             (set! //L (@CP_Clobber_Broke  //L))
             (@Foreach_Statement /foreach-constant_propagation-22 0 (@AS_Type) 0)
             (cond
              ((null? (@Program))
               (@New_Program (@Skips))))
             (set! /fl_flag1 1))
            ((and (> //State_/Saves (quotient /initial_call_budget 10)) (> /initial_call_budget 200))
             (display-list-flush "#")
             (@Paste_Over /orig)
             (set! //L (@CP_Clobber_Broke  //L))
             (@Foreach_Statement /foreach-constant_propagation-23 0 (@AS_Type) 0)
             (cond
              ((null? (@Program))
               (@New_Program (@Skips))))
             (set! /fl_flag1 1))
            (#t
             (set! //L (cons (list 0 /end/L) /saved/L2))
             (set! /entry/L /end/L)
             (@Paste_Over /orig)
             (set! /fl_flag1 0)))))))))
     (cond
      ((= /min 0)
       (wsl-set! //L (@CP_Merge /init/L (wsl-ref (wsl-ref //L 1) 2)) 1 2)))
     (@End_Edit))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Decrement a list of value lists, throwing away the 0th level list 
(define (@CP_Decrement_L //L)
 
 (if (null? //L) '() (if (= (wsl-ref (wsl-ref //L 1) 1) 0) (@CP_Decrement_L (cdr //L)) (cons (list (- (wsl-ref (wsl-ref //L 1) 1) 1) (wsl-ref (wsl-ref //L 1) 2)) (@CP_Decrement_L (cdr //L))))))

; Increment all but the zero element in a list of value lists 
(define (@CP_Increment_L //L)
 
 (if (null? //L) '() (if (= (wsl-ref (wsl-ref //L 1) 1) 0) (cons (wsl-ref //L 1) (@CP_Increment_L (cdr //L))) (cons (list (+ (wsl-ref (wsl-ref //L 1) 1) 1) (wsl-ref (wsl-ref //L 1) 2)) (@CP_Increment_L (cdr //L))))))

; For the FOR loop we also have to repeat to convergance. 
; Within the body of the loop, the loop variable should be constant. 
(define (@CP_For //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/i-save /i))
   (set! /i (@V (@Get_n (@I) 1)))
   (let ((//Constants-save //Constants))
    (set! //Constants (@Set_Difference //Constants (list (list /i))))
    ; If i is really constant in the body, it can be added to the list 
    (cond
     ((not (member /i (@Assigned (@Get_n (@I) 5))))
      (set! //Constants (union-n //Constants (list (list /i))))))
    (@Down_To 2)
    ; to the `start' expression 
    (set! //L (@CP_Update  //L))
    (@Right)
    ; to the `finish' expression 
    (set! //L (@CP_Update  //L))
    (@Right)
    ; to the `step' expression 
    (set! //L (@CP_Update  //L))
    (@Right)
    ; to loop body 
    ; Delete any known value for the loop variable 
    (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) (list /i)) 1 2)
    (set! //L (@CP_Gen_Loop  0 //L))
    (@Up)
    ; Back to loop 
    
    (set! //Constants //Constants-save))
   (set! /i /i-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@CP_For_In //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/i-save /i))
   (set! /i (@V (@Get_n (@I) 1)))
   (let ((//Constants-save //Constants))
    (set! //Constants (@Set_Difference //Constants (list (list /i))))
    ; If i is really constant in the body, it can be added to the list 
    (cond
     ((not (member /i (@Assigned (@Get_n (@I) 3))))
      (set! //Constants (union-n //Constants (list (list /i))))))
    (@Down_To 2)
    ; to the expression 
    (set! //L (@CP_Update  //L))
    (@Right)
    ; to loop body 
    ; Delete any known value for the loop variable 
    (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) (list /i)) 1 2)
    (set! //L (@CP_Gen_Loop  0 //L))
    (@Up)
    ; Back to loop 
    
    (set! //Constants //Constants-save))
   (set! /i /i-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Technically, we can assume anything we like after an ABORT. 
; There might be a small gain (fewer edits) if we treat is as  
; a guard which is not entered. If we merge the result of an ABORT with 
; any known result, then we will not lose any information. 
; Also NB: we use ABORT in Fix_Dispatch to replace CALL Z in potential 
; procedure bodies (to determine if the return register is sent to dispatch) 
; So we MUST clear L after each ABORT, since we don't need to preserve 
; the destination if the potential procedure body calls Z 
(define (@CP_Abort //L)
 (cond
  ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
   (set! //L (cdr //L))))
 //L)

; Proc_Call: 
; If the call is not already in Call_Path (ie not a recursive call) 
; then update L by processing the summary (if it is in Proc_Summaries) 
; Otherwise, clobber everything. 
; Summary is: <body, val_pars, var_pars, e, v, i, assigns...> 
; If reg_stack or call_stack are pushed or popped in the proc, then it appears 
; in the v list of modified elements 
(define (@CP_Proc_Call //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (cond
   ((or (null? //L) (not (= (wsl-ref (wsl-ref //L 1) 1) 0)))
    #t)
   ((member (@V (@Get_n (@I) 1)) //Call_/Path)
    (display-list "RECURSIVE CALL!"))
   (#t
    (cond
     ((= (gen-length //Call_/Path) 0)
      (set! //State_/Saves 0)
      (set! /call_budget (quotient /initial_call_budget 2))))
    (set! //Call_/Path (cons (@V (@Get_n (@I) 1)) //Call_/Path))
    ; A proc call can only affect the zero level of L 
    (let ((/summ (@S_Get_Proc_Summary (@V (@Get_n (@I) 1)) //Proc_/Summaries))
          (/old/L-save /old/L)
          (/rest (cdr //L)))
     (set! /old/L (wsl-ref (wsl-ref //L 1) 2))
     (set! //L (list (car //L)))
     (cond
      ((null? /summ)
       (display-list "BODY NOT FOUND for proc: " (@N_String (@V (@Get_n (@I) 1))))
       (set! //L (@CP_Clobber  //L)))
      ((and (not (@Cs? (@Get_n (@I) 2))) (not (@Cs? (@Get_n (@I) 3))) (or (member (list /reg_stack) (wsl-ref /summ 5)) (member (list /call_stack) (wsl-ref /summ 5))) (< //State_/Saves (quotient /initial_call_budget 2)) (> /call_budget (quotient /initial_call_budget 5)))
       ; Process the body, not the summary, so that register 
       ; save and restore calls can be handled 
       (set! //L (@CP_Parameterless_Proc_Body  (wsl-ref /summ 1) //L)))
      ((member /os_name (@Assigned (@Get_n (@I) 3)))
       (set! //L (@CP_Parameterless_Proc_Body  (wsl-ref /summ 1) //L)))
      (#t
       ; Summarise the call (to deal with any parameters): 
       (set! //L (@CP_Summary  (@Summarise (@I)) //L))
       (cond
        ((member //T_/A_/Proc_/Call (@Stat_Types (wsl-ref /summ 1)))
         (cond
          ((and (not (null? //A_/Proc_/Call_/Filter)) (member /os_name (@Variables (wsl-ref /summ 1))))
           (wsl-set! //L (@CP_All_But (wsl-ref (wsl-ref //L 1) 2) //A_/Proc_/Call_/Filter) 1 2))
          (#t
           (wsl-set! //L (@CP_Clobber_Zeros (wsl-ref (wsl-ref //L 1) 2)) 1 2)))))))
     ; L might be clobbered by an exit_flag := 1 in the body. 
     ; So resore it here, but also set destination so that 
     ; a subsequent CALL dispatch will become CALL Z: 
     (cond
      ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
       (set! //L (list (list 0 /old/L)))))
     (set! //L (cons (car //L) /rest))
     (set! /old/L /old/L-save))
    (set! //Call_/Path (cdr //Call_/Path))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Do Constant Propagation over a summary: 
; If we are not in a @Preserves_Destination call and the vars include 
; S_Tail_Recursive_Call, then don't use the assigns. 
; TAIL(summ[1]) is <<e1, e2,...>, <v1, v2, ...>, <i1, i2, ...>, <x1, y1>, ...> 
; If os_name is clobbered in the summary, then there must be a !P call 
; so call @CP_Clobber_Zeros on the resulting list 
(define (@CP_Summary /summ //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/clobber '())
        (/pair-save /pair)
        (/old/L-save /old/L)
        (/new/L-save /new/L)
        (/val-save /val))
   (set! /pair '())
   (set! /old/L '())
   (set! /new/L '())
   (set! /val '())
   (cond
    ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0) (not (null? /summ)) (= (wsl-ref (wsl-ref /summ 1) 1) 0))
     (set! /summ (cdr (wsl-ref /summ 1)))
     (set! /clobber (@Elt_Subtract (@Set_Difference (wsl-ref /summ 2) (wsl-ref /summ 3)) (@Make_Set (my-map HEAD (@Final_Seg /summ 4)))))
     ; If summ[3] is not empty then *something* is incremented 
     ; but we cannot guarantee that it is a dispatch code! 
     ; In particular, r15 is probably a return value. 
     (wsl-set! /summ (@Elt_Subtract (wsl-ref /summ 3) (list (list (@Make_Name "r15")))) 3)
     (cond
      ((not (null? (wsl-ref /summ 3)))
       (display-list "(some kind of inc)")
       (set! //C/P_/Return_/Code_/Inc (union-n //C/P_/Return_/Code_/Inc (list 1)))))
     (set! /old/L (wsl-ref (wsl-ref //L 1) 2))
     (set! /new/L (@CP_Clobber_List (wsl-ref (wsl-ref //L 1) 2) /clobber))
     (cond
      ((and (= //C/P_/In_/Preserves_/Dest 0) (member (list //S_/Tail_/Recursive_/Call) (wsl-ref /summ 2)))
       ; The assigns are only valid when exit_flag = 0 
       ; so only record copies of dest codes (clobber other vars): 
       (for-in /pair (@Final_Seg /summ 4) 
        (begin
         (cond
          ((sequence? (wsl-ref /pair 2))
           (set! /val (@CP_Get /old/L (wsl-ref /pair 2)))
           (cond
            ((null? /val)
             (set! /val (@Name_To_WSL (wsl-ref /pair 2))))))
          (#t
           (set! /val (@Name_To_WSL (wsl-ref /pair 2)))))
         (cond
          ((and (= (@ST /val) //T_/Number) (> (@V /val) 0) (member (@V /val) /dispatch_codes))
           (set! /new/L (@CP_Put /new/L (wsl-ref /pair 1) /val)))
          ((and (= (@ST /val) //T_/Variable) (member (@V /val) /reg_inits))
           (set! /new/L (@CP_Put /new/L (wsl-ref /pair 1) /val)))
          (#t
           (set! /new/L (@CP_Remove /new/L (wsl-ref /pair 1)))))))
       (cond
        (#f
         ; clobber the assigned variables: 
         (set! /new/L (@CP_Clobber_List /new/L (my-map HEAD (@Final_Seg /summ 4)))))))
      (#t
       (for-in /pair (@Final_Seg /summ 4) 
        (begin
         ; If a var is copied from another var, check if the source var 
         ; originally had a value. If so, then copy that value: 
         (cond
          ((sequence? (wsl-ref /pair 2))
           (set! /val (@CP_Get /old/L (wsl-ref /pair 2)))
           (cond
            ((null? /val)
             (set! /val (@Name_To_WSL (wsl-ref /pair 2))))))
          (#t
           (set! /val (@Name_To_WSL (wsl-ref /pair 2)))))
         ; If the value is a variable which is clobbered, 
         ; then don't record it! 
         (let ((/keep-save /keep))
          (set! /keep 1)
          (cond
           ((sequence? (wsl-ref /pair 2))
            (for-in /elt (concat /clobber (wsl-ref /summ 3)) 
             (cond
              ((equal? /elt (list /a_name))
               #t)
              ((@Either_Prefix? (wsl-ref /pair 2) /elt)
               (set! /keep 0))))))
          (cond
           ((= /keep 0)
            (set! /new/L (@CP_Remove /new/L (wsl-ref /pair 1))))
           (#t
            (set! /new/L (@CP_Put /new/L (wsl-ref /pair 1) /val))))
          (set! /keep /keep-save))))))
     (cond
      ((member (list /os_name) (wsl-ref /summ 2))
       (set! /new/L (@CP_Clobber_Zeros /new/L))))
     (wsl-set! //L /new/L 1 2)))
   (set! /pair /pair-save)
   (set! /old/L /old/L-save)
   (set! /new/L /new/L-save)
   (set! /val /val-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; Process a parameterless procedure body: 
; If all the terminal statements are also parameterless proc bodies, 
; then replace them by SKIPs, process the body, then process each call 
; and merge the results. 
; (This saves processing a body multiple times when there are several calls to it). 
(define (@CP_Parameterless_Proc_Body /body //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (set! /calls_processed (+ /calls_processed 1))
  (cond
   ((< /calls_processed /call_budget)
    (let ((/cb_save /call_budget))
     (set! /call_depth (+ /call_depth 1))
     (cond
      ((and (> /call_depth 2) (even? /call_depth))
       (set! /call_budget (quotient /call_budget 2))))
     (@Edit)
     (@New_Program (@Get_n /body 4))
     (let ((//O/K-save //O/K)
           (/calls-save /calls)
           (/move-save /move))
      (set! //O/K 1)
      (set! /calls '())
      (set! /move 0)
      (@Ateach_Terminal /foreach-constant_propagation-24 0 (@AS_Type) 1)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (cond
       ((and #f (= //O/K 1))
        (set! //L (@CP_Statements  //L))
        (display-list-flush (gen-length /calls))
        ; Process the calls and merge the results 
        (cond
         ((null? /calls)
          #t)
         ((not (null? (intersection-n /calls (@Make_Set //Call_/Path))))
          ; There is a recursive call, so clobber 
          (set! //L (@CP_Clobber_Broke  //L)))
         (#t
          (let ((//R-save //R)
                (/orig/L //L)
                (/name-save /name)
                (/body '())
                (/comps (list (@Make //T_/Expressions '() '()) (@Make //T_/Lvalues '() '()))))
           (set! //R '())
           (set! /name '())
           (for-in /name /calls 
            (begin
             (@New_Program (@Make //T_/Proc_/Call '() (cons (@Name /name) /comps)))
             (set! //L (@CP_Proc_Call  //L))
             (set! //R (@CP_MergeL //R //L))
             (set! //L /orig/L)))
           (set! //L //R)
           (set! //R //R-save)
           (set! /name /name-save)))))
       (#t
        ; Process the original body as normal 
        (display-list-flush "N")
        (@New_Program (@Get_n /body 4))
        (set! //L (@CP_Statements  //L))))
      (set! //O/K //O/K-save)
      (set! /calls /calls-save)
      (set! /move /move-save))
     (@Undo_Edit)
     (set! /call_budget /cb_save)
     (set! /call_depth (- /call_depth 1))))
   (#t
    (display-list-flush "-")
    (set! //L (@CP_Clobber_Broke  //L))))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; To process the procedure body we need to: 
; (1) Save values of shadowed globals (formal value parameters) 
; (2) Update L with the effect of assigning actual values to all formal parameters 
; (3) Propagate over the body (can do updates since this is a temp copy) 
; (4) Restore values of shadowed globals (formal value parameters) 
; (5) Update L with the effect of assigning formal var parameters 
;     to actual var parameters. 
(define (@CP_Proc_Body /body //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/v-save /v)
        (/c '())
        (/l-save /l)
        (/save '())
        (/vars-save /vars)
        (//A_vals (@Cs (@Get_n (@I) 1)))
        (//A_vars (@Cs (@Get_n (@I) 2)))
        (//F_vals (@Cs (@Get_n /body 1)))
        (//F_vars (@Cs (@Get_n /body 2))))
   (set! /v '())
   (set! /l (wsl-ref (wsl-ref //L 1) 2))
   (set! /vars '())
   ; We need to (temporarily) update the Constants list 
   ; by removing the (shadowed) globals 
   (let ((//Constants-save //Constants))
    (set! //Constants (@Set_Difference //Constants (@Make_Set (my-map @Struct_Elts (concat //F_vals //F_vars)))))
    ; Save the values of shadowed globals (formal value parameters) 
    (set! /vars (my-map @V //F_vals))
    (while (not (null? /vars)) 
     (begin
      (set! /v (car /vars))
      (set! /save (cons (list /v (@CP_Get /l /v)) /save))
      (set! /vars (cdr /vars))))
    ; Update L with the effect of F_vals := A_vals and F_vars := A_vars 
    (set! //L (@CP_Do_Assigns  (concat //F_vals //F_vars) (concat //A_vals //A_vars) //L))
    ; Process the body 
    (@Edit)
    (@New_Program (@Get_n /body 3))
    (set! //L (@CP_Statements  //L))
    (@Undo_Edit)
    ; delete formal var parameters from l and restore values of any shadowed globals: 
    (cond
     ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
      (set! /l (wsl-ref (wsl-ref //L 1) 2))
      (while (not (null? /save)) 
       (begin
        (set! /v (wsl-ref (wsl-ref /save 1) 1))
        (set! /c (wsl-ref (wsl-ref /save 1) 2))
        (cond
         ((null? /c)
          (set! /l (@CP_Remove /l /v)))
         (#t
          (set! /l (@CP_Put /l /v /c))))
        (set! /save (cdr /save))))
      (wsl-set! //L /l 1 2)
      ; Update L with the effect of assigning formal vars to actual vars 
      (set! //L (@CP_Do_Assigns  //F_vars //A_vars //L))))
    (set! //Constants //Constants-save))
   (set! /v /v-save)
   (set! /l /l-save)
   (set! /vars /vars-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; This proc updates L with the effect of the (multiple) assignment vars := vals 
(define (@CP_Do_Assigns /vars-par /vals //L-par)
 (let ((//L-save //L)
       (/vars-save /vars)
       (funct-result '()))
  (set! //L //L-par)
  (set! /vars /vars-par)
  (cond
   ((not (null? /vars))
    (let ((//A '()))
     (while (not (null? /vars)) 
      (begin
       (set! //A (cons (@Make //T_/Assign '() (list (car /vars) (car /vals))) //A))
       (set! /vars (cdr /vars))
       (set! /vals (cdr /vals))))
     (@Edit)
     (@New_Program (@Make //T_/Assigns '() //A))
     (set! //L (@CP_Assigns  //L))
     (@Undo_Edit))))
  (set! funct-result //L)
  (set! //L //L-save)
  (set! /vars /vars-save)
  funct-result))

; For local variables, we need to save all globals with the same name as a local. 
; Then apply the assigns, then apply the body, then restore the shadowed globals. 
; With VAR <x := x>: ... ENDVAR, a known initial value of x 
; will be propagated inside and restored again at the end. 
(define (@CP_Var //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/v-save /v)
        (/c '())
        (/l-save /l)
        (/save '())
        (/vars-save /vars))
   (set! /v '())
   (set! /l (wsl-ref (wsl-ref //L 1) 2))
   (set! /vars (@Make_Set (my-map @Struct_Elts (@Cs (@Get_n (@I) 1)))))
   ; We need to (temporarily) update the Constants list with the new variables: 
   ; First remove the (shadowed) globals, then add any constant locals. 
   (let ((//Constants-save //Constants))
    (set! //Constants (union-n (@Set_Difference //Constants /vars) (@Set_Difference /vars (@Elts_Assigned (@Get_n (@I) 2)))))
    ; Save the values of the `shadowed' globals with known values 
    (while (not (null? /vars)) 
     (begin
      (set! /v (car /vars))
      (set! /save (cons (list /v (@CP_Get /l /v)) /save))
      (set! /vars (cdr /vars))))
    (@Down)
    ; to the assigns 
    ; NB Don't delete any of the assigns, just update L 
    (@Edit)
    (set! //L (@CP_Assigns  //L))
    (@Undo_Edit)
    (@Right)
    ; to the body 
    (set! //L (@CP_Statements  //L))
    (@Up)
    ; back to var structure 
    ; delete locals from l and restore values of any shadowed globals: 
    (cond
     ((and (not (null? //L)) (= (wsl-ref (wsl-ref //L 1) 1) 0))
      (set! /l (wsl-ref (wsl-ref //L 1) 2))
      (while (not (null? /save)) 
       (begin
        (set! /v (wsl-ref (wsl-ref /save 1) 1))
        (set! /c (wsl-ref (wsl-ref /save 1) 2))
        (cond
         ((null? /c)
          (set! /l (@CP_Remove /l /v)))
         (#t
          (set! /l (@CP_Put /l /v /c))))
        (set! /save (cdr /save))))
      ; Remove entries where the value uses a local variable 
      (set! /vars (@Make_Set (my-map @Struct_Elts (@Cs (@Get_n (@I) 1)))))
      (for-in /pair /l 
       (cond
        ((not (@Elt_Clash_List? (@Elements (wsl-ref /pair 2)) /vars))
         (set! /save (cons /pair /save)))))
      (wsl-set! //L (reverse /save) 1 2)))
    (set! //Constants //Constants-save))
   (set! /v /v-save)
   (set! /l /l-save)
   (set! /vars /vars-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(define (@Get_Lvar_Name //I)
 
 (@CP_Var_Name (@Get_n //I 1)))

; Update Proc_Summaries with a new list of bodies while processing the WHERE body 
(define (@CP_Where //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (set! //Proc_/Summaries (cons (hash-table) //Proc_/Summaries))
  (display-list-flush "Summarising proc definitions... ")
  (@Summarise_Where_Defns (@Cs (@Get_n (@I) 2)))
  (display-list "Done.")
  ; Process each procedure body with an empty L and a small budget: 
  (@Down_To 2)
  ; to defns 
  (@Down)
  ; to first defn 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (cond
     ((= (@ST (@I)) //T_/Proc)
      (@Down_To 4)
      ; to body 
      (let ((//L-save //L)
            (/call_budget-save /call_budget)
            (/initial_call_budget-save /initial_call_budget))
       (set! //L (list (list 0 '())))
       (set! /call_budget 800)
       (set! /initial_call_budget 800)
       (display-list-flush "p")
       (set! //L (@CP_Statements  //L))
       (set! //L //L-save)
       (set! /call_budget /call_budget-save)
       (set! /initial_call_budget /initial_call_budget-save))
      (@Up)))
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@Up)
  (@Up)
  ; back to WHERE clause 
  (display-list "")
  ; We want to have a full budget available for the WHERE body: 
  (set! //State_/Saves 0)
  (@Down)
  (set! //L (@CP_Statements  //L))
  (@Up)
  (display-list "")
  (set! //Proc_/Summaries (cdr //Proc_/Summaries))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

; WHILE is similar to FOR (check for null loops and infinite loops) 
(define (@CP_While //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/min 0))
   (@Down)
   ; to condition 
   ; Don't actually edit the condition until the loop body has been processed 
   ; (unless the result is FALSE) 
   (@Edit)
   (set! //L (@CP_Update  //L))
   (@Trans //T/R_/Simplify "")
   (cond
    ((= (@ST (@I)) //T_/False)
     (@End_Edit)
     ; Loop body will not be entered, treat as a SKIP 
    )
    (#t
     ; If the updated loop test is TRUE, the the minumum number 
     ; of iterations for Gen_Loop is at least one: 
     (cond
      ((= (@ST (@I)) //T_/True)
       (set! /min 1)))
     (@Undo_Edit)
     ; Restore loop test, since the body might affect it! 
     ; If the non-updated loop test is TRUE then the loop never terminates 
     (cond
      ((= (@ST (@I)) //T_/True)
       (set! //L (@CP_Abort  //L))
       ; loop never terminates 
      )
      (#t
       ; Within the loop we can assert the terminating condition 
       (set! //L (@CP_Assert_Condition  //L))
       (@Right)
       ; to loop body 
       (set! //L (@CP_Gen_Loop  /min //L))))))
   (@Up)
   ; Back to loop 
   ; After the loop we can deny the terminating condition 
   (@Down)
   (set! //L (@CP_Update  //L))
   (set! //L (@CP_Deny_Condition  //L))
   (@Up))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

(set! //C/P_/Special_/Value 426294)
(set! //C/P_/Return_/Code_/Inc '())
(set! //C/P_/Return_/Code_/Normal 0)
(set! //C/P_/In_/Preserves_/Dest 0)
(set! //C/P_/Reg_/To_/Num (hash-table))
(let ((/n-save /n))
 (set! /n 0)
 (for-in /reg (my-map @Make_Name (list "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15")) 
  (begin
   (puthash //C/P_/Reg_/To_/Num /reg /n)
   (set! /n (+ /n 1))))
 (set! /n /n-save))
; Check that the given item sends the value of the given reg to destination: 
(define (@Preserves_Destination /name-par //I /reg-par /effort-par /call_budget-par /flag_n)
 (let ((/call_budget-save /call_budget)
       (/effort-save /effort)
       (/reg-save /reg)
       (/name-save /name)
       (//L-save //L)
       (//Entries-save //Entries)
       (//Call_/Path-save //Call_/Path)
       (//Unfold_/Dispatch-save //Unfold_/Dispatch)
       (/calls_processed-save /calls_processed)
       (/call_depth-save /call_depth)
       (//Constants-save //Constants)
       (//D/S/E/C/Ts-save //D/S/E/C/Ts)
       (//C/P_/State-save //C/P_/State)
       (/destination-save /destination)
       (/val-save /val)
       (/body '())
       (//R-save //R)
       (/initial_call_budget-save /initial_call_budget)
       (//State_/Saves-save //State_/Saves)
       (/registers-save /registers)
       (/x86_regs-save /x86_regs)
       (/cc_name-save /cc_name)
       (/zf_name-save /zf_name)
       (/cf_name-save /cf_name)
       (/r1_name-save /r1_name)
       (/push_regs-save /push_regs)
       (/pop_regs-save /pop_regs)
       (/chain_reg-save /chain_reg)
       (/reg_stack-save /reg_stack)
       (/call_stack-save /call_stack)
       (/call_via_ptr-save /call_via_ptr)
       (/call_via_ptr_pars-save /call_via_ptr_pars)
       (/pack-save /pack)
       (//E/X/E/C_/C/I/C/S-save //E/X/E/C_/C/I/C/S)
       (/true-save /true)
       (/false-save /false)
       (/dispatch-save /dispatch)
       (//A/S_/Type-save //A/S_/Type)
       (/exit_flag-save /exit_flag)
       (//Notused_/Value-save //Notused_/Value)
       (//Migration-save //Migration)
       (funct-result '()))
  (set! /call_budget /call_budget-par)
  (set! /effort /effort-par)
  (set! /reg /reg-par)
  (set! /name /name-par)
  (set! //L (list (list 0 '())))
  (set! //Entries '())
  (set! //Call_/Path '())
  (set! //Unfold_/Dispatch 0)
  (set! /calls_processed 0)
  (set! /call_depth 0)
  (set! //Constants '())
  (set! //D/S/E/C/Ts (hash-table))
  (set! //C/P_/State '())
  (set! /destination (@Make_Name "destination"))
  (set! /val '())
  (set! //R 0)
  (set! /initial_call_budget /call_budget-par)
  (set! //State_/Saves 0)
  (set! /registers (@Make_Set (my-map @Make_Name (list "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15"))))
  (set! /x86_regs (my-map @Make_Name (list "ax" "bx" "cx" "dx")))
  (set! /cc_name (@Make_Name "cc"))
  (set! /zf_name (@Make_Name "zf"))
  (set! /cf_name (@Make_Name "cf"))
  (set! /r1_name (@Make_Name "r1"))
  (set! /push_regs (@Make_Name "push_regs"))
  (set! /pop_regs (@Make_Name "pop_regs"))
  (set! /chain_reg (@Make_Name "chain_reg"))
  (set! /reg_stack (@Make_Name "reg_stack"))
  (set! /call_stack (@Make_Name "call_stack"))
  (set! /call_via_ptr (@Make_Name "call_via_ptr"))
  (set! /call_via_ptr_pars (@Make_Name "call_via_ptr_pars"))
  (set! /pack (@Make_Name "pack"))
  (set! //E/X/E/C_/C/I/C/S (@Make_Name "EXEC_CICS"))
  (set! /true (@Make //T_/True '() '()))
  (set! /false (@Make //T_/False '() '()))
  (set! /dispatch 0)
  (set! //A/S_/Type (@AS_Type))
  (set! /exit_flag (@Make_Name "exit_flag"))
  (set! //Notused_/Value (hash-table))
  (set! //Migration 1)
  (display-list "--> Checking if action " (@N_String /name) " sends " (@N_String /reg) " to destination...")
  ; First we construct a var/value list from reg, 
  ; giving it a special numeric value 
  (set! //L (list (list 0 (@CP_Put '() (list /reg) (@Make //T_/Number //C/P_/Special_/Value '())))))
  (@Edit)
  (@New_Program //I)
  (set! //D/S/E/C/Ts (@CP_Find_DSECTs  //D/S/E/C/Ts))
  (set! //L (@CP_Init_DSECT_Pointers  //D/S/E/C/Ts //L))
  (@CP_Memory_Address_Fix)
  (set! //C/P_/Return_/Code_/Inc '())
  (set! //C/P_/Return_/Code_/Normal 0)
  (set! //C/P_/In_/Preserves_/Dest /flag_n)
  (set! //L (@CP_Generic  //L))
  (set! //C/P_/In_/Preserves_/Dest 0)
  (@Undo_Edit)
  (display-list "")
  ; Check that the special numeric value is now in destination 
  (cond
   ((or (null? //L) (> (wsl-ref (wsl-ref //L 1) 1) 0))
    (set! /val '()))
   (#t
    (set! /val (@CP_Get (wsl-ref (wsl-ref //L 1) 2) (list /destination)))))
  (cond
   ((or (null? /val) (not (= (@ST /val) //T_/Number)) (not (equal? (@V /val) //C/P_/Special_/Value)))
    (display-list "<-- Failed for action " (@N_String /name) " register " (@N_String /reg))
    (set! //R 0))
   (#t
    (display-list "<-- Succeeded for action " (@N_String /name) " register " (@N_String /reg))
    (set! //R 1)
    (cond
     ((not (null? //C/P_/Return_/Code_/Inc))
      (set! //R 2)))))
  (set! funct-result //R)
  (set! /call_budget /call_budget-save)
  (set! /effort /effort-save)
  (set! /reg /reg-save)
  (set! /name /name-save)
  (set! //L //L-save)
  (set! //Entries //Entries-save)
  (set! //Call_/Path //Call_/Path-save)
  (set! //Unfold_/Dispatch //Unfold_/Dispatch-save)
  (set! /calls_processed /calls_processed-save)
  (set! /call_depth /call_depth-save)
  (set! //Constants //Constants-save)
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-save)
  (set! //C/P_/State //C/P_/State-save)
  (set! /destination /destination-save)
  (set! /val /val-save)
  (set! //R //R-save)
  (set! /initial_call_budget /initial_call_budget-save)
  (set! //State_/Saves //State_/Saves-save)
  (set! /registers /registers-save)
  (set! /x86_regs /x86_regs-save)
  (set! /cc_name /cc_name-save)
  (set! /zf_name /zf_name-save)
  (set! /cf_name /cf_name-save)
  (set! /r1_name /r1_name-save)
  (set! /push_regs /push_regs-save)
  (set! /pop_regs /pop_regs-save)
  (set! /chain_reg /chain_reg-save)
  (set! /reg_stack /reg_stack-save)
  (set! /call_stack /call_stack-save)
  (set! /call_via_ptr /call_via_ptr-save)
  (set! /call_via_ptr_pars /call_via_ptr_pars-save)
  (set! /pack /pack-save)
  (set! //E/X/E/C_/C/I/C/S //E/X/E/C_/C/I/C/S-save)
  (set! /true /true-save)
  (set! /false /false-save)
  (set! /dispatch /dispatch-save)
  (set! //A/S_/Type //A/S_/Type-save)
  (set! /exit_flag /exit_flag-save)
  (set! //Notused_/Value //Notused_/Value-save)
  (set! //Migration //Migration-save)
  funct-result))

; Find assignments of the form dsect := reg 
; If there is only one register associated with a dsect, 
; then assume that the dsect is already initialised (by adding 
; an entry to L) 
(define (@CP_Init_DSECT_Pointers //D/S/E/C/Ts-par //L-par)
 (let ((//L-save //L)
       (//D/S/E/C/Ts-save //D/S/E/C/Ts)
       (funct-result '()))
  (set! //L //L-par)
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-par)
  (let ((//D/S/E/C/T_reg-save //D/S/E/C/T_reg)
        (//D/S/E/C/T_bad-save //D/S/E/C/T_bad)
        (//D/S/E/C/T_list-save //D/S/E/C/T_list)
        (/name-save /name))
   (set! //D/S/E/C/T_reg (hash-table))
   (set! //D/S/E/C/T_bad (hash-table))
   (set! //D/S/E/C/T_list '())
   (set! /name '())
   (@Foreach_Statement /foreach-constant_propagation-25 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /name //D/S/E/C/T_list 
    (cond
     ((null? (gethash //D/S/E/C/T_bad /name))
      (set! //L (@CP_Put0 //L (list /name) (@Make //T_/Variable (gethash //D/S/E/C/T_reg /name) '()))))))
   (set! //D/S/E/C/T_reg //D/S/E/C/T_reg-save)
   (set! //D/S/E/C/T_bad //D/S/E/C/T_bad-save)
   (set! //D/S/E/C/T_list //D/S/E/C/T_list-save)
   (set! /name /name-save))
  (set! funct-result //L)
  (set! //L //L-save)
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-save)
  funct-result))

; Within Preserves_Destination, convert a[rX + n, 4] to a simple variable 
; cf: a[r1 + 72, 4] := r4; ...; r4 := a[r1 + 72, 4]; 
(define (@CP_Memory_Address_Fix)
 (let ((/new-save /new))
  (set! /new '())
  (@Foreach_Expn /foreach-constant_propagation-26 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Lvalue /foreach-constant_propagation-27 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  #t
  (set! /new /new-save)))

; Look for DSECT pointers which are assigned from a register 
; which is initialised at most once. If this is the case, we can delete 
; most assignments to the DSECT pointer. BUT: add an assignment 
; at each entry point, as well as keeping one after the assignment to the register. 
; This is in case the initial value of the register is used also. 
(define (@CP_Constant_DSECT_Pointers //D/S/E/C/Ts-par)
 (let ((//D/S/E/C/Ts-save //D/S/E/C/Ts))
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-par)
  (let ((/reg-save /reg)
        (/var-save /var)
        (/reg_init_count-save /reg_init_count)
        (/reg_/D/S/E/C/Ts-save /reg_/D/S/E/C/Ts)
        (//D/S/E/C/T_init-save //D/S/E/C/T_init)
        (//D/S/E/C/T_init_count-save //D/S/E/C/T_init_count)
        (//D/S/E/C/T_reg-save //D/S/E/C/T_reg)
        (//D/S/E/C/T_bad-save //D/S/E/C/T_bad)
        (/done-save /done)
        (//O/K-save //O/K)
        (/name-save /name)
        (/v-save /v)
        (/e-save /e)
        (/pointers-save /pointers)
        (/all_inits-save /all_inits))
   (set! /reg '())
   (set! /var '())
   (set! /reg_init_count (hash-table))
   (set! /reg_/D/S/E/C/Ts (hash-table))
   (set! //D/S/E/C/T_init (hash-table))
   (set! //D/S/E/C/T_init_count (hash-table))
   (set! //D/S/E/C/T_reg (hash-table))
   (set! //D/S/E/C/T_bad (hash-table))
   (set! /done (hash-table))
   (set! //O/K 0)
   (set! /name '())
   (set! /v '())
   (set! /e '())
   (set! /pointers '())
   (set! /all_inits (union-n /reg_inits (list (@Make_Name "___r1_init___"))))
   (for-in /reg (concat /registers /reg_inits) 
    (puthash /reg_init_count /reg 0))
   ; Check that the DSECT pointer is initialised to the same expression: 
   (@Foreach_Statement /foreach-constant_propagation-28 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /name (@Hash_Keys //D/S/E/C/Ts) 
    (cond
     ((and (null? (gethash //D/S/E/C/T_bad /name)) (not (null? (gethash //D/S/E/C/T_init /name))))
      (set! /e (gethash //D/S/E/C/T_init /name))
      (display-list-flush "DSECT_init(" (@N_String /name) ") = ")
      (@PP_Item /e 80 "")
      ; Hack for ptr := (reg + ptr) - ADDRESS_OF(a[ptr].foo.bar) 
      (cond
       ((and (= (@ST /e) //T_/Minus) (@CP_Constant? /e) (= (@ST (@Get_n /e 1)) //T_/Plus))
        (set! /e (@Get_n (@Get_n /e 1) 1))))
      (cond
       ((not (= (gen-length (@Set_Difference (@Variables /e) (list /a_name))) 1))
        (puthash //D/S/E/C/T_bad /name 1))
       (#t
        (set! /v (car (@Set_Difference (@Variables /e) (list /a_name))))
        (cond
         ((and (not-member /v /registers) (not-member /v /reg_inits))
          (puthash //D/S/E/C/T_bad /name 1))
         (#t
          (puthash //D/S/E/C/T_reg /name /v))))))))
   (@Foreach_Statement /foreach-constant_propagation-29 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; Check for registers initialised in a !P (other than pop_regs and chain_reg) 
   ; NB: which !P calls actually initialise registers? 
   (@Foreach_Statement /foreach-constant_propagation-30 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; Check if a suitable DSECT/reg pair has been found: 
   (set! //O/K 0)
   (for-in /var (@Hash_Keys //D/S/E/C/Ts) 
    (cond
     ((and (null? (gethash //D/S/E/C/T_bad /var)) (not (null? (gethash //D/S/E/C/T_reg /var))) (<= (gethash /reg_init_count (gethash //D/S/E/C/T_reg /var)) 1))
      (puthash /reg_/D/S/E/C/Ts (gethash //D/S/E/C/T_reg /var) (union-n (gethash /reg_/D/S/E/C/Ts (gethash //D/S/E/C/T_reg /var)) (list /var)))
      (display-list "Found (fairly) constant DSECT: " (@N_String /var) " reg = " (@N_String (gethash //D/S/E/C/T_reg /var)) " init count = " (gethash /reg_init_count (gethash //D/S/E/C/T_reg /var)))
      (set! //O/K 1))
     (#t
      (puthash //D/S/E/C/T_bad /var 1))))
   (cond
    ((= //O/K 0)
     (display-list "No constant DSECTs found"))
    (#t
     ; Delete all assignments to the constant DSECTs, 
     ; (these are assigned to a register value) 
     ; Insert an assignment after the (single) register initialisation: 
     (set! /pointers (@Make_Set (@Hash_Keys //D/S/E/C/Ts)))
     (@Foreach_Statement /foreach-constant_propagation-31 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     ; Set the DSECT pointer after the *last* reg assignment in the sequence. 
     ; But also check for references to the DSECT pointer before this! 
     (let ((/dsects_done-save /dsects_done))
      (set! /dsects_done '())
      (@Foreach_Stats /foreach-constant_propagation-32 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (set! /dsects_done /dsects_done-save))
     ; Add a initialisation from the initial value of the register 
     ; to the DSECT pointer, if the register is never initialised 
     ; or if the initialisation is safe (doesn't deref a pointer) 
     ; Test for EMPTY?(done.(reg_DSECTs.(DSECT_reg.(var)))) was taken out 
     ; since a called module may use the register(?). Have restored the test. 
     (for-in /var (@Sort_List (@Hash_Keys //D/S/E/C/Ts)) 
      (cond
       ((and (not (null? (gethash //D/S/E/C/T_reg /var))) (null? (gethash //D/S/E/C/T_bad /var)) (not-member /a_name (@Used (gethash //D/S/E/C/T_init /var))) (null? (gethash /done (gethash /reg_/D/S/E/C/Ts (gethash //D/S/E/C/T_reg /var)))))
        (@Ateach_Statement /foreach-constant_propagation-33 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips)))))))
     (display-list "")))
   (set! /reg /reg-save)
   (set! /var /var-save)
   (set! /reg_init_count /reg_init_count-save)
   (set! /reg_/D/S/E/C/Ts /reg_/D/S/E/C/Ts-save)
   (set! //D/S/E/C/T_init //D/S/E/C/T_init-save)
   (set! //D/S/E/C/T_init_count //D/S/E/C/T_init_count-save)
   (set! //D/S/E/C/T_reg //D/S/E/C/T_reg-save)
   (set! //D/S/E/C/T_bad //D/S/E/C/T_bad-save)
   (set! /done /done-save)
   (set! //O/K //O/K-save)
   (set! /name /name-save)
   (set! /v /v-save)
   (set! /e /e-save)
   (set! /pointers /pointers-save)
   (set! /all_inits /all_inits-save))
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-save)))

; Check for a cycle: copies+ occurrences of a subsequence of len+ values 
(define (@CP_Cycle? /posn /copies /len)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K 0)
  (cond
   ((>= (gen-length /posn) (* /copies /len))
    (let ((/occs (hash-table))
          (/nos '())
          (/n-save /n)
          (/total 0))
     (set! /n 0)
     ; Count the occurrences of each number in posn 
     (for-in /n /posn 
      (cond
       ((null? (gethash /occs /n))
        (puthash /occs /n 1)
        (set! /nos (cons /n /nos)))
       (#t
        (puthash /occs /n (+ (gethash /occs /n) 1)))))
     (for-in /n /nos 
      (cond
       ((>= (gethash /occs /n) /copies)
        (set! /total (+ /total (gethash /occs /n))))))
     (cond
      ((>= /total (* /copies /len))
       ; Possible cycle detected 
       (set! //O/K (@CP_Cycle2  (cdr (reverse /posn)) /copies /len //O/K))))
     (set! /n /n-save))))
  (set! funct-result (= //O/K 1))
  (set! //O/K //O/K-save)
  funct-result))

; Check for a cycle at the _beginning_ of list (list = REVERSE(@Posn)): 
(define (@CP_Cycle2 /list /copies /min_len //O/K)
 (let ((/a (make-vector-eval (gen-length /list) 0))
       (/n 0)
       (/l 0)
       (/start 0)
       (/c 0)
       (/i 0))
  ; Copy the list to an array and set n to the length of the list: 
  (while (not (null? /list)) 
   (begin
    (set! /n (+ /n 1))
    (wsl-set! /a (car /list) /n)
    (set! /list (cdr /list))))
  (set! /start (wsl-ref /a 1))
  ; l is the length of a candidate cycle : 
  (set! /l /min_len)
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    ; Scan backwards for another copy of start 
    (while (and (< (* /l /copies) /n) (not (equal? (wsl-ref /a (+ /l 1)) /start))) 
     (set! /l (+ /l 1)))
    (cond
     ((>= (* /l /copies) /n)
      (set! //O/K 0)
      (set! /fl_flag1 1))
     (#t
      ; Check for copies of the sequence 1..l at positions 
      ; from l+1..2l up to (copies-1)*l+1..copies*l 
      (set! /c 1)
      (set! //O/K 1)
      (while (and (< /c /copies) (= //O/K 1)) 
       (begin
        (set! /i 1)
        (while (and (<= /i /l) (= //O/K 1)) 
         (cond
          ((not (equal? (wsl-ref /a /i) (wsl-ref /a (+ (* /c /l) /i))))
           (set! //O/K 0))
          (#t
           (set! /i (+ /i 1)))))
        (cond
         ((= //O/K 1)
          (set! /c (+ /c 1))))))
      (cond
       ((= //O/K 1)
        (set! /fl_flag1 1))
       (#t
        ; Look for next copy of start and try again: 
        (set! /l (+ /l 1))
        (set! /fl_flag1 0))))))))
 //O/K)

(define (@CP_Print_L //L-par)
 (let ((//L-save //L))
  (set! //L //L-par)
  (let ((//L1 '()))
   (while (not (null? //L)) 
    (begin
     (display-list (wsl-ref (wsl-ref //L 1) 1) ":")
     (set! //L1 (wsl-ref (wsl-ref //L 1) 2))
     (while (not (null? //L1)) 
      (begin
       (cond
        ((<= (last-1 (wsl-ref (wsl-ref //L1 1) 1)) 0)
         (display-list-flush "  " (@Join "." (my-map @N_String (butlast-1 (wsl-ref (wsl-ref //L1 1) 1)))) "." (- (last-1 (wsl-ref (wsl-ref //L1 1) 1))) " := "))
        (#t
         (display-list-flush "  " (@Join "." (my-map @N_String (wsl-ref (wsl-ref //L1 1) 1))) " := ")))
       (@PP_Item (wsl-ref (wsl-ref //L1 1) 2) 80 "")
       (set! //L1 (cdr //L1))))
     (set! //L (cdr //L))
     (display-list ""))))
  (set! //L //L-save)))

; Find the destination values 
(define (@CP_Find_Dispatch_Codes /dispatch-par)
 (let ((/dispatch-save /dispatch)
       (/codes-save /codes)
       (/destination-save /destination)
       (/largest 0)
       (/regs '())
       (/tmp '())
       (funct-result '()))
  (set! /dispatch /dispatch-par)
  (set! /codes '())
  (set! /destination (@Make_Name "destination"))
  (@Edit)
  (@Down_Last)
  (@Down_Last)
  ; to the last action 
  (while (and (@Left?) (not (equal? (@V (@Get_n (@I) 1)) /dispatch))) 
   (@Left))
  (cond
   ((equal? (@V (@Get_n (@I) 1)) /dispatch)
    (@Down_Last)
    ; to dispatch body 
    (@Down)
    (while (and (not (= (@ST (@I)) //T_/Cond)) (@Right?)) 
     (@Right))
    (cond
     ((= (@ST (@I)) //T_/Cond)
      (@Down)
      ; to first guard 
      (set! /fl_flag1 0)
      (while (= /fl_flag1 0) 
       (begin
        (@Down)
        ; to cond 
        (cond
         ((= (@ST (@I)) //T_/Equal)
          (cond
           ((and (= (@ST (@Get_n (@I) 1)) //T_/Variable) (equal? (@V (@Get_n (@I) 1)) /destination) (= (@ST (@Get_n (@I) 2)) //T_/Number))
            (set! /codes (cons (@V (@Get_n (@I) 2)) /codes)))
           ((and (= (@ST (@Get_n (@I) 1)) //T_/Variable) (equal? (@V (@Get_n (@I) 1)) /destination) (= (@ST (@Get_n (@I) 2)) //T_/Negate) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Number))
            (set! /codes (cons (- (@V (@Get_n (@Get_n (@I) 2) 1))) /codes)))
           ((and (= (@ST (@Get_n (@I) 2)) //T_/Variable) (equal? (@V (@Get_n (@I) 2)) /destination) (= (@ST (@Get_n (@I) 1)) //T_/Number))
            (set! /codes (cons (@V (@Get_n (@I) 1)) /codes))))))
        (@Up)
        ; back to the guard 
        (cond
         ((not (@Right?))
          (set! /fl_flag1 1))
         (#t
          (@Right)
          (set! /fl_flag1 0)))))))))
  (set! /codes (reverse /codes))
  (@Undo_Edit)
  (set! funct-result /codes)
  (set! /dispatch /dispatch-save)
  (set! /codes /codes-save)
  (set! /destination /destination-save)
  funct-result))

(define (@CP_Find_Return_Elts /dispatch_codes-par)
 (let ((/dispatch_codes-save /dispatch_codes)
       (//R-save //R)
       (/posn (@Posn))
       (/done-save /done)
       (funct-result '()))
  (set! /dispatch_codes /dispatch_codes-par)
  (set! //R '())
  (set! /done 0)
  (@Goto '())
  (set! /done 1)
  (@Foreach_Statement /foreach-constant_propagation-34 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (while (not (= /done 1)) 
   (begin
    (set! /done 1)
    (@Foreach_Statement /foreach-constant_propagation-35 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (@Goto /posn)
  (cond
   (#f
    (display-list-flush "Return elts:")
    (@Print_Elts //R)))
  (set! funct-result //R)
  (set! /dispatch_codes /dispatch_codes-save)
  (set! //R //R-save)
  (set! /done /done-save)
  funct-result))

; Look for dispatch codes which are assigned to a variable 
; where this is the only assignment to the variable. 
; Replace all references to the variable by the dispatch code. 
; If this creates an assignment to destination, then we can 
; delete the assignment of the code to the variable, 
; and delete any assignment of the code to a register. 
; This should eventually allow pruning of the dispatch code. 
; NB: Fix_Dispatch already does something very similar when effort > 0 
(define (@CP_Once_Called_Codes //Code_/Hash-par)
 (let ((//Code_/Hash-save //Code_/Hash))
  (set! //Code_/Hash //Code_/Hash-par)
  (let ((/var_value-save /var_value)
        (/delete-save /delete)
        (/elt-save /elt)
        (/codes-save /codes)
        (/destination-save /destination))
   (set! /var_value (hash-table))
   (set! /delete (hash-table))
   (set! /elt '())
   (set! /codes '())
   (set! /destination (@Make_Name "destination"))
   (@Foreach_Statement /foreach-constant_propagation-36 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; Check that there are no other assignments 
   (@Foreach_Statement /foreach-constant_propagation-37 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /elt (@Hash_Keys /var_value) 
    (cond
     ((not (null? (gethash /var_value /elt)))
      (display-list "Replacing:  " (my-map @N_String /elt) " = " (gethash /var_value /elt)))))
   (@Ateach_Expn /foreach-constant_propagation-38 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Statement /foreach-constant_propagation-39 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /var_value /var_value-save)
   (set! /delete /delete-save)
   (set! /elt /elt-save)
   (set! /codes /codes-save)
   (set! /destination /destination-save))
  (set! //Code_/Hash //Code_/Hash-save)))

; Check for rX := code before the Cond 
; where each branch of the Cond is either a CALL 
; or proc call plus CALL. Can use Code_Hash. 
(define (@CP_Maybe_Absorb_Left //L-par)
 (let ((//L-save //L)
       (funct-result '()))
  (set! //L //L-par)
  (let ((/count 0)
        (//O/K-save //O/K)
        (//A/S (@AS_Type))
        (/used '()))
   (set! //O/K 1)
   ; Expand over any immediately following simple Cond(s): 
   (set! /count 0)
   (while (@Trans? //T/R_/Else_/If_/To_/Elsif) 
    (@Trans //T/R_/Else_/If_/To_/Elsif ""))
   (for-in /guard (@Cs (@I)) 
    (cond
     ((not (@Gen_Improper? (@Get_n /guard 2) //A/S))
      (set! /count (+ /count 1)))))
   (cond
    ((= /count 1)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (cond
         ((not (and (= (@ST (@I)) //T_/Cond) (= (@Size (@I)) 2) (member //T_/Call (@Stat_Types (@Get_n (@Get_n (@I) 1) 2))) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 2) 2) 1)) //T_/Skip)))
          (@Left)
          (set! /fl_flag1 1))
         (#t
          (@Left)
          (cond
           ((@Trans? //T/R_/Expand_/Forward)
            (@Trans //T/R_/Expand_/Forward "")
            (while (@Trans? //T/R_/Else_/If_/To_/Elsif) 
             (@Trans //T/R_/Else_/If_/To_/Elsif ""))
            (set! /fl_flag1 0))
           (#t
            (error "@CP_Maybe_Absorb_Left: cannot Expand_Forward!")
            (set! /fl_flag1 0))))))))))
   ; Count how many guards start with a CALL 
   (set! /count 0)
   (for-in /guard (@Cs (@I)) 
    (cond
     ((= (@ST (@Get_n (@Get_n /guard 2) 1)) //T_/Call)
      (set! /count (+ /count 1)))))
   (cond
    ((> /count 1)
     ; Scan back looking for an assignment of a dispatch code to a register 
     (@Left)
     (while (and (= (@ST (@I)) //T_/Comment) (@Left?)) 
      (@Left))
     (cond
      ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (member (@V (@Get_n (@Get_n (@I) 1) 1)) /registers) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (> (@V (@Get_n (@Get_n (@I) 1) 2)) 1) (not (null? (gethash //Code_/Hash (@V (@Get_n (@Get_n (@I) 1) 2))))))
       (wsl-set! //L (@CP_Remove (wsl-ref (wsl-ref //L 1) 2) (list (@V (@Get_n (@Get_n (@I) 1) 1)))) 1 2)
       (@Cut)
       (while (= (@ST (@I)) //T_/Comment) 
        (@Right))
       (@Paste_Before (@Buffer))
       (@Right)
       (cond
        ((@Trans? //T/R_/Absorb_/Left)
         (@Trans //T/R_/Absorb_/Left ""))))
      (#t
       (@Right)
       (while (= (@ST (@I)) //T_/Comment) 
        (@Right))))))
   (set! //O/K //O/K-save))
  (set! funct-result //L)
  (set! //L //L-save)
  funct-result))

#t
