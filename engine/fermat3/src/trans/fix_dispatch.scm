;;; Scheme translation of WSL code
(define (/foreach-fix_dispatch-1 //Depth //A/S_/Type)
 (@Down)
 (while (@Right?) 
  (cond
   ((= (@ST (@I)) //T_/Cond)
    (@Right)
    (cond
     ((= (@ST (@I)) //T_/Call)
      (@Left)
      (@Trans //T/R_/Absorb_/Right ""))))
   (#t
    (@Right)))))

(define (/foreach-fix_dispatch-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   ; to first assign 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (@FD_Is_Code? (@Get_n (@I) 2) //Codes) (not (@Starts_With? (@V (@Get_n (@I) 1)) "HANDLE_")))
       (set! /reg (@V (@Get_n (@I) 1)))
       (set! /call (@FD_Find_Call 4 //Codes))
       (cond
        ((and (number? /call) (< /call 0))
         (cond
          ((and (not (equal? (- /call) /dn)) (not (equal? (- /call) //Starting_/Action)) (not (= (- /call) (+ //N 1))))
           (puthash //Return_/Regs (- /call) (union-n (gethash //Return_/Regs (- /call)) (list /reg)))
           (cond
            ((not-member (- /call) //Entry_/Actions)
             (display-list-flush (@N_String (wsl-ref //Names (- /call))))
             (display-list-flush " ")
             (set! //Entry_/Actions (union-n (list (- /call)) //Entry_/Actions))))))))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))
   (@Up))))

(define (/foreach-fix_dispatch-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (- /dn)))
   (let ((/posn (@Posn)))
    (@FD_Move_To_Assignment)
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__fix_dispatch__1 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__v_save /v))
        (set! /v (vector-ref /__/Match_array 0))
        (cond
         ((= (@ST /v) //T_/Variable)
          (puthash //Dispatch_/Regs /i (union-n (gethash //Dispatch_/Regs /i) (list (@V /v))))))
        (set! /v /__v_save)))))
    (@Goto /posn)))))

(define (/foreach-fix_dispatch-4 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (@Down)
   ; to first assign 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((and (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (@FD_Is_Code? (@Get_n (@I) 2) //Codes) (not (@Starts_With? (@V (@Get_n (@I) 1)) "HANDLE_")))
       (set! /reg (@V (@Get_n (@I) 1)))
       (set! /call (@FD_Find_Call 4 //Codes))
       (cond
        ((and (number? /call) (< /call 0))
         (cond
          ((and (not (equal? (- /call) /dn)) (not (equal? (- /call) //Starting_/Action)) (not (= (- /call) (+ //N 1))))
           (puthash //Return_/Regs (- /call) (union-n (gethash //Return_/Regs (- /call)) (list /reg)))
           (cond
            ((not-member (- /call) //Entry_/Actions)
             (display-list-flush (@N_String (wsl-ref //Names (- /call))))
             (display-list-flush " ")
             (set! //Entry_/Actions (union-n (list (- /call)) //Entry_/Actions))))))))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))
   (@Up))))

(define (/foreach-fix_dispatch-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) (- /dn)))
   (let ((/posn (@Posn)))
    (@FD_Move_To_Assignment)
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__fix_dispatch__1 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__v_save /v))
        (set! /v (vector-ref /__/Match_array 0))
        (cond
         ((= (@ST /v) //T_/Variable)
          (puthash //Dispatch_/Regs /i (union-n (gethash //Dispatch_/Regs /i) (list (@V /v))))))
        (set! /v /__v_save)))))
    (@Goto /posn)))))

(define (/foreach-fix_dispatch-6 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (set! //J (gethash //Name2/Num (@V (@I))))
   (cond
    ((null? //J)
     (set! //J (+ //N 1))))
   (@Paste_Over (@Make //T_/Call (- //J) '())))))

(define (/foreach-fix_dispatch-7 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (< (@V (@I)) 0))
   (@Paste_Over (@Make //T_/Call (wsl-ref //Names (- (@V (@I)))) '())))))

(define (/foreach-fix_dispatch-8 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (< (@V (@I)) 0))
   (set! /v (- (@V (@I))))
   (cond
    ((member /v /body)
     ; Internal call, change number to name 
     (@Paste_Over (@Make //T_/Call (wsl-ref //Names /v) '())))
    ((equal? /v /dn)
     ; normal return: 
     (set! /dispatch_called 1)
     (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /flag) (@Make 205 0 '()))))))
     (@Paste_After /call_/Z))
    ((= /v (+ //N 1))
     ; error return: 
     (set! //Z_called 1)
     (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /flag) (@Make 205 1 '()))))))
     (@Paste_After /call_/Z))
    (#t
     ; Assume this is a tail recursive call, 
     ; Copy register to destination and set a flag: 
     (cond
      ((null? (gethash /name2flag /v))
       (set! /flag_n (+ /flag_n 1))
       (puthash /name2flag /v /flag_n)
       (puthash /flag2name /flag_n /v)))
     (set! /flag_val (@Make //T_/Number (gethash /name2flag /v) '()))
     (cond
      ((equal? (gethash //Return_/Regs /v) /reg)
       (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /flag) (@Var_To_Expn /flag_val))))))
       (@Paste_After (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Var_To_Expn /reg_var))))))
       (@Right))
      (#t
       (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /flag) (@Var_To_Expn /flag_val))))))))
     (@Paste_After /call_/Z))))))

(define (/foreach-fix_dispatch-9 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (equal? (- (@V (@I))) /start))
   (@Splice_Over (@Cs /body)))))

(define (/foreach-fix_dispatch-10 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__fix_dispatch__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e_save /e))
     (set! /e (vector-ref /__/Match_array 0))
     (@Delete)
     (set! /e /__e_save))))))

(define (/foreach-fix_dispatch-11 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Floop) (@Right?))
   (@Right)
   (cond
    ((= (@ST (@I)) //T_/Call)
     (@Left)
     (@Trans //T/R_/Absorb_/Right ""))
    (#t
     (@Left))))))

(define (/foreach-fix_dispatch-12 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (number? (@V (@I))) (equal? (- (@V (@I))) /start))
   (@Splice_Over (@Cs /body))
   ; Search backwards for the return code, if found, 
   ; insert an assignment to destination after the proc call: 
   (set! /posn (@Posn))
   (set! /return '())
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((not (@Left?))
       (cond
        ((not (@Up?))
         (set! /fl_flag1 1))
        (#t
         (@Up)
         (@Up_To_Statement)
         (set! /up 1)
         (cond
          ((not (@Left?))
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0))))))
      (#t
       (set! /fl_flag1 0)))
     (cond
      ((= /fl_flag1 0)
       (@Left)
       (cond
        ((and (not (= (@ST (@I)) //T_/Assignment)) (not (= (@ST (@I)) //T_/Comment)) (not (= (@ST (@I)) //T_/A_/Proc_/Call)))
         (set! /fl_flag1 1))
        ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n (@I) 1) 1)) /reg))
         (cond
          ((= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number)
           (set! /return (@Get_n (@Get_n (@I) 1) 2))
           (@Down)
           (@Down_To 2)
           ; We keep the original dispatch value so that 
           ; C_P can propagate it: at the risk of 
           ; not being able to prune it from dispatch: 
           (cond
            ((= /inc 0)
             (cond
              ((and (<= /flag_n 1) (= /up 0))
               (@FD_Clobber_Value))))
            (#f
             (@Paste_Over (@Make //T_/Number 0 '()))))
           (@Up)
           (@Up))
          ((and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Variable) (member (@V (@Get_n (@Get_n (@I) 1) 2)) /reg_inits))
           (set! /return (@Make //T_/Number 0 '())))
          ((and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/X_/Funct_/Call) (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 1) 2) 1)) /inline_par))
           (display-list "Found inline_par")
           ; Ignore increments on return address 
           (set! /inc 0)
           (@Down)
           (@Down_To 2)
           ; to funct call 
           (set! /return (@Get_n (@Get_n (@I) 2) 1))
           (@Paste_Over (@Get_n (@Get_n (@I) 2) 2))
           ; the data pointer 
           (@Up)
           (@Up)
           ; back to statement 
          ))
         (set! /fl_flag1 1))
        (#t
         (set! /fl_flag1 0)))))))
   (@Goto /posn)
   (cond
    ((not (null? /return))
     (display-list "...return code = " (@V /return) " at " (@Posn))
     (cond
      ((= /inc 0)
       (@Right)
       (@FD_Unfold_Dispatch /return /call_dn //Bodies))
      (#t
       (set! /rr 1)
       (@FD_Inc_Return_Code /return /call_dn /call_zn))))
    (#t
     (display-list "...can't find return code at " (@Posn))
     (set! /rr (@FD_Check_Return_Code_Inc  /rr))))
   (@Goto /posn))))

(define (/foreach-fix_dispatch-13 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__fix_dispatch__5 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__v_save /v)
          (/__code_save /code))
     (set! /v (vector-ref /__/Match_array 1))
     (set! /code (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /code) //T_/Number) (> (@V /code) 4) (not (null? (gethash //Codes (@V /code)))))
       (@Paste_Over /code)))
     (set! /v /__v_save)
     (set! /code /__code_save))))))

(define (/foreach-fix_dispatch-14 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__fix_dispatch__4 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e_save /e)
          (/__v_save /v))
     (set! /e (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /e) //T_/Variable) (= (@ST /v) //T_/Var_/Lvalue) (not (null? (gethash //D/S/E/C/Ts (@V /v)))) (member (@V /e) /registers))
       (puthash //D/S/E/C/T_regs (@V /v) (union-n (gethash //D/S/E/C/T_regs (@V /v)) (list (@V /e))))))
     (set! /e /__e_save)
     (set! /v /__v_save))))))

(define (/foreach-fix_dispatch-15 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__fix_dispatch__4 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__e_save /e)
          (/__v_save /v))
     (set! /e (vector-ref /__/Match_array 1))
     (set! /v (vector-ref /__/Match_array 0))
     (cond
      ((and (= (@ST /e) //T_/Number) (> (@V /e) 4) (not (null? (gethash //Codes (@V /e)))) (or (= (@ST /v) //T_/Struct_/Lvalue) (= (@ST /v) //T_/Sub_/Seg_/Lvalue)))
       (set! //L1 (union-n (list (car (@Elements /v))) //L1)))
      ((null? (@Elements /e))
       #t)
      ((and (= (@ST /e) //T_/Variable) (member (@V /e) /registers) (or (= (@ST /v) //T_/Struct_/Lvalue) (= (@ST /v) //T_/Sub_/Seg_/Lvalue)))
       (set! //L1 (union-n (list (car (@Elements /v))) //L1)))
      ((and (= (@ST /v) //T_/Var_/Lvalue) (member (@V /v) /registers) (or (= (@ST /e) //T_/Struct) (= (@ST /e) //T_/Sub_/Seg)))
       (set! //L2 (union-n (list (car (@Elements /e))) //L2))))
     (set! /e /__e_save)
     (set! /v /__v_save))))))

(define (/foreach-fix_dispatch-16 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/Struct) (and (= (@ST (@I)) //T_/Sub_/Seg) (= (@ST (@Get_n (@I) 2)) //T_/Number) (= (@ST (@Get_n (@I) 3)) //T_/Number) (= (- (@V (@Get_n (@I) 3)) (@V (@Get_n (@I) 2))) 3)))
   (set! /name (gethash /new (car (@Elements (@I)))))
   (cond
    ((not (null? /name))
     (puthash /rename /name (@I))
     (@Paste_Over (@Make //T_/Variable /name '())))))))

(define (/foreach-fix_dispatch-17 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/Struct_/Lvalue) (and (= (@ST (@I)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n (@I) 2)) //T_/Number) (= (@ST (@Get_n (@I) 3)) //T_/Number)))
   (set! /name (gethash /new (car (@Elements (@I)))))
   (cond
    ((not (null? /name))
     (puthash /rename /name (@Lvalue_To_Expn (@I)))
     (@Paste_Over (@Make //T_/Var_/Lvalue /name '())))))))

(define (/foreach-fix_dispatch-18 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (not (null? (gethash /rename (@V (@I))))))
   (@Paste_Over (gethash /rename (@V (@I)))))))

(define (/foreach-fix_dispatch-19 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Var_/Lvalue) (not (null? (gethash /rename (@V (@I))))))
   (@Paste_Over (@Expn_To_Lvalue (gethash /rename (@V (@I))))))))

(define (/foreach-fix_dispatch-20 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) /destination))
   (@Paste_Over /code))))

(define (/foreach-fix_dispatch-21 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /call_dn)
   (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() /body))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
   (@Foreach_Expn /foreach-fix_dispatch-20 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips)))))))

(define (/foreach-fix_dispatch-22 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Assignment)
   (set! /addr_vars (@FD_Update_Addr_Vars  (@I) //Code_/Hash //Registers /addr_vars)))
  ((= (@ST (@I)) //T_/Var)
   (set! /addr_vars (@FD_Update_Addr_Vars  (@Get_n (@I) 1) //Code_/Hash //Registers /addr_vars)))))

(define (/foreach-fix_dispatch-23 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (not (null? (gethash /addr_vars (@V (@Get_n (@Get_n (@I) 1) 1))))) (or (not (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number)) (and (not (= (@V (@Get_n (@Get_n (@I) 1) 2)) 0)) (null? (gethash //Code_/Hash (@V (@Get_n (@Get_n (@I) 1) 2)))))))
   (puthash /addr_vars (@V (@Get_n (@Get_n (@I) 1) 1)) '()))))

(define (/foreach-fix_dispatch-24 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (equal? (@V (@I)) /dispatch))
   (set! /posn (@Posn))
   (while (and (@Left?) (not (= (@ST (@I)) //T_/Assignment))) 
    (@Left))
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__fix_dispatch__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__v_save /v))
       (set! /v (vector-ref /__/Match_array 0))
       (cond
        ((and (= (@ST /v) //T_/Variable) (not (null? (gethash /addr_vars (@V /v)))))
         (@FD_Jump_Table_Fix2 (@V /v) (@V /v) /posn /addr_vars))
        ((and (= (@ST /v) //T_/Variable) (member (@V /v) //Registers))
         (@FD_Move_To_Assignment)
         (let ((/__/O/K 1))
          (vector-set! /__/Match_array 0 /v)
          (set! /__/O/K (@New_Match  /%const__fix_dispatch__6 (@I) /__/O/K))
          (cond
           ((= /__/O/K 1)
            (let ((/__e_save /e))
             (set! /e (vector-ref /__/Match_array 1))
             (cond
              ((and (= (@ST /e) //T_/Variable) (not (null? (gethash /addr_vars (@V /e)))))
               (@FD_Jump_Table_Fix2 (@V /e) (@V /v) /posn /addr_vars))
              (#t
               (@Goto /posn)))
             (set! /e /__e_save)))
           (#t
            (@Goto /posn)))))
        (#t
         (@Goto /posn)))
       (set! /v /__v_save)))
     (#t
      (@Goto /posn)))))))

(define (/foreach-fix_dispatch-25 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (vector-set! /__/Match_array 0 /var)
  (set! /__/O/K (@New_Match  /%const__fix_dispatch__6 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__exp_save /exp))
     (set! /exp (vector-ref /__/Match_array 1))
     (cond
      ((and (= (@ST /exp) //T_/Number) (not (null? (gethash /new (@V /exp)))))
       (set! /exp (@Make //T_/Number (gethash /new (@V /exp)) '()))
       (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /exp))))))))
     (set! /exp /__exp_save))))))

(define (/foreach-fix_dispatch-26 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/Proc_/Call)
   (for-in /elt (@Elts_Assigned (@I)) 
    (puthash /bad /elt 1)))
  ((= (@ST (@I)) //T_/Assignment)
   (cond
    ((and (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (not (null? (gethash //Code_/Hash (@V (@Get_n (@Get_n (@I) 1) 2))))) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Struct_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue)) (not (and (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (@Starts_With? (@V (@Get_n (@Get_n (@I) 1) 1)) "HANDLE_"))))
     (set! /elt (@Struct_Elts (@Get_n (@Get_n (@I) 1) 1)))
     (cond
      ((and (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) //T_/Number))
       (set! /elt (concat /elt (list (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)))))))
     (cond
      ((null? (gethash /value /elt))
       (puthash /value /elt (@V (@Get_n (@Get_n (@I) 1) 2))))
      ((equal? (gethash /value /elt) (@V (@Get_n (@Get_n (@I) 1) 2)))
       #t)
      (#t
       (puthash /bad /elt 1))))))))

(define (/foreach-fix_dispatch-27 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Struct_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue)))
   (set! /elt (@Struct_Elts (@Get_n (@Get_n (@I) 1) 1)))
   (cond
    ((and (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) //T_/Number))
     (set! /elt (concat /elt (list (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)))))))
   (cond
    ((and (null? (gethash /bad /elt)) (not (null? (gethash /value /elt))))
     (cond
      ((or (not (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number)) (not (equal? (@V (@Get_n (@Get_n (@I) 1) 2)) (gethash /value /elt))))
       (puthash /bad /elt 1))))))))

(define (/foreach-fix_dispatch-28 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (not (null? (gethash /code (@V (@Get_n (@Get_n (@I) 1) 2))))))
   (puthash /count1 (@V (@Get_n (@Get_n (@I) 1) 2)) (+ (gethash /count1 (@V (@Get_n (@Get_n (@I) 1) 2))) 1))))
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Variable) (not (null? (gethash /notused (@V (@Get_n (@Get_n (@I) 1) 2))))))
   (puthash /count2 (gethash /notused (@V (@Get_n (@Get_n (@I) 1) 2))) (+ (gethash /count2 (gethash /notused (@V (@Get_n (@Get_n (@I) 1) 2)))) 1)))))

(define (/foreach-fix_dispatch-29 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/Variable) (= (@ST (@I)) //T_/Struct) (= (@ST (@I)) //T_/Sub_/Seg))
   (set! /elt (@Struct_Elts (@I)))
   (cond
    ((and (= (@ST (@I)) //T_/Sub_/Seg) (= (@ST (@Get_n (@I) 2)) //T_/Number))
     (set! /elt (concat /elt (list (@V (@Get_n (@I) 2)))))))
   (cond
    ((and (not (null? (gethash /value /elt))) (not (and (= (@ST (@Parent)) //T_/Sub_/Seg) (= (@Posn_n) 1))) (not (and (= (@ST (@Parent)) //T_/Rel_/Seg) (= (@Posn_n) 1))))
     (@Paste_Over (@Make //T_/Number (gethash /value /elt) '()))
     (set! /found 1))))))

(define (/foreach-fix_dispatch-30 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (or (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Struct_/Lvalue) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue)))
   (set! /elt (@Struct_Elts (@Get_n (@Get_n (@I) 1) 1)))
   (cond
    ((and (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) //T_/Number))
     (set! /elt (concat /elt (list (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)))))))
   (cond
    ((not (null? (gethash /value /elt)))
     (@Down)
     (@Down_To 2)
     (set! /name (@Make_Name (string-append "NOTUSED_" (@String (gethash /value /elt)))))
     (@Paste_Over (@Make //T_/Variable /name '()))
     (@Up)
     (@Up))))))

(define (/foreach-fix_dispatch-31 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@I) 1) 2)) //T_/Number) (not (null? (gethash /code (@V (@Get_n (@Get_n (@I) 1) 2))))) (< (gethash /count2 (@V (@Get_n (@Get_n (@I) 1) 2))) 4))
   (@Down)
   (@Down_To 2)
   (set! /name (@Make_Name (string-append "NOTUSED_" (@String (@V (@I))))))
   (@Paste_Over (@Make //T_/Variable /name '()))
   (@Up)
   (@Up))))

(define (/foreach-fix_dispatch-32 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Floop) (@Right?) (@Up?))
   (cond
    ((= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Call)
     (cond
      ((@Trans? //T/R_/Absorb_/Right)
       (@Trans //T/R_/Absorb_/Right ""))))))))

(define (/foreach-fix_dispatch-33 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@Get_n (@I) (@Size (@I)))) //T_/Call) (equal? (@V (@Get_n (@I) (@Size (@I)))) //Dispatch))
   (let ((/-result- (@FD_Action  /name /names /count /new)))
    (set! /names (car /-result-)) (set! /-result- (cdr /-result-))
    (set! /count (car /-result-)) (set! /-result- (cdr /-result-))
    (set! /new (car /-result-)) (set! /-result- (cdr /-result-))))))

(define /%const__fix_dispatch__1 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Make 217 -1 '()))))))
(define /%const__fix_dispatch__2 (@Make 17 '() (list (@Make 145 '() '()))))
(define /%const__fix_dispatch__3 (@Make 308 '() '()))
(define /%const__fix_dispatch__4 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 217 -2 '()))))))
(define /%const__fix_dispatch__5 (@Make 201 '() (list (@Make 9 (@Make_Name "bit_or") '()) (@Make 10 '() (list (@Make 217 -1 '()) (@Make 217 -2 '()))))))
(define /%const__fix_dispatch__6 (@Make 110 '() (list (@Make 6 '() (list (@Make 510 '() (list (@Make 205 1 '()))) (@Make 217 -2 '()))))))
(define /%const__fix_dispatch__7 (@Make 110 '() (list (@Make 6 '() (list (@Make 506 -1 '()) (@Make 205 0 '()))))))
(define /%const__fix_dispatch__8 (@Make 114 '() (list (@Make 7 '() (list (@Make 309 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 112 (@Make_Name "Z") '()))))))))
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
; Algorithm for Fix_Dispatch: search for simple procedures, 
; Convert to PROCs, unfold all calls, and simplify dispatch calls 
; A simple procedure is a collection of actions, with a single 
; entry point and multiple exit points. The exit points are either 
; procedure returns (ie dispatch calls) or calls to Z. 
; The procedure must not call other procedures (except WSL procedures). 
; The collection of actions forming this procedure can be converted 
; to a nested action system, with a single exit point (set a flag and 
; CALL Z within the nested action system to exit). This nested action system 
; can be folded into a PROC, with an IF statement to test the flag 
; when the PROC_CALL returns (test the flag to see whether to call 
; Z or dispatch). 
; The entry point action can then be unfolded everywhere, and the 
; calls to dispatch simplified via Constant_Propagation 
; After converting all the simple procedures to PROCS, some other 
; procedures may become simple (since they only called simple procedures). 
; So the process can be repeated, `working up' the call graph 
; The technique will be defeated by (a) Mutually recursive procedures; 
; or (b) A procedure with a second entry point, inside a loop. 
; These will have to be dealt with manually, but hopefully should not 
; occur too often! 
; After finding all simple procedures, the main body and the procedure 
; bodies can be dealt with separately using Simplify_Action_System and 
; Collapse_Action_System as appropriate 
; It is possible that Simplify_Action_System may cause some problems for 
; Fix_Dispatch, for example, unfolding and removing entry actions 
; (But in this case, the procedure is only called once and therefore 
; has a single return point. So the CALL dispatch can be unfolded and simplified). 
; The Algorithm: 
; Search for rx:=dest_code; ...; CALL foo 
;   to find possible entry points of simple procedures 
; Do a breadth first search from an entry point: 
;   Stop at CALL dispatch (ie normal return) or CALL Z 
;     or a CALL to another entry point which uses the same register 
;     where the register is not set first (ie a tail-recursive call) 
;   Form the transitive closure 
;   Fail if any action in the closure is called from outside the closure 
;   (apart from the single entry point) 
; Form a nested action system from these actions 
; Turn it into a procedure 
; Check that the procedure preserves the destination register. 
; Unfold the entry point action everywhere. 
; Return_Regs: maps each action index number to the set of variables 
;              which are assigned a dispatch code just before the call. 
; Dispatch_Regs: maps each action index number to the set of variables 
;                which are copied to destination just before a call to dispatch 
;                in the body of this action (should be no more than one) 
(define (@Fix_Dispatch_Test)
 (cond
  ((not (= (@ST (@I)) //T_/A_/S))
   (@Fail "Not an action system"))
  (#t
   ; Check for a dispatch action as the last action 
   (cond
    ((not (equal? (@V (@Get_n (@Get_n (@Get_n (@I) 2) (@Size (@Get_n (@I) 2))) 1)) (@Make_Name "dispatch")))
     (@Fail "No dispatch action"))
    (#t
     (@Pass))))))

(define (@Fix_Dispatch_Code //Data)
 (let ((//Dispatch-save //Dispatch)
       (//Code_/List '())
       (/code-save /code)
       (//Code_/Hash-save //Code_/Hash)
       (/rest '())
       (/effort-save /effort)
       (/budget-save /budget)
       (/pair '())
       (/rename-save /rename)
       (/orig '())
       (/new_actions-save /new_actions))
  (set! //Dispatch (@Make_Name "dispatch"))
  (set! /code '())
  (set! //Code_/Hash (hash-table))
  (set! /effort 0)
  (set! /budget 25600)
  (set! /rename (hash-table))
  (set! /new_actions '())
  (set! //Assume_/A_/S_/Regular 1)
  ; If Data contains a space, then it is two numbers: effort plus budget. 
  ; Otherwise, Data is the budget for @Preserves_Destination 
  ; effort = 1 - check for vars which are only assigned a known dispatch code 
  ; effort = 1 - check for tail-recursive calls with the same return register 
  ; effort = 1 - convert external calls to returns, provided dispatch is also called 
  ;              and the return code ends up in destination for these dispatch calls 
  ; effort = 2 - take out more external calls: include indirect branches 
  ;              which lead to another subroutine call 
  (cond
   ((>= (my-index " " (@String //Data)) 0)
    (set! /pair (my-map @String_To_Num (@Split //Data)))
    (set! /effort (wsl-ref /pair 1))
    (cond
     ((> (wsl-ref /pair 2) 0)
      (set! /budget (wsl-ref /pair 2)))))
   ((> (@String_To_Num (@String //Data)) 0)
    (set! /budget (@String_To_Num (@String //Data)))))
  ; First we absorb calls into a preceeding COND, if possible: 
  (@Foreach_Stats /foreach-fix_dispatch-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /orig (@Program))
  ; It appears to work better if we do tail-recursive calls at the same time 
  ; as other external calls: 
  ; If effort > 0, check that the effort is actually needed! 
  (cond
   ((> /effort 0)
    (@Fix_Dispatch_Code (@String /budget))))
  (display-list "Fix_Dispatch effort = " /effort " budget = " /budget)
  (set! //Code_/List (@Find_Dispatch_Codes //Dispatch))
  (for-in /code //Code_/List 
   (cond
    ((> /code 0)
     (puthash //Code_/Hash /code 1))))
  (cond
   ((> /effort 0)
    (@FD_Once_Called_Subroutines //Code_/Hash /budget)))
  ; First we check the format of dispatch 
  ; This is not done in the test because it is an expensive test! 
  (cond
   ((and (> /effort 0) (not (@Equal? /orig (@Program))))
    (display-list "Effort of " /effort " was not used."))
   ((null? //Code_/List)
    (display-list "No dispatch codes found!"))
   (#t
    (@FD_Bit_Or_Code_Check //Code_/Hash)
    ; Must do at least one constant propagation before we do this 
    ; (otherwise HANDLE CONDITION processing creates huge jump tables): 
    (cond
     ((>= /effort 1)
      (@FD_Jump_Table_Fix //Code_/Hash)))
    (cond
     (#t
      ; Look for registers which are saved and restored in DSECTs 
      ; and convert to a simple variable to aid in constant propagation 
      (let ((/posn-save /posn))
       (set! /posn (@Posn))
       (@Goto '())
       (set! /rename (@FD_Rename_DSECT_Code_Stores  //Code_/Hash /rename))
       (@Goto /posn)
       (set! /posn /posn-save))))
    (cond
     ((>= /effort 1)
      (set! /new_actions (@FD_Separate_Dispatch_Calls  /new_actions))))
    ; Search for enclosing WHERE clauses and collect together 
    ; the definitions of any procedures into Proc_Summaries: 
    (let ((/posn-save /posn)
          (/posns '())
          (/pops 0))
     (set! /posn (@Posn))
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (begin
       (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
        (@Up))
       (cond
        ((not (= (@ST (@I)) //T_/Where))
         (set! /fl_flag1 1))
        (#t
         (set! /posns (cons (@Posn) /posns))
         (cond
          ((not (@Up?))
           (set! /fl_flag1 1))
          (#t
           (@Up)
           (set! /fl_flag1 0)))))))
     ; Number of times to pop Proc_Summaries: 
     (set! /pops (gen-length /posns))
     (while (not (null? /posns)) 
      (begin
       (@Goto (car /posns))
       (set! /posns (cdr /posns))
       (set! //Proc_/Summaries (cons (hash-table) //Proc_/Summaries))
       (@Summarise_Where_Defns (@Cs (@Get_n (@I) 2)))))
     (@Goto /posn)
     (@Edit)
     (@FD_Main //Code_/Hash //Dispatch_/Regs)
     (@End_Edit)
     (@Goto '())
     (@FD_Restore_DSECT_Code_Stores /rename)
     (@Goto /posn)
     (while (> /pops 0) 
      (begin
       (set! /pops (- /pops 1))
       (set! //Proc_/Summaries (cdr //Proc_/Summaries))))
     (cond
      ((@Syntax_OK? (@Program))
       (display-list "Syntax is OK"))
      (#t
       (display-list "Syntax NOT OK!")))
     (cond
      ((@Trans? //T/R_/Combine_/Wheres)
       (@Trans //T/R_/Combine_/Wheres "")))
     (cond
      ((not (= (@ST (@I)) //T_/A_/S))
       (@Find_Type //T_/A_/S)))
     (cond
      ((not (null? /new_actions))
       ; Unfold any created new actions 
       (let ((/unfold (hash-table)))
        (for-in /action /new_actions 
         (puthash /unfold (@V (@Get_n /action 1)) 1))
        (@Down_To 2)
        (@Down)
        ; to first action 
        (display-list "unfold = " (my-map @N_String (@Hash_Keys /unfold)))
        (cond
         ((not (null? (gethash /unfold (@V (@Get_n (@I) 1)))))
          (cond
           ((@Trans? //T/R_/Substitute_/And_/Delete)
            (@Trans //T/R_/Substitute_/And_/Delete "")
            (@Left)))))
        (while (@Right?) 
         (begin
          (@Right)
          (cond
           ((not (null? (gethash /unfold (@V (@Get_n (@I) 1)))))
            (cond
             ((@Trans? //T/R_/Substitute_/And_/Delete)
              (@Trans //T/R_/Substitute_/And_/Delete "")
              (@Left)))))))
        (@Up)
        (@Up)
        ; back to A_S 
       )))
     (set! /posn /posn-save))))
  (set! //Dispatch //Dispatch-save)
  (set! /code /code-save)
  (set! //Code_/Hash //Code_/Hash-save)
  (set! /effort /effort-save)
  (set! /budget /budget-save)
  (set! /rename /rename-save)
  (set! /new_actions /new_actions-save)))

(define (@FD_Main //Codes-par //Dispatch_/Regs-par)
 (let ((//Dispatch_/Regs-save //Dispatch_/Regs)
       (//Codes-save //Codes))
  (set! //Dispatch_/Regs //Dispatch_/Regs-par)
  (set! //Codes //Codes-par)
  (let ((//N-save //N))
   (set! //N (@Size (@Get_n (@I) 2)))
   (let ((//A/S_/Name (@V (@Get_n (@I) 1)))
         (//Starting_/Action-save //Starting_/Action)
         (//Bodies-save //Bodies)
         (//Names-save //Names)
         (//Name2/Num-save //Name2/Num)
         (//Return_/Regs-save //Return_/Regs)
         (//Dispatch_/Regs-save //Dispatch_/Regs)
         (//Succs-save //Succs)
         (//Preds-save //Preds)
         (//Entry_/Actions-save //Entry_/Actions)
         (/list '())
         (/posn-save /posn)
         (//Proc_/Defns-save //Proc_/Defns)
         (/a '())
         (/b-save /b)
         (/body-save /body)
         (/fringe '())
         (/tmpl '())
         (/ok 0)
         (/dn-save /dn)
         (/reg-save /reg)
         (/call-save /call)
         (/destination-save /destination)
         (//Found 0))
    (set! //Starting_/Action 0)
    (set! //Bodies (make-vector-eval //N '()))
    (set! //Names (make-vector-eval (+ //N 1) '()))
    (set! //Name2/Num (hash-table))
    (set! //Return_/Regs (hash-table))
    (set! //Dispatch_/Regs (hash-table))
    (set! //Succs (make-vector-eval (+ //N 1) '()))
    (set! //Preds (make-vector-eval (+ //N 1) '()))
    (set! //Entry_/Actions '())
    (set! /posn '())
    (set! //Proc_/Defns '())
    (set! /b '())
    (set! /body '())
    (set! /dn 0)
    (set! /reg '())
    (set! /call 0)
    (set! /destination (@Make_Name "destination"))
    ; Calculate Bodies, Names, Name2Num 
    (let ((/-result- (@FD_Init  //N //Bodies //Names //Name2/Num)))
     (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
     (set! //Names (car /-result-)) (set! /-result- (cdr /-result-))
     (set! //Name2/Num (car /-result-)) (set! /-result- (cdr /-result-)))
    ; Find the starting action and dispatch action number 
    (set! //Starting_/Action (gethash //Name2/Num //A/S_/Name))
    (set! /dn (gethash //Name2/Num (@Make_Name "dispatch")))
    (let ((/-result- (@FD_Succs_And_Preds  //N //Bodies //Starting_/Action //Succs //Preds)))
     (set! //Succs (car /-result-)) (set! /-result- (cdr /-result-))
     (set! //Preds (car /-result-)) (set! /-result- (cdr /-result-)))
    ; Repeat this loop while procs have been found: 
    (set! //Found 0)
    ; Calculate Entry_Actions list and Return_Regs table from the Codes table 
    (display-list "Looking for entry actions...")
    ; An entry action is an action whose CALL is preceded by rx := dest_code 
    (for /i 1 //N 1 
     (cond
      ((not (null? (wsl-ref //Bodies /i)))
       (@New_Program (wsl-ref //Bodies /i))
       (@Ateach_Statement /foreach-fix_dispatch-2 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips)))))))
    (display-list "")
    ; A valid entry action must have exactly one Return_Regs entry: 
    (let ((/deletes '()))
     (for-in /a //Entry_/Actions 
      (cond
       ((not (= (gen-length (gethash //Return_/Regs /a)) 1))
        (display-list "Return_Regs for " (@N_String (wsl-ref //Names /a)) " = " (my-map @N_String (gethash //Return_/Regs /a)) " ... ignoring this action.")
        (set! /deletes (cons /a /deletes)))))
     (cond
      ((> /effort 0)
       ; Compute Dispatch_Regs for each action which calls dispatch 
       ; Calculate the return register or variable copied to destination 
       ; before each dispatch call in each action. 
       ; Also, check for a variable in which Return_Regs is saved 
       ; and add it to Return_Regs 
       (for /i 1 //N 1 
        (cond
         ((not (null? (wsl-ref //Bodies /i)))
          (@New_Program (wsl-ref //Bodies /i))
          (@Ateach_Statement /foreach-fix_dispatch-3 0 (@AS_Type) 0)
          (cond
           ((null? (@Program))
            (@New_Program (@Skips))))
          ; Check for saved return register (other than destination!) 
          (cond
           ((= (gen-length (gethash //Return_/Regs /i)) 1)
            (let ((/reg-save /reg))
             (set! /reg (car (gethash //Return_/Regs /i)))
             (for-in //S (@Cs (@I)) 
              (cond
               ((and (= (@ST //S) //T_/Assignment) (= (@ST (@Get_n (@Get_n //S 1) 2)) //T_/Variable) (equal? (@V (@Get_n (@Get_n //S 1) 2)) /reg) (= (@ST (@Get_n (@Get_n //S 1) 1)) //T_/Var_/Lvalue) (not (equal? (@V (@Get_n (@Get_n //S 1) 1)) /destination)))
                (puthash //Return_/Regs /i (union-n (gethash //Return_/Regs /i) (list (@V (@Get_n (@Get_n //S 1) 1))))))))
             (set! /reg /reg-save))))
          ; Return_Regs and Dispatch_Regs are complete 
         )))))
     (cond
      ((= /effort 0)
       ; Remove A_xxx actions from consideration, at least until 
       ; the first constant propagation. Consider the sequence 
       ; L Rx,=A(code_label) / BR Rx 
       (for-in /a //Entry_/Actions 
        (cond
         ((@Starts_With? (@N_String (wsl-ref //Names /a)) "A_")
          (set! /deletes (cons /a /deletes)))))))
     (cond
      ((not (null? /deletes))
       (set! //Entry_/Actions (@Set_Difference (@Make_Set //Entry_/Actions) (@Make_Set /deletes))))))
    (display-list "Checking which entry actions are simple procedures...")
    ; Process entry actions in reverse order 
    (for-in /a (reverse //Entry_/Actions) 
     (cond
      ((not (null? (wsl-ref //Bodies /a)))
       (display-list-flush "Checking entry: " (@N_String (wsl-ref //Names /a)) ":")
       ; Find body of action by constructing the transitive closure 
       (set! /body (list /a))
       (set! /fringe (@Set_Difference (wsl-ref //Succs /a) (@Make_Set (list /a (+ //N 1) /dn))))
       (while (not (null? /fringe)) 
        (begin
         (set! /b (car /fringe))
         (set! /fringe (cdr /fringe))
         ; Don't include a call to another entry action 
         ; with the same return register: 
         (cond
          ((and (not (= /b (+ //N 1))) (not (equal? /b /dn)) (not (and (> /effort 0) (member /b //Entry_/Actions) (equal? (gethash //Return_/Regs /b) (gethash //Return_/Regs /a)))))
           ; Add b to body and sucessors of b to fringe 
           ; NB b's body might be empty because it has 
           ; been turned into a PROC 
           (cond
            ((not (null? (wsl-ref //Bodies /b)))
             (set! /body (union-n (list /b) /body))
             (display-list-flush " " (@N_String (wsl-ref //Names /b)))
             (set! /fringe (union-n /fringe (@Set_Difference (@Set_Difference (wsl-ref //Succs /b) (@Make_Set (list (+ //N 1) /dn))) /body)))))))))
       (display-list "")
       ; If effort >= 1 then convert all `external' actions 
       ; into tail-recursive calls 
       ; but make sure that dispatch is still called from the remaining body 
       (cond
        ((>= /effort 1)
         (set! /body (@FD_Remove_Ext_Calls  /a /dn //N //Succs //Preds //Names /body))))
       ; Check that no action outside the body calls an action in the body 
       ; other than the entry action, ie predecessors of all actions in 
       ; the body are also in body 
       (set! /ok 1)
       (set! /tmpl (@Set_Difference /body (list /a)))
       (while (and (not (null? /tmpl)) (= /ok 1)) 
        (begin
         (set! /b (car /tmpl))
         (set! /tmpl (cdr /tmpl))
         (cond
          ((equal? /b //Starting_/Action)
           (display-list "Procedure body includes the starting action " (@N_String (wsl-ref //Names /b)))
           (set! /ok 0))
          ((not (@Set_Subset? (wsl-ref //Preds /b) (@Make_Set (cons /dn (cons (+ //N 1) /body)))))
           (display-list "  " (@N_String (wsl-ref //Names /b)) " is called by " (@N_String (wsl-ref //Names (car (@Set_Difference (wsl-ref //Preds /b) (@Make_Set (cons /dn (cons (+ //N 1) /body))))))))
           (set! /ok 0)))))
       (cond
        ((= /ok 1)
         (set! /ok 0)
         (let ((/-result- (@FD_Process_Simple_Proc  /a /body /dn //N //Names //Return_/Regs //Bodies //Proc_/Defns /ok)))
          (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
          (set! //Proc_/Defns (car /-result-)) (set! /-result- (cdr /-result-))
          (set! /ok (car /-result-)) (set! /-result- (cdr /-result-)))
         (cond
          ((= /ok 1)
           (let ((/-result- (@FD_Succs_And_Preds  //N //Bodies //Starting_/Action //Succs //Preds)))
            (set! //Succs (car /-result-)) (set! /-result- (cdr /-result-))
            (set! //Preds (car /-result-)) (set! /-result- (cdr /-result-)))
           (set! //Found 1))))))))
    (while (not (= //Found 0)) 
     (begin
      (set! //Found 0)
      ; Calculate Entry_Actions list and Return_Regs table from the Codes table 
      (display-list "Looking for entry actions...")
      ; An entry action is an action whose CALL is preceded by rx := dest_code 
      (for /i 1 //N 1 
       (cond
        ((not (null? (wsl-ref //Bodies /i)))
         (@New_Program (wsl-ref //Bodies /i))
         (@Ateach_Statement /foreach-fix_dispatch-4 0 (@AS_Type) 0)
         (cond
          ((null? (@Program))
           (@New_Program (@Skips)))))))
      (display-list "")
      ; A valid entry action must have exactly one Return_Regs entry: 
      (let ((/deletes '()))
       (for-in /a //Entry_/Actions 
        (cond
         ((not (= (gen-length (gethash //Return_/Regs /a)) 1))
          (display-list "Return_Regs for " (@N_String (wsl-ref //Names /a)) " = " (my-map @N_String (gethash //Return_/Regs /a)) " ... ignoring this action.")
          (set! /deletes (cons /a /deletes)))))
       (cond
        ((> /effort 0)
         ; Compute Dispatch_Regs for each action which calls dispatch 
         ; Calculate the return register or variable copied to destination 
         ; before each dispatch call in each action. 
         ; Also, check for a variable in which Return_Regs is saved 
         ; and add it to Return_Regs 
         (for /i 1 //N 1 
          (cond
           ((not (null? (wsl-ref //Bodies /i)))
            (@New_Program (wsl-ref //Bodies /i))
            (@Ateach_Statement /foreach-fix_dispatch-5 0 (@AS_Type) 0)
            (cond
             ((null? (@Program))
              (@New_Program (@Skips))))
            ; Check for saved return register (other than destination!) 
            (cond
             ((= (gen-length (gethash //Return_/Regs /i)) 1)
              (let ((/reg-save /reg))
               (set! /reg (car (gethash //Return_/Regs /i)))
               (for-in //S (@Cs (@I)) 
                (cond
                 ((and (= (@ST //S) //T_/Assignment) (= (@ST (@Get_n (@Get_n //S 1) 2)) //T_/Variable) (equal? (@V (@Get_n (@Get_n //S 1) 2)) /reg) (= (@ST (@Get_n (@Get_n //S 1) 1)) //T_/Var_/Lvalue) (not (equal? (@V (@Get_n (@Get_n //S 1) 1)) /destination)))
                  (puthash //Return_/Regs /i (union-n (gethash //Return_/Regs /i) (list (@V (@Get_n (@Get_n //S 1) 1))))))))
               (set! /reg /reg-save))))
            ; Return_Regs and Dispatch_Regs are complete 
           )))))
       (cond
        ((= /effort 0)
         ; Remove A_xxx actions from consideration, at least until 
         ; the first constant propagation. Consider the sequence 
         ; L Rx,=A(code_label) / BR Rx 
         (for-in /a //Entry_/Actions 
          (cond
           ((@Starts_With? (@N_String (wsl-ref //Names /a)) "A_")
            (set! /deletes (cons /a /deletes)))))))
       (cond
        ((not (null? /deletes))
         (set! //Entry_/Actions (@Set_Difference (@Make_Set //Entry_/Actions) (@Make_Set /deletes))))))
      (display-list "Checking which entry actions are simple procedures...")
      ; Process entry actions in reverse order 
      (for-in /a (reverse //Entry_/Actions) 
       (cond
        ((not (null? (wsl-ref //Bodies /a)))
         (display-list-flush "Checking entry: " (@N_String (wsl-ref //Names /a)) ":")
         ; Find body of action by constructing the transitive closure 
         (set! /body (list /a))
         (set! /fringe (@Set_Difference (wsl-ref //Succs /a) (@Make_Set (list /a (+ //N 1) /dn))))
         (while (not (null? /fringe)) 
          (begin
           (set! /b (car /fringe))
           (set! /fringe (cdr /fringe))
           ; Don't include a call to another entry action 
           ; with the same return register: 
           (cond
            ((and (not (= /b (+ //N 1))) (not (equal? /b /dn)) (not (and (> /effort 0) (member /b //Entry_/Actions) (equal? (gethash //Return_/Regs /b) (gethash //Return_/Regs /a)))))
             ; Add b to body and sucessors of b to fringe 
             ; NB b's body might be empty because it has 
             ; been turned into a PROC 
             (cond
              ((not (null? (wsl-ref //Bodies /b)))
               (set! /body (union-n (list /b) /body))
               (display-list-flush " " (@N_String (wsl-ref //Names /b)))
               (set! /fringe (union-n /fringe (@Set_Difference (@Set_Difference (wsl-ref //Succs /b) (@Make_Set (list (+ //N 1) /dn))) /body)))))))))
         (display-list "")
         ; If effort >= 1 then convert all `external' actions 
         ; into tail-recursive calls 
         ; but make sure that dispatch is still called from the remaining body 
         (cond
          ((>= /effort 1)
           (set! /body (@FD_Remove_Ext_Calls  /a /dn //N //Succs //Preds //Names /body))))
         ; Check that no action outside the body calls an action in the body 
         ; other than the entry action, ie predecessors of all actions in 
         ; the body are also in body 
         (set! /ok 1)
         (set! /tmpl (@Set_Difference /body (list /a)))
         (while (and (not (null? /tmpl)) (= /ok 1)) 
          (begin
           (set! /b (car /tmpl))
           (set! /tmpl (cdr /tmpl))
           (cond
            ((equal? /b //Starting_/Action)
             (display-list "Procedure body includes the starting action " (@N_String (wsl-ref //Names /b)))
             (set! /ok 0))
            ((not (@Set_Subset? (wsl-ref //Preds /b) (@Make_Set (cons /dn (cons (+ //N 1) /body)))))
             (display-list "  " (@N_String (wsl-ref //Names /b)) " is called by " (@N_String (wsl-ref //Names (car (@Set_Difference (wsl-ref //Preds /b) (@Make_Set (cons /dn (cons (+ //N 1) /body))))))))
             (set! /ok 0)))))
         (cond
          ((= /ok 1)
           (set! /ok 0)
           (let ((/-result- (@FD_Process_Simple_Proc  /a /body /dn //N //Names //Return_/Regs //Bodies //Proc_/Defns /ok)))
            (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-))
            (set! //Proc_/Defns (car /-result-)) (set! /-result- (cdr /-result-))
            (set! /ok (car /-result-)) (set! /-result- (cdr /-result-)))
           (cond
            ((= /ok 1)
             (let ((/-result- (@FD_Succs_And_Preds  //N //Bodies //Starting_/Action //Succs //Preds)))
              (set! //Succs (car /-result-)) (set! /-result- (cdr /-result-))
              (set! //Preds (car /-result-)) (set! /-result- (cdr /-result-)))
             (set! //Found 1))))))))))
    (@FD_Rebuild_AS //N //Bodies //Names //A/S_/Name //Proc_/Defns)
    (set! //Starting_/Action //Starting_/Action-save)
    (set! //Bodies //Bodies-save)
    (set! //Names //Names-save)
    (set! //Name2/Num //Name2/Num-save)
    (set! //Return_/Regs //Return_/Regs-save)
    (set! //Dispatch_/Regs //Dispatch_/Regs-save)
    (set! //Succs //Succs-save)
    (set! //Preds //Preds-save)
    (set! //Entry_/Actions //Entry_/Actions-save)
    (set! /posn /posn-save)
    (set! //Proc_/Defns //Proc_/Defns-save)
    (set! /b /b-save)
    (set! /body /body-save)
    (set! /dn /dn-save)
    (set! /reg /reg-save)
    (set! /call /call-save)
    (set! /destination /destination-save))
   (set! //N //N-save))
  (set! //Dispatch_/Regs //Dispatch_/Regs-save)
  (set! //Codes //Codes-save)))

(define (@FD_Remove_Ext_Calls /start-par /dn-par //N-par //Succs-par //Preds-par //Names-par /body-par)
 (let ((/body-save /body)
       (//Names-save //Names)
       (//Preds-save //Preds)
       (//Succs-save //Succs)
       (//N-save //N)
       (/dn-save /dn)
       (/start-save /start)
       (funct-result '()))
  (set! /body /body-par)
  (set! //Names //Names-par)
  (set! //Preds //Preds-par)
  (set! //Succs //Succs-par)
  (set! //N //N-par)
  (set! /dn /dn-par)
  (set! /start /start-par)
  (let ((/new-save /new)
        (/fringe '())
        (/done '())
        (/tmpl '())
        (/c-save /c)
        (/ok 1))
   (set! /new (@Make_Set /body))
   (set! /c '())
   (set! /tmpl (@Set_Difference (@Set_Difference (@Make_Set /body) (list /start)) (list /dn)))
   ; Start with direct calls: 
   (set! /tmpl (concat (@Set_Difference (wsl-ref //Succs /start) (list /dn)) (@Make_Set (@Set_Difference (@Set_Difference (@Set_Difference /body (list /start)) (list /dn)) (wsl-ref //Succs /start)))))
   (display-list "@FD_Remove_Ext_Calls on " (@N_String (wsl-ref //Names /start)) " ok = " /ok)
   (display-list-flush "tmpl = ")
   (for-in /x /tmpl 
    (display-list-flush (@N_String (wsl-ref //Names /x)) " "))
   (display-list "")
   (while (and (not (null? /tmpl)) (= /ok 1)) 
    (begin
     (set! /b (car /tmpl))
     (set! /tmpl (cdr /tmpl))
     (display-list "...checking body action " (@N_String (wsl-ref //Names /b)))
     ; If b is called elsewhere, then remove it and all its successors 
     ; Or if b calls dispatch in a code which comes from some variable 
     ; other than the return register or a variable it is saved in: 
     (cond
      ((member /b /done)
       #t)
      ((or (and (not (null? (gethash //Return_/Regs /start))) (not (@Set_Subset? (gethash //Dispatch_/Regs /b) (gethash //Return_/Regs /start)))) (not (@Set_Subset? (@Set_Difference (wsl-ref //Preds /b) (list /dn)) (@Make_Set (cons (+ //N 1) /body)))))
       (display-list "  Found external call " (@N_String (wsl-ref //Names /b)) " in action " (@N_String (wsl-ref //Names /start)))
       (cond
        ((member /b //Entry_/Actions)
         (display-list "Not taking out entry action " (@N_String (wsl-ref //Names /b)))
         (set! /ok 0)))
       (set! /fringe (list /b))
       (set! /done (@Make_Set (list /start /dn (+ //N 1))))
       (while (and (not (null? /fringe)) (= /ok 1)) 
        (begin
         (set! /c (car /fringe))
         (set! /fringe (cdr /fringe))
         (display-list "  ...removing " (@N_String (wsl-ref //Names /c)))
         (cond
          ((and (member /c //Entry_/Actions) (< /effort 2))
           (display-list "Not taking out entry action " (@N_String (wsl-ref //Names /c)))
           (set! /ok 0)))
         (set! /new (@Set_Difference /new (list /c)))
         (set! /done (union-n (list /c) /done))
         (set! /fringe (union-n /fringe (@Set_Difference (wsl-ref //Succs /c) /done)))))))))
   (display-list "  ... ok = " /ok " new body = " /new)
   (cond
    ((and (< (gen-length /new) (gen-length /body)) (= /ok 1))
     ; We removed an external call, so check that dispatch is called: 
     (set! /ok 0)
     (for-in /b /new 
      (cond
       ((member /dn (wsl-ref //Succs /b))
        (set! /ok 1))))
     (cond
      ((= /ok 0)
       (display-list "  dispatch not called after deleting external calls."))
      (#t
       (set! /body (cons /start (@Set_Difference /new (list /start))))
       (display-list "   new body is: " /body)))))
   (set! /new /new-save)
   (set! /c /c-save))
  (set! funct-result /body)
  (set! /body /body-save)
  (set! //Names //Names-save)
  (set! //Preds //Preds-save)
  (set! //Succs //Succs-save)
  (set! //N //N-save)
  (set! /dn /dn-save)
  (set! /start /start-save)
  funct-result))

(define (@FD_Init //N-par //Bodies-par //Names-par //Name2/Num-par)
 (let ((//Name2/Num-save //Name2/Num)
       (//Names-save //Names)
       (//Bodies-save //Bodies)
       (//N-save //N)
       (funct-result '()))
  (set! //Name2/Num //Name2/Num-par)
  (set! //Names //Names-par)
  (set! //Bodies //Bodies-par)
  (set! //N //N-par)
  (let ((//J-save //J)
        (//A/S/Type (@AS_Type)))
   (set! //J 0)
   ; Calculate Bodies, Names, Name2Num 
   ; Hash table Name2Num maps action names (keys) to action numbers 
   (puthash //Name2/Num (@Make_Name "Z") (+ //N 1))
   (display-list //N " actions")
   (@Down_Last)
   (@Down)
   (set! //A/S/Type (@AS_Type))
   ; Put bodies of actions into array Bodies, 
   ; and their names into the hash table Name2Num. 
   (set! //J 1)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (wsl-set! //Names (@V (@Get_n (@I) 1)) //J)
     (cond
      ((not (null? (gethash //Name2/Num (wsl-ref //Names //J))))
       (error (string-append (string-append "@FD_Init: Duplicate name " (@N_String (@V (@Get_n (@I) 1)))) " found in action system!!!"))))
     (puthash //Name2/Num (wsl-ref //Names //J) //J)
     (wsl-set! //Bodies (@Get_n (@I) 2) //J)
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (set! //J (+ //J 1))
       (@Right)
       (set! /fl_flag1 0)))))
   (wsl-set! //Names (@Make_Name "Z") (+ //N 1))
   ; Convert all the calls to numbers using Name2Num 
   (for /i 1 //N 1 
    (begin
     (@New_Program (wsl-ref //Bodies /i))
     (@Foreach_Non_Action_Statement /foreach-fix_dispatch-6 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (wsl-set! //Bodies (@Program) /i)))
   (set! //J //J-save))
  (set! funct-result (list //Bodies //Names //Name2/Num))
  (set! //Name2/Num //Name2/Num-save)
  (set! //Names //Names-save)
  (set! //Bodies //Bodies-save)
  (set! //N //N-save)
  funct-result))

; Calculate Succs and Preds arrays. 
; NB: the calls in Bodies have numbers instead of names 
(define (@FD_Succs_And_Preds //N-par //Bodies-par //Starting_/Action-par //Succs-par //Preds-par)
 (let ((//Preds-save //Preds)
       (//Succs-save //Succs)
       (//Starting_/Action-save //Starting_/Action)
       (//Bodies-save //Bodies)
       (//N-save //N)
       (funct-result '()))
  (set! //Preds //Preds-par)
  (set! //Succs //Succs-par)
  (set! //Starting_/Action //Starting_/Action-par)
  (set! //Bodies //Bodies-par)
  (set! //N //N-par)
  (display-list-flush "Calculating Succs and Preds... ")
  (let ((/calls '())
        (/call-save /call))
   (set! /call 0)
   (for /i 1 //N 1 
    (wsl-set! //Preds '() /i))
   (for /i 1 //N 1 
    (begin
     (wsl-set! //Succs '() /i)
     (cond
      ((not (null? (wsl-ref //Bodies /i)))
       (set! /calls (@Calls (wsl-ref //Bodies /i)))
       ; Returns a list of <call, number> pairs 
       (while (not (null? /calls)) 
        (begin
         (set! /call (- (car (car /calls))))
         (set! /calls (cdr /calls))
         (cond
          ((not (number? /call))
           (display-list "Check me: " /call)
           (set! /call (+ //N 1))))
         (wsl-set! //Succs (union-n (list /call) (wsl-ref //Succs /i)) /i)
         (wsl-set! //Preds (union-n (wsl-ref //Preds /call) (list /i)) /call)))))))
   (set! /call /call-save))
  (display-list "Done.")
  (set! funct-result (list //Succs //Preds))
  (set! //Preds //Preds-save)
  (set! //Succs //Succs-save)
  (set! //Starting_/Action //Starting_/Action-save)
  (set! //Bodies //Bodies-save)
  (set! //N //N-save)
  funct-result))

(define (@FD_Rebuild_AS //N-par //Bodies-par //Names-par //A/S_/Name //Proc_/Defns-par)
 (let ((//Proc_/Defns-save //Proc_/Defns)
       (//Names-save //Names)
       (//Bodies-save //Bodies)
       (//N-save //N))
  (set! //Proc_/Defns //Proc_/Defns-par)
  (set! //Names //Names-par)
  (set! //Bodies //Bodies-par)
  (set! //N //N-par)
  ; Rebuild the action system from Bodies, surrounding it with the Proc_Defns 
  (display-list "Rebuilding the action system.")
  (let ((//Dispatch-save //Dispatch)
        (/actions '())
        (/i-save /i))
   (set! //Dispatch (@Make_Name "dispatch"))
   (set! /i 0)
   ; NB change the calls back to names 
   (for /i //N 1 (- 1) 
    (cond
     ((not (null? (wsl-ref //Bodies /i)))
      (@New_Program (wsl-ref //Bodies /i))
      (@Foreach_Non_Action_Statement /foreach-fix_dispatch-7 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (set! /actions (cons (@Make //T_/Action '() (list (@Name (wsl-ref //Names /i)) (@Program))) /actions)))))
   (display-list-flush "There are " (gen-length /actions) " actions in the main system")
   (cond
    ((null? //Proc_/Defns)
     (display-list "."))
    (#t
     (display-list " and " (gen-length //Proc_/Defns) " procedure(s).")))
   ; Build the action system 
   (@New_Program (@Make //T_/A_/S '() (list (@Name //A/S_/Name) (@Make //T_/Actions '() /actions))))
   (cond
    ((not (null? //Proc_/Defns))
     ; Put the action system in a WHERE clause 
     (let ((/body-save /body))
      (set! /body (@Make //T_/Statements '() (list (@Program))))
      (@New_Program (@Make //T_/Where '() (list /body (@Make //T_/Definitions '() //Proc_/Defns))))
      (set! /body /body-save))))
   (set! //Dispatch //Dispatch-save)
   (set! /i /i-save))
  (set! //Proc_/Defns //Proc_/Defns-save)
  (set! //Names //Names-save)
  (set! //Bodies //Bodies-save)
  (set! //N //N-save)))

(define (@FD_Process_Simple_Proc /start-par /body-par /dn-par //N-par //Names-par //Return_/Regs-par //Bodies-par //Proc_/Defns-par //O/K-par)
 (let ((//O/K-save //O/K)
       (//Proc_/Defns-save //Proc_/Defns)
       (//Bodies-save //Bodies)
       (//Return_/Regs-save //Return_/Regs)
       (//Names-save //Names)
       (//N-save //N)
       (/dn-save /dn)
       (/body-save /body)
       (/start-save /start)
       (funct-result '()))
  (set! //O/K //O/K-par)
  (set! //Proc_/Defns //Proc_/Defns-par)
  (set! //Bodies //Bodies-par)
  (set! //Return_/Regs //Return_/Regs-par)
  (set! //Names //Names-par)
  (set! //N //N-par)
  (set! /dn /dn-par)
  (set! /body /body-par)
  (set! /start /start-par)
  ; Convert the list of actions in body to a nested action system which replaces 
  ; action start. Convert the nested action system to a PROC. 
  ; Check that the resulting action preserves its return register(s). 
  ; Unfold start action everywhere and attempt to simplify any calls to dispatch 
  (let ((/sub_/A/S '())
        (/new_sub_/A/S '())
        (/proc_name (wsl-ref //Names /start))
        (/i-save /i)
        (/n 0)
        (/call_/Z-save /call_/Z)
        (/call_zn-save /call_zn)
        (/call_dn-save /call_dn)
        (/call-save /call)
        (//R 0)
        (/reg-save /reg)
        (/reg_var-save /reg_var)
        (/flag-save /flag)
        (/flag_n-save /flag_n)
        (/flag_val-save /flag_val)
        (/name2flag-save /name2flag)
        (/flag2name-save /flag2name)
        (//Z_called-save //Z_called)
        (/dispatch_called-save /dispatch_called)
        (/guardeds '())
        (/v-save /v)
        (/rr-save /rr))
   (set! /i 0)
   (set! /call_/Z (@Make //T_/Call (@Make_Name "Z") '()))
   (set! /call_zn (@Make //T_/Call (- (+ //N 1)) '()))
   (set! /call_dn (@Make //T_/Call (- /dn) '()))
   (set! /call '())
   (set! /reg (car (gethash //Return_/Regs /start)))
   (set! /reg_var '())
   (set! /flag (@Make //T_/Variable (@Make_Name "exit_flag") '()))
   (set! /flag_n 1)
   (set! /flag_val '())
   (set! /name2flag (hash-table))
   (set! /flag2name (hash-table))
   (set! //Z_called 0)
   (set! /dispatch_called 0)
   (set! /v 0)
   (set! /rr 0)
   (set! /reg_var (@Make //T_/Variable /reg '()))
   ; Build sub_AS action list from the list of action numbers in body: 
   (for-in /i /body 
    (cond
     ((null? (wsl-ref //Bodies /i))
      (display-list "Warning: action " (@N_String (wsl-ref //Names /i)) " already has no body!"))
     (#t
      (set! /sub_/A/S (cons (@Make //T_/Action '() (list (@Name (wsl-ref //Names /i)) (wsl-ref //Bodies /i))) /sub_/A/S)))))
   ; Replace external calls in sub_AS by an assignment to a flag followed by CALL Z 
   ; Restore internal calls (ie replace the number by the name) 
   (set! /new_sub_/A/S '())
   (while (not (null? /sub_/A/S)) 
    (begin
     (@New_Program (car /sub_/A/S))
     (set! /sub_/A/S (cdr /sub_/A/S))
     (@Down_Last)
     ; To the statement sequence 
     (@Foreach_Non_Action_Statement /foreach-fix_dispatch-8 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! /new_sub_/A/S (cons (@Program) /new_sub_/A/S))))
   (set! /sub_/A/S (@Make //T_/A_/S '() (list (@Name (wsl-ref //Names /start)) (@Make //T_/Actions '() /new_sub_/A/S))))
   ; Check that sub_AS sends its return register to destination 
   (cond
    ((and (= /dispatch_called 0) (= /flag_n 1))
     (set! //R 1))
    (#t
     (set! //R (@Preserves_Destination /proc_name /sub_/A/S /reg /effort /budget /flag_n))))
   (cond
    ((and (= //R 0) (> /effort 0) (= (@Size (@Get_n /sub_/A/S 2)) 1) (<= (@Stat_Count_NC /sub_/A/S) 7))
     ; Unfold the start action everywhere: 
     (let ((/body-save /body))
      (set! /body (wsl-ref //Bodies /start))
      (wsl-set! //Bodies '() /start)
      (display-list "Unfolding small action " (@N_String (wsl-ref //Names /start)))
      (for /i 1 //N 1 
       (cond
        ((not (null? (wsl-ref //Bodies /i)))
         (@New_Program (wsl-ref //Bodies /i))
         (@Foreach_Non_Action_Statement /foreach-fix_dispatch-9 0 (@AS_Type) 0)
         (cond
          ((null? (@Program))
           (@New_Program (@Skips))))
         (wsl-set! //Bodies (@Program) /i))))
      (set! /body /body-save))))
   (cond
    ((> //R 0)
     ; Success! 
     ; Clear the bodies of the removed actions: 
     (display-list "Found a simple procedure: " (@N_String (wsl-ref //Names /start)))
     (set! //O/K 1)
     (display-list-flush "Body consists of " (gen-length /body) " actions: ")
     (for /i 1 (gen-length /body) 1 
      (display-list-flush (@N_String (wsl-ref //Names (wsl-ref /body /i))) " "))
     (display-list "")
     (for-in /i /body 
      (wsl-set! //Bodies '() /i))
     ; The new version of the start action calls PROC proc_name and then 
     ; tests flag to decide whether to call Z or dispatch: 
     (@New_Program /%const__fix_dispatch__2)
     (@Down)
     (let ((/expns (@Make //T_/Expressions '() '()))
           (/lvalues (@Make //T_/Lvalues '() '())))
      (@Paste_Over (@Make //T_/Proc_/Call '() (list (@Name /proc_name) /expns /lvalues))))
     (cond
      ((and (= //Z_called 1) (= /dispatch_called 0))
       (@Paste_After /call_zn))
      ((= //Z_called 1)
       (@Paste_After (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /flag) (@Make 205 1 '()))) (@Make 17 '() (list /call_zn)))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list /call_dn))))))))
      (#t
       (@Paste_After /call_dn)))
     (set! /i /flag_n)
     (while (> /i 1) 
      (begin
       (set! /flag_val (@Make //T_/Number /i '()))
       (set! /call (@Make //T_/Call (- (gethash /flag2name /i)) '()))
       (@Paste_After (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /flag) (@Var_To_Expn /flag_val))) (@Make 17 '() (list /call)))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
       (set! /i (- /i 1))))
     (wsl-set! //Bodies (@Program) /start)
     ; Unfold the start action everywhere: 
     (let ((/-result- (@FD_Unfold_Start_Action  /start /reg (- //R 1) /flag_n /call_dn /call_zn /rr //Bodies)))
      (set! /rr (car /-result-)) (set! /-result- (cdr /-result-))
      (set! //Bodies (car /-result-)) (set! /-result- (cdr /-result-)))
     ; Create the proc defn, components are <lvalues, lvalues, statements> 
     (let ((/lvalues (@Make //T_/Lvalues '() '()))
           (/body-save /body))
      (set! /body (@Make //T_/Statements '() (list /sub_/A/S)))
      (@New_Program /body)
      (cond
       ((= /rr 0)
        ; All dispatch calls directly after each subroutine call 
        ; have been accounted for an unfolded. 
        ; So we can delete any assignments to destination in the body 
        (@Foreach_Statement /foreach-fix_dispatch-10 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))))
      (@Down)
      ; to action system 
      (cond
       ((@Trans? //T/R_/Merge_/Calls_/In_/System)
        (@Trans //T/R_/Merge_/Calls_/In_/System "")))
      (cond
       ((@Trans? //T/R_/Simplify_/Action_/System)
        (@Trans //T/R_/Simplify_/Action_/System "")))
      (cond
       ((and (= (@ST (@I)) //T_/A_/S) (= (@Size (@Get_n (@I) 2)) 1))
        (display-list "Removing action system: " (@N_String (@V (@Get_n (@I) 1))))
        (@Trans //T/R_/Simplify_/Item "")))
      (set! /body (@Program))
      (set! //Proc_/Defns (cons (@Make //T_/Proc '() (list (@Name /proc_name) /lvalues /lvalues /body)) //Proc_/Defns))
      ; Add a summary of the new body to Proc_Summaries: 
      (@S_Summarise_Body_Sub (car //Proc_/Defns))
      (set! /body /body-save))
     #t))
   (set! /i /i-save)
   (set! /call_/Z /call_/Z-save)
   (set! /call_zn /call_zn-save)
   (set! /call_dn /call_dn-save)
   (set! /call /call-save)
   (set! /reg /reg-save)
   (set! /reg_var /reg_var-save)
   (set! /flag /flag-save)
   (set! /flag_n /flag_n-save)
   (set! /flag_val /flag_val-save)
   (set! /name2flag /name2flag-save)
   (set! /flag2name /flag2name-save)
   (set! //Z_called //Z_called-save)
   (set! /dispatch_called /dispatch_called-save)
   (set! /v /v-save)
   (set! /rr /rr-save))
  (set! funct-result (list //Bodies //Proc_/Defns //O/K))
  (set! //O/K //O/K-save)
  (set! //Proc_/Defns //Proc_/Defns-save)
  (set! //Bodies //Bodies-save)
  (set! //Return_/Regs //Return_/Regs-save)
  (set! //Names //Names-save)
  (set! //N //N-save)
  (set! /dn /dn-save)
  (set! /body /body-save)
  (set! /start /start-save)
  funct-result))

; If inc = 1 then the return code may have been incremented: 
(define (@FD_Unfold_Start_Action /start-par /reg-par /inc-par /flag_n-par /call_dn-par /call_zn-par /rr-par //Bodies-par)
 (let ((//Bodies-save //Bodies)
       (/rr-save /rr)
       (/call_zn-save /call_zn)
       (/call_dn-save /call_dn)
       (/flag_n-save /flag_n)
       (/inc-save /inc)
       (/reg-save /reg)
       (/start-save /start)
       (funct-result '()))
  (set! //Bodies //Bodies-par)
  (set! /rr /rr-par)
  (set! /call_zn /call_zn-par)
  (set! /call_dn /call_dn-par)
  (set! /flag_n /flag_n-par)
  (set! /inc /inc-par)
  (set! /reg /reg-par)
  (set! /start /start-par)
  ; Unfold start everywhere: its new body consists of a PROC call 
  ; (whose body is the action system sub_AS) followed by a COND which contains 
  ; a call to dispatch. 
  (let ((/body-save /body)
        (/posn-save /posn)
        (/return-save /return)
        (/up-save /up))
   (set! /body (wsl-ref //Bodies /start))
   (set! /posn '())
   (set! /return '())
   (set! /up 0)
   (wsl-set! //Bodies '() /start)
   (display-list "Unfolding " (@N_String (wsl-ref //Names /start)) " inc = " /inc)
   (for /i 1 //N 1 
    (cond
     ((not (null? (wsl-ref //Bodies /i)))
      (@New_Program (wsl-ref //Bodies /i))
      (@Trans //T/R_/Simplify "")
      ; Check for a Floop followed by a CALL and absorb it into the loop. 
      ; (This helps to move the CALL closer to the return code) 
      (@Ateach_Statement /foreach-fix_dispatch-11 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (@Ateach_Non_Action_Statement /foreach-fix_dispatch-12 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (wsl-set! //Bodies (@Program) /i))))
   (display-list "")
   (set! /body /body-save)
   (set! /posn /posn-save)
   (set! /return /return-save)
   (set! /up /up-save))
  (set! funct-result (list /rr //Bodies))
  (set! //Bodies //Bodies-save)
  (set! /rr /rr-save)
  (set! /call_zn /call_zn-save)
  (set! /call_dn /call_dn-save)
  (set! /flag_n /flag_n-save)
  (set! /inc /inc-save)
  (set! /reg /reg-save)
  (set! /start /start-save)
  funct-result))

; Check if the return code is incremented by a known value (4, 8, 12, etc) 
; If so, then save and restore return register. 
; In not, then set rr := 1 so that we keep assingments to destination 
; in the extracted proc body. 
(define (@FD_Check_Return_Code_Inc /rr-par)
 (let ((/rr-save /rr)
       (funct-result '()))
  (set! /rr /rr-par)
  (cond
   ((and (= (gen-length //C/P_/Return_/Code_/Inc) 1) (not (= (car //C/P_/Return_/Code_/Inc) 1)) (= (@ST (@I)) //T_/Proc_/Call) (= //C/P_/Return_/Code_/Normal 0))
    (let ((/save (@Make //T_/Variable (@Make_Name (string-append (@N_String (@V (@Get_n (@I) 1))) "_RETURN")) '()))
          (/r (@Make //T_/Variable /reg '()))
          (/i-save /i)
          (/call-save /call))
     (set! /i (@Make //T_/Number (car //C/P_/Return_/Code_/Inc) '()))
     (set! /call (@I))
     (@Splice_Over (@Cs (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /save) (@Var_To_Expn /r))))) /call (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Make 220 '() (list (@Var_To_Expn /save) (@Var_To_Expn /i)))))))))))
     (set! /i /i-save)
     (set! /call /call-save)))
   (#t
    (set! /rr 1)))
  (set! funct-result /rr)
  (set! /rr /rr-save)
  funct-result))

; Create an IF statement which tests dispatch against the possible return codes: 
(define (@FD_Inc_Return_Code /code-par /call_dn-par /call_zn-par)
 (let ((/call_zn-save /call_zn)
       (/call_dn-save /call_dn)
       (/code-save /code))
  (set! /call_zn /call_zn-par)
  (set! /call_dn /call_dn-par)
  (set! /code /code-par)
  (let ((/offset (@Make //T_/Number (@V /code) '())))
   ; First move to the CALL dispatch (may be inside an IF flag test) 
   (@Right)
   ; Skip exit_flag > 1 tests 
   (while (and (= (@ST (@I)) //T_/Cond) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Equal) (@Equal? (@Get_n (@Get_n (@Get_n (@I) 1) 1) 1) /flag) (= (@ST (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) //T_/Number) (> (@V (@Get_n (@Get_n (@Get_n (@I) 1) 1) 2)) 1) (@Right?)) 
    (@Right))
   (cond
    ((= (@ST (@I)) //T_/Cond)
     (@Down_Last)
     (@Down_Last)
     (@Down_Last)))
   (@Find_Type //T_/Call)
   (cond
    ((not (= (@ST (@I)) //T_/Call))
     (display-list "Couldn't find the dispatch call!")
     (prit)
     (display-list "Whole program:")
     (prpr)
     (error "@FD_Inc_Return_Code")))
   (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list /call_zn)))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
   (@Down)
   ; to the first guarded 
   (while (not (null? (gethash //Code_/Hash (@V /code)))) 
    (begin
     (@Paste_Before (@Make 7 '() (list (@Make 313 '() (list (@Make 207 (@Make_Name "destination") '()) (@Var_To_Expn /offset))) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Var_To_Expn /code))))) /call_dn)))))
     (@Right)
     (set! /code (@Make //T_/Number (+ (@V /code) 4) '()))
     (set! /offset (@Make //T_/Number (+ (@V /offset) 4) '()))))
   ; Change the last test to TRUE: 
   (cond
    ((@Left?)
     (@Left)
     (@Down)
     (@Paste_Over /%const__fix_dispatch__3)
     (@Up)
     (@Delete_Rest))))
  (set! /call_zn /call_zn-save)
  (set! /call_dn /call_dn-save)
  (set! /code /code-save)))

; Search forwards up to n steps for a CALL and return it if found 
; Don't step over an assignment of a return code! 
(define (@FD_Find_Call /n //Codes-par)
 (let ((//Codes-save //Codes)
       (/posn-save /posn)
       (//R '())
       (funct-result '()))
  (set! //Codes //Codes-par)
  (set! /posn (@Posn))
  (while (and (@Up?) (not (= (@GT (@I)) //T_/Statement))) 
   (@Up))
  (cond
   ((= (@GT (@I)) //T_/Statement)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (let ((/__/O/K 1))
        (set! /__/O/K (@New_Match  /%const__fix_dispatch__4 (@I) /__/O/K))
        (cond
         ((= /__/O/K 1)
          (let ((/__e_save /e)
                (/__v_save /v))
           (set! /e (vector-ref /__/Match_array 1))
           (set! /v (vector-ref /__/Match_array 0))
           (cond
            ((or (and (= (@ST /e) //T_/Number) (not (null? (gethash //Codes (@V /e))))) (and (= (@ST /e) //T_/X_/Funct_/Call) (equal? (@V (@Get_n /e 1)) /inline_par)))
             ; Look no further for a CALL: 
             (set! /n 0)))
           (set! /e /__e_save)
           (set! /v /__v_save)))))
       (cond
        ((= /n 0)
         (set! /fl_flag1 1))
        ((= (@ST (@I)) //T_/Call)
         (set! //R (@V (@I)))
         (set! /fl_flag1 1))
        (#t
         (cond
          ((not (= (@ST (@I)) //T_/Comment))
           (set! /n (- /n 1))))
         (cond
          ((or (= (@ST (@I)) //T_/Cond) (= (@ST (@I)) //T_/Proc_/Call))
           (set! /fl_flag1 1))
          (#t
           (set! /fl_flag1 0))))))))))
  (@Goto /posn)
  (set! funct-result //R)
  (set! //Codes //Codes-save)
  (set! /posn /posn-save)
  funct-result))

; Overwrite the with a NOTUSED_ variable: 
(define (@FD_Clobber_Value)
 (let ((/val (@V (@I)))
       (/notused-save /notused))
  (set! /notused '())
  (cond
   ((< /val 0)
    (set! /notused (@Make //T_/Variable (@Make_Name (string-append "NOTUSED__" (@String (- /val)))) '())))
   (#t
    (set! /notused (@Make //T_/Variable (@Make_Name (string-append "NOTUSED_" (@String /val))) '()))))
  (@Paste_Over /notused)
  (set! /notused /notused-save)))

; Look for !XF bit_or(code, AMODE) and convert to code 
; (assume this is merely setting the mode) 
(define (@FD_Bit_Or_Code_Check //Codes-par)
 (let ((//Codes-save //Codes))
  (set! //Codes //Codes-par)
  (@Foreach_Expn /foreach-fix_dispatch-13 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //Codes //Codes-save)))

; Look for a[FOO].BAR[n..m] := reg or code plus 
; destination or register := a[FOO].BAR[n..m] 
; If both are found then rename to a_FOO_BAR_n 
; to assist with constant propagation. 
(define (@FD_Rename_DSECT_Code_Stores //Codes-par /rename-par)
 (let ((/rename-save /rename)
       (//Codes-save //Codes)
       (funct-result '()))
  (set! /rename /rename-par)
  (set! //Codes //Codes-par)
  (let ((//L1-save //L1)
        (//L2-save //L2)
        (/name-save /name)
        (//D/S/E/C/Ts-save //D/S/E/C/Ts)
        (//D/S/E/C/T_regs-save //D/S/E/C/T_regs)
        (/registers-save /registers)
        (/new-save /new))
   (set! //L1 '())
   (set! //L2 '())
   (set! /name '())
   (set! //D/S/E/C/Ts (hash-table))
   (set! //D/S/E/C/T_regs (hash-table))
   (set! /registers (@Make_Set (my-map @Make_Name (list "a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9" "a10" "a11" "a12" "a13" "a14" "a15" "x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "x10" "x11" "x12" "x13" "x14" "x15" "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15" "destination"))))
   (set! /new (hash-table))
   (set! //D/S/E/C/Ts (@CP_Find_DSECTs  //D/S/E/C/Ts))
   (@Foreach_Statement /foreach-fix_dispatch-14 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /v (@Hash_Keys //D/S/E/C/T_regs) 
    (display-list "Regs for " (@N_String /v) " = " (my-map @N_String (gethash //D/S/E/C/T_regs /v))))
   (@Foreach_Statement /foreach-fix_dispatch-15 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /elts (intersection-n //L1 //L2) 
    (begin
     (set! /name "")
     (cond
      ((and (> (gen-length /elts) 1) (<= (gen-length (gethash //D/S/E/C/T_regs (wsl-ref /elts 2))) 1))
       (for-in /elt /elts 
        (begin
         (cond
          ((not (equal? /name ""))
           (set! /name (string-append /name "_"))))
         (cond
          ((> /elt 0)
           (set! /name (concat /name (@N_String /elt))))
          (#t
           (set! /name (concat /name (@String (- /elt))))))))
       (set! /name (@Make_Name /name))
       (puthash /new /elts /name)))))
   (@Foreach_Expn /foreach-fix_dispatch-16 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Lvalue /foreach-fix_dispatch-17 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   #t
   (set! //L1 //L1-save)
   (set! //L2 //L2-save)
   (set! /name /name-save)
   (set! //D/S/E/C/Ts //D/S/E/C/Ts-save)
   (set! //D/S/E/C/T_regs //D/S/E/C/T_regs-save)
   (set! /registers /registers-save)
   (set! /new /new-save))
  (set! funct-result /rename)
  (set! /rename /rename-save)
  (set! //Codes //Codes-save)
  funct-result))

(define (@FD_Restore_DSECT_Code_Stores /rename-par)
 (let ((/rename-save /rename))
  (set! /rename /rename-par)
  (@Foreach_Expn /foreach-fix_dispatch-18 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Lvalue /foreach-fix_dispatch-19 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /rename /rename-save)))

; Find the destination values 
(define (@Find_Dispatch_Codes /dispatch-par)
 (let ((/dispatch-save /dispatch)
       (/codes '())
       (/destination-save /destination)
       (/largest 0)
       (/regs '())
       (/tmp '())
       (funct-result '()))
  (set! /dispatch /dispatch-par)
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
  (set! /destination /destination-save)
  funct-result))

(define (@FD_Unfold_Dispatch /code-par /call_dn-par //Bodies-par)
 (let ((//Bodies-save //Bodies)
       (/call_dn-save /call_dn)
       (/code-save /code))
  (set! //Bodies //Bodies-par)
  (set! /call_dn /call_dn-par)
  (set! /code /code-par)
  (let ((/body-save /body)
        (/destination-save /destination))
   (set! /body (@Cs (wsl-ref //Bodies (- (@V /call_dn)))))
   (set! /destination (@Make_Name "destination"))
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (@Foreach_Statement /foreach-fix_dispatch-21 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))
   (@Up)
   (@Trans //T/R_/Simplify "")
   (set! /body /body-save)
   (set! /destination /destination-save))
  (set! //Bodies //Bodies-save)
  (set! /call_dn /call_dn-save)
  (set! /code /code-save)))

; Look for a variable which is only assigned either zero 
; or two or more different dispatch codes 
; Replace a dispatch on this variable (maybe indirectly via a register) 
; by a jump table on small integers. 
; Replace the dispatch codes by integers so that dispatch can be pruned. 
(define (@FD_Jump_Table_Fix //Code_/Hash-par)
 (let ((//Code_/Hash-save //Code_/Hash))
  (set! //Code_/Hash //Code_/Hash-par)
  (let ((/addr_vars-save /addr_vars)
        (//Registers-save //Registers)
        (/destination-save /destination)
        (/dispatch-save /dispatch)
        (/posn-save /posn))
   (set! /addr_vars (hash-table))
   (set! //Registers (my-map @Make_Name (list "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15")))
   (set! /destination (@Make_Name "destination"))
   (set! /dispatch (@Make_Name "dispatch"))
   (set! /posn (@Posn))
   (@Goto '())
   (@Foreach_Statement /foreach-fix_dispatch-22 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; Remove bad vars (which are assigned other values) 
   (@Foreach_Statement /foreach-fix_dispatch-23 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Goto /posn)
   (display-list-flush "Jump Table Variables are:")
   (for-in /var (@Hash_Keys /addr_vars) 
    (cond
     ((> (gen-length (gethash /addr_vars /var)) 1)
      (display-list-flush " " (@N_String /var)))))
   (display-list "")
   ; Search for dispatches on addr_vars 
   (@Ateach_Statement /foreach-fix_dispatch-24 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /addr_vars /addr_vars-save)
   (set! //Registers //Registers-save)
   (set! /destination /destination-save)
   (set! /dispatch /dispatch-save)
   (set! /posn /posn-save))
  (set! //Code_/Hash //Code_/Hash-save)))

; Move left and up past comments and cc assignments to the previous statement 
; Skip assignments of the form var := 0 
(define (@FD_Move_To_Assignment)
 (let ((/cc_name (@Make_Name "cc"))
       (/done 0))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (cond
     ((and (not (@Left?)) (@Up?))
      (@Up)
      (while (and (@Up?) (not (= (@GT (@I)) //T_/Statement))) 
       (@Up))))
    (cond
     ((not (@Left?))
      (set! /fl_flag1 1))
     (#t
      (@Left)
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__fix_dispatch__7 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__var_save /var))
          (set! /var (vector-ref /__/Match_array 0))
          (set! /var /__var_save)))
        ((and (not (= //T_/Comment (@ST (@I)))) (not (@Gen_Proper? (@I) "Reg")))
         (set! /done 1))
        ((and (not (null? (intersection-n (@Stat_Types (@I)) //Call_/Types_/Set))) (not (= //T_/Comment (@ST (@I)))))
         (set! /done 1))
        ((and (not (equal? (@Assigned (@I)) (list /cc_name))) (not (= //T_/Comment (@ST (@I)))))
         (set! /done 1))))
      (cond
       ((= /done 1)
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0)))))))))

; Replace assignments e := code by small integers 
; Replace the CALL dispatch at posn by a jump table on v 
(define (@FD_Jump_Table_Fix2 /e-par /v-par /posn-par /addr_vars-par)
 (let ((/addr_vars-save /addr_vars)
       (/posn-save /posn)
       (/v-save /v)
       (/e-save /e))
  (set! /addr_vars /addr_vars-par)
  (set! /posn /posn-par)
  (set! /v /v-par)
  (set! /e /e-par)
  (let ((/codes (gethash /addr_vars /e))
        (/new-save /new)
        (/n 1)
        (/var-save /var)
        (/dest '()))
   (set! /new (hash-table))
   (set! /var (@Make //T_/Var_/Lvalue /e '()))
   (for-in /code /codes 
    (begin
     (puthash /new /code /n)
     (set! /n (+ /n 1))))
   (@Goto '())
   (@Foreach_Statement /foreach-fix_dispatch-25 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Goto /posn)
   (set! /var (@Make //T_/Variable /v '()))
   (@Paste_Over /%const__fix_dispatch__8)
   (@Down)
   (for-in /code (reverse /codes) 
    (begin
     (set! /exp (@Make //T_/Number (gethash /new /code) '()))
     (set! /dest (@Make //T_/Number /code '()))
     (@Paste_After (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /var) (@Var_To_Expn /exp))) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "destination") '()) (@Var_To_Expn /dest))))) (@Make 112 (@Make_Name "dispatch") '()))))))))
   (@Delete)
   (@Up)
   (display-list "Fixed jump table: " (@N_String /e) " " (@N_String /v) " at " /posn)
   (@Goto /posn)
   (set! /new /new-save)
   (set! /var /var-save))
  (set! /addr_vars /addr_vars-save)
  (set! /posn /posn-save)
  (set! /v /v-save)
  (set! /e /e-save)))

(define (@FD_Update_Addr_Vars //I //Code_/Hash-par //Registers-par /addr_vars-par)
 (let ((/addr_vars-save /addr_vars)
       (//Registers-save //Registers)
       (//Code_/Hash-save //Code_/Hash)
       (funct-result '()))
  (set! /addr_vars /addr_vars-par)
  (set! //Registers //Registers-par)
  (set! //Code_/Hash //Code_/Hash-par)
  (for-in /assign (@Cs //I) 
   (cond
    ((and (= (@ST (@Get_n /assign 2)) //T_/Number) (not (null? (gethash //Code_/Hash (@V (@Get_n /assign 2))))) (= (@ST (@Get_n /assign 1)) //T_/Var_/Lvalue) (not-member (@V (@Get_n /assign 1)) //Registers))
     (puthash /addr_vars (@V (@Get_n /assign 1)) (union-n (gethash /addr_vars (@V (@Get_n /assign 1))) (list (@V (@Get_n /assign 2))))))))
  (set! funct-result /addr_vars)
  (set! /addr_vars /addr_vars-save)
  (set! //Registers //Registers-save)
  (set! //Code_/Hash //Code_/Hash-save)
  funct-result))

; Check for subroutines which are only called once: 
; If a variable is only assigned once with a known dispatch code, 
; then replace all references by the dispatch code. 
(define (@FD_Once_Called_Subroutines //Code_/Hash-par /budget-par)
 (let ((/budget-save /budget)
       (//Code_/Hash-save //Code_/Hash))
  (set! /budget /budget-par)
  (set! //Code_/Hash //Code_/Hash-par)
  (let ((/bad-save /bad)
        (/value-save /value)
        (/code-save /code)
        (/elt-save /elt)
        (/found-save /found)
        (/posn-save /posn)
        (/name-save /name)
        (/count1-save /count1)
        (/count2-save /count2)
        (/notused-save /notused))
   (set! /bad (hash-table))
   (set! /value (hash-table))
   (set! /code (hash-table))
   (set! /elt '())
   (set! /found 0)
   (set! /posn (@Posn))
   (set! /name '())
   (set! /count1 (hash-table))
   (set! /count2 (hash-table))
   (set! /notused (hash-table))
   (@Goto '())
   (@Foreach_Statement /foreach-fix_dispatch-26 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Statement /foreach-fix_dispatch-27 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /elt (@Hash_Keys /value) 
    (cond
     ((null? (gethash /bad /elt))
      (display-list "Found a once called proc, var = " (my-map @N_String /elt)))
     (#t
      (puthash /value /elt '()))))
   (for-in /elt (@Hash_Keys /value) 
    (puthash /code (gethash /value /elt) 1))
   (for-in /c (@Hash_Keys /code) 
    (begin
     (puthash /count1 /c 0)
     (puthash /count2 /c 0)
     (puthash /notused (@Make_Name (string-append "NOTUSED_" (@String /c))) /c)))
   (@Foreach_Statement /foreach-fix_dispatch-28 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Ateach_Expn /foreach-fix_dispatch-29 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Statement /foreach-fix_dispatch-30 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   ; There could be more than one assignment of the code: 
   ; C_P gets rid of the one we are currently processing, 
   ; but another one elsewhere is still needed 
   ; and could get clobbered by this code. 
   (cond
    ((= /found 1)
     (@Trans //T/R_/Constant_/Propagation /budget)))
   (@Foreach_Statement /foreach-fix_dispatch-31 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Goto /posn)
   (set! /bad /bad-save)
   (set! /value /value-save)
   (set! /code /code-save)
   (set! /elt /elt-save)
   (set! /found /found-save)
   (set! /posn /posn-save)
   (set! /name /name-save)
   (set! /count1 /count1-save)
   (set! /count2 /count2-save)
   (set! /notused /notused-save))
  (set! /budget /budget-save)
  (set! //Code_/Hash //Code_/Hash-save)))

; Check if the given item is either a dispatch code number 
; or a function call of the form: !XF inline_par(code, par) 
(set! /inline_par (@Make_Name "inline_par"))
(define (@FD_Is_Code? //I //Codes-par)
 (let ((//Codes-save //Codes)
       (//O/K-save //O/K)
       (funct-result '()))
  (set! //Codes //Codes-par)
  (set! //O/K 0)
  (cond
   ((= (@ST //I) //T_/Number)
    (cond
     ((not (null? (gethash //Codes (@V //I))))
      (set! //O/K 1))))
   ((= (@ST //I) //T_/X_/Funct_/Call)
    (cond
     ((and (equal? (@V (@Get_n //I 1)) /inline_par) (= (@Size (@Get_n //I 2)) 2) (= (@ST (@Get_n (@Get_n //I 2) 1)) //T_/Number) (not (null? (gethash //Codes (@V (@Get_n (@Get_n //I 2) 1))))))
      (set! //O/K 1)))))
  (set! funct-result (= //O/K 1))
  (set! //Codes //Codes-save)
  (set! //O/K //O/K-save)
  funct-result))

; Look for actions which call dispatch more than once. 
; Put the statement sequence containing each dispatch call into 
; its own action. This is in preparation for checking for exits 
; from the subroutine which dispatch to a different return register/variable 
; First: absorb a call into a preceedig DO...OD loop. 
(define (@FD_Separate_Dispatch_Calls /new_actions-par)
 (let ((/new_actions-save /new_actions)
       (funct-result '()))
  (set! /new_actions /new_actions-par)
  (let ((/pair '())
        (/new-save /new)
        (/count-save /count)
        (/name-save /name)
        (/names-save /names))
   (set! /new '())
   (set! /count 0)
   (set! /name '())
   (set! /names '())
   ; Absorb calls: 
   (@Ateach_Statement /foreach-fix_dispatch-32 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (for-in /action (@Cs (@Get_n (@I) 2)) 
    (set! /names (cons (@V (@Get_n /action 1)) /names)))
   (@Down_To 2)
   (@Down)
   ; to first action 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((not (equal? (@V (@Get_n (@I) 1)) //Dispatch))
       (set! /pair (@Assoc //Dispatch (@Calls (@I))))
       (cond
        ((and (not (null? /pair)) (> (wsl-ref /pair 2) 1))
         (let ((/name-save /name))
          (set! /name (@V (@Get_n (@I) 1)))
          (@Foreach_Stats /foreach-fix_dispatch-33 0 (@AS_Type) 0)
          (cond
           ((null? (@Program))
            (@New_Program (@Skips))))
          (set! /name /name-save))))))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0)))))
   (cond
    ((not (null? /new))
     (@Splice_Before (reverse /new))
     (set! /new_actions (concat /new_actions /new))))
   (@Up)
   (@Up)
   (set! /new /new-save)
   (set! /count /count-save)
   (set! /name /name-save)
   (set! /names /names-save))
  (set! funct-result /new_actions)
  (set! /new_actions /new_actions-save)
  funct-result))

; Put the dispatch call into a new action: 
(define (@FD_Action /name-par /names-par /count-par /new-par)
 (let ((/new-save /new)
       (/count-save /count)
       (/names-save /names)
       (/name-save /name)
       (funct-result '()))
  (set! /new /new-par)
  (set! /count /count-par)
  (set! /names /names-par)
  (set! /name /name-par)
  (let ((/new_n '())
        (//L '()))
   (@Down_Last)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((not (@Left?))
      (set! /fl_flag1 1))
     (#t
      (@Left)
      (cond
       ((not (null? (@Calls (@I))))
        (@Right)
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0))))))
   (set! //L (@Cs (@Parent)))
   (set! //L (@Sub_Seg //L (@Posn_n) (@Size (@Parent))))
   (set! /count (+ /count 1))
   (set! /new_n (@Make_Name (concat (string-append (@N_String /name) "_") (@String /count))))
   (while (member /new_n /names) 
    (begin
     (set! /count (+ /count 1))
     (set! /new_n (@Make_Name (concat (string-append (@N_String /name) "_") (@String /count))))))
   (set! /names (cons /new_n /names))
   (set! /new (cons (@Make //T_/Action '() (list (@Name /new_n) (@Make //T_/Statements '() //L))) /new))
   (@Paste_Over (@Make //T_/Call /new_n '()))
   (@Delete_Rest))
  (set! funct-result (list /names /count /new))
  (set! /new /new-save)
  (set! /count /count-save)
  (set! /names /names-save)
  (set! /name /name-save)
  funct-result))

#t
