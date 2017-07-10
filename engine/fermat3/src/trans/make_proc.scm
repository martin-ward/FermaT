;;; Scheme translation of WSL code
(define (/foreach-make_proc-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (cond
    ((and (= /need_flag 1) (equal? (@V (@I)) //Z))
     (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /exit_flag) (@Make 205 1 '()))))))
     (@Right)))
   (@Paste_Over (@Make //T_/Exit //Depth '())))))

(define (/foreach-make_proc-2 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (vector-set! /__/Match_array 1 /exit_flag)
  (vector-set! /__/Match_array 0 /exit_flag)
  (set! /__/O/K (@New_Match  /%const__make_proc__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/S_save //S))
     (set! //S (vector-ref /__/Match_array 2))
     (@Splice_Over //S)
     (set! //S /__/S_save))))))

(define (/foreach-make_proc-3 //Depth //A/S_/Type)
 (cond
  ((>= (@Size (@I)) (gen-length /body))
   (let ((/b /body)
         (/c (@Cs (@I))))
    (@Down)
    ; to first statement 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (begin
      (cond
       ((@Prefix_Equal? /b /c)
        ; Found another proc body 
        ; Paste SKIPs over the body, to preserve orig_posn 
        (display-list-flush ".")
        (@Paste_Over (@Make //T_/Proc_/Call '() (list (@Name /name) /expns /lvalues)))
        (set! /i /span)
        (cond
         ((and (> /tv 0) (= /span 1))
          ; Put the EXIT into an IF to preserve orig_posn 
          (@Paste_Cond (list (@I) (@Make //T_/Exit /tv '()))))
         ((> /tv 0)
          (@Right)
          (set! /c (cdr /c))
          (set! /i (- /i 1))
          (@Paste_Over (@Make //T_/Exit /tv '()))))
        (while (> /i 1) 
         (begin
          (set! /i (- /i 1))
          (@Right)
          (set! /c (cdr /c))
          (@Paste_Over (@Skip))))))
      (cond
       ((not (@Right?))
        (set! /fl_flag1 1))
       (#t
        (@Right)
        (set! /c (cdr /c))
        (cond
         ((< (gen-length /c) (gen-length /b))
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0)))))))))))

(define (/foreach-make_proc-4 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (@Paste_Over /call))))

(define (/foreach-make_proc-5 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/Proc_/Call)
   (set! /calls (union-n (list (@V (@Get_n (@I) 1))) /calls)))))

(define /%const__make_proc__1 (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 205 1 '()))) (@Make 17 '() (list (@Make 110 '() (list (@Make 6 '() (list (@Make 510 '() (list (@Make 205 2 '()))) (@Make 205 1 '()))))) (@Make 107 -3 '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 163 -3 '()))))))))
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
(define (@Make_Proc_Test)
 (let ((/calls-save /calls)
       (/tvs '())
       (/as-save /as)
       (/body-save /body)
       (/span-save /span))
  (set! /calls '())
  (set! /as (@AS_Type))
  (set! /body (list (@I)))
  (set! /span (+ (@Span) 1))
  (cond
   ((= (@ST (@I)) //T_/Statements)
    (set! /span (@Size (@I)))
    (@Down)
    (set! /body (list (@I)))))
  (cond
   ((= (@ST (@I)) //T_/Action)
    (cond
     ((and (= (@Size (@Get_n (@I) 2)) 1) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Call))
      (@Fail "Cannot make an action call into a procedure call"))
     ((and (equal? /as "Rec") (or (null? (@Calls (@I))) (equal? (car (car (@Calls (@I)))) (@V (@Get_n (@I) 1)))))
      (@Pass))
     ((or (equal? /as "Reg") (null? (@Calls (@I))))
      (@Pass))
     (#t
      (@Fail "The action system is not regular, or the action contains calls"))))
   ((not (= (@GT (@I)) //T_/Statement))
    (@Fail "A procedure can only be made from an action or a list of statements"))
   (#t
    ; Test the suitability of the proposed body 
    (while (> /span 1) 
     (begin
      (@Right)
      (set! /body (cons (@I) /body))
      (set! /span (- /span 1))))
    (@Edit)
    (@New_Program (@Make //T_/Statements '() (reverse /body)))
    (set! /calls (@Calls (@Program)))
    (set! /tvs (@Gen_TVs (@Program) /as))
    (cond
     ((> (gen-length /calls) 1)
      (@Fail "The proc body calls two actions"))
     ((= (gen-length /calls) 1)
      (cond
       ((and (equal? /as "Reg") (@Regular? (@Program)))
        (@Pass))
       (#t
        (@Fail "The proc body has calls and is not regular"))))
     ((and (equal? /tvs (list 1)) (@Gen_Reducible? (@Program) /as))
      (@Pass))
     ((and (not (equal? /tvs (list 0))) (not (null? /tvs)))
      (@Fail "The EXITs are unsuitable for taking out of the proposed body"))
     (#t
      (@Pass)))
    (@Undo_Edit)))
  (set! /calls /calls-save)
  (set! /as /as-save)
  (set! /body /body-save)
  (set! /span /span-save)))

(define (@Make_Proc_Code //Data)
 (let ((/span-save /span))
  (set! /span (+ (@Span) 1))
  (cond
   ((= (@ST (@I)) //T_/Statements)
    (set! /span (@Size (@I)))
    (@Down)))
  (@Make_Proc_Span //Data /span)
  (set! /span /span-save)))

(define (@Make_Proc_Span /name-par /span-par)
 (let ((/span-save /span)
       (/name-save /name))
  (set! /span /span-par)
  (set! /name /name-par)
  (let ((/body-save /body)
        (/orig_posn '())
        (/base "")
        (/calls-save /calls)
        (/calls_z '())
        (/call-save /call)
        (/tvs '())
        (/as-save /as)
        (/i-save /i)
        (/tv-save /tv)
        (/exit_flag-save /exit_flag)
        (//Z-save //Z)
        (/need_flag-save /need_flag)
        (/flag_posn '())
        (/aname '())
        (/pos '())
        (//I (@I)))
   (set! /body (list (@I)))
   (set! /calls '())
   (set! /call '())
   (set! /as (@AS_Type))
   (set! /i 0)
   (set! /tv 0)
   (set! /exit_flag '())
   (set! //Z (@Make_Name "Z"))
   (set! /need_flag 0)
   (cond
    ((and (string? /name) (not (equal? /name "")))
     (set! /name (@Make_Name /name))))
   (cond
    ((equal? /name "")
     ; Choose a suitable name for the proc: 
     (cond
      ((= (@ST (@I)) //T_/Action)
       (set! /name (@V (@Get_n (@I) 1))))
      (#t
       (set! /name (@Make_Name "p"))))))
   (set! /calls (@MP_Proc_Calls))
   (cond
    ((or (member /name /calls) (equal? /name (@Make_Name "p")))
     ; Ensure that the name is unique: 
     (set! /i 1)
     (set! /base (@N_String /name))
     (set! /name (@Make_Name (concat (string-append /base "_") (@String /i))))
     (while (member /name /calls) 
      (begin
       (set! /i (+ /i 1))
       (set! /name (@Make_Name (concat (string-append /base "_") (@String /i))))))))
   ; Use a different exit_flag if we are in a sub action system. 
   (while (and (@Up?) (not (= (@ST (@I)) //T_/A_/S))) 
    (begin
     (set! /pos (cons (@Posn_n) /pos))
     (@Up)))
   (cond
    ((or (not (= (@ST (@I)) //T_/A_/S)) (equal? (@V (@Get_n (@I) 1)) (@Make_Name "_enter_")) (equal? (@V (@Get_n (@I) 1)) (@Make_Name "_enter_1")) (equal? (@V (@Get_n (@I) 1)) (@Make_Name "MAIN_1")))
     ; We are in the main action system: so use exit_flag 
     (set! /exit_flag (@Make //T_/Variable (@Make_Name "exit_flag") '())))
    (#t
     (set! /exit_flag (@Make //T_/Variable (@Make_Name (string-append "exit_flag_" (@N_String /name))) '()))
     (@Paste_Before (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /exit_flag) (@Make 205 0 '()))))))
     (set! /flag_posn (@Posn))
     (@Right)))
   (while (not (null? /pos)) 
    (begin
     (@Down_To (car /pos))
     (set! /pos (cdr /pos))))
   (set! /orig_posn (@Posn))
   (cond
    ((and (= (@ST (@I)) //T_/Action) (equal? /as "Rec"))
     (@Down_To 2)
     (set! /span (@Size (@I)))
     (@Down)
     (set! /orig_posn (@Posn))
     (set! /body (list (@I))))
    ((= (@ST (@I)) //T_/Action)
     ; Calculate how many statements can be included 
     ; If the action is regular and contains calls to only one action, 
     ; (plus Z perhaps) then the whole body can be used. 
     (set! /aname (@V (@Get_n (@I) 1)))
     (@Down_Last)
     ; to statement sqce 
     (set! /calls (@Calls (@I)))
     (set! /calls_z (@MP_Remove_Z /calls))
     (cond
      ((or (= (gen-length /calls) 0) (and (= (gen-length /calls_z) 0) (@Regular? (@I))) (and (= (gen-length /calls_z) 1) (not (equal? (wsl-ref (wsl-ref /calls_z 1) 1) /aname)) (@Regular? (@I))))
       (set! /span (@Size (@I)))
       (@Down)
       ; to first statement 
       (set! /orig_posn (@Posn)))
      (#t
       (@Down)
       ; to first statement in body 
       (set! /orig_posn (@Posn))
       (set! /span 0)
       (set! /fl_flag1 0)
       (while (= /fl_flag1 0) 
        (begin
         (set! /calls (@Calls (@I)))
         (cond
          ((not (null? /calls))
           (cond
            ((and (= (gen-length /calls) 1) (@Regular? (@I)))
             (set! /span (+ /span 1))))
           (set! /fl_flag1 1))
          (#t
           (set! /span (+ /span 1))
           (cond
            ((not (@Right?))
             (set! /fl_flag1 1))
            (#t
             (@Right)
             (set! /fl_flag1 0)))))))
       (@Goto /orig_posn)))
     (cond
      ((= /span 0)
       (@Paste_Before (@Skip))
       ; the skip is now selected 
       (set! /span 1)))
     (set! /body (list (@I)))))
   ; Check for calls in the proposed body. 
   ; If necessary, merge several action calls by creating a loop 
   (set! /i /span)
   (while (> /i 1) 
    (begin
     (@Right)
     (set! /body (cons (@I) /body))
     (set! /i (- /i 1))))
   (@Edit)
   (@New_Program (@Make //T_/Statements '() (reverse /body)))
   (set! /calls (@Calls (@Program)))
   (set! /tvs (@Gen_TVs (@Program) /as))
   (cond
    ((equal? /as "Rec")
     (set! /tv 0)
     (@Undo_Edit))
    ((and (equal? /tvs (list 1)) (null? /calls))
     (set! /tv 1)
     (@Undo_Edit))
    ((and (null? /calls) (or (equal? /tvs (list 0)) (null? /tvs)))
     (set! /tv 0)
     (@Undo_Edit))
    ((and (<= (gen-length (@MP_Remove_Z /calls)) 1) (equal? /tvs (list //Omega)))
     (set! /tv 0)
     (cond
      ((> (gen-length /calls) 1)
       (set! /need_flag 1)
       (set! /calls (@MP_Remove_Z /calls))))
     ; Take out the call 
     ; First check if the last statement in the span is the only call, 
     ; if so, then we simply shorten the span 
     (cond
      ((and (= /need_flag 0) (= (wsl-ref (wsl-ref /calls 1) 2) 1) (= (@ST (car /body)) //T_/Call))
       (set! /span (- /span 1))
       (@Undo_Edit)
       (@Goto /orig_posn))
      (#t
       ; Update the body by creating a loop, 
       ; then copy over the actual proposed body and update the span 
       (@New_Program (@Make //T_/Statements '() (list (@Make //T_/Floop '() (list (@Program))))))
       (@Foreach_Statement /foreach-make_proc-1 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))
       (@Foreach_Statement /foreach-make_proc-2 0 (@AS_Type) 0)
       (cond
        ((null? (@Program))
         (@New_Program (@Skips))))
       ; Remove the newly-created loop if possible 
       (@Down)
       (cond
        ((@Trans? //T/R_/Remove_/Dummy_/Loop)
         (@Trans //T/R_/Remove_/Dummy_/Loop "")))
       (@Up)
       (set! /call (@Make //T_/Call (wsl-ref (wsl-ref /calls 1) 1) '()))
       (cond
        ((= /need_flag 1)
         (set! /call (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /exit_flag) (@Make 205 1 '()))) (@Make 17 '() (list (@Make 112 (@Make_Name "Z") '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list /call)))))))))
       (set! /body (concat (@Cs (@Program)) (list /call)))
       (@Undo_Edit)
       (@Goto /orig_posn)
       ; Delete all but one statement of the old body 
       ; and splice over the new one 
       (while (> /span 1) 
        (begin
         (@Delete)
         (set! /span (- /span 1))))
       (@Splice_Over /body)
       ; The new span excludes the final call, of course 
       (set! /span (- (gen-length /body) 1)))))
    (#t
     (display-list "ERROR in Make_Proc_Code!!!")
     (display-list "calls = " /calls " tvs = " /tvs)
     (@PP_Item (@I) 80 "")
     (error "Make_Proc_Code" "Error" "???")))
   (cond
    ((and (= /need_flag 0) (not (null? /flag_posn)))
     (@Goto /flag_posn)
     (@Paste_Over (@Skip))))
   ; Finally, make the procedure! 
   (@Goto /orig_posn)
   (@Make_Proc /name /span /tv)
   (set! /body /body-save)
   (set! /calls /calls-save)
   (set! /call /call-save)
   (set! /as /as-save)
   (set! /i /i-save)
   (set! /tv /tv-save)
   (set! /exit_flag /exit_flag-save)
   (set! //Z //Z-save)
   (set! /need_flag /need_flag-save))
  (set! /span /span-save)
  (set! /name /name-save)))

; Make a procedure from the span statements starting at the current posn 
; If tv>0 then reduce the proc body by that amount and add an EXIT to the call 
(define (@Make_Proc /name-par /span-par /tv-par)
 (let ((/tv-save /tv)
       (/span-save /span)
       (/name-save /name))
  (set! /tv /tv-par)
  (set! /span /span-par)
  (set! /name /name-par)
  (display-list "Creating procedure `" (@N_String /name) "', size = " /span ", tv = " /tv)
  (display-list "  position = " (@Posn))
  (let ((/expns-save /expns)
        (/lvalues-save /lvalues)
        (/orig_posn (@Posn))
        (/body-save /body)
        (/rbody '())
        (/defn '())
        (/call-save /call)
        (/n-save /n)
        (/i-save /i)
        (/where_posn '()))
   (set! /expns (@Make //T_/Expressions '() '()))
   (set! /lvalues (@Make //T_/Lvalues '() '()))
   (set! /body (list (@I)))
   (set! /call '())
   (set! /n 0)
   (set! /i 0)
   (cond
    ((null? /tv)
     (set! /tv 0)))
   (@Paste_Over (@Make //T_/Proc_/Call '() (list (@Name /name) /expns /lvalues)))
   (cond
    ((> /tv 0)
     (@Paste_After (@Make //T_/Exit /tv '()))
     (@Right)))
   (cond
    ((> /span 1)
     (@Right)
     ; to next statement in body 
     (set! /i /span)
     (while (> /i 1) 
      (begin
       (set! /i (- /i 1))
       (set! /body (cons (@I) /body))
       (@Delete)))))
   (set! /body (reverse /body))
   ; Decrement the body if required 
   (set! /rbody (@Make //T_/Statements '() /body))
   (cond
    ((> /tv 0)
     (set! /rbody (@Increment /rbody /as (- /tv) 0))))
   ; Ensure that there is an enclosing WHERE 
   (set! /n (@MP_Ensure_Where  /n))
   (cond
    ((>= /n 0)
     (set! /orig_posn (concat (concat (@Sub_Seg /orig_posn 1 /n) (list 1 1)) (@Final_Seg /orig_posn (+ /n 1))))))
   (set! /where_posn (@Posn))
   ; Search for other occurrences of body, and replace by calls 
   (display-list-flush "Searching for other copies of body: ")
   (@Foreach_Stats /foreach-make_proc-3 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (display-list " ")
   (@Goto /where_posn)
   (cond
    ((not (= (@ST (@I)) //T_/Where))
     (error "ERROR_3 in @Make_Proc!!!")))
   (@Down_To 2)
   ; to definitions 
   (set! /defn (@Make //T_/Proc '() (list (@Name /name) /lvalues /lvalues /rbody)))
   (cond
    ((= (@Size (@I)) 0)
     (@Paste_Over (@Make //T_/Definitions '() (list /defn))))
    (#t
     (@Down_Last)
     (@Paste_After /defn)
     (@Right)))
   (set! /call (@Make //T_/Proc_/Call '() (list (@Name /name) /expns /lvalues)))
   (@Foreach_Statement /foreach-make_proc-4 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Goto /orig_posn)
   (set! /expns /expns-save)
   (set! /lvalues /lvalues-save)
   (set! /body /body-save)
   (set! /call /call-save)
   (set! /n /n-save)
   (set! /i /i-save))
  (set! /tv /tv-save)
  (set! /span /span-save)
  (set! /name /name-save)))

(define (@Paste_Cond /body-par)
 (let ((/body-save /body))
  (set! /body /body-par)
  (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() /body))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
  (set! /body /body-save)))

(define (@Prefix_Equal? //A //B)
 
 (or (null? //A) (and (not (null? //B)) (@Equal? (car //A) (car //B)) (@Prefix_Equal? (cdr //A) (cdr //B)))))

; Remove Z from the list of calls 
(define (@MP_Remove_Z /calls-par)
 (let ((/calls-save /calls)
       (//Z-save //Z)
       (//R '())
       (/call-save /call)
       (funct-result '()))
  (set! /calls /calls-par)
  (set! //Z (@Make_Name "Z"))
  (set! /call '())
  (for-in /call /calls 
   (cond
    ((not (equal? (wsl-ref /call 1) //Z))
     (set! //R (cons /call //R)))))
  (set! funct-result (reverse //R))
  (set! /calls /calls-save)
  (set! //Z //Z-save)
  (set! /call /call-save)
  funct-result))

; Should be sitting on a statement, this proc moves to an enclosing WHERE. 
; It creates one if necessary and returns the depth at which the WHERE 
; was created in n. 
; Use n to fix a position: posn := posn[1..n] ++ <1,1> ++ posn[n..] 
(define (@MP_Ensure_Where /n-par)
 (let ((/n-save /n)
       (funct-result '()))
  (set! /n /n-par)
  (let ((/orig_posn (@Posn))
        (/destination (@Make_Name "destination")))
   (set! /n (- 1))
   (@Up)
   ; to the sequence containing the call 
   (while (and (or (not (= //T_/Var (@ST (@I)))) (not-member /destination (@Assigned (@Get_n (@I) 1)))) (not (= //T_/Where (@ST (@I)))) (@Up?)) 
    (@Up))
   (cond
    ((= (@ST (@I)) //T_/Var)
     ; Put the var body inside a new WHERE clause: 
     (@Down_To 2)
     ; to the body 
     (set! /n (gen-length (@Posn)))
     (@Paste_Over (@Make //T_/Statements '() (list (@Make //T_/Where '() (list (@I) (@Make //T_/Definitions '() '()))))))
     (@Down)
     ;To the new WHERE clause 
    )
    ((not (= (@ST (@I)) //T_/Where))
     ; If the top level is a st sequence, then put it in a new where 
     ; otherwise, make a new where around the original sequence 
     (cond
      ((not (= (@ST (@I)) //T_/Statements))
       (@Goto /orig_posn)
       (@Up)))
     (set! /n (gen-length (@Posn)))
     (cond
      ((not (= (@ST (@I)) //T_/Statements))
       (error "ERROR_1 in @MP_Ensure_Where!!!")))
     (@Paste_Over (@Make //T_/Statements '() (list (@Make //T_/Where '() (list (@I) (@Make //T_/Definitions '() '()))))))
     (@Down)
     ;To the new WHERE clause 
    ))
   (cond
    ((not (= (@ST (@I)) //T_/Where))
     (error "ERROR_2 in @MP_Ensure_Where!!!"))))
  (set! funct-result /n)
  (set! /n /n-save)
  funct-result))

; Return the set of proc calls and !P calls (to avoid name clashes): 
(define (@MP_Proc_Calls)
 (let ((/calls-save /calls)
       (/posn (@Posn))
       (funct-result '()))
  (set! /calls '())
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
   (@Up))
  (set! /calls (@Make_Set (my-map HEAD (@Proc_Calls (@I)))))
  (cond
   ((= (@ST (@I)) //T_/Where)
    (set! /calls (union-n /calls (@Make_Set (my-map @V1 (@Cs (@Get_n (@I) 2))))))))
  (@Goto '())
  (@Foreach_Statement /foreach-make_proc-5 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Goto /posn)
  (set! funct-result /calls)
  (set! /calls /calls-save)
  funct-result))

#t
