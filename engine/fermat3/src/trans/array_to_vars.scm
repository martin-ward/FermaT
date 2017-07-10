;;; Scheme translation of WSL code
(define (/foreach-array_to_vars-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) (@V /ar)))
   (cond
    ((or (not (= (@ST (@Parent)) //T_/Aref)) (not (= (@ST (@Get_n (@Get_n (@Parent) 2) 1)) //T_/Number)))
     (@Fail "Bad reference found to the array variable"))))))

(define (/foreach-array_to_vars-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Var_/Lvalue) (equal? (@V (@I)) (@V /ar)))
   (cond
    ((or (not (= (@ST (@Parent)) //T_/Aref_/Lvalue)) (not (= (@ST (@Get_n (@Get_n (@Parent) 2) 1)) //T_/Number)))
     (@Fail "Bad reference found to the array variable"))))))

(define (/foreach-array_to_vars-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Aref) (= (@ST (@Get_n (@I) 1)) //T_/Variable) (equal? (@V (@Get_n (@I) 1)) (@V /ar)))
   ; the array index must be an integer 
   (@Paste_Over (@Make //T_/Variable (wsl-ref /vars (@V (@Get_n (@Get_n (@I) 2) 1))) '())))))

(define (/foreach-array_to_vars-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Aref_/Lvalue) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@I) 1)) (@V /ar)))
   ; the array index must be an integer 
   (@Paste_Over (@Make //T_/Var_/Lvalue (wsl-ref /vars (@V (@Get_n (@Get_n (@I) 2) 1))) '())))))

;
;==========================================================================
;FermaT Transformation System
;Copyright (C) 2015 Software Migrations Limited.
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
; Convert a local variable array to a set of local variables 
; VAR < ar := ARRAY(n, e) >: ...ar[n]... ENDVAR . 
(define (@Array_To_Vars_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Var))
   (@Fail "Current item is not a local variable statement"))
  (#t
   (let ((/ar-save /ar))
    (set! /ar '())
    (for-in /assign (@Cs (@Get_n (@I) 1)) 
     (cond
      ((and (= (@ST (@Get_n /assign 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n /assign 2)) //T_/Array) (= (@ST (@Get_n (@Get_n /assign 2) 1)) //T_/Number))
       (set! /ar (@Get_n /assign 1)))))
    (cond
     ((null? /ar)
      (@Fail "No array assignment in the VAR"))
     (#t
      ; Check that all references are of the form ar[n] 
      (@Down_To 2)
      (@AV_Test_Statements /ar)
      (cond
       ((and (not (@Failed?)) (member //T_/Proc_/Call (@Stat_Types (@I))))
        ; Check bodies of called procedures: 
        (let ((/posn (@Posn))
              (/procs (@AV_Find_Called)))
         (for-in /proc /procs 
          (begin
           (@Goto /proc)
           (cond
            ((or (member /ar (@Variables (@Get_n (@I) 3))) (member /ar (@Variables (@Get_n (@I) 2))))
             (@Fail "The array appears as a parameter"))
            (#t
             (@Down_To 4)
             (@AV_Test_Statements /ar))))))))
      (cond
       ((not (@Failed?))
        (@Pass)))))
    (set! /ar /ar-save)))))

(define (@Array_To_Vars_Code //Data)
 (let ((/ar-save /ar)
       (/str "")
       (/vars-save /vars)
       (/val '())
       (/new '())
       (/all (@All_Variables (@I)))
       (/i 1)
       (/v '()))
  (set! /ar '())
  (set! /vars '())
  (for-in /assign (@Cs (@Get_n (@I) 1)) 
   (cond
    ((and (= (@ST (@Get_n /assign 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n /assign 2)) //T_/Array) (= (@ST (@Get_n (@Get_n /assign 2) 1)) //T_/Number))
     (set! /ar (@Get_n /assign 1))
     (set! /val (@Get_n (@Get_n /assign 2) 2))
     (set! /str (string-append (@N_String (@V /ar)) "_"))
     (set! /vars '())
     (set! /i 1)
     (set! /v (@Make_Name (concat /str (@String /i))))
     (while (member /v /all) 
      (begin
       (set! /i (+ /i 1))
       (set! /v (@Make_Name (concat /str (@String /i))))))
     (for /j /i (- (+ (@V (@Get_n (@Get_n /assign 2) 1)) /i) 1) 1 
      (set! /vars (cons (@Make_Name (concat /str (@String /j))) /vars))))))
  (for-in /var /vars 
   (set! /new (cons (@Make //T_/Assign '() (list (@Make //T_/Var_/Lvalue /var '()) /val)) /new)))
  (set! /vars (reverse /vars))
  (@Down_To 2)
  (@AV_Process_Statements /ar /vars)
  (let ((/posn (@Posn)))
   (for-in /pos (@AV_Find_Called) 
    (begin
     (@Goto /pos)
     (@AV_Process_Statements /ar /vars)))
   (@Goto /posn))
  (@Left)
  (@Down)
  ; to first assign 
  (while (and (@Right?) (not (@Equal? (@Get_n (@I) 1) /ar))) 
   (@Right))
  (cond
   ((@Equal? (@Get_n (@I) 1) /ar)
    (@Splice_Over /new)))
  (@Up)
  (@Up)
  ; Check if there are no variables left: 
  (cond
   ((= (@Size (@Get_n (@I) 1)) 0)
    (@Splice_Over (@Cs (@Get_n (@I) 2)))))
  (set! /ar /ar-save)
  (set! /vars /vars-save)))

; Find the set of procedures called directly or indirectly 
; and return a list of posns of the proc bodies. 
(define (@AV_Find_Called)
 (let ((/posn (@Posn))
       (/todo (my-map HEAD (@Proc_Calls (@I))))
       (/done (hash-table))
       (/name '())
       (/body (hash-table))
       (/proc_posn (hash-table))
       (/posns '()))
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
   (@Up))
  (cond
   ((not (= (@ST (@I)) //T_/Where))
    (set! /todo '())
    ; No definitions to find 
   )
   (#t
    (@Down_To 2)
    (@Down)
    ; to first defn 
    (cond
     ((= //T_/Proc (@ST (@I)))
      (set! /name (@V (@Get_n (@I) 1)))
      (puthash /body /name (@Get_n (@I) 4))
      (puthash /proc_posn /name (@Posn))))
    (while (@Right?) 
     (begin
      (@Right)
      (cond
       ((= //T_/Proc (@ST (@I)))
        (set! /name (@V (@Get_n (@I) 1)))
        (puthash /body /name (@Get_n (@I) 4))
        (puthash /proc_posn /name (@Posn))))))))
  (while (not (null? /todo)) 
   (begin
    (set! /name (car /todo))
    (set! /todo (cdr /todo))
    (cond
     ((null? (gethash /done /name))
      (puthash /done /name 1)
      (set! /posns (cons (gethash /proc_posn /name) /posns))
      (for-in /pair (@Proc_Calls (gethash /body /name)) 
       (cond
        ((null? (gethash /done (car /pair)))
         (set! /todo (cons (car /pair) /todo)))))))))
  (@Goto /posn)
  /posns))

(define (@AV_Test_Statements /ar-par)
 (let ((/ar-save /ar))
  (set! /ar /ar-par)
  (@Ateach_Expn /foreach-array_to_vars-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Ateach_Lvalue /foreach-array_to_vars-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /ar /ar-save)))

(define (@AV_Process_Statements /ar-par /vars-par)
 (let ((/vars-save /vars)
       (/ar-save /ar))
  (set! /vars /vars-par)
  (set! /ar /ar-par)
  (@Foreach_Expn /foreach-array_to_vars-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Lvalue /foreach-array_to_vars-4 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /vars /vars-save)
  (set! /ar /ar-save)))

#t
