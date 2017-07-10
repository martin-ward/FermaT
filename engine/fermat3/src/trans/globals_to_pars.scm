;;; Scheme translation of WSL code
(define (/foreach-globals_to_pars-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Proc_/Call)
   (set! /name (@V (@Get_n (@I) 1)))
   (cond
    ((not (null? (gethash /new_vars /name)))
     (@Down_To 3)
     (@Paste_Over (@Make //T_/Lvalues '() (concat (@Cs (@I)) (gethash /new_vars /name))))
     (@Up)))
   (cond
    ((not (null? (gethash /new_vals /name)))
     (@Down_To 2)
     (@Paste_Over (@Make //T_/Expressions '() (concat (@Cs (@I)) (my-map @Lvalue_To_Expn (gethash /new_vals /name)))))
     (@Up))))))

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
; Convert global variables in procedures to parameters (including 
; varables used indirectly via called procedures) 
; (1) Compute transitive closure of call graph 
; (2) Compute variables used/assigned directly and in called procedures 
; (3) Update parameter list 
; If Data is given, then only convert global vars in Data to pars (eg registers!) 
(define (@Globals_To_Pars_Test)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Pass))
  (#t
   (@Fail "Select a WHERE clause to process all the procedures."))))

(define (@Globals_To_Pars_Code //Data)
 (let ((/bodies (hash-table))
       (/calls (hash-table))
       (/new '())
       (/assigned '())
       (/used '())
       (/pars '())
       (/glob_assigned (hash-table))
       (/glob_used (hash-table))
       (/new_vars-save /new_vars)
       (/new_vals-save /new_vals)
       (/name-save /name)
       (/succs (hash-table))
       (/p 0)
       (/vars '())
       (/elts '()))
  (set! /new_vars (hash-table))
  (set! /new_vals (hash-table))
  (set! /name '())
  (cond
   ((string? //Data)
    (let ((/-result- (@Parse_Words  //Data /p /vars)))
     (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /vars (car /-result-)) (set! /-result- (cdr /-result-)))
    ; Convert the list of SSA elements (list of strings) to a set of items 
    (for-in /elt /vars 
     (set! /elts (cons (my-map @SSA_Make_Name /elt) /elts)))
    (set! /elts (@Make_Set /elts)))
   ((and (not (null? //Data)) (not (sequence? (car //Data))))
    (for-in /var //Data 
     (set! /elts (cons (list /var) /elts)))
    (set! /elts (@Make_Set /elts)))
   (#t
    (set! /elts (@Make_Set //Data))))
  (cond
   ((not (null? /elts))
    (display-list "Converting only these global vars:")
    (@Print_Elts /elts)))
  (@Edit)
  ; Set up the bodies and succs tables 
  (for-in //I (@Cs (@Get_n (@I) 2)) 
   (cond
    ((= (@ST //I) //T_/Proc)
     (set! /name (@V (@Get_n //I 1)))
     (puthash /bodies /name //I)
     (puthash /succs /name (@Make_Set (my-map HEAD (@Proc_Calls (@Get_n //I 4)))))
     ; Find the global elements used and assigned directly in the body 
     ; We don't use @Elt_Subtract here because we want to preserve 
     ; as much information as possible for datflow analysis. 
     ; For example, if FOO is assigned and a[FOO].BAR is used 
     ; then we want to keep the a[FOO].BAR parameter. 
     (set! /pars (union-n (@Elts_Assigned (@Get_n //I 2)) (@Elts_Assigned (@Get_n //I 3))))
     (puthash /glob_assigned /name (@Set_Difference (@Elts_Assigned (@Get_n //I 4)) /pars))
     (puthash /glob_used /name (@Set_Difference (@Elements (@Get_n //I 4)) (union-n (@Elts_Assigned (@Get_n //I 4)) /pars)))
     (cond
      ((not (null? /elts))
       (puthash /glob_assigned /name (intersection-n (gethash /glob_assigned /name) /elts))
       (puthash /glob_used /name (intersection-n (gethash /glob_used /name) /elts)))))))
  ; Compute direct and indirect calls: 
  (set! /calls (@Transitive_Closure /succs))
  ; Compute the new vars and new values for each proc. 
  ; Find all assigned/used elements in the body and all called procs, 
  ; if there are clashes, then take the larger element, 
  ; then take out any existing var/val parameters. 
  (for-in //I (@Cs (@Get_n (@I) 2)) 
   (cond
    ((= (@ST //I) //T_/Proc)
     (set! /name (@V (@Get_n //I 1)))
     (set! /assigned (@Elts_Assigned (@Get_n //I 4)))
     (set! /used (@Elts_Used (@Get_n //I 4)))
     (for-in /call (gethash /calls /name) 
      (begin
       (set! /assigned (union-n /assigned (gethash /glob_assigned /call)))
       (set! /used (union-n /used (gethash /glob_used /call)))))
     (set! /used (@GTP_Trim (@Set_Difference /used (union-n /assigned (@Elements (@Get_n //I 2))))))
     (set! /assigned (@GTP_Trim (@Set_Difference /assigned (@Elements (@Get_n //I 3)))))
     (cond
      ((not (null? /elts))
       (set! /used (intersection-n /used /elts))
       (set! /assigned (intersection-n /assigned /elts))))
     (puthash /new_vals /name (@Mth_Sort (my-map @Elt_To_Expn /used)))
     (puthash /new_vars /name (@Mth_Sort (my-map @Elt_To_Lvalue /assigned))))))
  ; Update the proc calls in the main block 
  (@Down)
  (@GTP_Update_Calls /new_vars /new_vals)
  ; Update the proc definitions and calls in the bodies: 
  (@Right)
  (@Down)
  ; to first defn 
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (cond
     ((= (@ST (@I)) //T_/Proc)
      (set! /name (@V (@Get_n (@I) 1)))
      (@Down_To 2)
      ; to val pars 
      (@Down_Last)
      (@Splice_After (my-map @Expn_To_Lvalue (gethash /new_vals /name)))
      (@Up)
      (@Right)
      ; to var pars 
      (@Down_Last)
      (@Splice_After (gethash /new_vars /name))
      (@Up)
      (@Right)
      ; to body 
      (@GTP_Update_Calls /new_vars /new_vals)
      (@Up)
      ; back to defn 
     ))
    (cond
     ((not (@Right?))
      (set! /fl_flag1 1))
     (#t
      (@Right)
      (set! /fl_flag1 0)))))
  (@End_Edit)
  (set! /new_vars /new_vars-save)
  (set! /new_vals /new_vals-save)
  (set! /name /name-save)))

; Add the new parameters to each proc call in the current item 
(define (@GTP_Update_Calls /new_vars-par /new_vals-par)
 (let ((/new_vals-save /new_vals)
       (/new_vars-save /new_vars))
  (set! /new_vals /new_vals-par)
  (set! /new_vars /new_vars-par)
  (let ((/name-save /name))
   (set! /name '())
   (@Foreach_Statement /foreach-globals_to_pars-1 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /name /name-save))
  (set! /new_vals /new_vals-save)
  (set! /new_vars /new_vars-save)))

; Trim a parameter list: remove NOTUSED_nnn elements and if there are 
; a large number of DSECT elements, then just include the whole DSECT name 
(define (@GTP_Trim /elts)
 (let ((/new '())
       (/dsects '())
       (/dsect_count (hash-table))
       (/p 0)
       (/str ""))
  ; Delete byte offset references, either FOO.nnn or FOO__nnn 
  (for-in /elt /elts 
   (begin
    (cond
     ((and (number? (last-1 /elt)) (< (last-1 /elt) 0))
      (set! /elt (butlast-1 /elt))))
    (cond
     ((and (number? (last-1 /elt)) (> (last-1 /elt) 0))
      (set! /p (my-index "__" (@N_String (last-1 /elt)))))
     (#t
      (set! /p (- 1))))
    (cond
     ((> /p 0)
      (set! /str (@N_String (last-1 /elt)))
      (cond
       ((@Digits? (substr /str (+ /p 2)))
        (set! /str (substr (@N_String (last-1 /elt)) 0 /p))
        (set! /elt (concat (butlast-1 /elt) (list (@Make_Name /str))))))))
    (set! /new (cons /elt /new))))
  (set! /elts (@Make_Set /new))
  (set! /new '())
  (for-in /elt /elts 
   (cond
    ((and (equal? (wsl-ref /elt 1) /a_name) (> (gen-length /elt) 1))
     (cond
      ((null? (gethash /dsect_count (wsl-ref /elt 2)))
       (puthash /dsect_count (wsl-ref /elt 2) 1))
      (#t
       (puthash /dsect_count (wsl-ref /elt 2) (+ (gethash /dsect_count (wsl-ref /elt 2)) 1)))))))
  (for-in /elt /elts 
   (cond
    ((@Starts_With? (@N_String (wsl-ref /elt 1)) "NOTUSED_")
     #t)
    ((and (equal? (wsl-ref /elt 1) /a_name) (> (gen-length /elt) 1) (> (gethash /dsect_count (wsl-ref /elt 2)) 10))
     (set! /dsects (union-n (list (list (wsl-ref /elt 2))) /dsects)))
    (#t
     (set! /new (cons /elt /new)))))
  (union-n (@Make_Set /new) /dsects)))

#t
