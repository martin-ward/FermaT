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
; Create a local WHERE for procs which are only called in the selected item. 
; Note: this only looks at the nearest enclosing WHERE, but can be applied 
; to a WHERE clause so that procs can be iteratively moved to more local 
; WHERE clauses 
(define (@Localise_Procs_Test)
 (cond
  ((and (not (= (@GT (@I)) //T_/Statement)) (not (= (@GT (@I)) //T_/Statements)))
   (@Fail "Current item is not a Statement or Statements."))
  ((not (@Up?))
   (@Fail "Current statement is whole program (not a local statement)."))
  ((null? (@Proc_Calls (@I)))
   (@Fail "The current item contains no proc calls."))
  ((not (@Gen_Proper? (@I) (@AS_Type)))
   (@Fail "The current item is not a proper sequence"))
  ((not (null? (@Calls (@I))))
   (@Fail "The current item contains action calls"))
  ((null? (@LP_Localisable_Procs (@Proc_Calls (@I))))
   (@Fail "The current item contains no localisable proc calls."))
  (#t
   (@Pass))))

(define (@Localise_Procs_Code //Data)
 (let ((/procs (@LP_Localisable_Procs (@Proc_Calls (@I))))
       (/posn (@Posn))
       (/defns '())
       (/new '())
       (/rel '()))
  (cond
   ((null? /procs)
    (error "No procs returned by @LP_Localisable_Procs!")))
  (set! /rel (wsl-ref /procs 3))
  (@Goto (wsl-ref /procs 2))
  (for-in /defn (@Cs (@Get_n (@I) 2)) 
   (cond
    ((member (@V (@Get_n /defn 1)) (wsl-ref /procs 1))
     (display-list "Localising: " (@N_String (@V (@Get_n /defn 1))))
     (set! /defns (cons /defn /defns)))
    (#t
     (set! /new (cons /defn /new)))))
  (cond
   ((null? /new)
    ; Delete the WHERE 
    (@Splice_Over (@Cs (@Get_n (@I) 1)))
    (cond
     ((= (wsl-ref /rel 1) 1)
      (set! /rel (cdr /rel))
      (cond
       ((not (null? /rel))
        ; Do the first move: 
        (@To (+ (- (@Posn_n) 1) (car /rel)))
        (set! /rel (cdr /rel)))))))
   (#t
    ; Update the defns 
    (@Down_To 2)
    (@Paste_Over (@Make //T_/Definitions '() (reverse /new)))
    (@Up)))
  ; Return to the original item: 
  (while (not (null? /rel)) 
   (begin
    (@Down_To (car /rel))
    (set! /rel (cdr /rel))))
  ; Create a WHERE, or add defns to the existing WHERE: 
  (@LP_Add_Defns /defns)))

(define (@LP_Add_Defns /defns)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Down_To 2)
   (@Paste_Over (@Make //T_/Definitions '() (concat (@Cs (@I)) /defns)))
   (@Up))
  ((= (@GT (@I)) //T_/Statements)
   (@Paste_Over (@Make //T_/Statements '() (list (@Make //T_/Where '() (list (@I) (@Make //T_/Definitions '() /defns)))))))
  (#t
   (@Paste_Over (@Make //T_/Where '() (list (@Make //T_/Statements '() (list (@I))) (@Make //T_/Definitions '() /defns))))))
 (cond
  ((@Trans? //T/R_/Sort_/Procs)
   (@Trans //T/R_/Sort_/Procs ""))))

; Localising the procs may leave the outer WHERE empty of definitions 
; in which case, it should be deleted. In which case, the position of 
; the original item will change. So we also return the _relative_ position 
; from the WHERE to the original item 
(define (@LP_Localisable_Procs /local_calls)
 (let ((/procs '())
       (/global_calls '())
       (/posn (@Posn))
       (//R '())
       (/rel '()))
  (cond
   ((@Up?)
    (set! /rel (cons (@Posn_n) /rel))
    (@Up))
   (#t
    (error "@LP_Localisable_Procs: can't go up!")))
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
   (begin
    (set! /rel (cons (@Posn_n) /rel))
    (@Up)))
  (cond
   ((= (@ST (@I)) //T_/Where)
    (set! /global_calls (@Qry_Call_Join (@Proc_Calls (@Get_n (@I) 1)) (@Proc_Calls (@Get_n (@I) 2))))
    ; If the number of global calls is the same as the local calls 
    ; then all calls must be local and the proc can be localised 
    (set! /procs (my-map HEAD (intersection-n (@Make_Set /local_calls) (@Make_Set /global_calls))))
    (cond
     ((not (null? /procs))
      (set! //R (list (@Make_Set /procs) (@Posn) /rel))))))
  (@Goto /posn)
  //R))

#t
