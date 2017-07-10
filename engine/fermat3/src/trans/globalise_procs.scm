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
; Move procs to an enclosing WHERE (opposite of Localise_Procs) 
(define (@Globalise_Procs_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Where))
   (@Fail "Current item is not a WHERE clause."))
  ((not (@Up?))
   (@Fail "Current WHERE clause is already as global as possible."))
  ((and (= (@GT (@I)) //T_/Statement) (= (@Size (@Parent)) 1) (equal? (@Posn) (list 1)))
   (@Fail "Current WHERE clause is already as global as possible."))
  ((null? (@GP_Globalisable_Procs (@Cs (@Get_n (@I) 2))))
   (@Fail "The WHERE clause contains no globalisable proc definitions."))
  (#t
   (@Pass))))

(define (@Globalise_Procs_Code //Data)
 (let ((/procs (@GP_Globalisable_Procs (@Cs (@Get_n (@I) 2))))
       (/posn (@Posn))
       (/defns '()))
  (for-in /defn (@Cs (@Get_n (@I) 2)) 
   (cond
    ((member (@V (@Get_n /defn 1)) /procs)
     (display-list "Globalising: " (@N_String (@V (@Get_n /defn 1))))
     (set! /defns (cons /defn /defns)))
    (#t
     (set! /new (cons /defn /new)))))
  (cond
   ((null? /new)
    ; Delete the WHERE 
    (@Splice_Over (@Cs (@Get_n (@I) 1))))
   (#t
    ; Update the defns 
    (@Down_To 2)
    (@Paste_Over (@Make //T_/Definitions '() (reverse /new)))
    (@Up)))
  ; Find enclosing WHERE and update it (or create a new one) 
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
   (@Up))
  (cond
   ((not (= (@ST (@I)) //T_/Where))
    (set! /posn (cons 1 (cons 1 /posn)))))
  (@LP_Add_Defns /defns)
  ; Return to the original item: 
  (@Goto /posn)))

(define (@GP_Globalisable_Procs /defns)
 (let ((/procs '())
       (/posn (@Posn)))
  (for-in /defn /defns 
   (cond
    ((= (@ST /defn) //T_/Proc)
     (set! /procs (union-n (list (@V (@Get_n /defn 1))) /procs)))))
  (cond
   ((@Up?)
    (@Up)))
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
   (@Up))
  (cond
   ((= (@ST (@I)) //T_/Where)
    (@Down)))
  ; If a proc is called globally, then the local version cannot be globalised 
  ; (unless we rename it first): 
  (set! /procs (@Set_Difference /procs (@Make_Set (my-map HEAD (@Proc_Calls (@I))))))
  (@Goto /posn)
  /procs))

#t
