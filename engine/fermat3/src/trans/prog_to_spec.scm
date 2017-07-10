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
; Convert any program to an equivalent specification statement. 
; Not implemented for programs with recursion or iteration. 
(define (@Prog_To_Spec_Test)
 (cond
  ((and (not (= (@GT (@I)) //T_/Statements)) (not (= (@GT (@I)) //T_/Statement)))
   (@Fail "Not a statement or statement sequence"))
  ((not (@Set_Subset? (@Stat_Types (@I)) (@Make_Set (list //T_/Assert //T_/Comment //T_/Skip //T_/Print //T_/Prinflush //T_/Abort //T_/Assignment //T_/Cond //T_/D_/If //T_/Var //T_/Spec))))
   (@Fail "Program contains an statement type for which
this transformation is undefined."))
  (#t
   (@Pass))))

(define (@Prog_To_Spec_Code //Data)
 ; x := x'.(~WP(S, x <> x') AND WP(S, TRUE)) 
 (let ((//W/P '())
       (//W/P_/T '())
       (//R '())
       (/vars '())
       (/cond '())
       (/spec '())
       (//Budget (@String_To_Num //Data)))
  (cond
   ((<= //Budget 0)
    (set! //Budget 50)))
  ; First compute the special postcondition: 
  (for-in /var (@Assigned (@I)) 
   (begin
    (set! /vars (cons (@Make //T_/Var_/Lvalue /var '()) /vars))
    (set! //R (cons (@Make //T_/Not_/Equal '() (list (@Make //T_/Variable /var '()) (@Make //T_/Primed_/Var /var '()))) //R))))
  (set! //R (@Make //T_/Or '() //R))
  ; Now compute the weakest preconditions: 
  (set! //W/P_/T (@WP (@Item) (@Make //T_/True '() '())))
  (set! //W/P (@Simplify (@WP (@Item) //R) //Budget))
  ; Do an expensive simplify on the final condition: 
  (set! /cond (@Simplify (@Make //T_/And '() (list (@Not //W/P) //W/P_/T)) //Budget))
  (cond
   ((null? /vars)
    (set! /spec (@Make //T_/Assert '() (list /cond))))
   (#t
    (set! /spec (@Make //T_/Spec '() (list (@Make //T_/Lvalues '() (@Mth_Sort /vars)) /cond)))))
  ; Finally, insert the specification statement or assertion: 
  (cond
   ((= (@GT (@I)) //T_/Statements)
    (@Paste_Over (@Make //T_/Statements '() (list /spec))))
   (#t
    (@Paste_Over /spec)))))

#t
