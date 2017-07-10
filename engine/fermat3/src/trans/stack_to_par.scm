;;; Scheme translation of WSL code
(define (/foreach-stack_to_par-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) /name))
   ; Check for a PUSH 
   (let ((/posn_n (@Posn_n)))
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((not (@Left?))
       (set! //O/K 0)
       (set! /fl_flag1 1))
      (#t
       (@Left)
       (cond
        ((and (= (@ST (@I)) //T_/Push) (@Equal? (@Get_n (@I) 1) /stack))
         (set! /fl_flag1 1))
        ((or (= (@ST (@I)) //T_/Proc) (member (@V /stack) (@Assigned (@I))))
         (set! //O/K 0)
         (set! /fl_flag1 1))
        (#t
         (set! /fl_flag1 0))))))
    (@To /posn_n)
    (cond
     ((= //O/K 0)
      (@Fail "A proc call has no PUSH")))))))

(define (/foreach-stack_to_par-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) /name))
   ; Check for a PUSH 
   (let ((/posn_n (@Posn_n))
         (/apar '()))
    (while (not (and (= (@ST (@I)) //T_/Push) (@Equal? (@Get_n (@I) 1) /stack))) 
     (@Left))
    (set! /apar (@Get_n (@I) 2))
    (@Paste_Over (@Skip))
    (@To /posn_n)
    (@Down_To 2)
    ; To value pars 
    (@Paste_Over (@Make //T_/Expressions '() (concat (@Cs (@I)) (list /apar))))
    (@Up)))))

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
; Convert stack operations to a procedure parameter 
; Find the stack by looking for a POP at the start of the proc body 
; Check that each call is preceded by a PUSH 
; Pass the PUSHed value as a val parameter instead of on the stack. 
(define (@Stack_To_Par_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Proc))
   (@Fail "Selected item is not a procedure definition"))
  (#t
   (let ((/posn (@Posn))
         (/name-save /name)
         (//O/K-save //O/K)
         (/stack-save /stack))
    (set! /name (@V (@Get_n (@I) 1)))
    (set! //O/K 1)
    (set! /stack '())
    (@Down_To 4)
    (@Down)
    ; to first statement of body 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((= (@ST (@I)) //T_/Pop)
       (set! /stack (@Get_n (@I) 2))
       (set! /fl_flag1 1))
      ((= (@ST (@I)) //T_/Var)
       (@Down_To 2)
       (@Down)
       (set! /fl_flag1 0))
      ((@Right?)
       (@Right)
       (set! /fl_flag1 0))
      (#t
       (set! /fl_flag1 1))))
    (cond
     ((null? /stack)
      (@Fail "No suitable stack in proc body"))
     (#t
      (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
       (@Up))
      (@Ateach_Statement /foreach-stack_to_par-1 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (cond
       ((= //O/K 1)
        (@Pass)))))
    (@Goto /posn)
    (set! /name /name-save)
    (set! //O/K //O/K-save)
    (set! /stack /stack-save)))))

(define (@Stack_To_Par_Code //Data)
 (let ((/posn (@Posn))
       (/name-save /name)
       (//O/K-save //O/K)
       (/stack-save /stack)
       (/par '())
       (/i 1))
  (set! /name (@V (@Get_n (@I) 1)))
  (set! //O/K 1)
  (set! /stack '())
  (cond
   ((not (equal? //Data ""))
    (set! /par (@Make_Name //Data)))
   (#t
    (set! /par (@Make_Name "par"))
    (while (member (@Make_Name (concat (@N_String /par) (@String /i))) (@Variables (@I))) 
     (set! /i (+ /i 1)))
    (set! /par (@Make_Name (concat (@N_String /par) (@String /i))))))
  (set! /par (@Make //T_/Var_/Lvalue /par '()))
  (@Down_To 4)
  (@Down)
  ; to first statement of body 
  (while (not (= (@ST (@I)) //T_/Pop)) 
   (cond
    ((= (@ST (@I)) //T_/Var)
     (@Down_To 2)
     (@Down))
    (#t
     (@Right))))
  (set! /stack (@Get_n (@I) 2))
  (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var (@Get_n (@I) 1)) (@Var_To_Expn /par))))))
  (while (not (= (@ST (@I)) //T_/Proc)) 
   (@Up))
  (@Down_To 2)
  ; To value pars 
  (@Paste_Over (@Make //T_/Lvalues '() (concat (@Cs (@I)) (list /par))))
  (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
   (@Up))
  (@Ateach_Statement /foreach-stack_to_par-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Trans //T/R_/Delete_/All_/Skips "")
  (@Goto /posn)
  (set! /name /name-save)
  (set! //O/K //O/K-save)
  (set! /stack /stack-save)))

#t
