;;; Scheme translation of WSL code
(define (/foreach-proc_to_funct-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) (@V /name)))
   (set! /vals (@Get_n (@I) 2))
   (set! /par (@Get_n (@Get_n (@I) 3) 1))
   (cond
    ((= /add_par 1)
     (set! /vals (@Make //T_/Expressions '() (concat (@Cs /vals) (list (@Lvalue_To_Expn /par)))))))
   (@Paste_Over (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /par) (@Make 205 0 '()))))))
   (@Down)
   (@Down_Last)
   (@Paste_Over (@Make //T_/Funct_/Call '() (list /name /vals)))
   (@Up)
   (@Up))))

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
; Convert a proc to a function 
; Check that there is one VAR parameter. 
; If it is used before assigned, then convert it to a value parameter 
; Otherwise, keep the same value parameters. 
; Convert VAR parameter to a local variable in the assigns list. 
(define (@Proc_To_Funct_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Proc))
   (@Fail "Current item is not a procedure definition"))
  ((< (gen-length (@Posn)) 2)
   (@Fail "Cannot move up from here to the WHERE clause"))
  ((not (= (@Size (@Get_n (@I) 3)) 1))
   (@Fail "There must be exactly one VAR parameter in the definition"))
  ((not (= (@ST (@Get_n (@Get_n (@I) 3) 1)) //T_/Var_/Lvalue))
   (@Fail "VAR parameter is not a simple variable"))
  (#t
   (@Pass))))

(define (@Proc_To_Funct_Code //Data)
 (let ((/name-save /name)
       (/vals-save /vals)
       (/assigns '())
       (/par-save /par)
       (/body (@Get_n (@I) 4))
       (/add_par-save /add_par)
       (/posn (@Posn_n)))
  (set! /name (@Get_n (@I) 1))
  (set! /vals (@Get_n (@I) 2))
  (set! /par (@Get_n (@Get_n (@I) 3) 1))
  (set! /add_par 0)
  (cond
   ((member (@V /par) (@UBA /body))
    ;Par is also an input parameter
    (set! /add_par 1)
    (set! /assigns (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /par) (@Var_To_Expn /par))))))
    (set! /vals (@Make //T_/Lvalues '() (concat (@Cs /vals) (list /par)))))
   (#t
    (set! /assigns (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /par) (@Make 205 0 '()))))))))
  (@Paste_Over (@Make //T_/Funct '() (list /name /vals /assigns /body (@Lvalue_To_Expn /par))))
  ; Now fix all the calls: 
  (@Up)
  (@Up)
  ; to WHERE 
  (@Foreach_Statement /foreach-proc_to_funct-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /name /name-save)
  (set! /vals /vals-save)
  (set! /par /par-save)
  (set! /add_par /add_par-save)))

#t
