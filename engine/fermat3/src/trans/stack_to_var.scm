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
(define (@Stack_To_Var_Test)
 (cond
  ((= (@GT (@I)) //T_/Statements)
   (@Down)))
 (cond
  ((not (= (@ST (@I)) //T_/Push))
   (@Fail "Selected item is not a PUSH"))
  ((not (= (@ST (@Get_n (@I) 2)) //T_/Variable))
   (@Fail "Saved value is not a variable"))
  ((not (@Right?))
   (@Fail "Nothing to the right of the PUSH"))
  (#t
   (let ((/var (@Get_n (@I) 1))
         (/val (@Get_n (@I) 2)))
    (@Right)
    (while (and (or (not (= //T_/Pop (@ST (@I)))) (not (@LR_Equal? (@Get_n (@I) 1) /val)) (not (@Equal? (@Get_n (@I) 2) /var))) (not-member (@V /var) (@Assigned (@I))) (@Right?)) 
     (@Right))
    (cond
     ((and (= //T_/Pop (@ST (@I))) (@LR_Equal? (@Get_n (@I) 1) /val) (@Equal? (@Get_n (@I) 2) /var))
      (@Pass))
     ((member (@V /var) (@Assigned (@I)))
      (@Fail "Stack is modified after the PUSH"))
     ((not (@Right?))
      (@Fail "No suitable POP for this PUSH")))))))

(define (@Stack_To_Var_Code //Data)
 (cond
  ((= (@GT (@I)) //T_/Statements)
   (@Down)))
 (let ((/p1 (@Posn_n))
       (/p2 0)
       (/var (@Get_n (@I) 1))
       (/val (@Get_n (@I) 2))
       (/body '()))
  (while (not (and (= (@ST (@I)) //T_/Pop) (@Equal? (@Get_n (@I) 2) /var) (@LR_Equal? (@Get_n (@I) 1) /val))) 
   (@Right))
  (set! /p2 (@Posn_n))
  (set! /body (@Sub_Seg (@Cs (@Parent)) (+ /p1 1) (- /p2 1)))
  (cond
   ((null? /body)
    (set! /body (list (@Skip)))))
  (@Up)
  (@Paste_Over (@Make //T_/Statements '() (concat (concat (@Sub_Seg (@Cs (@I)) 1 (- /p1 1)) (list (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /val) (@Var_To_Expn /val))))) (@Make 17 '() /body))))) (@Final_Seg (@Cs (@I)) (+ /p2 1)))))
  (@Down_To /p1)))

#t
