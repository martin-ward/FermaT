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
(define (@For_To_While_Test)
 (cond
  ((not (= (@ST (@I)) //T_/For))
   (@Fail "Selected item is not a FOR loop."))
  (#t
   (@Pass))))

(define (@For_To_While_Code //Data)
 (let ((/var (@Get_n (@I) 1))
       (/init (@Get_n (@I) 2))
       (/final (@Get_n (@I) 3))
       (/step (@Get_n (@I) 4))
       (/body (@Cs (@Get_n (@I) 5)))
       (//B '()))
  (set! //B (@Simplify_Cond (@Make 311 '() (list (@Make 310 '() (list (@Make 315 '() (list (@Var_To_Expn /step) (@Make 205 0 '()))) (@Make 316 '() (list (@Var_To_Expn /var) (@Var_To_Expn /final))))) (@Make 310 '() (list (@Make 314 '() (list (@Var_To_Expn /step) (@Make 205 0 '()))) (@Make 317 '() (list (@Var_To_Expn /var) (@Var_To_Expn /final)))))))))
  (set! /step (@Simplify_Expn (@Make 220 '() (list (@Var_To_Expn /var) (@Var_To_Expn /step)))))
  (@Paste_Over (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /init))))) (@Make 17 '() (list (@Make 141 '() (list //B (@Make 17 '() (append /body (list (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /var) (@Var_To_Expn /step)))))))))))))))))

#t
