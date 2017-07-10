;;; Scheme translation of WSL code
(define (/foreach-rename_local_vars-1 //Depth //A/S_/Type)
 (cond
  ((not (null? (gethash /new (@V (@I)))))
   (@Paste_Over (@Make (@ST (@I)) (gethash /new (@V (@I))) '())))))

(define (/foreach-rename_local_vars-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Var)
   (set! /count (+ /count 1))
   (set! /vars (@Assigned (@Get_n (@I) 1)))
   (let ((/new-save /new))
    (set! /new (hash-table))
    (for-in /var /vars 
     (puthash /new /var (@Make_Name (concat (string-append (concat (string-append /prefix "_") (@String /count)) "__") (@N_String /var)))))
    (@Down)
    (@Down)
    ; to first assign 
    (@Down)
    (@Paste_Over (@Make //T_/Var_/Lvalue (gethash /new (@V (@I))) '()))
    (@Up)
    (while (@Right?) 
     (begin
      (@Right)
      (@Down)
      (@Paste_Over (@Make //T_/Var_/Lvalue (gethash /new (@V (@I))) '()))
      (@Up)))
    (@Up)
    (@Right)
    ; to body 
    (@Foreach_Global_Var /foreach-rename_local_vars-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (set! /new /new-save))
   (@Up)
   ; back to VAR 
   ; Now convert the VAR clause to assignments plus the body 
   (for-in /assign (@Cs (@Get_n (@I) 1)) 
    (begin
     (@Paste_Before (@Make //T_/Assignment '() (list /assign)))
     (@Right)))
   (@Splice_Over (@Cs (@Get_n (@I) 2))))))

(define (/foreach-rename_local_vars-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Var)
   (set! /locals (union-n (@Assigned (@Get_n (@I) 1)) /locals)))
  ((= (@ST (@I)) //T_/For)
   (set! /locals (union-n (@Assigned (@Get_n (@I) 1)) /locals)))
  ((= (@ST (@I)) //T_/Where)
   (set! /globals (union-n /globals (my-reduce @Set_Union (my-map @Globals_In_Defn (@Cs (@Get_n (@I) 2)))))))))

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
; TODO: We have deleted the @Check_Globals_In_Defns? check. 
; We need to check for proc calls in the VAR body. 
(define (@Rename_Local_Vars_Test)
 (cond
  ((not-member //T_/Var (@Stat_Types (@I)))
   (@Fail "No VAR clauses in selected item"))
  (#t
   (@Pass))))

(define (@Rename_Local_Vars_Code //Data)
 (let ((/prefix-save /prefix)
       (/var-save /var)
       (/vars-save /vars)
       (/count-save /count))
  (set! /prefix (@String //Data))
  (set! /var '())
  (set! /vars '())
  (set! /count 0)
  (cond
   ((equal? /prefix "")
    (set! /prefix "var")))
  (@Foreach_Statement /foreach-rename_local_vars-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  #t
  (set! /prefix /prefix-save)
  (set! /var /var-save)
  (set! /vars /vars-save)
  (set! /count /count-save)))

; Check if any local variables are referenced as globals in a proc body. 
; Note: this test is more conservative than it could be, but the problem 
; can be fixed anyway by converting globals to parameters. 
; Also checks FOR loops (since these are converted to VARS by the 
; Static_Single_Assignment transformation) 
(define (@Check_Globals_In_Defns?)
 (let ((/locals-save /locals)
       (/globals-save /globals)
       (funct-result '()))
  (set! /locals '())
  (set! /globals '())
  (@Foreach_Statement /foreach-rename_local_vars-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result (null? (intersection-n /locals /globals)))
  (set! /locals /locals-save)
  (set! /globals /globals-save)
  funct-result))

; Get the list of global variables in the given definition: 
(define (@Globals_In_Defn /defn)
 
 (@Set_Difference (@Set_Difference (@Variables (@Get_n /defn 4)) (@Assigned (@Get_n /defn 2))) (@Assigned (@Get_n /defn 3))))

#t
