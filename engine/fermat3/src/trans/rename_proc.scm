;;; Scheme translation of WSL code
(define (/foreach-rename_proc-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (set! /w_store (concat /w_store (list (@I))))
   (@Paste_Over (@Make //T_/X_/Proc_/Call '() (list (@Name (- (gen-length /w_store))) (@Make //T_/Expressions '() '())))))))

(define (/foreach-rename_proc-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) /old_name))
   (@Down)
   (@Paste_Over (@Name /new_name))
   (@Up))))

(define (/foreach-rename_proc-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/X_/Proc_/Call) (number? (@V (@Get_n (@I) 1))) (< (@V (@Get_n (@I) 1)) 0))
   (@Paste_Over (wsl-ref /w_store (- (@V (@Get_n (@I) 1)))))
   (@RC_Restore_Wheres /w_store))))

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
(define (@Rename_Proc_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Proc))
   (@Fail "Not a procedure"))
  (#t
   (@Pass))))

(define (@Rename_Proc_Code //Data)
 (let ((/calls '())
       (/orig_posn (@Posn))
       (/new_name-save /new_name)
       (/old_name-save /old_name)
       (/w_store-save /w_store))
  (set! /new_name (@Make_Name //Data))
  (set! /old_name (@V (@Get_n (@I) 1)))
  (set! /w_store '())
  ; Move up to where clause 
  (while (and (not (= (@ST (@I)) //T_/Where)) (@Up?)) 
   (@Up))
  ;Store any nested where clauses & replace with X_Proc_Call
  (@Foreach_Statement /foreach-rename_proc-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Restore the top where clause: 
  (@Paste_Over (wsl-ref /w_store (- (@V (@Get_n (@I) 1)))))
  (set! /calls (@Make_Set (my-map HEAD (@Proc_Calls (@Item)))))
  ; Store proc names and check against new name for a clash 
  (cond
   ((= (@Spec_Type (@Item)) //T_/Where)
    (set! /calls (union-n /calls (@Make_Set (my-map @V1 (@Cs (@Get_n (@I) 2))))))))
  (cond
   ((member /new_name /calls)
    (@Notice "The procedure name is already in use"))
   (#t
    (@Goto /orig_posn)
    ;Paste over new proc name
    (@Down)
    (@Paste_Over (@Name /new_name))
    (@Up)
    (while (and (not (= (@ST (@I)) //T_/Where)) (@Up?)) 
     (@Up))
    ;Check for calls to proc and replace with call to new proc name
    (@Foreach_Statement /foreach-rename_proc-2 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (@RC_Restore_Wheres /w_store)
  (@Goto /orig_posn)
  (set! /new_name /new_name-save)
  (set! /old_name /old_name-save)
  (set! /w_store /w_store-save)))

(define (@RC_Restore_Wheres /w_store-par)
 (let ((/w_store-save /w_store))
  (set! /w_store /w_store-par)
  ;Replace X_Proc_Call with correct where clause from w_store 
  (@Foreach_Statement /foreach-rename_proc-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /w_store /w_store-save)))

#t
