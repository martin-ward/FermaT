;;; Scheme translation of WSL code
(define (/foreach-static_single_assignment-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/X_/Funct_/Call) (equal? (@V (@Get_n (@I) 1)) /phi))
   (@Fail "WSL code already contains phi function calls"))))

(define (/foreach-static_single_assignment-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/For)
   (@Trans //T/R_/For_/To_/While ""))))

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
; Convert WSL code to Static Single Assignment form by renaming 
; and adding phi function assignments. 
; Note: this uses !XP perlscript() to call the perl script bbtossa 
; Note: Call Globals_To_Pars and Var_Pars_To_Val_Pars if necessary 
; before applying this transformation. 
(define (@Static_Single_Assignment_Test)
 (let ((/phi-save /phi))
  (set! /phi (@Make_Name "phi"))
  (@Ateach_Expn /foreach-static_single_assignment-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (cond
   ((and (or (member //T_/Var (@Stat_Types (@I))) (member //T_/For (@Stat_Types (@I)))) (member //T_/Where (@Stat_Types (@I))))
    (cond
     ((@Check_Globals_In_Defns?)
      (@Pass))
     (#t
      (@Fail "There may be a reference to a local variable
in a PROC, FUNCT or BFUNCT body.")))))
  (cond
   ((not (@Failed?))
    (@Pass)))
  (set! /phi /phi-save)))

(define (@Static_Single_Assignment_Code //Data)
 (let ((/block_file "tmp_ssa.bb")
       (/ssa_file "tmp_ssa.ssa")
       (//S/S/A_blocks '())
       (/p 0)
       (/vars '())
       (/elts '())
       (/random (@Random //Omega)))
  ; Add a random prefix to filenames to avoid race condition clashes: 
  (set! /block_file (concat (string-append (@String /random) "_") /block_file))
  (set! /ssa_file (concat (string-append (@String /random) "_") /ssa_file))
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
  (cond
   ((member //T_/For (@Stat_Types (@I)))
    (display-list "Converting FOR loops...")
    (@Foreach_Statement /foreach-static_single_assignment-2 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (cond
   ((@Trans? //T/R_/Rename_/Local_/Vars)
    (display-list "Renaming local variables...")
    (@Trans //T/R_/Rename_/Local_/Vars "")))
  (display-list "Computing Basic Blocks...")
  (@Basic_Blocks (@I) /block_file)
  (display-list "Computing SSA form of basic blocks...")
  (cond
   ((@File_Exists? /ssa_file)
    (@Delete_File /ssa_file)))
  (perlscript "bbtossa" (concat (string-append /block_file " ") /ssa_file))
  (cond
   ((@File_Exists? /ssa_file)
    (display-list "Generating new WSL...")
    (set! //S/S/A_blocks (@Parse_Basic_Blocks /ssa_file))
    (@Paste_Over (@WSL_To_SSA (@I) //S/S/A_blocks /elts))
    (@Delete_File /block_file)
    (@Delete_File /ssa_file))
   (#t
    (display-list (string-append "ERROR: bbtossa failed to create " /ssa_file))))))

#t
