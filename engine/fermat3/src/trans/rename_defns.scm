;;; Scheme translation of WSL code
(define (/foreach-rename_defns-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (not (null? (gethash //Orig_/Name (@V (@Get_n (@I) 1))))))
   (@Down)
   (@Paste_Over (gethash //Orig_/Name (@V (@I)))))
  ((= (@ST (@I)) //T_/Where)
   (@Down_Last)
   (@Down)
   (cond
    ((and (= //T_/Proc (@ST (@I))) (not (null? (gethash //Orig_/Name (@V (@Get_n (@I) 1))))))
     (@Down)
     (@Paste_Over (gethash //Orig_/Name (@V (@I))))
     (@Up)))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((and (= //T_/Proc (@ST (@I))) (not (null? (gethash //Orig_/Name (@V (@Get_n (@I) 1))))))
       (@Down)
       (@Paste_Over (gethash //Orig_/Name (@V (@I))))
       (@Up))))))))

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
(define (@Rename_Defns_Test)
 (cond
  ((not-member //T_/Where (@Stat_Types (@I)))
   (@Fail "No WHERE clauses in selected item."))
  (#t
   (let ((//Defn_/Count-save //Defn_/Count))
    (set! //Defn_/Count (@Count_Proc_Defns (@I)))
    (for-in /name (@Hash_Keys //Defn_/Count) 
     (cond
      ((> (gethash //Defn_/Count /name) 1)
       (@Pass))))
    (set! //Defn_/Count //Defn_/Count-save))
   (cond
    ((not (@Passed?))
     (@Fail "No renaming is necessary"))))))

; Count how many times each proc is defined (to see if renaming is necessary) 
; Rename the procs, computing and discarding the Orig_Name table. 
(define (@Rename_Defns_Code //Data)
 (let ((//Defn_/Count-save //Defn_/Count)
       (//Orig_/Name-save //Orig_/Name))
  (set! //Defn_/Count (@Count_Proc_Defns (@I)))
  (set! //Orig_/Name (hash-table))
  (set! //Orig_/Name (@Rename_Procs  //Defn_/Count //Orig_/Name))
  (set! //Defn_/Count //Defn_/Count-save)
  (set! //Orig_/Name //Orig_/Name-save)))

(define (@Count_Proc_Defns //I)
 (let ((//Defn_/Count-save //Defn_/Count)
       (funct-result '()))
  (set! //Defn_/Count (hash-table))
  (@RP_Count_Proc_Defns //I '())
  (set! funct-result //Defn_/Count)
  (set! //Defn_/Count //Defn_/Count-save)
  funct-result))

(define (@RP_Count_Proc_Defns //I /stack)
 (cond
  ((and (= (@ST //I) //T_/Proc_/Call) (not (@RP_In_Set_List? (@V (@Get_n //I 1)) /stack)))
   (@Inc_Hash (@V (@Get_n //I 1)) //Defn_/Count))
  ((= (@ST //I) //T_/Where)
   (let ((/local '()))
    (for-in /defn (@Cs (@Get_n //I 2)) 
     (cond
      ((= (@ST /defn) //T_/Proc)
       (@Inc_Hash (@V (@Get_n /defn 1)) //Defn_/Count)
       (set! /local (cons (@V (@Get_n /defn 1)) /local)))))
    (set! /stack (cons (@Make_Set /local) /stack))
    (@RP_Count_Proc_Defns (@Get_n //I 1) /stack)
    (@RP_Count_Proc_Defns (@Get_n //I 2) /stack)))
  ((and (@Cs? //I) (@Has_Statements_Type? (@GT //I)))
   (for-in /comp (@Cs //I) 
    (@RP_Count_Proc_Defns /comp /stack)))))

; Rename the procs and proc calls which clash: ie Defn_Count.(name) > 1 
; Any global (ie non-local) calls get to keep their original names. 
; Fill in the Orig_Name table to map the new name to the original T_Name item. 
(define (@Rename_Procs //Defn_/Count-par //Orig_/Name-par)
 (let ((//Orig_/Name-save //Orig_/Name)
       (//Defn_/Count-save //Defn_/Count)
       (funct-result '()))
  (set! //Orig_/Name //Orig_/Name-par)
  (set! //Defn_/Count //Defn_/Count-par)
  (let ((//Curr_/Name-save //Curr_/Name)
        (/tab (hash-table)))
   (set! //Curr_/Name '())
   ; The initial table in the Curr_Name stack is for global proc calls, 
   ; so we record the current name (as an item): 
   (for-in /name (@Hash_Keys //Defn_/Count) 
    (cond
     ((> (gethash //Defn_/Count /name) 1)
      (puthash /tab /name (@Make //T_/Name /name '()))
      (puthash //Defn_/Count /name 1))))
   (set! //Curr_/Name (list /tab))
   (@Edit)
   (@RP_Rename_Procs)
   (@End_Edit)
   (set! //Curr_/Name //Curr_/Name-save))
  (set! funct-result //Orig_/Name)
  (set! //Orig_/Name //Orig_/Name-save)
  (set! //Defn_/Count //Defn_/Count-save)
  funct-result))

; Uses Curr_Name stack and Defn_Count to generate a new name as needed. 
; Updates Orig_Name table to map new name to old. 
(define (@RP_Rename_Procs)
 (cond
  ((and (or (= (@ST (@I)) //T_/Proc_/Call) (= (@ST (@I)) //T_/Proc)) (not (null? (@RP_Get_Name (@V (@Get_n (@I) 1)) //Curr_/Name))))
   (@Down)
   (@Paste_Over (@RP_Get_Name (@V (@I)) //Curr_/Name))
   (@Up)))
 (cond
  ((= (@ST (@I)) //T_/Where)
   ; Stack a new set of name items for the duration of this WHERE clause 
   (let ((/tab (hash-table))
         (/name-save /name))
    (set! /name '())
    (for-in /defn (@Cs (@Get_n (@I) 2)) 
     (cond
      ((and (= (@ST /defn) //T_/Proc) (not (null? (gethash //Defn_/Count (@V (@Get_n /defn 1))))))
       (set! /name (@V (@Get_n /defn 1)))
       (puthash /tab /name (@RP_New_Name (gethash //Defn_/Count /name) /name))
       (puthash //Orig_/Name (@V (gethash /tab /name)) (@Get_n /defn 1))
       (puthash //Defn_/Count /name (+ (gethash //Defn_/Count /name) 1)))))
    (set! //Curr_/Name (cons /tab //Curr_/Name))
    (set! /name /name-save))
   (@Down)
   (@RP_Rename_Procs)
   (@Right)
   (@RP_Rename_Procs)
   (@Up)
   (set! //Curr_/Name (cdr //Curr_/Name)))
  ((and (@Cs? (@I)) (@Has_Statements_Type? (@GT (@I))))
   (@Edit)
   (@Down)
   (@RP_Rename_Procs)
   (while (@Right?) 
    (begin
     (@Right)
     (@RP_Rename_Procs)))
   (@Up)
   (@End_Edit))))

; If the proposed new name is in the Defn_Count table, then it is already in use, 
; so keep modifying it until we get an unused name. 
(define (@RP_New_Name /count /name-par)
 (let ((/name-save /name)
       (/new (@Make_Name (concat (string-append (@N_String /name-par) "__") (@String /count))))
       (funct-result '()))
  (set! /name /name-par)
  (while (not (null? (gethash //Defn_/Count /new))) 
   (set! /new (@Make_Name (string-append "_" (@N_String /new)))))
  (set! funct-result (@Make //T_/Name /new '()))
  (set! /name /name-save)
  funct-result))

(define (@RP_Get_Name /name /stack)
 (let ((//R '()))
  (cond
   ((not (null? /stack))
    (let ((/tab (car /stack)))
     (set! //R (gethash /tab /name))
     (cond
      ((null? //R)
       (set! //R (@RP_Get_Name /name (cdr /stack))))))))
  //R))

(define (@RP_In_Set_List? /name //L)
 
 (and (not (null? //L)) (or (member /name (car //L)) (@RP_In_Set_List? /name (cdr //L)))))

(define (@Restore_Names //Orig_/Name-par)
 (let ((//Orig_/Name-save //Orig_/Name))
  (set! //Orig_/Name //Orig_/Name-par)
  (@Foreach_Statement /foreach-rename_defns-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //Orig_/Name //Orig_/Name-save)))

#t
