;;; Scheme translation of WSL code
(define (/foreach-syntactic_slice-1 //Depth //A/S_/Type)
 (cond
  ((null? (@Gen_TVs (@I) //A/S/Type))
   (@Paste_Over (@Skips)))))

(define (/foreach-syntactic_slice-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Where) (@Cs? (@Get_n (@I) 2)))
   (@Down_Last)
   (@Down)
   (cond
    ((= (@ST (@I)) //T_/Proc)
     (puthash /orig_val (@V (@Get_n (@I) 1)) (@Elements (@Get_n (@I) 2)))
     (puthash /orig_var (@V (@Get_n (@I) 1)) (@Elements (@Get_n (@I) 3)))))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((= (@ST (@I)) //T_/Proc)
       (puthash /orig_val (@V (@Get_n (@I) 1)) (@Elements (@Get_n (@I) 2)))
       (puthash /orig_var (@V (@Get_n (@I) 1)) (@Elements (@Get_n (@I) 3))))))))))

(define (/foreach-syntactic_slice-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Trans //T/R_/Globals_/To_/Pars "")
   (@Trans //T/R_/Var_/Pars_/To_/Val_/Pars ""))))

(define (/foreach-syntactic_slice-4 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Proc_/Call)
   (set! /name (@V (@Get_n (@I) 1)))
   (@Down_To 2)
   (@Paste_Over (@SS_Filter (@I) (gethash /val_keep /name)))
   (@Right)
   (@Paste_Over (@SS_Filter (@I) (gethash /var_keep /name))))))

(define (/foreach-syntactic_slice-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Var) (null? (intersection-n //Call_/Types_/Set (@Stat_Types (@Get_n (@I) 2)))) (not (null? (@Set_Difference (@Assigned (@Get_n (@I) 1)) (@Used (@Get_n (@I) 2))))))
   (let ((/new '())
         (/used (@Used (@Get_n (@I) 2))))
    (for-in /assign (@Cs (@Get_n (@I) 1)) 
     (cond
      ((member (@V (@Get_n /assign 1)) /used)
       (set! /new (cons /assign /new)))))
    (cond
     ((null? /new)
      (@Splice_Over (@Cs (@Get_n (@I) 2))))
     (#t
      (@Down)
      (@Paste_Over (@Make //T_/Assigns '() (reverse /new)))
      (@Up)))))))

(define (/foreach-syntactic_slice-6 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Where) (@Cs? (@Get_n (@I) 2)))
   (@Down_Last)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((@Trans? //T/R_/Delete_/Item)
      (@Trans //T/R_/Delete_/Item "")
      (cond
       ((not (= (@GT (@I)) //T_/Definition))
        ; We deleted the last defn 
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0))))
     ((@Right?)
      (@Right)
      (set! /fl_flag1 0))
     (#t
      (set! /fl_flag1 1)))))))

(define (/foreach-syntactic_slice-7 //Depth //A/S_/Type)
 (@Down)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (cond
   ((@Is_Improper?)
    (@Delete_Rest)
    (set! /fl_flag1 1))
   ((@Right?)
    (@Right)
    (set! /fl_flag1 0))
   (#t
    (set! /fl_flag1 1)))))

(define (/foreach-syntactic_slice-8 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Floop)
   (cond
    ((and (= (@Size (@I)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Floop) (@Gen_Reducible? (@Get_n (@Get_n (@I) 1) 1) (@AS_Type)))
     (@Trans //T/R_/Double_/To_/Single_/Loop "")))
   (cond
    ((@Is_Dummy?)
     (@Trans //T/R_/Remove_/Dummy_/Loop ""))))))

(define (/foreach-syntactic_slice-9 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Call) (not (null? (gethash /target (@V (@I))))))
   (@Paste_Over (@Make //T_/Call (gethash /target (@V (@I))) '())))))

(define (/foreach-syntactic_slice-10 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/S)
   (let ((/target-save /target)
         (/start (@V (@Get_n (@I) 1)))
         (/size (@Size (@Get_n (@I) 2)))
         (/name-save /name)
         (/n 0)
         (/reached (hash-table))
         (/calls (hash-table))
         (/todo '())
         (/new '()))
    (set! /target (hash-table))
    (set! /name '())
    (for-in /action (@Cs (@Get_n (@I) 2)) 
     (cond
      ((and (= (@Size (@Get_n /action 2)) 1) (= (@ST (@Get_n (@Get_n /action 2) 1)) //T_/Call))
       (puthash /target (@V (@Get_n /action 1)) (@V (@Get_n (@Get_n /action 2) 1))))))
    ; Replace targets by final target (with a crude loop check) 
    ; Doing it in reverse order may be more efficient since actions 
    ; are more likely to call later ones (which will already be fixed). 
    ; Technically we should do this in bottom-up order of the call graph. 
    (for-in /action (reverse (@Cs (@Get_n (@I) 2))) 
     (begin
      (set! /name (@V (@Get_n /action 1)))
      (cond
       ((not (null? (gethash /target /name)))
        (set! /n 0)
        (while (and (< /n /size) (not (null? (gethash /target (gethash /target /name))))) 
         (begin
          (set! /n (+ /n 1))
          (puthash /target /name (gethash /target (gethash /target /name)))))))))
    (cond
     ((equal? (gethash /target /start) (@Make_Name "Z"))
      ; Whole action system is redundant: 
      (@Paste_Over (@Skip)))
     (#t
      (cond
       ((not (null? (gethash /target /start)))
        (@Down)
        (@Paste_Over (@Make //T_/Name (gethash /target /start) '()))
        (@Up)))
      ; Update the calls: 
      (@Foreach_Non_Action_Statement /foreach-syntactic_slice-9 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      ; Delete unreachable actions 
      (for-in /action (@Cs (@Get_n (@I) 2)) 
       (puthash /calls (@V (@Get_n /action 1)) (my-map HEAD (@Calls (@Get_n /action 2)))))
      (puthash /reached (@V (@Get_n (@I) 1)) 1)
      (set! /todo (list (@V (@Get_n (@I) 1))))
      (while (not (null? /todo)) 
       (begin
        (set! /name (car /todo))
        (set! /todo (cdr /todo))
        (for-in /call (gethash /calls /name) 
         (cond
          ((null? (gethash /reached /call))
           (puthash /reached /call 1)
           (set! /todo (cons /call /todo)))))))
      (for-in /action (@Cs (@Get_n (@I) 2)) 
       (cond
        ((not (null? (gethash /reached (@V (@Get_n /action 1)))))
         (set! /new (cons /action /new)))))
      (@Down_Last)
      (@Paste_Over (@Make //T_/Actions '() (reverse /new)))
      (@Up)))
    (set! /target /target-save)
    (set! /name /name-save)))))

(define (/foreach-syntactic_slice-11 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Where) (@Cs? (@Get_n (@I) 2)))
   (@Down_Last)
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((= (@ST (@I)) //T_/Proc)
       (set! /name (@V (@Get_n (@I) 1)))
       (@Down_To 2)
       (puthash /val_keep /name (@SS_Keep (@Cs (@I)) (gethash /orig_val /name)))
       (@Paste_Over (@Grep_Comps (@I) (gethash /orig_val /name)))
       (@Right)
       (puthash /var_keep /name (@SS_Keep (@Cs (@I)) (gethash /orig_var /name)))
       (@Paste_Over (@Grep_Comps (@I) (gethash /orig_var /name)))
       (@Up)))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))))

(define (/foreach-syntactic_slice-12 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Proc_/Call)
   (set! /name (@V (@Get_n (@I) 1)))
   (@Down_To 2)
   (@Paste_Over (@SS_Filter (@I) (gethash /val_keep /name)))
   (@Right)
   (@Paste_Over (@SS_Filter (@I) (gethash /var_keep /name)))
   (@Up)
   ; NB: The proc call will have already been deleted if there are no 
   ; var parameters (including global vars) required 
  )))

(define (/foreach-syntactic_slice-13 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/S)
   (let ((/preds (hash-table))
         (/this_level '())
         (/next_level '())
         (/done (hash-table))
         (/d 0))
    (for-in /action (@Cs (@Get_n (@I) 2)) 
     (for-in /pair (@Calls (@Get_n /action 2)) 
      (puthash /preds (wsl-ref /pair 1) (union-n (list (@V (@Get_n /action 1))) (gethash /preds (wsl-ref /pair 1))))))
    (puthash /done (@Make_Name "Z") 1)
    (set! /next_level (list (@Make_Name "Z")))
    (while (not (null? /next_level)) 
     (begin
      (set! /d (+ /d 1))
      (set! /this_level /next_level)
      (set! /next_level '())
      (for-in /name /this_level 
       (begin
        (cond
         ((not (null? (gethash /dist /name)))
          (error "@Action_Distances:" (@N_String /name) "encountered more than once!!!")))
        (puthash /dist /name /d)
        (for-in /pred (gethash /preds /name) 
         (cond
          ((null? (gethash /done /pred))
           (puthash /done /pred 1)
           (set! /next_level (cons /pred /next_level)))))))))))))

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
(define (@Syntactic_Slice_Test)
 (cond
  ((not (= (@GT (@I)) //T_/Statements))
   (@Fail "Syntactic_Slice can only slice a statement sequence"))
  (#t
   (@Pass))))

; Preprocess: paste SKIP over any statement sequences 
; with empty terminal values (eg DO SKIP OD). 
; This will avoid dying on line 843 of Blocks.pm 
; Compute the basic blocks file 
; Convert to SSA form via bbtossa 
; Apply ssa_slice to get a sliced SSA blocks file 
; Read the sliced blocks 
; Delete (SKIP) any statements which assign to variables 
;   which are not needed 
; Data string is of the form var[@posn],... and may include flags -v, -f, -b 
(define (@Syntactic_Slice_Code //Data)
 (let ((/block_file "tmp_ss.bb")
       (/ssa_file "tmp_ss.ssa")
       (/slice_file "tmp_ss.sli")
       (/blocks '())
       (/orig_val-save /orig_val)
       (/orig_var-save /orig_var)
       (/forward (if (>= (my-index "-f" //Data) 0) 1 0)))
  (set! /orig_val (hash-table))
  (set! /orig_var (hash-table))
  ; Avoid Blocks.pm dying due to blocks which are not linked to END node: 
  (@Foreach_Stats /foreach-syntactic_slice-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (cond
   ((equal? //Data "")
    (display-list "No slicing criterion given!"))
   (#t
    (@Edit)
    (cond
     ((and (= (@ST (@I)) //T_/Statements) (= (@Size (@I)) 1) (= (@ST (@Get_n (@I) 1)) //T_/Var))
      (@Down)
      (@Down_To 2))
     ((= (@ST (@I)) //T_/Var)
      (@Down_To 2)))
    (@Edit)
    ; Record original pars for each proc 
    (@Foreach_Statement /foreach-syntactic_slice-2 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (cond
     ((member //T_/Where (@Stat_Types (@I)))
      (display-list "Converting global variables to parameters...")
      (@Foreach_Statement /foreach-syntactic_slice-3 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))))
    (display-list "Computing Basic Blocks...")
    (@Basic_Blocks (@I) /block_file)
    (display-list "Computing SSA form...")
    (cond
     ((@File_Exists? /ssa_file)
      (@Delete_File /ssa_file)))
    (perlscript "bbtossa" (concat (string-append /block_file " ") /ssa_file))
    (cond
     ((not (@File_Exists? /ssa_file))
      (display-list (string-append "bbtossa failed to create " /ssa_file)))
     (#t
      (display-list (string-append (string-append "Slicing on " //Data) "..."))
      (cond
       ((@File_Exists? /slice_file)
        (@Delete_File /slice_file)))
      (perlscript "ssa_slice" (concat (concat (concat (string-append (concat (string-append /ssa_file " ") /slice_file) " ") //Quote) //Data) //Quote))
      (cond
       ((not (@File_Exists? /slice_file))
        (display-list (string-append "ssa_slice failed to create " /slice_file)))
       (#t
        (set! /blocks (@Parse_Basic_Blocks /slice_file))
        (cond
         (#t
          (@Delete_File /block_file)
          (@Delete_File /ssa_file)
          (@Delete_File /slice_file)))
        (@Trim_WSL_From_SSA /blocks /forward)
        (@Trim_Extra_Pars /orig_val /orig_var)))))
    (@End_Edit)
    (@End_Edit)))
  (set! /orig_val /orig_val-save)
  (set! /orig_var /orig_var-save)))

; Delete the WSL code which does not appear in the (SSA format) blocks list: 
; Format of a block is: <node, posn, len, succs, type, links, control, phi, assigns> 
(set! //Undefined_/Value (@Make //T_/Variable (@Make_Name "__UNDEF__") '()))
(define (@Trim_WSL_From_SSA /blocks /forward)
 (let ((/posn (@Posn))
       (/entry (car /blocks))
       (/block '())
       (/wanted '())
       (/name-save /name)
       (/val_keep-save /val_keep)
       (/var_keep-save /var_keep)
       (/distance (@Action_Distances (@I))))
  (set! /name '())
  (set! /val_keep (hash-table))
  (set! /var_keep (hash-table))
  (set! /blocks (cdr /blocks))
  (for-in /block /blocks 
   (begin
    (set! /wanted '())
    ; Process control vars: 
    (for-in /elt (wsl-ref /block 7) 
     (set! /wanted (union-n /wanted (list (@SSA_Orig_Elt /elt)))))
    ; Process assignments (we can ignore the phi functions): 
    (for-in /assign (wsl-ref /block 9) 
     (set! /wanted (union-n /wanted (list (@SSA_Orig_Elt (car /assign))))))
    (cond
     ((not (@Valid_Posn? (@Program) (wsl-ref /block 2)))
      ; Presumably an enclosing IF or WHILE was deleted 
     )
     (#t
      (@Goto (wsl-ref /block 2))
      (set! /len (wsl-ref /block 3))
      (cond
       ((and (= /len 1) (or (equal? (wsl-ref /block 5) "IF") (equal? (wsl-ref /block 5) "WHILE Header") (equal? (wsl-ref /block 5) "FOR Header")))
        ; Check that there are some control vars 
        (cond
         ((and (null? (wsl-ref /block 7)) (= /forward 0))
          (@SS_Delete /distance))))
       ((and (= /len 1) (or (equal? (wsl-ref /block 5) "WHERE Header") (equal? (wsl-ref /block 5) "FLOOP Header") (equal? (wsl-ref /block 5) "Save") (equal? (wsl-ref /block 5) "Restore") (equal? (wsl-ref /block 5) "FOR Init") (equal? (wsl-ref /block 5) "FOR Footer")))
        ; Skip this block 
       )
       ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "ACTION "))
        ; No code in the action header node 
       )
       ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC Return "))
        ; A return node has var := var assignments 
        ; Delete unwanted VAR parameters 
        ; and record the index numbers of the wanted ones 
        (set! /name (@Make_Name (substr (wsl-ref /block 5) 12)))
        (puthash /var_keep /name (@SS_Keep (@Cs (@I)) /wanted))
        (@Paste_Over (@Grep_Comps (@I) /wanted))
        (@Up))
       ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC Header "))
        ; A header node has val := < > assignments 
        ; Delete unwanted value parameters 
        ; and record the index numbers of the wanted ones 
        (set! /name (@Make_Name (substr (wsl-ref /block 5) 12)))
        (@Down_To 2)
        ; value pars 
        (puthash /val_keep /name (@SS_Keep (@Cs (@I)) /wanted))
        (@Paste_Over (@Grep_Comps (@I) /wanted))
        (@Up))
       ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC CALL 1"))
        #t)
       ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC CALL 2"))
        #t)
       ((or (= (@ST (@I)) //T_/Call) (= (@ST (@I)) //T_/Exit) (= (@ST (@I)) //T_/Comment))
        ; Don't delete CALLs or EXITs or comments here 
       )
       ((and (= /len 1) (equal? (wsl-ref /block 5) "VAR Init"))
        ; If an initialisation is not required, 
        ; then initialise to undef 
        (let ((/new '()))
         (for-in /assign (@Cs (@I)) 
          (cond
           ((member (@Struct_Elts (@Get_n /assign 1)) /wanted)
            (set! /new (cons /assign /new)))
           (#t
            (set! /new (cons (@Make //T_/Assign '() (list (@Get_n /assign 1) //Undefined_/Value)) /new)))))
         (@Paste_Over (@Make //T_/Assigns '() (reverse /new)))))
       ((null? (intersection-n (@Elts_Assigned (@I)) /wanted))
        (@Paste_Over (@Skip))))
      (while (not (= /len 1)) 
       (begin
        (@Right)
        (set! /len (- /len 1))
        (cond
         ((and (= /len 1) (or (equal? (wsl-ref /block 5) "IF") (equal? (wsl-ref /block 5) "WHILE Header") (equal? (wsl-ref /block 5) "FOR Header")))
          ; Check that there are some control vars 
          (cond
           ((and (null? (wsl-ref /block 7)) (= /forward 0))
            (@SS_Delete /distance))))
         ((and (= /len 1) (or (equal? (wsl-ref /block 5) "WHERE Header") (equal? (wsl-ref /block 5) "FLOOP Header") (equal? (wsl-ref /block 5) "Save") (equal? (wsl-ref /block 5) "Restore") (equal? (wsl-ref /block 5) "FOR Init") (equal? (wsl-ref /block 5) "FOR Footer")))
          ; Skip this block 
         )
         ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "ACTION "))
          ; No code in the action header node 
         )
         ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC Return "))
          ; A return node has var := var assignments 
          ; Delete unwanted VAR parameters 
          ; and record the index numbers of the wanted ones 
          (set! /name (@Make_Name (substr (wsl-ref /block 5) 12)))
          (puthash /var_keep /name (@SS_Keep (@Cs (@I)) /wanted))
          (@Paste_Over (@Grep_Comps (@I) /wanted))
          (@Up))
         ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC Header "))
          ; A header node has val := < > assignments 
          ; Delete unwanted value parameters 
          ; and record the index numbers of the wanted ones 
          (set! /name (@Make_Name (substr (wsl-ref /block 5) 12)))
          (@Down_To 2)
          ; value pars 
          (puthash /val_keep /name (@SS_Keep (@Cs (@I)) /wanted))
          (@Paste_Over (@Grep_Comps (@I) /wanted))
          (@Up))
         ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC CALL 1"))
          #t)
         ((and (= /len 1) (@Starts_With? (wsl-ref /block 5) "PROC CALL 2"))
          #t)
         ((or (= (@ST (@I)) //T_/Call) (= (@ST (@I)) //T_/Exit) (= (@ST (@I)) //T_/Comment))
          ; Don't delete CALLs or EXITs or comments here 
         )
         ((and (= /len 1) (equal? (wsl-ref /block 5) "VAR Init"))
          ; If an initialisation is not required, 
          ; then initialise to undef 
          (let ((/new '()))
           (for-in /assign (@Cs (@I)) 
            (cond
             ((member (@Struct_Elts (@Get_n /assign 1)) /wanted)
              (set! /new (cons /assign /new)))
             (#t
              (set! /new (cons (@Make //T_/Assign '() (list (@Get_n /assign 1) //Undefined_/Value)) /new)))))
           (@Paste_Over (@Make //T_/Assigns '() (reverse /new)))))
         ((null? (intersection-n (@Elts_Assigned (@I)) /wanted))
          (@Paste_Over (@Skip))))))))))
  (@Goto '())
  ; Fix up the proc calls by applying the changes made to the declarations: 
  (@Foreach_Statement /foreach-syntactic_slice-4 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Final simplifications: 
  (@Trans //T/R_/Delete_/All_/Skips "")
  ; Remove unused local variables 
  (@Foreach_Statement /foreach-syntactic_slice-5 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Delete uncalled procedures: 
  (@Foreach_Statement /foreach-syntactic_slice-6 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Remove unreachable code: 
  (@Down)
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((@Is_Improper?)
     (@Delete_Rest)
     (set! /fl_flag1 1))
    ((@Right?)
     (@Right)
     (set! /fl_flag1 0))
    (#t
     (set! /fl_flag1 1))))
  (@Up)
  (@Foreach_Stats /foreach-syntactic_slice-7 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Remove dummy loops: 
  (@Foreach_Statement /foreach-syntactic_slice-8 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; Unfold elementary actions: 
  ; For each elementary action, record it's target, then update all calls. 
  ; Then delete all unreachable actions 
  (@Foreach_Statement /foreach-syntactic_slice-10 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Trans //T/R_/Delete_/All_/Skips "")
  (set! /name /name-save)
  (set! /val_keep /val_keep-save)
  (set! /var_keep /var_keep-save)))

; Return a new item of the same type as I with only those components 
; whose elements are in the wanted set: 
(define (@Grep_Comps //I /wanted)
 (let ((//R '()))
  (for-in /v (@Cs //I) 
   (cond
    ((member (@Struct_Elts /v) /wanted)
     (set! //R (cons /v //R)))))
  (@Make (@ST //I) '() (reverse //R))))

; Return a list of 0's and 1's indicating which elements of L are wanted 
(define (@SS_Keep //L /wanted)
 (let ((//R '()))
  (for-in /v //L 
   (cond
    ((member (@Struct_Elts /v) /wanted)
     (set! //R (cons 1 //R)))
    (#t
     (set! //R (cons 0 //R)))))
  (reverse //R)))

; Filted the components of I using the `keep' bitmask (a list of 0's and 1's) 
(define (@SS_Filter //I /keep)
 (let ((//R '()))
  (cond
   ((not (null? /keep))
    (for-in /v (@Cs //I) 
     (begin
      (cond
       ((= (car /keep) 1)
        (set! //R (cons /v //R))))
      (set! /keep (cdr /keep))))))
  (@Make (@ST //I) '() (reverse //R))))

(define (@Trim_Extra_Pars /orig_val-par /orig_var-par)
 (let ((/orig_var-save /orig_var)
       (/orig_val-save /orig_val))
  (set! /orig_var /orig_var-par)
  (set! /orig_val /orig_val-par)
  (let ((/name-save /name)
        (/val_keep-save /val_keep)
        (/var_keep-save /var_keep))
   (set! /name '())
   (set! /val_keep (hash-table))
   (set! /var_keep (hash-table))
   (@Foreach_Statement /foreach-syntactic_slice-11 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (@Foreach_Statement /foreach-syntactic_slice-12 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /name /name-save)
   (set! /val_keep /val_keep-save)
   (set! /var_keep /var_keep-save))
  (set! /orig_var /orig_var-save)
  (set! /orig_val /orig_val-save)))

; Record distance to CALL Z for each action 
; (this assumes all action systems are regular and action names are unique) 
(define (@Action_Distances //I)
 (let ((/dist-save /dist)
       (funct-result '()))
  (set! /dist (hash-table))
  (cond
   ((member //T_/A_/S (@Stat_Types //I))
    (@Edit)
    (@Paste_Over //I)
    (@Foreach_Statement /foreach-syntactic_slice-13 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Undo_Edit)))
  (set! funct-result /dist)
  (set! /dist /dist-save)
  funct-result))

; `Delete' selected statement by pasting over a CALL, EXIT or SKIP as appropriate 
(define (@SS_Delete /distance)
 (let ((/tvs (@TVs))
       (/types (@Stat_Types (@I))))
  (cond
   ((member //T_/Call /types)
    (@Paste_Over (@Make //T_/Call (@Smallest_Call (@I) /distance) '())))
   ((equal? /tvs (list 0))
    (@Paste_Over (@Skip)))
   ((null? /tvs)
    (@Paste_Over (@Make //T_/Abort '() '())))
   (#t
    ; Find largest TV and paste over an EXIT 
    (@Paste_Over (@Make //T_/Exit (my-reduce MAX /tvs) '()))))))

(define (@Smallest_Call //I /distance)
 (let ((/min_name '())
       (/min_dist 999999))
  (for-in /pair (@Calls //I) 
   (cond
    ((< (gethash /distance (wsl-ref /pair 1)) /min_dist)
     (set! /min_name (wsl-ref /pair 1))
     (set! /min_dist (gethash /distance (wsl-ref /pair 1))))))
  /min_name))

#t
