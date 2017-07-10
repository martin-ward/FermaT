;;; Scheme translation of WSL code
(define (/foreach-delete_redundant_regs-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (set! /count (+ /count (@Size (@Get_n (@I) 2)))))))

(define (/foreach-delete_redundant_regs-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) /pop_regs))
   (@Paste_Over (@Skip)))))

(define (/foreach-delete_redundant_regs-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/For)
   (@Trans //T/R_/For_/To_/While ""))))

(define (/foreach-delete_redundant_regs-4 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (cond
    ((@Trans? //T/R_/Globals_/To_/Pars)
     (display-list "Converting global variables in procedures to parameters...")
     (@Trans //T/R_/Globals_/To_/Pars ""))))))

(define (/foreach-delete_redundant_regs-5 //Depth //A/S_/Type)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__delete_redundant_regs__2 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (@Paste_Before /dummy)))))

(define (/foreach-delete_redundant_regs-6 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Proc_/Call)
   (@Down_To 2)
   ; to value pars 
   (@Paste_Over (@Make //T_/Expressions '() '()))
   (@Right)
   (@Paste_Over (@Make //T_/Lvalues '() '())))
  ((= (@ST (@I)) //T_/Where)
   (@Down_Last)
   (@Down)
   ; to first definition 
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     (cond
      ((= (@ST (@I)) //T_/Proc)
       (@Down_To 2)
       ; to value pars 
       (@Paste_Over (@Make //T_/Lvalues '() '()))
       (@Right)
       (@Paste_Over (@Make //T_/Lvalues '() '()))
       (@Up)))
     (cond
      ((not (@Right?))
       (set! /fl_flag1 1))
      (#t
       (@Right)
       (set! /fl_flag1 0))))))))

(define (/foreach-delete_redundant_regs-7 //Depth //A/S_/Type)
 (cond
  ((@Equal? (@I) /dummy)
   (@Delete))))

(define (/foreach-delete_redundant_regs-8 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Var) (@Trans? //T/R_/Remove_/Redundant_/Vars))
   (@Trans //T/R_/Remove_/Redundant_/Vars "")
   (@Trans //T/R_/Simplify ""))))

(define /%const__delete_redundant_regs__1 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "dummy_reference_variable") '()) (@Make 220 '() (list (@Make 207 (@Make_Name "ax") '()) (@Make 207 (@Make_Name "bx") '()) (@Make 207 (@Make_Name "cx") '()) (@Make 207 (@Make_Name "dx") '()) (@Make 207 (@Make_Name "sp") '()) (@Make 207 (@Make_Name "bp") '()) (@Make 207 (@Make_Name "si") '()) (@Make 207 (@Make_Name "di") '()) (@Make 207 (@Make_Name "cs") '()) (@Make 207 (@Make_Name "ds") '()) (@Make 207 (@Make_Name "ss") '()) (@Make 207 (@Make_Name "es") '()) (@Make 207 (@Make_Name "zf") '()) (@Make 207 (@Make_Name "cf") '()) (@Make 207 (@Make_Name "r15") '()))))))))
(define /%const__delete_redundant_regs__2 (@Make 110 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "exit_flag") '()) (@Make 205 1 '()))))))
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
(set! /dummy_compiler_data "                                       ")
(define (@Delete_Redundant_Regs_Test)
 (let ((/count-save /count))
  (set! /count 0)
  (cond
   ((member //T_/A_/S (@Stat_Types (@I)))
    (@Fail "Item contains an action system"))
   (#t
    ; Don't do Delete_Redundant_Regs if there are too many procs: 
    (@Foreach_Statement /foreach-delete_redundant_regs-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (cond
     ((> /count 200)
      (@Fail (string-append (string-append "Too many PROC/FUNCT definitions (" (@String /count)) ")")))
     (#t
      (@Pass)))))
  (set! /count /count-save)))

(define (@Delete_Redundant_Regs_Code //Data)
 (let ((/posn (@Posn))
       (/block_file (string-append (@String //Data) "tmp_drr.bb"))
       (/del_file (string-append (@String //Data) "tmp_drr.del"))
       (/data "")
       (/pop_regs-save /pop_regs)
       (//L '())
       (/random (@Random //Omega))
       (/dummy-save /dummy))
  (set! /pop_regs (@Make_Name "pop_regs"))
  (set! /dummy /%const__delete_redundant_regs__1)
  ; Add a random prefix to filenames to avoid race condition clashes: 
  (set! /block_file (concat (string-append (@String /random) "_") (@Strip_Char "$" /block_file)))
  (set! /del_file (concat (string-append (@String /random) "_") (@Strip_Char "$" /del_file)))
  (cond
   ((equal? //Data "")
    (display-list "Warning: no data file supplied"))
   ((@File_Exists? //Data)
    (set! /data (string-append " data=" //Data)))
   (#t
    (display-list "Warning: data file `" //Data "' not found!")))
  (@Foreach_Statement /foreach-delete_redundant_regs-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (cond
   ((member //T_/For (@Stat_Types (@I)))
    (display-list "Converting FOR loops...")
    (@Foreach_Statement /foreach-delete_redundant_regs-3 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (cond
   ((@Trans? //T/R_/Rename_/Local_/Vars)
    (display-list "Renaming local variables...")
    (@Trans //T/R_/Rename_/Local_/Vars "")))
  (@Down)
  (while (and (@Right?) (= (@ST (@I)) //T_/Assignment) (= (@Size (@I)) 1) (= (@ST (@Get_n (@Get_n (@I) 1) 1)) //T_/Var_/Lvalue) (@Starts_With? (@V (@Get_n (@Get_n (@I) 1) 1)) "var_")) 
   (begin
    (set! //L (cons (@I) //L))
    (@Delete)))
  (@Up)
  (@Foreach_Statement /foreach-delete_redundant_regs-4 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (display-list "Adding a dummy reference at the end of the program")
  ; Note: when we create the blocks file we link nodes which set exit_flag to 1 
  ; directly to the end node (node 0), so this assignment will be skipped over! 
  (cond
   ((= (@ST (@I)) //T_/Statements)
    (@Down_Last)
    (@Paste_After /dummy)
    (@Up)))
  (@Foreach_Statement /foreach-delete_redundant_regs-5 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (display-list "Computing Basic Blocks...")
  (@Basic_Blocks (@I) /block_file)
  (display-list "Computing Deletion List...")
  (cond
   ((@File_Exists? /del_file)
    (@Delete_File /del_file)))
  (perlscript "bb_list_redundant" (concat (concat (string-append /block_file " ") /del_file) /data))
  (cond
   ((@File_Exists? /del_file)
    (display-list "Generating new WSL...")
    (@Parse_Del_File /del_file)
    (@Delete_File /block_file)
    (@Delete_File /del_file))
   (#t
    (display-list (string-append "ERROR: bb_list_redundant failed to create " /del_file))))
  ; NB: This assumes that all parameters were introduced by TR_Globals_To_Pars! 
  (display-list "Finished deleting.")
  (@Goto '())
  (display-list "Removing parameters from local procs")
  (@Foreach_Statement /foreach-delete_redundant_regs-6 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (display-list "Restoring local vars")
  (@Down)
  (for-in //I //L 
   (@Paste_Before //I))
  (@Up)
  (cond
   ((@Trans? //T/R_/Restore_/Local_/Vars)
    (@Trans //T/R_/Restore_/Local_/Vars "")))
  (display-list "Removing dummy reference")
  (@Goto /posn)
  (@Foreach_Statement /foreach-delete_redundant_regs-7 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  ; May now be able to remove redundant variables: 
  (@Foreach_Statement /foreach-delete_redundant_regs-8 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! /pop_regs /pop_regs-save)
  (set! /dummy /dummy-save)))

; Read the list of `var posn length'  
(define (@Parse_Del_File /file)
 (let ((/port (@Open_Input_File /file))
       (/line "")
       (/word "")
       (/var '())
       (/posn '())
       (/len 0)
       (/p 0))
  (display-list "Reading deletion list...")
  (set! /line (@Read_Line /port))
  (while (not (@EOF? /line)) 
   (begin
    (set! /p 0)
    (let ((/-result- (@Parse_Word  /line /p /word)))
     (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /word (car /-result-)) (set! /-result- (cdr /-result-)))
    (set! /var (@Make_Name /word))
    (set! /p (@Skip_Spaces  /line /p))
    (cond
     ((not (equal? (substr /line /p 1) "("))
      (error "Badly formatted del line:" /line)))
    (set! /p (+ /p 1))
    (set! /posn '())
    (let ((/-result- (@Parse_Nums  /line /p /posn)))
     (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /posn (car /-result-)) (set! /-result- (cdr /-result-)))
    (set! /p (+ /p 1))
    (set! /p (@Skip_Spaces  /line /p))
    (let ((/-result- (@Parse_Num  /line /p /len)))
     (set! /p (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /len (car /-result-)) (set! /-result- (cdr /-result-)))
    (display-list "Moving to: " /posn " length = " /len)
    (@Goto /posn)
    (while (>= /len 1) 
     (begin
      (cond
       ((and (= (@ST (@I)) //T_/Assignment) (member /var (@Assigned (@I))))
        (@Down)
        ; To first assign 
        (while (and (not-member /var (@Assigned (@I))) (@Right?)) 
         (@Right))
        (cond
         ((member /var (@Assigned (@I)))
          (@PP_Item (@I) 80 "")
          (@Delete)))
        (@Up)
        (cond
         ((= (@Size (@I)) 0)
          (@Paste_Over (@Skip))))))
      (cond
       ((and (> /len 1) (@Right?))
        (@Right)))
      (set! /len (- /len 1))))
    #t
    (set! /line (@Read_Line /port))))
  (@Close_Input_Port /port)))

#t
