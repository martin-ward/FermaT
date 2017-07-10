;;; Scheme translation of WSL code
(define (/foreach-expand_call-1 //Depth //A/S_/Type)
 (set! //Temp //The_/Assigns)
 (set! /fl_flag1 0)
 (while (= /fl_flag1 0) 
  (cond
   ((equal? (@Value (@I)) (@Value (car (car //Temp))))
    (@Paste_Over (car (cdr (car //Temp))))
    (set! /fl_flag1 1))
   (#t
    (set! //Temp (cdr //Temp))
    (cond
     ((null? //Temp)
      (set! /fl_flag1 1))
     (#t
      (set! /fl_flag1 0)))))))

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
(define (@Expand_Call_Test)
 (cond
  ((= (@ST (@I)) //T_/Call)
   (cond
    ((equal? (@V (@I)) (@Make_Name "Z"))
     (@Fail "A call to the action Z is a special statement which terminates the action system."))
    (#t
     (let ((//N (@V (@I))))
      (while (and (not (= (@ST (@I)) //T_/Actions)) (@Up?)) 
       (@Up))
      (cond
       ((not (= (@ST (@I)) //T_/Actions))
        (@Fail "Action system not found"))
       (#t
        (@Down)
        (set! /fl_flag1 0)
        (while (= /fl_flag1 0) 
         (cond
          ((equal? (@V (@Get_n (@I) 1)) //N)
           (@Pass)
           (set! /fl_flag1 1))
          ((@Right?)
           (@Right)
           (set! /fl_flag1 0))
          (#t
           (set! /fl_flag1 1))))))
      (cond
       ((not (@Passed?))
        (@Fail "Action definition not found")))))))
  ((= (@ST (@I)) //T_/Proc_/Call)
   (@Goto_Defn (@V (@Get_n (@I) 1)) //T_/Proc)
   (cond
    ((= (@ST (@I)) //T_/Proc)
     (@Pass))
    (#t
     (@Fail "Procedure definition not found"))))
  ((= (@ST (@I)) //T_/Funct_/Call)
   (@Goto_Defn (@V (@Get_n (@I) 1)) //T_/Funct)
   (cond
    ((and (= (@ST (@I)) //T_/Funct) (or (> (@Size (@Get_n (@I) 4)) 1) (not (= (@ST (@Get_n (@Get_n (@I) 4) 1)) //T_/Skip))))
     (@Fail "Function definition includes statements"))
    ((= (@ST (@I)) //T_/Funct)
     (@Pass))
    (#t
     (@Fail "Function definition not found"))))
  (#t
   (@Fail "The selected item is not any kind of call."))))

(define (@Expand_Call_Code //Data)
 (let ((//Orig_/Pos (@Posn))
       (//S '()))
  (cond
   ((= (@ST (@I)) //T_/Call)
    ; Selected item is an action call
    (let ((//N (@V (@I))))
     (while (not (= (@ST (@I)) //T_/Actions)) 
      (@Up))
     (@Down)
     (while (not (equal? (@V (@Get_n (@I) 1)) //N)) 
      (@Right))
     (set! //S (@Get_n (@I) 2))
     (@Goto //Orig_/Pos)
     (@Splice_Over (@Cs //S))))
   ((= (@ST (@I)) //T_/Proc_/Call)
    (@Goto_Defn (@V (@Get_n (@I) 1)) //T_/Proc)
    (let ((//F/Params (@Cs (@Get_n (@I) 2)))
          (//Var_/F/Params (@Cs (@Get_n (@I) 3)))
          (//Stmnts (@Get_n (@I) 4))
          (//Assigs '())
          (//Assigs2 '()))
     (@Goto //Orig_/Pos)
     (set! //Params (@Cs (@Get_n (@I) 2)))
     (set! //Var_/Params (@Cs (@Get_n (@I) 3)))
     ;Assign values of actual params to formal params
     (while (> (gen-length //F/Params) 0) 
      (begin
       (set! //Assigs (cons (@Make //T_/Assign '() (list (wsl-ref //F/Params 1) (wsl-ref //Params 1))) //Assigs))
       (set! //F/Params (cdr //F/Params))
       (set! //Params (cdr //Params))))
     (while (> (gen-length //Var_/F/Params) 0) 
      (begin
       ;Need to make var params (LValues) into Variables
       (cond
        ((not (@LR_Equal? (wsl-ref //Var_/Params 1) (wsl-ref //Var_/F/Params 1)))
         (set! //Assigs (cons (@Make //T_/Assign '() (list (wsl-ref //Var_/F/Params 1) (@Var_To_Expn (wsl-ref //Var_/Params 1)))) //Assigs))
         (set! //Assigs2 (cons (@Make //T_/Assign '() (list (wsl-ref //Var_/Params 1) (@Var_To_Expn (wsl-ref //Var_/F/Params 1)))) //Assigs2))))
       (set! //Var_/F/Params (cdr //Var_/F/Params))
       (set! //Var_/Params (cdr //Var_/Params))))
     (@Edit)
     (@Paste_Over (@Make //T_/Var '() (list (@Make //T_/Assigns '() (reverse //Assigs)) //Stmnts)))
     (@Down_Last)
     (@Down_Last)
     ;At the end of the proc, assign values of formal var params to actual params
     (cond
      ((not (null? //Assigs2))
       (@Paste_After (@Make //T_/Assignment '() (reverse //Assigs2)))))
     (@Up)
     (@Up)
     (@End_Edit)
     (cond
      ((@Trans? //T/R_/Separate_/Right)
       (@Trans //T/R_/Separate_/Right "")))
     (cond
      ((@Trans? //T/R_/Separate_/Left)
       (@Trans //T/R_/Separate_/Left "")))))
   ((= (@ST (@I)) //T_/Funct_/Call)
    (let ((//Exps (@Cs (@Get_n (@I) 2)))
          (//Replacement_/Expression '()))
     (@Goto_Defn (@V (@Get_n (@I) 1)) //T_/Funct)
     (let ((//The_/Assigns-save //The_/Assigns)
           (//F/Pars (@Cs (@Get_n (@I) 2))))
      (set! //The_/Assigns '())
      (@Down_To 3)
      (@Edit_Parent)
      (set! //The_/Assigns (my-map @Components (@Components (@I))))
      (@Right)
      (@Right)
      (@Substitute_Parameter_Values //The_/Assigns)
      (set! //The_/Assigns '())
      (while (not (null? //F/Pars)) 
       (begin
        (set! //The_/Assigns (concat //The_/Assigns (list (list (car //F/Pars) (car //Exps)))))
        (set! //F/Pars (cdr //F/Pars))
        (set! //Exps (cdr //Exps))))
      (@Substitute_Parameter_Values //The_/Assigns)
      (set! //Replacement_/Expression (@I))
      (@Undo_Edit)
      (@Goto //Orig_/Pos)
      (@Paste_Over //Replacement_/Expression)
      (set! //The_/Assigns //The_/Assigns-save)))))))

(define (@Substitute_Parameter_Values //The_/Assigns-par)
 (let ((//The_/Assigns-save //The_/Assigns))
  (set! //The_/Assigns //The_/Assigns-par)
  (cond
   ((not (null? //The_/Assigns))
    (let ((//Temp-save //Temp))
     (set! //Temp '())
     (@Foreach_Variable /foreach-expand_call-1 0 (@AS_Type) 0)
     (cond
      ((null? (@Program))
       (@New_Program (@Skips))))
     (set! //Temp //Temp-save))))
  (set! //The_/Assigns //The_/Assigns-save)))

; Move from a proc/funct/bfunct call to the corresponding definition 
; (which may be several WHERE clauses up) 
(define (@Goto_Defn /name /type)
 (set! /fl_flag2 0)
 (while (= /fl_flag2 0) 
  (cond
   ((not (@Up?))
    (set! /fl_flag2 1))
   (#t
    (@Up)
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((= (@ST (@I)) //T_/Where)
       (set! /fl_flag1 1))
      ((@Up?)
       (@Up)
       (set! /fl_flag1 0))
      (#t
       (set! /fl_flag1 2))))
    (cond
     ((= /fl_flag1 2)
      (set! /fl_flag2 1))
     (#t
      (@Down_Last)
      (@Down)
      (while (and (or (not (= /type (@ST (@I)))) (not (equal? /name (@V (@Get_n (@I) 1))))) (@Right?)) 
       (@Right))
      (cond
       ((and (= /type (@ST (@I))) (equal? /name (@V (@Get_n (@I) 1))))
        (set! /fl_flag2 1))
       (#t
        (@Up)
        (@Up)
        (set! /fl_flag2 0)))))))))

#t
