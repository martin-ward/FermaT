;;; Scheme translation of WSL code
(define (/foreach-merge_calls_in_action-1 //Depth //A/S_/Type)
 (@Down)
 (set! /fl_flag2 0)
 (while (= /fl_flag2 0) 
  (begin
   (cond
    ((and (= (@Spec_Type (@Item)) //T_/Cond) (member //T_/Call (@Stat_Types (@Item))))
     ;If this IF is followed by a Call then absorb it
     (set! //P (@Posn))
     (cond
      ((@Right?)
       (@Right)
       (while (and (not (@Components? (@Item))) (not (= (@Spec_Type (@Item)) //T_/Call)) (@Right?)) 
        (@Right))
       (cond
        ((= (@Spec_Type (@Item)) //T_/Call)
         (@Goto //P)
         (while (not (= (@Spec_Type (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Call)) 
          (@Trans //T/R_/Absorb_/Right ""))
         (@Trans //T/R_/Absorb_/Right "")
         (@Down)
         (set! /fl_flag1 0)
         (while (= /fl_flag1 0) 
          (cond
           ((member //T_/Call (@Stat_Types (@Item)))
            (@Down_Last)
            (@Down)
            (while (and (not (= (@Spec_Type (@Item)) //T_/Call)) (@Right?)) 
             (@Right))
            (cond
             ((and (= (@Spec_Type (@Item)) //T_/Call) (@Right?))
              (@Delete_Rest)))
            (@Up)
            (@Up)
            (cond
             ((not (@Right?))
              (set! /fl_flag1 1))
             (#t
              (@Right)
              (set! /fl_flag1 0))))
           (#t
            (set! /fl_flag1 0))))
         (@Up))
        (#t
         (@Goto //P)))))))
   (cond
    ((not (@Right?))
     (set! /fl_flag2 1))
    (#t
     (@Right)
     (set! /fl_flag2 0))))))

(define (/foreach-merge_calls_in_action-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@Spec_Type (@Item)) //T_/Cond) (member //T_/Call (@Stat_Types (@Item))) (@Trans? //T/R_/Simplify_/If))
   (@Trans //T/R_/Simplify_/If ""))))

(define (/foreach-merge_calls_in_action-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@Spec_Type (@Item)) //T_/Cond) (@Gen_Improper? (@Item) //A/S/Type))
   (set! //Calls (@Calls (@Item)))
   (while (and (not (null? //Calls)) (not (@Passed?))) 
    (begin
     (set! /el (car //Calls))
     (set! //Calls (cdr //Calls))
     (cond
      ((> (wsl-ref /el 2) 1)
       (@Pass))))))))

(define (/foreach-merge_calls_in_action-4 //Depth //A/S_/Type)
 (@Down)
 (set! /fl_flag2 0)
 (while (= /fl_flag2 0) 
  (begin
   (cond
    ((and (= (@ST (@I)) //T_/Cond) (member //T_/Call (@Stat_Types (@I))))
     (cond
      ((@Right?)
       (set! //P (@Posn))
       (@Right)
       (while (and (not (@Cs? (@I))) (not (= (@ST (@I)) //T_/Call)) (@Right?)) 
        (@Right))
       (cond
        ((= (@ST (@I)) //T_/Call)
         (@Goto //P)
         (while (not (= (@ST (@Get_n (@Parent) (+ (@Posn_n) 1))) //T_/Call)) 
          (@Trans //T/R_/Absorb_/Right ""))
         (@Trans //T/R_/Absorb_/Right "")
         (@Down)
         ; to first guarded
         (set! /fl_flag1 0)
         (while (= /fl_flag1 0) 
          (begin
           (@Down_Last)
           ;down to statement sequence
           (cond
            ((member //T_/Call (my-map @ST (@Cs (@I))))
             (@Down)
             (while (and (not (= (@ST (@I)) //T_/Call)) (@Right?)) 
              (@Right))
             (cond
              ((= (@ST (@I)) //T_/Call)
               (@Delete_Rest)))
             ;Up to statement sequence
             (@Up)))
           (@Up)
           ;To guarded
           (cond
            ((not (@Right?))
             (set! /fl_flag1 1))
            (#t
             (@Right)
             (set! /fl_flag1 0)))))
         (@Up))
        (#t
         (@Goto //P)))))))
   (cond
    ((not (@Right?))
     (set! /fl_flag2 1))
    (#t
     (@Right)
     (set! /fl_flag2 0))))))

(define (/foreach-merge_calls_in_action-5 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Cond) (member //T_/Call (@Stat_Types (@I))))
   (cond
    ((@Trans? //T/R_/Simplify_/If)
     (@Trans //T/R_/Simplify_/If "")))
   (cond
    ((and (equal? //A/S/Type "Reg") (@Gen_Improper? (@I) //A/S/Type))
     (set! //Calls (@Calls (@I)))
     (set! /freq 0)
     (while (not (null? //Calls)) 
      (begin
       (set! /el (car //Calls))
       (cond
        ((> (wsl-ref /el 2) /freq)
         (set! /freq (wsl-ref /el 2))
         (set! //Target (wsl-ref /el 1))))
       (set! //Calls (cdr //Calls))))
     (cond
      ((> /freq 1)
       (@Down)
       (set! /fl_flag1 0)
       (while (= /fl_flag1 0) 
        (begin
         (@Down_To 2)
         (@Down_Last)
         (cond
          ((equal? (@V (@I)) //Target)
           (@Paste_Over (@Make //T_/Skip '() '()))
           (@Goto (@Posn))))
         (@Up)
         (@Up)
         (cond
          ((not (@Right?))
           (set! /fl_flag1 1))
          (#t
           (@Right)
           (set! /fl_flag1 0)))))
       (@Up)
       (@Splice_After (list (@Make //T_/Call //Target '())))
       (@Goto (@Posn)))))))))

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
(define (@Merge_Calls_In_Action_Test)
 (let ((//Orig_/Pos (@Posn))
       (//Orig_/Act (@Item))
       (//P-save //P)
       (/found "false")
       (//N 0)
       (/el-save /el)
       (/freq-save /freq)
       (//Calls-save //Calls)
       (//A/S/Type-save //A/S/Type))
  (set! //P '())
  (set! /el '())
  (set! /freq 0)
  (set! //Calls '())
  (set! //A/S/Type (@AS_Type))
  (@Edit)
  (cond
   ((not (= (@Spec_Type (@Item)) //T_/Action))
    (@Fail "Not an action"))
   (#t
    (set! //Old_/Calls (@Calls (@Item)))
    (cond
     ((and (= (gen-length //Old_/Calls) 1) (= (car (cdr (car //Old_/Calls))) 1))
      (@Fail "Only one call in this action"))
     (#t
      (cond
       ((@Trans? //T/R_/Delete_/Unreachable_/Code)
        (@Trans //T/R_/Delete_/Unreachable_/Code '())))
      ;  1. For each IF statement which contains calls, in a regular action       
      ;     system, try to absorb any calls which follow it. Some of these        
      ;     will then be deleteable.                                              
      (cond
       ((equal? //A/S/Type "Reg")
        (@Foreach_Stats /foreach-merge_calls_in_action-1 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))))
      ;  2. Try to simplify any IF statements which contain calls.                
      ;     This might result in calls to the same action being                   
      ;     taken out and merged, or branches containing calls being deleted      
      (@Foreach_Statement /foreach-merge_calls_in_action-2 0 (@AS_Type) 0)
      (cond
       ((null? (@Program))
        (@New_Program (@Skips))))
      (set! //Calls_/Now (@Calls (@Item)))
      ;Have the number of calls been reduced?
      (cond
       ((< (gen-length //Calls_/Now) (gen-length //Old_/Calls))
        (@Pass))
       (#t
        (while (not (null? //Old_/Calls)) 
         (begin
          (set! /el (car //Old_/Calls))
          (set! //N (wsl-ref /el 1))
          (set! //Freq (wsl-ref /el 2))
          (cond
           ((< (@Call_Freq //N (@Item)) //Freq)
            ;# of calls is less, so Pass.
            (@Pass)))
          (set! //Old_/Calls (cdr //Old_/Calls))))))
      ;3. If we're in a regular system, and an IF statement is improper, 
      ;   we can take out right any call (which terminates a branch).    
      ;   If so, and two or more calls are to the same action, we can    
      ;   take these out and merge them - so Pass.                       
      (cond
       ((and (not (@Passed?)) (@In_Reg_System?))
        (@Ateach_Statement /foreach-merge_calls_in_action-3 0 (@AS_Type) 0)
        (cond
         ((null? (@Program))
          (@New_Program (@Skips))))))
      (cond
       ((not (@Passed?))
        (@Fail "Cannot merge any calls")))))))
  (@Undo_Edit)
  (set! //P //P-save)
  (set! /el /el-save)
  (set! /freq /freq-save)
  (set! //Calls //Calls-save)
  (set! //A/S/Type //A/S/Type-save)))

(define (@Merge_Calls_In_Action_Code //Data)
 (let ((//Calls-save //Calls)
       (/el-save /el)
       (/found "false")
       (//Target-save //Target)
       (//N '())
       (/freq-save /freq)
       (//Orig_/Pos (@Posn))
       (//P-save //P)
       (//Orig_/Act (@I))
       (//A/S/Type-save //A/S/Type))
  (set! //Calls '())
  (set! /el '())
  (set! //Target '())
  (set! /freq '())
  (set! //P '())
  (set! //A/S/Type (@AS_Type))
  (@Trans //T/R_/Delete_/Unreachable_/Code "")
  (@Goto //Orig_/Pos)
  (@Edit)
  (cond
   ((equal? //A/S/Type "Reg")
    (@Foreach_Stats /foreach-merge_calls_in_action-4 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (@Foreach_Statement /foreach-merge_calls_in_action-5 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Goto '())
  (cond
   ((@Equal? (@I) //Orig_/Act)
    (@Notice "Transformation `Merge Calls In Action' had nothing to do")))
  (cond
   ((@Trans? //T/R_/Delete_/All_/Skips)
    (@Trans //T/R_/Delete_/All_/Skips "")))
  (@End_Edit)
  (set! //Calls //Calls-save)
  (set! /el /el-save)
  (set! //Target //Target-save)
  (set! /freq /freq-save)
  (set! //P //P-save)
  (set! //A/S/Type //A/S/Type-save)))

