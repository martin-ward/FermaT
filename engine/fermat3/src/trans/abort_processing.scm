;;; Scheme translation of WSL code
(define (/foreach-abort_processing-1 //Depth //A/S_/Type)
 (cond
  ((member //T_/Abort (@Stat_Types (@I)))
   (@Down)
   ; to first statement 
   (while (and (not (= //T_/Abort (@ST (@I)))) (@Right?)) 
    (@Right))
   (cond
    ((= //T_/Abort (@ST (@I)))
     (set! /done (@AP_Abort  /done)))))))

(define (/foreach-abort_processing-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/S)
   (@Down_Last)
   (@Down)
   ; to first action 
   (cond
    ((and (equal? (@Stat_Types (@I)) /types) (@Trans? //T/R_/Substitute_/And_/Delete))
     (@Trans //T/R_/Substitute_/And_/Delete "")
     (set! /done 1)))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((and (equal? (@Stat_Types (@I)) /types) (@Trans? //T/R_/Substitute_/And_/Delete))
       (@Trans //T/R_/Substitute_/And_/Delete "")
       (set! /done 1))))))))

(define (/foreach-abort_processing-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Where) (@Trans? //T/R_/Unfold_/Proc_/Calls))
   (@Trans //T/R_/Unfold_/Proc_/Calls ""))))

(define (/foreach-abort_processing-4 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   ; B1 will be the condition that leads to the ABORT guard: 
   (set! //B1 (@Make //T_/True '() '()))
   (set! //B2 '())
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     ; Convert cc IF statements before processing: 
     (cond
      ((or (member /destination (@Variables (@Get_n (@I) 1))) (member /cc_name (@Variables (@Get_n (@I) 1))))
       (set! /fl_flag1 1))
      ((and (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Abort) (not-member //T_/Call (@Stat_Types (@Get_n (@I) 2))))
       ; If B1 AND @I^1 is true, then we abort 
       ; so the assertion must be the negation of this: 
       (set! //B2 (@Not (@And //B1 (@Get_n (@I) 1))))
       ; Replace this guard by an assertion 
       (@Delete)
       (set! /fl_flag1 1))
      (#t
       (set! //B1 (@And //B1 (@Not (@Get_n (@I) 1))))
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))))
   (@Up)
   (cond
    ((not (null? //B2))
     (set! /done 1)
     ; Fix up the IF statement if necessary: 
     (cond
      ((= (@Size (@I)) 1)
       (@Splice_Over (@Cs (@Get_n (@Get_n (@I) 1) 2)))))
     ; Check for an existing assertion 
     (cond
      ((@Left?)
       (@Left)
       (cond
        ((= (@ST (@I)) //T_/Assert)
         (@Paste_Over (@Make //T_/Assert '() (list (@And (@Get_n (@I) 1) //B2)))))
        (#t
         (@Right)
         (@Paste_Before (@Make //T_/Assert '() (list //B2))))))
      (#t
       (@Paste_Before (@Make //T_/Assert '() (list //B2))))))))))

(define (/foreach-abort_processing-5 //Depth //A/S_/Type)
 (cond
  ((member //T_/Abort (@Stat_Types (@I)))
   (@Down)
   ; to first statement 
   (while (and (not (= //T_/Abort (@ST (@I)))) (@Right?)) 
    (@Right))
   (cond
    ((= //T_/Abort (@ST (@I)))
     (set! /done (@AP_Abort  /done)))))))

(define (/foreach-abort_processing-6 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/A_/S)
   (@Down_Last)
   (@Down)
   ; to first action 
   (cond
    ((and (equal? (@Stat_Types (@I)) /types) (@Trans? //T/R_/Substitute_/And_/Delete))
     (@Trans //T/R_/Substitute_/And_/Delete "")
     (set! /done 1)))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((and (equal? (@Stat_Types (@I)) /types) (@Trans? //T/R_/Substitute_/And_/Delete))
       (@Trans //T/R_/Substitute_/And_/Delete "")
       (set! /done 1))))))))

(define (/foreach-abort_processing-7 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Where) (@Trans? //T/R_/Unfold_/Proc_/Calls))
   (@Trans //T/R_/Unfold_/Proc_/Calls ""))))

(define (/foreach-abort_processing-8 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Cond)
   ; B1 will be the condition that leads to the ABORT guard: 
   (set! //B1 (@Make //T_/True '() '()))
   (set! //B2 '())
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     ; Convert cc IF statements before processing: 
     (cond
      ((or (member /destination (@Variables (@Get_n (@I) 1))) (member /cc_name (@Variables (@Get_n (@I) 1))))
       (set! /fl_flag1 1))
      ((and (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Abort) (not-member //T_/Call (@Stat_Types (@Get_n (@I) 2))))
       ; If B1 AND @I^1 is true, then we abort 
       ; so the assertion must be the negation of this: 
       (set! //B2 (@Not (@And //B1 (@Get_n (@I) 1))))
       ; Replace this guard by an assertion 
       (@Delete)
       (set! /fl_flag1 1))
      (#t
       (set! //B1 (@And //B1 (@Not (@Get_n (@I) 1))))
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0)))))))
   (@Up)
   (cond
    ((not (null? //B2))
     (set! /done 1)
     ; Fix up the IF statement if necessary: 
     (cond
      ((= (@Size (@I)) 1)
       (@Splice_Over (@Cs (@Get_n (@Get_n (@I) 1) 2)))))
     ; Check for an existing assertion 
     (cond
      ((@Left?)
       (@Left)
       (cond
        ((= (@ST (@I)) //T_/Assert)
         (@Paste_Over (@Make //T_/Assert '() (list (@And (@Get_n (@I) 1) //B2)))))
        (#t
         (@Right)
         (@Paste_Before (@Make //T_/Assert '() (list //B2))))))
      (#t
       (@Paste_Before (@Make //T_/Assert '() (list //B2))))))))))

(define /%const__abort_processing__1 (@Make 112 (@Make_Name "Z") '()))
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
(define (@Abort_Processing_Test)
 (cond
  ((member //T_/Abort (@Stat_Types (@I)))
   (@Pass))
  (#t
   (@Fail "No ABORTs in the code to process"))))

; Note: sequences such as IF B THEN EXIT(1) FI; ABORT 
;                     and IF B THEN CALL FOO FI; ABORT 
; are not equivalent to ABORT! 
(define (@Abort_Processing_Code //Data)
 (let ((/types-save /types)
       (/done-save /done)
       (//B1-save //B1)
       (//B2-save //B2)
       (/cc_name-save /cc_name)
       (/destination-save /destination))
  (set! /types (@Make_Set (list //T_/Call //T_/Abort)))
  (set! /done 0)
  (set! //B1 '())
  (set! //B2 '())
  (set! /cc_name (@Make_Name "cc"))
  (set! /destination (@Make_Name "destination"))
  (set! /done 0)
  (@Foreach_Stats /foreach-abort_processing-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Statement /foreach-abort_processing-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Statement /foreach-abort_processing-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Foreach_Statement /foreach-abort_processing-4 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (while (not (= /done 0)) 
   (begin
    (set! /done 0)
    (@Foreach_Stats /foreach-abort_processing-5 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Foreach_Statement /foreach-abort_processing-6 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Foreach_Statement /foreach-abort_processing-7 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Foreach_Statement /foreach-abort_processing-8 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (set! /types /types-save)
  (set! /done /done-save)
  (set! //B1 //B1-save)
  (set! //B2 //B2-save)
  (set! /cc_name /cc_name-save)
  (set! /destination /destination-save)))

; Process the sequence (if any) around the ABORT: 
(define (@AP_Abort /done-par)
 (let ((/done-save /done)
       (funct-result '()))
  (set! /done /done-par)
  (let ((/posn_n (@Posn_n))
        (/call 0))
   (cond
    ((member //T_/Call (@Stat_Types (@Parent)))
     (set! /call 1)))
   (cond
    ((@Right?)
     (@Right)
     (let ((/__/O/K 1))
      (set! /__/O/K (@New_Match  /%const__abort_processing__1 (@I) /__/O/K))
      (cond
       ((= /__/O/K 1)
        (cond
         ((@Right?)
          (@Delete_Rest)
          (set! /done 1)))
        (@Left))
       (#t
        (@Left)
        (@Delete_Rest)
        (set! /done 1))))))
   ; Should still be on the ABORT after fixing what follows it. 
   (cond
    ((and (= /call 1) (not (@Right?)))
     (@Paste_After /%const__abort_processing__1)))
   (cond
    ((not (= (@ST (@I)) //T_/Abort))
     (error "Not on ABORT now!!!")))
   (cond
    ((@Left?)
     (@Left)
     (set! /fl_flag1 0)
     (while (= /fl_flag1 0) 
      (cond
       ((not (and (@Is_Proper?) (not-member //T_/Call (@Stat_Types (@I)))))
        (set! /fl_flag1 1))
       (#t
        (@Delete)
        (set! /done 1)
        (cond
         ((not (@Left?))
          (set! /fl_flag1 1))
         (#t
          (@Left)
          (set! /fl_flag1 0))))))
     (cond
      ((and (= (@ST (@I)) //T_/Cond) (@Trans? //T/R_/Fully_/Absorb_/Right))
       (@Trans //T/R_/Fully_/Absorb_/Right "")
       (set! /done 1))))))
  (set! funct-result /done)
  (set! /done /done-save)
  funct-result))

#t
