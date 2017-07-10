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
; Note: it is often better to move the guarded forwards to the end of the Cond
;before aligning. Consider this example:
;
;IF B1 THEN IF B2 THEN S1 ELSE S2 FI
;      ELSE S3 FI
;
;becomes
;
;IF B1 AND B2 THEN S1
;ELSIF B1 THEN S2
;  ELSE S3 FI
;
;which duplicates the test B1.
;
;But
;
;IF NOT B1 THEN S3
;ELSIF B2 THEN S1
;ELSE S2 FI
;
;does not duplicate any tests! 
; Quick fix hack added: don't process if the inner IF statement 
; has more than N branches. This seems to be a good tradeoff between 
; performance and getting a good looking result. 
; See LPSI1 for an example. 
(define (@Align_Nested_Statements_Test)
 (cond
  ((not (= (@GT (@I)) //T_/Guarded))
   (@Fail "The current item is not a guarded clause."))
  (#t
   ; Check for zero or more comments followed by a COND: 
   (@Down_Last)
   (@Down)
   ; to first statement of first guard 
   (while (and (or (not (= //T_/Cond (@ST (@I)))) (> (@Size (@I)) 60)) (= //T_/Comment (@ST (@I))) (@Right?)) 
    (@Right))
   (cond
    ((and (= //T_/Cond (@ST (@I))) (<= (@Size (@I)) 60))
     (@Pass)))
   (cond
    ((not (@Passed?))
     (@Fail "The first statement in the clause is not an `If'."))))))

(define (@Align_Nested_Statements_Code //Data)
 (let ((//B (@Get_n (@I) 1))
       (/comments '())
       (/rest '())
       (/new '())
       (//A/S (@AS_Type)))
  ; Move the guarded down to the end and then align: 
  (@Edit)
  ;Get the rest of the statements in the guard to merge the into the initial 'If'.
  (@Down_Last)
  (@Down)
  ; to first statement 
  (while (= (@ST (@I)) //T_/Comment) 
   (begin
    (set! /comments (cons (@I) /comments))
    (@Delete)))
  (cond
   ((not (= (@ST (@I)) //T_/Cond))
    (display-list "ERROR in Align_Nested_Statements!!!")))
  (@Up)
  (set! /comments (reverse /comments))
  (set! /rest (cdr (@Cs (@I))))
  ;Move down to the first guard.
  (@Down)
  (@Down)
  (cond
   ((and #f (not (null? /comments)))
    (@Down_Last)
    (@Down)
    (@Splice_Before /comments)
    (@Up)
    (@Up)))
  (let ((//D (@Make //T_/False '() '())))
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (begin
     ;For each guard, change the condition and add the extra statements.
     (@Down)
     (cond
      ((= (@Spec_Type (@I)) //T_/True)
       (@Paste_Over //B))
      ((not (= //T_/True (@Spec_Type //B)))
       (@Paste_Over (@And (@I) //B))))
     (cond
      ((and (not (null? /rest)) (not (@Gen_Improper? (@Get_n (@Parent) 2) //A/S)))
       (@Right)
       (@Down_Last)
       (cond
        ((= (@ST (@I)) //T_/Skip)
         (@Splice_Over /rest))
        (#t
         (@Splice_After /rest)))
       (@Up)))
     (@Up)
     (cond
      ((or (= (@ST (@Get_n (@I) 1)) //T_/False) (@Implies? (@Get_n (@I) 1) //D))
       (@Delete)
       (cond
        ((> (@Posn_n) (@Size (@Parent)))
         (set! /fl_flag1 1))
        (#t
         (set! /fl_flag1 0))))
      ((or (= (@ST (@Get_n (@I) 1)) //T_/True) (@Implies? //B //D))
       (@Down)
       (@Paste_Over (@Make //T_/True '() '()))
       (@Up)
       (@Delete_Rest)
       (set! /fl_flag1 1))
      (#t
       (set! //D (@Or //D (@Get_n (@I) 1)))
       (cond
        ((not (@Right?))
         (set! /fl_flag1 1))
        (#t
         (@Right)
         (set! /fl_flag1 0))))))))
  (set! /new (@Cs (@Parent)))
  (@Undo_Edit)
  (@Splice_Over /new)
  ; Insert the comments at the end of the previous guarded 
  ; or at the start of the first new guarded 
  (cond
   ((not (null? /comments))
    (cond
     ((@Left?)
      (@Left)
      (@Down_Last)
      (@Down_Last)
      (cond
       ((@Gen_Improper? (@I) (@AS_Type))
        (@Splice_Before /comments))
       (#t
        (@Splice_After /comments)))
      (@Up)
      (@Up)
      (@Right))
     (#t
      (@Down_Last)
      (@Down)
      (@Splice_Before /comments)
      (@Up)
      (@Up)))))))

