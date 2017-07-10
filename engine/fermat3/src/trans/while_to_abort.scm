;;; Scheme translation of WSL code
(define /%const__while_to_abort__1 (@Make 141 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 107 -1 '()))))))
(define /%const__while_to_abort__2 (@Make 141 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()) (@Make 109 '() (list (@Make 305 -3 '()))))))))
(define /%const__while_to_abort__3 (@Make 141 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 106 -2 '()))))))
(define /%const__while_to_abort__4 (@Make 108 '() '()))
(define /%const__while_to_abort__5 (@Make 141 '() (list (@Make 305 -1 '()) (@Make 17 '() (list (@Make 107 -2 '()))))))
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
(define (@While_To_Abort_Test)
 (cond
  ((not (= (@ST (@I)) //T_/While))
   (@Fail "Item is not a WHILE loop"))
  (#t
   ;Deal with the trivial case first 
   (let ((/__/O/K 1))
    (set! /__/O/K (@New_Match  /%const__while_to_abort__1 (@I) /__/O/K))
    (cond
     ((= /__/O/K 1)
      (let ((/__/S_save //S))
       (set! //S (vector-ref /__/Match_array 0))
       (@Pass)
       (set! //S /__/S_save)))
     (#t
      ;If the body ends with an assertion which implies the
      ;loop condition, the loop will never terminate.      
      (let ((/__/O/K 1))
       (set! /__/O/K (@New_Match  /%const__while_to_abort__2 (@I) /__/O/K))
       (cond
        ((= /__/O/K 1)
         (let ((/__/B1_save //B1)
               (/__/S_save //S)
               (/__/B_save //B))
          (set! //B1 (vector-ref /__/Match_array 2))
          (set! //S (vector-ref /__/Match_array 1))
          (set! //B (vector-ref /__/Match_array 0))
          (cond
           ((@Implies? //B1 //B)
            (@Pass)))
          (set! //B1 /__/B1_save)
          (set! //S /__/S_save)
          (set! //B /__/B_save)))
        (#t
         (@While_To_Abort_Test2)))))))
   (cond
    ((not (@Passed?))
     (@Fail "Cannot determine that the loop will not terminate"))))))

(define (@While_To_Abort_Test2)
 (let ((/assigned '()))
  ;Can we prove that the loop will never terminate ?
  (let ((/__/O/K 1))
   (set! /__/O/K (@New_Match  /%const__while_to_abort__3 (@I) /__/O/K))
   (cond
    ((= /__/O/K 1)
     (let ((/__/S_save //S)
           (/__/B_save //B))
      (set! //S (vector-ref /__/Match_array 1))
      (set! //B (vector-ref /__/Match_array 0))
      ;If the loop condition is not altered by the
      ;body, the loop will not terminate.         
      (for-in //I //S 
       (set! /assigned (union-n (@Assigned //I) /assigned)))
      (cond
       ((and (null? (intersection-n (@Variables //B) /assigned)) (null? (intersection-n //Ext_/Call_/Types_/Set (@Stat_Types (@I)))))
        (@Pass))
       (#t
        ;If the loop condition is altered in such a way that it 
        ;still remains true, the loop will never terminate.     
        ;We can test this by trying to move an assertion through
        ;the loop and examining the effect on its condition.    
        (@Edit)
        (@Trans //T/R_/Insert_/Assertion "")
        (while (@Trans? //T/R_/Move_/To_/Right) 
         (@Trans //T/R_/Move_/To_/Right ""))
        (while (@Up?) 
         (@Up))
        (let ((/__/O/K 1))
         (set! /__/O/K (@New_Match  /%const__while_to_abort__2 (@I) /__/O/K))
         (cond
          ((= /__/O/K 1)
           (let ((/__/B1_save //B1)
                 (/__/S_save //S)
                 (/__/B_save //B))
            (set! //B1 (vector-ref /__/Match_array 2))
            (set! //S (vector-ref /__/Match_array 1))
            (set! //B (vector-ref /__/Match_array 0))
            (cond
             ((@Implies? //B1 //B)
              (@Pass)))
            (set! //B1 /__/B1_save)
            (set! //S /__/S_save)
            (set! //B /__/B_save)))))
        (@Undo_Edit)))
      (set! //S /__/S_save)
      (set! //B /__/B_save)))))))

(define (@While_To_Abort_Code //Data)
 (let ((/__/O/K 1))
  (set! /__/O/K (@New_Match  /%const__while_to_abort__1 (@I) /__/O/K))
  (cond
   ((= /__/O/K 1)
    (let ((/__/S_save //S))
     (set! //S (vector-ref /__/Match_array 0))
     (@Paste_Over /%const__while_to_abort__4)
     (set! //S /__/S_save)))
   (#t
    (let ((/__/O/K 1))
     (set! /__/O/K (@New_Match  /%const__while_to_abort__5 (@I) /__/O/K))
     (cond
      ((= /__/O/K 1)
       (let ((/__/S_save //S)
             (/__/B_save //B))
        (set! //S (vector-ref /__/Match_array 1))
        (set! //B (vector-ref /__/Match_array 0))
        (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list //B (@Make 17 '() (list (@Make 108 '() '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
        (set! //S /__/S_save)
        (set! //B /__/B_save)))))))))

#t
