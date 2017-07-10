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
(define (@Edit)
 (set! /adt_/Edit_/Program_/Stack (cons /adt_/Program /adt_/Edit_/Program_/Stack))
 (set! /adt_/Edit_/Posn_/Stack (if (> /adt_/Posn_n 0) (cons (cons /adt_/Posn_n /adt_/Posn) /adt_/Edit_/Posn_/Stack) (cons '() /adt_/Edit_/Posn_/Stack)))
 (set! /adt_/Edit_/To_/Stack (cons 0 /adt_/Edit_/To_/Stack))
 (set! /adt_/Program /adt_/Item)
 (@Goto '()))

(define (@Edit_Parent)
 (set! /adt_/Edit_/Program_/Stack (cons /adt_/Program /adt_/Edit_/Program_/Stack))
 (set! /adt_/Edit_/Posn_/Stack (cons /adt_/Posn /adt_/Edit_/Posn_/Stack))
 (set! /adt_/Edit_/To_/Stack (cons /adt_/Posn_n /adt_/Edit_/To_/Stack))
 (set! /adt_/Program (car /adt_/Path_/Items))
 (@Goto (list /adt_/Posn_n)))

(define (@End_Edit)
 (let ((//New /adt_/Program)
       (//To (car /adt_/Edit_/To_/Stack)))
  (set! /adt_/Program (car /adt_/Edit_/Program_/Stack))
  (@Goto (reverse (car /adt_/Edit_/Posn_/Stack)))
  (set! /adt_/Edit_/Program_/Stack (cdr /adt_/Edit_/Program_/Stack))
  (set! /adt_/Edit_/Posn_/Stack (cdr /adt_/Edit_/Posn_/Stack))
  (set! /adt_/Edit_/To_/Stack (cdr /adt_/Edit_/To_/Stack))
  (cond
   ((null? //New)
    (@Clever_Delete))
   (#t
    (cond
     ((and (= (@GT (@I)) //T_/Statement) (= (@GT //New) //T_/Statements))
      (@Splice_Over (@Cs //New)))
     ((not (eq? //New (@I)))
      (@Paste_Over //New)))
    (cond
     ((and (> //To 0) (<= //To (@Size (@I))))
      (@Down_To //To)))))))

(define (@Undo_Edit)
 (set! /adt_/Program (car /adt_/Edit_/Program_/Stack))
 (@Goto (reverse (car /adt_/Edit_/Posn_/Stack)))
 (set! /adt_/Edit_/Program_/Stack (cdr /adt_/Edit_/Program_/Stack))
 (set! /adt_/Edit_/Posn_/Stack (cdr /adt_/Edit_/Posn_/Stack))
 (set! /adt_/Edit_/To_/Stack (cdr /adt_/Edit_/To_/Stack)))

#t
