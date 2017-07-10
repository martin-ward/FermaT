;;; Scheme translation of WSL code
(define (/foreach-find_terminals-1 //Depth //A/S_/Type)
 (set! /posn (@Posn))
 (while (and (= (@ST (@I)) //T_/Comment) (not (equal? (@V (@I)) (@V /comment))) (@Left?)) 
  (@Left))
 (cond
  ((and (= (@ST (@I)) //T_/Comment) (equal? (@V (@I)) (@V /comment)))
   #t)
  (#t
   (@Paste_After /comment)
   (cond
    ((and (= (@ST (@I)) //T_/Proc_/Call) (not-member (@V (@Get_n (@I) 1)) //Procs_/Done))
     (@Goto_Proc_Body (@V (@Get_n (@I) 1)))
     (cond
      ((= (@ST (@I)) //T_/Proc)
       (@FT_Process /comment (cons (@V (@Get_n (@I) 1)) //Procs_/Done)))))
    ((= (@ST (@I)) //T_/Cond)
     (@FT_Process /comment //Procs_/Done)))))
 (@Goto /posn))

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
(define (@Find_Terminals_Test)
 (@Pass))

(define (@Find_Terminals_Code //Data)
 (cond
  ((or (null? //Data) (equal? //Data ""))
   (set! //Data (@N_String (@Value (@I))))))
 (@FT_Process (@Make //T_/Comment (string-append (string-append "<Terminal: " //Data) ">") '()) '()))

(define (@FT_Process /comment-par //Procs_/Done-par)
 (let ((//Procs_/Done-save //Procs_/Done)
       (/comment-save /comment))
  (set! //Procs_/Done //Procs_/Done-par)
  (set! /comment /comment-par)
  (let ((/posn-save /posn))
   (set! /posn '())
   (@Ateach_Terminal /foreach-find_terminals-1 0 (@AS_Type) 1)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /posn /posn-save))
  (set! //Procs_/Done //Procs_/Done-save)
  (set! /comment /comment-save)))

; Search for the body of the proc with the given name. 
; If not found, then go to the root node. 
(define (@Goto_Proc_Body /name)
 (while (and (@Up?) (not (= (@ST (@I)) //T_/Where))) 
  (@Up))
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Down_To 2)
   (@Down)
   ; to first definition 
   (while (and (@Right?) (or (not (= (@ST (@I)) //T_/Proc)) (not (equal? (@V (@Get_n (@I) 1)) /name)))) 
    (@Right))
   (cond
    ((or (not (= (@ST (@I)) //T_/Proc)) (not (equal? (@V (@Get_n (@I) 1)) /name)))
     ; move up past the WHERE, if possible, and continue 
     (@Up)
     (@Up)
     ; back to the WHERE 
     (cond
      ((@Up?)
       (@Up)
       (@Goto_Proc_Body /name))))))))

#t
