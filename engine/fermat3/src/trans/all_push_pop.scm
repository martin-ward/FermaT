;;; Scheme translation of WSL code
(define (/foreach-all_push_pop-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Push) (@Trans? //T/R_/Push_/Pop))
   (@Pass))))

(define (/foreach-all_push_pop-2 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Push) (@Trans? //T/R_/Push_/Pop))
   (@Trans //T/R_/Push_/Pop "")
   (set! /done 0))))

(define (/foreach-all_push_pop-3 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Push) (@Trans? //T/R_/Push_/Pop))
   (@Trans //T/R_/Push_/Pop "")
   (set! /done 0))))

;
;==========================================================================
;FermaT Transformation System
;Copyright (C) 2015 Software Migrations Limited.
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
(define (@All_Push_Pop_Test)
 (@Ateach_Statement /foreach-all_push_pop-1 0 (@AS_Type) 0)
 (cond
  ((null? (@Program))
   (@New_Program (@Skips))))
 (cond
  ((not (@Passed?))
   (@Fail "No suitable PUSH statement found"))))

(define (@All_Push_Pop_Code //Data)
 (let ((/done-save /done))
  (set! /done 0)
  (set! /done 1)
  (@Ateach_Statement /foreach-all_push_pop-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (while (not (= /done 1)) 
   (begin
    (set! /done 1)
    (@Ateach_Statement /foreach-all_push_pop-3 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))))
  (set! /done /done-save)))

#t
