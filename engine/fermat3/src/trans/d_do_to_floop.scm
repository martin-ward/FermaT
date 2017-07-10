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
(define (@D_Do_To_Floop_Test)
 (cond
  ((not (= (@ST (@I)) //T_/D_/Do))
   (@Fail "Selected item is not a D_Do loop."))
  (#t
   (@Pass))))

(define (@D_Do_To_Floop_Code //Data)
 (let ((//B (@Make //T_/False '() '()))
       (//S '())
       (/guard '()))
  (for-in /guard (@Cs (@I)) 
   (set! //B (@Or //B (@Get_n /guard 1))))
  (set! //B (@Not //B))
  (set! /guard (@Make 7 '() (list //B (@Make 17 '() (list (@Make 117 1 '()))))))
  (@Down_Last)
  (@Paste_After /guard)
  (@Up)
  (@Paste_Over (@Make //T_/D_/If '() (@Cs (@I))))
  (set! //S (@I))
  (@Paste_Over (@Make 133 '() (list (@Make 17 '() (list //S)))))))

#t
