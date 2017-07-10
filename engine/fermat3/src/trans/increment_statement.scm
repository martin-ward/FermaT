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
(define (@Increment_Statement_Test)
 (let ((/tvs '())
       (/posn-save /posn)
       (/min 0))
  (set! /posn '())
  (cond
   ((and (= (@GT (@Item)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
    (@Up)))
  (cond
   ((= (@GT (@I)) //T_/Statements)
    (@Down_Last)))
  (set! /tvs (@Sort_Num (@TVs)))
  (cond
   ((not (= (@GT (@I)) //T_/Statement))
    (@Fail "Current item is not a statement"))
   ((not (@Up?))
    (@Fail "Statement is not in an enclosing loop"))
   ((null? /tvs)
    (@Fail "Statement is an infinite loop"))
   (#t
    (set! /min (car /tvs))
    (set! /posn (@Up_To_Loop  /posn))
    (while (and (> /min 0) (not (null? /posn))) 
     (begin
      (set! /min (- /min 1))
      (set! /posn (@Up_To_Loop  /posn))))
    (cond
     ((null? /posn)
      (@Fail "Statement is not in an enclosing loop"))
     ((< (gen-length (@Posn)) 2)
      (@Fail "Statement is not in an enclosing double loop"))
     ((or (not (= (@ST (@GParent)) //T_/Floop)) (> (@Size (@Parent)) 1))
      (@Fail "Statement is not in an enclosing double loop"))
     ((and (> (gen-length /tvs) 1) (not (@IS_Enclosing_Loops? (+ (- (last-1 /tvs) (car /tvs)) 1))))
      (@Fail "Statement is not surrounded by enough nested loops"))
     ((not (@Gen_Terminal? (@Get_n (@I) 1) (cdr /posn) (@AS_Type)))
      (@Fail "Statement is not in a terminal position in enclosing double loop"))
     (#t
      (@Pass)))))
  (set! /posn /posn-save)))

(define (@Increment_Statement_Code //Data)
 (cond
  ((and (= (@GT (@Item)) //T_/Assign) (@Up?) (not (@Left?)) (not (@Right?)))
   (@Up)))
 (cond
  ((= (@GT (@I)) //T_/Statements)
   (@Down_Last)))
 (@Splice_Over (@Increment (@I) (@AS_Type) 1 0)))

; Move up to the next enclosing loop, prepend relative position to posn 
(define (@Up_To_Loop /posn-par)
 (let ((/posn-save /posn)
       (funct-result '()))
  (set! /posn /posn-par)
  (let ((/len (gen-length /posn)))
   (cond
    ((not (@Up?))
     (set! /posn '()))
    (#t
     (set! /posn (cons (@Posn_n) /posn))
     (@Up)
     (while (and (not (= //T_/Floop (@ST (@I)))) (@Up?)) 
      (begin
       (set! /posn (cons (@Posn_n) /posn))
       (@Up)))
     (cond
      ((not (= (@ST (@I)) //T_/Floop))
       (let ((/n (- (gen-length /posn) /len)))
        (while (> /n 0) 
         (begin
          (@Down_To (car /posn))
          (set! /posn (cdr /posn))
          (set! /n (- /n 1))))
        (set! /posn '())))))))
  (set! funct-result /posn)
  (set! /posn /posn-save)
  funct-result))

; Check that the current item is enclosed in at least n nested loops 
(define (@IS_Enclosing_Loops? /n)
 (let ((/orig (@Posn))
       (/posn-save /posn)
       (funct-result '()))
  (set! /posn '())
  (while (> /n 0) 
   (begin
    (set! /posn (@Up_To_Loop  /posn))
    (set! /n (- /n 1))))
  (@Goto /orig)
  (set! funct-result (not (null? /posn)))
  (set! /posn /posn-save)
  funct-result))

#t
