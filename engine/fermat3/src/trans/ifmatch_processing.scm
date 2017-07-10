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
; See docs2/IFMATCH-implementation.txt for details 
(define (@Ifmatch_Processing_Test)
 (cond
  ((@Ifmatch_Type? (@ST (@I)))
   (@Pass))
  (#t
   (@Fail "Current item is not an IFMATCH"))))

; index.(name) is the index in __Match_array for the given variable. 
; This is used for ~?=name and ~*=name and also ensures that the variable 
; has already been seen before any backreference 
(define (@Ifmatch_Processing_Code //Data)
 (let ((/index-save /index)
       (/name-save /name)
       (/saves-save /saves)
       (/expns-save /expns)
       (/inits-save /inits)
       (/restores-save /restores)
       (/i-save /i)
       (//S1 (@Cs (@Get_n (@I) 2)))
       (//S2 (@Cs (@Get_n (@I) 3))))
  (set! /index (hash-table))
  (set! /name (make-vector-eval 1999 0))
  (set! /saves '())
  (set! /expns '())
  (set! /inits '())
  (set! /restores '())
  (set! /i 0)
  ; Walk over the pattern in depth-first order replacing variables 
  ; and interpolation expressions and setting up the lists of assigns as follows: 
  ; saves: save values of global variables 
  ; expns: copy interpolated expression values in __Match_array 
  ; inits: initialise matched variables from __Match_array 
  ; restores: restore saved global variable values 
  (@Down)
  ; to pattern 
  (let ((/-result- (@IP_Walk  /i /saves /expns /inits /restores /index)))
   (set! /i (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /saves (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /expns (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /inits (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /restores (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /index (car /-result-)) (set! /-result- (cdr /-result-)))
  (@Up)
  (cond
   ((null? /saves)
    (@Paste_Over (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "__OK") '()) (@Make 205 1 '()))))) (@Make 17 '() (append /expns (list (@Make 102 '() (list (@Make 9 (@Make_Name "@New_Match") '()) (@Make 10 '() (list (@Var_To_Expn (@Match_To_Fill (@I))) (@Make 202 '() (list (@Make 9 (@Make_Name "@I") '()) (@Make 10 '() '()))))) (@Make 12 '() (list (@Make 501 (@Make_Name "__OK") '()))))) (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Make 207 (@Make_Name "__OK") '()) (@Make 205 1 '()))) (@Make 17 '() //S1))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() //S2))))))))))))
   (#t
    (@Paste_Over (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "__OK") '()) (@Make 205 1 '()))))) (@Make 17 '() (append /expns (list (@Make 102 '() (list (@Make 9 (@Make_Name "@New_Match") '()) (@Make 10 '() (list (@Var_To_Expn (@Match_To_Fill (@I))) (@Make 202 '() (list (@Make 9 (@Make_Name "@I") '()) (@Make 10 '() '()))))) (@Make 12 '() (list (@Make 501 (@Make_Name "__OK") '()))))) (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Make 207 (@Make_Name "__OK") '()) (@Make 205 1 '()))) (@Make 17 '() (list (@Make 139 '() (list (@Make 13 '() /saves) (@Make 17 '() (append /inits //S1 /restores)))))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() //S2)))))))))))))
  (set! /index /index-save)
  (set! /name /name-save)
  (set! /saves /saves-save)
  (set! /expns /expns-save)
  (set! /inits /inits-save)
  (set! /restores /restores-save)
  (set! /i /i-save)))

(define (@IP_Walk /i-par /saves-par /expns-par /inits-par /restores-par /index-par)
 (let ((/index-save /index)
       (/restores-save /restores)
       (/inits-save /inits)
       (/expns-save /expns)
       (/saves-save /saves)
       (/i-save /i)
       (funct-result '()))
  (set! /index /index-par)
  (set! /restores /restores-par)
  (set! /inits /inits-par)
  (set! /expns /expns-par)
  (set! /saves /saves-par)
  (set! /i /i-par)
  (let ((//S/T (@ST (@I)))
        (/v '())
        (/v_save '()))
   ; Process components first (i.e. depth-first walk) 
   (cond
    ((@Down?)
     (@Down)
     (let ((/-result- (@IP_Walk  /i /saves /expns /inits /restores /index)))
      (set! /i (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /saves (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /expns (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /inits (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /restores (car /-result-)) (set! /-result- (cdr /-result-))
      (set! /index (car /-result-)) (set! /-result- (cdr /-result-)))
     (while (@Right?) 
      (begin
       (@Right)
       (let ((/-result- (@IP_Walk  /i /saves /expns /inits /restores /index)))
        (set! /i (car /-result-)) (set! /-result- (cdr /-result-))
        (set! /saves (car /-result-)) (set! /-result- (cdr /-result-))
        (set! /expns (car /-result-)) (set! /-result- (cdr /-result-))
        (set! /inits (car /-result-)) (set! /-result- (cdr /-result-))
        (set! /restores (car /-result-)) (set! /-result- (cdr /-result-))
        (set! /index (car /-result-)) (set! /-result- (cdr /-result-)))))
     (@Up)))
   ; Process current item 
   (cond
    ((or (@One_Pattern_Type? //S/T) (@Many_Pattern_Type? //S/T) (@Any_Pattern_Type? //S/T))
     (set! /i (+ /i 1))
     (set! /name (@V (@I)))
     (cond
      ((not (null? (gethash /index /name)))
       (error (string-append (string-append "Variable " (@N_String /name)) " appears twice in IFMATCH!"))))
     (puthash /index /name /i)
     (set! /v (@Make //T_/Variable /name '()))
     (set! /v_save (@Make //T_/Variable (@Make_Name (string-append (string-append "__" (@N_String /name)) "_save")) '()))
     (set! /saves (cons (@Make 6 '() (list (@Expn_To_Var /v_save) (@Var_To_Expn /v))) /saves))
     (set! /inits (cons (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Make 210 '() (list (@Make 207 (@Make_Name "__Match_array") '()) (@Make 10 '() (list (@Var_To_Expn (@Make //T_/Number /i '())))))))))) /inits))
     (set! /restores (cons (@Make 110 '() (list (@Make 6 '() (list (@Expn_To_Var /v) (@Var_To_Expn /v_save))))) /restores))
     (@Paste_Over (@Make (@ST (@I)) (- /i) '())))
    ((or (@One_Var_Type? //S/T) (@Any_Var_Type? //S/T))
     (set! /name (@V (@I)))
     (cond
      ((null? (gethash /index /name))
       (error (string-append (string-append "Backreference " (@N_String /name)) " not previously used!"))))
     (@Paste_Over (@Make (@ST (@I)) (- (gethash /index /name)) '())))
    ((or (@One_Int_Type? //S/T) (@Any_Int_Type? //S/T))
     (set! /i (+ /i 1))
     (set! /expns (cons (@Make 110 '() (list (@Make 6 '() (list (@Make 502 '() (list (@Make 501 (@Make_Name "__Match_array") '()) (@Make 10 '() (list (@Var_To_Expn (@Make //T_/Number /i '())))))) (@Var_To_Expn (@Get_n (@I) 1)))))) /expns))
     (@Down)
     (@Paste_Over (@Make //T_/Number /i '()))
     (@Up))))
  (set! funct-result (list /i /saves /expns /inits /restores /index))
  (set! /index /index-save)
  (set! /restores /restores-save)
  (set! /inits /inits-save)
  (set! /expns /expns-save)
  (set! /saves /saves-save)
  (set! /i /i-save)
  funct-result))

; Convert an IFMATCH pattern to a suitable FILL expression: 
(define (@Match_To_Fill //I)
 (let ((//S/T (@ST //I))
       (/type '()))
  (cond
   ((= //S/T //T_/Ifmatch_/Stat)
    (set! /type //T_/Fill_/Stat))
   ((= //S/T //T_/Ifmatch_/Expn)
    (set! /type //T_/Fill_/Expn))
   ((= //S/T //T_/Ifmatch_/Cond)
    (set! /type //T_/Fill_/Cond))
   ((= //S/T //T_/Ifmatch_/Defn)
    (set! /type //T_/Fill_/Defn))
   ((= //S/T //T_/Ifmatch_/Lvalue)
    (set! /type //T_/Fill_/Lvalue))
   ((= //S/T //T_/Ifmatch_/Assign)
    (set! /type //T_/Fill_/Assign))
   ((= //S/T //T_/Ifmatch_/Guarded)
    (set! /type //T_/Fill_/Guarded))
   ((= //S/T //T_/Ifmatch_/Action)
    (set! /type //T_/Fill_/Action))
   ((= //S/T //T_/Ifmatch_/Stats)
    (set! /type //T_/Fill_/Stats))
   ((= //S/T //T_/Ifmatch_/Expns)
    (set! /type //T_/Fill_/Expns))
   ((= //S/T //T_/Ifmatch_/Lvalues)
    (set! /type //T_/Fill_/Lvalues))
   ((= //S/T //T_/Ifmatch_/Assigns)
    (set! /type //T_/Fill_/Assigns))
   ((= //S/T //T_/Ifmatch_/Defns)
    (set! /type //T_/Fill_/Defns))
   (#t
    (error "Unknown type in IFMATCH: " (@Type_Name //S/T))))
  (@Make /type '() (list (@Get_n //I 1)))))

#t
