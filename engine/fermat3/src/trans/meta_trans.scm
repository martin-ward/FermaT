;;; Scheme translation of WSL code
(define (/foreach-meta_trans-1 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Foreach_/Expn)
   (set! /type //T_/Ifmatch_/Expn))
  ((= (@ST (@I)) //T_/Foreach_/Cond)
   (set! /type //T_/Ifmatch_/Cond))
  (#t
   (set! /type 0)))
 (cond
  ((> /type 0)
   (@Down)
   ; to statement sequence 
   (display-list "Size = " (@Size (@I)))
   (cond
    ((> (@Size (@I)) 4)
     (set! //O/K 1)
     (set! /comps (@Cs (@I)))
     (set! /matches '())
     (while (and (= //O/K 1) (not (null? /comps))) 
      (begin
       (cond
        ((or (= (@ST (car /comps)) //T_/Comment) (= (@ST (car /comps)) //T_/Skip))
         #t)
        ((not (= (@ST (car /comps)) /type))
         (set! //O/K 0)
         (display-list "Failed for:")
         (@Print_WSL (car /comps) ""))
        (#t
         (set! /matches (cons (car /comps) /matches))))
       (set! /comps (cdr /comps))))
     (cond
      ((= //O/K 1)
       (@MT_Replace (reverse /matches) /type))))))))

(define /%const__meta_trans__1 (@Make 17 '() (list (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@Size") '()) (@Make 10 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@I") '()) (@Make 10 '() '()))))))) (@Make 205 1 '()))) (@Make 17 '() (list (@Make 145 '() '()))))) (@Make 7 '() (list (@Make 313 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@Size") '()) (@Make 10 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@I") '()) (@Make 10 '() '()))))))) (@Make 205 2 '()))) (@Make 17 '() (list (@Make 145 '() '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))))
(define /%const__meta_trans__2 (@Make 207 (@Make_Name "MT_ST") '()))
(define /%const__meta_trans__3 (@Make 207 (@Make_Name "MT_ST_n") '()))
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
; Meta-Transformation: Process a sequence of IFMATCH constructs into 
; something more efficient. 
(define (@Meta_Trans_Test)
 (let ((/types (@Stat_Types (@Item))))
  (cond
   ((and (member //T_/Foreach_/Expn /types) (member //T_/Ifmatch_/Expn /types))
    (@Pass))
   ((and (member //T_/Foreach_/Cond /types) (member //T_/Ifmatch_/Cond /types))
    (@Pass))
   (#t
    (@Fail "No suitable structures found.")))))

(define (@Meta_Trans_Code //Data)
 ; Search for FOREACH Expression with a sequence of IFMATCH Expression components 
 (let ((//O/K-save //O/K)
       (/comps-save /comps)
       (/type-save /type)
       (/matches-save /matches))
  (set! //O/K 0)
  (set! /comps '())
  (set! /type 0)
  (set! /matches '())
  (@Foreach_Statement /foreach-meta_trans-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! //O/K //O/K-save)
  (set! /comps /comps-save)
  (set! /type /type-save)
  (set! /matches /matches-save)))

; Generate efficient code to replace the given sequence of IFMATCH statements. 
; Sort the matches according to the size and top level type matched for 
; (and sort within each section according to the lower level types 
; if the section has a lot of elements). 
; Note: when calculating the size of a match we should exclude the XXX_Pat_Any 
; items but note that the size may be `N or greater'. Sort according to size: 
; size=1, size=2, size>=3. Note that ~?x + ~*y + ~?z will appear in both 
; size=2 and size>=3 sets. 
(define (@MT_Replace /matches-par /type-par)
 (let ((/type-save /type)
       (/matches-save /matches))
  (set! /type /type-par)
  (set! /matches /matches-par)
  (display-list "MT_Replace called with " (gen-length /matches) " matches.")
  ; matches is a list of IFMATCH clauses of given type 
  (let ((/size_1 '())
        (/size_2 '())
        (/size_3 '())
        (/var_pats (list //T_/Expn_/Pat_/Many //T_/Expn_/Pat_/Any //T_/Expn_/Int_/Any //T_/Expn_/Var_/Any //T_/Cond_/Pat_/Many //T_/Cond_/Pat_/Any //T_/Cond_/Int_/Any //T_/Cond_/Var_/Any)))
   ; Sort the matches according to size 
   (@Paste_Over /%const__meta_trans__1)
   (for-in /match /matches 
    (cond
     ((= (@Size (@Get_n /match 1)) 1)
      (set! /size_1 (cons /match /size_1))
      (cond
       ((not (null? (intersection-n /var_pats (@Spec_Types (@Get_n /match 1)))))
        (set! /size_2 (cons /match /size_2))
        (set! /size_3 (cons /match /size_3)))))
     ((= (@Size (@Get_n /match 1)) 2)
      (set! /size_2 (cons /match /size_2))
      (cond
       ((not (null? (intersection-n /var_pats (@Spec_Types (@Get_n /match 1)))))
        (set! /size_3 (cons /match /size_3)))))
     (#t
      (set! /size_3 (cons /match /size_3)))))
   (@Down)
   (@Down)
   (@Down_To 2)
   ; Move to size=1 body 
   (cond
    ((not (= (@ST (@Get_n (@I) 1)) //T_/Skip))
     (display-list "ERROR in @MT_Replace size=1 !!!")))
   (@MT_Replace_ST 1 /size_1 /type 0)
   (@Up)
   (@Right)
   (@Down_To 2)
   ; Move to size=2 body 
   (cond
    ((not (= (@ST (@Get_n (@I) 1)) //T_/Skip))
     (display-list "ERROR in @MT_Replace size=2 !!!")))
   (@MT_Replace_ST 2 /size_2 /type 0)
   (@Up)
   (@Right)
   (@Down_To 2)
   ; Move to size=3 body 
   (cond
    ((not (= (@ST (@Get_n (@I) 1)) //T_/Skip))
     (display-list "ERROR in @MT_Replace size=3 !!!")))
   (@MT_Replace_ST 3 /size_3 /type 0)
   (@Up)
   (@Up)
   ; to main Cond 
   (@Paste_Over (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "MT_ST") '()) (@Make 202 '() (list (@Make 9 (@Make_Name "@ST") '()) (@Make 10 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@I") '()) (@Make 10 '() '()))))))))))) (@Make 17 '() (list (@I)))))))
  (set! /type /type-save)
  (set! /matches /matches-save)))

; comp = 0 means split on top level type 
; comp > 0 means split on type of component number comp (unless comp > size) 
(define (@MT_Replace_ST /size /matches-par /type-par /comp)
 (let ((/type-save /type)
       (/matches-save /matches))
  (set! /type /type-par)
  (set! /matches /matches-par)
  (display-list "MT_Replace_ST called with " (gen-length /matches) " matches.")
  (let ((/type2match (hash-table))
        (/types '())
        (/type-save /type)
        (/match-save /match)
        (/size_limit 4)
        (/guard '())
        (//S/T 0)
        (/e '())
        (/n 0)
        (/orig /matches)
        (/new '())
        (/saved '())
        (/pats (list //T_/Expn_/Pat_/Many //T_/Expn_/Pat_/Any //T_/Expn_/Pat_/One //T_/Cond_/Pat_/Many //T_/Cond_/Pat_/Any //T_/Cond_/Pat_/One //T_/Expn_/Int_/One //T_/Expn_/Int_/Any //T_/Expn_/Var_/One //T_/Expn_/Var_/Any //T_/Cond_/Int_/One //T_/Cond_/Int_/Any //T_/Cond_/Var_/One //T_/Cond_/Var_/Any)))
   (set! /type 0)
   (set! /match '())
   (@Down)
   (cond
    ((> /comp 0)
     ; Take out any matches where the nth component is a pattern 
     (set! /new '())
     (for-in /match /matches 
      (cond
       ((or (> /comp (@Size (@Get_n /match 1))) (member (@ST (@Get /match (list 1 /comp))) /pats))
        (set! /saved (cons /match /saved)))
       (#t
        (set! /new (cons /match /new)))))
     (set! /matches (reverse /new))))
   ; saved contains ifmatches which don't fit the pattern: 
   ; these will go after the IF: 
   (set! /saved (reverse /saved))
   (cond
    ((or (< (gen-length /matches) /size_limit) (> /comp /size))
     ; Either there is no point splitting such a small list, 
     ; or all the patterns are the same down to the first level 
     (cond
      ((null? /orig)
       (@Paste_Over (@Skip)))
      (#t
       (@Splice_Over /orig)))
     (@Up)
     ; back to statement sequence 
    )
    (#t
     ; Split matches list according to specific type: 
     (for-in /match /matches 
      (begin
       (cond
        ((= /comp 0)
         (set! //S/T (@ST (@Get_n /match 1))))
        (#t
         (set! //S/T (@ST (@Get /match (list 1 /comp))))))
       (cond
        ((null? (gethash /type2match //S/T))
         (puthash /type2match //S/T (list /match))
         (set! /types (cons //S/T /types)))
        (#t
         (puthash /type2match //S/T (cons /match (gethash /type2match //S/T)))))))
     (cond
      ((= /comp 0)
       (set! /e /%const__meta_trans__2))
      (#t
       (set! /n (@Make //T_/Number /comp '()))
       (set! /e (@Make 202 '() (list (@Make 9 (@Make_Name "@ST") '()) (@Make 10 '() (list (@Make 254 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@I") '()) (@Make 10 '() '()))) (@Var_To_Expn /n))))))))
       (set! /e /%const__meta_trans__3)))
     (@Paste_Over (@Make 114 '() (list (@Make 7 '() (list (@Make 313 '() (list (@Var_To_Expn /e) (@Make 207 (@Make_Name "T_Skip") '()))) (@Make 17 '() (list (@Make 145 '() '()))))) (@Make 7 '() (list (@Make 308 '() '()) (@Make 17 '() (list (@Make 145 '() '()))))))))
     ; Partially sort the types list to put the most common types first: 
     (set! /types (@MT_Sort_Types /types))
     (@Down)
     ; to first guarded 
     (for-in /type /types 
      (begin
       ; Duplicate, then edit, the selected guarded 
       (@Paste_After (@I))
       (@Down)
       (@Down_To 2)
       ; to the T_Skip 
       (@Paste_Over (@Make //T_/Number /type '()))
       (@Up)
       (@Right)
       ; to the body 
       (@MT_Replace_ST /size (gethash /type2match /type) /type (+ /comp 1))
       (@Up)
       (@Right)
       ; to next guarded 
      ))
     ; Delete the extra guard (leaving the ELSE clause intact): 
     (@Delete)
     (@Up)
     ; to Cond 
     (cond
      ((> /comp 0)
       (@Paste_Over (@Make 139 '() (list (@Make 13 '() (list (@Make 6 '() (list (@Make 501 (@Make_Name "MT_ST_n") '()) (@Make 202 '() (list (@Make 9 (@Make_Name "@ST") '()) (@Make 10 '() (list (@Make 254 '() (list (@Make 202 '() (list (@Make 9 (@Make_Name "@I") '()) (@Make 10 '() '()))) (@Var_To_Expn /n))))))))))) (@Make 17 '() (list (@I))))))))
     (@Splice_After /saved)
     (@Up)
     ; back to the statement sequence 
    ))
   (set! /type /type-save)
   (set! /match /match-save))
  (set! /type /type-save)
  (set! /matches /matches-save)))

(define (@MT_Sort_Types /types)
 (let ((/common_types (list //T_/Plus //T_/Times //T_/Negate //T_/Invert //T_/And //T_/Or //T_/Not //T_/Equal //T_/Not_/Equal //T_/Less //T_/Less_/Eq))
       (/type-save /type)
       (funct-result '()))
  (set! /type 0)
  (for-in /type (reverse /common_types) 
   (cond
    ((member /type /types)
     (set! /types (cons /type (@Set_Difference /types (list /type)))))))
  (set! funct-result /types)
  (set! /type /type-save)
  funct-result))

#t
