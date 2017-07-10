;;; Scheme translation of WSL code
(define (/foreach-delete_all_redundant-1 //Depth //A/S_/Type)
 (cond
  ((or (= (@ST (@I)) //T_/Skip) (= (@ST (@I)) //T_/Comment))
   ; Skip SKIPs 
  )
  ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) (@Make_Name "init_NOP_flag")))
   ; Keep init functions: these become static declarations 
  )
  ((@Trans? //T/R_/Delete_/Redundant_/Statement)
   (display-list-flush "X")
   (@Paste_Over (@Skip)))
  ((and (= (@ST (@I)) //T_/Assignment) (> (@Size (@I)) 1))
   (@Down)
   (set! /fl_flag1 0)
   (while (= /fl_flag1 0) 
    (cond
     ((@Trans? //T/R_/Delete_/Redundant_/Statement)
      (display-list-flush "X")
      (@Delete)
      (cond
       ((> (@Posn_n) (@Size (@Parent)))
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0))))
     ((@Right?)
      (@Right)
      (set! /fl_flag1 0))
     (#t
      (set! /fl_flag1 1))))
   (@Up)
   (cond
    ((= (@Size (@I)) 0)
     (@Paste_Over (@Skip)))))
  (#t
   (display-list-flush "."))))

(define (/foreach-delete_all_redundant-2 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Proc_/Call)
   (puthash /save (@Posn) (@I)))))

(define (/foreach-delete_all_redundant-3 //Depth //A/S_/Type)
 (cond
  ((= (@ST (@I)) //T_/Where)
   (@Down_Last)
   (@Down)
   ; to first defn 
   (cond
    ((= (@ST (@I)) //T_/Proc)
     (puthash /save (@Posn) (list (@Get_n (@I) 2) (@Get_n (@I) 3)))))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((= (@ST (@I)) //T_/Proc)
       (puthash /save (@Posn) (list (@Get_n (@I) 2) (@Get_n (@I) 3)))))))
   (@Up)
   (@Up)
   ; back to WHERE 
   (cond
    ((@Trans? //T/R_/Globals_/To_/Pars)
     (set! //D/R/S_/Globals_/To_/Pars_/Done 1)
     (@Trans //T/R_/Globals_/To_/Pars ""))))))

(define (/foreach-delete_all_redundant-4 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (not (null? (gethash /save (@Posn)))))
   (@Paste_Over (gethash /save (@Posn))))
  ((= (@ST (@I)) //T_/Where)
   (@Down_Last)
   (@Down)
   ; to first defn 
   (cond
    ((and (= (@ST (@I)) //T_/Proc) (not (null? (gethash /save (@Posn)))))
     (set! /pair (gethash /save (@Posn)))
     (@Paste_Over (@Make //T_/Proc '() (list (@Get_n (@I) 1) (wsl-ref /pair 1) (wsl-ref /pair 2) (@Get_n (@I) 4))))))
   (while (@Right?) 
    (begin
     (@Right)
     (cond
      ((and (= (@ST (@I)) //T_/Proc) (not (null? (gethash /save (@Posn)))))
       (set! /pair (gethash /save (@Posn)))
       (@Paste_Over (@Make //T_/Proc '() (list (@Get_n (@I) 1) (wsl-ref /pair 1) (wsl-ref /pair 2) (@Get_n (@I) 4))))))))
   (@Up)
   (@Up))))

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
(define (@Delete_All_Redundant_Test)
 (cond
  ((and (not (= (@ST (@I)) //T_/Assign)) (null? (@Stat_Types (@I))))
   (@Fail "The selected item does not include any statements"))
  (#t
   (@Pass))))

(define (@Delete_All_Redundant_Code //Data)
 (cond
  ((@Trans? //T/R_/Delete_/Redundant_/Statement)
   (@Trans //T/R_/Delete_/Redundant_/Statement ""))
  (#t
   (let ((/posn (@Posn))
         (/save-save /save)
         (/flag_save //D/R/S_/Globals_/To_/Pars_/Done))
    (set! /save (hash-table))
    (@Goto '())
    (set! /save (@DRS_Fix_Procs  /save))
    (@Goto /posn)
    (@Ateach_Statement /foreach-delete_all_redundant-1 0 (@AS_Type) 0)
    (cond
     ((null? (@Program))
      (@New_Program (@Skips))))
    (@Goto '())
    (set! /save (@DRS_Restore_Procs  /save))
    (@Goto /posn)
    (cond
     ((@Trans? //T/R_/Delete_/All_/Skips)
      (@Trans //T/R_/Delete_/All_/Skips "")))
    (display-list "")
    (set! //D/R/S_/Globals_/To_/Pars_/Done /flag_save)
    (set! /save /save-save)))))

(define (@DRS_Fix_Procs /save-par)
 (let ((/save-save /save)
       (funct-result '()))
  (set! /save /save-par)
  ; Save all proc call and defn parameters (using the posn as key) 
  (@Ateach_Statement /foreach-delete_all_redundant-2 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Ateach_Statement /foreach-delete_all_redundant-3 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (set! funct-result /save)
  (set! /save /save-save)
  funct-result))

(define (@DRS_Restore_Procs /save-par)
 (let ((/save-save /save)
       (funct-result '()))
  (set! /save /save-par)
  (let ((/pair-save /pair))
   (set! /pair '())
   (@Ateach_Statement /foreach-delete_all_redundant-4 0 (@AS_Type) 0)
   (cond
    ((null? (@Program))
     (@New_Program (@Skips))))
   (set! /pair /pair-save))
  (set! funct-result /save)
  (set! /save /save-save)
  funct-result))

#t
