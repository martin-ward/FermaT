;;; Scheme translation of WSL code
(define (/foreach-sort_procs-1 //Depth //A/S_/Type)
 (cond
  ((and (= (@ST (@I)) //T_/Proc_/Call) (null? (gethash /dfs (@V (@Get_n (@I) 1)))))
   (puthash /dfs (@V (@Get_n (@I) 1)) /n)
   (set! /n (+ /n 1))
   (cond
    ((not (null? (gethash /bodies (@V (@Get_n (@I) 1)))))
     (set! /dfs (@SP_DFS_Calls  (gethash /bodies (@V (@Get_n (@I) 1))) /bodies /n /dfs)))))))

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
; Sort_Procs 
(define (@Sort_Procs_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Where))
   (@Fail "Selected item is not a WHERE clause"))
  (#t
   (@Pass))))

(define (@Sort_Procs_Code //Data)
 (let ((/bodies-save /bodies)
       (/nodes '())
       (/succs (hash-table))
       (/preds (hash-table))
       (/dfs-save /dfs)
       (/topsort '())
       (/new '()))
  (set! /bodies (hash-table))
  (set! /dfs (hash-table))
  (for-in /defn (@Cs (@Get_n (@I) 2)) 
   (cond
    ((= (@ST /defn) //T_/Proc)
     (set! /nodes (cons (@V (@Get_n /defn 1)) /nodes))
     (puthash /bodies (@V (@Get_n /defn 1)) /defn)
     (for-in /pair (@Proc_Calls /defn) 
      (begin
       (puthash /succs (@V (@Get_n /defn 1)) (cons (wsl-ref /pair 1) (gethash /succs (@V (@Get_n /defn 1)))))
       (puthash /preds (wsl-ref /pair 1) (cons (@V (@Get_n /defn 1)) (gethash /preds (wsl-ref /pair 1)))))))
    (#t
     (set! /new (cons /defn /new)))))
  ; Compute the DFS order number for each proc, starting with the main body 
  (set! /dfs (@SP_DFS_Calls  (@Get_n (@I) 1) /bodies 1 /dfs))
  (set! /topsort (@Topological_Sort /nodes /succs /preds /dfs))
  (for-in /name /topsort 
   (set! /new (cons (gethash /bodies /name) /new)))
  (@Down_To 2)
  (@Paste_Over (@Make //T_/Definitions '() (reverse /new)))
  (@Up)
  (set! /bodies /bodies-save)
  (set! /dfs /dfs-save)))

; Compute a depth first search ordering for the proc calls 
; starting with the calls in the given item 
(define (@SP_DFS_Calls //I /bodies-par /n-par /dfs-par)
 (let ((/dfs-save /dfs)
       (/n-save /n)
       (/bodies-save /bodies)
       (funct-result '()))
  (set! /dfs /dfs-par)
  (set! /n /n-par)
  (set! /bodies /bodies-par)
  (@Edit)
  (@New_Program //I)
  (@Foreach_Statement /foreach-sort_procs-1 0 (@AS_Type) 0)
  (cond
   ((null? (@Program))
    (@New_Program (@Skips))))
  (@Undo_Edit)
  (set! funct-result /dfs)
  (set! /dfs /dfs-save)
  (set! /n /n-save)
  (set! /bodies /bodies-save)
  funct-result))

; ----------------------------------------------------------------------- 

