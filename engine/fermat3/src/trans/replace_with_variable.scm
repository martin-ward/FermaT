;;; Scheme translation of WSL code
(define /%const__replace_with_variable__1 (@Make 109 '() (list (@Make 313 '() (list (@Make 261 '() (list (@Make 205 1 '()))) (@Make 217 -2 '()))))))
(define /%const__replace_with_variable__2 (@Make 109 '() (list (@Make 313 '() (list (@Make 217 -1 '()) (@Make 261 '() (list (@Make 205 2 '()))))))))
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
(define (@Replace_With_Variable_Test)
 (cond
  ((not (= (@GT (@I)) //T_/Expression))
   (@Fail "The selected item is not an expression."))
  (#t
   (let ((/var (@Find_Variable (@I))))
    (cond
     ((null? /var)
      (@Fail "Cannot find a variable assigned this value."))
     (#t
      (@Pass)))))))

(define (@Replace_With_Variable_Code //Data)
 (let ((/var (@Find_Variable (@I))))
  (cond
   ((null? /var)
    (display-list "ERROR in Replace_With_Variable!!!"))
   (#t
    (@Paste_Over /var)))))

; Find a variable which is assigned to the given expression. 
; Return an item, or < > if a variable cannot be found 
(define (@Find_Variable /exp)
 (let ((//Orig_/Posn (@Posn))
       (/elts (@Elements /exp))
       (//R '())
       (/clobbered '())
       (/found 0)
       (//Calls (@Make_Set (list //T_/M/W_/Proc_/Call //T_/X_/Proc_/Call //T_/Proc_/Call))))
  ; Repeated move left and up until we find a variable assigned to the expression. 
  ; If the variable is clobbered, then give up. 
  (while (and (or (and (not (= (@ST (@Parent)) //T_/M/W_/Funct)) (not (= (@ST (@Parent)) //T_/M/W_/B/Funct))) (not (= (@Posn_n) 5))) (not (= //T_/Statement (@GT (@I)))) (@Up?)) 
   (@Up))
  (cond
   ((and (or (= (@ST (@Parent)) //T_/M/W_/Funct) (= (@ST (@Parent)) //T_/M/W_/B/Funct)) (not (= //T_/Statement (@GT (@I)))) (= (@Posn_n) 5))
    (@Left)
    (@Down_Last)))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (begin
    (cond
     ((or (= (@ST (@I)) //T_/Floop) (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/A_/S))
      ; If there are no calls and elts is not assigned in the loop or system, 
      ; then we can safely get its value from outside: 
      (cond
       ((or (not (null? (intersection-n (@Stat_Types (@I)) //Calls))) (@Elt_Clash_List? (@Elts_Assigned (@I)) /elts))
        (set! //R '())
        (set! /fl_flag1 1))
       (#t
        (set! /fl_flag1 0))))
     (#t
      (set! /fl_flag1 0)))
    (cond
     ((= /fl_flag1 0)
      (cond
       ((not (= (@GT (@I)) //T_/Statement))
        (set! //R '())
        (set! /fl_flag1 1))
       ((@Left?)
        (@Left)
        (cond
         ((= (@ST (@I)) //T_/Assert)
          (let ((/__/O/K 1))
           (vector-set! /__/Match_array 0 /exp)
           (set! /__/O/K (@New_Match  /%const__replace_with_variable__1 (@I) /__/O/K))
           (cond
            ((= /__/O/K 1)
             (let ((/__/X_save //X))
              (set! //X (vector-ref /__/Match_array 1))
              (cond
               ((or (= (@ST //X) //T_/Variable) (= (@ST //X) //T_/Struct))
                (set! //R //X)))
              (set! //X /__/X_save)))
            (#t
             (let ((/__/O/K 1))
              (vector-set! /__/Match_array 1 /exp)
              (set! /__/O/K (@New_Match  /%const__replace_with_variable__2 (@I) /__/O/K))
              (cond
               ((= /__/O/K 1)
                (let ((/__/X_save //X))
                 (set! //X (vector-ref /__/Match_array 0))
                 (cond
                  ((or (= (@ST //X) //T_/Variable) (= (@ST //X) //T_/Struct))
                   (set! //R //X)))
                 (set! //X /__/X_save))))))))
          (cond
           ((not (null? //R))
            (set! /fl_flag1 1))
           (#t
            (set! /fl_flag1 0))))
         ((= (@ST (@I)) //T_/Assignment)
          (cond
           ((@Elt_Clash_List? (@Elts_Assigned (@I)) /elts)
            (set! //R '())
            (set! /fl_flag1 1))
           (#t
            (for-in /assign (@Cs (@I)) 
             (cond
              ((and (or (= (@ST (@Get_n /assign 1)) //T_/Var_/Lvalue) (= (@ST (@Get_n /assign 1)) //T_/Struct_/Lvalue)) (@Equal? (@Get_n /assign 2) /exp) (not (@Elt_Clash_List? (@Elts_Assigned /assign) /elts)))
               (set! //R (@Lvalue_To_Expn (@Get_n /assign 1))))))
            (cond
             ((not (null? //R))
              (set! /fl_flag1 1))
             ((@Elt_Clash_List? (@Elts_Assigned (@I)) /elts)
              (set! //R '())
              (set! /fl_flag1 1))
             (#t
              (set! /fl_flag1 0))))))
         ((or (@Elt_Clash_List? (@Elts_Assigned (@I)) /elts) (not (null? (intersection-n (@Stat_Types (@I)) //Calls))))
          (set! //R '())
          (set! /fl_flag1 1))
         (#t
          (set! /fl_flag1 0)))
        (cond
         ((= /fl_flag1 0)
          (set! /clobbered (union-n /clobbered (@Elts_Assigned (@I))))
          (set! /fl_flag1 0))))
       ((not (@Up?))
        (set! //R '())
        (set! /fl_flag1 1))
       (#t
        (@Up)
        (cond
         ((not (@Up?))
          (set! //R '())
          (set! /fl_flag1 1))
         (#t
          (@Up)
          ; to item containing the statement sequence 
          (cond
           ((and (= (@ST (@I)) //T_/Var) (@Elt_Clash_List? (@Elt_Lvars (@Get_n (@I) 1)) /elts))
            ; A variable in the expression is local 
            (set! //R '())
            (set! /fl_flag1 1))
           ((and (or (= (@ST (@I)) //T_/M/W_/Funct) (= (@ST (@I)) //T_/M/W_/B/Funct)) (@Elt_Clash_List? (@Elt_Lvars (@Get_n (@I) 3)) /elts))
            ; A variable in the expression is local 
            (set! //R '())
            (set! /fl_flag1 1))
           ((= (@ST (@I)) //T_/Var)
            ; keep scanning left 
            (set! /fl_flag1 0))
           ((and (= (@GT (@I)) //T_/Guarded) (@Up?))
            (@Up)
            (set! /fl_flag1 0))
           ((and (or (= (@ST (@I)) //T_/Floop) (= (@ST (@I)) //T_/While) (= (@ST (@I)) //T_/A_/S)) (@Up?))
            ; If no variable is assigned in the loop or system, 
            ; and there are no proc calls in the loop 
            ; (which could modify exp) 
            ; then look outside for an assignment 
            (cond
             ((not (null? (intersection-n (@Stat_Types (@I)) //Calls)))
              (set! //R '())
              (set! /fl_flag1 1))
             ((@Elt_Clash_List? (@Elts_Assigned (@I)) /elts)
              (set! //R '())
              (set! /fl_flag1 1))
             (#t
              (set! /clobbered (union-n /clobbered (@Elts_Assigned (@I))))
              (set! /fl_flag1 0))))
           ((and (= (@ST (@I)) //T_/Action) (> (gen-length (@Posn)) 2))
            (@Up)
            (@Up)
            ; to the action system 
            (set! /fl_flag1 0))
           ((and (= (@ST (@I)) //T_/Where) (not (@Elt_Clash_List? (@Elts_Assigned (@I)) /elts)))
            ; keep scanning left 
            (set! /fl_flag1 0))
           (#t
            (set! //R '())
            (set! /fl_flag1 1)))))))))))
  (@Goto //Orig_/Posn)
  ; If the variable we found has been clobbered, then give up: 
  (cond
   ((and (not (null? //R)) (@Elt_Clash? /clobbered (@Struct_Elts //R)))
    (set! //R '())))
  //R))

#t
