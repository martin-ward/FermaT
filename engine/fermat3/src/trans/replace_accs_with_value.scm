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
(define (@Replace_Accs_With_Value_Test)
 ; For speed, we just check for occurrences of the accumulators 
 (let ((/accs (@Make_Set (my-map @Make_Name (list "ax" "bx" "cx" "dx" "si" "di" "zf" "cf" "__tmp" "__junk" "__tmp1" "__tmp2" "__tmp3" "__tmp4" "__tmp5" "__tmp6" "__tmp7" "__tmp8" "__tmp9" "__tmp10" "__tmp11" "__tmp12" "__tmp13" "__tmp14" "__tmp15" "__cpar1" "__cpar2" "__cpar3" "__cpar4" "__cpar5" "__cpar6" "__cpar7" "__cpar8" "__cpar9" "__cpar10" "__cpar11" "__cpar12" "__cpar13" "__cpar14" "__cpar15" "a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9" "a10" "a11" "a12" "a13" "a14" "a15" "x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "x10" "x11" "x12" "x13" "x14" "x15" "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15" "cc" "IHADCB")))))
  (cond
   ((or (not (null? (intersection-n /accs (@Used (@Item))))) (and (@Cs? (@I)) (not (null? (intersection-n /accs (@Used (@Get_n (@I) 1)))))))
    (@Pass))
   (#t
    (@Fail "No accumulators are used in the selected item.")))))

(set! //Max_/Expression_/Size 20)
(define (@Replace_Accs_With_Value_Code //Data)
 (let ((/accs (@Make_Set (my-map @Make_Name (list "ax" "bx" "cx" "dx" "si" "di" "zf" "cf" "__tmp" "__junk" "__tmp1" "__tmp2" "__tmp3" "__tmp4" "__tmp5" "__tmp6" "__tmp7" "__tmp8" "__tmp9" "__tmp10" "__tmp11" "__tmp12" "__tmp13" "__tmp14" "__tmp15" "__cpar1" "__cpar2" "__cpar3" "__cpar4" "__cpar5" "__cpar6" "__cpar7" "__cpar8" "__cpar9" "__cpar10" "__cpar11" "__cpar12" "__cpar13" "__cpar14" "__cpar15" "a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9" "a10" "a11" "a12" "a13" "a14" "a15" "x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "x10" "x11" "x12" "x13" "x14" "x15" "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15" "cc" "IHADCB"))))
       (//D/S/E/C/Ts-save //D/S/E/C/Ts))
  (set! //D/S/E/C/Ts (hash-table))
  (set! //Options (@Read_Options_File //Options_/File))
  (cond
   ((not (null? (gethash //Options "Max_Expression_Size")))
    (set! //Max_/Expression_/Size (gethash //Options "Max_Expression_Size"))))
  ; Compute the DSECT pointer names 
  (set! //D/S/E/C/Ts (@CP_Find_DSECTs  //D/S/E/C/Ts))
  (display-list "DSECTs = " (my-map @N_String (@Hash_Keys //D/S/E/C/Ts)))
  ; ATEACH Variable does not work at all!!! Grrr... 
  (@RAWV /accs)
  (display-list " ")
  (set! //D/S/E/C/Ts //D/S/E/C/Ts-save)))

(define (@RAWV /accs)
 (let ((/elt '()))
  ; Don't replace r15 in assignment destination := r15 
  ; (Otherwise C_P can't tell that its a call rather than a return) 
  (cond
   ((and (not (null? (@Posn))) (= (@ST (@Parent)) //T_/Struct))
    ; Don't replace in a struct 
   )
   ((and (= (@ST (@I)) //T_/Variable) (equal? (@V (@I)) (@Make_Name "r15")) (> (gen-length (@Posn)) 1) (= (@ST (@GParent)) //T_/Assignment) (= (@ST (@Get_n (@Get_n (@GParent) 1) 1)) //T_/Var_/Lvalue) (equal? (@V (@Get_n (@Get_n (@GParent) 1) 1)) (@Make_Name "destination")))
    ; Don't replace in destination := r15 
   )
   ((and (= (@ST (@I)) //T_/Variable) (member (@V (@I)) /accs))
    (set! /elt (list (@V (@I))))
    (cond
     ((and (equal? (@V (@I)) (@Make_Name "IHADCB")) (or (= (@ST (@GParent)) //T_/Aref) (= (@ST (@GParent)) //T_/Aref_/Lvalue)))
      (set! /elt '()))))
   ((and (= (@ST (@I)) //T_/Aref) (= (@ST (@Get_n (@I) 1)) //T_/Variable) (member (@V (@Get_n (@I) 1)) /accs) (= (@Size (@Get_n (@I) 2)) 1) (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Number))
    (set! /elt (list (@V (@Get_n (@I) 1)) (- (@V (@Get_n (@Get_n (@I) 2) 1)))))))
  (cond
   ((not (null? /elt))
    (let ((/val (@Find_Value /elt)))
     (cond
      ((null? /val)
       (display-list-flush "."))
      ((and (= (@ST /val) //T_/X_/Funct_/Call) (equal? (@V (@Get_n /val 1)) /address_of) (= (@ST (@Get_n (@Get_n /val 2) 1)) //T_/String))
       (display-list-flush ".")
       ; Don't replace the address of a string! 
      )
      ((not (null? (intersection-n (@Variables /val) /reg_inits)))
       (display-list-flush "."))
      ((and (member /a_name (@Variables /val)) (not (null? (intersection-n (@Variables /val) /accs))))
       ; Replacing a register with a[..reg..] is not very useful 
       (display-list-flush "r"))
      ((and (= (@ST /val) //T_/Variable) (@Starts_With? (@V /val) "NOTUSED_"))
       ; Don't replace a variable with NOTUSED_nnn 
       (display-list-flush "."))
      ((> (@Posn_n) 1)
       (display-list-flush "x")
       (@Paste_Over /val))
      ((and (or (= (@ST (@Parent)) //T_/Aref) (= (@ST (@Parent)) //T_/Sub_/Seg) (= (@ST (@Parent)) //T_/Rel_/Seg)) (not (= (@ST /val) //T_/Variable)))
       (display-list-flush "."))
      ((and (or (= (@ST (@Parent)) //T_/Aref_/Lvalue) (= (@ST (@Parent)) //T_/Sub_/Seg_/Lvalue) (= (@ST (@Parent)) //T_/Rel_/Seg_/Lvalue)) (not (= (@ST /val) //T_/Var_/Lvalue)))
       (display-list-flush "."))
      ((and (member /address_of (my-map HEAD (@X_Funct_Calls /val))) (> (gen-length (@Posn)) 2) (or (or (= (@ST (@GParent)) //T_/Aref) (= (@ST (@GParent)) //T_/Aref_/Lvalue)) (or (= (@ST (@GGParent)) //T_/Aref) (= (@ST (@GGParent)) //T_/Aref_/Lvalue))))
       (display-list-flush "x")
       (@Paste_Over /val))
      ((equal? (@Variables /val) (list /a_name))
       (display-list-flush "x")
       (@Paste_Over /val))
      ((and (not (@Is_Addr? /val)) (> (@Total_Size /val) //Max_/Expression_/Size))
       (display-list-flush "b"))
      (#t
       (display-list-flush "x")
       (@Paste_Over /val)))))
   ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (member (@Make_Name "vpp") (@Assigned (@I))))
    ; Don't replace in a vpp function 
   )
   ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (member (@Make_Name "regs") (@Assigned (@I))))
    ; Don't replace in a call 
   )
   ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) (@Make_Name "EXEC_CICS")))
    ; Don't replace in a CICS call since the register may be modified 
   )
   ((and (= (@ST (@I)) //T_/A_/Proc_/Call) (equal? (@V (@Get_n (@I) 1)) (@Make_Name "str_copy")))
    ; Don't replace in str_cpy since the length might be different 
   )
   ((and (= (@ST (@I)) //T_/Assign) (= (@ST (@Get_n (@I) 1)) //T_/Var_/Lvalue) (not (null? (gethash //D/S/E/C/Ts (@V (@Get_n (@I) 1))))))
    ; Don't replace the register in a DSECT pointer assignment 
   )
   ((@Cs? (@I))
    (@Down)
    (cond
     ((not (null? (intersection-n (@Used (@I)) /accs)))
      (@RAWV /accs))
     ((and (= (@ST (@I)) //T_/Var) (not (null? (intersection-n (@Used (@Get_n (@I) 1)) /accs))))
      (@RAWV /accs)))
    (while (@Right?) 
     (begin
      (@Right)
      (cond
       ((not (null? (intersection-n (@Used (@I)) /accs)))
        (@RAWV /accs))
       ((and (= (@ST (@I)) //T_/Var) (not (null? (intersection-n (@Used (@Get_n (@I) 1)) /accs))))
        (@RAWV /accs)))))
    (@Up)))))

#t
