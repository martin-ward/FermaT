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
;
;==========================================================================
;* Compute the list of variables needed.
;* Save the global values of the variables.
;* Define a local array (__Match_array) to store the matches.
;* Replace the match var names in the pattern by array index numbers.
;* Replace interpolations in the pattern by local variables (__Match_int_NNN).
;* Add assignments to the __Match_int_NNN variables.
;* Pass the pattern and current item to the @New_Match function
;    -- this returns true or false and updates __Match_array as needed.
;* If the result is TRUE, copy from __Match_array to the variables
;  and execute the THEN part. Otherwise, execute the ELSE part.
;* Finally, restore the global values of variables.
;
;IFMATCH Statement IF ~?B THEN ~?(S1); ~*S2 ELSE ~*=S2 FI
;  THEN ... match part ...
;  ELSE ... nomatch part FI
;
;Becomes:
;
;VAR < __OK := 1 >:
;  __Match_array[2] := S1;
;
;  @New_Match(FILL Statement IF ~?1 THEN ~?(2); ~*3 ELSE ~*=3 FI ENDFILL, @I
;             VAR __OK);
;  IF __OK = 1
;    THEN VAR < __B_save := B,
;               __S2_save := S2 >:
;           B := __Match_array[1];
;           S2 := __Match_array[3];
;           ... match part ...;
;	   B := __B_save;
;           S2 := __S2_sav ENDVAR
;    ELSE ... nomatch part ... FI ENDVAR
;
;
;==========================================================================
; Match the pattern (WSL item which may contain Pat/Int/Var types) 
; against the data (WSL item). All variables and expressions are actually 
; integer indexes into the __Match_array table. 
; Updates __Match_array table with the matched pattern variables. 
(set! /__/Match_array (make-vector-eval 1999 '()))
(define (@New_Match /pat /dat //O/K-par)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K //O/K-par)
  (let ((/type (vector-ref //Syntax_/Type (- (@ST /pat) 1))))
   (cond
    ((equal? /type //Syntax_/One_/Pattern)
     ; Store current item in table (matches anything): 
     (vector-set! /__/Match_array (- (+ (@V /pat) 1)) /dat))
    ((equal? /type //Syntax_/One_/Int)
     ; Match against the stored value in the table: 
     (cond
      ((not (@LR_Equal? (vector-ref /__/Match_array (- (@V (@Get_n /pat 1)) 1)) /dat))
       (set! //O/K 0))))
    ((equal? /type //Syntax_/One_/Var)
     ; Match against the stored value in the table: 
     (cond
      ((not (@LR_Equal? (vector-ref /__/Match_array (- (+ (@V /pat) 1))) /dat))
       (set! //O/K 0))))
    ((not (= (@ST /pat) (@ST /dat)))
     (set! //O/K 0))
    ((@Has_Value_Type? (@ST /pat))
     (cond
      ((not (equal? (@V /pat) (@V /dat)))
       (set! //O/K 0))))
    (#t
     ; Both pat and dat are types with components, 
     ; so match the two lists of components: 
     (set! //O/K (@New_Match_List  (@Cs /pat) (@Cs /dat) //O/K)))))
  (set! funct-result //O/K)
  (set! //O/K //O/K-save)
  funct-result))

; Set OK to 0 unless the list of patterns matches the list of data items 
(define (@New_Match_List /pats /dats //O/K-par)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K //O/K-par)
  (cond
   ((and (null? /pats) (null? /dats))
    #t)
   ((null? /pats)
    (set! //O/K 0))
   (#t
    (set! //O/K (@New_Match_List_Sub  /pats /dats (vector-ref //Syntax_/Type (- (@ST (car /pats)) 1)) //O/K))))
  (set! funct-result //O/K)
  (set! //O/K //O/K-save)
  funct-result))

(define (@New_Match_List_Sub /pats /dats /type //O/K-par)
 (let ((//O/K-save //O/K)
       (funct-result '()))
  (set! //O/K //O/K-par)
  (cond
   ((null? /dats)
    ; The pattern must be an X_Int_Any or X_Var_Any with an empty value, 
    ; or a ~*var which will match the empty list: 
    (cond
     ((not (null? (cdr /pats)))
      ; Cannot match empty data against more than one pattern 
      (set! //O/K 0))
     ((equal? /type //Syntax_/Any_/Int)
      (cond
       ((not (null? (vector-ref /__/Match_array (- (@V (@Get_n (car /pats) 1)) 1))))
        (set! //O/K 0))))
     ((equal? /type //Syntax_/Any_/Var)
      (cond
       ((not (null? (vector-ref /__/Match_array (- (+ (@V (car /pats)) 1)))))
        (set! //O/K 0))))
     ((equal? /type //Syntax_/Any_/Pattern)
      (vector-set! /__/Match_array (- (+ (@V (car /pats)) 1)) '()))
     (#t
      (set! //O/K 0))))
   ((or (equal? /type //Syntax_/Any_/Pattern) (equal? /type //Syntax_/Many_/Pattern))
    ; Store as many components as required to match 
    ; If this is the only star pattern, then we can work out how much it 
    ; has to match from the lengths: 
    (cond
     ((not (@Star_Patterns? (cdr /pats)))
      (let ((/dats1 '())
            (/len (gen-length (cdr /pats))))
       (cond
        ((< (gen-length /dats) /len)
         ; Not enough data items to match, even if this pat 
         ; matches zero data items 
         (set! //O/K 0))
        ((and (equal? /type //Syntax_/Many_/Pattern) (= (gen-length /dats) /len))
         ; This pat will consume at least one dat, so no match: 
         (set! //O/K 0))
        (#t
         (vector-set! /__/Match_array (- (+ (@V (car /pats)) 1)) (@Sub_Seg /dats 1 (- (gen-length /dats) /len)))
         ; Match the rest of the list: 
         (set! //O/K (@New_Match_List  (cdr /pats) (@Final_Seg /dats (+ (- (gen-length /dats) /len) 1)) //O/K))))))
     (#t
      ; There is another pattern in the pats list to be matched 
      ; so we may need to backtrack 
      ; Attempt to match progressively more dats against this pattern 
      ; until the remaining dats match the remaining pats: 
      (let ((/match '())
            (/i (- (@V (car /pats)))))
       (cond
        ((equal? /type //Syntax_/Many_/Pattern)
         ; We must match at least one dat against this pat 
         (set! /match (list (car /dats)))
         (set! /dats (cdr /dats))))
       (set! /pats (cdr /pats))
       (set! /fl_flag1 0)
       (while (= /fl_flag1 0) 
        (begin
         (vector-set! /__/Match_array (- /i 1) (reverse /match))
         (set! //O/K (@New_Match_List  /pats /dats //O/K))
         (cond
          ((= //O/K 1)
           (set! /fl_flag1 1))
          ((null? /dats)
           (set! //O/K 0)
           (set! /fl_flag1 1))
          (#t
           ; Add next dat to match and try again: 
           (set! //O/K 1)
           (set! /match (cons (car /dats) /match))
           (set! /dats (cdr /dats))
           (set! /fl_flag1 0)))))))))
   ((or (equal? /type //Syntax_/Any_/Int) (equal? /type //Syntax_/Any_/Var))
    (let ((/match 0))
     (cond
      ((equal? /type //Syntax_/Any_/Int)
       (set! /match (vector-ref /__/Match_array (- (@V (@Get_n (car /pats) 1)) 1))))
      (#t
       (set! /match (vector-ref /__/Match_array (- (+ (@V (car /pats)) 1))))))
     (cond
      ((not (@LR_Seq_Equal? /match (@Sub_Seg /dats 1 (gen-length /match))))
       (set! //O/K 0))
      (#t
       (set! //O/K (@New_Match_List  (cdr /pats) (@Final_Seg /dats (+ (gen-length /match) 1)) //O/K))))))
   (#t
    (set! //O/K (@New_Match  (car /pats) (car /dats) //O/K))
    (cond
     ((= //O/K 1)
      (set! //O/K (@New_Match_List  (cdr /pats) (cdr /dats) //O/K))))))
  (set! funct-result //O/K)
  (set! //O/K //O/K-save)
  funct-result))

; Check for ~*var, ~+var, ~*(e) or ~*=e patterns in the given list: 
(define (@Star_Patterns? /pats)
 
 (and (not (null? /pats)) (or (@Star_Match_Type? (@ST (car /pats))) (@Star_Patterns? (cdr /pats)))))

; Below are the old match functions, these will eventually be replaced: 
; ---------------------------------------------------------------------------  
; The main pattern matching function.  It takes three arguments as follows:    
;                                                                              
; Pattern - the pattern against which the WSL code is to be matched.           
;                                                                              
;    Data - the WSL code against which the pattern is to be matched.           
;                                                                              
;   Table - the initial pattern-match table which consists of a list of pairs  
;           in which each pair consists of a variable name and the WSL item    
;           that it matches.                                                   
;                                                                              
; The function returns a new pattern-match table or an empty table if the      
; match failed.                                                                
; ---------------------------------------------------------------------------  
(define (@Match //Pattern //Data //Table)
 (let ((//Result (@Do_Match //Pattern //Data //Table 0)))
  (cond
   ((and (number? //Result) (= //Result 1))
    (set! //Result (list "T")))
   ((and (not (null? //Result)) (or (not (number? //Result)) (not (= //Result 1))))
    (set! //Result (cons "T" //Result))))
  //Result))

; ---------------------------------------------------------------------------  
; The following functions perform the actual pattern matching.  In addition    
; to the parameters used by `@Match' it adds an extra parameter:               
;                                                                              
; Seq_More - this is set to 1 if it is allowable for the pattern matcher to    
;            match more code against a `*' pattern, ie it has not moved on to  
;            matching the next part of the pattern.                            
;                                                                              
; Note that all these functions use the tests `@LR_Equal?' and                 
; `@LR_Seq_Equal?' to compare literal items since expressions may be required  
; to match l-values; eg in looking for dummy assignments: `~?X := ~?X'.        
;                                                                              
; ---------------------------------------------------------------------------  
(define (@Do_Match //Pattern //Data //Table //Seq_/More)
 (let ((//New-save //New)
       (//Temp '())
       (//Pat (if (null? //Pattern) '() (@Components //Pattern)))
       (//Dat (if (null? //Data) '() (@Components //Data)))
       (//Hd_/Pat (if (or (null? //Pattern) (null? (@Components //Pattern))) '() (car (@Components //Pattern))))
       (//Hd_/Dat (if (or (null? //Data) (null? (@Components //Data))) '() (car (@Components //Data))))
       (//Tl_/Pat (if (or (null? //Pattern) (null? (@Components //Pattern))) '() (cdr (@Components //Pattern))))
       (//Tl_/Dat (if (or (null? //Data) (null? (@Components //Data))) '() (cdr (@Components //Data))))
       (funct-result '()))
  (set! //New '())
  (cond
   ((null? //Pattern)
    (cond
     ((null? //Data)
      (set! //New (@Pat_True //Table)))))
   ((@One_Pattern_Type? (@Spec_Type //Pattern))
    (set! //Temp (@Pat_Rcl (@Value //Pattern) //Table))
    ;If there has already been a match with this token, the result is stored in the variable `Temp'.
    (cond
     ((null? //Temp)
      (set! //New (@Pat_Sto (@Value //Pattern) //Data //Table)))
     ((@LR_Equal? (cdr //Temp) //Data)
      (set! //New //Table))))
   ((not (@Components? //Pattern))
    (set! //New (if (@Equal? //Pattern //Data) (@Pat_True //Table) '())))
   ((and (equal? (@Spec_Type //Pattern) (@Spec_Type //Data)) (equal? (@Value //Pattern) (@Value //Data)))
    (set! //New (@Do_Match_Sequence  //Pat //Hd_/Pat //Tl_/Pat //Dat //Hd_/Dat //Tl_/Dat //Table //Seq_/More //New))))
  (set! funct-result //New)
  (set! //New //New-save)
  funct-result))

; ---------------------------------------------------------------------------  
; The following function matches two lists of items.                           
; ---------------------------------------------------------------------------  
(define (@Do_Match_List //Pat //Dat //Table //Seq_/More)
 (let ((//New-save //New)
       (funct-result '()))
  (set! //New '())
  (cond
   ((null? //Pat)
    (cond
     ((null? //Dat)
      (set! //New (@Pat_True //Table)))))
   ((not (sequence? //Pat))
    (set! //New (if (@LR_Seq_Equal? //Pat //Dat) (@Pat_True //Table) '())))
   (#t
    (set! //New (@Do_Match_Sequence  //Pat (if (null? //Pat) '() (car //Pat)) (if (null? //Pat) '() (cdr //Pat)) //Dat (if (null? //Dat) '() (car //Dat)) (if (null? //Dat) '() (cdr //Dat)) //Table //Seq_/More //New))))
  (set! funct-result //New)
  (set! //New //New-save)
  funct-result))

; ---------------------------------------------------------------------------  
; The following function matches two sequences of items and takes into         
; account the fact that the pattern could contain wild cards.                  
; ---------------------------------------------------------------------------  
(define (@Do_Match_Sequence //Pat //Hd_/Pat //Tl_/Pat //Dat //Hd_/Dat //Tl_/Dat //Table //Seq_/More //New-par)
 (let ((//New-save //New)
       (funct-result '()))
  (set! //New //New-par)
  ;Pattern must contain a sequence of components.
  ;The value parts must match, if they have not already been skipped.
  (cond
   ((and (not (null? //Hd_/Pat)) (@Any_Pattern_Type? (@Spec_Type //Hd_/Pat)))
    (set! //New (@Do_Match_Any  //Pat //Hd_/Pat //Tl_/Pat //Dat //Hd_/Dat //Tl_/Dat //Table //Seq_/More //New)))
   ((and (not (null? //Hd_/Pat)) (@Many_Pattern_Type? (@Spec_Type //Hd_/Pat)))
    (set! //New (@Do_Match_Many  //Pat //Hd_/Pat //Tl_/Pat //Dat //Hd_/Dat //Tl_/Dat //Table //Seq_/More //New)))
   ((and (not (null? //Pat)) (not (null? //Dat)))
    (set! //New (@Do_Match //Hd_/Pat //Hd_/Dat //Table 0))
    (cond
     ((not (null? //New))
      (set! //New (@Do_Match_List //Tl_/Pat //Tl_/Dat //New 0)))))
   (#t
    (set! //New '())))
  (set! funct-result //New)
  (set! //New //New-save)
  funct-result))

; ---------------------------------------------------------------------------  
; The following function matches some actual WSL code against an `any' (ie     
; `*') wild card.  Note the use of `Seq_More' both being tested and being set  
; explicitly.                                                                  
; ---------------------------------------------------------------------------  
(define (@Do_Match_Any //Pat //Hd_/Pat //Tl_/Pat //Dat //Hd_/Dat //Tl_/Dat //Table //Seq_/More //New-par)
 (let ((//New-save //New)
       (funct-result '()))
  (set! //New //New-par)
  (let ((//Temp (@Pat_Rcl (@Value //Hd_/Pat) //Table)))
   ;If there has already been a match with this token, the result is stored in the variable `Temp'.
   (cond
    ((and (not (null? //Temp)) (= //Seq_/More 0))
     (set! //New (@Do_Match_List (concat (cdr //Temp) //Tl_/Pat) //Dat //Table 0)))
    (#t
     (cond
      ((and (sequence? //Dat) (not (null? //Dat)))
       ;Try to make this token match more of the data.
       (set! //New (@Pat_Add (@Value //Hd_/Pat) //Hd_/Dat //Table))
       (set! //New (@Do_Match_List //Pat //Tl_/Dat //New 1))))
     (cond
      ((null? //New)
       (cond
        ((null? //Temp)
         (set! //New (@Pat_Add (@Value //Hd_/Pat) '() //Table)))
        (#t
         (set! //New //Table)))
       (set! //New (@Do_Match_List //Tl_/Pat //Dat //New 0)))))))
  (set! funct-result //New)
  (set! //New //New-save)
  funct-result))

; ---------------------------------------------------------------------------  
; The following function matches some actual WSL code against a `many' (ie     
; `+') wild card.  Note the use of `Seq_More' both being tested and being set  
; explicitly.                                                                  
; ---------------------------------------------------------------------------  
(define (@Do_Match_Many //Pat //Hd_/Pat //Tl_/Pat //Dat //Hd_/Dat //Tl_/Dat //Table //Seq_/More //New-par)
 (let ((//New-save //New)
       (funct-result '()))
  (set! //New //New-par)
  (let ((//Temp (@Pat_Rcl (@Value //Hd_/Pat) //Table)))
   ;If there has already been a match with this token, the result is stored in the variable `Temp'.
   (cond
    ((and (not (null? //Temp)) (= //Seq_/More 0))
     (set! //New (@Do_Match_List (concat (cdr //Temp) //Tl_/Pat) //Dat //Table 0)))
    ((not (null? //Dat))
     (set! //New (@Pat_Add (@Value //Hd_/Pat) //Hd_/Dat //Table))
     (set! //New (@Do_Match_List //Tl_/Pat //Tl_/Dat //New 0))
     (cond
      ((null? //New)
       ;Try to make this token match more of the data.
       (set! //New (@Pat_Add (@Value //Hd_/Pat) //Hd_/Dat //Table))
       (set! //New (@Do_Match_List //Pat //Tl_/Dat //New 1)))))))
  (set! funct-result //New)
  (set! //New //New-save)
  funct-result))

; ---------------------------------------------------------------------------  
; Some support functions to make it all work.                                  
;   `@Pat_Sto'   stores some data in the named token of the table.             
;   `@Pat_Add'   concatenates some data in the named token of the table.       
;   `@Pat_Rcl'   recalls some data in the named token of the table.            
;   `@Pat_True'  sets a table to be `true' without storing anything.           
; ---------------------------------------------------------------------------  
(define (@Pat_Sto //Key //Data //Table)
 (let ((//New-save //New)
       (funct-result '()))
  (set! //New '())
  (cond
   ((or (null? //Table) (and (number? //Table) (= //Table 1)))
    (set! //New (list (cons //Key //Data))))
   (#t
    (cond
     ((equal? (car (car //Table)) //Key)
      (set! //New (cons (cons //Key //Data) (cdr //Table))))
     (#t
      (set! //New (cons (car //Table) (@Pat_Sto //Key //Data (cdr //Table))))))))
  (set! funct-result //New)
  (set! //New //New-save)
  funct-result))

(define (@Pat_Add //Key //Data //Table)
 (let ((//New-save //New)
       (funct-result '()))
  (set! //New '())
  (cond
   ((or (null? //Table) (and (number? //Table) (= //Table 1)))
    (cond
     ((null? //Data)
      (set! //New (list (list //Key))))
     (#t
      (set! //New (list (list //Key //Data))))))
   (#t
    (cond
     ((equal? (car (car //Table)) //Key)
      (cond
       ((null? //Data)
        (set! //New (cons (car //Table) (cdr //Table))))
       (#t
        (set! //New (cons (concat (car //Table) (list //Data)) (cdr //Table))))))
     (#t
      (set! //New (cons (car //Table) (@Pat_Add //Key //Data (cdr //Table))))))))
  (set! funct-result //New)
  (set! //New //New-save)
  funct-result))

; TODO: should use a different notation for variables which match anything 
; versis variables which must match their current value. 
; Currently, a variable containing < > is allowed to match anything: 
(define (@Pat_Rcl //Key //Table)
 (let ((/r (@Assoc //Key //Table)))
  (if (or (null? /r) (null? (cdr /r))) '() /r)))

(define (@Pat_True //Table)
 
 (if (null? //Table) 1 //Table))

; ---------------------------------------------------------------------------  

