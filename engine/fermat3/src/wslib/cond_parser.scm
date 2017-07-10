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
;-------------------------------------------------------------------------------  
;-------------------------------------------------------------------------------  
;  Parser for conditions.                                                         
;  Uses recursive-descent but switches to shift-reduce when parentheses           
;  are encountered, to dis-ambiguate bracketed conditions and expressions.        
;                                                                                 
;  Parses according to the following grammar:                                     
;                                                                                 
;  condition   ::=  b_term [`OR' b_term]*                                         
;                                                                                 
;  b_term      ::=  b_factor [`AND' b_factor]*                                    
;                                                                                 
;  b_factor    ::=  `NOT' b_factor | b_atom                                       
;                                                                                 
;  b_atom      ::=  `('condition `)' | `TRUE' | `FALSE' | `$Condition$'           
;                   expression rel_op expression | cond_pat | cond_prefix |       
;                   bfunct_call | mw_bfunct_call | x_bfunct_call                  
;                                                                                 
;                                                                                 
;  cond_pat    ::=  `~?' IDENTIFIER | `~+' IDENTIFIER | `~*' IDENTIFIER           
;                                                                                 
;  rel_op      ::=  `=' | `<>' | `<' | `<=' | `>' | `>=' |                        
;                   `IN' | `NOTIN'                                                
;                                                                                 
;                                                                                 
;  cond_prefix ::=  `EVEN?' `(' expression `)' | `ODD?' `(' expression `)' |      
;                   `EMPTY?' `(' s_expression `)' |                               
;                   `SUBSET?' `(' s_expression `,' s_expression `)' |             
;                   `MEMBER?' `(' expression `,' s_expression `)' |               
;                   `FORALL' lvalues `:' condition `END' |                        
;                   `EXISTS' lvalues `:' condition `END'                          
;                                                                                 
;  bfunct_call ::=  IDENTIFIER`?' [`(' [expression] {`,' expression}* `)']        
;                                                                                 
;  mw_bfunct_call ::= `@' IDENTIFIER`?' [`(' [expression {`,' expression}* ] `)'] 
;                                                                                 
;  x_bfunct_call  ::= `!XC' IDENTIFIER`?' `(' [expression {`,' expression}*] `)'  
;                                                                                 
;-------------------------------------------------------------------------------  
;-------------------------------------------------------------------------------  
(define (@yy_condition)
 (let ((//L (list (@yy_b_term)))
       (//C '()))
  (while (= /token1 //S_/O/R) 
   (begin
    (@yy_lex)
    (set! //L (cons (@yy_b_term) //L))))
  (cond
   ((= (gen-length //L) 1)
    (set! //C (car //L)))
   (#t
    (set! //C (@Make //T_/Or '() (reverse //L)))))
  //C))

(define (@yy_b_term)
 (let ((//L (list (@yy_b_factor)))
       (//C '()))
  (while (= /token1 //S_/A/N/D) 
   (begin
    (@yy_lex)
    (set! //L (cons (@yy_b_factor) //L))))
  (cond
   ((= (gen-length //L) 1)
    (set! //C (car //L)))
   (#t
    (set! //C (@Make //T_/And '() (reverse //L)))))
  //C))

(define (@yy_b_factor)
 (let ((//C '()))
  (cond
   ((= /token1 //S_/N/O/T)
    (@yy_lex)
    (set! //C (@Make //T_/Not '() (list (@yy_b_factor)))))
   (#t
    (set! //C (@yy_b_atom))))
  //C))

(define (@yy_b_atom)
 (let ((//C '())
       (//Temp '())
       (/op 0))
  (cond
   ((or (= /token1 //S_/T/R/U/E) (= /token1 //S_/F/A/L/S/E) (= /token1 //S_/C/O/N/D_/P/L/A/C/E))
    (set! /op (gethash //Type_/Table /token1))
    (set! //C (@Make /op '() '()))
    (@yy_lex))
   ((= (wsl-ref //Prefix_/Conds /token1) 1)
    (set! //C (@yy_prefix_cond)))
   ((= /token1 //S_/P/L/I/N/K_/X/C)
    (set! //C (@yy_x_bfunct_call)))
   ((= (wsl-ref //Patterns /token1) 1)
    ; Could be a cond pattern or expn pattern 
    (set! //C (@yy_cond_pattern)))
   ((= /token1 //S_/A/T)
    ; Could be a MW_Funct or MW_BFunct call: 
    ; call the SR parser to disambiguate.    
    (set! //C (@yy_SR_parse_cond)))
   ((not (= /token1 //S_/L/P/A/R/E/N))
    (cond
     ((and (= /token1 //S_/I/D/E/N/T/I/F/I/E/R) (equal? (car (@yy_look)) //S_/Q/U/E/R/Y))
      (set! //C (@yy_bfunct_call)))
     (#t
      ; Must be a relation
      (set! //Temp (@yy_expression))
      (cond
       ((= (wsl-ref //Rel_/Ops /token1) 0)
        (@Syntax_Error "Missing relational operator")
        (set! //C (@Make //T_/Cond_/Place '() '())))
       (#t
        (set! /op (gethash //Type_/Table /token1))
        (@yy_lex)
        (set! //C (@Make /op '() (list //Temp (@yy_expression)))))))))
   (#t
    ; Got a bracket: could contain an expression OR condition
    (set! //C (@yy_SR_parse_cond))))
  //C))

;-------------------------------------------------------------------------------
;  Functions for conditional prefix operators                                   
;-------------------------------------------------------------------------------
(define (@yy_prefix_cond)
 (let ((//S '()))
  (cond
   ((or (= /token1 //S_/E/V/E/N) (= /token1 //S_/O/D/D) (= /token1 //S_/E/M/P/T/Y) (= /token1 //S_/S/T/R/I/N/G/B/F) (= /token1 //S_/N/U/M/B/E/R/Q) (= /token1 //S_/S/E/Q/U/E/N/C/E))
    (set! //S (@yy_pc_unary)))
   ((or (= /token1 //S_/S/U/B/S/E/T) (= /token1 //S_/M/E/M/B/E/R))
    (set! //S (@yy_pc_binary)))
   (#t
    (set! //S (@yy_pc_forall))))
  //S))

(define (@yy_pc_unary)
 (let ((/type (gethash //Type_/Table /token1))
       (/arg '())
       (//S '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/Q/U/E/R/Y "Missing `?' after conditional function name")
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument")
  (cond
   ((= /token1 //S_/R/P/A/R/E/N)
    (@Syntax_Error "Missing argument")
    (set! //S (@Make //T_/Cond_/Place '() '())))
   (#t
    (cond
     ((= /type //T_/Empty)
      (set! /arg (@yy_s_expression)))
     (#t
      (set! /arg (@yy_expression))))
    (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
    (set! //S (@Make /type '() (list /arg)))))
  //S))

(define (@yy_pc_binary)
 (let ((/type (gethash //Type_/Table /token1))
       (/args '())
       (//S '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/Q/U/E/R/Y "Missing `?' after conditional function name")
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument")
  (cond
   ((= /token1 //S_/R/P/A/R/E/N)
    (@Syntax_Error "Missing arguments")
    (set! //S (@Make //T_/Cond_/Place '() '())))
   (#t
    (set! /args (list (@yy_expression)))
    (@yy_skip_symbol //S_/C/O/M/M/A "Missing `,' or 2nd argument")
    (cond
     ((= /token1 //S_/R/P/A/R/E/N)
      (@Syntax_Error "Missing 2nd argument")
      (set! //S (@Make //T_/Cond_/Place '() '())))
     (#t
      (set! /args (cons (@yy_s_expression) /args))
      (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
      (set! //S (@Make /type '() (reverse /args)))))))
  //S))

(define (@yy_pc_forall)
 (let ((/type (gethash //Type_/Table /token1))
       (/vars '())
       (/cond '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/L/A/N/G/L/E (string-append "Missing `<' in " (vector-ref //Syntax_/Name (- /type 1))))
  (set! /vars (@yy_lvalues))
  (@yy_skip_symbol //S_/R/A/N/G/L/E (string-append "Missing `>' in " (vector-ref //Syntax_/Name (- /type 1))))
  (@yy_skip_symbol //S_/C/O/L/O/N (string-append "Missing `:' in " (vector-ref //Syntax_/Name (- /type 1))))
  (set! /cond (@yy_condition))
  (@yy_skip_symbol //S_/E/N/D (string-append "Missing `END' in " (vector-ref //Syntax_/Name (- /type 1))))
  (@Make /type '() (list /vars /cond))))

;-------------------------------------------------------------------------------
;  BFuncts of various types                                                     
;-------------------------------------------------------------------------------
(define (@yy_bfunct_call)
 (let ((/name '())
       (/args '()))
  (set! /name (@yy_name "BFUNCT"))
  (cond
   ((= /token1 //S_/Q/U/E/R/Y)
    (cond
     ((= (@ST /name) //T_/Name)
      (set! /name (@Name (@Make_Name (string-append (@N_String (@V /name)) "?"))))))
    (@yy_lex)))
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `('")
  (set! /args (@yy_expressions))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)' ")
  (@Make //T_/B/Funct_/Call '() (list /name /args))))

(define (@yy_mw_bfunct_call)
 (let ((/name '())
       (/args '()))
  (set! /name (@Make_Name (string-append (string-append "@" (@String /token2)) "?")))
  (@yy_lex)
  (@yy_skip_symbol //S_/Q/U/E/R/Y "Missing `?' in bfunct call")
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (set! /args (@yy_expressions))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)' ")
  (@Make //T_/M/W_/B/Funct_/Call '() (list (@Make //T_/Name /name '()) /args))))

(define (@yy_x_bfunct_call)
 (let ((/name '())
       (/args '())
       (//S '()))
  (@yy_lex)
  (set! /name (@yy_name "!XC call"))
  (cond
   ((= /token1 //S_/Q/U/E/R/Y)
    (cond
     ((= (@ST /name) //T_/Name)
      (set! /name (@Name (@Make_Name (string-append (@N_String (@V /name)) "?"))))))
    (@yy_lex)))
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (set! /args (@yy_expressions))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)' ")
  (@Make //T_/X_/B/Funct_/Call '() (list /name /args))))

;-------------------------------------------------------------------------------
;  Condition which begins with a pattern of some sort                           
;  (could be cond pattern or expn pattern)                                      
;-------------------------------------------------------------------------------
(define (@yy_cond_pattern)
 (let ((/type /token1)
       (/name '())
       (//S '())
       (/comps '())
       (/cond '()))
  (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
   (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
  ; Look at what follows the pattern to decide if it's a condition 
  ; pattern or expression pattern. In this context, an expression  
  ; pattern must be followed by an arithmetic, sequence  or        
  ; relational operator. If anything else, we assume it's a        
  ; condition pattern.                                             
  (cond
   ((or (= /token1 //S_/E/X/P/O/N/E/N/T) (= (wsl-ref //Term_/Ops /token1) 1) (= (wsl-ref //Factor_/Ops /token1) 1) (= (wsl-ref //S_/Term_/Ops /token1) 1) (= (wsl-ref //S_/Factor_/Ops /token1) 1) (= (wsl-ref //S_/Atom_/Ops /token1) 1) (= (wsl-ref //Rel_/Ops /token1) 1))
    ; It's an expression pattern. 
    (set! /type (gethash /pattern_type (list //T_/Expression /type)))
    (set! //S (@yy_SR_cond_engine (list (@Make /type /name /comps)))))
   (#t
    ; Assume it's a condition pattern 
    (set! /type (gethash /pattern_type (list //T_/Condition /type)))
    (set! //S (@Make /type /name /comps))))
  //S))

; Pass the given condition item on the stack to the SR parser 
; then check for subsequent AND and OR tokens to parse the remainder 
; of the condition (if any) 
(define (@yy_rest_of_cond //S)
 (set! //S (@yy_SR_cond_engine (list //S)))
 (while (= /token1 //S_/A/N/D) 
  (begin
   (@yy_lex)
   (set! //S (@Make //T_/And '() (list //S (@yy_b_factor))))))
 (while (= /token1 //S_/O/R) 
  (begin
   (@yy_lex)
   (set! //S (@Make //T_/Or '() (list //S (@yy_b_term))))))
 //S)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;    Shift-reduce parser for condition-atoms                                    
;                                                                               
;    This deals with 2 ambiguous cases which the top-down LA parser             
;    can't cope with: opening bracket, which could contain either a             
;                     condition or an expression                                
;                and: `@IDENTIFIER', which could be a MW_Funct call             
;                      (expression) or MW_BFunct call (condition).              
;                                                                               
;    !!!!!!! Warning: monolith ahead !!!!!!                                     
;                                                                               
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(define (@yy_SR_parse_cond)
 (let ((//Temp '())
       (/stack '())
       (/op 0))
  ; Tell the viewers what's happening
  ; Eat up any leading `('s 
  (while (= /token1 //S_/L/P/A/R/E/N) 
   (begin
    (set! /stack (cons /token1 /stack))
    (@yy_lex)))
  ; The first term may be a negation, a const. condition, 
  ; a bfunct or mw_bfunct, a prefix operator              
  ; or an expression: the parentheses may contain only    
  ; the first term, or more ...                           
  (cond
   ((= /token1 //S_/N/O/T)
    (@yy_lex)
    (set! //Temp (@Make //T_/Not '() (list (@yy_b_factor)))))
   ((or (= /token1 //S_/T/R/U/E) (= /token1 //S_/F/A/L/S/E) (= /token1 //S_/C/O/N/D_/P/L/A/C/E))
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //Temp (@Make /op '() '())))
   ((= /token1 //S_/A/T)
    ; More complications! Is this a MW_BFunct call (a condition) 
    ; or a MW_Funct call ((part of) an expression) ?             
    (set! //Temp (@yy_mw_call)))
   ((= /token1 //S_/P/L/I/N/K_/X/C)
    (set! //Temp (@yy_x_bfunct_call)))
   ((= (wsl-ref //Prefix_/Conds /token1) 1)
    (set! //Temp (@yy_prefix_cond)))
   ((= (wsl-ref //Patterns /token1) 1)
    (set! //Temp (@yy_cond_pattern)))
   ((and (= /token1 //S_/I/D/E/N/T/I/F/I/E/R) (equal? (car (@yy_look)) //S_/Q/U/E/R/Y))
    (set! //Temp (@yy_bfunct_call)))
   (#t
    (set! //Temp (@yy_expression))))
  (set! /stack (cons //Temp /stack))
  (@yy_SR_cond_engine /stack)))

; The main shift-reduce engine - separated out 
; so that it can be called independently.      
(define (@yy_SR_cond_engine /stack)
 (let ((//C '())
       (//Temp '())
       (/op '()))
  (cond
   ((null? /stack)
    (set! /stack (list '()))))
  ; MPW hack!!! 
  (set! //Temp (car /stack))
  (set! /fl_flag2 0)
  (while (= /fl_flag2 0) 
   (begin
    ; First deal with any closing `)'s 
    (set! /fl_flag1 0)
    (while (= /fl_flag1 0) 
     (cond
      ((not (= /token1 //S_/R/P/A/R/E/N))
       (set! /fl_flag1 1))
      (#t
       (cond
        ((null? /stack)
         (set! /stack (list '()))))
       ; MPW hack!!! 
       (set! //Temp (car /stack))
       (set! /stack (cdr /stack))
       (cond
        ((null? /stack)
         (set! /stack (list '()))))
       ; MPW hack!!! 
       (cond
        ((not (equal? (car /stack) //S_/L/P/A/R/E/N))
         ; Unmatched ``)'' - must be returned to caller
         (set! /stack (cons //Temp /stack))
         (set! /fl_flag1 2))
        (#t
         (set! /stack (cons //Temp (cdr /stack)))
         (@yy_lex)
         (set! /fl_flag1 0))))))
    (cond
     ((= /fl_flag1 2)
      (set! /fl_flag2 1))
     (#t
      (cond
       ((null? /stack)
        (set! /stack (list '()))))
      ; MPW hack!!! 
      (cond
       ((= (@Gen_Type (car /stack)) //T_/Expression)
        ; An expression on the stack may be part of a larger expression
        ; (as the result of bracketing), or of a relation.             
        (cond
         ((and (= (wsl-ref //Rel_/Ops /token1) 0) (= (wsl-ref //Term_/Ops /token1) 0) (= (wsl-ref //Factor_/Ops /token1) 0) (= (wsl-ref //S_/Term_/Ops /token1) 0) (not (= /token1 //S_/E/X/P/O/N/E/N/T)) (= (wsl-ref //S_/Factor_/Ops /token1) 0))
          (@Syntax_Error "Missing operator")
          (set! /fl_flag2 1))
         (#t
          ; Reduce using the operator and following term 
          (set! //Temp (car /stack))
          (set! /stack (cdr /stack))
          (set! /op (gethash //Type_/Table /token1))
          (@yy_lex)
          ; Precedence rules for operators 
          (cond
           ((or (= /op //T_/Times) (= /op //T_/Divide))
            (set! //C (@Make /op '() (list //Temp (@yy_factor)))))
           ((or (= /op //T_/Plus) (= /op //T_/Minus))
            (set! //C (@Make /op '() (list //Temp (@yy_term)))))
           ((= /op //T_/Exponent)
            (set! //C (@Make /op '() (list //Temp (@yy_factor)))))
           ((or (= /op //T_/Set_/Diff))
            (set! //C (@Make /op '() (list //Temp (@yy_s_term)))))
           ((or (= /op //T_/Concat) (= /op //T_/Union))
            (set! //C (@Make /op '() (list //Temp (@yy_s_factor)))))
           ((or (= /op //T_/Intersection))
            (set! //C (@Make /op '() (list //Temp (@yy_s_atom)))))
           (#t
            ; Must be a relation
            (set! //C (@Make /op '() (list //Temp (@yy_expression))))))
          (set! /stack (cons //C /stack))
          (set! /fl_flag2 0))))
       ((= (@Gen_Type (car /stack)) //T_/Condition)
        ; As soon as we've built an atomic condition we're done,     
        ; unless we are still inside brackets. If we are, the length 
        ; of the stack will be >1 (condition + a number of `('s   ). 
        (cond
         ((= (gen-length /stack) 1)
          (set! /fl_flag2 1))
         ((= /token1 //S_/R/P/A/R/E/N)
          ; Gets dealt with on next iteration
          (set! /fl_flag2 0))
         ((= (wsl-ref //Bool_/Ops /token1) 1)
          ; Reduce using this operator, using 
          ; precedence rules for boolean op's 
          (set! //Temp (list (car /stack)))
          (set! /stack (cdr /stack))
          (while (= /token1 //S_/A/N/D) 
           (begin
            (@yy_lex)
            (set! //Temp (cons (@yy_b_factor) //Temp))))
          (cond
           ((not (= (gen-length //Temp) 1))
            (set! //Temp (list (@Make //T_/And '() (reverse //Temp))))))
          (while (= /token1 //S_/O/R) 
           (begin
            (@yy_lex)
            (set! //Temp (cons (@yy_b_term) //Temp))))
          (cond
           ((= (gen-length //Temp) 1)
            (set! //C (car //Temp)))
           (#t
            (set! //C (@Make //T_/Or '() (reverse //Temp)))))
          (set! /stack (cons //C /stack))
          (set! /fl_flag2 0))
         (#t
          (@Syntax_Error "Malformed condition -  maybe missing ``)''?")
          (set! /fl_flag2 1))))
       (#t
        (set! /fl_flag2 0)))))))
  (cond
   ((null? /stack)
    (set! /stack (list '()))))
  ; MPW hack!!! 
  (set! //C (car /stack))
  (cond
   ((not (= (@Gen_Type //C) //T_/Condition))
    (@Syntax_Error "Malformed condition")
    (set! //C (@Make //T_/Cond_/Place '() '()))))
  //C))

; This function parses an MW_Funct or MW_BFunct call and returns 
; the resulting node. Called by the condition SR parser only 
(define (@yy_mw_call)
 (let ((/type '())
       (/name '())
       (/args '()))
  (set! /name (string-append "@" /token2))
  (@yy_lex)
  (cond
   ((= /token1 //S_/Q/U/E/R/Y)
    (set! /type //T_/M/W_/B/Funct_/Call)
    (set! /name (string-append /name "?"))
    (@yy_lex))
   (#t
    (set! /type //T_/M/W_/Funct_/Call)))
  (set! /name (@Make_Name /name))
  (cond
   ((= /token1 //S_/L/P/A/R/E/N)
    (@yy_lex)
    (cond
     ((not (= /token1 //S_/R/P/A/R/E/N))
      (set! /args (@yy_expressions)))
     (#t
      (set! /args (@Make //T_/Expressions '() '()))))
    (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'"))
   (#t
    (set! /args (@Make //T_/Expressions '() '()))))
  (@Make /type '() (list (@Make //T_/Name /name '()) /args))))

;-------------------------------------------------------------------------------
;   End of condition parser                                                     
;-------------------------------------------------------------------------------

