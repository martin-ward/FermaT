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
;                  Expression parsing functions.                                
;-------------------------------------------------------------------------------
;                                                                               
; Grammar for expressions is as follows:-                                       
;                                                                               
;  Arithmetic expressions:                                                      
;                                                                               
;  expression   ::=  a_expression | s_expression                                
;                                                                               
;  a_expression ::=  term [term_op term]*                                       
;                                                                               
;  term         ::=  factor [factor_op factor]*                                 
;                                                                               
;  factor       ::=  true_factor  |  `-' factor                                 
;                                                                               
;  true_factor  ::=  exp_atom [ `**' factor ]*                                  
;                                                                               
;  exp_atom     ::=  NUMBER | `(' a_expression `)' |                            
;                    | a_prefix_op | gen_exp_atom                               
;                                                                               
;  a_prefix_op  ::=  `ABS' `(' a_expression `)' | `FRAC' `(' a_expression `)' | 
;                    `INT' `(' a_expression `)' | `SGN' `(' a_expression `)' |  
;                    `MAX' `(' a_expression {a_expression}* `)' |               
;                    `MIN' `(' a_expression {a_expression}* `)' |               
;                    `LENGTH' `(' s_expression `)'                              
;                                                                               
;  term_op      ::= `+' | `-'                                                   
;  factor_op    ::= `*' | `/' | `MOD' | `DIV'                                   
;                                                                               
;    ----------------------------------------------------------------------     
;                                                                               
; Generic expression atoms: (indeterminate type)                                
;                                                                               
;  gen_exp_atom ::=  IDENTIFIER | `$Expn$' | `$Var$' | array_ref |              
;                    g_prefix_op | exp_pattern | fill | funct_call |            
;                    gethash                                                    
;                                                                               
;  array_ref    ::=  s_expression `[' a_expression `]' |                        
;                    s_expression `[' a_expression `..' a_expression `]' |      
;                    s_expression `[' a_expression `..' `]' |                   
;                    s_expression `[' a_expression `,' a_expression `]'         
;                                                                               
;  exp_pattern  ::=  `~?' IDENTIFIER | `~+' IDENTIFIER | `~*' IDENTIFIER        
;                                                                               
;  funct_call   ::=  IDENTIFIER `(' [expression] {`,' expression}* `)' |        
;                    `@' IDENTIFIER `(' [expression] {`,' expression}* `)' |    
;                    `!XF' IDENTIFIER `(' [expression] {`,' expression}* `)'    
;                                                                               
;  get          ::=  gen_exp_atom `^' a_expression |                            
;                    gen_exp_atom `^^' s_expression                             
;                                                                               
;  g_prefix_op  ::= `REDUCE' `(' IDENTIFIER `,' s_expression `)' |              
;                   `HEAD' `(' s_expression `)' |                               
;                   `LAST' `(' s_expression `)' |                               
;                                                                               
;  gethash      ::=  gen_exp_atom`.'`(' expression `)'                          
;                                                                               
;    ----------------------------------------------------------------------     
;                                                                               
; Set/string/sequence expressions:                                              
;                                                                               
;  s_expression ::=  s_term [s_term_op s_term]*                                 
;                                                                               
;  s_term       ::=  s_factor [s_factor_op s_factor]*                           
;                                                                               
;  s_factor     ::=  s_atom [ `/' s_atom]*                                     
;                                                                               
;  s_atom       ::=  IDENTIFIER | sequence | string | set | numb_type |         
;                   `(' s_expression `)' | s_prefix_op | gen_exp_atom           
;                                                                               
;  sequence     ::= `<' expression [`,' expression]* `>'                        
;  set          ::= `{' expression `|' condition `}'                            
;  numb_type    ::= `%N' | `%Z' | `%Q' | `%R'                                   
;  s_prefix_op  ::= `MAP' `(' IDENTIFIER `,' s_expression `)' |                 
;                   `POWERSET' `(' s_expression `)' |                           
;                   `TAIL' `( s_expression `)' |                                
;                   `BUTLAST' `(' s_expression `)' |                            
;                   `SLENGTH' `(' s_expression `)' |                            
;                   `SUBSTR' `(' s_expressions `)' |                            
;                   `INDEX' `(' s_expressions `)'                               
;                                                                               
;  s_term_op    ::= `++' | `/'                                                 
;  s_factor_op  ::= `'                                                         
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;  Parsing functions begin here                                                 
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(define (@yy_expression)
 (let ((//S-save //S)
       (/op 0)
       (/seq 0)
       (funct-result '()))
  (set! //S '())
  ; We attempt to do some rudimentary type checking here.        
  ; We can't necessarily tell from the first token whether this  
  ; is an a_expression or an s_expression. If the first token is 
  ; number, a minus, or an arithmetic operator, it must be an    
  ; arithmetic expression. If it's a `<', a `{', a string or a   
  ; string/sequence operator, it must be an s_expression.        
  ; If it's a `(' we can just recurse, otherwise, it is safe to  
  ; look for an exp_atom;  if we get back an identifier, we must 
  ; look at the following operator to decide; otherwise, it must 
  ; be an arithmetic expression.                                 
  ; This whole nasty monolith is to deal with the type      
  ; ambiguity in the first token !                          
  (cond
   ((or (= /token1 //S_/L/A/N/G/L/E) (= /token1 //S_/L/B/R/A/C/E) (= (wsl-ref /s_prefix_ops /token1) 1))
    (set! //S (@yy_s_expression)))
   ((or (= /token1 //S_/N/U/M/B/E/R) (= /token1 //S_/M/I/N/U/S) (= (wsl-ref /a_prefix_ops /token1) 1))
    (set! //S (@yy_a_expression)))
   ((= /token1 //S_/F/I/L/L)
    (set! //S (@yy_fill)))
   (#t
    (cond
     ((= /token1 //S_/L/P/A/R/E/N)
      (@yy_lex)
      (set! //S (@yy_expression))
      (cond
       ((not (= /token1 //S_/R/P/A/R/E/N))
        (@Syntax_Error "Missing `)'"))
       (#t
        (@yy_lex))))
     (#t
      (set! //S (@yy_factor))))
    ; Now look and see what we've got, and what operator follows
    (cond
     ((= (wsl-ref /g_exp_types (@Spec_Type //S)) 1)
      ; Could be a sequence or a scalar
      ; Look at following operator, if any
      (cond
       ((or (= /token1 //S_/E/O/F) (= /token1 //S_/S/E/M/I/C/O/L/O/N))
        #t)
       ((or (= (wsl-ref //Factor_/Ops /token1) 1) (= (wsl-ref //Term_/Ops /token1) 1))
        (set! //S (@yy_build_a_expr //S)))
       ((or (= (wsl-ref //S_/Factor_/Ops /token1) 1) (= (wsl-ref //S_/Term_/Ops /token1) 1) (= (wsl-ref //S_/Atom_/Ops /token1) 1))
        (set! //S (@yy_build_s_expr //S)))))
     ((= (wsl-ref //Math_/Exps (@Spec_Type //S)) 1)
      (cond
       ((or (= /token1 //S_/E/O/F) (= /token1 //S_/S/E/M/I/C/O/L/O/N))
        #t)
       (#t
        (set! //S (@yy_build_a_expr //S)))))
     ((= (wsl-ref //Seq_/Exps (@Spec_Type //S)) 1)
      (cond
       ((or (= /token1 //S_/E/O/F) (= /token1 //S_/S/E/M/I/C/O/L/O/N))
        #t)
       (#t
        (set! //S (@yy_build_s_expr //S)))))
     (#t
      ; Now check for `^' or `^^' 
      (while (= /token1 //S_/C/A/R/E/T) 
       (begin
        (@yy_lex)
        (cond
         ((= /token1 //S_/C/A/R/E/T)
          (@yy_lex)
          (set! //S (@Make //T_/Get '() (list //S (@yy_s_expression)))))
         (#t
          (set! //S (@Make_Get_n //S (@yy_a_expression)))))))))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

; Make an item, checking for left-associative operations: 
; Took T_Concat out of L_Assoc_Types to work around a bug in FME! 
(set! //L_/Assoc_/Types (list //T_/Plus //T_/Times //T_/Union //T_/Intersection //T_/And //T_/Or //T_/Max //T_/Min //T_/Minus //T_/Divide))
(define (@Make_Assoc /op //S //I)
 
 (if (member /op //L_/Assoc_/Types) (if (= (@ST //S) /op) (@Make /op '() (concat (@Cs //S) (list //I))) (@Make /op '() (list //S //I))) (@Make /op '() (list //S //I))))

(define (@yy_build_a_expr //S-par)
 (let ((//S-save //S)
       (/op '())
       (funct-result '()))
  (set! //S //S-par)
  (while (= (wsl-ref //Factor_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make_Assoc /op //S (@yy_factor)))))
  (while (= (wsl-ref //Term_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make_Assoc /op //S (@yy_term)))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_build_s_expr //S-par)
 (let ((//S-save //S)
       (/op '())
       (funct-result '()))
  (set! //S //S-par)
  (while (= (wsl-ref //S_/Atom_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make_Assoc /op //S (@yy_s_atom)))))
  (while (= (wsl-ref //S_/Factor_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make_Assoc /op //S (@yy_s_factor)))))
  (while (= (wsl-ref //S_/Term_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make /op '() (list //S (@yy_s_term))))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

; Note how nicely structured the rest is :-) 
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;  Arithmetic expressions                                                       
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(define (@yy_a_expression)
 (let ((//S-save //S)
       (/op '())
       (funct-result '()))
  (set! //S '())
  (set! //S (@yy_term))
  (while (= (wsl-ref //Term_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make_Assoc /op //S (@yy_term)))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_term)
 (let ((//S-save //S)
       (/op '())
       (funct-result '()))
  (set! //S (@yy_factor))
  (while (= (wsl-ref //Factor_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make_Assoc /op //S (@yy_factor)))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_factor)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S '())
  (cond
   ((= /token1 //S_/M/I/N/U/S)
    (@yy_lex)
    (set! //S (@Make //T_/Negate '() (list (@yy_factor)))))
   ((= /token1 //S_/P/L/U/S)
    (@yy_lex)
    (set! //S (@yy_factor)))
   (#t
    (set! //S (@yy_true_factor))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_true_factor)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S (@yy_exp_atom))
  (while (= /token1 //S_/E/X/P/O/N/E/N/T) 
   (begin
    (@yy_lex)
    (set! //S (@Make //T_/Exponent '() (list //S (@yy_factor))))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_exp_atom)
 (let ((//S-save //S)
       (/temp '())
       (funct-result '()))
  (set! //S '())
  (cond
   ((= /token1 //S_/N/U/M/B/E/R)
    (set! //S (@Make //T_/Number /token2 '()))
    (@yy_lex))
   ((= (wsl-ref /a_prefix_ops /token1) 1)
    (set! //S (@yy_a_prefix_op)))
   ((= /token1 //S_/M/I/N/U/S)
    (@yy_lex)
    (set! //S (@Make //T_/Negate '() (list (@yy_exp_atom)))))
   ((= /token1 //S_/L/P/A/R/E/N)
    (@yy_lex)
    (set! //S (@yy_a_expression))
    (cond
     ((not (= /token1 //S_/R/P/A/R/E/N))
      (@Syntax_Error "Missing ``)''"))
     (#t
      (@yy_lex))))
   (#t
    (set! //S (@yy_gen_exp_atom))))
  (set! //S (@yy_checkfor_aref //S))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

;-------------------------------------------------------------------------------
(define (@yy_a_prefix_op)
 (let ((/type (gethash //Type_/Table /token1))
       (/args '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument")
  (cond
   ((= /type //T_/Length)
    (set! /args (list (@yy_s_expression))))
   ((or (= /type //T_/Abs) (= /type //T_/Int) (= /type //T_/Frac) (= /type //T_/Sgn) (= /type //T_/Slength) (= /type //T_/Address_/Of))
    (set! /args (list (@yy_a_expression))))
   ((= /type //T_/Index)
    (set! /args (list (@yy_expressions))))
   (#t
    (set! /args (@Cs (@yy_expressions)))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (@Make /type '() /args)))

; The following should replace the previous function if/when the syntax 
; of MIN, MAX are changed to take a single list argument as parameter.  
(define (@new_yy_a_prefix_op)
 (let ((/type (gethash //Type_/Table /token1))
       (/arg '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument")
  (cond
   ((or (= /type //T_/Abs) (= /type //T_/Int) (= /type //T_/Frac))
    (set! /arg (@yy_a_expression)))
   (#t
    (set! /arg (@yy_s_expression))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (@Make /type '() (list /arg))))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;  Generic expression atoms (arithmetic or set/list valued)                     
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(define (@yy_gen_exp_atom)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S '())
  (cond
   ((= /token1 //S_/S/T/R/I/N/G)
    (set! //S (@Make //T_/String /token2 '()))
    (@yy_lex))
   ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
    (cond
     ((equal? (car (@yy_look)) //S_/L/P/A/R/E/N)
      (set! //S (@yy_funct_call)))
     (#t
      (set! //S (@Make //T_/Variable (@Make_Name /token2) '()))
      (@yy_lex))))
   ((= /token1 //S_/H/A/S/H_/T/A/B/L/E)
    (set! //S (@Make //T_/Hash_/Table '() '()))
    (@yy_lex))
   ((= /token1 //S_/M/E/M)
    (set! //S (@yy_mem_ref)))
   ((= /token1 //S_/A/T)
    (set! //S (@yy_mw_funct_call)))
   ((= /token1 //S_/P/L/I/N/K_/X/F)
    (set! //S (@yy_x_funct_call)))
   ((= /token1 //S_/I/F)
    (set! //S (@yy_if_expression)))
   ((or (= /token1 //S_/E/X/P/N_/P/L/A/C/E) (= /token1 //S_/V/A/R_/P/L/A/C/E))
    (set! //S (@Make (gethash //Type_/Table /token1) '() '()))
    (@yy_lex))
   ((= (wsl-ref /g_prefix_ops /token1) 1)
    (set! //S (@yy_gen_prefix_op)))
   ((= (wsl-ref /numb_types /token1) 1)
    (set! //S (@yy_trivial)))
   ((= (wsl-ref //Patterns /token1) 1)
    (set! //S (@yy_exp_pattern)))
   (#t
    (@Syntax_Error "Missing expression or operator type mismatch")
    (set! //S (@Make //T_/Expn_/Place '() '()))))
  (set! //S (@yy_checkfor_aref //S))
  ; Now check for `^' or `^^' 
  (while (= /token1 //S_/C/A/R/E/T) 
   (begin
    (@yy_lex)
    (cond
     ((= /token1 //S_/C/A/R/E/T)
      (@yy_lex)
      (set! //S (@Make //T_/Get '() (list //S (@yy_s_expression)))))
     (#t
      (set! //S (@Make_Get_n //S (@yy_a_expression)))))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

; Check for nested Get_n, eg @I^(i^1) and fix it recursively: 
(define (@Make_Get_n //I /exp)
 
 (if (= (@ST /exp) //T_/Get_n) (@Make //T_/Get_n '() (list (@Make_Get_n //I (@Get_n /exp 1)) (@Get_n /exp 2))) (@Make //T_/Get_n '() (list //I /exp))))

; Function to test for and build an array reference, structure   
; reference, or gethash. This is called after an expression atom 
; has been built.                                                
(define (@yy_checkfor_aref /e)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S /e)
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((= /token1 //S_/F/U/L/L/S/T/O/P)
     (cond
      ((not (or (equal? (car (@yy_look)) //S_/I/D/E/N/T/I/F/I/E/R) (equal? (car (@yy_look)) //S_/L/P/A/R/E/N)))
       (set! /fl_flag1 1))
      (#t
       (set! //S (@yy_struct_ref //S))
       (set! /fl_flag1 0))))
    ((= /token1 //S_/L/B/R/A/C/K/E/T)
     (set! //S (@yy_array_ref //S))
     (set! /fl_flag1 0))
    ((= /token1 //S_/P/R/I/M/E)
     (set! //S (@Make //T_/Primed_/Var (@V //S) '()))
     (@yy_lex)
     (set! /fl_flag1 1))
    (#t
     (set! /fl_flag1 1))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_struct_ref /e)
 (let ((/temp '())
       (/name '())
       (//S-save //S)
       (funct-result '()))
  (set! //S '())
  (@yy_lex)
  (cond
   ((= /token1 //S_/L/P/A/R/E/N)
    ; Gethash
    (@yy_lex)
    (set! /temp (@yy_expression))
    (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)' in `Gethash'")
    (set! //S (@Make //T_/Gethash '() (list /e /temp))))
   (#t
    ; Structure reference 
    (set! /name (@Make_Name /token2))
    (@yy_lex)
    (set! //S (@Make //T_/Struct '() (list (@Make //T_/Name /name '()) /e)))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_array_ref /e)
 (let ((/exps '())
       (//S-save //S)
       (/type '())
       (funct-result '()))
  (set! //S '())
  (@yy_lex)
  (set! /exps (list (@yy_expression)))
  (cond
   ((= /token1 //S_/R/B/R/A/C/K/E/T)
    (set! /type //T_/Aref)
    ; Remove the next line to change the abstract syntax of
    ; Aref to be <Expression, Expression>
    (set! /exps (list (@Make //T_/Expressions '() /exps))))
   ((= /token1 //S_/C/O/M/M/A)
    (set! /type //T_/Rel_/Seg)
    (@yy_lex)
    (set! /exps (concat /exps (list (@yy_expression)))))
   ((= /token1 //S_/D/O/T/D/O/T)
    (@yy_lex)
    (cond
     ((= /token1 //S_/R/B/R/A/C/K/E/T)
      (set! /type //T_/Final_/Seg))
     (#t
      (set! /type //T_/Sub_/Seg)
      (set! /exps (concat /exps (list (@yy_expression)))))))
   (#t
    (@Syntax_Error "Malformed array subscript")
    (set! /type //T_/Aref)
    ;Try to continue
   ))
  (@yy_skip_symbol //S_/R/B/R/A/C/K/E/T "Missing `]' in array reference")
  (set! /exps (cons /e /exps))
  (set! funct-result (@Make /type '() /exps))
  (set! //S //S-save)
  funct-result))

(define (@yy_mem_ref)
 (let ((/exps '())
       (//S-save //S)
       (/type '())
       (funct-result '()))
  (set! //S '())
  (@yy_lex)
  (set! /exps (list (@yy_expression)))
  (cond
   ((= /token1 //S_/R/B/R/A/C/K/E/T)
    (set! /type //T_/Mem))
   ((= /token1 //S_/C/O/M/M/A)
    (set! /type //T_/Mem_/Rel)
    (@yy_lex)
    (set! /exps (concat /exps (list (@yy_expression)))))
   ((= /token1 //S_/D/O/T/D/O/T)
    (@yy_lex)
    (cond
     ((= /token1 //S_/R/B/R/A/C/K/E/T)
      (@Syntax_Error "Malformed memory segment"))
     (#t
      (set! /type //T_/Mem_/Seg)
      (set! /exps (concat /exps (list (@yy_expression)))))))
   (#t
    (@Syntax_Error "Malformed array subscript")
    (set! /type //T_/Aref)
    ;Try to continue
   ))
  (@yy_skip_symbol //S_/R/B/R/A/C/K/E/T "Missing `]' in memory reference")
  (set! funct-result (@Make /type '() /exps))
  (set! //S //S-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Function Call                                                                
;-------------------------------------------------------------------------------
(define (@yy_funct_call)
 (let ((/name (@Make_Name /token2))
       (/args '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list in function call")
  (cond
   ((not (= /token1 //S_/R/P/A/R/E/N))
    (set! /args (@yy_expressions)))
   (#t
    (set! /args (@Make //T_/Expressions '() '()))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (@Make //T_/Funct_/Call '() (list (@Make //T_/Name /name '()) /args))))

;-------------------------------------------------------------------------------
;  MW_Funct. Call                                                               
;-------------------------------------------------------------------------------
(define (@yy_mw_funct_call)
 (let ((/name '())
       (/args '()))
  (set! /name (@Make_Name (string-append "@" /token2)))
  (@yy_lex)
  (cond
   ((= /token1 //S_/L/P/A/R/E/N)
    (@yy_lex)
    (cond
     ((= /token1 //S_/R/P/A/R/E/N)
      (set! /args (@Make //T_/Expressions '() '())))
     (#t
      (set! /args (@yy_expressions))))
    (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)' in MW_Funct call"))
   (#t
    (set! /args (@Make //T_/Expressions '() '()))))
  (@Make //T_/M/W_/Funct_/Call '() (list (@Make //T_/Name /name '()) /args))))

;-------------------------------------------------------------------------------
;  External Function Call                                                       
;-------------------------------------------------------------------------------
(define (@yy_x_funct_call)
 (let ((/name '())
       (/args '()))
  (@yy_lex)
  (set! /name (@yy_name "!XF call"))
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (cond
   ((not (= /token1 //S_/R/P/A/R/E/N))
    (set! /args (@yy_expressions)))
   (#t
    (set! /args (@Make //T_/Expressions '() '()))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (set! //S (@Make //T_/X_/Funct_/Call '() (list /name /args)))
  //S))

;-------------------------------------------------------------------------------
;  IF - expression                                                              
;-------------------------------------------------------------------------------
(define (@yy_if_expression)
 (let ((/cond '())
       (/then '())
       (/else '()))
  (@yy_lex)
  (set! /cond (@yy_condition))
  (@yy_skip_symbol //S_/T/H/E/N "Missing `THEN'")
  (set! /then (@yy_expression))
  (@yy_skip_symbol //S_/E/L/S/E "Missing `ELSE'")
  (set! /else (@yy_expression))
  (@yy_skip_symbol //S_/F/I "Missing `FI'")
  (@Make //T_/If '() (list /cond /then /else))))

;-------------------------------------------------------------------------------
;  Generic prefix ops (arith. or set/list valued)                               
;-------------------------------------------------------------------------------
(define (@yy_gen_prefix_op)
 (let ((/type (gethash //Type_/Table /token1))
       (/name '())
       (/args '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument(s)")
  (cond
   ((= /type //T_/Substr)
    (set! /args (list (@yy_expressions))))
   ((or (= /type //T_/Reduce) (= /type //T_/Map))
    (cond
     ((not (= /token1 //S_/S/T/R/I/N/G))
      (@Syntax_Error "Missing string argument in MAP/REDUCE")
      (set! /args (list (@Name (@Make_Name "_Missing_")))))
     (#t
      (set! /args (list (@Name (@Make_Name /token2))))
      (@yy_lex)
      (@yy_skip_symbol //S_/C/O/M/M/A "Missing `,' or 2nd argument in MAP/REDUCE")))
    (set! /args (concat /args (list (@yy_s_expression)))))
   (#t
    (set! /args (list (@yy_s_expression)))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (@Make /type '() /args)))

;-------------------------------------------------------------------------------
;  Expression patterns                                                          
;-------------------------------------------------------------------------------
; stype is one of SEVBCGADNX or empty 
; Next token is either an S_IDENTIFIER or S_LPAREN 
(define (@yy_exp_pattern)
 (let ((/type /token1)
       (/stype /token2)
       (/name '())
       (//S-save //S)
       (/comps '())
       (funct-result '()))
  (set! //S '())
  (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
   (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
  (set! //S (@Make (gethash /pattern_type (list //T_/Expression /type)) /name /comps))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;  Functions for sequences, sets and strings                                    
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(define (@yy_s_expression)
 (let ((//S-save //S)
       (/op '())
       (funct-result '()))
  (set! //S '())
  (set! //S (@yy_s_term))
  (while (= (wsl-ref //S_/Term_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make /op '() (list //S (@yy_s_term))))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_s_term)
 (let ((//S-save //S)
       (/op '())
       (funct-result '()))
  (set! //S (@yy_s_factor))
  (while (= (wsl-ref //S_/Factor_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make /op '() (list //S (@yy_s_factor))))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_s_factor)
 (let ((//S-save //S)
       (/op '())
       (funct-result '()))
  (set! //S (@yy_s_atom))
  (while (= (wsl-ref //S_/Atom_/Ops /token1) 1) 
   (begin
    (set! /op (gethash //Type_/Table /token1))
    (@yy_lex)
    (set! //S (@Make /op '() (list //S (@yy_s_atom))))))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_s_atom)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S '())
  (cond
   ((= /token1 //S_/L/B/R/A/C/E)
    (set! //S (@yy_set)))
   ((= /token1 //S_/L/P/A/R/E/N)
    (@yy_lex)
    (set! //S (@yy_s_expression))
    (cond
     ((not (= /token1 //S_/R/P/A/R/E/N))
      (@Syntax_Error "Missing `)'"))
     (#t
      (@yy_lex))))
   ((= /token1 //S_/L/A/N/G/L/E)
    (@yy_lex)
    (cond
     ((= /token1 //S_/R/A/N/G/L/E)
      (set! //S (@Make //T_/Sequence '() (list (@Make //T_/Expressions '() '()))))
      (@yy_lex))
     (#t
      (set! //S (@yy_sequence)))))
   ((= /token1 //S_/A/R/R/A/Y)
    ; Arguments for ARRAY are different to 
    ; other s_prefix_ops 
    (set! //S (@yy_array)))
   ((= (wsl-ref /s_prefix_ops /token1) 1)
    (set! //S (@yy_gen_prefix_op)))
   (#t
    (set! //S (@yy_gen_exp_atom))))
  (set! //S (@yy_checkfor_aref //S))
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

;-------------------------------------------------------------------------------
(define (@yy_set)
 (let ((/exp '())
       (/cond '()))
  (@yy_lex)
  (set! /exp (@yy_expression))
  (@yy_skip_symbol //S_/V/B/A/R "Missing `|' in SET")
  (set! /cond (@yy_condition))
  (@yy_skip_symbol //S_/R/B/R/A/C/E "Missing `}' after SET")
  (@Make //T_/Set '() (list /exp /cond))))

(define (@yy_sequence)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S (@yy_expressions))
  (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>' at end of sequence")
  (set! funct-result (@Make //T_/Sequence '() (list //S)))
  (set! //S //S-save)
  funct-result))

(define (@yy_array)
 (let ((/len '())
       (/val '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' in `ARRAY'")
  (set! /len (@yy_a_expression))
  (@yy_skip_symbol //S_/C/O/M/M/A "Missing `,' or 2nd argument in `ARRAY'")
  (set! /val (@yy_expression))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)' or malformaed expression in `ARRAY'")
  (@Make //T_/Array '() (list /len /val))))

(define (@yy_expressions)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S (list (@yy_expression)))
  (while (= /token1 //S_/C/O/M/M/A) 
   (begin
    (@yy_lex)
    (set! //S (cons (@yy_expression) //S))))
  (set! funct-result (@Make //T_/Expressions '() (reverse //S)))
  (set! //S //S-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Functions for the various FILL types                                         
;-------------------------------------------------------------------------------
(define (@yy_fill)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S '())
  (@yy_lex)
  (cond
   ((= /token1 //S_/S/T/A/T/E/M/E/N/T)
    (set! //S (@yy_fill_s //T_/Statement //T_/Fill_/Stat)))
   ((= /token1 //S_/S/T/A/T/E/M/E/N/T/S)
    (set! //S (@yy_fill_s //T_/Statements //T_/Fill_/Stats)))
   ((= /token1 //S_/E/X/P/R/E/S/S/I/O/N)
    (set! //S (@yy_fill_s //T_/Expression //T_/Fill_/Expn)))
   ((= /token1 //S_/E/X/P/R/E/S/S/I/O/N/S)
    (set! //S (@yy_fill_s //T_/Expressions //T_/Fill_/Expns)))
   ((= /token1 //S_/C/O/N/D/I/T/I/O/N)
    (set! //S (@yy_fill_s //T_/Condition //T_/Fill_/Cond)))
   ((= /token1 //S_/D/E/F/I/N/I/T/I/O/N)
    (set! //S (@yy_fill_s //T_/Definition //T_/Fill_/Defn)))
   ((= /token1 //S_/D/E/F/I/N/I/T/I/O/N/S)
    (set! //S (@yy_fill_s //T_/Definitions //T_/Fill_/Defns)))
   ((= /token1 //S_/L/V/A/L/U/E)
    (set! //S (@yy_fill_s //T_/Lvalue //T_/Fill_/Lvalue)))
   ((= /token1 //S_/L/V/A/L/U/E/S)
    (set! //S (@yy_fill_s //T_/Lvalues //T_/Fill_/Lvalues)))
   ((= /token1 //S_/A/S/S/I/G/N)
    (set! //S (@yy_fill_s //T_/Assign //T_/Fill_/Assign)))
   ((= /token1 //S_/A/S/S/I/G/N/S)
    (set! //S (@yy_fill_s //T_/Assigns //T_/Fill_/Assigns)))
   ((= /token1 //S_/G/U/A/R/D/E/D)
    (set! //S (@yy_fill_s //T_/Guarded //T_/Fill_/Guarded)))
   ((= /token1 //S_/A/C/T/I/O/N)
    (set! //S (@yy_fill_s //T_/Action //T_/Fill_/Action)))
   (#t
    (@Syntax_Error "Incorrect type specified for FILL")
    (while (not (= /token1 //S_/E/N/D/F/I/L/L)) 
     (@yy_lex))
    (set! //S (@Make //T_/Expn_/Place '() '()))))
  (@yy_skip_symbol //S_/E/N/D/F/I/L/L "Missing `ENDFILL'")
  (set! funct-result //S)
  (set! //S //S-save)
  funct-result))

(define (@yy_fill_s //G/T /result_type)
 (let ((//S-save //S)
       (funct-result '()))
  (set! //S '())
  (@yy_lex)
  (set! //S (@yy_parse //G/T))
  (set! funct-result (@Make /result_type '() (list //S)))
  (set! //S //S-save)
  funct-result))

;-------------------------------------------------------------------------------
;   End of expression parser                                                    
;-------------------------------------------------------------------------------

