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
; ********************** 
;  The Lexical Analyser  
; ********************** 
; The value returned is the pair <token_type, token_value>.   
; If the token type does not have an associated value, < > is 
; returned in token_value
; A function @yy_next() must be provided, which returns the next  
; character in the input stream on each invocation. This function 
; should strip out newlines, and return the string ``eof'' when   
; end-of-file is reached. It should also count lines in the       
; variable yy_lineno (used in error reporting).                   
;                                                                 
; Three lookup tables are used: these are provided externally     
; (by the parser):                                                
; 1. Key_Table    - a hash table resolving strings to language    
;                   keywords                                      
; 2. Single_Chars - a list of single-character terminal symbols   
; 3. Char_Table   - a hash table mapping the above single-char    
;                   terminals to the corresponding lexical token  
;-------------------------------------------------------------------------------
;  The following two functions are the interface to the lexer. @yy_lex()        
;  returns the current lexical token and calls @yy_crank() to get the next one  
;  from the input stream. @yy_look() returns the current token but does not     
;  wind on the lexer - the input stream is not read, and a subsequent call to   
;  @yy_lex() or @yy_look() will return the same token.                          
;-------------------------------------------------------------------------------
(define (@yy_lex)
 (set! /token1 /yy_sym)
 (set! /token2 /yy_value)
 (@yy_crank))

(define (@yy_lex_debug)
 (set! /token1 /yy_sym)
 (set! /token2 /yy_value)
 (set! /yy_value "")
 (@yy_crank)
 (display-list "@yy_lex symbol = " /token1 " value = " /token2 " (" /yy_sym " " /yy_value ")"))

(define (@yy_look)
 
 (list /yy_sym /yy_value))

;-------------------------------------------------------------------------------
;  The following routine gets the next complete token from the input stream.    
;  The (numerical value corresponding to the) token is left in yy_sym, and      
;  the associated value (if any) is left in yy_value.                           
;-------------------------------------------------------------------------------
(define (@yy_crank)
 ; Gobble up whitespace between tokens 
 (while (not (null? (gethash //Whitespace /yy_ch))) 
  (set! /yy_ch (@yy_next)))
 (cond
  ((not (null? (gethash //Single_/Chars /yy_ch)))
   (set! /yy_sym (gethash //Char_/Table /yy_ch))
   (set! /yy_ch (@yy_next)))
  ((equal? /yy_ch "=")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "=")
     (set! /yy_sym //S_/D/E/F/I/N/E)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/E/Q/U/A/L))))
  ((equal? /yy_ch ":")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "=")
     (set! /yy_sym //S_/B/E/C/O/M/E/S)
     (set! /yy_ch (@yy_next)))
    ((equal? /yy_ch ":")
     (set! /yy_sym //S_/C/O/L/O/N/C/O/L/O/N)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/C/O/L/O/N))))
  ((equal? /yy_ch ".")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch ".")
     (set! /yy_sym //S_/D/O/T/D/O/T)
     (set! /yy_ch (@yy_next)))
    ((or (not (null? (gethash //Whitespace /yy_ch))) (equal? /yy_ch "eof"))
     (set! /yy_sym //S_/D/O/T/S/P/A/C/E)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/F/U/L/L/S/T/O/P))))
  ((equal? /yy_ch "+")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "+")
     (set! /yy_sym //S_/C/O/N/C/A/T)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/P/L/U/S))))
  ((equal? /yy_ch "-")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch ">")
     (set! /yy_sym //S_/A/R/R/O/W)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/M/I/N/U/S))))
  ((equal? /yy_ch "*")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "*")
     (set! /yy_sym //S_/E/X/P/O/N/E/N/T)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/T/I/M/E/S))))
  ((equal? /yy_ch "[")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "]")
     (set! /yy_sym //S_/B/O/X)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/L/B/R/A/C/K/E/T))))
  ((equal? /yy_ch "<")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch ">")
     (set! /yy_sym //S_/N/E/Q)
     (set! /yy_ch (@yy_next)))
    ((equal? /yy_ch "=")
     (set! /yy_sym //S_/L/E/Q)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/L/A/N/G/L/E))))
  ((equal? /yy_ch ">")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "=")
     (set! /yy_sym //S_/G/E/Q)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/R/A/N/G/L/E))))
  ((equal? /yy_ch "!")
   (set! /yy_ch (@yy_next))
   (@yy_external_call))
  ((equal? /yy_ch "~")
   (set! /yy_ch (@yy_next))
   (@yy_pattern))
  ((equal? /yy_ch "@")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "[")
     (set! /yy_sym //S_/M/E/M)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/A/T)
     (@yy_mw_name))))
  ((equal? /yy_ch "C")
   ; 2-character peek-ahead to differentiate comment and assign
   (let ((/c (@yy_next)))
    (cond
     ((equal? /c ":")
      (cond
       ((equal? (@yy_peek) "=")
        ; C:= ... 
        (set! /yy_sym //S_/I/D/E/N/T/I/F/I/E/R)
        (set! /yy_value "C"))
       (#t
        (set! /yy_sym //S_/C/O/M/M/E/N/T)))
      (set! /yy_ch /c))
     ((not (null? (gethash //Special_/Or_/White /c)))
      (set! /yy_sym //S_/I/D/E/N/T/I/F/I/E/R)
      (set! /yy_value "C")
      (set! /yy_ch /c))
     (#t
      (set! /yy_ch (concat /yy_ch /c))
      (@yy_Ident 1)))))
  ((equal? /yy_ch "/")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch //Backslash)
     (set! /yy_sym //S_/I/N/T/E/R/S/E/C/T)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/S/L/A/S/H))))
  ((equal? /yy_ch //Backslash)
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "/")
     (set! /yy_sym //S_/U/N/I/O/N)
     (set! /yy_ch (@yy_next)))
    (#t
     (set! /yy_sym //S_/B/A/C/K/S/L/A/S/H))))
  ((equal? /yy_ch //Quote)
   (@yy_String))
  ((not (null? (gethash //Digits /yy_ch)))
   (@yy_Number))
  ((equal? /yy_ch "'")
   (set! /yy_ch (@yy_next))
   (set! /yy_sym //S_/P/R/I/M/E))
  (#t
   (@yy_Ident 1))))

;-------------------------------------------------------------------------------
; Proc to scan !? or !X? 
;-------------------------------------------------------------------------------
(define (@yy_external_call)
 (cond
  ((equal? /yy_ch "P")
   ; !P
   (set! /yy_sym //S_/P/L/I/N/K_/P))
  ((equal? /yy_ch "X")
   (set! /yy_ch (@yy_next))
   (cond
    ((equal? /yy_ch "P")
     (set! /yy_sym //S_/P/L/I/N/K_/X/P))
    ((equal? /yy_ch "F")
     (set! /yy_sym //S_/P/L/I/N/K_/X/F))
    ((equal? /yy_ch "C")
     (set! /yy_sym //S_/P/L/I/N/K_/X/C))
    (#t
     (set! /yy_sym //S_/I/N/V/A/L/I/D)
     (set! /yy_value (string-append "!X" /yy_ch)))))
  (#t
   (set! /yy_sym //S_/I/N/V/A/L/I/D)
   (set! /yy_value (string-append "!X" /yy_ch))))
 (set! /yy_ch (@yy_next)))

;-------------------------------------------------------------------------------
; Proc. to scan for ~?, ~+ or ~* 
; ~?=var, ~*=var, ~?(expn) and ~*(expn) are the new interpolation types 
;-------------------------------------------------------------------------------
(define (@yy_pattern)
 (set! /yy_value "")
 (cond
  ((equal? /yy_ch "?")
   (cond
    ((equal? (@yy_peek) "(")
     (set! /yy_sym //S_/I/N/T_/O/N/E))
    ((equal? (@yy_peek) "=")
     ; step over the = 
     (set! /yy_ch (@yy_next))
     (set! /yy_sym //S_/V/A/R_/O/N/E))
    (#t
     (set! /yy_sym //S_/P/A/T_/O/N/E))))
  ((equal? /yy_ch "+")
   (set! /yy_sym //S_/P/A/T_/M/A/N/Y))
  ((equal? /yy_ch "*")
   (cond
    ((equal? (@yy_peek) "(")
     (set! /yy_sym //S_/I/N/T_/A/N/Y))
    ((equal? (@yy_peek) "=")
     ; step over the = 
     (set! /yy_ch (@yy_next))
     (set! /yy_sym //S_/V/A/R_/A/N/Y))
    (#t
     (set! /yy_sym //S_/P/A/T_/A/N/Y))))
  (#t
   (set! /yy_sym //S_/I/N/V/A/L/I/D)
   (set! /yy_value (string-append "~" /yy_ch))))
 (set! /yy_ch (@yy_next)))

;-------------------------------------------------------------------------------
; Proc. to scan a mw_proc, mw_funct or mw_bfunct name (following a `@')         
;-------------------------------------------------------------------------------
(define (@yy_mw_name)
 (while (not (null? (gethash //Whitespace /yy_ch))) 
  (set! /yy_ch (@yy_next)))
 (cond
  ((equal? /yy_ch "~")
   ; @~?foo is an S_AT_PAT_ONE lexeme 
   (set! /yy_ch (@yy_next))
   (cond
    ((not (equal? /yy_ch "?"))
     (set! /yy_sym //S_/I/N/V/A/L/I/D))
    (#t
     (set! /yy_sym //S_/A/T_/P/A/T_/O/N/E)))))
 (@yy_Ident 0))

;-------------------------------------------------------------------------------
; Scan an identifier  
; (in fact, the default for anything not recognised as something else)
; We look up whatever we find in the hash table Key_Table afterwards, 
; to see if it is a reserved word
;-------------------------------------------------------------------------------
(define (@yy_Ident /resolve_keyword)
 (set! /yy_value /yy_ch)
 (set! /yy_ch (@yy_next))
 ; Gobble up characters until we reach a special, a space or EOF 
 (while (null? (gethash //Special_/Or_/White /yy_ch)) 
  (begin
   (set! /yy_value (concat /yy_value /yy_ch))
   (set! /yy_ch (@yy_next))))
 (cond
  ((= /resolve_keyword 1)
   ; Look up the assembled `identifier' to see if it is a reserved word
   (set! /yy_sym (gethash //Key_/Table /yy_value))
   (cond
    ((not (equal? /yy_sym '()))
     (set! /yy_value '()))
    (#t
     (set! /yy_sym //S_/I/D/E/N/T/I/F/I/E/R))))))

;-------------------------------------------------------------------------------
; Scan a number 
;-------------------------------------------------------------------------------
(define (@yy_Number)
 (let ((/div 1)
       (/str /yy_ch)
       (/exponent 0)
       (/exp_sgn 1))
  (set! /yy_ch (@yy_next))
  (while (not (null? (gethash //Digits /yy_ch))) 
   (begin
    (set! /str (concat /str /yy_ch))
    (set! /yy_ch (@yy_next))))
  (set! /yy_value (@String_To_Num /str))
  (set! /str "")
  (cond
   ((and (equal? /yy_ch ".") (not (equal? (@yy_peek) ".")))
    (set! /yy_ch (@yy_next))
    (while (not (null? (gethash //Digits /yy_ch))) 
     (begin
      (set! /str (concat /str /yy_ch))
      (set! /div (* /div 10))
      (set! /yy_ch (@yy_next))))
    (set! /yy_value (+ /yy_value (/ (@String_To_Num /str) /div)))))
  (cond
   ((equal? /yy_ch "e")
    (set! /yy_ch (@yy_next))
    (cond
     ((equal? /yy_ch "-")
      (set! /yy_ch (@yy_next))
      (set! /exp_sgn (- 1)))
     ((equal? /yy_ch "+")
      (set! /yy_ch (@yy_next))))
    (set! /str "")
    (while (not (null? (gethash //Digits /yy_ch))) 
     (begin
      (set! /str (concat /str /yy_ch))
      (set! /yy_ch (@yy_next))))
    (set! /exponent (@String_To_Num /str))
    (cond
     ((not (= /exponent 0))
      (set! /exponent (* /exponent /exp_sgn))
      (set! /yy_value (* /yy_value (integer-expt 10 /exponent)))))))
  (set! /yy_sym //S_/N/U/M/B/E/R)))

;-------------------------------------------------------------------------------
;  Scan a string                                                                
;-------------------------------------------------------------------------------
(define (@yy_String)
 (set! /yy_ch (@yy_next))
 (set! /yy_value "")
 (while (and (not (equal? /yy_ch //Quote)) (not (equal? /yy_ch "eof"))) 
  (begin
   (cond
    ((equal? /yy_ch //Backslash)
     (set! /yy_ch (@yy_next))
     ; Code here to process backslashed characters 
    ))
   (set! /yy_value (concat /yy_value /yy_ch))
   (set! /yy_ch (@yy_next))))
 (cond
  ((equal? /yy_ch "eof")
   (display-list "EOF while reading string " /yy_value))
  (#t
   (set! /yy_ch (@yy_next))))
 (set! /yy_sym //S_/S/T/R/I/N/G)
 #t)

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;   The main input routine - returns the next character from the input file.    
;   A count of lines is kept in yy_lineno. The string `eol' is returned at      
;   end-of-line. The current line is held in yy_line. The string ``eof'' is     
;   returned when end-of-file is encountered (this condition is detected by     
;   an external Lisp routine). The last input line must be terminated by a      
;   newline because of the behaviour of the underlying lisp readline function   
;-------------------------------------------------------------------------------
(define (@yy_next)
 (let ((/c (@Read_Char //Input_/Port)))
  (cond
   ((@EOF? /c)
    (set! /c "eof"))
   ((@EOL? /c)
    (set! /c //Newline)
    (set! /yy_lineno (+ /yy_lineno 1)))
   (#t
    (set! /c (@String /c))))
  /c))

(define (@yy_peek)
 (let ((/c (@Peek_Char //Input_/Port)))
  (cond
   ((@EOF? /c)
    (set! /c "eof"))
   (#t
    (set! /c (@String /c))))
  /c))

#t
