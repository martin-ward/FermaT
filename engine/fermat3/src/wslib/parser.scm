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
;  Main parser functions for statements and programs.                           
;                                                                               
;  See also: lexer.wsl (lexical analyser)                                       
;            exp_parser.wsl (parser for expressions)                            
;            cond_parser.wsl (parser for conditions)                            
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Convert a list to a hash table with the list elements as keys: 
(define (@List_To_Set //L)
 (let ((//R (hash-table))
       (/elt '()))
  (for-in /elt //L 
   (puthash //R /elt 1))
  //R))

(define (@List_To_Vector //L)
 (let ((//R (make-vector-eval 1999 0))
       (/elt 0))
  (for-in /elt //L 
   (wsl-set! //R 1 /elt))
  //R))

; Terminals - numeric values of lexical tokens
(set! //S_/B/E/C/O/M/E/S 1)
(set! //S_/E/Q/U/A/L 2)
(set! //S_/P/L/U/S 3)
(set! //S_/M/I/N/U/S 4)
(set! //S_/T/I/M/E/S 5)
(set! //S_/S/L/A/S/H 6)
(set! //S_/S/E/M/I/C/O/L/O/N 7)
(set! //S_/C/O/L/O/N 8)
(set! //S_/N/E/Q 9)
(set! //S_/L/P/A/R/E/N 10)
(set! //S_/R/P/A/R/E/N 11)
(set! //S_/C/O/M/M/A 12)
(set! //S_/L/B/R/A/C/K/E/T 13)
(set! //S_/R/B/R/A/C/K/E/T 14)
(set! //S_/L/A/N/G/L/E 15)
(set! //S_/R/A/N/G/L/E 16)
(set! //S_/L/B/R/A/C/E 17)
(set! //S_/R/B/R/A/C/E 18)
(set! //S_/Q/U/O/T/E/S 19)
(set! //S_/C/O/N/C/A/T 20)
(set! //S_/D/E/F/I/N/E 21)
(set! //S_/I/F 22)
(set! //S_/T/H/E/N 23)
(set! //S_/E/L/S/I/F 24)
(set! //S_/E/L/S/E 25)
(set! //S_/F/I 26)
(set! //S_/W/H/I/L/E 27)
(set! //S_/D/O 28)
(set! //S_/O/D 29)
(set! //S_/V/A/R 30)
(set! //S_/A/N/D 31)
(set! //S_/O/R 32)
(set! //S_/N/O/T 33)
(set! //S_/C/O/M/M/E/N/T 34)
(set! //S_/I/D/E/N/T/I/F/I/E/R 35)
(set! //S_/N/U/M/B/E/R 36)
(set! //S_/S/T/R/I/N/G 37)
(set! //S_/L/E/Q 38)
(set! //S_/G/E/Q 39)
(set! //S_/B/O/X 40)
(set! //S_/A/R/R/O/W 41)
(set! //S_/I/N/T/E/R/S/E/C/T 42)
(set! //S_/U/N/I/O/N 43)
(set! //S_/B/A/C/K/S/L/A/S/H 44)
(set! //S_/D_/I/F 45)
(set! //S_/E/X/P/O/N/E/N/T 46)
(set! //S_/D_/D/O 47)
(set! //S_/S/K/I/P 48)
(set! //S_/T/R/U/E 49)
(set! //S_/F/A/L/S/E 50)
(set! //S_/Q/U/E/R/Y 51)
(set! //S_/F/U/L/L/S/T/O/P 52)
(set! //S_/I/N 53)
(set! //S_/N/O/T/I/N 54)
(set! //S_/M/E/M/B/E/R 55)
(set! //S_/S/U/B/S/E/T 56)
(set! //S_/E/V/E/N 57)
(set! //S_/O/D/D 58)
(set! //S_/E/M/P/T/Y 59)
(set! //S_/F/O/R/A/L/L 60)
(set! //S_/E/X/I/S/T/S 61)
(set! //S_/M/O/D 62)
(set! //S_/D/I/V 63)
(set! //S_/E/N/D/V/A/R 64)
(set! //S_/E/X/I/T 65)
(set! //S_/D/O/T/D/O/T 66)
(set! //S_/C/A/L/L 68)
(set! //S_/A/C/T/I/O/N/S 69)
(set! //S_/E/N/D/A/C/T/I/O/N/S 70)
(set! //S_/F/O/R 71)
(set! //S_/T/O 72)
(set! //S_/S/T/E/P 73)
(set! //S_/A/T 74)
(set! //S_/E/N/D 75)
(set! //S_/S/C/O/P/A/R/E/N 76)
(set! //S_/P/L/I/N/K_/P 77)
(set! //S_/P/L/I/N/K_/X/P 78)
(set! //S_/P/L/I/N/K_/X/F 79)
(set! //S_/P/L/I/N/K_/X/C 80)
(set! //S_/R/E/T/U/R/N/S 81)
(set! //S_/V/B/A/R 82)
(set! //S_/P/U/S/H 83)
(set! //S_/P/O/P 84)
(set! //S_/J/O/I/N 85)
(set! //S_/E/N/D/J/O/I/N 86)
(set! //S_/D/O/T/S/P/A/C/E 87)
(set! //S_/P/R/I/N/T 88)
(set! //S_/P/R/I/N/F/L/U/S/H 89)
(set! //S_/N/A/T/S 90)
(set! //S_/I/N/T/S 91)
(set! //S_/R/A/T/S 92)
(set! //S_/R/E/A/L/S 93)
(set! //S_/A/R/R/A/Y 94)
(set! //S_/S/E/Q/U/E/N/C/E 95)
(set! //S_/S/T/R/I/N/G/B/F 96)
(set! //S_/N/U/M/B/E/R/Q 97)
(set! //S_/C/O/L/O/N/C/O/L/O/N 98)
(set! //S_/A/B/S 100)
(set! //S_/I/N/T 101)
(set! //S_/F/R/A/C 102)
(set! //S_/S/G/N 103)
(set! //S_/M/I/N 104)
(set! //S_/M/A/X 105)
(set! //S_/M/A/P 106)
(set! //S_/R/E/D/U/C/E 107)
(set! //S_/H/E/A/D 108)
(set! //S_/T/A/I/L 109)
(set! //S_/L/A/S/T 110)
(set! //S_/B/U/T/L/A/S/T 111)
(set! //S_/R/E/V/E/R/S/E 112)
(set! //S_/P/O/W/E/R/S/E/T 113)
(set! //S_/L/E/N/G/T/H 114)
(set! //S_/F/I/L/L1 115)
(set! //S_/E/N/D/F/I/L/L 116)
(set! //S_/C/A/R/E/T 117)
(set! //S_/S/L/E/N/G/T/H 118)
(set! //S_/S/U/B/S/T/R 119)
(set! //S_/I/N/D/E/X 120)
(set! //S_/M/A/P/H/A/S/H 121)
(set! //S_/E/R/R/O/R 122)
(set! //S_/F/I/L/L 123)
(set! //S_/A/D/D/R 124)
(set! //S_/B/E/G/I/N 500)
(set! //S_/W/H/E/R/E 501)
(set! //S_/P/R/O/C 502)
(set! //S_/F/U/N/C/T 503)
(set! //S_/B/F/U/N/C/T 504)
(set! //S_/M/W_/P/R/O/C 505)
(set! //S_/M/W_/F/U/N/C/T 506)
(set! //S_/M/W_/B/F/U/N/C/T 507)
(set! //S_/S/P/E/C 508)
(set! //S_/E/N/D/S/P/E/C 509)
(set! //S_/F/O/R/E/A/C/H 510)
(set! //S_/A/T/E/A/C/H 511)
(set! //S_/I/F/M/A/T/C/H 512)
(set! //S_/E/N/D/M/A/T/C/H 513)
(set! //S_/M/E/M 514)
(set! //S_/S/T/A/T/E/M/E/N/T 520)
(set! //S_/S/T/A/T/E/M/E/N/T/S 521)
(set! //S_/E/X/P/R/E/S/S/I/O/N 522)
(set! //S_/E/X/P/R/E/S/S/I/O/N/S 523)
(set! //S_/C/O/N/D/I/T/I/O/N 524)
(set! //S_/V/A/R/I/A/B/L/E 526)
(set! //S_/D/E/F/I/N/I/T/I/O/N 527)
(set! //S_/D/E/F/I/N/I/T/I/O/N/S 528)
(set! //S_/L/V/A/L/U/E 529)
(set! //S_/L/V/A/L/U/E/S 530)
(set! //S_/S/T/S 531)
(set! //S_/N/A/S 532)
(set! //S_/A/S/S/I/G/N 533)
(set! //S_/A/S/S/I/G/N/S 534)
(set! //S_/G/U/A/R/D/E/D 535)
(set! //S_/A/C/T/I/O/N 536)
(set! //S_/T/E/R/M/I/N/A/L 537)
(set! //S_/G/L/O/B/A/L 538)
(set! //S_/P/A/T_/O/N/E 700)
(set! //S_/P/A/T_/M/A/N/Y 701)
(set! //S_/P/A/T_/A/N/Y 702)
(set! //S_/I/N/T_/O/N/E 703)
(set! //S_/I/N/T_/A/N/Y 704)
(set! //S_/V/A/R_/O/N/E 705)
(set! //S_/V/A/R_/A/N/Y 706)
(set! //S_/A/T_/P/A/T_/O/N/E 707)
(set! //S_/S/T/A/T_/P/L/A/C/E 800)
(set! //S_/E/X/P/N_/P/L/A/C/E 801)
(set! //S_/V/A/R_/P/L/A/C/E 802)
(set! //S_/C/O/N/D_/P/L/A/C/E 803)
(set! //S_/H/A/S/H_/T/A/B/L/E 804)
(set! //S_/P/R/I/M/E 805)
(set! //S_/A/B/O/R/T 997)
(set! //S_/I/N/V/A/L/I/D 998)
(set! //S_/E/O/F 999)
; The first 3 variables shouldn't ordinarily be used outside the lexer
(set! /yy_ch '())
; Current character being processed in lexer
(set! /yy_sym 0)
; Numerical value of lexer token
(set! /yy_value '())
; Value (number, string etc) associated with token
(set! /token1 0)
(set! /token2 '())
; Current token in parser (returned by @yy_lex(): <sym,value>) 
(set! /yy_lineno 0)
; Current line number in source file 
(set! /yy_line '())
; Current line being processed in lexer
(set! //Whitespace (@List_To_Set (list " " //Tab //Newline)))
(set! //Digits (@List_To_Set (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "0")))
; Reserved Words
; Used by initialisation function to build hash table, which
; is then used by the lexer as lookup table
(set! //Keywords (list (list //S_/I/F "IF") (list //S_/T/H/E/N "THEN") (list //S_/E/L/S/I/F "ELSIF") (list //S_/E/L/S/E "ELSE") (list //S_/F/I "FI") (list //S_/D_/I/F "D_IF") (list //S_/W/H/I/L/E "WHILE") (list //S_/D/O "DO") (list //S_/D_/D/O "D_DO") (list //S_/O/D "OD") (list //S_/V/A/R "VAR") (list //S_/E/N/D/V/A/R "ENDVAR") (list //S_/S/K/I/P "SKIP") (list //S_/A/N/D "AND") (list //S_/O/R "OR") (list //S_/N/O/T "NOT") (list //S_/C/O/M/M/E/N/T "Comment") (list //S_/C/O/M/M/E/N/T "COMMENT") (list //S_/P/R/I/N/T "PRINT") (list //S_/P/R/I/N/F/L/U/S/H "PRINFLUSH") (list //S_/T/R/U/E "TRUE") (list //S_/F/A/L/S/E "FALSE") (list //S_/I/N "IN") (list //S_/N/O/T/I/N "NOTIN") (list //S_/E/V/E/N "EVEN") (list //S_/O/D/D "ODD") (list //S_/E/M/P/T/Y "EMPTY") (list //S_/S/U/B/S/E/T "SUBSET") (list //S_/M/E/M/B/E/R "MEMBER") (list //S_/F/O/R/A/L/L "FORALL") (list //S_/E/X/I/S/T/S "EXISTS") (list //S_/S/E/Q/U/E/N/C/E "SEQUENCE") (list //S_/S/T/R/I/N/G/B/F "STRING") (list //S_/N/U/M/B/E/R/Q "NUMBER") (list //S_/A/R/R/A/Y "ARRAY") (list //S_/R/E/D/U/C/E "REDUCE") (list //S_/M/A/P "MAP") (list //S_/H/E/A/D "HEAD") (list //S_/T/A/I/L "TAIL") (list //S_/B/U/T/L/A/S/T "BUTLAST") (list //S_/L/A/S/T "LAST") (list //S_/R/E/V/E/R/S/E "REVERSE") (list //S_/L/E/N/G/T/H "LENGTH") (list //S_/P/O/W/E/R/S/E/T "POWERSET") (list //S_/A/B/S "ABS") (list //S_/I/N/T "INT") (list //S_/F/R/A/C "FRAC") (list //S_/S/G/N "SGN") (list //S_/M/A/X "MAX") (list //S_/M/I/N "MIN") (list //S_/M/O/D "MOD") (list //S_/D/I/V "DIV") (list //S_/P/U/S/H "PUSH") (list //S_/P/O/P "POP") (list //S_/S/L/E/N/G/T/H "SLENGTH") (list //S_/S/U/B/S/T/R "SUBSTR") (list //S_/I/N/D/E/X "INDEX") (list //S_/M/A/P/H/A/S/H "MAPHASH") (list //S_/E/R/R/O/R "ERROR") (list //S_/J/O/I/N "JOIN") (list //S_/E/N/D/J/O/I/N "ENDJOIN") (list //S_/C/A/L/L "CALL") (list //S_/A/C/T/I/O/N/S "ACTIONS") (list //S_/F/O/R "FOR") (list //S_/T/O "TO") (list //S_/S/T/E/P "STEP") (list //S_/E/N/D "END") (list //S_/B/E/G/I/N "BEGIN") (list //S_/W/H/E/R/E "WHERE") (list //S_/M/W_/P/R/O/C "MW_PROC") (list //S_/M/W_/F/U/N/C/T "MW_FUNCT") (list //S_/M/W_/B/F/U/N/C/T "MW_BFUNCT") (list //S_/P/R/O/C "PROC") (list //S_/F/U/N/C/T "FUNCT") (list //S_/B/F/U/N/C/T "BFUNCT") (list //S_/E/N/D/A/C/T/I/O/N/S "ENDACTIONS") (list //S_/R/E/T/U/R/N/S "RETURNS") (list //S_/S/P/E/C "SPEC") (list //S_/E/N/D/S/P/E/C "ENDSPEC") (list //S_/I/F/M/A/T/C/H "IFMATCH") (list //S_/E/N/D/M/A/T/C/H "ENDMATCH") (list //S_/F/O/R/E/A/C/H "FOREACH") (list //S_/A/T/E/A/C/H "ATEACH") (list //S_/F/I/L/L "FILL") (list //S_/E/N/D/F/I/L/L "ENDFILL") (list //S_/S/T/A/T_/P/L/A/C/E "$Statement$") (list //S_/E/X/P/N_/P/L/A/C/E "$Expn$") (list //S_/V/A/R_/P/L/A/C/E "$Var$") (list //S_/C/O/N/D_/P/L/A/C/E "$Condition$") (list //S_/H/A/S/H_/T/A/B/L/E "HASH_TABLE") (list //S_/N/A/T/S "%N") (list //S_/I/N/T/S "%Z") (list //S_/R/A/T/S "%Q") (list //S_/R/E/A/L/S "%R") (list //S_/S/T/A/T/E/M/E/N/T "Statement") (list //S_/S/T/A/T/E/M/E/N/T/S "Statements") (list //S_/E/X/P/R/E/S/S/I/O/N "Expression") (list //S_/E/X/P/R/E/S/S/I/O/N/S "Expressions") (list //S_/C/O/N/D/I/T/I/O/N "Condition") (list //S_/V/A/R/I/A/B/L/E "Variable") (list //S_/D/E/F/I/N/I/T/I/O/N "Definition") (list //S_/D/E/F/I/N/I/T/I/O/N/S "Definitions") (list //S_/L/V/A/L/U/E "Lvalue") (list //S_/L/V/A/L/U/E/S "Lvalues") (list //S_/S/T/S "STS") (list //S_/N/A/S "NAS") (list //S_/A/S/S/I/G/N "Assign") (list //S_/A/S/S/I/G/N/S "Assigns") (list //S_/G/U/A/R/D/E/D "Guarded") (list //S_/A/C/T/I/O/N "Action") (list //S_/T/E/R/M/I/N/A/L "Terminal") (list //S_/G/L/O/B/A/L "Global") (list //S_/E/X/I/T "EXIT") (list //S_/A/B/O/R/T "ABORT") (list //S_/A/D/D/R "ADDRESS_OF")))
; The following list is used in comment parsing to attempt error recovery, 
; and nowhere else. 
(set! //Reserved_/Words (@List_To_Vector (list //S_/I/F //S_/T/H/E/N //S_/E/L/S/I/F //S_/E/L/S/E //S_/F/I //S_/W/H/I/L/E //S_/D/O //S_/O/D //S_/S/K/I/P //S_/V/A/R //S_/A/N/D //S_/O/R //S_/N/O/T //S_/C/O/M/M/E/N/T //S_/D_/I/F //S_/D_/D/O //S_/E/O/F //S_/C/A/L/L //S_/M/W_/P/R/O/C //S_/M/W_/F/U/N/C/T //S_/M/W_/B/F/U/N/C/T //S_/B/E/G/I/N //S_/W/H/E/R/E //S_/E/N/D //S_/R/E/T/U/R/N/S //S_/F/O/R //S_/A/C/T/I/O/N/S //S_/E/N/D/A/C/T/I/O/N/S //S_/E/N/D/J/O/I/N //S_/E/N/D/S/P/E/C //S_/M/O/D //S_/D/I/V //S_/E/X/I/T //S_/A/B/O/R/T)))
; List of `closing' tokens for composite statements. 
; Used to detect end of statement sequences which clueless 
; programmers have terminated with semicolons :-) 
(set! //Closing_/Toks (@List_To_Vector (list //S_/F/I //S_/O/D //S_/L/P/A/R/E/N //S_/E/L/S/E //S_/E/L/S/I/F //S_/W/H/E/R/E //S_/E/N/D/V/A/R //S_/E/O/F //S_/R/E/T/U/R/N/S //S_/E/N/D/J/O/I/N)))
; Used in lexer to spot the end of identifiers/reserved words 
(set! //Special_/Chars (@List_To_Set (list //Quote "+" "=" "-" ")" "(" "*" "^" "}" "{" "?" "!" "|" "^" "]" "[" ":" "," "." "<" ">" "/" "'" //Backslash "eof" ";")))
(set! //Special_/Or_/White (@List_To_Set (list //Quote "+" "=" "-" ")" "(" "*" "^" "}" "{" "?" "!" "|" "^" "]" "[" ":" "," "." "<" ">" "/" "'" //Backslash "eof" ";" " " //Tab //Newline)))
; Table mapping lexer symbols to their corresponding meta-WSL spec. types.
; Used by initialisation function to build hash table
(set! //Types (list (list //T_/Equal //S_/E/Q/U/A/L) (list //T_/Not_/Equal //S_/N/E/Q) (list //T_/Less //S_/L/A/N/G/L/E) (list //T_/Less_/Eq //S_/L/E/Q) (list //T_/Greater //S_/R/A/N/G/L/E) (list //T_/Greater_/Eq //S_/G/E/Q) (list //T_/True //S_/T/R/U/E) (list //T_/False //S_/F/A/L/S/E) (list //T_/Plus //S_/P/L/U/S) (list //T_/Minus //S_/M/I/N/U/S) (list //T_/Times //S_/T/I/M/E/S) (list //T_/Divide //S_/S/L/A/S/H) (list //T_/Exponent //S_/E/X/P/O/N/E/N/T) (list //T_/Set_/Diff //S_/B/A/C/K/S/L/A/S/H) (list //T_/Union //S_/U/N/I/O/N) (list //T_/Intersection //S_/I/N/T/E/R/S/E/C/T) (list //T_/Mod //S_/M/O/D) (list //T_/Div //S_/D/I/V) (list //T_/Concat //S_/C/O/N/C/A/T) (list //T_/Array //S_/A/R/R/A/Y) (list //T_/Primed_/Var //S_/P/R/I/M/E) (list //T_/And //S_/A/N/D) (list //T_/Or //S_/O/R) (list //T_/Even //S_/E/V/E/N) (list //T_/Odd //S_/O/D/D) (list //T_/Empty //S_/E/M/P/T/Y) (list //T_/In //S_/I/N) (list //T_/Not_/In //S_/N/O/T/I/N) (list //T_/Member //S_/M/E/M/B/E/R) (list //T_/Subset //S_/S/U/B/S/E/T) (list //T_/Forall //S_/F/O/R/A/L/L) (list //T_/Exists //S_/E/X/I/S/T/S) (list //T_/Stringq //S_/S/T/R/I/N/G/B/F) (list //T_/Numberq //S_/N/U/M/B/E/R/Q) (list //T_/Sequenceq //S_/S/E/Q/U/E/N/C/E) (list //T_/Abs //S_/A/B/S) (list //T_/Int //S_/I/N/T) (list //T_/Frac //S_/F/R/A/C) (list //T_/Sgn //S_/S/G/N) (list //T_/Max //S_/M/A/X) (list //T_/Min //S_/M/I/N) (list //T_/Length //S_/L/E/N/G/T/H) (list //T_/Slength //S_/S/L/E/N/G/T/H) (list //T_/Index //S_/I/N/D/E/X) (list //T_/Substr //S_/S/U/B/S/T/R) (list //T_/Reduce //S_/R/E/D/U/C/E) (list //T_/Map //S_/M/A/P) (list //T_/Tail //S_/T/A/I/L) (list //T_/Head //S_/H/E/A/D) (list //T_/Butlast //S_/B/U/T/L/A/S/T) (list //T_/Last //S_/L/A/S/T) (list //T_/Powerset //S_/P/O/W/E/R/S/E/T) (list //T_/Reverse //S_/R/E/V/E/R/S/E) (list //T_/Skip //S_/S/K/I/P) (list //T_/For //S_/F/O/R) (list //T_/Actions //S_/A/C/T/I/O/N/S) (list //T_/Call //S_/C/A/L/L) (list //T_/Stat_/Place //S_/S/T/A/T_/P/L/A/C/E) (list //T_/Expn_/Place //S_/E/X/P/N_/P/L/A/C/E) (list //T_/Var_/Place //S_/V/A/R_/P/L/A/C/E) (list //T_/Cond_/Place //S_/C/O/N/D_/P/L/A/C/E) (list //T_/Exit //S_/E/X/I/T) (list //T_/Abort //S_/A/B/O/R/T) (list //T_/Address_/Of //S_/A/D/D/R)))
; Table of unambiguous single-character terminal symbols 
; and their lexical tokens. Used to build lexer tables.  
(set! /char_table (list (list "eof" //S_/E/O/F) (list "?" //S_/Q/U/E/R/Y) (list "," //S_/C/O/M/M/A) (list "]" //S_/R/B/R/A/C/K/E/T) (list "{" //S_/L/B/R/A/C/E) (list "}" //S_/R/B/R/A/C/E) (list "(" //S_/L/P/A/R/E/N) (list ")" //S_/R/P/A/R/E/N) (list "|" //S_/V/B/A/R) (list ";" //S_/S/E/M/I/C/O/L/O/N) (list "^" //S_/C/A/R/E/T)))
; Prefix condition operators 
(set! //Prefix_/Conds (@List_To_Vector (list //S_/E/V/E/N //S_/O/D/D //S_/E/M/P/T/Y //S_/S/U/B/S/E/T //S_/M/E/M/B/E/R //S_/F/O/R/A/L/L //S_/E/X/I/S/T/S //S_/S/T/R/I/N/G/B/F //S_/N/U/M/B/E/R/Q //S_/S/E/Q/U/E/N/C/E)))
; Number types 
(set! /numb_types (@List_To_Vector (list //S_/N/A/T/S //S_/I/N/T/S //S_/R/A/T/S //S_/R/E/A/L/S)))
; Prefix expression operators of various types 
(set! /a_prefix_ops (@List_To_Vector (list //S_/A/B/S //S_/I/N/T //S_/F/R/A/C //S_/S/G/N //S_/M/A/X //S_/M/I/N //S_/L/E/N/G/T/H //S_/S/L/E/N/G/T/H //S_/I/N/D/E/X //S_/A/D/D/R)))
(set! /s_prefix_ops (@List_To_Vector (list //S_/M/A/P //S_/P/O/W/E/R/S/E/T //S_/T/A/I/L //S_/B/U/T/L/A/S/T //S_/R/E/V/E/R/S/E //S_/A/R/R/A/Y //S_/S/U/B/S/T/R)))
(set! /g_prefix_ops (@List_To_Vector (list //S_/R/E/D/U/C/E //S_/H/E/A/D //S_/L/A/S/T)))
; Generic expression types 
(set! /g_exp_types (@List_To_Vector (list //T_/Variable //T_/Var_/Place //T_/Expn_/Place //T_/X_/Funct_/Call //T_/M/W_/Funct_/Call //T_/Aref //T_/Sub_/Seg //T_/Rel_/Seg //T_/Final_/Seg //T_/Funct_/Call //T_/Reduce //T_/Head //T_/Last //T_/Gethash //T_/If //T_/Expn_/Pat_/One //T_/Expn_/Pat_/Many //T_/Expn_/Pat_/Any //T_/Expn_/Int_/One //T_/Expn_/Int_/Any //T_/Expn_/Var_/One //T_/Expn_/Var_/Any //T_/Struct //T_/String //T_/Primed_/Var //T_/Mem //T_/Mem_/Seg //T_/Mem_/Rel)))
; Arithmetic operators - see expression grammar 
(set! //Term_/Ops (@List_To_Vector (list //S_/P/L/U/S //S_/M/I/N/U/S)))
(set! //Factor_/Ops (@List_To_Vector (list //S_/T/I/M/E/S //S_/S/L/A/S/H //S_/M/O/D //S_/D/I/V)))
(set! //Math_/Exps (@List_To_Vector (list //T_/Abs //T_/Int //T_/Max //T_/Min //T_/Length //T_/Number //T_/Negate //T_/Plus //T_/Minus //T_/Times //T_/Divide //T_/Exponent //T_/Mod //T_/Div)))
; Sequence/set operators - see grammar 
(set! //S_/Term_/Ops (@List_To_Vector (list //S_/B/A/C/K/S/L/A/S/H)))
(set! //S_/Factor_/Ops (@List_To_Vector (list //S_/C/O/N/C/A/T //S_/U/N/I/O/N)))
(set! //S_/Atom_/Ops (@List_To_Vector (list //S_/I/N/T/E/R/S/E/C/T)))
(set! //Seq_/Exps (@List_To_Vector (list //T_/Concat //T_/Set_/Diff //T_/Union //T_/Intersection //T_/Map //T_/Powerset //T_/Tail //T_/Butlast //T_/Reverse)))
; Relational and boolean operators - see condition grammar above
(set! //Rel_/Ops (@List_To_Vector (list //S_/E/Q/U/A/L //S_/N/E/Q //S_/L/A/N/G/L/E //S_/L/E/Q //S_/R/A/N/G/L/E //S_/G/E/Q //S_/I/N //S_/N/O/T/I/N)))
(set! //Bool_/Ops (@List_To_Vector (list //S_/A/N/D //S_/O/R)))
; Patterns 
(set! //Patterns (@List_To_Vector (list //S_/P/A/T_/O/N/E //S_/P/A/T_/M/A/N/Y //S_/P/A/T_/A/N/Y //S_/I/N/T_/O/N/E //S_/I/N/T_/A/N/Y //S_/V/A/R_/O/N/E //S_/V/A/R_/A/N/Y //S_/A/T_/P/A/T_/O/N/E)))
(set! //Error_/Count 0)
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; This function is the main interface to the parser                             
; It takes as arguments the name of a wsl file to be parsed,                    
; and the generic type of the structure the file contains:                      
; usually this will be T_Statements, for a WSL program.                         
;-------------------------------------------------------------------------------
(define (@Parse_File /infile /type)
 (let ((//S '())
       (//Input_/Port-save //Input_/Port)
       (funct-result '()))
  (set! //Input_/Port '())
  (cond
   ((equal? /infile "")
    (set! //Input_/Port //Standard_/Input_/Port)
    (@initialise)
    (set! //S (@yy_parse /type))
    (@yy_check_end))
   ((@File_Exists? /infile)
    (set! //Input_/Port (@Open_Input_File /infile))
    (@initialise)
    (set! //S (@yy_parse /type))
    (@yy_check_end)
    (@Close_Input_Port //Input_/Port))
   (#t
    (display-list (string-append (string-append "@Parse_File: file " (@String /infile)) " not found!"))))
  (set! funct-result //S)
  (set! //Input_/Port //Input_/Port-save)
  funct-result))

(set! //Input_/Port '())
;-------------------------------------------------------------------------------
;****************
; The parser     
;****************
; type can be any of the generic types below
(define (@yy_parse /type)
 (let ((//S '()))
  (cond
   ((= /type //T_/Statements)
    (set! //S (@yy_statements)))
   ((= /type //T_/Statement)
    (set! //S (@yy_statement)))
   ((= /type //T_/Expression)
    (set! //S (@yy_expression)))
   ((= /type //T_/Expressions)
    (set! //S (@yy_expressions)))
   ((= /type //T_/Condition)
    (set! //S (@yy_condition)))
   ((= /type //T_/Definition)
    (set! //S (@yy_define)))
   ((= /type //T_/Definitions)
    (set! //S (@yy_defines)))
   ((= /type //T_/Assign)
    (set! //S (@yy_assign)))
   ((= /type //T_/Assigns)
    (set! //S (@Make //T_/Assigns '() (@yy_assigns))))
   ((= /type //T_/Action)
    (set! //S (@yy_action)))
   ((= /type //T_/Guarded)
    (set! //S (@yy_guard_gen (list //S_/T/H/E/N //S_/A/R/R/O/W))))
   ((= /type //T_/Lvalue)
    (set! //S (@yy_lvalue)))
   ((= /type //T_/Lvalues)
    (set! //S (@yy_lvalues))))
  //S))

;-------------------------------------------------------------------------------
(define (@Syntax_Error /spiel)
 (set! //Error_/Count (+ //Error_/Count 1))
 (cond
  ((or (= /token1 //S_/I/D/E/N/T/I/F/I/E/R) (= /token1 //S_/N/U/M/B/E/R) (= /token1 //S_/S/T/R/I/N/G))
   (display-list "!!!! Line " /yy_lineno ": Syntax Error: " /spiel ": " /token1 ": " /token2))
  (#t
   (display-list "!!!! Line " /yy_lineno ": Syntax Error: " /spiel ": " /token1))))

(define (@Warning /spiel)
 (display-list "**** Line " /yy_lineno ": Warning: " /spiel ": " /token1))

;  This proc. looks for the expected symbol and skips over it.     
;  If the expected symbol is not found a syntax error is generated 
;  with the given message
(define (@yy_skip_symbol /symbol /message)
 (cond
  ((not (= /token1 /symbol))
   (@Syntax_Error /message))
  (#t
   (@yy_lex))))

(define (@yy_old_PRINT /str)
 (display-list /str))

(define (@yy_PRINT /str)
 #t
 #t)

; Table which maps a pattern symbol from the lexer plus a generic type 
; into the appropriate specific type 
(set! /pattern_type (hash-table))
(puthash /pattern_type (list //T_/Statement //S_/P/A/T_/O/N/E) //T_/Stat_/Pat_/One)
(puthash /pattern_type (list //T_/Statement //S_/P/A/T_/M/A/N/Y) //T_/Stat_/Pat_/Many)
(puthash /pattern_type (list //T_/Statement //S_/P/A/T_/A/N/Y) //T_/Stat_/Pat_/Any)
(puthash /pattern_type (list //T_/Statement //S_/I/N/T_/O/N/E) //T_/Stat_/Int_/One)
(puthash /pattern_type (list //T_/Statement //S_/I/N/T_/A/N/Y) //T_/Stat_/Int_/Any)
(puthash /pattern_type (list //T_/Statement //S_/V/A/R_/O/N/E) //T_/Stat_/Var_/One)
(puthash /pattern_type (list //T_/Statement //S_/V/A/R_/A/N/Y) //T_/Stat_/Var_/Any)
(puthash /pattern_type (list //T_/Condition //S_/P/A/T_/O/N/E) //T_/Cond_/Pat_/One)
(puthash /pattern_type (list //T_/Condition //S_/P/A/T_/M/A/N/Y) //T_/Cond_/Pat_/Many)
(puthash /pattern_type (list //T_/Condition //S_/P/A/T_/A/N/Y) //T_/Cond_/Pat_/Any)
(puthash /pattern_type (list //T_/Condition //S_/I/N/T_/O/N/E) //T_/Cond_/Int_/One)
(puthash /pattern_type (list //T_/Condition //S_/I/N/T_/A/N/Y) //T_/Cond_/Int_/Any)
(puthash /pattern_type (list //T_/Condition //S_/V/A/R_/O/N/E) //T_/Cond_/Var_/One)
(puthash /pattern_type (list //T_/Condition //S_/V/A/R_/A/N/Y) //T_/Cond_/Var_/Any)
(puthash /pattern_type (list //T_/Expression //S_/P/A/T_/O/N/E) //T_/Expn_/Pat_/One)
(puthash /pattern_type (list //T_/Expression //S_/P/A/T_/M/A/N/Y) //T_/Expn_/Pat_/Many)
(puthash /pattern_type (list //T_/Expression //S_/P/A/T_/A/N/Y) //T_/Expn_/Pat_/Any)
(puthash /pattern_type (list //T_/Expression //S_/I/N/T_/O/N/E) //T_/Expn_/Int_/One)
(puthash /pattern_type (list //T_/Expression //S_/I/N/T_/A/N/Y) //T_/Expn_/Int_/Any)
(puthash /pattern_type (list //T_/Expression //S_/V/A/R_/O/N/E) //T_/Expn_/Var_/One)
(puthash /pattern_type (list //T_/Expression //S_/V/A/R_/A/N/Y) //T_/Expn_/Var_/Any)
(puthash /pattern_type (list //T_/Lvalue //S_/P/A/T_/O/N/E) //T_/Lvalue_/Pat_/One)
(puthash /pattern_type (list //T_/Lvalue //S_/P/A/T_/M/A/N/Y) //T_/Lvalue_/Pat_/Many)
(puthash /pattern_type (list //T_/Lvalue //S_/P/A/T_/A/N/Y) //T_/Lvalue_/Pat_/Any)
(puthash /pattern_type (list //T_/Lvalue //S_/I/N/T_/O/N/E) //T_/Lvalue_/Int_/One)
(puthash /pattern_type (list //T_/Lvalue //S_/I/N/T_/A/N/Y) //T_/Lvalue_/Int_/Any)
(puthash /pattern_type (list //T_/Lvalue //S_/V/A/R_/O/N/E) //T_/Lvalue_/Var_/One)
(puthash /pattern_type (list //T_/Lvalue //S_/V/A/R_/A/N/Y) //T_/Lvalue_/Var_/Any)
(puthash /pattern_type (list //T_/Guarded //S_/P/A/T_/O/N/E) //T_/Guarded_/Pat_/One)
(puthash /pattern_type (list //T_/Guarded //S_/P/A/T_/M/A/N/Y) //T_/Guarded_/Pat_/Many)
(puthash /pattern_type (list //T_/Guarded //S_/P/A/T_/A/N/Y) //T_/Guarded_/Pat_/Any)
(puthash /pattern_type (list //T_/Guarded //S_/I/N/T_/O/N/E) //T_/Guarded_/Int_/One)
(puthash /pattern_type (list //T_/Guarded //S_/I/N/T_/A/N/Y) //T_/Guarded_/Int_/Any)
(puthash /pattern_type (list //T_/Guarded //S_/V/A/R_/O/N/E) //T_/Guarded_/Var_/One)
(puthash /pattern_type (list //T_/Guarded //S_/V/A/R_/A/N/Y) //T_/Guarded_/Var_/Any)
(puthash /pattern_type (list //T_/Assign //S_/P/A/T_/O/N/E) //T_/Assign_/Pat_/One)
(puthash /pattern_type (list //T_/Assign //S_/P/A/T_/M/A/N/Y) //T_/Assign_/Pat_/Many)
(puthash /pattern_type (list //T_/Assign //S_/P/A/T_/A/N/Y) //T_/Assign_/Pat_/Any)
(puthash /pattern_type (list //T_/Assign //S_/I/N/T_/O/N/E) //T_/Assign_/Int_/One)
(puthash /pattern_type (list //T_/Assign //S_/I/N/T_/A/N/Y) //T_/Assign_/Int_/Any)
(puthash /pattern_type (list //T_/Assign //S_/V/A/R_/O/N/E) //T_/Assign_/Var_/One)
(puthash /pattern_type (list //T_/Assign //S_/V/A/R_/A/N/Y) //T_/Assign_/Var_/Any)
(puthash /pattern_type (list //T_/Action //S_/P/A/T_/O/N/E) //T_/Action_/Pat_/One)
(puthash /pattern_type (list //T_/Action //S_/P/A/T_/M/A/N/Y) //T_/Action_/Pat_/Many)
(puthash /pattern_type (list //T_/Action //S_/P/A/T_/A/N/Y) //T_/Action_/Pat_/Any)
(puthash /pattern_type (list //T_/Action //S_/I/N/T_/O/N/E) //T_/Action_/Int_/One)
(puthash /pattern_type (list //T_/Action //S_/I/N/T_/A/N/Y) //T_/Action_/Int_/Any)
(puthash /pattern_type (list //T_/Action //S_/V/A/R_/O/N/E) //T_/Action_/Var_/One)
(puthash /pattern_type (list //T_/Action //S_/V/A/R_/A/N/Y) //T_/Action_/Var_/Any)
(puthash /pattern_type (list //T_/Definition //S_/P/A/T_/O/N/E) //T_/Defn_/Pat_/One)
(puthash /pattern_type (list //T_/Definition //S_/P/A/T_/M/A/N/Y) //T_/Defn_/Pat_/Many)
(puthash /pattern_type (list //T_/Definition //S_/P/A/T_/A/N/Y) //T_/Defn_/Pat_/Any)
(puthash /pattern_type (list //T_/Definition //S_/I/N/T_/O/N/E) //T_/Defn_/Int_/One)
(puthash /pattern_type (list //T_/Definition //S_/I/N/T_/A/N/Y) //T_/Defn_/Int_/Any)
(puthash /pattern_type (list //T_/Definition //S_/V/A/R_/O/N/E) //T_/Defn_/Var_/One)
(puthash /pattern_type (list //T_/Definition //S_/V/A/R_/A/N/Y) //T_/Defn_/Var_/Any)
(puthash /pattern_type (list //T_/Name //S_/P/A/T_/O/N/E) //T_/Name_/Pat_/One)
(puthash /pattern_type (list //T_/Name //S_/I/N/T_/O/N/E) //T_/Name_/Int_/One)
(puthash /pattern_type (list //T_/Name //S_/V/A/R_/O/N/E) //T_/Name_/Var_/One)
;-------------------------------------------------------------------------------
;                                                                               
;   The following functions parse an object whose type is given by the          
;   function name, and return an AST node of that type.                         
;   ALL functions must leave the NEXT token in `token' when they return         
;                                                                               
;-------------------------------------------------------------------------------
; Check that we are at the end of the file: 
(define (@yy_check_end)
 (cond
  ((not (= /token1 //S_/E/O/F))
   ; Extra chars after `last' statement: search forward for a terminator
   (while (and (not (= /token1 //S_/E/O/F)) (not (= /token1 //S_/S/E/M/I/C/O/L/O/N))) 
    (@yy_lex))
   (cond
    ((= /token1 //S_/E/O/F)
     (@Syntax_Error "Extra characters at end of program"))
    (#t
     ; Found a semicolon: there are in fact more statements
     (@Syntax_Error "Extra characters at end of statement")
     (@yy_lex))))))

;-------------------------------------------------------------------------------
;  Statement sequence                                                           
;-------------------------------------------------------------------------------
(define (@yy_statements)
 (let ((//S '())
       (/temp '()))
  (set! //S (list (@yy_statement)))
  (while (= /token1 //S_/S/E/M/I/C/O/L/O/N) 
   (begin
    (@yy_lex)
    (set! /temp (@yy_statement))
    (cond
     ((equal? /temp '())
      (cond
       ((= /token1 //S_/E/O/F)
        (@Syntax_Error "Missing <return> at end of final line"))))
     (#t
      (set! //S (cons /temp //S))))))
  (cond
   ((equal? //S (list '()))
    (set! //S (list (@Skip)))))
  (@Make //T_/Statements '() (reverse //S))))

;-------------------------------------------------------------------------------
;  Parse the generic type `Statement'                                           
;-------------------------------------------------------------------------------
(define (@yy_statement)
 (let ((//S '()))
  (cond
   ((= (wsl-ref //Closing_/Toks /token1) 1)
    #t)
   ((= /token1 //S_/I/N/V/A/L/I/D)
    (@Syntax_Error (string-append "Invalid token: " /token2))
    (set! //S (@Make //T_/Stat_/Place '() '()))
    (@yy_lex))
   ((= /token1 //S_/I/F)
    (set! //S (@yy_if)))
   ((= /token1 //S_/D_/I/F)
    (set! //S (@yy_d_generic //T_/D_/If //S_/F/I)))
   ((= /token1 //S_/D_/D/O)
    (set! //S (@yy_d_generic //T_/D_/Do //S_/O/D)))
   ((= /token1 //S_/W/H/I/L/E)
    (set! //S (@yy_while)))
   ((= /token1 //S_/D/O)
    (set! //S (@yy_floop)))
   ((= /token1 //S_/E/X/I/T)
    (set! //S (@yy_exit)))
   ((= /token1 //S_/F/O/R)
    (set! //S (@yy_for)))
   ((= /token1 //S_/V/A/R)
    (set! //S (@yy_var)))
   ((= /token1 //S_/C/O/M/M/E/N/T)
    (set! //S (@yy_comment)))
   ((= /token1 //S_/L/B/R/A/C/E)
    (set! //S (@yy_assert)))
   ((= /token1 //S_/L/A/N/G/L/E)
    (set! //S (@yy_assignment)))
   ((= /token1 //S_/P/U/S/H)
    (set! //S (@yy_push)))
   ((= /token1 //S_/P/O/P)
    (set! //S (@yy_pop)))
   ((= /token1 //S_/J/O/I/N)
    (set! //S (@yy_join)))
   ((= /token1 //S_/A/C/T/I/O/N/S)
    (set! //S (@yy_as)))
   ((= /token1 //S_/C/A/L/L)
    (set! //S (@yy_call)))
   ((= /token1 //S_/P/R/I/N/T)
    (set! //S (@yy_gen_print //T_/Print)))
   ((= /token1 //S_/P/R/I/N/F/L/U/S/H)
    (set! //S (@yy_gen_print //T_/Prinflush)))
   ((= /token1 //S_/M/W_/P/R/O/C)
    (set! //S (@yy_mw_proc)))
   ((= /token1 //S_/M/W_/F/U/N/C/T)
    (set! //S (@yy_mw_funct)))
   ((= /token1 //S_/M/W_/B/F/U/N/C/T)
    (set! //S (@yy_mw_bfunct)))
   ((= /token1 //S_/B/E/G/I/N)
    (set! //S (@yy_where)))
   ((= /token1 //S_/P/L/I/N/K_/P)
    (set! //S (@yy_aproc_call)))
   ((= /token1 //S_/A/T)
    (set! //S (@yy_mw_proc_call)))
   ((= /token1 //S_/A/T_/P/A/T_/O/N/E)
    (set! //S (@yy_mw_proc_call)))
   ((= /token1 //S_/P/L/I/N/K_/X/P)
    (set! //S (@yy_x_proc_call)))
   ((= /token1 //S_/F/O/R/E/A/C/H)
    (set! //S (@yy_foreach)))
   ((= /token1 //S_/A/T/E/A/C/H)
    (set! //S (@yy_ateach)))
   ((= /token1 //S_/I/F/M/A/T/C/H)
    (set! //S (@yy_ifmatch)))
   ((= /token1 //S_/M/A/P/H/A/S/H)
    (set! //S (@yy_maphash)))
   ((= /token1 //S_/E/R/R/O/R)
    (set! //S (@yy_error)))
   ((= /token1 //S_/S/P/E/C)
    (set! //S (@yy_spec)))
   ((= /token1 //S_/M/E/M)
    (set! //S (@Make //T_/Assignment '() (list (@yy_assign)))))
   ((= (wsl-ref //Patterns /token1) 1)
    (cond
     ((equal? /token2 "")
      (set! //S (@yy_stat_pattern)))
     (#t
      (error "@yy_gen_pat(S, <T_Statement>) not yet implemented!"))))
   ((or (= /token1 //S_/S/K/I/P) (= /token1 //S_/A/B/O/R/T) (= /token1 //S_/S/T/A/T_/P/L/A/C/E))
    (set! //S (@yy_trivial)))
   ((= /token1 //S_/N/U/M/B/E/R)
    (@Syntax_Error "Number invalid at start of statement")
    (@yy_lex))
   ((not (equal? (gethash //Key_/Table /token2) '()))
    (@Reserved_Word_Error /token2))
   ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
    (let ((/name-save /name))
     (set! /name (@Make_Name /token2))
     (@yy_lex)
     (cond
      ((= /token1 //S_/L/P/A/R/E/N)
       (set! //S (@yy_proc_call /name)))
      (#t
       (set! //S (@yy_single_assign (@Make //T_/Var_/Lvalue /name '())))))
     (set! /name /name-save)))
   (#t
    (@Syntax_Error "Invalid string at start of statement")))
  //S))

(define (@Reserved_Word_Error //Tok)
 (@Syntax_Error (string-append (string-append "reserved word " //Tok) " not valid here")))

;-------------------------------------------------------------------------------
;  First, deal with the trivial cases (single token structure-less constructs)  
;-------------------------------------------------------------------------------
(define (@yy_trivial)
 (let ((/type (gethash //Type_/Table /token1)))
  (@yy_lex)
  (@Make /type '() '())))

;-------------------------------------------------------------------------------
; Functions to parse an IF, D_IF or D_DO statement                              
;-------------------------------------------------------------------------------
(define (@yy_if)
 (let ((/guards '())
       (/cond '())
       (/body '())
       (/type '()))
  (@yy_lex)
  (cond
   ((= (wsl-ref //Patterns /token1) 1)
    (set! /guards (list (@yy_guarded_pattern (list //S_/T/H/E/N)))))
   (#t
    (set! /cond (@yy_condition))
    (@yy_skip_symbol //S_/T/H/E/N "Missing `THEN'")
    (set! /body (@yy_statements))
    (set! /guards (list (@Make //T_/Guarded '() (list /cond /body))))))
  (while (= /token1 //S_/E/L/S/I/F) 
   (set! /guards (cons (@yy_guarded) /guards)))
  (cond
   ((= /token1 //S_/E/L/S/E)
    (set! /guards (cons (@yy_guarded) /guards)))
   (#t
    (set! /cond (@Make //T_/True '() '()))
    (set! /body (@Skips))
    (set! /guards (cons (@Make //T_/Guarded '() (list /cond /body)) /guards))))
  (@yy_skip_symbol //S_/F/I "Duff guard syntax or un-terminated IF")
  (@Make //T_/Cond '() (reverse /guards))))

; Generic function for Dijkstra If or DO 
(define (@yy_d_generic /type /terminator)
 (let ((//Guards '())
       (//Cond '())
       (//Body '()))
  (@yy_lex)
  (set! //Cond (@yy_condition))
  (@yy_skip_symbol //S_/A/R/R/O/W "Missing ->")
  (set! //Body (@yy_statements))
  (set! //Guards (list (@Make //T_/Guarded '() (list //Cond //Body))))
  (while (= /token1 //S_/B/O/X) 
   (set! //Guards (cons (@yy_guarded) //Guards)))
  (@yy_skip_symbol /terminator "Duff guard syntax or un-terminated `Dijkstra'-construct")
  (@Make /type '() (reverse //Guards))))

; Guards in an IF have different terminals to D_IF and D_DO 
(define (@yy_guarded)
 (let ((/condition '())
       (/body '())
       (/type '())
       (//S '()))
  (cond
   ((= /token1 //S_/E/L/S/I/F)
    (@yy_lex)
    (set! //S (@yy_guard_gen (list //S_/T/H/E/N))))
   ((= /token1 //S_/B/O/X)
    (@yy_lex)
    (set! //S (@yy_guard_gen (list //S_/A/R/R/O/W))))
   ((= /token1 //S_/E/L/S/E)
    (set! /condition (@Make //T_/True '() '()))
    (@yy_lex)
    (set! /body (@yy_statements))
    (set! //S (@Make //T_/Guarded '() (list /condition /body)))))
  //S))

; Generic function to parse a guard 
(define (@yy_guard_gen /then_sym)
 (let ((/condition '())
       (/body '())
       (//S '()))
  (cond
   ((= (wsl-ref //Patterns /token1) 1)
    (set! //S (@yy_guarded_pattern /then_sym)))
   (#t
    (set! /condition (@yy_condition))
    (cond
     ((member /token1 /then_sym)
      (@yy_lex))
     (#t
      (@Syntax_Error "Missing delimiter in guard")))
    (set! /body (@yy_statements))
    (set! //S (@Make //T_/Guarded '() (list /condition /body)))))
  //S))

; Check the type of pattern and set the name or comps as appropriate: 
(define (@yy_parse_pattern /type /name-par /comps-par)
 (let ((/comps-save /comps)
       (/name-save /name)
       (funct-result '()))
  (set! /comps /comps-par)
  (set! /name /name-par)
  (@yy_lex)
  (cond
   ((or (equal? /type //S_/I/N/T_/O/N/E) (equal? /type //S_/I/N/T_/A/N/Y))
    (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing '(' in pattern interpolation")
    (set! /comps (list (@yy_expression)))
    (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing ')' in pattern interpolation"))
   ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
    (set! /name (@Make_Name /token2))
    (@yy_lex))
   ((= /token1 //S_/N/U/M/B/E/R)
    (set! /name (- /token2))
    (@yy_lex))
   (#t
    (@Syntax_Error "Missing name in pattern")
    (set! /name (@Make_Name "_Missing_"))))
  (set! funct-result (list /name /comps))
  (set! /comps /comps-save)
  (set! /name /name-save)
  funct-result))

; Function for guarded which begins with a pattern 
; This may be a guarded, condition or expression pattern 
(define (@yy_guarded_pattern /then_sym)
 (let ((/type /token1)
       (/cond '())
       (/body '())
       (/name-save /name)
       (//S '())
       (/comps-save /comps)
       (funct-result '()))
  (set! /name '())
  (set! /comps '())
  (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
   (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
  (cond
   ((or (= /token1 //S_/E/L/S/I/F) (= /token1 //S_/B/O/X) (= /token1 //S_/E/L/S/E) (= /token1 //S_/F/I) (= /token1 //S_/E/N/D/F/I/L/L))
    ; It's a guarded pattern 
    (set! /type (gethash /pattern_type (list //T_/Guarded /type)))
    (set! //S (@Make /type /name /comps)))
   (#t
    ; Parse the condition, then the statements 
    (cond
     ((member /token1 /then_sym)
      ; It's a condition pattern 
      (set! /type (gethash /pattern_type (list //T_/Condition /type)))
      (set! /cond (@Make /type /name /comps)))
     ((or (= /token1 //S_/E/X/P/O/N/E/N/T) (= (wsl-ref //Term_/Ops /token1) 1) (= (wsl-ref //Factor_/Ops /token1) 1) (= (wsl-ref //S_/Term_/Ops /token1) 1) (= (wsl-ref //S_/Factor_/Ops /token1) 1) (= (wsl-ref //S_/Atom_/Ops /token1) 1) (= (wsl-ref //Rel_/Ops /token1) 1))
      ; It's an expression pattern at the start of a condition
      (set! /type (gethash /pattern_type (list //T_/Expression /type)))
      (set! /cond (@yy_rest_of_cond (@Make /type /name /comps))))
     (#t
      ; Assume it's a condition pattern 
      (set! /type (gethash /pattern_type (list //T_/Condition /type)))
      (set! /cond (@yy_rest_of_cond (@Make /type /name /comps)))))
    (cond
     ((not-member /token1 /then_sym)
      (@Syntax_Error "Missing `THEN' or `[]'"))
     (#t
      (@yy_lex)))
    (set! /body (@yy_statements))
    (set! //S (@Make //T_/Guarded '() (list /cond /body)))))
  (set! funct-result //S)
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

;-------------------------------------------------------------------------------
; While - loop 
;-------------------------------------------------------------------------------
(define (@yy_while)
 (let ((/condition '())
       (/body '()))
  (@yy_lex)
  (set! /condition (@yy_condition))
  (@yy_skip_symbol //S_/D/O "Missing `DO' in while-loop")
  (set! /body (@yy_statements))
  (@yy_skip_symbol //S_/O/D "Unterminated WHILE loop")
  (@Make //T_/While '() (list /condition /body))))

;-------------------------------------------------------------------------------
;  DO loop                                                                      
;-------------------------------------------------------------------------------
(define (@yy_floop)
 (let ((/body '()))
  (@yy_lex)
  (set! /body (@yy_statements))
  (@yy_skip_symbol //S_/O/D "Missing `OD'")
  (@Make //T_/Floop '() (list /body))))

;-------------------------------------------------------------------------------
;  Exit                                                                         
;-------------------------------------------------------------------------------
(define (@yy_exit)
 (let ((/n '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' in exit statement")
  (cond
   ((not (= /token1 //S_/N/U/M/B/E/R))
    (@Syntax_Error "Numeric argument to EXIT expected")
    (set! /n (- 1)))
   ((not (equal? (quotient /token2 1) /token2))
    (@Syntax_Error (string-append (string-append "Natural number argument to EXIT required, got " /token2) " instead"))
    (set! /n (- 1))
    (@yy_lex))
   (#t
    (set! /n /token2)
    (@yy_lex)))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)' in EXIT statement")
  (@Make //T_/Exit /n '())))

;-------------------------------------------------------------------------------
;  FOR loop                                                                     
;-------------------------------------------------------------------------------
(define (@yy_for)
 (let ((/comps-save /comps)
       (/index '())
       (//S '())
       (funct-result '()))
  (set! /comps '())
  (@yy_lex)
  (cond
   ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
    (set! /index (@Make //T_/Var_/Lvalue (@Make_Name /token2) '()))
    (@yy_lex))
   ((= /token1 //S_/P/A/T_/O/N/E)
    (@yy_lex)
    (set! /index (@Make //T_/Lvalue_/Pat_/One (@Make_Name /token2) '()))
    (@yy_lex))
   (#t
    (@Syntax_Error "Missing for-loop index variable")
    (set! /index (@Make //T_/Var_/Lvalue (@Make_Name "_Missing_") '()))))
  (cond
   ((= /token1 //S_/I/N)
    (set! //S (@yy_forin /index)))
   (#t
    (@yy_skip_symbol //S_/B/E/C/O/M/E/S "Missing `:='")
    (set! /comps (list (@yy_expression)))
    (@yy_skip_symbol //S_/T/O "Missing `TO'")
    (set! /comps (cons (@yy_expression) /comps))
    (@yy_skip_symbol //S_/S/T/E/P "Missing `STEP'")
    (set! /comps (cons (@yy_expression) /comps))
    (@yy_skip_symbol //S_/D/O "Missing `DO'")
    (set! /comps (cons (@yy_statements) /comps))
    (@yy_skip_symbol //S_/O/D "Missing `OD'")
    (set! //S (@Make //T_/For '() (cons /index (reverse /comps))))))
  (set! funct-result //S)
  (set! /comps /comps-save)
  funct-result))

;-------------------------------------------------------------------------------
;  FOR I in S ...                                                               
;-------------------------------------------------------------------------------
(define (@yy_forin /index)
 (let ((/set '())
       (/body '()))
  (@yy_lex)
  (set! /set (@yy_expression))
  (@yy_skip_symbol //S_/D/O "Missing `DO'")
  (set! /body (@yy_statements))
  (@yy_skip_symbol //S_/O/D "Missing `OD'")
  (@Make //T_/For_/In '() (list /index /set /body))))

;-------------------------------------------------------------------------------
;  VAR block                                                                    
;-------------------------------------------------------------------------------
; As usual, most of this is error checking ;-) 
(define (@yy_var)
 (let ((/assigns '())
       (/body '())
       (/delimit 0))
  (@yy_lex)
  ; The assigns part may of may not have angle brackets round it 
  (cond
   ((= /token1 //S_/L/A/N/G/L/E)
    (set! /delimit 1)
    (@yy_lex)))
  (set! /assigns (@yy_assigns))
  (cond
   ((equal? /assigns '())
    (@Warning "No local variables in VAR")))
  (set! /assigns (@Make //T_/Assigns '() /assigns))
  (cond
   ((= /delimit 1)
    (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>' after assigns")))
  (@yy_skip_symbol //S_/C/O/L/O/N "Missing `:' after assigns")
  (set! /body (@yy_statements))
  (@yy_skip_symbol //S_/E/N/D/V/A/R "Missing `ENDVAR'")
  (@Make //T_/Var '() (list /assigns /body))))

;-------------------------------------------------------------------------------
;  Assertions                                                                   
;-------------------------------------------------------------------------------
(define (@yy_assert)
 (let ((/cond '()))
  (@yy_lex)
  (set! /cond (@yy_condition))
  (@yy_skip_symbol //S_/R/B/R/A/C/E "Missing ``}'' in assertion")
  (@Make //T_/Assert '() (list /cond))))

;-------------------------------------------------------------------------------
;  Comments                                                                     
;-------------------------------------------------------------------------------
(define (@yy_comment)
 (let ((//Text '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/C/O/L/O/N "Missing `:' in COMMENT statement")
  (cond
   ((not (= /token1 //S_/S/T/R/I/N/G))
    (@Syntax_Error "Missing comment text")
    ;Try to recover by eating up tokens until
    ;something that looks like a new statement
    (set! //Text " ")
    (while (and (not (= /token1 //S_/S/E/M/I/C/O/L/O/N)) (= (wsl-ref //Reserved_/Words /token1) 0)) 
     (@yy_lex)))
   (#t
    (set! //Text /token2)
    (@yy_lex)))
  (@Make //T_/Comment //Text '())))

;-------------------------------------------------------------------------------
;  Assignments                                                                  
;-------------------------------------------------------------------------------
; This first function does single assignments and puthash statements 
(define (@yy_single_assign //Lhs)
 (let ((/key '())
       (/value '())
       (//S '()))
  (set! /fl_flag1 0)
  (while (= /fl_flag1 0) 
   (cond
    ((= /token1 //S_/L/B/R/A/C/K/E/T)
     (set! //Lhs (@yy_lv_array_ref //Lhs))
     (set! /fl_flag1 0))
    ((= /token1 //S_/F/U/L/L/S/T/O/P)
     (@yy_lex)
     (cond
      ((= /token1 //S_/L/P/A/R/E/N)
       (set! /key (@yy_expression))
       ; key is also used as a flag to indicate
       ; that we need to build a puthash.
       (set! /fl_flag1 1))
      (#t
       (set! //Lhs (@yy_lv_struct_ref //Lhs))
       (set! /fl_flag1 0))))
    (#t
     (set! /fl_flag1 1))))
  (cond
   ((= /token1 //S_/B/E/C/O/M/E/S)
    (cond
     ((not (equal? /key '()))
      (@yy_lex)
      (set! /value (@yy_expression))
      (set! //S (@Make //T_/Puthash '() (list //Lhs /key /value))))
     (#t
      (set! //S (@Make //T_/Assignment '() (list (@yy_infix_op //Lhs //T_/Assign)))))))
   (#t
    (@Syntax_Error "`:=' expected")
    (set! //S (@Make //T_/Stat_/Place '() '()))
    (while (and (not (= /token1 //S_/S/E/M/I/C/O/L/O/N)) (not (= /token1 //S_/E/O/F))) 
     (@yy_lex))))
  //S))

(define (@yy_assignment)
 (let ((/assigns '()))
  (@yy_lex)
  (set! /assigns (@yy_assigns))
  (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>' after assignments")
  (@Make //T_/Assignment '() /assigns)))

(define (@yy_assigns)
 (let ((/assigns '())
       (/flag 0))
  ; May be called while sitting on a `<' which surrounds the assigns 
  (cond
   ((= /token1 //S_/L/A/N/G/L/E)
    (set! /flag 1)
    (@yy_lex)))
  (set! /assigns (list (@yy_assign)))
  (while (= /token1 //S_/C/O/M/M/A) 
   (begin
    (@yy_lex)
    (set! /assigns (cons (@yy_assign) /assigns))))
  (cond
   ((= /flag 1)
    (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>'")))
  (reverse /assigns)))

(define (@yy_assign)
 (let ((/lhs '())
       (//S '()))
  (cond
   ((= (wsl-ref //Patterns /token1) 1)
    (set! //S (@yy_assign_pattern)))
   (#t
    (cond
     ((and (not (= /token1 //S_/I/D/E/N/T/I/F/I/E/R)) (not (= /token1 //S_/M/E/M)))
      (@Syntax_Error "Variable name expected")
      (set! /lhs (@Make //T_/Var_/Place '() '())))
     (#t
      (set! /lhs (@yy_lvalue))
      (cond
       ((not (= /token1 //S_/B/E/C/O/M/E/S))
        (@Syntax_Error "Missing assignment operator")))))
    (set! //S (@yy_infix_op /lhs //T_/Assign))))
  //S))

(define (@yy_assign_pattern)
 (let ((//S '())
       (/name-save /name)
       (/type /token1)
       (/lhs '())
       (/comps-save /comps)
       (funct-result '()))
  (set! /name '())
  (set! /comps '())
  ; Could be an assign pattern or a lvalue pattern on lhs of assign.
  (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
   (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
  (cond
   ((= /token1 //S_/B/E/C/O/M/E/S)
    (set! /type (gethash /pattern_type (list //T_/Lvalue /type)))
    (set! /lhs (@Make /type /name /comps))
    (set! //S (@yy_infix_op /lhs //T_/Assign)))
   ((or (= /token1 //S_/L/B/R/A/C/K/E/T) (= /token1 //S_/F/U/L/L/S/T/O/P))
    ; Some kind of array or struct: so can't be an assign pattern 
    (set! /type (gethash /pattern_type (list //T_/Lvalue /type)))
    (set! /lhs (@yy_lv_array_check (@Make /type /name /comps)))
    (cond
     ((not (= /token1 //S_/B/E/C/O/M/E/S))
      (@Syntax_Error "Missing assignment operator")))
    (set! //S (@yy_infix_op /lhs //T_/Assign)))
   (#t
    (set! /type (gethash /pattern_type (list //T_/Assign /type)))
    (set! //S (@Make /type /name /comps))))
  (set! funct-result //S)
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

(define (@yy_lv_array_check //I)
 (let ((//S //I))
  (while (or (= //S_/F/U/L/L/S/T/O/P /token1) (= //S_/L/B/R/A/C/K/E/T /token1)) 
   (cond
    ((= /token1 //S_/L/B/R/A/C/K/E/T)
     (set! //S (@yy_lv_array_ref //S)))
    (#t
     (@yy_lex)
     (set! //S (@yy_lv_struct_ref //S)))))
  //S))

(define (@yy_infix_op /lhs /type)
 (let ((/rhs '()))
  (@yy_lex)
  (set! /rhs (@yy_expression))
  (@Make /type '() (list /lhs /rhs))))

;-------------------------------------------------------------------------------
;  Push                                                                         
;-------------------------------------------------------------------------------
(define (@yy_push)
 (let ((/args-save /args)
       (funct-result '()))
  (set! /args '())
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (set! /args (list (@yy_lvalue)))
  (@yy_skip_symbol //S_/C/O/M/M/A "Missing `,'")
  (set! /args (cons (@yy_expression) /args))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (set! funct-result (@Make //T_/Push '() (reverse /args)))
  (set! /args /args-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Pop                                                                          
;-------------------------------------------------------------------------------
(define (@yy_pop)
 (let ((/args-save /args)
       (funct-result '()))
  (set! /args '())
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (set! /args (list (@yy_lvalue)))
  (@yy_skip_symbol //S_/C/O/M/M/A "Missing `,'")
  (set! /args (cons (@yy_lvalue) /args))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (set! funct-result (@Make //T_/Pop '() (reverse /args)))
  (set! /args /args-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Join                                                                         
;-------------------------------------------------------------------------------
(define (@yy_join)
 (let ((/args-save /args)
       (funct-result '()))
  (set! /args '())
  (@yy_lex)
  (set! /args (list (@yy_statements)))
  (@yy_skip_symbol //S_/C/O/M/M/A "Missing `,'")
  (set! /args (cons (@yy_statements) /args))
  (@yy_skip_symbol //S_/E/N/D/J/O/I/N "Missing `)'")
  (set! funct-result (@Make //T_/Join '() (reverse /args)))
  (set! /args /args-save)
  funct-result))

; Parse a name or name pattern and return it 
; thing is the type of thing that the name is part of, eg `!P call' 
(define (@yy_name /thing)
 (let ((/name-save /name)
       (/type /token1)
       (/comps-save /comps)
       (funct-result '()))
  (set! /name '())
  (set! /comps '())
  (cond
   ((= (wsl-ref //Patterns /token1) 1)
    (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
     (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
    (cond
     ((null? (gethash /pattern_type (list //T_/Name /type)))
      (@Syntax_Error (string-append "Invalid pattern for Name in " /thing))
      (set! /name (@Name (@Make_Name "_missing_"))))
     (#t
      (set! /name (@Make (gethash /pattern_type (list //T_/Name /type)) /name /comps)))))
   ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
    (set! /name (@Name (@Make_Name /token2)))
    (@yy_lex))
   (#t
    (@Syntax_Error (string-append "Missing name in " /thing))
    (set! /name (@Name (@Make_Name "_Missing_")))))
  (set! funct-result /name)
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Action system                                                                
;-------------------------------------------------------------------------------
(define (@yy_as)
 (let ((/actions '())
       (/name-save /name)
       (funct-result '()))
  (set! /name '())
  (@yy_lex)
  (set! /name (@yy_name "action system"))
  (@yy_skip_symbol //S_/C/O/L/O/N "Missing `:'")
  (set! /actions (@yy_actions))
  (set! funct-result (@Make //T_/A_/S '() (list /name /actions)))
  (set! /name /name-save)
  funct-result))

(define (@yy_actions)
 (let ((/actions (list (@yy_action))))
  (while (and (not (= /token1 //S_/E/N/D/A/C/T/I/O/N/S)) (not (= /token1 //S_/E/O/F))) 
   (cond
    ((not (= /token1 //S_/E/N/D/A/C/T/I/O/N/S))
     (set! /actions (cons (@yy_action) /actions)))))
  (@yy_skip_symbol //S_/E/N/D/A/C/T/I/O/N/S "Missing `ENDACTIONS'")
  (@Make //T_/Actions '() (reverse /actions))))

; We could have a name pattern or an action pattern 
; depending on what comes next. 
(define (@yy_action)
 (let ((/name-save /name)
       (/body '())
       (/type /token1)
       (//S '())
       (/comps-save /comps)
       (funct-result '()))
  (set! /name '())
  (set! /comps '())
  (cond
   ((= (wsl-ref //Patterns /token1) 1)
    (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
     (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
    (cond
     ((= /token1 //S_/D/E/F/I/N/E)
      (cond
       ((null? (gethash /pattern_type (list //T_/Name /type)))
        (@Syntax_Error "Invalid pattern for Name in action")
        (set! /name (@Name (@Make_Name "_missing_"))))
       (#t
        (set! /name (@Make (gethash /pattern_type (list //T_/Name /type)) /name /comps)))))
     (#t
      (set! //S (@Make (gethash /pattern_type (list //T_/Action /type)) /name /comps)))))
   (#t
    (cond
     ((not (= /token1 //S_/I/D/E/N/T/I/F/I/E/R))
      (@Syntax_Error "Missing action name")
      (set! /name (@Name (@Make_Name "_missing_"))))
     (#t
      (set! /name (@Name (@Make_Name /token2)))))
    (@yy_lex)))
  (cond
   ((null? //S)
    ; Parse the action body: 
    (@yy_skip_symbol //S_/D/E/F/I/N/E "Missing `==' in action")
    (set! /body (@yy_statements))
    (cond
     ((and (not (= /token1 //S_/D/O/T/S/P/A/C/E)) (not (= /token1 //S_/E/N/D)))
      (@Syntax_Error "Missing `.' at end of action"))
     (#t
      (@yy_lex)))
    (set! //S (@Make //T_/Action '() (list /name /body)))))
  (set! funct-result //S)
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Action call                                                                  
;-------------------------------------------------------------------------------
(define (@yy_call)
 (let ((/name-save /name)
       (funct-result '()))
  (set! /name '())
  (@yy_lex)
  (cond
   ((not (= /token1 //S_/I/D/E/N/T/I/F/I/E/R))
    (@Syntax_Error "Missing action name")
    (set! /name (@Make_Name "_Missing_")))
   (#t
    (set! /name (@Make_Name /token2))
    (@yy_lex)))
  (set! funct-result (@Make //T_/Call /name '()))
  (set! /name /name-save)
  funct-result))

;-------------------------------------------------------------------------------
;  PRINT and PRINFLUSH                                                          
;-------------------------------------------------------------------------------
(define (@yy_gen_print /type)
 (let ((/args-save /args)
       (funct-result '()))
  (set! /args '())
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (set! /args (@yy_expressions))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (set! funct-result (@Make /type '() (list /args)))
  (set! /args /args-save)
  funct-result))

;-------------------------------------------------------------------------------
;  MW PROC                                                                      
;-------------------------------------------------------------------------------
(define (@yy_mw_proc)
 (let ((/name-save /name)
       (/body '())
       (funct-result '()))
  (set! /name '())
  (cond
   (#f
    (display-list "MW Proc: token1 = " /token1 " token2 = " /token2)))
  (@yy_lex)
  (set! /name (@yy_at_name))
  (set! /body (@yy_generic_proc))
  (set! funct-result (@Make //T_/M/W_/Proc '() (cons /name /body)))
  (set! /name /name-save)
  funct-result))

(define (@yy_at_name)
 (let ((/name-save /name)
       (funct-result '()))
  (set! /name '())
  (cond
   ((= /token1 //S_/A/T)
    (set! /name (@Make //T_/Name (@Make_Name (string-append "@" /token2)) '()))
    (@yy_lex))
   ((= /token1 //S_/A/T_/P/A/T_/O/N/E)
    (set! /name (@Make //T_/Name_/Pat_/One (@Make_Name /token2) '()))
    (@yy_lex))
   (#t
    (@Syntax_Error "Missing `@' symbol in proc name")
    (cond
     ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
      (set! /name (@Make //T_/Name (@Make_Name (string-append "@" /token2)) '()))
      (@yy_lex)))))
  (set! funct-result /name)
  (set! /name /name-save)
  funct-result))

;-------------------------------------------------------------------------------
;  MW FUNCT                                                                     
;-------------------------------------------------------------------------------
(define (@yy_mw_funct)
 (let ((/name-save /name)
       (/body '())
       (funct-result '()))
  (set! /name '())
  (@yy_lex)
  (cond
   ((= /token1 //S_/A/T)
    (set! /name (@Make_Name (string-append "@" /token2)))
    (@yy_lex))
   (#t
    (@Syntax_Error "Missing `@' symbol in funct. name")
    (cond
     ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
      (set! /name (@Make_Name (string-append "@" /token2)))
      (@yy_lex))
     (#t
      (@Syntax_Error "Missing funct. name")
      (set! /name (@Make_Name "@_Missing_"))))))
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `('")
  (cond
   ((= /token1 //S_/R/P/A/R/E/N)
    (set! /args (@Make //T_/Lvalues '() '())))
   (#t
    (set! /args (@yy_lvalues))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "`)' expected")
  (@yy_skip_symbol //S_/D/E/F/I/N/E "Missing `=='")
  (cond
   ((= /token1 //S_/V/A/R)
    (@yy_lex)
    (let ((/flag 0))
     (cond
      ((= /token1 //S_/L/A/N/G/L/E)
       (set! /flag 1)
       (@yy_lex)))
     (cond
      ((and (not (= /token1 //S_/R/A/N/G/L/E)) (not (= /token1 //S_/C/O/L/O/N)))
       (set! /ass (@Make //T_/Assigns '() (@yy_assigns))))
      (#t
       (set! /ass (@Make //T_/Assigns '() '()))))
     (cond
      ((= /flag 1)
       (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>'")))))
   (#t
    (set! /ass (@Make //T_/Assigns '() '()))))
  (@yy_skip_symbol //S_/C/O/L/O/N "Missing `:'")
  (set! /body (@yy_statements))
  (set! /exp (@yy_expression))
  (cond
   ((and (not (= /token1 //S_/F/U/L/L/S/T/O/P)) (not (= /token1 //S_/D/O/T/S/P/A/C/E)) (not (= /token1 //S_/E/N/D)))
    (@Syntax_Error "Missing end delimiter for MW Funct"))
   (#t
    (@yy_lex)))
  (set! funct-result (@Make //T_/M/W_/Funct '() (list (@Make //T_/Name /name '()) /args /ass /body /exp)))
  (set! /name /name-save)
  funct-result))

;-------------------------------------------------------------------------------
;  MW BFUNCT                                                                     
;-------------------------------------------------------------------------------
(define (@yy_mw_bfunct)
 (let ((/name-save /name)
       (/args-save /args)
       (/ass-save /ass)
       (/body '())
       (/cond '())
       (funct-result '()))
  (set! /name '())
  (set! /args '())
  (set! /ass '())
  (@yy_lex)
  (cond
   ((= /token1 //S_/A/T)
    (set! /name (@Make_Name (string-append (string-append "@" /token2) "?")))
    (@yy_lex))
   (#t
    (@Syntax_Error "Missing `@' symbol in b_funct. name")
    (cond
     ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
      (set! /name (@Make_Name (string-append (string-append "@" /token2) "?")))
      (@yy_lex))
     (#t
      (@Syntax_Error "Missing bfunct. name")
      (set! /name (@Make_Name "@_Missing_?"))))))
  (@yy_skip_symbol //S_/Q/U/E/R/Y "Missing `?' after bfunct. name")
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `('")
  (cond
   ((= /token1 //S_/R/P/A/R/E/N)
    (set! /args (@Make //T_/Lvalues '() '())))
   (#t
    (set! /args (@yy_lvalues))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "`)' expected")
  (@yy_skip_symbol //S_/D/E/F/I/N/E "Missing `=='")
  (cond
   ((= /token1 //S_/V/A/R)
    (@yy_lex)
    (let ((/flag 0))
     (cond
      ((= /token1 //S_/L/A/N/G/L/E)
       (set! /flag 1)
       (@yy_lex)))
     (cond
      ((and (not (= /token1 //S_/R/A/N/G/L/E)) (not (= /token1 //S_/C/O/L/O/N)))
       (set! /ass (@Make //T_/Assigns '() (@yy_assigns))))
      (#t
       (set! /ass (@Make //T_/Assigns '() '()))))
     (cond
      ((= /flag 1)
       (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>'")))))
   (#t
    (set! /ass (@Make //T_/Assigns '() '()))))
  (@yy_skip_symbol //S_/C/O/L/O/N "Missing `:'")
  (set! /body (@yy_statements))
  (set! /cond (@yy_condition))
  (cond
   ((and (not (= /token1 //S_/F/U/L/L/S/T/O/P)) (not (= /token1 //S_/D/O/T/S/P/A/C/E)) (not (= /token1 //S_/E/N/D)))
    (@Syntax_Error "Missing end delimiter for BFunct"))
   (#t
    (@yy_lex)))
  (set! funct-result (@Make //T_/M/W_/B/Funct '() (list (@Make //T_/Name /name '()) /args /ass /body /cond)))
  (set! /name /name-save)
  (set! /args /args-save)
  (set! /ass /ass-save)
  funct-result))

;-------------------------------------------------------------------------------
; Generic function for proc/mw_proc                                             
;-------------------------------------------------------------------------------
(define (@yy_generic_proc)
 (let ((/vals '())
       (/vars '())
       (/body '()))
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `('")
  (cond
   ((= /token1 //S_/R/P/A/R/E/N)
    #t)
   (#t
    (cond
     ((= /token1 //S_/V/A/R)
      (set! /vals (@Make //T_/Lvalues '() '())))
     (#t
      (set! /vals (@yy_lvalues))))
    (cond
     ((= /token1 //S_/V/A/R)
      (@yy_lex)
      (cond
       ((= /token1 //S_/R/P/A/R/E/N)
        (set! /vars (@Make //T_/Lvalues '() '())))
       (#t
        (set! /vars (@yy_lvalues))))))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "`)' expected")
  (@yy_skip_symbol //S_/D/E/F/I/N/E "Missing `=='")
  (set! /body (@yy_statements))
  (cond
   ((and (not (= /token1 //S_/F/U/L/L/S/T/O/P)) (not (= /token1 //S_/D/O/T/S/P/A/C/E)) (not (= /token1 //S_/E/N/D)))
    (@Syntax_Error "Missing `END'"))
   (#t
    (@yy_lex)))
  (cond
   ((equal? /vals '())
    (set! /vals (@Make //T_/Lvalues '() '()))))
  (cond
   ((equal? /vars '())
    (set! /vars (@Make //T_/Lvalues '() '()))))
  (list /vals /vars /body)))

;-------------------------------------------------------------------------------
;  WHERE block                                                                  
;-------------------------------------------------------------------------------
(define (@yy_where)
 (let ((//S '())
       (/defines '()))
  (@yy_lex)
  (set! //S (@yy_statements))
  (@yy_skip_symbol //S_/W/H/E/R/E "Missing `WHERE'")
  (set! /defines (@yy_defines))
  (@yy_skip_symbol //S_/E/N/D "Missing `END' after WHERE-block")
  (@Make //T_/Where '() (list //S /defines))))

; The commas separating the definitions are now optional 
(define (@yy_defines)
 (let ((/d (list (@yy_define))))
  (while (or (= /token1 //S_/P/R/O/C) (= /token1 //S_/F/U/N/C/T) (= /token1 //S_/B/F/U/N/C/T) (= /token1 //S_/C/O/M/M/A)) 
   (begin
    (cond
     ((= /token1 //S_/C/O/M/M/A)
      (@yy_lex)))
    (set! /d (cons (@yy_define) /d))))
  (@Make //T_/Definitions '() (reverse /d))))

(define (@yy_define)
 (let ((//S '()))
  (cond
   ((= (wsl-ref //Patterns /token1) 1)
    (set! //S (@yy_defn_pattern)))
   ((= /token1 //S_/P/R/O/C)
    (set! //S (@yy_proc)))
   ((= /token1 //S_/F/U/N/C/T)
    (set! //S (@yy_funct)))
   ((= /token1 //S_/B/F/U/N/C/T)
    (set! //S (@yy_bfunct)))
   (#t
    (@Syntax_Error "Expected PROC, FUNCT or BFUNCT definition")
    (@yy_lex)))
  //S))

(define (@yy_defn_pattern)
 (let ((/name-save /name)
       (/type /token1)
       (/comps-save /comps)
       (funct-result '()))
  (set! /name '())
  (set! /comps '())
  (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
   (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
  (set! /type (gethash /pattern_type (list //T_/Definition /type)))
  (set! funct-result (@Make /type /name /comps))
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

(define (@yy_proc)
 (let ((/name-save /name)
       (/body '())
       (funct-result '()))
  (set! /name '())
  (@yy_lex)
  (set! /name (@yy_name "PROC"))
  (set! /body (@yy_generic_proc))
  (set! funct-result (@Make //T_/Proc '() (cons /name /body)))
  (set! /name /name-save)
  funct-result))

(define (@yy_funct)
 (let ((/name-save /name)
       (/args-save /args)
       (/ass-save /ass)
       (/body '())
       (/exp-save /exp)
       (funct-result '()))
  (set! /name '())
  (set! /args '())
  (set! /ass '())
  (set! /exp '())
  (@yy_lex)
  (set! /name (@yy_name "FUNCT definition"))
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `('")
  (cond
   ((= /token1 //S_/R/P/A/R/E/N)
    (set! /args (@Make //T_/Lvalues '() '())))
   (#t
    (set! /args (@yy_lvalues))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "`)' expected")
  (@yy_skip_symbol //S_/D/E/F/I/N/E "Missing `=='")
  (cond
   ((= /token1 //S_/V/A/R)
    (@yy_lex)
    (@yy_skip_symbol //S_/L/A/N/G/L/E "Missing `<'")
    (cond
     ((= /token1 //S_/R/A/N/G/L/E)
      (set! /ass (@Make //T_/Assigns '() '())))
     (#t
      (set! /ass (@Make //T_/Assigns '() (@yy_assigns)))))
    (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>'"))
   ((= /token1 //S_/C/O/L/O/N)
    (set! /ass (@Make //T_/Assigns '() '())))
   (#t
    (set! /ass (@yy_f_assigns))))
  (@yy_skip_symbol //S_/C/O/L/O/N "Missing `:'")
  (set! /body (@yy_statements))
  (set! /exp (@yy_expression))
  (cond
   ((and (not (= /token1 //S_/F/U/L/L/S/T/O/P)) (not (= /token1 //S_/D/O/T/S/P/A/C/E)) (not (= /token1 //S_/E/N/D)))
    (@Syntax_Error "Missing `END'"))
   (#t
    (@yy_lex)))
  (set! funct-result (@Make //T_/Funct '() (list /name /args /ass /body /exp)))
  (set! /name /name-save)
  (set! /args /args-save)
  (set! /ass /ass-save)
  (set! /exp /exp-save)
  funct-result))

(define (@yy_bfunct)
 (let ((/name-save /name)
       (/args-save /args)
       (/ass-save /ass)
       (/body '())
       (/cond '())
       (funct-result '()))
  (set! /name '())
  (set! /args '())
  (set! /ass '())
  (@yy_lex)
  (set! /name (@yy_name "BFUNCT definition"))
  (cond
   ((= /token1 //S_/Q/U/E/R/Y)
    (cond
     ((= (@ST /name) //T_/Name)
      (set! /name (@Name (@Make_Name (string-append (@N_String (@V /name)) "?"))))))
    (@yy_lex)))
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `('")
  (cond
   ((= /token1 //S_/R/P/A/R/E/N)
    (set! /args (@Make //T_/Lvalues '() '())))
   (#t
    (set! /args (@yy_lvalues))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "`)' expected")
  (@yy_skip_symbol //S_/D/E/F/I/N/E "Missing `=='")
  (cond
   ((= /token1 //S_/V/A/R)
    (@yy_lex)
    (@yy_skip_symbol //S_/L/A/N/G/L/E "Missing `<'")
    (cond
     ((= /token1 //S_/R/A/N/G/L/E)
      (set! /ass (@Make //T_/Assigns '() '())))
     (#t
      (set! /ass (@Make //T_/Assigns '() (@yy_assigns)))))
    (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>'"))
   ((= /token1 //S_/C/O/L/O/N)
    (set! /ass (@Make //T_/Assigns '() '())))
   (#t
    (set! /ass (@yy_f_assigns))))
  (@yy_skip_symbol //S_/C/O/L/O/N "Missing `:'")
  (set! /body (@yy_statements))
  (set! /cond (@yy_condition))
  (cond
   ((and (not (= /token1 //S_/F/U/L/L/S/T/O/P)) (not (= /token1 //S_/D/O/T/S/P/A/C/E)) (not (= /token1 //S_/E/N/D)))
    (@Syntax_Error "Missing `END'"))
   (#t
    (@yy_lex)))
  (set! funct-result (@Make //T_/B/Funct '() (list /name /args /ass /body /cond)))
  (set! /name /name-save)
  (set! /args /args-save)
  (set! /ass /ass-save)
  funct-result))

; Parse the alternative form of ASSIGNS allowed in funct. definitions 
(define (@yy_f_assigns)
 (let ((/assigns '()))
  (set! /assigns (list (@yy_assign)))
  (while (= /token1 //S_/S/E/M/I/C/O/L/O/N) 
   (begin
    (@yy_lex)
    (set! /assigns (cons (@yy_assign) /assigns))))
  (@Make //T_/Assigns '() (reverse /assigns))))

;-------------------------------------------------------------------------------
;  Proc call                                                                    
;-------------------------------------------------------------------------------
(define (@yy_proc_call /name-par)
 (let ((/name-save /name)
       (/comps-save /comps)
       (funct-result '()))
  (set! /name /name-par)
  (set! /comps (@yy_proc_arg_list))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (set! funct-result (@Make //T_/Proc_/Call '() (cons (@Make //T_/Name /name '()) /comps)))
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

(define (@yy_proc_arg_list)
 (let ((/vals '())
       (/vars '()))
  (@yy_lex)
  (cond
   ((= /token1 //S_/V/A/R)
    (@yy_lex)
    (set! /vals (@Make //T_/Expressions '() '())))
   ((= /token1 //S_/R/P/A/R/E/N)
    (set! /vals (@Make //T_/Expressions '() '())))
   (#t
    (set! /vals (@yy_expressions))
    (cond
     ((= /token1 //S_/V/A/R)
      (@yy_lex)))))
  (cond
   ((= /token1 //S_/R/P/A/R/E/N)
    (set! /vars (@Make //T_/Lvalues '() '())))
   (#t
    ; List of Lvalues ...
    (set! /vars (@yy_lvalues))))
  (list /vals /vars)))

;-------------------------------------------------------------------------------
;  !P call                                                                      
;-------------------------------------------------------------------------------
(define (@yy_aproc_call)
 (let ((/name-save /name)
       (/args-save /args)
       (funct-result '()))
  (set! /name '())
  (set! /args '())
  (@yy_lex)
  (set! /name (@yy_name "!P call"))
  (cond
   ((not (= /token1 //S_/L/P/A/R/E/N))
    (@Syntax_Error "Missing `(' or missing argument list"))
   (#t
    (set! /args (@yy_proc_arg_list))
    (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")))
  (set! funct-result (@Make //T_/A_/Proc_/Call '() (cons /name /args)))
  (set! /name /name-save)
  (set! /args /args-save)
  funct-result))

;-------------------------------------------------------------------------------
;  MW_Proc call                                                                 
;-------------------------------------------------------------------------------
(define (@yy_mw_proc_call)
 (let ((/name-save /name)
       (/args-save /args)
       (funct-result '()))
  (set! /name '())
  (set! /args '())
  (set! /name (@Make_Name (string-append "@" /token2)))
  (@yy_lex)
  (cond
   ((= /token1 //S_/L/P/A/R/E/N)
    (set! /args (@yy_proc_arg_list))
    (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'"))
   (#t
    (set! /args (list (@Make //T_/Expressions '() '()) (@Make //T_/Lvalues '() '())))))
  (set! funct-result (@Make //T_/M/W_/Proc_/Call '() (cons (@Make //T_/Name /name '()) /args)))
  (set! /name /name-save)
  (set! /args /args-save)
  funct-result))

;-------------------------------------------------------------------------------
;  !XP call                                                                     
;-------------------------------------------------------------------------------
(define (@yy_x_proc_call)
 (let ((/name-save /name)
       (/args-save /args)
       (funct-result '()))
  (set! /name '())
  (set! /args '())
  (@yy_lex)
  (cond
   ((not (= /token1 //S_/I/D/E/N/T/I/F/I/E/R))
    (@Syntax_Error "Missing procedure name in call")
    (set! /name (@Make_Name "_Missing_")))
   (#t
    (set! /name (@Make_Name /token2))
    (@yy_lex)))
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (cond
   ((not (= /token1 //S_/R/P/A/R/E/N))
    (set! /args (@yy_expressions))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (cond
   ((equal? /args '())
    (set! /args (@Make //T_/Expressions '() '()))))
  (set! funct-result (@Make //T_/X_/Proc_/Call '() (list (@Make //T_/Name /name '()) /args)))
  (set! /name /name-save)
  (set! /args /args-save)
  funct-result))

;-------------------------------------------------------------------------------
;  FOREACH                                                                      
;-------------------------------------------------------------------------------
(define (@yy_foreach)
 (let ((/type '())
       (/body '())
       (//S '()))
  (@yy_lex)
  (cond
   ((= /token1 //S_/S/T/A/T/E/M/E/N/T)
    (set! /type //T_/Foreach_/Stat)
    (@yy_lex))
   ((= /token1 //S_/S/T/A/T/E/M/E/N/T/S)
    (set! /type //T_/Foreach_/Stats)
    (@yy_lex))
   ((= /token1 //S_/V/A/R/I/A/B/L/E)
    (set! /type //T_/Foreach_/Variable)
    (@yy_lex))
   ((= /token1 //S_/L/V/A/L/U/E)
    (set! /type //T_/Foreach_/Lvalue)
    (@yy_lex))
   ((= /token1 //S_/S/T/S)
    (set! /type //T_/Foreach_/S/T/S)
    (@yy_lex))
   ((= /token1 //S_/N/A/S)
    (set! /type //T_/Foreach_/N/A/S)
    (@yy_lex))
   ((= /token1 //S_/E/X/P/R/E/S/S/I/O/N)
    (set! /type //T_/Foreach_/Expn)
    (@yy_lex))
   ((= /token1 //S_/C/O/N/D/I/T/I/O/N)
    (set! /type //T_/Foreach_/Cond)
    (@yy_lex))
   ((= /token1 //S_/T/E/R/M/I/N/A/L)
    (@yy_lex)
    (cond
     ((= /token1 //S_/S/T/A/T/E/M/E/N/T)
      (set! /type //T_/Foreach_/T/S)
      (@yy_lex))
     ((= /token1 //S_/S/T/A/T/E/M/E/N/T/S)
      (set! /type //T_/Foreach_/T/Ss)
      (@yy_lex))
     (#t
      (@Syntax_Error "Wrong type in `FOREACH Terminal'")
      (@yy_lex))))
   ((= /token1 //S_/G/L/O/B/A/L)
    (@yy_lex)
    (cond
     ((= /token1 //S_/V/A/R/I/A/B/L/E)
      (@yy_lex)
      (set! /type //T_/Foreach_/Global_/Var))
     (#t
      (@Syntax_Error "Wrong type in `FOREACH Global'")
      (@yy_lex))))
   (#t
    (@Syntax_Error "Wrong or missing type in `FOREACH'")
    (@yy_lex)))
  (@yy_skip_symbol //S_/D/O "Missing `DO' in `FOREACH'")
  (set! /body (@yy_statements))
  (@yy_skip_symbol //S_/O/D "Missing `OD' in `FOREACH'")
  (cond
   ((equal? /type '())
    (set! //S (@Make //T_/Stat_/Place '() '())))
   (#t
    (set! //S (@Make /type '() (list /body)))))
  //S))

;-------------------------------------------------------------------------------
;  ATEACH                                                                      
;-------------------------------------------------------------------------------
(define (@yy_ateach)
 (let ((/type '())
       (/body '())
       (//S '()))
  (@yy_lex)
  (cond
   ((= /token1 //S_/S/T/A/T/E/M/E/N/T)
    (set! /type //T_/Ateach_/Stat)
    (@yy_lex))
   ((= /token1 //S_/S/T/A/T/E/M/E/N/T/S)
    (set! /type //T_/Ateach_/Stats)
    (@yy_lex))
   ((= /token1 //S_/V/A/R/I/A/B/L/E)
    (set! /type //T_/Ateach_/Variable)
    (@yy_lex))
   ((= /token1 //S_/L/V/A/L/U/E)
    (set! /type //T_/Ateach_/Lvalue)
    (@yy_lex))
   ((= /token1 //S_/S/T/S)
    (set! /type //T_/Ateach_/S/T/S)
    (@yy_lex))
   ((= /token1 //S_/N/A/S)
    (set! /type //T_/Ateach_/N/A/S)
    (@yy_lex))
   ((= /token1 //S_/E/X/P/R/E/S/S/I/O/N)
    (set! /type //T_/Ateach_/Expn)
    (@yy_lex))
   ((= /token1 //S_/C/O/N/D/I/T/I/O/N)
    (set! /type //T_/Ateach_/Cond)
    (@yy_lex))
   ((= /token1 //S_/T/E/R/M/I/N/A/L)
    (@yy_lex)
    (cond
     ((= /token1 //S_/S/T/A/T/E/M/E/N/T)
      (set! /type //T_/Ateach_/T/S)
      (@yy_lex))
     ((= /token1 //S_/S/T/A/T/E/M/E/N/T/S)
      (set! /type //T_/Ateach_/T/Ss)
      (@yy_lex))
     (#t
      (@Syntax_Error "Wrong type in `ATEACH Terminal'")
      (@yy_lex))))
   ((= /token1 //S_/G/L/O/B/A/L)
    (@yy_lex)
    (cond
     ((= /token1 //S_/V/A/R/I/A/B/L/E)
      (@yy_lex)
      (set! /type //T_/Ateach_/Global_/Var))
     (#t
      (@Syntax_Error "Wrong type in `ATEACH Global'")
      (@yy_lex))))
   (#t
    (@Syntax_Error "Wrong or missing type in `ATEACH'")
    (@yy_lex)))
  (@yy_skip_symbol //S_/D/O "Missing `DO' in `ATEACH'")
  (set! /body (@yy_statements))
  (@yy_skip_symbol //S_/O/D "Missing `OD' in `ATEACH'")
  (cond
   ((equal? /type '())
    (set! //S (@Make //T_/Stat_/Place '() '())))
   (#t
    (set! //S (@Make /type '() (list /body)))))
  //S))

;-------------------------------------------------------------------------------
;  IFMATCH                                                                      
;-------------------------------------------------------------------------------
(define (@yy_ifmatch)
 (let ((//S '()))
  (@yy_lex)
  (cond
   ((= /token1 //S_/S/T/A/T/E/M/E/N/T)
    (set! //S (@yy_ifmatch_s //T_/Statement //T_/Ifmatch_/Stat)))
   ((= /token1 //S_/S/T/A/T/E/M/E/N/T/S)
    (set! //S (@yy_ifmatch_s //T_/Statements //T_/Ifmatch_/Stats)))
   ((= /token1 //S_/L/V/A/L/U/E)
    (set! //S (@yy_ifmatch_s //T_/Lvalue //T_/Ifmatch_/Lvalue)))
   ((= /token1 //S_/L/V/A/L/U/E/S)
    (set! //S (@yy_ifmatch_s //T_/Lvalues //T_/Ifmatch_/Lvalues)))
   ((= /token1 //S_/E/X/P/R/E/S/S/I/O/N)
    (set! //S (@yy_ifmatch_s //T_/Expression //T_/Ifmatch_/Expn)))
   ((= /token1 //S_/E/X/P/R/E/S/S/I/O/N/S)
    (set! //S (@yy_ifmatch_s //T_/Expressions //T_/Ifmatch_/Expns)))
   ((= /token1 //S_/C/O/N/D/I/T/I/O/N)
    (set! //S (@yy_ifmatch_s //T_/Condition //T_/Ifmatch_/Cond)))
   ((= /token1 //S_/A/S/S/I/G/N)
    (set! //S (@yy_ifmatch_s //T_/Assign //T_/Ifmatch_/Assign)))
   ((= /token1 //S_/A/S/S/I/G/N/S)
    (set! //S (@yy_ifmatch_s //T_/Assigns //T_/Ifmatch_/Assigns)))
   ((= /token1 //S_/D/E/F/I/N/I/T/I/O/N)
    (set! //S (@yy_ifmatch_s //T_/Definition //T_/Ifmatch_/Defn)))
   ((= /token1 //S_/D/E/F/I/N/I/T/I/O/N/S)
    (set! //S (@yy_ifmatch_s //T_/Definitions //T_/Ifmatch_/Defns)))
   ((= /token1 //S_/G/U/A/R/D/E/D)
    (set! //S (@yy_ifmatch_s //T_/Guarded //T_/Ifmatch_/Guarded)))
   ((= /token1 //S_/A/C/T/I/O/N)
    (set! //S (@yy_ifmatch_s //T_/Action //T_/Ifmatch_/Action)))
   (#t
    (@Syntax_Error "Incorrect type in `IFMATCH'")
    (set! //S (@Make //T_/Stat_/Place '() '()))))
  //S))

(define (@yy_ifmatch_s //G/T /result_type)
 (let ((/pat '())
       (/body '()))
  (@yy_lex)
  (set! /pat (@yy_parse //G/T))
  (set! /body (@yy_ifmatch_body))
  (@Make /result_type '() (cons /pat /body))))

(define (@yy_ifmatch_body)
 (let ((//S1 '())
       (//S2 '()))
  (@yy_skip_symbol //S_/T/H/E/N "Missing `THEN' in Ifmatch_Statement")
  (set! //S1 (@yy_statements))
  (cond
   ((= /token1 //S_/E/L/S/E)
    (@yy_lex)
    (set! //S2 (@yy_statements)))
   (#t
    (set! //S2 (@Skips))))
  (@yy_skip_symbol //S_/E/N/D/M/A/T/C/H "Missing `ENDMATCH'")
  (list //S1 //S2)))

;-------------------------------------------------------------------------------
;  Error                                                                        
;-------------------------------------------------------------------------------
(define (@yy_error)
 (let ((/args-save /args)
       (funct-result '()))
  (set! /args '())
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (set! /args (list (@yy_expressions)))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (set! funct-result (@Make //T_/Error '() /args))
  (set! /args /args-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Specification Statement                                                      
;-------------------------------------------------------------------------------
(define (@yy_spec)
 (let ((/vars '())
       (/cond '()))
  (@yy_lex)
  (@yy_skip_symbol //S_/L/A/N/G/L/E "Missing `<' in specification statement")
  (set! /vars (@yy_lvalues))
  (@yy_skip_symbol //S_/R/A/N/G/L/E "Missing `>' in specification statement")
  (@yy_skip_symbol //S_/C/O/L/O/N "Missing `:' in specification statement")
  (set! /cond (@yy_condition))
  (@yy_skip_symbol //S_/E/N/D/S/P/E/C "Missing `ENDSPEC' in specification statement")
  (@Make //T_/Spec '() (list /vars /cond))))

;-------------------------------------------------------------------------------
;  Maphash                                                                      
;-------------------------------------------------------------------------------
(define (@yy_maphash)
 (let ((/args-save /args)
       (funct-result '()))
  (set! /args '())
  (@yy_lex)
  (@yy_skip_symbol //S_/L/P/A/R/E/N "Missing `(' or argument list")
  (cond
   ((not (= /token1 //S_/S/T/R/I/N/G))
    (@Syntax_Error "Missing string argument in MAPHASH")
    (set! /args (list (@Name (@Make_Name "_Missing_")))))
   (#t
    (set! /args (list (@Name (@Make_Name /token2))))
    (@yy_lex)
    (@yy_skip_symbol //S_/C/O/M/M/A "Missing `,' or 2nd argument in MAPHASH")))
  (set! /args (concat /args (list (@yy_s_expression))))
  (@yy_skip_symbol //S_/R/P/A/R/E/N "Missing `)'")
  (set! funct-result (@Make //T_/Maphash '() /args))
  (set! /args /args-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Statement patterns or Lvalue pats. at start of assignment statement.         
;-------------------------------------------------------------------------------
(define (@yy_stat_pattern)
 (let ((/type /token1)
       (/name-save /name)
       (//S '())
       (/comps-save /comps)
       (funct-result '()))
  (set! /name '())
  (set! /comps '())
  (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
   (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
   (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
  (cond
   ((or (= /token1 //S_/B/E/C/O/M/E/S) (= /token1 //S_/L/B/R/A/C/K/E/T) (= /token1 //S_/F/U/L/L/S/T/O/P))
    ; Tis an assignment 
    (set! /type (gethash /pattern_type (list //T_/Lvalue /type)))
    (set! //S (@yy_single_assign (@Make /type /name /comps))))
   (#t
    (set! /type (gethash /pattern_type (list //T_/Statement /type)))
    (set! //S (@Make /type /name /comps))))
  (set! funct-result //S)
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Comma-separated list of Lvalues.                                             
;-------------------------------------------------------------------------------
(define (@yy_lvalues)
 (let ((/vals (list (@yy_lvalue))))
  (while (= /token1 //S_/C/O/M/M/A) 
   (begin
    (@yy_lex)
    (set! /vals (cons (@yy_lvalue) /vals))))
  (@Make //T_/Lvalues '() (reverse /vals))))

;-------------------------------------------------------------------------------
;  Lvalue.                                                                      
;-------------------------------------------------------------------------------
(define (@yy_lvalue)
 (let ((/name-save /name)
       (//S '())
       (/type /token1)
       (/comps-save /comps)
       (funct-result '()))
  (set! /name '())
  (set! /comps '())
  (cond
   ((= (wsl-ref //Patterns /token1) 1)
    (let ((/-result- (@yy_parse_pattern  /type /name /comps)))
     (set! /name (car /-result-)) (set! /-result- (cdr /-result-))
     (set! /comps (car /-result-)) (set! /-result- (cdr /-result-)))
    (set! /type (gethash /pattern_type (list //T_/Lvalue /type)))
    (set! //S (@Make /type /name /comps)))
   ((= /token1 //S_/M/E/M)
    (set! //S (@yy_lv_mem_ref)))
   (#t
    (cond
     ((not (= /token1 //S_/I/D/E/N/T/I/F/I/E/R))
      (@Syntax_Error "Missing variable name for Lvalue")
      (set! /name (@Make_Name "_Missing_")))
     (#t
      (set! /name (@Make_Name /token2))
      (@yy_lex)))
    (set! //S (@Make //T_/Var_/Lvalue /name '()))))
  ; Now check for array or structure reference
  (set! //S (@yy_lv_array_check //S))
  (set! funct-result //S)
  (set! /name /name-save)
  (set! /comps /comps-save)
  funct-result))

;-------------------------------------------------------------------------------
;  Array references (bracketed parts).                                          
;-------------------------------------------------------------------------------
(define (@yy_lv_array_ref /lval)
 (let ((/exps '())
       (//S '())
       (/type '()))
  (@yy_lex)
  (set! /exps (list (@yy_expression)))
  (cond
   ((= /token1 //S_/R/B/R/A/C/K/E/T)
    (set! /type //T_/Aref_/Lvalue)
    ; TODO: Remove the next line to change the abstract syntax of
    ; Aref_Lvalue to be <Lvalue,Expression>
    (set! /exps (list (@Make //T_/Expressions '() /exps))))
   ((= /token1 //S_/C/O/M/M/A)
    (set! /type //T_/Rel_/Seg_/Lvalue)
    (@yy_lex)
    (set! /exps (concat /exps (list (@yy_expression)))))
   ((= /token1 //S_/D/O/T/D/O/T)
    (@yy_lex)
    (cond
     ((= /token1 //S_/R/B/R/A/C/K/E/T)
      (set! /type //T_/Final_/Seg_/Lvalue))
     (#t
      (set! /type //T_/Sub_/Seg_/Lvalue)
      (set! /exps (concat /exps (list (@yy_expression)))))))
   (#t
    (@Syntax_Error "Malformed array subscript")
    (set! /type //T_/Aref_/Lvalue)
    ;Try to continue
   ))
  (@yy_skip_symbol //S_/R/B/R/A/C/K/E/T "Missing `]'")
  (set! /exps (cons /lval /exps))
  (@Make /type '() /exps)))

;-------------------------------------------------------------------------------
;  Memory references.                                                           
;-------------------------------------------------------------------------------
(define (@yy_lv_mem_ref)
 (let ((/exps '())
       (//S '())
       (/type '()))
  (@yy_lex)
  (set! /exps (list (@yy_expression)))
  (cond
   ((= /token1 //S_/R/B/R/A/C/K/E/T)
    (set! /type //T_/Mem_/Lvalue))
   ((= /token1 //S_/C/O/M/M/A)
    (set! /type //T_/Mem_/Rel_/Lvalue)
    (@yy_lex)
    (set! /exps (concat /exps (list (@yy_expression)))))
   ((= /token1 //S_/D/O/T/D/O/T)
    (@yy_lex)
    (cond
     ((= /token1 //S_/R/B/R/A/C/K/E/T)
      (@Syntax_Error "Malformed memory sub seg"))
     (#t
      (set! /type //T_/Mem_/Seg_/Lvalue)
      (set! /exps (concat /exps (list (@yy_expression)))))))
   (#t
    (@Syntax_Error "Malformed memory subscript")
    (set! /type //T_/Mem_/Lvalue)
    ;Try to continue
   ))
  (@yy_skip_symbol //S_/R/B/R/A/C/K/E/T "Missing `]' in memory reference")
  (@Make /type '() /exps)))

;-------------------------------------------------------------------------------
;  Structure references.                                                        
;-------------------------------------------------------------------------------
(define (@yy_lv_struct_ref /lval)
 (let ((/name-save /name)
       (funct-result '()))
  (set! /name '())
  (cond
   ((= /token1 //S_/I/D/E/N/T/I/F/I/E/R)
    (set! /name (@Make_Name /token2))
    (@yy_lex))
   (#t
    (@Syntax_Error "Missing structure element name")
    (set! /name (@Make_Name "_Missing_"))))
  (set! funct-result (@Make //T_/Struct_/Lvalue '() (list (@Make //T_/Name /name '()) /lval)))
  (set! /name /name-save)
  funct-result))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;                       Here endeth the parser                                  
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;    Initialisation routine                                                     
;-------------------------------------------------------------------------------
(define (@initialise)
 (set! //Error_/Count 0)
 (set! /yy_lineno 1)
 ; Populate the hash table of reserved words
 (let ((/temp //Keywords)
       (//A '()))
  (set! //Key_/Table (hash-table))
  (while (not (equal? /temp '())) 
   (begin
    (set! //A (car /temp))
    (puthash //Key_/Table (wsl-ref //A 2) (wsl-ref //A 1))
    (set! /temp (cdr /temp))))
  ; Populate hash table of lexer/meta-WSL types
  (set! /temp //Types)
  (set! //A '())
  (set! //Type_/Table (hash-table))
  (while (not (equal? /temp '())) 
   (begin
    (set! //A (car /temp))
    (puthash //Type_/Table (wsl-ref //A 2) (wsl-ref //A 1))
    (set! /temp (cdr /temp))))
  ; Build list of single-char terminals and hash table 
  ; of their corresponding lexer symbols.              
  (set! /temp /char_table)
  (set! //A '())
  (set! //Char_/Table (hash-table))
  (set! //Single_/Chars (@List_To_Set (my-map HEAD /temp)))
  (while (not (equal? /temp '())) 
   (begin
    (set! //A (car /temp))
    (puthash //Char_/Table (wsl-ref //A 1) (wsl-ref //A 2))
    (set! /temp (cdr /temp)))))
 ; Prime the lexer: 
 ; Set up the first character in `yy_ch' ...
 (set! /yy_ch (@yy_next))
 ; Turn the lexer over once: read in the first token ... 
 (@yy_crank)
 ; ... and get the first token into token1 and token2 
 (@yy_lex))

;-------------------------------------------------------------------------------

