;; ==========================================================================
;; FermaT Transformation System
;; Copyright (C) 2001 Software Migrations Limited.
;; Email: martin@gkc.org.uk
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ==========================================================================

;;; Scheme functions for WSL specific types

; For SCM this traps ^C and exits (usually)
; It has no effect in Gambit:

(define (user-interrupt . args) (quit 13))

; T_Puthash (puthash tab key value) is defined in hash.scm

; T_Gethash (gethash-1 tab key) is defined in hash.scm

; WSL hash tables return the empty list if there is nothing under the key:

(define (gethash tab key)
  (let ((result (gethash-1 tab key)))
    (if result
	(cdr result)
	'())))


; Return the list of keys in a hash table:

(define (@Hash_Keys tab)
  (let ((res '()) (i 0) (x '()))
    (for i 0 (- (vector-length tab) 1) 1
       (set! x (vector-ref tab i))
       (while (not (null? x))
	 (set! res (cons (caar x) res))
	 (set! x (cdr x))))
    res))


(define (@Assoc key alist)
  (or (assq key alist) '()))


; T_Array

(define (build-vector n f)
  (let ((v (make-vector n)))
    (do ((i 0 (+ i 1))) ((>= i n) v)
      (vector-set! v i (f i)))))

; We don't use floats for compatibility with hobbit and fast arithmetic:

;(define (frac x)
;  (- x (floor x)))

(define (frac x)
  0)

;(define (int x)
;  (inexact->exact (floor x)))

(define (int x)
  x)


(define (sgn x)
  (cond ((< x 0) -1)
	((= x 0) 0)
	(#t      1)))


; The function list? tests that the whole list is well-formed,
; so this is more efficient:

(define (sequence? x)
  (or (null? x) (pair? x)))


; Concatenate strings or lists (NB: ++ is not a valid symbol for R5RS):

(define (concat a b)
  (if (or (null? a) (pair? a))
      (append a b)
      (string-append (@String a) (@String b))))


; Return a list of the elements in an array:

(define (vector-elts ar)
  (let ((res '()) (i 0))
    (for i (- (vector-length ar) 1) 0 -1
      (set! res (cons (vector-ref ar i) res)))
    res))


; tab_get is a macro in support-mac which uses assq

;(define (tab_get l key)
;  (cond ((null? l)
;         '())
;        ((eq? (car l) key)
;         (cdr l))
;        (#t
;         (tab_get (cdr l) key))))


; tab_put is currently not required
; (since we always know that the key is not in the list)

; (define (tab_put l key value)
;   (cond ((null? l)
; 	  (list (key value)))
; 	 ((eq? (car l) key)
;	  (cons (cons key value) (cdr l)))
;	 (#t
;	  (cons (car l) (tab_put (cdr l) key value)))))


; Return a new list with a different item in the nth position
; (pad with empty lists if necessary):

(define (replace-nth object n value)
  (cond ((vector? object)
         (vector-set! object (- n 1) value)
	 object)
        ((<= n 1)
         (if (null? object)
             (list value)
             (cons value (cdr object))))
        (#t
         (if (null? object)
	     (append (make-null-list (- n 1)) (list value))
             (cons (car object) (replace-nth (cdr object) (- n 1) value))))))

(define (make-null-list n)
  (let ((r '()))
    (do ((i 0 (+ i 1))) ((>= i n) r)
      (set! r (cons '() r)))))


;; Function to return a new array/list with a segment of the old value
;; replaced by a new list of values. Returns the new value:
;; array_name[1..from-1] ++ new_value ++ array_name[to+1..]
;; TODO: If the sizes of the old and new values differ, then the array may
;; need to be resized and values shifted. (Lists are OK since we build
;; a new list anyway).

(define (@Update_Segment object from to val)
  (cond ((vector? object)
	 ; Update the array and return it
	 (if (vector? val)
	     (for i from to 1
	       (vector-set! object (- i 1) (vector-ref val (- i from))))
	     (for i from to 1
	       (vector-set! object (- i 1) (list-ref val (- i from)))))
	 object)
	(#t
	 ; Construct a list and return it:
	 (if (vector? val)
	     ; Convert val to a list before appending:
	     (let ((val2 '()))
	       (for i 0 (vector-length val) 1
		 (set! val2 (cons (vector-ref val i) val2)))
	       (append (firstn (- from 1) object) val2 (nthcdr to object)))
	     (append (firstn (- from 1) object) val (nthcdr to object))))))


;; @Sub_Seg(object, from, to)
;; return a list of the values in array_name[from..to]

(define (@Sub_Seg object from to)
  (if (vector? object)
      ; Create a new array and return it:
      (let ((v (make-vector (+ 1 (- to from)) '())))
	(for i from to 1
	  (vector-set! v (- i from) (vector-ref object (- i 1))))
	 v)
      ; Create a new list and return it:
      (firstn (+ 1 (- to from)) (nthcdr (- from 1) object))))


;; @Rel_Seg(object, from, len)

(define (@Rel_Seg object from len)
  (if (vector? object)
      ; Create a new array and return it:
      (let ((v (make-vector len '())))
	(for i from (- (+ from len) 1) 1
	  (vector-set! v (- i from) (vector-ref object (- i 1))))
	 v)
      ; Create a new list and return it:
      (firstn len (nthcdr (- from 1) object))))


;; @Final_Seg(object, from)

(define (@Final_Seg object from)
  (if (vector? object)
      ; Create a new array and return it:
      (let ((v (make-vector (+ 1 (- (vector-length object) from)) '())))
	(for i from (vector-length object) 1
	  (vector-set! v (- i from) (vector-ref object (- i 1))))
	 v)
      ; Create a new list and return it:
      (nthcdr (- from 1) object)))


; Return the first n elements of a list (in a new list):

(define (firstn n l)
  (if (or (= n 0) (null? l))
      '()
      (cons (car l) (firstn (- n 1) (cdr l)))))


; Return the last elt of a list:

(define (last-1 lst)
  (if (null? (cdr lst))
      (car lst)
      (last-1 (cdr lst))))

(define (butlast-1 lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (butlast-1 (cdr lst)))))

(define (nthcdr n lst)
  (if (zero? n)
      lst
      (nthcdr (+ -1 n) (cdr lst))))


; Compare two elements (numbers, lists, strings or whatever)
; using lexicographic order for lists.
; Note: the most common case is two numbers, hence this is handled first.
; Next most common case is two lists of numbers.

; Ordering of types is: number, list, string
; Anything else (eg a symbol or a character) is converted to a string first.

(define (@Gen_Less? a b)
  (cond ((number? a)
	 (if (number? b)
	     (< a b)
	     #t))
	((or (null? a) (pair? a))
	 (cond ((number? b)
		#f)
	       ((or (null? b) (pair? b))
		(@Gen_Less_Lists? a b))
	       (#t #f)))
	((string? a)
	 (cond ((or (number? b) (pair? b))
		#f)
	       ((string? b)
		(string<? a b))
	       (#t (@Gen_Less? a (@String b)))))
	(#t (@Gen_Less? (@String a) b))))

; NB: the cdr of a list may not be a list (eg for items):

(define (@Gen_Less_Lists? a b)
  (cond ((null? a)
	 (not (null? b)))
	((null? b)
	 #f)
	((equal? (car a) (car b))
	 (@Gen_Less? (cdr a) (cdr b)))
	((@Gen_Less? (car a) (car b))
	 #t)
	(#t #f)))


(define (@String obj)
  (cond ((string? obj)
	 obj)
	((number? obj)
	 (number->string obj))
	((symbol? obj)
	 (symbol->string obj))
	((null? obj)
	 "")
	((char? obj)
	 (string obj))
	(#t
	 (error "@String" "Don't know how to convert:" obj))))

(define spaces-100 "                                                                                                    ")

(define (@Format len obj)
  (let ((str (@String obj)))
    (cond ((> len 0)
	   (string-append (substr spaces-100
			          0
                                  (min (max (- len
					       (string-length str))
                                            0)
                                       100))
			  str))
          ((< len 0)
	   (string-append str
			  (substr spaces-100
			          0
                                  (min (max (- (- len)
					       (string-length str))
                                            0)
                                       100))))
	  (#t str))))


; Random number generator (see rand.scm):

; (myrandom n) returns a random integer from 0 to n-1 inclusive

(define (@Random n)
  (+ (myrandom n) 1))

; Call this with a random string if /dev/urandom is not available,
; or if repeatability is required.

(define (@Seed_Random_State str)
  (set! random-state-global (seed->random-state str)))



(define //Backslash (string (integer->char 92)))

(define //Quote (string (integer->char 34)))

(define //Tab (string (integer->char 9)))

(define //Newline (string (integer->char 10)))


(define /__/D/U/M/M/Y__ '())


; (getenv "string") is available in Scheme -- but not in gambit!


; Basic file I/O:

; (open-input-file filename)  --> port
; (open-output-file filename)  --> port
; (write object port)
; (display object port)
; (read port) --> object
; (read-line port)  -->  string
; (write-line string port)
; (close-input-port port)
; (close-output-port port)
; (read-char port)
; (peek-char port)
; see QUICKREF for other file operations

(define //Standard_/Input_/Port (current-input-port))

(define //Standard_/Output_/Port (current-output-port))

(define (@Open_Input_File filename)
  (open-input-file filename))

(define (@Open_Output_File filename)
  (open-output-file filename))

(define (@Close_Input_Port port)
  (close-input-port port))

(define (@Close_Output_Port port)
  (close-output-port port))

(define return-char (integer->char 13))

(define (@Read_Line port)
  (let* ((char (read-char port)))
    (if (eof-object? char)
	char
	(do ((char char (read-char port))
	     (clist '() (cons char clist)))
	    ((or (eof-object? char) (char=? #\newline char))
	     (if (and (not (null? clist)) (char=? (car clist) return-char))
		 (list->string (reverse (cdr clist)))
		 (list->string (reverse clist))))))))

(define (@Read_Line_Proc line port)
  (cons (@Read_Line port) (cons port '())))

(define (@Write_Line str port)
  (display str port)
  (newline port))

(define (@Write str port)
  (display str port))

(define (@File_Exists? filename)
  (file-exists? filename))

(define (@EOF? obj)
  (eof-object? obj))

(define (@EOL? obj)
  (char=? obj #\newline))

(define (@Read_Char port)
  (read-char port))

(define (@Peek_Char port)
  (peek-char port))

(define (@Delete_File file)
  (delete-file file))

; Format data and return a formatted string
; Uses printf.scm and strcase.scm from slib.




; Call a perl script using the perl and fermat variables:

(define (perlscript cmd args)
  (system (string-append perl " -I " //Quote fermat ds "config" //Quote
			      " " //Quote fermat ds "bin" ds cmd //Quote
			      " -FermaT " //Quote fermat //Quote
			      " " args)))

(define (@Runtime) (get-internal-run-time))


; Convert a list of integers to/from a string:

(define (@List_To_String list)
  (list->string (map integer->char list)))

(define (@String_To_List str)
  (map char->integer (string->list str)))


; (error arg ...) is available
; (gc) is available

