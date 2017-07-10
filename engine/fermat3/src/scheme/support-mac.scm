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

;;; Scheme macros for WSL specific types

; Note: we use defmacro for all macros since this is the only
; form of macro definition recognised by the Hobbit compiler.

; NB Scheme's substring takes args: string, from, to
; and returns string[from..to-1]

; To avoid the clash, rename substring to substr:

(defmacro substr (str from . rest)
  (if (null? rest)
      ; two argument form of substr:
      `(let ((s ,str))
	 (substring s ,from (string-length s)))
      ; three argument form of substr:
      `(let ((f ,from))
         (substring ,str f (+ f ,@rest)))))



; T_Print and T_Prinflush

(defmacro display-list (item . rest)
  (if (null? rest)
      `(begin
	 (display ,item)
	 (newline)
	 (force-output))
      `(begin
	 (display ,item)
	 (display-list ,@rest))))

  
(defmacro display-list-flush (item . rest)
  (if (null? rest)
      `(begin
	 (display ,item)
	 (force-output))
      `(begin
	 (display ,item)
	 (display-list-flush ,@rest))))




; T_Assert (assertion B)

(defmacro assertion (B)
  `(if ,B
       #t
       (error "assertion failed:" ',B)))


; T_While (while B S)

(defmacro while (B . S)
  `(let while-loop ()
     (cond (,B ,@S (while-loop))
	   (#t #t))))


; T_For
; -- note that step may be +ve or -ve:
; -- note that for both FOR loops we save and restore the variable
; and update it globally: this is so that dynamic binding works properly.

(defmacro for (var start end step . body)
  `(let ((var-save ,var) (for-step ,step) (for-end ,end))
     (set! ,var ,start)
     (let for-loop ()
       (cond ((or (and (> ,step 0) (<= ,var for-end))
		  (and (< ,step 0) (>= ,var for-end)))
	      ,@body
	      (set! ,var (+ ,var for-step))
	      (for-loop))
	     (#t #t)))
     (set! ,var var-save)))


; T_For_In

(defmacro for-in-orig (var seq . body)
  `(let ((var-save ,var))
     (let for-in-loop ((for-tmp ,seq))
       (cond ((not (null? for-tmp))
	      (set! ,var (car for-tmp))
	      ,@body
	      (for-in-loop (cdr for-tmp)))
	     (#t #t)))
     (set! ,var var-save)))

(defmacro for-in (var seq . body)
  `(let ((var-save ,var)
	 (seq-save ,seq))
     (if (vector? seq-save)
	 (set! seq-save (vector-elts seq-save)))
     (let for-in-loop ((for-tmp seq-save))
       (cond ((not (null? for-tmp))
	      (set! ,var (car for-tmp))
	      ,@body
	      (for-in-loop (cdr for-tmp)))
	     (#t #t)))
     (set! ,var var-save)))


; T_Floop (floop name S)

(defmacro floop (name . body)
  `(call-with-current-continuation
     (lambda (,name)
       (do () (#f #t)
	  ,@body))))


; T_Pop (pop var var)

(defmacro pop (v1 v2)
  `(begin
     (set! ,v1 (car ,v2))
     (set! ,v2 (cdr ,v2))))



; T_Push (push var expn)

(defmacro push (v e)
  `(set! ,v (cons ,e ,v)))


; T_Hash_Table (hash-table)
; TODO: can we figure out a way to dynamically expand the size of a hash table?

(defmacro hash-table ()
  `(my-make-hash-table 16))

; (maphash funct tab) calls the (two argument) funct once
; for each key, value pair in tab.
; It can also be called as (maphash 'funct tab)
;                       or (maphash (@Make_Name "funct") tab)

(defmacro maphash (funct tab)
  `(let ((maphash-index 0) (key-val-pair '()))
     (for maphash-index 0 (- (vector-length ,tab) 1) 1
       (for-in key-val-pair (vector-ref ,tab maphash-index)
         (,(if (symbol? funct)
               funct
               (if (string? (cadr funct))
                   (string->symbol (cadr funct))
                   (cadr funct)))
          (car key-val-pair)
          (cdr key-val-pair))))))


; We need macros for map and reduce, since they may be called with
; a macro as the first argument:
 
(defmacro my-map (funct lst)
  `(let ((l ,lst) (map-result '()))
     (while (not (null? l))
       (set! map-result (cons (,funct (car l)) map-result))
       (set! l (cdr l)))
     (reverse map-result)))

(defmacro my-reduce (funct lst)
  `(let ((l ,lst) (reduce-result '()))
     (cond ((null? l)
	    '())
	   (#t
	    (set! reduce-result (car l))
	    (set! l (cdr l))
	    (while (not (null? l))
	      (set! reduce-result (,funct reduce-result (car l)))
	      (set! l (cdr l)))
	    reduce-result))))


; N-argument versions of union and intersection:

(defmacro union-n args
  (if (= (length args) 1)
      (car args)
      `(@Set_Union ,(car args) (union-n ,@(cdr args)))))

(defmacro intersection-n args
  (if (= (length args) 1)
      (car args)
      `(@Set_Intersect ,(car args) (intersection-n ,@(cdr args)))))


(defmacro not-member (a b)
  `(not (member ,a ,b)))

(defmacro implies (a b)
  `(or (not ,a) ,b))


; Code for association lists (list of cons pairs):

(defmacro tab_get (alist key)
  `(let ((result (assq ,key ,alist)))
     (if result
	 result
	 '())))

; Make an array, evaluate the value for each element:

(defmacro make-vector-eval (N value)
  (if (pair? value)
      (if (eq? (car value) 'make-vector-eval)
          `(make-vector-eval-orig ,N ,value)
          `(make-vector ,N ,value))
      `(make-vector ,N ,value)))

(defmacro make-vector-eval-orig (N value)
  `(build-vector ,N (lambda (_) ,value)))



; Generic WSL references (list or array):
; (The local variable prevents object from being evaluated twice):

(defmacro wsl-ref (object index . rest)
  (if (null? rest)
      `(let ((name ,object))
	 (if (vector? name)
	     (vector-ref-safe name (- ,index 1))
	     (list-ref name (- ,index 1))))
      `(let ((name ,object))
	 (if (vector? name)
	     (wsl-ref (vector-ref-safe name (- ,index 1)) ,@rest)
	     (wsl-ref (list-ref name (- ,index 1)) ,@rest)))))

; Safe version of vector-ref:

(defmacro vector-ref-safe (ar index)
  `(if (>= ,index (vector-length ,ar))
       (vector-ref ,ar 0)
       (vector-ref ,ar ,index)))


; Replace a component of an array or list by a new value:

(defmacro wsl-set-orig! (object value index . rest)
  `(if (vector? ,object)
       (vector-set! (vector-refs ,object ,@rest)
		    (- ,index 1)
		    ,value)
       (set! ,object (new-struct ,object ,value ,index ,@rest))))


(defmacro wsl-set! (object value index . rest)
  (if (null? rest)
      `(if (vector? ,object)
	   (vector-set! ,object (- ,index 1) ,value)
	   (set! ,object (replace-nth ,object ,index ,value)))
      `(if (vector? ,object)
	   (let ((sub (vector-ref ,object (- ,index 1))))
	     (wsl-set! sub ,value ,@rest)
	     (vector-set! ,object (- ,index 1) sub))
           (let ((sub (car (nthcdr (- ,index 1) ,object))))
	     (wsl-set! sub ,value ,@rest)
	     (set! ,object (replace-nth ,object ,index sub))))))



; N-dimensional version of vector-ref (with indexes starting at 1):

(defmacro vector-refs (object . rest)
  (if (null? rest)
      object
      `(vector-refs (vector-ref ,object (- ,(car rest) 1)) ,@(cdr rest))))


; Return a new structure by replacing the component
; at position (index1 index2 ...) in the given structure with the given value:

(defmacro new-struct (object value . rest)
  (if (null? rest)
      value
      `(replace-nth ,object
		    ,(car rest)
		    (new-struct (list-ref ,object (- ,(car rest) 1))
				,value
				,@(cdr rest)))))


; Get the nth component of an item, starting at one:

(defmacro @Get_n (lst n)
  (cond ((eq? ',n 1)
	 `(caddr ,lst))
	((eq? ',n 2)
	 `(cadddr ,lst))
	((eq? ',n 3)
	 `(car (cddddr ,lst)))
	(#t
	 `(list-ref ,lst (+ 1 ,n)))))


(defmacro @String_Less? (str1 str2)
  `(string<? ,str1 ,str2))


(defmacro @String_Less_Eq? (str1 str2)
  `(string<=? ,str1 ,str2))


(defmacro @ASCII_To_String (n)
  `(string (integer->char ,n)))


;;; Functions which are called via !XF/!XC/!XP:

; (getenv "string") is available in Scheme -- but not in gambit!
; ??? Pass the FermaT environment variable on the command line and use (argv) ???


; search for a substring in a string:
; (my-index sub string)
; (my-index sub string start-point)

(defmacro my-index (sub str . rest)
  (if (null? rest)
      `(let ((index-result (substring? ,sub ,str)))
	 (if index-result
	     index-result
	     -1))
      `(let* ((s ,str) (start ,@rest)
	      (index-result (substring? ,sub (substring s start (string-length s)))))
	 (if index-result
	     (+ start index-result)
	     -1))))


; Convert a string to lower/upper/reverse case:

(defmacro lowcase (str)
  `(my-string-downcase ,str))

(defmacro upcase (str)
  `(my-string-upcase ,str))

(defmacro swapcase (str)
  `(string-swapcase ,str))


(defmacro slength (str)
  `(string-length ,str))

(defmacro gen-length (x)
  `(if (vector? ,x)
       (vector-length ,x)
       (length ,x)))


; Convert (funct (@foo_Code)) --> @foo_Code
; for returning the function rather than calling it.

(defmacro funct (name)
  (car name))


(defmacro set_car (pair obj)
  `(set-car! ,pair ,obj))



; These are needed for MAP and REDUCE:

(defmacro HEAD (pair)
  `(car ,pair))

(defmacro TAIL (pair)
  `(cdr ,pair))

;(defmacro \/ args
;  `(union-n ,@args))

;(defmacro /\ args
;  `(intersection-n ,@args))


; (error arg ...) is available
; (gc) is available



(defmacro printwsl (item . rest)
  (if (null? rest)
      `(@Print_WSL ,item "")
      `(@Print_WSL ,item ,@rest)))

(defmacro prit ()
  `(@Print_WSL (@I) ""))

(defmacro prpr ()
  `(@Print_WSL (@Program) ""))





