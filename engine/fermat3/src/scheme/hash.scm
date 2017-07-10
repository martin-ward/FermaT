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

; Hashing functions, Association lists and hash tables:


(define (hash:hash-char char n)
  (modulo (char->integer (char-downcase char)) n))

(define (hash:hash-symbol sym n)
  (hash:hash-string (symbol->string sym) n))

; We don't use floats for compatibility with hobbit and fast arithmetic:

(define (hash:hash-number num n)
  (if (integer? num)
      (modulo num n)
      (hash:hash-string
       (number->string num)
       n)))

(define (hash:hash-string str n)
  (let ((len (string-length str)))
    (if (> len 5)
	(let loop ((h (modulo 264 n)) (i 5))
	  (if (positive? i)
	      (loop (modulo (+ (* h 256)
			       (char->integer
				(char-downcase
				 (string-ref str (modulo h len)))))
			    n)
		    (- i 1))
	      h))
	(let loop ((h 0) (i (- len 1)))
	  (if (>= i 0)
	      (loop (modulo (+ (* h 256)
			       (char->integer
				(char-downcase (string-ref str i))))
			    n)
		    (- i 1))
	      h)))))

(define (hash:hash obj n)
  (let hs ((d 10) (obj obj))
    (cond
     ((number? obj)      (hash:hash-number obj n))
     ((char? obj)        (modulo (char->integer (char-downcase obj)) n))
     ((symbol? obj)      (hash:hash-symbol obj n))
     ((string? obj)      (hash:hash-string obj n))
     ((vector? obj)
      (let ((len (vector-length obj)))
	(if (> len 5)
	    (let lp ((h 1) (i (quotient d 2)))
	      (if (positive? i)
		  (lp (modulo (+ (* h 256)
				 (hs 2 (vector-ref obj (modulo h len))))
			      n)
		      (- i 1))
		  h))
	    (let loop ((h (- n 1)) (i (- len 1)))
	      (if (>= i 0)
		  (loop (modulo (+ (* h 256) (hs (quotient d len)
						 (vector-ref obj i)))
				n)
			(- i 1))
		  h)))))
     ((pair? obj)
      (if (positive? d) (modulo (+ (hs (quotient d 2) (car obj))
				   (hs (quotient d 2) (cdr obj)))
				n)
	  1))
     (else
      (modulo
       (cond
	((null? obj)        256)
	((boolean? obj)     (if obj 257 258))
	((eof-object? obj)  259)
	((input-port? obj)  260)
	((output-port? obj) 261)
	((procedure? obj)   262)
	(else               263))
       n)))))



; Association Lists:

(define (alist-remove alist key)
  (cond ((null? alist) alist)
	((equal? key (caar alist)) (cdr alist))
	((null? (cdr alist)) alist)
	((equal? key (caadr alist))
	 (set-cdr! alist (cddr alist)) alist)
	(else
	 (let l ((al (cdr alist)))
	   (cond ((null? (cdr al)) alist)
		 ((equal? key (caadr al))
		  (set-cdr! al (cddr al)) alist)
		 (else (l (cdr al))))))))


; Hash tables:

(define (my-make-hash-table k) (make-vector k '()))  

(define (gethash-1 hashtab key)
  (assoc key
	 (vector-ref hashtab (hash:hash key (vector-length hashtab)))))

(define (asso alist key val)
  (let* ((pair (assoc key alist)))
    (cond (pair (set-cdr! pair val)
		alist)
	  (else (cons (cons key val) alist)))))

(define (puthash hashtab key val)
  (let* ((num (hash:hash key (vector-length hashtab))))
    (vector-set! hashtab num
		 (asso (vector-ref hashtab num) key val)))
  hashtab)

(define (remhash hashtab key)
  (let* ((num (hash:hash key (vector-length hashtab))))
    (vector-set! hashtab num
		 (alist-remove (vector-ref hashtab num) key)))
  hashtab)

