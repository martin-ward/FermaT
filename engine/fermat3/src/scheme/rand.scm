; Code taken from random.scm, byte.scm and logical.scm in SLIB
; with modifications to use /dev/urandom if available:


(define logical:integer-expt
  (lambda (n k)
    (logical:ipow-by-squaring n k 1 *)))


(define (logical:ipow-by-squaring x k acc proc)
  (cond ((zero? k) acc)
	((= 1 k) (proc acc x))
	(else (logical:ipow-by-squaring (proc x x)
					(quotient k 2)
					(if (even? k) acc (proc acc x))
					proc))))


(define (logical:logand n1 n2)
  (cond ((= n1 n2) n1)
	((zero? n1) 0)
	((zero? n2) 0)
	(else
	 (+ (* (logical:logand (logical:ash-4 n1) (logical:ash-4 n2)) 16)
	    (vector-ref (vector-ref logical:boole-and (modulo n1 16))
			(modulo n2 16))))))


(define (logical:ash-4 x)
  (if (negative? x)
      (+ -1 (quotient (+ 1 x) 16))
      (quotient x 16)))


(define (logical:integer-length n)
  (case n
    ((0 -1) 0)
    ((1 -2) 1)
    ((2 3 -3 -4) 2)
    ((4 5 6 7 -5 -6 -7 -8) 3)
    (else (+ 4 (logical:integer-length (logical:ash-4 n))))))


(define (logical:ash int cnt)
  (if (negative? cnt)
      (let ((n (logical:integer-expt 2 (- cnt))))
	(if (negative? int)
	    (+ -1 (quotient (+ 1 int) n))
	    (quotient int n)))
      (* (logical:integer-expt 2 cnt) int)))


(define (logical:bit-field n start end)
  (logical:logand (- (logical:integer-expt 2 (- end start)) 1)
		  (logical:ash n (- start))))


(define logical:boole-and
 '#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
    #(0 0 2 2 0 0 2 2 0 0 2 2 0 0 2 2)
    #(0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3)
    #(0 0 0 0 4 4 4 4 0 0 0 0 4 4 4 4)
    #(0 1 0 1 4 5 4 5 0 1 0 1 4 5 4 5)
    #(0 0 2 2 4 4 6 6 0 0 2 2 4 4 6 6)
    #(0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7)
    #(0 0 0 0 0 0 0 0 8 8 8 8 8 8 8 8)
    #(0 1 0 1 0 1 0 1 8 9 8 9 8 9 8 9)
    #(0 0 2 2 0 0 2 2 8 8 10 10 8 8 10 10)
    #(0 1 2 3 0 1 2 3 8 9 10 11 8 9 10 11)
    #(0 0 0 0 4 4 4 4 8 8 8 8 12 12 12 12)
    #(0 1 0 1 4 5 4 5 8 9 8 9 12 13 12 13)
    #(0 0 2 2 4 4 6 6 8 8 10 10 12 12 14 14)
    #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))


(define logand logical:logand)
(define integer-length logical:integer-length)
(define ash logical:ash)
(define bit-field logical:bit-field)


(define (byte-ref str ind) (char->integer (string-ref str ind)))
(define (byte-set! str ind val) (string-set! str ind (integer->char val)))
(define (make-bytes len . opt)
  (if (null? opt) (make-string len)
      (make-string len (integer->char (car opt)))))
(define bytes-length string-length)


;;; random:chunk returns an integer in the range of 0 to 255.
(define (random:chunk sta)
  (cond ((positive? (byte-ref sta 258))
	 (byte-set! sta 258 0)
	 (error "random:chunk" "random state called reentrantly" "")))
  (byte-set! sta 258 1)
  (let* ((idx (logand #xff (+ 1 (byte-ref sta 256))))
	 (xtm (byte-ref sta idx))
	 (idy (logand #xff (+ (byte-ref sta 257) xtm))))
    (byte-set! sta 256 idx)
    (byte-set! sta 257 idy)
    (let ((ytm (byte-ref sta idy)))
      (byte-set! sta idy xtm)
      (byte-set! sta idx ytm)
      (let ((ans (byte-ref sta (logand #xff (+ ytm xtm)))))
	(byte-set! sta 258 0)
	ans))))


(define (myrandom modu)
  (let ((state random-state-global))
    (letrec ((bitlen (integer-length (+ -1 modu)))
       (rnd (lambda ()
	      (do ((bln bitlen (+ -8 bln))
		   (rbs 0 (+ (ash rbs 8) (random:chunk state))))
		  ((<= bln 7)
		   (set! rbs (+ (ash rbs bln)
				(bit-field (random:chunk state) 0 bln)))
		   (and (< rbs modu) rbs))))))
    (do ((ans (rnd) (rnd))) (ans ans)))))


(define (seed->random-state seed)
  (define sta (make-bytes (+ 3 256) 0))
  (do ((idx #xff (+ -1 idx)))
      ((negative? idx))
    (byte-set! sta idx idx))
					; merge seed into state
  (do ((i 0 (+ 1 i))
       (j 0 (modulo (+ 1 j) seed-len))
       (seed-len (bytes-length seed))
       (k 0))
      ((>= i 256))
    (let ((swp (byte-ref sta i)))
      (set! k (logand #xff (+ k (byte-ref seed j) swp)))
      (byte-set! sta i (byte-ref sta k))
      (byte-set! sta k swp)))
  sta)


; Use /dev/urandom if it is present:

(set! random-state-global
  (seed->random-state 
    (if (file-exists? "/dev/urandom")
	(let* ((port (open-input-file "/dev/urandom"))
	       (char (read-char port)))
	  (do ((char char (read-char port))
	       (clist '() (cons char clist)))
	      ((= 259 (length clist))
	       (list->string clist))))
	(number->string (current-time)))))

