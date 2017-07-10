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

;str-upcase, str-downcase
; are obvious string conversion procedures and are non destructive.
;str-upcase!, str-downcase!
; are destructive versions.

(define (str-upcase! str)
  (do ((i (- (string-length str) 1) (- i 1)))
      ((< i 0) str)
    (string-set! str i (char-upcase (string-ref str i)))))

(define (my-string-upcase str)
  (str-upcase! (string-copy str)))
  
(define (str-downcase! str)
  (do ((i (- (string-length str) 1) (- i 1)))
      ((< i 0) str)
    (string-set! str i (char-downcase (string-ref str i)))))

(define (my-string-downcase str)
  (str-downcase! (string-copy str)))

(define (str-swapcase! str)
  (do ((i (- (string-length str) 1) (- i 1)))
      ((< i 0) str)
    (string-set! str i (char-swapcase (string-ref str i)))))

(define (string-swapcase str)
  (str-swapcase! (string-copy str)))

(define (char-swapcase c)
  (if (char-upper-case? c)
      (char-downcase c)
      (char-upcase c)))

;;; Return the index of the first occurence of a-char in str, or #f
(define (my-string-index str a-char)
  (let loop ((pos 0))
    (cond
     ;; whole string has been searched, in vain
     ((>= pos (string-length str)) #f)
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (+ 1 pos))))))

(define (substring? pattern str)
  (let* ((pat-len (string-length pattern))
	 (search-span (- (string-length str) pat-len))
	 (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
	 (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
     ((not c1) 0)                       ; empty pattern, matches upfront
     ((not c2) (my-string-index str c1)); one-char pattern
     (else                              ; matching pattern of > two chars
      (let outer ((pos 0))
	(cond
	 ((> pos search-span) #f)       ; nothing was found thru the whole str
	 ((not (char=? c1 (string-ref str pos)))
	  (outer (+ 1 pos)))            ; keep looking for the right beginning
	 ((not (char=? c2 (string-ref str (+ 1 pos))))
	  (outer (+ 1 pos)))            ; could've done pos+2 if c1 == c2....
	 (else                          ; two char matched: high probability
					; the rest will match too
	  (let inner ((i-pat 2) (i-str (+ 2 pos)))
	    (if (>= i-pat pat-len) pos  ; the whole pattern matched
		(if (char=? (string-ref pattern i-pat)
			    (string-ref str i-str))
		    (inner (+ 1 i-pat) (+ 1 i-str))
		    ;; mismatch after partial match
		    (outer (+ 1 pos))))))))))))

