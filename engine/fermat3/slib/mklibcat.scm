;"mklibcat.scm" Build catalog for SLIB
;Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(let ((catpath (in-vicinity (implementation-vicinity) "slibcat")))
  (and (file-exists? catpath) (delete-file catpath))
  (call-with-output-file catpath
    (lambda (op)
      (define (display* . args)
	(for-each (lambda (arg) (display arg op)) args)
	(newline op))
      (define (write* asp)
	(display " " op) (write asp op) (newline op))
      (display* ";\"slibcat\" SLIB catalog for "
		(scheme-implementation-type) (scheme-implementation-version)
		".        -*-scheme-*-")
      (display* ";")
      (display* "; DO NOT EDIT THIS FILE -- it is automagically generated")
      (display*)

      (display* "(")
      (for-each
       write*
       (append
	(list (cons 'schelog
		    (in-vicinity (sub-vicinity (library-vicinity) "schelog")
				 "schelog"))
	      (cons 'portable-scheme-debugger
		    (in-vicinity (sub-vicinity (library-vicinity) "psd")
				 "psd-slib"))
	      (cons 'jfilter
		    (in-vicinity (sub-vicinity (library-vicinity) "jfilter")
				 "jfilter")))
	(catalog:resolve
	 (library-vicinity)
	 (cons
	  (if (provided? 'defmacro)
	      '(fluid-let	defmacro	"fluidlet")
	      '(fluid-let	macro		"fluidlet"))
	  '(
	    ;; null is the start of SLIB associations.
	    (null		source		"null")
	    (aggregate		source		"null")
	    (r2rs	aggregate	rev3-procedures	rev2-procedures)
	    (r3rs	aggregate	rev3-procedures)
	    (r4rs	aggregate	rev4-optional-procedures)
	    (r5rs	aggregate	values	macro	eval)
	    (rev4-optional-procedures	source	"sc4opt")
	    (rev3-procedures	source		"null")
	    (rev2-procedures	source		"sc2")
	    (multiarg/and-	source		"mularg")
	    (multiarg-apply	source		"mulapply")
	    (rationalize	source		"ratize")
	    (transcript		source		"trnscrpt")
	    (with-file		source		"withfile")
	    (dynamic-wind	source		"dynwind")
	    (dynamic		source		"dynamic")
	    (alist		source		"alist")
	    (hash		source		"hash")
	    (sierpinski		source		"sierpinski")
	    (hilbert-fill	source		"phil-spc")
	    (peano-fill		source		"peanosfc")
	    (soundex		source		"soundex")
	    (hash-table		source		"hashtab")
	    (logical		source		"logical")
	    (random		source		"random")
	    (random-inexact	source		"randinex")
	    (modular		source		"modular")
	    (factor		source		"factor")
	    (primes				factor)
	    (limit		source		"limit")
	    (eps-graph		source		"grapheps")
	    (charplot		source		"charplot")
	    (sort		source		"sort")
	    (srfi-95				sort)
	    (tsort				topological-sort)
	    (topological-sort	source		"tsort")
	    (common-list-functions	source	"comlist")
	    (tree		source		"tree")
	    (coerce		source		"coerce")
	    (format		source		"format")
	    (generic-write	source		"genwrite")
	    (pretty-print	source		"pp")
	    (pprint-file	source		"ppfile")
	    (object->string	source		"obj2str")
	    (string-case	source		"strcase")
	    (line-i/o		source		"lineio")
	    (string-port	source		"strport")
	    (getopt		source		"getopt")
	    (qp			source		"qp")
	    (eval		source		"eval")
	    (record		source		"record")
	    (synchk		source		"synchk")
	    (defmacroexpand	source		"defmacex")

	    (printf		source		"printf")
	    (scanf		defmacro	"scanf")
	    (stdio-ports	source		"stdio")
	    (stdio		aggregate scanf printf stdio-ports)

	    (break		defmacro	"break")
	    (trace		defmacro	"trace")
	    (debugf		source		"debug")
	    (debug		aggregate trace break debugf)

	    (delay				promise)
	    (promise		macro		"promise")

	    (macro-by-example	defmacro	"mbe")

	    (syntax-case	source		"scainit")
	    (syntactic-closures	source		"scmacro")
	    (macros-that-work	source		"macwork")
	    (macro				macro-by-example)
	    (object		source		"object")
	    (yasos		macro		"yasyn")
	    (oop				yasos)
	    (collect		macro		"collectx")
	    (structure		syntax-case	"structure")
	    (values		source		"values")
	    (queue		source		"queue")
	    (priority-queue	source		"priorque")
	    (array		source		"array")
	    (subarray		source		"subarray")
	    (array-for-each	source		"arraymap")
	    (array-interpolate	source		"linterp")
	    (repl		source		"repl")
	    (process		source		"process")
	    (chapter-order	source		"chap")
	    (posix-time		source		"psxtime")
	    (common-lisp-time	source		"cltime")
	    (time-core		source		"timecore")
	    (time-zone		defmacro	"timezone")
	    (relational-database	source		"rdms")
	    (databases		source		"dbutil")
	    (database-utilities			databases)
	    (database-commands	source		"dbcom")
	    (database-browse	source		"dbrowse")
	    (database-interpolate	source	"dbinterp")
	    (within-database	macro		"dbsyn")
	    (html-form		source		"htmlform")
	    (alist-table	source		"alistab")
	    (parameters		source		"paramlst")
	    (getopt-parameters	source		"getparam")
	    (read-command	source		"comparse")
	    (batch		source		"batch")
	    (glob		source		"glob")
	    (filename				glob)
	    (crc		source		"crc")
	    (dft		source		"dft")
	    (fft				dft)
	    (Fourier-transform			dft)
	    (wt-tree		source		"wttree")
	    (string-search	source		"strsrch")
	    (root		source		"root")
	    (minimize		source		"minimize")
	    (precedence-parse	source		"prec")
	    (parse				precedence-parse)
	    (commutative-ring	source		"cring")
	    (self-set		source		"selfset")
	    (determinant	source		"determ")
	    (byte		source		"byte")
	    (byte-number	source		"bytenumb")
	    (tzfile		source		"tzfile")
	    (schmooz		source		"schmooz")
	    (transact		defmacro	"transact")
	    (net-clients			transact)
	    (db->html		source		"db2html")
	    (http		defmacro	"http-cgi")
	    (cgi				http)
	    (uri		defmacro	"uri")
	    (uniform-resource-identifier		uri)
	    (pnm		source		"pnm")
	    (metric-units	source		"simetrix")
	    (diff		source		"differ")
	    (solid		source		"solid")
	    (vrml97				solid)
	    (vrml				vrml97)
	    (color		defmacro	"color")
	    (color-space	source		"colorspc")
	    (cie				color-space)
	    (color-names	source		"colornam")
	    (color-database	defmacro	"mkclrnam")
	    (resene		color-names	"clrnamdb.scm")
	    (saturate		color-names	"clrnamdb.scm")
	    (nbs-iscc		color-names	"clrnamdb.scm")
	    (daylight		source		"daylight")
	    (matfile		source		"matfile")
	    (mat-file				matfile)
	    (spectral-tristimulus-values		color-space)
	    (cie1964 spectral-tristimulus-values "cie1964.xyz")
	    (cie1931 spectral-tristimulus-values "cie1931.xyz")
	    (ciexyz				cie1931)
	    (cvs		defmacro	"cvs")
	    (html-for-each	defmacro	"html4each")
	    (directory		source		"dirs")
	    (ncbi-dna		defmacro	"ncbi-dna")
	    (manifest		source		"manifest")
	    (top-refs		source		"top-refs")
	    (vet		source		"vet")
	    (srfi				srfi-0)
	    (srfi-0		defmacro	"srfi")
	    (srfi-1		source		"srfi-1")
	    (and-let*				srfi-2)
	    (srfi-2		defmacro	"srfi-2")
	    (receive				srfi-8)
	    (srfi-8		macro		"srfi-8")
	    (define-record-type			srfi-9)
	    (srfi-9		macro		"srfi-9")
	    (let-values				srfi-11)
	    (srfi-11		macro		"srfi-11")
	    (srfi-28				format)
	    (srfi-39		macro		"srfi-39")
	    (srfi-47				array)
	    (srfi-63				array)
	    (srfi-60				logical)
	    (guarded-cond-clause		srfi-61)
	    (srfi-61		macro		"srfi-61")
	    (srfi-23		source		"srfi-23")
	    (math-integer	source		"math-integer")
	    (math-real		source		"math-real")
	    (srfi-94		aggregate math-integer math-real)
	    (ssax				xml-parse)
	    (xml-parse		source		"xml-parse")
	    (new-catalog	source		"mklibcat")
	    )))))
      (let* ((req (in-vicinity (library-vicinity)
			       (string-append "require" (scheme-file-suffix)))))
	(write* (cons '*slib-version* (or (slib:version req) *slib-version*))))
      (display* ")")

      (let ((load-if-exists
	     (lambda (path)
	       (cond ((file-exists? (string-append path (scheme-file-suffix)))
		      (slib:load-source path))))))
	;;(load-if-exists (in-vicinity (implementation-vicinity) "mksitcat"))
	(load-if-exists (in-vicinity (implementation-vicinity) "mkimpcat")))

      (let ((catcat
	     (lambda (vicinity name specificity)
	       (let ((path (in-vicinity vicinity name)))
		 (and (file-exists? path)
		      (call-with-input-file path
			(lambda (ip)
			  (display*)
			  (display* "; " "\"" path "\"" " SLIB "
				    specificity "-specific catalog additions")
			  (display*)
			  (do ((c (read-char ip) (read-char ip)))
			      ((eof-object? c))
			    (write-char c op)))))))))
	(catcat (library-vicinity) "sitecat" "site")
	(catcat (implementation-vicinity) "implcat" "implementation")
	(catcat (implementation-vicinity) "sitecat" "site"))
      ))
  (set! *catalog* #f))