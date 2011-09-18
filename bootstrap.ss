#!/usr/bin/env scheme-script
;;; -*- Mode: scheme -*-

;;;; Bootstrap Tangler for ChezWEB
;;;; Allow ChezWEB to be bootstrapped

;;; Copyright (c) 2011 Aaron W. Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

;; This program is basically a hand composed version of TANGLE that
;; allows the ChezWEB system to be boostrapped from the ground up. It
;; works only on the CWEB syntax, and does not support the Scribble
;; syntax. It is designed to let the user build a running version of
;; TANGLE from the WEB syntax files tangle.w.

(import (chezscheme))

(module (@@< =>)
	(import (only (chezscheme) =>))

(define-syntax (@@< x)
	(syntax-case x (=>)
		[	(_ (name c ...) => (e ...) b1 b2 ...)
			(for-all identifier? #'(name c ... e ...))
			(module-form #'name #'(c ...) #'(e ...) #'(b1 b2 ...))]
		[	(_ (name c ...) b1 b2 ...)
			(for-all identifier? #'(name c ...))
		 	(value-form #'name #'(c ...) #'(b1 b2 ...))]))
		 	
(meta define (build-value-form name captures body)
	(with-syntax 
			(	[(ic ...) captures]
				[(oc ...) (datum->syntax name (syntax->list captures))]
				[(body+ ...) body])
		#'(let ()
			(alias ic oc) ...
			body+ ...)))

(meta define (value-form name captures body)
	(with-syntax 
			(	[name name]
				[(c ...) captures]
				[(b ...) body])
		#'(define-syntax (name x)
			(syntax-case x ()
				[	id (identifier? #'id)
					(build-value-form #'id #'(c ...) #'(((... ...) b) ...))]
				[	(id . rest) (identifier? #'id)
					(with-syntax
							([form (build-value-form #'id #'(c ...) #'(((... ...) b) ...))])
						#'(form . rest))]))))

(meta define (build-definition-form id captures exports body)
	(with-syntax 
			(	[(body+ ...) body]
				[(ic ...) captures]
				[(oc ...) (datum->syntax id (syntax->list captures))]
				[(ie ...) exports]
				[(oe ...) (datum->syntax id (syntax->list exports))])
		#'(module (oe ...)
			(alias ic oc) ...
			(module (ie ...) body+ ...)
			(alias oe ie) ...)))

(meta define (module-form name captures exports body)
	(with-syntax 
			(	[name name]
				[(c ...) captures]
				[(e ...) exports]
				[(b ...) body])
		#'(define-syntax (name x)
			(syntax-case x ()
				[	id (identifier? #'id)
					(build-definition-form
						#'id #'(c ...) #'(e ...) #'(((... ...) b) ...))]
				[	(id . rest) (identifier? #'id)
					(with-syntax
							([form	(build-definition-form
									#'id #'(c ...) #'(e ...) #'(((... ...) b) ...))])
						#'(form . rest))]))]))

(indirect-export @@<
	module-form value-form build-definition-form build-value-form link)
)

;;; All the chunk definitions go here

(@< |Parse possible control code| c cur tokens port loop)
(let ([nc (read-char port)])
	(case nc
		[	(#\@) (loop tokens (cons c cur))]
		[	(#\q) (read-line port) (loop tokens cur)]
		[	(#\space #\< #\p #\* #\e #\r #\( #\^ #\. #\: #\i #\c)
			(let ([token (string->symbol (string c nc))])
				(if	(null? cur)
					(loop (cons token tokens) '())
					(loop (cons* token (list->string (reverse cur)) tokens) '())))]
		[	(#\>)
			(let ([nnc (read-char port)])
				(if	(char=? #\= nnc)
					(if	(null? cur)
						(loop (cons '@@>= tokens) '())
						(loop 
							(cons* '@@>= (list->string (reverse cur)) tokens)
							'()))
					(loop tokens (cons* nnc nc c cur))))]
		[else
			(if	(eof-object? nc)
				(loop tokens cur)
				(loop tokens (cons c cur)))]))
)

(@< |Extend default top-level| loop tokens top-level)
(define body (cadr tokens))
(unless (string? body)
	(error #f "Expected a string body" body))
(hashtable-update top-level '*default*
	(lambda (cur) (string-append cur body))
	"")
(loop (cddr tokens) '() #f)
)

(@< |Extend file top-level| loop tokens top-level)
|Verify and extract delimited chunk|
(let ([name (strip-whitespace name)])
	(hashtable-update top-level name
		(lambda (cur) (string-append cur body))
		""))
(loop (cddddr tokens) '() #f)
)

(@< |Update the current captures| loop tokens)
(unless (string? (cadr tokens))
	(error #f "Expected captures line" (cadr tokens)))
(with-input-from-string (cadr tokens)
		(lambda ()
			(let* ([captures (read)] [arrow (read)] [exports (read)])
				(unless (and (list? captures) (for-all symbol? captures))
					(error #f "Expected list of identifiers for captures" captures))
				(unless (and (eof-object? arrow) (eof-object? exports))
					(unless (eq? '=> arrow)
						(error #f "Expected =>" arrow))
					(unless (and (list? exports) (for-all symbol? exports))
						(error #f "Expected list of identifiers for exports" exports)))
				(loop (cddr tokens) captures exports)))))
)

(@<Extend named chunk| loop tokens named current-captures current-exports captures)
|Verify and extract delimited chunk|
(let ([name (string->symbol (strip-whitespace name))])
	(hashtable-update named name
		(lambda (cur) (string-append cur body))
		"")
	(when current-captures 
		(hashtable-update captures name
			(lambda (cur) 
				(cons	(append (car cur) current-captures)
					|Extend exports list|))
			(cons '() '()))))
(loop (cddddr tokens) '() #f)
)

(@< |Extend exports list| current-exports cur name)
(define exports (cdr cur))
(when (and (not exports) current-exports)
	(error #f "attempt to extend a value named chunk as a definition chunk"
		name current-exports))
(when (and exports (not current-exports))
	(error #f "attempt to extend a definition chunk as a value chunk"
		name))
(and exports current-exports (union exports current-exports))
)

(@< |Verify and extract delimited chunk| tokens) => (name body)
(define name (cadr tokens))
(define closer (caddr tokens))
(define body (cadddr tokens))
(unless (eq? '@>= closer)
	(error #f "Expected closing @>=" closer))
(unless (string? name)
	(error #f "Expected name string" name))
(unless (string? body)
	(error #f "Expected string body" body))
)

(@< |Write tangled file contents| output-file top-level-chunks named-chunks captures)
(call-with-output-file output-file
	(lambda (output-port)
		(put-string output-port runtime-code)
		|Write named chunks to file|
		(put-string output-port
			(hashtable-ref top-level-chunks output-file "")))
	'replace)
)

(@<Write named chunks to file| captures named-chunks output-port)
(for-each
	(lambda (name)
		(let ([cell (hashtable-ref captures name '(() . #f))])
			(format output-port
				"(@< (~s ~{~s ~}) ~@[=> (~{~s ~})~]~n~s~n)~n"
				name (car cell) (cdr cell)
				(hashtable-ref named-chunks name ""))))
	(hashtable-keys named-chunks))
)

;;; All of the top-level code goes here

(define (chezweb-tokenize port)
	(let loop ([tokens '()] [cur '()])
		(let ([c (read-char port)])
			(cond
				[	(eof-object? c)
					(reverse
						(if	(null? cur)
							tokens
							(cons (list->string (reverse cur)) tokens)))]
				[(char=? #\@@ c) |Parse possible control code|]
				[else (loop tokens (cons c cur))]))))

(define runtime-code
"(module (@@< =>)
	(import (only (chezscheme) =>))

(define-syntax (@@< x)
	(syntax-case x (=>)
		[	(_ (name c ...) => (e ...) b1 b2 ...)
			(for-all identifier? #'(name c ... e ...))
			(module-form #'name #'(c ...) #'(e ...) #'(b1 b2 ...))]
		[	(_ (name c ...) b1 b2 ...)
			(for-all identifier? #'(name c ...))
		 	(value-form #'name #'(c ...) #'(b1 b2 ...))]))
		 	
(meta define (build-value-form name captures body)
	(with-syntax 
			(	[(ic ...) captures]
				[(oc ...) (datum->syntax name (syntax->list captures))]
				[(body+ ...) body])
		#'(let ()
			(alias ic oc) ...
			body+ ...)))

(meta define (value-form name captures body)
	(with-syntax 
			(	[name name]
				[(c ...) captures]
				[(b ...) body])
		#'(define-syntax (name x)
			(syntax-case x ()
				[	id (identifier? #'id)
					(build-value-form #'id #'(c ...) #'(((... ...) b) ...))]
				[	(id . rest) (identifier? #'id)
					(with-syntax
							([form (build-value-form #'id #'(c ...) #'(((... ...) b) ...))])
						#'(form . rest))]))))

(meta define (build-definition-form id captures exports body)
	(with-syntax 
			(	[(body+ ...) body]
				[(ic ...) captures]
				[(oc ...) (datum->syntax id (syntax->list captures))]
				[(ie ...) exports]
				[(oe ...) (datum->syntax id (syntax->list exports))])
		#'(module (oe ...)
			(alias ic oc) ...
			(module (ie ...) body+ ...)
			(alias oe ie) ...)))

(meta define (module-form name captures exports body)
	(with-syntax 
			(	[name name]
				[(c ...) captures]
				[(e ...) exports]
				[(b ...) body])
		#'(define-syntax (name x)
			(syntax-case x ()
				[	id (identifier? #'id)
					(build-definition-form
						#'id #'(c ...) #'(e ...) #'(((... ...) b) ...))]
				[	(id . rest) (identifier? #'id)
					(with-syntax
							([form	(build-definition-form
									#'id #'(c ...) #'(e ...) #'(((... ...) b) ...))])
						#'(form . rest))]))]))

(indirect-export @@<
	module-form value-form build-definition-form build-value-form link)
)

")

(define (construct-chunk-tables token-list)
	(let 
			(	[named (make-eq-hashtable)]
				[top-level (make-hashtable equal-hash equal?)]
				[captures (make-eq-hashtable)])
		(let loop 
				(	[tokens token-list] 
					[current-captures '()]
					[current-exports #f])
			(if	(null? tokens)
				(values top-level named captures)
				(case (car tokens)
					[	(|@@ | @@* @@e @@r @@^ @@. @@: @@q @@i)
						(loop (cddr tokens))]
					[	(@@p) 
						|Extend default top-level|]
					[	(@@<)
						|Extend named chunk|]
					[	(|@@(|)
						|Extend file top-level|]
					[	(@@c)
						|Update the current captures|])))))

(define (strip-whitespace str)
	(define (search str inc start end)
		(let loop ([i start])
			(cond
				[(= i end) #f]
				[(not (char-whitespace? (string-ref str i))) i]
				[else (loop (inc i))])))
	(let
  			(	[s (search str 1+ 0 (string-length str))]
  				[e (search str -1+ (-1+ (string-length str)) -1)])
  		(or	(and (not s) (not e) "")
  			(substring str s (1+ e)))))

(define (tangle-file web-file)
	(let 
			(	[tokens (call-with-input-file web-file chezweb-tokenize)]
				[default-file (format "~a.ss" (path-root web-file))])
		(let-values 
				([	(top-level-chunks named-chunks captures)
					(construct-chunk-tables tokens)])
			(for-each
				(lambda (file)
					(let ([output-file (if (eq? '*default* file) default-file file)])
						|Write tangled file contents|))
				(hashtable-keys top-level-chunks)))))

(tangle-file "chezweb.w")
(exit 0)