;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Weaving support for ChezWEB
;;; Version: 1.2
;;; 
;;; Copyright (c) 2010 Aaron W. Hsu <arcfide@sacrideo.us>
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

#!chezscheme
(library (arcfide chezweb weave)
  (export 
    @chezweb @ @* @> @< @<< @c @l module wrap code->string
    @section @section/header @define-chunk @chunk-ref
    @chunk-ref/thread @code @library
    export import capture include quote 
    quasiquote
    unquote unquote-splicing)
  (import (rename (chezscheme) (module %module) (quasiquote qq)))
 
(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing %internal)
    [(_ e) (list 'quasiquote (quasiquote %internal e))]
    [(_ %internal (unquote e)) (list 'unquote (wrap e))]
    [(_ %internal (unquote-splicing e)) (list 'unquote-splicing (wrap e))]
    [(_ %internal (e . rest)) 
     (cons (quasiquote %internal e) (quasiquote %internal rest))]
    [(_ %internal e) (quote e)]))

(define max-simple-elems 
  (make-parameter
    (let ([max-env (getenv "CHEZWEBMAXELEMS")])
      (or (and max-env (string->number max-env)) 7))))
(define list-columns 
  (make-parameter
    (let ([cols (getenv "CHEZWEBLISTCOLUMNS")])
      (or (and cols (string->number cols)) 3))))

(define-syntax define-quoter/except
  (syntax-rules ()
    [(_ wrap n1 n2 ...)
     (for-all identifier? #'(wrap n1 n2 ...))
     (trace-define-syntax wrap
       (syntax-rules (n1 n2 ...)
        [(_ (n1 rest (... ...)))
          (n1 rest (... ...))]
        [(_ (n2 rest (... ...)))
          (n2 rest (... ...))]
        ...
        [(_ (head rest (... ...)))
          (list (wrap head) (wrap rest) (... ...))]
        [(_ other)
          (quote other)]))]))

(define-quoter/except wrap 
  @chezweb @ @* @> @< @<< @p @c @l 
  @section @section/header @define-chunk @chunk-ref
  @chunk-ref/thread @code @library
  module include quasiquote quote)

(define-syntax define-syntax-alias
  (syntax-rules ()
    [(_ new old)
     (define-syntax (new x)
       (syntax-case x ()
         [(k . rest)
          (with-implicit (k old)
            #'(old . rest))]))]))

(define-syntax-alias @section @)
(define-syntax-alias @section/header @*)
(define-syntax-alias @define-chunk @>)
(define-syntax-alias @chunk-ref @<)
(define-syntax-alias @chunk-ref/thread @<<)
(define-syntax-alias @code @c)
(define-syntax-alias @library @l)

(define-record-type section-ref (fields name))

(define (section-ref-writer record port print)
  (put-string port (section-ref-name record)))

(define (code->string x)
  (define (walk x)
    (cond
      [(string? x) (sanitize/string x)]
      [(pair? x) (cons (walk (car x)) (walk (cdr x)))]
      [else x]))
  (if (string? x)
    x
    (sanitize 
      (with-output-to-string 
        (lambda () (pretty-print (walk x)))))))

(define (sanitize/symbol sym)
  (sanitize (symbol->string sym)))

(define (sanitize/symbol-or-pair x)
  (if (pair? x)
      (map sanitize/symbol-or-pair x)
      (sanitize/symbol x)))

(define (sanitize/string s)
  (list->string
    (fold-right
      (lambda (e s)
        (case e
          [(#\\) (cons* #\backspace #\s #\{ #\}  s)]
          [else (cons e s)]))
      '()
      (string->list
        (let ([t (with-output-to-string (lambda () (pretty-print s)))])
          (substring t 1 (- (string-length t) 2)))))))

(define (sanitize code)
  (let loop ([in (string->list (code->string code))] 
             [out '()])
    (case (and (pair? in) (car in))
      [(#f) (list->string (reverse out))]
      [(#\% #\$ #\&)
        (loop (cdr in) (cons* (car in) #\\ out))]
      #;[(#\\)
        (loop (cdr in) (cons* #\} #\{ #\s #\b #\\ out))]
      [(#\#)
        (if (char=? #\\ (cadr in))
          (loop 
            (cddr in)
            (append (string->list "$hsalskcab\\$#\\") out))
          (loop (cdr in) (cons* #\# #\\ out)))]
      [else (loop (cdr in) (cons (car in) out))])))

(define (strip-special id)
  (cond
    [(symbol? id) (strip-special/string (symbol->string id))]
    [(string? id) (strip-special/string id)]
    [(number? id) (strip-special/string (number->string id))]
    [else (error 'strip-special "unknown type for ~s" id)]))

(define (strip-special/string s)
  (let loop ([cl (string->list s)] [rl '()])
    (cond
      [(not (pair? cl)) (list->string (reverse rl))]
      [(char-alphabetic? (car cl)) (loop (cdr cl) (cons (car cl) rl))]
      [(char-numeric? (car cl)) (loop (cdr cl) (cons (car cl) rl))]
      [(memq (car cl) '(#\- #\?)) (loop (cdr cl) (cons (car cl) rl))]
      [else 
        (loop 
          (cdr cl) 
          (append 
            (string->list 
              (number->string
                (char->integer (car cl)))) 
            rl))])))
  
(meta define (maybe-list/identifier? x)
  (syntax-case x ()
    [(id ...) (for-all identifier? #'(id ...)) #t]
    [id (identifier? #'id) #t]
    [else #f]))

(meta define (maybe-tree/identifier? x)
  (syntax-case x ()
    [(id ...) (for-all maybe-tree/identifier? #'(id ...)) #t]
    [id (identifier? #'id) #t]
    [else #f]))

(define-syntax %@>
  (syntax-rules ()
    [(_ name (i ...) (e ...) (c ...) e1 e2 ...)
     (and 
      (for-all maybe-tree/identifier? #'(i ...))
      (for-all maybe-list/identifier? #'(e ...))
      (for-all maybe-tree/identifier? #'(name c ...)))
     (render-@> 'name '(i ...) '(e ...) '(c ...)
       (wrap e1) (wrap e2) ...)]))

(define-syntax (capture x)
  (syntax-violation 'capture "misplaced aux keyword" x))
(define-syntax (export x)
  (syntax-violation 'export "misplaced aux keyword" x))

(define-syntax (@> x)
  (define (single-form-check keyword stx subform)
    (unless (null? (syntax->datum stx))
      (syntax-violation '@> 
        (format "more than one ~a form encountered" keyword)
        x subform)))
  (syntax-case x (@>-params export import capture)
    [(_ name (@>-params imps exps caps) (export e ...) e1 e2 ...)
     (begin 
      (single-form-check 'export #'exps #'(export e ...))
      #'(@> name (@>-params imps (e ...) caps) e1 e2 ...))]
    [(_ name (@>-params imps exps caps) (import i ...) e1 e2 ...)
     (begin 
      (single-form-check 'import #'imps #'(import i ...))
      #'(@> name (@>-params (i ...) exps caps) e1 e2 ...))]
    [(_ name (@>-params imps exps caps) (capture c ...) e1 e2 ...)
     (begin 
      (single-form-check 'capture #'caps #'(capture c ...))
      #'(@> name (@>-params imps exps (c ...)) e1 e2 ...))]
    [(_ name (@>-params imps exps caps) e1 e2 ...)
      #'(%@> name imps exps caps e1 e2 ...)]
    [(_ name e1 e2 ...) #'(@> name (@>-params () () ()) e1 e2 ...)]))

(define (render-@> name imports exports captures . code)
  (let-values (
      [(efmt eargs) 
        (render-list (map sanitize/symbol-or-pair exports))]
      [(ifmt iargs)
        (render-list (map sanitize/symbol-or-pair imports))]
      [(cfmt cargs) (render-list (map sanitize/symbol-or-pair captures))])
    (format
      "\\chunk{~a}{~a}\n~{~a~}
       \\chunkinterface
       ~a ~?
       ~a ~?
       ~a ~?
       \\endchunkinterface
       \\endchunk\n"
      (sanitize/symbol name) (strip-special name)
      (map code->string code)
      (if (pair? imports) "\\chunkimports" "") ifmt iargs
      (if (pair? exports) "\\chunkexports" "") efmt eargs
      (if (pair? captures) "\\chunkcaptures" "") cfmt cargs)))

(define-syntax @<
  (syntax-rules ()
    [(_ id rest ...) (render-@< 'id)]))

(define-syntax @<<
  (syntax-rules ()
    [(k id rest ...) (render-@< 'id)]))

(define (render-@< id)
  (make-section-ref 
    (format "\\chunkref{~a}{~a}" id (strip-special id))))

(define-syntax @
  (syntax-rules ()
    [(_ documentation exp ...)
     (string? (syntax->datum #'documentation))
     (render-@ documentation (wrap exp) ...)]))

(define (render-@ doc . code)
  (format "\\sect ~a\n~{~a~}\\endsect\n"
    doc code))

(define-syntax @*
  (syntax-rules ()
    [(_ level name documentation e1 e2 ...)
     (and 
      (integer? (syntax->datum #'level))
      (exact? (syntax->datum #'level))
      (string? (syntax->datum #'name))
      (string? (syntax->datum #'documentation)))
     (render-@* level name documentation (wrap e1) (wrap e2) ...)]
    [(_ name documentation e1 e2 ...)
     (and 
      (string? (syntax->datum #'name))
      (string? (syntax->datum #'documentation)))
     (render-@* 0 name documentation (wrap e1) (wrap e2) ...)]
    [(_ level name e1 e2 ...)
     (and 
      (integer? (syntax->datum #'level))
      (exact? (syntax->datum #'level))
      (string? (syntax->datum #'name)))
     (render-@* level name "" (wrap e1) (wrap e2) ...)]
    [(_ name e1 e2 ...)
     (string? (syntax->datum #'name))
     (render-@* 0 name "" (wrap e1) (wrap e2) ...)]))

(define (render-@* level name docs . code)
  (format "\\nsect{~a}{~a}~a\n~{~a~}\\endnsect\n"
    level name docs code))

(define-syntax @c
  (syntax-rules ()
    [(_ e1 e2 ...) 
     (render-@c (wrap e1) (wrap e2) ...)]))

(define (render-@c . code)
  (format "\\code\n~{~a~}\\endcode\n" (map code->string code)))

(define (render-list lst)
  (let ([len (length lst)])
    (if (> len (max-simple-elems))
      (render-table lst len)
      (render-simple-list lst))))

(define (render-table lst len)
  (values "\n\\makecolumns ~a/~a: ~{~a\n~}\\par"
    (qq (,len ,(list-columns) ,lst))))

(define (render-simple-list lst)
  (values "~{~a ~}\\par" (qq (,lst))))

(define-syntax @l
  (syntax-rules ()
    [(k doc (n1 n2 ...) (export e ...) (import i ...) b1 b2 ...)
     (and 
      (string? (syntax->datum #'doc))
      (eq? 'export (syntax->datum #'export))
      (eq? 'import (syntax->datum #'import)))
     (let-values (
        [(efmt eargs)
          (render-list (map sanitize/symbol '(e ...)))]
        [(ifmt iargs) 
          (render-list (map sanitize/symbol-or-pair '(i ...)))])
       (format 
         "\\library{~a}
         ~a\\par
          \\export
          ~?
          \\endexport\\medskip
          \\import
          ~?
          \\endimport\\bigskip
          ~{~a~}\\endlibrary{~a}\n"
         '(n1 n2 ...) doc
         efmt eargs
         ifmt iargs
         (qq (,(wrap b1) ,(wrap b2) ...))
         '(n1 n2 ...)))]))

(define-syntax @chezweb
  (syntax-rules ()
    [(_) "\\input chezwebmac\n"]))

(define-syntax module
  (syntax-rules ()
    [(_ (exports ...) b1 b2 ...)
     (for-all maybe-list/identifier? #'(exports ...))
     (qq (module (exports ...) ,(wrap b1) ,(wrap b2) ...))]
    [(_ name (exports ...) b1 b2 ...)
     (and 
      (identifier? #'name)
      (for-all maybe-list/identifier? #'(exports ...)))
     (qq (module name (exports ...) ,(wrap b1) ,(wrap b2) ...))]))
    
(record-writer (record-type-descriptor section-ref) section-ref-writer)

)

(let ()
  (import (chezscheme))
  
(define env (environment '(arcfide chezweb weave)))

(define (weave-file file)
  (let ([out (format "~a.tex" (path-root file))])
    (call-with-input-file file
      (lambda (ip)
        (call-with-output-file out
          (lambda (op)
            (let loop ([in (read ip)])
              (unless (eof-object? in)
                (display (eval `(code->string (wrap ,in)) env) op)
                (loop (read ip))))
            (put-string op "\n\\bye\n"))
          'replace)))))

(define (init/start . fns)
  (when (null? fns)
    (printf "chezweave: <file> ...\n")
    (exit 1))
  (for-each weave-file fns))

(scheme-start init/start)

)
