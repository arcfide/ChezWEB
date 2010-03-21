;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Weaving support for ChezWEB
;;; Version: 0.9
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
  (export @chezweb @ @* @> @< @c @l wrap code->string module)
  (import (rename (chezscheme) (module %module)))

(define-syntax define-quoter/except
  (syntax-rules ()
    [(_ wrap n1 n2 ...)
     (for-all identifier? #'(wrap n1 n2 ...))
     (define-syntax wrap
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

(define-quoter/except wrap @chezweb @ @* @> @< @p @c @l module)

(define-record-type section-ref (fields name))

(define (section-ref-writer record port print)
  (put-string port (section-ref-name record)))

(define (code->string x)
  (if (string? x)
      x
      (with-output-to-string (lambda () (pretty-print x)))))
  
(meta define (maybe-list/identifier? x)
  (syntax-case x ()
    [(id ...) (for-all identifier? #'(id ...)) #t]
    [id (identifier? #'id) #t]
    [else #f]))

(define-syntax @>
  (syntax-rules ()
    [(_ name (i ...) (e ...) (c ...) e1 e2 ...)
     (and (for-all maybe-list/identifier? #'(i ... e ...))
          (for-all identifier? #'(name c ...)))
     (render-@> 'name '(i ...) '(e ...) '(c ...)
       (wrap e1) (wrap e2) ...)]))

(define (render-@> name imports exports captures . code)
  (format
    "\\chunk{~a}{~{~a~^, ~}}{~{~a~^, ~}}{~{~a~^, ~}}\n~{~a~}\\endchunk\n"
    name imports exports captures (map code->string code)))

(define-syntax @<
  (syntax-rules ()
    [(_ id) (render-@< 'id)]))

(define (render-@< id)
  (make-section-ref (format "\\chunkref{~a}" id)))

(define-syntax @
  (syntax-rules ()
    [(_ documentation exp ...)
     (string? (syntax->datum #'documentation))
     (render-@ documentation (wrap exp) ...)]))

(define (render-@ doc . code)
  (format "\\sect~a\n~{~a~}\\endsect\n"
    doc code))

(define-syntax @*
  (syntax-rules ()
    [(_  level name documentation e1 e2 ...)
     (and (exact? (syntax->datum #'level))
          (integer? (syntax->datum #'level))
          (string? (syntax->datum #'name))
          (string? (syntax->datum #'documentation)))
     (render-@* level name documentation (wrap e1) (wrap e2) ...)]
    [(_ name documentation e1 e2 ...)
     (and (string? (syntax->datum #'name))
          (string? (syntax->datum #'documentation)))
     (render-@* 0 name documentation (wrap e1) (wrap e2) ...)]
    [(_ level name e1 e2 ...)
     (and (exact? (syntax->datum #'level))
          (integer? (syntax->datum #'level))
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

;; Doesn't work right now.
(define-syntax @l
  (syntax-rules ()
    [(k doc (n1 n2 ...) (export e ...) (import i ...) b1 b2 ...)
     (and (string? (syntax->datum #'doc))
          (eq? 'export (syntax->datum #'export))
          (eq? 'import (syntax->datum #'import)))
     (format 
       "\\library{~a}{~a}{~{~a~^ ~}}{~{~a~^ ~}}\n~{~a~}\\endlibrary{~a}\n"
       doc '(n1 n2 ...) '(e ...) '(i ...)
       `(,(wrap b1) ,(wrap b2) ...)
       '(n1 n2 ...))]))

(define-syntax @chezweb
  (syntax-rules ()
    [(_) "\\input chezwebmac\n"]))

(define-syntax module
  (syntax-rules ()
    [(_ (exports ...) b1 b2 ...)
     (for-all identifier? #'(exports ...))
     `(module (exports ...) ,(wrap b1) ,(wrap b2) ...)]
    [(_ name (exports ...) b1 b2 ...)
     (for-all identifier? #'(name exports ...))
     `(module name (exports ...) ,(wrap b1) ,(wrap b2) ...)]))
    
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
  (printf "~s\n" fns)
  (when (null? fns)
    (printf "chezweave: <file> ...\n")
    (exit 1))
  (for-each weave-file fns))

(scheme-start init/start)

)
