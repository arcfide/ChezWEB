;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tangle capable environment for ChezWEB
;;; Version: 1.0
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

(library (arcfide chezweb tangle)
  (export @chezweb @> @* @< @<< @ @c @l export import capture)
  (import (chezscheme))

(define-syntax @chezweb
  (syntax-rules ()
    [(_) (begin)]))

(meta define (export-ids ids)
  (syntax-case ids ()
    [() '()]
    [((id rest ...) more ...) (cons #'id (export-ids #'(more ...)))]
    [(id more ...) (cons #'id (export-ids #'(more ...)))]))

(meta define (difference ids todrop)
  (syntax-case ids ()
    [() '()]
    [((id rest ...) more ...)
     (if (memp (lambda (y) (bound-identifier=? #'id y)) todrop)
         (difference #'(more ...) todrop)
         (cons #'(id rest ...) (difference #'(more ...) todrop)))]
    [(id more ...)
     (if (memp (lambda (y) (bound-identifier=? #'id y)) todrop)
         (difference #'(more ...) todrop)
         (cons #'id (difference #'(more ...) todrop)))]))

(meta define (intersect s1 s2)
  (syntax-case s1 ()
    [() '()]
    [((id rest ...) more ...)
     (if (memp (lambda (y) (bound-identifier=? #'id y)) s2)
         (cons #'(id rest ...) (intersect #'(more ...) s2))
         (intersect #'(more ...) s2))]
    [(id more ...)
     (if (memp (lambda (y) (bound-identifier=? #'id y)) s2)
         (cons #'id (intersect #'(more ...) s2))
         (intersect #'(more ...) s2))]))

(define-syntax %@>
  (lambda (x)
    (syntax-case x ()
      [(_ name (i ...) () (c ...) e1 e2 ...)
       #'(define-syntax name
           (syntax-rules ()
             [(_ k c ...) (let () (import i ...) e1 e2 ...)]))]
      [(_ name (i ...) (e ...) (c ...) e1 e2 ...)
       (with-syntax ([(imps ...) (difference #'(e ...) #'(c ...))]
                     [(ecap ...) (intersect #'(e ...) #'(c ...))])
         (with-syntax ([(imp-id ...) (export-ids #'(imps ...))]
                       [@<< (datum->syntax #'name '@<<)])
           #'(define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   [(_ k c ...)
                    (with-implicit (k @<< imp-id ...)
                      #'(module (imps ... ecap ...)
                          (import i ...) e1 e2 ...))])))))])))

(define @>-params)

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
     (begin (single-form-check 'export #'exps #'(export e ...))
       #'(@> name (@>-params imps (e ...) caps) e1 e2 ...))]
    [(_ name (@>-params imps exps caps) (import i ...) e1 e2 ...)
     (begin (single-form-check 'import #'imps #'(import i ...))
       #'(@> name (@>-params (i ...) exps caps) e1 e2 ...))]
    [(_ name (@>-params imps exps caps) (capture c ...) e1 e2 ...)
     (begin (single-form-check 'capture #'caps #'(capture c ...))
       #'(@> name (@>-params imps exps (c ...)) e1 e2 ...))]
    [(_ name (@>-params imps exps caps) e1 e2 ...)
     #'(%@> name imps exps caps e1 e2 ...)]
    [(_ name e1 e2 ...) #'(@> name (@>-params () () ()) e1 e2 ...)]))
    
(define-syntax @<
  (lambda (x)
    (syntax-case x ()
      [(k id rest ...) #'(id k rest ...)])))

(define-syntax @<<
  (lambda (x)
    (syntax-case x ()
      [(k id rest ...) #'(id k rest ...)])))

(define-syntax @
  (syntax-rules ()
    [(_ documentation exp ...)
     (string? (syntax->datum #'documentation))
     (begin exp ...)]))

(define-syntax @*
  (syntax-rules ()
    [(_  level name documentation exp ...)
     (and (integer? (syntax->datum #'level))
          (exact? (syntax->datum #'level))
          (string? (syntax->datum #'name))
          (string? (syntax->datum #'documentation)))
     (begin exp ...)]
    [(_ name documentation exp ...)
     (and (string? (syntax->datum #'name))
          (string? (syntax->datum #'documentation)))
     (begin exp ...)]
    [(_ level name exp ...)
     (and (integer? (syntax->datum #'level))
          (exact? (syntax->datum #'level))
          (string? (syntax->datum #'name)))
     (begin exp ...)]
    [(_ name exp ...)
     (string? (syntax->datum #'name))
     (begin exp ...)]))

(define-syntax @c
  (syntax-rules ()
    [(_ e1 e2 ...) (begin e1 e2 ...)]))

(define-syntax @l
  (lambda (x)
    (syntax-case x ()
      [(k doc (n1 n2 ...) (export e ...) (import i ...) body ...)
       (and (string? (syntax->datum #'doc))
            (eq? 'export (syntax->datum #'export))
            (eq? 'import (syntax->datum #'import)))
       (with-implicit (k library arcfide chezweb tangle import export)
         #'(library (n1 n2 ...)
             (export e ...)
             (import (arcfide chezweb tangle) i ...)
             body ...))])))

)

(import (arcfide chezweb tangle))
