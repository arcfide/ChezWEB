;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tangle capable environment for ChezWEB
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

(library (arcfide chezweb tangle)
  (export @chezweb @> @* @< @ @c @l)
  (import (chezscheme))

(define-syntax @chezweb
  (syntax-rules ()
    [(_) (begin)]))

(meta define (difference ids todrop)
  (let ([ids (syntax->list ids)]
        [todrop (filter identifier? (syntax->list todrop))])
    (remp
      (lambda (x)
        (memp (lambda (y) (bound-identifier=? x y)) todrop))
      ids)))

(meta define (intersect s1 s2)
  (filter
    (lambda (x)
      (memp (lambda (y) (bound-identifier=? x y)) s2))
    s1))

(define-syntax @>
  (lambda (x)
    (syntax-case x ()
      [(_ name (i ...) () (c ...) e1 e2 ...)
       #'(define-syntax name
           (syntax-rules ()
             [(_ k c ...) (let () (import i ...) e1 e2 ...)]))]
      [(_ name (i ...) (e ...) (c ...) e1 e2 ...)
       (with-syntax ([(imps ...) (difference #'(e ...) #'(c ...))]
                     [(ecap ...) (intersect #'(e ...) #'(c ...))])
         #'(define-syntax name
             (lambda (x)
               (syntax-case x ()
                 [(_ k c ...)
                  (with-implicit (k imps ...)
                    #'(module (imps ... ecap ...)
                        (import i ...) e1 e2 ...))]))))])))

(define-syntax @<
  (lambda (x)
    (syntax-case x ()
      [(k id rest ...) 
       (with-implicit (k nk)
         #'(id nk rest ...))])))

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
