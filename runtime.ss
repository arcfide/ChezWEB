(define-syntax (@< x)
  (define (m x)
    (syntax-case x ()
      [((name (c ...) (e ...)) b1 b2 ...)
       #'(define-syntax (name x)
           (syntax-case x ()
             [name (identifier? #'name)
               (with-syntax
                   ([((ie (... ...)) (oe (... ...)))
                     (link #'name #'(e ...))]
                    [((ic (... ...)) (oc (... ...)))
                     (link #'name #'(c ...))])
                 #'(module (oe (... ...))
                     (define-syntax ic
                       (identifier-syntax
                         [ic oc]
                         [(set! ic exp) (set! oc exp)]))
                     (... ...)
                     (module (ie (... ...))
                       b1 b2 ...)
                     (define-syntax oe
                       (identifier-syntax
                         [oe ie]
                         [(set! oe exp) (set! ie exp)]))
                     (... ...)))]))]))
  (define (v x)
    (syntax-case x ()
      [((name c ...) b1 b2 ...)
       #'(define-syntax (name x)
           (syntax-case x ()
             [name (identifier? #'name)
               (with-syntax ([((ic (... ...)) (oc (... ...)))
                              (link #'name #'(c ...))])
                 #'(let ()
                     (define-syntax ic
                       (identifier-syntax
                         [ic oc]
                         [(set! ic exp) (set! oc exp)]))
                     (... ...)
                     b1 b2 ...))]))]))
  (syntax-case x (=> →)
    [(_ (name c ...) → (e ...) b1 b2 ...)
     (m #'((name (c ...) (e ...)) b1 b2 ...))]
    [(_ (name c ...) => (e ...) b1 b2 ...)
     (m #'((name (c ...) (e ...)) b1 b2 ...))]
    [(_ (name c ...) b1 b2 ...)
     (v #'((name c ...) b1 b2 ...))]))