\def\ChezWEB{Chez{\tt WEB}}
\def\CWEB{{\tt CWEB}}

@* Introduction. This document describes the implementation of the
\ChezWEB\ system of documentation and programming. It is modelled
closely after the WEB%
\footnote{Knuth, ``WEB.''}
and CWEB%
\footnote{Author, ``CWEB.''}
systems. It allows a Chez Scheme programmer to write programs of
higher quality and better documentation. It produces print-quality
output for documents, and delivers programming convenience for doing
annotations and programming illustration. It also implements a new and
novel concept in Literate Programming: Hygienic LP. Hygienic literate
programming enables the programmer to explicitly control the
visibility of variables inside and outside of a chunk. This provides
the user with a cleaner and more reliable \WEB\ than would otherwise
be had. Notably, it means that you never have to worry about variables
accidently capturing or overwriting ones that you have used
internally. It also encourages a cleaner approach to code reuse, and
discourages hacks to get around the disadvantages of traditional
literate programming.

@* Parsing \CWEB\ style files. If one writes a \ChezWEB\ file in the
\CWEB\ syntax, we need to parse it into tokens representing either
control codes or text between control codes. We do this by
implementing |chezweb-tokenize|, which takes a port and returns a list
of tokens.

$${\tt chezweb-tokenize} : port \rarrow token-list$$

\noindent Fortunately, each and every token can be identified by
reading usually only three characters%
Each control code begins with an ampersand; most are only two characters,
though the |@@>=| form has more.
This makes it fairly
straightforward to build a tokenizer directly. We do this without much
abstraction below.

@(parse-web.ss@>=
(define (chezweb-tokenize port)
  (let loop ([tokens '()] [cur '()])
    (let ([c (read-char port)])
      (cond
        [(eof-object? c)
         (reverse
           (if (null? cur)
               tokens
               (cons (list->string (reverse cur)) tokens)))]
        [(char=? #\@@ c) |Parse possible control code|]
        [else (loop tokens (cons c cur))]))))

@ Most of the control codes can be determined by reading
ahead only one more character, but there is one that requires reading
two more characters. Additionally, there is an escape control code
(|@@@@|) that let's us escape out the ampersand if we really want to put
a literal two characters into our text rather than to use a control
code. If we do find a token, we want that token to be encoded as the
appropriate symbol. We will first add any buffer left in |cur| to our
tokens list, and then add our symbolic token to the list of tokens as
well. The list of tokens is accumulated in reverse order.

@c (c cur tokens port loop)
@<Parse possible control code@>=
(let ([nc (read-char port)])
  (case nc
    [(#\@) (loop tokens (cons c cur))]
    [(#\space #\< #\p #\* #\e #\r #\( #\^ #\. #\: #\q #\i #\c)
     (let ([token (string->symbol (string c nc))])
       (if (null? cur)
           (loop (cons token tokens) '())
           (loop (cons* token (list->string (reverse cur)) tokens) '())))]
    [(#\>)
     (let ([nnc (read-char port)])
       (if (char=? #\= nnc)
           (if (null? cur)
               (loop (cons '@@>= tokens) '())
               (loop 
                 (cons* '@@>= (list->string (reverse cur)) tokens)
                 '()))
           (loop tokens (cons* nnc nc c cur))))]
    [else
      (if (eof-object? nc)
          (loop tokens cur)
          (loop tokens (cons c cur)))]))

