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

@p
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

@* Tangling a WEB. Once we have this list of tokens, we can in turn
write a simple program to tangle the output. Tangling actually
consists of several steps.

\numberedlist
\li Accumulate named chunks
\li Gather file code and |@@p| code for output
\li Prepend runtime to files
\li Prepend named chunk definitions to those files that use those chunks
\endnumberedlist

\noindent For example, if we have not used the |@@(| control code, which
allows us to send data to one or more files, then we will send all of our
data to the default file. This means that we need to walk through the
code used in all of the |@@p| control codes to find which named chunks
are referenced in the program code. We then take these definitions and
prepend them with the runtime to the appropriate file name before finally
tacking on the code for the file.

The first step is to actually grab our runtime, which we will do when
we compile the program:

@c () => (runtime-code)
@<Grab runtime code@>=
(define-syntax (get-code x)
  (call-with-input-file "runtime.ss" get-string-all))
(define runtime-code (get-code))

@ For users who wish to use this runtime in their own code, we will
provide a simple library for them to load the runtime code themselves.

@(runtime.sls@>=
(library (arcfide chezweb runtime)
  (export @<)
  (import (chezscheme))
  (include "runtime.ss"))

@ The runtime itself for tangling programs is the macro that allows one to
arbitrarily reorder chunks of code in a hygienic manner. Unlike other
WEB systems, which have no visible presence in the final code, we do require
the use of runtime code in all of our tangled code, because we rely on
the expander to support our hygienic and lexical scoping properties.

The chunking macro itself is designed to support two important properties of
a given chunk. These correspond to the normal hygienic conditions of
hygiene proper and referential transparency. These properties may be
stated casually as follows:

\numberedlist
\li {\bf Hygiene.}
Any definition introduced in the body of the chunk that is not explicitly
exported by the export and captures clauses is visible only within the scope
of the chunk, and is not visible to any surround context that references
the chunk.

\li {\bf Referential Transparency.}
Any free reference that appears in the body of the chunk will refer to
the nearest lexical binding of the tangled output unless they are explicitly
enumerated in the captures clause, in which case, they will refer to the
nearest binding in the context surrounding the chunk whenever the chunk is
referenced.
\endnumberedlist

\noindent A subtlety occurs in the actual use of the referential transparency
property. Because the user does not have direct control over the location
of the chunk definitions when tangling a WEB file, this means that there is a
more restricted notion of what the scope is for a chunk than would be normally
if the tangling runtime were used as a program time entity. On the other
hand, this restriction only applies to the specific way in which \ChezWEB\
combines/tangles a WEB file, and it does not apply to how an user may
use the chunking mechanism. Users may, in fact, place chunk definitions
arbitrarily in their own code, if they would like to do so. In this case,
the referential transparency property must still hold in its full generality.

In the case when we are only dealing with chunks defined through the WEB
mechanism and tangled explicitly, the result is that all chunks will be
defined at the top level of the file after the definition of the runtime, but
before the inclusion of any other top level elements. This means that in
practice, the lexical scope of any free variable in a chunk is the top-level
of the file in which the chunk appears. So, top-level definitions will match
for any free reference in the chunk body, but these are really the only
references that are likely to be resolvable unless one explicitly captures
them through means of the capture facility.

The macro itself takes the following syntax:

\medskip\verbatim
(@< (name capture ...) body+ ...)
(@< (name capture ...) => (export ...) body+ ...)
|endverbatim
\medskip

\noindent The first instance is the value form of a chunk. It binds |name|
to an identifier syntax that will, when referenced, expand into a form
that evaluates |body+ ...| in its own scope, where |capture ...| are
bound to the values visible in the surrounding context (rather than lexically
scoped), whose return value is the value of the last expression appearing
in |body+ ...|.

The second form is the definition form of a chunk. In this form,
it works the same as above, except that a reference to the chunk may only
appear in definition contexts; it returns no value, but instead binds
in the surrounding context of the reference those identifiers enumerated
by |export ...| to the values to which they were bound in the chunk body.

These two different semantics require two different expansions. The overall
shape of our chunk defining macro then has the following form; we will
consider each case separately.

@(runtime.ss@>=
(module (@<)
  
(define-syntax (@< x)
  (syntax-case x (=>)
    [(_ (name c ...) => (e ...) b1 b2 ...)
     (for-all identifier? #'(name c ... e ...))
     (m #'((name (c ...) (e ...)) b1 b2 ...))]
    [(_ (name c ...) b1 b2 ...)
     (for-all identifier? #'(name c ...))
     (v #'((name c ...) b1 b2 ...))]))

@ In both the value and definition forms of chunk definitions, we need to
find a way of linking together the bindings in one scope to the bindings in
another scope. In either case, the following meta procedure
(that is, defined at the meta level)
will help us to create the links. Its purpose is to take an identifier that
is scoped in the target scope, and a set of identifiers in the original scoping,
and return two sets of identifiers that are identical except that one
set is scoped in the original scoping, while the second is scoped at the
target scope. In this way, we have two sets of identifiers that are equivalent,
but will capture different bindings.

@(runtime.ss@>=
(meta define (link tgt bindings)
  (with-syntax
      ([(i ...) bindings]
       [(o ...)
        (map (lambda (b) (datum->syntax tgt b))
          (map syntax->datum (syntax->list bindings)))])
    #'((i ...) (o ...))))

@ Let's consider the value form first, since it is slightly easier. In this
case, we want to define the macro |name| to be an identifier macro that
will expand into the following form:

\medskip\verbatim
(let ()
  (define-syntax ic
    (identifier-syntax
      [ic oc]
      [(set! ic exp) (set! oc exp)]))
  ...
  body+ ...)
|endverbatim
\medskip

\noindent Notice the use of |ic ...| and |oc ...|. These are the inner/outer
bindings that we will get from |link| when we apply it using the captures.
This technique of using an identifier syntax to link together two variables
from different scopes appears to work.

@(runtime.ss@>=
(meta define (build-value-form id captures body)
  (with-syntax ([((ic ...) (oc ...)) (link id captures)]
                [(body+ ...) body])
    #'(let ()
        (define-syntax ic
          (identifier-syntax [ic oc] [(set! ic exp) (set! oc exp)]))
        ...
        body+ ...)))

@ This form is used as a part of the |v| procedure, which is what
actually destructures the information that initially comes in from the
|@@<| macro, and then does the initial definition of the macro for
|name|. This macro is just an identifier syntax that has clauses for
the single identifier use and the macro call, but nothing for the
|set!| clause, since that doesn't make sense. Because we don't care about
this case, we can avoid the use of |make-variable-transformer| and
instead use a regular |syntax-case| form.

There is an interesting problem that arises if we try to just expand
the body directly. Because we are using syntax-case to do the matching,
the body that is expanded as a part of the first level (|v|) of expansion,
will lead to a possible ellipses problem. Take the following body as an
example:

\medskip\verbatim
(define-syntax a
  (syntax-rules ()
    [(e ...) (list 'e ...)]))
(a a b c)
|endverbatim
\medskip

\noindent This seems like it should be fine, and we expect that if we use
something like the following:

\medskip\verbatim
(@< (|List of a, b, and c|)
  (define-syntax a
    (syntax-rules ()
      [(_ e ...) (list 'e ...)]))
  (a a b c))
|endverbatim
\medskip

\noindent We might end up in some trouble. When run |v| on it, we will get
something like this:

\medskip\verbatim
(define-syntax (|List of a, b, and c| x)
  (syntax-case x ()
    [id (identifier? #'id)
     (build-value-form #'id #'()
       #'((define-syntax a
            (syntax-rules ()
              [(_ e ...) (list 'e ...)]))
          (a a b c)))]))
|endverbatim
\medskip

\noindent Obviously, the above syntax doesn't work, because there is no
pattern variable |e| in the pattern clause. This means that we will get an
error about an extra ellipses. What we need to do, when we run |v|, is to
make sure that the expanded code escapes the ellipses, so we would
expand the two body forms |(define...)| and |(a a b c)| with ellipses
around them instead.

@(runtime.ss@>=        
(meta define (v x)
  (syntax-case x ()
    [((name c ...) b1 b2 ...)
     #'(define-syntax (name x)
         (syntax-case x ()
           [id (identifier? #'id)
            (build-value-form #'id #'(c ...)
              #'(((... ...) b1) ((... ...) b2) ...))]
           [(id . rest) (identifier? #'id)
            (with-syntax
                ([form (build-value-form #'id #'(c ...)
                         #'(((... ...) b1) ((... ...) b2) ...))])
              #'(form . rest))]))]))

@ When we work with the definition form, we want to use a similar linking
technique as above. However, in this case, we need to link both exports
and captures. Furthermore, we need to expand into a |module| form instead
of using a |let| form as we do above.

\medskip\verbatim
(module (oe ...)
  (define ic (identifier-syntax oc)) ...
  (module (ie ...) body+ ...)
  (define oe (identifier-syntax ie)) ...)
|endverbatim
\medskip

\noindent In this case, as in the value form, the |ic ...| and
|ie ...| bindings are, respectively, the captures and exports of the
lexical (inner) scope, while the |oc ...| and |oe ...| are the same for
the surrounding context (outer).

@(runtime.ss@>=
(meta define (build-definition-form id captures exports body)
  (with-syntax ([(body+ ...) body]
                [((ic ...) (oc ...)) (link id captures)]
                [((ie ...) (oe ...)) (link id exports)])
    #'(module (oe ...)
        (define-syntax ic
          (identifier-syntax
            [ic oc]
            [(set! ic exp) (set! oc exp)]))
        ...
        (module (ie ...) body+ ...)
        (define-syntax oe
          (identifier-syntax
            [oe ie]
            [(set! oe exp) (set! ie exp)]))
        ...)))

@ And just as we did above, we implement the |m| procedure in the same
way, taking care to escape the body elements.

@(runtime.ss@>=
(meta define (m x)
  (syntax-case x ()
    [((name (c ...) (e ...)) b1 b2 ...)
     #'(define-syntax (name x)
         (syntax-case x ()
           [id (identifier? #'id)
            (build-definition-form #'id #'(c ...) #'(e ...)
              #'(((... ...) b1) ((... ...) b2) ...))]
           [(id . rest) (identifier? #'id)
            (with-syntax
                ([form (build-definition-form #'id #'(c ...) #'(e ...)
                         #'(((... ...) b1) ((... ...) b2) ...))])
              #'(form . rest))]))]))

@ And that concludes the definition of the runtime. We do want to mark
the indirect exports for the |@@<| macro.

@(runtime.ss@>=
(indirect-export @@< m v build-definition-form build-value-form link)
)

@ We can now proceed to define a program for tangling.
We want a program that takes a single file,
and generates the tangled output.

\medskip\verbatim
cheztangle <web_file>
|endverbatim
\medskip

\noindent We will use an R6RS style program for this, assuming that all
of our important library functions will be installed in
{\tt chezweb.ss}.

@(cheztangle@>=
#! /usr/bin/env scheme-script
(import (chezscheme))

(module (tangle-file)
  (include "chezweb.ss"))

(unless (= 1 (length (command-line-arguments)))
  (printf "Usage: cheztangle <web_file>\n")
  (exit 1))

(unless (file-exists? (car (command-line-arguments)))
  (printf "Specified file '~a' does not exist.\n"
    (car (command-line-arguments)))
  (exit 1))

(tangle-file (car (command-line-arguments)))
(exit 0)

@ We already have a tokenizer, but in order to get the |tangle-file| program,
we need a way to extract out the appropriate code parts.
We care about two types of code: top-level and named chunks.
Top level chunks are any chunks delineated by |@@(| or |@@p|, and
named chunks are those which start with |@@<|. We store the named chunks
and top-level chunks into two tables. These are association lists that
map either chunk names or file names to the chunk contents, which are strings.

$$\vbox{
  \offinterlineskip
  \halign{
    \strut # & # & # \cr
    {\bf Table} & {\bf Key Type} & {\bf Value Type}
    \noalign{\hrule}
    Top-level & Filename or |*default*| & Code String
    Named Chunk & Chunk Name Symbol & Code String
  }
}$$

\noindent Since we store these as association lists, we should be careful
how we work with them. To this end, we will create a set of basic abstractions
on tables first, and use only these abstractions when working with the
internal tables. These tables should not be visible or used by the outside
world directly.

@p
