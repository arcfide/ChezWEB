% \input graphicx

\input supp-pdf
\def\epsfbox#1{\hbox{\convertMPtoPDF{#1}{1}{1}}}

\def\ChezWEB{Chez\.{WEB}}
\def\CWEB{\.{CWEB}}
\def\WEB{\.{WEB}}

\def\title{ChezWEB (Version 2.0)}
\def\topofcontents{\null\vfill
  \centerline{\titlefont ChezWEB: Hygienic Literate Programming}
  \vskip 15pt
  \centerline{(Version 2.0)}
  \vfill}
\def\botofcontents{\vfill
\noindent
Copyright $\copyright$ 2012 Aaron W. Hsu \.{arcfide@sacrideo.us}
\smallskip\noindent
Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.
\smallskip\noindent
THE SOFTWARE IS PROVIDED ``AS IS'' AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
}

@* Introduction. This document describes the implementation of the
\ChezWEB\ system of documentation and programming. It is modeled
closely after the \WEB\
%\footnote{Knuth, ``WEB.''}
and \CWEB\
%\footnote{Author, ``\CWEB\.''}
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
@^traditional literate programming@>

@ Readers of this program are expected to be familiar with the
\ChezWEB\ guide, as this program does not discuss the language at an
user level in any detail. Instead, we concern ourselves here chiefly
with the implementation of the \WEB\ system itself. 

@ Some time ago, the now Professor Emeritus of the Art of Computer
Programming Donald E. Knuth began writing programs.  Sometime after
that, he began to construct programs in a new manner.  This manner, he
documented and labeled ``Literate Programming.'' In Professor Knuth's
vision, a program is not constructed to be read by the machine, but
rather, to be read as a pleasant book is read, to be read by the
human.  In this way, one constructs and builds the pieces of a program
together, as you might build up the necessary elements of math,
surrounding them with exposition, and ordering them in the manner that
best reveals the program's working and meaning to the reader.

This somewhat radical approach to programming leads to a drastically
different perspective on how to write programs.  Indeed, I feel that
writing my programs in a literate style has greatly improved my
ability to maintain and improve these same programs, and moreover, to
understand these programs as I am writing them.  I enjoy writing and
seeing the results of my writing, both in a printed or screen-readable
form, as well as in a machine executable form.

While I profess no particular skill in either writing or programming,
I do profess to enjoy both. This dual enjoyment is a
necessary condition for good programs, and is especially important in
literate programming, because it exposes your thoughts in two ways.
This enforced discipline can be embarrassing at times, but inevitably
leads to a better programmer.

\ChezWEB\ is my attempt at bringing the \WEB\ system of documentation
to the Schemer's world, to improve its usability and the reliability
of code that is written in a literate style. It is far from perfect,
but I hope that those who use it find it both appealing and efficient
for delivering programs of higher quality, that others can read and
understand more easily, and that can stand the rigors of many eyes and
fingers working over the document.

@ The current version of \ChezWEB\ is 2.0.

@p
(define (display-chezweb-version tangle/weave)
  (printf "This is ~a, ChezWEB Version 2.0.~n" tangle/weave))

@* 2 The ChezWEB System. We divide the \ChezWEB\ system into two primary
parts: the runtime elements, which are in charge of handling hygienic
guarantees, and the main program logic, which contains all the code for
dealing directly with webs. Both of these modules are described in this
document. Neither the runtime nor the web handling code is particularly
useful to the end user, so we encapsulate the runtime into a library,
and we provide access to the weave and tangle functionality of the web
handling code through two programs {\tt chezweave.ss} and {\tt
cheztangle.ss}. Thus, we have the following diagram, which illustrates
the relationship and dependencies of the various files that will be
produced by tangling this web.
@.chezweave@>
@.cheztangle@>

%$$\includegraphics[width=4in]{chezweb-5.eps}$$
$$\epsfbox{chezweb.5}$$

The runtime system is actually used by the tangle program when tangling
programs, as {\tt cheztangle} will embed the runtime into the tangled
code. This means that code produced by \ChezWEB\ is self-contained, and
does not require any additional libraries. We generate the runtime code
and library as separate entities specifically because programmers may
want to use them directly in their programs, outside of \ChezWEB{}.

We generate a single {\tt chezweb.ss} file for both the weaving and the
tangling in order to share common code between the two. We could have
generated a third {\tt common} file, but this actually makes things more
complicated, and it is easier to just use the same code base for the
tangle and weave programs. This makes the {\tt cheztangle.ss} and {\tt
chezweave.ss} programs quite small in themselves, with the bulk of their
logic and code inside of the {\tt chezweb.ss} file. 

Naturally, if you are working with \ChezWEB{}, you should not be
developing on the tangled code, but should be working directly from the
web file. 

@* Control Codes Cheat Sheet. This section describes in brief the
function and syntax of every control code. It does not go into detail,
but it is meant to be used as a general reference point for developers who
are familiar with the overall system, and want an at-a-glance picture of
the \ChezWEB\ language. 
@^Cheat Sheet@>

%$$\includegraphics[width=6in]{chezweb-3.eps}$$	
$$\epsfbox{chezweb.3}$$

{\parindent = 0.5in
\item{\.{@@\ }} Start a new normal section.
\item{\.{@@*}} New starred section, listed in table of contents.
\item{\.{@@p}} Begin top-level program code.
\item{\.{@@(}} Output code to a separate file.
\item{\.{@@<}} Define a named section code chunk.
\item{\.{@@>}} Delimit/end index codes or a section reference inside of a
code body.
\item{\.{@@>=}} Delimit the name of a section you are defining.
\item{\.{@@c}} Before a named section, list the captures and exports of
that section.
\item{\.{@@q}} A comment, only shows in source.
\item{\.{@@i}} Include the contents of another file.
\item{\.{@@\^}} Index an entry in roman type.
\item{\.{@@:}} Index an entry using $\.{\\9}$, default is |@@:blah}{code@@>|
which typesets |blah| as |code| in the index.
\item{\.{@@.}} Index an entry in $\.{typewriter}$ type.
\par}

@* The ChezWEB Runtime. Normal \CWEB\ programs do not have any
runtime, and they operate completely at the equivalent of a macro
@^runtime@>%
expansion phase before the C preprocessor runs. This is also how
systems like {\tt noweb} and others work. All of these systems lack
the hygiene properties that we want to preserve in a Scheme program,
especially as they relate to anything that might resemble macros.

In order to preserve hygiene in our system, we rely on the Scheme
macro system to do the hard lifting for us. This means that we have to
leave some code around that the macro system can use to do the work we
want it to.  This is the \ChezWEB\ runtime. In point of fact, the
runtime will not remain at the actual runtime of the code, but exists
during the macro expansion phase of program evaluation.

The runtime itself for tangling programs is a macro that allows one to
arbitrarily reorder chunks of code in a hygienic manner.  The chunking
macro itself is designed to support two important properties of a
given chunk. These correspond to the normal hygienic conditions of
hygiene proper and referential transparency. These properties may be
stated casually as follows:

\medskip{\narrower\noindent
{\bf Hygiene.}@^hygiene@>
Any definition introduced in the body of the chunk that is not
explicitly exported by the export and captures clauses is visible only
within the scope of the chunk, and is not visible to any surrounding
context that references the chunk.

\smallskip\noindent{\bf Referential Transparency.}
@^referential transparency@>%
Any free reference that appears in the body of the chunk will refer to
the nearest lexical binding of the tangled output unless they are
explicitly enumerated in the captures clause, in which case, they will
refer to the nearest binding in the context surrounding the chunk
whenever the chunk is referenced.\par}\medskip

\noindent A subtlety occurs in the actual use of the referential
transparency property. Because the user does not have direct control
over the location of the chunk definitions when tangling a \WEB\ file,
this means that there is a more restricted notion of what the scope is
for a chunk than would be normally if the tangling runtime were used
as a program time entity. On the other hand, this restriction only
applies to the specific way in which \ChezWEB\ combines/tangles a \WEB\
file, and it does not apply to how an user may use the chunking
mechanism. Users may, in fact, place chunk definitions arbitrarily in
their own code, if they would like to do so. In this case, the
referential transparency property must still hold in its full
generality.

In the case when we are only dealing with chunks defined through the
\WEB\ mechanism and tangled explicitly, the result is that all chunks
will be defined at the top level of the file after the definition of
the runtime, but before the inclusion of any other top level
elements. This means that in practice, the lexical scope of any free
variable in a chunk is the top-level of the file in which the chunk
appears. So, top-level definitions will match for any free reference
in the chunk body, but these are really the only references that are
likely to be resolvable unless one explicitly captures them through
means of the capture facility.
@^top-level@>
@^free variable@>
@^free reference@>

@ The macro itself takes the following syntax:

\medskip\verbatim
(@@< (name capture ...) body+ ...)
(@@< (name capture ...) => (export ...) body+ ...)
!endverbatim \medskip

\noindent The first instance is the value form of a chunk. It binds
|name| to an identifier syntax that will, when referenced, expand into
a form that evaluates |body+ ...| in its own scope, where
|capture ...| are bound to the values visible in the surrounding
context (rather than lexically scoped), whose return value is the
value of the last expression appearing in |body+ ...|.

The second form is the definition form of a chunk. In this form, it
works the same as above, except that a reference to the chunk may only
appear in definition contexts; it returns no value, but instead binds
in the surrounding context of the reference those identifiers
enumerated by |export ...| to the values to which they were bound in
the chunk body.

@(runtime.ss@>=
(module (@@< =>)
  (import-only (chezscheme))

(define-syntax @@< @.@@<@>
  (syntax-rules (=>)
    [(_ (name c ...) => (e ...) b1 b2 ...)
     (for-all identifier? #'(name c ... e ...))
     (module-form name (c ...) (e ...) b1 b2 ...)]
    [(_ (name c ...) b1 b2 ...)
     (value-form name (c ...) b1 b2 ...)]))

@ Let's consider the value form first, since it is slightly easier. In
this case, we want to define the macro |name| to be an identifier
macro that will expand into the following form:

\medskip\verbatim
(let ()
  (alias ic oc) ...
  body+ ...)
!endverbatim \medskip

\noindent Notice the use of |ic ...| and |oc ...|. These are the
inner/outer bindings that correspond exactly to one another except
that they capture different lexical bindings. That is, we create the
|oc| bindings by rewrapping the |ic| bindings with the wraps (marks
and substitutions) of the location where the |name| is referenced.  We
use |alias| to link the two identifiers to the same underlying
location.

@(runtime.ss@>=
(define-syntax (build-value-form x) @.build-value-form@>
  (syntax-case x ()
    [(_ id (ic ...) body ...)
     (with-syntax ([(oc ...) (datum->syntax #'id (syntax->datum #'(ic ...)))])
       #'(let () (alias ic oc) ... body ...))]))

@ The |build-value-form| syntax 
is used as a part of the |value-form| macro, which is what @.value-form@>
does the initial definition of the macro for |name|. The |name| macro
is just an identifier syntax that has clauses for the single
identifier use and the macro call, but nothing for the |set!| clause,
since that doesn't make sense. Because we don't care about this case,
we can avoid the use of |make-variable-transformer| and instead use a
@.make-variable-transformer@>%
regular |syntax-case| form.

There is an interesting problem that arises if we try to just expand
the body directly. Because we are using |syntax-case| to do the
matching, the body that is expanded as a part of the first
level (|value-form|) of expansion, will lead to a possible ellipses
problem.  Take the following body as an example:

\medskip\verbatim
(define-syntax a
  (syntax-rules ()
    [(_ e ...) (list 'e ...)]))
(a a b c)
!endverbatim \medskip

\noindent This seems like it should be fine, but consider what happens
if we do the following:

\medskip\verbatim
(@@< (|List of a, b, and c|)
  (define-syntax a
    (syntax-rules ()
      [(_ e ...) (list 'e ...)]))
  (a a b c))
!endverbatim \medskip

\noindent We might end up in some trouble. When |value-form| runs on it,
we will get something like this:

\medskip\verbatim
(define-syntax (|List of a, b, and c| x)
  (syntax-case x ()
    [id (identifier? #'id)
     #'(build-value-form id ()
         ((define-syntax a
            (syntax-rules ()
              [(_ e ...) (list 'e ...)]))
          (a a b c)))]))
!endverbatim \medskip

\noindent Obviously, the above syntax doesn't work, because there is
no pattern variable |e| in the pattern clause. This means that we will
get an error about an extra ellipses. What we need to do, when we run
|value-form|, is to make sure that the expanded code escapes the
ellipses, so we would expand the two body forms |(define...)| and
|(a a b c)| with ellipses around them instead.

@(runtime.ss@>=
(define-syntax value-form @.value-form@>
  (syntax-rules ()
    [(_ name (c ...) body ...)
     (define-syntax (name x)
       (syntax-case x ()
         [id (identifier? #'id)
          #'(build-value-form id (c ...) ((... ...) body) ...)]
         [(id . rest)
          #'((build-value-form id (c ...) ((... ...) body) ...) 
             . rest)]))]))

@ When we work with the definition form, we want to use a similar
aliasing technique as above. However, in this case, we need to link
both exports and captures. Furthermore, we need to expand into a
|module| form instead of using a |let| form as we do above.

\medskip\verbatim
(module (oe ...)
  (alias ic oc) ...
  (module (ie ...) body+ ...)
  (alias oe ie) ...)
!endverbatim \medskip

\noindent In this case, as in the value form, the |ic ...| and
|ie ...| bindings are, respectively, the captures and exports of the
lexical (inner) scope, while the |oc ...| and |oe ...| are the same for
the surrounding context (outer).

@(runtime.ss@>=
(define-syntax (build-module-form x) @.build-module-form@>
  (syntax-case x ()
    [(_ id (ic ...) (ie ...) body ...)
     (with-syntax ([(oc ...) (datum->syntax #'id (syntax->datum #'(ic ...)))]
                   [(oe ...) (datum->syntax #'id (syntax->datum #'(ie ...)))])
       #'(module (oe ...)
           (alias ic oc) ...
           (module (ie ...) body ...)
           (alias oe ie) ...))]))

@ And just as we did for the |value-form| macro, 
we implement the |module-form| macro in the same way, 
taking care to escape the body elements.
Unlike the value form of our call, though, we never expect to have the
|name| identifier syntax referenced at the call position of a form, as
in |(name x y z)| because that is not a valid definition context.
Thus, we only need to define the first form where it appears as a lone
identifier reference in a definition context.

@(runtime.ss@>=
(define-syntax module-form @.module-form@>
  (syntax-rules ()
    [(_ name (c ...) (e ...) body ...)
     (define-syntax (name x)
       (syntax-case x ()
         [id (identifier? #'id)
          #'(build-module-form id (c ...) (e ...)
              ((... ...) body) ...)]))]))
            
@ And that concludes the definition of the runtime. We do want to mark
the indirect exports for the |@@<| macro.

@(runtime.ss@>=
(indirect-export @@< @.indirect-export@>
  module-form value-form build-module-form build-value-form)
)

@* 2 The Runtime Library. For users who wish to use this runtime in
their own code, we provide a simple library for them to load the
runtime code themselves. This enables them to use the macro as
their own abstraction and have the chunk like reordering without
actually requiring them to write their entire program in \ChezWEB{}.
@^runtime library@>

@(runtime.sls@>=
#!chezscheme
(library (arcfide chezweb runtime)
  (export @@< =>)
  (import (chezscheme))
  (include "runtime.ss"))

@* Tokenizing WEB files. 
\ChezWEB\ programs written in the \WEB\ syntax are all treated as 
a single stream of tokens. This token stream can be obtained by using
the |chezweb-tokenize| procedure, whose signature is as follows.

$$\.{chezweb-tokenize} : \\{port}\to\\{token-list}$$

\noindent Each token is either a string or a symbol representing one of
the \ChezWEB\ control codes. Control codes in the text can be
identified by reading at most three characters.
Each control code begins with an ampersand; 
most are only two characters, though the |@@>=| form has more.
This makes it fairly straightforward to build a
tokenizer directly. We do this without much abstraction below.

We have the following parameters in our main loop:

\medskip{\parindent = 0.75in
\item{|tokens|} The reversed list of accumulated tokens so far
\item{|cur|} A character list buffer for accumulated string tokens
\item{|ports|} The set of file ports to read from, in order\par}
\medskip

\noindent I use the |ports| list because of the |@@i| control code
discussed further down.

@p
(define (chezweb-tokenize port) @.chezweb-tokenize@>
  (let loop ([tokens '()] [cur '()] [ports (list port)])
    (if (null? ports)
        @<Finish tokenizing and return token list@>
        (let ([c (read-char (car ports))])
          (cond
           [(eof-object? c) @<Finish tokenizing port and |loop|@>]
           [(char=? #\@@ c) @<Parse possible control code and |loop|@>]
           [else (loop tokens (cons c cur) ports)])))))

@ When we run out of ports to process, we want to handle any left over
elements in the |cur| list and return the reversed list of tokens.

@c (tokens cur)
@<Finish tokenizing and return token list@>=
(reverse
  (if (null? cur)
      tokens
      (cons (list->string (reverse cur)) tokens)))

@ When we run out of characters at one port, we have to consider this as
a token boundary, so that we don't collapse the code encapsulated inside
of one included file with the rest. That is, you should not be able to
do something like this:

\medskip\verbatim
@@i blah.w
kdkdkdksljklsdl
!endverbatim \medskip

\noindent You want the string starting with the line after the include
to be the start of a distinct token element,
and not be merged with any previous tokens.
Thus, the above should be an error because it begins a string of 
text or code without first delimiting it with a control code of some
sort.

@c (loop tokens cur ports)
@<Finish tokenizing port and |loop|@>=
(if (null? cur)
    (loop tokens cur (cdr ports))
    (loop (cons (list->string (reverse cur)) tokens)
          '()
          (cdr ports)))

@ Most of the control codes can be determined by reading ahead only
one more character, but dealing with |@@>=| requires two character
lookaheads. Additionally, there is an escape control code
(|@@@@|) that lets us escape out the ampersand if we really want to
put a literal two characters into our text rather than to use a
control code. If we do find a token, we want that token to be encoded
as the appropriate symbol. We will first add any buffer left in |cur|
to our tokens list, and then add our symbolic token to the list of
tokens as well. The list of tokens is accumulated in reverse order.

@c (c cur tokens loop ports)
@<Parse possible control code and |loop|@>=
(let ([nc (read-char (car ports))])
  (case nc
    [(#\@@) (loop tokens (cons c cur) ports)]
    [(#\q) (get-line (car ports)) 
           (loop tokens (cons #\newline cur) ports)]
    [(#\space #\< #\p #\* #\e #\r #\( #\^ #\. #\: #\c) @q )
     @<Add buffer and control code to |tokens| and |loop|@>]
    [(#\>) @<Parse possible |@@>=| delimiter and |loop|@>]
    [(#\i) @<Include new file in |ports| and |loop|@>]
    [else
      (if (eof-object? nc)
          (loop tokens cur ports)
          (loop tokens (cons* nc c cur) ports))]))

@ For the control codes that don't require any additional parsing, we
can simply add the |cur| buffer if it is non-empty and then add the
control code to the list of tokens.

@c (c nc cur tokens loop ports)
@<Add buffer and control code to |tokens| and |loop|@>=
(let ([token (string->symbol (string c nc))])
  (if (null? cur)
      (loop (cons token tokens) '() ports)
      (loop (cons* token (list->string (reverse cur)) tokens)
            '() ports)))

@ When we encounter an include directive, we want to splice in the
content from the file as its own set of sections. We do this by adding
the file's port at the head of |ports|. However, to maintain the
boundaries of sections, we also need to clear out things like the |cur|
buffer when we do this. We expect that the line of the include code
should contain a Scheme string that contains the path to the file. We
use this Scheme string notation instead of just a raw string because we
want to have some way of specifying strange files that may have
whitespace and other nonesense at the beginning and end of them, and so
forth. 

@c (loop ports cur tokens) 
@<Include new file in |ports| and |loop|@>=
(let ([fname (with-input-from-string (get-line (car ports)) read)])
  (unless (string? fname)
    (error #f "expected string file name" fname))
  (loop (if (pair? cur)
            (cons (list->string (reverse cur)) tokens)
            tokens)
        '()
        (cons fname ports)))

@ When we encounter the sequence |@@>| in our system, we may have a
closing delimiter, but we won't know that until we read ahead a bit
more.  When we do have a closing delimiter, we will ignore all of the
characters after that on the line. In essence, this is like having an
implicit |@@q| code sitting around. We do this in order to
provide a clean slate to the user when writing files, so that
extraneous whitespace is not inserted into a file if the programmer
does not intend it.  Extraneous whitespace at the beginning of a file
can cause problems with things like scripts if the user is using the
|@@(| control code to generate the script file.
@^scripts@>

If we do not find the correct character for closing, then we
will treat it like a normal |@@>| code, which is a code which
does not strip the rest of the line's contents.

@c (cur loop tokens c nc ports)
@<Parse possible |@@>=| delimiter and |loop|@>=
(define (extend tok ncur)
  (if (null? cur)
      (loop (cons tok tokens) ncur ports)
      (loop (cons* tok (list->string (reverse cur)) tokens)
            ncur ports)))
(let ([nnc (read-char (car ports))])
  (if (char=? #\= nnc)
      (begin (get-line (car ports)) (extend '@@>= '()))
      (extend '@@> (list nnc))))

@* Processing code bodies. When we are dealing with a token list,
the code bodies that may have chunk references in them will be
broken up into the code string elements and the delimiters surrounding
a code body. We want to make it easy to get a code body and treat
it like a single string of text. Tangling and weaving require
different textual representations of a chunk reference, but the
overall logic for handling slurping, as I call it, is the same
for both tangling and weaving. As such, we'll document the basic
logic here, and you can read about the special representations
in the appropriate section below.

$$\.{slurp-code} : \\{tokens}\times\\{encode}\times\\{clean}
\to\\{tokens-rest}\times\\{code-body-string}$$

\noindent The |slurp-code| procedure takes four arguments:

\medskip{\parindent=1.0in
\item{|tokens|} A list whose head should be the start of a code body;
\item{|encode|} A procedure that, when given a string representing a 
chunk name, will encode that string into another string, suitable for
use as part of the code body of either tangled or woven code;
\item{|clean|} A cleaning procedure that will sanitize a 
string for output;
\item{|render|} Finally, a procedure that will be called on the final
result to do any post-processing, such as stripping of trailing 
or leading whitespace.\par}\medskip

\noindent
The use of a cleaner allows the slurper to be used for both tangling 
and weaving. Specifically, if we tangle code, we don't want to 
prepare it for \TeX{}ing. On the other hand, if we are weaving the 
code, we need to remember to do things to it to make it nicer for the 
\TeX\ environments. 
The |slurp-code| procedure will return two values, one being the
pointer to the rest of the tokens after the body has been processed,
and the other, the code body itself as a single string.

@p
(define (slurp-code tokens encode clean render) @.slurp-code@>
  (let loop ([tokens tokens] [res '()])
    (cond
     [(null? tokens) @<Return rest of tokens and slurped body@>]
     [(string? (car tokens))
      (loop (cdr tokens)
	    (cons (clean (car tokens)) res))]
     [(eq? '@@< (car tokens))
      @<Verify chunk reference syntax@>
      (loop (cdddr tokens)
	    (cons (encode (cadr tokens)) res))]
     [else @<Return rest of tokens and slurped body@>])))

@ On completion of our slurping, we want to verify that we got 
something useful in the code to start with before we do the final
rendering and composition of the string elements.

@c (tokens res render) 
@<Return rest of tokens and slurped body@>=
(let ([res (apply string-append (reverse res))])
  (when (zero? (string-length res))
    (error #f 
	   "expected code body" 
	   (list-head tokens (min (length tokens) 3))))
  (when (for-all char-whitespace? (string->list res))
    (error #f "empty chunk body" res))
  (values tokens (render res)))

@ The syntax for a chunk reference is a chunk name string surrounded
by an opening |@@<| and a closing |@@>|.

\medskip\verbatim
@@<chunk name@@>
!endverbatim \medskip

\noindent The name of the chunk is the contents between the
delimiters with the leading and trailing whitespace removed. @^whitespace@>
We can verify this with a simple set of tests.
This isn't a fully thorough test but it should do the job.

@c (tokens)       
@<Verify chunk reference syntax@>=
(unless (<= 3 (length tokens))
  (error #f "unexpected end of token stream" tokens))
(unless (string? (cadr tokens))
  (error #f "expected chunk name" (list-head tokens 2)))
(unless (eq? '@@> (caddr tokens))
  (error #f "expected chunk closer" (list-head tokens 3)))
       
@* Tangling a WEB. Tangling is the process of taking a \WEB\ and
converting it to a Scheme file. In the current implementation, 
the tangled code should run self contained on its own, 
without the need of any other files.

%$$\includegraphics[width=3in]{chezweb-4.eps}$$
$$\epsfbox{chezweb.4}$$

\noindent Once we have this list of tokens, we can in turn
write a simple program to tangle the output. Tangling actually
consists of several steps.

\medskip{\parindent = 2em
\item{1.}
Accumulate named chunks;
\item{2.}
Gather file code and |@@p| code for output;
\item{3.}
Output each named file and the default file,
making sure to prepend the runtime, followed by the chunk definitions
to the default file.
\par}\medskip

\noindent For example, if we have not used the |@@(| control code,
which allows us to send data to extra files, then we will send
all of our data to the default file. 
For the default file, the user can put references to named chunks
inside of the top level code. To accomadate this, we need to embed 
the runtime code at the top of the default file, followed by all of 
the named code definitions. We can follow both of these by the main
top level definitions of the program. Note that we do not allow 
named chunk/section references inside of sections output to other 
files; that is, we do not allow you to put a reference to a named 
section inside of a section that was started with the |@@(| control
code. 

The first step is to actually grab our runtime, which we will do when
we compile the program, or, if we are using petite, we can hope that 
the user has set up their CHEZWEBHOME environment variable correctly so
that we can find it.

@p
(meta define (runtime-path)
  (define (path d) (format "~a~aruntime.ss" d (directory-separator)))
  (let loop ([dlst (source-directories)])
    (cond
      [(null? dlst) (error #f "runtime.ss not found")]
      [(file-exists? (path (car dlst))) (path (car dlst))]
      [else (loop (cdr dlst))])))
(define-syntax (get-code x)
  (call-with-input-file (runtime-path) get-string-all))
(define runtime-code (get-code))

@ We can now define a program for tangling.
We want a program that takes a single file,
and generates the tangled output.

\medskip\verbatim
cheztangle <web_file>
!endverbatim \medskip

\noindent We will use an R6RS style program for this, assuming that
all of our important library functions will be installed in {\tt
chezweb.ss}.  This creates a bit of a problem if the user is using 
petite for everything, and thus, does not compile the chezweb.ss file 
into the system.  To get around this, we allow the user to provide a 
{\tt CHEZWEBHOME} environment variable that will be used to find the 
chezweb.ss file in case it cannot be located. 

@(cheztangle.ss@>=
#! /usr/bin/env scheme-script
(import (chezscheme))

(define-syntax (include-chezweb x)
  (define chezweb-home (getenv "CHEZWEBHOME"))
  (source-directories
    (cons (or chezweb-home (path-parent (car (command-line))))
          (source-directories)))
  (syntax-case x ()
    [(k) #`(#,(datum->syntax #'k 'include) "chezweb.ss")]))

(module (tangle-file display-chezweb-version) (include-chezweb))

(display-chezweb-version "CHEZTANGLE")

(unless (= 1 (length (command-line-arguments)))
  (printf "Usage: cheztangle <web_file>\n")
  (exit 1))

(unless (file-exists? (car (command-line-arguments)))
  (printf "Specified file '~a' does not exist.\n"
    (car (command-line-arguments)))
  (exit 1))

(tangle-file (car (command-line-arguments)))
(exit 0)

@ We already have a tokenizer, but in order to get the |tangle-file|
program, we need a way to extract out the appropriate code parts.  We
care about two types of code: top-level and named chunks.  Top level
chunks are any chunks delineated by |@@(| or |@@p|, and named chunks
are those which start with |@@<|. We store the named chunks and
top-level chunks into two tables. These are tables that map either
chunk names or file names to the chunk contents, which are
strings. Additionally, named chunks have captures and export
information that must be preserved, so we also have a table for that.
The captures table is keyed on the same values as a named chunk table,
and indeed, there should be a one-to-one mapping from named chunk keys
to capture keys, but the value of a captures table is a pair of
captures and exports lists, where the exports list may be false for
value chunks.

$$\vbox{
  \offinterlineskip
  \halign{
    \strut #\hfill & #\hfill & #\hfill \cr
    {\bf Table} & {\bf Key Type} & {\bf Value Type} \cr
    \noalign{\hrule}
    Top-level & Filename or |*default*| & Code String \cr
    Named Chunk & Chunk Name Symbol & Code String \cr
    Captures & Chunk Name Symbol &
    Pair of captures and exports lists \cr
  }
}$$

\noindent We use hashtables for each table, but these hashtables are
only meant for internal use, and should never see the light of the
outside userspace. The only other gotcha to remember is that the
tokens list will contain a string as the first element only if there
is something in the limbo area of the file.
If there is nothing in limbo, there will be a token first.
We want to loop around assuming that we receive a
token before any string input, and we don't care about limbo when we
tangle a file, so when we seed the loop, we will take care to remove
the initial limbo string if there is any.

@c (tokens)
@<Construct chunk tables |named|, |top-level|, and |captures|@>=
(let ([named (make-eq-hashtable)]
      [top-level (make-hashtable equal-hash equal?)]
      [captures (make-eq-hashtable)])
  (let loop ([tokens (if (string? (car tokens)) (cdr tokens) tokens)] 
             [current-captures '()]
             [current-exports #f])
    (if (null? tokens)
        (values top-level named captures)
        @<Dispatch on control code and |loop|@>)))

@ On each step of the loop, we will expect to have a single control
code at the head of the |tokens| list.  Each time we iterate through
the loop, we should remove all of the strings and other elements
related to that control code, so that our next iteration will again
encounter a control code at the head of the list. We do not need to 
check for index control codes here or otherwise because we assume 
that we have already run |cleanse-tokens-for-tangle|
@.cleans-tokens-for-tangle@> that removes all the unimportant 
tokens that we don't want from the tokens list.  However, we do 
have to be aware of empty unnamed section bodies. 

@c (loop tokens top-level current-captures
     current-exports named captures)
@<Dispatch on control code and |loop|@>=
(case (car tokens)
  [(@@*) (loop (cddr tokens) '() #f)]
  [(|@@ |) 
   (if (string? (cadr tokens))
       (loop (cddr tokens) '() #f)
       (loop (cdr tokens) '() #f))]
  [(@@p) @<Extend default top-level and |loop|@>]
  [(@@<) @<Extend named chunk and |loop|@>]
  [(|@@(|) @<Extend file top-level and |loop|@>]
  [(@@c) @<Update the current captures and |loop|@>]
  [else (error #f "Unexpected token" (car tokens) (cadr tokens))])

@ Extending the default top level is the easiest. We just append the
string that we find to the |*default*| key in the |top-level| table.

@c (loop tokens top-level)
@<Extend default top-level and |loop|@>=
(define-values (ntkns body) 
  (slurp-code (cdr tokens) 
	      tangle-encode 
	      (lambda (x) x) 
	      strip-whitespace))
(hashtable-update! top-level '*default*
  (lambda (cur) (format "~a~a~n" cur body))
  "")
(loop ntkns '() #f)

@ I'd like to take a moment here to discuss what |tangle-encode| is. Our
|slurp-code| procedure, which is defined elsewhere, takes an encoder,
@.slurp-code@>
which will expect it to receive a string for encoding a chunk reference
name. The job of the encoder is to make sure that the string that it
returns is something that belongs as valid code. For weaving, this is a
form of \TeX{}ififcation, while with tangling, it's turning that name
into a proper Scheme identifier. 
We can assume that the name will be stripped of extraneous
whitespace. The encoder will be a pretty straightforward use of
|format|.

@p
(define (tangle-encode x) (format "~s" (string->symbol x))) @.tangle-encode@>

@ Handling file name top-level updates works much like a named chunk,
except that we do not have to deal with the issues of capture
variables, which we will discuss shortly. We must verify that we have
a valid syntax in the stream and then we can add the name in. We
should remember to strip off the leading and trailing whitespace from
the name in question. @^whitespace@>

@c (loop tokens top-level)
@<Extend file top-level and |loop|@>=
@<Verify and extract delimited chunk@>
(let ([name (strip-whitespace name)])
  (hashtable-update! top-level name
    (lambda (cur) (format "~a~a~n" cur body))
    ""))
(loop tknsrest '() #f)

@ Named chunk updates are complicated by the need to track
captures. In the \WEB\ syntax, if you have a capture that you want to
associate with a given named chunk, you list the |@@c| form right
before you define your chunk. When we parse this, we save the captures
as soon as we encounter them so that they can be used in the next
chunk. We reset the captures if we do not find a named chunk as our
next section.

$$\.{parse-captures-line} : \\{captures-string}
\to\\{captures}\times\\{maybe-exports}$$

\noindent The format of a captures form looks something like this:

\medskip\verbatim
@@c (c ...) [=> (e ...)]
!endverbatim \medskip

\noindent In the above, the exports are optional, and the captures
could be empty. This will come in to us as a string, so we will
need a way to convert it into a data representation that we can use.
In the following function, we will get two values back that
are the captures and exports, if no exports were provided to us,
then the second value will be false.

@p
(define (parse-captures-line str) @.parse-captures-line@>
  (with-input-from-string str
    (lambda ()
      (let* ([captures (read)] [arrow (read)] [exports (read)])
        (unless (and (list? captures) (for-all symbol? captures))
          (error #f
            "Expected list of identifiers for captures" captures))
        (unless (and (eof-object? arrow) (eof-object? exports))
          (unless (eq? '=> arrow)
            (error #f "Expected =>" arrow))
          (unless (and (list? exports) (for-all symbol? exports))
            (error #f
              "Expected list of identifiers for exports" exports)))
        (values captures (and (not (eof-object? exports)) exports))))))

@ With the above function, we can now trivially handle the captures
updating in our loop.

@c (loop tokens)
@<Update the current captures and |loop|@>=
(unless (string? (cadr tokens))
  (error #f "Expected captures line" (cadr tokens)))
(let-values ([(captures exports) (parse-captures-line (cadr tokens))])
  (loop (cddr tokens) captures exports))

@ When it comes to actually extending a named chunk, we will either
have nothing in the captures and exports forms, or we will have two
lists in |current-captures| and |current-exports| of symbols that
represent the identifiers that we want to capture and export,
respectively.  We need to update two hashtables, one that maps the
actual names of the chunks to their contents, and the other that
tracks the captures and exports for each named chunk. Why do both? If
someone uses the same chunk name to define two chunks, then those
chunks are linked together. Likewise, we do not want to force the user
to put all of the captures for a chunk into the first instance that
the chunk name was used as a definition. Rather, we should allow the
programmer to extend the captures and exports in the same way that the
programmer can extend the chunks. So, for example:

\medskip\verbatim
@@c (a b) => (x y z)
@@<blah@@>=
(define-values (x y z) (list a b 'c))

@@c (t) => (u v)
@@<blah@@>=
(define-values (u v) (list t t))
!endverbatim \medskip

\noindent In the above code example, we want the end result to have a 
captures list of |a b t| and the exports list to be |x y z u v|. 

@c (loop tokens named current-captures current-exports captures)
@<Extend named chunk and |loop|@>=
@<Verify and extract delimited chunk@>
(let ([name (string->symbol (strip-whitespace name))])
  (hashtable-update! named name
    (lambda (cur) (format "~a~a~n" cur body))
    "")
  (hashtable-update! captures name
    (lambda (cur) @<Extend captures and exports@>)
    #f))
(loop tknsrest '() #f)

@ We have to be careful about how we deal with the exports list.
Suppose that the user first defines a captures line without the
exports, and then later extends a chunk with a captures line that has
an export in it. The first chunk will have been written assuming that
it will return a value, and the second will have been written assuming
that it will not.  This causes a conflict, and we should not allow
this sort of thing to happen.  In the above, we partially deal with
this by assuming that if the chunk has not been extended it is fine to
extend it; this is equivalent to passing the nil object as our default
in the call to |hashtable-update!|. On the other hand, we have to make
sure that we give the right error if we do encounter a false value if
we don't expect one. That is, if we receive a pair in |cur| whose
|cdr| field is false, this means that the chunk was previously defined
and that this definition had no exports in it. We should then error
out if we have been given anything other than a false exports.

@c (current-exports current-captures cur name)
@<Extend captures and exports@>=
(define (union s1 s2) 
  (fold-left (lambda (s e) (if (memq e s) s (cons e s))) s1 s2))
(when (and cur (not (cdr cur)) current-exports)
  (error #f
    "attempt to extend a value named chunk as a definition chunk"
    name current-exports))
(when (and cur (cdr cur) (not current-exports))
  (error #f "attempt to extend a definition chunk as a value chunk"
    name (cdr cur)))
(if cur
    (cons
      (append (car cur) current-captures)
      (and (cdr cur) (append (cdr cur) current-exports)))
    (cons current-captures current-exports))

@ It is probably very likely that someone will make a mistake in
specifying their chunk names at some point. It is human nature, and
worse, typos occur more often than we would like. We want to verify
that the closing |@@>=| actually exists, and that the expected name
and body strings are actually there. At the same time, we will do the
work of extracting out the name and body strings so that they can be
later referred to as |name| and |body| rather than as |car|s and
|cdr|s into a tokens list, since that nesting gets a bit deep.

@c (tokens) => (name body tknsrest)
@<Verify and extract delimited chunk@>=
(define-values (name body tknsrest)
  (let ()
    (unless (<= 4 (length tokens))
      (error #f "unexpected end of file" tokens))
    (let ([name (list-ref tokens 1)] [closer (list-ref tokens 2)]) 
      (unless (eq? '@@>= closer)
        (error #f "Expected closing @@>=" name closer)) 
      (unless (string? name)
        (error #f "Expected name string" name))
      (let-values ([(ntkns body) 
                    (slurp-code (list-tail tokens 3) 
                                tangle-encode 
                                (lambda (x) x)
				strip-whitespace)])
        (values name body ntkns)))))

@ We also want to define our own procedure to strip the whitespace
from our strings. We could have used something from the SRFIs, such as
the strip function from SRFI 13, but we will write our own, simplified
version here to keep things easy and also to avoid unnecessary
dependencies.
Our basic technique is to take the string, and walk
down the ends from the right and the left to determine where the first
non-whitespace character occurs in each direction.

@p
(define (strip-whitespace str) @.strip-whitespace@>
  (define (search str inc start end)
    (let loop ([i start])
      (cond
        [(= i end) #f]
        [(not (char-whitespace? (string-ref str i))) i]
        [else (loop (inc i))])))
  (let ([s (search str 1+ 0 (string-length str))]
        [e (search str -1+ (-1+ (string-length str)) -1)])
    (or (and (not s) (not e) "")
        (substring str s (1+ e)))))

@ Now we have to create the actual output files. The default output 
file will have the following layout:

%$$\includegraphics[height=1.25in]{chezweb-1.eps}$$
$$\epsfbox{chezweb.1}$$
@^Chunk layout@>

\noindent The above diagram illustrates the relative positions of the 
three important pieces of a tangled file. In the first piece, we just 
put the contents of the runtime directly into the top of the file. 
Next, we put all of the chunks defined in the \WEB\ into the spot 
right below the runtime. Afterwards follows all of the top level 
code.

We do have a design decision to make at this point. What do we do 
with all of the files that are written using |@@(| codes in our file? 
It is not unreasonable to expect an user to want to use a chunk 
in those files as well as in the main top-level default. However, it 
is equally likely that the user may be trying to write a non-scheme 
file or a file that needs to have a very specific format, such as a 
library or a shell script. In these cases, having the runtime included 
at the top level will not be very useful. If we automatically 
include the runtime or any chunk definitions in the external file, 
then the user will have no way of guaranteeing a certain file layout, 
and this could break valid use cases for the |@@(| control code. 

Instead of doing this, we have taken the other approach. The user 
will not have direct access to chunks in the external files. Instead, 
if the user wishes to use those chunks in a given file, the main 
tangled file will need to be included explicitly. This has some 
disadvantages because the user will not be able to use the regular 
chunking mechanism outside of the default top-level, but this is 
mostly a matter of inconvenience rather than a reduction in 
expressive power. Note that in \CWEB\ style programs, if we were 
writing in C, this would be more of a hassle, because we may have 
header files that we wanted to write externally. An equivalent 
form in \ChezWEB\ files might be an R6RS library form. Here, we 
might have wished to have a chunk that could refer to all of the 
exports of a given library. However, while this technique would 
be usable in a \CWEB\ system, it is not so usable in the \ChezWEB\ 
approach because the chunks are Scheme code, not just strings of 
text, and this makes such an use invalid even within the tangled 
default top-level. Thus, we haven't really made the system any 
less usable for such things than it already was.

As a final note, we should remember to use the right mode for 
our tangled file. 
Since any tangled file is relying on Chez Scheme features, 
we will need to ensure that Chez Scheme mode rather than R6RS 
compatibility mode is enabled by putting the |#!chezscheme| 
directive in there at the top of the file.

We can thus sketch out a general process for writing out the 
correct contents of a file that we are writing to.

@c (file output-file top-level-chunks named-chunks captures)
@<Write tangled file contents@>=
(call-with-output-file output-file
  (lambda (output-port)
    (when (eq? file '*default*)
      (printf "Tangling ~a~n" output-file)
      (format output-port "#!chezscheme~n~a" runtime-code)
      @<Write named chunks to file@>)
    (unless (eq? file '*default*) (printf "Outputing ~a~n" output-file))
    (put-string output-port
      (hashtable-ref top-level-chunks 
        (if (eq? file '*default*) '*default* output-file) 
        "")))
  'replace)

@ For each named chunk that we find in the |named-chunks| hashtable,
we can print out the body of the chunk wrapped in the normal runtime 
format. If |body| is the string containing the body of the named chunk, 
as stored in the table, then we want to output something like:

\medskip\verbatim
(@@< (name clst ...) [=> (elst ...)]
body
)
!endverbatim \medskip

\noindent We grab the captures and exports from the |captures|
hashtable and we are careful to ensure that we don't put any exports 
in the form unless we intend to do so.

We should not have to worry about the ordering that we do the chunks 
in, since they should all be at the same phase and they should be 
definable in any order. 

@c (captures named-chunks output-port)
@<Write named chunks to file@>=
(let-values ([(keys vals) (hashtable-entries named-chunks)])
  (vector-for-each
    (lambda (name code)
      (let ([cell (hashtable-ref captures name '(() . #f))])
        (format output-port
          "(@@< (~s ~{~s ~}) ~@@[=> (~{~s ~})~]~n~a~n)~n~n"
          name (car cell) (cdr cell) code)))
    keys vals))

@ Now all of the pieces are in place to write the |tangle-file|
procedure that we talked about previously. We want to be
careful to cleanse the token list before we actually pass it
to the rest of the code, because the rest of our code assumes
a certain layout for the token list that may be invalidated
by additional annotations that we want to ignore, such as index
forms, or the like.

@p
(define (tangle-file web-file) @.tangle-file@>
  (let ([default-file (format "~a.ss" (path-root web-file))]
        [tokens
          (cleanse-tokens-for-tangle @.cleanse-tokens-for-tangle@>
            (call-with-input-file web-file chezweb-tokenize))])
    (let-values ([(top-level-chunks named-chunks captures)
                  @<Construct chunk tables |named|, |top-level|, and |captures|@>])
      (for-each
        (lambda (file)
          (let ([output-file (if (eq? '*default* file)
                                 default-file file)])
            @<Write tangled file contents@>))
        (vector->list (hashtable-keys top-level-chunks))))))

@* Weaving a WEB. Weaving is the process of converting or compiling a
\WEB\ into a \TeX\ file for rendering into a proper document. A
program like Xe\TeX\ can be used on the resulting \TeX\ file that
{\tt chezweave} outputs to make the PDF.

%$$\includegraphics[width=4in]{chezweb-2.eps}$$
$$\epsfbox{chezweb.2}$$

\noindent There are three distinct elements that make up
weaving. Firstly, there is the actual weaving, which takes
the \WEB\ text and converts it into the appropriate \TeX\ code. The
weaver must also handle the pretty printing of code and the
cross-referencing/indexing services that \ChezWEB\
offers. Fortunately, each of these may be handled in their own
passes. In this section we handle the pass that generates the
\TeX\ file proper. This means we ignore the index and pretty
printer; they are discussed elsewhere.

At a high level, we have a list of sections. Every
section consists of a code and text block. Text blocks are begun by
using |@@ | or |@@*| and code blocks begin with |@@<| or |@@p|. All
other control codes are annotations and notes describing how to format
text or to instantiate code blocks. We weave output pretty much in
order. We must keep track of the section number. Our macros that we
can use for formatting these chunks are as follows.

\medskip{\parindent = 2.5em
\item{\.{N}}
{\bf Starred Sections.} This allows for starred sections. It expects
two integers as the first parameters. These are, respectively, the
depth and the section number. After that, it will read up to the first
period for the section to highlight, and then the rest of the text
will follow after that.
\item{\.{M}}
{\bf Normal Sections.} This takes only the section number, and
typesets a normal section.
\item{\.{Y}}
{\bf Vertical Space.} Provides a little vertical space for use between
the text and code parts of a section.
\item{\.{B}}
{\bf Code.} Begins the typesetting mode for code.
\item{\.{X}}
{\bf Chunk Name.} Used to typeset a chunk name in a section, either
for files or for named chunks. It expects a section number, followed
by a colon, followed by another \.{X}.
\item{\.{4}}
{\bf Backspace.} This is used to backspace a bit, basically, to
backspace one notch.
\item{\.{fi}}
{\bf End section.} This follows the end of the section.
\item{\.{inx}}
{\bf Index.} Starts up the index.
\item{\.{fin}}
{\bf Finish.} Ends the index.
\item{\.{con}}
{\bf Sections.} Completes the section names.\par}\medskip

\noindent
Each section has the same basic layout, where it begins with
either an {\tt N} or an {\tt M} macro, and ends with {\tt fi}.
There are three distinct passes over the code that when
weaving a file. The first is the index, which
cleans up our token list so that we don't have to deal with
it in the rest of our code; it writes out the
index file with any explicit or implicit index entries found.
Next, we parse and write out the
list of sections that we have, to the section list, and finally,
we weave the actual \TeX\ for the
final code document. We deal chiefly with the third pass
here.

Let's examine the top-level loop that iterates over the tokens.

@p
(define (weave-file file) @.weave-file@>
  (call-with-output-file (format "~a.tex" (path-root file))
    (lambda (port)
      @<Define section iterator@>
      (define tokens
        (write-index file (call-with-input-file file chezweb-tokenize)))
      (define sections (index-sections file tokens))
      @<Define weave chunk reference encoder@>
      (printf "Weaving ~a..." file)
      (format port "\\input chezwebmac~n~n")
      @<Weave sections@>
      (printf "~n")
      (format port "\\inx~n\\fin~n\\con~n"))
    'replace))

@ We weave the sections by iterating over the tokens and processing
a section at a time. We expect at each stage to have the set of tokens
that were left after processing, and we can iterate on that.

@c (port tokens next-section encode sections)
@<Weave sections@>=
(let loop ([tokens tokens])
  (when (pair? tokens)
    (call-with-values 
      (lambda () @<Process a section@>)
      loop)))


@ For any given section, we know exactly what to do by looking at
the associated control code that started it. The only exception is
limbo, where there is no initial code prefixing its content. For limbo
we insert the code block literally into the output. We can divide our
sections and chunks into the following taxonomy:

%$$\includegraphics[width=6in]{chezweb-3.eps}$$
$$\epsfbox{chezweb.3}$$

\noindent Note that we do not allow a code section to immediately
follow another code section. Every section must start with a text
section, though that text section may have nothing but whitespace in
it. A section may or may not have any code section in it.

@c (port tokens next-section encode sections)
@<Process a section@>=
(define sectnum (next-section))
(case (car tokens)
  [(|@@ |) @<Process a normal section@>]
  [(@@*) @<Process a starred section@>]
  [else
    (if (string? (car tokens))
        (begin (put-string port (car tokens))
               (cdr tokens))
        (error #f "Section start expected, but found something else."
          (list-head tokens (min (length tokens) 3))))])

@ The above case is designed to map each step to one new section; that
way, we know that we must increment the section number every time we
dispatch on a new section control code. However, we will encapsulate
this work in a section iterator that is a nullary procedure. Calling
this procedure gives back the current section number. Repeated calls
give the next section numbers in order.

@c () => (next-section)
@<Define section iterator@>=
(define next-section
  (let ([section -1])
    (lambda ()
      (set! section (+ section 1))
      section)))

@ A normal section is set with a section number and nothing else. We
want to print using the {\tt M} macro. In this and in starred
sections, we want to typeset the code section if we have one right
after it. 

@c (port tokens sectnum encode sections)
@<Process a normal section@>=
(define-values (body has-body?)
  (let ([maybe (cadr tokens)])
    (if (string? maybe) 
        (values maybe #t)
        (values "" #f))))
(format port "\\M{~a}~a" sectnum (texify-section-text body))
(let ([leftover @<Weave optional code chunk@>])
  (format port "\\fi~n~n")
  leftover)

@ Processing a starred section is not unlike processing a normal
section, except that we need to extract the depth from the starred
section.

@c (port tokens sectnum encode sections)
@<Process a starred section@>=
(define has-body? #t)
(define-values (depth body)
  @<Scrape depth and body from starred section@>)
(printf "*~a" sectnum)
(format port "\\N{~a}{~a}~a~n"
  depth sectnum (texify-section-text body))
(let ([leftover @<Weave optional code chunk@>])
  (format port "\\fi~n~n")
  leftover)

@ When we tokenize our code, it recognizes the |@@*| sign, but it
won't do any parsing of the body of that section. Namely, you may have
an extra star in the section, indicating a zero depth, or you may have
a number, indicating a section depth that much in addition to the
default depth of one.  We need to strip out this extra information
from the body, as we don't want to typeset the number or the extra
star. In our form, we'll return the new body as we have it, and the
depth in two separate values.

@c (tokens)
@<Scrape depth and body from starred section@>=
(define orig
  (let ()
    (unless (string? (cadr tokens))
      (error #f "Section contains no body" (list-head tokens 2)))
    (cadr tokens)))
(define (strip-whitespace lst)
  (cond
    [(null? lst) '()]
    [(char-whitespace? (car lst)) (strip-whitespace (cdr lst))]
    [else lst]))
(define (extract-number cur body)
  (cond
    [(null? body) (error #f "Section contains no body" orig)]
    [(char-numeric? (car body))
     (extract-number (cons (car body) cur) (cdr body))]
    [else
      (if (null? cur)
          (values 1 (list->string body))
          (values
            (string->number (list->string (reverse cur)))
            (list->string body)))]))
(let ([body (strip-whitespace (string->list orig))])
  (cond
    [(null? body) (error #f "Section contains no body" orig)]
    [(char=? #\* (car body)) 
     (values 0 (list->string (strip-whitespace (cdr body))))]
    [else (extract-number '() body)]))

@ After we weave the textual part of the sections, we need to handle
any cases where we have code. We need to parse the code if it is
there, but do nothing if there is no code there. We should assume
at this point that we have a |tokens| value that has the text sections
still in them. There are three cases that we may encounter if the user
has actually provide a code chunk for the section.
We may at first discover a program chunk for the top-level. We may
have a named chunk without a captures and exports list, and we may
finally have a case where the captures list is given. 

@c (tokens port sectnum encode sections has-body?)
@<Weave optional code chunk@>=
(let ([txttkns ((if has-body? cddr cdr) tokens)])
  (cond
    [(null? txttkns) '()]
    [(not (symbol? (car txttkns)))
     (error #f "expected control code" (car txttkns))]
    [else
      @<Print spacer if we have a text body@>
      (case (car txttkns)
        [(@@p) @<Weave program chunk@>]
        [(@@<)
         (let ([captures '()] [exports '()])
           @<Weave named chunk@>)]
        [(@@c) @<Weave captures and named chunk@>]
        [(@@|(|) @<Weave file chunk@>]
        [(|@@ | @@*) txttkns]
        [else
          (error #f "unrecognized code" (car txttkns))])]))

@ In each of the following possibilities for weaving code, 
we will need to put vertical space between the text body 
and the code part of a section if there is a text body. 
On the other hand, if there is no text body, then we do not 
want to put that space, and we want to start the code body 
immediately instead. We do this here so that this operation 
is not scattered throughout the rest of the code.

@c (port txttkns has-body?)
@<Print spacer if we have a text body@>=
(when (memq (car txttkns) '(@@p @@< @@c @@|(|)) ;)
  (format port "~n~@[\\Y~]" has-body?))

@ Weaving a program chunk is by far the easiest of the options.
The basic format for doing a top-level piece of program code is to
print the space and then move directly into code mode before printing
the code in a pretty format. We then want to end the paragraph and
complete the section.

@c (port txttkns sectnum encode)
@<Weave program chunk@>=
(let-values ([(rest body) 
	      (slurp-code (cdr txttkns) 
			  encode 
			  clean-specials 
			  chezweb-pretty-print)])
  (format port "\\B ~a \\par~n" body)
  rest)

@ If we encounter a captures code, this means that we should expect
a captures line followed by a named chunk element. We can read
the captures and exports from the captures line using
the previously defined |parse-captures-line|. Then we can do
what we would do for any named chunk.

@c (port sectnum txttkns encode sections)
@<Weave captures and named chunk@>=
(unless (and (pair? (cdr txttkns)) (string? (cadr txttkns)))
  (error #f "expected captures line"
    (list-head txttkns (min (length txttkns) 2))))
(let-values ([(captures exports) (parse-captures-line (cadr txttkns))])
  (let ([txttkns (cddr txttkns)])
    @<Weave named chunk@>))

@ When we weave a named chunk, we need to know the captures and
exports that are mentioned for the current chunk. We don't have to
worry about the global captures like we do in tangling. Otherwise,
formatting follows a slightly more complicated template:

\medskip\verbatim
\B\4\X<sectnum>:<name>\X${}\E{}$\6
<code>\par<cap_exps><cross_refs>\fi
!endverbatim \medskip

\noindent The above form is basically the same for file chunks,
except that we do some different things with the name of the file
in terms of formatting. This means we can abstract away the code that
manages the printing for both. 

We deviate from the above template a little bit in the case where we
have a chunk that is extending another section definition of the same
name. Specifically, we do not print the cross-references, and we add a +
sign before the equivalence sign above. 

@p
(define (print-named-chunk port texify name code sectnum caps exps sections)
  @<Clean the captures and exports@> @.print-named-chunk@>
  (format port
    "\\B\\4\\X~a:~a\\X${}~@[~a~]\\E{}$\\6~n~a\\par~n~?~?~@[~?~]"
    sectnum (texify name)
    (and (not (weave-sec-def? sections name sectnum)) "\\mathrel+")
    code
    "~@[\\CAP ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}.~n~]"
    (list (and (not (null? clean-caps)) clean-caps))
    "~@[\\EXP ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}.~n~]"
    (list clean-exps)
    (and (weave-sec-def? sections name sectnum) "~@[~a~]~@[~a~]")
    (list (weave-sec-defs sections name) (weave-sec-refs sections name))))

@ Before we can put any identifiers into the captures or exports lists, 
we have to clean them and format them with an appropriate small typewriter 
font. 

@c (caps exps) => (clean-caps clean-exps)
@<Clean the captures and exports@>=
(define (clean id) (format "{\\smtt ~a}" (clean-specials id)))
(define clean-caps (and caps (map clean caps)))
(define clean-exps (and exps (map clean exps)))

@ Now we can easily handle the named chunk.

@c (txttkns port sectnum captures exports encode sections)
@<Weave named chunk@>=
(unless (<= 4 (length txttkns))
  (error #f "Missing pieces of a named chunk" txttkns))
(let ([name (list-ref txttkns 1)]
      [delim (list-ref txttkns 2)])
  (unless (string? name)
    (error #f "expected name for chunk" name))
  (unless (eq? '@@>= delim)
    (error #f "expected delimiter @@>=" delim))
  (let-values ([(rest body) 
                (slurp-code (list-tail txttkns 3) 
			    encode clean-specials 
			    chezweb-pretty-print)])
    (print-named-chunk @.print-named-chunk@>
      port texify-section-text name body sectnum captures exports
      sections)
    rest))

@ And we can use the same basic techniques to handle the file
chunk, with some slight variations on the codes that we're looking
for. Namely, a file chunk does not have any captures or exports.

@c (txttkns port sectnum encode sections)
@<Weave file chunk@>=
(unless (<= 4 (length txttkns))
  (error #f "Missing pieces of a named chunk" txttkns))
(let ([name (list-ref txttkns 1)]
      [delim (list-ref txttkns 2)])
  (unless (string? name)
    (error #f "expected filename for chunk" name))
  (unless (eq? '@@>= delim)
    (error #f "expected delimiter @@>=" delim))
  (let-values ([(rest body) 
                (slurp-code (list-tail txttkns 3) 
			    encode clean-specials
			    chezweb-pretty-print)])
    (print-named-chunk @.print-named-chunk@>
      port texify-filename name body sectnum '() #f sections)
    rest))

@ We have now completely defined the |weave-file| procedure, which we
@.weave-file@> @.chezweave@> @.cheztangle@>
will use in the {\tt chezweave} program. This program has the exact
same signature and layout as the {\tt cheztangle} program, except that
it uses |weave-file| instead of |tangle-file|.

@(chezweave.ss@>=
#! /usr/bin/env scheme-script
(import (chezscheme))

(define-syntax (include-chezweb x)
  (define chezweb-home (getenv "CHEZWEBHOME"))
  (source-directories
    (cons (or chezweb-home (path-parent (car (command-line))))
          (source-directories)))
  (syntax-case x ()
    [(k) #`(#,(datum->syntax #'k 'include) "chezweb.ss")]))

(module (weave-file display-chezweb-version) (include-chezweb))

(display-chezweb-version "CHEZWEAVE")

(unless (= 1 (length (command-line-arguments)))
  (printf "Usage: chezweave <web_file>\n")
  (exit 1))

(unless (file-exists? (car (command-line-arguments)))
  (printf "Specified file '~a' does not exist.\n"
    (car (command-line-arguments)))
  (exit 1))

(weave-file (car (command-line-arguments)))
(exit 0)

@* Pretty printing. To get the pretty printer, we need a token stream, 
but it is important that we have a token stream that does not 
strip important information from our code. We will use |read-token| to 
assist with this, but since it strips out important information such 
as the spaces and newlines in code, we will need to wrap this function 
in another that does not do this stripping. It takes in a string 
and tokenizes it appropriately, giving back a list of tokens. 
These are the valid token types:

@c () => ()
@<Pretty printer@>=
(define token-types '(whitespace newline token comment))

@ Are token list will be a list of token values, where a token value 
is a list whose |car| is a token type, and whose |cdr| contains 
relevant information for that token type. 

\medskip{\parindent = 1in 
\item{\sl whitespace} Stores the number of spaces of whitespace to have
\item{\sl newline} No additional information necessary
\item{\sl token} Stores the token subtype, it's value, and
its string representation
\item{\sl comment} The comment string
\par}\medskip

@c () => (string->token-list)
@<Pretty printer@>=
(define (string->token-list str)
  (define (token-list p)
    (let ([c (get-char p)])
      (cond
       [(eof-object? c) '()]
       [(eq? #\newline c) (cons `(newline) (token-list p))]
       [(char-whitespace? c)
	(let ([cnt (slurp-whitespace p)])
	  (cons `(whitespace ,cnt) (token-list p)))]
       [(eq? #\; c) 
	(let ([line (get-line p)])
	  (cons `(comment ,line) (token-list p)))]
       [else
	 (file-position p (- (file-position) -))
	 (let-values ([(type val s e) (read-token p)])
	   (let ([srep (get-string-n p (- e s))])
	     (cons `(token ,type ,val ,srep) (token-list p))))])))
  (let ([p (open-string-input-port str)])
    (let ([tlst (token-list p)])
      (close-port p)
      tlst)))

@ The above code requires |slurp-whitespace| to be defined.

@c () => ()
@<Pretty printer@>=
(define (slurp-whitespace p)
  (let loop ([cnt 1])
    (let ([c (peek-char p)])
      (cond
	[(eof-object? c) cnt]
	[(eq? #\newline c) cnt]
	[(char-whitespace? c) (get-char p) (loop (+ cnt 1))]
	[else cnt]))))

@* Pretty Printing. We want to implement some sort of pretty printing,
but at the moment, that is still pretty difficult. 
Instead, we do this sort of pseudo printing. This gets similiar results
to a verbatim environment when combined with the |clean-specials| 
procedure. Basically, we are explitizing all of the spaces in our file
so that we can see them. We also turn any tabs into eight spaces. 
We are assuming here that our code has been safely processed through 
something that properly escapes any of the special \TeX\ codes when 
it needs to. That is, we want the code to be preprocessed for the
\TeX\ environment. 

@p
(define chezweb-pretty-print
  (let ([subs `((#\space . (#\space #\\))
		(#\newline . ,(reverse (string->list "\\6\n")))
		(#\tab . ,(reverse (string->list "\\ \\ \\ \\ \\ \\ \\ \\ "))))])
    (lambda (code)
      (let loop ([res '()] [clst (string->list (strip-whitespace code))])
	(cond
	 [(null? clst) (format "{\\tt ~a}" (list->string (reverse res)))]
	 [(assq (car clst) subs) =>
	  (lambda (sub) (loop (append (cdr sub) res) (cdr clst)))]
	 [else (loop (cons (car clst) res) (cdr clst))])))))

@ Normally a Scheme identifier name does not need any escaping or cleaning
to be used directly in \TeX\ code. However, when special characters like
|%| and |$| appear, this requires some intervention. 
Before we write any Scheme code to our \TeX\ output we need to make sure
that all of these special characters have been escaped. 

@p
(define clean-specials 
  (let ()
    (define (revstr str) (reverse (string->list str)))
    (define subs
      `((#\| . ,(revstr "\\vrt{}"      ))
	(#\\ . ,(revstr "$\\backslash$"))
	(#\{ . ,(revstr "$\\{$"        ))
	(#\} . ,(revstr "$\\}$"        ))
	(#\% . ,(revstr "\\%"          ))
	(#\& . ,(revstr "\\AM{}"       ))
	(#\~ . ,(revstr "\\~{}"        ))
	(#\$ . ,(revstr "\\$"          ))
	(#\^ . ,(revstr "\\^{}"        ))
	(#\_ . ,(revstr "\\_{}"        ))
	(#\# . ,(revstr "\\#"          ))))
    (lambda (x) 
      (let loop ([res '()] [clst (string->list (format "~a" x))])
	(cond
	  [(null? clst) (list->string (reverse res))]
	  [(assq (car clst) subs) =>
	   (lambda (sub) (loop (append (cdr sub) res) (cdr clst)))]
	  [else (loop (cons (car clst) res) (cdr clst))])))))

@ We also want to handle the printing of some of the section text. In
this case, all of the vertical bars that are found in such text need
to be handled. Text inside of the vertical bars should be escaped
so that the user doesn't accidently trigger the special mode of
the verbatim mode, which is done with an exclamation mark. We can
escaped the exclamation marks by doubling them. 

@p
(define (texify-section-text text)
  (let loop ([text (string->list text)] [res '()] [bar? #f])
    (cond
      [(null? text) (list->string (reverse res))]
      [(char=? #\| (car text))
       (loop (cdr text) (cons #\| res) (not bar?))]
      [(char=? #\! (car text))
       (if bar?
           (loop (cdr text) (cons* #\! #\! res) bar?)
           (loop (cdr text) (cons #\! res) bar?))]
      [else
        (loop (cdr text) (cons (car text) res) bar?)])))

@ When we have a file chunk, we format the text of the name
of the chunk differently than with a named chunk. We wrap it in
italics using the double backslash macro.

@p
(define (texify-filename txt)
  (format "\\\\{~a}" txt))

@* Handling the Indexing. Generating the index is a combined
matter of explicit and implicit indexing. At the moment, we do
implicit indexing of the captures and exports on named sections, 
and we also support explicit indexing.
There are three explicit index codes:

\medskip{\parindent = 2em
\item{|@@\^|} Typeset the index in roman type
\item{|@@.|} Typeset the index in typewriter type
\item{|@@:|} Typeset the index using the \.{\\9} macro.
\par}\medskip

\noindent These form the main part of the explicit index. 
There is also implicit indexing of the code section names. 
Named chunks are listed
after the index is complete. Generating the index and the chunk
names can be done in two distinct passes. Let's deal with normal
indexing first.

One major issue with using the tokens and not turning them into an
abstract syntax tree before we process them is the unstructured nature
of the text. Text and code elements may be split up by index
annotations. As such, we want to run the indexer first to eliminate
these and unify the token list to the format we expect in the
weaving code. To do this, we will return the new token list with
all of the explicit elements removed, and all of the string elements
that were separated by index entries concatenated together.

We will index based on the section number, rather than by page
number. This makes things easier, since we don't have to handle
anything at the \TeX\ level. 

@p
(define (write-index file tokens) @.write-index@>
  (let ([ofile (format "~a.idx" (path-root file))]
        [index (make-hashtable string-hash string=?)])
    (printf "Writing index file...~n")
    (call-with-output-file ofile
      (lambda (port)
        (let loop ([tokens tokens] [res '()] [sectnum 0])
          @<Dispatch on token type@>))
      'replace)))

@ We have a couple of cases that we can encounter when we deal with a
token. At the end of the token list we need to print the index
that we have accumulated. We increment the section number whenever we
encounter a new code that starts a section (either |@@*| or |@@ |).
Otherwise, we need to concatenate the texts between control codes
together.

@c (tokens index port res sectnum loop)
@<Dispatch on token type@>=
(cond
  [(null? tokens) @<Write index to file@> (reverse res)]
  [(memq (car tokens) '(@@* |@@ |))
   (loop (cdr tokens) (cons (car tokens) res) (1+ sectnum))]
  [(memq (car tokens) '(@@^ @@. @@:)) @<Handle index token@>]
  [(eq? '@@c (car tokens)) @<Index captures line@>]
  [(symbol? (car tokens))
   (loop (cdr tokens) (cons (car tokens) res) sectnum)]
  [(string? (car tokens)) @<Deal with string token@>]
  [else
    (error #f "unrecognized token" (car tokens))])

@ For every index entry that we encounter, we need to update the
index table with the new section and remove the index entry from
the token list. We also want to verify that we have a valid
form of index entry. There are three different ways to enter
something into the index. We need to sort them all the same,
irrespective of the type, but we want to record the different
types. 

@c (tokens index loop sectnum res)
@<Handle index token@>=
(let ([code (car tokens)])
  @<Verify index syntax@>
  (index-db-insert index code (cadr tokens) sectnum)
  (loop (cdddr tokens) res sectnum))

@ We don't want to add any elements to our section number lists that is
there already, so we have |set-cons| to do this for us.

@p @.set-cons@>
(define (set-cons x lst) (if (memv x lst) lst (cons x lst)))

@ We also want to make it easy to insert a new entry into the index
database at any time, so we will make an insertion procedure that will
insert a new index with a given section into the index database.

@p
(define (index-db-insert index code entry sectnum) @.index-db-insert@>
  (hashtable-update! index (strip-whitespace entry)
    (lambda (db)
      (let ([res (assq code db)])
        (set-cdr! res (set-cons sectnum (cdr res)))
        db))
    (map list '(@@^ @@. @@: @@|\|))))

@ Our index syntax is easy to verify. It should have the beginning
control code, followed by a string which is the index entry, and
the closing |@@>| tag.

@c (tokens)
@<Verify index syntax@>=
(unless (<= 3 (length tokens))
  (error #f "invalid index entry" tokens))
(unless (string? (cadr tokens))
  (error #f "expected index entry text" (list-head tokens 3)))
(unless (eq? '@@> (caddr tokens))
  (error #f "expected index entry closer" (list-head tokens 3)))

@ When we encounter a captures line, we want to insert an index entry
for every symbol that we encounter in that line. We will make use of the
|parse-captures-line| procedure to get out the symbols for the captures
@.parse-captures-line@>
line.

@c (loop tokens sectnum res index)
@<Index captures line@>=
(define (insert x) 
  (and (< 1 (string-length x))
       (index-db-insert index '@@|\| x sectnum)))
(let ([body (cadr tokens)])
  (unless (string? body) (error #f "expected captures line" body))
  (let-values ([(captures exports) (parse-captures-line body)])
    (for-each insert (map symbol->string captures))
    (and exports (for-each insert (map symbol->string exports))))
  (loop (cddr tokens) (cons* (cadr tokens) (car tokens) res) sectnum))

@ When we encounter a string in the token list, we need to make sure
that we concatenate it in the right order if the string at the head of
the |res| list is a string. This case will arise whenever we cut out
the index tokens in between the two strings. Otherwise, we can just
leave it and add it to the result list as normal.

@c (loop tokens res sectnum)
@<Deal with string token@>=
(loop (cdr tokens)
  (if (and (pair? res) (string? (car res)))
      (cons
        (string-append (car res) (car tokens))
        (cdr res))
      (cons (car tokens) res))
  sectnum)

@ Once we have the entire index in the form of a hashtable, we want
to sort them in the appropriate order and print each one. We 
print the identifiers first, 
the |@@:| codes second, followed by |@@^| and |@@.|. Each
code is associated with a number of sections, and we 
print the sections in ascending order. A single index entry looks
like:

\medskip\verbatim
\I<entry>, <sections>.
!endverbatim \medskip

\noindent In this example, the entry is formatted according to
the code and the sections are comma separated.

@c (port index)
@<Write index to file@>=
(define (print name macro sects)
  (format port "\\I~a{~a}, ~{~a~^, ~}.~n" macro name (list-sort < sects)))
(for-each
  (lambda (entry)
    (let ([name (clean-specials (car entry))]
          [ident (cdr (assq '@@|\| (cdr entry)))]
          [roman (cdr (assq '@@^ (cdr entry)))]
          [typew (cdr (assq '@@. (cdr entry)))]
          [nine (cdr (assq '@@: (cdr entry)))])
      (when (pair? ident) (print name "\\\\" ident))
      (when (pair? nine) (print name "\\9" nine))
      (when (pair? roman) (print name " " roman))
      (when (pair? typew) (print name "\\." typew))))
  (list-sort (lambda (a b) (string<=? (car a) (car b)))
    (let-values ([(key val) (hashtable-entries index)])
      (map cons (vector->list key) (vector->list val)))))

@* 2 Stripping the indexes for tangling. When we are tangling a file
we don't care a hoot about the indexes. Thus, we should have a
simplified function that just gets rid of the
index elements entirely and gives us a
clean token list that conforms to what our tangling algorithm
expects.

@p
(define (cleanse-tokens-for-tangle tokens) @.cleans-tokens-for-tangle@>
  (let loop ([tokens tokens] [res '()])
    (cond
      [(null? tokens) (reverse res)]
      [(memq (car tokens) '(@@: @@^ @@.))
       @<Verify index syntax@>
       (loop (cdddr tokens) res)]
      [(string? (car tokens))
       (if (and (pair? res) (string? (car res)))
           (loop (cdr tokens)
             (cons (string-append (car res) (car tokens)) (cdr res)))
           (loop (cdr tokens) (cons (car tokens) res)))]
      [else
        (loop (cdr tokens) (cons (car tokens) res))])))

@* 2 Indexing the section names. We want to generate an index for the
names of all the named sections or files that have been created. This is
done by having a separate pass |index-sections| that parses the tokens,
writes the results out to a section name index file, and returns a
hashtable with all of the section information in it. We use this section
information whenever we want to write the section cross references or
when we want to know what section number to use when rendering a section
reference inside of a code body. The table is keyed on section names,
which are strings with the whitespace stripped from them. 
The value field is a |section-info| type that is described further down,
it gives information as to the type of the section, the list of sections
where that chunk name is defined, and where the chunk is referenced. 

@p
(define (index-sections file tokens) @.index-sections@>
  (printf "Writing section index...")
  (let ([sections (make-hashtable string-hash string=?)])
    (let loop ([tokens tokens] [sectnum 0])
      (when (pair? tokens)
        (case (car tokens)
          [(@@*) 
           (printf "*~a" (1+ sectnum))
           (loop (cdr tokens) (1+ sectnum))]
          [(|@@ |) (loop (cdr tokens) (1+ sectnum))]
          [(@@< |@@(|) @<Process named chunk@>] ;)
          [else (loop (cdr tokens) sectnum)])))
    @<Write sections index@>
    (printf "~n")
    sections))

@ We have three main pieces of information that we want to keep around
when dealing with a section, the type of the section, which should be
either a file or a name chunk, and then the section numbers where the
chunk is defined, and the section numbers where the chunk is
referenced. We encapsulate this information inside of a record for
easier use.

@p
(define-record-type section-info 
  (fields type defs refs)
  (protocol
    (lambda (n)
      (lambda (type defs refs)
        (unless (or (not type) (memq type '(@@< @@|(|))) ; )
          (error #f "invalid type" type))
        (unless (and (list? defs) (for-all integer? defs))
          (error #f "invalid defs list" defs))
        (unless (and (list? refs) (for-all integer? refs))
          (error #f "invalid refs list" refs))
        (n type defs refs)))))

@ When we encounter a named chunk either in file or regular form, we
want to add either a new section to the list, or we want to extend the 
existing form. Either we have a definition form, or we have a reference
from, depending on the closing delimiter.

@c (sections loop tokens sectnum)
@<Process named chunk@>=
(unless (<= 3 (length tokens)) (error #f "unexpected end of file" tokens))
(let ([type (car tokens)] [name (cadr tokens)] [delim (caddr tokens)])
  (unless (string? name) (error #f "expected chunk name" name))
  (unless (memq delim '(@@> @@>=)) (error #f "invalid delimiter" delim))
  (hashtable-update! sections (strip-whitespace name)
    (lambda (cur) 
      (let ([defs (section-info-defs cur)]
            [refs (section-info-refs cur)])
        (when (and (section-info-type cur) 
                   (not (eq? type (section-info-type cur))))
          (error #f "section type mismatch" name))
        (case delim 
          [(@@>) (make-section-info type defs (set-cons sectnum refs))]
          [(@@>=) (make-section-info type (set-cons sectnum defs) refs)]
          [else (error #f "this can't happen")])))
    (make-section-info #f '() '()))
  (loop (list-tail tokens 3) sectnum))

@ When we have finally built the entire section index, we generate
the file \.{name.scn} that contains a set of the entries where each
entry has the following form:

\medskip\verbatim
\I\X<setcnums>:<sectname>\X
<Uses>
!endverbatim \medskip

\noindent The section numbers are comma separated sets of decimal
numbers, and the section name includes the wrapping for the different
type of section. The usage string is a normal usage string as generated
by |weave-sec-refs|. We should be careful to escape out any special 
characters that are a part of the section name. If we have no section
numbers listed we will use a placeholder of |'(0)|.

@c (sections)
@<Write sections index@>=
(define (print-index port nums name type)
  (format port "\\I\\X~{~a~^, ~}:~?\\X~n~@[~a~n~]"
    (list-sort < (if (null? nums) '(0) nums))
    (case type [(@@|(|) "\\\\{~a}"] [(@@<) "~a"]) ;)
    (list name)
    (weave-sec-refs sections name)))

@ We want to use |print-index| on each section that we receive in the 
|sections| hashtable. All of the entries are sort alphabetically 
in ascending order. 
@.sections@> @.print-index@>

@c (file sections)
@<Write sections index@>=
(call-with-output-file (format "~a.scn" (path-root file))
  (lambda (port)
    (for-each 
      (lambda (e)
        (let ([name (car e)]
              [nums (section-info-defs (cdr e))]
              [type (section-info-type (cdr e))])
          (print-index port nums name type)))
      (let-values ([(keys vals) (hashtable-entries sections)])
        (list-sort (lambda (a b) (string<? (car a) (car b)))
          (map cons (vector->list keys) (vector->list vals))))))
  'replace)

@ This section information is especially useful when we want to do the 
encoding of the chunk references. If we have a chunk reference, we need
to typeset it specially inside of the code environment to make sure
that it looks right. We do this by defining an encoder that takes a 
single name string, stripped of its whitespace, and returns another
string that is suitable for entering it into the code environment.
We typeset section
names according to the following template, where 5 is the section number
and ``blah'' is the name of the section. 

\medskip\verbatim
\X5:blah\X
!endverbatim \medskip

\noindent Our encoder will close over a given, specific |sections| 
@.sections@>
database.

@c (sections) => (encode)
@<Define weave chunk reference encoder@>=
(define (encode x)
  (let ([res (hashtable-ref sections x (make-section-info '@@< '() '()))])
    (format "\\X~a:~?\\X"
      (let ([defs (section-info-defs res)])
        (if (null? defs) "" (fold-left min (most-positive-fixnum) defs)))
      (let ([type (section-info-type res)])
        (case type [(@@<) "{\\rm ~a}"] [(@@|(|) "\\\\{~a}"])) ; )
      (list x))))

@ When we are weaving, and in the index, we have a concept of section
cross references. This tells you a few things. Firstly, it tells you
where your sections are defined, especially if they are defined in
multiple places, or concatenated together, and it tells you where your
sections are used or referenced. We use these references in a few
places. All of the data is encapsulated in the sections hashtable
@^Sections Hashtable@> discussed above. Whenever you define a chunk, the
woven output will tell you whether that chunk is extended somewhere
else and who else uses it. When you are concatenating or extending a 
chunk, we do not print this information. 

In the sections index, we need to print the section numbers where the
chunk is defined, and also where it is used. Since there are many places
where we want to use this functionality, we define the following
procedures to help with the task. Firstly, two procedures
|weave-sec-defs| and |weave-sec-refs| give back a format string or
false, suitable for putting at the bottom of woven code chunks. These
use the |A| and |U| macros, respectively. We also provide a function
|weave-sec-def?| which tells you whether or not a given section name 
has its first definition at the given section number. 

Let's begin with the |weave-sec-defs| procedure. It has the following
signature:

$$\.{weave-sec-defs} : \\{sections}\ \\{name} \to \\{defs-fmt}$$

\noindent The |sections| argument is a sections hashtable, and the 
|name| argument should be a section name. We 
return the format string of section numbers 
where part of the code for that chunk
is defined. We return false if we cannot find the definitions in the 
sections table. 

The format string itself should be the |A| macro followed by 
an |s| if there is more than one extra section definition site, and then
the list of comma separated section number or numbers.

We only use this function in the main chunk printing, so we don't have
to worry about its use in the section index. This makes a difference,
and this is why we don't provide a string if there is only one section
number that defines a section. Specifically, we only give a cross
reference to other section definitions if other sections exist. It
doesn't make much sense to cross-reference yourself. However, this means
that this procedure makes no sense in the context of the section index,
since the section index needs to include all of the section numbers.
Fortunately, the formatting for the section index is different, simpler
even. Thus, we don't use this procedure for that.

@p
(define (weave-sec-defs sections name) @.weave-sec-defs@>
  (let ([res (hashtable-ref sections (strip-whitespace name) #f)])
    (and res 
         (let ([defs (section-info-defs res)])
           (and (pair? defs) (>= (length defs) 2)
                (format "\\A~[~;~{~s~}~;s~{~s and ~s~}~
                         ~:;s~{~#[~; and ~] ~s~^,~}~]."
                        (-1+ (length defs)) 
                        (cdr (list-sort < defs))))))))

@ The |weave-sec-refs| procedure is the same, except that it uses the 
|U| macro and it uses section references instead of definitions.

@p
(define (weave-sec-refs sections name) @.weave-sec-refs@>
  (let ([res (hashtable-ref sections (strip-whitespace name) #f)])
    (and res 
         (let ([refs (section-info-refs res)])
           (and (pair? refs) 
                (format "\\U~[~;~{~s~}~;s~{~s and ~s~}~
                         ~:;s~{~#[~; and ~] ~s~^,~}~]."
                        (length refs) (list-sort < refs)))))))

@ The |weave-sec-def?| procedure has the following signature:

$$\.{weave-sec-def?} : \\{sections}\ \\{name}\ \\{secnum} \to \&{boolean}$$

\noindent It will return true if the section number provided is the
lowest number of the definition sections for that given section name. In
other words, it will return true if the section number is the first time
that the given section |name| is defined.

@p
(define (weave-sec-def? sections name num) @.weave-sec-def?@>
  (let ([res (hashtable-ref sections (strip-whitespace name) #f)])
    (and res 
         (let ([defs (section-info-defs res)])
           (and (pair? defs) (= num (car (list-sort < defs))))))))

@* TeX Macros.  See the {\tt chezwebmac.tex} file for the macros that are 
used in this file as well as for the default macros that are available to 
the user of a \ChezWEB\ program. 

@* Index.

