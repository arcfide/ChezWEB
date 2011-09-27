\def\ChezWEB{Chez{\tt WEB}}
\def\CWEB{{\tt CWEB}}
\def\WEB{{\tt WEB}}

\font\rm = "STIXGeneral" at 12pt

\hoffset = 0.5in
\voffset = 0.5in
\hsize = 5.5in
\vsize = 8in

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

@* The ChezWEB Runtime. Normal \CWEB\ programs do not have any
runtime, and they operate completely at the equivalent of a macro
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

\numberedlist
\li {\bf Hygiene.}
Any definition introduced in the body of the chunk that is not
explicitly exported by the export and captures clauses is visible only
within the scope of the chunk, and is not visible to any surround
context that references the chunk.

\li {\bf Referential Transparency.}
Any free reference that appears in the body of the chunk will refer to
the nearest lexical binding of the tangled output unless they are
explicitly enumerated in the captures clause, in which case, they will
refer to the nearest binding in the context surrounding the chunk
whenever the chunk is referenced.
\endnumberedlist

\noindent A subtlety occurs in the actual use of the referential
transparency property. Because the user does not have direct control
over the location of the chunk definitions when tangling a WEB file,
this means that there is a more restricted notion of what the scope is
for a chunk than would be normally if the tangling runtime were used
as a program time entity. On the other hand, this restriction only
applies to the specific way in which \ChezWEB\ combines/tangles a WEB
file, and it does not apply to how an user may use the chunking
mechanism. Users may, in fact, place chunk definitions arbitrarily in
their own code, if they would like to do so. In this case, the
referential transparency property must still hold in its full
generality.

In the case when we are only dealing with chunks defined through the
WEB mechanism and tangled explicitly, the result is that all chunks
will be defined at the top level of the file after the definition of
the runtime, but before the inclusion of any other top level
elements. This means that in practice, the lexical scope of any free
variable in a chunk is the top-level of the file in which the chunk
appears. So, top-level definitions will match for any free reference
in the chunk body, but these are really the only references that are
likely to be resolvable unless one explicitly captures them through
means of the capture facility.

The macro itself takes the following syntax:

\medskip\verbatim
(@@< (name capture ...) body+ ...)
(@@< (name capture ...) => (export ...) body+ ...)
|endverbatim
\medskip

\noindent The first instance is the value form of a chunk. It binds
|name| to an identifier syntax that will, when referenced, expand into
a form that evaluates |body+ ...| in its own scope, where |capture
...| are bound to the values visible in the surrounding
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

(define-syntax @@<
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
|endverbatim
\medskip

\noindent Notice the use of |ic ...| and |oc ...|. These are the
inner/outer bindings that correspond exactly to one another except
that they capture different lexical bindings. That is, we create the
|oc| bindings by rewrapping the |ic| bindings with the wraps (marks
and substitutions) of the location where the |name| is referenced.  We
use |alias| to link the two identifiers to the same underlying
location.

@(runtime.ss@>=
(define-syntax (build-value-form x)
  (syntax-case x ()
    [(_ id (ic ...) body ...)
     (with-syntax
         ([(oc ...) (datum->syntax #'id (syntax->datum #'(ic ...)))])
       #'(let () (alias ic oc) ... body ...))]))

@ This form is used as a part of the |value-form| macro, which is what
does the initial definition of the macro for |name|. The |name| macro
is just an identifier syntax that has clauses for the single
identifier use and the macro call, but nothing for the |set!| clause,
since that doesn't make sense. Because we don't care about this case,
we can avoid the use of |make-variable-transformer| and instead use a
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
|endverbatim
\medskip

\noindent This seems like it should be fine, and we expect that if we
use something like the following:

\medskip\verbatim
(@@< (|List of a, b, and c|)
  (define-syntax a
    (syntax-rules ()
      [(_ e ...) (list 'e ...)]))
  (a a b c))
|endverbatim
\medskip

\noindent We might end up in some trouble. When run |value-form| on it,
we will get something like this:

\medskip\verbatim
(define-syntax (|List of a, b, and c| x)
  (syntax-case x ()
    [id (identifier? #'id)
     #'(build-value-form #'id #'()
       #'((define-syntax a
            (syntax-rules ()
              [(_ e ...) (list 'e ...)]))
               (a a b c)))]))
|endverbatim
\medskip

\noindent Obviously, the above syntax doesn't work, because there is
no pattern variable |e| in the pattern clause. This means that we will
get an error about an extra ellipses. What we need to do, when we run
|value-form|, is to make sure that the expanded code escapes the
ellipses, so we would expand the two body forms |(define...)| and |(a
a b c)| with ellipses around them instead.

@(runtime.ss@>=
(define-syntax value-form
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
linking technique as above. However, in this case, we need to link
both exports and captures. Furthermore, we need to expand into a
|module| form instead of using a |let| form as we do above.

\medskip\verbatim
(module (oe ...)
  (alias ic oc) ...
  (module (ie ...) body+ ...)
  (alias oe ie) ...)
|endverbatim
\medskip

\noindent In this case, as in the value form, the |ic ...| and
|ie ...| bindings are, respectively, the captures and exports of the
lexical (inner) scope, while the |oc ...| and |oe ...| are the same for
the surrounding context (outer).

@(runtime.ss@>=
(define-syntax (build-module-form x)
  (syntax-case x ()
    [(_ id (ic ...) (ie ...) body ...)
     (with-syntax 
         ([(oc ...) (datum->syntax #'id (syntax->datum #'(ic ...)))]
          [(oe ...) (datum->syntax #'id (syntax->datum #'(ie ...)))])
       #'(module (oe ...)
           (alias ic oc) ...
           (module (ie ...) body ...)
           (alias oe ie) ...))]))

@ And just as we did above for the |value-form| macro, 
we implement the |module-form| macro in the same way, 
taking care to escape the body elements.
Unlike the value form of our call, though, we never expect to have the
|name| identifier syntax referenced at the call position of a form, as
in |(name x y z)| because that is not a valid definition context.
Thus, we only need to define the first form where it appears as a lone
identifier reference in a definition context.

@(runtime.ss@>=
(define-syntax module-form
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
(indirect-export @@<
  module-form value-form build-module-form build-value-form)
)

@* The Runtime Library. For users who wish to use this runtime in
their own code, we will provide a simple library for them to load the
runtime code themselves. This will enable them to use the macro as
their own abstraction and have the chunk like reordering without
actually requiring them to write their entire program in \ChezWEB{}.

@(runtime.sls@>=
(library (arcfide chezweb runtime)
  (export @@< =>)
  (import (chezscheme))
  (include "runtime.ss"))

@* Tokenizing \WEB\ files. If one writes a \ChezWEB\ file in the \WEB\
syntax, we need to parse it into tokens representing either control
codes or text between control codes. We do this by implementing
|chezweb-tokenize|, which takes a port and returns a list of tokens.

$${\tt chezweb-tokenize} : port \rarrow token-list$$

\noindent Fortunately, each and every token can be identified by
reading usually only three characters.  Each control code begins with
an ampersand; most are only two characters, though the |@@>=| form has
more.  This makes it fairly straightforward to build a tokenizer
directly. We do this without much abstraction below.

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

@ Most of the control codes can be determined by reading ahead only
one more character, but there is one that requires reading two more
characters. Additionally, there is an escape control code
(|@@@@|) that let's us escape out the ampersand if we really want to
put a literal two characters into our text rather than to use a
control code. If we do find a token, we want that token to be encoded
as the appropriate symbol. We will first add any buffer left in |cur|
to our tokens list, and then add our symbolic token to the list of
tokens as well. The list of tokens is accumulated in reverse order.

@c (c cur tokens port loop)
@<Parse possible control code@>=
(let ([nc (read-char port)])
  (case nc
    [(#\@@) (loop tokens (cons c cur))]
    [(#\q) (get-line port) (loop tokens cur)]
    [(#\space #\< #\p #\* #\e #\r #\( #\^ #\. #\: #\i #\c) ;)
     (let ([token (string->symbol (string c nc))])
       (if (null? cur)
           (loop (cons token tokens) '())
           (loop
             (cons* token (list->string (reverse cur)) tokens)
             '())))]
    [(#\>) |Parse possible @@>= delimiter|]
    [else
      (if (eof-object? nc)
          (loop tokens cur)
          (loop tokens (cons* nc c cur)))]))

@ When we encounter the sequence |@@>| in our system, we may have a
closing delimiter, but we won't know that until we read ahead a bit
more.  When we do have a closing delimiter, we will ignore all of the
characters after that on the line. In essence, this is like having an
implicit |@@q| character sitting around. We do this in order to
provide a clean slate to the user when writing files, so that
extraneous whitespace is not inserted into a file if the programmer
does not intend it.  Extraneous whitespace at the beginning of a file
can cause problems with things like scripts if the user is using the
|@@(| control code to generate the script file.

@c (port cur loop tokens c nc)
@<Parse possible @@>= delimiter@>=
(let ([nnc (read-char port)])
  (if (char=? #\= nnc)
      (begin 
        (get-line port)
        (if (null? cur)
            (loop (cons '@@>= tokens) '())
            (loop 
              (cons* '@@>= (list->string (reverse cur)) tokens)
              '())))
      (loop tokens (cons* nnc nc c cur))))

@* Tangling a WEB. Once we have this list of tokens, we can in turn
write a simple program to tangle the output. Tangling actually
consists of several steps.

\numberedlist
\li Accumulate named chunks
\li Gather file code and |@@p| code for output
\li Prepend runtime to files
\li Prepend named chunk definitions to those files that use those chunks
\endnumberedlist

\noindent For example, if we have not used the |@@(| control code,
which allows us to send data to one or more files, then we will send
all of our data to the default file. This means that we need to walk
through the code used in all of the |@@p| control codes to find which
named chunks are referenced in the program code. We then take these
definitions and prepend them with the runtime to the appropriate file
name before finally tacking on the code for the file.

The first step is to actually grab our runtime, which we will do when
we compile the program:

@p
(define-syntax (get-code x)
  (call-with-input-file "runtime.ss" get-string-all))
(define runtime-code (get-code))

@ We can now define a program for tangling.
We want a program that takes a single file,
and generates the tangled output.

\medskip\verbatim
cheztangle <web_file>
|endverbatim
\medskip

\noindent We will use an R6RS style program for this, assuming that
all of our important library functions will be installed in {\tt
chezweb.ss}.

@(cheztangle.ss@>=
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
    \strut # & # & # \cr
    {\bf Table} & {\bf Key Type} & {\bf Value Type}
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
tokens list will return a string first only if there is something in
the limbo area of the file. If there is nothing in limbo, there will
be a token first.  We want to loop arround assuming that we receive a
token before any string input, and we don't care about limbo when we
tangle a file, so when we seed the loop, we will take care to remove
the initial limbo string if there is any.

@p
(define (construct-chunk-tables token-list)
  (let 
      ([named (make-eq-hashtable)]
       [top-level (make-hashtable equal-hash equal?)]
       [captures (make-eq-hashtable)])
    (let loop 
        ([tokens 
           (if (string? (car token-list)) 
               (cdr token-list)
               token-list)] 
         [current-captures '()]
         [current-exports #f])
      (if (null? tokens)
          (values top-level named captures)
          |Dispatch on control code|))))

@ On each step of the loop, we will expect to have a single control
code at the head of the |tokens| list.  Each time we iterate through
the loop, we should remove all of the strings and other elements
related to that control code, so that our next iteration will again
encounter a control code at the head of the list.

@c (loop tokens top-level current-captures
     current-exports named captures)
@<Dispatch on control code@>=
(case (car tokens)
  [(|@@ | @@* @@e @@r @@^ @@. @@: @@i) (loop (cddr tokens) '() #f)]
  [(@@p) |Extend default top-level|]
  [(@@<) |Extend named chunk|]
  [(|@@(|) |Extend file top-level|]
  [(@@c) |Update the current captures|]
  [else
    (error #f "Unexpected token" (car tokens) (cadr tokens))])

@ Extending the default top level is the easiest. We just append the
string that we find to the |*default*| key in the |top-level| table.

@c (loop tokens top-level)
@<Extend default top-level@>=
(define body (cadr tokens))
(unless (string? body)
  (error #f "Expected a string body" body))
(hashtable-update! top-level '*default*
  (lambda (cur) (string-append cur body))
  "")
(loop (cddr tokens) '() #f)

@ Handling file name top-level updates works much like a named chunk,
except that we do not have to deal with the issues of capture
variables, which we will discuss shortly. We must verify that we have
a valid syntax in the stream and then we can add the name in. We
should remember to strip off the leading and trailing whitespace from
the name in question.

@c (loop tokens top-level)
@<Extend file top-level@>=
|Verify and extract delimited chunk|
(let ([name (strip-whitespace name)])
  (hashtable-update! top-level name
    (lambda (cur) (string-append cur body))
    ""))
(loop (cddddr tokens) '() #f)

@ Named chunk updates are complicated by the need to track
captures. In the \WEB\ syntax, if you have a capture that you want to
associate with a given named chunk, you list the |@@c| form right
before you define your chunk. When we parse this, we save the captures
as soon as we encounter them so that they can be used in the next
chunk. We reset the captures if we do not find a named chunk as our
next section.

The format of a captures form looks something like this:

\medskip\verbatim
@@c (c ...) [=> (e ...)]
|endverbatim
\medskip

\noindent In the above, the exports are optional, and the captures
could be empty. This will come in to us as a string, so we will
need a way to convert it into a data representation that we can use.
In the following function, we will get two values back that
are the captures and exports, if no exports were provided to us,
then the second value will be false.

@p
(define (parse-captures-line str)
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
@<Update the current captures@>=
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
|endverbatim
\medskip

\noindent In the above code example, we want the end result to have a 
captures list of |a b t| and the exports list to be |x y z u v|. 

@c (loop tokens named current-captures current-exports captures)
@<Extend named chunk@>=
|Verify and extract delimited chunk|
(let ([name (string->symbol (strip-whitespace name))])
  (hashtable-update! named name
    (lambda (cur) (string-append cur body))
    "")
  (hashtable-update! captures name
    (lambda (cur) |Extend captures and exports|)
    #f))
(loop (cddddr tokens) '() #f)

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

@c (tokens) => (name body)
@<Verify and extract delimited chunk@>=
(define-values (name closer body)
  (let ()
    (unless (<= 4 (length tokens))
      (error #f "Unexpected end of file" tokens))
    (apply values (cdr (list-head tokens 4)))))
(unless (eq? '@@>= closer)
  (error #f "Expected closing @@>=" name closer body))
(unless (string? name)
  (error #f "Expected name string" name))
(unless (string? body)
  (error #f "Expected string body" body))

@ We also want to define out own procedure to strip the whitespace
from our strings. We could have used something from the SRFIs, such as
the strip function from SRFI 13, but we will write our own, simplified
version here to keep things easy and also to avoid unnecessary
dependencies on the code, which should, to the best extent possible,
be self-contained. Our basic technique is to take the string, and walk
down the ends from the right and the left to determine where the first
non-whitespace character occurs in each direction.

@p
(define (strip-whitespace str)
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

$$\includegraphics[height=2in]{chezweb-1.eps}$$

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

{\it In the future it might be nice to have the functionality 
of unhygienic textual copy and paste, but such functionality is 
for another time and place.}

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
      (put-string output-port "#!chezscheme\n")
      (put-string output-port runtime-code)
      |Write named chunks to file|)
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
|endverbatim
\medskip

\noindent We grab the captures and exports from the |captures|
hashtable and we are careful to ensure that we don't put any exports 
in the form unless we intend to do so.

We should not have to worry about the ordering that we do the chunks 
in, since they should all be at the same phase and they should be 
definable in any order. 

@c (captures named-chunks output-port)
@<Write named chunks to file@>=
(for-each
  (lambda (name)
    (let ([cell (hashtable-ref captures name '(() . #f))])
      (format output-port
        "(@@< (~s ~{~s ~}) ~@@[=> (~{~s ~})~]~n~a)~n~n"
        name (car cell) (cdr cell)
        (hashtable-ref named-chunks name ""))))
  (vector->list (hashtable-keys named-chunks)))

@ Now all of the pieces are in place to write the |tangle-file|
procedure that we talked about previously.

@p
(define (tangle-file web-file)
  (let ([tokens (call-with-input-file web-file chezweb-tokenize)]
        [default-file (format "~a.ss" (path-root web-file))])
    (let-values ([(top-level-chunks named-chunks captures)
                  (construct-chunk-tables tokens)])
      (for-each
        (lambda (file)
          (let ([output-file (if (eq? '*default* file)
                                 default-file file)])
            |Write tangled file contents|))
        (vector->list (hashtable-keys top-level-chunks))))))

@* Weaving a WEB. Weaving is the process of converting or compiling a
\WEB\ into a \TeX\ file for rendering into a proper document. A
program like Xe\TeX\ can be used on the resulting \TeX\ file that
{\tt chezweave} outputs to make the PDF.

$${\tt Missing figure here.}$$
% $$\includegraphics[width=???]{chezweb-2.eps}$$

\noindent There are three distinct elements that make up
weaving. Firstly, there is the actual weaving itself, which must take
the \WEB\ text and convert it into the appropriate \TeX\ code, but the
system must also handle the pretty printing of code and the
cross-referencing, indexing services that \ChezWEB\
offers. Fortunately, each of these may be handled in their own
passes. In this section we will handle the pass that generates the
\TeX\ file proper. This means we will ignore the index and pretty
printer; they are discussed elsewhere.

At a high level, we will have a list of sections. At a glance, every
section consists of a code and text block. Text blocks are begun by
using |@@ | or |@@*| and code blocks begin with |@@<| or |@@p|. All
other control codes are annotations and notes discribing how to format
text or to instantiate code blocks. We weave output pretty much in
order. We must keep track of the section number. Our macros that we
can use for formatting these chunks are as follows.

\medskip\item{N}
{\bf Starred Sections.} This allows for starred sections. It expects
two integers as the first parameters. These are, respectively, the
depth and the section number. After that, it will read up to the first
period for the section to highlight, and then the rest of the text
will follow after that.
\item{M}
{\bf Normal Sections.} This takes only the section number, and
typesets a normal section.
\item{Y}
{\bf Vertical Space.} Provides a little vertical space for use between
the text and code parts of a section.
\item{B}
{\bf Code.} Begins the typesetting mode for code.
\item{X}
{\bf Chunk Name.} Used to typeset a chunk name in a section, either
for files or for named chunks. It expects a section number, followed
by a colon, followed by another {\tt \X}.
\item{4}
{\bf Backspace.} This is used to backspace a bit, basically, to
backspace one notch.
\item{fi}
{\bf End section.} This follows the end of the section.
\item{inx}
{\bf Index.} Starts up the index.
\item{fin}
{\bf Finish.} Ends the index.
\item{con}
{\bf Sections.} Completes the section names.

\medskip\noindent
Each section has the same basic layout, where it will begin with
either an N or an M macro, and end with {\tt fi}.

Let's examine the top-level loop that iterates over the tokens.

@p
(define (weave-file file)
  (call-with-output-file (format "~a.tex" (path-root file))
    (lambda (port)
      |Define section iterator|
      (format port "\\input chezwebmac~n\\input eplain~n~n")
      (let loop ([tokens (call-with-input-file file
                           chezweb-tokenize)])
        (when (pair? tokens)
          (call-with-values (lambda () |Process a section|)
            loop)))
      (format port "\\inx~n\\fin~n\\con~n"))
    'replace))

@ For any given section, we know exactly what to do by looking at
the associated control code that started it. The only exception is
limbo, where there is no initial code prefixing its content. For limbo
we insert the code block literally into the output. We can divide our
sections and chunks into the following taxonomy:

$${\tt Missing figure here.}$$
%$$\includegraphics[width=???]{chezweb-3.eps}$$

\noindent Note that we do not allow a code section to immediately
follow another code section. Every section must start with a text
section, though that text section may have nothing but whitespace in
it. A section may or may not have any code section in it.

@c (port tokens next-section)
@<Process a section@>=
(define sectnum (next-section))
(case (car tokens)
  [(|@@ |) |Process a normal section|]
  [(@@*) |Process a starred section|]
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
  (let ([section 0])
    (lambda ()
      (set! section (+ section 1))
      section)))

@ A normal section is set with a section number and nothing else. We
want to print using the {\tt M} macro. In this and in starred
sections, we want to typeset the code section if we have one right
after it. 

@c (port tokens sectnum)
@<Process a normal section@>=
(define body
  (let ([maybe (cadr tokens)])
    (unless (string? maybe)
      (error #f "Section contains no body." (list-head tokens 2)))
    maybe))
(format port "\\M{~a}~a~n" sectnum (texify-section-text body))
(let ([leftover |Weave optional code chunk|])
  (format port "\\fi~n~n")
  leftover)

@ Processing a starred section is not unlike processing a normal
section, except that we need to extract the depth from the starred
section.

@c (port tokens sectnum)

@<Process a starred section@>=
(define-values (depth body)
  |Scrape depth and body from starred section|)
(format port "\\N{~a}{~a}~a~n"
  depth sectnum (texify-section-text body))
(let ([leftover |Weave optional code chunk|])
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
    [(char=? #\* (car body)) (values 0 (list->string (cdr body)))]
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

@c (tokens port sectnum)
@<Weave optional code chunk@>=
(let ([txttkns (cddr tokens)])
  (cond
    [(null? txttkns) '()]
    [(not (symbol? (car txttkns)))
     (error #f "expected control code" (car txttkns))]
    [else
      (case (car txttkns)
        [(@@p) |Weave program chunk|]
        [(@@<)
         (let ([captures '()] [exports '()])
           |Weave named chunk|)]
        [(@@c) |Weave captures and named chunk|]
        [(@@|(|) |Weave file chunk|]
        [(|@@ | @@*) txttkns]
        [else
          (error #f "unrecognized code" (car txttkns))])]))

@ Weaving a program chunk is by far the easiest of the options.
The basic format for doing a top-level piece of program code is to
print the space and then move directly into code mode before printing
the code in a pretty format. We then want to end the paragraph and
complete the section.

@c (port txttkns sectnum)
@<Weave program chunk@>=
(unless (and (pair? (cdr txttkns)) (string? (cadr txttkns)))
  (error #f "expected program body"
    (list-head txttkns (min (length txttkns) 2))))
(format port "\\Y\\B ~a \\par~n"
  (chezweb-pretty-print (cadr txttkns)))
(cddr txttkns)

@ If we encounter a captures code, this means that we should expect
a captures line followed by a named chunk element. We can read
the captures and exports from the captures line using
the previously defined |parse-captures-line|. Then we can do
what we would do for any named chunk.

@c (port sectnum txttkns)
@<Weave captures and named chunk@>=
(unless (and (pair? (cdr txttkns)) (string? (cadr txttkns)))
  (error #f "expected captures line"
    (list-head txttkns (min (length txttkns) 2))))
(let-values ([(captures exports)
              (parse-captures-line (cadr txttkns))])
  (let ([txttkns (cddr txttkns)])
    |Weave named chunk|))

@ When we weave a named chunk, we need to know the captures and
exports that are mentioned for the current chunk. We don't have to
worry about the global captures like we do in tangling. Otherwise,
formatting follows a slightly more complicated template:

\medskip\verbatim
\Y\B\4\X<sectnum>:<name>\X${}\E{}$\6
<code>\par<cap_exps>\fi
|endverbatim
\medskip

\noindent The above form is basically the same for file chunks,
except that we do some different things with the name of the file
in terms of formatting. This means we can abstract away the code that
manages the printing for both.

@p
(define (print-named-chunk port name code sectnum caps exps)
  (format port
    "\\Y\\B\\4\\X~a:~a\\X${}\\E{}$\\6~n~a\\par~n~?~?~?"
    sectnum name (chezweb-pretty-print code)
    "~@[This section captures ~{~a~^, ~}.~]"
    (list (and (not (null? caps)) caps))
    "~@[\\6~]"
    (list (and (not (null? caps)) exps))
    "~@[This section exports ~{~a~^, ~}.~]"
    (list exps)))

@ Now we can easily handle the named chunk.

@c (txttkns port sectnum captures exports)
@<Weave named chunk@>=
(unless (<= 4 (length txttkns))
  (error #f "Missing pieces of a named chunk" txttkns))
(let ([name (list-ref txttkns 1)]
      [delim (list-ref txttkns 2)]
      [body (list-ref txttkns 3)])
  (unless (string? name)
    (error #f "expected name for chunk" name))
  (unless (eq? '@@>= delim)
    (error #f "expected delimiter @@>=" delim))
  (unless (string? body)
    (error #f "expected code body" body))
  (print-named-chunk
    port (texify-section-text name) body sectnum captures exports)
  (list-tail txttkns 4))

@ And we can use the same basic techniques to handle the file
chunk, with some slight variations on the codes that we're looking
for. Namely, a file chunk does not have any captures or exports.

@c (txttkns port sectnum)
@<Weave file chunk@>=
(unless (<= 4 (length txttkns))
  (error #f "Missing pieces of a named chunk" txttkns))
(let ([name (list-ref txttkns 1)]
      [delim (list-ref txttkns 2)]
      [body (list-ref txttkns 3)])
  (unless (string? name)
    (error #f "expected filename for chunk" name))
  (unless (eq? '@@>= delim)
    (error #f "expected delimiter @@>=" delim))
  (unless (string? body)
    (error #f "expected code body" body))
  (print-named-chunk
    port (texify-filename name) body sectnum '() #f)
  (list-tail txttkns 4))

@ We have now completely defined the |weave-file| procedure, which we
will use in the {\tt chezweave} program. This program has the exact
same signature and layout as the {\tt cheztangle} program, except that
it uses |weave-file| instead of |tangle-file|.

@(chezweave.ss@>=
#! /usr/bin/env scheme-script
(import (chezscheme))

(module (weave-file)
  (include "chezweb.ss"))

(unless (= 1 (length (command-line-arguments)))
  (printf "Usage: chezweave <web_file>\n")
  (exit 1))

(unless (file-exists? (car (command-line-arguments)))
  (printf "Specified file '~a' does not exist.\n"
    (car (command-line-arguments)))
  (exit 1))

(weave-file (car (command-line-arguments)))
(exit 0)

@* Pretty Printing. We want to implement some sort of pretty printing,
but at the moment, that is still pretty difficult. Instead, we'll just
implement the Chez Scheme pretty printer and avoid the whole
question. Using the verbatim package also helps. In the eplain version
of verbatim, you have to be careful to make sure that the vertical
bars are doubled in case you end up with something that reads like
{\tt ||endverbatim} which will stop the environment. Ironically,
this very example is a case where you need the doubling.

@p
(define (chezweb-pretty-print code)
  (define (double code)
    (let loop ([code (string->list code)] [res '()])
      (cond
        [(null? code) (list->string (reverse res))]
        [(char=? #\| (car code))
         (loop (cdr code) (cons* #\| #\| res))]
        [else (loop (cdr code) (cons (car code) res))])))
  (with-output-to-string
    (lambda ()
      (printf "\\verbatim~n")
      (printf "~a" (double code))
      (printf "|endverbatim "))))

@ We also want to handle the printing of some of the section text. In
this case, all of the vertical bars that are found in such text need
to be handled. Two bars directly next to one another means to insert a
literal bar, but if they are separated, then it means that you want to
typeset or pretty print the code in there. Since we don't have the
full pretty printer enabled yet, we'll just use the verbatim package
again.

@p
(define (texify-section-text text)
  (define start (reverse (string->list "\\verbatim ")))
  (define end (reverse (string->list "|endverbatim{}")))
  (let loop ([text (string->list text)] [res '()] [bar? #f])
    (cond
      [(null? text) (list->string (reverse res))]
      [(char=? #\| (car text))
       (cond
         [bar? (loop (cdr text) (append start res) #f)]
         [(and (pair? (cdr text)) (char=? #\| (cadr text)))
          (loop (cddr text) (cons #\| res) #f)]
         [else
           (loop (cdr text) (append end res) #t)])]
      [else
        (loop (cdr text) (cons (car text) res) bar?)])))

@ When we have a file chunk, we format the text of the name
of the chunk differently than with a named chunk. We wrap it in
italics and so forth using the double backslash macro.

@p
(define (texify-filename txt)
  (format "\\\\{~a}" txt))

@* Handling the indexing.

@* TeX Macros.

@* Index.

