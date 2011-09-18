@q[of]:Limbo
\def\ChezWEB{Chez{\tt WEB}}
\def\CWEB{{\tt CWEB}}
\def\WEB{{\tt WEB}}

\font\rm = "STIXGeneral" at 12pt

\hoffset = 0.5in
\voffset = 0.5in
\hsize = 5.5in
\vsize = 8in

@q[cf]

@q[of]:Introduction
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
@q[cf]
@q[of]:The ChezWEB Runtime
@* The ChezWEB Runtime.

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
(@@< (name capture ...) body+ ...)
(@@< (name capture ...) => (export ...) body+ ...)
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

@ Let's consider the value form first, since it is slightly easier. In this
case, we want to define the macro |name| to be an identifier macro that
will expand into the following form:

\medskip\verbatim
(let ()
  (alias ic oc) ...
  body+ ...)
|endverbatim
\medskip

\noindent Notice the use of |ic ...| and |oc ...|. These are the inner/outer
bindings that correspond exactly to one another except that they capture
different lexical bindings. That is, we create the |oc| bindings by rewrapping
the |ic| bindings with the wraps (marks and substitutions) of the location
where the |name| is referenced.
We use |alias| to link the two identifiers to the same underlying
location. 

@(runtime.ss@>=
(meta define (build-value-form name captures body)
	(with-syntax 
			(	[(ic ...) captures]
				[(oc ...) (datum->syntax name (syntax->list captures))]
				[(body+ ...) body])
		#'(let ()
			(alias ic oc) ...
			body+ ...)))

@ This form is used as a part of the |value-form| procedure, which is what
does the initial definition of the macro for
|name|. This macro is just an identifier syntax that has clauses for
the single identifier use and the macro call, but nothing for the
|set!| clause, since that doesn't make sense. Because we don't care about
this case, we can avoid the use of |make-variable-transformer| and
instead use a regular |syntax-case| form.

There is an interesting problem that arises if we try to just expand
the body directly. Because we are using syntax-case to do the matching,
the body that is expanded as a part of the first level (|value-form|)
of expansion, will lead to a possible ellipses problem.
Take the following body as an example:

\medskip\verbatim
(define-syntax a
  (syntax-rules ()
    [(_ e ...) (list 'e ...)]))
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

\noindent We might end up in some trouble. When run |value-form| on it,
we will get something like this:

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
error about an extra ellipses. What we need to do, when we run |value-form|,
is to make sure that the expanded code escapes the ellipses, so we would
expand the two body forms |(define...)| and |(a a b c)| with ellipses
around them instead.

@(runtime.ss@>=
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

@ When we work with the definition form, we want to use a similar linking
technique as above. However, in this case, we need to link both exports
and captures. Furthermore, we need to expand into a |module| form instead
of using a |let| form as we do above.

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

@ And just as we did above, we implement the |module-form| procedure
in the same way, taking care to escape the body elements.

@(runtime.ss@>=
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

@ And that concludes the definition of the runtime. We do want to mark
the indirect exports for the |@@<| macro.

@(runtime.ss@>=
(indirect-export @@<
	module-form value-form build-definition-form build-value-form link)
)
@q[cf]
@q[of]:The Runtime Library
@* The Runtime Library. For users who wish to use this runtime in their 
own code, we will provide a simple library for them to load the runtime 
code themselves. This will enable them to use the macro as their own 
abstraction and have the chunk like reordering without actually requiring 
them to write their entire program in \ChezWEB{}. 

@(runtime.sls@>=
(library (arcfide chezweb runtime)
	(export @< =>)
	(import (chezscheme))
	(include "runtime.ss"))
@q[cf]
@q[of]:Tokenizing a WEB
@* Tokenizing \WEB\ files. If one writes a \ChezWEB\ file in the
\WEB\ syntax, we need to parse it into tokens representing either
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
				[	(eof-object? c)
					(reverse
						(if	(null? cur)
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
@q[cf]
@q[of]:Tangling a WEB
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
and top-level chunks into two tables. These are tables that
map either chunk names or file names to the chunk contents, 
which are strings. Additionally, named chunks have captures and export 
information that must be preserved, so we also have a table for that.
The captures table is keyed on the same values as a named chunk table, 
and indeed, there should be a one-to-one mapping from named chunk 
keys to capture keys, but the value of a captures table is a pair 
of captures and exports lists, where the exports list may be false for 
value chunks.

$$\vbox{
	\offinterlineskip
	\halign{
		\strut # & # & # \cr
		{\bf Table} & {\bf Key Type} & {\bf Value Type}
		\noalign{\hrule}
		Top-level & Filename or |*default*| & Code String \cr
		Named Chunk & Chunk Name Symbol & Code String \cr
		Captures & Chunk Name Symbol & Pair of captures and exports lists \cr
	}
}$$

\noindent Since we store these as association lists, we should be careful
how we work with them. To this end, we will create a set of basic abstractions
on tables first, and use only these abstractions when working with the
internal tables. These tables should not be visible or used by the outside
world directly.

@p
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

@ Extending the default top level is the easiest. We just append the
string that we find to the |*default*| key in the |top-level| table.

@c (loop tokens top-level)
@<Extend default top-level@>=
(define body (cadr tokens))
(unless (string? body)
	(error #f "Expected a string body" body))
(hashtable-update top-level '*default*
	(lambda (cur) (string-append cur body))
	"")
(loop (cddr tokens) '() #f)

@ Handling file name top-level updates works much like a named chunk, except  
that we do not have to deal with the issues of capture variables, which we 
will discuss shortly. We must verify that we have a valid syntax in the stream 
and then we can add the name in. We should remember to strip off the leading 
and trailing whitespace from the name in question.

@c (loop tokens top-level)
@<Extend file top-level@>=
|Verify and extract delimited chunk|
(let ([name (strip-whitespace name)])
	(hashtable-update top-level name
		(lambda (cur) (string-append cur body))
		""))
(loop (cddddr tokens) '() #f)

@ Named chunk updates are complicated by the need to track captures. In 
the \WEB\ syntax, if you have a capture that you want to associate with
a given named chunk, you list the |@@c| form right before you define your 
chunk. When we parse this, we save the captures as soon as we encounter 
them so that they can be used in the next chunk. We reset the captures 
if we do not find a named chunk as our next section. 

The format of a captures form looks something like this:

\medskip\verbatim
@@c (c ...) [=> (e ...)]
|endverbatim
\medskip

\noindent In the above, the exports are optional, and the captures could 
be empty. This will come in to us as a string, so we will read it out 
using the |read| procedure, and save it to our two loop variables for the 
captures and exports. If the export is not defined, we will use false there, 
to distinguish it from an empty export form, which is a nil. 

@c (loop tokens)
@<Update the current captures@>=
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


@ When it comes to actually extending a named chunk, we will either have 
nothing in the captures and exports forms, or we will have two lists 
in |current-captures| and |current-exports| of symbols that represent 
the identifiers that we want to capture and export, respectively. 
We need to update two hashtables, one that maps the actual names 
of the chunks to their contents, and the other that tracks the captures 
and exports for each named chunk. Why do both? If someone uses the 
same chunk name to define two chunks, then those chunks are linked 
together. Likewise, we do not want to force the user to put all of the 
captures for a chunk into the first instance that the chunk name was used 
as a definition. Rather, we should allow the programmer to extend the 
captures and exports in the same way that the programmer can extend 
the chunks. So, for example:

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

@ We have to be careful about how we deal with the exports list. 
Suppose that the user first defines a captures line without the exports, 
and then later extends a chunk with a captures line that has an export 
in it. The first chunk will have been written assuming that it will return 
a value, and the second will have been written assuming that it will not.
This causes a conflict, and we should not allow this sort of thing to happen. 
In the above, we partially deal with this by assuming that if the chunk 
has not been extended it is fine to extend it; this is equivalent to passing 
the nil object as our default in the call to |hashtable-update|. On the other 
hand, we have to make sure that we give the right error if we do encounter 
a false value if we don't expect one. That is, if we receive a pair in |cur| 
whose |cdr| field is false, this means that the chunk was previously defined 
and that this definition had no exports in it. We should then error out 
if we have been given anything other than a false exports.

@c (current-exports cur name)
@<Extend exports list@>=
(define exports (cdr cur))
(when (and (not exports) current-exports)
	(error #f "attempt to extend a value named chunk as a definition chunk"
		name current-exports))
(when (and exports (not current-exports))
	(error #f "attempt to extend a definition chunk as a value chunk"
		name))
(and exports current-exports (union exports current-exports))

@ It is probably very likely that someone will make a mistake in 
specifying their chunk names at some point. It is human nature, and worse, 
typos occur more often than we would like. We want to verify that 
the closing |@@>=| actually exists, and that the expected name and body 
strings are actually there. At the same time, we will do the work of 
extracting out the name and body strings so that they can be later 
referred to as |name| and |body| rather than as |car|s and |cdr|s into 
a tokens list, since that nesting gets a bit deep.

@c (tokens) => (name body)
@<Verify and extract delimited chunk@>=
(define name (cadr tokens))
(define closer (caddr tokens))
(define body (cadddr tokens))
(unless (eq? '@>= closer)
	(error #f "Expected closing @>=" closer))
(unless (string? name)
	(error #f "Expected name string" name))
(unless (string? body)
	(error #f "Expected string body" body))

@ We also want to define out own procedure to strip the whitespace from our 
strings. We could have used something from the SRFIs, such as the strip 
function from SRFI 13, but we will write our own, simplified version here to 
keep things easy and also to avoid unnecessary dependencies on the code, which 
should, to the best extent possible, be self-contained. Our basic technique is 
to take the string, and walk down the ends from the right and the left to 
determine where the first non-whitespace character occurs in each direction.

@p
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

@ Now we have to create the actual output files. Each file that we 
write will have the same basic shape and layout:

$$\includegraphics[height=2in]{chezweb-1.eps}$$

\noindent The above diagram illustrates the relative positions of the 
three important pieces of a tangled file. In the first piece, we just 
put the contents of the runtime directly into the top of the file. 
Next, we put all of the chunks defined in the \WEB\ into the spot 
right below the runtime. Afterwards follows all of the top level 
code.

We do have a design decision to make at this point. We could walk
through the code trying to find any reference to a chunk and then 
only include those chunks that have been referenced. The motivation 
for this would be to avoid including code that we don't need to include 
in the tangled file. If we worked hard enough at it, then we could 
also allow the user to rebind or redefine names that we bound to 
chunk names in the \WEB\ but that were not referenced in the 
specific file we are tangling. Unfortunately, a naive approach to 
searching for chunk references will not catch these instances, 
and arguably, we want the user to be aware when the user attempts 
to rebind an identifier that has already been bound to the name 
of a chunk. Indeed, because chunk names should be descriptive, 
there would be very little reason for an user to every conflict 
with the named chunks (automatically generated code being an 
exception here). A conflict could be thought of as a programming 
error; rather than letting the code silently fail, we will include 
all of the named chunks into the output of every tangled file that 
is written from a given \WEB{}. When this tangled file is included 
into either a library or a module, where redefinitions are not 
allowed, then the programmer will receive an error whenever 
two bindings to the same name, or a named chunk conflict occur. 

Using the above technique actually does nothing to our performance, 
because the chunks are implemented as macros (identifier macros), 
meaning that they exist only at macro expansion time. If a named 
chunk is never referenced inside of a tangled file, then, it will never 
appear in the runtime code, such as what is the result of 
compiling a Scheme file. This means that we are encuring zero 
runtime overhead for putting all of the chunks in our code, and 
the code that we have to write will be much simpler, and likely 
much less buggy. This is, overall, a good thing.

As a final note, we should remember to use the right mode for 
our files. Since any tangled file is relying on Chez Scheme features, 
we will need to ensure that Chez Scheme mode rather than R6RS 
compatibility mode is enabled by putting the |#!chezscheme| 
directive in there at the top of the file.

We can thus sketch out a general process for writing out the 
correct contents of a file that we are writing to.

@c (output-file top-level-chunks named-chunks captures)
@<Write tangled file contents@>=
(call-with-output-file output-file
	(lambda (output-port)
		(put-string output-port "#!chezscheme\n\n")
		(put-string output-port runtime-code)
		|Write named chunks to file|
		(put-string output-port
			(hashtable-ref top-level-chunks output-file "")))
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
				"(@< (~s ~{~s ~}) ~@[=> (~{~s ~})~]~n~s~n)~n"
				name (car cell) (cdr cell)
				(hashtable-ref named-chunks name ""))))
	(hashtable-keys named-chunks))

@ Now all of the pieces are in place to write the |tangle-file| procedure
that we talked about previously. 

@p
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
@q[cf]
@q[of]:Weaving a WEB
@* Weaving a WEB.
@q[cf]
@q[of]:TeX Macros
@* TeX Macros.
@q[cf]
@q[of]:Index
@* Index.
@q[cf]
