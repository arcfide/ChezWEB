#!/usr/bin/env scheme-script
;;; -*- Mode: scheme -*-

;;;; Bootstrap Tangler for ChezWEB
;;;; Allow ChezWEB to be bootstrapped

;;; Copyright (c) 2011 Aaron W. Hsu <arcfide@sacrideo.us>
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

;; This program is basically a hand composed version of TANGLE that
;; allows the ChezWEB system to be boostrapped from the ground up. It
;; works only on the CWEB syntax, and does not support the Scribble
;; syntax. It is designed to let the user build a running version of
;; TANGLE from the WEB syntax files tangle.w.

(import (chezscheme))

;; Basic structure
;;
;; There is a simple basic structure for parsing a WEB
;; syntax. Firstly, we tokenize the file into the following tokens:
;;
;; String @ @< @>= @p @* @e @r @( @^ @. @: @q @i @c
;;
;; In the above a String is just a normal Scheme string. The process
;; of tokenization then gives us a list of these tokens in the order
;; that they were read from the file.

(define (chezweb-tokenize port)
  (let loop ([tokens '()]
             [cur '()])
    (let ([c (read-char port)])
      (cond
        [(eof-object? c)
         (reverse
           (if (null? cur)
               tokens
               (cons (list->string (reverse cur)) tokens)))]
        [(char=? #\@ c)
         (let ([nc (read-char port)])
           (case nc
             [(#\space #\< #\p #\* #\e #\r #\( #\^ #\. #\: #\q #\i #\c)
              (if (null? cur)
                  (loop (cons (string->symbol (string c nc)) tokens) '())
                  (loop
                    (cons*
                      (string->symbol (string c nc))
                      (list->string (reverse cur))
                      tokens)
                    '()))]
             [(#\>)
              (let ([nnc (read-char port)])
                (if (char=? #\= nnc)
                    (if (null? cur)
                        (loop (cons '@>= tokens) '())
                        (loop
                          (cons* '@>= (list->string (reverse cur)) tokens)
                          '()))
                    (loop tokens (cons* nnc nc c cur))))]
             [(#\@)
              (loop tokens (cons c cur))]
             [else
               (if (eof-object? nc)
                   (loop tokens cur)
                   (loop tokens (cons* nc c cur)))]))]
        [else (loop tokens (cons c cur))]))))

