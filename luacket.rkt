#lang racket

(require br)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tokenizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct token (token-type value))

; todo refactoring
(define token-type-add 1)
(define token-type-sub 2)
(define token-type-mul 3)
(define token-type-div 4)
(define token-type-eql 5)
(define token-type-bra 6)
(define token-type-ket 7)
(define token-type-semi-colon 8)
(define token-type-num 9)
(define token-type-ident 10)


(define tokens empty)
(define (add-token! token)
  ;(set! tokens (cons token tokens)))
  (set! tokens (append tokens (list token))))


(define (char-symbol? c)
  (string-contains? "+-*/;=()" (string c)))

(define (scan-symbol content)
  ;(displayln "scan-symbol")
  (add-token! (first content))
  (rest content))

(define (char-digit? c)
  (and (char<=? #\0 c) (char<=? c #\9)))

(define (scan-number content)
  ;(displayln "scan-number")
  ;(let ([token empty] [c (first content)])
  (define token empty)
  (define c (first content))
  (while (char-digit? c)
		 (set! token (cons c token))
		 (set! content (rest content))
		 (set! c (first content)))
  (cond [(not (empty? token))
		 (add-token! (token token-type-num (list->string (reverse token))))])
  content)

(define (char-identifier? c)
  (or (char-alphabetic? c) (char=? c #\_)))

(define (scan-identifier content)
  ;(displayln "scan-number")
  ;(let ([token empty] [c (first content)])
  (define token empty)
  (define c (first content))
  (while (or (char-identifier? c) (char-digit? c))
		 (set! token (cons c token))
		 (set! content (rest content))
		 (set! c (first content)))
  (cond [(not (empty? token))
		 (add-token! (token token-type-ident (list->string (reverse token))))])
  content)



(define (scan content)
  (define token empty)
  (define c empty)
  (while (not (empty? content))
		 (set! c (first content))
		 (cond
		   [(char-symbol? c) (set! content (scan-symbol content))]
		   [(char-digit? c) (set! content (scan-number content))]
		   [(char-identifier? c) (set! content (scan-identifier content))]
		   [#t (set! content (rest content))])))

;(define (scan content)
;  (if (empty? content)
;	'()
;	(let ([token empty])
;	  (displayln (first content))
;	  (scan (rest content)))))

;  (cond
;	[(empty? content) '()]
;	[(not (empty? content))
;	 (let ([c (first content)] [rst (rest content)])
;	   (cond
;		 [(char=? c #\Linefeed)
;		  ;(add-tokens 
;		  '()
;		  ]))]))
	 ;(displayln `(,(first content)))
	 ;(scan (rest content))]))


(define (gen-ast)
  '())





(define parse-arguments
  (command-line
	#:args (filename)
	filename))

;(display parse-arguments)
(define filename parse-arguments)

;(parse-command-line "compile" (current-command-line-arguments))
;(display (for/list ([line (file->lines "mylang-test.rkt")])
(define content
  (let ([cnt (string->list (file->string filename))])
	;(append cnt (list #\Space)))) ; sentinel
	(append cnt (list #\Linefeed)))) ; sentinel



(displayln content)
(displayln (first content))
;(scan empty content)
(scan content)
(displayln content)
(displayln tokens)
;(display (eval lines))
;(for ([line (format-datums '~a lines)])
;  ;(eval line))
;  (displayln line))


