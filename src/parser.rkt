#lang typed/racket
(require typed/rackunit)
(require "lexer.rkt")
; (require "unsafe.rkt")

(struct None () #:transparent)

(define-type (Option T) (U (Some T) None))
(struct (T) Some ([val : T]) #:transparent)

(: map-option (All (T V) ((Option T) (T -> V) -> (Option V))))
(define (map-option option proc)
    (match option
        [(Some val) (Some (proc val))]
        [(None) (None)]))

(: flat-map-option (All (T V) ((Option T) (T -> (Option V)) -> (Option V))))
(define (flat-map-option option proc)
    (match option
        [(Some val) (proc val)]
        [(None) (None)]))


(struct (T) CombinatorPair ([result : T] [rst : (Listof Token)]) #:transparent)
(define-type (CombinatorResult T) (Option (CombinatorPair T)))
(define-type (Combinator T) ((Listof Token) -> (CombinatorResult T)))

(: map-combinator-pair (All (T V) ((Option (CombinatorPair T)) (T (Listof Token) -> V) -> (Option V))))
(define (map-combinator-pair option proc)
    (match option
        [(Some (CombinatorPair val rst)) (Some (proc val rst))]
        [(None) (None)]))

(: flat-map-combinator-pair (All (T V) ((Option (CombinatorPair T)) (T (Listof Token) -> (Option V)) -> (Option V))))
(define (flat-map-combinator-pair option proc)
    (match option
        [(Some (CombinatorPair val rst)) (proc val rst)]
        [(None) (None)]))

(define-type BinOp (U '+ '- '* '/))
(define-type Expr (U Ident Int BinOpC AppC LamC))
(struct BinOpC ([lhs : Expr] [op : BinOp] [rhs : Expr]) #:transparent)
(struct AppC ([fun-pos : Expr] [args : (Listof Expr)]) #:transparent)
(struct LamC ([params : (Listof String)] [body : Block]) #:transparent)
(define-predicate expression-ast? Expr)

(define-type Statement (U LetC ReturnC ExprSt))
(struct LetC ([ident : String] [expr : Expr]) #:transparent)
(struct ReturnC ([expr : Expr]) #:transparent)
(struct ExprSt ([expr : Expr]) #:transparent)

(define-type Block (Listof Statement))
(struct Program ([block : Block]) #:transparent)

(define (tag [token : Token]) : (Combinator Token)
    (lambda (tokens)
        (match tokens
            [(cons f r) #:when(equal? f token) (Some (CombinatorPair f r))]
            [_ (None)])))

(: alt (All (T) ((Listof (Combinator T)) -> (Combinator T))))
(define (alt parsers)
    (define (inner [tokens : (Listof Token)] [parsers : (Listof (Combinator T))]) : (CombinatorResult T)
        (match parsers
            ['() (None)]
            [(cons f-parser rest-parsers) (match (f-parser tokens)
                [(None) (inner tokens rest-parsers)]
                [result result])]))
    (lambda (tokens) (inner tokens parsers)))

(define (take-until [token : Token]) : (Combinator (Listof Token))
    (define (inner [tokens : (Listof Token)]) : (CombinatorResult (Listof Token))
        (match tokens
            ['() (None)]
            [(cons f rst) #:when(equal? f token) (Some (CombinatorPair '() rst))]
            [(cons f rst) (map-combinator-pair (inner rst) 
                (lambda ([result : (Listof Token)] [rst : (Listof Token)]) 
                    (CombinatorPair (cons f result) rst)))]))
    inner)

(define (take-until1 [token : Token]) : (Combinator (Listof Token))
    (lambda (tokens)
        (match ((take-until token) tokens)
            [(Some (CombinatorPair found-tokens rst)) #:when(> (length found-tokens) 0) 
                (Some (CombinatorPair found-tokens rst))]
            [_ (None)])))

(: many (All (T) ((Combinator T) -> (Combinator (Listof T)))))
(define (many combinator)
    (define (inner [tokens : (Listof Token)]) : (CombinatorResult (Listof T))
        (match (combinator tokens)
            [(Some (CombinatorPair f-result rst))
                (match (inner rst)
                    [(Some (CombinatorPair result rst)) (Some (CombinatorPair (cons f-result result) rst))])]
            [_ (Some (CombinatorPair '() tokens))]))
    inner)

(: terminal (All (T) ((Combinator T) -> ((Listof Token) -> (Option T)))))
(define (terminal combinator)
    (lambda (tokens)
        (match (combinator tokens)
            [(Some (CombinatorPair result '())) (Some result)]
            [_ (None)])))

(define (ident [tokens : (Listof Token)]) : (CombinatorResult Expr)
    (match tokens
        [(cons (Ident id) r) (Some (CombinatorPair (Ident id) r))]
        [_ (None)]))

(define (int [tokens : (Listof Token)]) : (CombinatorResult Expr)
    (match tokens
        [(cons (Int num) r) (Some (CombinatorPair (Int num) r))]
        [_ (None)]))

(define (bin-op [op-sym : BinOp]) : (Combinator Expr)
    (lambda (tokens)
        (match ((take-until1 op-sym) tokens)
            [(Some (CombinatorPair lhs-tokens rhs-tokens)) #:when(> (length rhs-tokens) 0)
                (match* ((terminal-expression lhs-tokens) (expression rhs-tokens))
                [((Some lhs) (Some (CombinatorPair rhs rst-tokens))) (Some (CombinatorPair (BinOpC lhs op-sym rhs) rst-tokens))]
                [(_ _) (None)])]
            [_ (None)])))

(define (fn-call [tokens : (Listof Token)]) : (CombinatorResult Expr)
    (flat-map-combinator-pair ((take-until1 'lParen) tokens) 
        (lambda ([fn-pos-tokens : (Listof Token)] [rst : (Listof Token)])
            (flat-map-option ((terminal expression) fn-pos-tokens) (lambda ([fun-pos : Expr])
                (match-define (CombinatorPair args after-args) (parse-args rst))
                (map-combinator-pair ((tag 'rParen) after-args) (lambda ([_ : Token] [rst : (Listof Token)])
                    (CombinatorPair (AppC fun-pos args) rst))))))))

; Parses comma seperated expressions
(define (parse-args [tokens : (Listof Token)]) : (CombinatorPair (Listof Expr))
    (match (expression tokens)
        [(Some (CombinatorPair result (cons '\, rst)) )
            (match-define (CombinatorPair rec-result rec-rst) (parse-args rst))
            (CombinatorPair (cons result rec-result) rec-rst)]
        [(Some (CombinatorPair result rst)) (CombinatorPair (list result) rst)]
        [(None) (CombinatorPair '() tokens)]))

(define (expression [tokens : (Listof Token)]) : (CombinatorResult Expr)
    ((alt 
        (list
            (bin-op '+) 
            (bin-op '-) 
            (bin-op '*)
            (bin-op '/)
            fn-call
            int
            ident)) 
        tokens))

(define terminal-expression (terminal expression))

(define (statement [tokens : (Listof Token)]) : (CombinatorResult Statement)
    (match tokens
        [(list 'let (Ident id) '= expr-tokens ... '\; rst ...)
            (match (terminal-expression (cast expr-tokens (Listof Token)))
                [(Some expr) (Some (CombinatorPair (LetC id expr) rst))]
                [_ (None)])]
        [(list 'return expr-tokens ... '\; rst ...)
            (match (terminal-expression (cast expr-tokens (Listof Token)))
                [(Some expr) (Some (CombinatorPair (ReturnC expr) rst))]
                [_ (None)])]
        [(list expr-tokens ... '\; rst ...)
            (match (terminal-expression (cast expr-tokens (Listof Token)))
                [(Some expr) (Some (CombinatorPair (ExprSt expr) rst))]
                [_ (None)])]
        ; Last ditch no semi expr statement
        [expr-tokens
            (match (expression (cast expr-tokens (Listof Token)))
                [(Some (CombinatorPair expr rst)) (Some (CombinatorPair (ExprSt expr) rst))]
                [_ (None)])]))

(define (block [tokens : (Listof Token)]) : (CombinatorResult (Listof Statement))
    ((many statement) tokens))

(define (parse [tokens : (Listof Token)]) : (Option Program)
    (map-option ((terminal block) tokens) Program))



(test-case "tag-tests"
    (check-equal? ((tag '+) (list '+)) (Some (CombinatorPair '+ '())))
    (check-equal? ((tag '+) (list '-)) (None))
    (check-equal? ((tag '+) (list '+ '- (Int 5))) (Some (CombinatorPair '+ (list '- (Int 5))))))

(test-case "alt-tests"
    (check-equal? ((alt (list (tag '+) (tag '-))) (list '/ (Int 5))) (None))
    (check-equal? ((alt (list (tag '+) (tag '-))) (list '+ (Int 5))) (Some (CombinatorPair '+ (list (Int 5)))))
    (check-equal? ((alt (list (tag '+) (tag '-))) (list '- (Int 5))) (Some (CombinatorPair '- (list (Int 5)))))
    (check-equal? ((alt (list (tag '+) (tag '-))) (list '+)) (Some (CombinatorPair '+ '()))))

(test-case "take-until-tests"
    (check-equal? ((take-until '+) (list '- '/ '+ '-)) (Some (CombinatorPair (list '- '/) (list '-))))
    (check-equal? ((take-until '*) (list '- '/ '+ '-)) (None))
    (check-equal? ((take-until '-) (list '- '/ '+ '-)) (Some (CombinatorPair '() (list '/ '+ '-))))
    (check-equal? ((take-until '*) (list '- '/ '+ '*)) (Some (CombinatorPair (list '- '/ '+) '()))))

(test-case "take-until1-tests"
    (check-equal? ((take-until1 '+) (list '- '/ '+ '-)) (Some (CombinatorPair (list '- '/) (list '-))))
    (check-equal? ((take-until1 '*) (list '- '/ '+ '-)) (None))
    (check-equal? ((take-until1 '-) (list '- '/ '+ '-)) (None))
    (check-equal? ((take-until1 '*) (list '- '/ '+ '*)) (Some (CombinatorPair (list '- '/ '+) '()))))


(test-case "many-tests"
    (check-equal? ((many (tag '+)) (list '+ '+ '-)) (Some (CombinatorPair (list '+ '+) (list '-))))
    (check-equal? ((many (tag '*)) (list '+ '+ '-)) (Some (CombinatorPair '() (list '+ '+ '-))))
    (check-equal? ((many (tag '+)) (list '+ '+ '+)) (Some (CombinatorPair(list '+ '+ '+) '()))))

(test-case "terminal-tests"
    (check-equal? ((terminal (tag '+)) (list '+)) (Some '+))
    (check-equal? ((terminal (tag '+)) (list '+ '-)) (None))
    (check-equal? ((terminal (many (tag '+))) (list '+ '-)) (None))
    (check-equal? ((terminal (many (tag '+))) (list '+ '+)) (Some (list '+ '+)))
    (check-equal? ((terminal (tag '+)) '()) (None)))

(parse (list (Int 5) '+ (Ident "foo")))
(parse (list (Int 5) '- (Ident "foo") '+ (Int 200)))
(parse (list (Int 5) '- (Ident "foo") '* (Int 200)))
(parse (list (Int 5) '* (Ident "foo") '- (Int 200)))
(parse (list (Int 200) (Int 5) '+ (Ident "foo")))
(parse (list (Ident "add") 'lParen (Int 5) '\, (Int 10) 'rParen))
(parse (list (Int 5) '+ (Ident "add") 'lParen (Int 5) '\, (Int 10) 'rParen))

(parse (lexer "let foo = 5 + 6; add(2 * 7, 2)"))
