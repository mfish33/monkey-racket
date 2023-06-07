#lang typed/racket
(require typed/rackunit)

(provide Token Ident Int lexer lexer-tests)

(define-type Token (U 
    Ident 
    Int 
    'ileagal 
    'eof 
    '\{ 
    '\} 
    'lParen
    'rParen 
    '\, 
    '\; 
    '+ 
    '- 
    '! 
    '!= 
    '> 
    '< 
    '* 
    '/
    '==
    '= 
    'fn
    'let 
    'if 
    'false 
    'true 
    'return 
    'else))

(struct Ident ([id : String]) #:transparent)
(struct Int ([num : Integer]) #:transparent)

(define single-char-map (hash
    #\{ '\{
    #\} '\}
    #\( 'lParen
    #\) 'rParen
    #\, '\,
    #\; '\;
    #\+ '+
    #\- '-
    #\! '!
    #\> '>
    #\< '<
    #\* '*
    #\/ '/
    #\= '=))

(define keyword-map(hash
   "fn" 'fn
   "let" 'let
   "if" 'if
   "else" 'else
   "true" 'true
   "false" 'false
    "return" 'return))

(define (char-lexer [chars : (Listof Char)]) : (Listof Token)
    (match chars
        ['() '()]
        [(list (? char-whitespace? _) rst ...) (char-lexer rst)]
        [(list #\! #\= rst ...) (cons '!= (char-lexer rst))]
        [(list #\= #\= rst ...) (cons '== (char-lexer rst))]
        [(list char rst ...) #:when(hash-has-key? single-char-map char) 
            (cons (hash-ref single-char-map char) (char-lexer rst))]
        [(list (? char-alphabetic-or-underscore? ident) ... rst ...) #:when(> (length ident) 0)
            (define ident-str (list->string (cast ident (Listof Char))))
            (define token (hash-ref keyword-map ident-str (lambda () (Ident ident-str))))
            (cons token (char-lexer rst))]
        [(list (? char-numeric? num-chars) ... rst ...) #:when(> (length num-chars) 0)
            (define string-num (list->string (cast num-chars (Listof Char))))
            (cons (Int (cast (string->number string-num) Integer)) (char-lexer rst))]
        [_ '(ileagal)]))


(define (lexer [input : String]) : (Listof Token)
    (char-lexer (string->list input)))

(define (char-alphabetic-or-underscore? [c : Char]) : Boolean
    (or (char-alphabetic? c) (eq? c #\_)))



(define longer-test #<<DELIM
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
DELIM
)

(define lexer-tests
    (test-suite
        "Lexer tests"
        (check-equal? (lexer "=+(){},;") (list '= '+ 'lParen 'rParen '\{ '\} '\, '\; 'eof))

        (check-equal? (lexer longer-test) (list 
        'let (Ident "five") '= (Int 5) '\;
        'let (Ident "ten") '= (Int 10) '\;
        'let (Ident "add") '= 'fn 'lParen (Ident "x") '\, (Ident "y") 'rParen '\{
            (Ident "x") '+ (Ident "y") '\;
        '\} '\;
        'let (Ident "result") '= (Ident "add") 'lParen (Ident "five") '\, (Ident "ten") 'rParen '\;
        'eof
        ))))