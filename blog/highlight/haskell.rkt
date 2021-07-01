#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     threading)
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         racket/list
         racket/match
         racket/stxparam
         syntax/parse/define
         threading)

(define-lex-abbrevs
  ; character classes
  [newline (:or (:: #\return #\newline) newline-char)]
  [newline-char (:or #\return #\newline #\page)]
  [id-char (:or lower-case upper-case title-case numeric #\_ #\')]
  [symbol-char (:& (:or symbolic punctuation) (:~ special-char #\_ #\" #\'))]
  [special-char (:or #\( #\) #\[ #\] #\{ #\} #\, #\; #\`)]
  [graphic-char (:or lower-case upper-case title-case numeric symbolic punctuation)]

  ; keywords
  [reserved-id (:or "anyclass" "case" "\\case" "class" "data" "default" "deriving" "do" "else" "forall"
                    "foreign" "if" "import" "in" "infix" "infixl" "infixr" "instance" "let" "module" "newtype"
                    "of" "proc" "stock" "then" "type" "where")]
  [reserved-op (:or ".." "::" #\: #\= #\\ #\| "<-" "->" #\@ #\~ "=>")]

  ; comments
  [line-comment (:: (:>= 2 #\-)                       ; at least two dashes
                    (:? (:~ symbol-char newline-char) ; followed by a non-symbol (so it isn’t an infix operator)
                        (:* (:~ newline-char)))       ; followed by arbitrary characters
                    newline)]                         ; until the next newline
  #;[block-comment (:: "{-"                       ; an open block comment sequence
                       (:* (:or (:~ #\{ #\-)      ; \ followed by some stretch of characters that
                                (:: #\{ (:~ #\-)) ; | aren’t an open or close block comment sequence
                                (:: #\- (:~ #\})) ; /
                                block-comment))   ; nested block comments are allowed
                       "-}")]                     ; terminated with a close block comment sequence

  ; variable and constructor names (identifiers and operators)
  [var-id (:: (:or lower-case #\_) (:* id-char))]
  [con-id (:: (:or upper-case title-case) (:* id-char))]
  [var-op (:: (:- symbol-char #\:) (:* symbol-char))]
  [con-op (:: #\: (:* symbol-char))]

  ; qualified names
  [mod-id (:: (:* con-id #\.) con-id)]
  [qual-var-id (:: (:? mod-id #\.) var-id)]
  [qual-con-id (:: (:? mod-id #\.) con-id)]
  [qual-var-op (:: (:? mod-id #\.) var-op)]
  [qual-con-op (:: (:? mod-id #\.) con-op)]

  ; numeric literals
  [decimal (:+ (:/ #\0 #\9))]
  [octal (:+ (:/ #\0 #\7))]
  [hexadecimal (:+ (:/ #\0 #\9 #\a #\f #\A #\F))]
  [integer (:or decimal
                (:: (:or "0o" "0O") octal)
                (:: (:or "0x" "0X") hexadecimal))]
  [float (:or (:: decimal "." decimal (:? exponent))
              (:: decimal exponent))]
  [exponent (:: (:or #\e #\E) (:? (:or #\+ #\-)) decimal)]

  ; character and string literals
  [escape (:: #\\ (:or #\a #\b #\f #\n #\r #\t #\v #\\ #\" #\'
                       decimal (:: #\o octal) (:: #\x hexadecimal)))]
  [char (:: #\' (:or (:- graphic #\\ #\') #\space escape) #\')]
  [string (:: #\"
              (:* (:or (:- graphic #\\ #\")
                       #\space
                       escape
                       (:: #\\ #\&)                   ; the “empty character” for delimiting escape sequences
                       (:: #\\ (:+ whitespace) #\\))) ; a “gap” in a string, which is ignored
              #\")])

(begin-for-syntax
  
  (define-syntax-class lexer-states-clause
    #:attributes [states {rule.trigger 1} {rule.action 2}]
    #:description "clause"
    #:commit
    #:literals [when]
    (pattern (when ~! [state-id:id ...]
               rule:lexer-states-rule ...+)
      #:attr states (map syntax-e (attribute state-id)))
    (pattern rule1:lexer-states-rule
      #:attr states #f
      #:attr {rule.trigger 1} (list #'rule1.trigger)
      #:attr {rule.action 2} (list (attribute rule1.action))))

  (define-syntax-class lexer-states-rule
    #:attributes [trigger {action 1}]
    #:description "rule"
    #:commit
    (pattern [trigger action:expr ...+])))

(define-simple-macro (lexer-action-procedure action:expr ...+)
  (λ (start-v end-v lexeme-v return-v)
    (syntax-parameterize ([start-pos (make-rename-transformer #'start-v)]
                          [end-pos (make-rename-transformer #'end-v)]
                          [lexeme (make-rename-transformer #'lexeme-v)]
                          [return-without-pos (make-rename-transformer #'return-v)])
      action ...)))

(define-syntax-parser lexer-states
  [(_ clause:lexer-states-clause ...+)

   (define all-states (~> (attribute clause.states)
                          (filter values _)
                          append*
                          (remove-duplicates eq?)))

   (define-values [state-rules rule-actions]
     (for/fold ([state-rules (hasheq)]
                [rule-actions '()])
               ([states (in-list (attribute clause.states))]
                [triggers (in-list (attribute clause.rule.trigger))]
                [actionss (in-list (attribute clause.rule.action))]
                #:when #t
                [trigger (in-list triggers)]
                [actions (in-list actionss)])
       (define action-id (generate-temporary actions))
       (values (for/fold ([state-rules state-rules])
                         ([state (in-list (or states all-states))])
                 (define rule #`[#,trigger (#,action-id start-pos end-pos lexeme return-without-pos)])
                 (hash-update state-rules state (λ~>> (cons rule)) '()))
               (cons #`[#,action-id (lexer-action-procedure #,@actions)]
                     rule-actions))))

   #`(let #,rule-actions
       (λ (in state)
         (match state
           #,@(for/list ([(state rules) (in-immutable-hash state-rules)])
                #`['#,state ((lexer-src-pos #:suppress-warnings #,@(reverse rules)) in)]))))])

(define current-lexer-states (make-parameter #f))
(define current-layout-contexts (make-parameter #f))

(define (current-lexer-state)
  (match (current-lexer-states)
    [(cons state _) state]
    [_              'default]))

(define (push-lexer-state! state)
  (current-lexer-states (cons state (current-lexer-states))))
(define (pop-lexer-state!)
  (current-lexer-states (rest (current-lexer-states))))

(struct implicit-layout (column) #:transparent)
(define (push-layout-context! ctx)
  (current-layout-contexts (cons ctx (current-layout-contexts))))
(define (pop-layout-context!)
  (current-layout-contexts (rest (current-layout-contexts))))

(define-tokens haskell-tokens
  [comment literal var-id con-id var-op con-op])
(define-empty-tokens haskell-empty-tokens
  [eof
   \( \) \[ \] \{ \} \, \; \` virtual-\; virtual-\{ virtual-\}
   .. :: : = \\ \| <- -> @ ~ =>
   anyclass case \\case class data default deriving do else forall foreign if import
   in infix infixl infixr instance let module newtype of proc stock then type where])

(define (lex-one in)
  (let again ()
    ((lexer-states
      [(eof) 'eof]
      [(:+ (:- whitespace newline-char)) (return-without-pos (again))]
      [line-comment (token-comment lexeme)]

      (when [start-of-line]
        [newline (return-without-pos (again))]
        [(::)    (define start-col (position-col start-pos))
                 (match (current-layout-contexts)
                   ; aligned with the current layout context; insert `;`
                   [(cons (implicit-layout layout-col) _)
                    #:when (= start-col layout-col)
                    (pop-lexer-state!)
                    'virtual-\;]
                   ; outside the current layout context; insert `}`
                   [(cons (implicit-layout layout-col) _)
                    #:when (< start-col layout-col)
                    ; don’t pop the lexer state, as we might have a `;` to insert
                    (pop-layout-context!)
                    'virtual-\}]
                   ; line continuation, don’t insert anything
                   [_
                    (pop-lexer-state!)
                    (return-without-pos (again))])])

      (when [layout]
        [#\{  (pop-lexer-state!) (push-layout-context! 'explicit-layout) '\{]
        [(::) (pop-lexer-state!)
              (define start-col (position-col start-pos))
              (match (current-layout-contexts)
                [(cons (implicit-layout layout-col) _)
                 #:when (<= start-col layout-col)
                 (push-lexer-state! 'layout/empty)]
                [_
                 (push-layout-context! (implicit-layout start-col))])
              'virtual-\{])

      (when [layout/empty]
        [(::) (pop-lexer-state!)
              (push-lexer-state! 'start-of-line)
              'virtual-\}])

      (when [default]
        [newline (push-lexer-state! 'start-of-line)
                 (return-without-pos (again))]

        [#\{ (push-layout-context! 'explicit-layout) '\{]
        [#\} (pop-layout-context!) '\}]

        [reserved-id (match lexeme
                       [(or "\\case" "do" "let" "of" "where")
                        (push-lexer-state! 'layout)]
                       [_ (void)])
                     (string->symbol lexeme)]
        [reserved-op (string->symbol lexeme)]

        [qual-var-id (token-var-id lexeme)]
        [qual-con-id (token-con-id lexeme)]
        [qual-var-op (token-var-op lexeme)]
        [qual-con-op (token-con-op lexeme)]

        [(:or integer float char string) (token-literal lexeme)]))
     in
     (current-lexer-state))))

(define (lex-all in)
  (port-count-lines! in)
  (parameterize ([current-lexer-states '()]
                 [current-layout-contexts '()])
    (let loop ([tokens '()])
      (match (lex-one in)
        [(position-token 'eof _ _)
         (reverse tokens)]
        [token
         (loop (cons token tokens))]))))

(define (parse in)
  (port-count-lines! in)
  (parameterize ([current-lexer-states '()]
                 [current-layout-contexts '()])
    ((parser
      (src-pos)
      (tokens haskell-tokens haskell-empty-tokens)
      (error (λ (ok? name value start-pos end-pos #:stack stack)
               #;(raise-arguments-error 'parse "cannot continue after error"
                                        "ok?" ok?
                                        "name" name
                                        "value" value
                                        "start" start-pos
                                        "end" end-pos
                                        "stack" stack)
               (void)))

      ;(debug "/tmp/parser-debug.out")

      (start exp)
      (end eof)
      (grammar
       ; ---------------------------------------------------
       ; declarations

       [decls  [(\{         decls1 \})    $2]
               [(virtual-\{ decls1 close) $2]]
       [decls1 [(decl semi decls1) (cons $1 $3)]
               [(decl semi)        (list $1)]
               [(decl)             (list $1)]]

       [decl [(var-id = exp) (list $1 $3)]]

       ; ---------------------------------------------------
       ; expressions
       
       [exp [(infixexp) $1]]

       [infixexp [(lexp op infixexp) (list $2 $1 $3)]
                 [(lexp)             $1]]

       [lexp [(let decls in exp) (list 'let $2 $4)]
             [(fexp)             $1]]

       [fexp [(fexp aexp) (list $1 $2)]
             [(aexp)      $1]]

       [aexp [(var-id)           $1]
             [(gcon)             $1]
             [(literal)          $1]
             [(\( exp \))        $2]]

       ; ---------------------------------------------------

       [semi  [(\;)         '\;]
              [(virtual-\;) 'virtual-\;]]
       [close [(virtual-\}) 'virtual-\}]
              [(error)      (begin (pop-layout-context!) '\})]]
       [op    [(var-op)     $1]
              [(con-op)     $1]]
       [gcon  [(\( \))      "()"]
              [(\[ \])      "[]"]
              [(con-id)     $1]]))

     (λ () (lex-one in)))))

#;(lex-all (open-input-string #<<EOF
f x = let a = 1
          b = 2  
          g y = exp2  
       in exp1
EOF
                            ))

(parse (open-input-string "let x = 3 in x"))

(define (lex in)
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      ((lexer-src-pos
        [(eof) eof]
        [newline lexeme]
        [(:+ (:- whitespace newline-char)) lexeme]
        [line-comment `#s(comment ,lexeme)]
        [reserved-id (string->symbol lexeme)]
        [reserved-op (string->symbol lexeme)]
        [qual-var-id `#s(var-id ,lexeme)]
        [qual-con-id `#s(con-id ,lexeme)]
        [qual-var-op `#s(var-op ,lexeme)]
        [qual-con-op `#s(con-op ,lexeme)]
        [(:or integer float char string) `#s(literal ,lexeme)])
       in))

    (match* {(position-token-token token) tokens}
      ; we’re done, return
      [{(? eof-object?) _} (reverse tokens)]

      ; start of a new layout context
      [{(not '\{) (app (λ~>> (findf (λ~> position-token-token string? not)))
                        (position-token (or 'let 'where 'do 'of '\\case) _ _))}
       (define token-start (position-token-start-pos token))
       (loop (list* token (position-token 'layout-start token-start token-start) tokens))]

      ; first lexeme on its line
      [{_ (cons (position-token (? string?) (position _ _ 0) _) _)}
       (define token-start (position-token-start-pos token))
       (loop (list* token (position-token 'line-indent token-start token-start) tokens))]

      ; boring cases
      [{_ _} (loop (cons token tokens))])))

(define (apply-layout tokens)
  (let loop ([tokens tokens]
             [contexts '()])
    (match* {tokens contexts}
      ; start of line
      [{(cons (position-token 'line-indent (and posn (position _ _ n)) _) tokens)
        (cons m contexts)}
       (cond
         [(= n m) (cons (position-token '\; posn posn) (loop tokens (cons m contexts)))]
         [(< n m) (cons (position-token '\} posn posn) (loop tokens contexts))]
         [else    (loop tokens (cons m contexts))])]

      ; new layout within existing context
      [{(cons (position-token 'layout-start (and posn (position _ _ n)) _) tokens)
        (cons m contexts)}
       #:when (> n m)
       (cons (position-token '\{ posn posn) (loop tokens (list* n m contexts)))]

      ; new layout without existing context
      [{(cons (position-token 'layout-start (and posn (position _ _ n)) _) tokens)
        '()}
       #:when (> n 0)
       (cons (position-token '\{ posn posn) (loop tokens (list n)))]

      ; new layout with empty body
      [{(cons (position-token 'layout-start (and posn (position _ _ n)) _) tokens)
        contexts}
       (list* (position-token '\{ posn posn)
              (position-token '\} posn posn)
              (loop (cons (position-token 'line-indent posn posn) tokens) contexts))]

      ; explicit close brace
      [{(cons (and token (position-token '\} _ _)) tokens) contexts}
       (match contexts
         [(cons 0 contexts) (cons token (loop tokens contexts))]
         [_ (error 'apply-layout "no matching open brace for close brace")])]

      ; explicit open brace
      [{(cons (and token (position-token '\{ _ _)) tokens) contexts}
       (cons token (loop tokens (cons 0 contexts)))]

      [{(cons token tokens) (cons m contexts)}
       #:when (not (zero? m))
       (cons token (loop tokens (cons 0 contexts)))])))


