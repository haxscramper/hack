#lang typed/racket

(require racket/list
         racket/match
         racket/string)

(require/typed json [write-json (-> Any Output-Port Void)])

(provide make-model
         with-model
         record!
         sum!
         tag!
         enum!
         case!
         field!
         extends!
         default!
         rule!
         expr!
         meta!
         escape!
         validate-model!
         model->jsexpr
         write-model)

;; The IR deliberately distinguishes a type name from a value used as a
;; generic parameter, such as the 16 in (Array 16 Char).
(struct QName
        ([kind : Symbol] [scope : (Option QName)]
                         [name : (U String Integer)]
                         [params : (Listof QName)])
  #:transparent)

(struct Expr ([op : Symbol] [args : (Listof Expr)] [value : Any]) #:transparent)

(struct Rule ([id : String] [message : String] [severity : String] [expression : Expr]) #:transparent)

(struct Meta ([backend : String] [attrs : Any]) #:transparent)
(struct Escape ([backend : String] [purpose : String] [severity : String] [code : String])
  #:transparent)
(struct Default ([value : Any]) #:transparent)
(struct Extends ([type : QName]) #:transparent)

(struct Field
        ([name : String] [type : QName]
                         [default : (Option Default)]
                         [rules : (Listof Rule)]
                         [meta : (Listof Meta)]
                         [escapes : (Listof Escape)])
  #:transparent)

(struct Tag
        ([name : String] [fields : (Listof Field)]
                         [rules : (Listof Rule)]
                         [meta : (Listof Meta)]
                         [escapes : (Listof Escape)])
  #:transparent)

(struct EnumCase ([name : String] [value : Integer]) #:transparent)

(struct Rec
        ([name : QName] [base : (Option QName)]
                        [fields : (Listof Field)]
                        [rules : (Listof Rule)]
                        [meta : (Listof Meta)]
                        [escapes : (Listof Escape)])
  #:transparent)

(struct Sum
        ([name : QName] [tags : (Listof Tag)]
                        [rules : (Listof Rule)]
                        [meta : (Listof Meta)]
                        [escapes : (Listof Escape)])
  #:transparent)

(struct EnumD
        ([name : QName] [cases : (Listof EnumCase)]
                        [meta : (Listof Meta)]
                        [escapes : (Listof Escape)])
  #:transparent)

(define-type Decl (U Rec Sum EnumD))

(struct Model ([declarations : (Mutable-HashTable String Decl)]) #:transparent)

(: make-model (-> Model))
(define (make-model)
  (Model (make-hash)))

(: current-model (Parameterof (Option Model)))
(define current-model (make-parameter #f))

(: current-parent (Parameterof (Option String)))
(define current-parent (make-parameter #f))

(define-syntax-rule (with-model model body ...)
  (parameterize ([current-model model]
                 [current-parent #f])
    body ...))

(: require-condition (-> Any String Void))
(define (require-condition condition message)
  (unless condition
    (error 'unidata "~a" message)))

(: q-head (-> QName String))
(define (q-head q)
  (define n (QName-name q))
  (if (string? n)
      n
      (error 'unidata "a type name cannot be an integer")))

(: q-path (-> QName String))
(define (q-path q)
  (define parent (QName-scope q))
  (if parent
      (string-append (q-path parent) "::" (q-head q))
      (q-head q)))

(: parse-name (-> Symbol QName))
(define (parse-name s)
  (define pieces (regexp-split #rx"::" (symbol->string s)))
  (require-condition (not (null? pieces)) "empty qualified name")
  (for ([piece (in-list pieces)])
    (require-condition (regexp-match? #px"^[A-Za-z_][A-Za-z_0-9]*$" piece)
                       (format "invalid qualified name: ~a" s)))
  (define result
    (for/fold ([parent
                :
                (Option QName)
                #f])
              ([piece (in-list pieces)])
      (QName 'type parent piece '())))
  (if result
      result
      (error 'unidata "empty qualified name")))

(: parse-type (-> Any QName))
(define (parse-type datum)
  (cond
    [(symbol? datum) (parse-name datum)]
    [(exact-integer? datum) (QName 'value #f datum '())]
    [(list? datum)
     (define items (cast datum (Listof Any)))
     (require-condition (pair? items) "empty type application")
     (define head (car items))
     (require-condition (symbol? head) "type constructor must be a name")
     (define base (parse-name (cast head Symbol)))
     (QName 'type (QName-scope base) (QName-name base) (map parse-type (cdr items)))]
    [else (error 'unidata "invalid type syntax: ~v" datum)]))

(: declaration-name (-> Symbol QName))
(define (declaration-name s)
  (define q (parse-name s))
  (require-condition (QName-scope q) (format "declaration ~a needs a fully qualified name" s))
  q)

(: parse-expr (-> Any Expr))
(define (parse-expr datum)
  (cond
    [(or (string? datum) (boolean? datum) (exact-integer? datum)) (Expr 'literal '() datum)]
    [(and (list? datum) (pair? datum))
     (define items (cast datum (Listof Any)))
     (define head (car items))
     (require-condition (symbol? head) "expression operator must be a name")
     (define op (cast head Symbol))
     (define tail (cdr items))
     (cond
       [(eq? op 'field)
        (require-condition (and (= (length tail) 1) (symbol? (car tail)))
                           "(field name) requires one field identifier")
        (Expr 'field '() (symbol->string (cast (car tail) Symbol)))]
       [(eq? op 'allowed-kinds)
        (require-condition (and (>= (length tail) 2) (andmap symbol? (cdr tail)))
                           "allowed-kinds requires a field expression and type names")
        (Expr 'allowed-kinds
              (cons (parse-expr (car tail))
                    (for/list :
                      (Listof Expr)
                      ([x (in-list (cdr tail))])
                      (Expr 'type-ref '() (parse-name (cast x Symbol)))))
              #f)]
       [else
        (require-condition (memq op '(eq lt le gt ge and or not implies size present))
                           (format "unknown expression operator: ~a" op))
        (Expr op (map parse-expr tail) #f)])]
    [else (error 'unidata "invalid expression: ~v" datum)]))

(define-syntax-rule (expr! expression)
  (parse-expr 'expression))

(define-syntax-rule (field! name type option ...)
  (field* name 'type (list option ...)))

(define-syntax-rule (extends! type)
  (Extends (parse-type 'type)))

(define-syntax-rule (record! name option ...)
  (record* 'name (lambda () (list option ...))))

(define-syntax-rule (sum! name option ...)
  (sum* 'name (lambda () (list option ...))))

(define-syntax-rule (enum! name option ...)
  (enum* 'name (lambda () (list option ...))))

(define-syntax-rule (tag! name option ...)
  (tag* 'name (list option ...)))

(define-syntax-rule (case! name value)
  (case* 'name value))

(: default! (-> Any Default))
(define (default! value)
  (Default value))

(: rule! (-> #:id String #:message String #:expr Expr [#:severity String] Rule))
(define (rule! #:id id #:message message #:expr expression #:severity [severity "error"])
  (require-condition (not (string=? id "")) "rule ID cannot be empty")
  (require-condition (member severity '("error" "warning")) "rule severity must be error or warning")
  (Rule id message severity expression))

(: meta! (-> String Any Meta))
(define (meta! backend attrs)
  (require-condition (not (string=? backend "")) "empty metadata backend")
  ;; attrs must be JSON-compatible. write-json checks that at export.
  (Meta backend attrs))

(: escape! (->* (String String) (#:purpose String #:severity String) Escape))
(define (escape! backend code #:purpose [purpose "validation"] #:severity [severity "error"])
  (require-condition (member severity '("error" "warning"))
                     "escape severity must be error or warning")
  (Escape backend purpose severity code))

(: select-fields (-> (Listof Any) (Listof Field)))
(define (select-fields options)
  (reverse (for/fold :
                     (Listof
                      Field)
             ([found '()])
             ([item (in-list options)])
             (if (Field? item)
                 (cons item found)
                 found))))

(: select-rules (-> (Listof Any) (Listof Rule)))
(define (select-rules options)
  (reverse (for/fold :
                     (Listof
                      Rule)
             ([found '()])
             ([item (in-list options)])
             (if (Rule? item)
                 (cons item found)
                 found))))

(: select-meta (-> (Listof Any) (Listof Meta)))
(define (select-meta options)
  (reverse (for/fold :
                     (Listof
                      Meta)
             ([found '()])
             ([item (in-list options)])
             (if (Meta? item)
                 (cons item found)
                 found))))

(: select-escapes (-> (Listof Any) (Listof Escape)))
(define (select-escapes options)
  (reverse (for/fold :
                     (Listof
                      Escape)
             ([found '()])
             ([item (in-list options)])
             (if (Escape? item)
                 (cons item found)
                 found))))

(: check-options (-> String (Listof Any) (Listof Symbol) Void))
(define (check-options location options allowed)
  (for ([item (in-list options)])
    (define kind
      (cond
        [(Field? item) 'field]
        [(Rule? item) 'rule]
        [(Meta? item) 'meta]
        [(Escape? item) 'escape]
        [(Default? item) 'default]
        [(Extends? item) 'extends]
        [(Tag? item) 'tag]
        [(EnumCase? item) 'case]
        [(or (Rec? item) (Sum? item) (EnumD? item)) 'nested]
        [else 'invalid]))
    (require-condition (member kind allowed) (format "invalid ~a option in ~a" kind location))))

(: field* (-> Symbol Any (Listof Any) Field))
(define (field* name type-datum options)
  (check-options "field" options '(rule meta escape default))
  (define defaults
    (for/fold :
              (Listof
               Default)
      ([found '()])
      ([item (in-list options)])
      (if (Default? item)
          (cons item found)
          found)))
  (require-condition (<= (length defaults) 1) (format "multiple defaults on ~a" name))
  (Field (symbol->string name)
         (parse-type type-datum)
         (if (null? defaults)
             #f
             (car defaults))
         (select-rules options)
         (select-meta options)
         (select-escapes options)))

(: case* (-> Symbol Any EnumCase))
(define (case* name value)
  (require-condition (exact-integer? value) (format "enum case ~a must have an integer value" name))
  (EnumCase (symbol->string name) (cast value Integer)))

(: tag* (-> Symbol (Listof Any) Tag))
(define (tag* name options)
  (check-options "tag"
                 options
                 '(field rule
                         meta
                         escape))
  (Tag (symbol->string name)
       (select-fields options)
       (select-rules options)
       (select-meta options)
       (select-escapes options)))

(: active-model (-> Model))
(define (active-model)
  (define m (current-model))
  (if m
      m
      (error 'unidata "declare types inside with-model")))

(: declare! (-> QName Decl Void))
(define (declare! name declaration)
  (define path (q-path name))
  (define parent (current-parent))
  (when parent
    (require-condition (string-prefix? path (string-append parent "::"))
                       (format "nested declaration ~a must start with ~a::" path parent)))
  (define declarations (Model-declarations (active-model)))
  (require-condition (not (hash-has-key? declarations path))
                     (format "duplicate declaration: ~a" path))
  (hash-set! declarations path declaration))

(: record* (-> Symbol (-> (Listof Any)) Rec))
(define (record* name body)
  (define q (declaration-name name))
  (define options
    (parameterize ([current-parent (q-path q)])
      (body)))
  (check-options "record"
                 options
                 '(field rule
                         meta
                         escape
                         extends
                         nested))
  (define bases
    (for/fold :
              (Listof
               Extends)
      ([found '()])
      ([item (in-list options)])
      (if (Extends? item)
          (cons item found)
          found)))
  (require-condition (<= (length bases) 1) (format "multiple bases on ~a" name))
  (define declaration
    (Rec q
         (if (null? bases)
             #f
             (Extends-type (car bases)))
         (select-fields options)
         (select-rules options)
         (select-meta options)
         (select-escapes options)))
  (declare! q declaration)
  declaration)

(: sum* (-> Symbol (-> (Listof Any)) Sum))
(define (sum* name body)
  (define q (declaration-name name))
  (define options
    (parameterize ([current-parent (q-path q)])
      (body)))
  (check-options "sum" options '(tag rule meta escape nested))
  (define tags
    (reverse (for/fold :
                       (Listof
                        Tag)
               ([found '()])
               ([item (in-list options)])
               (if (Tag? item)
                   (cons item found)
                   found))))
  (define declaration
    (Sum q tags (select-rules options) (select-meta options) (select-escapes options)))
  (declare! q declaration)
  declaration)

(: enum* (-> Symbol (-> (Listof Any)) EnumD))
(define (enum* name body)
  (define q (declaration-name name))
  (define options
    (parameterize ([current-parent (q-path q)])
      (body)))
  (check-options "enum"
                 options
                 '(case meta
                    escape))
  (define cases
    (reverse (for/fold :
                       (Listof
                        EnumCase)
               ([found '()])
               ([item (in-list options)])
               (if (EnumCase? item)
                   (cons item found)
                   found))))
  (define declaration (EnumD q cases (select-meta options) (select-escapes options)))
  (declare! q declaration)
  declaration)

(: lookup (-> Model QName (Option Decl)))
(define (lookup m q)
  (hash-ref (Model-declarations m) (q-path q) #f))

(: builtin? (-> String Boolean))
(define (builtin? name)
  (if (member name
              '("I8" "U8" "I16" "U16" "I32" "U32" "I64" "U64" "F32" "F64" "Bool" "Char" "String"))
      #t
      #f))

(: verify-type! (-> Model QName Void))
(define (verify-type! m q)
  (define head (q-head q))
  (define args (QName-params q))
  (cond
    [(eq? (QName-kind q) 'value) (error 'unidata "value parameter outside Array: ~v" (QName-name q))]
    [(not (QName-scope q))
     (cond
       [(builtin? head) (require-condition (null? args) (format "~a takes no type parameters" head))]
       [(member head '("Optional" "List"))
        (require-condition (= (length args) 1) (format "~a requires one type" head))
        (for ([arg (in-list args)])
          (verify-type! m arg))]
       [(string=? head "Map")
        (require-condition (= (length args) 2) "Map requires key and value types")
        (for ([arg (in-list args)])
          (verify-type! m arg))]
       [(string=? head "Tuple")
        (require-condition (>= (length args) 2) "Tuple requires at least two types")
        (for ([arg (in-list args)])
          (verify-type! m arg))]
       [(string=? head "Array")
        (require-condition (= (length args) 2) "Array requires a size and element type")
        (define size (first args))
        (define n (QName-name size))
        (require-condition (and (eq? (QName-kind size) 'value) (exact-integer? n) (positive? n))
                           "Array size must be a positive integer")
        (verify-type! m (second args))]
       [else (error 'unidata "undeclared type: ~a" head)])]
    [else
     (require-condition (null? args) (format "user type ~a cannot have parameters" (q-path q)))
     (require-condition (lookup m q) (format "undeclared type: ~a" (q-path q)))]))

(: decl-name (-> Decl QName))
(define (decl-name d)
  (cond
    [(Rec? d) (Rec-name d)]
    [(Sum? d) (Sum-name d)]
    [else (EnumD-name d)]))

(: unique-strings! (-> String (Listof String) Void))
(define (unique-strings! where names)
  (define seen
    :
    (Mutable-HashTable String Boolean)
    (make-hash))
  (for ([name (in-list names)])
    (require-condition (not (hash-has-key? seen name)) (format "duplicate name ~a in ~a" name where))
    (hash-set! seen name #t)))

(: inherited-fields (-> Model Rec (Listof String) (Listof Field)))
(define (inherited-fields m r visited)
  (define path (q-path (Rec-name r)))
  (require-condition (not (member path visited)) (format "inheritance cycle at ~a" path))
  (define base (Rec-base r))
  (if base
      (let ([found (lookup m base)])
        (require-condition (Rec? found) (format "base of ~a must be a declared record" path))
        (append (inherited-fields m (cast found Rec) (cons path visited)) (Rec-fields r)))
      (Rec-fields r)))

(: find-field (-> String (Listof Field) (Option Field)))
(define (find-field name fields)
  (cond
    [(null? fields) #f]
    [(string=? name (Field-name (car fields))) (car fields)]
    [else (find-field name (cdr fields))]))

(: subtype? (-> Model QName QName Boolean))
(define (subtype? m child parent)
  (cond
    [(string=? (q-path child) (q-path parent)) #t]
    [else
     (define d (lookup m child))
     (if (and (Rec? d) (Rec-base d))
         (subtype? m (cast (Rec-base d) QName) parent)
         #f)]))

(: type-sort (-> QName Symbol))
(define (type-sort q)
  (define name (q-head q))
  (cond
    [(member name '("I8" "U8" "I16" "U16" "I32" "U32" "I64" "U64" "F32" "F64")) 'number]
    [(string=? name "Bool") 'bool]
    [(member name '("String" "Char")) 'string]
    [(member name '("List" "Array" "Map" "Tuple")) 'collection]
    [(string=? name "Optional") 'optional]
    [else 'object]))

(: check-expression (-> Model (Listof Field) Expr Symbol))
(define (check-expression m fields expression)
  (define op (Expr-op expression))
  (define args (Expr-args expression))
  (cond
    [(eq? op 'literal)
     (define value (Expr-value expression))
     (cond
       [(boolean? value) 'bool]
       [(string? value) 'string]
       [else 'number])]
    [(eq? op 'field)
     (define name (cast (Expr-value expression) String))
     (define field (find-field name fields))
     (require-condition field (format "rule references unknown field ~a" name))
     (type-sort (Field-type (cast field Field)))]
    [(eq? op 'type-ref) (error 'unidata "type reference outside allowed-kinds")]
    [(eq? op 'allowed-kinds)
     (define first-expression (first args))
     (require-condition (eq? (Expr-op first-expression) 'field)
                        "allowed-kinds requires a field reference")
     (define name (cast (Expr-value first-expression) String))
     (define field (find-field name fields))
     (require-condition field (format "unknown field ~a" name))
     (define q (Field-type (cast field Field)))
     (require-condition (and (string=? (q-head q) "List") (= (length (QName-params q)) 1))
                        "allowed-kinds requires a List field")
     (define element (first (QName-params q)))
     (require-condition (Rec? (lookup m element)) "allowed-kinds list element must be a record")
     (for ([candidate (in-list (rest args))])
       (define target (cast (Expr-value candidate) QName))
       (require-condition (Rec? (lookup m target)) (format "unknown record kind: ~a" (q-path target)))
       (require-condition (subtype? m target element)
                          (format "~a is not derived from ~a" (q-path target) (q-path element))))
     'bool]
    [else
     (define sorts
       (for/list :
         (Listof Symbol)
         ([arg (in-list args)])
         (check-expression m fields arg)))
     (cond
       [(memq op '(lt le gt ge))
        (require-condition (and (= (length sorts) 2)
                                (andmap (lambda ([s : Symbol]) (eq? s 'number)) sorts))
                           (format "~a requires two numbers" op))
        'bool]
       [(eq? op 'eq)
        (require-condition (and (= (length sorts) 2) (eq? (first sorts) (second sorts)))
                           "eq requires two values of the same sort")
        'bool]
       [(memq op '(and or))
        (require-condition (and (>= (length sorts) 2)
                                (andmap (lambda ([s : Symbol]) (eq? s 'bool)) sorts))
                           (format "~a requires at least two booleans" op))
        'bool]
       [(eq? op 'implies)
        (require-condition (and (= (length sorts) 2)
                                (andmap (lambda ([s : Symbol]) (eq? s 'bool)) sorts))
                           "implies requires two booleans")
        'bool]
       [(eq? op 'not)
        (require-condition (equal? sorts '(bool)) "not requires one boolean")
        'bool]
       [(eq? op 'size)
        (require-condition (and (= (length sorts) 1) (memq (first sorts) '(collection string)))
                           "size requires a collection or string")
        'number]
       [(eq? op 'present)
        (require-condition (equal? sorts '(optional)) "present requires an Optional field")
        'bool]
       [else (error 'unidata "unsupported expression operator: ~a" op)])]))

(: verify-rules! (-> Model (Listof Field) (Listof Rule) (Mutable-HashTable String Boolean) Void))
(define (verify-rules! m fields rules ids)
  (for ([r (in-list rules)])
    (require-condition (not (hash-has-key? ids (Rule-id r)))
                       (format "duplicate rule ID: ~a" (Rule-id r)))
    (hash-set! ids (Rule-id r) #t)
    (require-condition (eq? 'bool (check-expression m fields (Rule-expression r)))
                       (format "rule ~a must produce a boolean" (Rule-id r)))))

(: verify-fields! (-> Model (Listof Field) (Mutable-HashTable String Boolean) Void))
(define (verify-fields! m fields ids)
  (for ([f (in-list fields)])
    (verify-type! m (Field-type f))
    (verify-rules! m fields (Field-rules f) ids)))

(: validate-model! (-> Model Void))
(define (validate-model! m)
  (define ids
    :
    (Mutable-HashTable String Boolean)
    (make-hash))
  (for ([d (in-list (hash-values (Model-declarations m)))])
    (cond
      [(Rec? d)
       (define base (Rec-base d))
       (when base
         (verify-type! m base))
       (define all-fields (inherited-fields m d '()))
       (unique-strings! (q-path (Rec-name d)) (map Field-name all-fields))
       (verify-fields! m all-fields ids)
       (verify-rules! m all-fields (Rec-rules d) ids)]
      [(Sum? d)
       (require-condition (pair? (Sum-tags d)) (format "sum ~a needs a tag" (q-path (Sum-name d))))
       (unique-strings! (q-path (Sum-name d)) (map Tag-name (Sum-tags d)))
       (verify-rules! m '() (Sum-rules d) ids)
       (for ([tag (in-list (Sum-tags d))])
         (unique-strings! (Tag-name tag) (map Field-name (Tag-fields tag)))
         (verify-fields! m (Tag-fields tag) ids)
         (verify-rules! m (Tag-fields tag) (Tag-rules tag) ids))]
      [else
       (require-condition (pair? (EnumD-cases d))
                          (format "enum ~a needs a case" (q-path (EnumD-name d))))
       (unique-strings! (q-path (EnumD-name d)) (map EnumCase-name (EnumD-cases d)))
       (unique-strings! (q-path (EnumD-name d))
                        (map (lambda ([c : EnumCase]) (number->string (EnumCase-value c)))
                             (EnumD-cases d)))])))

(: q-json (-> QName Any))
(define (q-json q)
  (hash 'kind
        (symbol->string (QName-kind q))
        'scope
        (if (QName-scope q)
            (q-json (cast (QName-scope q) QName))
            'null)
        'name
        (QName-name q)
        'params
        (map q-json (QName-params q))))

(: expression-json (-> Expr Any))
(define (expression-json e)
  (cond
    [(eq? (Expr-op e) 'literal) (hash 'op "literal" 'value (Expr-value e))]
    [(eq? (Expr-op e) 'field) (hash 'op "field" 'name (Expr-value e))]
    [(eq? (Expr-op e) 'type-ref) (hash 'op "type-ref" 'name (q-json (cast (Expr-value e) QName)))]
    [else (hash 'op (symbol->string (Expr-op e)) 'args (map expression-json (Expr-args e)))]))

(: rule-json (-> Rule Any))
(define (rule-json r)
  (hash 'id
        (Rule-id r)
        'message
        (Rule-message r)
        'severity
        (Rule-severity r)
        'expr
        (expression-json (Rule-expression r))))

(: meta-json (-> Meta Any))
(define (meta-json m)
  (hash 'backend (Meta-backend m) 'attrs (Meta-attrs m)))

(: escape-json (-> Escape Any))
(define (escape-json e)
  (hash 'backend
        (Escape-backend e)
        'purpose
        (Escape-purpose e)
        'severity
        (Escape-severity e)
        'code
        (Escape-code e)))

(: field-json (-> Field Any))
(define (field-json f)
  (hash 'name
        (Field-name f)
        'type
        (q-json (Field-type f))
        'has_default
        (if (Field-default f) #t #f)
        'default
        (if (Field-default f)
            (Default-value (cast (Field-default f) Default))
            'null)
        'rules
        (map rule-json (Field-rules f))
        'meta
        (map meta-json (Field-meta f))
        'escapes
        (map escape-json (Field-escapes f))))

(: decl-json (-> Decl Any))
(define (decl-json d)
  (cond
    [(Rec? d)
     (hash 'kind
           "record"
           'name
           (q-json (Rec-name d))
           'extends
           (if (Rec-base d)
               (q-json (cast (Rec-base d) QName))
               'null)
           'fields
           (map field-json (Rec-fields d))
           'rules
           (map rule-json (Rec-rules d))
           'meta
           (map meta-json (Rec-meta d))
           'escapes
           (map escape-json (Rec-escapes d)))]
    [(Sum? d)
     (hash 'kind
           "sum"
           'name
           (q-json (Sum-name d))
           'tags
           (for/list :
             (Listof Any)
             ([tag (in-list (Sum-tags d))])
             (hash 'name
                   (Tag-name tag)
                   'fields
                   (map field-json (Tag-fields tag))
                   'rules
                   (map rule-json (Tag-rules tag))
                   'meta
                   (map meta-json (Tag-meta tag))
                   'escapes
                   (map escape-json (Tag-escapes tag))))
           'rules
           (map rule-json (Sum-rules d))
           'meta
           (map meta-json (Sum-meta d))
           'escapes
           (map escape-json (Sum-escapes d)))]
    [else
     (hash 'kind
           "integer-enum"
           'name
           (q-json (EnumD-name d))
           'cases
           (for/list :
             (Listof Any)
             ([c (in-list (EnumD-cases d))])
             (hash 'name (EnumCase-name c) 'value (EnumCase-value c)))
           'meta
           (map meta-json (EnumD-meta d))
           'escapes
           (map escape-json (EnumD-escapes d)))]))

(: model->jsexpr (-> Model Any))
(define (model->jsexpr m)
  (validate-model! m)
  (define declarations
    (sort (hash-values (Model-declarations m))
          (lambda ([left : Decl] [right : Decl])
            (string<? (q-path (decl-name left)) (q-path (decl-name right))))))
  (hash 'version "0.0.1" 'types (map decl-json declarations)))

(: write-model (-> Model Path-String Void))
(define (write-model m path)
  (define document (model->jsexpr m))
  (call-with-output-file path
                         (lambda ([out : Output-Port])
                           (write-json document out)
                           (newline out))
                         #:exists 'replace))
