#lang typed/racket

(require "dsl.rkt")

(define model (make-model))

(with-model model
            ;; A root AST node. Inheritance models the C++ hierarchy; the DSL does
            ;; not prescribe whether a backend uses pointers, handles, or values.
            (record! demo::Org
                     (field! 'subnodes (List demo::Org) (default! '()))
                     (field! 'source (Optional String))
                     (meta! "cpp" (hash 'abstract #t)))
            (record! demo::File
                     (extends! demo::Org)
                     (field! 'path String)
                     (rule! #:id "file_children"
                            #:message "A file contains documents"
                            #:expr (expr! (allowed-kinds (field subnodes) demo::Document))))
            (record! demo::Document
                     (extends! demo::Org)
                     (field! 'title (Optional demo::Paragraph))
                     (field! 'options demo::DocumentOptions)
                     (field! 'tags (List String) (default! '()))
                     (rule! #:id "document_children"
                            #:message "Documents contain paragraphs and times"
                            #:expr
                            (expr! (allowed-kinds (field subnodes) demo::Paragraph demo::Time)))
                     (meta! "cpp" (hash 'reflection (hash 'enabled #t 'kind "Document"))))
            (record! demo::Paragraph
                     (extends! demo::Org)
                     (field! 'text
                             String
                             (rule! #:id "paragraph_text"
                                    #:message "Paragraph text must not be empty"
                                    #:expr (expr! (gt (size (field text)) 0))))
                     (rule! #:id "paragraph_children"
                            #:message "Paragraphs cannot have subnodes"
                            #:expr (expr! (eq (size (field subnodes)) 0))))
            (record! demo::Time
                     (extends! demo::Org)
                     (field! 'unix_seconds I64)
                     (rule! #:id "time_children"
                            #:message "Time nodes cannot have subnodes"
                            #:expr (expr! (eq (size (field subnodes)) 0))))
            (record! demo::DocumentOptions
                     (field! 'export_file (Optional String))
                     (field! 'language String (default! "en")))
            ;; Flat integer-valued enum, distinct from a tagged sum.
            (enum! demo::ActionKind (case! Modify 0) (case! Delete 1) (case! Rename 2) (case! Add 3))
            ;; A record containing a nested enum, record, and sum. Each nested
            ;; declaration still supplies its entire qualified name.
            (record! demo::Commit
                     (enum! demo::Commit::Status (case! Draft 0) (case! Published 1))
                     (record! demo::Commit::Action
                              (field! 'kind demo::ActionKind)
                              (field! 'track U64)
                              (field! 'old_path (Optional String))
                              (field! 'new_path (Optional String))
                              (field! 'added I32 (default! 0))
                              (field! 'removed I32 (default! 0))
                              (rule! #:id "nonnegative_added"
                                     #:message "Added-line count cannot be negative"
                                     #:expr (expr! (ge (field added) 0)))
                              (rule! #:id "nonnegative_removed"
                                     #:message "Removed-line count cannot be negative"
                                     #:expr (expr! (ge (field removed) 0)))
                              ;; This relationship depends on enum-value interpretation and is
                              ;; preserved for the SQL/protobuf/C++ translation layer.
                              (escape! "sql"
                                       "kind <> 0 OR (added IS NOT NULL AND removed IS NOT NULL)"
                                       #:purpose "validation"))
                     (sum! demo::Commit::Change
                           (tag! Addition (field! 'path String) (field! 'lines U32))
                           (tag! Deletion (field! 'path String))
                           (tag! Rename (field! 'old_path String) (field! 'new_path String)))
                     (field! 'hash String)
                     (field! 'author String)
                     (field! 'time I64)
                     (field! 'status demo::Commit::Status)
                     (field! 'actions (List demo::Commit::Action) (default! '()))
                     (field! 'change (Optional demo::Commit::Change))
                     (meta! "sql" (hash 'table "GitCommit" 'primary_key "hash")))
            ;; Nested parameterized types, value parameters, fixed-size arrays,
            ;; product types, maps, and the remaining numeric primitive types.
            (record! demo::TypeSampler
                     (field! 'signed8 I8)
                     (field! 'signed16 I16)
                     (field! 'signed32 I32)
                     (field! 'unsigned8 U8)
                     (field! 'unsigned16 U16)
                     (field! 'unsigned32 U32)
                     (field! 'unsigned64 U64)
                     (field! 'float32 F32)
                     (field! 'float64 F64)
                     (field! 'enabled Bool)
                     (field! 'initial Char)
                     (field! 'grid (Map (Tuple I32 I32) (Array 16 Char)))
                     (field! 'maybe_grid (Optional (Map (Tuple I32 I32) (Array 16 Char))))))

(write-model model "model.json")
(displayln "Wrote model.json")
