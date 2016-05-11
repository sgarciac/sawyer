;;;; sawyer.asd

(asdf:defsystem #:sawyer
  :description "A TOML parser"
  :author "Sergio Garcia <sergio.garcia@gmail.com>"
  :license "Artistic"
  :depends-on (#:lexer #:parse-number #:alexandria #:local-time)
  :serial t
  :components ((:file "package")
               (:file "lexer-strings")
               (:file "lexer")
               (:file "parser")
               (:file "tables")))

