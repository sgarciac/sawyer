(in-package #:sawyer)

;;; TOML entries parser
;;; Parses a toml document into a set of the following kind of entries
;;; key-value : a key value pair, i.e. name = "sergio"
;;; table header : a regular table header, i.e. [person.info]
;;; array table header : an array table header, i.e [[social_media_account]]

(defstruct toml-key-value-entry key value)
(defstruct toml-table-header-entry headers)
(defstruct toml-array-table-header-entry headers)
(defstruct toml-array-entry entries)
(defstruct toml-inline-table-entry entries)

;;; primitive types
(defstruct toml-integer value)
(defstruct toml-float value)
(defstruct toml-string value type) ;  :bare :literal :regular :literal-ml, :regular-ml
(defstruct toml-boolean value) ;  :bare :literal :regular :literal-ml, :regular-ml
(defstruct toml-datetime value original-value)


(defun str-concat (list)
  "Concatenates a list of strings or characters"
  (format nil "~{~A~}" list))

(define-parser toml-document-parser
  "Parse a TOML document (as produced by the lexer)"
  (.let* ((body (.many
                 (.or
                  'toml-table-header-entry-parser
                  'toml-array-table-header-entry-parser
                  'toml-key-value-entry-parser
                  ))))
    (.do
     (.eof)
     (.ret body))))

(define-parser toml-table-header-entry-parser
  (.between
   (.is :bracket-open)
   (.is :bracket-close)
   (.let* ((headers (.sep-by 'toml-key-parser (.is :dot))))
     (.ret (make-toml-table-header-entry :headers headers)))))

(define-parser toml-array-table-header-entry-parser
  (.between
   (.is :double-bracket-open)
   (.is :double-bracket-close)
   (.let* ((headers (.sep-by 'toml-key-parser (.is :dot))))
     (.ret (make-toml-array-table-header-entry :headers headers)))))

(define-parser toml-key-parser
  (.or
   'toml-literal-string-parser
   'toml-string-parser
   'toml-bare-keyword-parser))

(define-parser toml-literal-string-parser
  (.let* ((string (.is :literal-string)))
    (.ret (make-toml-string :value string :type :literal))))

(define-parser toml-bare-keyword-parser
  (.let* ((string (.is :bare-keyword)))
    (.ret (make-toml-string :value string :type :bare))))

(define-parser toml-string-parser
  (.between
   (.is :string)
   (.is :string)
   (.let* ((parts (.many
                   (.is :chars))))
     (.ret (make-toml-string :value (str-concat parts) :type :regular))
     )))

(define-parser toml-ml-literal-string-parser
  (.between
   (.is :multi-line-literal-string)
   (.is :multi-line-literal-string)
   (.let* ((parts (.many
                   (.is :chars))))
     (.ret (make-toml-string :value (str-concat parts) :type :literal-ml))
     )))

(define-parser toml-ml-string-parser
  (.between
   (.is :multi-line-string)
   (.is :multi-line-string)
   (.let* ((parts (.many
                   (.is :chars))))
     (.ret (make-toml-string :value (str-concat parts) :type :regular-ml))
     )))

(define-parser toml-array-parser
  (.between
   (.is :bracket-open)
   (.is :bracket-close)
   (.let* ((entries (.sep-by 'toml-value-parser (.is :comma))))
     (.do
      (.skip-many (.is :comma))
      (.ret (make-toml-array-entry :entries entries))) )))

(define-parser toml-inline-table-parser
  (.between
   (.is :curly-brace-open)
   (.is :curly-brace-close)
   (.let* ((entries (.sep-by 'toml-key-value-entry-parser (.is :comma))))
     (.ret (make-toml-inline-table-entry :entries entries)))))

(define-parser toml-value-parser
  (.or
   (.let* ((value (.is :number)))
     (.ret
      (if (integerp value)
          (make-toml-integer :value value)
          (make-toml-float :value value))))
   (.let* ((value (.is :date)))
     (.ret (make-toml-datetime :value (local-time:parse-timestring value) :original-value value)))
   (.let* ((value (.is :true)))
     (.ret (make-toml-boolean :value t)))
   (.let* ((value (.is :false)))
     (.ret (make-toml-boolean :value nil)))
   'toml-string-parser
   'toml-literal-string-parser
   'toml-ml-string-parser
   'toml-ml-literal-string-parser
   'toml-array-parser
   'toml-inline-table-parser
   ))

(define-parser toml-key-value-entry-parser
  (.let* ((k 'toml-key-parser)
          (v (.do (.is :equal) 'toml-value-parser)))
    (.ret (make-toml-key-value-entry :key k :value v))))

(defun parse-toml-file-to-entries (file)
  (parse-toml-string-to-entries (read-file-into-string file)))

(defun parse-toml-string-to-entries (string)
  (with-lexer (lexer 'toml-lexer string)
    (with-token-reader (next-token lexer)
      (parse 'toml-document-parser next-token))))
