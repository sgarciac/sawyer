;;; take int account:
;;; how to support all numerical identifier?
;;; leading zeros in integer is not allowed
;;;
;;; errors in fbernier:
;;;
;;; pepe = "wha\qever" should choke!
;;; 1234 = "asdasd" should be valid!
;;; pepe = 45_
;;; integers should not accept leading spaces
;;; Todo in sergio:
;;; sanitize integer? should accept leading spaces
(in-package #:sawyer)


;;; The main lexer
(define-lexer toml-lexer (s)
  ;; Spaces and comments
  ("%s+" :next-token)
  ("%n+" :next-token)
  ("#.-%n" :next-token)
  ;; bare keyword
  ("[%d%a_-]+" (values :bare-keyword $$))
  ;; basic strings
  ("\"" (push-lexer s #'string-lexer :string))
  ("'([^'%n]*)'" (values :literal-string $1))
  ;; equal
  ("=" (push-lexer s #'toml-value-lexer :equal))
  ;; arrays of tables
  ("%[%[" :double-bracket-open)
  ("%]%]" :double-bracket-close)
  ;; table headers:
  ("%[" :bracket-open)
  ("%]" :bracket-close)
  ("%." :dot)
  )

(define-lexer toml-value-lexer (s)
  ;; space
  ("%s+" :next-token)
  ;; Dates
  ("(%d%d%d%d)%-(%d%d)%-(%d%d)(T(%d%d):(%d%d):(%d%d)(%.%d+)?)?(Z|((%+|%-)(%d%d):(%d%d)))?"
   (pop-lexer s :date $$))
  ;; Numbers
  ("([+-])?((%d_)|(_%d)|%d)+(%.((%d_)|(_%d)|%d)+)?([Ee]([+-])?((%d_)|(_%d)|%d)+)?" (pop-lexer s :number (let ((*read-default-float-format* 'double-float))(parse-number $$))))
  ;; Strings
  ("'''%n?"
   (progn
     (pop-lexer s :wathever)
     (push-lexer s #'multi-line-literal-string-lexer :multi-line-literal-string)))
  ("'([^'%n]*)'" (pop-lexer s :literal-string $1))
  ("\"\"\"%n?"
   (progn (pop-lexer s :whatever)
          (push-lexer s #'multi-line-string-lexer :multi-line-string)))
  ("\"" (progn (pop-lexer s :whatever) (push-lexer s #'string-lexer :string)))
  ;; Booleans
  ("true" (pop-lexer s :true t))
  ("false" (pop-lexer s :false nil))
  ("%["
   (progn (pop-lexer s :whatever)
          (push-lexer s #'array-lexer :bracket-open)))
  ("%{"
   (progn (pop-lexer s :whatever)
          (push-lexer s #'inline-table-lexer :curly-brace-open)))
  )

(define-lexer array-lexer (s)
  ;; finish the array
  ("%]" (pop-lexer s :bracket-close))
  ;; Spaces and comments
  ("%s+|%n+" :next-token)
  ("#.-%n" :next-token)
  ;; comma separates values inside an array
  ("," :comma)
  ;; Dates
  ("(%d%d%d%d)%-(%d%d)%-(%d%d)(T(%d%d):(%d%d):(%d%d)(%.%d+)?)?(Z|((%+|%-)(%d%d):(%d%d)))?"
   (values :date $$))
  ;; Numbers
  ("([+-])?((%d_)|(_%d)|%d)+(%.((%d_)|(_%d)|%d)+)?([Ee]([+-])?((%d_)|(_%d)|%d)+)?"
   (values :number (let ((*read-default-float-format* 'double-float))(parse-number $$))))
  ;; Strings
  ("'''%n?"
   (push-lexer s #'multi-line-literal-string-lexer :multi-line-literal-string))
  ("'([^'%n]*)'" (values :literal-string $1))
  ("\"\"\"%n?"
   (push-lexer s #'multi-line-string-lexer :multi-line-string))
  ("\"" (push-lexer s #'string-lexer :string))
  ;; Booleans
  ("true" :true)
  ("false" :false)
  ("%[" (push-lexer s #'array-lexer :bracket-open))
  ("%{" (push-lexer s #'inline-table-lexer :curly-brace-open))
  ;; end of the value
  ("$" (error "Unterminated array"))
  )

(define-lexer inline-table-lexer (s)
  ;; finish the inline table
  ("%}" (pop-lexer s :curly-brace-close))
  ;; Spaces
  ("%s+" :next-token)
  ;; comma separates key/value pairs inside an inline table
  ("," :comma)
  ;; bare keyword
  ("[%d%a_-]+" (values :bare-keyword $$))
  ;; basic strings
  ("\"" (push-lexer s #'string-lexer :string))
  ;; equal
  ("=" (push-lexer s #'toml-value-lexer :equal))
  ;; end of the value
  ("$" (error "Unterminated inline table"))
  )

;;(tokenize #'toml-lexer (slurp "input"))