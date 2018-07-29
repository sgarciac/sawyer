(in-package #:sawyer)

;;; Standard string within by "s
(define-lexer string-lexer (s)
  ("\"" (pop-lexer s :string))
  ("\\t" (values :chars #\tab))
  ("\\\\" (values :chars #\\))
  ("\\\"" (values :chars #\"))
  ("\\f" (values :chars #\formfeed))
  ("\\b" (values :chars #\backspace))
  ("\\n" (values :chars #\newline))
  ("\\r" (values :chars #\return))
  ("\\u(%x%x%x%x)" (values :chars (parse-to-unicode $1)))
  ("\\U(%x%x%x%x%x%x%x%x)" (values :chars (parse-to-unicode $1)))
  ("[^\\\"%n]+" (values :chars $$))
  ("$" (error "Unterminated string")))

;;; Multi line string within """s
(define-lexer multi-line-string-lexer (s)
  ("\"\"\"" (pop-lexer s :multi-line-string))
  ("\\%s*%n[%s%n]*" :next-token)
  ("\"" (values :chars #\"))
  ("\\t" (values :chars #\tab))
  ("\\\\" (values :chars #\\))
  ("\\\"" (values :chars #\"))
  ("\\f" (values :chars #\formfeed))
  ("\\b" (values :chars #\backspace))
  ("\\n" (values :chars #\newline))
  ("\\r" (values :chars #\return))
  ("\\u(%x%x%x%x)" (parse-to-unicode $1))
  ("\\U(%x%x%x%x%x%x%x%x)" (parse-to-unicode $1))
  ("[^\\\"]+" (values :chars $$))
  ("$" (error "unterminated string")))

;;; Multi line string literal within '''s
(define-lexer multi-line-literal-string-lexer (s)
  ("'''" (pop-lexer s :multi-line-literal-string))
  ;; escaped characters
  ("'" (values :chars #\'))
  ;; all other characters
  ("[^']+" (values :chars $$))
  ;; don't reach the end of file or line
  ("$" (error "unterminated string")))

;; parse to unicode
(defun parse-to-unicode (str)
  (let ((scalar (parse-integer str :radix 16)))
    (if  (or
          (and (>= scalar 0)
               (<= scalar #xD7FF))
          (and (>= scalar #xE000)
               (<= scalar #x10FFFF)))
         (code-char scalar)
         (error "not a valid unicode scalar value")
         )))
