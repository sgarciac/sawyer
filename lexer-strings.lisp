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
  ("\\u(%x%x%x%x)" (let ((n (parse-integer $1 :radix 16)))
                     (values :chars (code-char n))))
  ("\\U(%x%x%x%x%x%x%x%x)" (let ((n (parse-integer $1 :radix 16)))
                             (values :chars (code-char n))))
  ("[^\\\"%n]+" (values :chars $$))
  ("$" (error "Unterminated string")))

;;; Multi line string within """s
(define-lexer multi-line-string-lexer (s)
  ("\"\"\"" (pop-lexer s :multi-line-string))
  ("\\%n[%s%n]*" :next-token)
  ("\"" (values :chars #\"))
  ("\\t" (values :chars #\tab))
  ("\\\\" (values :chars #\\))
  ("\\\"" (values :chars #\"))
  ("\\f" (values :chars #\formfeed))
  ("\\b" (values :chars #\backspace))
  ("\\n" (values :chars #\newline))
  ("\\r" (values :chars #\return))
  ("\\u(%x%x%x%x)" (let ((n (parse-integer $1 :radix 16)))
                     (values :chars (code-char n))))
  ("\\U(%x%x%x%x%x%x%x%x)" (let ((n (parse-integer $1 :radix 16)))
                             (values :chars (code-char n))))
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
