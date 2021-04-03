#lang racket/base

(require pollen/decode)
(require pollen/tag)
(require racket/string)
(provide (all-defined-out))

(module setup racket/base
  (provide (all-defined-out))

  ;; What formats to render for poly-targets by default.
  (define poly-targets '(html))
)

(define-tag-function (root attrs elements)
  ;; Follow semantic line breaking: empty lines delineate paragraphs,
  ;; single line breaks are for editability and should not be translated:
  ;; HTML will treat them as non-line-breaking whitespace.
  (define (decode-semantic-paragraphs elements)
    (define (leave-linebreaks-alone elements) elements)
    (decode-paragraphs elements #:linebreak-proc leave-linebreaks-alone) )
  ;; Follow LaTeX convention for non-breaking spaces.
  ;; Insert HTML entities instead. If we insert U+0020 NO-BREAK SPACE
  ;; then it gets eaten somewhere and converted into a regular space.
  (define (replace-nbsp str)
    (define (intersperse sep xs)
      (cond [(null? xs) '()]
            [(null? (cdr xs)) xs]
            [else (reverse (cdr (foldl (lambda (x result)
                                         (cons sep (cons x result)) )
                                       '() xs )))] ) )
    (intersperse 'nbsp (string-split str "~" #:trim? #f)) )
  (decode `(root ,attrs ,@elements)
          #:txexpr-elements-proc decode-semantic-paragraphs
          #:string-proc replace-nbsp
          ) )

(define chapter  (default-tag-function 'h1))
(define chapter* (default-tag-function 'h1))

(define section  (default-tag-function 'h2))
(define section* (default-tag-function 'h2))

(define term (default-tag-function 'i))
(define emph (default-tag-function 'em))
(define ic (default-tag-function 'code))
(define ii (default-tag-function 'i))
(define textcd (default-tag-function 'tt))
(define textit (default-tag-function 'i))

(define itemize (default-tag-function 'ul))
(define item (default-tag-function 'li))

(define-tag-function ($ attrs elems)
  `(code ,attrs "$" ,@elems "$") )

(define code:lisp (default-tag-function 'pre))

(define-tag-function (indexC attrs elems) "")
(define-tag-function (indexR attrs elems) "")
(define-tag-function (indexE attrs elems) "")

;; TODO: стиль
(define (UNIX) "UN*X")
(define (Meroon) "Meroon")
(define (Lisp1) '(span "Lisp" (sub "1")))
(define (Lisp2) '(span "Lisp" (sub "2")))
(define (CommonLisp) "Common Lisp")
(define (Dylan) "Dylan")
(define (EuLisp) "EuLisp")
(define (ISLisp) "ISLisp")
(define (LeLisp) "Le_Lisp")

;; TODO: Unicode
(define (is) "->")
(define (eq) "==")
