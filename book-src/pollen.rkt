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

(define subsection     (default-tag-function 'h3))
(define subsection*    (default-tag-function 'h3))

(define subsubsection  (default-tag-function 'h4))
(define subsubsection* (default-tag-function 'h4))

(define term (default-tag-function 'i))
(define emph (default-tag-function 'em))
(define ic (default-tag-function 'code))
(define ii (default-tag-function 'i))
(define textcd (default-tag-function 'tt))
(define textit (default-tag-function 'i))

(define envtable (default-tag-function 'table))

(define-tag-function (math-ii attrs elems)
  `(@ "\\textit{" ,@elems "}") )

(define-tag-function (math-ic attrs elems)
  `(@ "\\texttt{" ,@elems "}") )

(define-tag-function (Vset attrs elems)
  `(@ "\\boldsymbol{" ,@elems "}") )

(define itemize (default-tag-function 'ul))
(define enumerate (default-tag-function 'ol))
(define item (default-tag-function 'li))

(define-tag-function ($ attrs elems)
  `(code ,attrs "$" ,@elems "$") )

(define-tag-function ($$ attrs elems)
  `(pre ,attrs "$$\n" ,@elems "\n$$") )

(define code:lisp (default-tag-function 'pre))

(define-tag-function (indexC attrs elems) "")
(define-tag-function (indexR attrs elems) "")
(define-tag-function (indexE attrs elems) "")
(define-tag-function (phantomlabel attrs elems) "")

(define-tag-function (footnote attrs elems)
  ;; TODO: улучшить сноски
  ;; <span> очень плохо работает с выделением, а ещё, как оказывается, в <p>
  ;; НЕЛЬЗЯ вкладывать блочные элементы, потому что так сказал W3C и теперь
  ;; все парсеры HTML в браузерох автоматически закрывают теги <p>, когда видят
  ;; открывающий тег любого блочного элемента.
  ;; В общем, лучше сделать какую-то магию, чтобы переносить текст сносок
  ;; куда-то сразу после текущего абзаца, но чтобы сноска показывалась где-то
  ;; рядом с ним, а не после него.
  ;; Пока что -- как временное решение -- сойдёт и вот так:
  `(@ (label "*")
      (span ((class "sidenote")) ,@elems) ) )

(define-tag-function (trnote attrs elems)
  `(@ (label "*")
      (span ((class "sidenote")) ,@elems " — " ,(emph "Прим.~перев.")) ) )

(define-tag-function (seePage attrs elems)
  `(span ((class "sidenote")) "[" ,@elems "]") )

(define-tag-function (seeEx attrs elems)
  `(span ((class "sidenote")) "[" ,@elems "]") )

(define-tag-function (Lisp attrs elems)
  `(span ,attrs "Lisp" (sub ,@elems)) )

;; TODO: стиль
(define UNIX       "UN*X")
(define Meroon     "Meroon")
(define LISP-1.0   "Lisp~1.0")
(define LISP-1.5   "Lisp~1.5")
(define Lisp-1     (Lisp "1"))
(define Lisp-2     (Lisp "2"))
(define Lisp-3     (Lisp "3"))
(define CommonLisp "Common Lisp")
(define Dylan      "Dylan")
(define EuLisp     "EuLisp")
(define ISLisp     "ISLisp")
(define LeLisp     "Le_Lisp")
(define ZetaLisp   "ZetaLisp")
(define TeX        "TeX")

;; TODO: Unicode
(define (is) "->")
(define (eq) "==")
(define (dots) `(nobr "." thinsp "." thinsp "."))

(define (thinsp) 'thinsp)

;; TODO: реализовать как положено (если надо?) пока оставь просто маркером абзацев
(define noindent '(@))
