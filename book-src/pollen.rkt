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
(define sc (default-tag-function 'span)) ; TODO: small caps
(define textcd (default-tag-function 'tt))
(define textit (default-tag-function 'i))

(define cont (default-tag-function 'sub))

(define envtable (default-tag-function 'table))

(define-tag-function (math-ii attrs elems)
  `(@ "\\textit{" ,@elems "}") )

(define-tag-function (math-ic attrs elems)
  `(@ "\\texttt{" ,@elems "}") )

(define-tag-function (Vset attrs elems)
  `(@ "\\boldsymbol{" ,@elems "}") )

(define-tag-function (gate attrs elems)
  `(@ "\\mathbf{" ,@elems "}") )

(define-tag-function (comb attrs elems)
  `(@ "\\mathsf{" ,@elems "}") )

(define-tag-function (fn attrs elems)
  `(@ "\\textit{" ,@elems "}") )

(define switch "[]")

(define itemize (default-tag-function 'ul))
(define enumerate (default-tag-function 'ol))
(define item (default-tag-function 'li))

(define-tag-function ($ attrs elems)
  `(code ,attrs "$" ,@elems "$") )

(define-tag-function ($$ attrs elems)
  `(pre ,attrs "$$\n" ,@elems "\n$$") )

(define code:lisp (default-tag-function 'pre))
(define code:ml   (default-tag-function 'pre))
;; TODO: ой божечки, тебе ж ещё конвертор Scheme -> денотации писать
(define code:denotation (default-tag-function 'pre))

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

;; TODO: неразрывный пробел перед тире? или *здесь* это допустимо?
;; TODO: может вообще засунешь собственную важность куда-то подальше и не будешь вставлять эти примперевы?
(define-tag-function (trnote attrs elems)
  `(@ (label "*")
      (span ((class "sidenote")) ,@elems " — " ,(emph "Прим.~перев.")) ) )

(define-tag-function (seePage attrs elems)
  `(span ((class "sidenote")) "[" ,@elems "]") )

(define-tag-function (seeEx attrs elems)
  `(span ((class "sidenote")) "[" ,@elems "]") )

(define-tag-function (cite attrs elems)
  `(span "[" ,@elems "]") )

;; TODO: сделай так, чтобы эта штука делала предыдущее слово гиперссылкой
;; (или группу с неразрывным пробелами, или всё выражение в скобках)
;; а ещя если "текст~◊seeCite{XXX} текст", то в HTML не должно быть лишнего неразрывного пробела
(define-tag-function (seeCite attrs elems)
  `(span ((class "sidenote")) "[" ,@elems "]") )

(define-tag-function (Lisp attrs elems)
  `(span ,attrs "Lisp" (sub ,@elems)) )

;; TODO: стиль
(define UNIX       "UN*X")
(define Meroon     "Meroon")
(define Meroonet   "Meroonet")
(define LISP-1.0   "Lisp~1.0")
(define LISP-1.5   "Lisp~1.5")
(define Lisp-1     (Lisp "1"))
(define Lisp-2     (Lisp "2"))
(define Lisp-3     (Lisp "3"))
;; TODO: здесь должен быть *неразрывный* пробел, но ◊code:lisp[#:dialect CommonLisp] ломается, сделай так, чтобы не ломался
(define CommonLisp "Common Lisp")
(define CommonLoops "Common LOOPS")
(define Dylan      "Dylan")
(define EuLisp     "EuLisp")
(define ISLisp     "ISLisp")
(define LeLisp     "Le_Lisp")
(define ZetaLisp   "ZetaLisp")
(define TeX        "TeX")
(define TELOS      "ΤΕΛΟΣ")
;; TODO: где-то ты используешь ◊(RnRS)?
(define R5RS       '(@ "R" (sup "5") "RS"))
(define PL/I       "PL/I") ; TODO: красивая капитель

(define GCD "НОД")

;; TODO: Unicode
(define (is) "->")
(define (eq) "==")
;; TODO: не превращай это в юникод, но и не вставляй пробелы,
;; чтобы когда копировалось, что получалось "...", а не " . . ."
(define (dots) `(nobr "." thinsp "." thinsp "."))

(define (thinsp) 'thinsp)

;; TODO: реши как всё же это показывать
(define bigskip '(hr))

;; TODO: реализовать как положено (если надо?) пока оставь просто маркером абзацев
(define noindent '(@))

;; TODO: определить правильно
(define (box #:var [var ($ "v")] . elems)
  `(@ (span ((class "box")) ,@elems)
      (sup ,var) ) )

; только для математического режима
(define a "\\alpha")
(define e "\\varepsilon")
(define r "\\rho")
(define s "\\sigma")
(define n "\\nu")
(define p "\\pi")
(define k "\\kappa")
(define f "\\varphi")

; TODO: нотация для инъекции и проекции доменов, как только ты вспомнишь, что это означает, и перечитаешь пейперы
(define-tag-function (Inj attrs elems)
  (apply Vset (cons '() elems)) )
(define-tag-function (Prj attrs elems)
  `(@ "|" ,(apply Vset (cons '() elems))) )

; TODO: настоящие TeX-опредления сюда
(define Eval "\\mathcal{E}")
(define Lain "\\mathcal{L}")
