#lang pollen

◊section[#:label "escape/sect:init"]{Инициализуем интерпретатор}

Перед погружением в~сокровенные тайны управляющих форм,
давайте сначала подготовим наш интерпретатор к~запуску.
◊; TODO: дублирование ссылок
(Сравните ◊seePage{basics/sect:global-environment}
с~разделом~◊ref{basics/sect:global-environment} первой~главы.)
Для~начала неплохо было~бы определить разнообразные примитивные функции вроде~◊ic{car}.
Объявим пару макросов, которые помогут наполнить глобальное окружение смыслом:

◊indexC{definitial}
◊indexC{defprimitive}
◊indexC{primitive}
◊indexC{r.init}
◊indexC{cons}
◊indexC{car}
◊code:lisp{
(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (definitial name 'void) )
    ((definitial name value)
     (begin (set! r.init (make-variable-env r.init 'name value))
            'name ) ) ) )

(define-class primitive value (name address))

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (make-primitive 'name
         (lambda (v* r k)
           (if (= arity (length v*))
               (resume k (apply value v*))
               (wrong "Incorrect arity" 'name v*) ) ) ) ) ) ) )

(define r.init (make-null-env))

(defprimitive cons cons 2)
(defprimitive car car 1)
}

Интерпретатор должен уметь вызывать примитивы как~функции, то~есть через ◊ic{invoke}.
Каждый примитив имеет два поля.
Первое из них служит для упрощения отладки: там хранится имя примитива.
Естественно, это лишь подсказка, ведь ничто не~мешает связать один и тот~же примитив с~разными именами.
◊footnote{
  Многие реализации сохраняют изначальное имя примитива.
  ◊; TODO: ◊code:lisp?
  В~результате выражение ◊ic{(begin (set!~foo~car) (set!~car~3) foo)}
  обычно возвращает ◊ic{#<car>} — собственное имя примитива,
  независимо от~того, через какую переменную к~нему обращаются.
}
Второе поле хранит «адрес» примитива — ссылку на соответствующую реализацию функции,
которую будет исполнять интерпретатор внутри ◊ic{invoke}:

◊indexC{invoke!◊ic{primitive}}
◊code:lisp{
(define-method (invoke (f primitive) v* r k)
  ((primitive-address f) v* r k) )
}

◊; терминальное продолжение? и ссылку для предметного указателя на тот раздел, где чёрная дыра
◊; и дальше (раньше?) по тексту тоже называй его терминальным, а не начальным
◊indexR{продолжения (continuations)!терминальное продолжение}
◊indexR{терминальное продолжение}
Для запуска нашего прекрасного интерпретатора остаётся лишь определить терминальное продолжение,
аналогичное начальному окружению ◊ic{null-env}.
Это~продолжение отображает на экране всё, что ему передают.

◊indexC{bottom-cont}
◊indexC{resume!◊ic{bottom-cont}}
◊indexC{chapter3-interpreter}
◊code:lisp{
(define-class bottom-cont continuation (f))

(define-method (resume (k bottom-cont) v)
  ((bottom-cont-f k) v) )

(define (chapter3-interpreter)
  (define (toplevel)
    (evaluate (read)
              r.init
              (make-bottom-cont 'void display) )
    (toplevel) )
  (toplevel) )
}

Заметьте, что вместо Лиспа мы могли~бы легко написать похожий интерпретатор на истинно объектно-ориентированном языке,
например на Smalltalk,◊seeCite{gr83} получив заодно доступ к~его хвалёному отладчику и среде разработки.
Для полного счастья останется только открыть миллион окошек с~контекстными подсказками.
