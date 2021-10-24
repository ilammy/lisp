#lang pollen

◊subsection[#:label "assignment/implementation/ssect:values"]{Представление значений}

Мы~решили представлять данные внутри интерпретатора в~виде замыканий, обменивающихся сообщениями.
Для простоты ограничимся пустым списком, булевыми значениями, символами, числами, точечными парами и~функциями.
Рассмотрим эти типы данных по~очереди.

◊indexE{type@◊ic{'type}}
◊indexE{boolify@◊ic{'boolify}}
Все~значения будут основываться на функции, которая отвечает как минимум на два сообщения:
◊enumerate{
  ◊item{запрос типа (◊ic{type});}
  ◊item{приведение к~логическому типу (◊ic{boolify}).}
}
Значения конкретных типов данных будут реагировать на~свои специфичные сообщения.
Таким~образом, все определения значений имеют следующий скелет:

◊code:lisp{
(lambda (msg)
  (case msg
    ((type)    ...)
    ((boolify) ...)
    ... ) )
}

Существует только один пустой список и, согласно~◊R5RS, ◊ic{if}~считает его истиной:

◊indexC{the-empty-list}
◊code:lisp{
(define the-empty-list
  (lambda (msg)
    (case msg
      ((type)    'null)
      ((boolify) (lambda (x y) x)) ) ) )
}

Булевы значения создаются функцией ◊ic{create-boolean}:

◊indexC{create-boolean}
◊code:lisp{
(define (create-boolean value)
  (let ((combinator (if value (lambda (x y) x)
                              (lambda (x y) y) )))
    (lambda (msg)
      (case msg
        ((type)    'boolean)
        ((boolify) combinator) ) ) ) )
}

◊indexE{name@◊ic{'name}}
Символы имеют имя, которое они назовут, если их вежливо попросить с~помощью ◊ic{name}.
Имена представляются родными символами~Scheme.

◊indexC{create-symbol}
◊code:lisp{
(define (create-symbol v)
  (lambda (msg)
    (case msg
      ((type)    'symbol)
      ((name)    v)
      ((boolify) (lambda (x y) x)) ) ) )
}

◊indexE{value@◊ic{'value}}
Числа~же отвечают на сообщение ◊ic{value}:

◊indexC{create-number}
◊code:lisp{
(define (create-number v)
  (lambda (msg)
    (case msg
      ((type)    'number)
      ((value)    v)
      ((boolify) (lambda (x y) x)) ) ) )
}

◊indexE{behavior@◊ic{'behavior}}
◊indexE{tag@◊ic{'tag}}
Функции обладают более развитым словарным запасом из двух сообщений: ◊ic{tag} и~◊ic{behavior}.
Сообщение ◊ic{behavior}, конечно~же, возвращает код функции,
а~◊ic{tag} хранит адрес функции в~памяти.

◊indexC{create-function}
◊code:lisp{
(define (create-function tag behavior)
  (lambda (msg)
    (case msg
      ((type)     'function)
      ((boolify)  (lambda (x y) x))
      ((tag)      tag)
      ((behavior) behavior) ) ) )
}

◊indexR{точечные пары!представление}
◊indexR{представление!точечных пар}
◊indexC{cdr}
Остались только точечные пары.
Точечная пара — это объект, содержащий два значения, каждое из которых можно изменять независимо.
Поэтому мы представляем их в~виде двух адресов: один ссылается на~◊ic{car}, второй — на~◊ic{cdr}.
Такой подход несколько необычен,
так~как традиционно точечные пары представляются не~парой независимых значений,
а~коробкой с~двумя отделениями,
◊footnote{
  Причём по~результатам серьёзных исследований вроде~◊cite{cla79,cg77},
  первым желательно размещать значение ◊ic{cdr}, а~не~◊ic{car},
  чтобы доступ к~нему не~требовал дополнительных сдвигов.
  Другим преимуществом такого расположения является~то,
  что точечные пары представляют связные списки,
  и~возможно определить общий класс «связных~объектов» с~единственным полем~◊ic{cdr}.
}
но~одним адресом.
Иным подходом является размещение точечных пар в~параллельных массивах:
хранить значения ◊ic{car} и ◊ic{cdr} в~двух отдельных массивах,
но~по~одинаковым индексам,
используя эти индексы для адресации точечных~пар.
Однако у~каждого элемента массива тоже есть собственный адрес,
то~есть точечные~пары — это~пары адресов.

Для удобства мы также определим функцию, выделяющую в~памяти целые списки.
Она принимает список значений, которые необходимо разместить в~памяти, собственно память, и~продолжение,
которому передаётся созданный список и новое состояние памяти.

◊indexC{allocate-list}
◊indexC{allocate-pair}
◊indexC{create-pair}
◊code:lisp{
(define (allocate-list v* s k)
  (define (consify v* k)
    (if (pair? v*)
        (consify (cdr v*) (lambda (v ss)
                            (allocate-pair (car v*) v ss k) ))
        (k the-empty-list s) ) )
  (consify v* k) )

(define (allocate-pair a d s k)
  (allocate 2 s
   (lambda (a* ss)
     (k (create-pair (car a*) (cadr a*))
        (update (update ss (car a*) a) (cadr a*) d) ) ) ) )

(define (create-pair a d)
  (lambda (msg)
    (case msg
      ((type)    'pair)
      ((boolify) (lambda (x y) x))
      ((set-car) (lambda (s v) (update s a v)))
      ((set-cdr) (lambda (s v) (update s d v)))
      ((car)     a)
      ((cdr)     d) ) ) )
}
