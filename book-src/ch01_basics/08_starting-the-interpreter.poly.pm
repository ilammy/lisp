#lang pollen

◊section[#:label "basics/sect:starting-the-interpreter"]{Запускаем интерпретатор}

Нам осталось показать лишь одну вещь: дверь в~наш новый мир.

◊indexC{chapter1-scheme}
◊code:lisp[#:chunk "chapter1-scheme"]{
(define (chapter1-scheme)
  (define (toplevel)
    (display (evaluate (read) env.global))
    (toplevel) )
  (toplevel) )
}

Поскольку наш интерпретатор ещё мал и неопытен, но подаёт большие надежды,
предлагаем вам в~качестве упражнения написать функцию, позволяющую из него~выйти.
