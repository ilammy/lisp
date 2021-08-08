#lang pollen

◊subsection[#:label "escape/actors/ssect:quoting"]{Цитирование}

Специальная форма цитирования всё так~же является самой простой,
её~реализация сводится к~передаче цитируемого значения в~неизменной форме текущему продолжению:

◊indexC{evaluate-quotation}
◊code:lisp{
(define (evaluate-quotation v r k)
  (resume k v) )
}
