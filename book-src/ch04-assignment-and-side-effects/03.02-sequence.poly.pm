#lang pollen

◊subsection[#:label "assignment/implementation/ssect:sequence"]{Последовательность}

Хотя тут нет ничего интересного или нового, повторить смысл последовательных вычислений будет не~лишним.
Для~простоты мы считаем, что ◊ic{begin} передана как минимум одна~форма.

◊indexC{evaluate-begin}
◊code:lisp{
(define (evaluate-begin e* r s k)
  (if (pair? (cdr e*))
      (evaluate (car e*) r s
        (lambda (void ss)
          (evaluate-begin (cdr e*) r ss k) ) )
      (evaluate (car e*) r s k) ) )
}

Здесь чётко видно, как мы продолжаем вычисление тела~◊ic{begin}, передаём новое состояние памяти,
но полностью игнорируем значения всех вычисленных выражений,
кроме последнего, которое обрабатывается отдельно.
