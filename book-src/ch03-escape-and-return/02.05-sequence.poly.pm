#lang pollen

◊subsection[#:label "escape/actors/ssect:sequence"]{Последовательность}

Здесь нам тоже потребуются два~продолжения:
текущее и продолжение вычисления оставшихся~форм.

◊indexC{begin-cont}
◊indexC{evaluate-begin}
◊indexC{resume◊ic{begin-cont}}
◊code:lisp{
(define-class begin-cont continuation (e* r))

(define (evaluate-begin e* r k)
  (if (pair? e*)
    (if (pair? (cdr e*))
        (evaluate (car e*) r (make-begin-cont k e* r))
        (evaluate (car e*) r k) )
    (resume k empty-begin-value) ) )

(define-method (resume (k begin-cont) v)
  (evaluate-begin (cdr (begin-cont-e* k))
                  (begin-cont-r k)
                  (begin-cont-k k) ) )
}

Случаи ◊ic{(begin)} и ◊nobr{◊ic{(begin ◊${\pi})}} тривиальны.
Если~же ◊ic{begin} передано более одного выражения, то вычисление первого из~них продолжается ◊nobr{◊ic{(make-begin-cont k e* r)}}.
Это продолжение принимает значение~◊ic{v} с~помощью ◊ic{resume}, игнорирует~его,
и~рекурсивно продолжает оставшиеся вычисления в~том~же окружении~◊ic{r} и с~тем~же продолжением~◊ic{k}.
◊footnote{
  Внимательный читатель наверняка заметил странную форму ◊nobr{◊ic{(cdr (begin-cont-e* k))}} в~методе~◊ic{resume}.

  Конечно, мы могли~бы отбросить уже вычисленное выражение ещё в~◊ic{evaluate-begin}:
  ◊nobr{◊ic{(make-begin-cont k (cdr e*) r)}}, и~получить тот~же результат.

  Дело в~том, что если при вычислениях произойдёт ошибка, то наличие исходного выражения в~продолжении облегчает отладку.
}
