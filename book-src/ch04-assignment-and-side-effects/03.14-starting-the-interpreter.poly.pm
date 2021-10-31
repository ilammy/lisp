#lang pollen

◊subsection[#:label "assignment/implementation/ssect:starting"]{Запускаем интерпретатор}

Наконец, всё готово для запуска нового интерпретатора.
Главный цикл вызывает ◊ic{evaluate} с~рекурсивным продолжением,
которое обеспечивает непрерывность сознания интерпретатора,
передавая следующему вызову память предыдущего.

◊indexC{chapter4-interpreter}
◊code:lisp{
(define (chapter4-interpreter)
  (define (toplevel s)
    (evaluate (read) r.global s
              (lambda (v ss)
                (display (transcode-back v ss))
                (toplevel ss) ) ) )
  (toplevel s.global) )
}

◊indexR{представление!значений}
Главной особенностью этого интерпретатора является~то,
что представление данных интерпретируемого языка не~совпадает с~представлением данных в~языке реализации.
Особенно разительно отличаются точечные пары и, следовательно, списки.
Поэтому мы вынуждены воспользоваться функцией ◊ic{transcode-back}
для преобразования результата вычислений в~нечто понятное ◊ic{display},
чтобы его можно было вывести на~экран.
Эту~вспомогательную функцию можно было~бы~и сразу объединить с~примитивом ◊ic{display}.

◊indexC{transcode-back}
◊code:lisp{
(define (transcode-back v s)
  (case (v 'type)
    ((null)     '())
    ((boolean)  ((v 'boolify) #t #f))
    ((symbol)   (v 'name))
    ((number)   (v 'value))
    ((pair)     (cons (transcode-back (s (v 'car)) s)
                      (transcode-back (s (v 'cdr)) s) ))
    ((function) v) ; упражнение: какие ещё варианты возможны?
    (else       (wrong "Unknown type" (v 'type))) ) )
}
