#lang pollen

◊subsection[#:label "lisp1-2-omega/recusion/ssect:no-assignment"]{Рекурсия без~присваивания}

◊indexR{язык!чисто функциональный}
◊indexR{рекурсия!без присваивания}
Рассмотренные ранее реализации формы ◊ic{letrec} используют присваивания для инициализации переменных.
Языки, называемые ◊term{чисто функциональными}, не~имеют в~своём распоряжении операторов присваивания;
выражения в~чистых языках принципиально лишены побочных эффектов,
а~чем, как не~побочным эффектом вычислений, является изменение значения переменной?

Как парадигма программирования запрет на использования присваивание имеет свои преимущества:
гарантированная ссылочная прозрачность выражений развязывает руки множеству оптимизаций,
позволяя перемещать и распараллеливать части программ, использовать ленивые вычисления, и~т.~д.
С~другой стороны, без присваивания реализация некоторых алгоритмов значительно усложняется,
не~говоря уже о~том, что программы исполняются на реальных компьютерах,
которые работают исключительно благодаря побочным эффектам машинных инструкций.

◊indexC{letrec}
Первое, что приходит в~голову — это сделать ◊ic{letrec} ещё одной специальной формой,
как поступает ML и подобные ему языки.
Модифицируем ◊ic{evaluate} для обработки этого случая:

◊code:lisp{
...
((letrec)
 (let ((new-env (extend env
                        (map car (cadr e))
                        (map (lambda (binding) the-uninitialized-marker)
                             (cadr e) ) )))
      (map (lambda (binding)                ; map во имя беспорядка!
             (update! (car binding)
                      new-env
                      (evaluate (cadr binding) new-env) ) )
           (cadr e) )
      (eprogn (cddr e) new-env) ) ) ...
}

В~этом случае побочные эффекты всё равно присутствуют,
но уже контролируемо, на уровне интерпретатора, внутри ◊ic{update!}
С~точки зрения определяемого языка ◊ic{letrec} побочных эффектов не~имеет.
Стоит заметить, что мы намеренно не~указываем порядок вычислений, используя ◊ic{map},
которая, в~отличие от~◊ic{for-each}, вольна обрабатывать список в~любом удобном порядке.
◊footnote{
  За~это мы расплачиваемся тем, что ◊ic{map} собирает результаты вычислений в~бесполезный список,
  который тут~же удаляется после создания.
}