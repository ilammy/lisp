#lang pollen

◊subsection[#:label "basics/evaluating-forms/ssect:sequence"]{Последовательность}

◊indexC{begin}
◊indexC{progn}
Существует специальная форма, позволяющая вычислить группу форм последовательно и в~определённом порядке.
Как и старые~добрые блоки ◊ic{begin~...~end} из семейства языков Алгола,
в~Scheme эта форма называется ◊ic{begin};
в~других Лиспах она обычно зовётся ◊ic{progn} — обобщенная версия ◊ic{prog1}, ◊ic{prog2}, и~т.~д.
Собственно организацию последовательности мы перепоручаем функции~◊ic{eprogn}.

◊indexC{eprogn}
◊code:lisp{
... (case (car e)
      ((begin) (eprogn (cdr e) env)) ... ) ...
}
◊code:lisp{
(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env) )
          (evaluate (car exps) env) )
      '() ) )
}

Такое определение последовательных вычислений не~допускает разночтений.
Стоит обратить внимание на хвостовую рекурсию,
благодаря которой значением ◊ic{eprogn} является результат последнего вызова ◊ic{evaluate}.
(О~хвостовой рекурсии мы позже поговорим подробнее.)
◊seePage{escape/pr-cont/ssect:tail-recusion}

◊indexC{begin!возвращаемое значение}
◊indexR{возвращаемые значения!формы (begin)@формы ◊ic{(begin)}}
◊indexC{empty-begin}
Ещё одним интересным моментом является то, что́ возвращается при вычислении формы~◊ic{(begin)}.
Сейчас это пустой список.
Но почему именно ◊ic{()}, почему не~что-то другое, вроде ◊ic{:3} или~◊ic{(^_^)}?
Мы выбрали пустой список по привычке, доставшейся в~наследство от Лиспа:
в~любой непонятной ситуации возвращай ◊ic{nil}.
Но в~мире, где ◊term{ложь}, ◊ic{nil} и~◊ic{()} — это совершенно различные вещи,
что из них лучше подходит на роль ◊emph{ничего}?
Поэтому пусть в~нашем языке вычисление ◊ic{(begin)} будет возвращать специальное значение ◊ic{empty-begin},
которое определяется как (почти) случайное число~◊ic{813}.
◊seeCite{leb05}.

◊indexC{eprogn}
◊indexC{empty-begin}
◊code:lisp{
(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env) )
          (evaluate (car exps) env) )
      empty-begin ) )

(define empty-begin 813)
}

◊indexR{синтаксис!begin@◊ic{(begin)}}
Наше затруднение как авторов реализации языка возникает из-за того, что ◊ic{begin} ◊emph{обязана} вернуть ◊emph{какое-то} значение.
Как и Scheme, наш язык мог~бы не~придавать какого-либо смысла форме ◊ic{(begin)};
мы можем или допускать такое написание и возвращать любое удобное значение,
или~же не~допускать и считать такое выражение синтаксической ошибкой.

◊indexC{unspecified@#<unspecified>}
Пользователям нашего языка не~рекомендуется пользоваться ◊ic{begin} без аргументов,
так как не~определено, что именно получится в~результате.
Некоторые реализации имеют специальный объект: ◊ic{#<unspecified>},
который возвращается в~случае, когда нет ничего более подходящего.
Обычно единственное, что с~ним можно сделать — это~вывести на~печать.
(Не~следует путать ◊ic{#<unspecified>} с~псевдозначением у~неопределённых переменных.)
◊seePage{lisp1-2-omega/recusion/ssect:uninitialized}

◊bigskip

◊indexR{язык!чисто функциональный}
◊indexR{побочные эффекты}
Последовательное вычисление выражений не~имеет смысла в~чисто функциональных языках
(где функции не~имеют побочных эффектов).
Действительно, если значение функции не~используется, то зачем его вычислять?
Теоретически в~этом нет смысла, но практически — программы исполняют реальные компьютеры.
Представим себе игру, написанную на чисто функциональном языке;
очевидно, что вычисления занимают какое-то время вне~зависимости от того, используются~ли их результаты или нет.
Нас может интересовать именно этот «побочный эффект» — замедление работы, — а~не~получаемые результаты.
Тогда можно последовательно что-то вычислять, например, чтобы скорость игры была адекватна рефлексам игрока
(конечно~же, только если достаточно умный компилятор не~оптимизирует «бесполезный»~код).

◊indexC{begin!необходимость}
Так как в~Scheme есть операции ввода-вывода, которые имеют побочные эффекты,
то~для нас есть смысл пользоваться формой ◊ic{begin}.
Очевидно, что лучше сначала задать вопрос (с~помощью ◊ic{display}),
а~потом прочитать ответ (с~помощью ◊ic{read}), чем сделать наоборот.
Здесь-то как раз и нужно упорядочить вычисления.
Но~не~только ◊ic{begin} может их упорядочивать.
Например, условный оператор может:

◊code:lisp{
(if ◊${\alpha} ◊${\beta} ◊${\beta}) ◊(eq) (begin ◊${\alpha} ◊${\beta})
}

◊phantomlabel{basics/forms/sequence/par:gensym-puzzle}
◊noindent
И~◊ic{lambda} тоже может:
◊footnote[#:hug-next-blocks #t]{
  Переменная ◊ii{void} не~должна быть свободной в~◊${\beta}.
  Это условие выполняется, если ◊ii{void} никогда не~встречается в~◊${\beta}.
  Обычно в~таком случае используется ◊ic{gensym}, чтобы получить
  гарантированно уникальное имя переменной.
  ◊seeEx{basics/ex:no-gensym}
}

◊code:lisp{
(begin ◊${\alpha} ◊${\beta}) ◊(eq) ((lambda (◊ii{void}) ◊${\beta}) ◊${\alpha})
}

Как видно из этого примера, в~Scheme ◊ic{begin} не~является необходимой специальной формой,
так как её поведение можно проэмулировать с~помощью функций
благодаря тому, что при вызове функции её аргументы вычисляются перед исполнением тела
(передача аргументов ◊term{по~значению}).
