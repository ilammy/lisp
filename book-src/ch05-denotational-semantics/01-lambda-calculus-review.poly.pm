#lang pollen

◊section[#:label "denotational/sect:lambda-review" #:alt "Краткий обзор λ-исчисления"]{Краткий~обзор ◊${\lambda}-исчисления}

◊indexR{интерпретатор!в~денотационной семантике}
Суть денотационной семантики лежит в~определении специфичной для языка функции,
называемой ◊term{интерпретатором} (◊english{valuation~function}),
которая переводит корректные программе на языке в~соответствующие им денотации — элементы множества денотаций.
В~качестве такого множества мы решили взять ◊${\lambda}-исчисление,
так~как оно имеет простую структуру и довольно близко к~Scheme,
который иногда так и называют «эффективным интерпретатором~◊${\lambda}-термов».
◊seeCite{ss75,wan84}

◊indexR{абстракция}
◊indexR{аппликация}
Сейчас мы быстро пробежимся по~основам ◊${\lambda}-исчисления.
◊footnote{
  Хорошим введением в~◊${\lambda}-исчисление могут служить книги~◊cite{sto77, gor88}.
  Первой книгой библии ◊${\lambda}-исчисления считается~◊cite{bar84}.
}
Его~синтаксис очень простой:
выражения (термы) ограничиваются переменными, абстракциями и~аппликациями.
Множество всех доступных переменных будем обозначать ◊${◊Vset{Переменные}},
а~символом~◊${\Lambda} обозначим множество всех термов ◊${\lambda}-исчисления.
◊${\Lambda}~индуктивно определяется следующим образом:

◊; TODO: это всё надо красиво рендерить, всю пятую главу вычитывай внимательно
◊; конкретно здесь \in следует чуть віше поднять
◊$${
\begin{array}{rll}
  ◊math-ii{переменные}\colon & \forall x \in ◊Vset{Переменные} \colon                & x \in \Lambda             \\
  ◊math-ii{абстракции}\colon & \forall x \in ◊Vset{Переменные}, M \in \Lambda \colon & \lambda x . M \in \Lambda \\
  ◊math-ii{аппликации}\colon & \forall M, N \in \Lambda \colon                       & (M\ N) \in \Lambda        \\
\end{array}
}

◊indexR{Лисп!и лямбда-исчисление@и~◊${\lambda}-исчисление}
◊indexR{лямбда-исчисление@◊${\lambda}-исчисление!синтаксис}
◊indexR{синтаксис!лямбда-исчисления@◊${\lambda}-исчисления}
Как и в~случае с~Лиспом, синтаксис не~особо важен и термы ◊${\lambda}-исчисления одинаково удобно записываются в~виде S-выражений.
◊footnote{Фактически, именно это Джон~Маккарти и~сделал в~1960~году.}
Таким образом, множество термов ◊${\lambda}-исчисления синтаксически совпадает с~подмножеством программ на~Scheme,
которые используют лишь одну специальную форму — ◊ic{lambda}:

◊$${
x \qquad\qquad ◊math-ic{(lambda ($x$) $M$)} \qquad\qquad ◊math-ic{($M$ $N$)}
}

◊indexR{абстракция!редукция}
◊indexR{бета-редукция@◊${\beta}-редукция}
◊indexR{функции!в лямбда-исчислении@в~◊${\lambda}-исчислении}
◊indexR{функции!модель подстановки}
◊indexR{модель подстановки}
◊${\lambda}-исчисление позволяет определять функции.
Существует простое правило их применения — ◊term{◊${\beta}-редукция}:
в~результате применения функции~◊${\lambda x . M} к~терму~◊${N}
получается новый терм, являющийся телом функции~◊${M},◊; TODO: кернинг запятых
в~котором вместо переменной~◊${x} используется~◊${N},
что~коротко записывается: ◊${M[x \to N]}.
Это~обычная модель подстановки, испокон веков используемая математикой как само собой разумеющееся.
Именно так проводятся вычисления в~программах на~Scheme, принадлежащих вышеупомянутому подмножеству
(без~побочных эффектов, с~одной специальной формой).

◊$${
\beta\text{-редукция}\colon \quad (\lambda x . M\ N) ◊to*{\beta} M[x \to N]
}

◊indexR{лексическое связывание!в лямбда-исчислении@в~◊${\lambda}-исчислении}
Подстановка ◊${M[x \to N]} должна быть достаточно сообразительной, чтобы нечаянно не~затронуть лишние переменные.
Имеются в~виду свободные переменные, которые могут появиться при подстановке, если ◊${M} является абстракцией.
Свободные переменные терма~◊${N} не~получают значений одноимённых переменных терма~◊${M} при подстановке ◊${N}~в~◊${M};
подстановка значений вместо переменных может быть только явной.
Говоря более понятным языком, в~◊${\lambda}-исчислении принято лексическое связывание.
Подобная подстановка определяется следующим образом (скобки отделяют выражения, где происходят изменения):

◊$${
\begin{align*}
  &  x[x \to N] = N                                     \\
  &  y[x \to N] = y \quad \text{если} \ x \ne y           \\
  &  (\lambda x . M)[x \to N] = \lambda x . M           \\
  &  (\lambda y . M)[x \to N] = \lambda z . \big(M[y \to z][x \to N]\big)
     \quad \text{где} \ x \ne y \ \text{и} \ z \ \text{не~свободна~в} \ M \ \text{и} \ N \\
  &  (M_1\ M_2)[x \to N] = (M_1[x \to N]\ M_2[x \to N]) \\
\end{align*}
}

◊indexR{редексы}
◊indexR{приводимые!выражения}
◊indexR{форма!нормальная}
◊indexR{нормальная форма}
◊indexR{теорема Чёрча~–~Россера}
◊indexR{Чёрча~–~Россера, теорема}
◊term{Редексом} (от~◊english{reducible~expression}) или приводимым выражением
называется аппликация, где первый терм (стоящий на~месте функции) является абстракцией.
◊${\beta}-редукция сокращает редексы, избавляется от~них.
Если терм не~содержит редексов (является неприводимым), то~говорят, что он находится в~◊term{нормальной форме}.
Термы ◊${\lambda}-исчисления не~обязательно имеют нормальную форму,
но~если она существует, то,~в~соответствии с~теоремой Чёрча~–~Россера,
каждый приводимый терм имеет не~более одной нормальной~формы.

◊indexR{стратегия вычислений}
◊indexR{вычисления!стратегия вычислений}
◊indexE{Scheme!порядок вычислений}
◊indexR{нормальный порядок вычислений}
◊indexR{аппликативный порядок вычислений}
◊indexR{энергичный порядок вычислений}
◊indexR{порядок вычислений!нормальный}
◊indexR{порядок вычислений!аппликативный}
◊indexR{порядок вычислений!энергичный}
◊indexR{вызов!по~значению}
◊indexR{вызов!по~имени}
Если терм имеет нормальную форму, то~она обязательно достижима за конечное число ◊${\beta}-редукций.
◊term{Стратегией вычислений} называется правило выбора редекса (из~нескольких), который будет редуцирован следующим.
К~сожалению, есть как хорошие правила, так и~плохие.
Пример хорошего правила: редуцировать самый левый редекс.
Такая стратегия не~обязательно следует кратчайшим путём, но~гарантированно достигает нормальной формы (если~она вообще существует).
Это~правило называется~◊term{нормальным} порядком вычислений, он~же ◊term{вызов по~имени}.
Примером плохого правила — именно его использует Scheme — является ◊term{аппликативный} порядок вычислений,
также известный как ◊term{вызов по~значению}.
В~этом случае функция применяется лишь после вычисления аргументов.

Рассмотрим несколько примеров.

Вот~терм, который не~имеет нормальной~формы:

◊$${
(\omega\ \omega)
\quad \text{где} \ \omega = \lambda x . (x\ x)
}

◊noindent
так как

◊$${
(\omega\ \omega) ◊to*{\beta} (\omega\ \omega)
                 ◊to*{\beta} (\omega\ \omega)
                 ◊to*{\beta} \dots
}

◊noindent
В~Scheme подобная программа приводит к~бесконечному циклу и, очевидно, точно не~к~нормальной~форме.

А~вот пример терма, который имеет нормальную форму, но~правило вычислений, принятое в~Scheme, не~позволяет её~достичь.

◊$${
  \big((\lambda x . \lambda y . y\ (\omega\ \omega))\ z\big) ◊to*{\beta} (\lambda y . y\ z) ◊to*{\beta} z
}

◊noindent
Scheme сразу~же пойдёт вычислять аргумент~◊${(\omega\ \omega)} и~попадёт в~бесконечный цикл, так и не~дойдя до нормальной~формы.

И~наоборот, правило вычислений в~Scheme также позволяет успешно вычислять термы без нормальной формы.
Правило требует сокращать редексы в~аргументах сразу~же, но~запрещает избавляться от них в~теле функций:

◊$${
\lambda x . (\omega\ \omega)
}

◊indexR{порядок вычислений!ленивый}
◊indexR{ленивые вычисления}
◊indexR{вызов!по необходимости}
Так почему~же Scheme использует такое плохое правило?
Во-первых, компьютеры гораздо эффективнее обрабатывают вызовы по~значению, чем «хорошие» вызовы по~имени,
даже если их улучшить до ◊term{вызовов по~необходимости} (также известных как ◊term{ленивый} порядок вычислений).
Во-вторых, если плохое правило приводит к~нормальной форме,
то~это будет та~же форма, которую мы~бы получили при использовании хорошего правила.
Поэтому ради эффективности Scheme использует плохой аппликативный порядок.

◊indexR{лямбда-исчисление@◊${\lambda}-исчисление!синтаксис}
◊indexR{синтаксис!лямбда-исчисления@◊${\lambda}-исчисления}
В~◊${\lambda}-исчислении принят удобный синтаксический сахар для записи функций нескольких переменных:
считать~◊${\lambda xy . M} сокращением для~◊${\lambda x . \lambda y . M},
а~◊${(M\ N_1\ N_2)} — сокращённой формой~◊${((M\ N_1)\ N_2)}.
Предыдущий пример становится гораздо понятнее:

◊$${
(\lambda x y . y\ (\omega\ \omega)\ z)
}

◊noindent
Теперь бессмысленность вычисления значения локальной переменной~◊${x} очевидна:
ведь~это значение никак не~используется в~результате.

◊indexR{лямбда-исчисление@◊${\lambda}-исчисление!прикладное}
Естественно, мы легко могли~бы говорить про ◊${\lambda}-исчисление ещё пару~глав,
но~за этим отправляйтесь к~специализированной литературе: ◊cite{bar84,gor88,dil88}.
Среди всего прочего можно ввести вспомогательные термы, например, целые~числа.
В~результате такого расширения получается ◊term{прикладное} ◊${\lambda}-исчисление.
Мы~могли~бы также добавить особые правила приведения для новых~термов:
например, ◊${2 + 2 \to 4} — они~называются ◊${\delta}-правилами.
Однако, подобные расширения не~являются в~строгом смысле необходимыми,
так~как числа, булевы значения, сложение, вычитание, логическое~◊sc{или} —
всё~это можно представить и с~помощью обычных ◊${\lambda}-термов.
Даже~списочные структуры вместе с~◊ic{car}, ◊ic{cons} и~◊ic{cdr} возможно представить подобным образом.
◊seeCite{gor88}
◊seeEx{assignment/ex:lambda-cons}

◊indexR{эквивалентность!лямбда-термов@◊${\lambda}-термов}
◊indexR{машина Тьюринга}
◊indexR{Тьюринга, машина}
В~заключение стоит сказать, что ◊${\lambda}-исчисление —
это~хорошо проработанная теория вычислений, одновременно простая и~выразительная.
Действительно, ◊${\beta}-редукция равна по~выразительной силе машине~Тьюринга,
но~при этом не~такая запутанная.
Также важно, что ◊${\lambda}-исчисление обладает при той~же простоте более гибким понятием эквивалентных программ:
термы эквивалентны, если их нормальные формы совпадают.
По~всем перечисленным причинам именно ◊${\lambda}-исчисление было выбрано множеством денотаций для~этой~главы.
