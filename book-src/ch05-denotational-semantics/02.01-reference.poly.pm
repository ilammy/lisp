#lang pollen

◊subsection[#:label "denotational/semantics/ssect:var-ref"]{Обращения к~переменным}

Простейшая из денотаций — получение значения переменной:

◊code:denotation{
(define (meaning-reference n)
  (lambda (r k s)
    (k (s (r n)) s) ) )
}

Денотация ссылки на переменную~◊${◊n} — это ◊${\lambda}-терм,
который по~окружению~◊${◊r}, продолжению~◊${◊k} и~памяти~◊${◊s}
сначала определяет адрес переменной с~помощью окружения: ◊${(◊|r|\ ◊|n|)},
потом извлекает значение по~этому адресу из~памяти: ◊${(◊|s|\ (◊|r|\ ◊|n|))},
затем, наконец, передаёт полученное значение продолжению вместе с~исходной памятью
(так~как чтение её не~меняет).

Здесь мы воспользовались упомянутым ранее сокращением для функций нескольких аргументов.
Конечно, можно было~бы писать более~строго:

◊code:denotation{
(define (meaning-reference n)
  (lambda (r)
    (lambda (k)
      (lambda (s)
        (k (s (r n)) s) ) ) ) )
}

Но~нам нет~смысла выставлять напоказ свою педантичность, поэтому мы не~будем использовать подобную запутанную запись.
Пожалуй, мы~будем писать ещё проще, приняв синтаксис, похожий на~◊ic{define}, что позволит убрать ещё несколько слоёв~◊${\lambda}:

◊code:denotation{
(define ((meaning-reference n) r k s)
  (k (s (r n)) s) )
}

◊indexR{р о0@◊${◊|r|_0} (начальное окружение)}
◊indexE{r ho0@◊${◊|r|_0} (начальное окружение)}
Обращение к~переменной является ошибкой, если переменная не~существует в~окружении.
Денотация учитывает эту ситуацию в~определении начального окружения~◊${◊|r|_0}:

◊code:denotation{
(define (r.init n)
  (wrong "No such variable" n) )
}

◊indexE{.bot@◊${\bot}, дно}
◊indexR{.дно@◊${\bot}, дно}
◊indexR{строгие функции}
◊indexR{функции!строгие}
◊indexE{wrong@◊ii{wrong}}
Поиск неизвестной переменной в~окружении приведёт к~вычислению ◊ii{wrong},
которая в~свою очередь вернёт особое значение, обычно обозначаемое~◊${\bot}
(читается: «дно», по-английски: ◊english{bottom}).
Значение~◊${\bot} «отравляет» все последующие вычисления — то~есть ◊${\forall f \colon f(\bot) = \bot}.
Следовательно, если возникает ошибка, то~программа возвращает~◊${\bot}, что явно сигнализирует о~проблеме.
На~самом деле, это не~◊${\bot} само по себе имеет такое свойство,
а~функции так с~ним обращаются — в~этом случае они называются ◊term{строгими}.
Функция~◊${f} строга тогда и~только тогда, когда ◊${f(\bot) = \bot}.
Если условиться использовать только строгие функции,
то~это избавит нас в~некоторой мере от обработки ошибок и позволяет сконцентрироваться на более важных~вещах.
