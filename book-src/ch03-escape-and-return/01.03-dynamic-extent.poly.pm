#lang pollen

◊subsection[#:label "escape/forms/ssect:dynamic"]{Метки с~динамическим~временем~жизни}

Рассмотренная ранее эмуляция ◊ic{catch} через ◊ic{block} не~идеальна:
если ◊ic{catch} используется внутри ◊ic{block} или наоборот,
то переменная ◊ic{*active-catchers*} окажется искажённой.
Эмулировать неестественный для языка синтаксис непросто,
◊seeCite{fel90,bak92c}
это часто требует сложных архитектурных решений,
чтобы избежать конфликтов за ресурсы вроде~◊ic{*active-catchers*}.
Позже мы покажем, как подружить ◊ic{catch} и ◊ic{block} правильно —
для этого потребуется специальное приспособление: ◊nobr{◊ic{unwind-protect}}.
◊footnote{
  Альтернативным решением может быть эмуляция ◊ic{block} и~◊ic{return-from} с~помощью самих себя
  таким образом, чтобы они учитывали ◊ic{catch} и~◊ic{throw}.
  Это довольно непросто, но вполне возможно реализовать, например, с~помощью метаинтерпретации.
}
◊; TODO: нужны лы переносы в ◊ic? или лучше запретить? return-from, unwind-protect, etc.

◊; TODO: когда закончишь перевод в pollen и редактуру, хорошо было бы также пересмотреть предметный указатель
◊indexR{динамическое связывание}
◊indexR{продолжения (continuations)!время жизни!динамическое}
Как и все объекты в~Лиспе, продолжения имеют определённое время жизни.
Эмуляции ◊ic{catch} с~помощью ◊ic{block} хорошо показывает, что
продолжение, захватываемое ◊ic{catch}, доступно только во~время вычислений внутри тела~◊ic{catch}.
То~есть продолжение обладает ◊term{динамическим временем жизни}.
Такое поведение напоминает динамические переменные,
которые тоже существуют лишь во~время вычислений внутри связывающей формы, что их создала.
Если сделать список ◊ic{*active-catchers*} динамической переменной,
это позволит использовать ◊ic{catch} и ◊ic{block} одновременно,
так как задача поддержания целостности списка перекладывается на механизм динамических переменных.

◊indexC{catch!с~динамическими метками}
◊indexC{throw!с~динамическими метками}
◊code:lisp{
(define-syntax throw
  (syntax-rules ()
    ((throw tag value)
     (let* ((label tag)
            (escape (assv label (dynamic *active-catchers*))) )
       (if (pair? escape)
           ((cdr escape) value)
           (wrong "No associated catch to" label) ) ) ) ) )

(define-syntax catch
  (syntax-rules ()
    ((catch tag . body)
     (block label
       (dynamic-let ((*active-catchers*
                      (cons (cons tag (lambda (x)
                                        (return-from label x) ))
                            (dynamic *active-catchers*) ) ))
        . body ) ) ) ) )
}

◊indexR{переходы (escapes)!допустимость}
Продолжение, которое захватывает ◊ic{block} в~◊|CommonLisp|, также обладает динамическим временем жизни:
воспользоваться соответствующим ◊ic{return-from} можно лишь внутри формы ◊ic{block}.
Аналогично, ◊ic{throw} функционирует корректно только внутри соответствующей формы ◊ic{catch}.
Однако ◊ic{block} использует ◊emph{лексические} метки, что приводит к~интересным казусам, невозможным для ◊ic{catch}.
Если ◊ic{throw} и ◊ic{return-from} выполняют переход, чтобы отбросить оставшиеся вычисления,
то эти вычисления должны существовать в~момент совершения перехода.
Рассмотрим следующую программу:

◊code:lisp{
((block foo
   (lambda (x) (return-from foo x)) )
 33 )
}

◊indexR{замыкания (closures)!и переходы}
◊noindent
Здесь вызывается функция, выполняющая переход к~метке~◊ic{foo},
однако в~момент вызова этой функции соответствующее продолжение уже не~существует,
что должно привести к~ошибке во~время исполнения.
Создаваемое замыкание честно захватывает своё окружение,
включая и продолжение, связанное с~лексической меткой~◊ic{foo}.
Потом это замыкание возвращается как значение формы ◊ic{block} — и~на~этом вычисление формы~◊ic{block} завершено —
внутри больше не~осталось никаких вычислений, которые можно прервать переходом.
◊ic{return-from} должна проверять актуальность сохранённого продолжения
и~выполнять переход только при соблюдении этого условия.

Кроме того, не~стоит забывать и о~другом аспекте лексических меток, создаваемых ◊ic{block}.
Например:

◊; TODO: как это будет работать с интерактивными примерами?
◊; было бы неплохо спрятать ⟹, чтобы его нельзя было выделить
◊; может продолжить пользоваться ◊(is)?
◊code:lisp{
(block foo
  (let ((f1 (lambda (x) (return-from foo x))))
    (* 2 (block foo
           (f1 1) )) ) ) ⟹ 1
}

◊; TODO: <nobr> вообще не стандартный HTML, его следует транслировать в "white-space: nowrap"
◊noindent
Сравните с~поведением формы ◊ic{throw},
если заменить ◊nobr{“◊ic{block foo}”} на ◊nobr{“◊ic{catch 'foo}”}:

◊code:lisp{
(catch 'foo
  (let ((f1 (lambda (x) (throw 'foo x))))
    (* 2 (catch 'foo
           (f1 1) )) ) ) ⟹ 2
}

◊noindent
Функция~◊ic{f1} в~данном случае выйдет из ближайшей формы~◊ic{catch}, связанной с~меткой~◊ic{foo},
а~не~той, которая была видна при определении.
Соответственно, умножение будет выполнено и внешняя форма~◊ic{catch} получает значение~◊ic{2}.
