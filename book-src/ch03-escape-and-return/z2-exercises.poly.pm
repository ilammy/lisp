#lang pollen

◊section*[#:label "escape/sect:exercises"]{Упражнения}


◊exercise{escape/ex:cc-cc}

Что~вернёт ◊nobr{◊ic{(call/cc call/cc)}}?
Зависит~ли ответ от~порядка вычислений?


◊exercise{escape/ex:cc-cc-cc-cc}

А~что вернёт ◊nobr{◊ic{((call/cc call/cc) (call/cc call/cc))}}?


◊exercise{escape/ex:tagbody}

◊indexC{tagbody}
◊indexC{go}
Реализуйте пару ◊ic{tagbody}/◊ic{go} с~помощью ◊ic{block}, ◊ic{catch}, ◊ic{labels}.
Напомним синтаксис этой формы из~◊|CommonLisp|:

◊; TODO: форматирование здесь:
◊code:lisp{
(tagbody
         ◊ii{выражения◊sub{0}}...
  ◊ii{метка◊sub{1}} ◊ii{выражения◊sub{1}}...
         ...
  ◊ii{метка◊sub{i}} ◊ii{выражения◊sub{i}}...
         ... )
}

Все~◊ii{выражения◊sub{i}} (и~только~они) могут содержать
безусловные переходы~◊nobr{◊ic{(go ◊ii{метка})}}
и~возвраты~◊nobr{◊ic{(return ◊ii{значение})}}.
Если~◊ic{return} не~используется, то~форма ◊ic{tagbody} возвращает~◊ic{nil}.


◊exercise{escape/ex:arity-optimize}

Вы~скорее всего заметили, что функции при вызове проверяют фактическую арность:
количество переданных им аргументов.
Измените механизм создания функций так, чтобы правильная арность рассчитывалась только один~раз.
Можете считать, что функции бывают только фиксированной арности.


◊exercise{escape/ex:apply}

Определите функцию ◊ic{apply} для интерпретатора из этой~главы.


◊exercise{escape/ex:dotted}

Реализуйте поддержку функций переменной арности для интерпретатора из этой~главы.


◊exercise{escape/ex:evaluate}

Измените функцию запуска интерпретатора так, чтобы она вызывала ◊ic{evaluate} только единожды.


◊exercise{escape/ex:cc-value}

Способ реализации ◊ic{call/cc} в~разделе~◊ref{escape/implementation/ssect:call/cc}
возвращает непосредственно внутреннее представление продолжений интерпретатора,
не~делая различий между внутренними и реифицированными продолжениями.
Чтобы определяемый язык мог активировать эти продолжения,
мы~вынуждены реализовывать метод ◊ic{invoke} для всех экземпляров класса~◊ic{continuation}.
Переопределите ◊ic{call/cc} так, чтобы она возвращала объекты определяемого языка,
являющиеся экземплярами класса-наследника ◊ic{value}, соответствующего реифицированным продолжениям.


◊exercise{escape/ex:eternal}

◊indexR{бесконечный цикл}
Напишите на ◊CommonLisp функцию ◊ic{eternal-return}, принимающую замыкание и~вызывающую его в~бесконечном цикле.
Этот цикл должен быть истинно бесконечным: перекройте абсолютно все возможные выходы из~него.


◊exercise{escape/ex:crazy-cc}

Рассмотрим следующую хитроумную функцию (спасибо за~неё Алану~Бодену):

◊indexR{коробки}
◊indexC{make-box}
◊code:lisp{
(define (make-box value)
  (let ((box
         (call/cc
          (lambda (exit)
            (letrec
             ((behavior
               (call/cc
                (lambda (store)
                  (exit (lambda (msg . new)
                          (call/cc
                           (lambda (caller)
                             (case msg
                               ((get) (store (cons (car behavior)
                                                   caller )))
                               ((set)
                                (store
                                 (cons (car new)
                                       caller ) ) ) ) ) ) )) ) ) ))
             ((cdr behavior) (car behavior)) ) ) ) ))
    (box 'set value)
    box ) )
}

Предположим, в~◊ic{box1} лежит значение ◊ic{(make-box~33)},
тогда что получится в~результате следующих вычислений?

◊code:lisp{
(box1 'get)
(begin (box1 'set 44) (box1 'get))
}


◊exercise{escape/ex:generic-evaluate}

Среди всех наших функций только ◊ic{evaluate} не~является обобщённой.
Что~если создать абстрактный класс выражений,
от~которого будут наследоваться подклассы конкретных синтаксичесих~форм?
Конечно, в~этом случае ◊ic{evaluate} будет принимать не~S-выражения, а~объекты,
и~естественно, эта функция должна быть обобщённой.
Такой подход позволяет легко и последовательно вводить новые специальные формы,
без необходмости определять их все сразу.
Воплотите эту идею в~жизнь.


◊exercise{escape/ex:throw}

Реализуйте оператор ◊ic{throw} как функцию, а~не~специальную~форму.


◊exercise{escape/ex:cps-speed}

Сравните скорость выполнения обычного кода и переписанного в~стиле передачи продолжений.


◊exercise{escape/ex:the-current-cc}

◊indexC{the-current-continuation}
Реализуйте ◊ic{call/cc} с~помощью функции ◊ic{the-current-continuation}, которая определяется следующим образом:

◊code:lisp{
(define (the-current-continuation)
  (call/cc (lambda (k) k)) )
}
