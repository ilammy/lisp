% -*- coding: utf-8 -*-

◊section[#:label "escape/sect:exercises"]{Упражнения}

◊begin{exercise}◊label{escape/ex:cc-cc}
Что вернёт ◊ic{(call/cc call/cc)}? Зависит~ли ответ от порядка вычислений?
◊end{exercise}


◊begin{exercise}◊label{escape/ex:cc-cc-cc-cc}
А~что вернёт ◊ic{((call/cc call/cc) (call/cc call/cc))}?
◊end{exercise}


◊begin{exercise}◊label{escape/ex:tagbody}
◊indexC{tagbody}◊indexC{go}
Реализуйте пару ◊ic{tagbody}/◊ic{go} с~помощью ◊ic{block}, ◊ic{catch} и
◊ic{labels}.
Напомним синтаксис этой формы из~{◊CommonLisp}:

◊code:lisp{
(tagbody
          |◊ii{выражения◊sub{0}}|...
  |◊hbox to 0pt{◊ii{метка◊sub{1}}}|        |◊ii{выражения◊sub{1}}|...
          ...
  |◊hbox to 0pt{◊ii{метка◊sub{i}}}|        |◊ii{выражения◊sub{i}}|...
          ...
)
}

Все ◊ii{выражения◊sub{i}} (и~только они) могут содержать безусловные переходы
◊ic{(go~◊ii{метка})} и возвраты ◊ic{(return~◊ii{значение})}.
Если ◊ic{return}
не~будет, то форма ◊ic{tagbody} возвращает~◊ic{nil}.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:arity-optimize}
Вы скорее всего заметили, что функции при вызове проверяют фактическую арность:
количество переданных им аргументов.
Измените механизм создания функций так,
чтобы правильная арность рассчитывалась только один раз.
Можете считать, что
функции бывают только фиксированной арности.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:apply}
Определите функцию ◊ic{apply} для интерпретатора из этой главы.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:dotted}
Реализуйте поддержку функций переменной арности для интерпретатора из этой
главы.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:evaluate}
Измените функцию запуска интерпретатора так, чтобы она вызывала ◊ic{evaluate}
только единожды.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:cc-value}
Способ реализации продолжений из
раздела~◊ref{escape/implementation/ssect:call/cc} отделяет продолжения от других
значений.
Поэтому мы вынуждены реализовывать метод ◊ic{invoke} лично для класса
продолжений, представляемых функциями языка определения.
Переопределите
◊ic{call/cc} так, чтобы она возвращала объекты определяемого языка, являющиеся
экземплярами класса-наследника ◊ic{value}, соответствующего продолжениям.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:eternal}
◊indexR{бесконечный цикл}
Напишите на {◊CommonLisp} функцию ◊ic{eternal-return}, принимающую замыкание и
вызывающую его в~бесконечном цикле.
Этот цикл должен быть истинно бесконечным:
перекройте абсолютно все выходы из него.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:crazy-cc}
Рассмотрим следующую хитроумную функцию (спасибо за неё Алану~Бодену):

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

Предположим, в~◊ic{box1} лежит значение ◊ic{(make-box~33)}, тогда что получится
в~результате следующих вычислений?

◊code:lisp{
(box1 'get)
(begin (box1 'set 44) (box1 'get))
}
◊end{exercise}


◊begin{exercise}◊label{escape/ex:generic-evaluate}
Среди всех наших функций только ◊ic{evaluate} не~является обобщённой.
Можно
создать класс программ, от которого будут наследоваться подклассы программ
с~различным синтаксисом.
Правда, в~этом случае мы не~сможем хранить программы
как S-выражения, они должны быть объектами.
Соответственно, функция
◊ic{evaluate} уже должна быть обобщённой.
Это позволит легко вводить новые
специальные формы (возможно, даже прямо из определяемого языка).
Воплотите эту
идею в~жизнь.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:throw}
Реализуйте оператор ◊ic{throw} как функцию, а не~специальную форму.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:cps-speed}
Сравните скорость выполнения обычного кода и переписанного в~стиле передачи
продолжений.
◊end{exercise}


◊begin{exercise}◊label{escape/ex:the-current-cc}
◊indexC{the-current-continuation}
Реализуйте ◊ic{call/cc} с~помощью функции ◊ic{the-current-continuation}, которая
определяется следующим образом:

◊code:lisp{
(define (the-current-continuation)
  (call/cc (lambda (k) k)) )
}
◊end{exercise}


◊section*[#:label "escape/sect:recommended-reading"]{Рекомендуемая литература}

Годный, нетривиальный пример использования продолжений приведён в
◊cite{wan80a}.
Также стоит почитать~◊cite{hfw84} об~эмуляции сопрограмм.
В~◊cite{dr87} прекрасно рассказано о~развитии понимания важности рефлексии
для управляющих форм.
