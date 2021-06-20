#lang pollen

◊subsection[#:label "lisp1-2-omega/recusion/ssect:local-lisp2"]{Локальная~рекурсия в~◊(Lisp2)}

◊indexR{рекурсия!локальная}
Некоторые проблемы, с~которыми мы столкнулись при попытках определить~◊ic{fact} в~глобальном окружении,
возвращаются при попытках определить~◊ic{fact} локально.
Нам надо сделать что-то, чтобы вызов ◊ic{fact} из тела ◊ic{fact} был рекурсивным.
Чтобы ◊ic{fact} из функционального окружения была связана с~функцией вычисления факториала даже внутри самой функции вычисления факториала.
И~вот здесь как раз проявляется отличие между локальным и глобальным окружениями:
вспомните, что происходит, если искомая локальная переменная не~существует.
Рассмотрим следующую программу на~◊(Lisp2):

◊indexC{fact}
◊code:lisp{
(flet ((fact (n) (if (= n 0) 1
                     (* n (fact (- n 1))) )))
  (fact 6) )
}

Форма ◊ic{flet} связывает функцию вычисления факториала с~именем~◊ic{fact} в~своём внутреннем функциональном окружении.
Замыкание захватывает функциональное и параметрическое окружения, локальные для формы~◊ic{flet}.
Таким образом, ◊ic{fact} внутри ◊ic{fact} ссылается не~на функцию ◊ic{fact}, локальную для тела~◊ic{flet},
а~на какую-то другую функцию ◊ic{fact} из окружения всей формы~◊ic{flet}.
Эта функция не~обязана вычислять факториал (а~то и вовсе не~существует),
так что рекурсию мы тут не~получаем.

◊indexC{label}
◊indexR{специальные формы!label@◊ic{label}}
Эта проблема была очевидна ещё во~времена ◊(LISP)~1.5.
Для её решения была добавлена специальная форма~◊ic{label},
позволяющая определять локальные рекурсивные функции:

◊code:lisp{
(label fact (lambda (n) (if (= n 0) 1
                            (* n (fact (- n 1))) )))
}

Эта форма возвращает анонимную функцию, вычисляющую факториал.
Более того, это именно та функция, которая связана с~◊ic{fact} в~своём~же теле.

◊indexC{labels}
◊indexR{специальные формы!labels@◊ic{labels}}
К~сожалению, нельзя сказать, что ◊(LISP)~1.5 был ◊(Lisp2),
а~◊ic{label}, какой~бы удобной она ни~была, не~может легко справиться со~взаимной рекурсией.
Поэтому гораздо позже, если верить~◊cite{hs75}, был изобретён её ◊${n}-арный аналог — специальная форма~◊ic{labels}.
Синтаксис у~неё аналогичен ◊ic{flet}, но теперь гарантируется,
что замыкания будут создаваться в~окружении, где можно ссылаться на локальные функции.
С~помощью ◊ic{labels} можно определить и~◊ic{fact}:

◊code:lisp{
(labels ((fact (n) (if (= n 0) 1
                       (* n (fact (- n 1))) )))
  (fact 6) ) ◊(is) 720
}

◊noindent
и~взаимно рекурсивные ◊ic{odd?} и~◊ic{even?}:

◊code:lisp{
(funcall (labels ((even? (n) (if (= n 0) #t (odd? (- n 1))))
                  (odd? (n) (if (= n 0) #f (even? (- n 1)))) )
           (function even?) )
         4 ) ◊(is) #t
}

Таким образом, в~◊(Lisp2) необходимы две формы для расширения локального функционального окружения:
◊ic{flet} и~◊ic{labels}.
