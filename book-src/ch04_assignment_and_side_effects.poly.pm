% -*- coding: utf-8 -*-

◊section[#:label "assignment/sect:conclusions"]{Заключение}

Данная глава содержит колоссальное количество советов и описанных подводных ям.
Глобальные переменные таят множество неоднозначностей, которых становится ещё
больше с~появлением модулей.
Универсального определения равенства нет даже
в~математике, что~уж говорить о~программировании.
Наконец, цитирование также
отнюдь не~такое простое, каким кажется; от витиеватых цитат стоит
воздерживаться.


◊section[#:label "assignment/sect:exercises"]{Упражнения}

◊begin{exercise}◊label{assignment/ex:pure-min-max}
Определите чистую (без побочных эффектов) функцию ◊ic{min-max}.
◊end{exercise}

◊begin{exercise}◊label{assignment/ex:lambda-cons}
◊indexR{чисто функциональные структуры данных}
◊indexR{точечные пары!чисто функциональные}
Точечные пары, которые мы реализовали с~помощью замыканий, реагируют на
сообщения-символы.
Запретите модификацию пар (◊ic{set-car!} и~◊ic{set-cdr!})
и~реализуйте их, используя исключительно ◊ic{lambda}-формы.
◊end{exercise}

◊begin{exercise}◊label{assignment/ex:destructive-eq}
Определите ◊ic{eq?} для точечных пар с~помощью ◊ic{set-car!} или~◊ic{set-cdr!}.
◊end{exercise}

◊begin{exercise}◊label{assignment/ex:form-or}
◊indexR{поиск с возвратом}
◊indexC{or}
Определите новую специальную форму ◊ic{(or ◊${◊alpha} ◊${◊beta})}, которая
возвращает значение~◊${◊alpha}, если оно приводится к~истине; иначе ◊ic{or}
откатывает все побочные эффекты вычисления~◊${◊alpha} и возвращает
значение~◊${◊beta}.
◊end{exercise}

◊begin{exercise}◊label{assignment/ex:previous-value}
Присваивание в~текущем варианте возвращает только что присвоенное значение.
Перепишите ◊ic{set!} так, чтобы она возвращала значение переменной до
присваивания.
◊end{exercise}

◊begin{exercise}◊label{assignment/ex:apply/cc}
Определите функции ◊ic{apply} и ◊ic{call/cc} для интерпретатора из этой главы.
◊end{exercise}

◊begin{exercise}◊label{assignment/ex:dotted}
Встройте в~интерпретатор поддержку функций переменной арности (с~точечным
аргументом).
◊end{exercise}
