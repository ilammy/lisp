◊section{Упражнения}◊label{basics/sect:exercises}

◊begin{exercise}◊label{basics/ex:tracer}
◊indexR{трассировка}
Модифицируйте функцию ◊ic{evaluate} так, чтобы она стала трассировщиком. Все
вызовы функций должны выводить на экран фактические аргументы и возвращаемый
результат. Легко представить себе дальнейшее развитие этого инструмента
в~пошаговый отладчик, вдобавок позволяющий изменять порядок выполнения
отлаживаемой программы.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:excess-recursion}
Если функции~◊ic{evlis} передаётся список из одного выражения, она делает
один лишний рекурсивный вызов. Придумайте способ, как избавиться от него.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:new-extend}
Предположим, новая функция~◊ic{extend} определена так:

◊indexC{extend}
◊begin{code:lisp}
(define (extend env names values)
  (cons (cons names values) env) )
◊end{code:lisp}

Определите соответствующие функции ◊ic{lookup} и ◊ic{update!}. Сравните их
с~ранее рассмотренными.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:racks}
◊indexR{ближнее (shallow) связывание}
◊indexR{связывание!ближнее (shallow)}
◊indexE{rack}
В~работе~◊cite{ss80} предлагается другой механизм ближнего связывания, названный
◊term{rack}. Символ связывается с~полем, хранящим не~единственное значение, а
стек значений. В~каждый момент времени значением переменной является находящаяся
на вершине стека величина. Перепишите функции ◊ic{s.make-function},
◊ic{s.lookup} и~◊ic{s.update!} для реализации этой идеи.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:liar-liar!}
◊indexR{представление!логических значений}
Если вы ещё не~заметили, то в~определение функции ◊ic{<} вкралась ошибка! Ведь
эта функция должна возвращать логические значения определяемого языка, а
не~определяющего. Исправьте это досадное недоразумение.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:def-list}
Определите функцию~◊ic{list}.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:def-call/cc}
Для обожающих продолжения: определите ◊ic{call/cc}.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:def-apply}
Определите функцию ◊ic{apply}.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:def-end}
Определите функцию~◊ic{end}, позволяющую выйти из интерпретатора, разработанного
в~этой главе.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:slowpoke}
◊indexR{уровни интерпретации}
◊indexR{интерпретация!уровневая}
Сравните скорость Scheme и ◊ic{evaluate}. Затем сравните скорость ◊ic{evaluate}
и ◊ic{evaluate}, интерпретируемой с~помощью ◊ic{evaluate}.
◊end{exercise}

◊begin{exercise}◊label{basics/ex:no-gensym}
Ранее мы смогли успешно определить ◊ic{begin} через ◊ic{lambda}
◊seePage[basics/forms/sequence/par:gensym-puzzle], но для этого нам
потребовалось использовать функцию ◊ic{gensym}, чтобы избежать коллизий имён
переменных. Переопределите ◊ic{begin} в~таком же духе, но без использования
◊ic{gensym}.
◊end{exercise}


◊section*{Рекомендуемая литература}

Все работы по интерпретаторам, приведённые в~начале этой главы, являются
довольно интересными, но если вы не~можете столько читать, то вот наиболее
стоящие из них:
◊begin{itemize}
  ◊item среди «$◊lambda$"~papers»:~◊cite{ss78a};

  ◊item самая короткая в~мире статья, которая содержит полный интерпретатор
        Лиспа:~◊cite{mcc78b};

  ◊item «нестрого формальное» описание интерпретации:~◊cite{rey72};

  ◊item местная книга Бытия:~◊cite{mae+62}.
◊end{itemize}
