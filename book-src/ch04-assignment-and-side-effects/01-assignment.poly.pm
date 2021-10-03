#lang pollen

◊section[#:label "assignment/sect:assignment"]{Присваивание}

Как сказано ранее, присваивание позволяет изменять значения переменных.
Например, давайте напишем программу для определения минимального и максимального элемента в~двоичном дереве,
используя две переменные для хранения наименьшего и наибольшего элемента из уже просмотренных.
Получится~что-то~такое:

◊indexC{min-max}
◊code:lisp{
(define (min-max tree)
  (define (first-number tree)
    (if (pair? tree)
        (first-number (car tree))
        tree ) )
  (let* ((min (first-number tree))
         (max min) )
    (define (scan! tree)
      (cond ((pair? tree)
             (scan! (car tree))
             (scan! (cdr tree)) )
            (else (if (> tree max) (set! max tree)
                      (if (< tree min) (set! min tree)) )) ) )
    (scan! tree)
    (list min max) ) )
}

◊indexR{побочные эффекты!доброкачественные}
Функция ◊ic{min-max} легко читается и очень экономно использует ресурсы компьютера.
Алгоритм не~особо отличается от того, который~бы получился на~Паскале,
и~использует переменные точно так~же, как они используются в~императивных языках.
И~что особо важно, побочные эффекты изменения значений этих локальных переменных не~видны снаружи функции ◊ic{min-max},
то~есть присваивание не~распространяет метастазы в~остальные части программы.
Это пример доброкачественных побочных эффектов:
программа становится более понятной и эффективной, чем написанная в~чисто функциональном~стиле.
◊seeEx{assignment/ex:pure-min-max}

Присваивание локальным переменным, которые не~используются совместно несколькими функциями, не~вызывает существенных проблем.
Например, следующая функция последовательно возвращает натуральные числа, начиная с~нуля.
Очевидно, что значением переменной~◊ic{n} сразу после присваивания является только что присвоенное значение.

◊code:lisp{
(define enumerate
  (let ((n -1))
    (lambda () (set! n (+ n 1))
               n ) ) )
}

◊indexR{присваивание!и семантика подстановки}
◊indexR{семантика!подстановки}
Каждый вызов ◊ic{enumerate} возвращает следующее натуральное число.
Функция ◊ic{enumerate} имеет внутреннее состояние — число~◊ic{n}, — которое изменяется с~каждым вызовом функции.
Как~это вообще работает?
Ведь в~◊${\lambda}-исчислении нет никакого присваивания.
Когда мы применяем функцию в~математике (и,~соответственно, в~◊${\lambda}-исчислении),
то~просто заменяем в~её теле все переменные соответствующими значениями аргументов.
Из-за присваивания подобная семантика подстановки становится некорректной,
а~программы теряют ссылочную прозрачность.

Если подставить вместе переменной~◊ic{n} её начальное значение в~◊ic{enumerate},
то~эта функция возвращала~бы не~натуральные числа, а~исключительно~◊ic{-1}.
Таким образом, присваивание вынуждает нас отказаться от модели вычислений с~прямой подстановкой значений.
Теперь подстановка является отложенной и выполняется только тогда, когда мы явно потребуем значение переменной.
Соответственно, определённым образом расставленные присваивания могут изменить это значение до~подстановки.

Рассмотрим ещё один пример:

◊code:lisp{
(let ((name "Nemo"))
  (set! winner (lambda () name))
  (set! set-winner! (lambda (new-name) (set! name new-name)
                                       name ))
  (set-winner! "Me")
  (winner) )
}

◊indexR{присваивание!и замыкания}
Кто~же победит: ◊ic{"Nemo"} или~◊ic{"Me"}?
Другими словами, влияет~ли присваивание внутри~◊ic{set-winner!} на~значение, возвращаемое~◊ic{winner}?

И~снова ◊${\lambda}-исчисление оказывается бесполезным; оно и~понятно, там нет присваивания.
Ладно, мы вроде~бы говорили, что функция захватывает окружение своего определения.
Значит, функции ◊ic{winner} и ◊ic{set!-winner} по~крайней мере знают, что на~момент их создания переменная ◊ic{name} имела значение~◊ic{"Nemo"}.
Также кажется очевидным, что ◊nobr{◊ic{(set-winner! "Me")}} вернёт ◊ic{"Me"},
так как там чёрным по~белому написано, что мы изменяем значение переменной ◊ic{name} на другое и возвращаем её текущее значение.
Проблема в~том, увидит~ли ◊ic{winner} это значение?
Ведь мы изменяем значение одной и~той~же переменной, правда? ...правда?

Буквальное понимание идеи замыкания приводит к~мысли,
что у~каждого замыкания хранятся личные копии свободных переменных со~значениями, которые они имели на момент создания замыкания.
В~таком случае предыдущая программа эквивалентна следующей:

◊code:lisp{
(let ((name "Nemo"))
  (set! winner (lambda () name))
  (winner) )
}

В~◊cite{sam79} предлагается считать эффекты присваиваний видимыми только тем, кто это присваивание совершил.
Тогда действительно, ◊ic{set-winner!} вернёт новое значение, а~◊ic{winner} всегда будет возвращать~◊ic{"Nemo"}.
В~таком мире не~существует привязок.
Нет, конечно у~переменных есть значения, но~они намертво установлены окружением.
Если кому-то нужно значение переменной, то~оно запрашивается из~окружения.
Если~же это значение требуется изменить, то~просто взамен старого окружения создаётся новое, где переменная имеет нужное значение.

◊indexC{closure}
◊indexR{специальные формы!closure@◊ic{closure}}
◊phantomlabel{assignement/assignement/para:closure}
Такая точка зрения напоминает способ реализации замыканий в~старых, исключительно динамических диалектах~Лиспа.
Тогда замыкания создавались с~помощью специальной формы ◊ic{closure}:
она~принимала первым аргументом список переменных, которые надо сохранить, а~вторым аргументом следовала функция.
В~результате вызов ◊nobr{◊ic{(closure (x) (lambda (y) (+ x y)))}} раскрывался в~◊nobr{◊ic{(lambda (y) (let ((x '◊ii{значение-◊ic{x}})) (+ x y)))}}.
С~одной стороны, значение действительно захватывается,
а~с~другой — всё укладывается в~парадигму подстановки, унаследованную от~◊${\lambda}-исчисления.

Однако такая модель вычислений значительно усложняет как~присваивание, так и~совместное использование переменных.
Есть множество вариантов решения этих затруднений:
в~◊cite{sj87} рекомендуется научить форму ◊ic{closure} самостоятельно искать выделять свободные переменные в~функции.
Также, как предлагается в~◊cite{bcsj86,sj93}, ◊ic{closure} может определённым образом модифицировать найденные присваивания,
чтобы избежать неудобств с~присваиванием свободным переменным.

◊indexR{привязки (bindings)}
◊indexR{объекты!второго класса} ◊; TODO: никакого "второго класса" или "второго сорта" в тексте?
Scheme решает проблему иным путём, вводя понятие ◊term{привязок}, что приводит к~интересным ◊emph{эффектам}.
Форма~◊ic{let} связывает переменную ◊ic{name} со~значением ◊ic{"Nemo"}.
Замыкания, создаваемые в~теле ◊ic{let}, захватывают не~значение переменной ◊ic{name}, а~её~привязку.
Таким образом, ссылка на переменную ◊ic{name} вызывает поиск соответствующей привязки с~последующим извлечением значения из~неё.

Присваивание действует аналогично: сначала ищется привязка, затем изменяется хранимое в~ней значение.
Привязки не~являются полноценными объектами — они~не~существуют отдельно от переменных.
Переменная содержит привязку, а~привязка ссылается на~значение.
Присваивание переменной не~заменяет одну привязку другой, а~заменяет старое значение на~новое внутри привязки.