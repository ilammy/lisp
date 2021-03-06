#lang pollen

◊subsection[#:label "basics/representing-functions/ssect:dynamic-and-lexical-binding"]{Динамическая и~лексическая области~видимости}

Из~всего этого разговора об~окружениях можно сделать два вывода.
Во-первых, теперь нам понятно, что с~окружениями не~всё так просто и понятно.
Любое вычисление всегда производится в~каком-то окружении,
поэтому необходимо эффективно реализовывать их использование.
В~третьей главе рассматриваются более сложные вещи вроде раскрутки стека и соответствующей формы~◊ic{unwind-protect},
которые потребуют от нас ещё более точного контроля над окружениями.

◊indexR{лексическое связывание}
◊indexR{динамическое связывание}
◊indexR{связывание!лексическое}
◊indexR{связывание!динамическое}
◊indexR{Лисп!лексический}
◊indexR{Лисп!динамический}
Второй момент связан с~двумя рассмотренными в~предыдущем разделе вариантами,
которые являются примерами ◊term{лексического} и ◊term{динамического связывания}
◊footnote{
  В~объектно-ориентированных языках под динамическим связыванием обычно понимается
  механизм выбора метода объекта на основе его реального типа во~время исполнения программы,
  в~противоположность статическому связыванию,
  при котором метод выбирается компилятором исходя из типа переменной,
  хранящей рассматриваемый объект.
}
(также применяются термины лексическая и динамическая область видимости).
В~лексическом Лиспе функция выполняется в~окружении своего определения, расширенном собственными переменными,
тогда как в~динамическом — расширяет текущее окружение, окружение своего вызова.

Сейчас в~моде лексическое связывание, но это не~значит, что динамическое связывание не~нужно и у~него нет~будущего.
С~одной стороны, именно динамическое связывание применяется в~некоторых довольно популярных языках
вроде ◊|TeX|, ◊seeCite{knu84}, Emacs~Lisp, ◊seeCite{llst93} Perl. ◊seeCite{ws91}
◊trnote{Начиная с~Emacs~Lisp~24 и Perl~5, эти языки имеют и лексические переменные.}

С~другой стороны, сама идея динамической области видимости является важной концепцией программирования.
Порой бывает очень полезно иметь возможность выполнить некоторые действия перед вычислениями
и автоматически, гарантированно вернуть всё как было после завершения этапа работы.

◊indexR{исключения}
◊indexR{поиск с возвратом}
Такой подход можно эффективно применять, например, в~искусственном интеллекте.
В~этом случае сначала выдвигается некая гипотеза, затем из неё вырабатываются следствия.
Как только система натыкается на противоречие, то текущую гипотезу следует отвергнуть и перейти к~следующей.
Такой алгоритм называется ◊term{поиском с~возвратом}.
Если следствия гипотез сохраняются без использования побочных эффектов
— в~так называемых ◊term{персистентных} структурах данных, например, в~А-списках —
то отвержение гипотезы автоматически и без проблем утилизирует и все её следствия.
Но~если в~программе используются глобальные переменные, массивы, и~т.~д.,
то~за теперь ненужной гипотезой приходится долго убирать,
вспоминая, каким~же было состояние памяти в~момент формулировки гипотезы
и какие его части можно откатить до старых значений, чтобы ничего не~сломать!
Динамическая область видимости позволяет гарантировать существование переменной с~определённым значением
на~время и только во~время вычислений, независимо от того, будут они успешны или нет.
Это свойство также широко используется при обработке исключений.

◊indexR{область видимости}
◊term{Область видимости} переменной — это, можно сказать, географическое понятие в~программе:
местность, где переменная встречается и где её можно использовать.
В~чистом Scheme (не~обременённом приятными излишествами вроде~◊ic{let})
есть только одна связывающая форма: ◊ic{lambda}.
Это единственная форма, вводящая новые переменные и предоставляющая им область видимости в~рамках определяемой функции.
В~динамическом~же Лиспе область видимости в~принципе не~может быть ограничена функцией.
Рассмотрим следующий пример:

◊code:lisp{
(define (foo x) (list x y))
(define (bar y) (foo 1991))
}

В~лексическом Лиспе переменная~◊ic{y} внутри~◊ic{foo}
◊footnote{О~происхождении ◊emph{foo} см.~◊cite{ray91}.}
— это всегда ссылка на глобальную переменную~◊ic{y},
которая не~имеет никакого отношения к~переменной~◊ic{y} внутри~◊ic{bar}.
В~динамическом~же Лиспе именно переменная~◊ic{y} из~◊ic{bar} будет видна в~◊ic{foo} внутри ◊ic{bar},
потому что в~момент вызова ◊ic{foo} переменная с~именем~◊ic{y} уже находится в~текущем окружении.
Следовательно, если мы дадим глобальной~переменной~◊ic{y} значение~◊ic{0}, то получим следующие результаты:

◊code:lisp{
(define y 0)
(list (bar 100) (foo 3)) ◊(is) ((1991   0) (3 0)) ; в лексическом Лиспе
(list (bar 100) (foo 3)) ◊(is) ((1991 100) (3 0)) ; в динамическом Лиспе
}

◊indexR{свободные переменные!и области видимости}
Заметьте, что в~динамическом Лиспе ◊ic{bar} не~знает наперёд о~том, что ◊ic{foo} будет использовать её локальную переменную~◊ic{y},
а~◊ic{foo} не~знает наперёд о~том, что именно в~◊ic{bar} можно найти значение свободной переменной~◊ic{y}.
Просто ◊ic{bar} при вызове положила в~текущее окружение переменную~◊ic{y},
а~внутренняя функция ◊ic{foo} нашла её в~своём текущем окружении.
Непосредственно перед выходом ◊ic{bar} уберёт свою~◊ic{y} из окружения, и~глобальная переменная~◊ic{y} снова станет видна.

Конечно, если не~использовать свободные переменные, то нет особой разницы между динамической и лексической областями видимости.

Лексическая область видимости так называется оттого,
что читая код функции, можно с~уверенностью отнести каждую используемую в~ней переменную к~одному из двух классов:
либо переменная находится внутри связывающей формы и является локальной,
либо~же это глобальная переменная.
Берём карандаш (или мышку) и ведём его слева направо, снизу вверх до первой связывающей формы.
Исполнять программу не~обязательно, достаточно прочитать её~текст.

Динамическая~же область видимости своим именем обязана концепции ◊term{динамического времени жизни} переменных,
которую мы будем рассматривать позже.
◊seePage{escape/forms/ssect:dynamic}
Динамические свойства программы по~определению проявляются только во~время её~исполнения.

Scheme поддерживает только лексические переменные.
◊CommonLisp поддерживает оба типа с~одинаковым синтаксисом.
Синтаксис ◊EuLisp и ◊ISLisp чётко разделяет эти два типа переменных,
они даже находятся в~отдельных пространствах имён.
◊seePage{lisp1-2-omega/sect:namespaces}

◊indexR{область видимости!конфликт имён}
◊indexR{переменные!сокрытие имён}
◊indexR{сокрытие переменных}
◊indexR{сокрытие переменных|◊seealso{области видимости}}
Область видимости переменной может прерываться.
Такое случается, когда одна переменная ◊term{скрывает} другую из-за того, что обе имеют одинаковое имя.
Лексические области видимости вкладываются друг в~друга,
скрывая переменные с~совпадающими именами из внешних областей.
Этот известный «блокирующий» порядок разрешения конфликтов унаследован от~Алгола~60.

Под влиянием ◊${\lambda}-исчисления, в~честь которого названа специальная форма ◊ic{lambda}, ◊seeCite{per79}
◊LISP-1.0 был сделан динамическим.
Но~вскоре Джон~Маккарти осознал,
что он ожидал получить от следующего выражения ◊ic{(2~3)}, а~не~◊ic{(1~3)}:

◊code:lisp{
(let ((a 1))
  ((let ((a 2))
     (lambda (b) (list a b)) )
   3 ) )
}

◊indexC{function!для замыканий}
◊indexC{lambda!как ключевое слово}
Эта аномалия (не~осмелюсь назвать её ошибкой) была исправлена введением новой специальной формы~◊ic{function},
которая принимает ◊ic{lambda}-форму и создаёт ◊term{замыкание} — функцию, связанную с~окружением, в~котором она определена.
При вызове замыкания вместо текущего окружения расширяется замкнутое внутри окружение определения.
Вместе с~изменениями ◊ic{d.evaluate} и ◊ic{d.invoke},
форма~◊ic{function}
◊footnote{
  Наша имитация не~совсем точна,
  так как существует немало диалектов Лиспа (вроде CLtL1), ◊seeCite{ste84}
  где ◊ic{lambda} — это не~специальный оператор, а только ключевое слово-маркер,
  вроде ◊ic{else} внутри ◊ic{cond} и~◊ic{case}.
  В~таком случае ◊ic{d.evaluate} может вообще не~знать ни~о~какой ◊ic{lambda}.
  Иногда даже накладываются ограничения на положение ◊ic{lambda}-форм,
  которым разрешается находиться только внутри ◊ic{function} и в~определениях функций.
}
выражается следующим~образом:

◊indexC{d.invoke}
◊indexC{d.make-function}
◊indexC{d.make-closure}
◊code:lisp[#:label "basics/repr-func/dyn-and-lex-bind/src:closure-eval"]{
(define (d.evaluate e env)
  (if (atom? e) ...
      (case (car e)
        ...
        ((function)  ; Синтаксис: (function (lambda ◊ii{аргументы} ◊ii{тело}))
         (let* ((f   (cadr e))
                (fun (d.make-function (cadr f) (cddr f) env)) )
           (d.make-closure fun env) ) )
        ((lambda) (d.make-function (cadr e) (cddr e) env))
        (else     (d.invoke (d.evaluate (car e) env)
                            (evlis (cdr e) env)
                            env )) ) ) )

(define (d.invoke fn args env)
  (if (procedure? fn)
      (fn args env)
      (wrong "Not a function" fn) ) )

(define (d.make-function variables body env)
  (lambda (values current.env)
    (eprogn body (extend current.env variables values)) ) )

(define (d.make-closure fun env)
  (lambda (values current.env)
    (fun values env) ) )
}

◊indexR{переменные!специальные}
◊indexC{special}
На~этом история не~заканчивается.
◊ic{function} — это лишь костыль, на который опиралась слегка хромая реализация Лиспа.
С~созданием первых компиляторов стало ясно, что
с~точки зрения производительности у~лексической области видимости есть (ожидаемое при компиляции) преимущество:
анализируя программу, можно заранее сгенерировать машинный код для более-менее прямого доступа к~любой переменной
— вместо того, чтобы динамически, во~время исполнения программы каждый раз отыскивать значения переменных заново.
Компиляторы по умолчанию стали считать все переменные лексическими,
за исключением тех, которые были явно помечены как динамические или, как тогда их называли, ◊term{специальные}.
Выражение ◊ic{(declare (special~◊ii{x}))} является директивой компиляторам ◊LISP-1.5, ◊CommonLisp, Maclisp, и~других,
сообщающей, что переменная~◊ii{x} ведёт себя «особенно».

◊indexR{ссылочная прозрачность}
Эффективность была не~единственной причиной принятия такого решения.
Другой причиной была потеря ◊term{ссылочной прозрачности} (◊english{referential transparency}).
Ссылочная прозрачность — это свойство языка, заключающееся в~том,
что замена в~программе ссылок на выражение непосредственно самим выражением
не~изменяет поведение программы в~целом
(оба варианта либо вернут одно и то~же значение, либо вместе застрянут в~бесконечном цикле).
Например:

◊code:lisp{
(let ((x (lambda () 1))) (x)) ◊(eq) ((lambda () 1)) ◊(eq) 1
}

В~общем случае ссылочная прозрачность теряется, если язык позволяет выражениям иметь побочные эффекты.
Для сохранения ссылочной прозрачности в~присутствии побочных эффектов
необходимо точнее определить понятие эквивалентных выражений.
Scheme обладает ссылочной прозрачностью, если не~использовать присваивания, функции с~побочными эффектами и продолжения.
◊seeEx{escape/ex:crazy-cc}
Это очень полезное свойство языка, положительно влияющее на повторное использование кода,
уменьшающее зависимость функций от контекста их использования.

◊indexR{переменные!безымянные}
◊indexR{альфа-конверсия@◊${\alpha}-конверсия}
Локальные переменные функций вроде ◊ic{(lambda (u) (+~u~u))} иногда называются ◊term{безымянными}.
Их~имена ничего не~значат и могут быть абсолютно произвольными.
Функция ◊ic{(lambda (n347) (+~n347~n347))} — это та~же самая
◊footnote{В~терминах ◊${\lambda}-исчисления подобная замена имён называется ◊${\alpha}-конверсией.}
функция, что и ◊ic{(lambda (u) (+~u~u))}.

Мы ожидаем, что в~языке будет сохраняться этот инвариант.
Но~это невозможно в~динамическом Лиспе.
Рассмотрим следующий пример:

◊indexC{map}
◊code:lisp{
(define (map fn l) ; или mapcar, как кому нравится
  (if (pair? l)
      (cons (fn (car l)) (map fn (cdr l)))
      '() ) )

(let ((l '(a b c)))
  (map (lambda (x) (list-ref l x))
       '(2 1 0)))
}

(Функция~◊ic{(list-ref~◊${\ell}~◊${n})} возвращает ◊${n}-й~элемент списка~◊${\ell}.)

В~Scheme мы~бы получили ◊ic{(c~b~a)}, но в~динамическом Лиспе результатом будет ◊ic{(0~0~0)}!
Присмотритесь к~свободной переменной~◊ic{l} в~функции ◊ic{(lambda (x) (list-ref~l~x))},
которая конфликтует с~локальной переменной~◊ic{l} внутри~◊ic{map}.

Конкретно это затруднение можно решить, просто изменив конфликтующие имена, переименовав какую-нибудь из двух~◊ic{l}.
Например, ту, которая внутри ◊ic{map}, потому что это более разумно.
Но какое имя выбрать, чтобы подобная проблема не~возникла снова?
Если приписывать спереди к~имени каждой переменной номер паспорта программиста, а~сзади — текущее ◊|UNIX|-время,
то это, конечно, значительно снизит вероятность коллизий, но читабельность программ будет оставлять желать лучшего.

В~начале восьмидесятых годов сложилась довольно неприятная ситуация:
студентов учили Лиспу на примере интерпретаторов,
а~компиляторы понимали области видимости совершенно по-другому.
В~1975~году Scheme ◊seeCite{ss75} показал, что интерпретатор и компилятор возможно примирить,
поместив обоих в~мир, где все переменные лексические.
◊CommonLisp забил последний гвоздь в~гроб этого вопроса, постановив, что ◊emph{хорошее} понимание —
это понимание компилятора, а~для него удобнее лексические переменные.
Интерпретаторы были вынуждены подчиниться новым правилам.
Растущий успех Scheme и других функциональных языков, вроде~ML и компании,
популяризовал новый подход сначала в~языках программирования, а затем и в~умах~людей.
