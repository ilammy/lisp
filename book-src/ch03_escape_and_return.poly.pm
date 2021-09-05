% -*- coding: utf-8 -*-

◊subsection{◊texorpdfstring%
{Реализация ◊protect◊ic{unwind-protect}}%
{Реализация unwind-protect}}%
◊label{escape/implementation/ssect:unwind-protect}

◊indexCS{unwind-protect}{реализация}
Форма ◊ic{unwind-protect} является самой сложной для реализации; нам понадобится
изменить определения форм ◊ic{catch} и ◊ic{block}, чтобы они вели себя
правильно, когда находятся внутри ◊ic{unwind-protect}.
Это хороший пример
возможности, чьё введение требует переработки всего, что уже написано до этого.
Но отсутствие ◊ic{unwind-protect} приводит к~другим сложностям в~будущем, так
что оно того стоит.

Начнём с~определения поведения самой формы ◊ic{unwind-protect} (которая, как мы
уже говорили, мало чем отличается от~◊ic{prog1}):

◊indexC{unwind-protect-cont}
◊indexC{protect-return-cont}
◊indexC{evaluate-unwind-protect}
◊indexCS{resume}{◊ic{unwind-protect-cont}}
◊indexCS{resume}{◊ic{protect-return-cont}}
◊code:lisp{
(define-class unwind-protect-cont continuation (cleanup r))
(define-class protect-return-cont continuation (value))

(define (evaluate-unwind-protect form cleanup r k)
  (evaluate form r
            (make-unwind-protect-cont k cleanup r) ) )

(define-method (resume (k unwind-protect-cont) v)
  (evaluate-begin (unwind-protect-cont-cleanup k)
                  (unwind-protect-cont-r k)
                  (make-protect-return-cont
                   (unwind-protect-cont-k k) v ) ) )

(define-method (resume (k protect-return-cont) v)
  (resume (protect-return-cont-k k) (protect-return-cont-value k)) )
}

Далее необходимо доработать ◊ic{catch} и ◊ic{block}, чтобы они выполняли
действия, предписанные ◊ic{unwind-protect}, даже в~случае выхода из них
с~помощью ◊ic{throw} или ◊ic{return-from}.
Для ◊ic{catch} необходимо изменить
обработку ◊ic{throwing-cont}:

◊indexCS{resume}{◊ic{throwing-cont}}
◊code:lisp{
(define-method (resume (k throwing-cont) v)
  (unwind (throwing-cont-k k) v (throwing-cont-cont k)) )
}

◊noindent
И~научить ◊ic{unwind} выполнять сохранённые действия в~процессе обхода стека:

◊indexC{unwind-cont}
◊indexCS{unwind}{◊ic{unwind-protect-cont}}
◊indexCS{resume}{◊ic{unwind-cont}}
◊code:lisp{
(define-class unwind-cont continuation (value target))

(define-method (unwind (k unwind-protect-cont) v target)
  (evaluate-begin (unwind-protect-cont-cleanup k)
                  (unwind-protect-cont-r k)
                  (make-unwind-cont
                   (unwind-protect-cont-k k) v target ) ) )

(define-method (resume (k unwind-cont) v)
  (unwind (unwind-cont-k k)
          (unwind-cont-value k)
          (unwind-cont-target k) ) )
}

◊indexR{раскрутка стека (unwinding)}
Теперь, чтобы передать значение при переходе, нам недостаточно просто его отдать
нужному продолжению.
Нам необходимо подняться по стеку продолжений с~помощью
◊ic{unwind} (◊term{раскрутить} стек) от текущего до целевого продолжения,
выполняя по пути соответствующую уборку.
Продолжения форм-уборщиков имеют тип
◊ic{unwind-cont}.
Их обработка с~помощью ◊ic{resume} вызывает продолжение уборки
до достижения цели на~случай вложенных форм ◊ic{unwind-protect}, а также
устанавливает правильное продолжение на случай переходов внутри самих
форм-уборщиков (тот самый процесс отбрасывания продолжений, который
рассматривался на странице~◊pageref{escape/forms/protection/p:discard}).

Что касается ◊ic{block}, то тут даже делать ничего не~надо.
Как вы помните,
◊ic{block-lookup} уже вызывает ◊ic{unwind} для раскрутки стека с~целью проверки
актуальности перехода:

◊code:lisp{
(define-method (block-lookup (r block-env) n k v)
  (if (eq? n (block-env-name r))
      (unwind k v (block-env-cont r))
      (block-lookup (block-env-others r) n k v) ) )
}

◊noindent
Так что остаётся только сказать спасибо обобщённым функциям.

◊indexCS{block}{и~◊ic{unwind-protect}}
Может показаться, что с~появлением ◊ic{unwind-protect} форма ◊ic{block}
перестала быть быстрее ◊ic{catch}, ведь они обе вынуждены пользоваться медленной
◊ic{unwind}.
В~общем случае, конечно, да, но в~частностях, коих большинство, это
не~так: ◊ic{unwind-protect} является специальной формой, так что она не~может
быть спутана с~обычной функцией, её всегда надо использовать явно.
А~если
◊ic{return-from} прямо видит метку соответствующего~◊ic{block} (то~есть когда
между ними нет ◊ic{lambda}- или ◊ic{unwind-protect}-форм), то ◊ic{unwind} будет
работать так~же быстро, как и раньше.

◊bigskip

◊indexCS{unwind-protect}{ограничения ◊CommonLisp}
В~{◊CommonLisp} (CLtL2~◊cite{ste90}) присутствует ещё одно интересное
ограничение, касающееся переходов из форм-уборщиков.
Эти переходы не~могут вести
внутрь той формы, из которой в~теле ◊ic{unwind-protect} был вызван выход.
Введено такое ограничение с~целью недопущения бесконечных циклов из переходов,
любые попытки выбраться из которых пресекаются ◊ic{unwind-protect}.
◊seeEx[escape/ex:eternal] Следовательно, следующая программа выдаст ошибку, так
как форма-уборщик хочет прыгнуть ближе, чем прыжок на~◊ic{1}, который уже
в~процессе.

◊code:lisp{
(catch 1                  |◊dialect{◊CommonLisp}|
  (catch 2
    (unwind-protect (throw 1 'foo)
      (throw 2 'bar) ) ) )         |◊is| |◊ii{ошибка!}|
}


◊section{◊texorpdfstring%
{Сравнение ◊protect◊ic{call/cc}~и~◊protect◊ic{catch}}%
{Сравнение call/cc и catch}}%
◊label{escape/sect:comparing}

Благодаря объектам, продолжения можно представлять связным списком блоков.
Некоторые из этих блоков доступны прямо в~лексическом окружении; до других
необходимо пробираться, проходя через несколько промежуточных продолжений;
третьи вызывают выполнение определённых действий, когда через них проходят.

◊indexR{продолжения (continuations)!время жизни!динамическое}
В~языках вроде Лиспа, где есть продолжения с~динамическим временем жизни, стек
вызовов и продолжения являются синонимами.
Когда мы пишем ◊ic{(evaluate ec r
(make-if-cont k et ef r))}, мы явно кладём в~стек блок кода, который будет
обрабатывать значение, которое вернёт условие ◊ic{if}-формы.
И~наоборот, когда
мы пишем ◊ic{(evaluate-begin (cdr (begin-cont-e*~k)) (begin-cont-r~k)
(begin-cont-k~k))}, то это значит, что текущий блок~◊ic{k} надо выбросить и
поставить на его место ◊ic{(begin-cont-k k)}.
Можно легко убедиться в~том, что
такие блоки действительно выбрасываются, в~стеке не~остаются недовыполненные
куски продолжений.
Таким образом, когда мы выходим из блока, все продолжения,
указывающие на него и, возможно, сохранённые в~других блоках, становятся
недействительными.
Обычно продолжения неявно хранятся в~стеке или даже
в~нескольких стеках, согласованных между собой, а переходы между ними
компилируются в~примитивы языка~Си: ◊ic{setjmp}/◊ic{longjmp}.
◊seePage[cc/sect:call/cc]

◊indexC{let/cc}
В~диалекте {◊EuLisp}~◊cite{pe92} есть специальная форма ◊ic{let/cc} со~следующим
синтаксисом:

◊code:lisp{
(let/cc |◊ii{переменная}| |◊ii{формы}|...)  |◊dialect{◊EuLisp}|
}

◊phantomlabel{escape/comparing/par:bind-exit}
◊indexC{bind-exit}
В~диалекте Dylan~◊cite{app92b} тоже есть подобная форма:

◊code:lisp{
(bind-exit (|◊ii{переменная}|) |◊ii{формы}|...)  |◊dialect{Dylan}|
}

◊noindent
Эта форма связывает текущее продолжение с~◊ii{переменной}, имеющей область
видимости, ограниченную телом ◊ic{let/cc} или~◊ic{bind-exit}.
В~этом случае
продолжение несомненно является полноценным объектом, имеющим интерфейс унарной
функции.
Но его ◊emph{полезное} время жизни динамическое, его можно использовать
лишь во~время вычисления тела формы ◊ic{let/cc} или ◊ic{bind-exit}.
Точнее, само
продолжение, хранящееся в~◊ii{переменной}, имеет неограниченное время жизни, но
становится бесполезным при выходе из связывающей формы.
Это характерная для
{◊EuLisp} и Dylan черта, но её нет как в~Scheme (где продолжения истинно
неограниченны), так и в~{◊CommonLisp} (где они вообще объекты второго класса).
Тем не~менее, такое поведение можно проэмулировать в~Scheme:

◊code:lisp{
(define-syntax let/cc
  (syntax-rules ()
    ((let/cc variable . body)
     (block variable
       (let ((variable (lambda (x) (return-from variable x))))
         . body ) ) ) ) )
}

◊indexR{продолжения (continuations)!варианты представления}
В~мире Scheme продолжения больше нельзя считать неявной частью стека, так как
они могут храниться во~внешних структурах данных.
Поэтому приходится применять
другую модель: древовидную, которую иногда называют ◊term{стек-кактус} или
◊term{спагетти-стек}.
Наиболее простой способ её реализовать: вообще
не~пользоваться аппаратным стеком, размещая все фреймы в~куче.
◊; кадры, не фреймы -- кадры, везде

Такой подход унифицирует выделение памяти под структуры данных и, по
мнению~◊cite{as94}, облегчает портирование.
Тем не~менее, он приводит
к~фрагментации, что вынуждает явно хранить ссылки между продолжениями.
(Хотя
в~◊cite{mb93} приведено несколько вариантов решения этих проблем.) Как правило,
ради эффективности в~аппаратный стек стараются поместить максимум данных о~ходе
исполнения программы, так что каноническая реализация ◊ic{call/cc} делает снимки
стека и сохраняет в~куче именно их; таким образом, продолжения~— это как раз
такие снимки стека.
Конечно, существуют и другие варианты реализации,
рассмотренные, например, в~◊cite{cho88, hdb90}, где используются разделяемые
копии, отложенное копирование, частичное копирование {◊itd} Естественно,
каждый из этих вариантов даёт свои преимущества, но за определённую плату.

Форма ◊ic{call/cc} больше похожа на ◊ic{block}, нежели на~◊ic{catch}.
Оба типа
продолжений имеют лексическую область видимости, они различаются только временем
жизни.
В~некоторых диалектах, вроде~◊cite{im89}, есть урезанный вариант
◊ic{call/cc}.
Называется он ◊ic{call/ep} (от ◊term{call with exit procedure});
эта ◊emph{процедура выхода} хорошо видна в~◊ic{block}/◊ic{return-from}, равно
как и в~◊ic{let/cc}.
Интерфейс у~◊ic{call/ep} такой~же, как и~у~◊ic{call/cc}:

◊indexC{call/ep}
◊code:lisp{
(call/ep (lambda (exit) ...))
}

◊indexR{объекты!второго класса}
Переменная ◊ic{exit} унарной функции-аргумента связывается с~продолжением формы
◊ic{call/ep} на время вычисления тела этой функции.
Схожесть с~◊ic{block}
налицо, разве что мы используем обычное окружение переменных, а не~отдельное
окружение лексических меток.
Основное их отличие в~том, что ◊ic{call/ep} делает
продолжение полноценным объектом, который можно использовать так~же, как любой
другой объект вроде чисел, замыканий или списков.
Имея ◊ic{block}, мы тоже можем
создать функционально аналогичный объект, написав ◊ic{(lambda (x) (return-from
◊ii{метка} x))}.
Но все возможные места выхода из ◊ic{block} известны статически
(это соответствующие формы ◊ic{return-from}), тогда как в~◊ic{call/ep} совсем
по-другому: например, по выражению ◊ic{(call/ep foo)} нельзя понять, может~ли
произойти переход или нет.
Единственный способ это узнать~— проанализировать
◊ic{foo}, но эта функция может быть определена в~совершенно другом месте, а то и
вовсе генерироваться динамически.
Следовательно, функция ◊ic{call/ep} более
сложна для компилятора, чем специальная форма ◊ic{block}, но вместе с~тем имеет
и~больше возможностей.

Продолжая сравнивать ◊ic{call/ep} и~◊ic{block}, мы замечаем больше отличий.
Например, для формы ◊ic{call/ep}, в~которой аргумент записан в~виде явной
◊ic{lambda}-формы, можно не~создавать замыкание.
Следовательно, эффективный
компилятор должен отделять случай ◊ic{(call/ep (lambda~...))} от остальных.
Это
похоже на специальные формы, так как они тоже трактуются по-особенному.
В~Scheme
принято использовать функции как основной инструмент построения абстракций,
тогда как специальные формы являются чем-то вроде подсказок компилятору.
Они
часто одинаково мощны, вопрос лишь в~балансе сложности~— кому важнее
облегчить жизнь: пользователю или, наоборот, разработчику языка.

◊bigskip

Подводя итог, если вам нужна мощь за адекватную цену, то ◊ic{call/cc} к~вашим
услугам, так как она позволяет реализовать все мыслимые управляющие конструкции:
переходы, сопрограммы, частичные продолжения и~так~далее.
Если~же вам нужны
только «нормальные» вещи (а~Лисп уже не~раз показывал, что можно писать
удивительные программы и~без~◊ic{call/cc}), то используйте управляющие формы
{◊CommonLisp}, простые и компилирующиеся в~эффективный машинный~код.


◊section[#:label "escape/sect:pr-cont"]{Продолжения в~программировании}

◊indexE{CPS}
◊indexR{стиль передачи продолжений (CPS)}
◊indexR{продолжения (continuations)|seealso{стиль передачи продолжений (CPS)}}
Существует стиль программирования, называемый «◊term{стилем передачи
продолжений}» (◊english{continuation passing style}, CPS).
В~нём во~главу угла
ставится явное указание не~только того, что возвращать в~качестве результата
функции, но и~кому.
После завершения вычислений функция не~возвращает результат
абстрактному получателю куда-то «наверх», а применяет конкретного получателя,
представленного продолжением, к~результату.
◊; передаёт результат получателю
В~общем, если у~нас есть вычисление
◊ic{(foo (bar))}, то оно выворачивается наизнанку, преобразуясь в~следующий вид:
◊ic{(new-bar foo)}, где ◊ic{foo} и является продолжением, которому ◊ic{new-bar}
передаст результат вычислений.
Давайте рассмотрим данное преобразование
на~примере многострадального факториала.
Пусть мы хотим вычислить~◊${n(n!)}:

◊indexC{fact}
◊code:lisp{
(define (fact n k)
  (if (= n 0) (k 1)
      (fact (- n 1) (lambda (r) (k (* n r)))) ) )

(fact n (lambda (r) (* n r))) |◊is| |◊${n(n!)}|
}

Факториал теперь принимает дополнительный аргумент~◊ic{k}: получателя
вычисленного факториала.
Если результат равен единице, то к~ней просто
применяется~◊ic{k}.
Если~же результат сразу сказать нельзя, то следует ожидаемый
рекурсивный вызов.
Проблема состоит в~том, что хорошо было~бы сначала умножить
факториал~◊${(◊ic{n} - 1)} на~◊ic{n} и только потом уже передавать произведение
получателю, а форма ◊ic{(k (*~n (fact (-~n~1) k)))} делает всё наоборот! Поэтому
и мы всё сделаем шиворот-навыворот: пусть получатель сам умножает результат
на~◊ic{n}.
Настоящий получатель оборачивается в~функцию: ◊ic{(lambda (r) (k
(*~n~r)))}, и передаётся следующему рекурсивному вызову.

Такое определение факториала даёт возможность вычислять различные величины
с~помощью одного и того~же определения.
Например, обычный факториал: ◊ic{(fact
◊ii{n} (lambda (x) x))}, или удвоенный: ◊ic{(fact ◊ii{n} (lambda (x) (*~2~x)))},
или что-то более сложное.


◊subsection[#:label "escape/pr-cont/ssect:multiple"]{Составные значения}

◊indexR{возвращаемые значения!множественные}
◊indexR{множественные значения}
Продолжения очень удобно использовать для обработки составных величин.
Существуют вычисления, результатом которых является не~одна величина, а
несколько.
Например, в~{◊CommonLisp} целочисленное деление (◊ic{truncate})
одновременно возвращает частное и остаток.
Пусть у~нас тоже есть подобная
функция~— назовём её ◊ic{divide}, — которая принимает два числа и
продолжение, вычисляет частное и остаток от деления, а затем применяет
переданное продолжение к~этим величинам.
Например, вот так можно проверить
правильность выполнения деления этой функцией:

◊code:lisp{
(let* ((p (read)) (q (read)))
  (divide p q (lambda (quotient remainder)
                (= p (+ (* quotient q) remainder)) )) )
}

Менее тривиальный пример~— вычисление коэффициентов~Безу.◊footnote*{Фух!
Наконец-то мне удалось опубликовать эту функцию! Она с~1981~года валяется
у~меня без дела.} Соотношение Безу утверждает, что для любых целых чисел ◊${n}
и~◊${p} можно найти такую пару целых ◊${u} и~◊${v}, что ◊${un + vp = ◊NOD(n, p)}.
Для
вычисления коэффициентов ◊${u} и~◊${v} можно использовать расширенный алгоритм
Евклида.

◊indexC{bezout}
◊code:lisp{
(define (bezout n p k)  ; пусть ◊${n > p}
  (divide
   n p (lambda (q r)
         (if (= r 0)
             (k 0 1)    ; т.◊,к.
◊${0 ◊cdot qp + 1 ◊cdot p = p}
             (bezout
              p r (lambda (u v)
                    (k v (- u (* v q))) ) ) ) ) ) )
}

Функция ◊ic{bezout} использует ◊ic{divide}, чтобы сохранить в~◊ic{q} и~◊ic{r}
частное и остаток от деления ◊ic{n} на~◊ic{p}.
Если ◊${n} делится нацело на~◊${p},
то очевидно, что их наибольший общий делитель равен~◊${p} и есть тривиальное
решение: ◊${0}~и~◊${1}.
Если остаток не~равен нулю, то◊textdots◊ попробуйте доказать
правильность этого алгоритма самостоятельно; для этого не~надо быть экспертом
в~теории чисел, достаточно знать свойства~НОД.
А~здесь мы ограничимся простой
проверкой:

◊code:lisp{
(bezout 1991 1960 list) |◊is| (-569 578)
}


◊subsection[#:label "escape/pr-cont/ssect:tail-recusion"]{Хвостовая рекурсия}

В~примере с~вычислением факториала с~помощью продолжений вызов ◊ic{fact} в~конце
концов приводил к~ещё одному вызову ◊ic{fact}.
Если мы проследим за вычислением
◊ic{(fact~3~list)}, то, отбрасывая очевидные шаги, получим следующую картину:

◊code:lisp{
(fact 3 list)
|◊eq| (fact 2 (lambda (r) (k (* n r))))|◊begin{where}
                                        ◊- n {◊is} 3
                                        ◊- k {◊eq} list
                                        ◊end{where}|
|◊eq| (fact 1 (lambda (r) (k (* n r))))|◊begin{where}
                                ◊- n {◊is} 2
                                ◊- k {◊is} (lambda (r) (k (* n r)))◊begin{where}
                                                                  ◊- n {◊is} 3
                                                                  ◊- k {◊eq} list
                                                                  ◊end{where}
                                        ◊end{where}|
|◊eq| (k (* n 1))|◊begin{where}
                  ◊- n {◊is} 2
                  ◊- k {◊is} (lambda (r) (k (* n r)))◊begin{where}
                                                    ◊- n {◊is} 3
                                                    ◊- k {◊eq} list
                                                    ◊end{where}
                  ◊end{where}|
|◊eq| (k (* n 2))|◊begin{where}
                  ◊- n {◊is} 3
                  ◊- k {◊eq} list
                  ◊end{where}|
|◊is| (6)
}

◊indexR{рекурсия!хвостовая}
◊indexR{хвостовые вызовы!рекурсивные}
◊indexR{вызов!хвостовой}
Когда ◊ic{fact} вызывает ◊ic{fact}, вторая функция вычисляется с~тем~же
продолжением, что и первая.
Такое явление называется ◊term{хвостовой рекурсией}
— почему рекурсия, понятно, а хвостовая, потому что этот вызов выполняется
в~«хвосте» вычислений: сразу~же после него следует выход из функции.
Хвостовая
рекурсия~— это частный случай хвостового вызова.
Хвостовой вызов происходит
тогда, когда текущее вычисление может быть полностью заменено вызываемым.
То~есть вызов происходит из ◊term{хвостовой позиции}, если он выполняется
с~◊emph{неизменным продолжением}.

В~примере с~вычислением коэффициентов Безу функция ◊ic{bezout} вызывает
◊ic{divide} из хвостовой позиции.
Функция ◊ic{divide} вызывает своё продолжение
из хвостовой позиции.
Это продолжение рекурсивно вызывает ◊ic{bezout} опять-таки
из хвостовой позиции.

Но в~классическом факториале ◊ic{(*~n (fact (-~n~1)))} рекурсивный вызов
◊ic{fact} происходит не~из хвостовой позиции.
Он ◊emph{завёрнут} в~продолжение,
так как значение~◊ic{(fact (-~n~1))} ещё ожидается для умножения на~◊ic{n};
вызов тут не~является последней необходимой операцией, всё вычисление нельзя
свести к~нему.

Хвостовые вызовы позволяют отбрасывать ненужные окружения и фреймы стека, так
как при таких вызовах они больше никогда не~будут использоваться.
Следовательно,
их можно не~сохранять, экономя таким образом драгоценную стековую память.
Подобные оптимизации были детально изучены французским лисп-сообществом, что
позволило существенно ускорить интерпретацию ◊cite{gre77,cha80,sj87};
см.~также~◊cite{han90}.

◊bigskip

◊indexCS{evaluate-begin}{хвостовая рекурсия}
Оптимизация хвостовой рекурсии~— это очень желанное свойство интерпретатора;
не~только для пользователя, но и для самого интерпретатора.
Самое очевидное
место, где она была~бы полезной,~— это форма ◊ic{begin}.
До сих пор она
определялась следующим образом:

◊code:lisp{
(define (evaluate-begin e* r k)
  (if (pair? e)
      (if (pair? (cdr e*))
          (evaluate (car e*) r (make-begin-cont k e* r))
          (evaluate (car e*) r k) )
      (resume k empty-begin-value) ) )

(define-method (resume (k begin-cont) v)
  (evaluate-begin (cdr (begin-cont-e* k))
                  (begin-cont-r k)
                  (begin-cont-k k) ) )
}

Заметьте, здесь каждый вызов является хвостовым.
Также здесь используется одна
небольшая оптимизация.
Можно определить эту форму проще:

◊code:lisp{
(define (evaluate-begin e* r k)
  (if (pair? e*)
      (evaluate (car e*) r (make-begin-cont k e* r))
      (resume k empty-begin-value) ) )

(define-method (resume (k begin-cont) v)
  (let ((e* (cdr (begin-cond-e* k))))
    (if (pair? e*)
        (evaluate-begin e* (begin-cont-r k) (begin-cont-k k))
        (resume (begin-cont-k k) v) ) ) )
}

Но первый вариант предпочтительнее, так как в~этом случае при вычислении
последнего оставшегося выражения мы не~тратим время на создание лишнего
продолжения ◊ic{(make-begin-cont k e* r)}, которое фактически равно~◊ic{k}, а
сразу~же переходим в~нужное продолжение.
Конечно, в~Лиспе есть сборка мусора, но
это не~означает, что можно мусорить ненужными объектами на каждом шагу.
Это
небольшая, но важная оптимизация, ведь каждый ◊ic{begin} когда-нибудь
заканчивается!

◊indexCS{evaluate-arguments}{хвостовая рекурсия}
Аналогично можно оптимизировать и вычисление аргументов функции, переписав его
следующим образом:

◊code:lisp{
(define-class no-more-argument-cont continuation ())

(define (evaluate-arguments e* r k)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (evaluate (car e*) r (make-argument-cont k e* r))
          (evaluate (car e*) r (make-no-more-argument-cont k)) )
      (resume k no-more-arguments) ) )

(define-method (resume (k make-no-more-argument-cont) v)
  (resume (no-more-argument-cont-k k) (list v)) )
}

Это новое продолжение, хранящее список из последнего вычисленного значения,
избавляет нас от необходимости передавать окружение~◊ic{r} целиком.
Данный приём
впервые использован Митчеллом~Уондом и Дэниелом~Фридманом в~◊cite{wan80b}.


◊section[#:label "escape/sect:partial"]{Частичные продолжения}

◊indexR{продолжения (continuations)!частичные продолжения}
Среди прочих вопросов, поднимаемых продолжениями, есть ещё один довольно
интересный: что именно случается с~отбрасываемым при переходе кодом? Другими
словами, с~тем куском продолжения (или~стека), который находится между
положениями до прыжка и после.
Мы говорили, что такой ◊term{срез} стека
не~сохраняется при переходе.
Но он вовсе не~является бесполезным: ведь если~бы
через него не~перешагнули, то он~бы принял какое-то значение, выполнил
определённые действия и передал~бы полученное значение своему продолжению.
То~есть вёл~бы себя как обычная функция.
Во~многих работах, вроде
◊cite{ffdm87,ff87,fel88,df90,hd90,qs91,mq94}, приводятся способы сохранения и
приёмы использования этих срезов~— ◊term{частичных продолжений}
(◊english{partial/delimited continuations}).

Рассмотрим следующий простой пример:

◊code:lisp{
(+ 1 (call/cc (lambda (k) (set! foo k) 2))) |◊is| 3
(foo 3)                                     |◊is| 4
}

◊noindent
Какое именно продолжение хранится в~◊ic{foo}? Казалось~бы ◊${◊lambda u . 1 + u},
но чему тогда равно ◊ic{(foo~(foo~4))}?

◊code:lisp{
(foo (foo 4))                               |◊is| 5
}

◊indexR{композициональность!продолжений}
◊indexR{продолжения (continuations)!композициональность}
Получается~◊ic{5}, а не~ожидаемое значение~◊ic{6}, которое~бы получилось при
правильной композиции функций.
Дело в~том, что вызов продолжения означает
отбрасывание всех последующих вычислений ради продолжения других вычислений.
Таким образом, вызов продолжения внутри ◊ic{foo} приводит к~вычислению значения
$◊lambda u.
1 + u◊${ при }u = 4$, которое становится значением всего выражения, и
второй вызов ◊ic{foo} вообще не~происходит~— он не~нужен, ведь значение
выражения уже вычислено и передано продолжению! Именно в~этом проблема: мы
захватили обычное продолжение, а не~частичное.
Обычные продолжения
◊term{активируются} и полностью заменяют стек собой, а ◊emph{не~вызываются}
как функции.

Возможно, так будет понятнее.
В~◊ic{foo} мы сохранили ◊ic{(+~1~[])}.
Это всё,
что ещё осталось вычислить.
Так как аргументы передаются по значению, то
вычисление аргумента-продолжения в~◊ic{(foo (foo 4))} фактически завершает
вычисления, отбрасывает ◊ic{(foo~[])} и возвращает значение формы ◊ic{(+~1~4)},
которое, очевидно, равно~◊ic{5}.

◊indexR{продолжения (continuations)!и интерактивная сессия}
◊indexR{интерактивная сессия (REPL)!продолжения}
◊indexE{REPL!продолжения}
Частичные продолжения представляют собой лишь часть оставшихся вычислений, тогда
как обычные продолжения~— это ◊emph{все} оставшиеся вычисления.
В~статьях ◊cite{fwfd88,df90,hd90,qs91} приводятся способы захвата частичных и,
следовательно, поддающихся композиции продолжений.
Предположим, теперь
с~◊ic{foo} связано продолжение ◊ic{[(+~1~[])]}, где внешние квадратные скобки
означают, что оно ведёт себя как функция.
Тогда ◊ic{(foo (foo~4))} будет
эквивалентно уже ◊ic{(foo [(+~1~[4])])}, что превращается в~◊ic{(+~1~5)},
которое в~итоге даёт~◊ic{6}.
Захваченное продолжение ◊ic{[(+~1~[])]} определяет
не~все последующие вычисления, которые когда-либо произойдут, а только их часть
вплоть до момента возврата значения.
Для интерактивной сессии продолжением
обычных продолжений является ◊term{главный цикл} (он~же ◊ic{toplevel}), именно
ему продолжения передают своё значение, а он выводит его на экран, читает
следующее выражение из входного потока, вычисляет его и~так~далее.
Продолжение
частичных продолжений неизвестно, именно поэтому они конечны и ведут себя как
обычные функции~— ведь функции тоже не~знают, кому они вернут значение.

Давайте взглянем на наш пример с~◊ic{(set! foo~k)} с~другой стороны.
Оставим всё
по-прежнему, но объединим эти два выражения в~явную последовательность:

◊code:lisp{
(begin (+ 1 (call/cc (lambda (k) (set! foo k) 2)))
       (foo 3) )
}

Бабах! Мы получили бесконечный цикл, так как ◊ic{foo} оказывается теперь
связанной с~◊ic{(begin (+~1~[]) (foo~3))}, что приводит к~рекурсии.
Как видим,
главный цикл~— это не~только последовательное вычисление выражений.
Если мы
хотим правильно его проэмулировать, то вдобавок необходимо изменять продолжение
каждого вычисляемого в~главном цикле выражения:

◊code:lisp{
(let (foo sequel print?)
  (define-syntax toplevel
    (syntax-rules ()
      ((toplevel e) (toplevel-eval (lambda () e))) ) )
  (define (toplevel-eval thunk)
    (call/cc (lambda (k)
               (set! print? #t)
               (set! sequel k)
               (let ((v (thunk)))
                 (when print? (display v) (set! print? #f))
                 (sequel v) ) )) )
  (toplevel (+ 1 (call/cc (lambda (k) (set! foo k) 2))))
  (toplevel (foo 3))
  (toplevel (foo (foo 4))) )
}

Каждый раз, когда мы хотим вычислить выражение с~помощью ◊ic{toplevel}, его
продолжение~— ◊emph{продолжение} работы ◊ic{toplevel} — сохраняется
в~переменной ◊ic{sequel}.
Любое продолжение, захватываемое внутри ◊ic{thunk},
теперь будет ограничено текущей вычисляемой формой.
Аналогичным образом применяя
присваивание, можно сохранить любой срез стека в~виде частичного продолжения.
Как видим, все продолжения с~неограниченным временем жизни для своего создания
требуют побочных эффектов.

◊indexR{присваивание!роль для продолжений}
Частичные продолжения явно указывают, когда необходимо остановить вычисления.
Этот эффект может быть полезен в~некоторых случаях, а также интересен сам по
себе.
Мы вполне можем даже переписать нашу ◊ic{call/cc} так, чтобы она
захватывала именно частичные продолжения вплоть до ◊ic{toplevel}.
Естественно,
кроме них потребуются также и переходы на тот случай, когда мы действительно
не~заинтересованы в~сохранении срезов стека.
Но, с~другой стороны, частичные
продолжения в~реальности используются довольно редко; сложно привести пример
программы, где частичные продолжения были~бы действительно полезны, но при этом
не~усложняли~бы её сильнее обычных.
Тем не~менее, они важны как ещё один пример
управляющей формы, которую можно реализовать на~Scheme с~помощью ◊ic{call/cc}
и~присваивания.


◊section[#:label "escape/sect:conclusions"]{Заключение}

Продолжения вездесущи.
Если вы понимаете продолжения, вы одновременно овладели
ещё одним стилем программирования, получили широчайшие возможности управления
ходом вычислений и знаете, во~что вам обойдётся это управление.
Продолжения
тесно связаны с~потоком исполнения, так как они динамически определяют всё, что
ещё осталось сделать.
Поэтому они так важны и полезны для обработки исключений.

Интерпретатор, определённый в~этой главе, довольно мощный, но легко понятный
только по частям.
Это обычное дело для объектно-ориентированного стиля: есть
много маленьких и простых кусочков, но не~так просто составить понимание цельной
картины того, как они работают вместе.
Интерпретатор модульный и легко
расширяется новыми возможностями.
Он не~особо быстрый, так как в~процессе работы
создаёт целую гору объектов, которые удаляются тут~же после использования.
Конечно, это является одной из задач компилятора: выяснить, какие из объектов
действительно стоит создавать и сохранять.


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
