% -*- coding: utf-8 -*-

◊subsection{◊texorpdfstring{Локальная~рекурсия в~◊Lisp1}%
{Локальная рекурсия в Lisp₁}}%
◊label{lisp1-2-omega/recusion/ssect:local-lisp1}

◊indexR{рекурсия!локальная}
Проблема определения локальных рекурсивных функций существует и в~◊Lisp1;
решается она похожим способом.
Форма~◊ic{letrec} (рекурсивная~◊ic{let}) очень
похожа по~смыслу на~◊ic{labels}.

В~Scheme ◊ic{let} имеет следующий синтаксис:

◊begin{code:lisp}
(let ((|◊ii{переменная◊sub{1}}| |◊ii{выражение◊sub{1}}|)
      (|◊ii{переменная◊sub{2}}| |◊ii{выражение◊sub{2}}|)
       ...
      (|◊ii{переменная◊sub{n}}| |◊ii{выражение◊sub{n}}|) )
  |◊ii{выражения}|...
)
◊end{code:lisp}

◊indexC{let}
◊noindent
И~она эквивалентна такому выражению:

◊begin{code:lisp}
((lambda (|◊ii{переменная◊sub{1}}| |◊ii{переменная◊sub{2}}| ...
|◊ii{переменная◊sub{n}}|) |◊ii{выражения}|...)
 |◊ii{выражение◊sub{1}}| |◊ii{выражение◊sub{2}}| ...
|◊ii{выражение◊sub{n}}| )
◊end{code:lisp}

Поясним, что здесь происходит.
Сперва вычисляются все аргументы аппликации:
◊ii{выражение◊sub{1}}, ◊ii{выражение◊sub{2}}, ◊dots, ◊ii{выражение◊sub{n}};
затем переменные ◊ii{перемен◊-ная◊sub{1}}, ◊ii{перемен◊-ная◊sub{2}}, ◊dots,
◊ii{перемен◊-ная◊sub{n}} связываются с~только что полученными значениями;
наконец, ◊ii{выражения}, составляющие тело~◊ic{let}, вычисляются в~расширенном
окружении внутри неявной формы~◊ic{begin}, а её значение становится значением
всей формы~◊ic{let}.

Как видим, в~принципе нет необходимости делать ◊ic{let} специальной формой, так
как её полностью заменяет ◊ic{lambda}; следовательно, ◊ic{let} может быть всего
лишь макросом.
(Именно так и поступили в~Scheme: ◊ic{let} — это встроенный
макрос.) Тем~не~менее, ◊ic{let} хороша с~точки зрения стиля кодирования, потому
что позволяет не~разделять имя переменной и её начальное значение большим куском
кода.
Теперь самое время заметить, что начальные значения локальных переменных
формы ◊ic{let} вычисляются в~текущем окружении; в~расширенном вычисляется только
её тело.

◊indexC{letrec}
По тем~же причинам, с~которыми мы столкнулись в~◊Lisp2, это значительно
усложняет написание взаимно рекурсивных функций.
Поэтому вводится форма
◊ic{letrec}, аналог~◊ic{labels}.

Синтаксис~◊ic{letrec} такой~же, как и у~◊ic{let}.
Например:

◊begin{code:lisp}
(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))) )
  (even? 4) )
◊end{code:lisp}

◊indexCS{letrec}{как макрос}
Отличается ◊ic{letrec} от ◊ic{let} тем, что выражения"=инициализаторы
вычисляются в~том~же окружении, что и тело~◊ic{letrec}.
Операции, которые
выполняет~◊ic{letrec}, те~же, что и у~◊ic{let}, но их порядок несколько иной.
Сначала локальное окружение расширяется переменными ◊ic{letrec}.
Затем в~этом
расширенном окружении вычисляются начальные значения переменных.
Наконец,
в~том~же расширенном окружении вычисляется тело~◊ic{letrec}.
По~этому описанию
довольно легко понять, как реализовать такое поведение.
Действительно,
достаточно написать следующее:

◊indexC{even"?}◊indexC{odd"?}◊indexC*{void}{'void}
◊begin{code:lisp}
(let ((even? 'void) (odd? 'void))
  (set! even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
  (set! odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
  (even? 4) )
◊end{code:lisp}

Сначала создаются привязки для ◊ic{even?} и~◊ic{odd?}.
(Их начальные значения
не~важны, просто ◊ic{let} и ◊ic{lambda} требуют какое"~то значение.) Затем эти
переменные инициализируются значениями, вычисленными в~окружении, где известны
переменные ◊ic{even?} и~◊ic{odd?}.
Мы говорим «известны», потому что хотя для
этих переменных и созданы привязки, их значения не~имеют смысла, так как они
ещё не~были правильно инициализированы.
Про ◊ic{even?} и ◊ic{odd?} известно
достаточно, чтобы ссылаться на них, но пока ещё недостаточно, чтобы они
участвовали в~вычислениях.

◊indexR{порядок вычислений!неопределённый}
Однако, такое преобразование не~совсем корректно из"~за порядка вычислений:
действительно, ◊ic{let} раскрывается в~применение функции, следовательно,
◊ic{letrec}, по~идее, должна вести себя так~же, а это значит, что начальные
значения переменных должны вычисляться как аргументы функции — то~есть
в~неопределённом порядке.
К~сожалению, подобный вариант всегда вычисляет их
слева направо.
◊seeEx[lisp1-2-omega/ex:orderless-letrec]


◊subsubsection{◊texorpdfstring{Уравнения и~◊protect◊ic{letrec}}%
{Уравнения и letrec}}

◊indexCS{letrec}{и~уравнения}
С~формой~◊ic{letrec} есть ещё одна серьёзная проблема: её синтаксис не~является
строгим.
При текущей трактовке ◊ic{letrec} допускает в~качестве инициализаторов
всё что угодно, не~только функции; тогда как ◊ic{labels} в~{◊CommonLisp}
разрешает определять исключительно функции.
То~есть в~Scheme теоретически можно
будет написать следующее:

◊begin{code:lisp}
(letrec ((x (/ (+ x 1) 2))) x)
◊end{code:lisp}

Заметьте, что переменная~◊ic{x} фактически определяется через саму себя.
Это,
похоже, обыкновенное уравнение
%
◊[  x = ◊frac{x + 1}{2}  ◊]
%
Логично будет сделать значением~◊ic{x} корень этого уравнения.
То~есть такое
выражение должно вернуть~◊ic{1}.

Но что делать, если у~уравнения нет корней или если их несколько?

◊begin{code:lisp}
(letrec ((x (+ x 1))) x)            ; $x = x + 1$
(letrec ((x (+ (power x 37) 1))) x) ; $x = x^{37} + 1$
◊end{code:lisp}

Однако, существуют множества, вроде известного вам множества S"=выражений, где
достаточно легко убедиться в~том, что уравнение имеет единственное решение
◊cite{ms80}.
Например, следующим образом можно без побочных эффектов определить
бесконечный список — как корень данного «списочного» уравнения:

◊begin{code:lisp}
(letrec ((foo (cons 'bar foo))) foo)
◊end{code:lisp}

◊indexR{ленивые вычисления}
Значением этого выражения может быть или лениво вычисляемый бесконечный
список ◊ic{(bar bar bar ...)}, как это сделано в~◊cite{fw76,pj87}, так и
закольцованная структура данных (менее дорогая с~вычислительной точки зрения):

◊begin{code:lisp}
(let ((foo (cons 'bar 'wait)))
  (set-cdr! foo foo)
  foo )
◊end{code:lisp}

Эффективно это одно и то~же, но на самом деле нет.
В~общем, из"~за всех этих
неоднозначностей стоит ввести правило, запрещающее использовать переменную,
определяемую ◊ic{letrec}, для определения значения этой~же переменной.
В~двух
предыдущих примерах необходимо было знать значение~◊ic{x} для того чтобы
инициализировать~◊ic{x}.
Теперь они, очевидно, являются ошибочными.
Однако мы
помним, что порядок инициализации в~Scheme должен быть неопределённым, а значит,
некоторые конструкции, допускаемые данным правилом, могут быть ошибочными
в~одних реализациях, но работать в~других.
Рассмотрим следующий пример:

◊begin{code:lisp}
(letrec ((x (+ y 1))
         (y 2) )
  x )
◊end{code:lisp}

Если ◊ic{y} инициализируется до~◊ic{x}, то всё в~порядке.
В~противном случае
возникает ошибка, потому что мы хотим увеличить значение переменной~◊ic{y},
которая уже существует, но ещё не~имеет значения.
Некоторые компиляторы Scheme
и ML анализируют выражения"=инициализаторы и проводят топологическую сортировку
для определения подходящего порядка инициализации.
Естественно, такое решение
тоже не~всегда срабатывает; в~частности, при взаимной зависимости◊footnote{Ведь
◊ic{(42~42)} вполне подходит как корень данного уравнения, но почему
именно~◊ic{42}?} вроде такой:

◊begin{code:lisp}
(letrec ((x y) (y x)) (list x y))
◊end{code:lisp}

Рассмотренные примеры напоминают о~нашей дискуссии вокруг глобального окружения
и семантики ◊ic{define}.
Там возникла похожая проблема: что делать
с~неинициализированными привязками и как узнать о~том, что они вообще
существуют.


◊subsection{Объявление неинициализированных~привязок}%
◊label{lisp1-2-omega/recusion/ssect:uninitialized}

◊indexR{привязки (bindings)!неинициализированные}
Официально семантика Scheme считает ◊ic{letrec} производной формой; то есть
удобным, но отнюдь не~обязательным сокращением.
Соответственно, любую
◊ic{letrec}-форму можно переписать с~помощью примитивных форм Scheme.
Чуть
раньше мы попробовали это сделать, временно связывая переменные ◊ic{letrec}
со~значением ◊ic{void}.
К~сожалению, это тоже инициализация, так что обращения
к~неинициализированным переменным подобным образом отловить нельзя.
Наша
ситуация усугубляется тем, что ни~одна из четырёх специальных форм Scheme
не~позволяет создавать «родные» неинициализированные привязки.

◊indexC*{UFO}{◊#<UFO>}
В~первом приближении можно было~бы решить проблему, используя некий объект
◊ic{◊#<UFO>} ◊seePage[basics/s:env/ufo] вместо ◊ic{void}.
С~ним ничего нельзя
сделать: ни~прибавить к~нему число, ни~взять его~◊ic{car}; однако, это всё~же
полноценный объект, так что его можно передать как аргумент в~◊ic{cons}, а
значит, следующая программа не~будет ошибочной и вернёт ◊ic{◊#<UFO>}:

◊begin{code:lisp}
(letrec ((foo (cons 'foo foo))) (cdr foo))
◊end{code:lisp}

Причина такого поведения в~том, что неинициализированность — это свойство
самой привязки, а не~её значения.
Следовательно, мы не~сможем решить проблему,
используя объекты первого класса.

◊ForLayout{display}{◊medskip}

◊indexC*{uninitialized}{◊#<uninitialized>}
И~всё~же, многие реализации дают неинициализированным переменным специальное
значение.
Давайте назовём его ◊ic{◊#<uninitialized>} и предположим, что это
полноценный объект.
Любая переменная с~таким значением считается
неинициализированной.
Следовательно, используя вместо ◊ic{void} значение
◊ic{◊#<uninitialized>}, мы получаем желаемую возможность обнаружить ошибку.
Однако, эта возможность чересчур явная: ничто не~запрещает передавать
◊ic{◊#<uninitialized>} в~функцию как аргумент, а значит, больше нельзя
предполагать, что все аргументы функции имеют значения.
Мы будем вынуждены
каждый раз проверять, действительно~ли это так:

◊begin{code:lisp}
(define (fact n)
  (if (eq? n '#<uninitialized>)
      (wrong "Uninitialized n")
      (if (= n 0) 1
          (* n (fact (- n 1))) ) ) )
◊end{code:lisp}

Делать так со~всеми переменными — это слишком большая плата за ◊ic{letrec}.
Так что ◊ic{◊#<uninitialized>} нельзя делать полноценным объектом, это должно
быть особое внутреннее значение интерпретатора, которое нельзя использовать
в~программах.
Для того, чтобы им можно было пользоваться безопасно, необходим
специальный синтаксис.

◊ForLayout{display}{◊medskip}

Третий вариант решения состоит во~введении специальной формы, создающей
неинициализированные привязки.
Например, перенесём синтаксис ◊ic{let} из
{◊CommonLisp}, выполняющей данное действие, в~Scheme:

◊begin{code:lisp}
(let (|◊ii{переменная}| ...)
  ...
)
◊end{code:lisp}

Если имя переменной указано само по себе, без начального значения, то привязка
к~этому имени не~будет инициализирована.
Если нам понадобится её значение, то мы
будем вынуждены проверять, была~ли инициализирована данная переменная или нет.
Теперь можно будет написать нормальную реализацию ◊ic{letrec}.
В~следующем коде
переменные ◊ii{temp◊sub{i}} являются «гигиеничными»: им выдаются специальные
имена, гарантированно не~конфликтующие с~именами переменных ◊ic{letrec} или
свободными переменными её тела.

◊ForLayout{display}{◊clearpage}

{◊def◊N#1{◊ii{имя◊sub{#1}}}
◊def◊T#1{◊ii{temp◊sub{#1}}}
◊def◊E#1{◊ii{выражение◊sub{#1}}}
◊def◊Q{◊hbox to 0pt{◊kern0.3em$◊equals$}}
◊begin{code:lisp}
(letrec ((|◊N 1| |◊E 1|)       (let (|◊N 1| ...
|◊N n|)
         ...
                       (let ((|◊T 1| |◊E 1|)
         (|◊N n| |◊E n|) )  |◊Q|           ...
  |◊ii{тело}| )                                  (|◊T n| |◊E n|) )
                                      (set! |◊N 1| |◊T 1|)
                                      ...
                                      (set! |◊N n| |◊T n|)
                                      |◊ii{тело}| ) )
◊end{code:lisp}}

◊indexCS{let}{специальная форма}
Итого, проблема решена с~приемлемой эффективностью: лишь неинициализированные
переменные вызывают накладные расходы, потому что за особенности надо платить.
Но теперь форма~◊ic{let} не~является просто синтаксическим сахаром, теперь это
полноценная специальная форма, которую должен обрабатывать лично интерпретатор.
Добавляем соответствующий код в~◊ic{evaluate}:

◊begin{code:lisp}
...
((let)
 (eprogn (cddr e)
         (extend env
                 (map (lambda (binding)
                        (if (symbol? binding) binding
                            (car binding) ) )
                      (cadr e) )
                 (map (lambda (binding)
                        (if (symbol? binding) the-uninitialized-marker
                            (evaluate (cadr binding) env) ) )
                      (cadr e) ) ) ) ) ...
◊end{code:lisp}

Переменная ◊ic{the-uninitialized-marker} принадлежит языку определения.
Зададим
её, например, так:

◊indexC{the-uninitialized-marker}
◊begin{code:lisp}
(define the-uninitialized-marker (cons 'not 'initialized))
◊end{code:lisp}

Конечно, теперь необходимо встроить поддержку этого внутреннего значения
в~функцию ◊ic{lookup}.
Функция ◊ic{update!} в~изменениях не~нуждается по
очевидным причинам.
Обращения к~◊ic{wrong} отвечают за два различных типа
ошибок: несуществующую привязку и неинициализированную привязку.

◊indexC{lookup}
◊begin{code:lisp}
(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (let ((value (cdar env)))
            (if (eq? value the-uninitialized-marker)
                (wrong "Uninitialized binding" id)
                value ) )
          (lookup id (cdr env)) )
      (wrong "No such binding" id) ) )
◊end{code:lisp}

После блужданий по пустыне~семантики и синтаксиса, у~нас наконец"~то получилась
форма ◊ic{letrec}, позволяющая определять локальные взаимно рекурсивные функции.


◊subsection{Рекурсия без~присваивания}%
◊label{lisp1-2-omega/recusion/ssect:no-assignment}

◊indexR{язык!чисто функциональный}
◊indexR{рекурсия!без присваивания}
Форма ◊ic{letrec}, которую мы рассматривали, использует присваивания для
обеспечения правильного вычисления начальных значений.
Языки, называемые
◊emph{чисто функциональными}, не~имеют в~своём распоряжении операторов
присваивания; в~них принципиально нет побочных эффектов, а чем, как
не~побочным эффектом вычислений, является изменение значения переменной?

В~качестве парадигмы программирования запрет на присваивание имеет свои
преимущества: он гарантирует сохранение ссылочной прозрачности и этим
развязывает руки множеству оптимизаций, позволяя перемещать и распараллеливать
части программ, использовать ленивые вычисления {◊itd} Однако, если нет
возможности использовать присваивания, то некоторые алгоритмы становятся
не~такими простыми, а также несколько усложняется перенос программ на реальные
компьютеры, так как побочные эффекты являются неотъемлемой частью их работы.

◊indexC{letrec}
Первое, что приходит в~голову, это сделать ◊ic{letrec} ещё одной специальной
формой, как это и сделано в~ML и подобных ему языках.
Модифицируем ◊ic{evaluate}
для обработки этого случая:

◊begin{code:lisp}
...
((letrec)
 (let ((new-env (extend env
                        (map car (cadr e))
                        (map (lambda (binding) the-uninitialized-marker)
                             (cadr e) ) )))
      (map (lambda (binding)         ; ◊ic{map} во~имя беспорядка!
             (update! (car binding)
                      new-env
                      (evaluate (cadr binding) new-env) ) )
           (cadr e) )
      (eprogn (cddr e) new-env) ) ) ...
◊end{code:lisp}

В~этом случае побочные эффекты всё равно присутствуют, но на уровне
интерпретатора, внутри ◊ic{update!}; с~точки зрения определяемого языка побочных
эффектов нет.
Стоит заметить, что мы намеренно не~указываем порядок вычислений,
используя ◊ic{map}, которая, в~отличие от~◊ic{for-each}, вольна обрабатывать
список в~любом удобном порядке.◊footnote*{Правда, расплачиваясь за это
необходимостью собирать бесполезный список, который тут~же удаляется после
создания.}


◊subsubsection{◊texorpdfstring%
{◊protect◊ic{letrec} и~полностью~лексическое глобальное~окружение}%
{letrec и полностью лексическое глобальное окружение}}

В~гиперстатическом глобальном окружении переменную можно использовать только
после того, как она была определена.
С~такими ограничениями мы не~можем легко
определять ни~взаимно, ни даже просто рекурсивные функции.
Форма ◊ic{letrec}
решает эту проблему, а заодно служит индикатором рекурсивных определений.

◊begin{code:lisp}
(letrec ((fact (lambda (n)
                 (if (= n 0) 1 (* n (fact (- n 1)))) )))
  (letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
           (even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) )
    ...
) )
◊end{code:lisp}

◊noindent
В~данном случае ◊ic{letrec} создаёт опережающие ссылки на ◊ic{fact}, ◊ic{odd?}
и~◊ic{even?}, так что определения будут работать и в~гиперстатическом окружении.


◊subsubsection{Парадоксальный комбинатор}

◊indexR{комбинаторы!неподвижной точки}
◊indexR{комбинаторы!Y@◊protect◊comb{Y}}
◊indexE{Y@◊protect◊comb{Y}, комбинатор}
◊indexR{теорема о~неподвижной точке}
◊indexR{неподвижная точка}
◊indexR{лямбда-исчисление@$◊lambda$-исчисление!комбинаторы}
Если вы имели дело с~$◊lambda$"=исчислением, то вы наверняка помните, что
такое ◊emph{комбинаторы неподвижной точки} и как записывается самый известный
из них — ◊emph{парадоксальный} или Y-комбинатор.
Функция~$f$ имеет
неподвижную точку, если в~её области определения существует элемент~$x$ такой,
что $f(x) = x$.
Комбинатор~◊comb{Y} принимает любую функцию
$◊lambda$"=исчисления и возвращает её неподвижную точку.
Эта идея выражена
в~одной из наиболее прекрасных и содержательных теорем $◊lambda$"=исчисления:

◊begin{theorem}{Теорема о~неподвижной точке}
$◊exists ◊comb{Y}◊colon ◊forall F◊colon ◊comb{Y} F = F(◊comb{Y} F)$
◊end{theorem}

В~терминах Лиспа, ◊comb{Y} — это значение выражения

◊begin{code:lisp}
(let ((W (lambda (w)
           (lambda (f)
             (f ((w w) f)) ) )))
  (W W) )
◊end{code:lisp}

Доказать это весьма просто.
Если предположить, что ◊comb{Y} равен $(W W)$,
то какой должна быть $W$◊!, чтобы $(W W)F$ равнялось $F ((W W) F)$?
Очевидно, что функция $W$◊! должна быть ничем иным, как $◊lambda W.
◊lambda F.
F ((W W) F)$.
Приведённое выражение лишь записывает эту идею на~Лиспе.

◊indexR{комбинаторы!Z@◊protect◊comb{Z}}
◊indexE{Z@◊protect◊comb{Z}, комбинатор}
◊indexR{эта-конверсия@$◊eta$-конверсия}
Правда, здесь возникает небольшое затруднений из"~за принятой в~Scheme передачи
аргументов по~значению.
Терм ◊ic{((w~w)~f)} не~следует вычислять слишком рано,
поэтому мы вынуждены добавить излишнюю (в~$◊lambda$"~исчислении)
$◊eta$"=конверсию, чтобы избежать проблем.
В~итоге мы приходим к~так называемому
Z-комбинатору, где ◊ic{(lambda (x) (...
x))} означает $◊eta$"=конверсию:

◊ForLayout{display}{◊clearpage}

◊indexC{fix}
◊begin{code:lisp}
(define fix
  (let ((d (lambda (w)
             (lambda (f)
               (f (lambda (x) (((w w) f) x))) ) )))
  (d d) ) )
◊end{code:lisp}

Самое сложное в~этом определении — понять, как оно работает.
Сейчас мы этим
и займёмся.
Определим функцию ◊ic{meta-fact}:

◊indexC{meta-fact}
◊begin{code:lisp}[label=lisp1-2-omega/recursion/y-combinator/code:meta-fact]
(define (meta-fact f)
  (lambda (n)
    (if (= n 0) 1
        (* n (f (- n 1))) ) ) )
◊end{code:lisp}

Эта функция подозрительно похожа на факториал.
Проверив, мы убеждаемся, что
◊ic{(meta-fact fact)} вычисляет факториал с~таким~же успехом, что и ◊ic{fact},
разве что несколько медленнее.
Теперь предположим, что мы знаем неподвижную
точку~$f$ функции ◊ic{meta-fact}: $f = ◊text{◊ic{(meta-fact $f$)}}$.
Эта
неподвижная точка по определению является решением следующего функционального
уравнения относительно~$f$:

◊begin{code:lisp}
|◊ii{f}| = (lambda (n)
      (if (= n 0) 1
          (* n (|◊ii{f}| (- n 1))) ) )
◊end{code:lisp}

Итак, что~же такое~$f$? Не~что иное, как всем известный факториал!

Вообще"~то говоря, нет ни~единого основания полагать, что уравнение выше имеет
решение и что оно единственно.
(Конечно, эти термины надо~бы определить строго
математически, но это выходит за рамки данной книги.) Действительно, есть как
минимум ещё одно решение:

◊indexC{fact}
◊begin{code:lisp}
(define (another-fact n)
  (cond ((< n 1) (- n))
        ((= n 1) 1)
        (else (* n (another-fact (- n 1)))) ) )
◊end{code:lisp}

◊indexR{неподвижная точка!наименьшая}
Проверьте, пожалуйста, что ◊ic{another-fact} также является неподвижной точкой
◊ic{meta-fact}.
Анализируя возможные неподвижные точки, можно прийти к~выводу,
что есть такая область определения, на которой их значения совпадают: все они
вычисляют факториал натуральных чисел.
Их поведение различно только тогда, когда
исходный вариант ◊ic{fact} попадает в~бесконечный цикл.
Для отрицательных целых
чисел ◊ic{another-fact} возвращает одно значение, хотя вполне могла~бы вернуть
какое-нибудь другое, потому что исходное функциональное уравнение не~указывает,%
◊footnote*{Более подробное объяснение см.~в~◊cite{man74}.} что делать в~таком
случае.
Если упорядочить функции по некоторой мере их определённости, то должна
существовать наименьшая неподвижная точка — наименее определённое решение
функционального уравнения.

Математический смысл глобальных рекурсивных определений вроде ◊ic{fact} состоит
в~том, что они определяют функции, являющиеся наименьшими неподвижными точками
соответствующих функциональных уравнений.
Когда мы пишем:

◊begin{code:lisp}
(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1))) ) )
◊end{code:lisp}

◊noindent
то фактически записываем уравнение относительно переменной~◊ic{fact}.
Форма
◊ic{define} решает это уравнение и связывает полученное решение
с~переменной~◊ic{fact}.
Такая трактовка уводит нас далеко от обсуждения
инициализации глобальных переменных
◊seePage[lisp1-2-omega/recusion/ssect:simple]
и превращает ◊ic{define} в~магический решатель уравнений.
В~действительности,
◊ic{define} реализована именно так, как предложено ранее.
Просто рекурсия
в~глобальном окружении вместе с~нормальным порядком вычислений действительно
способны находить наименьшие неподвижные точки.

А~теперь вернёмся к~◊ic{fix}, нашему Z-комбинатору, и проследим, как~же
вычисляется ◊ic{((fix~meta-fact)~3)}.
Помните, что здесь функции не~имеют
побочных эффектов, а значит, результаты вычислений можно свободно подставлять
друг в~друга, чем мы и будем пользоваться.

◊indexC{fix}
◊begin{code:lisp}
((fix meta-fact) 3)
|◊Equals|   (((d d)|◊begin{where}
                    ◊- d {◊eq} ◊begin{complex}
                              ◊-(lambda (w)
                              ◊-  (lambda (f)
                              ◊-   (f (lambda (x)
                              ◊-        (((w w) f) x) )) ) )
                              ◊end{complex}
                    ◊end{where}|
     meta-fact )
    3 )

|◊Equals|   (((lambda (f)               ; ◊term{шаг I}
       (f (lambda (x)
            (((w w) f) x) )) )|◊begin{where}
                               ◊- w {◊eq} ◊begin{complex}
                                         ◊-(lambda (w)
                                         ◊-  (lambda (f)
                                         ◊-   (f (lambda (x)
                                         ◊-        (((w w) f) x) )) ) )
                                         ◊end{complex}
                              ◊end{where}|
     meta-fact )
    3 )

|◊Equals|   ((meta-fact (lambda (x)
                 (((w w) f) x) ))|◊begin{where}
                                  ◊- w {◊eq} ◊begin{complex}
                                            ◊-(lambda (w)
                                            ◊-  (lambda (f)
                                            ◊-    (f (lambda (x)
                                            ◊-         (((w w) f) x) )) ) )
                                            ◊end{complex}
                                  ◊end{where}|
    3 )
|◊Equals|   ((lambda (n)
      (if (= n 0) 1
          (* n (f (- n 1))) ) )|◊begin{where}
                                ◊- f {◊eq} ◊begin{complex}
                                          ◊-(lambda (x)
                                          ◊-  (((w w) meta-fact) x) )◊begin{where}
                                                         ◊- w {◊eq} ◊begin{complex*}{6.2cm}
                                                                   ◊-(lambda (w)
                                                                   ◊-  (lambda (f)
                                                                   ◊-    (f (lambda (x)
                                                                   ◊-         (((w w) f) x) )) ) )
                                                                   ◊end{complex*}
                                                         ◊end{where}
                                                  ◊end{complex}
                                ◊end{where}|
    3 )
|◊Equals|   (* 3 (f 2))|◊begin{where}
                        ◊- f {◊eq} ◊begin{complex}
                                  ◊-(lambda (x)
                                  ◊-  (((w w) meta-fact) x) )◊begin{where}
                                                             ◊- w {◊eq} ◊begin{complex*}{4.8cm}
                                                                       ◊-(lambda (w)
                                                                       ◊-  (lambda (f)
                                                                       ◊-    (f (lambda (x)
                                                                       ◊-         (((w w) f) x) )) )  )
                                                                       ◊end{complex*}
                                                                      ◊end{where}
                                  ◊end{complex}
                        ◊end{where}|
|◊Equals|   (* 3 (((w w) meta-fact) 2))|◊begin{where}
                                        ◊- w {◊eq} ◊begin{complex}
                                                  ◊-(lambda (w)
                                                  ◊-  (lambda (f)
                                                  ◊-    (f (lambda (x)
                                                  ◊-         (((w w) f) x) )) ) )
                                                  ◊end{complex}
                                        ◊end{where}|
|◊Equals|   (* 3 (((lambda (f)           ; ◊term{шаг II}
            (f (lambda (x)
                 (((w w) f) x) )) )|◊begin{where}
                                    ◊- w {◊eq} ◊begin{complex}
                                              ◊-(lambda (w)
                                              ◊-  (lambda (f)
                                              ◊-    (f (lambda (x)
                                              ◊-         (((w w) f) x) )) ) )
                                              ◊end{complex}
                                    ◊end{where}|
          meta-fact )
         2 ) )
◊end{code:lisp}

Остановимся на минутку, чтобы заметить, что на шаге~II мы получили то~же самое
выражение, что и на шаге~I.
Естественно, оно появится и в~третий~раз:

◊begin{code:lisp}
(* 3 (* 2 (((lambda (f)
              (f (lambda (x)
                   (((w w) f) x) )) )|◊begin{where}
                                      ◊- w {◊eq} ◊begin{complex}
                                                ◊-(lambda (w)
                                                ◊-  (lambda (f)
                                                ◊-    (f (lambda (x)
                                                ◊-         (((w w) f) x) )) ) )
                                                ◊end{complex}
                                      ◊end{where}|
            meta-fact )
           1 )))
|◊Equals|   (* 3 (* 2 ((meta-fact (lambda (x)
                           (((w w) meta-fact) x) ))|◊begin{where}
                                                    ◊- w {◊eq} ◊begin{complex*}{4.5cm}
                                                              ◊-(lambda (w)
                                                              ◊-  (lambda (f)
                                                              ◊-    (f (lambda (x)
                                                              ◊-         (((w w) f) x) )) ) )
                                                              ◊end{complex*}
                                                    ◊end{where}|
              1 )))
|◊Equals|   (* 3 (* 2 ((lambda (n)
                (if (= n 0) 1
                    (* n (f (- n 1))) ) )|◊begin{where}
                                          ◊- f {◊is} ...
                                          ◊end{where}|
              1 )))
|◊Equals|   (* 3 (* 2 (if (= n 0) 1 (* n (f (- n 1))))))|◊begin{where}
                                                         ◊- n {◊is} 1
                                                         ◊- f {◊is} ...
                                                         ◊end{where}|
|◊Equals|   (* 3 (* 2 1))

|◊is| 6
◊end{code:lisp}

Обратите внимание, что в~процессе вычислений мы действительно используем
функцию, вычисляющую факториал.
Это значение выражения:

◊begin{code:lisp}
(lambda (x)
  (((w w) f) x) )|◊begin{where}
                  ◊- f {◊eq} meta-fact
                  ◊- w {◊is} ◊begin{complex}
                            ◊-(lambda (w)
                            ◊-  (lambda (f)
                            ◊-    (f (lambda (x)
                            ◊-         (((w w) f) x) )) ) )
                            ◊end{complex}
                  ◊end{where}|
◊end{code:lisp}

◊indexR{самоприменение!и рекурсия}
Идея состоит в~том, что благодаря самоприменению мы помним, как создать заново
данную функцию, и делаем это каждый раз, когда для вычислений требуется
рекурсивный вызов.

◊indexCS{define}{как решатель уравнений}
Таким образом можно получить простую рекурсию без использования побочных
эффектов, только с~помощью~◊ic{fix}, комбинатора неподвижной точки.
Благодаря
◊comb{Y} (или ◊ic{fix}), ◊ic{define} можно определить как решатель рекурсивных
уравнений; она принимает уравнение и связывает решение с~переданным именем.
В~итоге, если мы передадим ◊ic{define} уравнение для факториала, то с~◊ic{fact}
будет связано следующее значение:

◊begin{code:lisp}
(fix (lambda (fact)
       (lambda (n)
         (if (= n 0) 1
             (* n (fact (- n 1))) ) ) ))
◊end{code:lisp}

Аналогично можно решать системы уравнений, а значит, и задавать взаимно
рекурсивные функции, собирая их уравнения воедино:

◊indexC{odd"?}◊indexC{even"?}
◊begin{code:lisp}
(define odd-and-even
  (fix (lambda (f)
         (lambda (which)
           (case which
             ((odd) (lambda (n) (if (= n 0) #f
                                    ((f 'even) (- n 1)) )))
             ((even) (lambda (n) (if (= n 0) #t
                                     ((f 'odd) (- n 1)) ))) ) ) )) )
(define odd? (odd-and-even 'odd))
(define even? (odd-and-even 'even))
◊end{code:lisp}

У~этого метода есть один большой недостаток: неэффективность, даже по сравнению
с~наивной реализацией~◊ic{letrec}.
(И~всё~же, см.~◊cite{roz92,ser93}.) Тем
не~менее, он используется, особенно в~качестве книжного примера.
Функциональные
языки, по мнению ◊cite{pj87}, тоже особо не~жалуют данный метод, так как,
во-первых, он неэффективен, а во-вторых, ◊ic{fix} плохо сочетается с~системами
вывода типов.
Действительно, ◊ic{fix} принимает функционал,%
◊footnote*{Терминология Маккарти из~◊cite{mae+62}: функционал — это функция,
принимающая другие функции как аргументы.} принимающий функцию типа $◊alpha ◊to
◊beta$, и возвращает неподвижную точку этого функционала.
То~есть типом~◊ic{fix}
является
%
◊[ ◊big((◊alpha ◊to ◊beta) ◊to (◊alpha ◊to ◊beta)◊big) ◊to (◊alpha ◊to ◊beta) ◊]

◊indexR{самоприменение!типизация}
Но в~определении ◊ic{fix} есть самоприменение: ◊ic{(d~d)}.
Обозначив его тип
$◊gamma$, имеем:
%
◊[ ◊gamma = ◊gamma ◊to (◊alpha ◊to ◊beta) ◊]

Потребуется или нетривиальная система типов, чтобы в~ней можно было выразить
подобный рекурсивный тип, или~же мы будем вынуждены реализовать ◊ic{fix}
в~интерпретаторе как примитивную функцию, так как её нельзя выразить средствами
самого языка.


◊section{Заключение}◊label{lisp1-2-omega/sect:conclusions}

В~этой главе мы прошлись по наиболее заметным из вопросов, на которые сообщество
Лиспа за последние несколько десятков лет так и не~смогло дать однозначного
ответа.
Рассмотрев причины данных разногласий, мы поняли, что они вовсе не~такие
серьёзные по своей сути.
Большая часть из них связана с~неоднозначностью
толкования смысла формы ◊ic{lambda} и различными способами применения функций.
Хотя идея функции достаточно хорошо проработана в~математике, но
в~функциональных~(!) языках вроде Лиспа это отнюдь не~так.
Различные мнения по
таким вопросам — это часть истории Лиспа.
Подобно изучению истории родного
народа, их знание облегчает понимание причин тех или иных решений в~дизайне
языка, а также улучшает стиль программирования в~общем.

Также данная глава демонстрирует существенную важность понятия связывания.
В~◊Lisp1 переменная (имя) ассоциируется с~уникальной привязкой (возможно
глобальной), которая в~свою очередь ассоциируется с~каким-либо значением.
Так
как привязка уникальна, то мы говорим о~значении переменной, а не~о~значении
привязки этой переменной.
Если рассматривать привязки как абстрактный тип
данных, то можно сказать, что объекты этого типа создаются связывающими формами,
их значение определяется вычислением, изменяются они присваиванием, и могут быть
захвачены при создании замыкания, если тело замыкания ссылается на переменную,
которая ассоциирована с~данной привязкой.

Привязки не~являются полноценными объектами.
Они не~существуют в~отрыве от
переменных и могут быть изменены только косвенно.
Собственно, привязки полезны
именно потому, что они крепко-накрепко связаны со~своими переменными.

◊indexR{форма!связывающая}
◊indexR{связывающие формы}
◊indexR{область видимости!лексическая}
Бок~о~бок со~связывающими формами следует идея областей видимости.
Область
видимости переменной — это пространство в~тексте программы, где можно
обращаться к~данной переменной.
Область видимости переменных, создаваемых
формой ◊ic{lambda}, ограничена телом данной формы.
Поэтому она называется
текстуальной или лексической.

Присваивание вносит множество неоднозначностей в~идею связывания, мы изучим этот
вопрос подробнее в~следующих главах.


◊section{Упражнения}◊label{lisp1-2-omega/sect:exercises}

◊begin{exercise}◊label{lisp1-2-omega/ex:funcall}
Следующее выражение записано на {◊CommonLisp}.
Как бы вы его перевели на~Scheme?

◊begin{code:lisp}
(funcall (function funcall) (function funcall) (function cons) 1 2)
◊end{code:lisp}
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:lexical}
Что вернёт данная программа на псевдо-{◊CommonLisp} из этой главы?
О~чём она вам напоминает?

◊begin{code:lisp}
(defun test (p)
  (function bar) )

(let ((f (test #f)))
  (defun bar (x) (cdr x))
  (funcall f '(1 .
2)) )
◊end{code:lisp}
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:innovations}
Реализуйте в~вашем интерпретаторе первые две инновации из
раздела~◊ref{lisp1-2-omega/sect:extensions}
◊seePage[lisp1-2-omega/sect:extensions].
Речь идёт о~трактовке чисел и
списков как функций.
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:assoc-with-comparator}
Можно научить функцию ◊ic{assoc/de} явно принимать компаратор (вроде ◊ic{eq?},
◊ic{equal?} {◊itp}) через аргумент, а не~задавать его внутри.
Сделайте это.
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:dynamic}
Используя ◊ic{bind/de} и ◊ic{assoc/de}, напишите макросы, эмулирующие
специальные формы ◊ic{dynamic-let}, ◊ic{dynamic} и~◊ic{dynamic-set!}.
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:write-put/get-prop}
◊indexC{putprop}◊indexC{getprop}
Напишите функции ◊ic{getprop} и ◊ic{putprop}, которые реализуют списки свойств.
Любой символ имеет личный список свойств в~виде пар «ключ — значение»;
добавление в~этот список осуществляет функция~◊ic{putprop}, поиск значения по
ключу осуществляет функция~◊ic{getprop}.
Также, естественно, должно выполняться
утверждение

◊begin{code:lisp}
(begin (putprop 'symbol 'key 'value)
       (getprop 'symbol 'key) )      |◊is| value
◊end{code:lisp}
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:label}
Определите специальную форму ◊ic{label} на ◊Lisp1.
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:labels}
Определите специальную форму ◊ic{labels} на ◊Lisp2.
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:orderless-letrec}
◊indexC{letrec}
Придумайте, как реализовать ◊ic{letrec} с~помощью ◊ic{let} и ◊ic{set!} так,
чтобы порядок вычисления значений"=инициализаторов был неопределённым.
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:fixn}
◊indexR{комбинаторы!неподвижной точки!универсальный}
У~нашего комбинатора неподвижной точки на Scheme обнаружился недостаток: он
поддерживает только унарные функции.
Реализуйте ◊ic{fix2}, работающий
с~бинарными функциями.
Затем ◊ic{fixN}, поддерживающий функции любой арности.
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:nfixn}
Далее напишите функцию ◊ic{NfixN}, возвращающую неподвижные точки для списка
функционалов произвольной арности.
Её можно использовать, например, следующим
образом:

◊begin{code:lisp}
(let ((odd-and-even
       (NfixN (list (lambda (odd? even?)    ; ◊ic{odd?}
                      (lambda (n)
                        (if (= n 0) #f (even? (- n 1))) ) )
                    (lambda (odd? even?)    ; ◊ic{even?}
                      (lambda (n)
                        (if (= n 0) #t (odd? (- n 1))) ) ) )) ))
  (set! odd? (car odd-and-even))
  (set! even? (cadr odd-and-even)) )
◊end{code:lisp}
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:klop}
Рассмотрим функцию ◊ic{klop}.
Является~ли она комбинатором неподвижной точки?
Попробуйте доказать или опровергнуть, что ◊ic{(klop $f$)} тоже возвращает
неподвижную точку~$f$ подобно~◊ic{fix}.

◊indexC{klop}
◊begin{code:lisp}
(define klop
  (let ((r (lambda (s c h e m)
             (lambda (f)
               (f (lambda (n)
                    (((m e c h e s) f) n) )) ) )))
    (r r r r r r) ) )
◊end{code:lisp}
◊end{exercise}

◊begin{exercise}◊label{lisp1-2-omega/ex:hyper-fact}
◊indexC{fact}
Если функция ◊ic{hyper-fact} определена так:

◊begin{code:lisp}
(define (hyper-fact f)
  (lambda (n)
    (if (= n 0) 1
        (* n ((f f) (- n 1))) ) ) )
◊end{code:lisp}

◊noindent
то что вернёт ◊ic{((hyper-fact hyper-fact)~5)}?
◊end{exercise}

◊section*{Рекомендуемая литература}%
◊label{lisp1-2-omega/sect:recommended-reading}

Кроме упомянутой ранее работы по $◊lambda$"=исчислению ◊cite{ss78a} также имеет
смысл почитать про анализ функций в~◊cite{mos70} и сравнительный анализ ◊Lisp1
и ◊Lisp2 в~◊cite{gp88}.

В~◊cite{gor88} есть интересное введение в~$◊lambda$"=исчисление.

Комбинатор ◊comb{Y} разбирается подробнее в~◊cite{gab88}.
