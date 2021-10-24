% -*- coding: utf-8 -*-

◊subsection{Запускаем интерпретатор}%
◊label{assignment/implementation/ssect:starting}

Наконец, всё готово для запуска нашего интерпретатора.
Главный цикл вызывает
◊ic{evaluate} с~рекурсивным продолжением, которое обеспечивает непрерывность
сознания интерпретатора, передавая следующему вызову память предыдущего.

◊indexC{chapter4-interpreter}
◊code:lisp{
(define (chapter4-interpreter)
  (define (toplevel s)
    (evaluate (read) r.global s
              (lambda (v ss)
                (display (transcode-back v ss))
                (toplevel ss) ) ) )
  (toplevel s.global) )
}

◊indexR{представление!значений}
Главной особенностью данного интерпретатора является отличие представления
данных интерпретируемого языка от представления данных в~языке реализации.
Особенно разительно отличаются точечные пары и, следовательно, списки, поэтому
мы вынуждены воспользоваться функцией ◊ic{transcode-back} для преобразования
результата вычислений в~нечто понятное ◊ic{display}, чтобы его можно было
вывести на экран.
Эту вспомогательную функцию вполне можно сразу объединить
с~примитивом ◊ic{display}.

◊indexC{transcode-back}
◊code:lisp{
(define (transcode-back v s)
  (case (v 'type)
    ((null)     '())
    ((boolean)  ((v 'boolify) #t #f))
    ((symbol)   (v 'name))
    ((number)   (v 'value))
    ((pair)     (cons (transcode-back (s (v 'car)) s)
                      (transcode-back (s (v 'cdr)) s) ))
    ((function) v)  ; почему бы и нет?
    (else       (wrong "Unknown type" (v 'type))) ) )
}


◊section[#:label "assignment/sect:io-and-memory"]{Ввод-вывод и~память}

◊indexR{потоки ввода-вывода}
Теперь поговорим о~вводе-выводе.
Если ограничиться одним потоком ввода и одним
потоком вывода (то~есть функциями ◊ic{display} и ◊ic{read}), то достаточно
добавить каждой функции интерпретатора ещё два аргумента и передавать через них
эти потоки.
Поток ввода содержит всё, что программа может прочитать, а поток
вывода содержит всё, что она туда написала.
Можно даже предположить, что эти
потоки являются единственным средством связи программы с~внешним миром.

Поток вывода можно легко представить списком пар (◊ii{память}, ◊ii{значение}),
которые передаются функции ◊ic{transcode-back}.
Но как представить поток ввода?
Хитрость здесь в~том, что мы можем вводить и точечные пары со~списками, а они
не~могут существовать отдельно от памяти.
Поэтому очевидно, что в~случае чтения
точечных пар необходимо сохранять в~памяти их компоненты.
Переводчиком будет
служить функция ◊ic{transcode}, которая принимает значение языка реализации
(обозначим его~◊ic{c}), память и~продолжение.
Она переводит это значение
в~представление реализуемого языка и передаёт его продолжению вместе с~новым
состоянием памяти.

◊indexC{transcode}
◊code:lisp{
(define (transcode c s k)
  (cond
    ((null? c)    (k the-empty-list s))
    ((boolean? c) (k (create-boolean c) s))
    ((symbol? c)  (k (create-symbol c) s))
    ((number? c)  (k (create-number c) s))
    ((pair? c)
     (transcode (car c)
                s
                (lambda (a ss)
                  (transcode (cdr c)
                             ss
                             (lambda (d sss)
                               (allocate-pair a d sss k) ) ) ) ) )
    (else (wrong "Not supported" c)) ) )
}

На этом мы остановимся, так как дальнейшая реализация слишком объёмна, чтобы
приводить её здесь полностью: помимо примитивов ◊ic{read} и~◊ic{display},
необходимо в~каждой функции, которую мы написали до этого, расширить набор
аргументов ◊ic{e}, ◊ic{r}, ◊ic{s}, ◊ic{k} ещё двумя: ◊ic{i} и~◊ic{o}, через
которые и передавать потоки ввода-вывода подобно памяти (потому что они, как и
память, должны быть доступны отовсюду).


◊section[#:label "assignment/sect:quotation"]{Семантика цитирования}

◊indexR{семантика!цитирования}
Наверное, вы уже заметили, что в~этот раз мы как-то обошли стороной
цитирование.
Форма ◊ic{quote} раньше всегда записывалась элементарно, но только
потому, что представление значений в~интерпретируемом языке совпадало
с~представлением значений в~самом интерпретаторе.
Теперь это не~так, поэтому
определять ◊ic{quote} следующим образом нельзя:

◊code:lisp{
(define (evaluate-quote v r s k)  |◊dialect{Ошибка!}|
  (k v s) )
}

Здесь ошибочно считается, что ◊ic{v} — это именно то значение, которое
необходимо вернуть.
В~смысле, конечно, это именно то, что надо, но это значение
записано на языке, которого наш интерпретатор не~понимает.
В~предыдущих главах
интерпретатор пользовался языком реализации, но теперь ему необходим переводчик
— ◊ic{transcode}.
Поэтому (более-менее) правильное определение цитирования
выглядит~так:

◊indexC{evaluate-quote}
◊code:lisp{
(define (evaluate-quote c r s k)
  (transcode c s k) )
}

◊indexR{цитаты!композициональность}
◊indexR{композициональность!цитат}
Такое определение цитирования ◊term{композиционально} — его смысл зависит
только от аргумента~◊ic{c}, оно не~учитывает контекст (значения других
переменных).
Но, к~сожалению, точный смысл таких цитат немного отличается от
того, который обычно подразумевается в~Лиспе и~Scheme.
Рассмотрим выражение
◊ic{(quote (a~.~b))}.
По~определению оно ◊emph{в~точности} эквивалентно
◊ic{(cons 'a~'b)}.◊footnote*{Ещё точнее: ◊ic{'(a~.~b) {◊equals} `(,'a~.~,'b)}.}
Когда мы цитируем составной объект вроде точечной пары ◊ic{(a~.~b)}, мы создаём
новую пару в~памяти.
Таким образом, такое цитирование — это лишь краткая
запись для наших ◊ic{create-}функций.
Поэтому ничуть не~удивительно, что
следующее выражение возвращает ложь:

◊code:lisp{
(let ((foo (lambda () '(f o o))))
  (eq? (foo) (foo)) )
}

◊indexR{эквивалентность!цитат}
◊indexR{цитаты!эквивалентность}
◊indexR{мемоизация!цитат}
Функция ◊ic{foo} каждый раз создаёт новый список; все создаваемые значения
независимы, различны в~смысле~◊ic{eq?}.
Если мы хотим, чтобы данное выражение
всегда возвращало истину, нам необходимо доработать ◊ic{evaluate-quote} так,
чтобы она возвращала идентичные значения, а не~эквивалентные; для этого она
должна запоминать всё, что возвращала ранее.
Это называется ◊term{мемоизация}.
Одной из причин желательности такого поведения является то, что если в~языке нет
побочных эффектов и ◊ic{eq?}, то нельзя отличить идентичные объекты от просто
эквивалентных, поэтому нет никакого смысла тратить память на бесполезные копии
эквивалентных объектов, когда можно обойтись одним экземпляром.

Конечно, если допускать побочные эффекты, то дело меняется, но всё~же обычно
считается, что цитаты уникальны — как числа, символы и логические значения.

◊indexC{evaluate-memo-quote}
◊indexC*{shared-memo-quotations}{*shared-memo-quotations*}
◊code:lisp{
(define *shared-memo-quotations* '())
(define evaluate-memo-quote
  (lambda (c r s k)
    (let ((couple (assoc c *shared-memo-quotations*)))
      (if (pair? couple)
          (k (cdr couple) s)
          (transcode c s (lambda (v ss)
                           (set! *shared-memo-quotations*
                                 (cons (cons c v)
                                       *shared-memo-quotations* ) )
                           (k v ss) )) ) ) ) )
}

◊phantomlabel{assignment/quotation/par:transform}
◊indexR{преобразование!перенос цитат}
Однако, такой вариант, во-первых, требует побочных эффектов и глобальной
переменной, чего хотелось~бы избежать, а во-вторых, учёт используемых цитат
вполне можно выполнить и заранее, за что компилятор скажет нам спасибо: сначала
мы переносим вычисление цитат в~самое начало программы, затем заменяем все
обращения к~цитатам ссылками на переменные, хранящие вычисленные ранее значения.
На~предыдущем примере это выглядит~так:

{◊def◊T{◊hbox to 0pt{◊${◊leadsto}}}
◊code:lisp{
(let ((foo (lambda () '(f o o))))    (define quote35 '(f o o))
  (eq? (foo) (foo)) )             |◊T|   (let ((foo (lambda () quote35)))
                                       (eq? (foo) (foo)) )
}}

В~итоге цитаты обретают должный смысл, а ◊ic{evaluate-quote} остаётся такой~же
простой.

Можно было~бы пойти и дальше, полностью избавившись от неоднозначностей,
связанных с~цитированием составных объектов, сделав их создание явным:

◊ForLayout{display}{◊begingroup
◊lstset{aboveskip=◊smallskipamount, belowskip=◊smallskipamount}}

◊code:lisp{
(define quote36 (cons 'o '())
(define quote37 (cons 'o quote36)
(define quote38 (cons 'f quote37)
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(let ((foo (lambda () quote38)))
  (eq? (foo) (foo)) )
}

И~ещё дальше, на всякий случай гарантируя правильное цитирование символов
вручную:

◊code:lisp{
(define symbol39 (string->symbol "o"))
(define symbol40 (string->symbol "f"))
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(define quote36 (cons symbol39 '())
(define quote37 (cons symbol39 quote36)
(define quote38 (cons symbol40 quote37)
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(let ((foo (lambda () quote38)))
  (eq? (foo) (foo)) )
}

Пожалуй, на этом остановимся.
На языке ассемблера или Си строки можно
представлять непосредственно, так что было~бы глупо разбивать их на отдельные
символы — а~то так ведь можно и до элементарных частиц дойти!

В~конце концов, композициональное определение цитирования оказывается
приемлемым, так как имеет простую формулировку, которая хоть и отличается по
смыслу от традиционного понимания цитирования, но это легко поправимо простым
дополнительным преобразованием программ.

◊indexR{гомоиконичность}
Однако, унифицированное представление кода и данных может сыграть с~нами злую
шутку.
С~одной стороны это удобно: читать и~данные, и~программы с~помощью одной
функции ◊ic{read}.
Для того, чтобы она могла отличить формы-данные от
форм-программ, как раз и существует специальная форма~◊ic{quote}.
Но в~том-то
и дело, что после ◊ic{quote} данные становятся частью программы.
Рассмотрим
пример:

◊ForLayout{display}{◊endgroup}

◊indexC{vowel<=}
◊code:lisp{
(define vowel<=
  (let ((vowels '(#◊a #◊e #◊i #◊o #◊u)))
    (lambda (c1 c2)
      (memq c2 (memq c1 vowels)) ) ) )
|◊ForLayout{display}{◊vskip-◊baselineskip}|
(set-cdr! (vowel<= #◊a #◊e) '())
(vowel<= #◊o #◊u) |◊is| |◊ii{?}|
}

При интерпретации этой программы есть высокая вероятность получить не~◊ic{#t},
а~ошибку.
И~если после этого мы распечатаем определение ◊ic{vowel<=} (или
если~бы ◊ic{vowels} была глобальной), то увидим следующее:

◊code:lisp{
(define vowel1<=
  (let ((vowels '(#◊a #◊e)))
    (lambda (c1 c2)
      (memq c2 (memq c1 vowels)) ) ) )
}

◊indexR{свёртка констант}
◊indexR{константы}
Ой, мы нечаянно кусок программы! Аналогично можно добавлять элементы в~список
◊ic{vowels} или изменять их.
Правда, такие трюки можно проделывать только
в~интерпретаторе; в~случае компилятора мы скорее всего получим ошибку.
Дело
в~том, что если глобальная переменная ◊ic{vowel<=} неизменяема, то все формы
вроде ◊ic{(vowel<= #◊bslash a ◊#◊bslash e)} могут быть вычислены ещё на
этапе компиляции, чем компилятор и пользуется, подставляя вместо них сразу
готовый результат.
Такая оптимизация называется ◊term{свёрткой констант}
(◊english{constant folding}), её обобщением является техника частичных
вычислений ◊cite{jgs93}.

Компилятор также может решить подставить константы и в~само определение функции
◊ic{vowel<=} ради небольшого ускорения:

◊code:lisp{
(define vowel2<=
  (lambda (c1 c2)
    (case c1
      ((#◊a) (memq c2 '(#◊a #◊e #◊i #◊o #◊u)))
      ((#◊e) (memq c2 '(#◊e #◊i #◊o #◊u)))
      ((#◊i) (memq c2 '(#◊i #◊o #◊u)))
      ((#◊o) (memq c2 '(#◊o #◊u)))
      ((#◊u) (eq? c2 #◊u))
      (else  #f) ) ) )
}

◊indexR{склеивание цитат}
◊indexR{цитаты!склеивание}
Теперь выражение ◊ic{(eq? (cdr (vowel<= #◊bslash a ◊#◊bslash e)) (vowel<=
#◊bslash e ◊#◊bslash i))} имеет неопределённое значение.
С~одной стороны,
оно не~должно быть истиной, так как сравниваемые значения принадлежат явно
различным цитатам.
С~другой стороны, иногда оно всё~же истинно: компиляторы
часто оставляют за собой право оптимизировать расположение констант в~памяти,
накладывая их друг на друга и превращая исходную программу в~следующую:

◊code:lisp{
(define quote82 (cons #◊u '()))
(define quote81 (cons #◊o quote82))
(define quote80 (cons #◊i quote81)
(define quote79 (cons #◊e quote80)
(define quote78 (cons #◊a quote79)

(define vowel3<=
  (lambda (c1 c2)
    (case c1
      ((#◊a) (memq c2 quote78))
      ((#◊e) (memq c2 quote79))
      ((#◊i) (memq c2 quote80))
      ((#◊o) (memq c2 quote81))
      ((#◊u) (eq? c2 #◊u))
      (else  #f) ) ) )
}

◊indexR{константы}
◊indexR{связывание!неизменяемое}
Такая оптимизация влияет на разделяемые цитаты и ломает~◊ic{eq?}.
Есть простой
способ избежать всех этих проблем: запретить изменять цитаты.
Именно так и
поступают Scheme и~{◊CommonLisp}.
Но они делают это мягко, в~общем случае считая
результат изменения значения цитаты неопределённым.

Можно поступить более жёстко, запретив изменение цитат синтаксически:

◊indexC{evaluate-immutable-quote}
◊indexC{immutable-transcode}
◊indexC{allocate-immutable-pair}
◊indexC{create-immutable-pair}
◊code:lisp{
(define (evaluate-immutable-quote c r s k)
  (immutable-transcode c s k) )

(define (immutable-transcode c s k)
  (cond
    ((null? c)    (k the-empty-list s))
    ((pair? c)
     (immutable-transcode
      (car c) s (lambda (a ss)
                  (immutable-transcode
                   (cdr c) ss (lambda (d sss)
                                (allocate-immutable-pair
                                 a d sss k ) ) ) ) ) )
    ((boolean? c) (k (create-boolean c) s))
    ((symbol? c)  (k (create-symbol c) s))
    ((number? c)  (k (create-number c) s))
    (else (wrong "Not supported" c)) ) )

(define (allocate-immutable-pair a d s k)
  (allocate 2 s
   (lambda (a* ss)
     (k (create-immutable-pair (car a*) (cadr a*))
        (update (update ss (car a*) a) (cadr a*) d) ) ) ) )

(define (create-immutable-pair a d)
  (lambda (msg)
    (case msg
      ((type)    'pair)
      ((boolify) (lambda (x y) x))
      ((set-car) (lambda (s v) (wrong "Immutable pair")))
      ((set-cdr) (lambda (s v) (wrong "Immutable pair")))
      ((car)     a)
      ((cdr)     d) ) ) )
}

В~таком случае любая попытка модифицировать цитату обречена на провал.
Подобным
образом можно ввести неизменяемые строки (как это сделано, например, в~Mesa),
неизменяемые векторы {◊itd}

◊indexR{цитаты!циклических объектов}
◊indexR{циклические структуры данных}
Подводя итог, лучше воспользоваться мудростью поколений и, как советуют Scheme и
{◊CommonLisp}, стараться не~изменять значения цитат.
Но это не~последняя из
проблем, вызываемых цитированием.
Мы уже упоминали методику «склеивания»
цитат, приводящую к~физическому наложению данных.
А~можно~ли намеренно создать
цитату с~таким эффектом? Есть как минимум два способа это сделать: макросы и
программируемый поток ввода.

◊indexR{макросимволы}
В~{◊CommonLisp} можно вводить специальные макросимволы, при чтении которых
вызываются определённые процедуры.
Например, ◊ic{'}~является именно
макросимволом, оборачивающим следующее прочитанное выражение в~◊ic{quote}.
Или
◊ic{#.◊ii{выражение}}, вместо которого подставляется значение ◊ii{выражения};
то есть следующее за~◊ic{#.} выражение считывается, вычисляется%
◊footnote{Окружение и продолжение вычислений выбираются на усмотрение
реализации.} и его значение подставляется вместо всей конструкции.
В~итоге, мы
можем написать так:

◊code:lisp{
(define bar (quote #.(let ((list '(0 1)))     |◊dialect{◊CommonLisp}|
                       (set-cdr! (cdr list) list)
                       list )))
}

◊noindent
После этого переменная ◊ic{bar} будет содержать бесконечный список из
чередующихся нулей и~единиц.
С~помощью ◊ic{#.} мы процитировали пару, чей
◊ic{cddr} равен ей самой.
В~CLtL2~◊cite{ste90} прямым текстом написано, что
◊ic{#.} и существует для ввода таких неудобных значений.
Если мы захотим
реализовать такое поведение у~себя, то это немного усложнит трансформацию
программ (которая выносит вычисление цитат в~начало), так как она должна будет
учитывать возможность циклов.

Хорошая новость: в~Scheme так писать нельзя, так как ◊ic{read}
не~программируется и макросимволы не~поддерживаются.
Плохая новость: в~Scheme
есть просто макросы, с~помощью которых можно добиться того~же результата.
Форма
◊ic{define-syntax}, правда, такого не~позволяет, поэтому мы используем
◊ic{define-abbreviation} — макрос для определения макросов, который детально
рассматривается в~девятой главе.
◊seePage[chapter:macros]

◊indexC{cycle}
◊code:lisp{
(define-abbreviation (cycle n)
  (let ((c (iota n)))           ; ◊ic{(iota 3)} {◊is} ◊ic{(0 1 2 3)}
    (set-cdr! (last-pair c) c)  ; ◊ic{(last-pair '(1 2 3))} {◊is} ◊ic{(3)}
    `(quote ,c) ) )
(define bar (cycle 2))
}

% ## -- чтобы TeX не считал #n за аргумент команды
◊indexE{##n##@◊ic{#◊ii{n}◊#}, ◊ic{◊#◊ii{n}=}}
Таким образом в~Scheme определяется циклический список из нулей и единиц,
подставляемый в~цитату, которой инициализируется переменная~◊ic{bar}.
Мы
не~можем написать такой список прямым текстом в~определении ◊ic{bar}, так как
◊ic{read} Scheme не~умеет считывать циклические данные.
В~{◊CommonLisp} это
возможно с~помощью макросимволов ◊ic{#◊ii{n}=} и~◊ic{◊#◊ii{n}◊#}◊footnote{Здесь
считываемый объект ссылается на самого себя.
Следовательно, функция ◊ic{read}
должна уметь создать в~памяти точечную пару, «зациклить» её и подставить адрес
на место макроса.
Ей также желательно быть достаточно сообразительной, чтобы
не~попасть впросак на чём-нибудь вроде~◊ic{#1=◊#1◊#}.}:

◊code:lisp{
(define bar #1=(0 1 . #1#))
}

В~настоящее время в~Scheme подобные списки нельзя записывать непосредственно
в~виде ◊term{литералов}, только создавать вручную за несколько шагов.
Поэтому и
цитировать подобные структуры можно только лишь с~помощью ухищрений вроде
макросов.

Девятая глава будет посвящена подробному рассмотрению макросов, их возможностей,
ограничений и вызываемых ими проблем.
◊seePage[chapter:macros]


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
