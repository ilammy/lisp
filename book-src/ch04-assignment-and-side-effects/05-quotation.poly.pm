#lang pollen

◊section[#:label "assignment/sect:quotation"]{Семантика цитирования}

◊indexR{семантика!цитирования}
Вы уже наверно заметили, что в~этот раз мы как-то обошли стороной цитирование.
Ранее форма ◊ic{quote} всегда записывалась элементарно,
но~только лишь потому, что представление значений в~интерпретируемом языке
совпадало с~представлением значений в~самом интерпретаторе.
Теперь это не~так, поэтому определять ◊ic{quote} следующим образом нельзя:

◊; TODO: это прям лучшый способ сдеать такую аннотацию?
◊code:lisp[#:dialect "Ошибка!"]{
(define (evaluate-quote v r s k)
  (k v s) )
}

Здесь ошибочно считается, что ◊ic{v} — это именно то значение, которое необходимо вернуть.
В~смысле, конечно, это правильное значение, однако записанное на~языке, которого интерпретатор не~понимает.
В~предыдущих главах интерпретатор пользовался языком реализации, но~теперь ему необходим переводчик — функция~◊ic{transcode}.
Поэтому ◊nobr{(более-менее)} правильное определение цитирования выглядит~так:

◊indexC{evaluate-quote}
◊code:lisp{
(define (evaluate-quote c r s k)
  (transcode c s k) )
}

◊indexR{цитаты!композициональность}
◊indexR{композициональность!цитат}
Такое определение цитирования ◊term{композиционально} — его~смысл зависит только от аргументов ◊ic{evaluate-quote}.
Но,~к~сожалению, точный смысл таких цитат немного отличается от~того, который обычно подразумевается в~Лиспе и~Scheme.
Рассмотрим выражение ◊nobr{◊ic{(quote (a . b))}}.
По~определению оно ◊emph{в~точности} эквивалентно ◊nobr{◊ic{(cons 'a 'b)}}.
◊footnote{Ещё~точнее: ◊nobr{◊ic{'(a . b) ◊(equals) `(,'a . ,'b)}}.}
Когда цитируется составной объект вроде точечной пары ◊nobr{◊ic{(a . b)}},
в~памяти создаётся новая пара.
Подобное цитирование — это лишь краткая запись для конструирования объектов.
Таким образом, следующее выражение не~может быть истинным:

◊code:lisp{
(let ((foo (lambda () '(f o o))))
  (eq? (foo) (foo)) )
}

◊indexR{эквивалентность!цитат}
◊indexR{цитаты!эквивалентность}
◊indexR{мемоизация!цитат}
◊noindent
Здесь функция ◊ic{foo} каждый раз создаёт новый список;
возвращаемые значения независимы, различны в~смысле~◊ic{eq?}.
Если мы хотим, чтобы такое сравнение возвращало истину,
необходимо доработать ◊ic{evaluate-quote} так,
чтобы она возвращала идентичные значения, а~не~эквивалентные.
То~есть ◊ic{evaluate-quote} должна запоминать всё, что возвращала ранее,
и~при повторном вызове с~одними и~теми~же аргументами возвращать одни и~те~же ранее созданные объекты —
такой подход называется ◊term{мемоизацией}.
Если в~языке отсутствуют побочные эффекты и~◊ic{eq?},
то~невозможно отличить идентичные объекты от эквивалентных —
поэтому нет никакого смысла тратить ресурсы на создание и хранение одинаковых копий эквивалентных объектов,
когда можно обойтись всего одним экземпляром.

Конечно, в~присутствии побочных эффектов дела меняются,
но~всё~же обычно считается, что цитаты уникальны — как~числа, символы и логические значения.
Рассмотрим следующую реализацию цитирования:

◊indexC{evaluate-memo-quote}
◊indexC{shared-memo-quotations!*shared-memo-quotations*}
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
◊; TODO: многоточия замени автоматически, да?
Хм... оказывается, мемоизация требует побочных эффектов и глобальных переменных, чего нам хотелось~бы избежать.
Другим подходом к~реализации цитирования является трансформация программы:
вынести все значеня цитат за~скобки, вычислить их один раз в~начале исполнения,
а~затем просто ссылаться на~переменные, хранящие неизменные цитаты.
Приведённый ранее пример тогда преобразуется следующим образом:

◊; TODO: здесь наведи красоту
◊code:lisp{
(let ((foo (lambda () '(f o o))))
  (eq? (foo) (foo)) )

}
◊(is)
◊code:lisp{
(define quote35 '(f o o))

(let ((foo (lambda () quote35)))
  (eq? (foo) (foo)) )
}

В~итоге цитаты обретают должный смысл, а~◊ic{evaluate-quote} остаётся такой~же простой.

Можно пойти и~дальше —
полностью избавиться от неоднозначностей, связанных с~цитированием составных объектов,
есть сделать их создание явным:

◊code:lisp{
(define quote36 (cons 'o '()))
(define quote37 (cons 'o quote36))
(define quote38 (cons 'f quote37))

(let ((foo (lambda () quote38)))
  (eq? (foo) (foo)) )
}

И~ещё дальше, на всякий случай гарантируя уникальность символов вручную:

◊code:lisp{
(define symbol39 (string->symbol "o"))
(define symbol40 (string->symbol "f"))

(define quote36 (cons symbol39 '()))
(define quote37 (cons symbol39 quote36))
(define quote38 (cons symbol40 quote37))

(let ((foo (lambda () quote38)))
  (eq? (foo) (foo)) )
}

Пожалуй, здесь можно и~остановиться.
На~языке ассемблера или~Си строки можно представлять непосредственно,
так~что было~бы глупо разбивать их дальше на отдельные символы —
а~то так ведь можно и до элементарных частиц дойти!

В~итоге, композициональное определение цитирования оказывается вполне приемлемым.
Буквальное понимание такой простой формулировки несколько отличается по смыслу от традиционного понимания цитирования,
но~это легко поправимо простым дополнительным преобразованием программ.

◊indexR{гомоиконичность}
Однако, унифицированное представление кода и данных может сыграть с~нами злую шутку.
С~одной стороны очень удобно читать и~данные, и~программы с~помощью одной и~той~же функции~◊ic{read}.
Для~того, чтобы она могла отличить формы-данные от форм-программ,
как раз и существует специальная форма~◊ic{quote}.
Но~в~том-то и дело, что после ◊ic{quote} данные становятся частью программы.
Рассмотрим пример:

◊; TODO: стрелочки в программах, ага
◊indexC{vowel<=}
◊code:lisp{
(define vowel<=
  (let ((vowels '(#\a #\e #\i #\o #\u)))
    (lambda (c1 c2)
      (memq c2 (memq c1 vowels)) ) ) )

(set-cdr! (vowel<= #\a #\e) '())
(vowel<= #\o #\u) ◊(is) ◊ii{?}
}

При интерпретации этой программы очень вероятно, что мы получим не~◊ic{#t}, а~ошибку.
И~если после этого распечатать определение ◊ic{vowel<=}
(или если~бы переменная ◊ic{vowels} была глобальной),
то~мы увидим следующее:

◊code:lisp{
(define vowel1<=
  (let ((vowels '(#\a #\e)))
    (lambda (c1 c2)
      (memq c2 (memq c1 vowels)) ) ) )
}

◊indexR{свёртка констант}
◊indexR{константы}
Ой, мы~нечаянно кусок программы!
Аналогично можно добавлять элементы в~список ◊ic{vowels} или изменять~их.
Правда, такие трюки можно проделывать только в~интерпретаторе —
при компиляции подобные выражения лишены смысла.
Более того, если глобальная переменная ◊ic{vowel<=} неизменяема,
то~все формы вроде ◊nobr{◊ic{(vowel<= #\a #\e)}} можно вычислить ещё на этапе компиляции и~подставить вместо них готовые значения.
Такая оптимизация называется ◊term{свёрткой~констант} (◊english{constant~folding}),
её~обобщением является техника частичных вычислений.
◊seeCite{jgs93}

Компилятор также может решить подставить константы и в~само определение функции ◊ic{vowel<=} ради небольшого ускорения:

◊code:lisp{
(define vowel2<=
  (lambda (c1 c2)
    (case c1
      ((#\a) (memq c2 '(#\a #\e #\i #\o #\u)))
      ((#\e) (memq c2 '(#\e #\i #\o #\u)))
      ((#\i) (memq c2 '(#\i #\o #\u)))
      ((#\o) (memq c2 '(#\o #\u)))
      ((#\u) (eq? c2 #\u))
      (else  #f) ) ) )
}

◊indexR{склеивание цитат}
◊indexR{цитаты!склеивание}
Теперь выражение ◊nobr{◊ic{(eq? (cdr (vowel<= #\a #\e)) (vowel<= #\e #\i))}} имеет неопределённое значение.
С~одной стороны, оно не~должно быть истиной, ведь сравниваемые значения принадлежат явно различным цитатам.
С~другой стороны, иногда оно всё~же истинно — компиляторы часто оставляют за собой право оптимизировать расположение констант в~памяти,
накладывая их друг на~друга, превращая исходную программу в~следующую:

◊code:lisp{
(define quote82 (cons #\u '()))
(define quote81 (cons #\o quote82))
(define quote80 (cons #\i quote81)
(define quote79 (cons #\e quote80)
(define quote78 (cons #\a quote79)

(define vowel3<=
  (lambda (c1 c2)
    (case c1
      ((#\a) (memq c2 quote78))
      ((#\e) (memq c2 quote79))
      ((#\i) (memq c2 quote80))
      ((#\o) (memq c2 quote81))
      ((#\u) (eq? c2 #\u))
      (else  #f) ) ) )
}

◊indexR{константы}
◊indexR{связывание!неизменяемое}
Подобная оптимизация очевидно влияет на цитаты с~общими подвыражениями и ломает~◊ic{eq?}.
Есть простой способ избежать всех этих проблем: запретить изменять цитаты.
Именно так и поступают Scheme и~◊|CommonLisp|,
где изменение значения цитаты приводит к~неопределённому поведению.

Можно поступить чуть более жёстко, запретив изменение цитат синтаксически:

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
Подобным образом можно ввести неизменяемые строки (как~это сделано, например, в~Mesa),
неизменяемые векторы, и~т.~д.

◊(bigskip)

◊indexR{цитаты!циклических объектов}
◊indexR{циклические структуры данных}
Подводя итог, лучше воспользоваться мудростью поколений и, как советуют Scheme и ◊|CommonLisp|, стараться не~изменять значения цитат.
Однако это не~последняя из проблем, вызываемых цитированием.
Мы~уже упоминали методику «склеивания» цитат, приводящую к~физическому наложению данных.
А~можно~ли намеренно создать цитату с~подобным эффектом?
Существуют как минимум два~способа: макросы и программируемый поток ввода.

◊indexR{макросимволы}
В~◊CommonLisp можно вводить специальные макросимволы, при чтении которых выполняются определённые процедуры.
Например, ◊ic{'}~является именно макросимволом, оборачивающим следующее прочитанное выражение в~◊ic{quote}.
Или~же ◊ic{#.◊ii{выражение}}, вместо которого подставляется значение ◊ii{выражения} —
то~есть следующее за~◊ic{#.} выражение считывается, вычисляется
◊footnote{Окружение и продолжение вычислений выбираются на усмотрение реализации.}
и~полученное значение подставляется вместо всей конструкции.
Соответственно, если написать:

◊code:lisp[#:dialect CommonLisp]{
(define bar (quote #.(let ((list '(0 1)))
                       (set-cdr! (cdr list) list)
                       list )))
}

◊noindent
то~переменная ◊ic{bar} будет содержать бесконечный список из чередующихся нулей и~единиц.
С~помощью~◊ic{#.} мы процитировали пару, чей ◊ic{cddr} равен ей самой.
В~CLtL2 ◊seeCite{ste90} прямым текстом написано, что ◊ic{#.} существует именно для ввода таких неудобных значений.
Если мы захотим реализовать подобное поведение у~себя,
то~это несколько усложнит трансформацию программ (которая выносит вычисление цитат в~начало),
так~как будет необходимо учитывать и правильно обрабатывать подобные циклы.

Хорошая~новость: в~Scheme так писать нельзя, так как ◊ic{read} не~программируется и макросимволы не~поддерживаются.
Плохая~новость: в~Scheme есть просто макросы, с~помощью которых можно добиться того~же результата.
Форма ◊ic{define-syntax}, правда, такого не~позволяет,
поэтому мы воспользуемся ◊ic{define-abbreviation} —
макросом для определения макросов, который детально рассматривается в~девятой~главе.
◊seePage{chapter:macros}
◊; TODO: это ж просто ссылка на главу

◊; TODO: шрифт для программ должен отображать ' как ’, а ` как ‘
◊indexC{cycle}
◊code:lisp{
(define-abbreviation (cycle n)
  (let ((c (iota n)))           ; (iota 3) ◊(is) (0 1 2 3)
    (set-cdr! (last-pair c) c)  ; (last-pair '(1 2 3)) ◊(is) (3)
    `(quote ,c) ) )

(define bar (cycle 2))
}

◊indexE{#n#@◊ic{#◊ii{n}#}, ◊ic{#◊ii{n}=}}
Таким образом в~Scheme определяется циклический список из нулей и единиц,
подставляемый в~цитату, которой инициализируется переменная~◊ic{bar}.
Подобный список нельзя процитировать прямым текстом в~определении ◊ic{bar},
так~как ◊ic{read} в~Scheme не~умеет считывать циклические данные.
В~◊CommonLisp это возможно с~помощью макросимволов ◊ic{#◊ii{n}=} и~◊ic{#◊ii{n}#}
◊footnote{
  Здесь считываемый объект ссылается на самого себя.
  Следовательно, функция ◊ic{read} должна уметь создать в~памяти точечную пару,
  «зациклить» её и~подставить адрес на~место макроса.
  ◊ic{read} также желательно быть достаточно сообразительной,
  чтобы не~попасть впросак на чём-нибудь вроде~◊ic{#1=#1#}.
}

◊code:lisp{
(define bar #1=(0 1 . #1#))
}

В~настоящее время семантика Scheme не~поддерживает бесконечные списки —
в~том смысле, что невозможно записать бесконечный список в~виде ◊term{литерала},
который непосредственно присутствует в~программе.
А~раз нельзя записать, то~и нельзя процитировать.

Девятая~глава посвящена подробному рассмотрению макросов,
их~возможностям, ограничениям и~проблемам.
◊seePage{chapter:macros}
◊; TODO: это ж всё ещё ссылка, которую ты уже приводил, может дропни этот абзац вообще?
