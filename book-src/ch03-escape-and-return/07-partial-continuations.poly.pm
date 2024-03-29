#lang pollen

◊section[#:label "escape/sect:partial"]{Частичные продолжения}

◊indexR{продолжения (continuations)!частичные продолжения}
Среди вопросов, поднимаемых продолжениями, есть ещё один довольно интересный:
что именно происходит с~кодом, отбрасываемым при переходе?
Другими словами, с~тем куском продолжения (или~стека), который находится между состояниями до~прыжка и~после.
Мы~говорили, что такой ◊term{срез стека} не~сохраняется при~переходе.
Действительно, переход отказывается от исполнения этого кода, однако он вовсе не~является бессмысленным или некорректным:
ведь если~бы через него не~перешагнули, то он~бы принял какое-то значение,
выполнил определённые действия и передал~бы полученный результат своему продолжению.
То~есть вёл~бы себя как обычная функция.
◊; TODO: разные представления цитат
◊; здесь бы хотелось захватить всю фразу "во многих работах" как ссылку на цитируемые работы
◊; плюс хотелось бы ещё указывать, надо ли отображать полное название или только код типа FF87
Во~многих работах ◊seeCite{ffdm87,ff87,fel88,df90,hd90,qs91,mq94}
приводятся приёмы сохранения и использования таких срезов стека —
◊term{частичных продолжений} (◊english{partial/delimited continuations}).

Рассмотрим следующий простой пример:

◊code:lisp{
(+ 1 (call/cc (lambda (k) (set! foo k) 2))) ◊(is) 3
(foo 3)                                     ◊(is) 4
}

◊noindent
Какое именно продолжение хранится в~◊ic{foo}?
Казалось~бы ◊${\lambda u . 1 + u}, но~чему тогда равно ◊nobr{◊ic{(foo (foo 4))}}?

◊code:lisp{
(foo (foo 4))                               ◊(is) 5
}

◊indexR{композициональность!продолжений}
◊indexR{продолжения (continuations)!композициональность}
Получается~◊ic{5}, а~не~ожидаемое значение~◊ic{6}, как было~бы при композиции функций.
Дело в~том, что вызов продолжения означает отбрасывание всех последующих вычислений ради продолжения других вычислений.
Таким образом, вызов продолжения внутри ◊ic{foo} приводит к~вычислению значения ◊${\lambda u. 1 + u} при ◊${u = 4},
которое становится значением всего выражения, а~второй вызов ◊ic{foo} вообще не~происходит — он не~нужен,
ведь значение выражения уже вычислено и передано продолжению!
Именно такое поведение ожидается от обычных, не~частичных продолжений.
Обычные продолжения ◊term{активируются} и~полностью заменяют стек вызовов собой,
они~◊emph{не~вызываются} как функции.

Возможно, так будет понятнее.
В~◊ic{foo} мы сохранили ◊nobr{◊ic{(+ 1 [])}} — это всё, что ещё осталось вычислить.
Так как аргументы передаются по значению,
то~вычисление аргумента-продолжения в~◊nobr{◊ic{(foo (foo 4))}} фактически завершает вычисления,
отбрасывает ◊nobr{◊ic{(foo [])}} и возвращает значение формы ◊nobr{◊ic{(+ 1 4)}},
которое, очевидно, равно~◊ic{5}.

◊indexR{продолжения (continuations)!и интерактивная сессия}
◊indexR{интерактивная сессия (REPL)!продолжения}
◊indexE{REPL!продолжения}
Частичные продолжения представляют лишь часть оставшихся вычислений,
тогда как обычные продолжения — это ◊emph{все} оставшиеся вычисления.
В~статьях ◊cite{fwfd88,df90,hd90,qs91} разбирается идея частичных продолжений поддающихся композиции.
Предположим, с~◊ic{foo} связано продолжение ◊nobr{◊ic{[(+ 1 [])]}}, где внешние квадратные скобки означают, что оно ведёт себя как функция.
Тогда ◊nobr{◊ic{(foo (foo 4))}} будет эквивалентно уже ◊nobr{◊ic{(foo [(+ 1 [4])])}},
что превращается в~◊nobr{◊ic{(+ 1 5)}}, которое в~итоге даёт~◊ic{6}.
Захваченное продолжение ◊nobr{◊ic{[(+ 1 [])]}} определяет не~все последующие вычисления, которые когда-либо произойдут,
а~лишь их часть вплоть до момента возврата значения.
Для интерактивной сессии продолжением обычных продолжений является ◊term{главный цикл} (он~же ◊ic{toplevel}),
именно ему продолжения передают своё значение,
а~он выводит его на экран, читает следующее выражение из входного потока, вычисляет его, и~так~далее.
Продолжение частичных продолжений неизвестно,
именно поэтому они конечны и ведут себя как обычные функции —
ведь функции тоже не~знают, кому они вернут вычисляемое значение.

Давайте взглянем на наш пример с~◊nobr{◊ic{(set! foo k)}} с~другой стороны.
Оставим всё по-прежнему, но~объединим эти два выражения в~явную последовательность:

◊code:lisp{
(begin (+ 1 (call/cc (lambda (k) (set! foo k) 2)))
       (foo 3) )
}

Бабах!
Мы~получили бесконечный цикл, так как ◊ic{foo} оказывается связанной с~◊nobr{◊ic{(begin (+ 1 []) (foo 3))}},
что приводит к~рекурсии.
Как видим, главный цикл REPL — это не~только последовательное вычисление выражений.
Если мы хотим правильно его проэмулировать,
то~вдобавок необходимо изменять продолжение каждого вычисляемого в~главном цикле выражения:

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

Каждый раз, когда мы хотим вычислить выражение с~помощью ◊ic{toplevel},
его продолжение — ◊emph{продолжение} работы ◊ic{toplevel} — сохраняется в~переменной ◊ic{sequel}.
Любое продолжение, захватываемое внутри ◊ic{thunk}, теперь будет ограничено текущей вычисляемой формой.
Аналогичным образом применяя присваивание, можно сохранить любой срез стека в~виде частичного продолжения.
Как видим, при использовании неограниченных продолжений ◊ic{call/cc}
всегда требуется либо использовать побочные эффекты,
либо анализировать активируемые продолжения, чтобы избежать бесконечных циклов.

◊indexR{присваивание!роль для продолжений}
Частичные продолжения явно указывают, когда необходимо остановиться,
выполняя ровно столько вычислений, сколько нужно, но не~больше.
Мы вполне можем даже переписать нашу ◊ic{call/cc} так,
чтобы она захватывала именно частичные продолжения вплоть до~◊ic{toplevel}.
Естественно, кроме них потребуются также и переходы —
на~тот случай, когда мы действительно не~заинтересованы в~сохранении срезов стека.
Однако, с~другой стороны, частичные продолжения в~реальности используются довольно редко.
Сложно привести пример программы, где частичные продолжения были~бы действительно полезны
и~необходимы — то~есть, чтобы такую программу нельзя написать проще, не~используя продолжения вовсе.
Тем не~менее, частичные продолжения важны как ещё один пример управляющей конструкции,
которую можно реализовать на~Scheme с~помощью ◊ic{call/cc} и~присваивания.
