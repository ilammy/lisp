#lang pollen

◊; TODO: это очень кривое название
◊subsection[#:label "escape/forms/ssect:protection"]{Защитные формы}

◊indexC{unwind-protect}
◊indexC{car}
◊indexC{cdr}
Осталось рассмотреть ещё один интересный эффект, связанный со~специальной формой ◊ic{unwind-protect}.
Названием она обязана принципу первой реализации
◊footnote{
  Ситуация аналогична ◊ic{car} и~◊ic{cdr}, которые на~самом деле являются акронимами:
  «◊english{contents of address register}» и~«◊english{contents of decrement register}».
  Оригинальная реализация ◊LISP-1.5 использовала эти регистры IBM~704 для точечных~пар.
  Естественно, они не~имеют ничего общего с~текущими реализациями, но~названия приклеились.
}
и~задуманному функциональному назначению.
Используется ◊ic{unwind-protect} следующим образом:

◊code:lisp{
(unwind-protect ◊ii{форма}
  ◊ii{финальные-формы}... )
}

Сначала вычисляется ◊ii{форма} — её значение станет значением всей формы ◊ic{unwind-protect}.
Затем выполняются ◊ii{финальные-формы}, а~уже потом ◊ic{unwind-protect} возвращает вычисленное ранее значение.
В~◊CommonLisp существует очень похожая форма ◊ic{prog1}.
Некоторые реализации Scheme называют~её ◊ic{begin0}.
Все эти формы выполняют последовательные вычисления подобно ◊ic{begin},
но~возвращают значение первой вычисленной формы, а~не~последней.
Форма ◊ic{unwind-protect} особенна тем, что всегда вычисляет ◊ii{финальные-формы} —
даже если ◊ii{форма} прерывается переходом:

◊code:lisp[#:dialect CommonLisp]{
(let ((a 'on))
  (cons (unwind-protect (list a)
          (setq a 'off) )
        a ) )                         ⟹ ((on) . off)

(block foo
  (unwind-protect (return-from foo 1)
    (print 2) ) )                     ⟹ 1     ; и печатает 2
}

◊ic{unwind-protect} полезна, когда состояние системы должно быть восстановлено вне~зависимости от результата производимых действий.
Классический пример — работа с~ресурсами:
открытый файл обязательно необходимо закрыть, даже если в~процессе работы происходит ошибка.
Другим примером является эмуляция ◊ic{catch} с~помощью ◊ic{block}.
Если эти формы используются одновременно, то возможна рассинхронизация состояния ◊ic{*active-catchers*}.
Недостаток можно исправить с~помощью ◊ic{unwind-protect}, гарантируя восстановление значения ◊ic{*active-catchers*}:

◊indexC{catch!с~помощью ◊ic{unwind-protect}}
◊code:lisp{
(define-syntax catch
  (syntax-rules ()
    ((catch tag . body)
     (let ((saved-catchers *active-catchers*))
       (unwind-protect
         (block label
           (set! *active-catchers*
                 (cons (cons tag (lambda (x) (return-from label x)))
                       *active-catchers*) )
           . body )
         (set! *active-catchers* saved-catchers) ) ) ) ) )
}

Теперь формы ◊ic{catch} и ◊ic{block} можно вкладывать друг в~друга не~опасаясь,
что ◊ic{catch} забудет восстановить значение ◊ic{*active-catchers*} из-за выполняемого ◊ic{return-from}.
Форма ◊ic{unwind-protect} защищает реализацию от подобной ошибки.
Правда, реализация всё ещё не~идеальна:
прямой доступ к~глобальной переменной ◊ic{*active-catchers*} позволяет случайно или намеренно вмешаться в~работу макросов ◊ic{catch} и~◊ic{throw}.

Форма ◊ic{unwind-protect} обеспечивает соблюдение инвариантов в~системе,
гарантируя выполнение определённых действий после завершения вычислений.
Соответственно, эта форма обязана знать, ◊emph{когда} вычисления завершаются.
Как вы помните, подобная определённость возможна только если продолжения обладают динамическим временем жизни.
Поэтому ◊ic{unwind-protect}
◊footnote{
  Корректно работающий аналог ◊ic{unwind-protect} для Scheme — ◊ic{dynamic-wind} — описан в~◊cite{fwh92}.
  См.~также~◊cite{que93c}.
}
не~очень хорошо сочетается с~◊ic{call/cc} и неограниченным временем жизни продолжений.

◊; TODO: последовательно используй термины: "управляющие конструкции", не формы, не структуры -- конструкции
Как мы уже не~раз говорили, точный смысл управляющих конструкций определить непросто.
Рассмотрим лишь несколько каверзных примеров:

◊code:lisp{
(block foo
  (unwind-protect (return-from foo 1)
    (return-from foo 2) ) )                          ⟹ ?

(catch 'bar
  (block foo
    (unwind-protect (return-from foo (throw 'bar 1))
      (throw 'something (return-from foo 2)) ) ) )   ⟹ ?
}

◊; TODO: аналогично: "поток исполнения", не выполнения, не инструкций -- исполнения
◊indexC{unwind-protect!проблемы семантики}
Приведённые ранее описания формы ◊ic{unwind-protect} ничего не~говорят о~подобных крайних случаях.
Здесь пригодилось~бы строгое, формальное определение семантики и взаимодействия с~другими конструкциями языка.
В~то~же время, нельзя игнорировать очевидную неопределённость, привносимую продолжениями в~понятие «после вычислений».
Продолжения по определению ◊emph{динамичны}, так как являются воплощением потока исполнения.
Рассмотрим следующий пример:

◊; TODO: пример странный всё же, может сверь со вторым изданием (на французском?)
◊code:lisp{
(block bar
  (unwind-protect (return-from bar 1)
    (block foo ...) ) )
}

◊phantomlabel{escape/forms/protection/p:discard}
◊noindent
◊ic{unwind-protect} вклинивается в~поток исполнения и не~даёт завершить переход в~◊ic{bar}, который выполняет охраняемая форма.
Теперь этот переход становится продолжением вложенной формы ◊nobr{◊ic{(block foo ...)}}.
Если она просто вернёт результат, то продолжение активируется
и форма ◊nobr{◊ic{(block bar ...)}} передаст~◊ic{1} своему продолжению.
Если~же ◊nobr{◊ic{(block foo ...)}} сама прерывается переходом, то её исходное продолжение отбрасывается,
а~переход ◊nobr{◊ic{(return-from bar 1)}} никогда не~выполняется.
(Мы~обсудим этот феномен подробнее, когда будем разбирать реализацию формы~◊ic{unwind-protect}.)

◊bigskip

◊; TODO: ◊indexR{управляюшие формы!выразительность}
Конечно~же существуют и другие управляющие конструкции.
Особенно их жалует ◊|CommonLisp|, где реализована даже старая форма ◊ic{prog} в~виде ◊ic{tagbody} и~◊ic{go};
она~легко эмулируется ◊seeEx{escape/ex:tagbody} с~помощью ◊ic{labels} и~◊ic{block}.
Интересный факт: в~языках с~динамическими продолжениями
для реализации любой управляющей формы достаточно лишь ◊ic{block}, ◊ic{return-from} и~◊ic{unwind-protect}.
Аналогично, в~Scheme достаточно одной лишь ◊ic{call/cc} и неограниченных продолжений.
Очевидно, что ◊ic{call/cc} невозможно реализовать, имея лишь продолжения с~динамическим временем жизни.
Обратное~же вполне возможно, хоть это и стрельба из пушки по воробьям.
Мы~разберём этот вопрос после реализации интерпретатора с~явными продолжениями.
