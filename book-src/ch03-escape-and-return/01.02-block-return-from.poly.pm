#lang pollen

◊subsection[#:label "escape/forms/ssect:block-return"]{Пара ◊ic{block}/◊ic{return-from}}

◊indexR{переходы (escapes)!лексические}
◊indexR{специальные формы!block@◊ic{block}}
Переходы, которые реализуют ◊ic{catch} и ◊ic{throw}, выполняются динамически.
Когда ◊ic{throw} запрашивает переход, она должна во~время исполнения программы отыскать соответствующую ◊ic{catch}-форму и её продолжение.
Затраты на поиск возможно сократить с~помощью ◊term{лексических меток}, как их называют в~◊|CommonLisp|.
Специальные формы ◊ic{block} и ◊ic{return-from} напоминают ◊ic{catch} и~◊ic{throw}.

◊; TODO: еще seealso + ◊ic, и ниже
◊indexC{block}
◊indexC{block|seealso{◊ic{return-from}}}
Форма ◊ic{block} имеет следующий синтаксис:

◊code:lisp{
(block ◊ii{метка} ◊ii{формы}...)
}

◊indexR{лексическое окружение!окружение меток}
◊indexR{окружение меток!лексическое}
◊noindent
Первый аргумент не~вычисляется и должен быть идентификатором.
Форма ◊ic{block} связывает текущее продолжение с~◊ii{меткой} в~◊term{лексическом окружении меток}.
Далее вычисляется тело ◊ic{block}, как в~◊ic{progn}, и последнее вычисленное значение становится значением всей формы~◊ic{block}.
Вычисления прерываются с~помощью ◊ic{return-from}.

◊indexC{return-from}
◊indexC{return-from|seealso{◊ic{block}}}
Форма ◊ic{return-from} имеет следующий синтаксис:

◊code:lisp{
(return-from ◊ii{метка} ◊ii{форма})
}

◊noindent
Первый аргумент тоже не~вычисляется и должен быть именем лексически видимой метки;
читай: ◊ic{return-from} может находиться только внутри ◊ic{block} с~одноимённой меткой,
как переменная может использоваться только внутри соответствующей ◊ic{lambda}-формы.
При вычислении ◊ic{return-from} соответствующий ◊ic{block} прерывается и возвращает значение ◊ii{формы}.

Лексические метки образуют новое пространство имён, чьи свойства описываются следующей таблицей:

◊envtable{
  ◊tr{◊td{Ссылка}      ◊td{◊ic{(return-from ◊ii{метка} ...)}}     }
  ◊tr{◊td{Значение}    ◊td{отсутствует, это объекты второго сорта}}
  ◊tr{◊td{Изменение}   ◊td{запрещено}                             }
  ◊tr{◊td{Расширение}  ◊td{◊ic{(block ◊ii{метка} ...)}}           }
  ◊tr{◊td{Определение} ◊td{запрещено}                             }
}

Перепишем
◊footnote{
  Нам пришлось воспользоваться ◊ic{letrec},
  так как в~Scheme запрещено располагать ◊ic{define} непосредственно внутри ◊ic{block},
  а~выражение ◊nobr{◊ic{(let () (define ...))}} некорректно из-за~◊ic{()}.
}
наш пример с~деревом на новый~лад:

◊indexC{find-symbol!с~переходами}
◊code:lisp{
(define (find-symbol id tree)
  (block find
    (letrec ((find (lambda (tree)
                     (if (pair? tree)
                         (or (find (car tree))
                             (find (cdr tree)) )
                         (if (eq? id tree)
                             (return-from find #t)
                             #f ) ) )))
      (find tree) ) ) )
}

Заметьте, мы не~просто поменяли все “◊ic{catch~'find}” на “◊ic{block~find}” —
нам потребовалось переместить тело функции внутрь ◊ic{block},
иначе ◊ic{return-from} не~находилась~бы в~лексической области видимости метки~◊ic{find}.

◊indexC{block!эффективность}
Форма ◊ic{block} допускает очень эффективную компиляцию.
Грубо говоря, ◊ic{block} связывает текущую высоту стека вызовов с~соответствующей меткой.
◊ic{return-from}~же надо лишь положить возвращаемое значение ◊ic{#t} туда, где его ждут (в~регистр, например),
после чего вернуть указатель верхушки стека в~положение, соответствующее метке ◊ic{find}.
Это всего лишь пара-тройка инструкций, а~не~поиск среди всех доступных меток, который устраивает ◊ic{catch}.
Различие станет более заметным, если попробовать реализовать ◊ic{catch}
◊footnote{
  Этот вариант ◊ic{catch} использует ◊ic{block} с~меткой ◊ic{label}.
  Естественно, система макросов должна обеспечивать гигиеничность имён
  (гарантировать отсутствие коллизий) и~для окружения меток.
}
с~помощью ◊ic{block}:

◊indexC{active-catchers@*active-catchers*}
◊indexC{catch!с~лексическими метками}
◊indexC{throw!c~лексическими метками}
◊code:lisp{
(define *active-catchers* '())

(define-syntax throw
  (syntax-rules ()
    ((throw tag value)
     (let* ((label tag)   ; вычисляется единожды
            (escape (assv label *active-catchers*)) ) ; узкое место
       (if (pair? escape)
           ((cdr escape) value)
           (wrong "No associated catch to" label) ) ) ) ) )

(define-syntax catch
  (syntax-rules ()
    ((catch tag . body)
     (let* ((saved-catchers *active-catchers*)
            (result (block label
                      (set! *active-catchers*
                            (cons (cons tag
                                        (lambda (x)
                                          (return-from label x) ) )
                                  *active-catchers* ) )
                      . body )) )
      (set! *active-catchers* saved-catchers)
      result ) ) ) )
}

◊; TODO: здесь был какой-то особый слеш между catch и throw, не нужен ли он также в других местах и заголовках?
Практически вся стоимость использования ◊ic{catch}/◊ic{throw} сосредоточена в~вызове ◊ic{assv}
◊footnote{Кстати, в~этом случае равенство меток проверяется предикатом ◊ic{eqv?}}
при раскрытии макроса ◊ic{throw}.
Рассмотрим, как работает эта реализация.
Глобальная переменная ◊ic{*active-catchers*} хранит все активные ◊ic{catch}-формы (исполнение которых ещё не~завершилось)
в~виде ◊nobr{А-списка} пар «метка — продолжение».
◊ic{*active-catchers*} обновляется при входе в~◊ic{catch} и последующем выходе оттуда (как нормальном, так и через ◊ic{throw}).
Этот список фактически соответствует динамическому окружению,
которым пользовались исходные ◊ic{catch} и ◊ic{throw} для обмена информацией о~метках.
