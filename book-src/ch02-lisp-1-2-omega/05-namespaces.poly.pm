#lang pollen

◊section[#:label "lisp1-2-omega/sect:namespaces"]{Пространства имён}

◊indexR{пространства имён}
◊term{Окружения} устанавливают соответствия между именами и объектами.
На~данный момент нам известны два типа окружений: обычное окружение ◊ic{env} и функциональное ◊ic{fenv}.
Мы~разделили окружения с~целью ускорить вызовы функций и отделить функции от переменных.
В~результате мы получили язык с~более сложной семантикой:
каждое окружение имеет свой отдельный интерпретатор,
появляются специальные механизмы для переноса объектов между окружениями.
При обсуждении динамических переменных мы упомянули, что современные диалекты Лиспа
(вроде ILOG~Talk, ◊(EuLisp), ◊(ISLisp))
обрабатывают динамические переменные схожим образом,
помещая их в отдельное ◊term{пространство~имён}.
Рассмотрим эту языковую вариацию подробнее.

◊(bigskip)

◊indexR{привязки (bindings)}
◊indexR{привязки (bindings)|seealso{связывание}}
◊indexR{привязки (bindings)!захват}
◊indexR{объекты!второго класса}
Окружения можно считать абстрактным типом данных,
коллекцией ◊term{привязок} (◊english{bindings}) объектов к~соответствующим именам.
Объекты могут быть значениями — полноценными объектами, которые можно передавать, копировать, присваивать, и~т.~д.
Окружения могут содержать и сущности — объекты второго сорта, которыми программа может оперировать лишь посредством их имён,
с~помощью ограниченного числа специальных форм или иных синтаксических конструкций.
Сейчас нам известен только один тип подобных сущностей — сами привязки.
Они существуют как понятие в~программе — это нечто, захватываемое замыканиями, — но взаимодействовать с~ними напрямую нельзя.
Позже мы рассмотрим свойства привязок подробнее, когда будем изучать побочные эффекты.

Окружения поддерживают множество полезных операций.
Например, можно проверить, используется ли какое-то имя в~окружении;
найти объект по его имени; заменить этот объект другим.
Окружения также можно расширять, добавляя новые привязки в~текущее, локальное или глобальное окружение.
Конечно~же, не~все возможности необходимы для каждого из окружений.
В~действительности, многие окружения полезны именно благодаря накладываемым ими ограничениям.
Следующая таблица показывает особенности окружения переменных Scheme:

◊envtable{
  ◊tr{◊td{Ссылка}      ◊td{◊ii{x}}                            }
  ◊tr{◊td{Значение}    ◊td{◊ii{x}}                            }
  ◊tr{◊td{Изменение}   ◊td{◊ic{(set! ◊ii{x} ...)}}            }
  ◊tr{◊td{Расширение}  ◊td{◊ic{(lambda (... ◊ii{x} ...) ...)}}}
  ◊tr{◊td{Определение} ◊td{◊ic{(define ◊ii{x} ...)}}          }
}

Мы будем использовать такие таблицы и понятия из них довольно часто при обсуждении свойств окружений, так что остановимся на них подробнее.
Первая строка показывает синтаксис, используемый для получения ссылки на переменную, обращения к~ней.
Вторая строка показывает, как получить значение переменной;
в~данном случае синтаксис совпадает, но так бывает отнюдь не~всегда.
Третья строка показывает, как связать переменную с~другим значением.
Четвёртая строка показывает, как расширить окружение локальных переменных новой привязкой:
с~помощью ◊ic{lambda}-формы или, конечно~же, макросов вроде ◊ic{let} или ◊ic{let*}, раскрывающихся в~◊ic{lambda}-формы.
Наконец, последняя строка показывает, как определить глобальную переменную.

Подобные различия могут показаться вам чересчур тонкими, даже избыточными.
Собственно, этому вопросу и посвящена глава:
какие различия можно заметить в~окружениях, чем они могут быть полезны и когда необходимы.

Например, пространство имён функций нашего ◊(Lisp2) описывается следующей таблицей:

◊envtable{
  ◊tr{◊td{Ссылка}      ◊td{◊ic{(◊ii{f} ...)}}                     }
  ◊tr{◊td{Значение}    ◊td{◊ic{(function ◊ii{f})}}                }
  ◊tr{◊td{Изменение}   ◊td{запрещено}                             }
  ◊tr{◊td{Расширение}  ◊td{◊ic{(flet (... (◊ii{f} ...) ...) ...)}}}
  ◊tr{◊td{Определение} ◊td{не~рассматривалось (◊ic{defun})}       }
}
