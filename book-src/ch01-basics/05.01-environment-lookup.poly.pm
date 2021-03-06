#lang pollen

◊subsection*{Поиск в~окружении}

Функция ◊ic{lookup} определяется элементарно:

◊indexC{lookup}
◊code:lisp[#:chunk "lookup"]{
(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)) )
      (wrong "No such binding" id) ) )
}

Тут мы видим второй тип
◊footnote{Первый — это синтаксические ошибки (см.~◊pageref{basic/atoms/para:the-first-error}).}
возможных ошибок, возникающих при попытке узнать значение неизвестной переменной.
Мы опять лишь вызовем ◊ic{wrong}, чтобы сообщить о~проблеме куда~следует.

◊indexR{автоцитирование}
Когда компьютеры были большими, а память была маленькой,
◊footnote{
  Память (вместе с~подсистемами ввода-вывода) всё ещё остаётся одной из наиболее дорогих частей компьютера, хоть и постоянно дешевеет.
}
для переменных часто применялось ◊term{автоцитирование}.
Если с~переменной не~было связано какое-либо значение, то этим значением становился символ с~именем переменной.
Было~бы очень обидно видеть, как понятия переменной и символа, которые мы так усердно разделяли, опять смешиваются и перепутываются.

Хотя это несомненно удобно — никогда не~совершать ошибок,
но если программа замалчивает ошибки программиста — она не~становится более корректной.
Наоборот, ошибки должны быть обнаружены как можно раньше, чтобы как можно быстрее их исправить.
Следовательно, использование автоцитирования — плохое решение,
потому что оно скрывает некоторые ошибки, которые могли~бы быть исправлены раньше.
