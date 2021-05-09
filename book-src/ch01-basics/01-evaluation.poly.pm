#lang pollen

◊section[#:label "basics/sect:evaluation"]{Вычисления}

◊indexC{eval}
◊indexR{вычислитель}
Важнейшая часть интерпретатора Лиспа сосредоточена в~одной функции, вокруг которой крутится все остальное.
Эта функция, называемая ◊ic{eval}, принимает на вход программу, а на выходе даёт результат её исполнения.
Явное наличие исполнителя кода отнюдь не~случайно, а, наоборот, является намеренно заложенной, характерной чертой~Лиспа.

◊indexR{язык!универсальный}
◊indexR{универсальный язык}
◊indexR{машина Тьюринга}
◊indexR{Тьюринга, машина}
Язык программирования называется ◊term{универсальным}, если он не~уступает в~выразительных возможностях машине~Тьюринга.
Так как машина~Тьюринга довольно примитивна, то несложно разработать язык, который~бы удовлетворял этому условию;
действительно, сложнее придумать полезный язык программирования, который~бы случайно не~оказался полным по~Тьюрингу.

◊indexR{тезис Чёрча}
◊indexR{Чёрча, тезис}
В~соответствии с~тезисом Чёрча, любая алгоритмически вычислимая функция может быть записана на любом тьюринг-полном языке.
Интерпретатор Лиспа можно представить как такую функцию, которая принимает программы и возвращает результаты их исполнения.
Так что если такая функция вообще существует, её возможно записать на любом тьюринг-полном языке.
Следовательно, функцию-вычислитель Лиспа ◊ic{eval} возможно записать в~частности на том~же самом~Лиспе.
Тут нет никаких противоречий: точно так~же, к~примеру, Фортран может быть реализован на~Фортране.

Но~что делает Лисп уникальным (и~оправдывает существование~◊ic{eval}), так это небольшой размер кода интерпретатора:
обычно от~одной до~двадцати страниц в~зависимости от детализации.
◊footnote{Интерпретатор, описываемый в~этой главе, занимает около 150~строк.}
Таков результат желания сделать язык последовательным, с~минимальным количеством исключений из правил,
и,~что~самое главное, с~простым, но выразительным синтаксисом.

◊indexC{eval!свойства}
Сам факт существования ◊ic{eval}, а также возможность её описания на Лиспе имеют несколько интересных следствий.

◊itemize{
  ◊item{
    Один из подходов к~изучению Лиспа — это прочитать руководство, где описываются все доступные функции языка.
    С~другой стороны, можно и непосредственно изучить функцию~◊ic{eval}.
    Второй подход сложен тем, что надо уже знать Лисп для того, чтобы понять описание ◊ic{eval}.
    Но~ведь знание Лиспа по~идее должно быть ◊emph{следствием} изучения ◊ic{eval}, а~не~◊emph{предпосылкой}!

    На~самом деле, достаточно знать лишь ту часть Лиспа, которая используется для описания ◊ic{eval}.
    Кроме того, язык, определяемый одной ◊ic{eval}, не~является всем~Лиспом:
    он есть лишь сутью языка, в~нём присутствуют только специальные формы и немного примитивных функций.

    Тем не~менее, в~возможности изучать язык двумя разными, но всё~же связанными путями лежит несомненный плюс~Лиспа.
  }
  ◊item{
    Тот факт, что ◊ic{eval} написана на Лиспе, значит также и то,
    что среда разработки является составной частью языка и не~требует значительных накладных расходов.
    Под средой разработки понимаются такие вещи как отладчик, трассировщик,
    возможность пошагового или обратного исполнения ◊cite{lie87}.
    Практически, реализация таких инструментов — это лишь доработка ◊ic{eval},
    к~примеру, чтобы она выводила сообщения при вызове функций,
    приостанавливала вычисления в~интересных местах, и~так~далее.

    Долгое время среда разработки с~такими возможностями была уникальной для Лиспа.
    Но и сегодня то, что ◊ic{eval} может быть описана на самом Лиспе,
    даёт возможность легко экспериментировать с~новыми вариантами реализации вычислений или~отладки.
  }
  ◊item{
    Наконец, сама по себе ◊ic{eval} способна быть инструментом программирования.
    Достаточно спорным инструментом,
    ведь использование ◊ic{eval} требует присутствия интерпретатора или компилятора в~исполнимом~коде;
    но~ещё более серьёзной проблемой является невозможность применения в~таком случае некоторых оптимизаций.
    Другими словами, использование ◊ic{eval} имеет свою цену.
    В~некоторых случаях её использование полностью оправдано,
    например, когда Лисп используется для описания и реализации метаязыков.

    Кроме ощутимой стоимости использования, семантика ◊ic{eval} часто неоднозначна.
    Именно поэтому функция ◊ic{eval} вообще не~входила в~стандарт до~ревизии~◊(RnRS)~◊cite{cr91b,kcr98}.
    ◊seePage{chapter:reflection}
  }
}