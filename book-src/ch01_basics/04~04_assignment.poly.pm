#lang pollen

◊subsection[#:label "basics/evaluating-forms/ssect:assignment"]{Присваивание}

◊indexC{set!}
◊indexR{присваивание}
◊indexR{соглашения именования!побочных эффектов}
Как и во~многих других языках, в~нашем диалекте значения переменных можно изменять.
Изменение значения переменной называется ◊term{присваиванием}.
Так как значение переменной надо изменять в~её окружении,
то~мы поручаем эту задачу функции~◊ic{update!}.
◊footnote*{В~соответствии с~принятым в~Scheme соглашением,
имена функций с~побочными эффектами оканчиваются на восклицательный~знак.}
Её настоящая суть объясняется позже, в~разделе~◊pageref{assignment/implementation/ssect:environment}.

◊code:lisp{
... (case (car e)
      ((set!) (update! (cadr e) env
                       (evaluate (caddr e) env))) ... ) ...
}

Присваивание выполняется в~два шага:
сначала вычисляется новое значение, потом новое значение заменяет старое.
Стоит заметить, что обновлённая переменная не~является значением данной формы.
Мы~ещё вернёмся к~вопросу о~возвращаемом значении операции присваивания.
◊seePage{chapter:assignment}
Пока только запомните, что стандартом оно не~определено.