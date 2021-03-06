#lang pollen

◊section[#:label "basics/sect:evaluating-forms"]{Вычисляем формы}

◊indexR{специальные формы}
◊indexR{форма!специальная}
◊indexR{Лисп!специальные формы}
Каждый язык имеет некоторое количество «неприкасаемых» синтаксических конструкций:
их~нельзя программно переопределить, а~если и~можно, то всё равно не~стоит.
В~Лиспе такие конструкции называются ◊term{специальными формами}.
Они представляются списками, где первый элемент — это определённый символ,
принадлежащий множеству ◊term{специальных операторов}
(или синтаксических ключевых слов, как их называют в~Scheme).

◊indexR{функции!примитивы}
◊indexR{примитивы}
◊indexR{диалекты Лиспа}
Конкретный диалект Лиспа характеризуется набором специальных форм и библиотекой примитивных функций
— эти функции нельзя определить на самом диалекте, так как они тесно связаны с~реализацией;
например, для Scheme это~◊ic{call/cc}.

В~некотором понимании, Лисп является лишь прикладным ◊${\lambda}-исчислением
вместе с~расширяющим его набором специальных форм.
Специфика каждого конкретного диалекта Лиспа лежит только в~этом наборе.
В~Scheme используется минимальный набор специальных операторов
(◊ic{quote}, ◊ic{if}, ◊ic{set!}, ◊ic{lambda}),
тогда как ◊CommonLisp ◊seeCite{ste90} определяет более тридцати,
описывая таким образом конструкции, которые компилируются в~эффективный машинный~код.

Так как специальные формы представляются списками, то их синтаксический анализ прост:
достаточно одного ◊ic{case}-выражения, разбирающего первый элемент списка.
Если форма начинается не~со специального оператора, то она означает применение функции.
В~данный момент мы ограничимся лишь небольшим подмножеством специальных форм:
◊ic{quote}, ◊ic{if}, ◊ic{begin}, ◊ic{set!}, ◊ic{lambda}.
(В~следующих главах мы введём другие, более специализированные формы.)

◊indexC{evaluate}
◊code:lisp[#:chunk "evaluate"]{
(define (evaluate e env)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e)(string? e)(char? e)
                 (boolean? e)(vector? e) ) e)
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (evaluate (cadr e) env)
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env) ))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env) )) ) ) )
}

◊indexR{синтаксис!if@◊ic{if}}
Чтобы упростить определение, синтаксический анализ оставлен минимальным:
мы не~проверяем, правильно~ли записаны цитаты, действительно~ли ◊ic{if} передано три аргумента,
◊footnote{
  ◊ic{if}~не~обязательно принимает условие и ровно две альтернативы.
  К~примеру, в~Scheme и ◊CommonLisp ◊ic{if}~может принимать как три, так и два аргумента;
  ◊ic{if}~в~◊|EuLisp| и ◊|ISLisp| исключительно тернарный;
  ◊|LeLisp|~в~случае, если условие ложно, вычисляет остаток формы~◊ic{if}, обернув его в~◊ic{begin}.
}
и~так~далее.
Мы~априори считаем интерпретируемые программы синтаксически корректными.
