#lang pollen

◊exanswer{basics/ex:tracer}

Просто поместите трассирующий код в~соответствующее место — обработку вызовов функций:

◊indexC{tracing.evaluate}
◊code:lisp{
(define (tracing.evaluate exp env)
  (if ...
      ...
      (case (car exp)
        ...
        (else (let ((fn   (evaluate (car e) env))
                    (args (evlis (cdr e) env)) )
                (display `(calling ,(car e) with . ,args)
                         *trace-port* )
                (let ((result (invoke fn args)))
                  (display `(returning from ,(car e) with ,result)
                           *trace-port* )
                  result ) )) ) ) )
}

Обратите внимание на два момента.
Во-первых, трассировка испольует ◊emph{имя} функции, а~не~её значение:
так сообщения получаются гораздо понятнее.
Во-вторых, ◊ic{display} осуществляет вывод в~настраиваемый ◊ic{*trace-port*}.
Это облегчает перенаправление результатов трассировки в~специальное окно,
или в~лог-файл, или в~стандартный поток вывода.


◊exanswer{basics/ex:excess-recursion}

◊cite{wan80b} приписывает изобретение этой оптимизации Дэниелу~Фридмену и Дэвиду~Уайзу.
Для начала, избавимся от бессмысленного вычисления выражения ◊ic{(evlis '() env)} в~конце последовательности.
Кроме того, окружение ◊ic{env} неизменно в~процессе вычислений,
поэтому его можно не~включать лишний раз в~список аргументов рекурсивно вызываемой функции.
◊ic{evlis} обычно имеет дело со~списками длиной порядка трёх-четырёх элементов.
Убрать бесполезные ◊nobr{20◊(thinsp)%}~вызовов ◊ic{evaluate} и ◊ic{pair?} — вполне неплохая оптимизация.

◊indexC{evlis}
◊code:lisp{
(define (evlis exps env)
  (define (evlis exps)
    ;; (assume (pair? exps))
    (if (pair? (cdr exps))
        (cons (evaluate (car exps) env)
              (evlis (cdr exps)) )
        (list (evaluate (car exps) env)) ) )
  (if (pair? exps) (evlis exps) '()) )
}


◊exanswer{basics/ex:new-extend}

Такое представление окружений известно как ◊term{гирлянда} ввиду очевидного визуального сходства.
Здесь используется меньше точечных пар — ценой некоторого замедления поиска и модификации значений переменных.
Кроме того, такая реализация ◊ic{extend} делает невозможной проверку арности для функций с~точечным аргументом.

◊indexC{lookup}
◊code:lisp{
(define (lookup id env)
  (if (pair? env)
      (let look ((names  (caar env))
                 (values (cdar env)) )
        (cond ((symbol? names)
               (if (eq? names id)
                   values
                   (lookup id (cdr env)) ) )
              ((null? names)
               (lookup id (cdr env)) )
              ((eq? (car names) id)
               (if (pair? values) (car values)
                   (wrong "Too few values") ) )
              (else (if (pair? values)
                        (look (cdr names) (cdr values))
                        (wrong "Too few values") )) ) )
      (wrong "No such binding" id) ) )
}

◊noindent
Реализация ◊ic{update!} аналогична по структуре.


◊exanswer{basics/ex:racks}

◊code:lisp{
(define (s.make-function variables body env)
  (lambda (values current.env)
    (for-each (lambda (var val)
                (putprop var 'apval (cons val (getprop var 'apval))) )
              variables values )
    (let ((result (eprogn body current.env)))
      (for-each (lambda (var)
                  (putprop var 'apval (cdr (getprop var 'apval))) )
                variables )
      result ) ) )

(define (s.lookup id env)
  (car (getprop id 'apval)) )

(define (s.update! id env value)
  (set-car! (getprop id 'apval) value) )
}


◊exanswer{basics/ex:liar-liar!}

Так как эта проблема касается не~только~◊ic{<},
имеет смысл написать макрос, определяющий предикаты правильно.
Обратите внимание, что для языка реализации — Scheme — значение ◊ic{the-false-value} является логической~◊term{истиной}.

◊indexC{defpredicate}
◊code:lisp{
(define-syntax defpredicate
  (syntax-rules ()
    ((defpredicate name value arity)
     (defprimitive name
      (lambda values (or (apply value values) the-false-value))
      arity ) ) ) )

(defpredicate < < 2)
}


◊exanswer{basics/ex:def-list}

Главная сложность здесь в~том, что ◊ic{list} — это функция с~переменной арностью,
а~наш интерпретатор не~поддерживает такой синтаксис.
Но~если хорошенько подумать и вспомнить, как в~интерпретаторе представляются примитивы,
то в~голову приходит отличная мысль:

◊indexC{list}
◊code:lisp{
(definitial list
  (lambda (values) values) )
}


◊exanswer{basics/ex:def-call/cc}

Естественно, наиболее простой способ определить ◊ic{call/cc} — это использовать ◊ic{call/cc}.
Хитрость здесь в~том, чтобы вложенная ◊ic{call/cc} правильно вызвала передаваемую ей функцию определяемого Лиспа.
Для этого её надо вручную заворачивать в~◊ic{invoke}:

◊indexC{call/cc}
◊code:lisp{
(defprimitive call/cc
  (lambda (f)
    (call/cc (lambda (g)
               (invoke f
                (list (lambda (values)
                        (if (= (length values) 1)
                            (g (car values))
                            (wrong "Incorrect arity" g) ) )) ) )) )
  1 )
}


◊exanswer{basics/ex:def-apply}

Здесь возникает та~же проблема, что и с~◊ic{call/cc}:
приходится работать одновременно в~двух мирах с~двумя различными языками.
Кроме того, функция ◊ic{apply} имеет переменную арность,
так что вдобавок требуется правильно собрать в~список аргументы вызываемой функции
(и~учесть, что у~неё тоже может быть точечный аргумент).

◊indexC{apply}
◊code:lisp{
(definitial apply
  (lambda (values)
    (if (>= (length values) 2)
        (let ((f (car values))
              (args (let flat ((args (cdr values)))
                      (if (null? (cdr args))
                          (car args)
                          (cons (car args) (flat (cdr args))) ) )) )
          (invoke f args) )
        (wrong "Incorrect arity" 'apply) ) ) )
}


◊exanswer{basics/ex:def-end}

Просто захватите продолжение вызова интерпретатора и свяжите его с~◊ic{end}.
(К~счастью, в~Scheme синтаксис активации продолжений не~отличается от вызова~функций.)

◊code:lisp{
(define (chapter1-scheme)
  (define (toplevel)
    (display (evaluate (read) env.global))
    (toplevel) )
  (call/cc (lambda (end)
             (defprimitive end end 1)
             (toplevel) )) )
}


◊exanswer{basics/ex:slowpoke}

Конечно, результаты подобных сравнений одновременно зависят как от используемой реализации, так и от рассматриваемых программ.
Но в~среднем можно сказать, что разница в~быстродействии будет порядка 5–15~раз.
◊seeCite{itw86}

Главной задачей этого упражнения было подтолкнуть вас к~осознанию того,
что ◊ic{evaluate} написана на очень простом и ограниченном диалекте~Лиспа,
поэтому с~равным успехом может быть исполнена как интерпретатором Scheme,
так и интерпретатором того языка, который сама~же определяет.


◊exanswer{basics/ex:no-gensym}

◊emph{Список} упорядоченных выражений всегда можно представить в~виде упорядоченной пары из выражения и~списка.
Таким образом, достаточно будет показать, как последовательно вычислить два выражения.
Идея состоит в~хитроумном использовании замыканий для разрешения возможных конфликтов~имён.

◊indexC{begin}
◊; TODO: сделать из этого таблицу
◊code:lisp{
(begin ◊${◊math-ii{выражение}_1} ◊${◊math-ii{выражение}_2})
}
◊(eq)
◊code:lisp{
((lambda (void other) (other))
 ◊${◊math-ii{выражение}_1}
 (lambda () ◊${◊math-ii{выражение}_2}) )
}
