#lang pollen

◊subsection[#:label "escape/actors/ssect:functions"]{Функции}

Создавать функции легко и приятно, достаточно вызвать ◊ic{make-function}:

◊indexC{function}
◊indexC{evaluate-lambda}
◊code:lisp{
(define-class function value (variables body env))

(define (evaluate-lambda n* e* r k)
  (resume k (make-function n* e* r)) )
}

Реализовать вызов функций лишь чуточку сложнее.
Обратите внимание на неявную форму ◊ic{progn}/◊ic{begin} в~теле функций.

◊indexC{invoke!◊ic{function}}
◊code:lisp{
(define-method (invoke (f function) v* r k)
  (let ((env (extend-env (function-env f)
                         (function-variables f)
                         v* )))
    (evaluate-begin (function-body f) env k) ) )
}

Зачем передавать ◊ic{invoke} текущее окружение~◊ic{r}, если оно никак не~используется?
Так~сделано по~нескольким причинам.
Во-первых, многие реализации хранят текущее окружение в~специальном, постоянно доступном регистре.
Во-вторых, некоторым функциям окружение может потребоваться, например, для отладки
(мы~поговорим об~этом позже, когда будем рассматривать рефлексию).

Следующая функция расширяет окружение исполнения новыми переменными:

◊indexC{extend-env}
◊code:lisp{
(define (extend-env env names values)
  (cond ((and (pair? names) (pair? values))
         (make-variable-env
          (extend-env env (cdr names) (cdr values))
          (car names)
          (car values) ) )
        ((and (null? names) (null? values)) env)
        ((symbol? names) (make-variable-env env names values))
        (else (wrong "Arity mismatch")) ) )
}

Осталось определить только собственно применение функций.
Здесь надо помнить о~том, что функция применяется к~списку аргументов.

◊indexC{apply-cont}
◊indexC{argument-cont}
◊indexC{evaluate-application}
◊indexC{evaluate-arguments}
◊indexC{evfun-cont}
◊indexC{gather-cont}
◊indexC{no-more-arguments}
◊indexC{resume!◊ic{apply-cont}}
◊indexC{resume!◊ic{argument-cont}}
◊indexC{resume!◊ic{evfun-cont}}
◊indexC{resume!◊ic{gather-cont}}
◊code:lisp{
(define-class evfun-cont    continuation (e* r))
(define-class apply-cont    continuation (f  r))
(define-class argument-cont continuation (e* r))
(define-class gather-cont   continuation (v))

(define (evaluate-application e e* r k)
  (evaluate e r (make-evfun-cont k e* r)) )

(define-method (resume (k evfun-cont) f)
  (evaluate-arguments (evfun-cont-e* k)
                      (evfun-cont-r k)
                      (make-apply-cont (evfun-cont-k k)
                                       f
                                       (evfun-cont-r k) ) ) )
(define no-more-arguments '())

(define (evaluate-arguments e* r k)
  (if (pair? e*)
      (evaluate (car e*) r (make-argument-cont k e* r))
      (resume k no-more-arguments) ) )

(define-method (resume (k argument-cont) v)
  (evaluate-arguments (cdr (argument-cont-e* k))
                      (argument-cont-r k)
                      (make-gather-cont (argument-cont-k k) v) ) )

(define-method (resume (k gather-cont) v*)
  (resume (gather-cont-k k) (cons (gather-cont-v k) v*)) )

(define-method (resume (k apply-cont) v)
  (invoke (apply-cont-f k)
          v
          (apply-cont-r k)
          (apply-cont-k k) ) )
}

На~первый взгляд здесь всё слишком сложно — но только на~первый.
Вычисления выполняются слева направо,
первой вычисляется сама функция с~продолжением ◊ic{evfun-cont},
где с~помощью ◊ic{argument-cont} последовательно вычисляется весь список аргументов,
перемежаясь продолжениями ◊ic{gather-cont}, которые собирают список результатов.
Наконец, ◊ic{apply-cont} продолжает исполнение тела~функции.

◊; TODO: придумай красивное отображеине, а пока синоним:
◊; курсив выглядит *ужасно*
◊(define iv ii)

◊indexR{продолжения (continuations)!иллюстрация стека}
Давайте рассмотрим на примере, что происходит при вычислении ◊nobr{◊ic{(cons foo bar)}}.
Пусть переменная ◊ic{foo} имеет значение~◊iv{33}, а~◊ic{bar} содержит~◊iv{–77}.
Слева показано текущее вычисляемое выражение, а~справа — стек продолжений.
◊ii{k} — это текущее продолжение, а~◊ii{r} — текущее окружение.
Значение переменной~◊ic{cons} — функцию-конструктор точечных пар — будем записать как~◊iv{cons}.

◊; TODO: форматирование таблицы
◊; интересно, как эта выглядит на телефонах?
◊(define eval ii)
◊(define cont ii)
◊table{
   ◊tr{◊td{◊eval{evaluate} ◊ic{(cons foo bar)} ◊ii{r}}                                                                                             ◊td{◊ii{k}}}
   ◊tr{◊td{◊eval{evaluate} ◊ic{cons} ◊ii{r}}                                                               ◊td{◊cont{evfun-cont} ◊ic{(foo bar)} ◊ii{r} ◊ii{k}}}
   ◊tr{◊td{◊eval{resume} ◊iv{cons}}                                                                        ◊td{◊cont{evfun-cont} ◊ic{(foo bar)} ◊ii{r} ◊ii{k}}}
   ◊tr{◊td{◊eval{evaluate-arguments} ◊ic{(foo bar)} ◊ii{r}}                                                            ◊td{◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{evaluate} ◊ic{foo} ◊ii{r}}                                 ◊td{◊cont{argument-cont} ◊ic{(foo bar)} ◊ii{r} ◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{resume} ◊iv{33}}                                           ◊td{◊cont{argument-cont} ◊ic{(foo bar)} ◊ii{r} ◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{evaluate-arguments} ◊ic{(bar)} ◊ii{r}}                                     ◊td{◊cont{gather-cont} ◊iv{33} ◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{evaluate} ◊ic{bar} ◊ii{r}}             ◊td{◊cont{argument-cont} ◊ic{()} ◊ii{r} ◊cont{gather-cont} ◊iv{33} ◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{resume} ◊iv{–77}}                      ◊td{◊cont{argument-cont} ◊ic{()} ◊ii{r} ◊cont{gather-cont} ◊iv{33} ◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{evaluate-arguments} ◊ic{()} ◊ii{r}}            ◊td{◊cont{gather-cont} ◊iv{–77} ◊cont{gather-cont} ◊iv{33} ◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{resume} ()}                                    ◊td{◊cont{gather-cont} ◊iv{–77} ◊cont{gather-cont} ◊iv{33} ◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{resume} (◊iv{–77})}                                                        ◊td{◊cont{gather-cont} ◊iv{33} ◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{resume} (◊iv{33} ◊iv{–77})}                                                                           ◊td{◊cont{apply-cont} ◊ii{cons} ◊ii{k}}}
   ◊tr{◊td{◊eval{invoke} ◊iv{cons} (◊iv{33} ◊iv{–77})}                                                                                             ◊td{◊ii{k}}}
}
