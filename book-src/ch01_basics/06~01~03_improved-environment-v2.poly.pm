#lang pollen

◊subsubsection{Улучшенное окружение (вторая~попытка)}

Так как нам надо видеть переменную~◊ic{a} во~внутренней функции,
то~достаточно будет передать ◊ic{invoke} текущее окружение,
а~она в~свою очередь передаст его вызываемой функции.
Чтобы реализовать эту идею, надо немного подправить ◊ic{evaluate} и~◊ic{invoke};
чтобы не~путать эти определения с~предыдущими, пусть они начинаются на~◊ic{d.}:

◊indexC{d.evaluate}
◊indexC{d.invoke}
◊indexC{d.make-function}
◊code:lisp{
(define (d.evaluate e ◊ii{env})
  (if (atom? e) ...
      (case (car e)
        ...
        ((lambda) (d.make-function (cadr e) (cddr e) ◊ii{env}))
        (else     (d.invoke (d.evaluate (car e) env)
                            (evlis (cdr e) env)
                            env )) ) ) )

(define (d.invoke fn args env)
  (if (procedure? fn)
      (fn args env)
      (wrong "Not a function" fn) ) )

(define (d.make-function variables body ◊ii{def.env})
  (lambda (values ◊ii{current.env})
    (eprogn body (extend ◊ii{current.env} variables values)) ) )
}

В~этом определении стоит отметить,
что передача окружения определения ◊ic{env} через переменную~◊ii{def.env} бессмысленна,
так~как при вызове используется лишь текущее окружение ◊ii{current.env}.

◊indexR{стек!вызовов}
Давайте теперь ещё раз рассмотрим пример, приведённый выше.
Сейчас переменные не~пропадают:

◊code:lisp{
((lambda (a) ((lambda (b) (list a b)) (+ 2 a))) 1)◊where{
                                                  | ◊ii{env.global}
                                                  }
◊(eq) ((lambda (b) (list a b)) (+ 2 a))◊where{
                                       | a ◊(is) 1
                                       | ◊ii{env.global}
                                       }
◊(eq) (list a b)◊where{
                | b ◊(is) 3
                | a ◊(is) 1
                | ◊ii{env.global}
                }
}

Заодно мы явно видим ◊term{стек вызовов}:
каждая связывающая форма сначала укладывает свои новые переменные поверх текущего окружения,
а~затем убирает их оттуда после окончания вычислений.
