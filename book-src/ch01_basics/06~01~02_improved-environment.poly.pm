#lang pollen

◊subsubsection{Улучшенное окружение}

Хорошо, попробуем улучшить наше определение следующим~образом:

◊code:lisp{
(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend ◊ii{env.global} variables values)) ) )
}

Замечательно, теперь наши функции имеют доступ к~глобальному окружению и всем его функциям.
А~что если мы попробуем определить взаимно рекурсивные функции?
Также, какой результат даст программа слева (справа она~же с~раскрытыми макросами)?

◊; TODO: таблица

◊code:lisp{
(let ((a 1))
  (let ((b (+ 2 a)))
    (list a b) ) )
}

◊(is)

◊code:lisp{
((lambda (a)
   ((lambda (b)
          (list a b) )
    (+ 2 a) ) )
 1 )
}

Давайте рассмотрим по шагам, как вычисляется это~выражение:

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
                | ◊ii{env.global}
                }
}

Тело внутренней функции ◊ic{(lambda~(b) (list~a~b))} выполняется в~окружении,
полученном расширением глобального окружения переменной~◊ic{b}.
Всё верно.
Но~в~этом окружении нет необходимой переменной~◊ic{a}!
