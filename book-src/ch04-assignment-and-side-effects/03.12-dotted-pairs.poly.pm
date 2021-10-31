#lang pollen

◊subsection[#:label "assignment/implementation/ssect:dotted-pairs"]{Точечные~пары}

Впервые в~нашем интерпретаторе Scheme точечные пары не~используют родные точечные пары Scheme, на~котором написан интерпретатор.

Благодаря ◊ic{allocate-pair}, функция ◊ic{cons} определяется элементарно:

◊indexC{cons}
◊code:lisp{
(defprimitive cons
  (lambda (v* s k)
    (allocate-pair (car v*) (cadr v*) s k) )
  2 )
}

◊noindent
С~чтением и модификацией тоже нет~проблем:

◊indexC{car}
◊indexC{set-cdr!}
◊code:lisp{
(defprimitive car
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (k (s ((car v*) 'car)) s)
        (wrong "Not a pair" (car v*)) ) )
  1 )

(defprimitive set-cdr!
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (let ((pair (car v*)))
          (k pair ((pair 'set-cdr) s (cadr v*))) )
        (wrong "Not a pair" (car v*)) ) )
  2 )
}

Так~как все объекты отвечают на~сообщение ◊ic{type},
написать предикат ◊ic{pair?} также не~составляет труда.
Надо лишь не~забыть перевести результат сравнения типов во~внутреннее представление булевых значений:

◊indexC{pair?}
◊code:lisp{
(defprimitive pair?
  (lambda (v* s k)
    (k (create-boolean (eq? ((car v*) 'type) 'pair)) s) )
  1 )
}
