#lang pollen

◊section[#:label "lisp1-2-omega/sect:lisp1" #:alt "Lisp-1"]{◊Lisp-1}

◊indexE{Lisp-1@◊Lisp-1}
◊indexR{Лисп!Lisp-1@◊Lisp-1}
В~предыдущей главе мы реализовали функциональный язык программирования:
функции являются полноценным объектом языка (их~конструктор — ◊ic{make-function});
в~процессе вычислений мы не~различали функции и аргументы:
при вычислении формы аппликации не~было никакой разницы между вычислением элемента-◊emph{функции}
и~вычислением элементов, считающихся ◊emph{параметрами} функции.
Давайте посмотрим на интерпретатор из предыдущей~главы:

◊code:lisp{
(define (evaluate e env)
  (if (atom? e) ...
      (case (car e)
        ...
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else (invoke (evaluate (car e) env)
                      (evlis (cdr e) env) )) ) ) )
}

Самое важное в~нём:

◊enumerate{
  ◊item{
    ◊ic{lambda} создаёт полноценные объекты — замыкания, —
    которые сохраняют окружение своего определения.
  }
  ◊item{
    При вызове функции всё вычисляется одинаковым образом: с~помощью ◊ic{evaluate};
    ◊ic{evlis} всего лишь вызывает ◊ic{evaluate} для всех элементов списка.
  }
}

Благодаря этим двум качествам Scheme считается представителем семейства~◊|Lisp-1|.
