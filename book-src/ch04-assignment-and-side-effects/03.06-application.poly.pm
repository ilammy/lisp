#lang pollen

◊subsection[#:label "assignment/implementation/ssect:application"]{Аппликация}

Перед применением функции необходимо вычислить все элементы формы~вызова.
Вычислять их можно, как~известно, в~произвольном порядке.
Пусть это будет порядок слева~направо.

◊indexC{evaluate-application}
◊code:lisp{
(define (evaluate-application e e* r s k)
  (define (evaluate-arguments e* r s k)
    (if (pair? e*)
        (evaluate (car e*) r s
          (lambda (v ss)
            (evaluate-arguments (cdr e*) r ss
              (lambda (v* sss)
                (k (cons v v*) sss) ) ) ) )
        (k '() s) ) )
  (evaluate e r s
    (lambda (f ss)
      (evaluate-arguments e* r ss
        (lambda (v* sss)
          (if (eq? (f 'type) 'function)
              ((f 'behavior) v* sss k)
              (wrong "Not a function" f) ) ) ) ) ) )
}

◊indexE{behavior@◊ic{'behavior}}
Здесь функция, которая обычно называлась ◊ic{evlis}, является локальной и называется ◊ic{evaluate-arguments}.
Она~вычисляет аргументы слева направо и собирает их в~список.
Затем функция (первый~элемент~формы) применяется к~аргументам вместе с~состоянием памяти после их вычисления и продолжением вызова функции.
Обратите внимание на~то, как~лаконично и точно записано определение.

Функции — это полноценные значения, которые представляются специальными замыканиями,
отвечающими на~сообщения вроде ◊ic{boolify}, ◊ic{type},~и~т.~п.
Для~функций мы~определим новое сообщение — ◊ic{behavior}, — которое возвращает поведение функции,
то~есть вычисления, которые следует выполнить.
