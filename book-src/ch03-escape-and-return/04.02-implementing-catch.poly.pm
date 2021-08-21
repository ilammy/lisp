#lang pollen

◊subsection[#:label "escape/implementation/ssect:catch"]{Реализация ◊ic{catch}}

◊indexC{catch!реализация}
◊indexC{throw!реализация}
Форма ◊ic{catch} по-своему интересна, так как требует разительно иного подхода, нежели форма~◊ic{block}, которую мы рассмотрим чуть позже.
Как~обычно, начнём с~добавления анализа ◊ic{catch} и~◊ic{throw} в~◊ic{evaluate}:

◊code:lisp{
...
((catch) (evaluate-catch (cadr e) (cddr e) r k))
((throw) (evaluate-throw (cadr e) (caddr e) r k))
...
}

Мы~решили сделать ◊ic{catch} и ◊ic{throw} специальными формами, как~в~◊|CommonLisp|,
а~не~функциями, принимающим замыкания.
Далее определим правила обработки формы~◊ic{catch}:

◊indexC{catch-cont}
◊indexC{labeled-cont}
◊indexC{evaluate-catch}
◊indexC{resume!◊ic{catch-cont}}
◊code:lisp{
(define-class catch-cont   continuation (body r))
(define-class labeled-cont continuation (tag))

(define (evaluate-catch tag body r k)
  (evaluate tag r (make-catch-cont k body r)) )

(define-method (resume (k catch-cont) v)
  (evaluate-begin (catch-cont-body k)
                  (catch-cont-r k)
                  (make-labeled-cont (catch-cont-k k) v) ) )
}

Как видите, ◊ic{catch} сначала вычисляет первый аргумент (метку)
и~связывает с~ней своё продолжение, создавая таким образом помеченный блок.
Затем последовательно вычисляются формы, составляющие тело~◊ic{catch}.
Нормальное продолжение этих вычислений просто передаёт значение продолжению всей формы ◊ic{catch}.
Форма ◊ic{throw}~же использует ◊ic{labeled-cont} по-другому:

◊indexC{throw-cont}
◊indexC{throwing-cont}
◊indexC{evaluate-throw}
◊indexC{catch-lookup}
◊indexC{resume!◊ic{throw-cont}}
◊indexC{resume!◊ic{throwing-cont}}
◊indexC{eqv?}
◊code:lisp{
(define-class throw-cont    continuation (form r))
(define-class throwing-cont continuation (tag cont))

(define (evaluate-throw tag form r k)
  (evaluate tag r (make-throw-cont k form r)) )

(define-method (resume (k throw-cont) tag)
  (catch-lookup k tag k) )

(define-generic (catch-lookup (k) tag kk)
  (wrong "Not a continuation" k tag kk) )

(define-method (catch-lookup (k continuation) tag kk)
  (catch-lookup (continuation-k k) tag kk) )

(define-method (catch-lookup (k bottom-cont) tag kk)
  (wrong "No associated catch" k tag kk) )

(define-method (catch-lookup (k labeled-cont) tag kk)
  (if (eqv? tag (labeled-cont-tag k))   ; внимание на компаратор
      (evaluate (throw-cont-form kk)
                (throw-cont-r kk)
                (make-throwing-cont kk tag k) )
      (catch-lookup (labeled-cont-k k) tag kk) ) )

(define-method (resume (k throwing-cont) v)
  (resume (throwing-cont-cont k) v) )
}

◊indexR{переходы (escapes)!вложенные}
Форма ◊ic{throw} вычисляет первый аргумент, затем ищет продолжение с~совпадающей меткой.
Если такого продолжения не~существует, то~◊ic{throw} сигнализирует об~ошибке.
Иначе вычисляется второй аргумент ◊ic{throw} и его значение передаётся найденному продолжению.
Однако передаётся не~напрямую, а~через ◊ic{throwing-cont}.
Дело в~том, что в~процессе вычисления значения тоже может возникнуть переход.
Если~бы продолжением здесь было ранее найденное продолжение,
то~вложенная форма ◊ic{throw} начинала~бы поиски ◊ic{catch} так, как будто~бы переход на внешнюю метку уже произошёл.
Но~это не~так, поэтому поиск следует вести от текущей формы ◊ic{throw},
что~и обеспечивается специальным промежуточным продолжением.
В~итоге, когда мы пишем:

◊code:lisp{
(catch 2
  (* 7 (catch 1
         (* 3 (catch 2
                (throw 1 (throw 2 5)) )) )) )
}

◊noindent
то~получаем ◊nobr{◊ic{(* 7 3 5)}}, а~не~◊ic{5}.

Кроме того, такая реализация ◊ic{throw} позволяет отлавливать больше ошибок.

◊code:lisp{
(catch 2 (* 7 (throw 1 (throw 2 3))))
}

◊noindent
Эта форма вернёт не~◊ic{3}, а~ошибку ◊nobr{◊ic{"No~associated catch"}},
потому что ◊ic{catch} с~меткой~◊ic{1} действительно не~существует.
