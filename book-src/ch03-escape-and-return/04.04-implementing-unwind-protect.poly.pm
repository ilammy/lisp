#lang pollen

◊subsection[#:label "escape/implementation/ssect:unwind-protect"]{Реализация ◊ic{unwind-protect}}

◊indexC{unwind-protect!реализация}
Форма ◊ic{unwind-protect} является самой сложной для реализации.
Нам также придётся изменить определения форм ◊ic{catch} и ◊ic{block},
чтобы они вели себя правильно, когда находятся внутри ◊ic{unwind-protect}.
Форма ◊ic{unwind-protect} — это хороший пример возможности,
которая требует полного переосмысления существующих концепций.
Однако отсутствие ◊ic{unwind-protect} приводит к~другим сложностям в~будущем, так что оно того стоит.

Начнём с~определения поведения самой формы ◊ic{unwind-protect}
(которая, как мы уже говорили, мало чем отличается от~◊ic{prog1}):

◊indexC{unwind-protect-cont}
◊indexC{protect-return-cont}
◊indexC{evaluate-unwind-protect}
◊indexC{resume!◊ic{unwind-protect-cont}}
◊indexC{resume!◊ic{protect-return-cont}}
◊code:lisp{
(define-class unwind-protect-cont continuation (cleanup r))
(define-class protect-return-cont continuation (value))

(define (evaluate-unwind-protect form cleanup r k)
  (evaluate form r
            (make-unwind-protect-cont k cleanup r) ) )

(define-method (resume (k unwind-protect-cont) v)
  (evaluate-begin (unwind-protect-cont-cleanup k)
                  (unwind-protect-cont-r k)
                  (make-protect-return-cont
                   (unwind-protect-cont-k k) v ) ) )

(define-method (resume (k protect-return-cont) v)
  (resume (protect-return-cont-k k) (protect-return-cont-value k)) )
}

Далее необходимо доработать ◊ic{catch} и ◊ic{block},
чтобы они выполняли действия, предписанные ◊ic{unwind-protect},
даже в~случае выхода из них с~помощью ◊ic{throw} или ◊ic{return-from}.
Для~◊ic{catch} необходимо изменить обработку ◊ic{throwing-cont}:

◊indexC{resume!◊ic{throwing-cont}}
◊code:lisp{
(define-method (resume (k throwing-cont) v)
  (unwind (throwing-cont-k k) v (throwing-cont-cont k)) )
}

◊noindent
И~научить ◊ic{unwind} выполнять сохранённые действия в~процессе обхода стека:

◊indexC{unwind-cont}
◊indexC{unwind!◊ic{unwind-protect-cont}}
◊indexC{resume!◊ic{unwind-cont}}
◊code:lisp{
(define-class unwind-cont continuation (value target))

(define-method (unwind (k unwind-protect-cont) v target)
  (evaluate-begin (unwind-protect-cont-cleanup k)
                  (unwind-protect-cont-r k)
                  (make-unwind-cont
                   (unwind-protect-cont-k k) v target ) ) )

(define-method (resume (k unwind-cont) v)
  (unwind (unwind-cont-k k)
          (unwind-cont-value k)
          (unwind-cont-target k) ) )
}

◊indexR{раскрутка стека (unwinding)}
Теперь, чтобы передать значение при переходе, нам недостаточно просто отдать его нужному продолжению.
Нам необходимо подняться по~стеку продолжений с~помощью ◊ic{unwind} (◊term{раскрутить}~стек)
от текущего до целевого продолжения, выполняя по пути соответствующую финализацию,
если в~коде присутствует ◊ic{unwind-protect}.
Продолжения форм-финализаторов принадлежат классу ◊ic{unwind-cont}.
Их~обработка с~помощью ◊ic{resume} продолжает раскрутку стека,
а~также устанавливает правильное продолжение на случай переходов внутри самих форм-финализаторов
(тот самый процесс отбрасывания продолжений, который рассматривался на странице~◊pageref{escape/forms/protection/p:discard}).
◊; TODO: на странице? здесь правильная ссылка необходима

Что касается ◊ic{block}, то здесь делать ничего не~надо.
Как вы помните, ◊ic{block-lookup} уже вызывает ◊ic{unwind} для раскрутки стека с~целью проверки актуальности перехода:

◊code:lisp{
(define-method (block-lookup (r block-env) n k v)
  (if (eq? n (block-env-name r))
      (unwind k v (block-env-cont r))
      (block-lookup (block-env-others r) n k v) ) )
}

◊noindent
Остаётся только сказать спасибо обобщённым функциям.

◊indexC{block!и~◊ic{unwind-protect}}
Может показаться, что с~появлением ◊ic{unwind-protect} форма ◊ic{block} перестала быть быстрее ◊ic{catch},
ведь теперь они обе вынуждены выполнять раскрутку стека через ◊ic{unwind}.
Однако учитывая, что ◊ic{unwind-protect} является специальной формой,
большинство вызовов ◊ic{return-from} всё ещё можно оптимизировать —
в~том случае, когда между ◊ic{block} и соответствующей ◊ic{return-from} не~стоят формы ◊ic{unwind-protect} или~◊ic{lambda},
переходы выполняются эффективнее.

◊(bigskip)

◊; TODO: index: бесконечный цикл, этот прикол точно должен попасть в указатель
◊indexC{unwind-protect!ограничения ◊CommonLisp}
В~◊CommonLisp ◊seeCite{ste90} присутствует ещё одно интересное ограничение, касающееся переходов из форм-финализаторов.
Если сама форма ◊ic{unwind-protect} выполняет переход, то финализатор обязан переходить ◊emph{ещё~дальше}.
Таким образом запрещается формировать бесконечные циклы из переходов,
любые попытки выбраться из которых пресекаются ◊ic{unwind-protect}.
◊seeEx{escape/ex:eternal}
Следовательно, следующая программа приведёт к~ошибке,
так как финализатор хочет прыгнуть ближе, чем прыжок на~◊ic{1}, который уже в~процессе.

◊code:lisp[#:dialect CommonLisp]{
(catch 1
  (catch 2
    (unwind-protect (throw 1 'foo)
      (throw 2 'bar) ) ) )         ◊(is) ◊ii{ошибка!}
}
