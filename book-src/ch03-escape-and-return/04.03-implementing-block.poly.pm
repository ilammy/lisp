#lang pollen

◊subsection[#:label "escape/implementation/ssect:block"]{Реализация ◊ic{block}}

◊indexC{block!реализация}
◊indexC{return-from!реализация}
Для реализации лексических переходов требуется решить две проблемы.
Во-первых, необходимо гарантировать динамическое время жизни продолжений.
Во-вторых, обеспечить лексическую область видимости меток.
Вторая задача легко решается с~помощью лексических окружений.
Чтобы избежать путаницы, у~◊ic{block} будет личный класс лексических окружений для хранения привязок меток к~продолжениям переходов.

Начинаем как обычно:
добавляем распознавание формы ◊ic{block} в~◊ic{evaluate} и описываем необходимые функции-обработчики.

◊indexC{block-cont}
◊indexC{block-env}
◊indexC{evaluate-block}
◊indexC{resume!◊ic{block-cont}}
◊code:lisp{
(define-class block-cont continuation (label))
(define-class block-env full-env (cont))

(define (evaluate-block label body r k)
  (let ((k (make-block-cont k label)))
    (evaluate-begin body
                    (make-block-env r label k)
                    k ) ) )

(define-method (resume (k block-cont) v)
  (resume (block-cont-k k) v) )
}

С~нормальным поведением закончили, переходим к~◊ic{return-from}.
Сначала добавляем её в~◊ic{evaluate}:

◊code:lisp{
...
((block)       (evaluate-block (cadr e) (cddr e) r k))
((return-from) (evaluate-return-from (cadr e) (caddr e) r k))
...
}

◊noindent
Затем описываем обработку:

◊indexC{return-from-cont}
◊indexC{evaluate-return-from}
◊indexC{resume!◊ic{return-from-cont}}
◊indexC{block-lookup}
◊indexC{unwind}
◊indexC{unwind!◊ic{continuation}}
◊indexC{unwind!◊ic{bottom-cont}}
◊code:lisp{
(define-class return-from-cont continuation (r label))

(define (evaluate-return-from label form r k)
  (evaluate form r (make-return-from-cont k r label)) )

(define-method (resume (k return-from-cont) v)
  (block-lookup (return-from-cont-r k)
                (return-from-cont-label k)
                (return-from-cont-k k)
                v ) )

(define-generic (block-lookup (r) n k v)
  (wrong "Not an environment" r n k v) )

(define-method (block-lookup (r block-env) n k v)
  (if (eq? n (block-env-name r))
      (unwind k v (block-env-cont r))
      (block-lookup (block-env-others r) n k v) ) )

(define-method (block-lookup (r full-env) n k v)
  (block-lookup (variable-env-others r) n k v) )

(define-method (block-lookup (r null-env) n k v)
  (wrong "Unknown block label" n r k v) )

(define-generic (unwind (k) v ktarget))

(define-method (unwind (k continuation) v ktarget)
  (if (eq? k ktarget)
      (resume k v)
      (unwind (continuation-k k) v ktarget) ) )

(define-method (unwind (k bottom-cont) v ktarget)
  (wrong "Obsolete continuation" v) )
}

После вычисления необходимого значения функция ◊ic{block-lookup} ищет продолжение,
связанное с~меткой~◊ic{tag} в~лексическом окружении формы~◊ic{return-from}.
Затем, если продолжение перехода в~принципе существует,
функция~◊ic{unwind} сперва убеждается в~том, что переход всё ещё актуален.

Поиск именованного блока, хранящего нужное продолжение, реализуется обобщённой функцией ◊ic{block-lookup}.
Она обучена пропускать ненужные окружения с~обычными переменными,
останавливаясь только на экземплярах ◊ic{block-env}, которые хранят нужные нам ◊ic{block-cont}.
Аналогично и обобщённая функция ◊ic{lookup} пропускает экземпляры ◊ic{block-env},
останавливаясь лишь на интересующих~её ◊ic{variable-env}.
Обе функции также определяют методы для общего предка ◊ic{full-env},
что позволяет легко добавлять новые классы окружений в~будущем.

Наконец, обобщённая функция ◊ic{unwind} передаёт вычисленное значение найденному продолжению,
но~только если оно ещё актуально — то~есть является частью текущего продолжения.
