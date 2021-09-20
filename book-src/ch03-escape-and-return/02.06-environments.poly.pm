#lang pollen

◊subsection[#:label "escape/actors/ssect:variables"]{Окружения}

Значения переменных хранятся в~окружениях.
Они~тоже представляются объектами:

◊indexC{null-env}
◊indexC{full-env}
◊indexC{variable-env}
◊code:lisp{
(define-class null-env environment ())
(define-class full-env environment (others name))
(define-class variable-env full-env (value))
}

Окружение ◊ic{variable-env} хранит привязку имени~◊ic{name} к~значению~◊ic{value},
а~также ссылку на остальные привязки в~◊ic{others}.
Пустое окружение без привязок представляется объектом~◊ic{null-env}.
В~общем, это обычный А-список, только вместо двух~точечных~пар используются объекты с~тремя~полями.
◊; TODO: не забудь про магию, чтобы "A-список" заворачивался в nobr

Поиск значения переменной реализуется следующим образом:

◊indexC{evaluate-variable}
◊code:lisp{
(define (evaluate-variable n r k)
  (lookup r n k) )

(define-method (lookup (r null-env) n k)
  (wrong "Unknown variable" n r k) )

(define-method (lookup (r full-env) n k)
  (lookup (full-env-others r) n k) )

(define-method (lookup (r variable-env) n k)
  (if (eqv? n (variable-env-name r))
      (resume k (variable-env-value r))
      (lookup (variable-env-others r) n k) ) )
}

Обобщённая функция ◊ic{lookup} проходит по окружению, пока не~найдёт подходящую привязку:
с~искомым именем и~хранящую значение переменной.
Найденное значение передаётся исходному продолжению с~помощью~◊ic{resume}.

Изменение значения выполняется похожим образом:

◊; TODO: ты помнишь же, что надо сделать магию, чтобы она разбивала ! на элементы? проследи, чтобы тут всё правильно разделялось
◊indexC{set!-cont}             ◊; вот здесь это один элемент: ◊ic{set!-cont}
◊indexC{evaluate-set!}
◊indexC{resume!◊ic{set!-cont}} ◊; а здесь два: ◊ic{resume} > ◊ic!{set!-cont}
◊indexC{update!!◊ic{null-env}} ◊; и здесь два: ◊ic{update!} > ◊ic{null-env}
◊indexC{update!!◊ic{full-env}}
◊indexC{update!!◊ic{variable-env}}
◊code:lisp{
(define-class set!-cont continuation (n r))

(define (evaluate-set! n e r k)
  (evaluate e r (make-set!-cont k n r)) )

(define-method (resume (k set!-cont) v)
  (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v) )

(define-method (update! (r null-env) n k v)
  (wrong "Unknown variable" n r k) )

(define-method (update! (r full-env) n k v)
  (update! (full-env-others r) n k v) )

(define-method (update! (r variable-env) n k v)
  (if (eqv? n (variable-env-name r))
      (begin (set-variable-env-value! r v)
             (resume k v) )
      (update! (variable-env-others r) n k v) ) )
}

Нам потребовалось вспомогательное продолжение, так~как присваивание выполняется в~два~этапа:
сначала надо вычислить присваиваемое значение, затем~присвоить его переменной.
Класс ◊ic{set!-cont} представляет необходимые продолжения,
его~метод ◊ic{resume} лишь вызывает ◊ic{update!} для установки значения,
после~чего продолжает дальнейшие вычисления.
