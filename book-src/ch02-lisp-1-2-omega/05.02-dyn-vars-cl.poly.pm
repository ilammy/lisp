#lang pollen

◊subsection[#:label "lisp1-2-omega/namespaces/ssect:dyn-vars-cl"]{Динамические~переменные в~◊(CommonLisp)}

◊indexR{динамические переменные!синтаксис}
◊indexR{синтаксис!динамических переменных}
◊(CommonLisp) старается концептуально разделять динамические и лексические переменные,
но с~точки зрения синтаксиса они практически неотличимы.
Например, вместо специальной формы ◊ic{dynamic-let} следует использовать следующий эквивалент:

◊; TODO: таблица

◊code:lisp{
(dynamic-let ((x ◊${\alpha}))
  ◊${\beta} )
}

◊(eq)

◊code:lisp{
(let ((x ◊${\alpha}))
  (declare (special x))
  ◊${\beta} )
}

Другое отличие состоит в~том, что для получения значения динамической переменной~◊ic{x} внутри ◊${\beta}
нет надобности использовать форму ◊ic{dynamic}, достаточно писать просто~◊ic{x}.
Выражение ◊ic{(declare (special~x))} указывает ◊ic{let}, что привязка для~◊ic{x} должна быть динамической,
а~все последующие обращения~к~◊ic{x} следует понимать как ◊ic{(dynamic~x)}.

Этот подход не~вполне удобен, так как внутри~◊${\beta} нельзя обращаться к~лексической переменной~◊ic{x} —
нам будет видна только её динамическая тёзка.
Альтернативой является локальное объявление динамической переменной~◊ic{x}:
◊ic{(locally (declare (special~x))~x)},
что в~принципе эквивалентно форме~◊ic{dynamic}.

Стратегия ◊(CommonLisp) состоит в~указании типа привязки с~помощью конструкций языка.
Попробуем реализовать что-то похожее в~нашем интерпретаторе:

◊indexC{df.evaluate}
◊indexC{special-extend}
◊indexC{cl.lookup}
◊code:lisp{
(define (df.evaluate e env fenv denv)
  (if (atom? e)
      (cond ((symbol? e) (cl.lookup e env))
            ((or (number? e)(string? e)(char? e)
                 (boolean? e)(vector? e) )
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (df.evaluate (cadr e) env fenv denv)
                      (df.evaluate (caddr e) env fenv denv)
                      (df.evaluate (cadddr e) env fenv denv) ))
        ((begin)  (df.eprogn (cdr e) env fenv denv))
        ((set!)   (update! (cadr e)
                           env
                           (df.evaluate (caddr e) env fenv denv) ))
        ((function)
         (cond ((symbol? (cadr e))
                (f.lookup (cadr e) fenv) )
               ((and (pair? (cadr e)) (eq? (car (cadr e)) 'lambda))
                (df.make-function
                 (cadr (cadr e)) (cddr (cadr e)) env fenv ) )
               (else (wrong "Incorrect function" (cadr e))) ) )
        ((dynamic) (lookup (cadr e) denv))
        ((dynamic-set!)
         (update! (cadr e)
                  denv
                  (df.evaluate (caddr e) env fenv denv) ) )
        ((dynamic-let)
         (df.eprogn (cddr e)
                    ;; === изменения здесь ===
                    (special-extend env
                                    (map car (cadr e)) )
                    fenv
                    (extend denv
                            (map car (cadr e))
                            (map (lambda (e)
                                   (df.evaluate e env fenv denv) )
                                 (map cadr (cadr e)) ) ) ) )
        (else (df.evaluate-application (car e)
                                       (df.evlis (cdr e) env fenv denv)
                                       env
                                       fenv
                                       denv )) ) ) )

(define (special-extend env variables)
  (append variables env) )

(define (cl.lookup var env denv)
  (let look ((env env))
    (if (pair? env)
        (if (pair? (car env))
            (if (eq? (caar env) var)
                (cdar env)
                (look (cdr env)) )
            (if (eq? (car env) var)
                ;; ищем в~текущем динамическом окружении
                (let lookup-in-denv ((denv denv))
                  (if (pair? denv)
                      (if (eq? (caar denv) var)
                          (cdar denv)
                          (lookup-in-denv (cdr denv)) )
                      ;; если не~находим, ищем в~глобальном лексическом
                      (lookup var env.global) ) )
                (look (cdr env)) ) )
        (wrong "No such binding" var) ) ) )
}

Теперь разберём, что именно здесь происходит.
Когда ◊ic{dynamic-let} создаёт динамическую переменную,
она не~только связывает её со~значением в~динамическом окружении,
но и помечает её как динамическую в~лексическом окружении, записывая туда её имя.
Для поддержки этих меток изменяется механизм поиска значения по ссылке (◊ic{cl.lookup}):
он должен проанализировать ссылку, чтобы определить тип привязки (лексическая или динамическая),
после чего отыскать значение в~правильном окружении.
Также, если переменная не~найдена в~текущем динамическом окружении,
то следующим просматривается глобальное лексическое окружение,
которое в~◊(CommonLisp) одновременно является глобальным динамическим.

Приведём пример работы такого ◊(Lisp3), имитирующего ◊(CommonLisp):

◊code:lisp{
(dynamic-let ((x 2))
  (+ x                        ; динамическая переменная
     (let ((x (+              ; лексическая
                 x x )))      ; динамические
       (+ x                   ; лексическая
          (dynamic x) ) ) ) ) ; динамическая
◊(is) 8
}
