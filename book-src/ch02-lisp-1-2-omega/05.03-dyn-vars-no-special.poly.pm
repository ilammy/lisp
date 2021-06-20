#lang pollen

◊subsection[#:label "lisp1-2-omega/namespaces/ssect:dyn-vars-no-special"]{Динамические~переменные без~специальных~форм}

Сейчас для работы с~динамическими переменными используются целых три специальные формы.
Так как Scheme исповедует минимализм по отношению к~специальным формам, стоит рассмотреть и~другие варианты реализации.
За~долгую историю Scheme их придумано достаточно много,
так что остановимся на самом простом варианте, который использует всего две функции.
Первая функция динамически связывает два значения — идентификатор переменной и её значение;
вторая функция по идентификатору находит соответствующее значение.
На~роль идентификаторов динамических переменных прекрасно подходят символы.
Кроме них нам потребуется некий изменяемый тип данных вроде точечных пар, если мы хотим изменять установленные связи.
Такое решение будет как раз в~аскетичных традициях Scheme.

Мы будем использовать интерпретатор с~двумя окружениями: ◊ic{env}~и~◊ic{denv}.
Он устроен так же, как и в~предыдущих разделах, но многие вещи мы оттуда уберём:
ненужные специальные формы, поддержку функциональных окружений, ссылки на переменные как в~◊|CommonLisp|.
Остаётся только суть динамического окружения:
каждое выражение имеет доступ к~динамическому окружению,
которое передаётся дальше во~все подвыражения.
Чтобы отличать эту вариацию от предыдущих, возьмём для нового интерпретатора префикс~◊ic{dd}.

◊indexC{dd.evaluate}
◊indexC{dd.make-function}
◊indexC{dd.evlis}
◊indexC{dd.eprogn}
◊code:lisp{
(define (dd.evaluate e env denv)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e)(string? e)(char? e)
                 (boolean? e)(vector? e) )
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (dd.evaluate (cadr e) env denv)
                      (dd.evaluate (caddr e) env denv)
                      (dd.evaluate (cadddr e) env denv) ))
        ((begin)  (dd.eprogn (cdr e) env denv))
        ((set!)   (update! (cadr e) env
                           (dd.evaluate (caddr e) env denv) ))
        ((lambda) (dd.make-function (cadr e) (cddr e) env))
        (else (invoke (dd.evaluate (car e) env denv)
                      (dd.evlis (cdr e) env denv)
                      denv )) ) ) )

(define (dd.make-function variables body env)
  (lambda (values denv)
    (dd.eprogn body (extend env variables values) denv) ) )

(define (dd.evlis e* env denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (cons (dd.evaluate (car e*) env denv)
                (dd.evlis (cdr e*) env denv) )
          (list (dd.evaluate (car e*) env denv)) )
      '() ) )

(define (dd.eprogn e* env denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (begin (dd.evaluate (car e*) env denv)
                 (dd.eprogn (cdr e*) env denv) )
          (dd.evaluate (car e*) env denv) )
      empty-begin ) )
}

Теперь определим обещанную ранее пару примитивов.
Первая функция обычно называется ◊ic{bind-with-dynamic-extent},
но это слишком длинное имя, так что сократим его до~◊ic{bind/de}.
Функция принимает три аргумента:
ключ~◊ic{tag};
значение~◊ic{value}, связываемое с~ключом;
и~замыкание~◊ic{thunk}, функцию без аргументов, которая исполняется в~расширенном динамическом окружении.

◊indexC{bind/de}
◊code:lisp{
(definitial bind/de
  (lambda (values denv)
    (if (= 3 (length values))
        (let ((tag   (car values))
              (value (cadr values))
              (thunk (caddr values)) )
          (invoke thunk '()
                  (extend denv (list tag) (list value)) ) )
        (wrong "Incorrect arity" 'bind/de) ) ) )
}

Вторая функция — ◊ic{assoc/de} — возвращает значение динамической переменной по~ключу~◊ic{tag}.
Такой переменной может и не~оказаться в~окружении, поэтому ◊ic{assoc/de} принимает ещё один аргумент —
функцию ◊ic{default}, которой передаются несуществующие ключи.
Таким образом можно либо вернуть какое-нибудь значение по умолчанию,
либо сигнализировать об ошибке.

◊indexC{assoc/de}
◊code:lisp{
(definitial assoc/de
  (lambda (values current.denv)
    (if (= 2 (length values))
        (let ((tag     (car values))
              (default (cadr values)) )
          (let look ((denv current.denv))
            (if (pair? denv)
                (if (eqv? tag (caar denv))
                    (cdar denv)
                    (look (cdr denv)) )
                (invoke default (list tag) current.denv) ) ) )
        (wrong "Incorrect arity" 'assoc/de) ) ) )
}

Возможны различные варианты поведения ◊ic{assoc/de} в~зависимости от используемого механизма сравнения (◊ic{eqv?}~или~◊ic{equal?}).
◊seeEx{lisp1-2-omega/ex:assoc-with-comparator}

Предыдущий пример использования динамических переменных теперь выглядит~так:

◊code:lisp{
(bind/de 'x 2
  (lambda () (+ (assoc/de 'x error)
                (let ((x (+ (assoc/de 'x error)
                            (assoc/de 'x error) )))
                  (+ x (assoc/de 'x error)) ) )) )
◊(is) 8
}

Таким образом мы опровергли необходимость использования специальных форм для реализации механизма динамических переменных.
Вдобавок, наша реализация позволяет связывать любой ключ с~любым значением.
Конечно, подобной выразительностью можно немного пожертвовать ради эффективности.
Например, ближнее связывание потребует, чтобы ключи были только символами.
◊footnote{
  Многие Лиспы также запрещают использовать ключевые слова вроде ◊ic{nil} или~◊ic{if} в~качестве имён переменных.
}
Кроме того, вызов функции — это недешёвая операция сама по~себе.
Наше решение довольно далеко от органичного сочетания динамических и лексических переменных в~◊|CommonLisp|.

Среди всех неудобств также стоит отметить синтаксис:
◊ic{bind/de} требует замыканий, а~◊ic{assoc/de} надо явно передавать обработчик ошибок.
Хотя, конечно~же, это поправимо с~помощью макросов.
Другое неудобство возникает при реализации компилятора,
которому потребуется обрабатывать эти функции особым образом.
К~счастью, это всё~же просто функции, так что наивный компилятор может их вызывать в~лоб,
а~более продвинутый — распознает доступ к~динамическим переменным
и~подставит вместо функций свою, более эффективную реализацию.
