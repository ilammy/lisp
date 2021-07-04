#lang pollen

◊; TODO: аналогично ◊exercise?
◊exanswer{lisp1-2-omega/ex:funcall}

На Scheme это выражение переводится непосредственно как ◊ic{(cons~1~2)}.
Если быть дотошным, то можно сделать так:

◊code:lisp{
(define (funcall f . args) (apply f args))
(define (function f) f)
}

◊noindent
Или то~же самое с~помощью макросов:

◊code:lisp{
(define-syntax funcall
  (syntax-rules ()
    ((funcall f arg ...) (f arg ...)) ) )

(define-syntax function
  (syntax-rules ()
    ((function f) f) ) )
}


◊exanswer{lisp1-2-omega/ex:lexical}

Перед ответом на этот вопрос сначала попробуйте ответить на два других:

◊enumerate{
  ◊item{
    Можно~ли ссылаться на функцию ◊ic{bar} до того, как она была определена?
  }
  ◊item{
    Если ◊ic{bar} всё~же была определена ранее, то как поведёт себя ◊ic{defun}:
    выдаст ошибку или переопределит функцию?
  }
}

А~собственно результат исполнения программы зависит от того, что возвращает специальная форма ◊ic{function}:
◊emph{саму} функцию ◊ic{bar} или некоторое значение, связанное с~именем ◊ic{bar} в~пространстве функций.


◊exanswer{lisp1-2-omega/ex:innovations}

За вызовы функций отвечает ◊ic{invoke},
так что достаточно просто научить её не~пугаться при виде чисел и списков.
Вот~так:

◊indexC{invoke}
◊code:lisp{
(define (invoke fn args)
  (cond ((procedure? fn) (fn args))
        ((number? fn)
         (if (= (length args) 1)
             (if (>= fn 0)
                 (list-ref (car args) fn)
                 (list-tail (car args) (- fn)) )
             (wrong "Incorrect arity" fn) ) )
        ((pair? fn)
         (map (lambda (f) (invoke f args)) fn) )
        (else (wrong "Cannot apply" fn)) ) )
}


◊exanswer{lisp1-2-omega/ex:assoc-with-comparator}

Сложность здесь в~том, что функция-компаратор берётся из интерпретируемого Лиспа и возвращает логические значения оттуда~же.

◊indexC{assoc/de}
◊code:lisp{
(definitial new-assoc/de
  (lambda (values current.denv)
    (if (= 3 (length values))
        (let ((tag        (car values))
              (default    (cadr values))
              (comparator (caddr values)) )
          (let look ((denv current.denv))
            (if (pair? denv)
                (if (eq? the-false-value
                         (invoke comparator (list tag (caar denv))
                                            current.denv ) )
                    (look (cdr denv))
                    (cdar denv) )
                (invoke default (list tag) current.denv) ) ) )
        (wrong "Incorrect arity" 'assoc/de) ) ) )
}


◊exanswer{lisp1-2-omega/ex:dynamic}

Функция-обработчик ◊ic{specific-error} должна будет вывести соответствующее сообщение о~неизвестной динамической переменной.

◊indexC{dynamic-let}
◊indexC{dynamic}
◊indexC{dynamic-set!}
◊code:lisp{
(define-syntax dynamic-let
  (syntax-rules ()
    ((dynamic-let () . body)
     (begin . body) )
    ((dynamic-let ((variable value) others ...) . body)
     (bind/de 'variable (list value)
              (lambda () (dynamic-let (others ...) . body)) ) ) ) )

(define-syntax dynamic
  (syntax-rules ()
    ((dynamic variable)
     (car (assoc/de 'variable specific-error)) ) ) )

(define-syntax dynamic-set!
  (syntax-rules ()
    ((dynamic-set! variable value)
     (set-car! (assoc/de 'variable specific-error) value) ) ) )
}


◊exanswer{lisp1-2-omega/ex:write-put/get-prop}

Переменная ◊ic{properties}, замыкаемая обеими функциями, содержит список свойств всех символов.

◊code:lisp{
(let ((properties '()))
  (set! putprop
        (lambda (symbol key value)
          (let ((plist (assq symbol properties)))
            (if (pair? plist)
                (let ((couple (assq key (cdr plist))))
                  (if (pair? couple)
                      (set-cdr! couple value)
                      (set-cdr! plist (cons (cons key value)
                                            (cdr plist) )) ) )
                (let ((plist (list symbol (cons key value))))
                  (set! properties (cons plist properties)) ) ) )
          value ) )
  (set! getprop
        (lambda (symbol key)
          (let ((plist (assq symbol properties)))
            (if (pair? plist)
                (let ((couple (assq key (cdr plist))))
                  (if (pair? couple)
                      (cdr couple)
                      #f ) )
                #f ) ) ) ) )
}


◊exanswer{lisp1-2-omega/ex:label}

Просто добавьте следующие строки в~◊ic{evaluate}:

◊; TODO: последовательно используй многоточие после закрывающей скобки: или на следующей строке везде, или на той же
◊code:lisp{
...
((label)  ; Синтаксис: (label ◊ii{имя} (lambda (◊ii{аргументы}) ◊ii{тело}))
 (let* ((name    (cadr e))
        (new-env (extend env (list name) (list 'void)))
        (def     (caddr e))
        (fun     (make-function (cadr def) (cddr def) new-env)) )
   (update! name new-env fun)
   fun ) )
...
}


◊exanswer{lisp1-2-omega/ex:labels}

Достаточно добавить следующий фрагмент в~◊ic{f.evaluate}.
Обратите внимание на его схожесть с~определением ◊ic{flet};
разница только в~окружении, где создаются локальные функции.

◊code:lisp{
...
((labels)
 (let ((new-fenv (extend fenv
                         (map car (cadr e))
                         (map (lambda (def) 'void) (cadr e)) )))
   (for-each (lambda (def)
               (update! (car def)
                        new-fenv
                        (f.make-function (cadr def) (cddr def)
                                         env new-fenv ) ) )
             (cadr e) )
   (f.eprogn (cddr e) env new-fenv) ) )
...
}


◊exanswer{lisp1-2-omega/ex:orderless-letrec}

Так как форма ◊ic{let} сохраняет неопределённый порядок вычислений,
то её следует использовать для вычисления значений переменных формы~◊ic{letrec}.
Связывание~же этих переменных с~полученными значениями необходимо выполнять отдельно.
Имена для всех этих ◊ii{temp◊sub{i}} можно получить или с~помощью механизма макрогигиены,
или просто кучей вызовов ◊ic{gensym}.

◊code:lisp{
(let ((◊ii{переменная◊sub{1}} 'void)
      ...
      (◊ii{переменная◊sub{n}} 'void) )
  (let ((◊ii{temp◊sub{1}} ◊ii{выражение◊sub{1}})
        ...
        (◊ii{temp◊sub{n}} ◊ii{выражение◊sub{n}}|) )
    (set! ◊ii{переменная◊sub{1}} ◊ii{temp◊sub{1}})
    ...
    (set! ◊ii{переменная◊sub{n}} ◊ii{temp◊sub{n}})
    ◊ii{тело} ) )
}


◊exanswer{lisp1-2-omega/ex:fixn}

Вот вам вариант для бинарных функций.
◊${\eta}-конверсия была модифицирована соответствующим образом.

◊code:lisp{
(define fix2
  (let ((d (lambda (w)
             (lambda (f)
               (f (lambda (x y) (((w w) f) x y))) ) )))
    (d d) ) )
}

◊noindent
После этого довольно легко догадаться, как сделать ◊${n}-арную версию:

◊code:lisp{
(define fixN
  (let ((d (lambda (w)
             (lambda (f)
               (f (lambda args (apply ((w w) f) args))) ) )))
    (d d) ) )
}


◊exanswer{lisp1-2-omega/ex:nfixn}

Ещё одно умственное усилие — и вы увидите, что предыдущее определение ◊ic{fixN} легко расширяется:

◊code:lisp{
(define 2fixN
  (let ((d (lambda (w)
             (lambda (f*)
               (list ((car f*)
                      (lambda a (apply (car ((w w) f*)) a))
                      (lambda a (apply (cadr ((w w) f*)) a)) )
                     ((cadr f*)
                      (lambda a (apply (car ((w w) f*)) a))
                      (lambda a (apply (cadr ((w w) f*)) a)) ) ) ) )))
    (d d) ) )
}

После этого остаётся понять, когда именно должен быть вычислен терм ◊ic{((w~w)~f)},
и~можно будет написать правильную универсальную версию:

◊code:lisp{
(define NfixN
  (let ((d (lambda (w)
             (lambda (f*)
               (map (lambda (f)
                      (apply f (map (lambda (i)
                                      (lambda a
                                        (apply (list-ref ((w w) f*) i)
                                               a ) ) )
                                    (iota 0 (length f*)) )) )
                    f* ) ) )))
    (d d) ) )
}

Внимание: порядок функций важен.
Если определение ◊ic{odd?} идёт первым в~списке функционалов,
то~именно эта функция будет связана с~их первыми аргументами.

◊indexC{iota}
Функция ◊ic{iota} аналогична одноимённому примитиву~◊${\iota} языка~APL:

◊code:lisp{
(define (iota start end)
  (if (< start end)
      (cons start (iota (+ 1 start) end))
      '() ) )
}


◊exanswer{lisp1-2-omega/ex:klop}

◊cite{bar84} приписывает эту функцию Яну~Виллему~Клопу.
Убедитесь сами, что ◊ic{((klop meta-fact) 5)} действительно возвращает~◊ic{120}.

Так как все внутренние переменные ◊ic{s}, ◊ic{c}, ◊ic{h}, ◊ic{e},~◊ic{m} связываются с~одной~◊ic{r},
то их порядок в~аппликации ◊ic{(m~e~c~h~e~s)} не~имеет значения.
Важно только их количество.
Вернее, согласованная арность: можно оставить одну переменную~◊ic{w},
а~можно использовать хоть весь алфавит — в~любом случае получится~◊comb{Y}.
◊; TODO: кернинг точки под Y


◊exanswer{lisp1-2-omega/ex:hyper-fact}

Абсолютно неожиданный ответ:~120.
Вам ведь понравилось выражать рекурсию с~помощью самоприменения, правда?
Определение можно записать немного по-другому, используя вложенные ◊ic{define}:

◊code:lisp{
(define (factfact n)
  (define (internal-fact f n)
    (if (= n 0) 1
        (* n (f f (- n 1))) ) )
  (internal-fact internal-fact n) )
}
