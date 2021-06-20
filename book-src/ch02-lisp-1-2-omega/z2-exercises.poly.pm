#lang pollen

◊section*[#:label "lisp1-2-omega/sect:exercises"]{Упражнения}


◊; TODO: сменить на опциональный аргумент: ◊exercise[#:label "..."], для унификации
◊exercise{lisp1-2-omega/ex:funcall}

Следующее выражение записано на ◊(CommonLisp).
Как бы вы его перевели на~Scheme?

◊code:lisp{
(funcall (function funcall) (function funcall) (function cons) 1 2)
}


◊exercise{lisp1-2-omega/ex:lexical}

Что вернёт данная программа на псевдо-◊(CommonLisp) из этой главы?
О~чём она вам напоминает?

◊code:lisp{
(defun test (p)
  (function bar) )

(let ((f (test #f)))
  (defun bar (x) (cdr x))
  (funcall f '(1 . 2)) )
}


◊exercise{lisp1-2-omega/ex:innovations}

Реализуйте в~вашем интерпретаторе первые две инновации из раздела~◊ref{lisp1-2-omega/sect:extensions}.
◊seePage{lisp1-2-omega/sect:extensions}
Речь идёт о~трактовке чисел и списков как функций.


◊exercise{lisp1-2-omega/ex:assoc-with-comparator}

Научите функцию ◊ic{assoc/de} явно принимать компаратор (вроде ◊ic{eq?}, ◊ic{equal?}, и~т.~п.) через аргумент, а~не~задавать его внутри.


◊exercise{lisp1-2-omega/ex:dynamic}

Используя ◊ic{bind/de} и ◊ic{assoc/de}, напишите макросы, эмулирующие специальные формы ◊ic{dynamic-let}, ◊ic{dynamic} и~◊ic{dynamic-set!}.


◊exercise{lisp1-2-omega/ex:write-put/get-prop}

◊indexC{putprop}
◊indexC{getprop}
Напишите функции ◊ic{getprop} и ◊ic{putprop}, которые реализуют списки свойств.
Любой символ имеет личный список свойств в~виде пар «ключ — значение»;
добавление в~этот список осуществляет функция~◊ic{putprop},
поиск значения по ключу осуществляет функция~◊ic{getprop}.
Также, естественно, должно выполняться утверждение

◊code:lisp{
(begin (putprop 'symbol 'key 'value)
       (getprop 'symbol 'key) )      ◊(is) value
}


◊exercise{lisp1-2-omega/ex:label}

Определите специальную форму ◊ic{label} на ◊(Lisp1).


◊exercise{lisp1-2-omega/ex:labels}

Определите специальную форму ◊ic{labels} на ◊(Lisp2).


◊exercise{lisp1-2-omega/ex:orderless-letrec}

◊indexC{letrec}
Придумайте, как реализовать ◊ic{letrec} с~помощью ◊ic{let} и ◊ic{set!}
так, чтобы порядок вычисления значений-инициализаторов был неопределённым.


◊exercise{lisp1-2-omega/ex:fixn}

◊indexR{комбинаторы!неподвижной точки!универсальный}
У~нашего комбинатора неподвижной точки на Scheme обнаружился недостаток:
он поддерживает только унарные функции.
Реализуйте ◊ic{fix2}, работающий с~бинарными функциями.
Затем ◊ic{fixN}, поддерживающий функции любой арности.


◊exercise{lisp1-2-omega/ex:nfixn}

Далее напишите функцию ◊ic{NfixN}, возвращающую неподвижные точки для списка функционалов произвольной арности.
Её можно использовать, например, следующим образом:

◊code:lisp{
(let ((odd-and-even
       (NfixN (list (lambda (odd? even?)    ; odd?
                      (lambda (n)
                        (if (= n 0) #f (even? (- n 1))) ) )
                    (lambda (odd? even?)    ; even?
                      (lambda (n)
                        (if (= n 0) #t (odd? (- n 1))) ) ) )) ))
  (set! odd? (car odd-and-even))
  (set! even? (cadr odd-and-even)) )
}


◊exercise{lisp1-2-omega/ex:klop}

Рассмотрим функцию ◊ic{klop}.
Является~ли она комбинатором неподвижной точки?
Попробуйте доказать или опровергнуть, что ◊ic{(klop ◊ii{f})} возвращает неподвижную точку~◊ii{f} подобно~◊ic{fix}.

◊indexC{klop}
◊code:lisp{
(define klop
  (let ((r (lambda (s c h e m)
             (lambda (f)
               (f (lambda (n)
                    (((m e c h e s) f) n) )) ) )))
    (r r r r r r) ) )
}


◊exercise{lisp1-2-omega/ex:hyper-fact}

◊indexC{fact}
Если функция ◊ic{hyper-fact} определена так:

◊code:lisp{
(define (hyper-fact f)
  (lambda (n)
    (if (= n 0) 1
        (* n ((f f) (- n 1))) ) ) )
}

◊noindent
то что вернёт ◊ic{((hyper-fact hyper-fact)~5)}?
