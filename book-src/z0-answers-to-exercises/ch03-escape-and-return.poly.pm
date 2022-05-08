#lang pollen

◊exanswer{escape/ex:cc-cc}

◊indexC{the-current-continuation}
Эту форму можно назвать ◊ic{(the-current-continuation)}, так как она возвращает собственное продолжение.
Очевидно, что форма ◊ic{call/cc} более выразительна, но~пока оставим вопрос полезности ◊ic{the-current-continuation} в~стороне.
Давайте разберёмся, как у~неё получается вернуть собственное продолжение.
Для~понятности, пронумеруем используемые продолжения и функции, а~◊ic{call/cc} сократим до просто~◊ic{cc}.
Итак, вычисляемое выражение: ◊${◊cont{k_0}\!◊math-ic{(cc$_1$ cc$_2$)}},
где~◊${k_0} — продолжение этого выражения.
Вспомните определение ◊ic{call/cc}:

◊$${
◊cont{k}◊math-ic{(call/cc $\varphi$)} \to ◊cont{k}◊math-ic{($\varphi$ $k$)}
}

◊noindent
Следовательно, ◊${◊cont{k_0}\!◊math-ic{(cc$_1$ cc$_2$)}} становится ◊${◊cont{k_0}\!◊math-ic{(cc$_2$ $k_0$)}},
что в~свою очередь переходит в~◊${◊cont{k_0}\!◊math-ic{($k_0$ $k_0$)}},
значением которого очевидно является~◊${k_0}.


◊exanswer{escape/ex:cc-cc-cc-cc}

Используя нотацию предыдущего упражнения, запишем:

◊$${
◊cont{k_0}\!◊math-ic{((cc$_1$ cc$_2$) (cc$_3$ cc$_4$))}
}

Для простоты будем считать, что термы аппликаций вычисляются слева направо.
Тогда исходное выражение эквивалентно

◊$${
◊cont{k_0}\!◊math-ic{($◊cont{k_1}\!$(cc$_1$ cc$_2$) $◊cont{k_2}\!$(cc$_3$ cc$_4$))}
}

где~◊${k_1} это ◊${\lambda\varphi . ◊math-ic{$◊cont{k_0}\!$($\varphi$ $◊cont{k_2}\!$(cc$_3$ cc$_4$))}},
а~◊${k_2} соответственно ◊${\lambda\varepsilon . ◊math-ic{$◊cont{k_0}\!$($k_1$ $\varepsilon$)}}.

Вычисление первого терма приводит к~◊${◊math-ic{$◊cont{k_0}\!$($k_1$ $k_2$)}},
а~вычисление второго даёт ◊${◊math-ic{$◊cont{k_0}\!$($k_2$ $◊cont{k'_2}\!$(cc$_3$ cc$_4$))}},
где~◊${k'_2} соответствует ◊${\lambda\varepsilon . ◊math-ic{$◊cont{k_0}\!$($k_2$ $\varepsilon$)}}.

Эта~форма вычисляется в~◊${◊math-ic{$◊cont{k_0}\!$($k_1$ $k'_2$)}},
что в~свою очередь даёт ◊${◊math-ic{$◊cont{k_0}\!$($k'_1$ $k''_2$)}},
и~так~далее.
Как видите, вычисления зацикливаются.
Легко доказать, что результат не~зависит от порядка вычисления термов аппликаций.
Вполне может быть, что это самая короткая программа на~Лиспе, выражающая бесконечный~цикл.


◊exanswer{escape/ex:tagbody}

Метки разделяют тело ◊ic{tagbody} на отдельные последовательности выражений,
которые оборачиваются в~функции и собираются в~одну большую форму ◊ic{labels}.
Формы~◊ic{go} преобразуются в~вызовы соответствующих функций,
однако эти вызовы выполняются специальным образом,
чтобы ◊ic{go} получила правильное продолжение.

◊; TODO: форматирование как к формулировке упражнения
◊code:lisp{
(block EXIT
  (let (LABEL (TAG (list 'tagbody)))
    (labels ((INIT   () ◊ii{выражения◊sub{0}}... (◊ii{метка◊sub{1}}))
             (◊ii{метка◊sub{1}} () ◊ii{выражения◊sub{1}}... (◊ii{метка◊sub{2}}))
             ...
             (◊ii{метка◊sub{n}} () ◊ii{выражения◊sub{n}}... (return-from EXIT nil)) )
      (setq LABEL (function INIT))
      (while #t
        (setq LABEL (catch TAG (funcall LABEL))) ) ) ) )
}

Формы ◊nobr{◊ic{(go ◊ii{метка})}} становятся~◊nobr{◊ic{(throw TAG ◊ii{метка})}},
а~◊nobr{◊ic{(return ◊ii{значение})}} превращается в~◊nobr{◊ic{(return-from EXIT ◊ii{значение})}}.
Имена переменных, записанные ◊ic{ПРОПИСНЫМИ} буквами, не~должны конфликтовать с~переменными, используемыми в~теле~◊ic{tagbody}.

Такое сложное представление ◊ic{go} необходимо для того, чтобы обеспечить переходам правильное продолжение:
в~форме ◊ic{(bar (go~L))} не~надо вызывать функцию ◊ic{bar} после того, как ◊ic{(go~L)} вернёт значение.
Если этого не~сделать, то следующая программа будет вести себя неправильно:

◊code:lisp{
(tagbody  A (return (+ 1 (catch 'foo (go B))))
          B (* 2 (throw 'foo 5)) )
}

◊noindent
См.~также~◊cite{bak92c}.


◊exanswer{escape/ex:arity-optimize}

Введите новый класс функций:

◊code:lisp{
(define-class function-with-arity function (arity))
}

◊noindent
Затем измените обработку ◊ic{lambda}-форм, чтобы они возвращали именно такие объекты:

◊code:lisp{
(define (evaluate-lambda n* e* r k)
  (resume k (make-function-with-arity n* e* r (length n*))) )
}

◊noindent
И,~наконец, реализуйте оптимизированный протокол вызова функций:

◊code:lisp{
(define-method (invoke (f function-with-arity) v* r k)
  (if (= (function-with-arity-arity f) (length v*))
      (let ((env (extend-env (function-env f)
                             (function-variables f) v* )))
        (evaluate-begin (function-body f) env k) )
      (wrong "Incorrect arity" (function-variables f) v*) ) )
}


◊exanswer{escape/ex:apply}

◊indexC{apply}
◊code:lisp{
(definitial apply
  (make-primitive 'apply
   (lambda (v* r k)
     (if (>= (length v*) 2)
         (let ((f (car v*))
               (args (let flat ((args (cdr v*)))
                       (if (null? (cdr args))
                           (car args)
                           (cons (car args) (flat (cdr args))) ) )) )
           (invoke f args r k) )
         (wrong "Incorrect arity" 'apply) ) ) ) )
}


◊exanswer{escape/ex:dotted}

Определите новый класс функций по аналогии с~предыдущим упражнением.

◊code:lisp{
(define-class function-nary function (arity))

(define (evaluate-lambda n* e* r k)
  (resume k (make-function-nary n* e* (length n*))) )

(define-method (invoke (f function-nary) v* r k)
  (define (extend-env env names values)
    (if (pair? names)
        (make-variable-env
         (extend-env env (cdr names) (cdr values))
         (car names)
         (car values) )
        (make-variable-env env names values) ) )
  (if (>= (length v*) (function-nary-arity f))
      (let ((env (extend-env (function-env f)
                             (function-variables f)
                             v* )))
        (evaluate-begin (function-body f) env k) )
      (wrong "Incorrect arity" (function-variables f) v*) ) )
}


◊exanswer{escape/ex:evaluate}

Поместите главный цикл интерпретатора непосредственно в~терминальное продолжение:

◊code:lisp{
(define (chapter3-interpreter-2)
  (letrec ((k.init (make-bottom-cont
                    'void (lambda (v) (display v)
                                      (toplevel) ) ))
           (toplevel (lambda () (evaluate (read r.init k.init)))) )
    (toplevel) ) )
}


◊exanswer{escape/ex:cc-value}

Определите соответствующий класс реифицированных продолжений.
Теперь внутренние продолжения интерпретатора скрыты от пользовательских программ,
◊ic{call/cc} использует значения определяемого языка,
а~◊ic{invoke} реализуется только там, где необходимо.

◊; TODO: слушай, может эти ссылки на кусках кода надо размещать на абзаце повыше?
◊indexC{call/cc}
◊code:lisp{
(define-class reified-continuation value (k))

(definitial call/cc
  (make-primitive 'call/cc
   (lambda (v* r k)
     (if (= 1 (length v*))
         (invoke (car v*) (list (make-reified-continuation k)) r k)
         (wrong "Incorrect arity" 'call/cc v*) ) ) ) )

(define-method (invoke (f reified-continuation) v* r k)
  (if (= 1 (length v*))
      (resume (reified-continuation-k f) (car v*))
      (wrong "Continuations expect one argument" v* r k) ) )
}


◊exanswer{escape/ex:eternal}

Вычисление функции заканчивается возвратом значения.
Значит, просто не~давайте ей вернуть значение.

◊code:lisp[#:dialect CommonLisp]{
(defun eternal-return (thunk)
  (labels ((loop ()
             (unwind-protect (thunk)
               (loop) ) ))
    (loop) ) )
}


◊exanswer{escape/ex:crazy-cc}

◊; TODO: мне не очень нравится нестандартный термин "коробка", но я не хочу говорить "боксинг" везде
◊; хорошо бы принять решение в четвяртой главе, где они повсеместно используются
Приведённые выражения, конечно~же, возвращают~◊ic{33}~и~◊ic{44}.
Функция ◊ic{make-box} создаёт ◊term{коробку}, значением которой можно манипулировать без видимых побочных эффектов.
Достигается такое поведение с~помощью ◊ic{call/cc} и ◊ic{letrec}.
Если вспомнить, что форма ◊ic{letrec} эквивалентна комбинации ◊ic{let} и ◊ic{set!},
то~станет немного понятнее, каким образом достигается этот эффект.
Полноценные продолжения Scheme, способные сколько угодно раз возвращаться к~прерванным вычислениям,
несколько усложняют реализацию ◊ic{letrec} в~виде специальной~формы.


◊exanswer{escape/ex:generic-evaluate}

Сначала сделайте ◊ic{evaluate} обобщённой:

◊code:lisp{
(define-generic (evaluate (e) r k)
  (wrong "Not a program" e) )
}

◊noindent
Затем реализуйте методы, вызывающие соответствующие функции:

◊code:lisp{
(define-method (evaluate (e quotation) r k)
  (evaluate-quote (quotation-value e) r k) )

(define-method (evaluate (e assignment) r k)
  (evaluate-set! (assignment-name e)
                 (assignment-form e)
                 r k ) )
...
}

◊noindent
Также вам понадобятся новые классы объектов для представления различных частей программ:

◊code:lisp{
(define-class program    Object  ())
(define-class quotation  program (value))
(define-class assignment program (name form))
...
}

Всё, теперь остаётся только определить функцию, преобразующую текст программ в~объекты класса ◊ic{program}.
Эта функция, называемая ◊ic{objectify}, рассматривается в~разделе~◊ref{macros/macrosystem/ssect:object}.


◊exanswer{escape/ex:throw}

Функция ◊ic{throw} определяется вот~так:

◊code:lisp{
(definitial throw
  (make-primitive 'throw
   (lambda (v* r k)
     (if (= 2 (length v*))
         (catch-lookup k (car v*)
                       (make-throw-cont k
                        `(quote ,(cadr v*)) r ) )
         (wrong "Incorrect arity" 'throw v*) ) ) ) )
}

Вместо того, чтобы определять новый метод для ◊ic{catch-lookup},
мы~просто подсунули ей фальшивое продолжение, чтобы заставить интерпретатор вести себя ожидаемым образом:
вычислить и вернуть второй аргумент ◊ic{throw}, когда найдётся соответствующая форма~◊ic{catch}.


◊exanswer{escape/ex:cps-speed}

◊indexE{CPS}
◊indexR{стиль передачи продолжений (CPS)}
CPS-код медленнее обычного, так как он вынужден постоянно создавать замыкания для явного представления продолжений.

Между прочим, CPS-преобразование не~идемпотентно —
то~есть, применив его к~программе, уже переписанной в~стиле передачи продолжений,
мы~получим ещё одну, третью версию той~же программы.
Рассмотрим, например, определение факториала:

◊; TODO: для прикола, убедись, что *все* определяемые факториалы входят в предметный указатель
◊indexC{fact}
◊code:lisp{
(define (cps-fact n k)
  (if (= n 0) (k 1)
      (cps-fact (- n 1) (lambda (v) (k (* n v)))) ) )
}

Очевидно, что ◊ic{k} — это просто аргумент функции ◊ic{cps-fact}.
Он~может быть вообще чем угодно.
В~том числе и таким продолжением:

◊code:lisp{
(call/cc (lambda (k) (* 2 (cps-fact 4 k)))) ◊(is) 24
}


◊exanswer{escape/ex:the-current-cc}

Функцию ◊ic{the-current-continuation} также можно определить подобно упражнению~◊ref{escape/ex:cc-cc}.

◊indexC{call/cc}
◊code:lisp{
(define (cc f)
  (let ((reified? #f))
    (let ((k (the-current-continuation)))
      (if reified? k
          (begin (set! reified? #t)
                 (f k) ) ) ) ) )
}

◊noindent
Большое спасибо Люку~Моро ◊seeCite{mor94} за эту пару~упражнений.
