#lang pollen

◊section[#:label "denotational/sect:varargs"]{Функции с~переменной~арностью}

◊indexR{переменная арность}
◊indexR{арность!переменная}
В~этом разделе мы покажем, как привнести в~Scheme функции с~точечным аргументом.
Особенность этих функций в~том, что они собирают «лишние» аргументы в~список, который передаётся через последний аргумент.
Следовательно, при каждом вызове подобной функции необходимо создавать в~памяти новый список, что влияет на производительность не~лучшим образом.
Функция ◊ic{apply} порой снижает накладные расходы, переиспользуя уже существующий список аргументов.
В~общем, переменная арность неразлучно связана со~списками,
поэтому нам потребуются денотации соответствующих функций:
◊ic{cons}, ◊ic{car}, ◊ic{set-cdr!}, и~т.~п.

◊indexR{Пары@◊Vset{Пары}}
Точечные пары принадлежат домену~◊${◊Vset{Пар}}.
Естественно, этот домен также входит в~дизъюнктивную сумму ◊${◊Vset{Значений}}.
Точечные пары представляются так~же, как и в~предыдущей главе — парой~адресов.

◊$${
\begin{align*}
  ◊Vset{Значения} & = ◊Vset{Функции} + ◊Vset{Числа} + ◊Vset{Пары} + \cdots  \\
      ◊Vset{Пара} & = ◊Vset{Адрес} \times ◊Vset{Адрес}                      \\
\end{align*}
}

Денотации ◊ic{cons}, ◊ic{car} и ◊ic{set-cdr!} (надо~же показать хотя~бы один побочный~эффект) ничем не~выделяются.
Мы~используем в~точности такой~же подход, что и в~предыдущей главе.
Единственное, о~чём стоит договориться — это ассоциативность операций.
Выражение ◊${◊|e|^*◊d:car{1}◊Prj{Пары}◊d:car{2}} читается слева~направо:
◊${◊|e|^*◊d:car{1}} — первый аргумент функции, ◊${◊Vset{Значение}},
проецируется на домен ◊${◊Prj{Пары}}
(если это значение на самом деле не~было парой, то~мы получаем~◊${\bot}),
после чего извлекается ◊ic{cdr} этой пары, её~второй компонент~◊${◊d:car{2}}.

◊; TODO: все индексы надо пересмотреть и подвинуть чуть выше, чтобы они захватывали абзац текста, а не были привязаны к коду, наверное
◊indexC{cons}
◊indexC{car}
◊indexC{set-cdr!}
◊code:denotation{
(definitial cons
  (inValue
   (lambda (v* k s)
     (if (= 2 (length v*))
         (allocate s 2
                   (lambda (s a*) (k (inValue (list (car a*) (cadr a*)))
                                     (extend* s a* v*) )) )
         (wrong "incorrect arity" 'cons) ) ) ) )

(definitial car
  (inValue
   (lambda (v* k s)
     (if (= 1 (length v*))
         (k (s (car (Value->Pair (car v*)))) s)
         (wrong "incorrect arity" 'car) ) ) ) )

(definitial set-cdr!
  (inValue
   (lambda (v* k s)
     (if (= 2 (length v*))
         (k (car v*)
            (extend s (cadr (Value->Pair (car v*)))
                    (cadr v*) ) )
         (wrong "incorrect arity" 'set-cdr!) ) ) ) )
}

После определения структуры списков реализация ◊ic{apply} не~составляет проблем.
Собираем в~последовательность все элементы формы аппликации, кроме первого (функции) и последнего.
Если предпоследний элемент является списком,
то~его необходимо пришить к~концу формируемой последовательности.
После всех этих манипуляций выполняется собственно вызов функции.

◊indexC{apply}
◊; TODO: как эти функции записывать? через ◊ii или какой-то ◊fn?
◊indexE{collect@◊ii{collect}}
◊indexE{flat@◊ii{flat}}
◊; TODO: вот эти волшебные ◊WHERE надо использовать в генераторе
◊indexE{where-and@◊${◊WHERE \ldots ◊AND \ldots}}
◊code:denotation{
(definitial apply
  (inValue
   (lambda (v* k s)
     (if (>= (length v*) 2)
         (letrec
             ((collect
               (lambda (v*)
                 (if (null? (cdr v*))
                     (flat (car v*))
                     (cons (car v*) (collect (cdr v*))) ) ))
              (flat
               (lambda (v)
                 (if (cons? (Value-content v))
                     (cons (s (car (Value->Pair v)))
                           (flat (s (cadr (Value->Pair v)))) )
                     (list) ) ) ) )
           ((Value->Function (car v*))
            (collect (cdr v*))
            k s ) )
         (wrong "Incorrect arity" 'apply) ) ) ) )
}

Здесь ◊${◊WHERE \ldots ◊AND \ldots} определяет взаимно рекурсивные функции.

◊indexR{интерпретатор!B@◊${◊Bind}}
◊indexE{B@◊${◊Bind}, интерпретатор}
◊; TODO: эти пробелы здесь для того, чтобы греческий в индексе сортировался первым, да?
◊indexR{т ау@◊${\tau} (контекст вычислений)}
◊indexE{t au@◊${\tau} (контекст вычислений)}
◊indexR{м ю@◊${\mu} (денотация функции)}
◊indexE{m u@◊${\mu} (денотация функции)}
Теперь перейдём к~собственно обработке точечных аргументов.
Для этого нам потребуется изменить форму~◊ic{lambda}, а~также определить специальный интерпретатор.
Его~единственной задачей будет правильным образом связывать переменные со~значениями.
Мы~назовём его ◊${◊Bind}, от~◊english{◊term{binding}}.
Тип~◊${◊Bind} связан с~типом денотаций, которые произодит~◊${◊Eval}.
Для~краткости обозначим буквой~◊${\mu} денотацию функции,
для~которой ◊${◊Bind} подготавливает аргументы, а~для контекста вычислений используем букву~◊${\tau}:

◊$${
\tau \equals ◊Vset{Значения}^* \times ◊Vset{Окружение} \times ◊Vset{Продолжение} \times ◊Vset{Память}
\begin{align*}
◊Eval \colon \quad & ◊Vset{Программа} \to (\tau \to ◊Vset{Значение}) \\
◊Bind \colon \quad & ◊Vset{СписокАргументов} \to \big( \underbrace{(\tau \to ◊Vset{Значение})}_{\displaystyle\mu} \times \tau \to ◊Vset{Значение} \big) \\
\end{align*}
}

Cвязывающий интерпретатор~◊${◊Bind} начинает работу после проверки арности формой~◊ic{lambda}.
Если всё в~порядке, то~фактические значения аргументов заносятся одно за~другим в~память.
Затем лексическое окружение функции последовательно расширяется соответствующими переменными.
Наконец, по~готовности управление передаётся телу функции.
Определение функций фиксированной арности с~помощью ◊${◊Bind} выглядит~так:

◊; TODO: особенный трансформер, ага
◊indexC{lambda}
◊indexE{let-in@◊${◊LET \ldots ◊IN}}
◊code:denotation{
(define ((meaning-fix-abstraction n* e+) r k s)
  (k (inValue (lambda (v* k1 s1)
                (if (= (length v*) (length n*))
                    (((meaning-regular-variables n*)
                     (lambda (v* r k s) ((meaning*-sequence e+) r k s)) )
                     v* r k1 s1 )
                    (wrong "Incorrect arity") ) ))
     s ) )

(define (meaning-regular-variables n*)
  (if (pair? n*)
      (meaning-some-regular-variables (car n*) (cdr n*))
      (meaning-no-regular-variables) ) )

(define ((meaning-some-regular-variables n n*) m)
  ((meaning-variable n) ((meaning-regular-variables n*) m)) )

(define ((meaning-no-regular-variables) m)
  m )

(define ((meaning-variable n) m)
  (lambda (v* r k s)
    (allocate
     s 1 (lambda (s a*)
           (let ((a (car a*)))
             (m (cdr v*) (extend r n a) k (extend s a (car v*))) ) ) ) ) )
}

Конструкция ◊${◊LET \ldots ◊IN} вводит локальные нерекурсивные определения.

Для обработки случая функций переменной арности
потребуется определить дополнительный синтаксис формы ◊ic{lambda}, принимающей список с~точкой.
Кроме того, необходимо определить соответствующую ветку~◊${◊Bind},
которая принимает последовательность «лишних» значений,
превращает её в~список (настоящий, из~точечных~пар),
и~связывает его с~именем последнего аргумента.
Совместное существование функций с~переменной арностью, функции~◊ic{apply} и~побочных эффектов в~Scheme
вынуждает при каждом вызове создавать в~памяти новый список из свежевыделенных точечных~пар.
Поэтому следующее выражение должно возвращать ложь:

◊code:lisp{
(let ((arguments (list 1 2 3)))
  (apply (lambda args (eq? args arguments)) arguments) )
}

Если реализация захочет переиспользовать точечные пары,
то~сперва необходимо доказать, что подобная подобная оптимизация не~изменит смысла программы.

Наконец, переходим к~денотациям:

◊indexE{listify@◊ii{listify}}
◊code:denotation{
(define (meaning-possibly-dotted-abstraction n* e+)
  (let parse ((n* n*)
              (regular '()) )
    (cond
     ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
     ((null? n*) (meaning-fix-abstraction (reverse regular) e+))
     (else       (meaning-dotted-abstraction (reverse regular) n* e+)) ) ) )

(define ((meaning-dotted-abstraction n* n e+) r k s)
  (k (inValue (lambda (v* k1 s1)
                (if (>= (length v*) (length n*))
                    (((meaning-regular-variables n*)
                      ((meaning-dotted-variable n)
                       (lambda (v* r k s) ((meaning*-sequence e+) r k s)) ) )
                     v* r k1 s1 )
                    (wrong "Incorrect arity") ) ))
     s ) )

(define ((meaning-dotted-variable n) m)
  (lambda (v* r k s)
    (letrec ((listify
              (lambda (v* s q)
                (if (pair? v*)
                    (allocate
                     s 2 (lambda (s a*)
                           (let ((qq (lambda (v s)
                                       (q (inValue a*)
                                          (extend s (cadr a*) v) ) )))
                             (listify (cdr v*)
                                      (extend s (car a*) (car v*))
                                      qq ) ) ) )
                    (q (inValue (list)) s) ) )))
      (listify v* s (lambda (v s)
                      (allocate s 1
                                (lambda (s a*)
                                  (let ((a (car a*)))
                                    (m (list)
                                       (extend r n a)
                                       k
                                       (extend s a v) ) ) ) ) )) ) ) )
}

◊indexR{переменная арность!сложность}
Как~видите, реализация нетривиальных возможностей вроде функций переменной арности требует существенных усилий.
Нам~пришлось практически написать ещё один дополнительный интерпретатор.
Если сравнить получившуюся версию с~элегантной реализацией ядра Scheme,
то~станет очевидным, что добавление всего одной интересной, но~фундаментально несущественной детали
почти~что удвоило размер~денотаций, не~говоря уже о~читабельности и~понятности.
