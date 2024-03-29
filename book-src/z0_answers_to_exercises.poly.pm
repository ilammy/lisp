% -- Глава 5 ------------------------

◊begingroup◊ChapterFiveSpecials

◊exanswer{denotational/ex:truly-random}

Доказывается индукцией по количеству термов аппликации.


◊exanswer{denotational/ex:label}

◊begin{denotation}
$◊Lain◊sem*{(label $◊n$ $◊p$)}◊r =
    (◊comb{Y}◊ ◊lambda ◊e.(◊Lain◊sem{◊p}◊ ◊r[◊n ◊to ◊e]))$
◊end{denotation}


◊exanswer{denotational/ex:dynamic-fallback}

◊begin{denotation}
$◊Eval◊sem*{(dynamic $◊n$)}◊r◊d◊k◊s = {}$                                     ◊◊
  $◊LET ◊e = (◊d◊ ◊n)$                                                        ◊◊
  $◊IN {}$◊.$◊IF   ◊e = ◊ii{no-dynamic-binding}$                              ◊◊
            $◊THEN {}$◊.$◊LET ◊a = (◊g◊ ◊n)$                                  ◊◊
                        $◊IN {}$◊.$◊IF   ◊a = ◊ii{no-global-binding}$         ◊◊
                                  $◊THEN ◊ii{wrong}◊ ◊ic{"No such variable"}$ ◊◊
                                  $◊ELSE (◊k◊ (◊s◊ ◊a)◊ ◊s)$                  ◊◊
                                  $◊ENDIF$                                ◊-◊-◊◊
            $◊ELSE (◊k◊ ◊e◊ ◊s)$                                              ◊◊
            $◊ENDIF$
◊end{denotation}


◊exanswer{denotational/ex:quantum}

Этот макрос помещает вычисление каждого терма в~собственное замыкание, после
чего выполняет все эти вычисления в~произвольном порядке, определяемом функцией
◊ic{determine!}.

◊begin{code:lisp}
(define-syntax unordered
  (syntax-rules ()
    ((unordered f) (f))
    ((unordered f arg ...)
     (determine! (lambda () f) (lambda () arg) ...) ) ) )

(define (determine! . thunks)
  (let ((results (iota 0 (length thunks))))
    (let loop ((permut (random-permutation (length thunks))))
      (if (pair? permut)
          (begin (set-car! (list-tail results (car permut))
                           (force (list-ref thunks (car permut))) )
                 (loop (cdr permut)) )
          (apply (car results) (cdr results)) ) ) ) )
◊end{code:lisp}

Заметьте, что порядок выбирается перед началом вычислений, так что такое
определение не~совсем идентично денотации, приведённой в~этой главе. Если
функция ◊ic{random-permutation} определена вот~так:

◊begin{code:lisp}
(define (random-permutation n)
  (shuffle (iota 0 n)) )
◊end{code:lisp}

◊noindent
то последовательность вычислений выбирается действительно динамически:

◊begin{code:lisp}
(define (d.determine! . thunks)
  (let ((results (iota 0 (length thunks))))
    (let loop ((permut (random-permutation (length thunks))))
      (if (pair? permut)
          (begin (set-car! (list-tail results (car permut))
                           (force (list-ref thunks (car permut))) )
                 (loop [(shuffle (cdr permut))]) )
          (apply (car results) (cdr results)) ) ) ) )
◊end{code:lisp}

◊endgroup %◊ChapterFiveSpecials


% -- Глава 6 ------------------------

◊exanswer{fast/ex:symbol-table}

Самый простой способ — это добавить ◊ic{CHECKED-GLOBAL-REF} ещё один аргумент
с~именем соответствующей переменной:

◊begin{code:lisp}
(define (CHECKED-GLOBAL-REF- i n)
  (lambda ()
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable" n)
          v ) ) ) )
◊end{code:lisp}

Однако такой подход нерационально расходует память и дублирует информацию. Более
правильным решением будет создать специальную таблицу символов для хранения
соответствий между адресами переменных и их именами.

◊begin{code:lisp}
(define sg.current.names (list 'foo))
(define (standalone-producer e)
  (set! g.current (original.g.current))
  (let* ((m (meaning e r.init #t))
         (size (length g.current))
         (global-names (map car (reverse g.current))) )
    (lambda ()
      (set! sg.current (make-vector size undefined-value))
      (set! sg.current.names global-names)
      (set! *env* sr.init)
      (m) ) ) )

(define (CHECKED-GLOBAL-REF+ i)
  (lambda ()
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable"
                 (list-ref sg.current.names i) )
          v ) ) ) )
◊end{code:lisp}


◊exanswer{fast/ex:list}

Функция ◊ic{list} — это, конечно~же, просто ◊ic{(lambda l~l)}. Вам надо
только выразить это определение с~помощью комбинаторов:

◊begin{code:lisp}
(definitial list ((NARY-CLOSURE (SHALLOW-ARGUMENT-REF 0) 0)))
◊end{code:lisp}


◊exanswer{fast/ex:disassemble}

Всё просто: достаточно переопределить каждый комбинатор~◊ii{k} как ◊ic{(lambda
args `(◊ii{k}~. ,args))} и распечатать результат предобработки.


◊exanswer{fast/ex:act-rec-before}

Решение в~лоб: вычислять термы аппликации справа налево:

◊begin{code:lisp}
(define (FROM-RIGHT-STORE-ARGUMENT m m* index)
  (lambda ()
    (let* ([(v* (m*))]
           [(v  (m))] )
      (set-activation-frame-argument! v* index v)
      v* ) ) )

(define (FROM-RIGHT-CONS-ARGUMENT m m* arity)
  (lambda ()
    (let* ([(v* (m*))]
           [(v  (m))] )
      (set-activation-frame-argument!
       v* arity (cons v (activation-frame-argument v* arity)) )
      v* ) ) )
◊end{code:lisp}

Также можно изменить не~порядок вычисления аргументов, а определение
◊ic{meaning*}, чтобы она создавала запись активации первой. В~любом случае
эффективнее будет сначала вычислить функциональный терм (порядок вычисления
остальных аргументов здесь не~важен), так как это позволяет узнать истинную
арность вызываемого замыкания и сразу создавать запись активации правильного
размера.


◊exanswer{fast/ex:redefine}

Определите синтаксис новой специальной формы в~◊ic{meaning}:

◊begin{code:lisp}
... ((redefine) (meaning-redefine (cadr e))) ...
◊end{code:lisp}

◊noindent
Затем реализуйте её предобработку:

◊begin{code:lisp}
(define (meaning-redefine n)
  (let ((kind1 (global-variable? g.init n)))
    (if kind1
        (let ((value (vector-ref sg.init (cdr kind)))
              (kind2 (global-variable? g.current n)) )
          (if kind2
              (static-wrong "Already redefined variable" n)
              (let ((index (g.current-extend! n)))
                (vector-set! sg.current index value) ) ) )
        (static-wrong "Can't redefine variable" n) )
    (lambda () 2001) ) )
◊end{code:lisp}

Подобные переопределения производятся во~время предобработки, ещё до исполнения
программы. Возвращаемое значение формы ◊ic{redefine} не~важно.


◊exanswer{fast/ex:boost-thunks}

Вызов функции без аргументов не~требует выделения памяти под переменные, то есть
расширения текущего окружения. Каждый дополнительный уровень окружения
увеличивает стоимость обращений к~свободным переменным замыканий, что
сказывается на быстродействии. Реализуйте новый комбинатор и добавьте
в~определение ◊ic{meaning-fix-abstraction} обработку соответствующего
специального случая.

◊begin{code:lisp}
(define (THUNK-CLOSURE m+)
  (let ((arity+1 (+ 0 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (= (activation-frame-argument-length v*) arity+1)
            (begin (set! *env* sr)
                   (m+) )
            (wrong "Incorrect arity") ) )
      (make-closure the-function *env*) ) ) )

(define (meaning-fix-abstraction n* e+ r tail?)
  (let ((arity (length n*)))
    (if (= arity 0)
        (let ((m+ (meaning-sequence e+ r #t)))
          (THUNK-CLOSURE m+) )
        (let* ((r2 (r-extend* r n*))
               (m+ (meaning-sequence e+ r2 #t)) )
          (FIX-CLOSURE m+ arity) ) ) ) )
◊end{code:lisp}


% -- Глава 7 ------------------------

◊exanswer{compilation/ex:dynamic}

Сначала создайте новый регистр:

◊begin{code:lisp}
(define *dynenv* -1)
◊end{code:lisp}

◊noindent
Затем сохраняйте его вместе с~остальным окружением:

◊begin{code:lisp}
(define (preserve-environment)
  (stack-push *dynenv*)
  (stack-push *env*) )

(define (restore-environment)
  (set! *env* (stack-pop))
  (set! *dynenv* (stack-pop)) )
◊end{code:lisp}

◊noindent
Теперь динамическое окружение извлекается элементарно; лишь несколько изменилась работа со~стеком:

◊begin{code:lisp}
(define (search-dynenv-index)
  *dynenv* )

(define (pop-dynamic-binding)
  (stack-pop)
  (stack-pop)
  (set! *dynenv* (stack-pop)) )

(define (push-dynamic-binding index value)
  (stack-push *dynenv*)
  (stack-push value)
  (stack-push index)
  (set! *dynenv* (- *stack-index* 1)) )
◊end{code:lisp}


◊exanswer{compilation/ex:load}

Сама функция-то простая:

◊begin{code:lisp}
(definitial load
  (let* ((arity 1)
         (arity+1 (+ 1 arity)) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((filename (activation-frame-argument *val* 0)))
             (set! *pc* (install-object-file! filename)) )
           (signal-exception
            #t (list "Incorrect arity" 'load) ) ) ) ) ) )
◊end{code:lisp}

◊noindent
Но вот при её использовании возникают определённые сложности. Всё дело
в~продолжениях. Допустим, с~помощью ◊ic{load} загружается следующий файл:

◊begin{code:lisp}
(display 'attention)
(call/cc (lambda (k) (set! *k* k)))
(display 'caution)
◊end{code:lisp}

◊noindent
Что случится, если после этого активировать продолжение ◊ic{*k*}? Правильно,
выведется символ ◊ic{caution}! А~потом?

Кроме того, определения глобальных переменных из загружаемого файла не~переходят
в~текущий (что, согласитесь, будет сюрпризом для функций, которые от них
зависят).


◊exanswer{compilation/ex:global-value}

Всё просто:

◊begin{code:lisp}
(definitial global-value
  (let* ((arity 1)
         (arity+1 (+ 1 arity)) )
    (define (get-index name)
      (let ((where (memq name sg.current.names)))
        (if where
            (- (length where) 1)
            (signal-exception
             #f (list "Undefined global variable" name) ) ) ) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let* ((name (activation-frame-argument *val* 0))
                  (i (get-index name)) )
             (set! *val* (global-fetch i))
             (when (eq? *val* undefined-value)
               (signal-exception #f (list "Uninitialized variable" i)) )
             (set! *pc* (stack-pop)) )
           (signal-exception
            #t (list "Incorrect arity" 'global-value) ) ) ) ) ) )
◊end{code:lisp}

Во~время вызова этой функции переменная может как просто не~существовать, так и
ещё не~иметь значения. Оба этих случая необходимо проверять.


◊exanswer{compilation/ex:shallow-dynamic}

Для начала добавьте в~◊ic{run-machine} инициализацию вектора текущего состояния
динамического окружения:

◊begin{code:lisp}
... (set! *dynamics* (make-vector (+ 1 (length dynamics))
                                  undefined-value )) ...
◊end{code:lisp}

◊noindent
После чего переопределите функции-аксессоры на новый лад:

◊begin{code:lisp}
(define (find-dynamic-value index)
  (let ((v (vector-ref *dynamics* index)))
    (if (eq? v undefined-value)
        (signal-exception #f (list "No such dynamic binding" index))
        v ) ) )

(define (push-dynamic-binding index value)
  (stack-push (vector-ref *dynamics* index))
  (stack-push index)
  (vector-set! *dynamics* index value) )

(define (pop-dynamic-binding)
  (let* ((index (stack-pop))
         (old-value (stack-pop)) )
    (vector-set! *dynamics* index old-value) ) )
◊end{code:lisp}

Увы, но такое решение в~общем случае неверно. В~стеке сейчас сохраняются только
предыдущие значения динамических переменных, но не~текущие. Следовательно, любой
переход или активация продолжения приведут к~неправильному состоянию
динамического окружения, так как мы не~сможем восстановить значение
◊ic{*dynamics*} на момент входа в~форму ◊ic{bind-exit} или ◊ic{call/cc}. Чтобы
реализовать данное поведение, необходима форма ◊ic{unwind-protect}; ну, или
можно отказаться от такого подхода в~пользу дальнего связывания, где подобные
проблемы не~возникают в~принципе.


◊exanswer{compilation/ex:export-rename}

С~помощью следующей функции можно выразить даже взаимные переименования вида
◊ic{((fact fib) (fib fact))}. Но не~стоит этим злоупотреблять.

◊begin{code:lisp}
(define (build-application-with-renaming-variables
         new-application-name application-name substitutions )
  (if (probe-file application-name)
      (call-with-input-file application-name
        (lambda (in)
          (let* ((dynamics     (read in))
                 (global-names (read in))
                 (constants    (read in))
                 (code         (read in))
                 (entries      (read in)) )
            (close-input-port in)
            (write-result-file
             new-application-name
             (list ";;; Renamed variables from " application-name)
             dynamics
             (let sublis ((global-names global-names))
               (if (pair? global-names)
                   (cons (let ((s (assq (car global-names)
                                        substitutions )))
                           (if (pair? s)
                               (cadr s)
                               (car global-names) ) )
                         (sublis (cdr global-names)) )
                   global-names ) )
             constants
             code
             entries ) ) ) )
      (signal-exception #f (list "No such file" application-name)) ) )
◊end{code:lisp}


◊exanswer{compilation/ex:unchecked-ref}

Сделать это просто, только не~перепутайте коды инструкций и смещения!

◊begin{code:lisp}
(define-instruction (CHECKED-GLOBAL-REF i) 8
  (set! *val* (global-fetch i))
  (if (eq? val undefined-value)
      (signal-exception #t (list "Uninitialized variable" i))
      (vector-set! *code* (- *pc* 2) 7) ) )
◊end{code:lisp}


% -- Глава 8 ------------------------

◊exanswer{reflection/ex:no-cycles}

Она может не~волноваться об~этом, потому как сравнивает переменные не~по именам.
Такой подход правильно работает даже для списков с~циклами.


◊exanswer{reflection/ex:optimize-ce}

Вот вам подсказка:

◊begin{code:lisp}
(define (prepare e)
  (eval/ce `(lambda () ,e)) )
◊end{code:lisp}


◊exanswer{reflection/ex:no-capture}

◊begin{code:lisp}
(define (eval/at e)
  (let ((g (gensym)))
    (eval/ce `(lambda (,g) (eval/ce ,g))) ) )
◊end{code:lisp}


◊exanswer{reflection/ex:defined}

Да, определив специальный обработчик исключений:

◊begin{code:lisp}
(set! variable-defined?
      (lambda (env name)
        (bind-exit (return)
          (monitor (lambda (c ex) (return #f))
            (eval/b name env)
            #t ) ) ) )
◊end{code:lisp}


◊exanswer{reflection/ex:rnrs}

Реализацию специальной формы ◊ic{monitor}, которая используется в~рефлексивном
интерпретаторе, мы молча пропустим, так как она принципиально непереносима.
В~конце концов, если не~делать ошибок, то ◊ic{monitor} эквивалентна ◊ic{begin}.
Строго говоря, остальной код, что следует далее, тоже не~совсем легален, так
как использует переменные с~именами специальных форм. Однако, большинство
реализаций Scheme допускают такие вольности.

Форма ◊ic{the-environment}, захватывающая привязки:

◊begin{code:lisp}
(define-syntax the-environment
  (syntax-rules ()
    ((the-environment)
     (capture-the-environment make-toplevel make-flambda flambda?
      flambda-behavior prompt-in prompt-out exit it extend error
      global-env toplevel eval evlis eprogn reference quote if set!
      lambda flambda monitor ) ) ) )

(define-syntax capture-the-environment
  (syntax-rules ()
    ((capture-the-environment word ...)
     (lambda (name . value)
       (case name
         ((word) ((handle-location word) value)) ...
         ((display) (if (pair? value)
                        (wrong "Immutable" 'display)
                        show ))
         (else (if (pair? value)
                   (set-top-level-value! name (car value))
                   (top-level-value name) )) ) ) ) ) )

(define-syntax handle-location
  (syntax-rules ()
    ((handle-location name)
     (lambda (value)
       (if (pair? value) (set! name (car value))
           name ) ) ) ) )
◊end{code:lisp}

Функции ◊ic{variable-defined?}, ◊ic{variable-value} и ◊ic{set-variable-value!},
манипулирующие захваченными полноценными окружениями:

◊begin{code:lisp}
(define undefined (cons 'un 'defined))

(define-class Envir Object
  ( name value next ) )

(define (enrich env . names)
  (let enrich ((env env) (names names))
    (if (pair? names)
        (enrich (make-Envir (car names) undefined env)
                (cdr names) )
        env ) ) )

(define (variable-defined? name env)
  (if (Envir? env)
      (or (eq? name (Envir-name env))
          (variable-defined? name (Envir-next env)) )
      #f ) )

(define (variable-value name env)
  (if (Envir? env)
      (if (eq? name (Envir-name env))
          (let ((value (Envir-value env)))
            (if (eq? value undefined)
                (error "Uninitialized variable" name)
                value ) )
          (variable-value name (Envir-next env)) )
      (env name) ) )
◊end{code:lisp}

Как видите, окружения — это связные списки, заканчивающиеся замыканием.
Теперь рефлексивный интерпретатор может быть запущен!


% -- Глава 9 ------------------------

◊exanswer{macros/ex:repeat}

Используйте гигиеничные макросы Scheme:

◊begin{code:lisp}
(define-syntax repeat1
  (syntax-rules (:while :unless :do)
    ((_ :while p :unless q :do body ...)
     (let loop ()
       (if p (begin (if (not q) (begin body ...))
                    (loop) )) ) ) ) )
◊end{code:lisp}

◊noindent
Как вариант, можно всё сделать вручную с~помощью ◊ic{define-abbreviation}:

◊begin{code:lisp}
(with-aliases ((+let let) (+begin begin) (+when when) (+not not))
  (define-abbreviation (repeat2 . params)
    (let ((p    (list-ref  params 1))
          (q    (list-ref  params 3))
          (body (list-tail params 5))
          (loop (gensym)) )
      `(,+let ,loop ()
          (,+when ,p (,+begin (,+when (,+not ,q) . ,body)
                              (,loop) )) ) ) ) )
◊end{code:lisp}


◊exanswer{macros/ex:arg-sequence}

Вся хитрость в~том, как представить числа с~помощью одних только
макроопределений. Один из вариантов — это использовать списки такой~же длины,
что и представляемое ими число. Тогда во~время исполнения программы можно будет
получить нормальные числа с~помощью функции ◊ic{length}.

◊begin{code:lisp}
(define-syntax enumerate
  (syntax-rules ()
    ((enumerate) (display 0))
    ((enumerate e1 e2 ...)
     (begin (display 0)
            (enumerate-aux e1 (e1) e2 ...) ) ) ) )

(define-syntax enumerate-aux
  (syntax-rules ()
    ((enumerate-aux e1 len) (begin (display e1)
                                   (display (length 'len)) ))
    ((enumerate-aux e1 len e2 e3 ...)
     (begin (display e1)
            (display (length 'len))
            (enumerate-aux e2 (e2 . len) e3 ...) ) ) ) )
◊end{code:lisp}


◊exanswer{macros/ex:unique}

Достаточно переопределить функцию ◊ic{make-macro-environment} так, чтобы она
использовала текущий уровень, а не~создавала следующий:

◊begin{code:lisp}
(define (make-macro-environment current-level)
  (let ((metalevel [(delay current-level)]))
    (list (make-Magic-Keyword 'eval-in-abbreviation-world
           (special-eval-in-abbreviation-world metalevel) )
          (make-Magic-Keyword 'define-abbreviation
           (special-define-abbreviation metalevel) )
          (make-Magic-Keyword 'let-abbreviation
           (special-let-abbreviation metalevel) )
          (make-Magic-Keyword 'with-aliases
           (special-with-aliases metalevel) ) ) ) )
◊end{code:lisp}


◊exanswer{macros/ex:decompile}

Написать такой конвертер проще пареной репы. Единственный интересный момент —
это сборка списка аргументов функции. Здесь используется А-список для хранения
соответствий между аргументами и их именами.

◊begin{code:lisp}
(define-generic (->Scheme (e) r))

(define-method (->Scheme (e Alternative) r)
  `(if ,(->Scheme (Alternative-condition e) r)
       ,(->Scheme (Alternative-consequent e) r)
       ,(->Scheme (Alternative-alternant e) r) ) )

(define-method (->Scheme (e Local-Assignment) r)
  `(set! ,(->Scheme (Local-Assignment-reference e) r)
         ,(->Scheme (Local-Assignment-form e) r) ) )

(define-method (->Scheme (e Reference) r)
  (variable->Scheme (Reference-variable e) r) )

(define-method (->Scheme (e Function) r)
  (define (renamings-extend r variables names)
    (if (pair? names)
        (renamings-extend (cons (cons (car variables) (car names)) r)
                          (cdr variables) (cdr names) )
        r ) )
  (define (pack variables names)
    (if (pair? variables)
        (if (Local-Variable-dotted? (car variables))
            (car names)
            (cons (car names) (pack (cdr variables) (cdr names))) )
        '() ) )
  (let* ((variables (Function-variables e))
         (new-names (map (lambda (v) (gensym))
                         variables ))
         (newr (renamings-extend r variables new-names)) )
    `(lambda ,(pack variables new-names)
       ,(->Scheme (Function-body e) newr) ) ) )

(define-generic (variable->Scheme (e) r))
◊end{code:lisp}


◊exanswer{macros/ex:study}

В~текущем состоянии {◊Meroonet} действительно существует в~двух мирах
одновременно. Например, функция ◊ic{register-class} вызывается как во~время
раскрытия макросов, так и в~процессе динамической загрузки файлов.


% -- Глава 10 -----------------------

◊exanswer{cc/ex:boost-calls}

Во-первых, доработайте функцию ◊ic{SCM◊_invoke}: возьмите за основу протокол
вызова примитивов и сделайте подобную специализацию для замыканий. Во-вторых,
не~забудьте передать замыкание самому себе в~качестве первого аргумента.
В-третьих, специализируйте также кодогенераторы для замыканий, чтобы сигнатуры
соответствующих функций совпадали с~тем, чего ожидает ◊ic{SCM◊_invoke}.


◊exanswer{cc/ex:global-check}

Добавьте глобальным переменным флажок, показывающий их инициализированность.
Его начальное значение устанавливается в~функции
◊ic{objectify-free-global-reference}.

◊begin{code:lisp}
(define-class Global-Variable Variable (initialized?))
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(define (objectify-free-global-reference name r)
  (let ((v (make-Global-Variable name #f)))
    (insert-global! v r)
    (make-Global-Reference v) ) )
◊end{code:lisp}

Затем встройте анализ глобальных переменных в~компилятор. Он будет выполняться
обходчиком кода с~помощью обобщённой функции ◊ic{inian!}.

◊indexC{inian"!}
◊begin{code:lisp}
(define (compile->C e out)
  (set! g.current '())
  (let ((prg (extract-things!
              (lift! (initialization-analyze! (Sexp->object e))) )))
    (gather-temporaries! (closurize-main! prg))
    (generate-C-program out e prg) ) )
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(define (initialization-analyze! e)
  (call/cc (lambda (exit)
             (inian! e (lambda () (exit 'finished))) )) )
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(define-generic (inian! (e) exit)
  (update-walk! inian! e exit) )
◊end{code:lisp}

Задачей этой функции будет выявить все глобальные переменные, которые
гарантированно получили значение до того, как это значение кому-то
потребовалось. Сложность выполнения данного анализа зависит от желаемого уровня
общности. Мы выберем простой путь и определим все глобальные переменные, которые
всегда инициализируются.

◊begin{code:lisp}
(define-method (inian! (e Global-Assignment) exit)
  (call-next-method)
  (let ((gv (Global-Assignment-variable e)))
    (set-Global-Variable-initialized! gv #t)
    (inian-warning "Surely initialized variable" gv)
    e ) )
|◊ForLayout{display}{◊vskip-◊baselineskip}|
(define-method (inian! (e Global-Reference) exit)
  (let ((gv (Global-Reference-variable e)))
    (cond ((Predefined-Variable? gv) e)
          ((Global-Variable-initialized? gv) e)
          (else (inian-error "Surely uninitialized variable" gv)
                (exit) ) ) ) )
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(define-method (inian! (e Alternative) exit)
  (inian! (Alternative-condition e) exit)
  (exit) )
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(define-method (inian! (e Application) exit)
  (call-next-method)
  (exit) )
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(define-method (inian! (e Function) exit) e)
◊end{code:lisp}

Анализатор проходит по коду, находит все присваивания глобальным переменным и
останавливается, когда программа становится слишком сложной; то~есть когда он
встречает ветвление или вызов функции. Кстати, ◊ic{lambda}-формы не~являются
«слишком сложным кодом», так как они всегда безошибочно вычисляются за
конечное время и не~трогают глобальные переменные.


% -- Глава 11 -----------------------

◊exanswer{objects/ex:precise-predicate}

◊indexC{Object"?}
Предикат ◊ic{Object?} можно улучшить, добавив в~векторы, которыми представляются
объекты, ещё одно поле, хранящее уникальную метку. Соответственно, также
потребуется изменить аллокаторы, чтобы они заполняли это поле во~всех
создаваемых объектах. (И~не~забыть добавить его в~примитивные классы, которые
определяются вручную.)

◊begin{code:lisp}
(define *starting-offset* 2)
(define meroonet-tag (cons 'meroonet 'tag))
|◊ForLayout{display}{◊vskip-0.333◊baselineskip}|
(define (Object? o)
  (and (vector? o)
       (>= (vector-length o) *starting-offset*)
       (eq? (vector-ref o 1) meroonet-tag) ) )
◊end{code:lisp}

При таком подходе предикат ◊ic{Object?} будет реже ошибаться, но ценой этого
является некоторая потеря быстродействия. Однако, его всё равно можно обмануть,
ведь не~мешает пользователю извлечь метку из любого объекта с~помощью
◊ic{vector-ref} и вставить её в~какой-нибудь другой вектор.


◊exanswer{objects/ex:clone}

◊indexC{clone}
Так как это обобщённая функция, то её можно специализировать для конкретных
классов. Универсальная реализация слишком уж неэффективно расходует память:

◊begin{code:lisp}
(define-generic (clone (o))
  (list->vector (vector->list o)) )
◊end{code:lisp}


◊exanswer{objects/ex:metaclass}

Определите новый класс классов: метакласс ◊ic{CountingClass}, у~которого есть
поле для подсчёта создаваемых объектов.

◊begin{code:lisp}
(define-class CountingClass Class (counter))
◊end{code:lisp}

К~счастью, {◊Meroonet} написана так, что для её расширения не~требуется изменять
половину существующих определений. Новый метакласс можно определить как-то так:

◊begin{code:lisp}
(define-meroonet-macro (define-CountingClass name super-name
                                             own-fields )
  (let ((class (register-CountingClass name super-name own-fields)))
    (generate-related-names class) ) )

(define (register-CountingClass name super-name own-fields)
  (CountingClass-initialize! (allocate-CountingClass)
                             name
                             (->Class super-name)
                             own-fields ) )
◊end{code:lisp}

Однако более правильным решением будет расширить синтаксис формы
◊ic{define-class} так, чтобы она принимала тип создаваемого класса (по~умолчанию
◊ic{Class}). При этом потребуется сделать некоторые функции обобщёнными:

◊begin{code:lisp}
(define-generic (generate-related-names (class)))

(define-method (generate-related-names (class Class))
  (Class-generate-related-names class) )

(define-generic (initialize! (o) . args))

(define-method (initialize! (o Class) . args)
  (apply Class-initialize o args) )

(define-method (initialize! (o CountingClass) . args)
  (set-CountingClass-counter! class 0)
  (call-next-method) )
◊end{code:lisp}

Обновлять значение поля ◊ic{counter} будут, конечно~же, аллокаторы нового
метакласса:

◊begin{code:lisp}
(define-method (generate-related-names (class CountingClass))
  (let* ((cname      (symbol-concatenate (Class-name class) '-class))
         (alloc-name (symbol-concatenate 'allocate- (Class-name class)))
         (make-name  (symbol-concatenate 'make- (Class-name class))) )
    `(begin ,(call-next-method)
            (set! ,alloc-name                 ; аллокатор
                  (let ((old ,alloc-name))
                    (lambda sizes
                      (set-CountingClass-counter! ,cname
                       (+ 1 (CountingClass-counter ,cname)) )
                      (apply old sizes) ) ) )
            (set! ,make-name                  ; конструктор
                  (let ((old ,make-name))
                    (lambda args
                      (set-CountingClass-counter! ,cname
                       (+ 1 (CountingClass-counter ,cname)) )
                      (apply old args) ) ) ) ) ) )
◊end{code:lisp}

В~качестве заключения рассмотрим пример использования данного метакласса:

◊begin{code:lisp}
(define-CountingClass CountedPoint Object (x y))

(unless (and (= 0 (CountingClass-counter CountedPoint-class))
             (allocate-CountedPoint)
             (= 1 (CountingClass-counter CountedPoint-class))
             (make-CountedPoint 11 22)
             (= 2 (CountingClass-counter CountedPoint-class)) )
  ;; не~выполнится, если всё в~порядке
  (meroonet-error "Failed test on CountedPoint") )
◊end{code:lisp}


◊exanswer{objects/ex:field-reflection}

Определите метакласс ◊ic{ReflectiveClass}, обладающий дополнительными полями:
◊ic{predicate}, ◊ic{allocator} и~◊ic{maker}. Затем измените определение
генератора сопутствующих функций, чтобы он заполнял эти поля при создании
экземпляра класса. Аналогичные действия необходимо выполнить для классов полей
(наследников ◊ic{Field}).

◊begin{code:lisp}
(define-class ReflectiveClass Class (predicate allocator maker))

(define-method (generate-related-names (class ReflectiveClass))
  (let ((cname      (symbol-concatenate (Class-name class) '-class))
        (pred-name  (symbol-concatenate (Class-name) '?))
        (alloc-name (symbol-concatenate 'allocate- (Class-name class)))
        (make-name  (symbol-concatenate 'make- (Class-name class))) )
    `(begin ,(call-next-method)
            (set-ReflectiveClass-predicate! ,cname ,pred-name)
            (set-ReflectiveClass-allocator! ,cname ,alloc-name)
            (set-ReflectiveClass-maker!     ,cname ,make-name) ) ) )
◊end{code:lisp}


◊exanswer{objects/ex:auto-generic}

Главная сложность здесь в~том, как узнать, существует~ли уже обобщённая
функция или нет. В~Scheme нельзя определить, существует или нет глобальная
переменная, поэтому придётся искать имя функции в~списке ◊ic{*generics*}.

◊begin{code:lisp}
(define-meroonet-macro (define-method call . body)
  (parse-variable-specifications
   (cdr call)
   (lambda (discriminant variables)
     (let ((g (gensym)) (c (gensym)))
       `(begin
          [(unless (->Generic ',(car call))]
            [(define-generic ,call) )]
          (register-method
           ',(car call)
           (lambda (,g ,c)
             (lambda ,(flat-variables variables)
               (define (call-next-method)
                 ((if (Class-superclass ,c)
                      (vector-ref (Generic-dispatch-table ,g)
                                  (Class-number (Class-superclass ,c)) )
                      (Generic-default ,g) )
                  . ,(flat-variables variables) ) )
               . ,body ) )
           ',(cadr discriminant)
           ',(cdr call) ) ) ) ) ) )
◊end{code:lisp}


◊exanswer{objects/ex:next-method}

Просто добавьте в~определение каждого метода пару локальных функций
◊ic{call-next-method} и ◊ic{next-method?}. Несомненно, было~бы лучше сделать
так, чтобы эти функции создавались только тогда, когда они действительно
используются, но это реализовать сложнее.

◊begin{code:lisp}
(define-meroonet-macro (define-method call . body)
  (parse-variable-specifications
   (cdr call)
   (lambda (discriminant variables)
     (let ((g (gensym)) (c (gensym)))
       `(register-method
         ',(car call)
         (lambda (,g ,c)
           (lambda ,(flat-variables variables)
             [,@(generate-next-method-functions g c variables)]
             . ,body ) )
         ',(cadr discriminant)
         ',(cdr call) ) ) ) ) )
◊end{code:lisp}

Функция ◊ic{next-method?} похожа на ◊ic{call-next-method}, но она только ищет
суперметод, не~вызывая его.

◊begin{code:lisp}
(define (generate-next-method-functions g c variables)
  (let ((get-next-method (gensym)))
    `((define (,get-next-method)
        (if (Class-superclass ,c)
            (vector-ref (Generic-dispatch-table ,g)
                        (Class-number (Class-superclass ,c)) )
            (Generic-default ,g) ) )
      (define (call-next-method)
        ((,get-next-method) . ,(flat-variables variables)) )
      (define (next-method?)
        (not (eq? (,get-next-method) (Generic-default ,g))) ) ) ) )
◊end{code:lisp}
