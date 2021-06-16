#lang pollen

◊subsection[#:label "lisp1-2-omega/namespaces/ssect:dyn-vars"]{Динамические переменные}

◊indexR{динамические переменные}
◊indexR{динамическое окружение}
◊indexR{переменные!динамические}
◊indexR{окружение!динамическое}
◊term{Динамические переменные} принципиально отличаются от лексических,
поэтому имеет смысл ввести для них отдельное окружение.
Следующая таблица показывает желаемые свойства нового окружения — окружения динамических переменных:

◊envtable{
  ◊tr{◊td{Ссылка}      ◊td{не~может быть получена}                       }
  ◊tr{◊td{Значение}    ◊td{◊ic{(dynamic ◊ii{d})}}                        }
  ◊tr{◊td{Изменение}   ◊td{◊ic{(dynamic-set! ◊ii{d} ...)}}               }
  ◊tr{◊td{Расширение}  ◊td{◊ic{(dynamic-let (... (◊ii{d} ...) ...) ...)}}}
  ◊tr{◊td{Определение} ◊td{здесь не~рассматривается}                     }
}

Окружение учитывает новые особенности:
локальные динамические переменные
◊footnote{
  Термин «локальные» здесь не~совсем удачный,
  так как поведение динамических переменных кардинально отличается от поведения обычных (лексических) переменных.
}
создаются с~помощью формы ◊ic{dynamic-let}, подобной ◊ic{let} и~◊ic{flet};
значение динамической переменной получается с~помощью ◊ic{dynamic},
а~изменяется — с~помощью ◊ic{dynamic-set!}.

◊indexE{Lisp-3@◊(Lisp3)}
◊indexR{Лисп!Lisp-3@◊(Lisp3)}
Пока что это три специальные формы, но далее будут рассмотрены другие варианты реализации.
А~сейчас мы всего лишь добавим в~наш интерпретатор ◊ic{f.evaluate} поддержку динамического окружения: ◊ic{denv}.
Это окружение будет содержать только динамические переменные.
Новый интерпретатор, назовём его~◊(Lisp3), будет использовать функции с~префиксом~◊ic{df.}
Вот его код.
(Форма~◊ic{flet} не~показана для краткости.)

◊indexC{df.evaluate}
◊indexC{df.evaluate-application}
◊indexC{df.make-function}
◊indexC{df.eprogn}
◊code:lisp{
(define (df.evaluate e env fenv denv)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e)
                 (boolean? e) (vector? e) )
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
                    env
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

(define (df.evaluate-application fn args env fenv denv)
  (cond ((symbol? fn) ((f.lookup fn fenv) args denv))
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (df.eprogn (cddr fn)
                    (extend env (cadr fn) args)
                    fenv
                    denv ) )
        (else (wrong "Incorrect functional term" fn)) ) )

(define (df.make-function variables body env fenv)
  (lambda (values denv)
    (df.eprogn body (extend env variables values) fenv denv) ) )

(define (df.eprogn e* env fenv denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (begin (df.evaluate (car e*) env fenv denv)
                 (df.eprogn (cdr e*) env fenv denv) )
          (df.evaluate (car e*) env fenv denv) )
      empty-begin ) )
}

Для поддержки нового окружения ◊ic{denv} потребовалось изменить прототипы ◊ic{df.evaluate} и ◊ic{df.eprogn},
чтобы не~терять это окружение при вычислениях.
Далее, ◊ic{df.evaluate} определяет три новые специальные формы для операций над ◊ic{denv}, динамическим окружением.
Есть и менее заметные изменения:
◊ic{df.evaluate-application} передаёт в~функции ◊emph{текущее} динамическое окружение.
Мы уже встречались с~таким поведением,
когда были вынуждены передавать текущее лексическое окружение в~вызываемую функцию.
◊seePage{basics/representing-functions/fixing/src:inject-current-env}

◊indexR{динамическое окружение!варианты}
◊indexR{окружение!динамическое!варианты}
Для применения функции используются одновременно несколько окружений.
Во-первых, окружения с~переменными и функциями, захваченными при определении функции.
Теперь также присутствует окружение с~динамическими переменными, существующими в~момент вызова.
Это окружение не~может быть захвачено и сохранено в~замыкании —
значение динамических переменных всегда извлекается из~текущего динамического окружения.
Если значение там отсутствует, то, конечно~же, такая программа содержит ошибку.
Возможны и другие варианты реализации:
например, просто сделать все глобальные переменные динамическими, как в~◊(ISLisp);
или выдать каждому модулю собственное глобальное динамическое окружение, как в~◊(EuLisp);
или даже иметь глобальное окружение с~лексическими переменными, как в~◊(CommonLisp).
◊seePage{lisp1-2-omega/namespaces/ssect:dyn-vars-cl}

Одно из преимуществ отдельного окружения:
становится чётко видно, какие переменные динамические, а~какие~нет.
При всяком обращении к~динамическому окружению необходимо использовать специальную форму с~префиксом ◊ic{dynamic}.
Явные обращения к~динамическим переменным невозможно не~заметить.
Это очень важно, так как в~◊(Lisp3) поведение функции определяется не~только значениями локальных переменных,
но~и~текущим состоянием динамического окружения.

◊indexR{привязки (bindings)!динамические}
◊indexR{обработка ошибок!динамические переменные}
Среди традиционных вариантов использования динамических окружений наиболее полезным является обработка ошибок.
При возникновении ошибки или исключительной ситуации создаётся некий объект, описывающий, что произошло,
и~к~этому объекту применяется соответствующая функция, которая обработает исключительную ситуацию
(и,~возможно, попытается восстановить работоспособность программы).
Этот обработчик мог~бы быть общей глобальной функцией,
но такой подход требует нежелетельных присваиваний для использования правильного обработчика в~правильное время.
«Герметичность» лексических окружений плохо сочетается со~сквозным указанием различных функций-обработчиков.
Хотя мы всегда можем ограничить время жизни локальных переменных,
заключив вычисления в~◊ic{let} или ◊ic{dynamic-let} —
у~◊ic{dynamic-let} есть несколько серьёзных преимуществ:

◊enumerate{
  ◊item{создаваемые привязки не~могут быть захвачены;}
  ◊item{эти привязки доступны только во~время вычислений, вложенных в~форму;}
  ◊item{привязки автоматически уничтожаются после завершения вычислений.}
}

◊noindent{Поэтому ◊ic{dynamic-let} идеально подходит для временной установки функций-обработчиков ошибок.}

Вот ещё один пример разумного использования динамических переменных.
Функции вывода в~◊(CommonLisp) настраиваются динамическими переменными вроде ◊ic{*print-base*}, ◊ic{*print-circle*}, и~т.~д.
В~них хранится информация об~основании счисления для вывода чисел,
о~том, как представлять циклические структуры данных, и~тому подобное.
Конечно, можно передавать всю эту информацию через аргументы,
но только представьте, что вместо ◊ic{(print ◊ii{выражение})} приходилось~бы писать
◊ic{(print ◊ii{выражение} ◊ii{escape-символы?} ◊ii{основание} ◊ii{циклы?} ◊ii{pretty-print?}
◊ii{регистр?} ◊ii{уровень-вложенности} ◊ii{векторы?} ◊ii{использовать-gensym?})}.
Каждый раз.
Динамические переменные позволяют один раз установить значения по умолчанию для таких параметров
и больше никогда их не~указывать, если не~требуется особого поведения.

◊indexR{циклические структуры данных}
Scheme использует похожий механизм для указания портов ввода-вывода
в~формах ◊ic{(display~◊ii{выражение})} и ◊ic{(display~◊ii{выражение}~◊ii{порт})}.
◊footnote{
  Лисп очень щепетильно относится к~равенству —
  в~том числе к~равенству возможностей разработчиков языка и программмистов.
  Функция ◊ic{display} может принимать как один, так и два аргумента,
  но в~Scheme нет какой-либо особой поддержки такого синтаксиса.
  В~свою очередь, Лисп позволяет функциям иметь необязательные аргументы.
}
Первая форма, с~одним аргументом, выводит ◊ii{выражение} в~текущий порт вывода.
Вторая~же использует явно указанный порт.
Текущий порт изменяется функцией ◊ic{with-output-to-file}, но только на время вычисления переданного ей выражения.
Узнать текущий порт можно с~помощью функции ◊ic{current-output-port}.
Рассмотрим в~качестве примера утилиту для печати циклических списков.
◊footnote{См.~также реализацию функции ◊ic{list-length} в~◊(CommonLisp).}

◊indexC{display-cyclic-spine}
◊code:lisp{
(define (display-cyclic-spine list)
  (define (scan l1 l2 flip)
    (cond ((atom? l1)  (unless (null? l1)
                          (display " . ") (display l1) )
                       (display ")") )
          ((eq? l1 l2) (display "...)"))
          (else        (display (car l1))
                       (when (pair? (cdr l1)) (display " "))
                       (scan (cdr l1)
                             (if (and flip (pair? l2))
                                 (cdr l2)
                                 l2 )
                             (not flip) ) ) ) )
  (display "(")
  (scan list (cons 42 list) #f) )

(display-cyclic-spine        ; напечатает (1 2 3 4 1 ...)
  (let ((l (list 1 2 3 4)))
    (set-cdr! (cdddr l) l)
    l ) )
}

Можно даже составить таблицу характеристик для портов вывода Scheme:

◊envtable{
  ◊tr{◊td{Ссылка}      ◊td{автоматически, если не~упоминается явно}                 }
  ◊tr{◊td{Значение}    ◊td{◊ic{(current-output-port)}}                              }
  ◊tr{◊td{Изменение}   ◊td{запрещено}                                               }
  ◊tr{◊td{Расширение}  ◊td{◊ic{(with-output-to-file ◊ii{имя-файла} ◊ii{замыкание})}}}
  ◊tr{◊td{Определение} ◊td{неприменимо}                                             }
}

◊indexC{standard-output@*standard-output*}
◊indexR{соглашения именования!динамических переменных}
В~◊(CommonLisp) данный механизм явно использует динамические переменные.
По~умолчанию функции вывода вроде ◊ic{print} или ◊ic{write} используют порт, хранящийся в~переменной ◊ic{*standard-output*}.
◊footnote{По соглашению, имена динамических переменных выделяются звёздочками.}
Функция ◊ic{with-output-to-file} реализуется
◊footnote{Разумеется, не~в~◊(CommonLisp), а~в~нашем~◊(Lisp3).}
следующим образом:

◊indexC{with-output-to-file}
◊code:lisp{
(define (with-output-to-file filename thunk)
  (dynamic-let ((*standard-output* (open-input-file filename)))
    (thunk) ) )
}