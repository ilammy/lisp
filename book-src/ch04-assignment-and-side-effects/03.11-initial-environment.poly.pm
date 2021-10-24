#lang pollen

◊subsection[#:label "assignment/implementation/ssect:init-env"]{Начальное окружение}

Как~обычно, мы определим два вспомогательных макроса,
которые помогут построить начальное окружение (а~также состояние~памяти).
Под каждую глобальную переменную необходимо выделить ячейку памяти,
адрес которой будет храниться в~глобальном окружении.
Макрос ◊ic{definitial} выделяет память и кладёт туда указанное значение.

◊indexC{s.global}
◊indexC{r.global}
◊indexC{definitial}
◊code:lisp{
(define s.global s.init)
(define r.global r.init)

(define-syntax definitial
  (syntax-rules ()
   ((definitial name value)
    (allocate 1 s.global
     (lambda (a* ss)
       (set! r.global (update r.global 'name (car a*)))
       (set! s.global (update ss (car a*) value)) ) ) ) ) )
}

◊noindent
Макрос ◊ic{defprimitive} определяет глобальные функции.

◊indexC{defprimitive}
◊code:lisp{
(define-syntax defprimitive
  (syntax-rules ()
   ((defprimitive name value arity)
    (definitial name
     (allocate 1
      s.global
      (lambda (a* ss)
        (set! s.global (expand-store (car a*) ss))
        (create-function (car a*)
         (lambda (v* s k)
           (if (= arity (length v*))
               (value v* s k)
               (wrong "Incorrect arity" 'name ) )) ) ) ) ) ) ) )
}

По~уже сложившейся традиции, определим глобальные переменные для истины, лжи и~пустого списка:

◊code:lisp{
(definitial t (create-boolean #t))
(definitial f (create-boolean #f))
(definitial nil the-empty-list)
}

В~качестве примера определения примитивов покажем предикат и арифметическую операцию.
Они проверяют типы аргументов, извлекают значения, выполняют соответствующие действия,
и~наконец упаковывают результат обратно во~внутреннее представление.

◊code:lisp{
(defprimitive <=
  (lambda (v* s k)
    (if (and (eq? ((car v*)  'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-boolean (<= ((car v*) 'value)
                               ((cadr v*) 'value) )) s)
        (wrong "Numbers required" '<=) ) )
  2 )

(defprimitive *
  (lambda (v* s k)
    (if (and (eq? ((car v*)  'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-number (* ((car v*) 'value)
                             ((cadr v*) 'value) )) s)
        (wrong "Numbers required" '*) ) )
  2 )
}
