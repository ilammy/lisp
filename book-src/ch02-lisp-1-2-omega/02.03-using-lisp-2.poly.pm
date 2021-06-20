#lang pollen

◊subsection[#:label "lisp1-2-omega/lisp2/ssect:using" #:alt "Используем Lisp-2"]{Используем ◊(Lisp2)}

Чтобы полностью определить наш ◊(Lisp2), остаётся указать начальное функциональное окружение
и~запустить в~цикле сам интерпретатор, ◊ic{f.evaluate}.
Определение глобального функционального окружения мало чем отличается от окружения переменных:
надо всего лишь расширять другое начальное окружение.

◊indexC{fenv.global}
◊indexC{definitial-function}
◊indexC{defprimitive}
◊code:lisp{
(define fenv.global '())

(define-syntax definitial-function
  (syntax-rules ()
    ((definitial-function name)
     (begin (set! fenv.global (cons (cons 'name 'void) fenv.global))
            'name) )
    ((definitial-function name value)
     (begin (set! fenv.global (cons (cons 'name value) fenv.global))
            'name) ) ) )

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial-function name
       (lambda (values)
         (if (= arity (length values))
             (apply value values)
             (wrong "Incorrect arity"
              (list 'name 'values) ) ) ) ) ) ) )

(defprimitive car car 1)
(defprimitive cons cons 2)
}

◊noindent
И~наконец:

◊code:lisp{
(define (◊fbox{◊ii{определённый ◊(Lisp2)}})
  (define (toplevel)
    (display (f.evaluate (read) env.global fenv.global))
    (toplevel) )
  (toplevel) )
}
