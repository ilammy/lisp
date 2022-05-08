#lang pollen
◊subsection[#:label "denotational/semantics/ssect:application"]{Аппликация}

◊indexR{интерпретатор!E-seq@◊${◊|Eval|^*}}
◊indexE{E-seq@◊${◊|Eval|^*}, интерпретатор}
Функции создаются для того, чтобы их применяли, поэтому на~очереди аппликация.
Как~обычно, нам потребуется вспомогательный интерпретатор:
◊${◊|Eval|^*\!}, денотационный аналог~◊ic{evlis}.

◊code:denotation{
(define ((meaning-application e e*) r k s)
  ((meaning e) r
               (lambda (f s1)
                 ((meaning* e*) r
                                (lambda (v* s2)
                                  ((Value->Function f) v* k s2) )
                                s1 ) )
               s ) )

(define (meaning* e*)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*))
      (meaning-no-argument) ) )

(define ((meaning-some-arguments e e*) r k s)
  ((meaning e) r
               (lambda (v s1)
                 ((meaning* e*) r
                                (lambda (v* s2)
                                  (k (append (list v) v*) s2) )
                                s1 ) )
               s ) )

(define ((meaning-no-argument) r k s)
  (k (list) s) )
}

◊; TODO: кернинг
Продолжения ◊${◊|Eval|^*} работают со~списками значений, а~не~с~отдельными значениями, в~отличие~от~◊${◊|Eval|^+\!}.
В~приведённых выше определениях продолжения~◊${◊k} имеют~тип

◊$${
◊Vset{Значения}^* \times ◊Vset{Память} \to ◊Vset{Значение}
}
