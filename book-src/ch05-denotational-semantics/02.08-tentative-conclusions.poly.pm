#lang pollen

◊subsection[#:label "denotational/sematics/ssect:conclusions"]{Предварительные выводы}

Вот~и~всё — шаг за~шагом мы определили функцию, которая ставит в~соответствие каждой программе аналогичный ◊${\lambda}-терм.
Очевидно, такая функция существует, ведь мы руководствовались принципом композициональности при её определении.
Если допускать синтаксически рекурсивные программы,
◊cite{que92a}
то~потребуется немного больше математики, чтобы исчерпывающим образом доказать корректность подобного преобразования.

◊indexE{Scheme!семантика}
Теперь вы можете своими глазами убедиться в~том, что семантика ядра Scheme умещается на одной странице:

◊; TODO: это должна быть красивая таблица
◊table:semantic-denotation[#:label "denotational/sematics/fig:naked-scheme"]{
◊code:denotation{
(define (meaning-reference n)
  (lambda (r k s)
    (k (s (r n)) s) ) )

(define ((meaning-assignment n e) r k s)
  ((meaning e) r
               (lambda (v s1)
                 (k v (extend s1 (r n) v)) )
               s ) )

(define ((meaning-alternative e1 e2 e3) r k s)
  ((meaning e1) r
                (lambda (v s1)
                  (ef (boolify v)
                      ((meaning e2) r k s1)
                      ((meaning e3) r k s1) ) )
                s ) )

(define ((meaning-abstraction n* e+) r k s)
  (k (inValue (lambda (v* k1 s1)
                (if (= (length v*) (length n*))
                    (allocate s1 (length n*)
                              (lambda (s2 a*)
                                ((meaning*-sequence e+)
                                 (extend* r n* a*)
                                 k1
                                 (extend* s2 a* v*) ) ) )
                    (wrong "Incorrect arity") ) ))
     s ) )

(define ((meaning-application e e*) r k s)
  ((meaning e) r
               (lambda (f s1)
                 ((meaning* e*) r
                                (lambda (v* s2)
                                  ((Value->Function f) v* k s2) )
                                s1 ) )
               s ) )

(define ((meaning-some-arguments e e*) r k s)
  ((meaning e) r
               (lambda (v s1)
                 ((meaning* e*) r
                                (lambda (v* s2)
                                  (k (cons v v*) s2) )
                                s1 ) )
               s ) )

(define ((meaning-no-argument) r k s)
  (k (list) s) )

(define ((meaning-sequence e+) r k s)
  ((meaning*-sequence e+) r k s) )

(define ((meaning*-single-sequence e) r k s)
  ((meaning e) r k s) )

(define ((meaning*-multiple-sequence e e+) r k s)
  ((meaning e) r
               (lambda (v s1)
                 ((meaning*-sequence e+) r k s1) )
               s ) )

(definitial call/cc
  (inValue
   (lambda (v1* k1 s1)
     (if (= 1 (length v1*))
         ((Value->Function (car v1*))
          (list (inValue
                 (lambda (v2* k2 s2)
                   (if (= 1 (length v2*))
                       (k1 (car v2*) s2)
                       (wrong "Incorrect arity" 'k) ) ) ))
          k1
          s1 )
         (wrong "Incorrect arity" 'call/cc) ) ) ) )
}
◊caption{Сущность Scheme}
}

Конечно, специальная форма ◊ic{quote} здесь отсутствует
(как~вы помните, с~точным определением цитирования не~всё так~просто ◊seePage{assignment/sect:quotation}),
функции с~переменной арностью также отложены на~потом.
Кроме~того, не~хватает определения ◊ic{eq?} для сравнения функций, а~также множества других примитивов.
Правда, у~нас есть простейшее формальное определение ◊ic{call/cc}.
На~самом деле важно то, что полученный базис позволяет определить все остальные функции
вроде ◊ic{car}, ◊ic{cons}, ◊ic{set-cdr!} без каких-либо дополнительных уточнений или изменений семантики.
Теперь они по~сути являются тривиальным расширением библиотеки прикладных функций и~не~представляют существенного интереса.
Для~понимания семантического ядра языка достаточно специальных форм и~некоторых примитивных функций вроде~◊ic{call/cc}.

Как~бы то ни~было, мы~настаиваем на~том, что выражение сути Scheme с~подобной точностью и лаконичностью стоит затраченных усилий.
Таким образом, наше путешествие достигло кульминации:
мы~начали в~первой главе с~определения подмножества Scheme с~помощью всего Scheme,
а~теперь мы определили весь Scheme с~помощью одного лишь ◊${\lambda}-исчисления.
