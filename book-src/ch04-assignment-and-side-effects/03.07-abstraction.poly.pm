#lang pollen

◊subsection[#:label "assignment/implementation/ssect:abstraction"]{Абстракция}

◊indexC{lambda!и~память}
Для простоты будем считать, что специальная форма ◊ic{lambda} поддерживает только функции с~фиксированным количеством аргументов.
При создании функции происходит две вещи:
в~памяти выделяется новая ячейка,
затем конструируется новый объект — замыкание, — которое и размещается в~только что выделенной ячейке памяти.
Очевидно, что созданную функцию надо где-то хранить для дальнейшего использования,
поэтому форма ◊ic{lambda} вынуждена изменять состояние памяти.
Работа по~созданию и сохранению замыканий перекладывается на функцию ◊ic{create-function},
которой передаётся адрес выделенного участка памяти и код функции —
именно~тот, который замыкание возвращает в~ответ на сообщение ◊ic{behavior}.

◊indexC{evaluate-lambda}
◊code:lisp{
(define (evaluate-lambda n* e* r s k)
  (allocate 1 s
    (lambda (a* ss)
      (k (create-function
          (car a*)
          (lambda (v* s k)
            (if (= (length n*) (length v*))
                (allocate (length n*) s
                  (lambda (a* ss)
                    (evaluate-begin e*
                                    (update* r  n* a*)
                                    (update* ss a* v*)
                                    k ) ) )
                (wrong "Incorrect arity") ) ) )
          ss ) ) ) )
}

При~вызове функции подразумевается, что каждый фактический аргумент размещается в~собственной ячейке памяти.
Поэтому сперва выделяется столько~же новых ячеек, сколько аргументов передано функции,
затем выделенные ячейки инициализируются фактически вычисленными значениями аргументов,
а~после этого уже управление передаётся собственно телу функции.

◊indexR{функции!в~Фортране}
Возможен и другой вариант реализации:

◊code:lisp{
(define (evaluate-ftn-lambda n* e* r s k)
  (allocate (+ 1 (length n*)) s
    (lambda (a* ss)
      (k (create-function
          (car a*)
          (lambda (v* s k)
            (if (= (length n*) (length v*))
                (evaluate-begin e*
                                (update* r n* (cdr a*))
                                (update* s (cdr a*) v*)
                                k )
                (wrong "Incorrect arity") ) ) )
      ss ) ) ) )
}

◊noindent
В~этом случае место в~памяти под аргументы выделяется лишь один раз при создании функции,
как поступает, например,~Фортран.
Однако при таком подходе мы лишаемся рекурсии —
ведь теперь нет возможности хранить несколько отдельных наборов аргументов для каждого вызова функции.
Фортран не~поддерживает динамическое создание замыканий,
а~запрет на~рекурсию позволяет немного ускорить исполнение программ,
так~как память под аргументы выделяется один раз при компиляции по~известным адресам.
В~то~же время, в~функциональных языках вроде Лиспа никуда без~рекурсии и~замыканий.
