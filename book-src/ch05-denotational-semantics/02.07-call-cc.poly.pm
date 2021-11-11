#lang pollen

◊subsection[#:label "denotational/semantics/ssect:call/cc"]{◊ic{call/cc}}

Разумеется, наше знакомство с~денотационной семантикой было~бы неполным
без~определения существенной для Scheme функции — ◊ic{call/cc}.
Глобальная примитивная функция ◊ic{call/cc} определяется следующим образом:

◊; TODO: в оргинале в книге inValue зафакаплено, не сломай в своём трансформере
◊code:denotation{
(definitial call/cc
  (inValue
   (lambda (v1* k1 s1)
     (if (= (length v1*) 1)
         ((Value->Function (car v1*))
          (list (inValue
                 (lambda (v2* k2 s2)
                   (if (= (length v2*) 1)
                       (k1 (car v2*) s2)
                       (wrong "Incorrect arity" 'k) ) ) ))
          k1
          s1 )
         (wrong "Incorrect arity" 'call/cc) ) ) ) )
}

Обратите внимание на переходы между доменами ◊${◊Vset{Значений}} и~◊${◊Vset{Функций}}.
Сама~денотация не~сложнее любого другого из способов определения ◊ic{call/cc},
которые были рассмотрены ранее в~третьей~главе.
◊seePage{escape/implementation/ssect:call/cc}
