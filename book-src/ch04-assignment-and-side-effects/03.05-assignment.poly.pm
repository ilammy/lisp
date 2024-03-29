#lang pollen

◊subsection[#:label "assignment/implementation/ssect:assignment"]{Присваивание}

Для присваивания необходимо промежуточное продолжение:

◊indexC{evaluate-set!}
◊code:lisp{
(define (evaluate-set! n e r s k)
  (evaluate e r s
    (lambda (v ss)
      (k v (update ss (r n) v)) ) ) )
}

Вычисленное новое значение передаётся продолжению,
которое записывает его в~память по адресу обновляемой переменной.
Дальше промежуточное продолжение передаёт новое состояние памяти изначальному продолжению формы~◊ic{set!}.
Новое значение также становится значением самой формы присваивания.

Теперь понятно, зачем мы представляем память в~виде функции?
Такой~подход позволяет определить изменение значений переменных без использования побочных эффектов.
Фактически, память представляется историей изменений своего состояния.
Конечно~же, по~сравнению с~реальной памятью этот вариант является жутко неэффективным и избыточным,
однако ведение истории изменений имеет свои преимущества.
◊seeEx{assignment/ex:previous-value}
