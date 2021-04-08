#lang pollen

◊subsection[#:label "basics/evaluating-forms/ssect:abstraction"]{Абстракция}

◊indexR{абстракция}
Функции (также называемые ◊term{процедурами} в~Scheme)
являются результатом вычисления специальной формы ◊ic{lambda},
чьё имя ссылается на понятие ◊term{абстракции} в~◊nobr{◊${\lambda}-исчислении}.
Работу по созданию функции мы поручаем функции~◊ic{make-function},
которой передаём всё необходимое:
список аргументов, тело функции и текущее окружение.

◊code:lisp{
... (case (car e)
      ((lambda) (make-function (cadr e) (cddr e) env)) ... ) ...
}
