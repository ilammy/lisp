#lang pollen

◊; TODO: Интепретатор с~продолжениями?
◊subsection[#:label "escape/actors/ssect:interpreter"]{Интерпретатор}

◊; TODO: эти indexE должны попасть в раздел идентификаторов, а не просто английских терминов
◊indexR{соглашения именования}
◊indexE{e@◊ic{e} (выражения)}
◊indexE{r@◊ic{r} (лексическое окружение)}
◊indexE{k@◊ic{k} (продолжения)}
◊indexE{v@◊ic{v} (значения)}
◊indexE{f@◊ic{f} (функции)}
◊indexE{n@◊ic{n} (идентификаторы)}
Функция ◊ic{evaluate} принимает три аргумента: выражение, окружение и продолжение.
Первым делом выполняется синтаксический анализ выражения, чтобы понять, как его дальше интерпретировать.
Каждому синтаксическому классу выражений соответствует собственный метод.
Прежде чем продолжить, давайте договоримся о~правилах именования переменных, которых теперь будет довольно много.
Первое правило: сущность «список~◊ii{x}» записывается со~звёздочкой:~◊ic{◊ii{x}*}.
Второе правило: сущности интерпретатора будем называть одной-двумя буквами для~краткости:

◊; TODO: выравнивание в таблице, центрирование
◊table{
  ◊tr{◊td{◊ic{e}, ◊ic{et}, ◊ic{ec}, ◊ic{ef}} ◊td{выражения, формы}}
  ◊tr{◊td{                           ◊ic{r}} ◊td{окружения}}
  ◊tr{◊td{                  ◊ic{k}, ◊ic{kk}} ◊td{продолжения}}
  ◊tr{◊td{                           ◊ic{v}} ◊td{значения (числа, пары, замыкания, и~т.~д.)}}
  ◊tr{◊td{                           ◊ic{f}} ◊td{функции}}
  ◊tr{◊td{                           ◊ic{n}} ◊td{идентификаторы}}
}

Всё, теперь принимаемся за интерпретатор.
Для простоты будем считать все атомы, кроме имён переменных, автоцитированными значениями.

◊indexC{evaluate}
◊code:lisp{
(define (evaluate e r k)
  (if (atom? e)
      (cond ((symbol? e) (evaluate-variable e r k))
            (else        (evaluate-quote e r k)) )
      (case (car e)
        ((quote)  (evaluate-quote  (cadr e) r k))
        ((if)     (evaluate-if     (cadr e) (caddr e) (cadddr e) r k))
        ((begin)  (evaluate-begin  (cdr e) r k))
        ((set!)   (evaluate-set!   (cadr e) (caddr e) r k))
        ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
        (else     (evaluate-application (car e) (cdr e) r k)) ) ) )
}

Собственно интерпретатор состоит из трёх функций: ◊ic{evaluate}, ◊ic{invoke} и~◊ic{resume}.
Две последние являются обобщёнными и знают, как вызывать вызываемое и продолжать продолжаемое.
Все вычисления в~конечном итоге сводятся к~обмену значениями между этими функциями.
Вдобавок мы введём ещё пару полезных обобщённых функций для работы с~переменными: ◊ic{lookup} и~◊ic{update!}.

◊indexC{invoke}
◊indexC{resume}
◊indexC{lookup}
◊indexC{update!}
◊code:lisp{
(define-generic (invoke (f) v* r k)
  (wrong "Not a function" f r k) )

(define-generic (resume (k continuation) v)
  (wrong "Unknown continuation" k) )

(define-generic (lookup (r environment) n k)
  (wrong "Not an environment" r n k) )

(define-generic (update! (r environment) n k v)
  (wrong "Not an environment" r n k) )
}

Все сущности, которыми мы будем оперировать, наследуются от трёх базовых классов:

◊indexC{value}
◊indexC{environment}
◊indexC{continuation}
◊code:lisp{
(define-class value        Object ())
(define-class environment  Object ())
(define-class continuation Object (k))
}

◊noindent
Значения являются наследниками ◊ic{value};
классы окружений — наследники ◊ic{environment};
а~◊ic{continuation}, разумеется, объединяет все продолжения.
