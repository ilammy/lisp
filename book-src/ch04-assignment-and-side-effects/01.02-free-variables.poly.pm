#lang pollen

◊subsection[#:label "assignment/assignment/ssect:free-vars"]{Присваивание свободным~переменным}

Следующая проблема касается свободных переменных: как~трактовать присваивание для~них.
Рассмотрим пример:

◊code:lisp{
(let ((passwd "timhukiTrolrk"))   ; Кто-то засветил свой пароль!
  (set! can-access? (lambda (pw) (string=? password (crypt pw)))) )
}

Здесь переменная ◊ic{can-access?} является свободной и~ей присваивается новое значение.
По~правилам Scheme ◊ic{can-access?} должна быть глобальной переменной,
так~как в~области видимости не~видно ни~одной локальной связывающей формы для~неё.
Но~ведь то, что переменная ◊emph{должна} быть где-то в~глобальном окружении, вовсе не~означает, что она там~есть!
Что~делать, если такой переменной не~существует?
Мы~уже разговаривали на~эту тему,
◊seePage{lisp1-2-omega/recusion/simple/code:redefine} ◊; TODO: recusion? не пропустил ли ты r?
рассмотрев несколько возможных вариантов поведения.

Что ещё можно делать с~глобальными переменными — кроме присваивания?
Да~в~общем-то всё~то~же, что и с~любыми другими:
ссылаться на них, захватывать в~замыкания, получать их значения, определять перед использованием.
Глобальное окружение переменных тоже является пространством имён со~своими правилами,
поэтому давайте рассмотрим подробнее различные варианты его реализации.
