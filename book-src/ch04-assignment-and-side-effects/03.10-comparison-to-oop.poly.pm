#lang pollen

◊subsection[#:label "assignment/implementation/ssect:cmp-to-oop"]{Сравнение с~объектным~подходом}

◊indexR{замыкания (closures)!и объекты}
◊indexR{объекты!как замыкания}
Замыкания, реагирующие на сообщения, с~помощью которых мы представляем значения внутри нового интерпретатора,
во~многом напоминают объекты из предыдущей главы.
Тем~не~менее, между подобными объектами-замыканиями и объектами~◊Meroonet есть ряд отличий.
Объекты-замыкания все поддерживаемые методы замыкают внутри~себя —
нет~возможности добавлять однажды созданным объектам новые методы.
Понятия классов и подклассов здесь присутствуют лишь умозрительно.
В~то~же время, обобщённые функции позволяют расширять поведение объектов в~любое время и в~любом месте.
С~помощью обобщённых функций можно реализовать как методы, так и мультиметоды.
Объекты из предыдущей главы поддерживают наследование,
что позволяет выносить общие свойства в~базовые классы, избегая дублирования кода.
Поэтому не~стоит думать, что объекты — это «замыкания для бедных», как считают некоторые фанатики~Scheme.
Ведь даже простые объекты из предыдущей главы оказываются весьма выразительными.
Правда, это вовсе не~означает, что замыкания не~способны ни~на~что интересное.
◊seeCite{ar88}
