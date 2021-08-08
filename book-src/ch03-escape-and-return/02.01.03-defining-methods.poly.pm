#lang pollen

◊subsubsection*{Определение методов}

◊indexC{define-method}
Форма ◊ic{define-method} используется для специализации обобщённых функций конкретными методами:

◊code:lisp{
(define-method (◊ii{функция} ◊ii{аргументы})
  ◊ii{тело}... )
}

Аргументы указываются аналогично ◊ic{define-generic}.
Вместе с~дискриминантом указывается класс, для которого определяется метод.
Например, вот так определяется метод ◊ic{invoke} для класса ◊ic{primitive}:

◊code:lisp{
(define-method (invoke (f primitive) v* r k)
  ((primitive-address f) v* r k) )
}

На~этом мы заканчиваем обзор объектной системы и переходим к~написанию интерпретатора.
Детали реализации объектов будут рассмотрены в~одиннадцатой главе.
Здесь мы ограничимся наиболее простыми и известными возможностями,
чтобы упростить код и избежать недопонимания.