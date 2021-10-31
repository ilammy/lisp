#lang pollen

◊subsection[#:label "assignment/implementation/ssect:environment"]{Окружения}

Окружения представляются функциями, которые отображают имена переменных в~адреса.
Аналогично, память — это~функция, преобразующая адреса в~значения.
Начальное окружение, естественно, пустое:

◊indexC{r.init}
◊code:lisp{
(define (r.init id)
  (wrong "No binding for" id) )
}

◊indexR{память!чисто функциональная}
◊indexR{чисто функциональные структуры данных}
◊indexR{побочные эффекты}
Итак, как~же выразить изменение окружения (или~памяти), не~используя побочные эффекты или присваивание?
Изменение памяти означает изменение значения, связанного с~адресом.
Функция~◊ic{update} изменяет память по~известному адресу следующим образом:

◊indexC{update}
◊code:lisp{
(define (update s a v)
  (lambda (aa)
    (if (eqv? a aa) v (s aa)) ) )
}

Так~как память — это~функция~◊ic{s}, которая принимает адреса и~возвращает значения,
то~изменённая память — это очень похожая функция,
где по~адресу~◊ic{a} расположено новое значение~◊ic{v}, а~всё~остальное — как~в~◊ic{s}.
Функцию~◊ic{update} можно легко обобщить для изменения сразу нескольких значений:

◊indexC{update*}
◊code:lisp{
(define (update* s a* v*)
  ;; ◊ic{(assume (= (length a*) (length v*)))}
  (if (pair? a*)
      (update* (update s (car a*) (car v*)) (cdr a*) (cdr v*))
      s ) )
}

Аналогичным образом реализуется расширение окружений, поэтому нам не~нужна отдельная функция ◊ic{extend}.
Если~бы интерпретатор был написан на~ML, то~◊ic{update} была~бы полиморфной функцией, так~как окружения и память имеют разные~типы.
Также необходимо учитывать, что ◊ic{update} требуется некий критерий сравнения адресов,
поэтому для их представления стоит выбрать гарантированно сравнимые объекты: например,~целые~числа.
Имена переменных мы будем представлять символами.
Оба~типа данных прекрасно поддерживаются ◊ic{eqv?}, поэтому в~◊ic{update} используется именно этот~предикат.
