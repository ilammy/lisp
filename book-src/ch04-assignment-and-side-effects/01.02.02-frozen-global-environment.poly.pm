#lang pollen

◊; TODO: фиксированное? может "неизменяемое"?
◊subsubsection*[#:label "assignment/assignment/free-vars/sssect:frozen"]{Неизменяемое глобальное~окружение}

◊indexR{глобальное окружение!неизменяемое}
◊indexR{окружение!глобальное!неизменяемое}
◊indexR{неизменяемое окружение}
Теперь представим, что с~каждым именем связано не~более одной глобальной переменной,
а~список определённых переменных заранее известен и~неизменен.
Такая ситуация возникает в~скомпилированной программе без динамически исполняемого~кода (без~вызовов~◊ic{eval}).
Именно так себя ведут и~все наши предыдущие интерпретаторы,
которые не~поддерживают определение новых глобальных переменных —
все переменные созданы заранее с~помощью формы~◊ic{definitial}.

В~таком окружении переменная существует только после явного создания формой~◊ic{define}.
Значение переменной можно считывать и изменять только после того, как переменная была определена.
Тем не~менее, существование не~более одной переменной с~уникальным именем всё~же позволяет ссылаться на переменные, которые ещё не~были определены.
(Это~необходимо для взаимной рекурсии.)
Так~как имена переменных уникальны, определить ещё одну переменную с~таким~же именем уже~нельзя.
В~итоге, окружение обладает следующими свойствами:

◊envtable{
  ◊tr{◊td{Ссылка}      ◊td{◊ii{x}}                                               }
  ◊tr{◊td{Значение}    ◊td{◊ii{x}, но ◊ii{x} должна существовать}                }
  ◊tr{◊td{Изменение}   ◊td{◊ic{(set! ◊ii{x} ...)}, но ◊ii{x} должна существовать}}
  ◊tr{◊td{Расширение}  ◊td{◊ic{define} (единожды)}                               }
  ◊tr{◊td{Определение} ◊td{запрещено}                                            }
}

Теперь попробуем запустить предыдущий пример в~новом окружении:

◊; TODO: колонка комментариев и значения (P 10) должна быть на одном и том же уровне во всех примерах
◊code:lisp{
g                           ; ошибка: неизвестная переменная ◊ic{g}
(define (P m) (* m g))      ; опережающая ссылка на ◊ic{g}
(define g 10)
(define g 9.81)             ; ошибка: переопределение ◊ic{g}
(set! g 9.81)               ; изменение ◊ic{g}
(P 10)           ◊(is) 98.1
(set! e 2.78)               ; ошибка: неизвестная переменная ◊ic{e}
}

Теперь легко сообразить, как перенести программы в~подобное окружение.
Пусть программа представлена последовательностью выражений ◊${\pi_1 \dots \pi_n}.
Заворачиваем их в~◊nobr{◊ic{let}-форму}, где перечислены все свободные переменные,
присутствущие в~◊${\pi_1 \dots \pi_n}.
Затем все формы ◊ic{define} заменяются эквивалентными ◊nobr{◊ic{set!}-формами}.

Для пояснения рассмотрим следующую программу на~Scheme:

◊code:lisp{
(define (crypt pw) ...)

(let ((passwd "timhukiTrolrk"))
  (set! can-access? (lambda (pw) (string=? passwd (crypt pw)))) )

(define (gatekeeper)
  (until (can-access? (read))
    (gatekeeper) ) )
}

Эта небольшая программка спрашивает у~пользователя пароль и не~отпускает его, пока не~услышит правильный ответ.
В~новом окружении наш ◊nobr{кибер-Цербер} выглядит вот~так:

◊; TODO: форматирование окружения
◊code:lisp{
(let (crypt make-can-access? can-access? gatekeeper)
  (set! crypt (lambda (pw) ...))
  (set! make-can-acceess?
        (lambda (passwd)
          (lambda (pw) (string?= passwd (crypt pw))) ) )
  (set! can-access? (make-can-access? "timhukiTrolrk"))
  (set! gatekeeper
        (lambda () (until (can-access? (read))
                     (gatekeeper) )) )
  (gatekeeper) )◊where{
                | ◊ic{car} ◊(eq) ◊ic{car}
                | ◊ic{cons} ◊(eq) ◊ic{cons}
                | ...
                }
}

Глобальные переменные теперь определяются локально формой~◊ic{let}, которая оставляет их неинициализированными.
Формы ◊ic{define} превращаются в~◊ic{set!}, выполняя инициализацию.
◊footnote{
  К~сожалению, сейчас повторное определение переменных считается допустимым.
  Для~точного соответствия необходимо внести коррективы в~процедуру преобразования ◊ic{define} в~◊ic{set!},
  запретив подобные выражения на~синтаксическом уровне.
}
Естественно, встроенные функции вроде ◊ic{read}, ◊ic{string=?} или ◊ic{string-append} всё так~же остаются видимыми.
В~этом мире глобальное окружение конечно, неизменяемо, и~ограничено предопределёнными функциями (вроде ◊ic{car} и~◊ic{cons}),
а~также свободными переменными, использующимися в~программе.
Присваивания иным глобальным переменным невозможны, по~определению.
Единственный способ совершить такую ошибку — это~динамическое исполнение кода с~помощью~◊ic{eval}.