#lang pollen
◊subsection[#:label "assignment/side-effects/ssect:equality"]{Равенство}

◊indexR{сравнение!объектов}
◊indexR{взаимозаменимость}
◊indexR{эквивалентность}
◊indexR{равенство}
◊;index{упячка}
Одним из главных неприятных последствий существования побочных эффектов является усложнение понятия равенства.
Когда можно считать два~объекта равными?
По~мнению~Лейбница, объекты эквивалентны, если~они взаимозаменимы.
В~программировании объекты взаимозаменимы, если~их нельзя~различить.
Отношение эквивалентности обладает свойством рефлексивности:
любой объект эквивалентен самому себе, так как очевидно, что он может заменить самого себя.
Рефлексивность можно считать слабой эквивалентностью;
именно~так мы различаем числа: каждое отдельное число эквивалентно только самому~себе.
Однако всё усложняется, когда помимо самих объектов нам доступны ещё и преобразования между~ними.
Например, пусть у~нас есть пара аналогичных выражений,
вроде ◊nobr{◊ic{(* 2 2)} и ◊ic{(+ 2 2)}} — одно~и~то~же~ли~они, взаимозаменимы~ли~они?

Если объекты можно изменять, то~два объекта различны, если изменение одного не~влияет на~другой.
Заметьте, что мы говорим о~◊emph{различии}, а~не~о~равенстве,
но~для понятия равенства подобные различия существенны:
изменяемые объекты ведут себя иначе, чем~неизменные.

◊; TODO: "символы алфавита" -- ты где-то в предисловии называл из "знаками" (characters),
◊; как насчёт того, чтобы последовательно, везде называть их "символами алфавита" или типа того?
◊indexR{эквивалентность!атомов}
◊indexR{значения!атомарные}
Кронекер говорил, что целые числа созданы~Богом,
поэтому логично считать их неизменными — число~3 остаётся числом~3 в~любом выражении.
Также логично будет считать неизменяемыми и~прочие атомарные объекты —
то~есть те, которые не~делятся на составляющие:
символы~алфавита, булевы значения, пустой список.
Все~остальные объекты являются составными, а~значит, не~первозданными, и~следовательно, не~обязательно неизменными.

◊indexR{идентичность}
◊indexR{значения!составные}
◊indexC{equal?}
◊indexC{eq?}
Составные объекты вроде списков, строк, векторов логично считать равными,
если они состоят из равных частей.
Именно такой смысл равенства олицетворяют всевозможные варианты
◊footnote{Например, ◊ic{equalp} и~◊ic{tree-equal} в~◊|CommonLisp|.}
предиката ◊ic{equal?}.
К~сожалению, если содержимое объектов изменяется, то~равные объекты тут~же становятся разными.
Поэтому существует также предикат~◊ic{eq?}, проверяющий идентичность:
два~объекта равны в~смысле~◊ic{eq?}, если это один и~тот~же объект.
Такая проверка очень эффективно реализуется сравнением адресов:
если объекты имеют один и~тот~же адрес в~памяти,
то~очевидно, что это один и~тот~же объект.

◊; TODO: вот эти bigskip... что если вместо них просто разбивать страницу на меньшие?
◊bigskip

◊indexR{тождественность}
Итак, сейчас у~нас есть два~предиката:
◊ic{eq?} для проверки идентичности и ◊ic{equal?} для проверки структурного равенства.
Но~оба они впадают в~крайности, когда дело касается изменяемых объектов.
Равенство тождественно идентичности для неизменяемых объектов — два равных неизменных объекта такими навсегда и~останутся.
Однако изменяемые объекты тождественны (всегда~равны) лишь тогда, когда они идентичны (то~есть суть один и~тот~же объект).
Именно поэтому разумно понимать эквивалентность как взаимозаменимость:
объекты~эквивалентны, если ни~одна программа не~сможет их~различить.

Предикат~◊ic{eq?} очень низкоуровневый:
он~сравнивает адреса объектов в~памяти
◊footnote{
  В~случае распределённых вычислений ◊ic{eq?}~может иметь дело с~объектами, расположенными на различных компьютерах.
  Поэтому низкоуровневость этого предиката ещё не~означает молниеносность его~работы.
}
и~непосредственные константы.
В~зависимости от~реализации,
эквивалентные значения могут иметь различные представления в~памяти.
◊footnote{
  Например, в~Scheme форма ◊nobr{◊ic{(eq? 33 #x21)}} не~обязана возвращать~◊ic{#t}.
}
Естественно, ◊ic{eq?} не~будет считать такие объекты равными,
несмотря на их полную неразличимость иным способом.
Таким образом, ◊ic{eq?} не~обладает рефлексивностью и~не~определяет отношение эквивалентности.

◊indexC{eqv?}
◊indexC{eql}
Существует улучшенный вариант~◊ic{eq?}, который в~Scheme называется~◊ic{eqv?}, ◊nobr{а~в~◊CommonLisp — ◊ic{eql}}.
В~целом, это тот~же предикат~◊ic{eq?}, но~объекты сравниваются немного тщательнее,
чтобы точно убедиться в~эквивалентности неизменяемых величин.
Например, при сравнении чисел ◊ic{eqv?} проверит, что они равны, даже если по-разному представляются в~памяти:
с~учётом всех форматов, точности, длинной арифметики, и~тому~подобного.
◊; TODO: я уже где-то это писал, но не забудь сделать так, чтобы "по-разному" было в ◊nobr и не разбивалось на дефисе

◊bigskip

◊indexR{точечные пары!неизменяемые}
◊indexR{связывание!неизменяемое}
Если критически рассмотреть наши предыдущие интерпретаторы, то~легко заметить, как~мало в~них изменяемых объектов.
Практически всё, за~исключением привязок, никогда не~изменяется.
Действительно, в~большинстве программ совсем немного активно изменяемых данных,
а~большинству точечных~пар удаётся избежать встречи с~◊ic{set-car!} и~◊ic{set-cdr!}
В~некоторых диалектах~ML, а~также в~◊CommonLisp можно явно указывать неизменяемые поля объектов.
Вот~так, например, определяется неизменяемая точечная~пара:

◊code:lisp[#:dialect CommonLisp]{
(defstruct immutable-pair
  (car ’() :read-only t)
  (cdr ’() :read-only t) )
}

Неизменяемые объекты можно размещать в~отдельной области памяти.
Также это отличный способ представления констант в~программе.

◊indexC{egal}
Генри~Бейкер предложил ◊seeCite{bak93} унифицировать предикаты равенства,
объединив их в~один — ◊ic{egal},
◊trnote{Немецкое слово ◊german{egal} и французское ◊french{égal} буквально означают «равен».}
определяемый следующим образом:
если сравниваемые объекты изменяемы, то~◊ic{egal} ведёт себя как~◊ic{eq?},
а~неизменяемые объекты сравниваются как~◊ic{equal?}.
Таким образом, равенство объектов в~смысле~◊ic{egal} гарантирует их взаимозаменимость.
Очень удобный предикат, например, в~параллельном ◊seeCite{qd93,que94} программировании.

◊indexR{сравнение!циклических объектов}
◊indexR{циклические структуры данных}
◊indexC{equal?!стоимость}
Ещё~одним интересным частным случаем являются циклические структуры данных.
Хоть их и~возможно сравнивать, порой это обходится довольно дорого.
Если ◊ic{equal?} не~знает, как обращаться с~циклами, то~она попросту там застрянет.
И~даже если мы каким-либо образом определим наличие цикла,
то~всё ещё не~ясно, как~именно проводить сравнение.
Допустим, мы имеем дело со~следующими объектами:

◊; TODO: ◊input{figures/fig4.cycle}

◊code:lisp{
(define o1 (let ((pair (cons 1 2)))
             (set-car! pair pair)
             (set-cdr! pair pair)
             pair ))
(define o2 (let ((pair (cons (cons 1 2))))
             (set-car! (car pair) pair)
             (set-cdr! (car pair) pair)
             (set-cdr! pair pair)
             pair ))
}

Предположим, мы хотим вычислить~◊nobr{◊ic{(equal? o1 o1)}}.
Если ◊ic{equal?} сначала проверяет~◊ic{eq?} и~лишь~затем —
если~ей переданы разные объекты, которые действительно могут оказаться не~эквивалентны, —
выполняется структурная проверка равенства,
то~такая реализация ◊ic{equal?} быстро подтвердит равенство.
Иная~же реализация попадёт в~бесконечный~цикл.
◊footnote{
  Я~запускал этот пример в~четырёх различных интерпретаторах~Scheme и~половина из~них зациклились.
  Я~не~буду их называть, ведь такое поведение разрешено стандартом ◊cite{kcr98}
}

Ладно, пусть результат ◊nobr{◊ic{(equal? o1 o1)}} зависит от~реализации;
тогда~что насчёт ◊nobr{◊ic{(equal? o1 o2)}}?
Ответ зависит от~того, изменяемы или~нет точечные пары, из~которых состоят ◊ic{o1} и~◊ic{o2}.
Если их нельзя изменять, то~без~◊ic{eq?} нельзя написать программу, которая~бы смогла отличить ◊ic{o1} от~◊ic{o2}.
В~общем, лучше всего воздержаться от сравнения циклических структур данных с~помощью~◊ic{equal?}.
◊; TODO: что насчёт знаков препинания в "◊ic{...?}." нужна ли эта точка? здесь и в других местах

◊bigskip

◊indexR{сравнение!символов}
Рассмотренные предикаты равенства поддерживают большую часть используемых типов данных,
за~исключением символов и~функций.
Символы порой являются не~вполне атомарными,
так~как реализации часто используют их, например, для хранения значений одноимённых глобальных переменных.
В~принципе, символами можно считать не~только имена сами по~себе,
но~и~связанные с~этими именами уникальные объекты.

◊indexR{списки свойств}
◊indexR{символы!списки свойств}
◊indexR{хеш-таблицы}
◊; TODO: "списки свойств"? ты точно использовал именно такой термин?
Символы также часто поддерживают списки свойств —
довольно опасное расширение языка, ведь это фактически глобальная изменяемая структура данных.
Причём, не~особо эффективная структура данных,
как~с~точки зрения использования памяти (по~две точечные~пары на~каждое свойство),
так~и по~производительности (список требует линейнего поиска).
◊emph{Хеш-таблицы}
◊footnote{
  По~моему скромному мнению, изобретение хеш-таблиц является одним из величайших достижений информатики.
}
здесь подходят гораздо~лучше.
