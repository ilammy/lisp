#lang pollen

◊chapter[#:label "chapter:denotational"]{Денотационная семантика}

◊initial{Эта глава} начинается кратким обзором ◊${\lambda}-исчисления,
после чего переходит к~денотационной семантике во~всей её~красе.
Мы~рассмотрим ещё одно определение Лиспа — денотационное, —
которое слабо отличается от предыдущих интерпретаторов по~смыслу, но~сильно отличается по~сути:
программы теперь будет представлены чисто математическими объектами — термами~◊${\lambda}-исчисления.

◊; TODO: помнишь же? разбить такие страницы на несколько?
◊bigskip

◊indexR{смысл программ}
◊indexR{программы!смысл}
◊indexR{язык!и смысл программ}
Чем именно является программа?
◊term{Программа} — это описание вычислений, которые приводят к~определённому результату: значению ◊nobr{и/или}~эффекту.

Очень часто программы путают с~их исполнимым воплощением для той или иной машины.
Аналогично, определением программы часто считают файл с~её исходным кодом.
Однако всё это совершенно различные понятия.

Программы записываются на некотором ◊term{языке};
определение языка придаёт корректным программам ◊term{смысл}.
Смысл программы — это не~только и не~столько получаемые результаты вычислений,
так как они зависят от исходных данных, от событий во~внешнем мире, от действий других программ.
Смысл программы — это~нечто большее, это фундаментальная суть проводимых вычислений.

Если мы хотим изучать смысл программ и обрабатывать их, то~они обязаны иметь математическое представление.
Возьмём любое преобразование, например, перевод программы в~«коробочный стиль» из предыдущей главы.
◊seePage{assignment/assignment/boxes/p:boxing}
Как~мы можем быть уверены, что после подобной операции смысл программы остаётся неизменным?
Только с~помощью тысячелетнего опыта математики — науки об~отношениях, структуре и преобразованиях.
А~для этого необходимо связать смысл программ с~математическими объектами.
Такой подход кажется вполне разумным и полезным:
например, если~бы функция~◊ic{fact} выражалась буквально через факториал,
то~стало~бы гораздо проще убедиться в~том, что она вычисляет именно факториал,
или~что другие функции, вроде~◊ic{meta-fact},
◊seePage{lisp1-2-omega/recursion/y-combinator/code:meta-fact}
действительно ей эквивалентны.

Для определения смысла программы необходим её математический эквивалент,
а~для его построения требуется в~точности понимать свойства языка, на~котором программа записана.
В~общем, проблема сводится к~отысканию способа построения математических эквивалентов всех конструкций языка.
То~есть к~формализации его ◊term{семантики}, что оказывается весьма полезным.
Понимание семантики позволяет реализовать язык в~любом окружении,
доказывать правильность программ и их преобразований,
сравнивать языки между собой, и~многое~другое.
Действительно, применений семантики великое множество, но~семантика далеко не~уникальна в~подобных возможностях.

◊indexR{эталонная реализация}
Одним из древнейших способов определения языков является предоставление ◊term{эталонной~реализации}.
Когда необходима некая информация о~языке, например, последствия выполнения какой-либо программы,
то~вопрос возносится к~эталонной реализации и вскоре оракул даёт ответ:
каким должно быть возвращаемое значение или побочные эффекты.
Однако, как~и с~любым другим чёрным ящиком, очень непросто построить полную теорию его работы, пользуясь лишь отрывочными наблюдениями.
Если мы решим открыть ящик, то внутри окажется ещё одна программа на каком-то языке,
что возвращает нас к~исходному вопросу о~смысле программ.

◊indexR{виртуальная машина}
Следующий подход основывается на идее ◊term{виртуальной машины}.
Это не~решает проблему одним махом, но~разделяет её на две~задачи попроще.
Сперва необходимо описать конструкции языка с~помощью ограниченного числа операций вычислительной машины определённой архитектуры.
Теперь~же, полностью понимая, как~функционирует эта машина, мы~получаем и полное понимание языка.
Сама виртуальная машина является формальной абстракцией, так что может быть реализована на любой реальной вычислительной машине.

Многие языки определяются именно таким образом:
◊|PL/I|~(на~VDM), PSL◊seeCite{gbm82}, ◊|LeLisp|~(на~LLM3◊seeCite{cha80}), Gambit поверх~PVM◊seeCite{fm90}.

◊indexR{семантика!операционная}
Главная сложность здесь — это разработать хорошую виртуальную машину.
Это~не~так легко, как~кажется:
виртуальная машина должна одновременно хорошо подходить для языка, быть простой в~использовании и тривиальной в~реализации.
На~выбор есть множество вариантов: стековые машины, регистровые, основанные на деревьях, графах — в~общем, всё, что душе угодно.
(Здесь мы сначала выбираем идеальный язык ассемблера, а~затем подгоняем под него архитектуру машины.)
В~этом случае сравнение программ сводится к~сравнению соответствующих им машинных кодов или~же хода их исполнения машиной.
Так~как в~итоге всё упирается в~работу вычислительной машины, виртуальной или реальной,
то~подобный способ определения языка называется ◊term{операционной~семантикой}.

◊indexR{денотация!определение}
Однако у~такого подхода есть врождённый недостаток:
каждому языку необходима специальная вычислительная машина — иными словами, формальная теория вычислений.
Если в~качестве такой теории взять какую-нибудь простую, всем понятную концепцию,
то~можно избавиться от поддержки зоопарка всевозможных машин.
Программа — это, прежде всего, функция, преобразующая входные данные в~выходные.
Для этого не~требуется сложная машина: математика веками оттачивала такой способ «исполнения программ», называя его «применением функций».
Таким образом, идея состоит в~том, чтобы преобразовать программу в~функцию (из определённого множества функций).
Подобная функция называется ◊term{денотацией} программы.
Теперь остаётся определить и понять вышеупомянутое множество денотаций.

◊indexR{лямбда-исчисление@◊${\lambda}-исчисление}
◊indexR{лямбда-терм@◊${\lambda}-терм}
◊indexR{терм!лямбда-исчисления@◊${\lambda}-исчисления}
◊indexR{семантика!денотационная}
Для наших целей прекрасно подойдёт ◊${\lambda}-исчисление.
Его аксиомы настолько просты, что их толкование не~вызывает никаких разногласий.
Таким образом, семантику языка программирования можно понимать как способ перевода программ в~соответствующие денотации.
Этот процесс, между~прочим, тоже можно представить как математическую функцию.
Денотация программы — это выражение ◊${\lambda}-исчисления (◊term{◊${\lambda}-терм}), представляющее смысл программы.
Вооружившись теорией ◊${\lambda}-исчисления, мы теперь в~состоянии сказать, эквивалентны~ли две~программы, предсказать результат вычислений, и~т.~д.
Конечно, остаётся определить ещё множество тонкостей:
используемый вариант ◊${\lambda}-исчисления,
способ определения семантической функции, преобразующей программы в~денотации,
и~многое другое,
но~общая идея — это~именно то, что называется ◊term{денотационной~семантикой}.

◊indexR{семантика!аксиоматическая}
Стоит упомянуть ещё один способ понимания программ, особо полезный для доказательства их корректности.
Речь идёт об~◊term{аксиоматической семантике} Ричарда~Флойда и~Тони~Хоара.
◊; TODO: эта формула должна рендериться корректно
Идея состоит в~том, что для всех конструкций языка составляются логические утверждения вида ◊${{P}◊text{◊ii{форма}}{Q}},
что~означает: если~утверждение~◊${P} истинно перед исполнением~◊ii{формы}, то~в~результате её исполнения станет истинным~◊${Q}.
В~итоге конструкции языка сводятся к~набору подобных утверждений-аксиом, из~которых выводятся утверждения-теоремы о~поведении программ.
Такой подход несомненно удобен для доказательства корректности программ,
но,~к~сожалению, это неконструктивное определение языка, которое ничего не~говорит о~том, как реализовать вычислитель для~него.
Ни~даже о~том, возможно~ли построить такой вычислитель в~принципе.

◊indexR{семантика!естественная}
◊indexR{семантика!алгебраическая}
Конечно~же, многообразие семантик не~исчерпывается перечисленными вариантами.
Например, существует ◊term{естественная семантика}◊seeCite{kah87}, подобная денотационной, но~с~большим упором на логический вывод свойств программ из~аксиом.
Или~◊term{алгебраическая семантика}◊seeCite{ff89}, рассматривающая правила эквивалентных преобразований программ.
