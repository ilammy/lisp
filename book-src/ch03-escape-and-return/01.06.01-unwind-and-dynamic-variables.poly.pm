#lang pollen

◊subsubsection*{◊ic{unwind-protect} и динамические~переменные}

◊indexC{fluid-let}
◊indexC{unwind-protect!динамические переменные}
◊indexR{динамические переменные!◊ic{unwind-protect}}
Некоторые реализации Scheme реализуют динамические переменные не~так, как мы рассматривали ранее.
Вместо отдельного окружения они используют ◊ic{unwind-protect} или аналогичный механизм.
Идея состоит в~том, чтобы «одолжить» нужную лексическую переменную, восстановив впоследствии её значение обратно.
Подобные динамические переменные реализуются с~помощью формы ◊ic{fluid-let}:

◊; TODO: таблица?
◊code:lisp{
(fluid-let ((x ◊${\alpha}))
  ◊${\beta}... )
}
◊(eq)
◊code:lisp{
(let ((◊ii{tmp} x))
  (set! x ◊${\alpha})
  (unwind-protect
    (begin ◊${\beta}...)
    (set! x ◊ii{tmp}) ) )
}

На~время выполнения~◊${\beta} переменная~◊ic{x} принимает значение~◊${\alpha}.
Предыдущее значение ◊ic{x} сохраняется в~локальной переменной~◊ii{tmp}
и~восстанавливается после завершения~◊${\beta}.
Реализация подразумевает, что существует такая переменная~◊ic{x}, которой можно временно воспользоваться.
Обычно используется глобальная переменная, чтобы она была видна отовсюду, как и положено динамической переменной.
Если~же ◊ic{fluid-let} заимствует локальную переменную~◊ic{x},
то её поведение будет (значительно) отличаться от поведения динамических переменных в~◊|CommonLisp|,
так как новое значение будет видно лишь внутри формы ◊ic{fluid-let} и только там.
Далее, очевидно, что ◊ic{fluid-let} также не~дружит с~◊ic{call/cc} и неограниченными продолжениями.
В~итоге получается нечто ещё более хитрое, чем динамические переменные ◊|CommonLisp|.