#lang pollen

◊; TODO: неудачное название, игра слов с "актёрами" потерялась
◊section[#:label "escape/sect:actors"]{Участники вычислений}

◊indexR{вычисления!контекст}
◊indexR{контекст вычислений}
На~текущем уровне понимания мы считаем, что в~вычислениях участвуют три сущности: выражение, окружение, продолжение.
Тактическая цель вычислений: определить значение выражения в~окружении,
а~стратегическая — передать это значение продолжению.

◊; TODO: стековые фреймы => стековые кадры, везде
◊; TODO: записи активации, без "й"
◊indexR{записи активации}
◊indexR{кадры стека}
◊indexR{стековые кадры}
Наш новый интерпретатор покажет, какие продолжения требуются на каждом этапе вычислений.
Обычно продолжения представляются в~виде стековых кадров или записей активации,
поэтому интерпретатор будет написан в~объектно-ориентированном~стиле.
