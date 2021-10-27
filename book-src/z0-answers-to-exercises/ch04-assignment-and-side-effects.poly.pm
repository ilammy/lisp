#lang pollen

◊exanswer{assignment/ex:pure-min-max}

Существует огромное множество способов реализовать эту функцию.
Например, можно возвращать промежуточные результаты или использовать продолжения:

◊indexC{min-max}
◊code:lisp{
(define (min-max1 tree)
  (define (mm tree)
    (if (pair? tree)
        (let ((a (mm (car tree)))
              (b (mm (cdr tree))) )
          (list (min (car a) (car d))
                (max (cadr a) (cadr d)) ) )
        (list tree tree) ) )
  (mm tree) )

(define (min-max2 tree)
  (define (mm tree k)
    (if (pair? tree)
        (mm (car tree)
            (lambda (mina maxa)
              (mm (cdr tree)
                  (lambda (mind maxd)
                    (k (min mina mind)
                       (max maxa maxd) ) ) ) ) )
        (k tree tree) ) )
  (mm tree list) )
}

◊indexE{deforestation}
Первый вариант в~процессе работы постоянно создаёт и тут~же уничтожает кучу списков.
Ситуацию можно поправить с~помощью известной оптимизации, называемой ◊term{deforestation}.
◊seeCite{wad88}
Таким образом можно избавиться от лишних промежуточных структур данных.
Второй вариант в~этом плане ничем не~лучше: просто вместо списков здесь замыкания.
Исходная версия ◊ic{min-max} гораздо быстрее любого из~них
(однако~она использует богомерзкие побочные~эффекты).


◊exanswer{assignment/ex:lambda-cons}

Функции начинаются на~◊ic{q}, чтобы избежать путаницы.

◊indexC{cons}
◊indexC{car}
◊indexC{cdr}
◊code:lisp{
(define (qons a d) (lambda (msg) (msg a d)))
(define (qar pair) (pair (lambda (a d) a)))
(define (qdr pair) (pair (lambda (a d) d)))
}


◊exanswer{assignment/ex:destructive-eq}

Идея в~том, что две точечные пары идентичны,
если модификация одной из них приводит к~изменениям в~другой.

◊code:lisp{
(define (pair-eq? a b)
  (let ((tag (list 'tag))
        (old-car (car a)) )
    (set-car! a tag)
    (let ((result (eq? (car b) tag)))
      (set-car! a old-car)
      result ) ) )
}


◊exanswer{assignment/ex:form-or}

Добавляете анализ новой специальной формы в~◊ic{evaluate}:

◊code:lisp{
...
((or) (evaluate-or (cadr e) (caddr e) r s k))
...
}

◊noindent
После этого определяете её как-то~так:

◊code:lisp{
(define (evaluate-or e1 e2 r s k)
  (evaluate e1 r s (lambda (v ss)
                     (((v 'boolify)
                       (lambda () (k v ss))
                       (lambda () (evaluate e2 r k s)) )) )) )
}

Суть~в~том, что вычисление альтернативной ветки~$\beta$ производится в~старой памяти~◊ic{s}, а~не~в~новой~◊ic{ss}.


◊exanswer{assignment/ex:previous-value}

◊indexC{set!!возвращаемое значение}
◊indexR{возвращаемые значения!присваивания}
◊indexR{присваивание!возвращаемое значение}
Вообще-то такая формулировка задания допускает разночтения:
возвращается~ли значение переменной, которое она имела до вычисления её нового значения или~после.

◊code:lisp{
(define (pre-evaluate-set! n e r s k)
  (evaluate e r s
    (lambda (v ss)
      (k (s (r n)) (update ss (r n) v)) ) ) )

(define (post-evaluate-set! n e r s k)
  (evaluate e r s
    (lambda (v ss)
      (k (ss (r n)) (update ss (r n) v)) ) ) )
}

◊noindent
Это~важно.
Например, значение следующего выражения зависит от~реализации:

◊code:lisp{
(let ((x 1))
  (set! x (set! x 2)) )
}


◊exanswer{assignment/ex:apply/cc}

Основная сложность в~◊ic{apply} — это правильно обработать список её аргументов,
который нужно распаковать из внутреннего представления интерпретатора.

Кроме того, примитивные функции — это тоже функции,
поэтому у~них должны быть определённые «адреса».
Для~◊ic{apply} мы~выбрали произвольное значение~◊ic{-11},
которое гарантированно не~совпадает с~другими функциями.

◊indexC{apply}
◊code:lisp{
(definitial apply
  (create-function -11
    (lambda (v* s k)
      (define (first-pairs v*)
        ;; (assume (pair? v*))
        (if (pair? (cdr v*))
            (cons (car v*) (first-pairs (cdr v*)))
            '() ) )
      (define (terms-of v s)
        (if (eq? (v 'type) 'pair)
            (cons (s (v 'car)) (terms-of (s (v 'cdr)) s))
            '() ) )
      (if (>= (length v*) 2)
          (if (eq? ((car v*) 'type) 'function)
              (((car v*) 'behavior)
               (append (first-pairs (cdr v*))
                       (terms-of (car (last-pair (cdr v*))) s) )
               s k )
              (wrong "First argument not a function") )
          (wrong "Incorrect arity") ) ) ) )
}

Функция ◊ic{call/cc} сохраняет каждое продолжение в~собственной ячейке памяти,
чтобы сделать их уникальными.

◊indexC{call/cc}
◊code:lisp{
(definitial call/cc
  (create-function -13
    (lambda (v* s k)
      (if (= 1 (length v*))
          (if (eq? ((car v*) 'type) 'function)
              (allocate 1 s
               (lambda (a* ss)
                 (((car v*) 'behavior)
                  (list (create-function
                         (car a*)
                         (lambda (vv* sss kk)
                           (if (= 1 (length vv*))
                               (k (car vv*) sss)
                               (wrong "Incorrect arity") ) ) ))
                  ss k ) ) )
              (wrong "Argument not a function") )
          (wrong "Incorrect arity") ) ) ) )
}


◊exanswer{assignment/ex:dotted}

Сложность здесь состоит в~проверке совместимости количества фактически полученных аргументов с~арностью вызываемой функции,
а~также в~преобразовании списков и значений при передаче их между~языками.

◊code:lisp{
(define (evaluate-nlambda n* e* r s k)
  (define (arity n*)
    (cond ((pair? n*) (+ 1 (arity (cdr n*))))
          ((null? n*) 0)
          (else       1) ) )

  (define (update-environment r n* a*)
    (cond ((pair? n*) (update-environment
                       (update r (car n*) (car a*))
                       (cdr n*) (cdr* a) ))
          ((null? n*) r)
          (else (update r n* (car a*))) ) )

  (define (update-store s a* v* n*)
    (cond ((pair? n*) (update-store (update s (car a*) (car v*))
                                    (cdr a*) (cdr v*) (cdr n*) ))
          ((null? n*) s)
          (else (allocate-list v* s (lambda (v ss)
                                      (update ss (car a*) v) ))) ) )
  (allocate 1 s
    (lambda (a* ss)
      (k (create-function
          (car a*)
          (lambda (v* s k)
            (if (compatible-arity? n* v*)
                (allocate (arity n*) s
                 (lambda (a* ss)
                   (evaluate-begin e*
                                   (update-environment r n* a*)
                                   (update-store ss a* v n*)
                                   k ) ) )
                (wrong "Incorrect arity") ) ) )
         ss ) ) ) )

(define (compatible-arity? n* v*)
  (cond ((pair? n*) (and (pair? v*)
                         (compatible-arity? (cdr n*) (cdr v*)) ))
        ((null? n*) (null? v*))
        ((symbol? n*) #t) ) )
}
