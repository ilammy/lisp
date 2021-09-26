#lang pollen

◊(local-require racket/base racket/list racket/path
                pollen/pagetree)

◊; TODO: extract into module
◊; TODO: document and cleanup
◊; TODO: strip <root> elements
◊; TODO: metas

◊(define (full-chapter-text)
  (parameterize ([current-pagetree (get-pagetree "index.ptree")])
    (let* ([root-path (path->complete-path (current-project-root))]
           [here-path (path->complete-path (select-from-metas 'here-path metas))]
           [here (->pagenode (find-relative-path root-path here-path))]
           [next-sibling (third (memq here (siblings here)))]
           [chapter (takef (next* here) (lambda (node) (not (eq? node next-sibling))))])
      (cons '@ (map get-doc chapter)) ) ) )

◊(full-chapter-text)
