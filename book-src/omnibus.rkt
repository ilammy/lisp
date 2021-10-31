#lang racket
;
; Collect and flatten Pollen chapters into single pages.
;
; TODO: merge metas of chapter pages? is there anything useful there?
; TODO: include answers to exercises

(provide get-chapter-doc)

(require racket/list racket/path)
(require pollen/core pollen/pagetree pollen/setup)

(define (here-node)
  "Returns the current pagenode of the pagetree."
  (define root-path (path->complete-path (current-project-root)))
  (define here-path (path->complete-path (select-from-metas 'here-path (current-metas))))
  (->pagenode (find-relative-path root-path here-path)) )

(define (current-chapter-nodes)
  "Compiles and returns a list of nodes that comprise the current chapter."
  ; For some reason this parameter is not set to what I expect to be...
  (parameterize ([current-pagetree (get-pagetree "index.ptree")])
    ; Now, the current file -- the one this function is called from -- is the chapter summary.
    ; The actual chapter tree is the next sibling node. And the next node after that is
    ; the first page that in not in the current chapter.
    (define here (here-node))
    (define next-chapter (third (memq here (siblings here))))
    ; Flatten all following nodes and extract them, stopping when the chapter ends.
    ; That is, grab the current chapter stopping at the next node after it.
    (takef (next* here) (lambda (node) (not (eq? node next-chapter)))) ) )

(define (get-doc-content node)
  "Extracts the content of a node: the list of elements of its root element."
  (select-from-doc 'root (get-doc node)) )

(define (get-chapter-doc)
  "Returns merged chapter content."
  (append* '(@) (map get-doc-content (current-chapter-nodes))) )
