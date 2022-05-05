#lang racket/base

(provide render-katex)

(require racket/string
         racket/list
         racket/path
         racket/port
         pollen/setup
         json
         xml)

;; Start the server process and keep it running. When this process dies,
;; it will close the stdin pipe which is a cue for the server to exit.
;; This is a persistent server as spawning Node.js process is slow.
(define-values (*katex-server-process*
                *katex-server-stdout*
                *katex-server-stdin*)
  (let* ((root-path (path->complete-path (current-project-root)))
         (katex-server (build-path root-path "katex-server")) )
    (define-values (process stdout stdin _)
      (subprocess #f #f (current-error-port) katex-server) )
    (file-stream-buffer-mode stdin 'line)
    (values process stdout stdin) ) )

; TODO: apply attrs to the top-level response
(define (render-katex attrs elements #:display? (display-mode? #f))
  (let ((request (hasheq 'tex (gather-strings elements)
                         'displayMode display-mode? )))
    (write-json request *katex-server-stdin*)
    (newline *katex-server-stdin*)
    (let ((response (read-json *katex-server-stdout*)))
      (string->xexpr response) ) ) )

(define (gather-strings elements)
  (string-append* (filter string? (flatten elements))) )
