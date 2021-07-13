#lang racket/base

(require json
         racket/contract
         racket/exn
         racket/match
         racket/port
         racket/runtime-path
         racket/system
         threading
         xml)

(provide pygments-server?
         open-pygments-server
         (contract-out [pygments-server-closed? (-> pygments-server? boolean?)]
                       [current-pygments-server (parameter/c (or/c pygments-server? #f))]
                       [close-pygments-server (-> pygments-server? void?)]
                       [call-with-pygments-server (-> (-> pygments-server? any) any)]
                       [call-with-current-pygments-server (-> (-> any) any)]
                       [pygmentize (->* [string? #:language string?] [#:server pygments-server?] xexpr?)]))

(define-runtime-path pygments-server.py "pygments-server.py")

(struct pygments-server (pid stdout stdin stderr interact lock cust)
  #:property prop:object-name (struct-field-index pid)
  #:property prop:custom-write
  (λ (self out mode) (fprintf out "#<pygments-server:~a>" (pygments-server-pid self))))

(define (pygments-server-closed? server)
  (custodian-shut-down? (pygments-server-cust server)))

(define current-pygments-server (make-parameter #f))

(define (open-pygments-server)
  (define python3 (find-executable-path "python3"))
  (unless python3
    (raise-arguments-error 'make-pygmentize "python3 is not in PATH"))

  (define cust (make-custodian))
  (match-define (list stdout stdin pid stderr interact)
    (parameterize* ([current-subprocess-custodian-mode 'kill]
                    [current-custodian cust])
      (process* python3 pygments-server.py)))
  (define server (pygments-server pid stdout stdin stderr interact (make-semaphore 1) cust))
  server)

(define (close-pygments-server server)
  (custodian-shutdown-all (pygments-server-cust server)))

(define (call-with-pygments-server proc)
  (call-with-continuation-barrier
   (λ ()
     (define break-paramz (current-break-parameterization))
     (parameterize-break #f
       (define server (open-pygments-server))
       (dynamic-wind
        (λ () #f)
        (λ () (call-with-break-parameterization break-paramz
                (λ () (proc server))))
        (λ () (close-pygments-server server)))))))

(define (call-with-current-pygments-server thunk)
  (call-with-pygments-server
   (λ (server)
     (parameterize ([current-pygments-server server])
       (thunk)))))

(define (pygmentize source
                    #:language language
                    #:server [server (or (current-pygments-server)
                                         (raise-arguments-error 'pygmentize "current-pygments-server is not set"))])
  (define (raise-closed-error)
    (raise-argument-error 'pygmentize "(not/c pygments-server-closed?)" pygments-server))
  (when (pygments-server-closed? server)
    (raise-closed-error))

  (call-with-semaphore
   (pygments-server-lock server)
   (λ ()
     (unless (eq? ((pygments-server-interact server) 'status) 'running)
       (define stderr (port->string (pygments-server-stderr server)))
       (close-pygments-server server)
       (raise-arguments-error 'pygmentize "pygments server is no longer running"
                              "stderr..." stderr
                              "exit code" ((pygments-server-interact server) 'exit-code)))

     (with-handlers ([(λ (exn) (and (exn:fail? exn) (pygments-server-closed? server)))
                      (λ (_) (raise-closed-error))])
       (call-in-nested-thread
        (λ ()
          (write-json (hasheq 'language language 'source source)
                      (pygments-server-stdin server))
          (newline (pygments-server-stdin server))
          (flush-output (pygments-server-stdin server)))
        (pygments-server-cust server)))

     (with-handlers* ([exn:fail?
                       (λ (exn)
                         ((pygments-server-interact server) 'kill)
                         ((pygments-server-interact server) 'wait)
                         (define stderr (port->string (pygments-server-stderr server)))
                         (close-pygments-server server)
                         (raise-arguments-error 'pygmentize "error reading from pygments server"
                                                "read error..." (exn->string exn)
                                                "stderr..." stderr
                                                "exit code" ((pygments-server-interact server) 'exit-code)))]
                      [(λ (exn) (and (exn:fail? exn) (pygments-server-closed? server)))
                       (λ (_) (raise-closed-error))])
       (call-in-nested-thread
        (λ ()
          (~> (read-json (pygments-server-stdout server))
              (string-append "<code class=\"pygments\">" _ "</code>")
              string->xexpr))
        (pygments-server-cust server))))))
