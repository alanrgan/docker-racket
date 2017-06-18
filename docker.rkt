#lang racket

(require racket/unix-socket
         racket/system
         racket/string
         json)

(provide images/ls
         containers/ls
         call-with-command)

(define (any->string x)
  (format "~a" x))

(define (string->bytes str)
  (let ([port (open-output-bytes)])
    (write str port)
    (get-output-bytes port)))

(define (words str)
  (string-split str " "))

(define (concat-strs lst)
  (foldl (lambda (s acc) (string-append acc s) "" lst)))

(define (join-pair with pair)
  (if (null? pair)
      '()
      (string-append (car pair)
                     with
                     (any->string (cadr pair)))))

;; Right now only Unix systems supported

(define docker-sock
	"/var/run/docker.sock")

;; HTTP utils

(define (http-parse response)
  (let* ([status
          (words
            (list-ref (string-split
                        response "\r\n")
                      0))]
         [status-code (list-ref status 1)]
         [spl (string-split response "\r\n\r\n")]
         [body (if (>= (length spl) 2)
                   (list-ref spl 1)
                   "")])
    (cons status-code body)))

(define (make-request-str type str)
  (let ([ctype (match type
                 ['GET "GET"]
					       ['POST "POST"]
					       [_ (error "Invalid request type "
					                        type)])])
  (format "\"~a ~a ~a\""
          ctype
          str
          "HTTP/1.0\\r\\n")))

;; Docker call utils

(define (docker-call command)
  (define request
    (format "echo ~a | nc -U ~a"
            command
            docker-sock))
  (let ([response
		      (http-parse
  				  (with-output-to-string
  				    (lambda () (system request))))])
    (cons (car response)
		      (string->jsexpr
		        (cdr response)))))

(define build-command
  (lambda (domain args #:query [query '()])
    (define base-command
      (string-append "/"
        (string-join (cons domain args) "/")))
    (let ([query (map (curry join-pair "=") query)])
      (cond ((null? query) base-command)
            (else
              (string-append
                base-command
                "?"
                (string-join query "&")))))))

(define (call-with-command type command)
  (docker-call
    (make-request-str type command)))

;; Docker methods

(define (images/ls)
  (call-with-command 'GET "/images/json"))

(define containers/ls
  (lambda (#:all? [allv #f])
    (define all (if allv 1 0))
  	(call-with-command
     	'GET
      (build-command "containers"
                     '("json")
        #:query `(["all" ,all])))))

(define (containers/start name)
  (let ([command (build-command "containers"
                    #:query `(,name "start"))])
    (call-with-command 'POST command)))

(define containers/stop
  (lambda (name #:wait-time [wait-time 0])
    (let ([command (build-command "containers"
                                  `(,name "stop")
                     #:query `(["t" ,wait-time]))])
      (call-with-command 'POST command))))

(define containers/restart
  (lambda (name #:wait-time [wait-time 0])
    (when (number? wait-time)
          (set! wait-time (number->string wait-time)))
    (let ([command (build-command "containers"
                                  `(,name "restart")
                      #:query `(["t" ,wait-time]))])
      (call-with-command 'POST command))))

(define (containers/rename name new-name)
  (let ([command (format "/containers/~a/rename?name=~a"
                         name
                         new-name)])
    (call-with-command 'POST command)))