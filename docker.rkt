#lang racket

(require racket/unix-socket
         racket/system
         racket/string
         json)

(provide images/ls
         containers/ls
         call-with-command)

(define (string->bytes str)
  (let ([port (open-output-bytes)])
    (write str port)
    (get-output-bytes port)))

(define (words str)
  (string-split str " "))

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
         [body (if (< (length spl) 3)
                   (list-ref spl 1)
                   '())])
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
      (format "/containers/json?all=~a" all))))