#lang racket

(require racket/unix-socket
         racket/system
         racket/string
         json
         "types.rkt")

(provide images/ls
         containers/ls
         containers/start
         containers/stop
         containers/restart
         containers/rename
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
  (let ([split-response (string-split response "\r\n")])
    (if (null? split-response)
        '()
        (let* ([status (words (list-ref split-response 0))]
              [status-code (list-ref status 1)]
              [spl (string-split response "\r\n\r\n")]
              [body (if (>= (length spl) 2)
                        (list-ref spl 1)
                        "")])
          (cons status-code body)))))

(define (make-request-str type str data)
  (let ([ctype (match type
                 ['GET "GET"]
					       ['POST "POST"]
					       [_ (error "Invalid request type "
					                        type)])])
    (define req
      (format "\"~a ~a ~a\""
            ctype
            str
            "HTTP/1.0\\r\\n"))
    (if (null? data)
        req
        (string-append
          req
          (format "Content-Type: application/json\r\n\r\n~a"
                  data)))))

(define (http-successful? response)
  (and (not (null? response))
       (equal? (car response) "200")))

;; Docker call utils

(define (docker-call command)
  (define request (format "echo ~a | nc -U ~a"
                          command
                          docker-sock))
  (let ([response (http-parse
          				  (with-output-to-string
          				    (lambda () (system request))))])
    (if (http-successful? response)
        (cons (car response)
    		      (string->jsexpr (cdr response)))
        (if (null? response)
            (cons 'failed "No response from daemon")
            response))))

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

(define call-with-command
  (lambda (type command #:data [data null]
                        #:res-only [res-only #f])
    (let ([result (docker-call
                    (make-request-str type command data))])
      (when (or (null? result)
                (and (pair? result)
                     (eq? (car result) 'failed)))
        (raise (format "failed API call: ~a" (cdr result))))
      (if res-only
          (cdr result)
          result))))

;; Docker methods

(define (docker/version)
  (call-with-command 'GET "/version" #:res-only #t))

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
                                `(,name "start"))])
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
  (let ([command (build-command "containers"
                                `(,name "rename")
                   #:query `(["name" ,new-name]))])
    (call-with-command 'POST command)))

(define containers/create
  (lambda (image #:command [command null]
                 #:hostname [hostname null]
                 #:user [user null]
                 #:detach [detach #f]
                 #:stdin-open [stdin-open #f]
                 #:tty [tty #f]
                 #:mem-limit [mem-limit null]
                 #:ports [ports null]
                 #:environment [environment null]
                 #:dns [dns null]
                 #:volumes [volumes null]
                 #:volumes-from [volumes-from null]
                 #:network-disabled [network-disabled #f]
                 #:name [name null]
                 #:entrypoint [entrypoint null]
                 #:cpu-shares [cpu-shares null]
                 #:working-dir [working-dir null]
                 #:domainname [domainname null]
                 #:memswap-limit [memswap-limit null]
                 #:cpuset [cpuset null]
                 #:host-config [host-config null]
                 #:mac-address [mac-address null]
                 #:labels [labels null]
                 #:volume-driver [volume-driver null]
                 #:stop-signal [stop-signal null]
                 #:networking-config [networking-config null]
                 #:healthcheck [healthcheck null]
                 #:stop-timeout [stop-timeout null]
                 #:runtime [runtime null])
    (let* ([version (hash-ref (docker/version) 'ApiVersion)]
           [config (create-container-config
                      version image command hostname user detach
                      stdin-open tty mem-limit ports
                      environment dns volumes volumes-from
                      network-disabled entrypoint
                      cpu-shares working-dir domainname
                      memswap-limit cpuset host-config
                      mac-address labels volume-driver stop-signal
                      networking-config healthcheck stop-timeout runtime)]
           [command (build-command "containers"
                                   '("create")
                      #:query `(["name" ,name]))])
      config
      #|(call-with-command 'POST command #:data config)|#)))