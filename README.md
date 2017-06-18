# docker-racket
Simple Docker client library for Racket (WIP)

**Note:** Only works if Docker is run using a unix socket 

**Example**
```Racket
(require "docker.rkt")

; list all images
(images/ls)

; list all active containers
(containers/ls)

; list all containers
(containers/ls #:all? #t)
```
