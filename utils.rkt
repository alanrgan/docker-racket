#lang racket

; TODO - contracts on util functions
(provide (all-defined-out))

(define (normalize-links links)
	(sort links <
		#:key (lambda (x) (cons x))))

(define (compare-versions v1 v2)
  (let* ([sv1 (map string->number (string-split v1 "."))]
         [sv2 (map string->number (string-split v2 "."))]
         [compared (map <= sv1 sv2)])
    (cond ((and (= (car sv1) (car sv2))
                (= (cadr sv1) (cadr sv2))) 0)
          ((and (= (car sv1) (car sv2))
                (cadr compared)) 1)
          ((and (= (car sv1) (car sv2))
                (not (cadr compared))) -1)
          ((car compared) 1)
          (else -1))))

(define (version-gte v1 v2)
  (< (compare-versions v1 v2) 1))
(define (version-lt v1 v2)
  (not (version-gte v1 v2)))

(define (parse-devices devices)
  (define device-list '())
  (for ([device devices])
    (let ([device-mapping (string-split device ":")])
      (unless (null? device-mapping)
        (define host-path (car device-mapping))
        (define path-in-container
          (if (> (length device-mapping) 1)
            	(cadr device-mapping)
            	host-path))
        (define permissions
          (if (> (length device-mapping) 2)
              (list-ref device-mapping 2)
              "rwm"))
        (define tmp-hash (make-hash))
        (hash-set! tmp-hash 'PathOnHost host-path)
        (hash-set! tmp-hash 'PathInContainer path-in-container)
        (hash-set! tmp-hash 'CgroupPermissions permissions)
        (set! device-list
              (cons tmp-hash device-list)))))
  device-list)

(define (format-hashenv env)
  (map (lambda (x) (format "~a=~a" (car x) (cdr x)))
       (hash->list env)))