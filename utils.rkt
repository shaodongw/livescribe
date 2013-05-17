#lang racket/base

(require racket/list)

(provide map-append
         id
         pwd
         cd
         ls
         string-first
         string-reverse
         string-last
         ensure-string-path
         ensure-object-path
         tilde
         ~)

(define (map-append proc lst)
  (map proc (apply append lst)))

(define (id x) x)

(define (pwd)
  (current-directory))

(define (cd [path (expand-user-path "~/")])
  (current-directory path))

(define (ls . args)
  (apply directory-list args))

(define (string-first str)
  (string-ref str 0))

(define (string-reverse str)
  (list->string (reverse (string->list str))))

(define (string-last str)
  (string-first (string-reverse str)))

(define (ensure-string-path path)
  (if (path? path)
      (path->string path)
      path))

(define (ensure-object-path path)
  (if (string? path)
      (string->path path)
      path))

(define (tilde-first? str)
  (char=? #\~ (string-first str)))

(define (tilde [arg #f])
  (if (not arg)
      (expand-user-path "~")
      (let ([path (ensure-string-path arg)])
        (cond [(tilde-first? path)
               (expand-user-path
                (build-path path))]
              [else
               (expand-user-path
                (build-path (string-append "~" path)))]))))

(define (~ . args)
  (apply tilde args))
