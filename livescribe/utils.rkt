#lang racket/base

(require racket/contract)
(require racket/list)

(provide
 (contract-out
  [map-append
   (->* (procedure? list?) ()
        list?)]
  [ffirst
   (->* (list?) ()
        any/c)]
  [rrest
   (->* (list?) ()
        any/c)]
  [id
   (->* (any/c) ()
        any/c)]
  [pwd
   (->* () ()
        path?)]
  [cd
   (->* () (path?)
        void?)]
  [ls
   (->* () ()
        #:rest (listof (or/c path? string?))
        (or/c empty? (listof path?)))]
  [string-first
   (->* (string?) ()
        char?)]
  [string-reverse
   (->* (string?) ()
        char?)]
  [string-last
   (->* (string?) ()
        char?)]
  [ensure-string-path
   (->* ((or/c path? string)) ()
        string?)]
  [ensure-object-path
   (->* ((or/c path? string)) ()
        path?)]
  [tilde-first?
   (->* (string?) ()
        char?)]
  [tilde
   (->* () ((or/c boolean? path? string?))
        path?)]
  [~
   (->* () ()
        #:rest (listof (or/c path? string?))
        path?)]
  [foldl-string-append
   (->* ((listof string?)) ()
        (listof string?))]
  [foldr-string-append
   (->* ((listof string?)) ()
        (listof string?))]
  [collect
   (->* (procedure? any/c list?) ()
        list?)]))

(define (map-append proc lst)
  (map proc (apply append lst)))

(define (ffirst lst)
  (first (first lst)))

(define (rrest lst)
  (rest (rest lst)))

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

(define (foldl-string-append lst)
  (foldl string-append "" lst))

(define (foldr-string-append lst)
  (foldr string-append "" lst))

(define (collect proc input args)
  (let loop ([proc proc]
             [input input]
             [args args]
             [acc '()])
    (cond [(null? args) (reverse acc)]
          [else (loop proc input (cdr args)
                      (cons (proc (car args) input) acc))])))
