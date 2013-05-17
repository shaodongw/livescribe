#lang racket/base

(require racket/list)
(require racket/string)
(require racket/match)

(require xml)
(require (planet clements/sxml2:1:3))

(require "utils.rkt")

(provide xml->xexp
         xml-file->xexp
         entry-file?
         comment-file?
         dispatch-file)

(struct lj-entry
  (subject
   taglist
   eventtime
   url
   itemid
   ditemid
   timestamp
   replies
   logtime
   body))

(struct lj-comment
  (subject
   date
   id
   parentid
   state
   body))

(define (xml->xexp data)
  (xml->xexpr
   (document-element
    (read-xml data))))

(define (xml-file->xexp file)
  (call-with-input-file file
    (lambda (in)
      (xml->xexp in))))

(define (remove-newlines str)
  (string-replace str
                  "\n"
                  (string-replace str "\n " "")))

(define (tag-value lst)
  (if (= (length lst) 3)
      (string-trim (third lst))
      ""))

(define (sxpath-value path data)
  (let ([value ((sxpath `(// ,path)) data)])
    (cond [(null? value) (list '())]
          [else value])))

(define (collect proc input args)
  (let loop ([proc proc]
             [input input]
             [args args]
             [acc '()])
    (cond [(null? args) (reverse acc)]
          [else (loop proc input (cdr args)
                      (cons (proc (car args) input) acc))])))

(define (entry-metadata data)
  (map-append tag-value
              (collect sxpath-value
                       data
                       '(subject
                         taglist
                         logtime
                         eventtime
                         url
                         itemid
                         ditemid
                         event_timestamp
                         reply_count))))

(define (entry-body data)
  (list (remove-newlines
         (foldr string-append ""
                (rrest (first (sxpath-value 'event data)))))))

(define (entry-contents file)
  (let ([data (xml-file->xexp file)])
    (match-let ([(list a b c d e f g h i j)
                 (append
                  (entry-metadata data)
                  (entry-body data))])
               (list (lj-entry a b c d e f g h i j)))))

(define (comment-metadata data)
  (map (lambda (lst)
         (map tag-value lst))
       (collect sxpath-value
                data
                '(subject
                  date
                  id
                  parentid
                  state))))

(define (comment-body data)
  (map (lambda (lst)
         (map tag-value lst))
       (collect sxpath-value data '(body))))

(define (comment-contents file)
  (let ([data (xml-file->xexp file)])
    (map (lambda (x)
           (match-let ([(list subject
                              date
                              id
                              parentid
                              state
                              body)
                        x])
                      (lj-comment subject
                                  date
                                  id
                                  parentid
                                  state
                                  body)))
         (match-let ([(list a b c d e f)
                      (append (comment-metadata data)
                              (comment-body data))])
                    (for/list ([subject a]
                               [date b]
                               [id c]
                               [parentid d]
                               [state e]
                               [body f])
                      (append-map list
                                  (list subject
                                        date
                                        id
                                        parentid
                                        state
                                        body)))))))

(define (entry-xexp? xexp)
  (eqv? (car xexp) 'event))

(define (comment-xexp? xexp)
  (eqv? (car xexp) 'comments))

(define (entry-file? file)
  (entry-xexp? (xml-file->xexp file)))

(define (comment-file? file)
  (comment-xexp? (xml-file->xexp file)))

(define (dispatch-file file)
  (if (file-exists? file)
      (cond [(entry-file? file)
             (entry-contents file)]
            [(comment-file? file)
             (comment-contents file)]
            [else (format "foo!" file)])
      '()))
