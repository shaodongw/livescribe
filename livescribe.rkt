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

(define (third-or-empty-string lst)
  (if (= (length lst) 3)
      (string-trim (third lst))
      ""))

(define (sxpath-value path data)
  (let ([value ((sxpath `(// ,path)) data)])
    (cond [(null? value) (list '())]
          [else value])))

(define (entry-metadata data)
  (map-append third-or-empty-string
              (list (sxpath-value 'subject data)
                    (sxpath-value 'taglist data)
                    (sxpath-value 'logtime data)
                    (sxpath-value 'eventtime data)
                    (sxpath-value 'url data)
                    (sxpath-value 'itemid data)
                    (sxpath-value 'ditemid data)
                    (sxpath-value 'event_timestamp data)
                    (sxpath-value 'reply_count data))))

(define (entry-file-metadata file)
  (let ([data (xml-file->xexp file)])
    (entry-metadata data)))

(define (entry-body data)
  (list (remove-newlines
         (foldr string-append ""
                (rest (rest (first ((sxpath '(event)) data))))))))

(define (entry-contents file)
  (let ([data (xml-file->xexp file)])
    (match-let ([(list a b c d e f g h i j)
                 (append
                  (entry-metadata data)
                  (entry-body data))])
               (list (lj-entry a b c d e f g h i j)))))

(define (comment-metadata data)
  (map (lambda (lst)
         (map third-or-empty-string lst))
       (list
        (sxpath-value 'subject data)
        (sxpath-value 'date data)
        (sxpath-value 'id data)
        (sxpath-value 'parentid data)
        (sxpath-value 'state data))))

(define (comment-body data)
  (map (lambda (lst)
         (map third-or-empty-string lst))
       (list
        (sxpath-value 'body data))))

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
