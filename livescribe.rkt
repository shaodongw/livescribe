#lang racket/base

(require racket/list)
(require racket/string)
(require racket/match)

(require xml)
(require (planet clements/sxml2:1:3))

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

(define (entry-metadata data)
  (map third-or-empty-string
       (append ((sxpath '(// subject)) data)
               ((sxpath '(// taglist)) data)
               ((sxpath '(// logtime)) data)
               ((sxpath '(// eventtime)) data)
               ((sxpath '(// url)) data)
               ((sxpath '(// itemid)) data)
               ((sxpath '(// ditemid)) data)
               ((sxpath '(// event_timestamp)) data)
               ((sxpath '(// reply_count)) data))))

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
        ((sxpath '(// subject)) data)
        ((sxpath '(// date)) data)
        ((sxpath '(// id)) data)
        ((sxpath '(// parentid)) data)
        ((sxpath '(// state)) data))))

(define (comment-body data)
  (map (lambda (lst)
         (map third-or-empty-string lst))
       (list
        ((sxpath '(// body)) data))))

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
         (match-let ([(list subject date id parentid state body)
                      (append (comment-metadata data)
                              (comment-body data))])
                    (for/list ([subject subject]
                               [date date]
                               [id id]
                               [parentid parentid]
                               [state state]
                               [body body])
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
            [else (format "Error: The file ~A is of unknown type.~N" file)])
      '()))
