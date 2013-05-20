#lang racket/base

(require
 racket/contract
 racket/list
 racket/string
 racket/match
 racket/cmdline
 xml
 sxml
 "utils.rkt")

(provide
 (contract-out
  [xml->xexp
   (->* (any/c) ()
        xexpr/c)]
  [xml-file->xexp
   (->* (path?) ()
        xexpr/c)]
  [entry-file?
   (->* ((or/c string? path?)) ()
        boolean?)]
  [comment-file?
   (->* ((or/c string? path?)) ()
        boolean?)]
  [entry-contents
   (->* ((or/c string? path?)) ()
        any/c)]
  [read-file
   (->* ((or/c string? path?)) ()
        (or/c (listof lj-entry?)
              (listof lj-comment?)))]))

;; This is lame.
(define entry-fields
  '(subject
    taglist
    eventtime
    url
    itemid
    ditemid
    timestamp
    replies
    logtime
    body))

(define comment-fields
  '(subject
    date
    id
    parentid
    state
    body))

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
    (λ (in)
      (xml->xexp in))))

(define (remove-newlines str)
  (string-replace str
                  "\n"
                  (string-replace str "\n " "")))

(define (xml-suffix? str)
  (if (regexp-match "[xX][mM][lL]" (suffix str))
      #t
      #f))

(define (suffix->scrbl path)
  (path-replace-suffix path ".scrbl"))

(define (tag-value lst)
  (if (= (length lst) 3)
      (string-trim (third lst))
      ""))

(define (sxpath-value path data)
  (let ([value ((sxpath `(// ,path)) data)])
    (cond [(null? value) (list '())]
          [else value])))

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
    (match-let ([(list subject
                       taglist
                       logtime
                       eventtime
                       url
                       itemid
                       ditemid
                       event_timestamp
                       reply_count
                       body)
                 (append (entry-metadata data)
                         (entry-body data))])
      (list (lj-entry subject
                      taglist
                      logtime
                      eventtime
                      url
                      itemid
                      ditemid
                      event_timestamp
                      reply_count
                      body)))))

(define (comment-metadata data)
  (map (λ (lst)
         (map tag-value lst))
       (collect sxpath-value
                data
                '(subject
                  date
                  id
                  parentid
                  state))))

(define (comment-body data)
  (map (λ (lst)
         (map tag-value lst))
       (collect sxpath-value data '(body))))

(define (comment-contents file)
  (let ([data (xml-file->xexp file)])
    (map (λ (x)
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

(define (read-file file)
  (if (file-exists? file)
      (cond [(entry-file? file)
             (entry-contents file)]
            [(comment-file? file)
             (comment-contents file)]
            [else (format "foo!" file)])
      '()))

;; Write these
(define (xml-file->scribble file) '())

(define (write-scribble-file file)
  (write (xml-file->scribble file)))

(define (make-scribble-file path)
  (let ([file (ensure-object-path path)])
    (when (and (path-exists? file)
               (xml-suffix? (path->string file)))
      (with-output-to-file (suffix->scrbl file)
        #:exists 'truncate/replace
        (λ ()
          (write-scribble-file file))))))

(define (make-scribble-files path)
  (when (directory-exists? path)
    (for-each make-scribble-file
              (directory-list path))))

(define (dispatch-file arg)
  (cond [(file-exists? arg) (make-scribble-file arg)]
        [(directory-exists? arg) (make-scribble-files arg)]
        [else #f]))

(define (main args)
  (cond [(> (length args) 0)
         (for-each dispatch-file args)]
        [else (dispatch-file (current-directory))]))

(module+ main
  (command-line
   #:args args
   (main args)))
