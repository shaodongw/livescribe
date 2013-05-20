#lang racket/base

(require
 racket/contract
 racket/list
 racket/string
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


;;; Global definitions
(define entry-marker 'event)

(define comment-marker 'comments)

(define entry-metadata-fields-xml
  '(itemid
    eventtime
    url
    ditemid
    event_timestamp
    reply_count
    logtime
    opt_preformatted
    personifi_tags
    hasscreened
    commentalter
    revtime
    opt_backdated
    current_moodid
    current_music
    revnum
    can_comment
    anum))

(define entry-content-fields-xml
  '(subject
    event
    taglist))

(define comment-metadata-fields-xml
  '(id
    parentid
    state
    date))

(define comment-content-fields-xml
  '(subject
    body))

;;; Structures
(struct lj-entry                        ;XML tags
  (item-id                              ;itemid
   event-time                           ;eventtime
   url                                  ;url
   d-item-id                            ;ditemid
   event-timestmap                      ;event_timestamp
   reply-count                          ;reply_count
   log-time                             ;logtime
   opt-preformatted                     ;opt_preformatted
   personifi-tags                       ;personifi_tags
   has_screened                         ;hasscreened
   comment-alter                        ;commentalter
   rev-time                             ;revtime
   opt-backdated                        ;opt_backdated
   current-mood-id                      ;current_moodid
   current-music                        ;current_music
   rev-num                              ;revnum
   can-comment                          ;can_comment
   a-num                                ;anum
   subject                              ;subject
   body                                 ;event
   tag-list                             ;taglist
   ))

(struct lj-comment
  (id                                   ;id
   parent-id                            ;parentid
   state                                ;state
   date                                 ;date
   subject                              ;subject
   body                                 ;body
   ))

;;; Essentials
(define (xml->xexp data)
  (xml->xexpr
   (document-element
    (read-xml data))))

(define (xml-file->xexp file)
  (call-with-input-file file
    (λ (in)
      (xml->xexp in))))

;;; Helpers
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

(define (collect-tag-values data tags)
  (map-append tag-value
              (collect sxpath-value data tags)))

(define (merge-tag-values data tags)
  (map (λ (tag)
         (remove-newlines
          (foldr string-append ""
                 (rrest (first (sxpath-value tag data))))))
       tags))

;;; Entries
(define (entry-metadata data)
  (collect-tag-values data entry-metadata-fields-xml))

(define (entry-body data)
  (append-map (λ (tag)
                (case tag
                  [(subject taglist)
                   (collect-tag-values data (list tag))]
                  [else
                   (merge-tag-values data (list tag))]))
              entry-content-fields-xml))

(define (entry-contents-raw data)
  (append (entry-metadata data)
          (entry-body data)))

(define (entry-contents file)
  (let ([data (xml-file->xexp file)])
    (list (apply lj-entry (entry-contents-raw data)))))

;;; Comments
(define (comment-metadata data)
  (for/list ([items (collect sxpath-value data
                             comment-metadata-fields-xml)])
    (map tag-value items)))

(define (comment-body data)
  (for/list ([items (collect sxpath-value data
                             comment-content-fields-xml)])
    (map tag-value items)))

(define (comment-contents-raw data)
  (collect-cars
   (append (comment-metadata data)
           (comment-body data))))

(define (comment-contents file)
  (let ([data (xml-file->xexp file)])
    (map (λ (x)
           (apply lj-comment x))
         (comment-contents-raw data))))

;;; Predicates
(define (entry-xexp? xexp)
  (eqv? (car xexp) entry-marker))

(define (comment-xexp? xexp)
  (eqv? (car xexp) comment-marker))

(define (entry-file? file)
  (entry-xexp? (xml-file->xexp file)))

(define (comment-file? file)
  (comment-xexp? (xml-file->xexp file)))

;;; Readers
(define (read-file file)
  (if (file-exists? file)
      (cond [(entry-file? file)
             (entry-contents file)]
            [(comment-file? file)
             (comment-contents file)]
            [else (error 'read-file "blah")])
      '()))

;;; String formatters
(define (fmt cmd datum str #:open [open "{"] #:close [close "}"])
  (let ([dat (cond [(not (empty-string? datum))
                      (string-append "[" datum "]")]
                     [else ""])])
    (format (string-append "@" cmd dat open "~a" close)
            str)))

(define (fmt-curly cmd str)
  (fmt cmd str #:open "{" #:close "}"))

(define (fmt-square cmd str)
  (fmt cmd str #:open "[" #:close "}"))

;;; TODO: File writers
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

;;; Top-level
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
