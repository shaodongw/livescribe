#lang racket/base

(require
 racket/contract
 racket/list
 racket/string
 racket/cmdline
 racket/match
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
  [entry-file-contents
   (->* ((or/c string? path?)) ()
        any/c)]
  [comment-file-contents
   (->* ((or/c string? path?)) ()
        any/c)]
  [xml-file->scribble
   (->* ((and/c (or/c string? path?) file-exists?)) ()
        any/c)]))

;;; Global definitions
(define scribble-suffix ".scrbl")

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

(define ln displayln)

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
  (path-replace-suffix path scribble-suffix))

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

(define (entry-data-contents data)
  (append (entry-metadata data)
          (entry-body data)))

(define (entry-file-contents file)
  (let ([data (xml-file->xexp file)])
    (entry-data-contents data)))

;;; Comments
(define (comment-metadata data)
  (for/list ([items (collect sxpath-value data
                             comment-metadata-fields-xml)])
    (map tag-value items)))

(define (comment-body data)
  (for/list ([items (collect sxpath-value data
                             comment-content-fields-xml)])
    (map tag-value items)))

(define (comment-data-contents data)
  (collect-cars
   (append (comment-metadata data)
           (comment-body data))))

(define (comment-file-contents file)
  (let ([data (xml-file->xexp file)])
    (comment-data-contents data)))

;;; Predicates
(define (entry-xexp? xexp)
  (eqv? (car xexp) entry-marker))

(define (comment-xexp? xexp)
  (eqv? (car xexp) comment-marker))

(define (entry-file? file)
  (entry-xexp? (xml-file->xexp file)))

(define (comment-file? file)
  (comment-xexp? (xml-file->xexp file)))

;;; String formatters
(define (fmt cmd str #:open open #:close close #:datum [datum ""])
  (let ([dat (cond [(not (empty-string? datum))
                      (string-append "[" datum "]")]
                     [else ""])])
    (format (string-append "@" cmd dat open "~a" close)
            str)))

(define (fmt-curly cmd str)
  (fmt cmd str #:open "{" #:close "}"))

(define (fmt-square cmd str)
  (fmt cmd str #:open "[" #:close "}"))

(define (fmt-round cmd str)
  (fmt cmd str #:open "(" #:close ")"))

(define (ln-fmt-curly . rst)
  (ln (apply fmt-curly rst)))

(define (ln-fmt-square . rst)
  (ln (apply fmt-square rst)))

(define (ln-fmt-round . rst)
  (ln (apply fmt-round rst)))

;;; File writers
(define (entry-file->scribble file)
  (let ([item (entry-file-contents file)])
    (match-let
     ([(list item-id
             event-time
             url
             d-item-id
             event-timestmap
             reply-count
             log-time
             opt-preformatted
             personifi-tags
             has_screened
             comment-alter
             rev-time
             opt-backdated
             current-mood-id
             current-music
             rev-num
             can-comment
             a-num
             subject
             body
             tag-list)
       item])
     (ln-fmt-curly "title" subject)
     (ln-fmt-curly "para" body)
     (ln-fmt-curly "para" tag-list))))

(define (comment-file->scribble file)
  (ln-fmt-curly "title" "Comments")
  (for ([item (comment-file-contents file)])
    (match-let
     ([(list id
             parent-id
             state
             date
             subject
             body)
       item])
     (ln-fmt-curly "section" subject)
     (ln-fmt-curly "para" date)
     (ln-fmt-curly "para" body))))

(define (xml-file->scribble file)
  (cond [(entry-file? file)
         (entry-file->scribble file)]
        [(comment-file? file)
         (comment-file->scribble file)]))

(define (write-scribble-file file)
  (write (xml-file->scribble file)))

(define (make-scribble-file path)
  (let ([file (ensure-object-path path)])
    (when (path-exists? file)
      (with-output-to-file (suffix->scrbl file)
        #:exists 'truncate/replace
        (λ ()
          (write-scribble-file file))))))

(define (make-scribble-files path)
  (when (directory-exists? path)
    (for-each make-scribble-file
              (directory-list path))))

;;; Top-level calls
(define (dispatch-input arg)
  (ln arg)
  (cond [(file-exists? arg) (make-scribble-file arg)]
        [(directory-exists? arg) (make-scribble-files arg)]
        [else #f]))

(define (main args)
  (cond [(> (length args) 0)
         (for-each dispatch-input args)]
        [else (dispatch-input (current-directory))]))

(module+ main
  (command-line
   #:args args
   (main args)))
