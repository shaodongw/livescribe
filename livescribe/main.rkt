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
  [xml-file->scribble-data
   (->* ((and/c (or/c string? path?) file-exists?)) ()
        any/c)]
  [xml-file->scribble-file
   (->* ((and/c (or/c string? path?) file-exists?) string?) ()
        any)]
  [main
   (->* ((list/c file-exists? string?)) ()
        any/c)]))

;;; Parameters
(define current-verbosity (make-parameter 0))
(define current-outfile (make-parameter #f))

;;; Global definitions
(define program-name "livescribe")

(define scribble-suffix ".scrbl")

(define scribble-header "#lang scribble/base")

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

(define entry-body-fields-xml
  '(subject
    event
    taglist))

(define comment-metadata-fields-xml
  '(id
    parentid
    state
    date))

(define comment-body-fields-xml
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

;;; Shamelessly stolen from greghendershott's frog.rkt
(define (prn level format . args)
  (when (>= (current-verbosity) level)
    (apply printf format args)
    (newline)))

(define (prn0 format . args) (apply prn 0 format args))
(define (prn1 format . args) (apply prn 1 format args))
(define (prn2 format . args) (apply prn 2 format args))

;;; Entries
(define (entry-metadata data)
  (collect-tag-values data entry-metadata-fields-xml))

(define (entry-body data)
  (append-map (λ (tag)
                (let ([ltag (list tag)])
                  (case tag
                    [(subject taglist)
                     (collect-tag-values data ltag)]
                    [(event)
                     (map (λ (t)
                            (remove-newlines
                             (foldr string-append ""
                                    (rrest (first (sxpath-value t data))))))
                          ltag)])))
              entry-body-fields-xml))

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
  (append-map (λ (tag)
                (let ([ltag (list tag)])
                  (case tag
                    [(subject)
                     (list (collect-tag-values data ltag))]
                    [(body)
                     (list (map (λ (x)
                                  (foldr string-append "" (rrest x)))
                                (sxpath-value tag data)))])))
              comment-body-fields-xml))

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
    (printf "~a~n"
            (format (string-append "@" cmd dat open "~a" close)
                    str))))

(define (fmt-curly cmd str) (fmt cmd str #:open "{" #:close "}"))
(define (fmt-square cmd str) (fmt cmd str #:open "[" #:close "}"))
(define (fmt-round cmd str) (fmt cmd str #:open "(" #:close ")"))

(define (sfmt-title text) (fmt-curly "title" text))
(define (sfmt-para text) (fmt-curly "para" text))
(define (sfmt-section text) (fmt-curly "section" text))

;;; File writers
(define (display-scribble-header)
  (ln scribble-header))

(define (entry-file->scribble file)
  (let ([item (entry-file-contents file)])
    (display-scribble-header)
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
     (sfmt-title subject)
     (sfmt-para body)
     (sfmt-para tag-list))))

(define (comment-file->scribble file)
  (display-scribble-header)
  (sfmt-title "Comments")
  (for ([item (comment-file-contents file)])
    (match-let
     ([(list id
             parent-id
             state
             date
             subject
             body)
       item])
     (sfmt-section subject)
     (sfmt-para date)
     (sfmt-para body))))

(define (xml-file->scribble-data file)
  (cond [(entry-file? file)
         (entry-file->scribble file)]
        [(comment-file? file)
         (comment-file->scribble file)]))

(define (xml-file->scribble-file infile outfile)
  (let ([ifile (ensure-object-path infile)]
        [ofile (ensure-object-path outfile)])
    (with-output-to-file ofile
      #:exists 'truncate/replace
      (λ ()
        (xml-file->scribble-data ifile)))))

;;; TODO
(define (xml-file->markdown-file infile outfile) '())

;;; Top-level calls
(define (main args)
  (match-let
   ([(list infile outfile) args])
   (case (string->symbol (suffix outfile))
     [(scrbl)
      (xml-file->scribble-file infile outfile)]
     [(md markdown text)
      (xml-file->markdown-file infile outfile)])))

(module+ main
  (command-line
   #:program program-name
   #:once-any
   [("-v" "--verbose")
    "Compile with verbose messages."
    (current-verbosity 1)]
   [("-V" "--very-verbose")
    "Compile with very verbose messages."
    (current-verbosity 2)]
   [("-o" "--outfile") outfile
    "Specify output file."
    (current-outfile outfile)]
   #:args args
   (main args)))
