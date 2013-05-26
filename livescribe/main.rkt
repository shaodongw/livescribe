#lang racket/base

(require
 racket/contract
 racket/list
 racket/string
 racket/cmdline
 racket/match
 racket/file
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

(define (dl0 . rst)
  (displayln (apply string-append rst)))

(define (dl . rst)
  (displayln (apply string-append (add-between rst " ")))
  (newline))

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
                (case tag
                  [(subject taglist)
                   (collect-tag-values data (list tag))]
                  [(event)
                   (list (foldr string-append ""
                                (rrest (first (sxpath-value tag data)))))]))
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
(define ($ cmd str [open "{"] [close "}"] [datum ""])
  (let ([at "@"]
        [dat (cond [(not (empty-string? datum))
                    (string-append "[" datum "]")]
                   [else ""])]
        [scmd (symbol->string cmd)])
    (string-append at scmd dat open str close)))


;;; Headers
(define (display-scribble-header)
  (dl scribble-header))

(define (display-sutils-header)
  (dl "@(require \"sutils.rkt\")"))

(define (create-sutils-file) '())

(define (display-headers)
  (display-scribble-header)
  ;; (display-sutils-header)
  )

;;; File writers
(define (entry-file->scribble file)
  (let ([item (entry-file-contents file)])
    (display-headers)
    (match-let
     ([(list item-id
             event-time
             url
             d-item-id
             event-timestamp
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
     (dl ($ 'title subject))
     (dl ($ 'bold "Subject:") subject)
     (dl ($ 'bold "Event Time:") event-time)
     (dl ($ 'bold "Event Timestamp:") event-timestamp)
     (dl ($ 'bold "Current Mood:") current-mood-id)
     (dl ($ 'bold "Current Music:") current-music)
     (dl ($ 'bold "URL:") url)
     (dl ($ 'bold "Tags:") tag-list)
     (dl ($ 'para body)))))

(define (comment-file->scribble file)
  (display-headers)
  (dl ($ 'title "Comments"))
  (for ([item (comment-file-contents file)])
    (match-let
     ([(list id
             parent-id
             state
             date
             subject
             body)
       item])
     (dl ($ 'section subject))
     (dl ($ 'bold "Subject:") subject)
     (dl ($ 'bold "ID:") id)
     (dl ($ 'bold "Parent ID:") parent-id)
     (dl ($ 'bold "Date:") date)
     (dl ($ 'para body)))))

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

;;; Todo
(define (xml-file->markdown-data file) '())

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
   #:args args
   (main args)))
