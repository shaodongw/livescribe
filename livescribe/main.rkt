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
 "utils.rkt"
 scribble/render
 (prefix-in text:     scribble/text-render)
 (prefix-in markdown: scribble/markdown-render)
 (prefix-in html:     scribble/html-render)
 (prefix-in latex:    scribble/latex-render)
 (prefix-in pdf:      scribble/pdf-render))

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
(define current-render-type (make-parameter #f))

;;; Global definitions
;; (define multi-html:render-mixin
;;   (lambda (%) (html:render-multi-mixin (html:render-mixin %))))

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
(define (prn lvl fmt . args)
  (when (>= (current-verbosity) lvl)
    (apply printf fmt args)
    (newline)))

(define (prn0 fmt . args) (apply prn 0 fmt args))
(define (prn1 fmt . args) (apply prn 1 fmt args))
(define (prn2 fmt . args) (apply prn 2 fmt args))

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
(define ($ cmd str [open "|{"] [close "}|"] [datum ""])
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
(define (entry-file->scribble-data file)
  (let ([item (entry-file-contents file)])
    (display-headers)
    (match item
      [(list item-id
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
       (dl ($ 'title subject))
       (dl ($ 'bold "Item ID:") item-id)
       (dl ($ 'bold "Event Time:") event-time)
       (dl ($ 'bold "Event Timestamp:") event-timestamp)
       (dl ($ 'bold "Revision time:") rev-time)
       (dl ($ 'bold "Revision number:") rev-num)
       (dl ($ 'bold "Log time:") log-time)
       (dl ($ 'bold "Reply count:") reply-count)
       (dl ($ 'bold "Current Mood:") current-mood-id)
       (dl ($ 'bold "Current Music:") current-music)
       (dl ($ 'bold "URL:") ($ 'url url))
       (dl ($ 'bold "Tags:") tag-list)
       (dl ($ 'bold "Body:"))
       (dl ($ 'para body))])))

(define (comment-file->scribble-data file)
  (display-headers)
  (dl ($ 'title "Comments"))
  (for ([item (comment-file-contents file)])
    (match item
      [(list id
             parent-id
             state
             date
             subject
             body)
       (dl ($ 'section subject))
       (dl ($ 'bold "Subject:") subject)
       (dl ($ 'bold "ID:") id)
       (dl ($ 'bold "Parent ID:") parent-id)
       (dl ($ 'bold "State:") state)
       (dl ($ 'bold "Date:") date)
       (dl ($ 'bold "Body:"))
       (dl ($ 'para body))])))

(define (xml-file->scribble-data file)
  (cond [(entry-file? file)
         (entry-file->scribble-data file)]
        [(comment-file? file)
         (comment-file->scribble-data file)]))

(define (xml-file->scribble-file infile outfile)
  (prn1 "Converting ~a to ~a." infile outfile)
  (let ([ifile (ensure-object-path infile)]
        [ofile (ensure-object-path outfile)])
    (with-output-to-file ofile
      #:exists 'truncate/replace
      (λ ()
        (xml-file->scribble-data ifile)))))

;;; Render
(define (build-listof-parts files)
  (map (λ (file)
         (dynamic-require `(file ,file) 'doc))
       files))

(define (build-listof-dests files suffix)
  (map (λ (file)
         (path-replace-suffix file suffix))
       files))

(define (render-file type file)
  (let* ([files (map ensure-string-path (list file))]
         [parts (build-listof-parts files)])
  (case (string->symbol type)
    [(markdown md)
     (prn1 "Rendering ~a as Markdown." file)
     (render parts (build-listof-dests files ".md")
             #:render-mixin markdown:render-mixin)]
    [(text txt)
     (prn1 "Rendering ~a as Plaintext." file)
     (render parts (build-listof-dests files ".txt")
             #:render-mixin text:render-mixin)]
    [(html)
     (prn1 "Rendering ~a as single HTML file." file)
     (render parts (build-listof-dests files ".html")
             #:render-mixin html:render-mixin)]
    ;; [(htmls)
    ;;  (prn1 "Rendering ~a as multiple HTML files." file)
    ;;  (render parts (build-listof-dests files ".html")
    ;;          #:render-mixin multi-html:render-mixin)]
    [(latex)
     (prn1 "Rendering ~a as LaTeX." file)
     (render parts (build-listof-dests files ".tex")
             #:render-mixin latex:render-mixin)]
    [(pdf)
     (prn1 "Rendering ~a as PDF." file)
     (render parts (build-listof-dests files ".pdf")
             #:render-mixin pdf:render-mixin)]
    [else (error 'render-file "Unknown render type: ~a" type)])))

;;; Top-level
(define (main files)
  (for ([file files])
    (let ([dest-file (suffix->scrbl file)]
          [render-type (current-render-type)])
      (xml-file->scribble-file file dest-file)
      (when (and (file-exists? dest-file)
                 render-type)
        (render-file render-type dest-file)))))

(module+ main
  (command-line
   #:program program-name
   #:once-any
   [("-v" "--verbose")
    "Compile with verbose messages."
    (current-verbosity 1)
    (prn1 "Verbose output enabled.")]
   [("-V" "--very-verbose")
    "Compile with very verbose messages."
    (current-verbosity 2)
    (prn2 "Very verbose output enabled.")]
   #:once-each
   [("-r" "--render") type
    ("Render Scribble file as <type>,"
     "where <type> is [markdown|md|text|txt|html|text|pdf]")
    (current-render-type type)]
   #:args (file . another-file)
   (let ([files (cons file another-file)])
     (main files))))
